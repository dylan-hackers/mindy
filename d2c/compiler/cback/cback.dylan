module: cback
rcs-header: $Header: /scm/cvs/src/d2c/compiler/cback/cback.dylan,v 1.1 1998/05/03 19:55:31 andreas Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// All rights reserved.
// 
// Use and copying of this software and preparation of derivative
// works based on this software are permitted, including commercial
// use, provided that the following conditions are observed:
// 
// 1. This copyright notice must be retained in full on any copies
//    and on appropriate parts of any derivative works.
// 2. Documentation (paper or online) accompanying any system that
//    incorporates this software, or any part of it, must acknowledge
//    the contribution of the Gwydion Project at Carnegie Mellon
//    University.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports, questions, comments, and suggestions should be sent by
// E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
//
//======================================================================

// External interfaces:
//
//  The primary external interfaces used by "main" are: emit-component
// emit-tlf-gunk and emit-prologue.  A few classes and accessors are also
// exported, mostly for the benefit of the heap builder.
//
//   emit-tlf-gunk (tlf :: <top-level-form>, file :: <file-state>) => ();
//      Basically, this converts Dylan "declarations" (definitions) into C
//      declarations.  Actually, it writes arbitrary information about a given
//      top-level-form.  This may be just a comment, or it may be a set of
//      concrete declarations.  "Emit-tlf-gunk" also sometimes produces
//      side-effects upon the current <file-state> -- i.e. adding a new "root".
//
//   emit-component (component :: <fer-component>, file :: <file-state>) => ();
//      Deal with translating executable Dylan into C procedures.  Many of the
//      functions compiled are compiler generated functions such as "entry
//      points", "makers", etc.
//

// Notes about internal interfaces:
//
// Types:
//   <unit-state>
//      Encapsulates state for object-code generation.  Covers all
//      object files for a single compilation unit.  Keywords include
//      "prefix:".  Operations include "unit-prefix", "new-c-global", and
//      "new-root".
//   <file-state>
//      Encapsulates per-object-file state.  Keywords include "unit:".
//      Operations include "file-unit".  It is also passed as a
//      mutable parameter to most "emit-" functions.
//   <backend-var-info>
//      Encapsulates info about either variables or <definition>s.
//      Holds a representation and an optional name.
//
// Functions:
//   Emit- functions
//      Emit functions write some sort of information corresponding to the
//      given object to the "current object file" (derived from the given
//      <file-state>").  As noted above, emit-tlf-gunk and emit-component are
//      exported.  All others are internal and may have obscure side effects.
//   make-indenting-stream-string(#rest keys)
//      Wrapper function for "make(<buffered-byte-string-output-stream>)"
//   get-string(stream :: <indenting-stream>)
//      Equivalent to "stream-contents".  Only works on
//      streams which wrap <string-stream>s.
//
//   emit-prototype-for(name :: <byte-string>, info :: <object>,
//                      file :: <file-state>)  => ();
//      Writes an "extern" declaration which will provide the C
//      compiler with enough information to use an object defined in a
//      different file.  "Info" may be as vague as #"generic" or may
//      be a specific object or declaration.
//   emit-tlf-gunk(tlf :: <top-level-form>, file :: <file-state>) => ();
//	This is generic, and there are many methods defined (for just about
//      every kind of top-level form.)
//   emit-copy(target :: <string>, target-rep :: <c-representation>,
//             source :: <string>, source-rep :: <c-representation>,
//             file :: <file-state>) => ();
//      Writes out whatever code is necessary to copy "target"s data
//      into "source".
//   c-name-and-rep(leaf :: <abstract-variable>, file :: <file-state>)
//     => (name :: <string>, rep :: <c-representation>);
//      Looks up the "info" for the given variable and returns a legal
//      C variable name and the "representation" of the variable.
//   get-info-for(thing :: <annotatable>, file :: false-or(<file-state>));
//      Retrieves the back-end specific info corresponding to "thing".
//      The type of the result depends entirely upon the type of
//      "thing", but its name will most likely end in "-info>".
//   make-info-for(thing :: <annotatable>, file :: false-or(<file-state>));
//      Actually computes the back-end specific info for an object.
//      This is an arbitrary computation and the type of the result is
//      entirely dependent upon the type of "thing".
//
//========================================================================

// This variable may be set to #t to cause function objects to be
// emitted in the roots vector for *all* functions.  This is useful
// for debugging purposes.
//
define variable *emit-all-function-objects?* = #f;


// <indenting-stream> convenience functions
//========================================================================

define constant $indentation-step = 4;

define constant make-indenting-string-stream
  = method (#rest keys)
	=> res :: <indenting-stream>;
      apply(make, <indenting-stream>,
	    inner-stream: make(<buffered-byte-string-output-stream>),
	    keys);
    end;

define method get-string (stream :: <indenting-stream>)
    => res :: <byte-string>;
  stream.inner-stream.stream-contents;
end;


// Output file state

// <unit-state>  --  Exported
//
// Various state related to a compilation unit.  Most slots are exported &
// shared with the heap builder and main.
//
define class <unit-state> (<object>)
  //
  // String prefix for this unit.
  /* exported */ slot unit-prefix :: <byte-string>,
    required-init-keyword: prefix:;
  //
  // keeps track of names used already.
  slot unit-global-table :: <table>,
    init-function: method () make(<string-table>) end method;
  //
  // Vector of the initial values for the roots vector.
  /* exported */ slot unit-init-roots :: <stretchy-vector>,
    init-function: curry(make, <stretchy-vector>);
  //
  // Vector of the ctvs we want to force into the local heap irrespective of
  // whether or not they are actually referenced.  We do this for things we
  // are optimistic about being referenced someplace but don't want to have
  // to wait until the global heap to dump.
  /* exported */ slot unit-eagerly-reference :: <stretchy-vector>,
    init-function: curry(make, <stretchy-vector>);
end;

// <root>  --  Exported
//
//  Used as an interface to the heap builder describing each root of the heap.
//
define class <root> (<object>)
  //
  // The name for this root, or #f if it will be accessed by index.
  /* exported */ slot root-name :: false-or(<byte-string>),
    init-value: #f, init-keyword: name:;
  //
  // The initial value for this root.
  /* exported */ slot root-init-value :: false-or(<ct-value>),
    init-value: #f, init-keyword: init-value:;
  //
  // Some comment about what this root entry is used for.
  /* exported */ slot root-comment :: false-or(<byte-string>),
    init-value: #f, init-keyword: comment:;
end class <root>;

define class <file-state> (<object>)
  //
  // The unit info for this output info.
  slot file-unit :: <unit-state>,
    required-init-keyword: unit:;
  //
  // Files we have already included.
  slot file-includes-exist-for :: <string-table>,
    init-function: curry(make, <string-table>);
  //
  // Things we have already spewed defns for.
  slot file-prototypes-exist-for :: <string-table>,
    init-function: curry(make, <string-table>);
  //
  // Maps from vectors of C type name strings to the name of an already-defined
  // structure type which can be used to return those multiple values.
  slot file-result-structures :: <equal-table>,
    init-function: curry(make, <equal-table>);
  //
  // Used to uniquely name the structure types that we generate.
  slot file-next-mv-result-struct :: <integer>, init-value: 0;
  //
  // The actual underlying output stream where all the output goes eventually.
  slot file-body-stream :: <stream>,
    required-init-keyword: body-stream:;
  //
  // These two streams seperately collect the local variable declarations and
  // the function body so that we can emit variable declarations on the fly
  // during code generation.
  slot file-vars-stream :: <stream>,
    init-function: curry(make-indenting-string-stream,
			 indentation: $indentation-step);
  slot file-guts-stream :: <stream>,
    init-function: curry(make-indenting-string-stream,
			 indentation: $indentation-step);
  //
  // Whenever the guts stream buffer exceeds 64K, we push the contents here and
  // empty the stream.  In addition to being more efficient, this avoids object
  // size limitations in Mindy.
  slot file-guts-overflow :: <stretchy-vector>,
    init-function: curry(make, <stretchy-vector>, size: 0);
  //
  // Chain of <pending-define>s.
  slot file-pending-defines :: false-or(<pending-define>),
    init-value: #f;
  //
  // id number for the next block.  This is used as a C label marking the end
  // of the block.  Zeroed at the start of each function.
  slot file-next-block :: <integer>, init-value: 0;
  //
  // keeps track of names used already.  This is cleared whenever we start
  // emitting a new function.
  slot file-local-table :: <table>,
    init-function: method () make(<string-table>) end method;
  //
  // we keep track of the components for all the xeps that we lazily generated
  // and dump them after the referencing component has compiled, since we are
  // in the middle of generating code when we discover that we need them.
  slot file-deferred-xeps :: <sequence> = make(<deque>);
end;




// Utilities.
//
// The "Maybe-" functions all check to see whether the
// named operations have already been performed (for this file), and
// do them if they have not.  Thus, after the call, you may depend
// upon the operation having been performed at some time.
//========================================================================

// Include a given ".h" file, which is expected to be in your include
// search path (as specified in CPATH or via -I switches.
//
define method maybe-emit-include
    (name :: <byte-string>, file :: <file-state>)
  unless (element(file.file-includes-exist-for, name, default: #f))
    format(file.file-body-stream, "#include <%s>\n\n", name);
    element(file.file-includes-exist-for, name) := #t;
  end;
end;


// Returns a "<...-info>" object for the given "thing".  Since there is no
// consistent structure to "<...-info>" objects, it is difficult to
// know precisely what you will be getting.  The unifying concept is
// that the returned object will be back-end specific data describing
// "thing". 
//
define method get-info-for
    (thing :: <annotatable>, file :: false-or(<file-state>))
    => res :: <object>;
  thing.info | (thing.info := make-info-for(thing, file));
end;


// C character set translation:

// Lots of places we emit ``useful'' information inside comments.  But if the
// useful information contains a ``*/'' it will confuse the C compiler.  So
// we use this routine to clobber all occurances of */ before actually writing
// the comment.
// 
define method clean-for-comment
    (string :: <byte-string>, #key copy? :: <boolean> = #t)
    => res :: <byte-string>;
  let len = string.size;
  unless (len < 2)
    let previous-is-star? = string[0] == '*';
    for (index :: <integer> from 1 below len)
      let char = string[index];
      if (char == '*')
	previous-is-star? := #t;
      elseif (previous-is-star?)
	previous-is-star? := #f;
	if (char == '/')
	  if (copy?)
	    string := copy-sequence(string);
	    copy? := #f;
	  end if;
	  string[index - 1] := 'X';
	  string[index] := 'X';
	end if;
      end if;
    end for;
  end unless;
  string;
end method clean-for-comment;
//
define method clean-for-comment (thing :: <object>, #key)
    => res :: <byte-string>;
  clean-for-comment(format-to-string("%s", thing), copy?: #f);
end method clean-for-comment;


// $c-name-transform is a vector which maps from the Dylan to C character
// set.  If the value is #f, there is no mapping (the character is illegal in
// all Dylan names, including operator names.)  This is an information-
// preserving, invertible transformation.
//
define constant $c-name-transform :: <vector>
  = begin
      let map = make(<simple-object-vector>, size: 256, fill: #f);
      for (i from 0 below 256)
        map[i] := format-to-string("X%x", i);
      end for;
      local
	method fill-range
	    (start :: <character>, stop :: <character>, xform :: <function>)
	    => ();
	  for (i from as(<integer>, start) to as(<integer>, stop))
	    map[i] := make(<byte-string>, size: 1,
	    		   fill: xform(as(<character>, i)));
	  end for;
	end method fill-range;
      map[as(<integer>, ' ')] := "BLANK";
      map[as(<integer>, '!')] := "D";
      map[as(<integer>, '$')] := "C";
      map[as(<integer>, '%')] := "PCT";
      map[as(<integer>, '&')] := "AND";
      map[as(<integer>, '*')] := "V";
      map[as(<integer>, '+')] := "PLUS";
      map[as(<integer>, '-')] := "_";
      map[as(<integer>, '/')] := "SLASH";
      fill-range('0', '9', identity);
      map[as(<integer>, '<')] := "LESS";
      map[as(<integer>, '=')] := "EQUAL";
      map[as(<integer>, '>')] := "GREATER";
      map[as(<integer>, '?')] := "QUERY";
      fill-range('A', 'Z', as-lowercase);
      map[as(<integer>, '^')] := "RAISE";
      map[as(<integer>, '_')] := "X_";
      fill-range('a', 'z', identity);
      map[as(<integer>, '|')] := "OR";
      map[as(<integer>, '~')] := "NOT";
      map;
    end;


// Map a string of legal dylan identifier characters into legal C name
// characters using the $c-name-transform.
//
define function string-to-c-name (str :: <byte-string>) 
 => res :: <byte-string>;
  let res = make(<byte-string>, size: str.size);
  let res-idx = 0;
  for (ch in str)
    let mapping = $c-name-transform[as(<integer>, ch)];
    if (mapping.size == 1)
      res[res-idx] := mapping[0];
      res-idx := res-idx + 1;
    else
      res := concatenate(copy-sequence(res, end: res-idx), mapping,
      			 copy-sequence(res, start: res-idx + 1));
      res-idx := res-idx + mapping.size;
    end if;
  end for;
  res;
end function;


// Based upon a <name> object, compute a string suitable for use as a C
// variable name,
//
define generic c-name (name :: <name>) => (result :: <byte-string>);

define method c-name (name :: <basic-name>) => (result :: <byte-string>);
  let mod-name = string-to-c-name(as(<string>, name.name-module.module-name));
  let def-name = as(<string>, name.name-symbol);

  // deal with <class> convention.
  let lastidx = def-name.size - 1;
  if (def-name[0] == '<' & def-name[lastidx] == '>')
    concatenate
      (mod-name, "ZCLS_",
       string-to-c-name(copy-sequence(def-name, start: 1, end: lastidx)));
  else
    concatenate(mod-name, "Z", string-to-c-name(def-name));
  end if;
end method c-name;


define method c-name (name :: <method-name>) => (result :: <byte-string>);
  concatenate(name.method-name-generic-function.c-name, "_METH");
end method c-name;

define method c-name (name :: <derived-name>) => (result :: <byte-string>);
  concatenate(name.derived-name-base.c-name,
	      select (name.derived-name-how)
		#"general-entry" => "_GENERAL";
		#"generic-entry" => "_GENERIC";
		#"discriminator" => "_DISCRIM";
		#"deferred-evaluation" => "_DEFER";
		#"init-function" => "_INIT";
		#"setter" => "_SETTER";
		#"getter" => "_GETTER";
		#"type-cell" => "_TYPE";
		#"maker" => "_MAKER";
	      end select);
end method;

define method c-name (name :: <internal-name>) => res :: <byte-string>;
  concatenate(name.internal-name-base.c-name, "_INT_",
  	      string-to-c-name(as(<string>, name.internal-name-symbol)));
end method;

// really shouldn't be any unknown source locations here, but we'll get to that
// later... 
define method c-name (name :: <anonymous-name>) => res :: <byte-string>;
  let loc = name.anonymous-name-location;
  if (instance?(loc, <file-source-location>))
    format-to-string("LINE_%d", loc.start-line);
  else
    "UNKNOWN";
  end;
end method;


// New-{scope}
//
// The new-.... routines all allocate new identifiers which are
// guranteed to be unique in the given scope.  The "name" and
// "modifier" keywords may be used to provide more meaningful names,
// but their usage is idiosyncratic.
//========================================================================

define method new-local
    (file :: <file-state>,
     #key name :: <string> = "L_", modifier :: <string> = "anon")
 => res :: <string>;
  let result = stringify(name, modifier);
  let num = element(file.file-local-table, result, default: 0) + 1;
  file.file-local-table[result] := num;
  if (num == 1)
    result;
  else
    new-local(file, name: result, modifier: stringify('_', num));
  end if;
end;

// If the name is unique? then no _number is used, and the 
// name must be unique without any such suffix.  Name can be #f if there is no
// <name> object, but you then must supply a modifier.
//
define method new-c-global
    (name :: false-or(<name>), file :: <file-state>, 
     #key modifier :: <string> = "")
 => res :: <string>;
  let unit = file.file-unit;
  let da-name = if (name) name.c-name else "" end;
  let result = stringify(unit.unit-prefix, 'Z', da-name, modifier);
  let num = element(unit.unit-global-table, result, default: 0) + 1;
  unit.unit-global-table[result] := num;
  if (num == 1)
    result;
  else
//    if (name & name.name-unique?)
//      error("%S should have been unique, but it wasn't.", result);
//    end if;
    new-c-global(name, file, modifier: stringify(modifier, '_', num));
  end if;
end method new-c-global;


// Add a new root description and return the root index.
//
define method new-root
    (init-value :: false-or(<ct-value>), name :: <byte-string>,
     file :: <file-state>, #key comment :: false-or(<byte-string>))
 => res :: <integer>;
  let unit = file.file-unit;
  let roots = unit.unit-init-roots;
  let index = roots.size;
  let root = make(<root>, init-value: init-value, name: name,
		  comment: comment);
  roots[index] := root;
  index;
end;


define method eagerly-reference (ctv :: <ct-value>, file :: <file-state>)
    => ();
  add!(file.file-unit.unit-eagerly-reference, ctv);
end method eagerly-reference;



// "Cluster" operations
//
// Produce pairs of names ("bottom" and "top") for "clusters"
// associated with certain definitions.  These clusters typically
// correspond to #rest arguments or #rest return values.
//========================================================================

define method cluster-names (depth :: <integer>)
    => (bottom-name :: <string>, top-name :: <string>);
  if (zero?(depth))
    values("orig_sp", "cluster_0_top");
  else
    values(stringify("cluster_", depth - 1, "_top"),
	   stringify("cluster_", depth, "_top"));
  end;
end;

define method consume-cluster
    (cluster :: <abstract-variable>, file :: <file-state>)
    => (bottom-name :: <string>, top-name :: <string>);
  cluster-names(cluster.info);
end;

define method produce-cluster
    (cluster :: <abstract-variable>, file :: <file-state>)
    => (bottom-name :: <string>, top-name :: <string>);
  cluster-names(cluster.info);
end;

define method produce-cluster
    (cluster :: <initial-definition>, file :: <file-state>)
    => (bottom-name :: <string>, top-name :: <string>);
  produce-cluster(cluster.definition-of, file);
end;


// Emitting prototypes

// Write out a C prototye for an object with the given name.  More
// information about the object will be provided by the "info"
// parameter.  The exact behavior depends upon "info"s type.
//
define method maybe-emit-prototype
    (name :: <byte-string>, info :: <object>, file :: <file-state>)
    => ();
  unless (element(file.file-prototypes-exist-for, name,
		  default: #f))
    emit-prototype-for(name, info, file);
    file.file-prototypes-exist-for[name] := #t;
  end;
end;

// Emit-prototype-for
//   Writes an "extern" declaration which will provide the C compiler with
//   enough information to use an object defined in a different file.
//
define generic emit-prototype-for
    (name :: <byte-string>, info :: <object>, file :: <file-state>);



// We need prototypes for things with "immediate" representations or
// for associated intialization variables.  Handling of specific
// prototypes is simply handed off to "format" (i.e. "print-message").
// "Name" is ignored.
//
define method emit-prototype-for
    (name :: <byte-string>, defn :: <definition>, file :: <file-state>)
    => ();
  let info = get-info-for(defn, file);
  let stream = file.file-body-stream;
  let rep = info.backend-var-info-rep;
  if (instance?(rep, <immediate-representation>))
    format(stream, "extern %s %s;\t/* %s */\n\n",
	   rep.representation-c-type,
	   info.backend-var-info-name,
	   defn.defn-name.clean-for-comment);
  else
    // We must have a user meaningful-name, so make sure it's
    // declared.
    format(stream, "extern descriptor_t %s;\t/* %s */\n\n",
	   info.backend-var-info-name, defn.defn-name.clean-for-comment);
  end if;
  unless (rep.representation-has-bottom-value?
	    | defn.defn-guaranteed-initialized?)
    format(stream, "extern boolean %s_initialized;\n\n",
	   info.backend-var-info-name);
  end;
end;  

// We don't know what sort of object we're working with, but we can
// presume that the given name is meaningful.  This general case must
// be overridden for some classes -- especially functions.
//
define method emit-prototype-for
    (name :: <byte-string>, defn :: <object>, file :: <file-state>)
    => ();
  let stream = file.file-body-stream;
  format(stream, "extern descriptor_t %s;\t/* %s */\n\n",
	 name, defn.clean-for-comment);
end;  

define method emit-prototype-for
    (name :: <byte-string>, defn :: <class-definition>, file :: <file-state>)
    => ();
  let stream = file.file-body-stream;
  format(stream, "extern descriptor_t %s;\t/* %s */\n\n",
	 name, defn.defn-name.clean-for-comment);
end;  


// Definitions and variables


define method defn-guaranteed-initialized? (defn :: <definition>)
    => res :: <boolean>;
  defn.ct-value ~== #f;
end method defn-guaranteed-initialized?;

define method defn-guaranteed-initialized? (defn :: <variable-definition>)
  defn.defn-init-value ~== #f;
end method defn-guaranteed-initialized?;



// Encapsulates the back-end specific info for a <variable> or <definition>.
//
define class <backend-var-info> (<object>)
  slot backend-var-info-rep :: <c-representation>,
    required-init-keyword: representation:;
  slot backend-var-info-name :: false-or(<string>),
    required-init-keyword: name:;
end;

add-make-dumper(#"backend-var-info", *compiler-dispatcher*, <backend-var-info>,
		list(backend-var-info-rep, representation:, #f,
		       backend-var-info-name, name:, #f));

// Local variables are simple things.  Just pick a reasonable
// representation, and create an anonymous <backend-var-info>
//
define method make-info-for
    (var :: type-union(<initial-variable>, <ssa-variable>),
     // ### Should really only be ssa-variable.
     file :: <file-state>)
 => res :: <backend-var-info>;
  let rep = pick-representation(var.derived-type, #"speed");
  make(<backend-var-info>, representation: rep, name: #f);
end;

// A global variable is created for each definition.  The type of
// variable depends upon the representation we choose.  It may end up
// as a simple C variable containing a primitive type, or it may be
// allocated upon the initial heap.  In either case, the variable is
// global, so we must generate a name and store it away for future
// use.
//
define method make-info-for (defn :: <definition>, file :: <file-state>)
    => res :: <backend-var-info>;
  let type = defn.defn-type;
  let rep = if (type)
	      pick-representation(type, #"speed");
	    else
	      *general-rep*;
	    end;
  let da-c-name = new-c-global(defn.defn-name, file);
  if (instance?(rep, <immediate-representation>))
    make(<backend-var-info>, representation: rep, name: da-c-name);
  else
    new-root(if (instance?(defn, <variable-definition>))
	       defn.defn-init-value;
	     else
	       defn.ct-value;
	     end,
	     da-c-name, 
	     file,
	     comment: format-to-string("%s", defn.defn-name));
    make(<backend-var-info>, representation: *general-rep*, name: da-c-name);
  end;
end;


// We never make an "-info" record for <initial-definition>s.  We just
// defer to the actual definition object.
//
define method get-info-for (leaf :: <initial-definition>,
			    file :: <file-state>)
    => res :: <backend-var-info>;
  get-info-for(leaf.definition-of, file);
end;


// Returns a legal C variable name and the "representation" of the
// variable.
//
// If it's a global variable, we just look up the stored
// values.  However, we must now stop and create names (and local
// definitions) for local variables which (for some reason) do not get
// named by "make-info-for".
//
define method c-name-and-rep (leaf :: <abstract-variable>,
			      // ### Should really be ssa-variable
			      file :: <file-state>)
    => (name :: <string>, rep :: <c-representation>);
  let info = get-info-for(leaf, file);
  let name = info.backend-var-info-name;
  unless (name)
    if (instance?(leaf.var-info, <debug-named-info>))
      let dname = string-to-c-name(as(<string>, leaf.var-info.debug-name));
      name := new-local(file, modifier: dname);
    else
      name := new-local(file);
    end if;
    let stream = file.file-vars-stream;
    format(stream, "%s %s;",
	   info.backend-var-info-rep.representation-c-type, name);
    if (instance?(leaf.var-info, <debug-named-info>))
      format(stream, " /* %s */", leaf.var-info.debug-name.clean-for-comment);
    end;
    write(stream, "\n");
    info.backend-var-info-name := name;
  end;
  values(name, info.backend-var-info-rep);
end;


// Shortcut method for retrieving the (previously computed) representation 
// for a variable.
//
define method variable-representation (leaf :: <abstract-variable>,
				       // ### Should really be ssa-variable
				       file :: <file-state>)
    => rep :: <c-representation>;
  get-info-for(leaf, file).backend-var-info-rep;
end;


// function region stuff.

// Stores back-end-specific information about a function.
// <function-info>s may correspond to either <fer-function-region>s or
// <ct-function>s.
//
define class <function-info> (<object>)
  //
  // The name of the function-region this is the function info for.
  slot function-info-name :: <name>,
    required-init-keyword: name:;
  //
  // The C name of the function corresponding to this function-region.
  slot function-info-main-entry-c-name :: false-or(<byte-string>),
    init-value: #f, init-keyword: main-entry-c-name:;
  //
  // Sequence of the representations for the arguments.
  slot function-info-argument-representations :: <simple-object-vector>,
    required-init-keyword: argument-reps:;
  //
  // Representation of the result.  If a <c-representation>, then that single
  // value is returned.  If a sequence, then a structure of those values are
  // returned.  If #"doesn't-return" then it doesn't return.  If #"cluster",
  // it returns a cluster of values.
  slot function-info-result-representation
    :: type-union(<c-representation>, <sequence>,
	       one-of(#"doesn't-return", #"cluster")),
    required-init-keyword: result-rep:;
  //
  // The result type.
  slot function-info-result-type :: <values-ctype>,
    required-init-keyword: result-type:;
end;

define constant $function-info-slots
  = list(function-info-name, name:, #f,
	 function-info-main-entry-c-name, main-entry-c-name:, #f,
	 function-info-argument-representations, argument-reps:, #f,
	 function-info-result-representation, result-rep:, #f,
	 function-info-result-type, result-type:, #f);

// If the function name is unique, then we suffix with _FUN so that a constant
// method definition can a value distinct from the underlying main entry.
//
define method main-entry-c-name
    (info :: <function-info>, file :: <file-state>)
    => res :: <byte-string>;
  let name-obj = info.function-info-name;
  info.function-info-main-entry-c-name
    | (info.function-info-main-entry-c-name
	 := new-c-global(name-obj, file,
	 		 modifier: if (name-unique?(name-obj))
			 	     "_FUN";
				   else
				     "";
				   end));
end;

define method make-function-info
    (class :: <class>, name :: <name>, signature :: <signature>,
     closure-var-types :: <sequence>)
    => res :: <function-info>;
  let argument-reps
    = begin
	let reps = make(<stretchy-vector>);
	for (type in closure-var-types)
	  add!(reps, pick-representation(type, #"speed"));
	end;
	for (type in signature.specializers)
	  add!(reps, pick-representation(type, #"speed"));
	end;
	if (signature.next?)
	  add!(reps,
	       pick-representation(specifier-type(#"<list>"), #"speed"));
	end;
	if (signature.rest-type | (signature.next? & signature.key-infos))
	  add!(reps,
	       pick-representation(specifier-type(#"<simple-object-vector>"),
				   #"speed"));
	end;
	if (signature.key-infos)
	  for (key-info in signature.key-infos)
	    add!(reps, pick-representation(key-info.key-type, #"speed"));
	    if (key-info.key-needs-supplied?-var)
	      add!(reps,
		   pick-representation(specifier-type(#"<boolean>"),
				       #"speed"));
	    end;
	  end;
	end;
	as(<simple-object-vector>, reps);
      end;

  let result-type = signature.returns;
  let result-rep
    = if (result-type == empty-ctype())
	#"doesn't-return";
      else
	let min-values = result-type.min-values;
	let positionals = result-type.positional-types;
	let rest-type = result-type.rest-value-type;
	if (min-values == positionals.size & rest-type == empty-ctype())
	  if (min-values == 1)
	    pick-representation(result-type, #"speed");
	  else
	    map(rcurry(pick-representation, #"speed"), positionals);
	  end;
	else
	  #"cluster";
	end;
      end;

  make(class,
       name: name,
       argument-reps: argument-reps,
       result-type: result-type,
       result-rep: result-rep);
end;


define method make-info-for
    (function :: <fer-function-region>, file :: <file-state>)
    => res :: <function-info>;
  make-function-info(<function-info>, function.name,
		     make(<signature>, specializers: function.argument-types,
			  returns: function.result-type),
		     #[]);
end;

// entry-point-c-name  --  Exported
//
// Used by the heap builder to get its hands on a raw pointer to an entry point.
// The function must have already been named, since its definition must have
// been emitted by the time the heap builder runs.
//
define method entry-point-c-name (entry :: <ct-entry-point>)
    => res :: <string>;
  let info = entry.ct-entry-point-for.info;
  unless (info)
    error("Too late to be making an info for %=", entry.ct-entry-point-for);
  end unless;
  let name
    = select (entry.ct-entry-point-kind)
	#"main" => info.function-info-main-entry-c-name;
	#"general" => info.function-info-general-entry-c-name;
	#"generic" => info.function-info-generic-entry-c-name;
      end select;
  unless (name)
    error("Too late to be picking a name for %=", entry);
  end unless;
  name;
end method;


// maybe-emit-entries
//
//  We lazily build entry points (and generate code) so that we can avoid
// compiling entry points that aren't actually used.
//

// This function is used for cases in which we might have references
// to constant functions and don't know how they might be called.  It
// will create all the entries which might be required.
//
define generic maybe-emit-entries
    (ctv :: <object>, file :: <file-state>) => ();


define method maybe-emit-generic-entry
    (ctv :: <ct-method>, file :: <file-state>) => (name :: <string>);
  let info = get-info-for(ctv, file);
  let name = generic-entry-c-name(info, file);
  if (~ctv.has-generic-entry?)
    let (entry, component) = build-xep-component(ctv, #t);
    let entry-info = get-info-for(entry, file);
    // We've already allocated a meaningful name for this entry, so
    // we want copy it into the entry's info.
    entry-info.function-info-main-entry-c-name := name;
    ctv.has-generic-entry? := #t;
    push-last(file.file-deferred-xeps, component);
  end if;
  name;
end method maybe-emit-generic-entry;

define method maybe-emit-general-entry
    (ctv :: <ct-function>, file :: <file-state>) => (name :: <string>);
  let info = get-info-for(ctv, file);
  let name = general-entry-c-name(info, file);
  if (~ctv.has-general-entry?)
    let (entry, component) = build-xep-component(ctv, #f);
    let entry-info = get-info-for(entry, file);
    // We've already allocated a meaningful name for this entry, so
    // we want copy it into the entry's info.
    entry-info.function-info-main-entry-c-name := name;
    ctv.has-general-entry? := #t;
    push-last(file.file-deferred-xeps, component);
  end if;
  name;
end method maybe-emit-general-entry;


define method maybe-emit-entries
    (ctv :: <object>, file :: <file-state>) => ();
  #f;
end method maybe-emit-entries;

define method maybe-emit-entries
    (ctv :: <ct-function>, file :: <file-state>) => ();
  maybe-emit-general-entry(ctv, file);
end method maybe-emit-entries;

define method maybe-emit-entries
    (ctv :: <ct-generic-function>, file :: <file-state>) => ();
  let defn = ctv.ct-function-definition;
  // Open generics will already have all relevant entries.
  if (defn & defn.generic-defn-sealed?)
    let disc = defn.generic-defn-discriminator;
    if (disc)
      maybe-emit-general-entry(disc, file);
    else
      map(method (m) maybe-emit-entries(m.ct-value, file) end,
	  defn.generic-defn-methods);
    end if;
  end if;
end method maybe-emit-entries;

define method maybe-emit-entries
    (ctv :: <ct-method>, file :: <file-state>) => ();
  maybe-emit-generic-entry(ctv, file);
  // A method is hidden if it can only be called via its associated GF.  A
  // general entry is only needed if the function could somehow become visible
  // as a "bare" method.
  if (~ctv.ct-method-hidden?)
    maybe-emit-general-entry(ctv, file);
  end if;
end method maybe-emit-entries;


// Constant and constant-function info

define class <constant-info> (<object>)
  //
  // The C ``expression'' for referencing this constant.
  slot const-info-expr :: false-or(<byte-string>),
    init-value: #f, init-keyword: expr:;
  //
  // Flag used by the heap dumper to indicate when an object has been queued
  // for dumping.
  slot const-info-dumped? :: <boolean>, init-value: #f, init-keyword: dumped:;
  //
  // Set of heap labels for this object.  Maintained and used by the
  // heap dumper.
  slot const-info-heap-labels :: <simple-object-vector>,
    init-value: #[], init-keyword: labels:;
end;

define constant $constant-info-slots
  = list(const-info-expr, expr:, #f,
	 const-info-dumped?, dumped:, #f,
	 const-info-heap-labels, labels:, #f);

add-make-dumper(#"constant-info", *compiler-dispatcher*, <constant-info>,
		$constant-info-slots);


// merge-ctv-infos -- method on imported GF.
// 
define method merge-ctv-infos
    (old-info :: <constant-info>, new-info :: <constant-info>)
    => ();
  if (old-info.const-info-dumped?)
    unless (new-info.const-info-dumped?
	      | new-info.const-info-heap-labels.empty?)
      error("Merging infos would drop some labels.");
    end unless;
  else
    if (new-info.const-info-dumped?)
      old-info.const-info-dumped? := #t;
      unless (old-info.const-info-heap-labels.empty?)
	error("Merging infos would drop some labels.");
      end unless;
    end if;
  end if;
  old-info.const-info-heap-labels
    := union(old-info.const-info-heap-labels, new-info.const-info-heap-labels,
	     test: method (str1 :: <byte-string>, str2 :: <byte-string>)
		     str1 = str2;
		   end method)
end method merge-ctv-infos;


define method make-info-for (ctv :: <ct-value>, file :: false-or(<file-state>))
    => res :: <constant-info>;
  make(<constant-info>);
end;


define class <constant-function-info> (<constant-info>, <function-info>)
  slot function-info-general-entry-c-name :: false-or(<byte-string>),
    init-value: #f, init-keyword: general-entry-c-name:;
end;

define constant $constant-function-info-slots
  = concatenate($constant-info-slots,
		$function-info-slots,
		list(function-info-general-entry-c-name, general-entry-c-name:,
		     #f));

add-make-dumper(#"constant-function-info", *compiler-dispatcher*,
		<constant-function-info>, $constant-function-info-slots);

define method general-entry-c-name
    (info :: <constant-function-info>, file :: <file-state>)
    => res :: <byte-string>;
  info.function-info-general-entry-c-name
    | (info.function-info-general-entry-c-name
	 := new-c-global(info.function-info-name, file,
	 		 modifier: "_GENERAL"));
end;

define method make-info-for
    (ctv :: <ct-function>, file :: false-or(<file-state>))
    => res :: <constant-function-info>;
  make-function-info(<constant-function-info>, ctv.ct-function-name,
		     ctv.ct-function-signature,
		     ctv.ct-function-closure-var-types);
end;

define class <constant-method-info> (<constant-function-info>)
  slot function-info-generic-entry-c-name :: false-or(<byte-string>),
    init-value: #f, init-keyword: generic-entry-c-name:;
end;

define constant $constant-method-info-slots
  = concatenate($constant-function-info-slots,
		list(function-info-generic-entry-c-name, generic-entry-c-name:,
		     #f));

add-make-dumper(#"constant-method-info", *compiler-dispatcher*,
		<constant-method-info>, $constant-method-info-slots);

define method generic-entry-c-name
    (info :: <constant-method-info>, file :: <file-state>)
    => res :: <byte-string>;
  info.function-info-generic-entry-c-name
    | (info.function-info-generic-entry-c-name
	 := new-c-global(info.function-info-name, file, modifier: "_GENERIC"));
end;

define method make-info-for
    (ctv :: <ct-method>, file :: false-or(<file-state>))
    => res :: <constant-function-info>;
  make-function-info(<constant-method-info>, ctv.ct-function-name,
		     ctv.ct-function-signature,
		     ctv.ct-function-closure-var-types);
end;


// File prologue and epilogue

define method emit-prologue
    (file :: <file-state>, other-units :: <simple-object-vector>)
    => ();
  maybe-emit-include("stdlib.h", file);
  maybe-emit-include("stdio.h", file);
  maybe-emit-include("runtime.h", file);
 
   // The most important thing math.h includes is a prototype for rint,
   // although it helps if we ever want to inline functions in the
   // Transcendental library
   maybe-emit-include("math.h", file);

  let stream = file.file-body-stream;
  for (unit in other-units)
    format(stream, "extern descriptor_t %s_roots[];\n", unit);
  end;
  format(stream, "extern descriptor_t %s_roots[];\n\n",
	 file.file-unit.unit-prefix);
  format(stream, "#define obj_True %s.heapptr\n",
	 c-expr-and-rep(as(<ct-value>, #t), *general-rep*, file));
  format(stream, "#define obj_False %s.heapptr\n\n",
	 c-expr-and-rep(as(<ct-value>, #f), *general-rep*, file));
  format(stream, "#define GENERAL_ENTRY(func) \\\n");
  format(stream, "    ((entry_t)SLOT(func, void *, %d))\n",
	 dylan-slot-offset(function-ctype(), #"general-entry"));
  format(stream, "#define GENERIC_ENTRY(func) \\\n");
  format(stream, "    ((entry_t)SLOT(func, void *, %d))\n\n",
	 dylan-slot-offset(specifier-type(#"<method>"), #"generic-entry"));
end;

define method dylan-slot-offset (cclass :: <cclass>, slot-name :: <symbol>)
  block (return)
    for (slot in cclass.all-slot-infos)
      if (slot.slot-getter & slot.slot-getter.variable-name == slot-name)
	return(find-slot-offset(slot, cclass)
		 | error("%s isn't at a constant offset in %=",
			 slot-name, cclass));
      end;
    end;
    error("%= doesn't have a slot named %s", cclass, slot-name);
  end;
end;


// emit-tlf-gunk  --  exported
//
// Top level form processing

// Emit-tlf-gunk
//   Writes arbitrary information about a given top-level-form.  This
//   may be just a comment, or it may be a set of concrete
//   declarations.  "Emit-tlf-gunk" also sometimes produces
//   side-effects upon the current <file-state> -- i.e. adding a new
//   "root".
//
define generic emit-tlf-gunk (tlf :: <top-level-form>, file :: <file-state>)
    => ();

define method emit-tlf-gunk (tlf :: <top-level-form>, file :: <file-state>)
    => ();
  format(file.file-body-stream, "\n/* %s */\n\n", tlf.clean-for-comment);
end;

define method emit-tlf-gunk (tlf :: <magic-interal-primitives-placeholder>,
			     file :: <file-state>)
    => ();
  let bstream = file.file-body-stream;
  format(bstream, "\n/* %s */\n\n", tlf.clean-for-comment);

  let gstream = file.file-guts-stream;

  format(bstream, "descriptor_t *pad_cluster(descriptor_t *start, "
	   "descriptor_t *end,\n");
  format(bstream, "                          int min_values)\n{\n");
  format(gstream, "descriptor_t *ptr = start + min_values;\n\n");
  format(gstream, "while (end < ptr)\n");
  format(gstream, "    *end++ = %s;\n",
	 c-expr-and-rep(as(<ct-value>, #f), *general-rep*, file));
  format(gstream, "return end;\n");
  write(bstream, get-string(gstream));
  write(bstream, "}\n\n");

  format(bstream,
	 "descriptor_t *values_sequence"
	   "(descriptor_t *sp, heapptr_t vector)\n{\n");
  let sov-cclass = specifier-type(#"<simple-object-vector>");
  format(gstream, "long elements = SLOT(vector, long, %d);\n",
	 dylan-slot-offset(sov-cclass, #"size"));
  format(gstream, "memcpy(sp, (char *)vector + %d, elements * "
	   "sizeof(descriptor_t));\n",
	 dylan-slot-offset(sov-cclass, #"%element"));
  format(gstream, "return sp + elements;\n");
  write(bstream, get-string(gstream));
  write(bstream, "}\n\n");

  unless (instance?(*double-rep*, <c-data-word-representation>))
    let cclass = specifier-type(#"<double-float>");
    format(bstream, "heapptr_t make_double_float(double value)\n{\n");
    format(gstream, "heapptr_t res = allocate(%d);\n",
	   cclass.instance-slots-layout.layout-length);
    let (expr, rep) = c-expr-and-rep(cclass, *heap-rep*, file);
    format(gstream, "SLOT(res, heapptr_t, %d) = %s;\n",
	   dylan-slot-offset(cclass, #"%object-class"),
	   conversion-expr(*heap-rep*, expr, rep, file));
    let value-offset = dylan-slot-offset(cclass, #"value");
    format(gstream, "SLOT(res, double, %d) = value;\n", value-offset);
    format(gstream, "return res;\n");
    write(bstream, get-string(gstream));
    write(bstream, "}\n\n");

    format(bstream, "double double_float_value(heapptr_t df)\n{\n");
    format(gstream, "return SLOT(df, double, %d);\n", value-offset);
    write(bstream, get-string(gstream));
    write(bstream, "}\n\n");
  end;

  unless (instance?(*long-double-rep*, <c-data-word-representation>))
    let cclass = specifier-type(#"<extended-float>");
    format(bstream, "heapptr_t make_extended_float(long double value)\n{\n");
    format(gstream, "heapptr_t res = allocate(%d);\n",
	   cclass.instance-slots-layout.layout-length);
    let (expr, rep) = c-expr-and-rep(cclass, *heap-rep*, file);
    format(gstream, "SLOT(res, heapptr_t, %d) = %s;\n",
	   dylan-slot-offset(cclass, #"%object-class"),
	   conversion-expr(*heap-rep*, expr, rep, file));
    let value-offset = dylan-slot-offset(cclass, #"value");
    format(gstream, "SLOT(res, long double, %d) = value;\n", value-offset);
    format(gstream, "return res;\n");
    write(bstream, get-string(gstream));
    write(bstream, "}\n\n");

    format(bstream, "long double extended_float_value(heapptr_t xf)\n{\n");
    format(gstream, "return SLOT(xf, long double, %d);\n", value-offset);
    write(bstream, get-string(gstream));
    write(bstream, "}\n\n");
  end;
end;

// This method does useful stuff to insure that heap dumping does the
// right thing for generic functions.
//
define method emit-tlf-gunk (tlf :: <define-generic-tlf>, file :: <file-state>)
    => ();
  format(file.file-body-stream, "\n/* %s */\n\n", tlf.clean-for-comment);
  let defn = tlf.tlf-defn;
  emit-definition-gunk(defn, file);
  write(file.file-body-stream, "\n");

  // If dynamic, force module var to be allocated now.  Otherwise the client
  // libraries will create it on demand, referencing its init-value which is
  // not a fixed name.
  if (defn.defn-dynamic?)
    get-info-for(defn, file);
  end if;

  let ctv = defn.ct-value;
  if (ctv)
    if (*emit-all-function-objects?*)
      // It's a bit convoluted, but this seems to be the best way to
      // insure that a function object is emitted and added to the
      // roots vector.
      get-info-for(defn, file);
      maybe-emit-entries(ctv, file);
    end if;
    if (defn.generic-defn-sealed?)
      //
      // We dump sealed generic in the local heap instead of the letting them
      // all accumulate in the global heap.  This trades off a faster global
      // heap build against potentially dumping generics that are never
      // referenced, which seems to be a win.
      eagerly-reference(ctv, file);
      if (defn.generic-defn-discriminator
	    & defn.defn-name.name-inherited-or-exported?)
	maybe-emit-general-entry(defn.generic-defn-discriminator, file);
      end if;
    end if;
  end if;
end method emit-tlf-gunk;

define method check-generic-method-xep
    (defn :: <method-definition>, file :: <file-state>) => ();
  let ctv = defn.ct-value;
  let gf = defn.method-defn-of;
  case
    (~gf | ~ctv) => #t;
    (~gf.generic-defn-sealed?) =>
      maybe-emit-generic-entry(ctv, file);
      // By adding generic function methods to the heap now, we save
      // effort during the global dump phase.  There is, of course, the
      // possibility of writing heap info for methods which will not be
      // referenced.
      eagerly-reference(ctv, file);
    (gf.defn-name.name-inherited-or-exported?
       & ~gf.generic-defn-discriminator) =>
      maybe-emit-generic-entry(ctv, file);
  end case;
end method check-generic-method-xep;

define method emit-tlf-gunk
    (tlf :: <define-method-tlf>, file :: <file-state>)
    => ();
  format(file.file-body-stream, "\n/* %s */\n\n", tlf.clean-for-comment);
  check-generic-method-xep(tlf.tlf-defn, file);
end method emit-tlf-gunk;
      
define method emit-tlf-gunk (tlf :: <define-class-tlf>, file :: <file-state>)
    => ();
  format(file.file-body-stream, "\n/* %s */\n\n", tlf.clean-for-comment);
  // This class was obviously defined in this lib, so it is a local class.
  let defn = tlf.tlf-defn;
  emit-definition-gunk(defn, file);
  write(file.file-body-stream, "\n");
  let ctv = defn.ct-value;
  if (ctv)
    if (ctv.sealed?)
      //
      // By adding sealed classes to the heap now, we save effort duing the
      // global dump phase.  And this costs us nothing, because all classes
      // are referenced through the super/subclass links.
      eagerly-reference(ctv, file);
    elseif (ctv.primary?)
      //
      // We also force the slot infos for open primary classes into the local
      // heap.  We can because the position table will be static (because no
      // subclasses can allocate the slot somewhere else).
      for (slot in ctv.new-slot-infos)
	eagerly-reference(slot, file);
      end for;
    end if;
  end if;
  // Walk through various structures and assure that any needed xeps
  // are generated
  let deferred = defn.class-defn-defered-evaluations-function;
  if (deferred) maybe-emit-entries(deferred, file) end if;
  let maker = defn.class-defn-maker-function;
  if (maker) maybe-emit-entries(maker, file) end if;
  // There may be function hidden in various overrides for this class.
  // Generate xeps for all such.
  for (elem in defn.class-defn-overrides)
    let info = elem.override-defn-info;
    if (info)
      maybe-emit-entries(info.override-init-value, file);
      maybe-emit-entries(info.override-init-function, file);
    end if;
  end for;
  // As above, except for slot info.  In addition, look at each of the
  // accessor methods and figure out whether we should eagerly emit
  // xeps.
  for (elem in defn.class-defn-slots)
    let info = elem.slot-defn-info;
    if (info)
      maybe-emit-entries(info.slot-init-value, file);
      maybe-emit-entries(info.slot-init-function, file);
    end if;
    unless (elem.slot-defn-allocation == #"virtual")
      let getter = elem.slot-defn-getter;
      check-generic-method-xep(getter, file);
      let setter = elem.slot-defn-setter;
      if (setter) check-generic-method-xep(setter, file) end if;
    end unless;
  end for;
end method emit-tlf-gunk;

define method emit-tlf-gunk
    (tlf :: <define-bindings-tlf>, file :: <file-state>)
    => ();
  format(file.file-body-stream, "\n/* %s */\n\n", tlf.clean-for-comment);
  for (defn in tlf.tlf-required-defns)
    if (instance?(defn, <function-definition>)
	  & defn.defn-name.name-inherited-or-exported?)
      maybe-emit-entries(defn.function-defn-ct-value, file);
    end if;
    emit-definition-gunk(defn, file);
  end;
  if (tlf.tlf-rest-defn)
    emit-definition-gunk(tlf.tlf-rest-defn, file);
  end;
  write(file.file-body-stream, "\n");
end;

define method emit-definition-gunk
    (defn :: <definition>, file :: <file-state>) => ();
  let info = get-info-for(defn, file);
  let stream = file.file-body-stream;
  let rep = info.backend-var-info-rep;
  if (instance?(rep, <immediate-representation>))
    let name = info.backend-var-info-name;
    format(stream, "%s %s = ", rep.representation-c-type, name);
    let init-value = if (instance?(defn, <variable-definition>))
		       defn.defn-init-value;
		     else
		       defn.ct-value;
		     end;
    if (init-value)
      let (init-value-expr, init-value-rep)
	= c-expr-and-rep(init-value, rep, file);
      format(stream, "%s;\t/* %s */\n",
	     conversion-expr(rep, init-value-expr, init-value-rep,
			     file),
	     defn.defn-name.clean-for-comment);
    else
      format(stream, "0;\t/* %s */\nint %s_initialized = FALSE;\n",
	     defn.defn-name.clean-for-comment, name);
    end;
    file.file-prototypes-exist-for[name] := #t;
  else
    format(stream, "/* %s allocated as %s */\n",
	   defn.defn-name.clean-for-comment,
	   info.backend-var-info-name.clean-for-comment);
  end;
end;

define method emit-definition-gunk
    (defn :: <variable-definition>, file :: <file-state>, #next next-method)
    => ();
  next-method();
  let type-defn = defn.var-defn-type-defn;
  if (type-defn)
    emit-definition-gunk(type-defn, file);
  end;
end;

define method emit-definition-gunk
    (defn :: <abstract-constant-definition>, file :: <file-state>,
     #next next-method)
    => ();
  let ctv = defn.ct-value;
  if (instance?(ctv, <eql-ct-value>))
    format(file.file-body-stream, "/* %s is %s */\n",
	   defn.defn-name.clean-for-comment,
	   ctv.clean-for-comment);
  else
    next-method();
  end;
end;


// emit-component  --  exported interface

define method emit-component
    (component :: <fer-component>, file :: <file-state>) => ();

  // Do pre-pass over all function literals, allocating c-names for the entry
  // points that have already been created.  Later similar stuff is done on the
  // fly when we lazily introduce new entry points.
  for (func-lit in component.all-function-literals)
    let ctv = func-lit.ct-function;
    if (ctv)
      let ctv-info = get-info-for(ctv, file);
      begin
        // make info for the main entry be the same as info for the
        // ct-function.
	let m-entry = func-lit.main-entry;
	if (m-entry.info)
	  error("%= is already annotated?", m-entry);
	end;
	m-entry.info := ctv-info;
      end;
      if (func-lit.general-entry)
	let gen-info = get-info-for(func-lit.general-entry, file);
	if (gen-info.function-info-main-entry-c-name)
	  error("%= already has a name?", func-lit.general-entry);
	end;
	gen-info.function-info-main-entry-c-name
	  := general-entry-c-name(ctv-info, file);
      end;
      if (instance?(func-lit, <method-literal>) & func-lit.generic-entry)
	let gen-info = get-info-for(func-lit.generic-entry, file);
	if (gen-info.function-info-main-entry-c-name)
	  error("%= already has a name?", func-lit.general-entry);
	end;
	gen-info.function-info-main-entry-c-name
	  := generic-entry-c-name(ctv-info, file);
      end;
    end;
  end;

  do(rcurry(emit-function, file), component.all-function-regions);

  // Either emit-tlf-gunk or emit-component may have ended up
  // creating new components to hold required xeps.  Since we're
  // between things, this is an excellent time to spew them.
  // Eventually, we'll run out and stop iterating.
  if (~file.file-deferred-xeps.empty?)
    emit-component(pop(file.file-deferred-xeps), file);
  end if;
end;



// Function region emitters:
//
// These functions deal emitting function regions, which includes the C
// function definition and prototype.

define method emit-function
    (function :: <fer-function-region>, file :: <file-state>)
    => ();
  file.file-next-block := 0;
  file.file-local-table := make(<string-table>);
  assert(file.file-pending-defines == #f);

  let function-info = get-info-for(function, file);
  let c-name = main-entry-c-name(function-info, file);
  file.file-prototypes-exist-for[c-name] := #t;

  let max-depth = analyze-stack-usage(function);
  for (i from 0 below max-depth)
    format(file.file-vars-stream,
	   "descriptor_t *cluster_%d_top;\n",
	   i);
  end;
  emit-region(function.body, file);

  let stream = file.file-body-stream;
  format(stream, "/* %s */\n", function.name.clean-for-comment);
  format(stream, "%s\n{\n",
	 compute-function-prototype(function, function-info, file));
  write(stream, get-string(file.file-vars-stream));
  write(stream, "\n");
  let overflow = file.file-guts-overflow;
  unless (overflow.empty?)
    for (string in overflow)
      write(stream, string);
    end;
    overflow.size := 0;
  end unless;
  write(stream, get-string(file.file-guts-stream));
  write(stream, "}\n\n");
end;

define method compute-function-prototype
    (function :: false-or(<fer-function-region>),
     function-info :: <function-info>,
     file :: <file-state>)
    => res :: <byte-string>;
  let c-name = main-entry-c-name(function-info, file);
  let stream = make(<buffered-byte-string-output-stream>);
  let result-rep = function-info.function-info-result-representation;
  case
    (result-rep == #"doesn't-return") => write(stream, "void");
    (result-rep == #"cluster") => write(stream, "descriptor_t *");
    (instance?(result-rep, <sequence>)) =>
      if (result-rep.empty?)
	write(stream, "void");
      else
	format(stream, "struct %s",
	       pick-result-structure(result-rep, file));
      end if;
    otherwise => write(stream, result-rep.representation-c-type);
  end;
  format(stream, " %s(descriptor_t *orig_sp", c-name);
  for (rep in function-info.function-info-argument-representations,
       index from 0,
       var = function & function.prologue.dependents.dependent.defines
	 then var & var.definer-next)
    format(stream, ", %s A%d", rep.representation-c-type, index);
    if (var)
      let varinfo = var.var-info;
      if (instance?(varinfo, <debug-named-info>))
	format(stream, " /* %s */", varinfo.debug-name.clean-for-comment);
      end;
    end;
  end;
  write-element(stream, ')');
  stream.stream-contents;
end;

define method pick-result-structure
    (results :: <sequence>, file :: <file-state>) => res :: <byte-string>;
  let types = map-as(<simple-object-vector>, representation-c-type, results);
  let table = file.file-result-structures;
  let struct = element(table, types, default: #f);
  if (struct)
    struct;
  else
    let id = file.file-next-mv-result-struct;
    file.file-next-mv-result-struct := id + 1;
    let name = stringify("mv_result_", id);
    let stream = file.file-body-stream;
    format(stream, "struct %s {\n", name);
    for (type in types, index from 0)
      format(stream, "    %s R%d;\n", type, index);
    end;
    format(stream, "};\n\n");
    element(table, types) := name;
    name;
  end;
end;

define method emit-prototype-for
    (name :: <byte-string>, function-info :: <function-info>,
     file :: <file-state>)
    => ();
  format(file.file-body-stream,
	 "extern %s;\t/* %s */\n\n",
	 compute-function-prototype(#f, function-info, file),
	 function-info.function-info-name.clean-for-comment);
end;

define method emit-prototype-for
    (name :: <byte-string>, function-info == #"general",
     file :: <file-state>)
    => ();
  format(file.file-body-stream,
	 "extern descriptor_t * %s(descriptor_t *orig_sp, "
	   "heapptr_t A0 /* self */, long A1 /* nargs */);\n\n",
	 name);
end;

define method emit-prototype-for
    (name :: <byte-string>, function-info == #"generic",
     file :: <file-state>)
    => ();
  format(file.file-body-stream,
	 "extern descriptor_t * %s(descriptor_t *orig_sp, "
	   "heapptr_t A0 /* self */, long A1 /* nargs */, "
	   "heapptr_t A2 /* next-info */);\n\n",
	 name);
end;


// Non-function region emitters

// 
define method emit-region
    (region :: <simple-region>, file :: <file-state>)
    => ();
  let byte-string :: <buffered-byte-string-output-stream>
    = file.file-guts-stream.inner-stream;
  for (assign = region.first-assign then assign.next-op,
       while: assign)
    emit-assignment(assign.defines, assign.depends-on.source-exp, file);
    if (byte-string.stream-size >= 65536)
      add!(file.file-guts-overflow, byte-string.stream-contents);
    end if;
  end;
end;

define method emit-region (region :: <compound-region>,
			   file :: <file-state>)
    => ();
  for (subregion in region.regions)
    emit-region(subregion, file);
  end;
end;
 
define method elseif-able? (region :: <if-region>) => answer :: <boolean>;
  #t;
end method elseif-able?;

define method elseif-able? (region :: <compound-region>)
 => answer :: <boolean>;
  region.regions.size == 2
    & instance?(region.regions.first, <simple-region>)
    & instance?(region.regions.second, <if-region>)
    & begin
	let simple = region.regions.first;
	let if-region = region.regions.second;
	simple.first-assign ~== #f
	  & simple.first-assign == simple.last-assign  // only one assignment
	  & simple.first-assign.defines == if-region.depends-on.source-exp
	  & simple.first-assign.depends-on.source-exp.c-code-moveable?;
      end;
end method elseif-able?;

define method elseif-able? (region :: <region>) => answer :: <boolean>;
  #f;
end method elseif-able?;


define method emit-region (if-region :: <if-region>, file :: <file-state>)
 => ();
  let stream = file.file-guts-stream;
  let cond = ref-leaf(*boolean-rep*, if-region.depends-on.source-exp, file);
  spew-pending-defines(file);
  format(stream, "if (%s) {\n", cond);
  indent(stream, $indentation-step);
  emit-region(if-region.then-region, file);
  /* ### emit-joins(if-region.join-region, file); */
  spew-pending-defines(file);
  indent(stream, -$indentation-step);
  write(stream, "}\n");

  // We need to detect places where we can use "else if" rather than
  // "else { if" because if we don't, we'll break the parsers on
  // crappy C compilers (such as Microsoft Visual C++)
  let clause-is-an-elseif = if-region.else-region.elseif-able?;
  
  if (clause-is-an-elseif)
    write(stream, "else ");
  else
    write(stream, "else {\n");
    indent(stream, $indentation-step);
  end if;
  
  emit-region(if-region.else-region, file);
  /* ### emit-joins(if-region.join-region, file); */

  spew-pending-defines(file);
  if (~clause-is-an-elseif)
    indent(stream, -$indentation-step);
    write(stream, "}\n");
  end if;
end method emit-region;


define method emit-region (region :: <loop-region>,
			   file :: <file-state>)
    => ();
  /* ### emit-joins(region.join-region, file); */
  spew-pending-defines(file);
  let stream = file.file-guts-stream;
  write(stream, "while (1) {\n");
  indent(stream, $indentation-step);
  emit-region(region.body, file);
  /* ### emit-joins(region.join-region, file); */
  spew-pending-defines(file);
  indent(stream, -$indentation-step);
  write(stream, "}\n");
end;

define method make-info-for
    (block-region :: <block-region>, file :: <file-state>) => res :: <object>;
  let id = file.file-next-block;
  file.file-next-block := id + 1;
  id;
end;

define method emit-region
    (region :: <block-region>, file :: <file-state>) => ();
  unless (region.exits)
    error("A block with no exits still exists?");
  end;
  let stream = file.file-guts-stream;
  emit-region(region.body, file);
  /* ### emit-joins(region.join-region, file); */
  spew-pending-defines(file);
  let half-step = ash($indentation-step, -1);
  indent(stream, - half-step);
  format(stream, "block%d:;\n", get-info-for(region, file));
  indent(stream, half-step);
end;

define method emit-region (region :: <unwind-protect-region>,
			   file :: <file-state>)
    => ();
  emit-region(region.body, file);
end;

define method emit-region (region :: <exit>, file :: <file-state>)
    => ();
  /* ### emit-joins(region.join-region, file); */
  spew-pending-defines(file);
  let stream = file.file-guts-stream;
  let target = region.block-of;
  for (region = region.parent then region.parent,
       until: region == #f | region == target)
    finally
    unless (region)
      error("Non-local raw exit?");
    end;
  end;
  if (instance?(target, <block-region>))
    format(stream, "goto block%d;\n", get-info-for(target, file));
  else
    format(stream, "not_reached();\n");
  end;
end;

define method emit-region (return :: <return>, file :: <file-state>)
    => ();
  /* ### emit-joins(region.join-region, file); */
  let function :: <fer-function-region> = return.block-of;
  let function-info = get-info-for(function, file);
  let result-rep = function-info.function-info-result-representation;
  emit-return(return, result-rep, file);
end;

define method emit-return
    (return :: <return>, result-rep == #"doesn't-return",
     file :: <file-state>)
    => ();
  error("have a return region for a function that doesn't return?");
end;

define method emit-return
    (return :: <return>, result-rep == #"cluster",
     file :: <file-state>)
    => ();
  let stream = file.file-guts-stream;
  let results = return.depends-on;
  if (results & instance?(results.source-exp, <abstract-variable>)
	& instance?(results.source-exp.var-info, <values-cluster-info>))
    let (bottom-name, top-name)
      = consume-cluster(results.source-exp, file);
    unless (bottom-name = "orig_sp")
      error("Delivering a cluster that isn't at the bottom of the frame?");
    end;
    spew-pending-defines(file);
    format(stream, "return %s;\n", top-name);
  else
    for (dep = results then dep.dependent-next,
	 count from 0,
	 while: dep)
      format(stream, "orig_sp[%d] = %s;\n", count,
	     ref-leaf(*general-rep*, dep.source-exp, file));
    finally
      spew-pending-defines(file);
      format(stream, "return orig_sp + %d;\n", count);
    end;
  end;
end;

define method emit-return
    (return :: <return>, result-rep :: <c-representation>,
     file :: <file-state>)
    => ();
  let stream = file.file-guts-stream;
  let expr = ref-leaf(result-rep, return.depends-on.source-exp, file);
  spew-pending-defines(file);
  format(stream, "return %s;\n", expr);
end;

//define method emit-return
//    (return :: <return>, result-rep == #(), file :: <file-state>)
//    => ();
//  spew-pending-defines(file);
//  write(file.file-guts-stream, "return;\n");
//end;

define method emit-return
    (return :: <return>, result-reps :: <sequence>, file :: <file-state>)
    => ();
  if (result-reps.empty?)
    spew-pending-defines(file);
    write(file.file-guts-stream, "return;\n");
  else
    let stream = file.file-guts-stream;  
    let temp = new-local(file, modifier: "temp");
    let function = return.block-of;
    let function-info = get-info-for(function, file);
    let name = pick-result-structure(result-reps, file);
    format(file.file-vars-stream, "struct %s %s;\n",
	   name, temp);
    for (rep in result-reps,
	 index from 0,
	 dep = return.depends-on then dep.dependent-next)
      format(stream, "%s.R%d = %s;\n",
	     temp, index, ref-leaf(rep, dep.source-exp, file));
    end;
    spew-pending-defines(file);
    format(stream, "return %s;\n", temp);
  end if;
end;


define method block-id (region :: <false>) => id :: <false>;
  #f;
end;

define method block-id (region :: <region>) => id :: false-or(<integer>);
  region.parent.block-id;
end;

define method block-id (region :: <block-region>)
    => id :: false-or(<integer>);
  let parent-id = region.parent.block-id;
  if (~region.exits)
    parent-id;
  elseif (parent-id)
    parent-id + 1;
  else
    0;
  end;
end;



// Assignments.

define method emit-assignment (defines :: false-or(<definition-site-variable>),
			       var :: <abstract-variable>,
			       file :: <file-state>)
    => ();
  if (defines)
    if (instance?(var.var-info, <values-cluster-info>))
      let (bottom-name, top-name) = consume-cluster(var, file);
      deliver-cluster(defines, bottom-name, top-name,
		      var.derived-type.min-values, file);
    else
      let rep = if (instance?(defines.var-info, <values-cluster-info>))
		  *general-rep*;
		else
		  variable-representation(defines, file)
		end;

      deliver-result(defines, ref-leaf(rep, var, file), rep, #f,
		     file);
    end;
  end;
end;

define method emit-assignment (defines :: false-or(<definition-site-variable>),
			       expr :: <literal-constant>,
			       file :: <file-state>)
    => ();
  if (defines)
    let rep-hint = if (instance?(defines.var-info, <values-cluster-info>))
		     *general-rep*;
		   else
		     variable-representation(defines, file)
		   end;
    let (expr, rep) = c-expr-and-rep(expr.value, rep-hint, file);
    deliver-result(defines, expr, rep, #f, file);
  end;
end;

define method emit-assignment (defines :: false-or(<definition-site-variable>),
			       leaf :: <function-literal>,
			       file :: <file-state>)
    => ();
  deliver-result(defines, ref-leaf(*heap-rep*, leaf, file),
		 *heap-rep*, #f, file);
end;

define method emit-assignment (defines :: false-or(<definition-site-variable>),
			       leaf :: <definition-constant-leaf>,
			       file :: <file-state>)
    => ();
  let defn = leaf.const-defn;
  let info = get-info-for(defn, file);
  let name = info.backend-var-info-name;
  maybe-emit-prototype(name, defn, file);
  if (instance?(defn, <function-definition>))
    maybe-emit-entries(defn.function-defn-ct-value, file);
  end if;
  deliver-result(defines, name, info.backend-var-info-rep, #f, file);
end;

define method emit-assignment (results :: false-or(<definition-site-variable>),
			       leaf :: <uninitialized-value>,
			       file :: <file-state>)
    => ();
  if (results)
    let rep = variable-representation(results, file);
    if (rep == *general-rep*)
      deliver-result(results, "0", *heap-rep*, #f, file);
    else
      deliver-result(results, "0", rep, #f, file);
    end;
  end;
end;


// Function calls:

define method emit-assignment
    (results :: false-or(<definition-site-variable>), call :: <abstract-call>,
     file :: <file-state>)
    => ();
  let stream = file.file-guts-stream;

  let use-generic-entry?
    = instance?(call, <general-call>) & call.use-generic-entry?;

  let (next-info, arguments)
    = if (use-generic-entry?)
	let dep = call.depends-on.dependent-next;
	values(ref-leaf(*heap-rep*, dep.source-exp, file),
	       dep.dependent-next);
      else
	values(#f, call.depends-on.dependent-next);
      end;

  let (bottom-name, call-top-name, return-top-name, count)
    = if (arguments & instance?(arguments.source-exp, <abstract-variable>)
	    & instance?(arguments.source-exp.var-info, <values-cluster-info>))
	let (bot, top) = consume-cluster(arguments.source-exp, file);
	values(bot,
	       top,
	       top,
	       stringify(top, " - ", bot));
      else
	let (args, sp) = cluster-names(call.info);
	let setup-stream = make(<buffered-byte-string-output-stream>);
	for (arg-dep = arguments then arg-dep.dependent-next,
	     count from 0,
	     while: arg-dep)
	  format(setup-stream, "%s[%d] = %s;\n", args, count,
		 ref-leaf(*general-rep*, arg-dep.source-exp, file));
	finally
	  write(stream, setup-stream.stream-contents);
	  values(args,
		 stringify(args, " + ", count),
		 sp,
		 stringify(count));
	end for;
      end if;
  
  spew-pending-defines(file);

  let function = call.depends-on.source-exp;
  let (entry, name)
    = xep-expr-and-name(function, use-generic-entry?, file);
  let func = ref-leaf(*heap-rep*, function, file);

  if (name)
    format(stream, "/* %s */\n",
    	   clean-for-comment(format-to-string("%s", name)));
  end;
  if (results)
    format(stream, "%s = ", return-top-name);
  end;
  format(stream, "%s(%s, %s, %s", entry, call-top-name, func, count);
  if (next-info)
    write(stream, ", ");
    write(stream, next-info);
  end;
  write(stream, ");\n");
  deliver-cluster
    (results, bottom-name, return-top-name,
     call.derived-type.min-values, file);
end;

define method xep-expr-and-name
    (func :: <leaf>, generic-entry? :: <boolean>, file :: <file-state>)
    => (expr :: <string>, name :: false-or(<name>));
  spew-pending-defines(file);
  values(stringify(if (generic-entry?)
		     "GENERIC_ENTRY(";
		   else
		     "GENERAL_ENTRY(";
		   end,
		   ref-leaf(*heap-rep*, func, file),
		   ')'),
	 #f);
end;

define method xep-expr-and-name
    (func :: <function-literal>, generic-entry? :: <boolean>,
     file :: <file-state>)
    => (expr :: <string>, name :: <name>);
  if (generic-entry?)
    error("%= doesn't have a generic entry.", func);
  end;
  let general-entry = func.general-entry;
  let entry-info = get-info-for(general-entry, file);
  let entry-name = main-entry-c-name(entry-info, file);
  maybe-emit-prototype(entry-name, entry-info, file);
  values(entry-name, general-entry.name);
end;

define method xep-expr-and-name
    (func :: <method-literal>, generic-entry? :: <true>,
     file :: <file-state>)
    => (expr :: <string>, name :: <name>);
  let generic-entry = func.generic-entry;
  let entry-info = get-info-for(generic-entry, file);
  let entry-name = main-entry-c-name(entry-info, file);
  maybe-emit-prototype(entry-name, entry-info, file);
  values(entry-name, generic-entry.name);
end;

define method xep-expr-and-name
    (func :: <definition-constant-leaf>, generic-entry? :: <boolean>,
     file :: <file-state>,
     #next next-method)
    => (expr :: <string>, name :: <name>);
  let defn = func.const-defn;
  let (expr, name) = xep-expr-and-name(defn, generic-entry?, file);
  values(expr | next-method(),
	 name | defn.defn-name);
end;

define method xep-expr-and-name
    (func :: <literal-constant>, generic-entry? :: <boolean>,
     file :: <file-state>, #next next-method)
    => (expr :: false-or(<string>), name :: false-or(<name>));
  let ctv = func.value;
  let (expr, name) = xep-expr-and-name(ctv, generic-entry?, file);
  values(expr | next-method(),
	 name | ctv.ct-function-name);
end;

define method xep-expr-and-name
    (defn :: <abstract-constant-definition>, generic-entry? :: <boolean>,
     file :: <file-state>)
    => (expr :: false-or(<string>), name :: false-or(<name>));
  let ctv = ct-value(defn);
  if (ctv)
    xep-expr-and-name(ctv, generic-entry?, file);
  else
    values(#f, #f);
  end;
end;

define method xep-expr-and-name
    (ctv :: <ct-function>, generic-entry? :: <boolean>,
     file :: <file-state>)
    => (expr :: false-or(<string>), name :: false-or(<name>));
  if (generic-entry?)
    error("%= doesn't have a generic entry.", ctv);
  end;
  let name = maybe-emit-general-entry(ctv, file);  
  maybe-emit-prototype(name, #"general", file);
  values(name, ctv.ct-function-name);
end;

define method xep-expr-and-name
    (ctv :: <ct-generic-function>, generic-entry? :: <boolean>,
     file :: <file-state>)
    => (expr :: false-or(<string>), name :: false-or(<name>));
  if (generic-entry?)
    error("%= doesn't have a generic entry.", ctv);
  end;
  let defn = ctv.ct-function-definition;
  if (defn)
    let discriminator = defn.generic-defn-discriminator;
    if (discriminator)
      xep-expr-and-name(discriminator, #f, file);
    else
      values(#f, #f);
    end;
  else
    values(#f, #f);
  end;
end;

define method xep-expr-and-name
    (ctv :: <ct-method>, generic-entry? :: <true>,
     file :: <file-state>)
    => (expr :: false-or(<string>), name :: false-or(<name>));
  let name = maybe-emit-generic-entry(ctv, file);
  maybe-emit-prototype(name, #"generic", file);
  values(name, ctv.ct-function-name);
end;



// Known calls:

define method emit-assignment
    (results :: false-or(<definition-site-variable>), call :: <known-call>,
     file :: <file-state>)
    => ();
  let function = call.depends-on.source-exp;
  let func-info = find-main-entry-info(function, file);
  let stream = make(<buffered-byte-string-output-stream>);
  let c-name = main-entry-c-name(func-info, file);
  let (sp, new-sp) = cluster-names(call.info);
  format(stream, "%s(%s", c-name, sp);
  for (arg-dep = call.depends-on.dependent-next then arg-dep.dependent-next,
       rep in func-info.function-info-argument-representations)
    unless (arg-dep)
      error("Not enough arguments in a known call?");
    end;
      write(stream, ", ");
      write(stream, ref-leaf(rep, arg-dep.source-exp, file));
  finally
    if (arg-dep)
      error("Too many arguments in a known call?");
    end;
  end;
  write-element(stream, ')');
  let call = stream-contents(stream);
  format(file.file-guts-stream, "/* %s */\n",
	 func-info.function-info-name.clean-for-comment);
  let result-rep = func-info.function-info-result-representation;
  case
    (results == #f) =>
      format(file.file-guts-stream, "%s;\n", call);
      deliver-results(results, #[], #f, file);
    (result-rep == #"doesn't-return") =>
      error("Trying to get some values back from a function that "
	      "doesn't return?");
    (result-rep == #"cluster") =>
      format(file.file-guts-stream, "%s = %s;\n", new-sp, call);
      deliver-cluster(results, sp, new-sp,
		      func-info.function-info-result-type.min-values,
		      file);
    (instance?(result-rep, <sequence>)) =>
      if (result-rep.empty?)
	format(file.file-guts-stream, "%s;\n", call);
	deliver-results(results, #[], #f, file);
      else
	let temp = new-local(file, modifier: "temp");
	format(file.file-vars-stream, "struct %s %s;\n",
	       pick-result-structure(result-rep, file),
	       temp);
	format(file.file-guts-stream, "%s = %s;\n", temp, call);
	let result-exprs = make(<vector>, size: result-rep.size);
	for (rep in result-rep,
	     index from 0)
	  result-exprs[index]
	    := pair(stringify(temp, ".R", index), rep);
	end;
	deliver-results(results, result-exprs, #f, file);
      end if;
    otherwise =>
      deliver-result(results, call, result-rep, #t, file);
  end;
end;

define method find-main-entry-info
    (func :: <function-literal>, file :: <file-state>)
    => res :: <function-info>;
  let entry = func.main-entry;
  let info = get-info-for(entry, file);
  maybe-emit-prototype(main-entry-c-name(info, file), info, file);
  info;
end;

define method find-main-entry-info
    (func :: <definition-constant-leaf>, file :: <file-state>)
    => res :: <function-info>;
  find-main-entry-info(func.const-defn.ct-value, file);
end;

define method find-main-entry-info
    (func :: <literal-constant>, file :: <file-state>)
    => res :: <function-info>;
  find-main-entry-info(func.value, file);
end;

define method find-main-entry-info
    (defn :: <generic-definition>, file :: <file-state>)
    => res :: <function-info>;
  let discriminator = defn.generic-defn-discriminator;
  if (discriminator)
    find-main-entry-info(discriminator, file);
  else
    error("Known call of a generic function without a static discriminator?");
  end;
end;

define method find-main-entry-info
    (defn :: <abstract-method-definition>, file :: <file-state>)
    => res :: <function-info>;
  find-main-entry-info(defn.ct-value, file);
end;

define method find-main-entry-info
    (ctv :: <ct-function>, file :: <file-state>)
    => res :: <function-info>;
  let info = get-info-for(ctv, file);
  maybe-emit-prototype(main-entry-c-name(info, file), info, file);
  info;
end;

define method find-main-entry-info
    (ctv :: <ct-generic-function>, file :: <file-state>)
    => res :: <function-info>;
  find-main-entry-info(ctv.ct-function-definition, file);
end;



// Primitive calls:

define method emit-assignment (defines :: false-or(<definition-site-variable>),
			       expr :: <primitive>,
			       file :: <file-state>)
    => ();
  let emitter
    = expr.primitive-info.priminfo-emitter | default-primitive-emitter;
  emitter(defines, expr, file);
end;

define method emit-assignment
    (defines :: false-or(<definition-site-variable>), expr :: <catch>,
     file :: <file-state>)
    => ();
  let func = extract-operands(expr, file, *heap-rep*);
  let (values, sp) = cluster-names(expr.info);
  let stream = file.file-guts-stream;
  if (defines)
    format(stream, "%s = ", sp);
  end;
  let catch-defn = dylan-defn(#"catch");
  assert(instance?(catch-defn, <abstract-method-definition>));
  let catch-info = find-main-entry-info(catch-defn, file);
  format(stream, "catch(%s, %s, %s);\n",
	 main-entry-c-name(catch-info, file), values, func);
  if (defines)
    deliver-cluster(defines, values, sp, 0, file);
  end;
end;

define method emit-assignment (defines :: false-or(<definition-site-variable>),
			       expr :: <prologue>,
			       file :: <file-state>)
    => ();
  let function-info = get-info-for(expr.function, file);
  deliver-results(defines,
		  map(method (rep, index)
			pair(stringify('A', index), rep);
		      end,
		      function-info.function-info-argument-representations,
		      make(<range>, from: 0)),
		  #f, file);
end;

define method emit-assignment
    (defines :: false-or(<definition-site-variable>),
     ref :: <module-var-ref>, file :: <file-state>)
    => ();
  let defn = ref.variable;
  let info = get-info-for(defn, file);
  let name = info.backend-var-info-name;
  maybe-emit-prototype(name, defn, file);
  let rep = info.backend-var-info-rep;
  let stream = file.file-guts-stream;
  unless (defn.defn-guaranteed-initialized?)
    if (rep.representation-has-bottom-value?)
      let temp = new-local(file, modifier: "temp");
      format(file.file-vars-stream, "%s %s;\n",
	     rep.representation-c-type, temp);
      format(stream, "if ((%s = %s).heapptr == NULL) abort();\n", temp, name);
      name := temp;
    else
      format(stream, "if (!%s_initialized) abort();\n", name);
    end;
  end;
  deliver-result(defines, name, rep, #f, file);
end;

define method emit-assignment
    (defines :: false-or(<definition-site-variable>),
     set :: <module-var-set>, file :: <file-state>)
    => ();
  let defn = set.variable;
  let info = get-info-for(defn, file);
  let target = info.backend-var-info-name;
  maybe-emit-prototype(target, defn, file);
  let rep = info.backend-var-info-rep;
  let source = extract-operands(set, file, rep);
  spew-pending-defines(file);
  emit-copy(target, rep, source, rep, file);
  unless (defn.defn-guaranteed-initialized?
	    | rep.representation-has-bottom-value?)
    let stream = file.file-guts-stream;
    format(stream, "%s_initialized = TRUE;\n", target);
  end;
  deliver-results(defines, #[], #f, file);
end;

define method emit-assignment
    (results :: false-or(<definition-site-variable>),
     call :: <self-tail-call>, file :: <file-state>)
    => ();
  spew-pending-defines(file);
  let function = call.self-tail-call-of;
  for (param = function.prologue.dependents.dependent.defines
	 then param.definer-next,
       closure-var
	 = instance?(function, <lambda>) & function.environment.closure-vars
	 then closure-var.closure-next,
       index from 0,
       while: closure-var & param)
  finally
    let stream = file.file-guts-stream;
    for (param = param then param.definer-next,
	 arg-dep = call.depends-on then arg-dep.dependent-next,
	 index from index,
	 while: arg-dep & param)
      let (name, rep) = c-name-and-rep(param, file);
      format(stream, "A%d = %s;\n",
	     index, ref-leaf(rep, arg-dep.source-exp, file));
    finally
      if (arg-dep | param)
	error("Wrong number of operands in a self-tail-call?");
      end;
    end;
  end;
  deliver-results(results, #[], #f, file);
end;

define method emit-assignment
    (results :: false-or(<definition-site-variable>),
     op :: <heap-slot-ref>, file :: <file-state>)
    => ();
  let slot = op.slot-info;
  let slot-rep = slot.slot-representation;

  let instance-leaf = op.depends-on.source-exp;
  let instance-rep = pick-representation(instance-leaf.derived-type, #"speed");

  let expr
    = if (instance?(slot, <vector-slot-info>))
	let (instance, offset, index)
	  = extract-operands(op, file, *heap-rep*, *long-rep*, *long-rep*);
	let c-type = slot-rep.representation-c-type;
	stringify("SLOT(", instance, ", ", c-type, ", ",
		  offset, " + ", index, " * sizeof(", c-type, "))");
      elseif (instance?(instance-rep, <immediate-representation>)
		& instance?(slot-rep, <immediate-representation>))
	assert(instance-rep == slot-rep);
	let (instance, offset)
	  = extract-operands(op, file, instance-rep, *long-rep*);
	instance;
      else
	let (instance, offset)
	  = extract-operands(op, file, *heap-rep*, *long-rep*);
	stringify("SLOT(", instance, ", ",
		  slot-rep.representation-c-type, ", ",
		  offset, ')');
      end if;
  if (slot.slot-read-only?)
    deliver-result(results, expr, slot-rep, #f, file);
  else
    spew-pending-defines(file);
    deliver-result(results, expr, slot-rep, #t, file);
  end if;
end;

define method emit-assignment
    (results :: false-or(<definition-site-variable>),
     op :: <data-word-ref>, file :: <file-state>)
    => ();
  let slot = op.slot-info;
  let slot-rep = slot.slot-representation;
  assert(instance?(slot-rep, <c-data-word-representation>));
  
  let instance-leaf = op.depends-on.source-exp;
  let instance-rep = pick-representation(instance-leaf.derived-type, #"speed");

  let expr
    = if (instance?(instance-rep, <general-representation>))
	// The instance is currently being represented with the general
	// representation, either because the instance has both heap slots and
	// a data-word slot, or because we still need the type info.  We just
	// use ref-leaf to extract the data word by lying about the
	// representation.
	ref-leaf(slot-rep, instance-leaf, file);
      elseif (instance?(instance-rep, <c-data-word-representation>))
	// Both the instance and slot are in data-words.  Make sure they
	// are the same flavor of data-word, and then reference it.
	assert(instance-rep.representation-data-word-member
		 = slot-rep.representation-data-word-member);
	ref-leaf(instance-rep, instance-leaf, file);
      else
	error("Trying to extract the data-word from something that doens't "
		"have one.");
      end if;

  deliver-result(results, expr, slot-rep, #f, file);
end method emit-assignment;

define method emit-assignment
    (results :: false-or(<definition-site-variable>),
     op :: <heap-slot-set>, file :: <file-state>)
    => ();
  let slot = op.slot-info;
  let slot-rep = slot.slot-representation;
  if (instance?(slot, <vector-slot-info>))
    let (new, instance, offset, index)
      = extract-operands(op, file, slot-rep, *heap-rep*,
			 *long-rep*, *long-rep*);
    let c-type = slot-rep.representation-c-type;
    format(file.file-guts-stream,
	   "SLOT(%s, %s, %s + %s * sizeof(%s)) = %s;\n",
	   instance, c-type, offset, index, c-type, new);
  else
    let (new, instance, offset)
      = extract-operands(op, file, slot-rep, *heap-rep*, *long-rep*);
    format(file.file-guts-stream,
	   "SLOT(%s, %s, %s) = %s;\n",
	   instance, slot-rep.representation-c-type, offset, new);
  end;
  deliver-results(results, #[], #f, file);
end;


define method emit-assignment
    (results :: false-or(<definition-site-variable>),
     op :: <truly-the>, file :: <file-state>)
    => ();
  if (results)
    let rep = variable-representation(results, file);
    let source = extract-operands(op, file, rep);
    deliver-result(results, source, rep, #f, file);
  end;
end;



// Deliver-results utility:

define method deliver-cluster
    (defines :: false-or(<definition-site-variable>),
     src-start :: <string>, src-end :: <string>,
     min-values :: <integer>, file :: <file-state>)
    => ();

  if (defines)
    let stream = file.file-guts-stream;
    if (instance?(defines.var-info, <values-cluster-info>))
      let (dst-start, dst-end) = produce-cluster(defines, file);
      if (src-start ~= dst-start)
	format(stream, "%s = %s;\n", dst-end, dst-start);
	format(stream, "while (%s < %s)\n", src-start, src-end);
	format(stream, "    *%s++ = *%s++;\n", dst-end, src-start);
      elseif (src-end ~= dst-end)
	format(stream, "%s = %s;\n", dst-end, src-end);
      end;
    elseif (defines.definer-next == #f)
      let (name, rep) = c-name-and-rep(defines, file);
      let (false, false-rep) = c-expr-and-rep(as(<ct-value>, #f), rep, file);
      let source
	= stringify('(', src-start, " == ", src-end, " ? ",
		    conversion-expr(rep, false, false-rep, file), " : ",
		    conversion-expr
		      (rep, stringify(src-start, "[0]"), *general-rep*, file),
		    ')');
      deliver-single-result(defines, source, rep, #t, file);
    else
      let count = for (var = defines then var.definer-next,
		       index from 0,
		       while: var)
		  finally
		    index;
		  end;
      unless (count <= min-values)
	format(stream, "%s = pad_cluster(%s, %s, %d);\n",
	       src-end, src-start, src-end, count);
      end;
      for (var = defines then var.definer-next,
	   index from 0,
	   while: var)
	let source = stringify(src-start, '[', index, ']');
	deliver-single-result(var, source, *general-rep*, #t, file);
      end;
    end;
  end;
end;

define method deliver-results
    (defines :: false-or(<definition-site-variable>),
     values :: <sequence> /* of <pair>s */,
     now-dammit? :: <boolean>, file :: <file-state>)
    => ();
  if (defines & instance?(defines.var-info, <values-cluster-info>))
    let stream = file.file-guts-stream;
    let (bottom-name, top-name) = produce-cluster(defines, file);
    format(stream, "%s = %s + %d;\n", top-name, bottom-name, values.size);
    for (val in values, index from 0)
      emit-copy(stringify(bottom-name, '[', index, ']'), *general-rep*,
		val.head, val.tail, file);
    end;
  else
    for (var = defines then var.definer-next,
	 val in values,
	 while: var)
      deliver-single-result(var, val.head, val.tail, now-dammit?, file);
    finally
      if (var)
	let false = make(<literal-false>);
	for (var = var then var.definer-next,
	     while: var)
	  let target-rep = variable-representation(var, file);
	  let (source-name, source-rep)
	    = c-expr-and-rep(false, target-rep, file);
	  deliver-single-result(var, source-name, source-rep, #f, file);
	end;
      end;
    end;
  end;
end;

define method deliver-result
    (defines :: false-or(<definition-site-variable>), value :: <string>,
     rep :: <c-representation>, now-dammit? :: <boolean>,
     file :: <file-state>)
    => ();
  if (defines)
    if (instance?(defines.var-info, <values-cluster-info>))
      let stream = file.file-guts-stream;
      let (bottom-name, top-name) = produce-cluster(defines, file);
      format(stream, "%s = %s + 1;\n", top-name, bottom-name);
      emit-copy(stringify(bottom-name, "[0]"), *general-rep*,
		value, rep, file);
    else
      deliver-single-result(defines, value, rep, now-dammit?, file);
      let next = defines.definer-next;
      if (next)
	let false = make(<literal-false>);
	for (var = next then var.definer-next,
	     while: var)
	  let target-rep = variable-representation(var, file);
	  let (source-name, source-rep)
	    = c-expr-and-rep(false, target-rep, file);
	  deliver-single-result(var, source-name, source-rep, #f, file);
	end;
      end;
    end;
  end;
end;

define method deliver-single-result
    (var :: <abstract-variable>, // ### Should really be ssa-variable
     source :: <string>, source-rep :: <c-representation>,
     now-dammit? :: <boolean>, file :: <file-state>)
    => ();
  if (var.dependents)
    if (now-dammit? | var.dependents.source-next)
      let (target-name, target-rep) = c-name-and-rep(var, file);
      emit-copy(target-name, target-rep, source, source-rep, file);
    else
      add-pending-define(var, source, source-rep, file);
    end;
  end;
end;

define method deliver-single-result
    (var :: <initial-definition>, source :: <string>,
     source-rep :: <c-representation>, now-dammit? :: <boolean>,
     file :: <file-state>)
    => ();
  spew-pending-defines(file);
  deliver-single-result(var.definition-of, source, source-rep, now-dammit?,
			file);
end;


// Pending defines:
//
// Used by deliver-result to queue up moves in order to allow generation of
// nested C expressions instead of always generating an explicit temporary for
// each intermediate result.

define class <pending-define> (<object>)
  constant slot pending-var :: <abstract-variable>,
    required-init-keyword: var:;
  constant slot pending-expr :: <byte-string>,
    required-init-keyword: expr:;
  constant slot pending-rep :: <c-representation>,
    required-init-keyword: rep:;
  slot pending-next :: false-or(<pending-define>),
    init-value: #f;
end class <pending-define>;

define method add-pending-define
    (var :: <abstract-variable>, expr :: <byte-string>,
     rep :: <c-representation>, file :: <file-state>)
    => ();
  for (prev = #f then pending,
       pending = file.file-pending-defines then pending.pending-next,
       while: pending)
    if (pending.pending-var == var)
      error("Overwriting a pending define?");
    end if;
  finally
    let new = make(<pending-define>, var: var, expr: expr, rep: rep);
    if (prev)
      prev.pending-next := new;
    else
      file.file-pending-defines := new;
    end if;
  end for;
end method add-pending-define;

define method spew-pending-defines (file :: <file-state>) => ();
  for (pending = file.file-pending-defines then pending.pending-next,
       while: pending)
    let (target, target-rep) = c-name-and-rep(pending.pending-var, file);
    emit-copy(target, target-rep, pending.pending-expr, pending.pending-rep,
	      file);
  end for;
  file.file-pending-defines := #f;
end method spew-pending-defines;


// ref-leaf

// Make sure abstract-variable-is-inlined? agrees with what this
// method will do...
//
define method ref-leaf
    (target-rep :: <c-representation>, leaf :: <abstract-variable>,
     file :: <file-state>)
    => res :: <string>;
  let (expr, rep)
    = block (return)
	for (prev = #f then pending,
	     pending = file.file-pending-defines then pending.pending-next,
	     while: pending)
	  if (pending.pending-var == leaf)
	    if (prev)
	      prev.pending-next := pending.pending-next;
	    else
	      file.file-pending-defines := pending.pending-next;
	    end if;
	    return(pending.pending-expr, pending.pending-rep);
	  end if;
	finally
	  c-name-and-rep(leaf, file);
	end for;
      end block;
  conversion-expr(target-rep, expr, rep, file);
end;

define method ref-leaf (target-rep :: <c-representation>,
			leaf :: <literal-constant>,
			file :: <file-state>)
    => res :: <string>;
  let (expr, rep) = c-expr-and-rep(leaf.value, target-rep, file);
  conversion-expr(target-rep, expr, rep, file);
end;

define method ref-leaf (target-rep :: <c-representation>,
			leaf :: <definition-constant-leaf>,
			file :: <file-state>)
    => res :: <string>;
  let defn = leaf.const-defn;
  let info = get-info-for(defn, file);
  let name = info.backend-var-info-name;
  maybe-emit-prototype(name, defn, file);
  if (instance?(defn, <function-definition>))
    maybe-emit-entries(defn.function-defn-ct-value, file);
  end if;
  conversion-expr(target-rep, info.backend-var-info-name,
		  info.backend-var-info-rep, file);
end;

define method ref-leaf (target-rep :: <c-representation>,
			leaf :: <function-literal>,
			file :: <file-state>)
    => res :: <string>;
  let ctv = leaf.ct-function;
  if (ctv == #f)
    ctv := make(if (instance?(leaf, <method-literal>))
		  <ct-method>;
		else
		  <ct-function>;
		end,
		name: leaf.main-entry.name,
		signature: leaf.signature);
    let ctv-info = get-info-for(ctv, file);
    ctv-info.function-info-main-entry-c-name
      := main-entry-c-name(get-info-for(leaf.main-entry, file), file);
    if (leaf.general-entry)
      ctv-info.function-info-general-entry-c-name
	:= main-entry-c-name(get-info-for(leaf.general-entry, file), file);
    end if;
    if (instance?(leaf, <method-literal>) & leaf.generic-entry)
      ctv-info.function-info-generic-entry-c-name
	:= main-entry-c-name(get-info-for(leaf.generic-entry, file), file);
    end;
    leaf.ct-function := ctv;
  end;
  let (expr, rep) = c-expr-and-rep(ctv, target-rep, file);
  conversion-expr(target-rep, expr, rep, file);
end;

define method ref-leaf (target-rep :: <c-representation>,
			leaf :: <uninitialized-value>,
			file :: <file-state>)
    => res :: <string>;
  if (target-rep == *general-rep*)
    conversion-expr(target-rep, "0", *heap-rep*, file);
  else
    "0";
  end;
end;


// c-expr-and-rep  --- emitting literals


// Emit a reference to a constant which must be referenced via the roots (not
// as a C literal.)  name and modifier are passed through to
// new-root.  Lit is the init-value, and defn is some thingie used to indicate
// whether we've prototyped this thing yet or not.
//
// Putting things in the roots in done lazily because we don't know if we we
// will be able to represent as a C literal or constant until now.
//
define method aux-c-expr-and-rep
    (lit :: <ct-value>, file :: <file-state>,
     #key name :: false-or(<name>), modifier :: <byte-string> = "",
	  defn :: <object> = lit)
 => (name :: <string>, rep :: <c-representation>);
  let info = get-info-for(lit, file);
  let c-name = info.const-info-expr;
  unless (c-name)
    c-name := new-c-global(name, file, modifier: modifier);
    info.const-info-expr := c-name;
    new-root(lit, c-name, file);
  end unless;
  maybe-emit-prototype(c-name, defn, file);
  values(c-name, *general-rep*);
end method aux-c-expr-and-rep;


define method c-expr-and-rep
    (lit :: <ct-value>, rep-hint :: <c-representation>, file :: <file-state>)
 => (name :: <string>, rep :: <c-representation>);
  aux-c-expr-and-rep(lit, file, modifier: "literal");
end;

define method c-expr-and-rep
    (lit :: <ct-function>, rep-hint :: <c-representation>,
     file :: <file-state>)
 => (name :: <string>, rep :: <c-representation>);
  maybe-emit-entries(lit, file);
  let fname = lit.ct-function-name;
  aux-c-expr-and-rep(lit, file, name: fname);
end;

define method c-expr-and-rep
    (lit :: <literal-false>, rep-hint :: <c-representation>,
     file :: <file-state>)
 => (name :: <string>, rep :: <c-representation>);
 aux-c-expr-and-rep(lit, file, modifier: "false");
end;

define method c-expr-and-rep
    (lit :: <literal-true>, rep-hint :: <c-representation>,
     file :: <file-state>)
 => (name :: <string>, rep :: <c-representation>);
  aux-c-expr-and-rep(lit, file, modifier: "true");
end;

define method c-expr-and-rep
    (lit :: <literal-pair>, rep-hint :: <c-representation>,
     file :: <file-state>)
 => (name :: <string>, rep :: <c-representation>);
  // This is a messy special case.  There are situations in which we
  // pre-compute lists of methods and emit them as heap values.  Since
  // we can't be sure that nobody will call them directly, we must
  // create xeps for them, just in case.
  for (elem = lit then elem.literal-tail,
       while: instance?(elem, <literal-pair>))
    maybe-emit-entries(elem.literal-head, file);
  end for;
  aux-c-expr-and-rep(lit, file, modifier: "literal");
end method c-expr-and-rep;

define method c-expr-and-rep
    (lit :: <literal-empty-list>, rep-hint :: <c-representation>,
     file :: <file-state>)
 => (name :: <string>, rep :: <c-representation>);
  aux-c-expr-and-rep(lit, file, modifier: "empty_list");
end;

define method c-expr-and-rep
    (lit :: <literal-symbol>, rep-hint :: <c-representation>,
     file :: <file-state>)
 => (name :: <string>, rep :: <c-representation>);
  aux-c-expr-and-rep
    (lit, file,
     modifier:
       concatenate("SYM_",
	 string-to-c-name(as(<string>, lit.literal-value))));
end;

define method c-expr-and-rep
    (lit :: <literal-string>, rep-hint :: <c-representation>,
     file :: <file-state>)
 => (name :: <string>, rep :: <c-representation>);
  aux-c-expr-and-rep(lit, file, modifier: "str");
end;

define method c-expr-and-rep
    (class :: <defined-cclass>, rep-hint :: <c-representation>,
     file :: <file-state>)
 => (name :: <string>, rep :: <c-representation>);
  aux-c-expr-and-rep(class, file, name: class.cclass-name,
  		     defn: class.class-defn);
end;

define method c-expr-and-rep
    (lit :: <literal-true>, rep-hint :: <heap-representation>,
     file :: <file-state>)
    => (name :: <string>, rep :: <c-representation>);
  values("obj_True", rep-hint);
end;

define method c-expr-and-rep
    (lit :: <literal-false>, rep-hint :: <heap-representation>,
     file :: <file-state>)
    => (name :: <string>, rep :: <c-representation>);
  values("obj_False", rep-hint);
end;

define method c-expr-and-rep
    (lit :: <literal-true>, rep-hint :: <immediate-representation>,
     file :: <file-state>)
    => (name :: <string>, rep :: <c-representation>);
  values("TRUE", rep-hint);
end;

define method c-expr-and-rep
    (lit :: <literal-false>, rep-hint :: <immediate-representation>,
     file :: <file-state>)
    => (name :: <string>, rep :: <c-representation>);
  values("FALSE", rep-hint);
end;

define method c-expr-and-rep (lit :: <literal-integer>,
			      rep-hint :: <c-representation>,
			      file :: <file-state>)
    => (name :: <string>, rep :: <c-representation>);
  let val = lit.literal-value;
  // Can't use stringify, because val is an extended integer.
  values(if (val == ash(as(<extended-integer>, -1),
			*current-target*.platform-integer-length - 1))
	   // Some compilers (gcc) warn about minimum-fixed-integer.  So we
	   // print it in hex (assuming 2's compliment).
	   format-to-string("0x%x", -val);
	 else
	   format-to-string("%d", val);
	 end if,
	 pick-representation(dylan-value(#"<integer>"), #"speed"));
end;

define method c-expr-and-rep (lit :: <literal-single-float>,
			      rep-hint :: <immediate-representation>,
			      file :: <file-state>)
    => (name :: <string>, rep :: <c-representation>);
  values(float-to-string(lit.literal-value, 8),
	 pick-representation(dylan-value(#"<single-float>"), #"speed"));
end;

define method c-expr-and-rep (lit :: <literal-double-float>,
			      rep-hint :: <immediate-representation>,
			      file :: <file-state>)
    => (name :: <string>, rep :: <c-representation>);
  values(float-to-string(lit.literal-value, 16),
	 pick-representation(dylan-value(#"<double-float>"), #"speed"));
end;

define method c-expr-and-rep (lit :: <literal-extended-float>,
			      rep-hint :: <immediate-representation>,
			      file :: <file-state>)
    => (name :: <string>, rep :: <c-representation>);
  values(float-to-string(lit.literal-value, 35),
	 pick-representation(dylan-value(#"<extended-float>"), #"speed"));
end;

define method float-to-string (value :: <ratio>, digits :: <integer>)
    => res :: <string>;
  if (zero?(value))
    "0.0";
  else
    let stream = make(<buffered-byte-string-output-stream>);
    if (negative?(value))
      value := -value;
      write-element(stream, '-');
    end;
    let one = ratio(1, 1);
    let ten = ratio(10, 1);
    let one-tenth = one / ten;
    let (exponent, fraction)
      = if (value >= one)
	  for (exponent from 1,
	       fraction = value / ten then fraction / ten,
	       while: fraction >= one)
	  finally
	    values(exponent, fraction);
	  end;
	else
	  for (exponent from 0 by -1,
	       fraction = value then fraction * ten,
	       while: fraction < one-tenth)
	  finally
	    values(exponent, fraction);
	  end;
	end;
    write(stream, "0.");
    let zeros = 0;
    for (count from 0 below digits,
	 until: zero?(fraction))
      let (digit, remainder) = floor(fraction * ten);
      if (zero?(digit))
	zeros := zeros + 1;
      else
	for (i from 0 below zeros)
	  write-element(stream, '0');
	end;
	write-element(stream, as(<character>, as(<integer>, digit) + 48));
	zeros := 0;
      end;
      fraction := remainder;
    end;
    write-element(stream, 'e');
    print(exponent, stream);
    stream.stream-contents;
  end;
end;


define method c-expr-and-rep (lit :: <literal-character>,
			      rep-hint :: <c-representation>,
			      file :: <file-state>)
    => (name :: <string>, rep :: <c-representation>);
  let char = lit.literal-value;
  values(if (char == '\0')
	   "'\\0'";
	 elseif (char == '\b')
	   "'\\b'";
	 elseif (char == '\t')
	   "'\\t'";
	 elseif (char == '\n')
	   "'\\n'";
	 elseif (char == '\r')
	   "'\\r'";
	 elseif (char < ' ')
	   // Can't use stringify, because it insists on decimal.
	   format-to-string("'\\%o'", as(<integer>, char));
	 elseif (char == '\'' | char == '\\')
	   stringify("'\\", char, '\'');
	 elseif (char <= '~')
	   stringify('\'', char, '\'');
	 else
	   stringify(as(<integer>, char));
	 end,
	 pick-representation(dylan-value(#"<character>"), #"speed"));
end;

define method c-expr-and-rep
    (ep :: <ct-entry-point>, rephint :: <c-representation>, file :: <file-state>)
    => (name :: <string>, rep :: <c-representation>);
  let info = get-info-for(ep.ct-entry-point-for, file);
  let name = main-entry-c-name(info, file);
  maybe-emit-prototype(name, info, file);
  values(stringify("((void *)", name, ')'), *ptr-rep*);
end;


// Emit-copy

define generic emit-copy
    (target :: <string>, target-rep :: <c-representation>,
     source :: <string>, source-rep :: <c-representation>,
     file :: <file-state>)
    => ();

define method emit-copy
    (target :: <string>, target-rep :: <general-representation>,
     source :: <string>, source-rep :: <general-representation>,
     file :: <file-state>)
    => ();
  let stream = file.file-guts-stream;
  format(stream, "%s = %s;\n", target, source);
end;

define method emit-copy
    (target :: <string>, target-rep :: <general-representation>,
     source :: <string>, source-rep :: <c-data-word-representation>,
     file :: <file-state>)
    => ();
  let stream = file.file-guts-stream;
  let (proxy, proxy-rep)
    = c-expr-and-rep(make(<proxy>, for: source-rep.representation-class),
		     *heap-rep*, file);
  format(stream, "%s.heapptr = %s;\n",
	 target, conversion-expr(*heap-rep*, proxy, proxy-rep, file));
  format(stream, "%s.dataword.%s = %s;\n",
	 target, source-rep.representation-data-word-member, source);
end;

define method emit-copy
    (target :: <string>, target-rep :: <general-representation>,
     source :: <string>, source-rep :: <c-representation>,
     file :: <file-state>)
    => ();
  let stream = file.file-guts-stream;
  let heapptr = conversion-expr(*heap-rep*, source, source-rep, file);
  format(stream, "%s.heapptr = %s;\n", target, heapptr);
  format(stream, "%s.dataword.l = 0;\n", target);
end;

define method emit-copy
    (target :: <string>, target-rep :: <c-representation>,
     source :: <string>, source-rep :: <c-representation>,
     file :: <file-state>)
    => ();
  let stream = file.file-guts-stream;
  let expr = conversion-expr(target-rep, source, source-rep, file);
  format(stream, "%s = %s;\n", target, expr);
end;



// conversion-expr

define method conversion-expr
    (target-rep :: <general-representation>,
     source :: <string>, source-rep :: <c-representation>,
     file :: <file-state>)
    => res :: <string>;
  if (target-rep == source-rep)
    source;
  else
    let temp = new-local(file, modifier: "temp");
    format(file.file-vars-stream, "%s %s;\n",
	   target-rep.representation-c-type, temp);
    emit-copy(temp, target-rep, source, source-rep, file);
    temp;
  end;
end;

define method conversion-expr
    (target-rep :: <c-representation>,
     source :: <string>, source-rep :: <c-representation>,
     file :: <file-state>)
    => res :: <string>;
  if (target-rep == source-rep)
    source;
  elseif (target-rep.representation-depth < source-rep.representation-depth)
    let to-more-general = source-rep.representation-to-more-general;
    conversion-expr(target-rep,
		    select (to-more-general)
		      #t => source;
		      #f => error("Can't happen.");
		      otherwise => format-to-string(to-more-general, source);
		    end,
		    source-rep.more-general-representation,
		    file);
  else
    let from-more-general = target-rep.representation-from-more-general;
    let more-general = conversion-expr(target-rep.more-general-representation,
				       source, source-rep, file);
    select (from-more-general)
      #t => more-general;
      #f => error("Can't happen.");
      otherwise => format-to-string(from-more-general, more-general);
    end;
  end;
end;


// Seals for file cback.dylan

// <unit-state> -- subclass of <object>
define sealed domain make(singleton(<unit-state>));
define sealed domain initialize(<unit-state>);
// <root> -- subclass of <object>
define sealed domain make(singleton(<root>));
define sealed domain initialize(<root>);
// <file-state> -- subclass of <object>
define sealed domain make(singleton(<file-state>));
define sealed domain initialize(<file-state>);
// <backend-var-info> -- subclass of <object>
define sealed domain make(singleton(<backend-var-info>));
define sealed domain initialize(<backend-var-info>);
// <function-info> -- subclass of <object>
define sealed domain make(singleton(<function-info>));
define sealed domain initialize(<function-info>);
// <constant-info> -- subclass of <object>
define sealed domain make(singleton(<constant-info>));
define sealed domain initialize(<constant-info>);
// <constant-function-info> -- subclass of <constant-info>, <function-info>
define sealed domain make(singleton(<constant-function-info>));
// <constant-method-info> -- subclass of <constant-function-info>
define sealed domain make(singleton(<constant-method-info>));
// <pending-define> -- subclass of <object>
define sealed domain make(singleton(<pending-define>));
define sealed domain initialize(<pending-define>);
