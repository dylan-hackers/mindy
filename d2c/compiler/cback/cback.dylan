module: cback
rcs-header: $Header: /scm/cvs/src/d2c/compiler/cback/cback.dylan,v 1.46 2003/02/19 23:29:57 gabor Exp $
copyright: see below

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000, 2001, 2002  Gwydion Dylan Maintainers
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
//    University, and the Gwydion Dylan Maintainers.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
// Also, see http://www.gwydiondylan.org/ for updates and documentation. 
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
//      into "source". May be a zero to several lines of C.
//   conversion-expr(target-rep :: <c-representation>,
//		     source :: <string>, source-rep :: <c-representation>,
//		     file :: <file-state>) => res :: <string>;
//      converts a C expression to another C expression changing the
//      representation as required. May introduce temporaries on the way.
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
  /* exported */ slot unit-prefix :: <byte-string>;
  //  required-init-keyword: prefix:;
  //
  // keeps track of names used already.
  slot unit-global-table :: <table> = make(<string-table>);
  //
  // Vector of the initial values for the roots vector.
  /* exported */ slot unit-init-roots :: <stretchy-vector> = make(<stretchy-vector>);
  //
  // Vector of the ctvs we want to force into the local heap irrespective of
  // whether or not they are actually referenced.  We do this for things we
  // are optimistic about being referenced someplace but don't want to have
  // to wait until the global heap to dump.
  /* exported */ slot unit-eagerly-reference :: <stretchy-vector> = make(<stretchy-vector>);
end;

define method initialize(obj :: <unit-state>, #next next-method, #key prefix) => ()
  next-method();
  obj.unit-prefix := string-to-c-name(prefix);
end method initialize;

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
  slot file-includes-exist-for :: <string-table> = make(<string-table>);
  //
  // Things we have already spewed defns for.
  slot file-prototypes-exist-for :: <string-table> = make(<string-table>);
  //
  // Maps from vectors of C type name strings to the name of an already-defined
  // structure type which can be used to return those multiple values.
  slot file-result-structures :: <equal-table> = make(<equal-table>);
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
  slot file-vars-stream :: <stream>
  	    = make-indenting-string-stream(indentation: $indentation-step);
  slot file-guts-stream :: <stream>
        = make-indenting-string-stream(indentation: $indentation-step);
  //
  // Whenever the guts stream buffer exceeds 64K, we push the contents here and
  // empty the stream.  In addition to being more efficient, this avoids object
  // size limitations in Mindy.
  slot file-guts-overflow :: <stretchy-vector> = make(<stretchy-vector>, size: 0);
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
  slot file-local-table :: <string-table> = make(<string-table>);
  //
  // table of all local vars allocated, whether in use or not
  slot file-local-vars :: <string-table> = make(<string-table>);
  //
  // a list of local variables that are know to not be in use at a
  // particular time, and therefor available for reuse
  slot file-freed-locals :: false-or(<local-var>) = #f;
  //
  // we keep track of the components for all the xeps that we lazily generated
  // and dump them after the referencing component has compiled, since we are
  // in the middle of generating code when we discover that we need them.
  slot file-deferred-xeps :: <sequence> = make(<deque>);
  //
  // we save the last <source-location> emitted to the file to avoid multiple
  // identical #line tokens.  Used by maybe-emit-source-location.
  slot file-source-location :: <source-location> = make(<unknown-source-location>);

  // <heap-file-state> -- internal.
  // 
  // A catch-all object to quantify the state of the "heap output" process.
  // Almost every routine in this module accepts a "state" argument and
  // destructively modifies it as necessary to account for its actions.
  //
  //
  // Layouts we have already spewed struct declarations for.
  constant slot file-layouts-exist-for :: <string-table>
    = make(<string-table>);
  //
  // The prefix we are pre-pending to each symbol to guarantee uniqueness.
  constant slot id-prefix :: <byte-string> = "L", init-keyword: #"id-prefix";
  //
  // The id counter used to generate unique names when we aren't dumping the
  // object ourselves (and therefore cannot reference it relative to the
  // heap base).
  slot next-id :: <integer> = 0;
  //
  // A queue of objects that we have decided to dump but haven't gotten
  // around to dumping yet.  Used as a fifo so that we can use the the
  // heap-size at the time of queueing as the object offset.
  constant slot object-queue :: <deque> = make(<deque>);

  // <global-heap-state> -- internal.
  //
  // The additional information needed while dumping the final global heap.
  // 
  //
  // When dumping symbols, we chain them together.  This holds the current
  // head of that chain.
  slot symbols :: type-union(<literal-false>, <literal-symbol>) = make(<literal-false>);

  // <local-heap-file-state> -- internal.
  //
  // The additional information needed while dumping a library local heap.
  // 
  //
  // Holds the objects that have been referenced but are not going to be
  // dumped until the global heap is dumped.
  constant slot undumped-objects :: <stretchy-vector> = make(<stretchy-vector>);
  //
  // Holds the extra labels we've had to allocate for externally defined
  // ctvs.
  constant slot extra-labels :: <stretchy-vector> = make(<stretchy-vector>);

  slot dumping-global-heap? :: <boolean> = #f;
  constant slot single-file-mode? :: <boolean> = #f, init-keyword: single-file-mode?:;
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
define function maybe-emit-include
    (name :: <byte-string>, file :: <file-state>, #key left = '<', right = '>')
  unless (element(file.file-includes-exist-for, name, default: #f))
    format(file.file-body-stream, "#include %s%s%s\n\n", left, name, right);
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
// we use this routine to clobber all occurrences of */ before actually writing
// the comment. We also clobber ``/*'' because some C compilers warn about nested
// comments if they encounter it.
// 
define method clean-for-comment
    (string :: <byte-string>, #key copy? :: <boolean> = #t)
    => res :: <byte-string>;
    let len = string.size;
    unless (len < 2)
      let previous-is-star? = string[0] == '*';
      for (index :: <integer> from 1 below len)
        local method punch(at :: <integer>) => ();
            if (copy?)
              string := copy-sequence(string);
              copy? := #f;
            end if;
            string[at] := 'X';
            string[at + 1] := 'X';
          end method punch;

        let char = string[index];

        if (char == '*')
          previous-is-star? := #t;
        elseif (char == '/')
          if (previous-is-star?)
            previous-is-star? := #f;
            punch(index - 1);
          elseif (index + 1 < len & string[index + 1] == '*')
            punch(index);
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
define constant $c-name-transform :: <simple-object-vector>
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

  let total-size = 0;
  for (ch in str)
    let mapping :: <byte-string> = $c-name-transform[as(<integer>, ch)];
    total-size := total-size + mapping.size;
  end;

  let res = make(<byte-string>, size: total-size);
  let res-idx = 0;
  for (ch in str)
    let mapping :: <byte-string> = $c-name-transform[as(<integer>, ch)];
    let sz = mapping.size;
    for (i from 0 below sz)
      res[res-idx + i] := mapping[i];
    end;
    res-idx := res-idx + sz;
  end for;
  res;
end function;


// Based upon a <name> object, compute a string suitable for use as a C
// variable name,
//
define generic c-name (name :: <name>) => (result :: <byte-string>);

define method c-name (name :: <basic-name>) => (result :: <byte-string>);
  let mod-name = string-to-c-name(as(<string>, name.name-module.module-name));
  let def-name :: <byte-string> = as(<string>, name.name-symbol);

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
		#"callback-entry" => "_CALLBACK";
		#"discriminator" => "_DISCRIM";
		#"deferred-evaluation" => "_DEFER";
		#"init-function" => "_INIT";
		#"setter" => "_SETTER";
		#"getter" => "_GETTER";
		#"type-cell" => "_TYPE";
		#"maker" => "_MAKER";
		#"key-defaulter" => "_KEYWORD";
		#"class-meta" => "_CLASS_META";
		#"each-subclass-meta" => "_EACH_META";
	      end select);
end method;

define method c-name (name :: <internal-name>) => res :: <byte-string>;
  concatenate(name.internal-name-base.c-name, "_INT_",
  	      string-to-c-name(as(<string>, name.internal-name-symbol)));
end method;

// really shouldn't be any unknown source locations here, but we'll get to that
// later... FIXME
define method c-name (name :: <anonymous-name>) => res :: <byte-string>;
  let loc = name.anonymous-name-location;
  if (instance?(loc, <known-source-location>))
    format-to-string("LINE_%d", loc.start-line);
  else
    "UNKNOWN";
  end;
end method;

define generic c-name-global (name :: <name>) => (result :: <byte-string>);

define method c-name-global (name :: <name>) => (result :: <byte-string>);
  let library-name
    = string-to-c-name(as(<string>,
			  name.name-module.module-home.library-name));
  concatenate(library-name, "Z", c-name(name));
end method;

define method c-name-global (name :: <derived-name>) => (result :: <byte-string>);
  let base-name = name.derived-name-base;
  let library-name
    = string-to-c-name(as(<string>,
			  base-name.name-module.module-home.library-name));
  concatenate(library-name, "Z", c-name(name));
end method;

// Emit a description of the <source-location> in C.  For 
// <known-source-location>s, this will be a #line directive.  For
// other types of <source-location>, this will be a comment.

define method maybe-emit-source-location(source-loc :: <known-source-location>,
				   file :: <file-state>) => ();
  if (file.file-source-location ~= source-loc)
    format(file.file-guts-stream, "\n/* #line %d \"%s\" */\n",
	   source-loc.end-line, source-loc.source.full-file-name); // FIXME
    file.file-source-location := source-loc;
  end if;
end method;

define method maybe-emit-source-location(source-loc :: <source-location>,
				   file :: <file-state>) => ();
  if (file.file-source-location ~= source-loc)
    format(file.file-guts-stream, "/* #line %= */\n", object-class(source-loc));
    file.file-source-location := source-loc;
  end if;
end method;


// New-{scope}
//
// The new-.... routines all allocate new identifiers which are
// guranteed to be unique in the given scope.  The "name" and
// "modifier" keywords may be used to provide more meaningful names,
// but their usage is idiosyncratic.
//========================================================================

define function contact-bgh() => ();
  dformat
    ("\n\n\n\n%s\n%s\n%s\n%s\n%s\n\n\n\n",
     "Could you please contact bruce@hoult.org and provide a copy of",
     "the program you are compiling.  There is no problem with your",
     "program but it is doing something that I didn't think could",
     "happen, or don't have a test case for and I'd like to use it to",
     "help improve the compiler.  Thank you!");
end;

define inline function contact-bgh-if(test :: <boolean>)
 => ();
  if (test) contact-bgh() end;
end;

define constant <temp-locals-list> = <list>;

define inline function contact-bgh-unless-empty(temps :: <temp-locals-list>)
 => ();
  unless (temps.empty?) contact-bgh() end;
end;


define class <local-var> (<object>)
  constant slot local-name :: <byte-string>,
    required-init-keyword: name:;
  constant slot base-name :: <byte-string>,
    required-init-keyword: base-name:;
  constant slot seq :: <integer>,
    required-init-keyword: seq:;
  constant slot rep :: false-or(<byte-string>),
    required-init-keyword: rep:;
  slot next :: false-or(<local-var>) = #f;
end <local-var>;


define function new-local
    (file :: <file-state>,
     #key name :: <byte-string> = "L_",
     modifier :: <byte-string> = "anon",
     wanted-rep :: false-or(<byte-string>) = #f,
     dont-add? :: <boolean> = #f,
     comment :: false-or(<byte-string>) = #f)
 => res :: <byte-string>;
  block (return)
    let result = stringify(name, modifier);
    if (wanted-rep)
      // no point even trying unless they told us what type of
      // C variable we need...
      for(prev = #f then curr,
          curr = file.file-freed-locals then curr.next,
          while: curr)
        let curr-rep = curr.rep;
        // we only really need to make sure the c-rep is the
        // same, but it makes it easier to read the generated
        // code if the base-name is the same too :-)
        if (curr-rep
              & curr.base-name = result
              & curr-rep = wanted-rep)
          if (prev)
            prev.next := curr.next
          else
            file.file-freed-locals := curr.next
          end;
          //dformat("reusing %= ", curr.local-name);
          return(curr.local-name);
        end if;
      end for;
    end if;

    let last-num :: <integer>
      = element(file.file-local-table, result, default: 0);
    let num = last-num + 1;
    let local-name =
      if (num == 1)
        result
      else
        stringify(result, '_', num)
      end;
    file.file-local-table[result] := num;
    file.file-local-vars[local-name] :=
      make(<local-var>,
           name: local-name,
           base-name: result,
           seq: num,
           rep: wanted-rep);
    unless (dont-add?)
      let stream = file.file-vars-stream;
      format(stream, "%s %s;", wanted-rep, local-name);
      if (comment)
        format(stream, " /* %s */", comment);
      end;
      new-line(stream);
    end;
    local-name;
  end block;
end;


define function free-temp(name :: <string>, file :: <file-state>)
 => ();
  //dformat("var %s freed\n", name);
//  let var :: <local-var> = file.file-local-vars[name];
//  var.next := file.file-freed-locals;
//  file.file-freed-locals := var;
end;

define inline function free-temp-if
    (temp? :: <boolean>, name :: <string>, file :: <file-state>)
 => ();
  if (temp?)
    // this is true 0.5% of the time, when compiling d2c
    free-temp(name, file);
  end;
end;

// Compiling d2c, the average number of items in the list is 0.3:
//
//  76% of the time the list is empty
//  19% of the time the list has a single item
//  3.5% of the time the list has two items
//  1.5% of the time there are more than two
//
// Conclusion: a list does six times less consing than a stretchy!!

define function free-temps(names :: <temp-locals-list>, file :: <file-state>)
 => ();
  for (name in names)
    free-temp(name, file);
  end;
end;

define inline function make-temp-locals-list()
 => (list :: <temp-locals-list>);
  #();
end;


// If the name is unique? then no _number is used, and the 
// name must be unique without any such suffix.  Name can be #f if there is no
// <name> object, but you then must supply a modifier.
//
define function new-c-global
    (name :: false-or(<name>), file :: <file-state>, 
     #key modifier :: <string> = "")
 => res :: <byte-string>;
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
end function new-c-global;


// Add a new root description.
//
define function new-root
    (init-value :: false-or(<ct-value>), name :: <byte-string>,
     file :: <file-state>, #key comment :: false-or(<byte-string>))
 => ();
  let unit = file.file-unit;
  let roots = unit.unit-init-roots;
  let root = make(<root>, init-value: init-value, name: name,
		  comment: comment);
  add!(roots, root);
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

define function cluster-names (depth :: <integer>)
    => (bottom-name :: <string>, top-name :: <string>);
  if (zero?(depth))
    values("orig_sp", "cluster_0_top");
  else
    values(stringify("cluster_", depth - 1, "_top"),
	   stringify("cluster_", depth, "_top"));
  end;
end;

define function consume-cluster
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

// Write out a C prototype for an object with the given name.  More
// information about the object will be provided by the "info"
// parameter.  The exact behavior depends upon "info"s type.
//
define function maybe-emit-prototype
    (name :: <byte-string>, info :: <object>, file :: <file-state>)
    => did :: <boolean>;
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

// We know that the object is located in the global heap,
// so we do not take the descriptor.heapptr route.
define method emit-prototype-for
    (name :: <byte-string>, heap-info == #"heap",
     file :: <file-state>)
    => ();
  let stream = file.file-body-stream;
  format(stream, "extern struct heapobj %s;\n\n", name);
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
define function c-name-and-rep
    (leaf :: <abstract-variable>,
     // ### Should really be ssa-variable
     file :: <file-state>,
     #key dont-add :: <boolean>,
	  prefix :: <byte-string> = "L_")
    => (name :: <byte-string>, rep :: <c-representation>);
  let info = get-info-for(leaf, file);
  let name = info.backend-var-info-name;
  unless (name)
    let c-type = info.backend-var-info-rep.representation-c-type;
    if (instance?(leaf.var-info, <debug-named-info>))
      let comment =
        ~dont-add & leaf.var-info.debug-name.clean-for-comment;
      let dname = string-to-c-name(as(<string>, leaf.var-info.debug-name));
      name := new-local(file,
                        name: prefix,
                        modifier: dname,
                        wanted-rep: c-type,
                        comment: comment,
                        dont-add?: dont-add);
    else
      name := new-local(file,
                        name: prefix,
                        wanted-rep: c-type,
                        dont-add?: dont-add);
    end if;
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
// method definition can be a value distinct from the underlying main entry.
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
	      add!(reps, pick-representation(boolean-ctype(), #"speed"));
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
    error("Too late for making an info for %=", entry.ct-entry-point-for);
  end unless;
  let name
    = select (entry.ct-entry-point-kind)
	#"main" => info.function-info-main-entry-c-name;
	#"general" => info.function-info-general-entry-c-name;
	#"generic" => info.function-info-generic-entry-c-name;
	#"callback" => info.function-info-callback-entry-c-name;
      end select;
  unless (name)
    error("Too late for picking a name for %=", entry);
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
    let (entry, component) = build-xep-component(*current-optimizer*, ctv, #t);
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
    let (entry, component) = build-xep-component(*current-optimizer*, ctv, #f);
    let entry-info = get-info-for(entry, file);
    // We've already allocated a meaningful name for this entry, so
    // we want copy it into the entry's info.
    entry-info.function-info-main-entry-c-name := name;
    ctv.has-general-entry? := #t;
    push-last(file.file-deferred-xeps, component);
  end if;
  name;
end method maybe-emit-general-entry;

define method maybe-emit-callback-entry
    (ctv :: <ct-callback-function>, file :: <file-state>)
 => (name :: <string>);
  let info = get-info-for(ctv, file);
  let name = callback-entry-c-name(info, file);
  if (~ctv.has-callback-entry?)
    let (entry, component) = build-xep-component(*current-optimizer*, ctv, #f);
    let entry-info = get-info-for(entry, file);
    // We've already allocated a meaningful name for this entry, so
    // we want copy it into the entry's info.
    entry-info.function-info-callback-entry-c-name := name;
    ctv.has-callback-entry? := #t;
    push-last(file.file-deferred-xeps, component);
  end if;
  name;
end method maybe-emit-callback-entry;


define method maybe-emit-entries
    (ctv :: <object>, file :: <file-state>) => ();
  #f;
end method maybe-emit-entries;

define method maybe-emit-entries
    (ctv :: <ct-function>, file :: <file-state>) => ();
  maybe-emit-general-entry(ctv, file);
end method maybe-emit-entries;

define method maybe-emit-entries
    (ctv :: <ct-callback-function>, file :: <file-state>) => ();
  maybe-emit-callback-entry(ctv, file);
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

define class <constant-callback-function-info> (<constant-function-info>)
  slot function-info-callback-entry-c-name :: false-or(<byte-string>),
    init-value: #f, init-keyword: callback-entry-c-name:;
end;

define constant $constant-callback-function-info-slots
  = concatenate($constant-function-info-slots,
		list(function-info-callback-entry-c-name,
		     callback-entry-c-name:,
		     #f));

add-make-dumper(#"constant-callback-function-info", *compiler-dispatcher*,
		<constant-callback-function-info>,
		$constant-callback-function-info-slots);

define method callback-entry-c-name
    (info :: <constant-callback-function-info>, file :: <file-state>)
    => res :: <byte-string>;
  info.function-info-callback-entry-c-name
    | (info.function-info-callback-entry-c-name
	 := new-c-global(info.function-info-name, file, modifier: "_CALLBACK"));
end;

define method make-info-for
    (ctv :: <ct-callback-function>, file :: false-or(<file-state>))
    => res :: <constant-callback-function-info>;
  make-function-info(<constant-callback-function-info>, ctv.ct-function-name,
		     ctv.ct-function-signature,
		     ctv.ct-function-closure-var-types);
end;


// File prologue and epilogue

define method emit-prologue
    (file :: <file-state>, other-units :: <simple-object-vector>)
    => ();
  maybe-emit-include("stddef.h", file);
  maybe-emit-include("stdlib.h", file);

  if (instance?(*double-rep*, <c-data-word-representation>))
    format(file.file-body-stream, "#define GD_DATAWORD_D\n");
  end;
  if (instance?(*long-double-rep*, <c-data-word-representation>))
    format(file.file-body-stream, "#define GD_DATAWORD_X\n");
  end;
  if (*current-target*.long-long-size)
    format(file.file-body-stream, "#define GD_HAVE_LONG_LONG\n");
  end;
  
  maybe-emit-include("runtime.h", file, left: '"', right: '"');
 
  // The most important thing math.h includes is a prototype for rint,
  // although it helps if we ever want to inline functions in the
  // Transcendental library
  maybe-emit-include("math.h", file);

  let stream = file.file-body-stream;
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

  format(stream, "#define GD_CTASSERT(name, x) \\\n"
                 "    typedef char gd_assert_ ## name[(x) ? 1 : -1];\n");

  format(stream, "#define GD_VERIFY_SIZE_ASSUMPTION(name, type, size)\\\n");
  format(stream, "    GD_CTASSERT(size_ ## name, sizeof(type) == (size))\n");

  format(stream, "#define GD_VERIFY_ALIGN_ASSUMPTION(name, type, align)\\\n");
  format(stream, "    typedef struct { char c; type x; } \\\n");
  format(stream, "      gd_align_ ## name; \\\n");
  format(stream, "    GD_CTASSERT(align_ ## name, "
                 "offsetof(gd_align_ ## name, x) == (align))\n\n");

  for(c-type in #[#"general", #"heap", #"boolean", #"long-long", #"long",
                  #"int", #"unsigned-int", #"short", #"unsigned-short",
                  #"float", #"double", #"long-double", #"ptr"])
    let rep = c-rep(c-type);
    if (rep)
      format(stream, "GD_VERIFY_SIZE_ASSUMPTION(%s, %s, %d);\n",
             string-to-c-name(as(<string>, rep.representation-name)),
             rep.representation-c-type, rep.representation-size);
      format(stream, "GD_VERIFY_ALIGN_ASSUMPTION(%s, %s, %d);\n",
             string-to-c-name(as(<string>, rep.representation-name)),
             rep.representation-c-type, rep.representation-alignment);
    end if;
  end for;
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
  format(bstream, "}\n\n");

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
  format(bstream, "}\n\n");

  begin
    let cclass = specifier-type(#"<double-integer>");
    let c-type = cclass.direct-speed-representation.representation-c-type;
    format(bstream, "heapptr_t make_double_integer(%s value)\n{\n", c-type);
    format(gstream, "heapptr_t res = allocate(%d);\n",
           cclass.instance-slots-layout.layout-length);

    let (expr, rep) = c-expr-and-rep(cclass, *heap-rep*, file);
    let (c-code, temp?) = conversion-expr(*heap-rep*, expr, rep, file);
    format(gstream, "SLOT(res, heapptr_t, %d) = %s;\n",
           dylan-slot-offset(cclass, #"%object-class"),
           c-code);
    let value-offset = dylan-slot-offset(cclass, #"value");
    format(gstream, "SLOT(res, %s, %d) = value;\n", c-type, value-offset);
    format(gstream, "return res;\n");
    write(bstream, get-string(gstream));
    format(bstream, "}\n\n");

    format(bstream, "%s double_integer_value(heapptr_t df)\n{\n", c-type);
    format(gstream, "return SLOT(df, %s, %d);\n", c-type, value-offset);
    write(bstream, get-string(gstream));
    format(bstream, "}\n\n");
  end;

  unless (instance?(*double-rep*, <c-data-word-representation>))
    let cclass = specifier-type(#"<double-float>");
    format(bstream, "heapptr_t make_double_float(double value)\n{\n");
    format(gstream, "heapptr_t res = allocate(%d);\n",
	   cclass.instance-slots-layout.layout-length);

    let (expr, rep) = c-expr-and-rep(cclass, *heap-rep*, file);
    let (c-code, temp?) = conversion-expr(*heap-rep*, expr, rep, file);
    format(gstream, "SLOT(res, heapptr_t, %d) = %s;\n",
	   dylan-slot-offset(cclass, #"%object-class"),
	   c-code);

    let value-offset = dylan-slot-offset(cclass, #"value");
    format(gstream, "SLOT(res, double, %d) = value;\n", value-offset);
    format(gstream, "return res;\n");
    write(bstream, get-string(gstream));
    format(bstream, "}\n\n");

    format(bstream, "double double_float_value(heapptr_t df)\n{\n");
    format(gstream, "return SLOT(df, double, %d);\n", value-offset);
    write(bstream, get-string(gstream));
    format(bstream, "}\n\n");
  end;

  unless (instance?(*long-double-rep*, <c-data-word-representation>))
    let cclass = specifier-type(#"<extended-float>");
    format(bstream, "heapptr_t make_extended_float(long double value)\n{\n");
    format(gstream, "heapptr_t res = allocate(%d);\n",
	   cclass.instance-slots-layout.layout-length);

    let (expr, rep) = c-expr-and-rep(cclass, *heap-rep*, file);
    let (c-code, temp?) = conversion-expr(*heap-rep*, expr, rep, file);
    format(gstream, "SLOT(res, heapptr_t, %d) = %s;\n",
	   dylan-slot-offset(cclass, #"%object-class"),
	   c-code);

    let value-offset = dylan-slot-offset(cclass, #"value");
    format(gstream, "SLOT(res, long double, %d) = value;\n", value-offset);
    format(gstream, "return res;\n");
    write(bstream, get-string(gstream));
    format(bstream, "}\n\n");

    format(bstream, "long double extended_float_value(heapptr_t xf)\n{\n");
    format(gstream, "return SLOT(xf, long double, %d);\n", value-offset);
    write(bstream, get-string(gstream));
    format(bstream, "}\n\n");
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
  new-line(file.file-body-stream);

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
    (defn.method-defn-inline-type == #"inline-only") => #t;
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
  new-line(file.file-body-stream);
  let ctv = defn.ct-value;
  if (ctv)
    if (ctv.all-subclasses-known?)
      //
      // By adding sealed classes to the heap now, we save effort doing the
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
  let deferred = defn.class-defn-deferred-evaluations-function;
  if (deferred) maybe-emit-entries(deferred, file) end if;
  let defaulter = defn.class-defn-key-defaulter-function;
  if (defaulter) maybe-emit-entries(defaulter, file) end if;
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
  new-line(file.file-body-stream);
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
      let (c-code, temp?)
        = conversion-expr(rep, init-value-expr, init-value-rep, file);
      format(stream, "%s;\t/* %s */\n",
	     c-code,
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
      if (instance?(func-lit, <callback-literal>) & func-lit.callback-entry)
	let gen-info = get-info-for(func-lit.callback-entry, file);
	if (gen-info.function-info-main-entry-c-name)
	  error("%= already has a name?", func-lit.callback-entry);
	end;
	gen-info.function-info-main-entry-c-name
	  := callback-entry-c-name(ctv-info, file);
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
  file.file-local-vars := make(<string-table>);
  file.file-freed-locals := #f;
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

  maybe-emit-source-location(function.source-location, file);
  emit-region(function.body, file);

  let stream = file.file-body-stream;
  format(stream, "/* %s */\n", function.name.clean-for-comment);

  format(stream, "%s\n{\n",
	 compute-function-prototype(function, function-info, file));
  if(function.calling-convention == #"callback")
    format(file.file-vars-stream,
	   "descriptor_t *orig_sp = allocate_stack();\n");
  end if;
  write(stream, get-string(file.file-vars-stream));
  new-line(stream);

  // Actually write out the (already generated) code:
  let overflow = file.file-guts-overflow;
  unless (overflow.empty?)
    for (string in overflow)
      write(stream, string);
    end;
    overflow.size := 0;
  end unless;
  write(stream, get-string(file.file-guts-stream));
  format(stream, "}\n\n");
end;

define method compute-function-prototype
    (function :: false-or(<fer-function-region>),
     function-info :: <function-info>,
     file :: <file-state>)
    => res :: <byte-string>;
  let c-name = if(instance?(function-info, <constant-callback-function-info>)
		    & ~function)
		 callback-entry-c-name(function-info, file);
	       else
		 main-entry-c-name(function-info, file);
	       end if;
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
  format(stream, " %s(", c-name);
  let sp-arg? = if(function)
		  function.calling-convention == #"standard";
		else
		  ~instance?(function-info, <constant-callback-function-info>);
		end if;
  if (sp-arg?)
    write(stream, "descriptor_t *orig_sp");
  end if;
  for (rep in function-info.function-info-argument-representations,
       index from 0,
       first-arg = ~sp-arg? then #f,
       var = function & function.prologue.dependents.dependent.defines
	 then var & var.definer-next)
    if(~first-arg)
      write(stream, ", ");
    end if;
    format(stream, "%s ", rep.representation-c-type);
    let preferred-names = function & function.prologue.preferred-names;
    let preferred-name = preferred-names
			 & element(preferred-names, index, default: #f);
    if (preferred-name)
      format(stream, "%s", preferred-name);
    else
      format(stream, "A%d", index);
    end;
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

define function pick-result-structure
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

    maybe-emit-source-location(assign.source-location, file);

    emit-assignment(assign.defines, assign.depends-on.source-exp, 
		    assign.source-location, file);
    if (byte-string.stream-size >= 65536)
      add!(file.file-guts-overflow, byte-string.stream-contents);
    end if;
  end for;
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
  let (cond, temp?) = ref-leaf(*boolean-rep*, if-region.depends-on.source-exp, file);
  contact-bgh-if(temp?);
  spew-pending-defines(file);
  format(stream, "if (%s) {\n", cond);
  indent(stream, $indentation-step);
  emit-region(if-region.then-region, file);
  /* ### emit-joins(if-region.join-region, file); */
  spew-pending-defines(file);
  indent(stream, -$indentation-step);
  format(stream, "}\n");

  unless (instance?(if-region.else-region, <empty-region>))
    // We need to detect places where we can use "else if" rather than
    // "else { if" because if we don't, we'll break the parsers on
    // crappy C compilers (such as Microsoft Visual C++)
    let clause-is-an-elseif = if-region.else-region.elseif-able?;
    
    if (clause-is-an-elseif)
      write(stream, "else ");
    else
      format(stream, "else {\n");
      indent(stream, $indentation-step);
    end if;
    
    emit-region(if-region.else-region, file);
    /* ### emit-joins(if-region.join-region, file); */
    
    spew-pending-defines(file);
    if (~clause-is-an-elseif)
      indent(stream, -$indentation-step);
      format(stream, "}\n");
    end if;
  end unless;
end method emit-region;


define method emit-region (region :: <loop-region>,
			   file :: <file-state>)
    => ();
  /* ### emit-joins(region.join-region, file); */
  spew-pending-defines(file);
  let stream = file.file-guts-stream;
  format(stream, "while (1) {\n");
  indent(stream, $indentation-step);
  emit-region(region.body, file);
  /* ### emit-joins(region.join-region, file); */
  spew-pending-defines(file);
  indent(stream, -$indentation-step);
  format(stream, "}\n");
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
      error("Delivering a cluster that isn't at the bottom of the frame? (%s)", bottom-name);
    end;
    spew-pending-defines(file);
    format(stream, "return %s;\n", top-name);
  else
    let temps = make-temp-locals-list();
    for (dep = results then dep.dependent-next,
	 count from 0,
	 while: dep)
      let (leaf, temp?) = ref-leaf(*general-rep*, dep.source-exp, file);
      if (temp?) temps := add!(temps, leaf) end;
      format(stream, "orig_sp[%d] = %s;\n", count, leaf);
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
  let (expr, temp?) = ref-leaf(result-rep, return.depends-on.source-exp, file);
  spew-pending-defines(file);
  format(stream, "return %s;\n", expr);
end;

//define method emit-return
//    (return :: <return>, result-rep == #(), file :: <file-state>)
//    => ();
//  spew-pending-defines(file);
//  format(file.file-guts-stream, "return;\n");
//end;

define method emit-return
    (return :: <return>, result-reps :: <sequence>, file :: <file-state>)
    => ();
  if (result-reps.empty?)
    spew-pending-defines(file);
    format(file.file-guts-stream, "return;\n");
  else
    let stream = file.file-guts-stream;
    let temps = make-temp-locals-list();
    let function = return.block-of;
    let function-info = get-info-for(function, file);
    let c-type = concatenate("struct ", pick-result-structure(result-reps, file));
    let ret-val = new-local(file, modifier: "temp", wanted-rep: c-type);
    temps := add!(temps, ret-val);

    for (rep in result-reps,
	 index from 0,
	 dep = return.depends-on then dep.dependent-next)
      let (leaf, temp?) = ref-leaf(rep, dep.source-exp, file);
      if (temp?) temps := add!(temps, leaf) end;
      format(stream, "%s.R%d = %s;\n", ret-val, index, leaf);
    end;

    spew-pending-defines(file);
    format(stream, "return %s;\n", ret-val);
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
			       source-location :: <source-location>,
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
      let (leaf, temp?) = ref-leaf(rep, var, file);
      // bgh
      // This deliver-result had now-dammit=#f, but we can't allow that if
      // we're freeing the temp.
      // Ideally, we'd like to make the temp pending, which would require
      // telling deliver-result that it *is* a temp, so that if it makes the copy
      // pending it can store that fact in the <pending-define> object and free
      // up the temp when it goes non-pending.  Or, maybe it's not worth the
      // bother?
      deliver-result(defines, leaf, rep, temp?, file);
    end;
  end;
end;

define method emit-assignment (defines :: false-or(<definition-site-variable>),
			       expr :: <literal-constant>,
			       source-location :: <source-location>,
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
			       source-location :: <source-location>,
			       file :: <file-state>)
    => ();
  let (leaf, temp?) = ref-leaf(*heap-rep*, leaf, file);
  contact-bgh-if(temp?);
  deliver-result(defines, leaf, *heap-rep*, #f, file);
end;

define method emit-assignment (defines :: false-or(<definition-site-variable>),
			       leaf :: <definition-constant-leaf>,
			       source-location :: <source-location>,
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
                               source-location :: <source-location>,
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
     source-location :: <source-location>,
     file :: <file-state>)
    => ();
  let stream = file.file-guts-stream;
  let temps = make-temp-locals-list();

  let use-generic-entry?
    = instance?(call, <general-call>) & call.use-generic-entry?;

  let (next-info, arguments, next-info-temp?)
    = if (use-generic-entry?)
	let dep = call.depends-on.dependent-next;
        let (leaf, temp?) = ref-leaf(*heap-rep*, dep.source-exp, file);
        contact-bgh-if(temp?);
	values(leaf, dep.dependent-next, temp?);
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
          let (leaf, temp?) = ref-leaf(*general-rep*, arg-dep.source-exp, file);
          if (temp?) temps := add!(temps, leaf) end;
	  format(setup-stream, "%s[%d] = %s;\n", args, count, leaf);
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
  let (entry, name, is-gf-call?)
    = xep-expr-and-name(function, use-generic-entry?, file);
  let (func, func-temp?) = ref-leaf(*heap-rep*, function, file);
  contact-bgh-if(func-temp?);

  if (name)
    format(stream, "/* %s */\n",
    	   clean-for-comment(format-to-string("%s", name)));
  end;

  if (is-gf-call?)
    // this is a bit of a hack.  Basically, we're manually inlining the contents
    // of gf-call().  Must be kept in sync with runtime/dylan/func.dylan

    let dispatch = dylan-defn(#"gf-call-lookup");
    unless (dispatch)
      error("Can't find gf-call-lookup -- wrong runtime?");
    end;
    let gf-call-lookup = dispatch.ct-value;

    let info = get-info-for(gf-call-lookup, file);
    let c-name = main-entry-c-name(info, file);
    let result-rep = info.function-info-result-representation;
    maybe-emit-prototype(c-name, info, file)
      & eagerly-reference(gf-call-lookup, file);

    let (srcloc, srcloc-temp?) 
      = if(call.ct-source-location)
          ref-leaf(*general-rep*, call.ct-source-location, file);
        else
          let (expr, rep) 
            = c-expr-and-rep(dylan-value(#"$unknown-source-location"),
                             *general-rep*,
                             file);
          conversion-expr(*general-rep*, expr, rep, file);
        end if;

    // contact-bgh-if(srcloc-temp?);

    format(stream, "{\n  struct %s L_temp_gf_lookup = %s(%s, %s, %s, %s);\n",
           pick-result-structure(result-rep, file), 
           c-name, call-top-name, func, count, srcloc);
    format(stream, "  heapptr_t L_meth = L_temp_gf_lookup.R0;\n");
    format(stream, "  heapptr_t L_next_info = L_temp_gf_lookup.R1;\n  ");
    if (results)
      format(stream, "%s = ", return-top-name);
    end;
    format(stream, "GENERIC_ENTRY(L_meth)(%s, L_meth, %s, L_next_info);\n}\n",
           call-top-name, count);
  else
    if (results)
      format(stream, "%s = ", return-top-name);
    end;
    format(stream, "%s(%s, %s, %s", entry, call-top-name, func, count);
    if (next-info)
      write(stream, ", ");
      write(stream, next-info);
    end;
    format(stream, ");\n");
  end;

  deliver-cluster
    (results, bottom-name, return-top-name,
     call.derived-type.min-values, file);
end;

define generic xep-expr-and-name
    (expr, generic-entry? :: <boolean>, file :: <file-state>)
 => (expr :: false-or(<string>), name :: false-or(<name>), is-gf-call :: <boolean>);

define method xep-expr-and-name
    (func :: <leaf>, generic-entry? :: <boolean>, file :: <file-state>)
 => (expr :: <string>, name :: false-or(<name>), is-gf-call :: <boolean>);
  spew-pending-defines(file);
  let (leaf, temp?) = ref-leaf(*heap-rep*, func, file);
  contact-bgh-if(temp?);
  values
    (stringify
       (if (generic-entry?) "GENERIC" else "GENERAL" end,
        "_ENTRY(", leaf, ')'),
     #f);
end;

define function entry-by-slot
    (func :: <function-literal>,
     entry-getter :: <function>,
     file :: <file-state>)
 => (expr :: <string>, name :: <name>);
  let entry = func.entry-getter;
  let entry-info = get-info-for(entry, file);
  let entry-name = main-entry-c-name(entry-info, file);
  maybe-emit-prototype(entry-name, entry-info, file);
  values(entry-name, entry.name);
end;

define method xep-expr-and-name
    (func :: <function-literal>, generic-entry? :: <boolean>,
     file :: <file-state>)
 => (expr :: <string>, name :: <name>, is-gf-call :: <boolean>);
  if (generic-entry?)
    error("%= doesn't have a generic entry.", func);
  end;
  entry-by-slot(func, general-entry, file);
end;

define method xep-expr-and-name
    (func :: <method-literal>, generic-entry? :: <true>,
     file :: <file-state>)
 => (expr :: <string>, name :: <name>, is-gf-call :: <boolean>);
  entry-by-slot(func, generic-entry, file);
end;

define method xep-expr-and-name
    (func :: <callback-literal>, generic-entry? :: <boolean>,
     file :: <file-state>)
 => (expr :: <string>, name :: <name>, is-gf-call :: <boolean>);
  if (generic-entry?)
    error("%= doesn't have a generic entry.", func);
  end;
  entry-by-slot(func, callback-entry, file);
end;

define method xep-expr-and-name
    (func :: <definition-constant-leaf>, generic-entry? :: <boolean>,
     file :: <file-state>,
     #next next-method)
 => (expr :: <string>, name :: <name>, is-gf-call :: <boolean>);
  let defn = func.const-defn;
  let (expr, name, is-gf?) = xep-expr-and-name(defn, generic-entry?, file);
  values(expr | next-method(),
	 name | defn.defn-name,
         is-gf?);
end;

define method xep-expr-and-name
    (func :: <literal-constant>, generic-entry? :: <boolean>,
     file :: <file-state>, #next next-method)
 => (expr :: <string>, name :: false-or(<name>), is-gf-call :: <boolean>);
  let ctv = func.value;
  let (expr, name, is-gf?)
    = xep-expr-and-name(ctv, generic-entry?, file);
  values(expr | next-method(),
	 name | ctv.ct-function-name,
         is-gf?);
end;

define method xep-expr-and-name
    (defn :: <abstract-constant-definition>, generic-entry? :: <boolean>,
     file :: <file-state>)
 => (expr :: false-or(<string>), name :: false-or(<name>), is-gf-call :: <boolean>);
  let ctv = ct-value(defn);
  if (ctv)
    xep-expr-and-name(ctv, generic-entry?, file);
  end;
end;

define method xep-expr-and-name
    (ctv :: <ct-function>, generic-entry? :: <boolean>,
     file :: <file-state>)
 => (expr :: false-or(<string>), name :: false-or(<name>), is-gf-call :: <boolean>);
  if (generic-entry?)
    error("%= doesn't have a generic entry.", ctv);
  end;
  let name = maybe-emit-general-entry(ctv, file);  
  maybe-emit-prototype(name, #"general", file);
  values(name, ctv.ct-function-name);
end;

define method xep-expr-and-name
    (ctv :: <ct-callback-function>, generic-entry? :: <boolean>,
     file :: <file-state>)
 => (expr :: false-or(<string>), name :: false-or(<name>), is-gf-call :: <boolean>);
  if (generic-entry?)
    error("%= doesn't have a generic entry.", ctv);
  end;
  let name = maybe-emit-callback-entry(ctv, file);  
  values(name, ctv.ct-function-name);
end;

define function gf-generic-entry-point(object :: <ct-generic-function>)
 => (entry :: false-or(<ct-entry-point>), is-gf-call :: <boolean>);
  let defn = object.ct-function-definition;
  let discriminator = defn.generic-defn-discriminator;
  if (discriminator)
    discriminator.has-general-entry?
      & make(<ct-entry-point>, for: discriminator, kind: #"general");
  else
    let dispatch = dylan-defn(#"gf-call");
    dispatch 
      & values(make(<ct-entry-point>, for: dispatch.ct-value, kind: #"main"),
               #t);
  end if;
end function;

define method xep-expr-and-name
    (ctv :: <ct-generic-function>, generic-entry? :: <boolean>,
     file :: <file-state>)
 => (expr :: false-or(<string>), name :: false-or(<name>), is-gf-call :: <boolean>);
  if (generic-entry?)
    error("%= doesn't have a generic entry.", ctv);
  end;
  let defn = ctv.ct-function-definition;
  let discriminator = defn & defn.generic-defn-discriminator;
  if (discriminator)
    xep-expr-and-name(discriminator, #f, file);
  else
    let (entry-point, is-gf-call?) = ctv.gf-generic-entry-point;
    if (entry-point)
      let entry-point-for = entry-point.ct-entry-point-for;
      let info = get-info-for(entry-point-for, file);
      let (c-name, info)
	= if (entry-point.ct-entry-point-kind == #"main")
	    values(main-entry-c-name(info, file), info);
	  else
	    values(general-entry-c-name(info, file), #"general");
	  end if;
      maybe-emit-prototype(c-name, info, file)
	& eagerly-reference(ctv, file);
      values(c-name, #f, is-gf-call?);
    end if;
  end if;
end;

define method xep-expr-and-name
    (ctv :: <ct-method>, generic-entry? :: <true>,
     file :: <file-state>)
 => (expr :: false-or(<string>), name :: false-or(<name>), is-gf-call :: <boolean>);
  let name = maybe-emit-generic-entry(ctv, file);
  maybe-emit-prototype(name, #"generic", file);
  values(name, ctv.ct-function-name);
end;



// Known calls:

define method emit-assignment
    (results :: false-or(<definition-site-variable>), call :: <known-call>,
     source-location :: <source-location>,
     file :: <file-state>)
    => ();
  let function = call.depends-on.source-exp;
  let func-info = find-main-entry-info(function, file);
  let stream = make(<buffered-byte-string-output-stream>);
  let c-name = main-entry-c-name(func-info, file);
  let (sp, new-sp) = cluster-names(call.info);
  let temps = make-temp-locals-list();
  format(stream, "%s(%s", c-name, sp);

  for (arg-dep = call.depends-on.dependent-next then arg-dep.dependent-next,
       rep in func-info.function-info-argument-representations)
    unless (arg-dep)
      error("Not enough arguments in a known call?");
    end;
    let (leaf, temp?) = ref-leaf(rep, arg-dep.source-exp, file);
    if (temp?) temps := add!(temps, leaf) end;
    write(stream, ", ");
    write(stream, leaf);
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
      new-sp ~= call & format(file.file-guts-stream, "%s = %s;\n", new-sp, call);
      deliver-cluster(results, sp, new-sp,
		      func-info.function-info-result-type.min-values,
		      file);
    (instance?(result-rep, <sequence>)) =>
      if (result-rep.empty?)
	format(file.file-guts-stream, "%s;\n", call);
	deliver-results(results, #[], #f, file);
      else
        let c-type = concatenate("struct ", pick-result-structure(result-rep, file));
	let temp = new-local(file, modifier: "temp", wanted-rep: c-type);
	format(file.file-guts-stream, "%s = %s;\n", temp, call);
	let result-exprs = make(<vector>, size: result-rep.size);
	for (rep in result-rep,
	     index from 0)
	  result-exprs[index]
	    := pair(stringify(temp, ".R", index), rep);
	end;
        deliver-results(results, result-exprs, #t, file);
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
                               source-location :: <source-location>,
			       file :: <file-state>)
    => ();
  let emitter
    = expr.primitive-info.priminfo-emitter | default-primitive-emitter;
  emitter(defines, expr, file);
end;

define method emit-assignment
    (defines :: false-or(<definition-site-variable>), expr :: <catch>,
     source-location :: <source-location>,
     file :: <file-state>)
    => ();
  let (temps, func) = extract-operands(expr, file, *heap-rep*);
  contact-bgh-unless-empty(temps);
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
                               source-location :: <source-location>,
			       file :: <file-state>)
    => ();
    for (var = defines then var.definer-next, index from 0,
	 while: var)
      let preferred-names = expr.preferred-names
			    | (expr.preferred-names := make(<stretchy-vector>));
      let target-name = c-name-and-rep(var, file, prefix: "A_", dont-add: #t);
      preferred-names[index] := target-name;
    finally
      let function-info = get-info-for(expr.function, file);
      deliver-results
	(var,
	 map(method (rep, index)
	       pair(stringify('A', index), rep);
	     end,
	     copy-sequence(function-info.function-info-argument-representations,
			   start: index),
	     range(from: index)),
	 #f, file);
    end for;
end;

define method emit-assignment
    (defines :: false-or(<definition-site-variable>),
     ref :: <module-var-ref>, 
     source-location :: <source-location>,
     file :: <file-state>)
    => ();
  let defn = ref.variable;
  let info = get-info-for(defn, file);
  let name = info.backend-var-info-name;
  maybe-emit-prototype(name, defn, file);
  let rep = info.backend-var-info-rep;
  let stream = file.file-guts-stream;
  case
    defn.defn-guaranteed-initialized? =>
      deliver-result(defines, name, rep, #t, file);

    rep.representation-has-bottom-value? =>
      let c-type = rep.representation-c-type;
      let temp = new-local(file, modifier: "temp", wanted-rep: c-type);
      format(stream, "if ((%s = %s).heapptr == NULL) abort();\n", temp, name);
      deliver-result(defines, temp, rep, #t, file);
    otherwise =>
      format(stream, "if (!%s_initialized) abort();\n", name);
      deliver-result(defines, name, rep, #t, file);
  end;
end;

define method emit-assignment
    (defines :: false-or(<definition-site-variable>),
     set :: <module-var-set>, 
     source-location :: <source-location>,
     file :: <file-state>)
    => ();
  let defn = set.variable;
  let info = get-info-for(defn, file);
  let target = info.backend-var-info-name;
  maybe-emit-prototype(target, defn, file);
  let rep = info.backend-var-info-rep;
  let (temps, source) = extract-operands(set, file, rep);
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
     call :: <self-tail-call>, 
     source-location :: <source-location>,
     file :: <file-state>)
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
      let prefs = function.prologue.preferred-names;
      let pref = element(prefs, index, default: #f);
      let (name, rep) = c-name-and-rep(param, file);
      let (source, source-temp?) = ref-leaf(rep, arg-dep.source-exp, file);
      unless (source = pref)
	if (pref)
	  format(stream, "%s", pref);
	else
	  format(stream, "A%d", index); // should never happen!
	end if;
	format(stream, " = %s;\n", source);
      end unless;
    finally
      if (param)
	error("Too many operands in a self-tail-call?");
      elseif (arg-dep & #f /* index >= prolog.type.<multi-value-ctype>.min-values*/)
	// we should check if we passed over enough arguments...
	// currently never reached
	// TODO, FIXME
	error("Not enough operands in a self-tail-call?");
      end;
    end;
  end;
  deliver-results(results, #[], #f, file);
end;

define method emit-assignment
    (results :: false-or(<definition-site-variable>),
     op :: <heap-slot-ref>,
     source-location :: <source-location>,
     file :: <file-state>)
    => ();
  let slot = op.slot-info;
  let slot-rep = slot.slot-representation;

  let instance-leaf = op.depends-on.source-exp;
  let instance-rep = pick-representation(instance-leaf.derived-type, #"speed");

  let expr
    = if (instance?(slot, <vector-slot-info>))
	let (temps, instance, offset, index)
	  = extract-operands(op, file, *heap-rep*, *long-rep*, *long-rep*);
        contact-bgh-unless-empty(temps);
	let c-type = slot-rep.representation-c-type;
	stringify("SLOT(", instance, ", ", c-type, ", ",
		  offset, " + ", index, " * sizeof(", c-type, "))");
      elseif (instance?(instance-rep, <immediate-representation>)
		& instance?(slot-rep, <immediate-representation>))
	assert(instance-rep == slot-rep);
	let (temps, instance, offset)
	  = extract-operands(op, file, instance-rep, *long-rep*);
        contact-bgh-unless-empty(temps);
	instance;
      else
	let (temps, instance, offset)
	  = extract-operands(op, file, *heap-rep*, *long-rep*);
        contact-bgh-unless-empty(temps);
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
     op :: <data-word-ref>, 
     source-location :: <source-location>,
     file :: <file-state>)
    => ();
  let slot = op.slot-info;
  let slot-rep = slot.slot-representation;
  assert(instance?(slot-rep, <c-data-word-representation>));
  
  let instance-leaf = op.depends-on.source-exp;
  let instance-rep = pick-representation(instance-leaf.derived-type, #"speed");

  let (expr, temp?)
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
	error("Trying to extract the data-word from something that doesn't "
		"have one.");
      end if;

  deliver-result(results, expr, slot-rep, #f, file);
  contact-bgh-if(temp?);
end method emit-assignment;

define method emit-assignment
    (results :: false-or(<definition-site-variable>),
     op :: <heap-slot-set>, 
     source-location :: <source-location>,
     file :: <file-state>)
    => ();
  let slot = op.slot-info;
  let slot-rep = slot.slot-representation;
  if (instance?(slot, <vector-slot-info>))
    let (temps, new, instance, offset, index)
      = extract-operands(op, file, slot-rep, *heap-rep*,
			 *long-rep*, *long-rep*);
    let c-type = slot-rep.representation-c-type;
    format(file.file-guts-stream,
	   "SLOT(%s, %s, %s + %s * sizeof(%s)) = %s;\n",
	   instance, c-type, offset, index, c-type, new);
  else
    let (temps, new, instance, offset)
      = extract-operands(op, file, slot-rep, *heap-rep*, *long-rep*);
    format(file.file-guts-stream,
	   "SLOT(%s, %s, %s) = %s;\n",
	   instance, slot-rep.representation-c-type, offset, new);
  end;
  deliver-results(results, #[], #f, file);
end;


define method emit-assignment
    (results :: false-or(<definition-site-variable>),
     op :: <truly-the>, 
     source-location :: <source-location>,
     file :: <file-state>)
    => ();
  if (results)
    let rep = variable-representation(results, file);
    let (temps, source) = extract-operands(op, file, rep);
    // bgh
    // now-dammit was #f.  Should be passing the temps through
    // for deliver-result for storage
    deliver-result(results, source, rep, temps.size > 0, file);
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
      let (guts, guts-temp?) = conversion-expr
        (rep, stringify(src-start, "[0]"), *general-rep*, file);
      let source =
        if (min-values > 0)
          guts
        else
          let (false, false-rep) = c-expr-and-rep(as(<ct-value>, #f), rep, file);
          stringify('(', src-start, " == ", src-end, " ? ",
                    conversion-expr(rep, false, false-rep, file), " : ",
                    guts,
                    ')');
        end;
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
    => (res :: <string>, temp? :: <boolean>);
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
    => (res :: <string>, temp? :: <boolean>);
  let (expr, rep) = c-expr-and-rep(leaf.value, target-rep, file);
  conversion-expr(target-rep, expr, rep, file);
end;

define method ref-leaf (target-rep :: <c-representation>,
			leaf :: <definition-constant-leaf>,
			file :: <file-state>)
    => (res :: <string>, temp? :: <boolean>);
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
    => (res :: <string>, temp? :: <boolean>);
  let ctv = leaf.ct-function;
  if (ctv == #f)
    ctv := make(if (instance?(leaf, <method-literal>))
		  <ct-method>;
		elseif (instance?(leaf, <callback-literal>))
		  <ct-callback-function>;
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
    if (instance?(leaf, <callback-literal>) & leaf.callback-entry)
      ctv-info.function-info-callback-entry-c-name
	:= main-entry-c-name(get-info-for(leaf.callback-entry, file), file);
    end;
    leaf.ct-function := ctv;
  end;
  let (expr, rep) = c-expr-and-rep(ctv, target-rep, file);
  conversion-expr(target-rep, expr, rep, file);
end;

define method ref-leaf (target-rep :: <c-representation>,
			leaf :: <uninitialized-value>,
			file :: <file-state>)
    => (res :: <string>, temp? :: <boolean>);
  if (target-rep == *general-rep*)
    conversion-expr(target-rep, "0", *heap-rep*, file);
  else
    "0";
  end;
end;


// c-expr-and-rep  --- emitting literals


// Emit a reference to a constant which must be referenced via the roots (not
// as a C literal.)  Name and modifier are passed through to new-root.
// Lit is the init-value, and defn is some thingie used to indicate
// whether we've prototyped this thing yet or not.
//
// Putting things in the roots is done lazily because we don't know if we
// will be able to represent as a C literal or constant until now.
//
define function aux-c-expr-and-rep
    (lit :: <ct-value>, file :: <file-state>,
     #key name :: false-or(<name>), modifier :: <byte-string> = "",
	  defn :: <object> = lit)
  let info = get-info-for(lit, file);

  local make-global-root()
   => (name :: <string>, rep :: <c-representation>);
    let c-name = info.const-info-expr;
    unless (c-name)
      c-name := new-c-global(name, file, modifier: modifier);
      info.const-info-expr := c-name;
      new-root(lit, c-name, file);
    end unless;
    maybe-emit-prototype(c-name, defn, file);
    values(c-name, *general-rep*);
  end;
  
  if (instance?(lit, <proxy>))
    make-global-root();
  else
    let best-rep = pick-representation(lit.ct-value-cclass, #"speed");
    if (instance?(best-rep, <heap-representation>))
      let labels = info.const-info-heap-labels;
      if (labels.empty?) // suggest a label for lit
	let label = object-label(lit)
		    | new-c-global
			(name, file,
			 modifier: if (modifier.empty?)
				     "_ROOT"
				    else
				      concatenate(modifier, "_ROOT")
				    end if);
	info.const-info-heap-labels := add!(labels, label);
      end if;
      let c-name = info.const-info-heap-labels.first;
      let single-file-mode? = file.single-file-mode?;

      if (single-file-mode?)
        spew-heap-prototype(c-name, lit, file)
      else
        maybe-emit-prototype(c-name, #"heap", file)
      end
	& (info.const-info-expr | eagerly-reference(lit, file));
      values(stringify(if (single-file-mode?) "(heapptr_t)&" else "&" end,
		       c-name),
	     best-rep);
    else
      make-global-root();
    end if;
  end if;
end function aux-c-expr-and-rep;


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
  aux-c-expr-and-rep(lit, file, name: lit.ct-function-name);
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
	   // print it in hex (assuming 2's complement).
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
  target ~= source & format(stream, "%s = %s;\n", target, source);
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
  let (c-code, temp?) = conversion-expr(*heap-rep*, proxy, proxy-rep, file);
  format(stream, "%s.heapptr = %s;\n",
	 target, c-code);
  format(stream, "%s.dataword.%s = %s;\n",
	 target, source-rep.representation-data-word-member, source);
end;

define method emit-copy
    (target :: <string>, target-rep :: <general-representation>,
     source :: <string>, source-rep :: <c-representation>,
     file :: <file-state>)
    => ();
  let stream = file.file-guts-stream;
  let (heapptr, temp?) = conversion-expr(*heap-rep*, source, source-rep, file);
  format(stream, "%s.heapptr = %s;\n", target, heapptr);
  format(stream, "%s.dataword.l = 0;\n", target);
end;

define method emit-copy
    (target :: <string>, target-rep :: <c-representation>,
     source :: <string>, source-rep :: <c-representation>,
     file :: <file-state>)
    => ();
  let stream = file.file-guts-stream;
  let (expr, temp?) = conversion-expr(target-rep, source, source-rep, file);
  target ~= expr & format(stream, "%s = %s;\n", target, expr);
end;



// conversion-expr

define method conversion-expr
    (target-rep :: <general-representation>,
     source :: <string>, source-rep :: <c-representation>,
     file :: <file-state>)
    => (res :: <string>, temp? :: <boolean>);
  if (target-rep == source-rep)
    values(source, #f);
  else
    let c-type = target-rep.representation-c-type;
    let temp = new-local(file, modifier: "temp", wanted-rep: c-type);
    emit-copy(temp, target-rep, source, source-rep, file);
    values(temp, #t);
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
// <local-var> -- subclass of <object>
define sealed domain make(singleton(<local-var>));
define sealed domain initialize(<local-var>);
