module: cback
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/cback/cback.dylan,v 1.78 1995/11/12 03:33:34 rgs Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.

define constant $indentation-step = 4;


// Indenting streams.

define class <indenting-stream> (<stream>)
  slot is-target :: <stream>, required-init-keyword: target:;
  slot is-buffer :: <buffer>, init-function: curry(make, <buffer>);
  slot is-after-newline? :: <boolean>, init-value: #t;
  slot is-column :: <fixed-integer>, init-value: 0;
  slot is-indentation :: <fixed-integer>,
    init-value: 0, init-keyword: indentation:;
end;

define method stream-extension-get-output-buffer (stream :: <indenting-stream>)
    => (buf :: <buffer>, next :: <buffer-index>, size :: <buffer-index>);
  values(stream.is-buffer, 0, stream.is-buffer.size);
end;

define constant $tab = as(<integer>, '\t');
define constant $space = as(<integer>, ' ');
define constant $newline = as(<integer>, '\n');

define method indenting-stream-spew-output
    (stream :: <indenting-stream>, stop :: <buffer-index>)
    => ();
  unless (zero?(stop))
    let (target-buffer, target-next, target-size)
      = get-output-buffer(stream.is-target);
    local
      method spew-n-chars (n, char)
	let available = target-size - target-next;
	while (available < n)
	  for (i from target-next below target-size)
	    target-buffer[i] := char;
	  end;
	  empty-output-buffer(stream.is-target, target-size);
	  target-next := 0;
	  n := n - available;
	  available := target-size;
	end;
	for (i from target-next below target-next + n)
	  target-buffer[i] := char;
	finally
	  target-next := i;
	end;
      end,
      method spew-char (char)
	if (target-next == target-size)
	  empty-output-buffer(stream.is-target, target-size);
	  target-next := 0;
	end;
	target-buffer[target-next] := char;
	target-next := target-next + 1;
      end;
    let buffer = stream.is-buffer;
    let column = stream.is-column;
    let after-newline? = stream.is-after-newline?;
    for (i from 0 below stop)
      let char = buffer[i];
      if (char == $newline)
	spew-char(char);
	column := 0;
	after-newline? := #t;
      elseif (char == $space)
	unless (after-newline?)
	  spew-char(char);
	end;
	column := column + 1;
      elseif (char == $tab)
	let old-column = column;
	column := ceiling/(column + 1, 8) * 8;
	unless (after-newline?)
	  spew-n-chars(column - old-column, $space);
	end;
      else
	if (after-newline?)
	  let (tabs, spaces) = floor/(stream.is-indentation + column, 8);
	  spew-n-chars(tabs, $tab);
	  spew-n-chars(spaces, $space);
	  after-newline? := #f;
	end;
	spew-char(char);
	column := column + 1;
      end;
    end;
    release-output-buffer(stream.is-target, target-next);
    stream.is-after-newline? := after-newline?;
    stream.is-column := column;
  end;
end;

define method stream-extension-release-output-buffer
    (stream :: <indenting-stream>, next :: <buffer-index>)
    => ();
  indenting-stream-spew-output(stream, next);
end;

define method stream-extension-empty-output-buffer
    (stream :: <indenting-stream>, stop :: <buffer-index>)
    => ();
  indenting-stream-spew-output(stream, stop);
end;

define method stream-extension-force-secondary-buffers
    (stream :: <indenting-stream>)
    => ();
  force-secondary-buffers(stream.is-target);
end;  

define method stream-extension-synchronize (stream :: <indenting-stream>)
    => ();
  synchronize(stream.is-target);
end;

define method close (stream :: <indenting-stream>) => ();
  force-output(stream);
end;

define method indent (stream :: <indenting-stream>, delta :: <fixed-integer>)
    => ();
  stream.is-indentation := stream.is-indentation + delta;
end;

define constant make-indenting-string-stream
  = method (#rest keys)
	=> res :: <indenting-stream>;
      apply(make, <indenting-stream>,
	    target: make(<byte-string-output-stream>),
	    keys);
    end;

define method string-output-stream-string (stream :: <indenting-stream>)
    => res :: <byte-string>;
  stream.is-target.string-output-stream-string;
end;


// Output file state

define class <unit-info> (<object>)
  //
  // String prefix for this unit.
  slot unit-info-prefix :: <byte-string>, required-init-keyword: prefix:;
  //
  // id number for the next global.
  slot unit-info-next-global :: <fixed-integer>, init-value: 0;
  //
  //
  // keeps track of names used already.
  slot unit-info-global-table :: <dictionary>,
    init-function: method () make(<string-table>) end method;
  // Vector of the initial values for the roots vector.
  slot unit-info-init-roots :: <stretchy-vector>,
    init-function: curry(make, <stretchy-vector>);
end;

define class <output-info> (<object>)
  //
  // The unit info for this output info.
  slot output-info-unit-info :: <unit-info>,
    required-init-keyword: unit-info:;
  //
  // Things we have already spewed defns for.
  slot output-info-prototypes-exist-for :: <string-table>,
    init-function: curry(make, <string-table>);
  //
  slot output-info-result-structures :: <equal-table>,
    init-function: curry(make, <equal-table>);
  //
  slot output-info-body-stream :: <stream>,
    required-init-keyword: body-stream:;
  //
  slot output-info-vars-stream :: <stream>,
    init-function: curry(make-indenting-string-stream,
			 indentation: $indentation-step);
  //
  slot output-info-guts-stream :: <stream>,
    init-function: curry(make-indenting-string-stream,
			 indentation: $indentation-step);
  //
  slot output-info-next-mv-result-struct :: <fixed-integer>, init-value: 0;
  //
  slot output-info-local-vars :: <object-table>,
    init-function: curry(make, <object-table>);
  //
  // id number for the next block.
  slot output-info-next-block :: <fixed-integer>, init-value: 0;
  //
  // id number for the next local.  Reset at the start of each function.
  slot output-info-next-local :: <fixed-integer>, init-value: 0;
  //
  // keeps track of names used already.
  slot output-info-local-table :: <dictionary>,
    init-function: method () make(<string-table>) end method;
  //
  // C variable holding the current stack top.
  slot output-info-cur-stack-depth :: <fixed-integer>,
    init-value: 0;
end;




// Utilities.

define method maybe-emit-prototype
    (name :: <byte-string>, info :: <object>, output-info :: <output-info>)
    => ();
  unless (element(output-info.output-info-prototypes-exist-for, name,
		  default: #f))
    emit-prototype-for(name, info, output-info);
    output-info.output-info-prototypes-exist-for[name] := #t;
  end;
end;

define generic emit-prototype-for
    (name :: <byte-string>, info :: <object>, output-info :: <output-info>)
    => ();


define method get-info-for (thing :: <annotatable>,
			    output-info :: <output-info>)
    => res :: <object>;
  thing.info | (thing.info := make-info-for(thing, output-info));
end;

define constant c-prefix-transform :: <vector>
  = begin
      let map = make(<byte-string>, size: 256);
      for (i from 0 below 256)
	map[i] := as(<character>, i);
      end for;
      map[as(<integer>, '-')] := '_';
      map[as(<integer>, '%')] := '_';
      map[as(<integer>, '*')] := 'O';
      map[as(<integer>, '/')] := 'O';
      map[as(<integer>, '+')] := 'O';
      map[as(<integer>, '~')] := 'O';
      map[as(<integer>, '$')] := '_';
      map[as(<integer>, '?')] := 'P';
      map[as(<integer>, '!')] := 'D';
      map[as(<integer>, '<')] := 'C';
      map[as(<integer>, '>')] := 'C';
      map[as(<integer>, '=')] := 'O';
      map[as(<integer>, '&')] := 'O';
      map[as(<integer>, '|')] := 'O';
      map;
    end;

define method is-prefix? (short :: <string>, long :: <string>)
  if (short.size > long.size)
    #f;
  else
    for (i from 0 below short.size, until: short[i] ~= long[i])
    finally
      i == short.size;
    end for;
  end if;
end method is-prefix?;

// Return a name suitable for use in a C name.  "Description" is expected to
// be a descriptive string like "foo{<bar>, <baz>} in module Dylan, library
// Dylan".
define method c-prefix (description :: <byte-string>) => (result :: <string>);
  if (is-prefix?("Define Constant ", description))
    description := copy-sequence(description, start: 16);
  end if;
  if (is-prefix?("Discriminator for ", description))
    description := copy-sequence(description, start: 18);
  end if;
  for (i from 0 below description.size,
       until: description[i] = ' ' | description[i] = '{')
  finally
    let result = make(<byte-string>, size: i);
    for (j from 0 below i)
      result[j] := c-prefix-transform[as(<integer>, description[j])];
    end for;
    result;
  end for;
end method c-prefix;

define method c-prefix (description :: <basic-name>) => (result :: <string>);
  as(<byte-string>, description.name-symbol).c-prefix;
end method c-prefix;

define method c-prefix (description :: <symbol>) => (result :: <string>);
  as(<byte-string>, description).c-prefix;
end method c-prefix;

define method new-local
    (output-info :: <output-info>,
     #key prefix :: <string> = "L_", modifier :: <string> = "anon")
 => res :: <string>;
  let result = format-to-string("%s%s", prefix, modifier);
  if (key-exists?(output-info.output-info-local-table, result))
    let num = output-info.output-info-next-local;
    output-info.output-info-next-local := num + 1;
    new-local(output-info, prefix: prefix,
	      modifier: format-to-string("%s_%d", modifier, num));
  else
    output-info.output-info-local-table[result] := result;
  end if;
end;

define method new-global
    (output-info :: <output-info>,
     #key prefix :: <string> = "G", modifier :: <string> = "")
 => res :: <string>;
  let unit-info = output-info.output-info-unit-info;

  let result = format-to-string("%s_%s%s", unit-info.unit-info-prefix,
				prefix, modifier);
  if (key-exists?(unit-info.unit-info-global-table, result))
    let num = unit-info.unit-info-next-global;
    unit-info.unit-info-next-global := num + 1;
    new-global(output-info, prefix: prefix,
	       modifier: format-to-string("%s_%d", modifier, num));
  else
    unit-info.unit-info-global-table[result] := result;
  end if;
end method new-global;

define method new-root (init-value :: union(<false>, <ct-value>),
			output-info :: <output-info>,
			#key prefix)
  let unit-info = output-info.output-info-unit-info;
  let roots = unit-info.unit-info-init-roots;
  let index = roots.size;
  add!(roots, init-value);

  format-to-string("%s_roots[%d]", unit-info.unit-info-prefix, index);
end;

define method cluster-names (depth :: <fixed-integer>)
    => (bottom-name :: <string>, top-name :: <string>);
  if (zero?(depth))
    values("orig_sp", "cluster_0_top");
  else
    values(format-to-string("cluster_%d_top", depth - 1),
	   format-to-string("cluster_%d_top", depth));
  end;
end;

define method consume-cluster
    (cluster :: <abstract-variable>, output-info :: <output-info>)
    => (bottom-name :: <string>, top-name :: <string>);
  let depth = cluster.info;
  if (depth >= output-info.output-info-cur-stack-depth)
    error("Consuming a cluster that isn't on the stack?");
  end;
  output-info.output-info-cur-stack-depth := depth;
  cluster-names(depth);
end;

define method produce-cluster
    (cluster :: <abstract-variable>, output-info :: <output-info>)
    => (bottom-name :: <string>, top-name :: <string>);
  let depth = cluster.info;
  if (depth > output-info.output-info-cur-stack-depth)
    error("Leaving a gap when producing a cluster?");
  end;
  output-info.output-info-cur-stack-depth := depth + 1;
  cluster-names(depth);
end;

define method produce-cluster
    (cluster :: <initial-definition>, output-info :: <output-info>)
    => (bottom-name :: <string>, top-name :: <string>);
  produce-cluster(cluster.definition-of, output-info);
end;




// variable stuff.

define class <backend-var-info> (<object>)
  slot backend-var-info-rep :: <representation>,
    required-init-keyword: representation:;
  slot backend-var-info-name :: union(<false>, <string>),
    required-init-keyword: name:;
end;

add-make-dumper(#"backend-var-info", *compiler-dispatcher*, <backend-var-info>,
		list(backend-var-info-rep, representation:, #f,
		     backend-var-info-name, name:, #f));

define method make-info-for (var :: union(<initial-variable>, <ssa-variable>),
			     // ### Should really only be ssa-variable.
			     output-info :: <output-info>)
    => res :: <backend-var-info>;
  let varinfo = var.var-info;
  let rep = pick-representation(var.derived-type, #"speed");
  make(<backend-var-info>, representation: rep, name: #f);
end;

define method make-info-for (defn :: <definition>,
			     output-info :: <output-info>)
    => res :: <backend-var-info>;
  let type = defn.defn-type;
  let rep = if (type)
	      pick-representation(type, #"speed");
	    else
	      *general-rep*;
	    end;
  if (instance?(rep, <immediate-representation>))
    let name = new-global(output-info, prefix: defn.defn-name.c-prefix);
    make(<backend-var-info>, representation: rep, name: name);
  else
    let name = new-root(if (instance?(defn, <variable-definition>))
			  defn.defn-init-value;
			else
			  defn.ct-value;
			end,
			output-info);
    make(<backend-var-info>, representation: *general-rep*, name: name);
  end;
end;


define method get-info-for (leaf :: <initial-definition>,
			    output-info :: <output-info>)
    => res :: <backend-var-info>;
  get-info-for(leaf.definition-of, output-info);
end;

define method c-name-and-rep (leaf :: <abstract-variable>,
			      // ### Should really be ssa-variable
			      output-info :: <output-info>)
    => (name :: <string>, rep :: <representation>);
  let info = get-info-for(leaf, output-info);
  let name = info.backend-var-info-name;
  unless (name)
    if (instance?(leaf.var-info, <debug-named-info>))
      name := new-local(output-info,
			modifier: leaf.var-info.debug-name.c-prefix);
    else
      name := new-local(output-info);
    end if;
    let stream = output-info.output-info-vars-stream;
    format(stream, "%s %s;",
	   info.backend-var-info-rep.representation-c-type, name);
    if (instance?(leaf.var-info, <debug-named-info>))
      format(stream, " /* %s */", leaf.var-info.debug-name);
    end;
    write('\n', stream);
    info.backend-var-info-name := name;
  end;
  values(name, info.backend-var-info-rep);
end;

define method variable-representation (leaf :: <abstract-variable>,
				       // ### Should really be ssa-variable
				       output-info :: <output-info>)
    => rep :: <representation>;
  get-info-for(leaf, output-info).backend-var-info-rep;
end;


// function region stuff.

define class <function-info> (<object>)
  //
  // The name of the function-region this is the function info for.
  slot function-info-name :: <string>,
    required-init-keyword: name:;
  //
  // The C name of the function corresponding to this function-region.
  slot function-info-main-entry-name :: false-or(<byte-string>),
    init-value: #f, init-keyword: main-entry-name:;
  //
  // List of the representations for the arguments.
  slot function-info-argument-representations :: <list>,
    required-init-keyword: argument-reps:;
  //
  // Representation of the result.  If a <representation>, then that single
  // value is returned.  If a list, then a structure of those values are
  // returned.  If #"doesn't-return" then it doesn't return.  If #"cluster",
  // it returns a cluster of values.
  slot function-info-result-representation
    :: type-or(<representation>, <list>,
	       one-of(#"doesn't-return", #"cluster")),
    required-init-keyword: result-rep:;
  //
  // The result type.
  slot function-info-result-type :: <values-ctype>,
    required-init-keyword: result-type:;
end;

define constant $function-info-slots
  = list(function-info-name, name:, #f,
	 function-info-main-entry-name, main-entry-name:, #f,
	 function-info-argument-representations, argument-reps:, #f,
	 function-info-result-representation, result-rep:, #f,
	 function-info-result-type, result-type:, #f);

define method main-entry-name
    (info :: <function-info>, output-info :: <output-info>)
    => res :: <byte-string>;
  info.function-info-main-entry-name
    | (info.function-info-main-entry-name
	 := new-global(output-info, modifier: "_main",
		       prefix: info.function-info-name.c-prefix));
end;

define method make-function-info
    (class :: <class>, name :: <string>, signature :: <signature>,
     closure-var-types :: <list>, output-info :: <output-info>)
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
	as(<list>, reps);
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
    (function :: <fer-function-region>, output-info :: <output-info>)
    => res :: <function-info>;
  make-function-info(<function-info>, function.name,
		     make(<signature>, specializers: function.argument-types,
			  returns: function.result-type),
		     #(), output-info);
end;

define method entry-point-c-name (entry :: <ct-entry-point>)
    => res :: <string>;
  let info = entry.ct-entry-point-for.info;
  unless (info)
    error("Too late to be making an info for %=", entry.ct-entry-point-for);
  end unless;
  let name
    = select (entry.ct-entry-point-kind)
	#"main" => info.function-info-main-entry-name;
	#"general" => info.function-info-general-entry-name;
	#"generic" => info.function-info-generic-entry-name;
      end select;
  unless (name)
    error("Too late to be picking a name for %=", entry);
  end unless;
  name;
end method;


// Constant stuff.

define class <constant-info> (<object>)
  slot const-info-expr :: false-or(<byte-string>),
    init-value: #f, init-keyword: expr:;
end;

define constant $constant-info-slots
  = list(const-info-expr, expr:, #f);

add-make-dumper(#"constant-info", *compiler-dispatcher*, <constant-info>,
		$constant-info-slots);

define method make-info-for (ctv :: <ct-value>, output-info :: <output-info>)
    => res :: <constant-info>;
  make(<constant-info>);
end;

define class <constant-function-info> (<constant-info>, <function-info>)
  slot function-info-general-entry-name :: false-or(<byte-string>),
    init-value: #f, init-keyword: general-entry-name:;
end;

define constant $constant-function-info-slots
  = concatenate($constant-info-slots,
		$function-info-slots,
		list(function-info-general-entry-name, general-entry-name:,
		       #f));

add-make-dumper(#"constant-function-info", *compiler-dispatcher*,
		<constant-function-info>, $constant-function-info-slots);

define method general-entry-name
    (info :: <constant-function-info>, output-info :: <output-info>)
    => res :: <byte-string>;
  info.function-info-general-entry-name
    | (info.function-info-general-entry-name
	 := new-global(output-info, modifier: "_general",
		       prefix: info.function-info-name.c-prefix));

end;

define method make-info-for
    (ctv :: <ct-function>, output-info :: <output-info>)
    => res :: <constant-function-info>;
  make-function-info(<constant-function-info>, ctv.ct-function-name,
		     ctv.ct-function-signature,
		     ctv.ct-function-closure-var-types, output-info);
end;

define class <constant-method-info> (<constant-function-info>)
  slot function-info-generic-entry-name :: false-or(<byte-string>),
    init-value: #f, init-keyword: generic-entry-name:;
end;

define constant $constant-method-info-slots
  = concatenate($constant-function-info-slots,
		list(function-info-generic-entry-name, generic-entry-name:,
		       #f));

add-make-dumper(#"constant-method-info", *compiler-dispatcher*,
		<constant-method-info>, $constant-method-info-slots);

define method generic-entry-name
    (info :: <constant-method-info>, output-info :: <output-info>)
    => res :: <byte-string>;
  info.function-info-generic-entry-name
    | (info.function-info-generic-entry-name
	 := new-global(output-info, modifier: "_generic",
		       prefix: info.function-info-name.c-prefix));
end;

define method make-info-for
    (ctv :: <ct-method>, output-info :: <output-info>)
    => res :: <constant-function-info>;
  make-function-info(<constant-method-info>, ctv.ct-function-name,
		     ctv.ct-function-signature,
		     ctv.ct-function-closure-var-types, output-info);
end;


// Prologue and epilogue stuff.

define method emit-prologue (output-info :: <output-info>) => ();
  let stream = output-info.output-info-body-stream;
  format(stream, "#include <stdlib.h>\n\n");

  format(stream, "#include <runtime.h>\n\n");

  format(stream, "extern descriptor_t %s_roots[];\n\n",
	 output-info.output-info-unit-info.unit-info-prefix);
  format(stream, "#define obj_True %s.heapptr\n",
	 c-expr-and-rep(as(<ct-value>, #t), *general-rep*, output-info));
  format(stream, "#define obj_False %s.heapptr\n\n",
	 c-expr-and-rep(as(<ct-value>, #f), *general-rep*, output-info));
  format(stream, "#define GENERAL_ENTRY(func) \\\n");
  format(stream, "    ((entry_t)SLOT(func, void *, %d))\n",
	 dylan-slot-offset(function-ctype(), #"general-entry"));
  format(stream, "#define GENERIC_ENTRY(func) \\\n");
  format(stream, "    ((entry_t)SLOT(func, void *, %d))\n\n",
	 dylan-slot-offset(specifier-type(#"<method>"), #"generic-entry"));
end;

define method emit-epilogue
    (init-functions :: <vector>, output-info :: <output-info>) => ();
  let bstream = output-info.output-info-body-stream;
  let gstream = output-info.output-info-guts-stream;

  format(bstream, "void %s_init(descriptor_t *sp)\n{\n",
	 output-info.output-info-unit-info.unit-info-prefix);
  for (init-function in init-functions)
    let main-entry = init-function.main-entry;
    let func-info = get-info-for(main-entry, output-info);
    format(gstream, "/* %s */\n", main-entry.name);
    format(gstream, "%s(sp);\n\n", main-entry-name(func-info, output-info));
    write(gstream.string-output-stream-string, bstream);
  end;
  write("}\n", bstream);
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


// Top level form processors.

define generic emit-tlf-gunk (tlf :: <top-level-form>,
			      output-info :: <output-info>)
    => ();

define method emit-tlf-gunk (tlf :: <top-level-form>,
			     output-info :: <output-info>)
    => ();
  format(output-info.output-info-body-stream, "/* %s */\n\n", tlf);
end;

define method emit-tlf-gunk (tlf :: <magic-interal-primitives-placeholder>,
			     output-info :: <output-info>)
    => ();
  let bstream = output-info.output-info-body-stream;
  format(bstream, "/* %s */\n\n", tlf);

  let gstream = output-info.output-info-guts-stream;

  format(bstream, "descriptor_t *pad_cluster(descriptor_t *start, "
	   "descriptor_t *end,\n");
  format(bstream, "                          int min_values)\n{\n");
  format(gstream, "descriptor_t *ptr = start + min_values;\n\n");
  format(gstream, "while (end < ptr)\n");
  format(gstream, "    *end++ = %s;\n",
	 c-expr-and-rep(as(<ct-value>, #f), *general-rep*, output-info));
  format(gstream, "return end;\n");
  write(gstream.string-output-stream-string, bstream);
  write("}\n\n", bstream);

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
  write(gstream.string-output-stream-string, bstream);
  write("}\n\n", bstream);

  unless (instance?(*double-rep*, <data-word-representation>))
    let cclass = specifier-type(#"<double-float>");
    format(bstream, "heapptr_t make_double_float(double value)\n{\n");
    format(gstream, "heapptr_t res = allocate(%d);\n",
	   cclass.instance-slots-layout.layout-length);
    let (expr, rep) = c-expr-and-rep(cclass, *heap-rep*, output-info);
    format(gstream, "SLOT(res, heapptr_t, %d) = %s;\n",
	   dylan-slot-offset(cclass, #"%object-class"),
	   conversion-expr(*heap-rep*, expr, rep, output-info));
    let value-offset = dylan-slot-offset(cclass, #"value");
    format(gstream, "SLOT(res, double, %d) = value;\n", value-offset);
    format(gstream, "return res;\n");
    write(gstream.string-output-stream-string, bstream);
    write("}\n\n", bstream);

    format(bstream, "double double_float_value(heapptr_t df)\n{\n");
    format(gstream, "return SLOT(df, double, %d);\n", value-offset);
    write(gstream.string-output-stream-string, bstream);
    write("}\n\n", bstream);
  end;

  unless (instance?(*long-double-rep*, <data-word-representation>))
    let cclass = specifier-type(#"<extended-float>");
    format(bstream, "heapptr_t make_extended_float(long double value)\n{\n");
    format(gstream, "heapptr_t res = allocate(%d);\n",
	   cclass.instance-slots-layout.layout-length);
    let (expr, rep) = c-expr-and-rep(cclass, *heap-rep*, output-info);
    format(gstream, "SLOT(res, heapptr_t, %d) = %s;\n",
	   dylan-slot-offset(cclass, #"%object-class"),
	   conversion-expr(*heap-rep*, expr, rep, output-info));
    let value-offset = dylan-slot-offset(cclass, #"value");
    format(gstream, "SLOT(res, long double, %d) = value;\n", value-offset);
    format(gstream, "return res;\n");
    write(gstream.string-output-stream-string, bstream);
    write("}\n\n", bstream);

    format(bstream, "long double extended_float_value(heapptr_t xf)\n{\n");
    format(gstream, "return SLOT(xf, long double, %d);\n", value-offset);
    write(gstream.string-output-stream-string, bstream);
    write("}\n\n", bstream);
  end;
end;

define method emit-tlf-gunk (tlf :: <define-bindings-tlf>,
			     output-info :: <output-info>)
    => ();
  format(output-info.output-info-body-stream, "/* %s */\n\n", tlf);
  for (defn in tlf.tlf-required-defns)
    emit-bindings-definition-gunk(defn, output-info);
  end;
  if (tlf.tlf-rest-defn)
    emit-bindings-definition-gunk(tlf.tlf-rest-defn, output-info);
  end;
  write('\n', output-info.output-info-body-stream);
end;

define method emit-bindings-definition-gunk
    (defn :: <bindings-definition>, output-info :: <output-info>) => ();
  let info = get-info-for(defn, output-info);
  let stream = output-info.output-info-body-stream;
  let rep = info.backend-var-info-rep;
  if (instance?(rep, <immediate-representation>))
    let name = info.backend-var-info-name;
    format(stream, "static %s %s = ", rep.representation-c-type, name);
    let init-value = if (instance?(defn, <variable-definition>))
		       defn.defn-init-value;
		     else
		       defn.ct-value;
		     end;
    if (init-value)
      let (init-value-expr, init-value-rep)
	= c-expr-and-rep(init-value, rep, output-info);
      format(stream, "%s;\t/* %s */\n",
	     conversion-expr(rep, init-value-expr, init-value-rep,
			     output-info),
	     defn.defn-name);
    else
      format(stream, "0;\t/* %s */\nstatic int %s_initialized = FALSE;\n",
	     defn.defn-name, name);
    end;
    output-info.output-info-prototypes-exist-for[name] := #t;
  else
    format(stream, "/* %s allocated as %s */\n",
	   defn.defn-name,
	   info.backend-var-info-name);
  end;
end;

define method emit-prototype-for
    (name :: <byte-string>, defn :: <bindings-definition>,
     output-info :: <output-info>)
    => ();
  let info = get-info-for(defn, output-info);
  let stream = output-info.output-info-body-stream;
  let rep = info.backend-var-info-rep;
  if (instance?(rep, <immediate-representation>))
    format(stream, "static %s %s;\t/* %s */\n",
	   rep.representation-c-type,
	   info.backend-var-info-name,
	   defn.defn-name);
    let init-value = if (instance?(defn, <variable-definition>))
		       defn.defn-init-value;
		     else
		       defn.ct-value;
		     end;
    unless (init-value)
      format(stream, "static boolean %s_initialized = FALSE;\n",
	     info.backend-var-info-name);
    end;
    write('\n', stream);
  end;
end;  

define method emit-bindings-definition-gunk
    (defn :: <variable-definition>, output-info :: <output-info>,
     #next next-method)
    => ();
  next-method();
  let type-defn = defn.var-defn-type-defn;
  if (type-defn)
    emit-bindings-definition-gunk(type-defn, output-info);
  end;
end;

define method emit-bindings-definition-gunk
    (defn :: <constant-definition>, output-info :: <output-info>,
     #next next-method)
    => ();
  unless (instance?(defn.ct-value, <eql-ct-value>))
    next-method();
  end;
end;


// Emitting Components.

define method emit-component
    (component :: <fer-component>, output-info :: <output-info>) => ();
  for (func-lit in component.all-function-literals)
    let ctv = func-lit.ct-function;
    if (ctv)
      let ctv-info = get-info-for(ctv, output-info);
      begin
	let main-entry = func-lit.main-entry;
	if (main-entry.info)
	  error("%= is already annotated?", main-entry);
	end;
	main-entry.info := ctv-info;
      end;
      if (func-lit.general-entry)
	let gen-info = get-info-for(func-lit.general-entry, output-info);
	if (gen-info.function-info-main-entry-name)
	  error("%= already has a name?", func-lit.general-entry);
	end;
	gen-info.function-info-main-entry-name
	  := general-entry-name(ctv-info, output-info);
      end;
      if (instance?(func-lit, <method-literal>) & func-lit.generic-entry)
	let gen-info = get-info-for(func-lit.generic-entry, output-info);
	if (gen-info.function-info-main-entry-name)
	  error("%= already has a name?", func-lit.general-entry);
	end;
	gen-info.function-info-main-entry-name
	  := generic-entry-name(ctv-info, output-info);
      end;
    end;
  end;

  do(rcurry(emit-function, output-info), component.all-function-regions);
end;



// Control flow emitters

define method emit-function
    (function :: <fer-function-region>, output-info :: <output-info>)
    => ();
  output-info.output-info-next-block := 0;
  output-info.output-info-next-local := 0;
  output-info.output-info-local-table := make(<string-table>);
  output-info.output-info-cur-stack-depth := 0;
  assert(output-info.output-info-local-vars.size == 0);

  let function-info = get-info-for(function, output-info);
  let c-name = main-entry-name(function-info, output-info);
  output-info.output-info-prototypes-exist-for[c-name] := #t;

  let max-depth = analize-stack-usage(function);
  for (i from 0 below max-depth)
    format(output-info.output-info-vars-stream,
	   "descriptor_t *cluster_%d_top;\n",
	   i);
  end;
  emit-region(function.body, output-info);

  let stream = output-info.output-info-body-stream;
  format(stream, "/* %s */\n", function.name);
  format(stream, "%s\n{\n",
	 compute-function-prototype(function, function-info, output-info));
  write(output-info.output-info-vars-stream.string-output-stream-string,
	stream);
  write('\n', stream);
  write(output-info.output-info-guts-stream.string-output-stream-string,
	stream);
  write("}\n\n", stream);
end;

define method compute-function-prototype
    (function :: false-or(<fer-function-region>),
     function-info :: <function-info>,
     output-info :: <output-info>)
    => res :: <byte-string>;
  let c-name = main-entry-name(function-info, output-info);
  let stream = make(<byte-string-output-stream>);
  let result-rep = function-info.function-info-result-representation;
  if (result-rep == #() | result-rep == #"doesn't-return")
    write("void", stream);
  elseif (result-rep == #"cluster")
    write("descriptor_t *", stream);
  elseif (instance?(result-rep, <pair>))
    format(stream, "struct %s",
	   pick-result-structure(result-rep, output-info));
  else
    write(result-rep.representation-c-type, stream);
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
	format(stream, " /* %s */", varinfo.debug-name);
      end;
    end;
  end;
  write(')', stream);
  stream.string-output-stream-string;
end;

define method pick-result-structure
    (results :: <list>, output-info :: <output-info>) => res :: <byte-string>;
  let types = map-as(<simple-object-vector>, representation-c-type, results);
  let table = output-info.output-info-result-structures;
  let struct = element(table, types, default: #f);
  if (struct)
    struct;
  else
    let id = output-info.output-info-next-mv-result-struct;
    output-info.output-info-next-mv-result-struct := id + 1;
    let name = format-to-string("mv_result_%d", id);
    let stream = output-info.output-info-body-stream;
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
     output-info :: <output-info>)
    => ();
  format(output-info.output-info-body-stream,
	 "extern %s;\t/* %s */\n\n",
	 compute-function-prototype(#f, function-info, output-info),
	 function-info.function-info-name);
end;

define method emit-prototype-for
    (name :: <byte-string>, function-info == #"general",
     output-info :: <output-info>)
    => ();
  format(output-info.output-info-body-stream,
	 "extern descriptor_t * %s(descriptor_t *orig_sp, "
	   "heapptr_t A0 /* self */, long A1 /* nargs */);\n\n",
	 name);
end;

define method emit-prototype-for
    (name :: <byte-string>, function-info == #"generic",
     output-info :: <output-info>)
    => ();
  format(output-info.output-info-body-stream,
	 "extern descriptor_t * %s(descriptor_t *orig_sp, "
	   "heapptr_t A0 /* self */, long A1 /* nargs */, "
	   "heapptr_t A2 /* next-info */);\n\n",
	 name);
end;

define method emit-region
    (region :: <simple-region>, output-info :: <output-info>)
    => ();
  for (assign = region.first-assign then assign.next-op,
       while: assign)
    emit-assignment(assign.defines, assign.depends-on.source-exp, output-info);
  end;
end;

define method emit-region (region :: <compound-region>,
			   output-info :: <output-info>)
    => ();
  for (subregion in region.regions)
    emit-region(subregion, output-info);
  end;
end;

define method emit-region (region :: <if-region>, output-info :: <output-info>)
    => ();
  let stream = output-info.output-info-guts-stream;
  let cond = ref-leaf(*boolean-rep*, region.depends-on.source-exp, output-info);
  spew-pending-defines(output-info);
  let initial-depth = output-info.output-info-cur-stack-depth;
  format(stream, "if (%s) {\n", cond);
  indent(stream, $indentation-step);
  emit-region(region.then-region, output-info);
  /* ### emit-joins(region.join-region, output-info); */
  spew-pending-defines(output-info);
  indent(stream, -$indentation-step);
  write("}\n", stream);
  let after-then-depth = output-info.output-info-cur-stack-depth;
  output-info.output-info-cur-stack-depth := initial-depth;
  write("else {\n", stream);
  indent(stream, $indentation-step);
  emit-region(region.else-region, output-info);
  /* ### emit-joins(region.join-region, output-info); */
  spew-pending-defines(output-info);
  indent(stream, -$indentation-step);
  write("}\n", stream);
  let after-else-depth = output-info.output-info-cur-stack-depth;
  output-info.output-info-cur-stack-depth
    := max(after-then-depth, after-else-depth);
end;

define method emit-region (region :: <loop-region>,
			   output-info :: <output-info>)
    => ();
  /* ### emit-joins(region.join-region, output-info); */
  spew-pending-defines(output-info);
  let stream = output-info.output-info-guts-stream;
  write("while (1) {\n", stream);
  indent(stream, $indentation-step);
  emit-region(region.body, output-info);
  /* ### emit-joins(region.join-region, output-info); */
  spew-pending-defines(output-info);
  indent(stream, -$indentation-step);
  write("}\n", stream);
end;

define method make-info-for
    (block-region :: <block-region>, output-info :: <output-info>) => res;
  let id = output-info.output-info-next-block;
  output-info.output-info-next-block := id + 1;
  id;
end;

define method emit-region (region :: <block-region>,
			   output-info :: <output-info>)
    => ();
  unless (region.exits)
    error("A block with no exits still exists?");
  end;
  let stream = output-info.output-info-guts-stream;
  emit-region(region.body, output-info);
  /* ### emit-joins(region.join-region, output-info); */
  spew-pending-defines(output-info);
  let half-step = ash($indentation-step, -1);
  indent(stream, - half-step);
  format(stream, "block%d:;\n", get-info-for(region, output-info));
  indent(stream, half-step);
end;

define method emit-region (region :: <unwind-protect-region>,
			   output-info :: <output-info>)
    => ();
  emit-region(region.body, output-info);
end;

define method emit-region (region :: <exit>, output-info :: <output-info>)
    => ();
  /* ### emit-joins(region.join-region, output-info); */
  spew-pending-defines(output-info);
  let stream = output-info.output-info-guts-stream;
  let target = region.block-of;
  for (region = region.parent then region.parent,
       until: region == #f | region == target)
    finally
    unless (region)
      error("Non-local raw exit?");
    end;
  end;
  if (instance?(target, <block-region>))
    format(stream, "goto block%d;\n", get-info-for(target, output-info));
  else
    format(stream, "abort();\n");
  end;
end;

define method emit-region (return :: <return>, output-info :: <output-info>)
    => ();
  /* ### emit-joins(region.join-region, output-info); */
  let function :: <fer-function-region> = return.block-of;
  let function-info = get-info-for(function, output-info);
  let result-rep = function-info.function-info-result-representation;
  emit-return(return, result-rep, output-info);
end;

define method emit-return
    (return :: <return>, result-rep == #"doesn't-return",
     output-info :: <output-info>)
    => ();
  error("have a return region for a function that doesn't return?");
end;

define method emit-return
    (return :: <return>, result-rep == #"cluster",
     output-info :: <output-info>)
    => ();
  let stream = output-info.output-info-guts-stream;
  let results = return.depends-on;
  if (results & instance?(results.source-exp, <abstract-variable>)
	& instance?(results.source-exp.var-info, <values-cluster-info>))
    let (bottom-name, top-name)
      = consume-cluster(results.source-exp, output-info);
    unless (bottom-name = "orig_sp")
      error("Delivering a cluster that isn't at the bottom of the frame?");
    end;
    spew-pending-defines(output-info);
    format(stream, "return %s;\n", top-name);
  else
    for (dep = results then dep.dependent-next,
	 count from 0,
	 while: dep)
      format(stream, "orig_sp[%d] = %s;\n", count,
	     ref-leaf(*general-rep*, dep.source-exp, output-info));
    finally
      spew-pending-defines(output-info);
      format(stream, "return orig_sp + %d;\n", count);
    end;
  end;
end;

define method emit-return
    (return :: <return>, result-rep :: <representation>,
     output-info :: <output-info>)
    => ();
  let stream = output-info.output-info-guts-stream;
  let expr = ref-leaf(result-rep, return.depends-on.source-exp, output-info);
  spew-pending-defines(output-info);
  format(stream, "return %s;\n", expr);
end;

define method emit-return
    (return :: <return>, result-rep == #(), output-info :: <output-info>)
    => ();
  spew-pending-defines(output-info);
  write("return;\n", output-info.output-info-guts-stream);
end;

define method emit-return
    (return :: <return>, result-reps :: <list>, output-info :: <output-info>)
    => ();
  let stream = output-info.output-info-guts-stream;  
  let temp = new-local(output-info, modifier: "temp");
  let function = return.block-of;
  let function-info = get-info-for(function, output-info);
  let name = pick-result-structure(result-reps, output-info);
  format(output-info.output-info-vars-stream, "struct %s %s;\n",
	 name, temp);
  for (rep in result-reps,
       index from 0,
       dep = return.depends-on then dep.dependent-next)
    format(stream, "%s.R%d = %s;\n",
	   temp, index, ref-leaf(rep, dep.source-exp, output-info));
  end;
  spew-pending-defines(output-info);
  format(stream, "return %s;\n", temp);
end;


define method block-id (region :: <false>) => id :: <false>;
  #f;
end;

define method block-id (region :: <region>) => id :: false-or(<fixed-integer>);
  region.parent.block-id;
end;

define method block-id (region :: <block-region>)
    => id :: false-or(<fixed-integer>);
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
			       output-info :: <output-info>)
    => ();
  if (defines)
    if (instance?(var.var-info, <values-cluster-info>))
      let (bottom-name, top-name) = consume-cluster(var, output-info);
      deliver-cluster(defines, bottom-name, top-name,
		      var.derived-type.min-values, output-info);
    else
      let rep = if (instance?(defines.var-info, <values-cluster-info>))
		  *general-rep*;
		else
		  variable-representation(defines, output-info)
		end;

      deliver-result(defines, ref-leaf(rep, var, output-info), rep, #f,
		     output-info);
    end;
  end;
end;

define method emit-assignment (defines :: false-or(<definition-site-variable>),
			       expr :: <literal-constant>,
			       output-info :: <output-info>)
    => ();
  if (defines)
    let rep-hint = if (instance?(defines.var-info, <values-cluster-info>))
		     *general-rep*;
		   else
		     variable-representation(defines, output-info)
		   end;
    let (expr, rep) = c-expr-and-rep(expr.value, rep-hint, output-info);
    deliver-result(defines, expr, rep, #f, output-info);
  end;
end;

define method emit-assignment (defines :: false-or(<definition-site-variable>),
			       leaf :: <function-literal>,
			       output-info :: <output-info>)
    => ();
  deliver-result(defines, ref-leaf(*heap-rep*, leaf, output-info),
		 *heap-rep*, #f, output-info);
end;

define method emit-assignment (defines :: false-or(<definition-site-variable>),
			       leaf :: <definition-constant-leaf>,
			       output-info :: <output-info>)
    => ();
  let info = get-info-for(leaf.const-defn, output-info);
  deliver-result(defines, info.backend-var-info-name,
		  info.backend-var-info-rep, #f, output-info);
end;

define method emit-assignment (results :: false-or(<definition-site-variable>),
			       leaf :: <uninitialized-value>,
			       output-info :: <output-info>)
    => ();
  if (results)
    let rep = variable-representation(results, output-info);
    if (rep == *general-rep*)
      deliver-result(results, "0", *heap-rep*, #f, output-info);
    else
      deliver-result(results, "0", rep, #f, output-info);
    end;
  end;
end;

define method emit-assignment
    (results :: false-or(<definition-site-variable>),
     call :: union(<unknown-call>, <error-call>),
     output-info :: <output-info>)
    => ();
  let setup-stream = make(<byte-string-output-stream>);
  let function = call.depends-on.source-exp;
  let use-generic-entry?
    = instance?(call, <unknown-call>) & call.use-generic-entry?;
  let (next-info, arguments)
    = if (use-generic-entry?)
	let dep = call.depends-on.dependent-next;
	values(ref-leaf(*heap-rep*, dep.source-exp, output-info),
	       dep.dependent-next);
      else
	values(#f, call.depends-on.dependent-next);
      end;
  let (args, sp) = cluster-names(output-info.output-info-cur-stack-depth);
  for (arg-dep = arguments then arg-dep.dependent-next,
       count from 0,
       while: arg-dep)
    format(setup-stream, "%s[%d] = %s;\n", args, count,
	   ref-leaf(*general-rep*, arg-dep.source-exp, output-info));
  finally
    let (entry, name)
      = xep-expr-and-name(function, use-generic-entry?, output-info);
    let func = ref-leaf(*heap-rep*, function, output-info);
    spew-pending-defines(output-info);
    let stream = output-info.output-info-guts-stream;
    write(setup-stream.string-output-stream-string, stream);
    if (name)
      format(stream, "/* %s */\n", name);
    end;
    if (results)
      format(stream, "%s = ", sp);
    end;
    format(stream, "%s(%s + %d, %s, %d", entry, args, count, func, count);
    if (next-info)
      write(", ", stream);
      write(next-info, stream);
    end;
    write(");\n", stream);
    deliver-cluster(results, args, sp, call.derived-type.min-values,
		    output-info);
  end;
end;

define method xep-expr-and-name
    (func :: <leaf>, generic-entry? :: <boolean>, output-info :: <output-info>)
    => (expr :: <string>, name :: false-or(<string>));
  spew-pending-defines(output-info);
  values(format-to-string(if (generic-entry?)
			    "GENERIC_ENTRY(%s)";
			  else
			    "GENERAL_ENTRY(%s)";
			  end,
			  ref-leaf(*heap-rep*, func, output-info)),
	 #f);
end;

define method xep-expr-and-name
    (func :: <function-literal>, generic-entry? :: <boolean>,
     output-info :: <output-info>)
    => (expr :: <string>, name :: <string>);
  if (generic-entry?)
    error("%= doesn't have a generic entry.", func);
  end;
  let general-entry = func.general-entry;
  let entry-info = get-info-for(general-entry, output-info);
  let entry-name = main-entry-name(entry-info, output-info);
  maybe-emit-prototype(entry-name, entry-info, output-info);
  values(entry-name, general-entry.name);
end;

define method xep-expr-and-name
    (func :: <method-literal>, generic-entry? :: <true>,
     output-info :: <output-info>)
    => (expr :: <string>, name :: <string>);
  let generic-entry = func.generic-entry;
  let entry-info = get-info-for(generic-entry, output-info);
  let entry-name = main-entry-name(entry-info, output-info);
  maybe-emit-prototype(entry-name, entry-info, output-info);
  values(entry-name, generic-entry.name);
end;

define method xep-expr-and-name
    (func :: <definition-constant-leaf>, generic-entry? :: <boolean>,
     output-info :: <output-info>,
     #next next-method)
    => (expr :: <string>, name :: <string>);
  let defn = func.const-defn;
  let (expr, name) = xep-expr-and-name(defn, generic-entry?, output-info);
  values(expr | next-method(),
	 name | format-to-string("%s", defn.defn-name));
end;

define method xep-expr-and-name
    (func :: <literal-constant>, generic-entry? :: <boolean>,
     output-info :: <output-info>, #next next-method)
    => (expr :: false-or(<string>), name :: false-or(<string>));
  let ctv = func.value;
  let (expr, name) = xep-expr-and-name(ctv, generic-entry?, output-info);
  values(expr | next-method(),
	 name | ctv.ct-function-name);
end;

define method xep-expr-and-name
    (defn :: <abstract-constant-definition>, generic-entry? :: <boolean>,
     output-info :: <output-info>)
    => (expr :: false-or(<string>), name :: false-or(<string>));
  let ctv = ct-value(defn);
  if (ctv)
    xep-expr-and-name(ctv, generic-entry?, output-info);
  else
    values(#f, #f);
  end;
end;

define method xep-expr-and-name
    (ctv :: <ct-function>, generic-entry? :: <boolean>,
     output-info :: <output-info>)
    => (expr :: false-or(<string>), name :: false-or(<string>));
  if (generic-entry?)
    error("%= doesn't have a generic entry.", ctv);
  end;
  let info = get-info-for(ctv, output-info);
  let name = general-entry-name(info, output-info);
  maybe-emit-prototype(name, #"general", output-info);
  values(name, ctv.ct-function-name);
end;

define method xep-expr-and-name
    (ctv :: <ct-generic-function>, generic-entry? :: <boolean>,
     output-info :: <output-info>)
    => (expr :: false-or(<string>), name :: false-or(<string>));
  if (generic-entry?)
    error("%= doesn't have a generic entry.", ctv);
  end;
  let defn = ctv.ct-function-definition;
  if (defn)
    let discriminator = defn.generic-defn-discriminator;
    if (discriminator)
      xep-expr-and-name(discriminator, #f, output-info);
    else
      values(#f, #f);
    end;
  else
    values(#f, #f);
  end;
end;

define method xep-expr-and-name
    (ctv :: <ct-method>, generic-entry? :: <true>,
     output-info :: <output-info>)
    => (expr :: false-or(<string>), name :: false-or(<string>));
  let info = get-info-for(ctv, output-info);
  let name = generic-entry-name(info, output-info);
  maybe-emit-prototype(name, #"generic", output-info);
  values(name, ctv.ct-function-name);
end;


define method emit-assignment
    (results :: false-or(<definition-site-variable>), call :: <known-call>,
     output-info :: <output-info>)
    => ();
  let function = call.depends-on.source-exp;
  let func-info = find-main-entry-info(function, output-info);
  let stream = make(<byte-string-output-stream>);
  let c-name = main-entry-name(func-info, output-info);
  let (sp, new-sp) = cluster-names(output-info.output-info-cur-stack-depth);
  format(stream, "%s(%s", c-name, sp);
  for (arg-dep = call.depends-on.dependent-next then arg-dep.dependent-next,
       rep in func-info.function-info-argument-representations)
    unless (arg-dep)
      error("Not enough arguments in a known call?");
    end;
      write(", ", stream);
      write(ref-leaf(rep, arg-dep.source-exp, output-info), stream);
  finally
    if (arg-dep)
      error("Too many arguments in a known call?");
    end;
  end;
  write(')', stream);
  let call = string-output-stream-string(stream);
  format(output-info.output-info-guts-stream, "/* %s */\n",
	 func-info.function-info-name);
  let result-rep = func-info.function-info-result-representation;
  if (results == #f | result-rep == #())
    format(output-info.output-info-guts-stream, "%s;\n", call);
    deliver-results(results, #[], #f, output-info);
  elseif (result-rep == #"doesn't-return")
    error("Trying to get some values back from a function that "
	    "doesn't return?");
  elseif (result-rep == #"cluster")
    format(output-info.output-info-guts-stream, "%s = %s;\n", new-sp, call);
    deliver-cluster(results, sp, new-sp,
		    func-info.function-info-result-type.min-values,
		    output-info);
  elseif (instance?(result-rep, <list>))
    let temp = new-local(output-info, modifier: "temp");
    format(output-info.output-info-vars-stream, "struct %s %s;\n",
	   pick-result-structure(result-rep, output-info),
	   temp);
    format(output-info.output-info-guts-stream, "%s = %s;\n", temp, call);
    let result-exprs = make(<vector>, size: result-rep.size);
    for (rep in result-rep,
	 index from 0)
      result-exprs[index]
	:= pair(format-to-string("%s.R%d", temp, index), rep);
    end;
    deliver-results(results, result-exprs, #f, output-info);
  else
    deliver-result(results, call, result-rep, #t, output-info);
  end;
end;

define method find-main-entry-info
    (func :: <function-literal>, output-info :: <output-info>)
    => res :: <function-info>;
  let entry = func.main-entry;
  let info = get-info-for(entry, output-info);
  maybe-emit-prototype(main-entry-name(info, output-info), info, output-info);
  info;
end;

define method find-main-entry-info
    (func :: <definition-constant-leaf>, output-info :: <output-info>)
    => res :: <function-info>;
  find-main-entry-info(func.const-defn.ct-value, output-info);
end;

define method find-main-entry-info
    (func :: <literal-constant>, output-info :: <output-info>)
    => res :: <function-info>;
  find-main-entry-info(func.value, output-info);
end;

define method find-main-entry-info
    (defn :: <generic-definition>, output-info :: <output-info>)
    => res :: <function-info>;
  let discriminator = defn.generic-defn-discriminator;
  if (discriminator)
    find-main-entry-info(discriminator, output-info);
  else
    error("Known call of a generic function without a static discriminator?");
  end;
end;

define method find-main-entry-info
    (defn :: <abstract-method-definition>, output-info :: <output-info>)
    => res :: <function-info>;
  find-main-entry-info(defn.ct-value, output-info);
end;

define method find-main-entry-info
    (ctv :: <ct-function>, output-info :: <output-info>)
    => res :: <function-info>;
  let info = get-info-for(ctv, output-info);
  maybe-emit-prototype(main-entry-name(info, output-info), info, output-info);
  info;
end;

define method find-main-entry-info
    (ctv :: <ct-generic-function>, output-info :: <output-info>)
    => res :: <function-info>;
  find-main-entry-info(ctv.ct-function-definition, output-info);
end;


define method emit-assignment
    (results :: false-or(<definition-site-variable>), call :: <mv-call>, 
     output-info :: <output-info>)
    => ();
  let stream = output-info.output-info-guts-stream;
  let function = call.depends-on.source-exp;
  let use-generic-entry? = call.use-generic-entry?;
  let (next-info, cluster)
    = if (use-generic-entry?)
	let dep = call.depends-on.dependent-next;
	values(ref-leaf(*heap-rep*, dep.source-exp, output-info),
	       dep.dependent-next.source-exp);
      else
	values(#f, call.depends-on.dependent-next.source-exp);
      end;
  let (entry, name)
    = xep-expr-and-name(function, use-generic-entry?, output-info);
  let func = ref-leaf(*heap-rep*, function, output-info);
  spew-pending-defines(output-info);
  let (bottom-name, top-name) = consume-cluster(cluster, output-info);
  if (name)
    format(stream, "/* %s */\n", name);
  end;
  if (results)
    format(stream, "%s = ", top-name);
  end;
  format(stream, "%s(%s, %s, %s - %s",
	 entry, top-name, func, top-name, bottom-name);
  if (next-info)
    write(", ", stream);
    write(next-info, stream);
  end;
  write(");\n", stream);
  deliver-cluster(results, bottom-name, top-name, call.derived-type.min-values,
		  output-info);
end;

define method emit-assignment (defines :: false-or(<definition-site-variable>),
			       expr :: <primitive>,
			       output-info :: <output-info>)
    => ();
  let emitter = expr.info.primitive-emitter | default-primitive-emitter;
  emitter(defines, expr, output-info);
end;

define method emit-assignment
    (defines :: false-or(<definition-site-variable>), expr :: <catch>,
     output-info :: <output-info>)
    => ();
  let func = extract-operands(expr, output-info, *heap-rep*);
  let (values, sp) = cluster-names(output-info.output-info-cur-stack-depth);
  let stream = output-info.output-info-guts-stream;
  if (defines)
    format(stream, "%s = ", sp);
  end;
  let catch-defn = dylan-defn(#"catch");
  assert(instance?(catch-defn, <abstract-method-definition>));
  let catch-info = find-main-entry-info(catch-defn, output-info);
  format(stream, "catch(%s, %s, %s);\n",
	 main-entry-name(catch-info, output-info), values, func);
  if (defines)
    deliver-cluster(defines, values, sp, 0, output-info);
  end;
end;

define method emit-assignment (defines :: false-or(<definition-site-variable>),
			       expr :: <prologue>,
			       output-info :: <output-info>)
    => ();
  let function-info = get-info-for(expr.function, output-info);
  deliver-results(defines,
		  map(method (rep, index)
			pair(format-to-string("A%d", index),
			     rep);
		      end,
		      function-info.function-info-argument-representations,
		      make(<range>, from: 0)),
		  #f, output-info);
end;

define method emit-assignment
    (defines :: false-or(<definition-site-variable>),
     ref :: <module-var-ref>, output-info :: <output-info>)
    => ();
  let defn = ref.variable;
  let info = get-info-for(defn, output-info);
  let name = info.backend-var-info-name;
  maybe-emit-prototype(name, defn, output-info);
  let rep = info.backend-var-info-rep;
  let stream = output-info.output-info-guts-stream;
  unless (defn.defn-init-value)
    if (rep.representation-has-bottom-value?)
      let temp = new-local(output-info, modifier: "temp");
      format(output-info.output-info-vars-stream, "%s %s;\n",
	     rep.representation-c-type, temp);
      format(stream, "if ((%s = %s).heapptr == NULL) abort();\n", temp, name);
      name := temp;
    else
      format(stream, "if (!%s_initialized) abort();\n", name);
    end;
  end;
  deliver-result(defines, name, rep, #f, output-info);
end;

define method emit-assignment
    (defines :: false-or(<definition-site-variable>),
     set :: <module-var-set>, output-info :: <output-info>)
    => ();
  let defn = set.variable;
  let info = get-info-for(defn, output-info);
  let target = info.backend-var-info-name;
  maybe-emit-prototype(target, defn, output-info);
  let rep = info.backend-var-info-rep;
  let source = extract-operands(set, output-info, rep);
  spew-pending-defines(output-info);
  emit-copy(target, rep, source, rep, output-info);
  unless (defn.defn-init-value | rep.representation-has-bottom-value?)
    let stream = output-info.output-info-guts-stream;
    format(stream, "%s_initialized = TRUE;\n", target);
  end;
  deliver-results(defines, #[], #f, output-info);
end;

define method emit-assignment
    (results :: false-or(<definition-site-variable>),
     call :: <self-tail-call>, output-info :: <output-info>)
    => ();
  spew-pending-defines(output-info);
  let function = call.self-tail-call-of;
  for (param = function.prologue.dependents.dependent.defines
	 then param.definer-next,
       closure-var = function.environment.closure-vars
	 then closure-var.closure-next,
       index from 0,
       while: closure-var & param)
  finally
    let stream = output-info.output-info-guts-stream;
    for (param = param then param.definer-next,
	 arg-dep = call.depends-on then arg-dep.dependent-next,
	 index from index,
	 while: arg-dep & param)
      let (name, rep) = c-name-and-rep(param, output-info);
      format(stream, "A%d = %s;\n",
	     index, ref-leaf(rep, arg-dep.source-exp, output-info));
    finally
      if (arg-dep | param)
	error("Wrong number of operands in a self-tail-call?");
      end;
    end;
  end;
  deliver-results(results, #[], #f, output-info);
end;

define method emit-assignment
    (results :: false-or(<definition-site-variable>),
     op :: <slot-ref>, output-info :: <output-info>)
    => ();
  let offset = op.slot-offset;
  let instance-leaf = op.depends-on.source-exp;
  let instance-rep = pick-representation(instance-leaf.derived-type, #"speed");
  let slot = op.slot-info;
  let slot-rep = slot.slot-representation;
  let (expr, now-dammit?)
    = if (~zero?(offset) & instance?(instance-rep, <immediate-representation>))
	// Extracting the data-word.
	unless (instance-rep == slot-rep
		  | (representation-data-word-member(instance-rep)
		       = representation-data-word-member(slot-rep)))
	  error("The instance and slot representations don't match in a "
		  "data-word reference?");
	end;
	values(ref-leaf(instance-rep, instance-leaf, output-info), #f);
      else
	let instance-expr = ref-leaf(*heap-rep*, instance-leaf, output-info);
	let offset-expr
	  = if (op.depends-on.dependent-next)
	      let index = ref-leaf(*long-rep*,
				   op.depends-on.dependent-next.source-exp,
				   output-info);
	      format-to-string("%d + %s * sizeof(%s)",
			       offset, index, slot-rep.representation-c-type);
	    else
	      format-to-string("%d", offset);
	    end;
	spew-pending-defines(output-info);
	values(format-to-string("SLOT(%s, %s, %s)",
				instance-expr,
				slot-rep.representation-c-type,
				offset-expr),
	       ~slot.slot-read-only?);
      end;
  deliver-result(results, expr, slot-rep, now-dammit?, output-info);
end;

define method emit-assignment
    (results :: false-or(<definition-site-variable>),
     op :: <slot-set>, output-info :: <output-info>)
    => ();
  let slot = op.slot-info;
  let offset = op.slot-offset;
  let slot-rep = slot.slot-representation;
  if (instance?(slot, <vector-slot-info>))
    let (new, instance, index)
      = extract-operands(op, output-info, slot-rep, *heap-rep*, *long-rep*);
    let c-type = slot-rep.representation-c-type;
    format(output-info.output-info-guts-stream,
	   "SLOT(%s, %s, %d + %s * sizeof(%s)) = %s;\n",
	   instance, c-type, offset, index, c-type, new);
  else
    let (new, instance)
      = extract-operands(op, output-info, slot-rep, *heap-rep*);
    format(output-info.output-info-guts-stream,
	   "SLOT(%s, %s, %d) = %s;\n",
	   instance, slot-rep.representation-c-type, offset, new);
  end;
  deliver-results(results, #[], #f, output-info);
end;


define method emit-assignment
    (results :: false-or(<definition-site-variable>),
     op :: <truly-the>, output-info :: <output-info>)
    => ();
  if (results)
    let rep = variable-representation(results, output-info);
    let source = extract-operands(op, output-info, rep);
    deliver-result(results, source, rep, #f, output-info);
  end;
end;


define method deliver-cluster
    (defines :: false-or(<definition-site-variable>),
     src-start :: <string>, src-end :: <string>,
     min-values :: <fixed-integer>, output-info :: <output-info>)
    => ();

  if (defines)
    let stream = output-info.output-info-guts-stream;
    if (instance?(defines.var-info, <values-cluster-info>))
      let (dst-start, dst-end) = produce-cluster(defines, output-info);
      if (src-start ~= dst-start)
	format(stream, "%s = %s;\n", dst-end, dst-start);
	format(stream, "while (%s < %s) {\n", src-start, src-end);
	format(stream, "    *%s++ = *%s++;\n", dst-end, src-start);
      elseif (src-end ~= dst-end)
	format(stream, "%s = %s;\n", dst-end, src-end);
      end;
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
	let source = format-to-string("%s[%d]", src-start, index);
	deliver-single-result(var, source, *general-rep*, #t, output-info);
      end;
    end;
  end;
end;

define method deliver-results
    (defines :: false-or(<definition-site-variable>), values :: <sequence>,
     now-dammit? :: <boolean>, output-info :: <output-info>)
    => ();
  if (defines & instance?(defines.var-info, <values-cluster-info>))
    let stream = output-info.output-info-guts-stream;
    let (bottom-name, top-name) = produce-cluster(defines, output-info);
    format(stream, "%s = %s + %d;\n", top-name, bottom-name, values.size);
    for (val in values, index from 0)
      emit-copy(format-to-string("%s[%d]", bottom-name, index), *general-rep*,
		val.head, val.tail, output-info);
    end;
  else
    for (var = defines then var.definer-next,
	 val in values,
	 while: var)
      deliver-single-result(var, val.head, val.tail, now-dammit?, output-info);
    finally
      if (var)
	let false = make(<literal-false>);
	for (var = var then var.definer-next,
	     while: var)
	  let target-rep = variable-representation(var, output-info);
	  let (source-name, source-rep)
	    = c-expr-and-rep(false, target-rep, output-info);
	  deliver-single-result(var, source-name, source-rep, #f, output-info);
	end;
      end;
    end;
  end;
end;

define method deliver-result
    (defines :: false-or(<definition-site-variable>), value :: <string>,
     rep :: <representation>, now-dammit? :: <boolean>,
     output-info :: <output-info>)
    => ();
  if (defines)
    if (instance?(defines.var-info, <values-cluster-info>))
      let stream = output-info.output-info-guts-stream;
      let (bottom-name, top-name) = produce-cluster(defines, output-info);
      format(stream, "%s = %s + 1;\n", top-name, bottom-name);
      emit-copy(format-to-string("%s[0]", bottom-name), *general-rep*,
		value, rep, output-info);
    else
      deliver-single-result(defines, value, rep, now-dammit?, output-info);
      let next = defines.definer-next;
      if (next)
	let false = make(<literal-false>);
	for (var = next then var.definer-next,
	     while: var)
	  let target-rep = variable-representation(var, output-info);
	  let (source-name, source-rep)
	    = c-expr-and-rep(false, target-rep, output-info);
	  deliver-single-result(var, source-name, source-rep, #f, output-info);
	end;
      end;
    end;
  end;
end;

define method deliver-single-result
    (var :: <abstract-variable>, // ### Should really be ssa-variable
     source :: <string>, source-rep :: <representation>,
     now-dammit? :: <boolean>, output-info :: <output-info>)
    => ();
  if (var.dependents)
    if (now-dammit? | var.dependents.source-next)
      let (target-name, target-rep) = c-name-and-rep(var, output-info);
      emit-copy(target-name, target-rep, source, source-rep, output-info);
    else
      output-info.output-info-local-vars[var] := pair(source, source-rep);
    end;
  end;
end;

define method deliver-single-result
    (var :: <initial-definition>, source :: <string>,
     source-rep :: <representation>, now-dammit? :: <boolean>,
     output-info :: <output-info>)
    => ();
  spew-pending-defines(output-info);
  deliver-single-result(var.definition-of, source, source-rep, now-dammit?,
			output-info);
end;


// Value manipulation utilities.

define method spew-pending-defines (output-info :: <output-info>) => ();
  let table = output-info.output-info-local-vars;
  let vars = key-sequence(table);
  let stream = output-info.output-info-guts-stream;
  for (var in vars)
    let (target, target-rep) = c-name-and-rep(var, output-info);
    let noise = table[var];
    emit-copy(target, target-rep, noise.head, noise.tail, output-info);
    remove-key!(table, var);
  end;
end;

define method ref-leaf (target-rep :: <representation>,
			leaf :: <abstract-variable>,
			output-info :: <output-info>)
    => res :: <string>;
  let (expr, rep)
    = begin
	let info
	  = element(output-info.output-info-local-vars, leaf, default: #f);
	if (info)
	  remove-key!(output-info.output-info-local-vars, leaf);
	  values(info.head, info.tail);
	else
	  c-name-and-rep(leaf, output-info);
	end;
      end;
  conversion-expr(target-rep, expr, rep, output-info);
end;

define method ref-leaf (target-rep :: <representation>,
			leaf :: <literal-constant>,
			output-info :: <output-info>)
    => res :: <string>;
  let (expr, rep) = c-expr-and-rep(leaf.value, target-rep, output-info);
  conversion-expr(target-rep, expr, rep, output-info);
end;

define method ref-leaf (target-rep :: <representation>,
			leaf :: <definition-constant-leaf>,
			output-info :: <output-info>)
    => res :: <string>;
  let info = get-info-for(leaf.const-defn, output-info);
  conversion-expr(target-rep, info.backend-var-info-name,
		  info.backend-var-info-rep, output-info);
end;

define method ref-leaf (target-rep :: <representation>,
			leaf :: <function-literal>,
			output-info :: <output-info>)
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
    let ctv-info = get-info-for(ctv, output-info);
    ctv-info.function-info-general-entry-name
      := main-entry-name(get-info-for(leaf.general-entry, output-info),
			 output-info);
    if (instance?(leaf, <method-literal>))
      ctv-info.function-info-generic-entry-name
	:= main-entry-name(get-info-for(leaf.generic-entry, output-info),
			   output-info);
    end;
    leaf.ct-function := ctv;
  end;
  let (expr, rep) = c-expr-and-rep(ctv, target-rep, output-info);
  conversion-expr(target-rep, expr, rep, output-info);
end;

define method ref-leaf (target-rep :: <representation>,
			leaf :: <uninitialized-value>,
			output-info :: <output-info>)
    => res :: <string>;
  if (target-rep == *general-rep*)
    conversion-expr(target-rep, "0", *heap-rep*, output-info);
  else
    "0";
  end;
end;

define method c-expr-and-rep (lit :: <ct-value>,
			      rep-hint :: <representation>,
			      output-info :: <output-info>)
    => (name :: <string>, rep :: <representation>);
  let info = get-info-for(lit, output-info);
  values(info.const-info-expr
	   | (info.const-info-expr := new-root(lit, output-info)),
	 *general-rep*);
end;

define method c-expr-and-rep
    (lit :: <literal-true>, rep-hint :: <immediate-representation>,
     output-info :: <output-info>)
    => (name :: <string>, rep :: <representation>);
  values("TRUE", rep-hint);
end;

define method c-expr-and-rep
    (lit :: <literal-false>, rep-hint :: <immediate-representation>,
     output-info :: <output-info>)
    => (name :: <string>, rep :: <representation>);
  values("FALSE", rep-hint);
end;

define method c-expr-and-rep (lit :: <literal-fixed-integer>,
			      rep-hint :: <representation>,
			      output-info :: <output-info>)
    => (name :: <string>, rep :: <representation>);
  values(format-to-string("%d", lit.literal-value),
	 pick-representation(dylan-value(#"<fixed-integer>"), #"speed"));
end;

define method c-expr-and-rep (lit :: <literal-single-float>,
			      rep-hint :: <immediate-representation>,
			      output-info :: <output-info>)
    => (name :: <string>, rep :: <representation>);
  values(float-to-string(lit.literal-value, 8),
	 pick-representation(dylan-value(#"<single-float>"), #"speed"));
end;

define method c-expr-and-rep (lit :: <literal-double-float>,
			      rep-hint :: <immediate-representation>,
			      output-info :: <output-info>)
    => (name :: <string>, rep :: <representation>);
  values(float-to-string(lit.literal-value, 16),
	 pick-representation(dylan-value(#"<double-float>"), #"speed"));
end;

define method c-expr-and-rep (lit :: <literal-extended-float>,
			      rep-hint :: <immediate-representation>,
			      output-info :: <output-info>)
    => (name :: <string>, rep :: <representation>);
  values(float-to-string(lit.literal-value, 35),
	 pick-representation(dylan-value(#"<extended-float>"), #"speed"));
end;

define method float-to-string (value :: <float>, digits :: <fixed-integer>)
    => res :: <string>;
  float-to-string(as(<ratio>, value), digits);
end;

define method float-to-string (value :: <integer>, digits :: <fixed-integer>)
    => res :: <string>;
  float-to-string(ratio(value, 1), digits);
end;

define method float-to-string (value :: <ratio>, digits :: <fixed-integer>)
    => res :: <string>;
  if (zero?(value))
    "0.0";
  else
    let stream = make(<byte-string-output-stream>);
    if (negative?(value))
      value := -value;
      write('-', stream);
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
    write("0.", stream);
    let zeros = 0;
    for (count from 0 below digits,
	 until: zero?(fraction))
      let (digit, remainder) = floor(fraction * ten);
      if (zero?(digit))
	zeros := zeros + 1;
      else
	for (i from 0 below zeros)
	  write('0', stream);
	end;
	write(as(<character>, as(<fixed-integer>, digit) + 48), stream);
	zeros := 0;
      end;
      fraction := remainder;
    end;
    write('e', stream);
    print(exponent, stream);
    stream.string-output-stream-string;
  end;
end;


define method c-expr-and-rep (lit :: <literal-character>,
			      rep-hint :: <representation>,
			      output-info :: <output-info>)
    => (name :: <string>, rep :: <representation>);
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
	   format-to-string("'\\%o'", as(<integer>, char));
	 elseif (char == '\'' | char == '\\')
	   format-to-string("'\\%c'", char);
	 elseif (char <= '~')
	   format-to-string("'%c'", lit.literal-value);
	 else
	   format-to-string("%d", as(<integer>, char));
	 end,
	 pick-representation(dylan-value(#"<character>"), #"speed"));
end;

define method c-expr-and-rep (ep :: <ct-entry-point>,
			      rep-hint :: <representation>,
			      output-info :: <output-info>)
    => (name :: <string>, rep :: <representation>);
  values(format-to-string("((void *)%s)",
			  entry-point-c-name(ep, output-info)),
	 *ptr-rep*);
end;


define generic emit-copy
    (target :: <string>, target-rep :: <representation>,
     source :: <string>, source-rep :: <representation>,
     output-info :: <output-info>)
    => ();

define method emit-copy
    (target :: <string>, target-rep :: <general-representation>,
     source :: <string>, source-rep :: <general-representation>,
     output-info :: <output-info>)
    => ();
  let stream = output-info.output-info-guts-stream;
  format(stream, "%s = %s;\n", target, source);
end;

define method emit-copy
    (target :: <string>, target-rep :: <general-representation>,
     source :: <string>, source-rep :: <data-word-representation>,
     output-info :: <output-info>)
    => ();
  let stream = output-info.output-info-guts-stream;
  let (proxy, proxy-rep)
    = c-expr-and-rep(make(<proxy>, for: source-rep.representation-class),
		     *heap-rep*, output-info);
  format(stream, "%s.heapptr = %s;\n",
	 target, conversion-expr(*heap-rep*, proxy, proxy-rep, output-info));
  format(stream, "%s.dataword.%s = %s;\n",
	 target, source-rep.representation-data-word-member, source);
end;

define method emit-copy
    (target :: <string>, target-rep :: <general-representation>,
     source :: <string>, source-rep :: <c-representation>,
     output-info :: <output-info>)
    => ();
  let stream = output-info.output-info-guts-stream;
  let heapptr = conversion-expr(*heap-rep*, source, source-rep, output-info);
  format(stream, "%s.heapptr = %s;\n", target, heapptr);
  format(stream, "%s.dataword.l = 0;\n", target);
end;

define method emit-copy
    (target :: <string>, target-rep :: <c-representation>,
     source :: <string>, source-rep :: <c-representation>,
     output-info :: <output-info>)
    => ();
  let stream = output-info.output-info-guts-stream;
  let expr = conversion-expr(target-rep, source, source-rep, output-info);
  format(stream, "%s = %s;\n", target, expr);
end;


define method conversion-expr
    (target-rep :: <general-representation>,
     source :: <string>, source-rep :: <c-representation>,
     output-info :: <output-info>)
    => res :: <string>;
  if (target-rep == source-rep)
    source;
  else
    let temp = new-local(output-info, modifier: "temp");
    format(output-info.output-info-vars-stream, "%s %s;\n",
	   target-rep.representation-c-type, temp);
    emit-copy(temp, target-rep, source, source-rep, output-info);
    temp;
  end;
end;

define method conversion-expr
    (target-rep :: <c-representation>,
     source :: <string>, source-rep :: <c-representation>,
     output-info :: <output-info>)
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
		    output-info);
  else
    let from-more-general = target-rep.representation-from-more-general;
    let more-general = conversion-expr(target-rep.more-general-representation,
				       source, source-rep, output-info);
    select (from-more-general)
      #t => more-general;
      #f => error("Can't happen.");
      otherwise => format-to-string(from-more-general, more-general);
    end;
  end;
end;
