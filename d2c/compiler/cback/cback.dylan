module: cback
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/cback/cback.dylan,v 1.31 1995/05/03 09:44:44 wlott Exp $
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

define class <output-info> (<object>)
  //
  // Stream for the header file.
  slot output-info-header-stream :: <stream>,
    init-function: make-indenting-string-stream;
  //
  slot output-info-body-stream :: <stream>,
    init-function: make-indenting-string-stream;
  //
  slot output-info-vars-stream :: <stream>,
    init-function: curry(make-indenting-string-stream,
			 indentation: $indentation-step);
  //
  slot output-info-guts-stream :: <stream>,
    init-function: curry(make-indenting-string-stream,
			 indentation: $indentation-step);
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
  // id number for the next global.
  slot output-info-next-global :: <fixed-integer>, init-value: 0;
  //
  // Vector of the initial values for the roots vector.
  slot output-info-init-roots :: <stretchy-vector>,
    init-function: curry(make, <stretchy-vector>);
  //
  // Hash table mapping constants to indices in the roots table.
  slot output-info-constants :: <table>,
    init-function: curry(make, <object-table>);
  //
  // C variable holding the current stack top.
  slot output-info-cur-stack-depth :: <fixed-integer>,
    init-value: 0;
end;


define method output-info-results (output-info :: <output-info>) => ();
  let s = *standard-output*;
  write("/**** Header: */\n", s);
  write(output-info.output-info-header-stream.string-output-stream-string, s);
  write("/**** Body: */\n", s);
  write(output-info.output-info-body-stream.string-output-stream-string, s);
  write("/**** Vars: */\n", s);
  write(output-info.output-info-vars-stream.string-output-stream-string, s);
  write("/**** Guts: */\n", s);
  write(output-info.output-info-guts-stream.string-output-stream-string, s);
  force-output(s);
end;


// Utilities.

define method get-info-for (thing :: <annotatable>,
			    output-info :: <output-info>)
    => res :: <object>;
  thing.info | (thing.info := make-info-for(thing, output-info));
end;

define method new-local (output-info :: <output-info>)
    => res :: <string>;
  let num = output-info.output-info-next-local;
  output-info.output-info-next-local := num + 1;

  format-to-string("L%d", num);
end;

define method new-global (output-info :: <output-info>)
    => res :: <string>;
  let num = output-info.output-info-next-global;
  output-info.output-info-next-global := num + 1;

  format-to-string("G%d", num);
end;

define method new-root (init-value :: union(<false>, <ct-value>),
			output-info :: <output-info>)
  let roots = output-info.output-info-init-roots;
  let index = roots.size;
  add!(roots, init-value);

  format-to-string("roots[%d]", index);
end;

define method debug-name-string (name :: <basic-name>) => res :: <string>;
  as(<string>, name.name-symbol);
end;

define method debug-name-string (name :: <type-cell-name>) => res :: <string>;
  format-to-string("type cell for %s", name.type-cell-name-base.name-symbol);
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
	      $general-rep;
	    end;
  if (instance?(rep, <immediate-representation>))
    let name = new-global(output-info);
    make(<backend-var-info>, representation: rep, name: name);
  else
    let name = new-root(defn.ct-value, output-info);
    make(<backend-var-info>, representation: $general-rep, name: name);
  end;
end;


define method get-info-for (leaf :: <initial-definition>,
			    output-info :: <output-info>)
    => res :: <backend-var-info>;
  get-info-for(leaf.definition-of, output-info);
end;

define method get-info-for (leaf :: <global-variable>,
			    output-info :: <output-info>)
    => res :: <backend-var-info>;
  get-info-for(leaf.var-info.var-defn, output-info);
end;

define method c-name-and-rep (leaf :: <abstract-variable>,
			      // ### Should really be ssa-variable
			      output-info :: <output-info>)
    => (name :: <string>, rep :: <representation>);
  let info = get-info-for(leaf, output-info);
  let name = info.backend-var-info-name;
  unless (name)
    name := new-local(output-info);
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



// Lambda stuff.

define class <lambda-info> (<object>)
  slot lambda-info-lambda :: <lambda>,
    required-init-keyword: lambda:;
  slot lambda-info-main-entry-name :: <byte-string>,
    required-init-keyword: main-entry-name:;
  slot lambda-info-prototype :: <byte-string>,
    required-init-keyword: prototype:;
  slot lambda-info-argument-representations :: <list>,
    required-init-keyword: argument-reps:;
  slot lambda-info-result-representation
    :: type-or(<representation>, <list>,
	       one-of(#"doesn't-return", #"cluster", #"void")),
    required-init-keyword: result-rep:;
end;

define method make-info-for (lambda :: <lambda>, output-info :: <output-info>)
    => res :: <lambda-info>;
  // Compute the prototype.

  let stream = make(<byte-string-output-stream>);
  let name = new-global(output-info);
  let result-type = lambda.result-type;
  let result-rep
    = if (result-type == empty-ctype())
	write("void", stream);
	#"doesn't-return";
      else
	let min-values = result-type.min-values;
	let positionals = result-type.positional-types;
	let rest-type = result-type.rest-value-type;
	if (min-values == positionals.size & rest-type == empty-ctype())
	  if (min-values == 0)
	    write("void", stream);
	    #"void";
	  elseif (min-values == 1)
	    let rep = pick-representation(result-type, #"speed");
	    write(rep.representation-c-type, stream);
	    rep;
	  else
	    let header = output-info.output-info-header-stream;
	    format(header, "struct %s_results {\n", name);
	    indent(header, $indentation-step);
	    let reps
	      = map(method (type, index)
		      let rep = pick-representation(type, #"speed");
		      format(header, "%s R%d;\n",
			     rep.representation-c-type, index);
		      rep;
		    end,
		    positionals,
		    make(<range>, from: 0));
	    indent(header, -$indentation-step);
	    format(header, "};\n");
	    format(stream, "struct %s_results", name);
	    reps;
	  end;
	else
	  write("descriptor_t *", stream);
	  #"cluster";
	end;
      end;

  format(stream, " %s(descriptor_t *orig_sp", name);
  let argument-reps = #();
  for (arg-type in lambda.argument-types,
       index from 0,
       var = lambda.prologue.dependents.dependent.defines
	 then var & var.definer-next)
    let rep = pick-representation(arg-type, #"speed");
    format(stream, ", %s A%d", rep.representation-c-type, index);
    if (var)
      let varinfo = var.var-info;
      if (instance?(varinfo, <debug-named-info>))
	format(stream, " /* %s */", varinfo.debug-name);
      end;
    end;
    argument-reps := pair(rep, argument-reps);
  end;
  write(')', stream);

  make(<lambda-info>,
       lambda: lambda,
       main-entry-name: name,
       prototype: stream.string-output-stream-string,
       argument-reps: reverse!(argument-reps),
       result-rep: result-rep);
end;


// Top level form processors.

define generic emit-tlf-gunk (tlf :: <top-level-form>,
			      output-info :: <output-info>)
    => ();

define method emit-tlf-gunk (tlf :: <top-level-form>,
			     output-info :: <output-info>)
    => ();
end;

define method emit-tlf-gunk (tlf :: <define-bindings-tlf>,
			     output-info :: <output-info>)
    => ();
  for (defn in tlf.tlf-required-defns)
    emit-bindings-definition-gunk(defn, output-info);
  end;
  if (tlf.tlf-rest-defn)
    emit-bindings-definition-gunk(tlf.tlf-rest-defn, output-info);
  end;
end;

define method emit-bindings-definition-gunk
    (defn :: <bindings-definition>, output-info :: <output-info>) => ();
  let info = get-info-for(defn, output-info);
  let stream = output-info.output-info-header-stream;
  let rep = info.backend-var-info-rep;
  if (instance?(rep, <immediate-representation>))
    format(stream, "static %s %s",
	   rep.representation-c-type,
	   info.backend-var-info-name);
    let init-value = defn.ct-value;
    if (init-value)
      let (init-value-expr, init-value-rep)
	= c-expr-and-rep(init-value, rep, output-info);
      format(stream, "= %s;\t/* %s */\n",
	     conversion-expr(rep, init-value-expr, init-value-rep,
			     output-info),
	     debug-name-string(defn.defn-name));
    else
      format(stream, ";\t/* %s */\nstatic int %s_initialized = FALSE;\n",
	     debug-name-string(defn.defn-name),
	     info.backend-var-info-name);
    end;
  else
    format(stream, "/* %s allocated as %s */\n",
	   debug-name-string(defn.defn-name),
	   info.backend-var-info-name);
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

define method emit-bindings-definition-gunk
    (defn :: <constant-method-definition>, output-info :: <output-info>,
     #next next-method)
    => ();
  unless (defn.method-defn-leaf)
    next-method();
  end;
end;


// Control flow emitters

define method emit-lambda (lambda :: <lambda>, output-info :: <output-info>)
    => ();
  output-info.output-info-next-block := 0;
  output-info.output-info-next-local := 0;
  output-info.output-info-cur-stack-depth := 0;
  assert(output-info.output-info-local-vars.size == 0);

  let lambda-info = get-info-for(lambda, output-info);
  let prototype = lambda-info.lambda-info-prototype;
  format(output-info.output-info-header-stream, "static %s;\n", prototype);

  let stream = output-info.output-info-body-stream;
  format(stream, "/* %s */\n", lambda.name);
  format(stream, "static %s\n{\n", prototype);

  let max-depth = analize-stack-usage(lambda);
  for (i from 0 below max-depth)
    format(output-info.output-info-vars-stream,
	   "descriptor_t *cluster_%d_top;\n",
	   i);
  end;

  emit-region(lambda.body, output-info);

  write(output-info.output-info-vars-stream.string-output-stream-string,
	stream);
  write('\n', stream);
  write(output-info.output-info-guts-stream.string-output-stream-string,
	stream);
  write("}\n\n", stream);
end;

define method emit-region (region :: <simple-region>,
			   output-info :: <output-info>)
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
  let cond = ref-leaf($boolean-rep, region.depends-on.source-exp, output-info);
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
  if (region.catcher)
    if (region.catcher.exit-function)
      error("An exit function still exists?");
    end;
    format(stream, "if (!setjmp(### jmpbuf)) {\n");
    indent(stream, $indentation-step);
  end;
  emit-region(region.body, output-info);
  /* ### emit-joins(region.join-region, output-info); */
  spew-pending-defines(output-info);
  if (region.catcher)
    indent(stream, - $indentation-step);
    format(stream, "}\n");
  end;
  let half-step = ash($indentation-step, -1);
  indent(stream, - half-step);
  format(stream, "block%d:\n", get-info-for(region, output-info));
  indent(stream, half-step);
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
  spew-pending-defines(output-info);
  let lambda :: <lambda> = return.block-of;
  let lambda-info = get-info-for(lambda, output-info);
  let result-rep = lambda-info.lambda-info-result-representation;
  emit-return(return, result-rep, output-info);
end;

define method emit-return
    (return :: <return>, result-rep == #"doesn't-return",
     output-info :: <output-info>)
    => ();
  error("have a return region for lambda that doesn't return?");
end;

define method emit-return
    (return :: <return>, result-rep == #"void", output-info :: <output-info>)
    => ();
  let stream = output-info.output-info-guts-stream;
  write("return;\n", stream);
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
    format(stream, "return %s;\n", top-name);
  else
    for (dep = results then dep.dependent-next,
	 count from 0,
	 while: dep)
      format(stream, "orig_sp[%d] = %s;\n", count,
	     ref-leaf($general-rep, dep.source-exp, output-info));
    finally
      format(stream, "return orig_sp + %d;\n", count);
    end;
  end;
end;

define method emit-return
    (return :: <return>, result-rep :: <representation>,
     output-info :: <output-info>)
    => ();
  let stream = output-info.output-info-guts-stream;
  format(stream, "return %s;\n",
	 ref-leaf(result-rep, return.depends-on.source-exp, output-info));
end;

define method emit-return
    (return :: <return>, result-reps :: <list>, output-info :: <output-info>)
    => ();
  let stream = output-info.output-info-guts-stream;  
  let temp = new-local(output-info);
  let lambda = return.block-of;
  let lambda-info = get-info-for(lambda, output-info);
  let name = lambda-info.lambda-info-main-entry-name;
  format(output-info.output-info-vars-stream, "struct %s_results %s;\n",
	 name, temp);
  for (rep in result-reps,
       index from 0,
       dep = return.depends-on then dep.dependent-next)
    format(stream, "%s.R%d = %s;\n",
	   temp, index, ref-leaf(rep, dep.source-exp, output-info));
  end;
  format(stream, "return %s;\n", temp);
end;


define method emit-region (pitcher :: <pitcher>, output-info :: <output-info>)
    => ();
  spew-pending-defines(output-info);
  let stream = output-info.output-info-guts-stream;
  let target = region.block-of;
  for (region = region.parent then region.parent,
       until: region == #f | region == target)
    finally
    if (region)
      error("local pitcher left behind?");
    end;
    let id = target.block-id;
    unless (id)
      error("Pitcher to the component?");
    end;

    let (bottom-name, top-name)
      = consume-cluster(pitcher.depends-on.source-exp, output-info);
    format(stream, "pitch(%s, %s);\n", bottom-name, top-name);
  end;
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
      deliver-cluster(defines, #f, bottom-name, top-name, var.derived-type,
		      output-info);
    else
      let rep = if (instance?(defines.var-info, <values-cluster-info>))
		  $general-rep;
		else
		  variable-representation(defines, output-info)
		end;

      deliver-results(defines,
		      vector(pair(ref-leaf(rep, var, output-info), rep)),
		      #f, output-info);
    end;
  end;
end;

define method emit-assignment (defines :: false-or(<definition-site-variable>),
			       expr :: <global-variable>,
			       output-info :: <output-info>)
    => ();
  let (name, rep) = c-name-and-rep(expr, output-info);
  if (~expr.var-info.var-defn.ct-value)
    let stream = output-info.output-info-guts-stream;
    if (rep.representation-has-bottom-value?)
      let temp = new-local(output-info);
      format(output-info.output-info-vars-stream, "%s %s;\n",
	     rep.representation-c-type, temp);
      format(stream, "if ((%s = %s) == NULL) abort();\n", temp, name);
      name := temp;
    else
      format(stream, "if (!%s_initialized) abort();\n", name);
    end;
  end;
  deliver-results(defines, vector(pair(name, rep)), #f, output-info);
end;

define method emit-assignment (defines :: false-or(<definition-site-variable>),
			       expr :: <literal-constant>,
			       output-info :: <output-info>)
    => ();
  if (defines)
    let rep-hint = if (instance?(defines.var-info, <values-cluster-info>))
		     $general-rep;
		   else
		     variable-representation(defines, output-info)
		   end;
    let (expr, rep) = c-expr-and-rep(expr.value, rep-hint, output-info);
    deliver-results(defines, vector(pair(expr, rep)), #f, output-info);
  end;
end;

define method emit-assignment (defines :: false-or(<definition-site-variable>),
			       leaf :: <definition-constant-leaf>,
			       output-info :: <output-info>)
    => ();
  let info = get-info-for(leaf.const-defn, output-info);
  deliver-results(defines,
		  vector(pair(info.backend-var-info-name,
			      info.backend-var-info-rep)),
		  #f, output-info);
end;

define method emit-assignment
    (results :: false-or(<definition-site-variable>),
     call :: union(<unknown-call>, <error-call>),
     output-info :: <output-info>)
    => ();
  let setup-stream = make(<byte-string-output-stream>);
  let function = call.depends-on.source-exp;
  let func = ref-leaf($heap-rep, function, output-info);
  
  let (args, sp) = cluster-names(output-info.output-info-cur-stack-depth);
  for (arg-dep = call.depends-on.dependent-next then arg-dep.dependent-next,
       count from 0,
       while: arg-dep)
    format(setup-stream, "%s[%d] = %s;\n", args, count,
	   ref-leaf($general-rep, arg-dep.source-exp, output-info));
  finally
    spew-pending-defines(output-info);
    let stream = output-info.output-info-guts-stream;
    write(setup-stream.string-output-stream-string, stream);
    if (results)
      format(stream, "%s = CALL(%s, %s, %d);\n", sp, func, args, count);
      deliver-cluster(results, #f, args, sp, call.derived-type, output-info);
    else
      format(stream, "CALL(%s, %s, %d);\n", func, args, count);
    end;
  end;
end;

define method emit-assignment
    (results :: false-or(<definition-site-variable>), call :: <known-call>,
     output-info :: <output-info>)
    => ();
  emit-known-call(results, call.depends-on.source-exp,
		  call.depends-on.dependent-next, output-info);
end;

define method emit-known-call
    (results :: false-or(<definition-site-variable>),
     function :: <definition-constant-leaf>,
     arguments :: false-or(<dependency>), output-info :: <output-info>)
  emit-known-call(results, function.const-defn, arguments, output-info);
end;

define method emit-known-call
    (results :: false-or(<definition-site-variable>),
     function :: <abstract-method-definition>,
     arguments :: false-or(<dependency>), output-info :: <output-info>)
    => ();
  emit-known-call(results, function.method-defn-leaf, arguments, output-info);
end;

define method emit-known-call
    (results :: false-or(<definition-site-variable>),
     function :: <hairy-method-literal>,
     arguments :: false-or(<dependency>), output-info :: <output-info>)
    => ();
  emit-known-call(results, function.main-entry, arguments, output-info);
end;

define method emit-known-call
    (results :: false-or(<definition-site-variable>), function :: <lambda>,
     arguments :: false-or(<dependency>), output-info :: <output-info>)
    => ();
  let func-info = get-info-for(function, output-info);
  let stream = make(<byte-string-output-stream>);
  let name = func-info.lambda-info-main-entry-name;
  let (sp, new-sp) = cluster-names(output-info.output-info-cur-stack-depth);
  format(stream, "%s(%s", name, sp);
  for (arg-dep = arguments then arg-dep.dependent-next,
       rep in func-info.lambda-info-argument-representations)
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
  let result-rep = func-info.lambda-info-result-representation;
  if (results == #f | result-rep == #"void")
    format(output-info.output-info-guts-stream, "%s;\n", call);
    deliver-results(results, #[], #f, output-info);
  elseif (result-rep == #"doesn't-return")
    error("Trying to get some values back from a function that "
	    "doesn't return?");
  elseif (result-rep == #"cluster")
    format(output-info.output-info-guts-stream, "%s = %s;\n", new-sp, call);
    deliver-cluster(results, #f, sp, new-sp, function.result-type,
		    output-info);
  elseif (instance?(result-rep, <list>))
    let temp = new-local(output-info);
    format(output-info.output-info-vars-stream, "struct %s_results %s;\n",
	   name, temp);
    format(output-info.output-info-guts-stream, "%s = %s;\n", temp, call);
    let result-exprs = make(<stretchy-vector>);
    for (dep = function.depends-on then dep.dependent-next,
	 index from 0,
	 while: dep)
      let rep = variable-representation(dep.source-exp, output-info);
      add!(result-exprs,
	   pair(format-to-string("%s.R%d", temp, index),
		rep));
    end;
    deliver-results(results, result-exprs, #f, output-info);
  else
    deliver-results(results, vector(pair(call, result-rep)), #t, output-info);
  end;
end;

define method emit-assignment
    (results :: false-or(<definition-site-variable>), call :: <mv-call>, 
     output-info :: <output-info>)
    => ();
  let stream = make(<byte-string-output-stream>);
  let func = ref-leaf($heap-rep, call.depends-on.source-exp, output-info);
  spew-pending-defines(output-info);
  let cluster = call.depends-on.dependent-next.source-exp;
  let (bottom-name, top-name) = consume-cluster(cluster, output-info);
  if (results)
    format(output-info.output-info-guts-stream,
	   "%s = CALL(%s, %s, %s - %s);\n",
	   top-name, func, bottom-name, top-name, bottom-name);
    deliver-cluster(results, #f, bottom-name, top-name, call.derived-type,
		    output-info);
  else
    format(output-info.output-info-guts-stream,
	   "CALL(%s, %s, %s - %s);\n",
	   func, bottom-name, top-name, bottom-name);
  end;
end;

define method emit-assignment (defines :: false-or(<definition-site-variable>),
			       expr :: <primitive>,
			       output-info :: <output-info>)
    => ();
  emit-primitive(expr.name, defines, expr, output-info);
end;

define method emit-assignment (defines :: false-or(<definition-site-variable>),
			       expr :: <prologue>,
			       output-info :: <output-info>)
    => ();
  let lambda-info = get-info-for(expr.lambda, output-info);
  deliver-results(defines,
		  map(method (rep, index)
			pair(format-to-string("A%d", index),
			     rep);
		      end,
		      lambda-info.lambda-info-argument-representations,
		      make(<range>, from: 0)),
		  #f, output-info);
end;

define method emit-assignment (defines :: false-or(<definition-site-variable>),
			       expr :: <catcher>,
			       output-info :: <output-info>)
    => ();
  deliver-cluster(defines, #f, "caught_args", "caught_sp",
		  region.catcher.derived-type, output-info);
end;

define method emit-assignment
    (defines :: false-or(<definition-site-variable>),
     set :: <set>, output-info :: <output-info>)
    => ();
  let defn = set.variable;
  let info = get-info-for(defn, output-info);
  let target = info.backend-var-info-name;
  let rep = info.backend-var-info-rep;
  let source = extract-operands(set, output-info, rep);
  spew-pending-defines(output-info);
  emit-copy(target, rep, source, rep, output-info);
  unless (defn.ct-value)
    unless (rep.representation-has-bottom-value?)
      let stream = output-info.output-info-guts-stream;
      format(stream, "%s_initialized = TRUE;\n", target);
    end;
  end;
  deliver-results(defines, #[], #f, output-info);
end;

define method emit-assignment
    (results :: false-or(<definition-site-variable>),
     call :: <self-tail-call>, output-info :: <output-info>)
    => ();
  spew-pending-defines(output-info);
  let lambda = call.self-tail-call-of;
  for (param = lambda.prologue.dependents.dependent.defines
	 then param.definer-next,
       closure-var = lambda.environment.closure-vars
	 then closure-var.closure-next,
       while: closure-var & param)
  finally
    let stream = output-info.output-info-guts-stream;
    for (param = param then param.definer-next,
	 arg-dep = call.depends-on then arg-dep.dependent-next,
	 while: arg-dep & param)
      let (name, rep) = c-name-and-rep(param, output-info);
      format(stream, "%s = %s;\n",
	     name, ref-leaf(rep, arg-dep.source-exp, output-info));
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
     op :: <truly-the>, output-info :: <output-info>)
    => ();
  if (results)
    let rep = variable-representation(results, output-info);
    let source = extract-operands(op, output-info, rep);
    deliver-results(results, vector(pair(source, rep)), #f, output-info);
  end;
end;


define method deliver-cluster
    (defines :: false-or(<definition-site-variable>),
     last-gets-rest? :: <boolean>, src-start :: <string>, src-end :: <string>,
     type :: <values-ctype>, output-info :: <output-info>)
    => ();
  let stream = output-info.output-info-guts-stream;
  if (instance?(defines.var-info, <values-cluster-info>))
    let (dst-start, dst-end) = produce-cluster(defines, output-info);
    unless (src-start = dst-start)
      format(stream, "%s = %s;\n", dst-end, dst-start);
      format(stream, "while (%s < %s) {\n", src-start, src-end);
      format(stream, "    *%s++ = *%s++;\n", dst-end, src-start);
    end;
  else
    let count = for (var = defines then var.definer-next,
		     index from 0,
		     while: var)
		finally
		  if (last-gets-rest?)
		    index - 1;
		  else
		    index;
		  end;
		end;
    unless (count <= type.min-values)
      format(stream, "pad_cluster(%s, %s, %d);\n", src-start, src-end, count);
    end;
    for (var = defines then var.definer-next,
	 index from 0,
	 while: var)
      let source
	= if (index == count)
	    format-to-string("make_rest_arg(%s + %d, %s)",
			     src-start, index, src-end);
	  else
	    format-to-string("%s[%d]", src-start, index);
	  end;
      deliver-single-result(var, source, $general-rep, #t, output-info);
    end;
  end;
end;

define method deliver-results
    (defines :: false-or(<definition-site-variable>), values :: <sequence>,
     now-dammit? :: <boolean>, output-info :: <output-info>)
    => ();
  let stream = output-info.output-info-guts-stream;
  if (defines & instance?(defines.var-info, <values-cluster-info>))
    let (bottom-name, top-name) = produce-cluster(defines, output-info);
    format(stream, "%s = %s + %d;\n", top-name, bottom-name, values.size);
    for (val in values, index from 0)
      emit-copy(format-to-string("%s[%d]", bottom-name, index), $general-rep,
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
  deliver-single-result(var.definition-of, source, source-rep, now-dammit?,
			output-info);
end;


// Primitives.

define constant $primitive-emitters = make(<object-table>);

define method define-primitive (name :: <symbol>, emitter :: <function>)
    => ();
  $primitive-emitters[name] := emitter;
end;

define method emit-primitive (name :: <symbol>,
			      defines :: false-or(<definition-site-variable>),
			      operation :: <primitive>,
			      output-info :: <output-info>)
    => ();
  let emitter = element($primitive-emitters, name,
			default: default-primitive-emitter);
  emitter(defines, operation, output-info);
end;

define method default-primitive-emitter
    (defines :: false-or(<definition-site-variable>),
     operation :: <primitive>,
     output-info :: <output-info>)
    => ();
  let stream = output-info.output-info-guts-stream;
  let ops = make(<stretchy-vector>);
  for (op = operation.depends-on then op.dependent-next,
       while: op)
    add!(ops, ref-leaf($general-rep, op.source-exp, output-info));
  end;
  spew-pending-defines(output-info);
  format(stream, "PRIMITIVE %s(", operation.name);
  for (op in ops, first? = #t then #f)
    unless (first?)
      write(", ", stream);
    end;
    write(op, stream);
  end;
  write(");\n", stream);
  deliver-results(defines, #[], #f, output-info);
end;

define-primitive
  (#"canonicalize-results",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let cluster = operation.depends-on.source-exp;
     let (bottom-name, top-name) = consume-cluster(cluster, output-info);
     deliver-cluster(defines, #t, bottom-name, top-name, cluster.derived-type,
		     output-info);
   end);

define-primitive
  (#"values",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let results = make(<stretchy-vector>);
     for (dep = operation.depends-on then dep.dependent-next,
	  while: dep)
       let expr = ref-leaf($general-rep, dep.source-exp, output-info);
       add!(results, pair(expr, $general-rep));
     end;
     deliver-results(defines, results, #f, output-info);
   end);


define-primitive
  (#"fixnum-=",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *long-rep*, *long-rep*);
     deliver-results(defines,
		     vector(pair(format-to-string("(%s == %s)", x, y),
				 $boolean-rep)),
		     #f, output-info);
   end);

define-primitive
  (#"fixnum-<",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *long-rep*, *long-rep*);
     deliver-results(defines,
		     vector(pair(format-to-string("(%s < %s)", x, y),
				 $boolean-rep)),
		     #f, output-info);
   end);

define-primitive
  (#"fixnum-+",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *long-rep*, *long-rep*);
     deliver-results(defines,
		     vector(pair(format-to-string("(%s + %s)", x, y),
				 *long-rep*)),
		     #f, output-info);
   end);

define-primitive
  (#"fixnum-*",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *long-rep*, *long-rep*);
     deliver-results(defines,
		     vector(pair(format-to-string("(%s * %s)", x, y),
				 *long-rep*)),
		     #f, output-info);
   end);

define-primitive
  (#"fixnum--",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *long-rep*, *long-rep*);
     deliver-results(defines,
		     vector(pair(format-to-string("(%s - %s)", x, y),
				 *long-rep*)),
		     #f, output-info);
   end);

define-primitive
  (#"fixnum-negative",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let x = extract-operands(operation, output-info, *long-rep*);
     deliver-results(defines,
		     vector(pair(format-to-string("(- %s)", x), *long-rep*)),
		     #f, output-info);
   end);

define-primitive
  (#"fixnum-floor/",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     spew-pending-defines(output-info);
     let (x, y) = extract-operands(operation, output-info,
				   *long-rep*, *long-rep*);
     deliver-results(defines,
		     vector(pair(format-to-string("(%s / %s)", x, y),
				 *long-rep*),
			    pair(format-to-string("(%s %% %s)", x, y),
				 *long-rep*)),
		     #f, output-info);
   end);

define-primitive
  (#"fixnum-ceiling/",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     spew-pending-defines(output-info);
     let (x, y) = extract-operands(operation, output-info,
				   *long-rep*, *long-rep*);
     deliver-results(defines,
		     vector(pair(format-to-string("(%s / %s)", x, y),
				 *long-rep*),
			    pair(format-to-string("(%s %% %s)", x, y),
				 *long-rep*)),
		     #f, output-info);
   end);

define-primitive
  (#"fixnum-round/",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     spew-pending-defines(output-info);
     let (x, y) = extract-operands(operation, output-info,
				   *long-rep*, *long-rep*);
     deliver-results(defines,
		     vector(pair(format-to-string("(%s / %s)", x, y),
				 *long-rep*),
			    pair(format-to-string("(%s %% %s)", x, y),
				 *long-rep*)),
		     #f, output-info);
   end);

define-primitive
  (#"fixnum-truncate/",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     spew-pending-defines(output-info);
     let (x, y) = extract-operands(operation, output-info,
				   *long-rep*, *long-rep*);
     deliver-results(defines,
		     vector(pair(format-to-string("(%s / %s)", x, y),
				 *long-rep*),
			    pair(format-to-string("(%s %% %s)", x, y),
				 *long-rep*)),
		     #f, output-info);
   end);

define-primitive
  (#"fixnum-logior",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *long-rep*, *long-rep*);
     deliver-results(defines,
		     vector(pair(format-to-string("(%s | %s)", x, y),
				 *long-rep*)),
		     #f, output-info);
   end);

define-primitive
  (#"fixnum-logxor",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *long-rep*, *long-rep*);
     deliver-results(defines,
		     vector(pair(format-to-string("(%s ^ %s)", x, y),
				 *long-rep*)),
		     #f, output-info);
   end);

define-primitive
  (#"fixnum-logand",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *long-rep*, *long-rep*);
     deliver-results(defines,
		     vector(pair(format-to-string("(%s & %s)", x, y),
				 *long-rep*)),
		     #f, output-info);
   end);

define-primitive
  (#"fixnum-lognot",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let x = extract-operands(operation, output-info, *long-rep*);
     deliver-results(defines,
		     vector(pair(format-to-string("(~ %s)", x), *long-rep*)),
		     #f, output-info);
   end);

define-primitive
  (#"fixnum-ash",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *long-rep*, *long-rep*);
     deliver-results(defines,
		     vector(pair(format-to-string("fixnum_ash(%s, %s)", x, y),
				 *long-rep*)),
		     #f, output-info);
   end);


define method extract-operands
    (operation :: <operation>, output-info :: <output-info>,
     #rest representations)
    => (#rest str :: <string>);
  let results = make(<stretchy-vector>);
  block (return)
    for (op = operation.depends-on then op.dependent-next,
	 index from 0)
      if (index == representations.size)
	if (op)
	  error("Too many operands for %s", operation);
	else
	  return();
	end;
      else
	let rep = representations[index];
	if (rep == #"rest")
	  if (index + 2 == representations.size)
	    let rep = representations[index + 1];
	    for (op = op then op.dependent-next)
	      add!(results, ref-leaf(rep, op.source-exp, output-info));
	    end;
	    return();
	  end;
	elseif (op)
	  add!(results, ref-leaf(rep, op.source-exp, output-info));
	else
	  error("Not enough operands for %s", operation);
	end;
      end;
    end;
  end;
  apply(values, results);
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
			leaf :: <global-variable>,
			output-info :: <output-info>)
    => res :: <string>;
  let (name, rep) = c-name-and-rep(leaf, output-info);
  if (~leaf.var-info.var-defn.ct-value)
    let stream = output-info.output-info-guts-stream;
    if (rep.representation-has-bottom-value?)
      let temp = new-local(output-info);
      format(output-info.output-info-vars-stream, "%s %s;\n",
	     rep.representation-c-type, temp);
      format(stream, "if ((%s = %s) == NULL) abort();\n", temp, name);
      name := temp;
    else
      format(stream, "if (!%s_initialized) abort();\n", name);
    end;
  end;

  conversion-expr(target-rep, name, rep, output-info);
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

define method c-expr-and-rep (lit :: <ct-value>,
			      rep-hint :: <representation>,
			      output-info :: <output-info>)
    => (name :: <string>, rep :: <representation>);
  let constants = output-info.output-info-constants;
  values(element(constants, lit, default: #f)
	   | (element(constants, lit) := new-root(lit, output-info)),
	 $general-rep);
end;

define method c-expr-and-rep (lit :: <literal-true>,
			      rep-hint == $boolean-rep,
			      output-info :: <output-info>)
    => (name :: <string>, rep :: <representation>);
  values("TRUE", $boolean-rep);
end;

define method c-expr-and-rep (lit :: <literal-true>,
			      rep-hint :: <representation>,
			      output-info :: <output-info>)
    => (name :: <string>, rep :: <representation>);
  values("obj_True", $heap-rep);
end;

define method c-expr-and-rep (lit :: <literal-false>,
			      rep-hint == $boolean-rep,
			      output-info :: <output-info>)
    => (name :: <string>, rep :: <representation>);
  values("FALSE", $boolean-rep);
end;

define method c-expr-and-rep (lit :: <literal-false>,
			      rep-hint :: <representation>,
			      output-info :: <output-info>)
    => (name :: <string>, rep :: <representation>);
  values("obj_False", $heap-rep);
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
  values(format-to-string("%=", lit.literal-value),
	 pick-representation(dylan-value(#"<single-float>"), #"speed"));
end;

define method c-expr-and-rep (lit :: <literal-double-float>,
			      rep-hint :: <immediate-representation>,
			      output-info :: <output-info>)
    => (name :: <string>, rep :: <representation>);
  values(format-to-string("%=", lit.literal-value),
	 pick-representation(dylan-value(#"<double-float>"), #"speed"));
end;

define method c-expr-and-rep (lit :: <literal-extended-float>,
			      rep-hint :: <immediate-representation>,
			      output-info :: <output-info>)
    => (name :: <string>, rep :: <representation>);
  values(format-to-string("%=", lit.literal-value),
	 pick-representation(dylan-value(#"<extended-float>"), #"speed"));
end;

define method c-expr-and-rep (lit :: <literal-character>,
			      rep-hint :: <representation>,
			      output-info :: <output-info>)
    => (name :: <string>, rep :: <representation>);
  let code = as(<integer>, lit.literal-value);
  values(if (code == 0)
	   "'\\0'";
	 elseif (code == 8)
	   "'\\b'";
	 elseif (code == 9)
	   "'\\t'";
	 elseif (code == 10)
	   "'\\n'";
	 elseif (code == 13)
	   "'\\r'";
	 elseif (code < 32)
	   format-to-string("'\\%o'", code);
	 elseif (code <= 126)
	   format-to-string("'%c'", code);
	 else
	   format-to-string("%d", code);
	 end,
	 pick-representation(dylan-value(#"<character>"), #"speed"));
end;

define method c-expr-and-rep (lit :: <literal-empty-list>,
			      rep-hint :: <representation>,
			      output-info :: <output-info>)
    => (name :: <string>, rep :: <representation>);
  values("obj_Nil", $heap-rep);
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
  format(stream, "%s.heapptr = ### proxy;\n", target);
  format(stream, "%s.dataword.%s = %s;\n",
	 target, source-rep.representation-data-word-member, source);
end;

define method emit-copy
    (target :: <string>, target-rep :: <general-representation>,
     source :: <string>, source-rep :: <c-representation>,
     output-info :: <output-info>)
    => ();
  let stream = output-info.output-info-guts-stream;
  let heapptr = conversion-expr($heap-rep, source, source-rep, output-info);
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
    let temp = new-local(output-info);
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
