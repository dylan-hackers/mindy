module: cback
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/cback/cback.dylan,v 1.10 1995/04/25 23:00:25 wlott Exp $
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


// variable stuff.

define class <backend-var-info> (<object>)
  slot backend-var-info-rep :: <representation>,
    required-init-keyword: representation:;
  slot backend-var-info-name :: <string>,
    required-init-keyword: name:;
end;

define method make-info-for (var :: union(<initial-variable>, <ssa-variable>),
			     // ### Should really only be ssa-variable.
			     output-info :: <output-info>)
    => res :: <backend-var-info>;
  let rep = pick-representation(var.derived-type, #"speed");
  let varinfo = var.var-info;
  let name = new-local(output-info);
  let stream = output-info.output-info-vars-stream;
  format(stream, "%s %s;", rep.representation-c-type, name);
  if (instance?(varinfo, <debug-named-info>))
    format(stream, " /* %s */", varinfo.debug-name);
  end;
  write('\n', stream);

  make(<backend-var-info>, representation: rep, name: name);
end;

define method make-info-for (defn :: <bindings-definition>,
			     output-info :: <output-info>)
    => res :: <backend-var-info>;
  let type = defn.defn-type;
  let rep = if (type)
	      pick-representation(type, #"speed");
	    else
	      $general-rep;
	    end;
  let name
    = if (instance?(rep, <immediate-representation>))
	new-global(output-info);
      else
	new-root(defn.ct-value, output-info);
      end;
  make(<backend-var-info>, representation: rep, name: name);
end;


define generic c-name-and-rep (thing :: <abstract-variable>,
			       output-info :: <output-info>)
    => (name :: <string>, rep :: <representation>);

define method c-name-and-rep (leaf :: <abstract-variable>,
			      // ### Should really be ssa-variable
			      output-info :: <output-info>)
    => (name :: <string>, rep :: <representation>);
  let info = get-info-for(leaf, output-info);
  values(info.backend-var-info-name, info.backend-var-info-rep);
end;

define method c-name-and-rep (leaf :: <initial-definition>,
			      output-info :: <output-info>)
    => (name :: <string>, rep :: <representation>);
  c-name-and-rep(leaf.definition-of, output-info);
end;

define method c-name-and-rep (leaf :: <global-variable>,
			      output-info :: <output-info>)
    => (name :: <string>, rep :: <representation>);
  let info = get-info-for(leaf.var-info.var-defn, output-info);
  values(info.backend-var-info-name, info.backend-var-info-rep);
end;



// Lambda stuff.

define class <lambda-info> (<object>)
  slot lambda-info-lambda :: <lambda>,
    required-init-keyword: lambda:;
  slot lambda-info-main-entry-name :: <byte-string>,
    required-init-keyword: main-entry-name:;
  slot lambda-info-prototype :: <byte-string>,
    required-init-keyword: prototype:;
end;

define method make-info-for (lambda :: <lambda>, output-info :: <output-info>)
    => res :: <lambda-info>;
  // Compute the prototype.

  let defines = lambda.prologue.dependents.dependent.defines;
  for (var = defines then var.definer-next,
       index from 0,
       while: var)
    if (var.info)
      error("lambda arg already has a backend-info?");
    end;
    let rep = pick-representation(var.derived-type, #"speed");
    let name = format-to-string("A%d", index);
    var.info := make(<backend-var-info>, representation: rep, name: name);
  end;

  let stream = make(<byte-string-output-stream>);
  let name = new-global(output-info);
  let result = lambda.depends-on;
  if (~result)
    write("void", stream);
  elseif (~result.dependent-next)
    let var = result.source-exp;
    if (instance?(var.var-info, <values-cluster-info>))
      write("VALUES-CLUSTER", stream);
    else
      let (name, rep) = c-name-and-rep(var, output-info);
      write(rep.representation-c-type, stream);
    end;
  else
    let header = output-info.output-info-header-stream;
    format(header, "struct %s_results {\n", name);
    indent(header, $indentation-step);
    for (dep = result then dep.dependent-next,
	 index from 0,
	 while: dep)
      let var = dep.source-exp;
      let (name, rep) = c-name-and-rep(var, output-info);
      format(header, "%s R%d;\n", rep.representation-c-type, index);
    end;
    indent(header, -$indentation-step);
    format(header, "};\n");
    format(stream, "struct %s_results", name);
  end;

  format(stream, " %s(", name);
  if (defines)
    for (var = defines then var.definer-next,
	 index from 0,
	 while: var)
      unless (zero?(index))
	write(", ", stream);
      end;
      let (name, rep) = c-name-and-rep(var, output-info);
      format(stream, "%s %s", rep.representation-c-type, name);
      let varinfo = var.var-info;
      if (instance?(varinfo, <debug-named-info>))
	format(stream, " /* %s */", varinfo.debug-name);
      end;
    end;
  else
    write("void", stream);
  end;
  write(')', stream);

  make(<lambda-info>,
       lambda: lambda,
       main-entry-name: name,
       prototype: stream.string-output-stream-string);
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
  local
    method process-defn (defn :: <bindings-definition>) => ();
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
      if (instance?(defn, <variable-definition>))
	let type-defn = defn.var-defn-type-defn;
	type-defn & process-defn(type-defn);
      end;
    end;
  do(process-defn, tlf.tlf-required-defns);
  tlf.tlf-rest-defn & process-defn(tlf.tlf-rest-defn);
end;


// Control flow emitters

define method emit-lambda (lambda :: <lambda>, output-info :: <output-info>)
    => ();
  let info = get-info-for(lambda, output-info);
  let prototype = info.lambda-info-prototype;
  format(output-info.output-info-header-stream, "static %s;\n", prototype);

  let stream = output-info.output-info-body-stream;
  format(stream, "static %s\n{\n", prototype);

  emit-region(lambda.body, output-info);
  let result = lambda.depends-on;
  if (result)
    let stream = output-info.output-info-guts-stream;
    if (~result.dependent-next)
      let var = result.source-exp;
      if (instance?(var.var-info, <values-cluster-info>))
	write("return VALUES-CLUSTER;\n", stream);
      else
	let (name, rep) = c-name-and-rep(var, output-info);
	format(stream, "return %s;\n", name);
      end;
    else
      let temp = new-local(output-info);
      format(output-info.output-info-vars-stream, "struct %s_results %s;\n",
	     info.lambda-info-main-entry-name, temp);
      for (dep = result then dep.dependent-next,
	   index from 0,
	   while: dep)
	let var = dep.source-exp;
	let (name, rep) = c-name-and-rep(var, output-info);
	emit-copy(format-to-string("%s.R%d", temp, index), rep, name, rep,
		  output-info);
      end;
      format(stream, "return %s;\n", temp);
    end;
  end;

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
  format(stream, "if (%s) {\n", cond);
  indent(stream, $indentation-step);
  emit-region(region.then-region, output-info);
  /* ### emit-joins(region.join-region, output-info); */
  indent(stream, -$indentation-step);
  write("}\n", stream);
  write("else {\n", stream);
  indent(stream, $indentation-step);
  emit-region(region.else-region, output-info);
  /* ### emit-joins(region.join-region, output-info); */
  indent(stream, -$indentation-step);
  write("}\n", stream);
end;

define method emit-region (region :: <loop-region>,
			   output-info :: <output-info>)
    => ();
  /* ### emit-joins(region.join-region, output-info); */
  let stream = output-info.output-info-guts-stream;
  write("while (1) {\n", stream);
  indent(stream, $indentation-step);
  emit-region(region.body, output-info);
  /* ### emit-joins(region.join-region, output-info); */
  indent(stream, -$indentation-step);
  write("}\n", stream);
end;

define method emit-region (region :: <block-region>,
			   output-info :: <output-info>)
    => ();
  let stream = output-info.output-info-guts-stream;
  emit-region(region.body, output-info);
  /* ### emit-joins(region.join-region, output-info); */
  if (region.exits)
    let half-step = ash($indentation-step, -1);
    indent(stream, - half-step);
    format(stream, "block%d:\n", region.block-id);
    indent(stream, half-step);
  end;
end;

define method emit-region (region :: <exit>, output-info :: <output-info>)
    => ();
  /* ### emit-joins(region.join-region, output-info); */
  let stream = output-info.output-info-guts-stream;
  let id = region.block-of.block-id;
  if (id)
    format(stream, "goto block%d;\n", id);
  else
    format(stream, "abort();\n");
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
			       expr :: <expression>,
			       output-info :: <output-info>)
    => ();
  let stream = output-info.output-info-guts-stream;
  write("ASSIGNMENT;\n", stream);
end;

define method emit-assignment (defines :: false-or(<definition-site-variable>),
			       var :: <abstract-variable>,
			       output-info :: <output-info>)
    => ();
  if (instance?(var.var-info, <values-cluster-info>))
    let stream = output-info.output-info-guts-stream;
    if (defines & instance?(defines.var-info, <values-cluster-info>))
      write("VALUES-CLUSTER = VALUES-CLUSTER;\n", stream);
    else
      let count
	= for (var = defines then var.definer-next,
	       index from 0,
	       while: var)
	  finally
	    index;
	  end;
      unless (count <= var.derived-type.min-values)
	format("pad(VALUES-CLUSTER, %d);\n", count);
      end;
      for (var = defines then var.definer-next,
	   index from 0,
	   while: var)
	let source = format-to-string("VALUES-CLUSTER[%d]", index);
	deliver-single-result(var, source, $general-rep, output-info);
      end;
    end;
  else
    let (name, rep) = c-name-and-rep(var, output-info);
    deliver-results(defines, vector(pair(name, rep)), output-info);
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
  deliver-results(defines, vector(pair(name, rep)), output-info);
end;

define method emit-assignment (defines :: false-or(<definition-site-variable>),
			       expr :: <literal-constant>,
			       output-info :: <output-info>)
    => ();
  if (defines)
    let varinfo = defines.var-info;
    let rep-hint = if (instance?(varinfo, <values-cluster-info>))
		     $general-rep;
		   else
		     let (name, rep) = c-name-and-rep(defines, output-info);
		     rep;
		   end;
    let (expr, rep) = c-expr-and-rep(expr.value, rep-hint, output-info);
    deliver-results(defines, vector(pair(expr, rep)), output-info);
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
end;

define method emit-assignment (defines :: false-or(<definition-site-variable>),
			       expr :: <catcher>,
			       output-info :: <output-info>)
    => ();
end;

define method emit-assignment
    (pitcher-defines :: false-or(<definition-site-variable>),
     pitcher :: <pitcher>, output-info :: <output-info>)
    => ();
  let args = pitcher.depends-on;
  let catcher-defines = pitcher.catcher.dependents.dependent.defines;

  if (args & instance?(args.source-exp, <abstract-variable>)
	& instance?(args.source-exp.var-info, <values-cluster-info>))
    emit-assignment(catcher-defines, args.source-exp, output-info);
  else
    let results = make(<stretchy-vector>);
    for (dep = pitcher.depends-on then dep.dependent-next,
	 while: dep)
      let expr = ref-leaf($general-rep, dep.source-exp, output-info);
      add!(results, pair(expr, $general-rep));
    end;
    deliver-results(catcher-defines, results, output-info);
  end;
end;

define method emit-assignment
    (pitcher-defines :: false-or(<definition-site-variable>),
     set :: <set>, output-info :: <output-info>)
    => ();
  let defn = set.variable;
  let info = get-info-for(defn, output-info);
  let target = info.backend-var-info-name;
  let rep = info.backend-var-info-rep;
  let source = extract-operands(set, output-info, rep);
  emit-copy(target, rep, source, rep, output-info);
  unless (defn.ct-value)
    unless (rep.representation-has-bottom-value?)
      let stream = output-info.output-info-guts-stream;
      format(stream, "%s_initialized = TRUE;\n", target);
    end;
  end;
end;



define method deliver-results (defines :: false-or(<definition-site-variable>),
			       values :: <vector>,
			       output-info :: <output-info>)
  let stream = output-info.output-info-guts-stream;
  if (defines & instance?(defines.var-info, <values-cluster-info>))
    // ### Make a values cluster.
    write("VALUES-CLUSTER = [", stream);
    for (val in values, first? = #t then #f)
      unless (first?)
	write(", ", stream);
      end;
      write(val.head, stream);
    end;
    write("];\n", stream);
  else
    for (var = defines then var.definer-next,
	 val in values,
	 while: var)
      deliver-single-result(var, val.head, val.tail, output-info);
    finally
      if (var)
	let false = make(<literal-false>);
	for (var = var then var.definer-next,
	     while: var)
	  let (target-name, target-rep) = c-name-and-rep(var, output-info);
	  let (source-name, source-rep)
	    = c-expr-and-rep(false, target-rep, output-info);
	  deliver-single-result(var, source-name, source-rep, output-info);
	end;
      end;
    end;
  end;
end;

define method deliver-single-result (var :: <abstract-variable>,
				     // ### Should really be ssa-variable
				     source :: <string>,
				     source-rep :: <representation>,
				     output-info :: <output-info>)
    => ();
  let (target-name, target-rep) = c-name-and-rep(var, output-info);
  emit-copy(target-name, target-rep, source, source-rep, output-info);
end;

define method deliver-single-result (var :: <initial-definition>,
				     source :: <string>,
				     source-rep :: <representation>,
				     output-info :: <output-info>)
    => ();
  deliver-single-result(var.definition-of, source, source-rep, output-info);
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
  format(stream, "PRIMITIVE %s(", operation.name);
  for (op in ops, first? = #t then #f)
    unless (first?)
      write(", ", stream);
    end;
    write(op, stream);
  end;
  write(");\n", stream);
  deliver-results(defines, #[], output-info);
end;

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
     deliver-results(defines, results, output-info);
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
		     vector(pair(format-to-string("%s == %s", x, y),
				 $boolean-rep)),
		     output-info);
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
		     vector(pair(format-to-string("%s < %s", x, y),
				 $boolean-rep)),
		     output-info);
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
		     vector(pair(format-to-string("%s + %s", x, y),
				 *long-rep*)),
		     output-info);
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
		     vector(pair(format-to-string("%s * %s", x, y),
				 *long-rep*)),
		     output-info);
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
		     vector(pair(format-to-string("%s - %s", x, y),
				 *long-rep*)),
		     output-info);
   end);

define-primitive
  (#"fixnum-negative",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let x = extract-operands(operation, output-info, *long-rep*);
     deliver-results(defines,
		     vector(pair(format-to-string("- %s", x), *long-rep*)),
		     output-info);
   end);

define-primitive
  (#"fixnum-floor/",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *long-rep*, *long-rep*);
     deliver-results(defines,
		     vector(pair(format-to-string("%s / %s", x, y),
				 *long-rep*),
			    pair(format-to-string("%s %% %s", x, y),
				 *long-rep*)),
		     output-info);
   end);

define-primitive
  (#"fixnum-ceiling/",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *long-rep*, *long-rep*);
     deliver-results(defines,
		     vector(pair(format-to-string("%s / %s", x, y),
				 *long-rep*),
			    pair(format-to-string("%s %% %s", x, y),
				 *long-rep*)),
		     output-info);
   end);

define-primitive
  (#"fixnum-round/",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *long-rep*, *long-rep*);
     deliver-results(defines,
		     vector(pair(format-to-string("%s / %s", x, y),
				 *long-rep*),
			    pair(format-to-string("%s %% %s", x, y),
				 *long-rep*)),
		     output-info);
   end);

define-primitive
  (#"fixnum-truncate/",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *long-rep*, *long-rep*);
     deliver-results(defines,
		     vector(pair(format-to-string("%s / %s", x, y),
				 *long-rep*),
			    pair(format-to-string("%s %% %s", x, y),
				 *long-rep*)),
		     output-info);
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
		     vector(pair(format-to-string("%s | %s", x, y),
				 *long-rep*)),
		     output-info);
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
		     vector(pair(format-to-string("%s ^ %s", x, y),
				 *long-rep*)),
		     output-info);
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
		     vector(pair(format-to-string("%s & %s", x, y),
				 *long-rep*)),
		     output-info);
   end);

define-primitive
  (#"fixnum-lognot",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let x = extract-operands(operation, output-info, *long-rep*);
     deliver-results(defines,
		     vector(pair(format-to-string("~ %s", x), *long-rep*)),
		     output-info);
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
		     output-info);
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


define method ref-leaf (target-rep :: <representation>,
			leaf :: <abstract-variable>,
			output-info :: <output-info>)
    => res :: <string>;
  let (name, rep) = c-name-and-rep(leaf, output-info);
  conversion-expr(target-rep, name, rep, output-info);
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
  let temp = new-local(output-info);
  format(output-info.output-info-vars-stream, "%s %s;\n",
	 target-rep.representation-c-type, temp);
  emit-copy(temp, target-rep, source, source-rep, output-info);
  temp;
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
