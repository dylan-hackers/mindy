module: cback
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/cback/primemit.dylan,v 1.13 1995/11/16 04:11:38 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.


define method default-primitive-emitter
    (results :: false-or(<definition-site-variable>),
     operation :: <primitive>, output-info :: <output-info>)
    => ();
  let stream = make(<byte-string-output-stream>);

  let use-deliver-result?
    = if (results)
	if (results.definer-next)
	  write('[', stream);
	  for (var = results then var.definer-next,
	       first? = #t then #f,
	       while: var)
	    unless (first?)
	      write(", ", stream);
	    end;
	    write(c-name-and-rep(var, output-info), stream);
	  end;
	  write("] = ", stream);
	  #f;
	elseif (instance?(results.var-info, <values-cluster-info>))
	  let (bottom-name, top-name) = produce-cluster(results, output-info);
	  format(stream, "%s = ", top-name);
	  #f;
	else
	  #t;
	end;
      else
	#f;
      end;

  let info = operation.info;
  format(stream, "### %%primitive %s (", info.primitive-name);
  block (return)
    for (dep = operation.depends-on then dep.dependent-next,
	 types = info.primitive-arg-types then types.tail,
	 first? = #t then #f,
	 while: dep)
      let type = types.head;
      if (type == #"rest")
	let rest-type = types.tail.head;
	let rep = pick-representation(rest-type, #"speed");
	for (dep = dep then dep.dependent-next,
	     first? = first? then #f,
	     while: dep)
	  unless (first?)
	    write(", ", stream);
	  end;
	  write(ref-leaf(rep, dep.source-exp, output-info), stream);
	end;
	return();
      else
	unless (first?)
	  write(", ", stream);
	end;
	if (type == #"cluster")
	  let (bottom-name, top-name)
	    = consume-cluster(dep.source-exp, output-info);
	  format(stream, "%s...%s", bottom-name, top-name);
	else
	  let rep = pick-representation(type, #"speed");
	  write(ref-leaf(rep, dep.source-exp, output-info), stream);
	end;
      end;
    end;
  end;
  write(')', stream);
  let expr = stream.string-output-stream-string;

  if (use-deliver-result?)
    let (name, rep) = c-name-and-rep(results, output-info);
    deliver-result(results, expr, rep, #f, output-info);
  else
    format(output-info.output-info-guts-stream, "%s;\n", expr);
  end;
end;


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



// Argument primitives.

define-primitive-emitter
  (#"extract-args",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let nargs = extract-operands(operation, output-info, *long-rep*);
     let expr = format-to-string("((void *)(orig_sp - %s))", nargs);
     deliver-result(results, expr, *ptr-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"extract-arg",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (args, index) = extract-operands(operation, output-info,
					  *ptr-rep*, *long-rep*);
     let expr = format-to-string("(((descriptor_t *)%s)[%s])", args, index);
     deliver-result(results, expr, *general-rep*, #t, output-info);
   end);

define-primitive-emitter
  (#"make-rest-arg",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (args, nfixed, nargs)
       = extract-operands(operation, output-info,
			  *ptr-rep*, *long-rep*, *long-rep*);
     let cur-top = cluster-names(output-info.output-info-cur-stack-depth);
     let mra-defn = dylan-defn(#"make-rest-arg");
     let mra-info = find-main-entry-info(mra-defn, output-info);
     let expr
       = format-to-string("%s(%s, (descriptor_t *)%s + %s, %s - %s)",
			  main-entry-name(mra-info, output-info), cur-top,
			  args, nfixed, nargs, nfixed);
     deliver-result(defines, expr, *heap-rep*, #t, output-info);
   end);

define-primitive-emitter
  (#"pop-args",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let stream = output-info.output-info-guts-stream;
     let args = extract-operands(operation, output-info, *ptr-rep*);
     spew-pending-defines(output-info);
     assert(zero?(output-info.output-info-cur-stack-depth));
     if (results)
       format(stream, "cluster_0_top = orig_sp;\n");
     end;
     format(stream, "orig_sp = %s;\n", args);
     deliver-cluster(results, "orig_sp", "cluster_0_top", 0, output-info);
   end);


// Value primitives.

define-primitive-emitter
  (#"canonicalize-results",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let stream = output-info.output-info-guts-stream;
     let cluster = operation.depends-on.source-exp;
     let nfixed-leaf = operation.depends-on.dependent-next.source-exp;
     let nfixed = if (instance?(nfixed-leaf, <literal-constant>))
		    nfixed-leaf.value.literal-value;
		  else
		    error("nfixed arg to %%%%primitive canonicalize-results "
			    "isn't constant?");
		  end;
     let (bottom-name, top-name) = consume-cluster(cluster, output-info);
     format(stream, "%s = pad_cluster(%s, %s, %d);\n",
	    top-name, bottom-name, top-name, nfixed);
     let results = make(<vector>, size: nfixed + 1);
     for (index from 0 below nfixed)
       results[index] := pair(format-to-string("%s[%d]", bottom-name, index),
			      *general-rep*);
     end;
     let mra-defn = dylan-defn(#"make-rest-arg");
     let mra-info = find-main-entry-info(mra-defn, output-info);
     results[nfixed]
       := pair(format-to-string("%s(%s, %s + %d, %s - %s - %d)",
				main-entry-name(mra-info, output-info),
				top-name,
				bottom-name, nfixed,
				top-name, bottom-name, nfixed),
	       *heap-rep*);
     deliver-results(defines, results, #t, output-info);
   end);

define-primitive-emitter
  (#"merge-clusters",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     local
       method repeat (dep :: false-or(<dependency>))
	   => (bottom-name, top-name, values-count);
	 if (dep)
	   let (next-bottom, next-top, values-count)
	     = repeat(dep.dependent-next);
	   let cluster = dep.source-exp;
	   let (my-bottom, my-top) = consume-cluster(cluster, output-info);
	   unless (next-bottom == #f | my-top = next-bottom)
	     error("Merging two clusters that arn't adjacent?");
	   end;
	   values(my-bottom, next-top | my-top,
		  values-count + cluster.derived-type.min-values);
	 else
	   values(#f, #f, 0);
	 end;
       end;
     let (bottom-name, top-name, values-count)
       = repeat(operation.depends-on);
     if (bottom-name)
       deliver-cluster(defines, bottom-name, top-name, values-count,
		       output-info);
     else
       deliver-results(defines, #[], #f, output-info);
     end;
   end);

define-primitive-emitter
  (#"values-sequence",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let vec = extract-operands(operation, output-info, *heap-rep*);
     let (cur-sp, new-sp)
       = cluster-names(output-info.output-info-cur-stack-depth);
     format(output-info.output-info-guts-stream,
	    "%s = values_sequence(%s, %s);\n", new-sp, cur-sp, vec);
     deliver-cluster(defines, cur-sp, new-sp, 0, output-info);
   end);

define-primitive-emitter
  (#"values",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let results = make(<stretchy-vector>);
     for (dep = operation.depends-on then dep.dependent-next,
	  while: dep)
       let expr = ref-leaf(*general-rep*, dep.source-exp, output-info);
       add!(results, pair(expr, *general-rep*));
     end;
     deliver-results(defines, results, #f, output-info);
   end);


// Allocation primitives.

define-primitive-emitter
  (#"allocate",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let bytes = extract-operands(operation, output-info, *long-rep*);
     deliver-result(defines, format-to-string("allocate(%s)", bytes),
		    *heap-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"make-data-word-instance",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let cclass = operation.derived-type;
     let target-rep = pick-representation(cclass, #"speed");
     let source-rep
       = pick-representation(cclass.all-slot-infos[1].slot-type, #"speed");
     unless (source-rep == target-rep
	       | (representation-data-word-member(source-rep)
		    = representation-data-word-member(target-rep)))
       error("The instance and slot representations don't match in a "
	       "data-word reference?");
     end;
     let source = extract-operands(operation, output-info, source-rep);
     deliver-result(defines, source, target-rep, #f, output-info);
   end);


// Foreign code interface primitives.

define-primitive-emitter
  (#"call-out",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let stream = make(<byte-string-output-stream>);

     let func-dep = operation.depends-on;
     let func = func-dep.source-exp;
     unless (instance?(func, <literal-constant>)
	       & instance?(func.value, <literal-string>))
       error("function in call-out isn't a constant string?");
     end;
     format(stream, "%s(", func.value.literal-value);

     let res-dep = func-dep.dependent-next;
     let result-rep = rep-for-c-type(res-dep.source-exp);

     local
       method repeat (dep :: false-or(<dependency>), first? :: <boolean>)
	 if (dep)
	   unless (first?)
	     write(", ", stream);
	   end;
	   let rep = rep-for-c-type(dep.source-exp);
	   let next = dep.dependent-next;
	   format(stream, "(%s)%s",
		  rep.representation-c-type,
		  ref-leaf(rep, next.source-exp, output-info));
	   repeat(next.dependent-next, #f);
	 end;
       end;
     repeat(res-dep.dependent-next, #t);

     write(')', stream);

     spew-pending-defines(output-info);
     if (result-rep)
       deliver-result(defines, string-output-stream-string(stream),
		      result-rep, #t, output-info);
     else
       format(output-info.output-info-guts-stream, "%s;\n",
	      string-output-stream-string(stream));
       deliver-results(defines, #[], #f, output-info);
     end;
   end);

define-primitive-emitter
  (#"c-decl",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let decl = operation.depends-on.source-exp;
     unless (instance?(decl, <literal-constant>)
	       & instance?(decl.value, <literal-string>))
       error("decl in c-decl isn't a constant string?");
     end;
     format(output-info.output-info-body-stream, "%s\n",
	    decl.value.literal-value);
     deliver-results(defines, #[], #f, output-info);
   end);

define-primitive-emitter
  (#"c-expr",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let stream = make(<byte-string-output-stream>);

     let res-dep = operation.depends-on;
     let result-rep = rep-for-c-type(res-dep.source-exp);

     let expr = res-dep.dependent-next.source-exp;
     unless (instance?(expr, <literal-constant>)
	       & instance?(expr.value, <literal-string>))
       error("expr in c-expr isn't a constant string?");
     end;

     spew-pending-defines(output-info);
     if (result-rep)
       deliver-result(defines, expr.value.literal-value,
		      result-rep, #t, output-info);
     else
       format(output-info.output-info-guts-stream, "%s;\n",
	      expr.value.literal-value);
       deliver-results(defines, #[], #f, output-info);
     end;
   end);


define method rep-for-c-type (leaf :: <leaf>)
    => rep :: false-or(<representation>);
  unless (instance?(leaf, <literal-constant>))
    error("Type spec in call-out isn't a constant?");
  end;
  let ct-value = leaf.value;
  unless (instance?(ct-value, <literal-symbol>))
    error("Type spec in call-out isn't a symbol?");
  end;
  let c-type = ct-value.literal-value;
  select (c-type)
    #"long" => *long-rep*;
    #"int" => *int-rep*;
    #"unsigned-int" => *uint-rep*;
    #"short" => *short-rep*;
    #"unsigned-short" => *ushort-rep*;
    #"char" => *byte-rep*;
    #"unsigned-char" => *ubyte-rep*;
    #"ptr" => *ptr-rep*;
    #"float" => *float-rep*;
    #"double" => *double-rep*;
    #"long-double" => *long-double-rep*;
    #"void" => #f;
  end;
end;

define-primitive-emitter
  (#"c-string",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let leaf = operation.depends-on.source-exp;
     unless (instance?(leaf, <literal-constant>))
       error("argument to c-string isn't a constant?");
     end;
     let lit = leaf.value;
     unless (instance?(lit, <literal-string>))
       error("argument to c-string isn't a string?");
     end;
     let stream = make(<byte-string-output-stream>);
     write('"', stream);
     for (char in lit.literal-value)
       let code = as(<integer>, char);
       if (char < ' ')
	 select (char)
	   '\b' => write("\\b", stream);
	   '\t' => write("\\t", stream);
	   '\n' => write("\\n", stream);
	   '\r' => write("\\r", stream);
	   otherwise =>
	     format(stream, "\\0%d%d",
		    ash(code, -3),
		    logand(code, 7));
	 end;
       elseif (char == '"' | char == '\\')
	 format(stream, "\\%c", char);
       elseif (code < 127)
	 write(char, stream);
       elseif (code < 256)
	 format(stream, "\\%d%d%d",
		ash(code, -6),
		logand(ash(code, -3), 7),
		logand(code, 7));
       else
	 error("%= can't be represented in a C string.");
       end;
     end;
     write('"', stream);
     deliver-result(defines, string-output-stream-string(stream), *ptr-rep*,
		    #f, output-info);
   end);


// Predicate primitives

define-primitive-emitter
  (#"as-boolean",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let expr = extract-operands(operation, output-info, *boolean-rep*);
     deliver-result(defines, expr, *boolean-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"not",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let arg = operation.depends-on.source-exp;
     let expr
       = if (csubtype?(arg.derived-type, specifier-type(#"<boolean>")))
	   format-to-string("!%s", ref-leaf(*boolean-rep*, arg, output-info));
	 else
	   format-to-string("(%s == obj_False)",
			    ref-leaf(*heap-rep*, arg, output-info));
	 end;
     deliver-result(defines, expr, *boolean-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"==",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *heap-rep*, *heap-rep*);
     deliver-result(defines, format-to-string("(%s == %s)", x, y),
		    *boolean-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"initialized?",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let expr = format-to-string("(%s != NULL)",
				 extract-operands(operation, output-info,
						  *heap-rep*));
     deliver-result(defines, expr, *boolean-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"initial-symbols",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     extract-operands(operation, output-info);
     deliver-result(defines, "initial_symbols", *heap-rep*, #f, output-info);
   end);
   


// NLX primitives.

define-primitive-emitter
  (#"current-sp",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let sp = cluster-names(output-info.output-info-cur-stack-depth);
     deliver-result(defines, sp, *ptr-rep*, #t, output-info);
   end);

define-primitive-emitter
  (#"unwind-stack",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let sp = cluster-names(output-info.output-info-cur-stack-depth);
     format(output-info.output-info-guts-stream, "%s = %s;\n",
	    sp, extract-operands(operation, output-info, *ptr-rep*));
     deliver-results(defines, #[], #f, output-info);
   end);

define-primitive-emitter
  (#"throw",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let state-expr
       = ref-leaf(*ptr-rep*, operation.depends-on.source-exp, output-info);
     let cluster = operation.depends-on.dependent-next.source-exp;
     let (bottom-name, top-name) = consume-cluster(cluster, output-info);
     spew-pending-defines(output-info);
     format(output-info.output-info-guts-stream,
	    "throw(%s, %s);\n", state-expr, top-name);
   end);


// Fixnum primitives.

define-primitive-emitter
  (#"fixnum-=",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *long-rep*, *long-rep*);
     deliver-result(defines, format-to-string("(%s == %s)", x, y),
		    *boolean-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"fixnum-<",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *long-rep*, *long-rep*);
     deliver-result(defines, format-to-string("(%s < %s)", x, y), *boolean-rep*,
		    #f, output-info);
   end);

define-primitive-emitter
  (#"fixnum-+",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *long-rep*, *long-rep*);
     deliver-result(defines, format-to-string("(%s + %s)", x, y), *long-rep*,
		    #f, output-info);
   end);

define-primitive-emitter
  (#"fixnum-*",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *long-rep*, *long-rep*);
     deliver-result(defines, format-to-string("(%s * %s)", x, y), *long-rep*,
		    #f, output-info);
   end);

define-primitive-emitter
  (#"fixnum--",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *long-rep*, *long-rep*);
     deliver-result(defines, format-to-string("(%s - %s)", x, y), *long-rep*,
		    #f, output-info);
   end);

define-primitive-emitter
  (#"fixnum-negative",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let x = extract-operands(operation, output-info, *long-rep*);
     deliver-result(defines, format-to-string("(- %s)", x), *long-rep*,
		    #f, output-info);
   end);

define-primitive-emitter
  (#"fixnum-divide",
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

define-primitive-emitter
  (#"fixnum-logior",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *long-rep*, *long-rep*);
     deliver-result(defines, format-to-string("(%s | %s)", x, y), *long-rep*,
		    #f, output-info);
   end);

define-primitive-emitter
  (#"fixnum-logxor",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *long-rep*, *long-rep*);
     deliver-result(defines, format-to-string("(%s ^ %s)", x, y), *long-rep*,
		    #f, output-info);
   end);

define-primitive-emitter
  (#"fixnum-logand",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *long-rep*, *long-rep*);
     deliver-result(defines, format-to-string("(%s & %s)", x, y), *long-rep*,
		    #f, output-info);
   end);

define-primitive-emitter
  (#"fixnum-lognot",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let x = extract-operands(operation, output-info, *long-rep*);
     deliver-result(defines, format-to-string("(~ %s)", x), *long-rep*,
		    #f, output-info);
   end);

define-primitive-emitter
  (#"fixnum-shift-left",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *long-rep*, *long-rep*);
     deliver-result(defines, format-to-string("(%s << %s)", x, y),
		    *long-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"fixnum-shift-right",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *long-rep*, *long-rep*);
     deliver-result(defines, format-to-string("(%s >> %s)", x, y),
		    *long-rep*, #f, output-info);
   end);


// Single float primitives.

define-primitive-emitter
  (#"fixed-as-single",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let x = extract-operands(operation, output-info, *long-rep*);
     deliver-result(results, format-to-string("((float)%s)", x),
		    *float-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"double-as-single",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let x = extract-operands(operation, output-info, *double-rep*);
     deliver-result(results, format-to-string("((float)%s)", x),
		    *float-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"extended-as-single",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let x = extract-operands(operation, output-info, *long-double-rep*);
     deliver-result(results, format-to-string("((float)%s)", x),
		    *float-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"single-<",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *float-rep*, *float-rep*);
     deliver-result(results, format-to-string("(%s < %s)", x, y), *boolean-rep*,
		    #f, output-info);
   end);

define-primitive-emitter
  (#"single-<=",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *float-rep*, *float-rep*);
     deliver-result(results,
		    format-to-string("(%s <= %s)", x, y), *boolean-rep*,
		    #f, output-info);
   end);

define-primitive-emitter
  (#"single-=",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *float-rep*, *float-rep*);
     deliver-result(results, format-to-string("(%s == %s)", x, y),
		    *boolean-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"single-==",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *float-rep*, *float-rep*);
     // ### This isn't right -- should really be doing a bitwise comparison.
     deliver-result(results, format-to-string("(%s == %s)", x, y),
		    *boolean-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"single-~=",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *float-rep*, *float-rep*);
     deliver-result(results,
		    format-to-string("(%s != %s)", x, y), *boolean-rep*,
		    #f, output-info);
   end);

define-primitive-emitter
  (#"single-+",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *float-rep*, *float-rep*);
     deliver-result(results, format-to-string("(%s + %s)", x, y), *float-rep*,
		    #f, output-info);
   end);

define-primitive-emitter
  (#"single-*",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *float-rep*, *float-rep*);
     deliver-result(results, format-to-string("(%s * %s)", x, y), *float-rep*,
		    #f, output-info);
   end);

define-primitive-emitter
  (#"single--",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *float-rep*, *float-rep*);
     deliver-result(results, format-to-string("(%s - %s)", x, y), *float-rep*,
		    #f, output-info);
   end);

define-primitive-emitter
  (#"single-/",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *float-rep*, *float-rep*);
     deliver-result(results, format-to-string("(%s / %s)", x, y), *float-rep*,
		    #f, output-info);
   end);

define-primitive-emitter
  (#"single-abs",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let x = extract-operands(operation, output-info, *float-rep*);
     deliver-result(results, format-to-string("fabsf(%s)", x), *float-rep*,
		    #f, output-info);
   end);

define-primitive-emitter
  (#"single-negative",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let x = extract-operands(operation, output-info, *float-rep*);
     deliver-result(results, format-to-string("(-%s)", x), *float-rep*,
		    #f, output-info);
   end);

define-primitive-emitter
  (#"single-floor",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let x = extract-operands(operation, output-info, *float-rep*);
     deliver-result(results, format-to-string("((long)floor(%s))", x),
		    *long-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"single-ceiling",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let x = extract-operands(operation, output-info, *float-rep*);
     deliver-result(results, format-to-string("((long)ceil(%s))", x),
		    *long-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"single-round",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let x = extract-operands(operation, output-info, *float-rep*);
     deliver-result(results, format-to-string("((long)rint(%s))", x),
		    *long-rep*, #f, output-info);
   end);


// Double float primitives.

define-primitive-emitter
  (#"fixed-as-double",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let x = extract-operands(operation, output-info, *long-rep*);
     deliver-result(results, format-to-string("((double)%s)", x),
		    *double-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"single-as-double",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let x = extract-operands(operation, output-info, *float-rep*);
     deliver-result(results, format-to-string("((double)%s)", x),
		    *double-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"extended-as-double",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let x = extract-operands(operation, output-info, *long-double-rep*);
     deliver-result(results, format-to-string("((double)%s)", x),
		    *double-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"double-<",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *double-rep*, *double-rep*);
     deliver-result(results, format-to-string("(%s < %s)", x, y), *boolean-rep*,
		    #f, output-info);
   end);

define-primitive-emitter
  (#"double-<=",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *double-rep*, *double-rep*);
     deliver-result(results,
		    format-to-string("(%s <= %s)", x, y), *boolean-rep*,
		    #f, output-info);
   end);

define-primitive-emitter
  (#"double-=",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *double-rep*, *double-rep*);
     deliver-result(results, format-to-string("(%s == %s)", x, y),
		    *boolean-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"double-==",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *double-rep*, *double-rep*);
     // ### This isn't right -- should really be doing a bitwise comparison.
     deliver-result(results, format-to-string("(%s == %s)", x, y),
		    *boolean-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"double-~=",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *double-rep*, *double-rep*);
     deliver-result(results,
		    format-to-string("(%s != %s)", x, y), *boolean-rep*,
		    #f, output-info);
   end);

define-primitive-emitter
  (#"double-+",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *double-rep*, *double-rep*);
     deliver-result(results, format-to-string("(%s + %s)", x, y), *double-rep*,
		    #f, output-info);
   end);

define-primitive-emitter
  (#"double-*",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *double-rep*, *double-rep*);
     deliver-result(results, format-to-string("(%s * %s)", x, y), *double-rep*,
		    #f, output-info);
   end);

define-primitive-emitter
  (#"double--",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *double-rep*, *double-rep*);
     deliver-result(results, format-to-string("(%s - %s)", x, y), *double-rep*,
		    #f, output-info);
   end);

define-primitive-emitter
  (#"double-/",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *double-rep*, *double-rep*);
     deliver-result(results, format-to-string("(%s / %s)", x, y), *double-rep*,
		    #f, output-info);
   end);

define-primitive-emitter
  (#"double-abs",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let x = extract-operands(operation, output-info, *double-rep*);
     deliver-result(results, format-to-string("fabsf(%s)", x), *double-rep*,
		    #f, output-info);
   end);

define-primitive-emitter
  (#"double-negative",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let x = extract-operands(operation, output-info, *double-rep*);
     deliver-result(results, format-to-string("(-%s)", x), *double-rep*,
		    #f, output-info);
   end);

define-primitive-emitter
  (#"double-floor",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let x = extract-operands(operation, output-info, *double-rep*);
     deliver-result(results, format-to-string("((long)floor(%s))", x),
		    *long-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"double-ceiling",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let x = extract-operands(operation, output-info, *double-rep*);
     deliver-result(results, format-to-string("((long)ceil(%s))", x),
		    *long-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"double-round",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let x = extract-operands(operation, output-info, *double-rep*);
     deliver-result(results, format-to-string("((long)rint(%s))", x),
		    *long-rep*, #f, output-info);
   end);


// Extended float primitives.

define-primitive-emitter
  (#"fixed-as-extended",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let x = extract-operands(operation, output-info, *long-rep*);
     deliver-result(results, format-to-string("((long double)%s)", x),
		    *long-double-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"single-as-extended",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let x = extract-operands(operation, output-info, *float-rep*);
     deliver-result(results, format-to-string("((long double)%s)", x),
		    *long-double-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"double-as-extended",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let x = extract-operands(operation, output-info, *double-rep*);
     deliver-result(results, format-to-string("((long double)%s)", x),
		    *long-double-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"extended-<",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *long-double-rep*, *long-double-rep*);
     deliver-result(results, format-to-string("(%s < %s)", x, y), *boolean-rep*,
		    #f, output-info);
   end);

define-primitive-emitter
  (#"extended-<=",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *long-double-rep*, *long-double-rep*);
     deliver-result(results,
		    format-to-string("(%s <= %s)", x, y), *boolean-rep*,
		    #f, output-info);
   end);

define-primitive-emitter
  (#"extended-=",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *long-double-rep*, *long-double-rep*);
     deliver-result(results, format-to-string("(%s == %s)", x, y),
		    *boolean-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"extended-==",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *long-double-rep*, *long-double-rep*);
     // ### This isn't right -- should really be doing a bitwise comparison.
     deliver-result(results, format-to-string("(%s == %s)", x, y),
		    *boolean-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"extended-~=",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *long-double-rep*, *long-double-rep*);
     deliver-result(results,
		    format-to-string("(%s != %s)", x, y), *boolean-rep*,
		    #f, output-info);
   end);

define-primitive-emitter
  (#"extended-+",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *long-double-rep*, *long-double-rep*);
     deliver-result(results, format-to-string("(%s + %s)", x, y),
		    *long-double-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"extended-*",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *long-double-rep*, *long-double-rep*);
     deliver-result(results, format-to-string("(%s * %s)", x, y),
		    *long-double-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"extended--",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *long-double-rep*, *long-double-rep*);
     deliver-result(results, format-to-string("(%s - %s)", x, y),
		    *long-double-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"extended-/",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *long-double-rep*, *long-double-rep*);
     deliver-result(results, format-to-string("(%s / %s)", x, y),
		    *long-double-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"extended-abs",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let x = extract-operands(operation, output-info, *long-double-rep*);
     deliver-result(results, format-to-string("fabsf(%s)", x),
		    *long-double-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"extended-negative",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let x = extract-operands(operation, output-info, *long-double-rep*);
     deliver-result(results, format-to-string("(-%s)", x), *long-double-rep*,
		    #f, output-info);
   end);

define-primitive-emitter
  (#"extended-floor",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let x = extract-operands(operation, output-info, *long-double-rep*);
     deliver-result(results, format-to-string("((long)floor(%s))", x),
		    *long-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"extended-ceiling",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let x = extract-operands(operation, output-info, *long-double-rep*);
     deliver-result(results, format-to-string("((long)ceil(%s))", x),
		    *long-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"extended-round",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let x = extract-operands(operation, output-info, *long-double-rep*);
     deliver-result(results, format-to-string("((long)rint(%s))", x),
		    *long-rep*, #f, output-info);
   end);


// raw pointer operations.

define-primitive-emitter
  (#"make-raw-pointer",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let x = extract-operands(operation, output-info, *long-rep*);
     deliver-result(results, format-to-string("((void *)%s)", x),
		    *ptr-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"raw-pointer-address",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let x = extract-operands(operation, output-info, *ptr-rep*);
     deliver-result(results, format-to-string("((long)%s)", x),
		    *long-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"pointer-+",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *ptr-rep*, *long-rep*);
     deliver-result(results,
		    format-to-string("((void *)((char *)%s + %s))", x, y),
		    *ptr-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"pointer--",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *ptr-rep*, *ptr-rep*);
     deliver-result(results,
		    format-to-string("((char *)%s - (char *)%s)", x, y),
		    *long-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"pointer-<",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *ptr-rep*, *ptr-rep*);
     deliver-result(results,
		    format-to-string("((char *)%s < (char *)%s)", x, y),
		    *boolean-rep*, #f, output-info);
   end);

define-primitive-emitter
  (#"pointer-=",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   output-info :: <output-info>)
       => ();
     let (x, y) = extract-operands(operation, output-info,
				   *ptr-rep*, *ptr-rep*);
     deliver-result(results, format-to-string("(%s == %s)", x, y),
		    *boolean-rep*, #f, output-info);
   end);
