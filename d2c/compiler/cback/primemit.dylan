module: cback
rcs-header: $Header: /scm/cvs/src/d2c/compiler/cback/primemit.dylan,v 1.18 2003/05/25 15:39:16 housel Exp $
copyright: see below


//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000, 2001  Gwydion Dylan Maintainers
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

define method default-primitive-emitter
    (results :: false-or(<definition-site-variable>),
     operation :: <primitive>, file :: <file-state>)
    => ();
  let stream = make(<buffered-byte-string-output-stream>);

  let use-deliver-result?
    = if (results)
	if (results.definer-next)
	  write-element(stream, '[');
	  for (var = results then var.definer-next,
	       first? = #t then #f,
	       while: var)
	    unless (first?)
	      write(stream, ", ");
	    end;
	    write(stream, c-name-and-rep(var, file));
	  end;
	  write(stream, "] = ");
	  #f;
	elseif (instance?(results.var-info, <values-cluster-info>))
	  let (bottom-name, top-name) = produce-cluster(results, file);
	  format(stream, "%s = ", top-name);
	  #f;
	else
	  #t;
	end;
      else
	#f;
      end;

  let info = operation.primitive-info;
  format(stream, "### %%primitive %s (", info.priminfo-name);
  block (return)
    for (dep = operation.depends-on then dep.dependent-next,
	 types = info.priminfo-arg-types then types.tail,
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
	    write(stream, ", ");
	  end;
          let (leaf, temp?) = ref-leaf(rep, dep.source-exp, file);
          contact-bgh-if(temp?);
	  write(stream, leaf);
	end;
	return();
      else
	unless (first?)
	  write(stream, ", ");
	end;
	if (type == #"cluster")
	  let (bottom-name, top-name)
	    = consume-cluster(dep.source-exp, file);
	  format(stream, "%s...%s", bottom-name, top-name);
	else
	  let rep = pick-representation(type, #"speed");
          let (leaf, temp?) = ref-leaf(rep, dep.source-exp, file);
          contact-bgh-if(temp?);
	  write(stream, leaf);
	end;
      end;
    end;
  end;
  write-element(stream, ')');
  let expr = stream.stream-contents;

  if (use-deliver-result?)
    let (name, rep) = c-name-and-rep(results, file);
    deliver-result(results, expr, rep, #f, file);
  else
    format(file.file-guts-stream, "%s;\n", expr);
  end;
end;


define method extract-operands
    (operation :: <operation>, file :: <file-state>,
     #rest representations)
    => (temps :: <temp-locals-list>, #rest str :: <string>);
  let results = make(<stretchy-vector>);
  let temps = make-temp-locals-list();
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
              let (leaf, temp?) = ref-leaf(rep, op.source-exp, file);
              if (temp?) temps := add!(temps, leaf) end;
	      add!(results, leaf);
	    end;
	    return();
	  end;
	elseif (op)
          let (leaf, temp?) = ref-leaf(rep, op.source-exp, file);
          if (temp?) temps := add!(temps, leaf) end;
	  add!(results, leaf);
	else
	  error("Not enough operands for %s", operation);
	end;
      end;
    end;
  end;
  apply(values, temps, results);
end;



// Argument primitives.

define-primitive-emitter
  (#"main-entry",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     error("argument to %%%%primitive main-entry isn't a constant method?");
   end method);

define-primitive-emitter
  (#"extract-args",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, nargs) = extract-operands(operation, file, *long-rep*);
     contact-bgh-unless-empty(temps);
     let expr = stringify("((void *)(orig_sp - ", nargs, "))");
     deliver-result(results, expr, *ptr-rep*, #f, file);
   end);

define-primitive-emitter
  (#"extract-arg",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, args, index) = extract-operands(operation, file,
					  *ptr-rep*, *long-rep*);
     contact-bgh-unless-empty(temps);
     let expr = stringify("(((descriptor_t *)", args, ")[", index, "])");
     deliver-result(results, expr, *general-rep*, #t, file);
   end);

define-primitive-emitter
  (#"make-rest-arg",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, args, nfixed, nargs)
       = extract-operands(operation, file,
			  *ptr-rep*, *long-rep*, *long-rep*);
     contact-bgh-unless-empty(temps);
     let cur-top = cluster-names(operation.info);
     let mra-defn = dylan-defn(#"make-rest-arg");
     let mra-info = find-main-entry-info(mra-defn, file);
     let expr
       = stringify(main-entry-c-name(mra-info, file), '(', cur-top,
		   ", (descriptor_t *)", args, " + ", nfixed, ", ",
		   nargs, " - ", nfixed, ')');
     deliver-result(defines, expr, *heap-rep*, #t, file);
   end);

define-primitive-emitter
  (#"pop-args",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let stream = file.file-guts-stream;
     let (temps, args) = extract-operands(operation, file, *ptr-rep*);
     contact-bgh-unless-empty(temps);
     spew-pending-defines(file);
     assert(zero?(operation.info));
     if (results)
       format(stream, "cluster_0_top = orig_sp;\n");
     end;
     format(stream, "orig_sp = %s;\n", args);
     deliver-cluster(results, "orig_sp", "cluster_0_top", 0, file);
   end);


// Value primitives.

define-primitive-emitter
  (#"canonicalize-results",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let stream = file.file-guts-stream;
     let cluster = operation.depends-on.source-exp;
     let nfixed-leaf = operation.depends-on.dependent-next.source-exp;
     let nfixed = if (instance?(nfixed-leaf, <literal-constant>))
		    as(<integer>, nfixed-leaf.value.literal-value);
		  else
		    error("nfixed arg to %%%%primitive canonicalize-results "
			    "isn't constant?");
		  end;
     let (bottom-name, top-name) = consume-cluster(cluster, file);
     unless (nfixed == 0)
       format(stream, "%s = pad_cluster(%s, %s, %d);\n",
	      top-name, bottom-name, top-name, nfixed);
     end unless;
     let results = make(<vector>, size: nfixed + 1);
     for (index from 0 below nfixed)
       results[index] := pair(stringify(bottom-name, '[', index, ']'),
			      *general-rep*);
     end;
     let mra-defn = dylan-defn(#"make-rest-arg");
     let mra-info = find-main-entry-info(mra-defn, file);
     results[nfixed]
       := pair(stringify(main-entry-c-name(mra-info, file), '(',
       			 top-name, ", ",
			 bottom-name, " + ", nfixed, ", ",
			 top-name, " - ", bottom-name, " - ", nfixed, ')'),
	       *heap-rep*);
     deliver-results(defines, results, #t, file);
   end);

define-primitive-emitter
  (#"merge-clusters",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     local
       method repeat (dep :: false-or(<dependency>))
	   => (bottom-name, top-name, values-count);
	 if (dep)
	   let (next-bottom, next-top, values-count)
	     = repeat(dep.dependent-next);
	   let cluster = dep.source-exp;
	   let (my-bottom, my-top) = consume-cluster(cluster, file);
	   unless (next-bottom == #f | my-top = next-bottom)
	     error("Merging two clusters that aren't adjacent?");
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
		       file);
     else
       deliver-results(defines, #[], #f, file);
     end;
   end);

define-primitive-emitter
  (#"values-sequence",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, vec) = extract-operands(operation, file, *heap-rep*);
     contact-bgh-unless-empty(temps);
     let (cur-sp, new-sp) = cluster-names(operation.info);
     format(file.file-guts-stream,
	    "%s = values_sequence(%s, %s);\n", new-sp, cur-sp, vec);
     deliver-cluster(defines, cur-sp, new-sp, 0, file);
   end);

define-primitive-emitter
  (#"values",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let results = make(<stretchy-vector>);
     let temps = make-temp-locals-list();
     for (dep = operation.depends-on then dep.dependent-next,
	  while: dep)
       let (expr, temp?) = ref-leaf(*general-rep*, dep.source-exp, file);
       if (temp?) temps := add!(temps, expr) end;
       add!(results, pair(expr, *general-rep*));
     end;
     deliver-results(defines, results, #f, file);
     free-temps(temps, file);
   end);


// Allocation primitives.

define-primitive-emitter
  (#"allocate",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let class = operation.depends-on.source-exp.value;
     assert(class.data-word-slot == #f);

     let bytes-leaf = operation.depends-on.dependent-next.source-exp;
     let (bytes, temp?) = ref-leaf(*long-rep*, bytes-leaf, file);
     contact-bgh-if(temp?);

     deliver-result(defines, stringify("allocate(", bytes, ')'),
		    *heap-rep*, #f, file);
   end);

define-primitive-emitter
  (#"allocate-with-data-word",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let class = operation.depends-on.source-exp.value;
     let slot-rep = class.data-word-slot.slot-representation;
     let data-word-member = slot-rep.representation-data-word-member;

     let bytes-leaf = operation.depends-on.dependent-next.source-exp;
     let (bytes, bytes-temp?) = ref-leaf(*long-rep*, bytes-leaf, file);
     contact-bgh-if(bytes-temp?);

     let data-word-leaf
       = operation.depends-on.dependent-next.dependent-next.source-exp;
     let data-word-rep
       = pick-representation(data-word-leaf.derived-type, #"speed");
     let (data-word, data-word-temp?)
       = ref-leaf(data-word-rep, data-word-leaf, file);
     contact-bgh-if(data-word-temp?);

     assert(data-word-member = data-word-rep.representation-data-word-member);

     if (defines)
       if (instance?(defines.var-info, <values-cluster-info>))
	 let name = new-local(file);
	 write(file.file-vars-stream,
	       stringify("descriptor_t ", name, ";"));
	 new-line(file.file-vars-stream);
	 write(file.file-guts-stream,
	       stringify(name, ".heapptr = allocate(", bytes, ");"));
	 new-line(file.file-vars-stream);
	 write(file.file-guts-stream,
	       stringify(name, ".dataword.", data-word-member,
			 " = ", data-word, ";"));
	 new-line(file.file-guts-stream);
	 deliver-result(defines, name, *general-rep*, #f, file);
       else
	 let (name, rep) = c-name-and-rep(defines, file);
	 assert(rep == *general-rep*);

	 write(file.file-guts-stream,
	       stringify(name, ".heapptr = allocate(", bytes, ");"));
	 new-line(file.file-guts-stream);
	 write(file.file-guts-stream,
	       stringify(name, ".dataword.", data-word-member,
			 " = ", data-word, ";"));
	 new-line(file.file-guts-stream);
	 
	 deliver-results(defines.definer-next, #[], #f, file);
       end if;
     end if;
   end);

define-primitive-emitter
  (#"make-immediate",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let class :: <cclass> = operation.depends-on.source-exp.value;

     let immediate-slot = #f;
     for (slot in class.all-slot-infos)
       if (instance?(slot, <instance-slot-info>)
	     & slot.slot-introduced-by ~== object-ctype())
	 if (immediate-slot)
	   error("Trying to make an immediate with multiple instance slots?");
	 end;
	 immediate-slot := slot;
       end if;
     end for;

     if (immediate-slot)
       let source-rep = immediate-slot.slot-representation;
       let source-leaf = operation.depends-on.dependent-next.source-exp;
       let source = ref-leaf(source-rep, source-leaf, file);

       let target-rep
	 = pick-representation
	     (make(<direct-instance-ctype>, base-class: class), #"speed");
       unless (source-rep == target-rep
		 | (representation-data-word-member(source-rep)
		      = representation-data-word-member(target-rep)))
	 error("The instance and slot representations don't match in "
		 "make-immediate of %s",
	       class);
       end;

       deliver-result(defines, source, target-rep, #f, file);
     elseif (class == specifier-type(#"<true>"))
       deliver-result(defines, "1", *boolean-rep*, #f, file);
     elseif (class == specifier-type(#"<false>"))
       deliver-result(defines, "0", *boolean-rep*, #f, file);
     else
       error("Don't know how to make the immediate representation of %s",
	     class);
     end if;
   end method);


// Foreign code interface primitives.

define-primitive-emitter
  (#"call-out",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let stream = make(<buffered-byte-string-output-stream>);
     let temps = make-temp-locals-list();

     let func-dep = operation.depends-on;
     let func = func-dep.source-exp;

     let res-dep = func-dep.dependent-next;
     let result-rep = rep-for-c-type(res-dep.source-exp);

     if (instance?(func, <literal-constant>)
	   & instance?(func.value, <literal-string>))
       format(stream, "%s(", func.value.literal-value);
     elseif(csubtype?(func.derived-type, specifier-type(#"<raw-pointer>")))
       let (leaf, temp?) = ref-leaf(*ptr-rep*, func, file);
       if (temp?) temps := add!(temps, leaf) end;
       format(stream, "((%s (*)())%s)(",
	      if (result-rep) result-rep.representation-c-type else "void" end,
	      leaf);
     else
       error("First argument to call-out must be a literal string "
	       "or an instance of <raw-pointer>");
     end;

     local
       method repeat (dep :: false-or(<dependency>), first? :: <boolean>)
	 if (dep)
	   unless (first?)
	     write(stream, ", ");
	   end;
	   let rep = rep-for-c-type(dep.source-exp);
	   let next = dep.dependent-next;
           let (leaf, temp?) = ref-leaf(rep, next.source-exp, file);
           if (temp?) temps := add!(temps, leaf) end;
           format(stream, "%s", leaf);
	   repeat(next.dependent-next, #f);
	 end;
       end;
     repeat(res-dep.dependent-next, #t);

     write-element(stream, ')');

     spew-pending-defines(file);
     if (result-rep & defines)
       deliver-result(defines, stream-contents(stream),
		      result-rep, #t, file);
     else
       format(file.file-guts-stream, "%s;\n",
	      stream-contents(stream));
       deliver-results(defines, #[], #f, file);
     end;
     free-temps(temps, file);
   end);

define function c-include-emitter
    (defines :: false-or(<definition-site-variable>),
    operation :: <primitive>,
    file :: <file-state>,
    left, right)
  => ();
  let include = operation.depends-on.source-exp;
  unless (instance?(include, <literal-constant>)
	  & instance?(include.value, <literal-string>))
    error("file name in c-include isn't a constant string?");
  end;
  maybe-emit-include(include.value.literal-value, file, left: left, right: right);
  deliver-results(defines, #[], #f, file);
end;

define-primitive-emitter
  (#"c-system-include", rcurry(c-include-emitter, '<', '>'));

define-primitive-emitter
  (#"c-include", rcurry(c-include-emitter, '"', '"'));

define-primitive-emitter
  (#"c-decl",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let decl = operation.depends-on.source-exp;
     unless (instance?(decl, <literal-constant>)
	       & instance?(decl.value, <literal-string>))
       error("decl in c-decl isn't a constant string?");
     end;
     format(file.file-body-stream, "%s\n",
	    decl.value.literal-value);
     deliver-results(defines, #[], #f, file);
   end);

define-primitive-emitter
  (#"c-local-decl",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let decl = operation.depends-on.source-exp;
     unless (instance?(decl, <literal-constant>)
	       & instance?(decl.value, <literal-string>))
       error("decl in c-local-decl isn't a constant string?");
     end;
     format(file.file-vars-stream, "%s\n",
	    decl.value.literal-value);
     deliver-results(defines, #[], #f, file);
   end);

define-primitive-emitter
  (#"c-expr",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let res-dep = operation.depends-on;
     let result-rep = rep-for-c-type(res-dep.source-exp);

     let expr = res-dep.dependent-next.source-exp;
     unless (instance?(expr, <literal-constant>)
	       & instance?(expr.value, <literal-string>))
       error("expr in c-expr isn't a constant string?");
     end;

     spew-pending-defines(file);
     if (result-rep)
       deliver-result(defines, expr.value.literal-value,
		      result-rep, #t, file);
     else
       format(file.file-guts-stream, "%s;\n",
	      expr.value.literal-value);
       deliver-results(defines, #[], #f, file);
     end;
   end);


define-primitive-emitter
  (#"c-literal",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let res-dep = operation.depends-on;
     let result-rep = rep-for-c-type(res-dep.source-exp);

     let expr = res-dep.dependent-next.source-exp;
     unless (instance?(expr, <literal-constant>)
	       & instance?(expr.value, <literal-string>))
       error("expr in c-literal isn't a constant string?");
     end;

     spew-pending-defines(file);
     
/*     define method compute-c-literal-value(lit :: <byte-string>)
		=> val :: <integer>;
	      lit.empty?
		& compiler-fatal-error("expr in c-literal is an empty string?", expr);

	      reduce(0,
		     method(sofar :: <integer>, this) => comb :: <integer>;
		       ash(sofar, 8) + as(<integer>, this)
		     end,
		     lit)
	    end method;*/
     
//     if (result-rep)	// not yet###
//       deliver-result(defines, expr.value.compute-c-literal-value, //expr.value.literal-value,
//		      result-rep, #t, file);
//     else
       format(file.file-guts-stream, "'%s';\n",
	      expr.value.literal-value);
       deliver-results(defines, #[], #f, file);
//     end;
   end);

define-primitive-emitter
  (#"c-struct-field",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let res-dep = operation.depends-on;
     let result-rep = rep-for-c-type(res-dep.source-exp);

     let ptr-dep = res-dep.dependent-next;
     let (ptr, temp?) = ref-leaf(*ptr-rep*, ptr-dep.source-exp, file);
     contact-bgh-if(temp?);

     let struct-dep = ptr-dep.dependent-next;
     let struct = struct-dep.source-exp;
     unless (instance?(struct, <literal-constant>)
	       & instance?(struct.value, <literal-string>))
       error("struct in c-struct-field isn't a constant string?");
     end;

     let field = struct-dep.dependent-next.source-exp;
     unless (instance?(field, <literal-constant>)
	       & instance?(field.value, <literal-string>))
       error("struct in c-struct-field isn't a constant string?");
     end;
     
     spew-pending-defines(file);
     deliver-result(defines,
		    stringify("((", struct.value.literal-value, " *) ",
			      ptr, ")->", field.value.literal-value),
		    result-rep, #f, file);
   end);

define-primitive-emitter
  (#"c-struct-field-setter",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let new-dep = operation.depends-on;
     let type-dep = new-dep.dependent-next;
     let rep = rep-for-c-type(type-dep.source-exp);
     let (new, new-temp?) = ref-leaf(rep, new-dep.source-exp, file);
     contact-bgh-if(new-temp?);

     let ptr-dep = type-dep.dependent-next;
     let (ptr, ptr-temp?) = ref-leaf(*ptr-rep*, ptr-dep.source-exp, file);
     contact-bgh-if(ptr-temp?);

     let struct-dep = ptr-dep.dependent-next;
     let struct = struct-dep.source-exp;
     unless (instance?(struct, <literal-constant>)
	       & instance?(struct.value, <literal-string>))
       error("struct in c-struct-field-setter isn't a constant string?");
     end;

     let field = struct-dep.dependent-next.source-exp;
     unless (instance?(field, <literal-constant>)
	       & instance?(field.value, <literal-string>))
       error("struct in c-struct-field-setter isn't a constant string?");
     end;
     
     spew-pending-defines(file);
     format(file.file-guts-stream, "((%s *) %s)->%s = %s;",
	    struct.value.literal-value, ptr, field.value.literal-value, new);
     
     deliver-results(defines, #[], #f, file);
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
  c-rep(c-type);
end;

define-primitive-emitter
  (#"c-string",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let leaf = operation.depends-on.source-exp;
     unless (instance?(leaf, <literal-constant>))
       error("argument to c-string isn't a constant?");
     end;
     let lit = leaf.value;
     unless (instance?(lit, <literal-string>))
       error("argument to c-string isn't a string?");
     end;
     let stream = make(<buffered-byte-string-output-stream>);
     write-element(stream, '"');
     for (char in lit.literal-value)
       let code = as(<integer>, char);
       if (char < ' ')
	 select (char)
	   '\b' => write(stream, "\\b");
	   '\t' => write(stream, "\\t");
	   '\n' => write(stream, "\\n");
	   '\r' => write(stream, "\\r");
	   otherwise =>
	     format(stream, "\\0%d%d",
		    ash(code, -3),
		    logand(code, 7));
	 end;
       elseif (char == '"' | char == '\\')
	 format(stream, "\\%c", char);
       elseif (code < 127)
	 write-element(stream, char);
       elseif (code < 256)
	 format(stream, "\\%d%d%d",
		ash(code, -6),
		logand(ash(code, -3), 7),
		logand(code, 7));
       else
	 error("%= can't be represented in a C string.");
       end;
     end;
     write-element(stream, '"');
     deliver-result(defines, stream-contents(stream), *ptr-rep*,
		    #f, file);
   end);


// Predicate primitives

define-primitive-emitter
  (#"as-boolean",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, expr) = extract-operands(operation, file, *boolean-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(defines, expr, *boolean-rep*, #f, file);
   end);

define-primitive-emitter
  (#"not",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let arg = operation.depends-on.source-exp;
     let expr
       = if (csubtype?(arg.derived-type, specifier-type(#"<boolean>")))
           let (leaf, temp?) = ref-leaf(*boolean-rep*, arg, file);
           contact-bgh-if(temp?);
	   stringify('!', leaf);
	 else
           let (leaf, temp?) = ref-leaf(*heap-rep*, arg, file);
           contact-bgh-if(temp?);
	   stringify('(', leaf, " == obj_False)");
	 end;
     deliver-result(defines, expr, *boolean-rep*, #f, file);
   end);

define-primitive-emitter
  (#"==",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
				   *heap-rep*, *heap-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(defines, stringify('(', x, " == ", y, ')'),
		    *boolean-rep*, #f, file);
   end);

define-primitive-emitter
  (#"initialized?",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, inited) = extract-operands(operation, file, *heap-rep*);
     contact-bgh-unless-empty(temps);
     let expr = stringify('(', inited,
			  " != NULL)");
     deliver-result(defines, expr, *boolean-rep*, #f, file);
   end);

define-primitive-emitter
  (#"initial-symbols",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let temps = extract-operands(operation, file);
     contact-bgh-unless-empty(temps);
     deliver-result(defines, "initial_symbols", *heap-rep*, #f, file);
   end);
   


// Slot access primitives.

define-primitive-emitter
  (#"ref-slot",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let instance-dep = operation.depends-on;
     let (instance, instance-temp?) = ref-leaf(*heap-rep*, instance-dep.source-exp, file);
     contact-bgh-if(instance-temp?);
     let repsym-dep = instance-dep.dependent-next;
     let repsym-leaf = repsym-dep.source-exp;
     unless (instance?(repsym-leaf, <literal-constant>)
	       & instance?(repsym-leaf.value, <literal-symbol>))
       error("Representation spec in ref-slot isn't a literal symbol.");
     end unless;
     let repsym = repsym-leaf.value.literal-value;
     let rep = select (repsym)
		 #"general" => *general-rep*;
		 #"heap" => *heap-rep*;
		 #"boolean" => *boolean-rep*;
	       end select;
     let offset-dep = repsym-dep.dependent-next;
     let (offset, offset-temp?) = ref-leaf(*long-rep*, offset-dep.source-exp, file);
     contact-bgh-if(offset-temp?);

     spew-pending-defines(file);
     
     deliver-result(defines,
		    stringify("SLOT(", instance, ", ",
			      rep.representation-c-type, ", ",
			      offset, ')'),
		    rep, #f, file);
   end method);

define-primitive-emitter
  (#"set-slot",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let newval-dep = operation.depends-on;
     let instance-dep = newval-dep.dependent-next;
     let repsym-dep = instance-dep.dependent-next;
     let offset-dep = repsym-dep.dependent-next;
     let repsym-leaf = repsym-dep.source-exp;
     unless (instance?(repsym-leaf, <literal-constant>)
	       & instance?(repsym-leaf.value, <literal-symbol>))
       error("Representation spec in ref-slot isn't a literal symbol.");
     end unless;
     let repsym = repsym-leaf.value.literal-value;
     let rep = select (repsym)
		 #"general" => *general-rep*;
		 #"heap" => *heap-rep*;
		 #"boolean" => *boolean-rep*;
	       end select;
     let (newval, newval-temp?) = ref-leaf(rep, newval-dep.source-exp, file);
     let (instance, instance-temp?) = ref-leaf(*heap-rep*, instance-dep.source-exp, file);
     let (offset, offset-temp?) = ref-leaf(*long-rep*, offset-dep.source-exp, file);
     contact-bgh-if(newval-temp? | instance-temp? | offset-temp?);

     format(file.file-guts-stream,
	    "SLOT(%s, %s, %s) = %s;\n",
	    instance, rep.representation-c-type, offset, newval);

     deliver-results(defines, #[], #f, file);
   end method);




// NLX primitives.

define-primitive-emitter
  (#"current-sp",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let sp = cluster-names(operation.info);
     deliver-result(defines, sp, *ptr-rep*, #t, file);
   end);

define-primitive-emitter
  (#"unwind-stack",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let sp = cluster-names(operation.info);
     let (temps, unwind-amount) =
       extract-operands(operation, file, *ptr-rep*);
     contact-bgh-unless-empty(temps);
     format(file.file-guts-stream, "%s = %s;\n",
	    sp, unwind-amount);
     deliver-results(defines, #[], #f, file);
   end);

define-primitive-emitter
  (#"throw",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (state-expr, temp?)
       = ref-leaf(*ptr-rep*, operation.depends-on.source-exp, file);
     contact-bgh-if(temp?);
     let cluster = operation.depends-on.dependent-next.source-exp;
     let (bottom-name, top-name) = consume-cluster(cluster, file);
     spew-pending-defines(file);
     format(file.file-guts-stream,
	    "throw(%s, %s);\n", state-expr, top-name);
   end);


// Fixnum primitives.

define-primitive-emitter
  (#"fixnum-=",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
				   *long-rep*, *long-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(defines, stringify('(', x, " == ", y, ')'),
		    *boolean-rep*, #f, file);
   end);

define-primitive-emitter
  (#"fixnum-<",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
				   *long-rep*, *long-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(defines, stringify('(', x, " < ", y, ')'), *boolean-rep*,
		    #f, file);
   end);

define-primitive-emitter
  (#"fixnum-+",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
				   *long-rep*, *long-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(defines, stringify('(', x, " + ", y, ')'), *long-rep*,
		    #f, file);
   end);

define-primitive-emitter
  (#"fixnum-*",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
				   *long-rep*, *long-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(defines, stringify('(', x, " * ", y, ')'), *long-rep*,
		    #f, file);
   end);

define-primitive-emitter
  (#"fixnum--",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
				   *long-rep*, *long-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(defines, stringify('(', x, " - ", y, ')'), *long-rep*,
		    #f, file);
   end);

define-primitive-emitter
  (#"fixnum-negative",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x) = extract-operands(operation, file, *long-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(defines, stringify("(- ", x, ')'), *long-rep*,
		    #f, file);
   end);

define-primitive-emitter
  (#"fixnum-divide",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     spew-pending-defines(file);
     let (temps, x, y) = extract-operands(operation, file,
                                          *long-rep*, *long-rep*);
     contact-bgh-unless-empty(temps);
     deliver-results(defines,
		     vector(pair(stringify('(', x, " / ", y, ')'), *long-rep*),
			    pair(stringify('(', x, " % ", y, ')'),
				 *long-rep*)),
		     #f, file);
   end);

define-primitive-emitter
  (#"fixnum-logior",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
				   *long-rep*, *long-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(defines, stringify('(', x, " | ", y, ')'), *long-rep*,
		    #f, file);
   end);

define-primitive-emitter
  (#"fixnum-logxor",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
				   *long-rep*, *long-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(defines, stringify('(', x, " ^ ", y, ')'), *long-rep*,
		    #f, file);
   end);

define-primitive-emitter
  (#"fixnum-logand",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
				   *long-rep*, *long-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(defines, stringify('(', x, " & ", y, ')'), *long-rep*,
		    #f, file);
   end);

define-primitive-emitter
  (#"fixnum-lognot",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x) = extract-operands(operation, file, *long-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(defines, stringify("(~ ", x, ')'), *long-rep*,
		    #f, file);
   end);

define-primitive-emitter
  (#"fixnum-shift-left",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
				   *long-rep*, *long-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(defines, stringify('(', x, " << ", y, ')'),
		    *long-rep*, #f, file);
   end);

define-primitive-emitter
  (#"fixnum-shift-right",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
				   *long-rep*, *long-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(defines, stringify('(', x, " >> ", y, ')'),
		    *long-rep*, #f, file);
   end);

define-primitive-emitter
  (#"fixnum-logical-shift-right",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
                                          *long-rep*, *long-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(defines, stringify("((unsigned long)", x, " >> ", y, ')'),
                    *long-rep*, #f, file);
   end);


// Double-width Fixnum primitives.

define-primitive-emitter
  (#"dblfix-=",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y)
       = extract-operands(operation, file, *long-long-rep*, *long-long-rep*);
     contact-bgh-unless-empty(temps);
     if (*current-target*.long-long-size)
       deliver-result(defines, stringify('(', x, " == ", y, ')'),
                      *boolean-rep*, #f, file);
     else
       deliver-result(defines,
                      stringify("double_integer_eq(", x, ", ", y, ')'),
                      *boolean-rep*, #f, file);
     end if;
   end);

define-primitive-emitter
  (#"dblfix-<",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y)
       = extract-operands(operation, file, *long-long-rep*, *long-long-rep*);
     contact-bgh-unless-empty(temps);
     if (*current-target*.long-long-size)
       deliver-result(defines, stringify('(', x, " < ", y, ')'),
                      *boolean-rep*, #f, file);
     else
       deliver-result(defines,
                      stringify("double_integer_lt(", x, ", ", y, ')'),
                      *boolean-rep*, #f, file);
     end if;
   end);

define-primitive-emitter
  (#"dblfix-+",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y)
       = extract-operands(operation, file, *long-long-rep*, *long-long-rep*);
     contact-bgh-unless-empty(temps);
     if (*current-target*.long-long-size)
       deliver-result(defines, stringify('(', x, " + ", y, ')'),
                      *long-long-rep*, #f, file);
     else
       deliver-result(defines,
                      stringify("double_integer_add(", x, ", ", y, ')'),
                      *long-long-rep*, #f, file);
     end if;
   end);

define-primitive-emitter
  (#"dblfix-*",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y)
       = extract-operands(operation, file, *long-long-rep*, *long-long-rep*);
     contact-bgh-unless-empty(temps);
     if (*current-target*.long-long-size)
       deliver-result(defines, stringify('(', x, " * ", y, ')'),
                      *long-long-rep*, #f, file);
     else
       deliver-result(defines,
                      stringify("double_integer_mul(", x, ", ", y, ')'),
                      *long-long-rep*, #f, file);
     end if;
   end);

define-primitive-emitter
  (#"dblfix--",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y)
       = extract-operands(operation, file, *long-long-rep*, *long-long-rep*);
     contact-bgh-unless-empty(temps);
     if (*current-target*.long-long-size)
       deliver-result(defines, stringify('(', x, " - ", y, ')'),
                      *long-long-rep*, #f, file);
     else
       deliver-result(defines,
                      stringify("double_integer_sub(", x, ", ", y, ')'),
                      *long-long-rep*, #f, file);
     end if;
   end);

define-primitive-emitter
  (#"dblfix-negative",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x) = extract-operands(operation, file, *long-long-rep*);
     contact-bgh-unless-empty(temps);
     if (*current-target*.long-long-size)
       deliver-result(defines, stringify("(- ", x, ')'), *long-long-rep*,
                      #f, file);
     else
       deliver-result(defines, stringify("double_integer_neg(", x, ')'),
                      *long-long-rep*, #f, file);
     end if;
   end);

define-primitive-emitter
  (#"dblfix-divide",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     spew-pending-defines(file);
     let (temps, x, y)
       = extract-operands(operation, file, *long-long-rep*, *long-long-rep*);
     contact-bgh-unless-empty(temps);
     if (*current-target*.long-long-size)
       deliver-results(defines,
                       vector(pair(stringify('(', x, " / ", y, ')'),
                                   *long-long-rep*),
                              pair(stringify('(', x, " % ", y, ')'),
                                   *long-long-rep*)),
                       #f, file);
     else
       deliver-results(defines,
                       vector(pair(stringify("double_integer_div(", x, ", ",
                                             y, ')'),
                                   *long-long-rep*),
                              pair(stringify("double_integer_mod(", x, ", ",
                                             y, ')'),
                                   *long-long-rep*)),
                       #f, file);
     end if;
   end);

define-primitive-emitter
  (#"dblfix-logior",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y)
       = extract-operands(operation, file, *long-long-rep*, *long-long-rep*);
     contact-bgh-unless-empty(temps);
     if (*current-target*.long-long-size)
       deliver-result(defines, stringify('(', x, " | ", y, ')'),
                      *long-long-rep*, #f, file);
     else
       deliver-result(defines,
                      stringify("double_integer_logior(", x, ", ", y, ')'),
                      *long-long-rep*, #f, file);
     end if;
   end);

define-primitive-emitter
  (#"dblfix-logxor",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y)
       = extract-operands(operation, file, *long-long-rep*, *long-long-rep*);
     contact-bgh-unless-empty(temps);
     if (*current-target*.long-long-size)
       deliver-result(defines, stringify('(', x, " ^ ", y, ')'),
                      *long-long-rep*, #f, file);
     else
       deliver-result(defines,
                      stringify("double_integer_logxor(", x, ", ", y, ')'),
                      *long-long-rep*, #f, file);
     end if;
   end);

define-primitive-emitter
  (#"dblfix-logand",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y)
       = extract-operands(operation, file, *long-long-rep*, *long-long-rep*);
     contact-bgh-unless-empty(temps);
     if (*current-target*.long-long-size)
       deliver-result(defines, stringify('(', x, " & ", y, ')'),
                      *long-long-rep*, #f, file);
     else
       deliver-result(defines,
                      stringify("double_integer_logand(", x, ", ", y, ')'),
                      *long-long-rep*, #f, file);
     end if;
   end);

define-primitive-emitter
  (#"dblfix-lognot",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x) = extract-operands(operation, file, *long-long-rep*);
     contact-bgh-unless-empty(temps);
     if (*current-target*.long-long-size)
       deliver-result(defines, stringify("(~ ", x, ')'), *long-long-rep*,
                      #f, file);
     else
       deliver-result(defines, stringify("double_integer_lognot(", x, ')'),
                      *long-long-rep*, #f, file);
     end if;
   end);

define-primitive-emitter
  (#"dblfix-shift-left",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y)
       = extract-operands(operation, file, *long-long-rep*, *long-rep*);
     contact-bgh-unless-empty(temps);
     if (*current-target*.long-long-size)
       deliver-result(defines, stringify('(', x, " << ", y, ')'),
                      *long-long-rep*, #f, file);
     else
       deliver-result(defines,
                      stringify("double_integer_shl(", x, ", ", y, ')'),
                      *long-long-rep*, #f, file);
     end if;
   end);

define-primitive-emitter
  (#"dblfix-shift-right",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y)
       = extract-operands(operation, file, *long-long-rep*, *long-rep*);
     contact-bgh-unless-empty(temps);
     if (*current-target*.long-long-size)
       deliver-result(defines, stringify('(', x, " >> ", y, ')'),
                      *long-long-rep*, #f, file);
     else
       deliver-result(defines,
                      stringify("double_integer_shr(", x, ", ", y, ')'),
                      *long-long-rep*, #f, file);
     end if;
   end);

define-primitive-emitter
  (#"fixed-as-dblfix",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x) = extract-operands(operation, file, *long-rep*);
     contact-bgh-unless-empty(temps);
     if (*current-target*.long-long-size)
       deliver-result(results, stringify("((long long)", x, ')'),
                      *long-long-rep*, #f, file);
     else
       deliver-result(results, stringify("integer_to_double_integer(", x, ')'),
                      *long-long-rep*, #f, file);
     end if;
   end);

define-primitive-emitter
  (#"dblfix-as-fixed",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x) = extract-operands(operation, file, *long-long-rep*);
     contact-bgh-unless-empty(temps);
     if (*current-target*.long-long-size)
       deliver-result(results, stringify("((long)", x, ')'),
                      *long-rep*, #f, file);
     else
       deliver-result(results, stringify("double_integer_to_integer(", x, ')'),
                      *long-rep*, #f, file);
     end if;
   end);


// Single float primitives.

define-primitive-emitter
  (#"fixed-as-single",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x) = extract-operands(operation, file, *long-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify("((float)", x, ')'),
		    *float-rep*, #f, file);
   end);

define-primitive-emitter
  (#"double-as-single",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x) = extract-operands(operation, file, *double-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify("((float)", x, ')'),
		    *float-rep*, #f, file);
   end);

define-primitive-emitter
  (#"extended-as-single",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x) = extract-operands(operation, file, *long-double-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify("((float)", x, ')'),
		    *float-rep*, #f, file);
   end);

define-primitive-emitter
  (#"single-<",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
                                          *float-rep*, *float-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify('(', x, " < ", y, ')'), *boolean-rep*,
		    #f, file);
   end);

define-primitive-emitter
  (#"single-<=",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
                                          *float-rep*, *float-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify('(', x, " <= ", y, ')'), *boolean-rep*,
		    #f, file);
   end);

define-primitive-emitter
  (#"single-=",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
                                          *float-rep*, *float-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify('(', x, " == ", y, ')'),
		    *boolean-rep*, #f, file);
   end);

define-primitive-emitter
  (#"single-==",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
                                          *float-rep*, *float-rep*);
     contact-bgh-unless-empty(temps);
     // ### This isn't right -- should really be doing a bitwise comparison.
     deliver-result(results, stringify('(', x, " == ", y, ')'),
		    *boolean-rep*, #f, file);
   end);

define-primitive-emitter
  (#"single-~=",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
                                          *float-rep*, *float-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify('(', x, " != ", y, ')'), *boolean-rep*,
		    #f, file);
   end);

define-primitive-emitter
  (#"single-+",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
                                          *float-rep*, *float-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify('(', x, " + ", y, ')'), *float-rep*,
		    #f, file);
   end);

define-primitive-emitter
  (#"single-*",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
                                          *float-rep*, *float-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify('(', x, " * ", y, ')'), *float-rep*,
		    #f, file);
   end);

define-primitive-emitter
  (#"single--",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
                                          *float-rep*, *float-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify('(', x, " - ", y, ')'), *float-rep*,
		    #f, file);
   end);

define-primitive-emitter
  (#"single-/",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
                                          *float-rep*, *float-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify('(', x, " / ", y, ')'), *float-rep*,
		    #f, file);
   end);

define-primitive-emitter
  (#"single-abs",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x) = extract-operands(operation, file, *float-rep*);
     contact-bgh-unless-empty(temps);
     maybe-emit-include("math.h", file);
     deliver-result(results, stringify("fabsf(", x, ')'), *float-rep*,
		    #f, file);
   end);

define-primitive-emitter
  (#"single-negative",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x) = extract-operands(operation, file, *float-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify("(-", x, ')'), *float-rep*, #f, file);
   end);

define-primitive-emitter
  (#"single-floor",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x) = extract-operands(operation, file, *float-rep*);
     contact-bgh-unless-empty(temps);
     maybe-emit-include("math.h", file);
     deliver-result(results, stringify("((long)floor(", x, "))"),
		    *long-rep*, #f, file);
   end);

define-primitive-emitter
  (#"single-ceiling",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x) = extract-operands(operation, file, *float-rep*);
     contact-bgh-unless-empty(temps);
     maybe-emit-include("math.h", file);
     deliver-result(results, stringify("((long)ceil(", x, "))"),
		    *long-rep*, #f, file);
   end);

define-primitive-emitter
  (#"single-round",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x) = extract-operands(operation, file, *float-rep*);
     contact-bgh-unless-empty(temps);
     maybe-emit-include("math.h", file);
     deliver-result(results, stringify("((long)rint(", x, "))"),
		    *long-rep*, #f, file);
   end);

define-primitive-emitter
  (#"single-decode",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     spew-pending-defines(file);
     let (temps, x) = extract-operands(operation, file, *float-rep*);
     contact-bgh-unless-empty(temps);
     maybe-emit-include("math.h", file);
     let temp = new-local(file, modifier: "exp", wanted-rep: "int");
     deliver-results(defines,
                     vector(pair(stringify("frexpf(", x, ", &", temp, ')'),
                                 *float-rep*),
			    pair(temp, *int-rep*)),
                     #t, file);
   end);

define-primitive-emitter
  (#"single-scale",
   method (results :: false-or(<definition-site-variable>),
           operation :: <primitive>,
           file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
                                          *float-rep*, *int-rep*);
     contact-bgh-unless-empty(temps);
     maybe-emit-include("math.h", file);
     deliver-result(results, stringify("ldexpf(", x, ",", y,")"),
                    *float-rep*, #f, file);
   end);

// Double float primitives.

define-primitive-emitter
  (#"fixed-as-double",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x) = extract-operands(operation, file, *long-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify("((double)", x, ')'),
		    *double-rep*, #f, file);
   end);

define-primitive-emitter
  (#"single-as-double",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x) = extract-operands(operation, file, *float-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify("((double)", x, ')'),
		    *double-rep*, #f, file);
   end);

define-primitive-emitter
  (#"extended-as-double",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x) = extract-operands(operation, file, *long-double-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify("((double)", x, ')'),
		    *double-rep*, #f, file);
   end);

define-primitive-emitter
  (#"double-<",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
				   *double-rep*, *double-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify('(', x, " < ", y, ')'), *boolean-rep*,
		    #f, file);
   end);

define-primitive-emitter
  (#"double-<=",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
				   *double-rep*, *double-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results,
		    stringify('(', x, " <= ", y, ')'), *boolean-rep*,
		    #f, file);
   end);

define-primitive-emitter
  (#"double-=",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
				   *double-rep*, *double-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify('(', x, " == ", y, ')'),
		    *boolean-rep*, #f, file);
   end);

define-primitive-emitter
  (#"double-==",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
				   *double-rep*, *double-rep*);
     contact-bgh-unless-empty(temps);
     // ### This isn't right -- should really be doing a bitwise comparison.
     deliver-result(results, stringify('(', x, " == ", y, ')'),
		    *boolean-rep*, #f, file);
   end);

define-primitive-emitter
  (#"double-~=",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
				   *double-rep*, *double-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results,
		    stringify('(', x, " != ", y, ')'), *boolean-rep*,
		    #f, file);
   end);

define-primitive-emitter
  (#"double-+",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
				   *double-rep*, *double-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify('(', x, " + ", y, ')'), *double-rep*,
		    #f, file);
   end);

define-primitive-emitter
  (#"double-*",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
				   *double-rep*, *double-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify('(', x, " * ", y, ')'), *double-rep*,
		    #f, file);
   end);

define-primitive-emitter
  (#"double--",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
				   *double-rep*, *double-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify('(', x, " - ", y, ')'), *double-rep*,
		    #f, file);
   end);

define-primitive-emitter
  (#"double-/",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
				   *double-rep*, *double-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify('(', x, " / ", y, ')'), *double-rep*,
		    #f, file);
   end);

define-primitive-emitter
  (#"double-abs",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x) = extract-operands(operation, file, *double-rep*);
     contact-bgh-unless-empty(temps);
     maybe-emit-include("math.h", file);
     deliver-result(results, stringify("fabs(", x, ')'), *double-rep*,
		    #f, file);
   end);

define-primitive-emitter
  (#"double-negative",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x) = extract-operands(operation, file, *double-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify("(-", x, ')'), *double-rep*,
		    #f, file);
   end);

define-primitive-emitter
  (#"double-floor",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x) = extract-operands(operation, file, *double-rep*);
     contact-bgh-unless-empty(temps);
     maybe-emit-include("math.h", file);
     deliver-result(results, stringify("((long)floor(", x, "))"),
		    *long-rep*, #f, file);
   end);

define-primitive-emitter
  (#"double-ceiling",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x) = extract-operands(operation, file, *double-rep*);
     contact-bgh-unless-empty(temps);
     maybe-emit-include("math.h", file);
     deliver-result(results, stringify("((long)ceil(", x, "))"),
		    *long-rep*, #f, file);
   end);

define-primitive-emitter
  (#"double-round",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x) = extract-operands(operation, file, *double-rep*);
     contact-bgh-unless-empty(temps);
     maybe-emit-include("math.h", file);
     deliver-result(results, stringify("((long)rint(", x, "))"),
		    *long-rep*, #f, file);
   end);

define-primitive-emitter
  (#"double-decode",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     spew-pending-defines(file);
     let (temps, x) = extract-operands(operation, file, *double-rep*);
     contact-bgh-unless-empty(temps);
     maybe-emit-include("math.h", file);
     let temp = new-local(file, modifier: "exp", wanted-rep: "int");
     deliver-results(defines,
                     vector(pair(stringify("frexp(", x, ", &", temp, ')'),
                                 *double-rep*),
                            pair(temp, *int-rep*)),
		     #t, file);
   end);

define-primitive-emitter
  (#"double-scale",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
                                          *double-rep*, *int-rep*);
     contact-bgh-unless-empty(temps);
     maybe-emit-include("math.h", file);
     deliver-result(results, stringify("ldexp(", x, ",", y, ")"),
                    *double-rep*, #f, file);
   end);

// Extended float primitives.

define-primitive-emitter
  (#"fixed-as-extended",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x) = extract-operands(operation, file, *long-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify("((long double)", x, ')'),
		    *long-double-rep*, #f, file);
   end);

define-primitive-emitter
  (#"single-as-extended",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x) = extract-operands(operation, file, *float-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify("((long double)", x, ')'),
		    *long-double-rep*, #f, file);
   end);

define-primitive-emitter
  (#"double-as-extended",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x) = extract-operands(operation, file, *double-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify("((long double)", x, ')'),
		    *long-double-rep*, #f, file);
   end);

define-primitive-emitter
  (#"extended-<",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
				   *long-double-rep*, *long-double-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify('(', x, " < ", y, ')'), *boolean-rep*,
		    #f, file);
   end);

define-primitive-emitter
  (#"extended-<=",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
				   *long-double-rep*, *long-double-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results,
		    stringify('(', x, " <= ", y, ')'), *boolean-rep*,
		    #f, file);
   end);

define-primitive-emitter
  (#"extended-=",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
				   *long-double-rep*, *long-double-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify('(', x, " == ", y, ')'),
		    *boolean-rep*, #f, file);
   end);

define-primitive-emitter
  (#"extended-==",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
				   *long-double-rep*, *long-double-rep*);
     contact-bgh-unless-empty(temps);
     // ### This isn't right -- should really be doing a bitwise comparison.
     deliver-result(results, stringify('(', x, " == ", y, ')'),
		    *boolean-rep*, #f, file);
   end);

define-primitive-emitter
  (#"extended-~=",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
				   *long-double-rep*, *long-double-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results,
		    stringify('(', x, " != ", y, ')'), *boolean-rep*,
		    #f, file);
   end);

define-primitive-emitter
  (#"extended-+",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
				   *long-double-rep*, *long-double-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify('(', x, " + ", y, ')'),
		    *long-double-rep*, #f, file);
   end);

define-primitive-emitter
  (#"extended-*",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
				   *long-double-rep*, *long-double-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify('(', x, " * ", y, ')'),
		    *long-double-rep*, #f, file);
   end);

define-primitive-emitter
  (#"extended--",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
				   *long-double-rep*, *long-double-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify('(', x, " - ", y, ')'),
		    *long-double-rep*, #f, file);
   end);

define-primitive-emitter
  (#"extended-/",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
				   *long-double-rep*, *long-double-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify('(', x, " / ", y, ')'),
		    *long-double-rep*, #f, file);
   end);

define-primitive-emitter
  (#"extended-abs",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x) = extract-operands(operation, file, *long-double-rep*);
     contact-bgh-unless-empty(temps);
     maybe-emit-include("math.h", file);
     deliver-result(results, stringify("fabs(", x, ')'),
		    *long-double-rep*, #f, file);
   end);

define-primitive-emitter
  (#"extended-negative",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x) = extract-operands(operation, file, *long-double-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify("(-", x, ')'), *long-double-rep*,
		    #f, file);
   end);

define-primitive-emitter
  (#"extended-floor",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x) = extract-operands(operation, file, *long-double-rep*);
     contact-bgh-unless-empty(temps);
     maybe-emit-include("math.h", file);
     deliver-result(results, stringify("((long)floor(", x, "))"),
		    *long-rep*, #f, file);
   end);

define-primitive-emitter
  (#"extended-ceiling",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x) = extract-operands(operation, file, *long-double-rep*);
     contact-bgh-unless-empty(temps);
     maybe-emit-include("math.h", file);
     deliver-result(results, stringify("((long)ceil(", x, "))"),
		    *long-rep*, #f, file);
   end);

define-primitive-emitter
  (#"extended-round",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x) = extract-operands(operation, file, *long-double-rep*);
     contact-bgh-unless-empty(temps);
     maybe-emit-include("math.h", file);
     deliver-result(results, stringify("((long)rint(", x, "))"),
		    *long-rep*, #f, file);
   end);

define-primitive-emitter
  (#"extended-decode",
   method (defines :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     spew-pending-defines(file);
     let (temps, x) = extract-operands(operation, file, *long-double-rep*);
     contact-bgh-unless-empty(temps);
     maybe-emit-include("math.h", file);
     let temp = new-local(file, modifier: "exp", wanted-rep: "int");
     deliver-results(defines,
                     vector(pair(stringify("frexpl(", x, ", &", temp, ')'),
                                 *long-double-rep*),
                            pair(temp, *int-rep*)),
		     #t, file);
   end);

define-primitive-emitter
  (#"extended-scale",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file,
                                          *long-double-rep*, *int-rep*);
     contact-bgh-unless-empty(temps);
     maybe-emit-include("math.h", file);
     deliver-result(results, stringify("ldexpl(", x, ",", y, ")"),
                    *long-double-rep*, #f, file);
   end);

// raw pointer operations.

define-primitive-emitter
  (#"make-raw-pointer",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x) = extract-operands(operation, file, *long-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify("((void *)", x, ')'),
		    *ptr-rep*, #f, file);
   end);

define-primitive-emitter
  (#"raw-pointer-address",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x) = extract-operands(operation, file, *ptr-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify("((long)", x, ')'),
		    *long-rep*, #f, file);
   end);

define-primitive-emitter
  (#"pointer-+",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file, *ptr-rep*, *long-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results,
		    stringify("((void *)((char *)", x, " + ", y, "))"),
		    *ptr-rep*, #f, file);
   end);

define-primitive-emitter
  (#"pointer--",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file, *ptr-rep*, *ptr-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results,
		    stringify("((char *)", x, " - (char *)", y, ')'),
		    *long-rep*, #f, file);
   end);

define-primitive-emitter
  (#"pointer-<",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file, *ptr-rep*, *ptr-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results,
		    stringify("((char *)", x, " < (char *)", y, ')'),
		    *boolean-rep*, #f, file);
   end);

define-primitive-emitter
  (#"pointer-=",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, x, y) = extract-operands(operation, file, *ptr-rep*, *ptr-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results, stringify('(', x, " == ", y, ')'),
		    *boolean-rep*, #f, file);
   end);

define-primitive-emitter
  (#"pointer-deref",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let type-dep = operation.depends-on;
     let rep = rep-for-c-type(type-dep.source-exp);
     let ptr-dep = type-dep.dependent-next;
     let (ptr, ptr-temp?) = ref-leaf(*ptr-rep*, ptr-dep.source-exp, file);
     let offset-dep = ptr-dep.dependent-next;
     let (offset, offset-temp?) = ref-leaf(*long-rep*, offset-dep.source-exp, file);
     contact-bgh-if(ptr-temp? | offset-temp?);

     spew-pending-defines(file);
     deliver-result(results,
		    stringify("(*(", rep.representation-c-type, " *)((char *)",
			      ptr, " + ", offset, " ))"),
		    rep, #f, file);
   end);

define-primitive-emitter
  (#"pointer-deref-setter",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let new-dep = operation.depends-on;
     let type-dep = new-dep.dependent-next;
     let rep = rep-for-c-type(type-dep.source-exp);
     let (new, new-temp?) = ref-leaf(rep, new-dep.source-exp, file);
     let ptr-dep = type-dep.dependent-next;
     let (ptr, ptr-temp?) = ref-leaf(*ptr-rep*, ptr-dep.source-exp, file);
     let offset-dep = ptr-dep.dependent-next;
     let (offset, offset-temp?) = ref-leaf(*long-rep*, offset-dep.source-exp, file);
     contact-bgh-if(new-temp? | ptr-temp? | offset-temp?);

     spew-pending-defines(file);
     format(file.file-guts-stream, "*(%s *)((char *)%s + %s) = %s;\n",
	    rep.representation-c-type, ptr, offset, new);
     
     deliver-results(results, #[], #f, file);
   end);

define-primitive-emitter
  (#"vector-elements",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let vec = operation.depends-on.source-exp;
     let (temps, vec-expr) = extract-operands(operation, file, *heap-rep*);
     contact-bgh-unless-empty(temps);
     let classes = vec.derived-type.find-direct-classes;
     assert(classes ~== #f & classes ~== #());
     let offset = dylan-slot-offset(classes.first, #"%element");
     for (class in classes.tail)
       assert(offset == dylan-slot-offset(class, #"%element"));
     end for;
     deliver-result
       (results,
	stringify("((void *)((char *)", vec-expr, " + ", offset, "))"),
	*ptr-rep*, #f, file);
   end);

define-primitive-emitter
  (#"object-address",
   method (results :: false-or(<definition-site-variable>),
	   operation :: <primitive>,
	   file :: <file-state>)
       => ();
     let (temps, object) = extract-operands(operation, file, *heap-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results,
		    stringify("((void *)", object, ')'),
		    *ptr-rep*, #f, file);
   end);

define-primitive-emitter
  (#"heap-object-at",
   method (results :: false-or(<definition-site-variable>),
           operation :: <primitive>,
           file :: <file-state>)
    => ();
     let (temps, ptr) = extract-operands(operation, file, *ptr-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results,
                    stringify("((heapptr_t)", ptr, ')'),
                    *heap-rep*, #f, file);
   end);

define-primitive-emitter
  (#"general-object-at",
   method (results :: false-or(<definition-site-variable>),
           operation :: <primitive>,
           file :: <file-state>)
    => ();
     let (temps, ptr) = extract-operands(operation, file, *ptr-rep*);
     contact-bgh-unless-empty(temps);
     deliver-result(results,
                    stringify("(*((descriptor_t*)", ptr, "))"),
                    *general-rep*, #f, file);
   end);

// Code moveability

// A list of primitives which can safely be moved around in C code.
// It is always safe to not list a primitive here.
//
define constant $sequence-of-moveable-primitives 
  = #[#"extract-args",
      #"values",                     // Value primitives.
      #"allocate",                   // Allocation primitives.
      #"allocate-with-data-word",
      #"make-immediate",
      #"c-string",                   // Foreign code interface primitives.
      #"as-boolean",                 // Predicate primitives
      #"not",
      #"==",
      #"initialized?",
      #"initial-symbols",
      #"ref-slot",                   // Slot access primitives.
      #"fixnum-=",                   // Fixnum primitives.
      #"fixnum-<",
      #"fixnum-+",
      #"fixnum-*",
      #"fixnum--",
      #"fixnum-negative",
      #"fixnum-divide",
      #"fixnum-logior",
      #"fixnum-logxor",
      #"fixnum-logand",
      #"fixnum-lognot",
      #"fixnum-shift-left",
      #"fixnum-shift-right",
      #"fixnum-logical-shift-right",
      #"fixed-as-single",            // Single float primitives.
      #"double-as-single",
      #"extended-as-single",
      #"single-<",
      #"single-<=",
      #"single-=",
      #"single-==",
      #"single-~=",
      #"single-+",
      #"single-*",
      #"single--",
      #"single-/",
      #"single-abs",
      #"single-negative",
      #"single-floor",
      #"single-ceiling",
      #"single-round",
      #"single-scale",
      #"fixed-as-double",            // Double float primitives.
      #"single-as-double",
      #"extended-as-double",
      #"double-<",
      #"double-<=",
      #"double-=",
      #"double-==",
      #"double-~=",
      #"double-+",
      #"double-*",
      #"double--",
      #"double-/",
      #"double-abs",
      #"double-negative",
      #"double-floor",
      #"double-ceiling",
      #"double-round",
      #"double-scale",
      #"fixed-as-extended",          // Extended float primitives.
      #"single-as-extended",
      #"double-as-extended",
      #"extended-<",
      #"extended-<=",
      #"extended-=",
      #"extended-==",
      #"extended-~=",
      #"extended-+",
      #"extended-*",
      #"extended--",
      #"extended-/",
      #"extended-abs",
      #"extended-negative",
      #"extended-floor",
      #"extended-ceiling",
      #"extended-round",
      #"extended-scale",
      #"make-raw-pointer",           // raw pointer operations.
      #"raw-pointer-address",
      #"pointer-+",
      #"pointer--",
      #"pointer-<",
      #"pointer-=",
      #"pointer-deref",
      #"vector-elements",
      #"object-address",
      #"heap-object-at",
      #"general-object-at"];

define constant *moveable-primitives-table* 
  = begin
      let table = make(<object-table>);
      for (prim-name in $sequence-of-moveable-primitives)
      table[prim-name] := #t;
      end for;
      table;
    end;

define method c-code-moveable? (exp :: <expression>) => answer :: <boolean>;
  #f;
end method c-code-moveable?;

define method c-code-moveable? (prim :: <primitive>) => answer :: <boolean>;
  element(*moveable-primitives-table*, prim.primitive-name, default: #f);
end method c-code-moveable?;
