module: cheese
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/optimize/primopt.dylan,v 1.19 1996/02/09 00:00:50 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.


define method optimize (component :: <component>, primitive :: <primitive>)
    => ();
  let info = primitive.primitive-info;
  maybe-restrict-type(component, primitive, info.priminfo-result-type);
  let transformer = info.priminfo-transformer;
  if (transformer)
    transformer(component, primitive);
  end;
end;


// magic debugging primitives.

define-primitive-transformer
  (#"break",
   method (component :: <component>, primitive :: <primitive>) => ();
     break("Hit break primitive.");
     replace-expression(component, primitive.dependents,
			make-literal-constant(make-builder(component),
					      as(<ct-value>, #f)));
   end);


// Call related primitives.

define-primitive-transformer
  (#"invoke-generic-entry",
   method (component :: <component>, primitive :: <primitive>) => ();
     replace-expression
       (component, primitive.dependents,
	make-operation(make-builder(component), <mv-call>,
		       listify-dependencies(primitive.depends-on),
		       use-generic-entry: #t));
			 
   end);


// Values manipulation primitives.

define-primitive-transformer
  (#"values",
   method (component :: <component>, primitive :: <primitive>) => ();
     let assign = primitive.dependents.dependent;
     let defns = assign.defines;
     if (defns & instance?(defns.var-info, <values-cluster-info>))
       // Assigning to a cluster.  Just compute a values type and propagate
       // it.
       for (dep = primitive.depends-on then dep.dependent-next,
	    types = #() then pair(dep.source-exp.derived-type, types),
	    while: dep)
       finally
	 maybe-restrict-type(component, primitive,
			     make-values-ctype(reverse!(types), #f));
       end;
     else
       // Assigning to a bunch of discreet variables.  Replace the assignment
       // with individual assignments for each value.
       let builder = make-builder(component);
       let next-var = #f;
       let let? = instance?(assign, <let-assignment>);
       for (var = defns then next-var,
	    val-dep = primitive.depends-on
	      then val-dep & val-dep.dependent-next,
	    while: var)
	 next-var := var.definer-next;
	 var.definer-next := #f;
	 let val = if (val-dep)
		     val-dep.source-exp;
		   else
		     make-literal-constant(builder, make(<literal-false>));
		   end;
	 if (let?)
	   build-let(builder, assign.policy, assign.source-location, var, val);
	 else
	   build-assignment(builder, assign.policy, assign.source-location,
			    var, val);
	 end;
       end;
       assign.defines := #f;
       // Insert the spred out assignments.
       insert-after(component, assign, builder.builder-result);
       // Nuke the original assignment.
       delete-and-unlink-assignment(component, assign);
     end;
   end);

define-primitive-transformer
  (#"canonicalize-results",
   method (component :: <component>, primitive :: <primitive>) => ();
     let nfixed-leaf = primitive.depends-on.dependent-next.source-exp;
     if (instance?(nfixed-leaf, <literal-constant>))
       let nfixed = as(<integer>, nfixed-leaf.value.literal-value);
       let cluster = primitive.depends-on.source-exp;
       let type = cluster.derived-type;
       if (fixed-number-of-values?(type))
	 let orig-assign = primitive.dependents.dependent;
	 let builder = make-builder(component);
	 let temps = map(method (type)
			   make-local-var(builder, #"temp", type);
			 end,
			 type.positional-types);
	 build-assignment(builder, orig-assign.policy,
			  orig-assign.source-location, temps, cluster);
	 let op
	   = if (nfixed < type.min-values)
	       let fixed = copy-sequence(temps, end: nfixed);
	       let rest = copy-sequence(temps, start: nfixed);
	       let op = make-operation(builder, <primitive>, rest,
				       name: #"vector");
	       let rest-temp
		 = make-local-var(builder, #"temp", object-ctype());
	       build-assignment(builder, orig-assign.policy,
				orig-assign.source-location, rest-temp, op);
	       make-operation(builder, <primitive>,
			      concatenate(fixed, list(rest-temp)),
			      name: #"values");
	     else
	       let false = make-literal-constant(builder, as(<ct-value>, #f));
	       let falses = make(<list>, size: nfixed - type.min-values,
				 fill: false);
	       let empty-vect
		 = make-literal-constant(builder, as(<ct-value>, #[]));
	       make-operation(builder, <primitive>,
			      concatenate(temps, falses, list(empty-vect)),
			      name: #"values");
	     end;
	 replace-expression(component, orig-assign.depends-on, op);
	 insert-before(component, orig-assign, builder-result(builder));
       else
	 let types = make(<stretchy-vector>);
	 for (remaining = type.positional-types then remaining.tail,
	      index from 0 below min(type.min-values, nfixed),
	      until: remaining == #())
	   add!(types, remaining.head);
	 finally
	   unless (index == nfixed)
	     let rest = ctype-union(type.rest-value-type,
				    specifier-type(#"<false>"));
	     for (remaining = remaining then remaining.tail,
		  index from index below nfixed,
		  until: remaining == #())
	       add!(types, ctype-union(remaining.head, rest));
	     finally
	       for (index from index below nfixed)
		 add!(types, rest);
	       end;
	     end;
	   end;
	 end;
	 add!(types, specifier-type(#"<simple-object-vector>"));
	 maybe-restrict-type(component, primitive,
			     make-values-ctype(as(<list>, types), #f));
       end;
     end;
   end);

define-primitive-transformer
  (#"values-sequence",
   method (component :: <component>, primitive :: <primitive>) => ();
     for (vec = primitive.depends-on.source-exp
	    then vec.definer.depends-on.source-exp,
	  assign = #f then vec.definer,
	  while: instance?(vec, <ssa-variable>))
     finally
       if (instance?(vec, <primitive>))
	 if (vec.primitive-name == #"vector")	 
	   let builder = make-builder(component);
	   let policy = assign.policy;
	   let source = assign.source-location;
	   let copy-arg
	     = if (primitive.home-function-region == vec.home-function-region)
		 method (arg :: <leaf>) => new :: <leaf>;
		   if (arg.expression-movable?)
		     arg;
		   else
		     let temp
		       = make-ssa-var(builder,
				      if (instance?(arg, <abstract-variable>))
					arg.var-info.debug-name;
				      else
					#"temp";
				      end,
				      arg.derived-type);
		     build-assignment(builder, policy, source, temp, arg);
		     temp;
		   end;
		 end;
	       else
		 method (arg :: <leaf>) => new :: <leaf>;
		   if (if (instance?(arg, <ssa-variable>))
			 instance?(arg.var-info, <lexical-var-info>);
		       else
			 arg.expression-movable?;
		       end)
		     arg;
		   else
		     let temp
		       = make-lexical-var(builder, 
					  if (instance?(arg,
							<abstract-variable>))
					    arg.var-info.debug-name;
					  else
					    #"temp";
					  end,
					  assign.source-location,
					  arg.derived-type);
		     build-let(builder, policy, source, temp, arg);
		     temp;
		   end;
		 end;
	       end if;
	   for (value-dep = vec.depends-on then value-dep.dependent-next,
		values = #() then pair(copy-arg(value-dep.source-exp), values),
		while: value-dep)
	   finally
	     replace-expression
	       (component, primitive.dependents,
		make-operation(make-builder(component), <primitive>,
			       reverse!(values), name: #"values"));
	   end;
	   insert-before(component, assign, builder-result(builder));
	 elseif (vec.primitive-name == #"canonicalize-results")
	   let vec-assign = vec.dependents.dependent;
	   let prim-assign = primitive.dependents.dependent;
	   if (vec-assign.region == prim-assign.region
		 & vec-assign.depends-on == vec.dependents)
	     let nfixed = vec.depends-on.dependent-next.source-exp;
	     if (instance?(nfixed, <literal-constant>) & nfixed.value = 0)
	       block (return)
		 for (assign = vec-assign.next-op then assign.next-op,
		      until: assign == prim-assign)
		   if ((instance?(assign.defines, <abstract-variable>)
			  & instance?(assign.defines.var-info,
				      <values-cluster-info>))
			 | consumes-cluster?(assign.depends-on.source-exp))
		     return();
		   end;
		 end;
		 replace-expression(component, prim-assign.depends-on,
				    vec.depends-on.source-exp);
	       end;
	     end;
	   end;
	 end;
       end;
     end;
   end);

define method consumes-cluster? (expr :: <leaf>) => res :: <boolean>;
  #f;
end;

define method consumes-cluster? (expr :: <abstract-variable>)
    => res :: <boolean>;
  instance?(expr.var-info, <values-cluster-info>);
end;

define method consumes-cluster? (expr :: <operation>)
    => res :: <boolean>;
  block (return)
    for (dep = expr.depends-on then dep.dependent-next,
	 while: dep)
      if (consumes-cluster?(dep.source-exp))
	return(#t);
      end;
    end;
    #f;
  end;
end;

define-primitive-transformer
  (#"merge-clusters",
   method (component :: <component>, primitive :: <primitive>) => ();
     local
       method repeat (dep :: false-or(<dependency>),
		      prev :: false-or(<dependency>),
		      all-fixed? :: <boolean>)
	 if (dep)
	   let cluster = dep.source-exp;
	   let type = cluster.derived-type;
	   if (fixed-number-of-values?(type))
	     if (type.min-values == 0)
	       let next = dep.dependent-next;
	       if (prev)
		 prev.dependent-next := next;
	       else
		 primitive.depends-on := next;
	       end;
	       remove-dependency-from-source(component, dep);
	       repeat(next, prev, all-fixed?);
	     else
	       repeat(dep.dependent-next, dep, all-fixed?);
	     end;
	   else
	     repeat(dep.dependent-next, dep, #f);
	   end;
	 elseif (all-fixed?)
	   let next = #f;
	   for (dep = primitive.depends-on then next,
		while: dep)
	     next := dep.dependent-next;
	     let cluster = dep.source-exp;
	     expand-cluster(component, cluster,
			    cluster.derived-type.min-values, #());
	   finally
	     for (dep = primitive.depends-on then dep.dependent-next,
		  vars = #() then pair(dep.source-exp, vars),
		  while: dep)
	     finally
	       replace-expression
		 (component, primitive.dependents,
		  make-operation(make-builder(component), <primitive>,
				 reverse!(vars), name: #"values"));
	     end;
	   end;
	 elseif (primitive.depends-on.dependent-next == #f)
	   replace-expression(component, primitive.dependents,
			      primitive.depends-on.source-exp);
	 end;
       end method repeat;
     repeat(primitive.depends-on, #f, #t);
   end method);


// Foreign code support primitives

define-primitive-transformer
  (#"call-out",
   method (component :: <component>, primitive :: <primitive>) => ();
     let func-dep = primitive.depends-on;
     let result-dep = func-dep.dependent-next;
     begin
       let result-type = result-dep.source-exp.dylan-type-for-c-type;
       maybe-restrict-type(component, primitive, result-type);
     end;
     let assign = primitive.dependents.dependent;
     local
       method repeat (dep :: false-or(<dependency>))
	 if (dep)
	   let type = dep.source-exp.dylan-type-for-c-type;
	   let next = dep.dependent-next;
	   if (next)
	     assert-type(component, assign, next, type);
	     repeat(next.dependent-next);
	   else
	     compiler-error("Type spec with no argument in call-out");
	   end;
	 end;
       end;
     repeat(result-dep.dependent-next);
   end);

define-primitive-transformer
  (#"c-expr",
   method (component :: <component>, primitive :: <primitive>) => ();
     let result-dep = primitive.depends-on;
     let result-type = result-dep.source-exp.dylan-type-for-c-type;
     maybe-restrict-type(component, primitive, result-type);
   end);

define method dylan-type-for-c-type (leaf :: <leaf>) => res :: <values-ctype>;
  if (instance?(leaf, <literal-constant>))
    let ct-value = leaf.value;
    if (instance?(ct-value, <literal-symbol>))
      let c-type = ct-value.literal-value;
      select (c-type)
	#"char", #"short", #"int", #"long",
	#"unsigned-char", #"unsigned-short", #"unsigned-int" =>
	  specifier-type(#"<integer>");
	#"ptr" => specifier-type(#"<raw-pointer>");
	#"float" => specifier-type(#"<single-float>");
	#"double" => specifier-type(#"<double-float>");
	#"long-double" => specifier-type(#"<extended-float>");
	#"void" => make-values-ctype(#(), #f);
      end;
    else
      object-ctype();
    end;
  else
    object-ctype();
  end;
end;



// Boolean canonicalization stuff.

define-primitive-transformer
  (#"as-boolean",
   method (component :: <component>, primitive :: <primitive>) => ();
     let arg = primitive.depends-on.source-exp;
     let arg-type = arg.derived-type;
     let false-type = specifier-type(#"<false>");
     if (csubtype?(arg.derived-type, specifier-type(#"<boolean>")))
       replace-expression(component, primitive.dependents, arg);
     elseif (~ctypes-intersect?(arg-type, false-type))
       replace-expression(component, primitive.dependents,
			  make-literal-constant(make-builder(component),
						as(<ct-value>, #t)));
     end;
   end);
			  
define-primitive-transformer
  (#"not",
   method (component :: <component>, primitive :: <primitive>) => ();
     let arg = primitive.depends-on.source-exp;
     let arg-type = arg.derived-type;
     let false-type = specifier-type(#"<false>");
     if (csubtype?(arg-type, false-type))
       replace-expression(component, primitive.dependents,
			  make-literal-constant(make-builder(component),
						as(<ct-value>, #t)));
     elseif (~ctypes-intersect?(arg-type, false-type))
       replace-expression(component, primitive.dependents,
			  make-literal-constant(make-builder(component),
						as(<ct-value>, #f)));
     elseif (instance?(arg, <ssa-variable>))
       let arg-source = arg.definer.depends-on.source-exp;
       if (instance?(arg-source, <primitive>)
	     & arg-source.primitive-name == #"not")
	 let source-source = arg-source.depends-on.source-exp;
	 let op = make-operation(make-builder(component), <primitive>,
				 list(source-source), name: #"as-boolean");
	 replace-expression(component, primitive.dependents, op);
       end;
     end;
   end);

define-primitive-transformer
  (#"==",
   method (component :: <component>, primitive :: <primitive>) => ();
     trivial-==-optimization(component, primitive,
			     primitive.depends-on.source-exp,
			     primitive.depends-on.dependent-next.source-exp);
   end);

define-primitive-transformer
  (#"initialized?",
   method (component :: <component>, primitive :: <primitive>) => ();
     let x = ct-initialized?(primitive.depends-on.source-exp);
     unless (x == #"can't tell")
       replace-expression(component, primitive.dependents,
			  make-literal-constant(make-builder(component),
						as(<ct-value>, x)));
     end;
   end);

define method ct-initialized?
    (expr :: <expression>) => res :: one-of(#t, #f, #"can't tell");
  #t;
end;

define method ct-initialized?
    (expr :: <primitive>) => res :: one-of(#t, #f, #"can't tell");
  if (expr.primitive-name == #"ref-slot")
    #"can't tell";
  else
    #t;
  end if;
end;

define method ct-initialized?
    (expr :: <prologue>) => res :: one-of(#t, #f, #"can't tell");
  #"can't tell";
end;

define method ct-initialized?
    (expr :: <slot-ref>) => res :: one-of(#t, #f, #"can't tell");
  if (slot-guaranteed-initialized?(expr.slot-info,
				   expr.depends-on.source-exp.derived-type))
    #t;
  else
    #"can't tell";
  end;
end;

define method ct-initialized?
    (expr :: <uninitialized-value>) => res :: one-of(#t, #f, #"can't tell");
  #f;
end;

define method ct-initialized?
    (expr :: <abstract-variable>) => res :: one-of(#t, #f, #"can't tell");
  #"can't tell";
end;

define method ct-initialized?
    (expr :: <ssa-variable>) => res :: one-of(#t, #f, #"can't tell");
  ct-initialized?(expr.definer.depends-on.source-exp);
end;


// Fixnums.

define-primitive-transformer
  (#"fixnum-=",
   method (component :: <component>, primitive :: <primitive>) => ();
     let x = primitive.depends-on.source-exp;
     let x-type = x.derived-type;
     let y = primitive.depends-on.dependent-next.source-exp;
     let y-type = y.derived-type;
     if (~ctypes-intersect?(x-type, y-type))
       replace-expression(component, primitive.dependents,
			  make-literal-constant(make-builder(component),
						as(<ct-value>, #f)));
     elseif (instance?(x-type, <limited-integer-ctype>)
	       & x-type == y-type
	       & x-type.low-bound = x-type.high-bound)
       replace-expression(component, primitive.dependents,
			  make-literal-constant(make-builder(component),
						as(<ct-value>, #t)));
     end;
   end);

define-primitive-transformer
  (#"fixnum-<",
   method (component :: <component>, primitive :: <primitive>) => ();
     let x = primitive.depends-on.source-exp;
     let x-type = x.derived-type;
     let y = primitive.depends-on.dependent-next.source-exp;
     let y-type = y.derived-type;
     if (instance?(x-type, <limited-integer-ctype>)
	   & instance?(y-type, <limited-integer-ctype>))
       if (x-type.high-bound < y-type.low-bound)
	 replace-expression(component, primitive.dependents,
			    make-literal-constant(make-builder(component),
						  as(<ct-value>, #t)));
       elseif (x-type.low-bound >= y-type.high-bound)
	 replace-expression(component, primitive.dependents,
			    make-literal-constant(make-builder(component),
						  as(<ct-value>, #f)));
       end;
     end;
   end);


// raw pointer

define-primitive-transformer
  (#"pointer-deref",
   method (component :: <component>, primitive :: <primitive>) => ();
     let result-dep = primitive.depends-on;
     let result-type = result-dep.source-exp.dylan-type-for-c-type;
     maybe-restrict-type(component, primitive, result-type);
   end);

define-primitive-transformer
  (#"pointer-deref-setter",
   method (component :: <component>, primitive :: <primitive>) => ();
     let new-dep = primitive.depends-on;
     let type = new-dep.dependent-next.source-exp.dylan-type-for-c-type;
     assert-type(component, primitive.dependents.dependent, new-dep, type);
   end);
