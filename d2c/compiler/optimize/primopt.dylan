module: cheese
rcs-header: $Header: /scm/cvs/src/d2c/compiler/optimize/primopt.dylan,v 1.7 2002/10/31 20:59:56 housel Exp $
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

define method optimize (component :: <component>, primitive :: <primitive>)
    => ();
  let info = primitive.primitive-info;
  let type-deriver = info.priminfo-type-deriver;
  maybe-restrict-type
    (component, primitive,
     if (type-deriver)
       let args = listify-dependencies(primitive.depends-on);
       type-deriver(primitive, map(derived-type, args)).ctype-extent;
     else
       info.priminfo-result-type;
     end if);
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
			make-literal-constant(make-builder(component), #f));
   end);


// Call related primitives.

define-primitive-transformer
  (#"mv-call",
   method (component :: <component>, primitive :: <primitive>) => ();
     replace-expression
       (component, primitive.dependents,
	make-operation(make-builder(component), <mv-call>,
		       listify-dependencies(primitive.depends-on),
		       use-generic-entry: #f));
   end method);

define-primitive-transformer
  (#"invoke-generic-entry",
   method (component :: <component>, primitive :: <primitive>) => ();
     replace-expression
       (component, primitive.dependents,
	make-operation(make-builder(component), <mv-call>,
		       listify-dependencies(primitive.depends-on),
		       use-generic-entry: #t));
			 
   end);

define-primitive-transformer
  (#"main-entry",
   method (component :: <component>, primitive :: <primitive>) => ();
     let func = primitive.depends-on.source-exp;
     if (instance?(func, <definition-constant-leaf>))
       replace-expression
	 (component, primitive.dependents,
	  make-literal-constant
	    (make-builder(component),
	     make(<ct-entry-point>, for: func.const-defn.ct-value,
		  kind: #"main")));
     end if;
   end method);


// Values manipulation primitives.

define-primitive-type-deriver
  (#"values",
   method (primitive :: <primitive>, arg-types :: <list>)
       => result :: <values-ctype>;
     make-values-ctype(arg-types, #f);
   end method);

define-primitive-transformer
  (#"values",
   method (component :: <component>, primitive :: <primitive>) => ();
     let assign = primitive.dependents.dependent;
     let defns = assign.defines;
     unless (defns & instance?(defns.var-info, <values-cluster-info>))
       // Assigning to a bunch of discrete variables.  Replace the assignment
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
		     make-literal-constant(builder, #f);
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
	       let false = make-literal-constant(builder, #f);
	       let falses = make(<list>, size: nfixed - type.min-values,
				 fill: false);
	       let empty-vect
		 = make-literal-constant(builder, #[]);
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
	 maybe-restrict-type
	   (component, primitive,
	    make-values-ctype(as(<list>, types), #f).ctype-extent);
       end;
     end;
   end);

define-primitive-transformer
  (#"values-sequence",
   method (component :: <component>, primitive :: <primitive>) => ();
     let leaf = primitive.depends-on.source-exp;
     if (instance?(leaf, <ssa-variable>))
       let assign = leaf.definer;
       let vec = assign.depends-on.source-exp;
       if (instance?(vec, <primitive>))
	 if (vec.primitive-name == #"vector")
	   let ref-site = primitive.home-function-region;
	   local method copy-arg (arg :: <leaf>) => copy :: <leaf>;
		   maybe-copy(component, arg, assign, ref-site);
		 end method copy-arg;
	   for (value-dep = vec.depends-on then value-dep.dependent-next,
		values = #() then pair(copy-arg(value-dep.source-exp), values),
		while: value-dep)
	   finally
	     replace-expression
	       (component, primitive.dependents,
		make-operation(make-builder(component), <primitive>,
			       reverse!(values), name: #"values"));
	   end;
	 elseif (vec.primitive-name == #"canonicalize-results")
	   let vec-assign = vec.dependents.dependent;
	   let prim-assign = primitive.dependents.dependent;
	   if (vec-assign.region == prim-assign.region
		 & vec-assign.depends-on == vec.dependents)
	     let nfixed = vec.depends-on.dependent-next.source-exp;
	     if (instance?(nfixed, <literal-constant>)
		   & nfixed.value.literal-value = 0)
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


// Arithmetic optimizers.

define-primitive-transformer
  (#"fixnum-<",
   method (component :: <component>, primitive :: <primitive>)
     let x = primitive.depends-on.source-exp;
     let y = primitive.depends-on.dependent-next.source-exp;
     let (maybe-less?, maybe-eql?, maybe-greater?)
       = compare-integer-types(x.derived-type, y.derived-type);
     if (~maybe-less?)
       replace-expression
	 (component, primitive.dependents,
	  make-literal-constant(make-builder(component), #f));
     elseif (~(maybe-greater? | maybe-eql?))
       replace-expression
	 (component, primitive.dependents,
	  make-literal-constant(make-builder(component), #t));
     end if;
   end method);
	 
define-primitive-transformer
  (#"fixnum-=",
   method (component :: <component>, primitive :: <primitive>)
     let x = primitive.depends-on.source-exp;
     let y = primitive.depends-on.dependent-next.source-exp;
     let (maybe-less?, maybe-eql?, maybe-greater?)
       = compare-integer-types(x.derived-type, y.derived-type);
     if (~maybe-eql?)
       replace-expression
	 (component, primitive.dependents,
	  make-literal-constant
	    (make-builder(component), #f));
     elseif (~(maybe-less? | maybe-greater?))
       replace-expression
	 (component, primitive.dependents,
	  make-literal-constant
	    (make-builder(component), #t));
     end if;
   end method);
	 
define generic compare-integer-types
    (type1 :: <ctype>, type2 :: <ctype>)
    => (maybe-less? :: <boolean>,
	maybe-eql? :: <boolean>,
	maybe-greater? :: <boolean>);

define method compare-integer-types
    (type1 :: <ctype>, type2 :: <ctype>)
    => (maybe-less? :: <boolean>,
	maybe-eql? :: <boolean>,
	maybe-greater? :: <boolean>);
  values(#t, #t, #t);
end method compare-integer-types;

define method compare-integer-types
    (type1 :: <limited-integer-ctype>, type2 :: <limited-integer-ctype>)
    => (maybe-less? :: <boolean>,
	maybe-eql? :: <boolean>,
	maybe-greater? :: <boolean>);
  let low1 = type1.low-bound;
  let high1 = type1.high-bound;
  let low2 = type2.low-bound;
  let high2 = type2.high-bound;

  let intersection-low
    = if (low1)
	if (low2)
	  max(low1, low2);
	else
	  low1;
	end;
      else
	low2;
      end;
  let intersection-high
    = if (high1)
	if (high2)
	  min(high1, high2);
	else
	  high1;
	end;
      else
	high2;
      end if;

  values(~low1 | ~high2 | low1 < high2,
	 ~intersection-low | ~intersection-high
	   | intersection-low <= intersection-high,
	 ~low2 | ~high1 | low2 < high1);
end method compare-integer-types;


define-primitive-type-deriver
  (#"fixnum-+",
   method (primitive :: <primitive>, arg-types :: <list>)
       => res :: <values-ctype>;
     let x = primitive.depends-on.source-exp;
     if (instance?(x, <literal-constant>))
       type-adding-constant(arg-types.second, x.value.literal-value);
     else
       let y = primitive.depends-on.dependent-next.source-exp;
       if (instance?(y, <literal-constant>))
	 type-adding-constant(arg-types.first, y.value.literal-value);
       else
	 specifier-type(#"<integer>");
       end if;
     end if;
   end method);

define-primitive-type-deriver
  (#"fixnum--",
   method (primitive :: <primitive>, arg-types :: <list>)
       => res :: <values-ctype>;
     let x = primitive.depends-on.source-exp;
     if (instance?(x, <literal-constant>))
       type-adding-constant(type-negated(arg-types.second),
			    x.value.literal-value);
     else
       let y = primitive.depends-on.dependent-next.source-exp;
       if (instance?(y, <literal-constant>))
	 type-adding-constant(arg-types.first, -y.value.literal-value);
       else
	 specifier-type(#"<integer>");
       end if;
     end if;
   end method);

define-primitive-type-deriver
  (#"fixnum-negative",
   method (primitive :: <primitive>, arg-types :: <list>)
       => res :: <values-ctype>;
     type-negated(arg-types.first);
   end method);

define generic type-adding-constant
    (ctype :: <ctype>, constant :: <extended-integer>) => res :: <ctype>;

define method type-adding-constant
    (ctype :: <ctype>, constant :: <extended-integer>) => res :: <ctype>;
  ctype;
end method type-adding-constant;

define method type-adding-constant
    (ctype :: <limited-integer-ctype>, constant :: <extended-integer>)
    => res :: <ctype>;
  //
  // Instead of computing a precise result, we assume that we are going
  // to end up doing this addition zero or more times.  This lets us infer
  // that in ``for (i from 0) ... end'' i will be positive.  Which is all
  // we are trying to get for now.
  if (constant.positive?)
    make-canonical-limited-integer(ctype.base-class, ctype.low-bound, #f);
  elseif (constant.negative?)
    make-canonical-limited-integer(ctype.base-class, #f, ctype.high-bound);
  else
    ctype;
  end if;
end method type-adding-constant;

define generic type-negated
    (ctype :: <ctype>) => res :: <ctype>;

define method type-negated
    (ctype :: <ctype>) => res :: <ctype>;
  ctype;
end method type-negated;

define method type-negated
    (ctype :: <limited-integer-ctype>) => res :: <ctype>;
  let high = ctype.high-bound;
  let low = ctype.low-bound;
  make-canonical-limited-integer(ctype.base-class, high & -high, low & -low);
end method type-negated;


define-primitive-transformer
  (#"fixnum-+",
   method (component :: <component>, primitive :: <primitive>) => ();
     let x = primitive.depends-on.source-exp;
     let y = primitive.depends-on.dependent-next.source-exp;
     if (instance?(x, <literal-constant>) & x.value.literal-value = 0)
       replace-expression(component, primitive.dependents, y);
     elseif (instance?(y, <literal-constant>) & y.value.literal-value = 0)
       replace-expression(component, primitive.dependents, x);
     end if;
   end method);

define-primitive-transformer
  (#"fixnum--",
   method (component :: <component>, primitive :: <primitive>) => ();
     let x = primitive.depends-on.source-exp;
     let y = primitive.depends-on.dependent-next.source-exp;
     if (instance?(x, <literal-constant>) & x.value.literal-value = 0)
       replace-expression
	 (component, primitive.dependents,
	  make-operation
	    (make-builder(component), <primitive>, list(y),
	     name: #"fixnum-negative"));
     elseif (instance?(y, <literal-constant>) & y.value.literal-value = 0)
       replace-expression(component, primitive.dependents, x);
     end if;
   end method);



define-primitive-type-deriver
  (#"fixnum-logand",
   method (primitive :: <primitive>, arg-types :: <list>)
       => res :: <values-ctype>;
     let (x-sign, x-bits) = sign-and-bits-from-type(arg-types.first);
     let (y-sign, y-bits) = sign-and-bits-from-type(arg-types.second);
     if (x-sign == #"off" | y-sign == #"off")
       type-from-sign-and-bits(#"off", min(x-bits, y-bits));
     elseif (x-sign == #"on" & y-sign == #"on")
       type-from-sign-and-bits(#"on", max(x-bits, y-bits));
     else
       type-from-sign-and-bits(#"either", max(x-bits, y-bits));
     end if;
   end method);


define generic sign-and-bits-from-type (ctype :: <ctype>)
    => (sign :: one-of(#"on", #"off", #"either"), bits :: <integer>);

define method sign-and-bits-from-type (ctype :: <ctype>)
    => (sign :: one-of(#"on", #"off", #"either"), bits :: <integer>);
  values(#"either", *current-target*.platform-integer-length - 1);
end method sign-and-bits-from-type;

define method sign-and-bits-from-type (ctype :: <limited-integer-ctype>)
    => (sign :: one-of(#"on", #"off", #"either"), bits :: <integer>);
  sign-and-bits-from-range(ctype.high-bound, ctype.low-bound);
end method sign-and-bits-from-type;

define generic sign-and-bits-from-range
    (high :: false-or(<extended-integer>), low :: false-or(<extended-integer>))
    => (sign :: one-of(#"on", #"off", #"either"), bits :: <integer>);

define method sign-and-bits-from-range
    (high :: <extended-integer>, low :: <extended-integer>)
    => (sign :: one-of(#"on", #"off", #"either"), bits :: <integer>);
  values(if (high < 0)
	   if (low < 0) #"on" else #"either" end if;
	 else
	   if (low < 0) #"either" else #"off" end if;
	 end,
	 max(high.integer-length, low.integer-length));
end method sign-and-bits-from-range;


define method type-from-sign-and-bits
    (sign :: one-of(#"on", #"off", #"either"), bits :: <integer>)
    => res :: <ctype>;
  let low = if (sign == #"off")
	      0;
	    else
	      ash(as(<extended-integer>, -1), bits);
	    end if;
  let high = if (sign == #"on")
	       -1;
	     else
	       lognot(ash(as(<extended-integer>, -1), bits));
	     end if;
  make-canonical-limited-integer(specifier-type(#"<integer>"), low, high);
end method type-from-sign-and-bits;


define-primitive-transformer
  (#"fixnum-logior",
   method (component :: <component>, primitive :: <primitive>) => ();
     let x = primitive.depends-on.source-exp;
     let y = primitive.depends-on.dependent-next.source-exp;
     if ((instance?(x, <literal-constant>) & x.value.literal-value = 0)
	   | (instance?(y, <literal-constant>) & y.value.literal-value = -1))
       replace-expression(component, primitive.dependents, y);
     elseif ((instance?(x, <literal-constant>) & x.value.literal-value = -1)
	       | (instance?(y, <literal-constant>)
		    & y.value.literal-value = 0))
       replace-expression(component, primitive.dependents, x);
     end if;
   end method);

define-primitive-transformer
  (#"fixnum-logxor",
   method (component :: <component>, primitive :: <primitive>) => ();
     let x = primitive.depends-on.source-exp;
     let y = primitive.depends-on.dependent-next.source-exp;
     if (instance?(x, <literal-constant>) & x.value.literal-value = 0)
       replace-expression(component, primitive.dependents, y);
     elseif (instance?(x, <literal-constant>) & x.value.literal-value = -1)
       replace-expression
	 (component, primitive.dependents,
	  make-operation
	    (make-builder(component), <primitive>, list(y),
	     name: #"fixnum-lognot"));
     elseif (instance?(y, <literal-constant>) & y.value.literal-value = 0)
       replace-expression(component, primitive.dependents, x);
     elseif (instance?(y, <literal-constant>) & y.value.literal-value = -1)
       replace-expression
	 (component, primitive.dependents,
	  make-operation
	    (make-builder(component), <primitive>, list(x),
	     name: #"fixnum-lognot"));
     end if;
   end method);

define-primitive-transformer
  (#"fixnum-logand",
   method (component :: <component>, primitive :: <primitive>) => ();
     let x = primitive.depends-on.source-exp;
     let y = primitive.depends-on.dependent-next.source-exp;
     if ((instance?(x, <literal-constant>) & x.value.literal-value = -1)
	   | (instance?(y, <literal-constant>) & y.value.literal-value = 0))
       replace-expression(component, primitive.dependents, y);
     elseif ((instance?(x, <literal-constant>) & x.value.literal-value = 0)
	       | (instance?(y, <literal-constant>)
		    & y.value.literal-value = -1))
       replace-expression(component, primitive.dependents, x);
     end if;
   end method);

// ### need transformers for the dblfix-primitives


// Foreign code support primitives

define-primitive-transformer
  (#"call-out",
   method (component :: <component>, primitive :: <primitive>) => ();
     let func-dep = primitive.depends-on;
     let result-dep = func-dep.dependent-next;
     begin
       let result-type = result-dep.source-exp.dylan-type-for-c-type;
       maybe-restrict-type(component, primitive, result-type.ctype-extent);
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
	     compiler-fatal-error("Type spec with no argument in call-out");
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
     maybe-restrict-type(component, primitive, result-type.ctype-extent);
   end);

define-primitive-transformer
  (#"c-struct-field",
   method (component :: <component>, primitive :: <primitive>) => ();
     let result-dep = primitive.depends-on;
     let result-type = result-dep.source-exp.dylan-type-for-c-type;
     maybe-restrict-type(component, primitive, result-type.ctype-extent);
   end);

define-primitive-transformer
  (#"c-struct-field-setter",
   method (component :: <component>, primitive :: <primitive>) => ();
     let new-dep = primitive.depends-on;
     let type = new-dep.dependent-next.source-exp.dylan-type-for-c-type;
     assert-type(component, primitive.dependents.dependent, new-dep, type);
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
        #"long-long" => specifier-type(#"<double-integer>");
	#"ptr" => specifier-type(#"<raw-pointer>");
	#"float" => specifier-type(#"<single-float>");
	#"double" => specifier-type(#"<double-float>");
	#"long-double" => specifier-type(#"<extended-float>");
	#"object" => object-ctype();
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
     if (csubtype?(arg.derived-type, boolean-ctype()))
       replace-expression(component, primitive.dependents, arg);
     elseif (~ctypes-intersect?(arg-type, false-type))
       replace-expression(component, primitive.dependents,
			  make-literal-constant(make-builder(component), #t));
     elseif (instance?(arg, <ssa-variable>))
       let arg-source = arg.definer.depends-on.source-exp;
       if (instance?(arg-source, <primitive>)
	     & arg-source.primitive-name == #"make-next-method")
	 replace-expression
	   (component, primitive.dependents,
	    make-operation
	      (make-builder(component), <primitive>,
	       list(expand-next-method-if-ref
		      (component, primitive, arg-source)),
	       name: #"not"));
       end if;
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
			  make-literal-constant(make-builder(component), #t));
     elseif (~ctypes-intersect?(arg-type, false-type))
       replace-expression(component, primitive.dependents,
			  make-literal-constant(make-builder(component), #f));
     elseif (instance?(arg, <ssa-variable>))
       let arg-source = arg.definer.depends-on.source-exp;
       if (instance?(arg-source, <primitive>))
	 select (arg-source.primitive-name)
	   #"not" =>
	     let source-source = arg-source.depends-on.source-exp;
	     let op = make-operation(make-builder(component), <primitive>,
				     list(source-source), name: #"as-boolean");
	     replace-expression(component, primitive.dependents, op);
	   #"make-next-method" =>
	     replace-expression
	       (component, primitive.dependents,
		expand-next-method-if-ref
		  (component, primitive, arg-source));
	   otherwise =>
	     begin end;
	 end select;
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
			  make-literal-constant(make-builder(component), x));
     end;
   end);

define method ct-initialized?
    (expr :: <expression>) => res :: singleton(#t);
  #t;
end;

define method ct-initialized?
    (expr :: <primitive>) => res :: one-of(#t, #"can't tell");
  if (expr.primitive-name == #"ref-slot")
    #"can't tell";
  else
    #t;
  end if;
end;

define method ct-initialized?
    (expr :: <prologue>) => res :: singleton(#"can't tell");
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
    (expr :: <uninitialized-value>) => res :: singleton(#f);
  #f;
end;

define method ct-initialized?
    (expr :: <abstract-variable>) => res :: singleton(#"can't tell");
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
			  make-literal-constant(make-builder(component), #f));
     elseif (instance?(x-type, <limited-integer-ctype>)
	       & x-type == y-type
	       & x-type.low-bound = x-type.high-bound)
       replace-expression(component, primitive.dependents,
			  make-literal-constant(make-builder(component), #t));
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
       if (x-type.high-bound & y-type.low-bound
	   & x-type.high-bound < y-type.low-bound)
	 replace-expression(component, primitive.dependents,
			    make-literal-constant(make-builder(component), #t));
       elseif (x-type.low-bound & y-type.high-bound
	       & x-type.low-bound >= y-type.high-bound)
	 replace-expression(component, primitive.dependents,
			    make-literal-constant(make-builder(component), #f));
       end;
     end;
   end);


// raw pointer

define-primitive-transformer
  (#"pointer-deref",
   method (component :: <component>, primitive :: <primitive>) => ();
     let result-dep = primitive.depends-on;
     let result-type = result-dep.source-exp.dylan-type-for-c-type;
     maybe-restrict-type(component, primitive, result-type.ctype-extent);
   end);

define-primitive-transformer
  (#"pointer-deref-setter",
   method (component :: <component>, primitive :: <primitive>) => ();
     let new-dep = primitive.depends-on;
     let type = new-dep.dependent-next.source-exp.dylan-type-for-c-type;
     assert-type(component, primitive.dependents.dependent, new-dep, type);
   end);
