module: cheese
copyright: see below

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998 - 2003  Gwydion Dylan Maintainers
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

// Optimistic type inference.
// 

define method optimistic-type-inference (component :: <component>) => ();
  //
  // Make an initial guess for each type and queue everything that might
  // be improved.
  for (function in component.all-function-regions)
    make-initial-guess-region(component, function);
  end for;
  reverse-queue(component, #f);
  //
  // Drain the queue.
  while (component.reoptimize-queue)
    let queueable = component.reoptimize-queue;
    component.reoptimize-queue := queueable.queue-next;
    queueable.queue-next := #"absent";
    infer-type(component, queueable);
  end while;
  //
  // Now that the types have converged, record the guesses.
  for (function in component.all-function-regions)
    record-guessed-type-region(component, function);
  end for;
  reverse-queue(component, #f);
end method optimistic-type-inference;



define generic make-initial-guess-region
    (component :: <component>, region :: <region>) => ();

define method make-initial-guess-region
    (component :: <component>, region :: <simple-region>) => ();
  for (assign = region.first-assign then assign.next-op,
       while: assign)
    make-initial-guess-expression(component, assign.depends-on.source-exp);
    for (def = assign.defines then def.definer-next,
	 while: def)
      def.guessed-type := empty-ctype();
      if (instance?(def, <initial-definition>))
	let var = def.definition-of;
	var.guessed-type := empty-ctype();
      end if;
    end for;
    reoptimize(component, assign);
  end for;
end method make-initial-guess-region;

define method make-initial-guess-region
    (component :: <component>, region :: <compound-region>) => ();
  for (subregion in region.regions)
    make-initial-guess-region(component, subregion);
  end for;
end method make-initial-guess-region;

define method make-initial-guess-region
    (component :: <component>, region :: <if-region>) => ();
  make-initial-guess-region(component, region.then-region);
  make-initial-guess-region(component, region.else-region);
end method make-initial-guess-region;

define method make-initial-guess-region
    (component :: <component>, region :: <body-region>) => ();
  make-initial-guess-region(component, region.body);
end method make-initial-guess-region;

define method make-initial-guess-region
    (component :: <component>, region :: <fer-function-region>) => ();
  make-initial-guess-region(component, region.body);
  if (region.hidden-references?)
    region.guessed-result-type := region.result-type;
  else
    region.guessed-result-type := empty-ctype();
    reoptimize(component, region);
  end if;
end method make-initial-guess-region;

define method make-initial-guess-region
    (component :: <component>, region :: <exit>) => ();
  // Do nothing.
end method make-initial-guess-region;

define method make-initial-guess-region
    (component :: <component>, region :: <return>) => ();
  make-initial-guess-dependent(component, region);
  region.guessed-returned-type := empty-ctype();
  reoptimize(component, region);
end method make-initial-guess-region;


define generic make-initial-guess-expression
    (component :: <component>, expr :: <expression>) => ();

define method make-initial-guess-expression
    (component :: <component>, expr :: <expression>) => ();
  expr.guessed-type := expr.derived-type;
end method make-initial-guess-expression;

define method make-initial-guess-expression
    (component :: <component>, expr :: <abstract-variable>) => ();
end method make-initial-guess-expression;

define method make-initial-guess-expression
    (component :: <component>, call :: <known-call>) => ();
  make-initial-guess-dependent(component, call);
  call.guessed-type := empty-ctype();
  reoptimize(component, call);
end method make-initial-guess-expression;

define method make-initial-guess-expression
    (component :: <component>, call :: <unknown-call>) => ();
  make-initial-guess-dependent(component, call);
  call.guessed-type := empty-ctype();
  reoptimize(component, call);
end method make-initial-guess-expression;

define method make-initial-guess-expression
    (component :: <component>, primitive :: <primitive>) => ();
  if (primitive.primitive-info.priminfo-type-deriver)
    make-initial-guess-dependent(component, primitive);
    primitive.guessed-type := empty-ctype();
    reoptimize(component, primitive);
  else
    primitive.guessed-type := primitive.derived-type;
  end if;
end method make-initial-guess-expression;


define method make-initial-guess-dependent
    (component :: <component>, dependent :: <dependent-mixin>) => ();
  for (dep = dependent.depends-on then dep.dependent-next,
       while: dep)
    make-initial-guess-expression(component, dep.source-exp);
  end for;
end method make-initial-guess-dependent;



define generic fix-type-guess
    (component :: <component>, expr :: <expression>, guess :: <values-ctype>)
    => ();

define method fix-type-guess
    (component :: <component>, expr :: <expression>, guess :: <values-ctype>)
    => ();
  let old-guess = expr.guessed-type;
  unless (values-subtype?(guess, old-guess))
    assert(old-guess == empty-ctype() | values-subtype?(old-guess, guess));
    expr.guessed-type := guess;
    queue-dependents(component, expr);
  end unless;
end method fix-type-guess;

define method fix-type-guess
    (component :: <component>, var :: <initial-definition>,
     guess :: <values-ctype>)
    => ();
  let old-guess = var.guessed-type;
  unless (values-subtype?(guess, old-guess))
    assert(old-guess == empty-ctype() | values-subtype?(old-guess, guess));
    var.guessed-type := guess;
    let defn-of = var.definition-of;
    for (defn in defn-of.definitions,
	 type = empty-ctype() then values-type-union(type, defn.guessed-type))
    finally
      fix-type-guess(component, defn-of, type);
    end for;
  end unless;
end method fix-type-guess;



define generic infer-type
    (component :: <component>, queueable :: <queueable-mixin>) => ();

define method infer-type
    (component :: <component>, queueable :: <queueable-mixin>) => ();
end method infer-type;


define method infer-type
    (component :: <component>, assign :: <assignment>) => ();
  let source-type = assign.depends-on.source-exp.guessed-type;
  unless (source-type == empty-ctype())
    let defines = assign.defines;
    if (defines & instance?(defines.var-info, <values-cluster-info>))
      fix-type-guess(component, defines, source-type);
    else
      for (var = defines then var.definer-next,
	   index from 0,
	   while: var)
	fix-type-guess(component, var, defaulted-type(source-type, index));
      end for;
    end if;
  end unless;
end method infer-type;


define method infer-type
    (component :: <component>, region :: <fer-function-region>) => ();
  unless (region.hidden-references?)
    for (return = region.exits then return.next-exit,
	 guess = empty-ctype()
	   then values-type-union(guess, return.guessed-returned-type),
	 while: return)
    finally
      let old-guess = region.guessed-result-type;
      unless (values-subtype?(guess, old-guess))
	region.guessed-result-type := guess;
	if (region.literal)
	  queue-dependents(component, region.literal);
	end if;
      end unless;
    end for;
  end unless;
end method infer-type;


define method infer-type
    (component :: <component>, region :: <return>) => ();
  let results = region.depends-on;
  let cluster?
    = (results & instance?(results.source-exp, <abstract-variable>)
	 & instance?(results.source-exp.var-info, <values-cluster-info>));
  let guess
    = if (cluster?)
	results.source-exp.guessed-type;
      else
	let types = make(<stretchy-vector>);
	for (dep = results then dep.dependent-next,
	     while: dep)
	  add!(types, dep.source-exp.guessed-type);
	end;
	make-values-ctype(as(<list>, types), #f);
      end;
  let old-guess = region.guessed-returned-type;
  unless (values-subtype?(guess, old-guess))
    region.guessed-returned-type := guess;
    reoptimize(component, region.block-of);
  end;
end method infer-type;


define method infer-type
    (component :: <component>, call :: <known-call>) => ();
  fix-type-guess(component, call,
		 function-result-type(call.depends-on.source-exp));
end method infer-type;



define method infer-type
    (component :: <component>, prim :: <primitive>) => ();
  let type-deriver = prim.primitive-info.priminfo-type-deriver;
  if (type-deriver)
    let arg-types = map(guessed-type, listify-dependencies(prim.depends-on));
    fix-type-guess
      (component, prim, type-deriver(prim, arg-types).ctype-extent);
  end if;
end method infer-type;



define generic function-result-type
    (leaf :: <expression>) => res :: <values-ctype>;

define method function-result-type
    (leaf :: <expression>) => res :: <values-ctype>;
  wild-ctype();
end method function-result-type;

define method function-result-type
    (leaf :: <function-literal>) => res :: <values-ctype>;
  leaf.main-entry.guessed-result-type;
end method function-result-type;

define method function-result-type
    (leaf :: <exit-function>) => res :: <values-ctype>;
  empty-ctype();
end method function-result-type;

define method function-result-type
    (leaf :: <literal-constant>) => res :: <values-ctype>;
  let ctv = leaf.value;
  if (instance?(ctv, <ct-function>))
    ctv.ct-function-signature.returns.ctype-extent;
  else
    wild-ctype();
  end if;
end method function-result-type;

define method function-result-type
    (leaf :: <definition-constant-leaf>) => res :: <values-ctype>;
  let defn = leaf.const-defn;
  if (instance?(defn, <function-definition>))
    defn.function-defn-signature.returns.ctype-extent;
  else
    wild-ctype();
  end if;
end method function-result-type;


define method infer-type
    (component :: <component>, call :: <unknown-call>) => ();
  infer-unknown-call-type-leaf(component, call, call.depends-on.source-exp);
end method infer-type;
  

define generic infer-unknown-call-type-leaf
    (component :: <component>, call :: <unknown-call>, leaf :: <leaf>) => ();

define method infer-unknown-call-type-leaf
    (component :: <component>, call :: <unknown-call>, leaf :: <leaf>) => ();
  fix-type-guess(component, call, function-result-type(leaf));
end method infer-unknown-call-type-leaf;

define method infer-unknown-call-type-leaf
    (component :: <component>, call :: <unknown-call>,
     leaf :: <literal-constant>)
    => ();
  let ctv = leaf.value;
  if (instance?(ctv, <ct-function>))
    let defn = ctv.ct-function-definition;
    if (defn)
      infer-unknown-call-type-defn(component, call, defn);
    else
      fix-type-guess
	(component, call, ctv.ct-function-signature.returns.ctype-extent);
    end if;
  else
    fix-type-guess(component, call, wild-ctype());
  end if;
end method infer-unknown-call-type-leaf;

define method infer-unknown-call-type-leaf
    (component :: <component>, call :: <unknown-call>,
     leaf :: <definition-constant-leaf>)
    => ();
  infer-unknown-call-type-defn(component, call, leaf.const-defn);
end method infer-unknown-call-type-leaf;


define generic infer-unknown-call-type-defn
    (component :: <component>, call :: <unknown-call>, defn :: <definition>)
    => ();

define method infer-unknown-call-type-defn
    (component :: <component>, call :: <unknown-call>, defn :: <definition>)
    => ();
  fix-type-guess(component, call, wild-ctype());
end method infer-unknown-call-type-defn;

define method infer-unknown-call-type-defn
    (component :: <component>, call :: <unknown-call>,
     defn :: <function-definition>)
    => ();
  fix-type-guess
    (component, call, defn.function-defn-signature.returns.ctype-extent);
end method infer-unknown-call-type-defn;

define method infer-unknown-call-type-defn
    (component :: <component>, call :: <unknown-call>,
     defn :: <generic-definition>)
    => ();
  block (return)
    let sig = defn.function-defn-signature;
    let arg-types = #();
    for (dep = call.depends-on.dependent-next then dep.dependent-next,
	 specializer in sig.specializers)
      unless (dep)
	error("Bogus call to a generic was left as an <unknown-call>?");
      end unless;
      let arg-type = dep.source-exp.guessed-type;
      if (arg-type == empty-ctype())
	return();
      end if;
      arg-types := pair(arg-type, arg-types);
    end for;
    arg-types := reverse!(arg-types);

    let (definitely, maybe) = ct-applicable-methods(defn, arg-types);
    if (definitely == #f)
      fix-type-guess(component, call, sig.returns.ctype-extent);
      return();
    end if;

    let applicable = concatenate(definitely, maybe);

    if (applicable == #())
      return();
    end if;

    local
      method could-be-any () => ();
	for (meth in applicable,
	     result-type = empty-ctype()
	       then values-type-union(result-type,
				      meth.function-defn-signature.returns))
	finally
	  fix-type-guess(component, call, result-type.ctype-extent);
	end for;
	return();
      end method could-be-any;

    unless (maybe == #() | (maybe.tail == #() & definitely == #()))
      // We can't tell what methods actually are applicable.
      could-be-any();
    end unless;

    // Sort the applicable methods.
    let (ordered, ambiguous) = sort-methods(applicable, #f);

    if (ordered == #f)
      // We can't tell jack about how to order the methods.
      could-be-any();
    end if;

    unless (ordered == #())
      fix-type-guess
	(component, call,
	 ordered.first.function-defn-signature.returns.ctype-extent);
    end unless;
  end block;
end method infer-unknown-call-type-defn;




define generic record-guessed-type-region
    (component :: <component>, region :: <region>) => ();

define method record-guessed-type-region
    (component :: <component>, region :: <simple-region>) => ();
  for (assign = region.first-assign then assign.next-op,
       while: assign)
    let expr = assign.depends-on.source-exp;
    let guess = expr.guessed-type;
    unless (values-subtype?(expr.derived-type, guess))
      maybe-restrict-type(component, expr, guess);
    end unless;
  end for;
end method record-guessed-type-region;

define method record-guessed-type-region
    (component :: <component>, region :: <compound-region>) => ();
  for (subregion in region.regions)
    record-guessed-type-region(component, subregion);
  end for;
end method record-guessed-type-region;

define method record-guessed-type-region
    (component :: <component>, region :: <if-region>) => ();
  record-guessed-type-region(component, region.then-region);
  record-guessed-type-region(component, region.else-region);
end method record-guessed-type-region;

define method record-guessed-type-region
    (component :: <component>, region :: <body-region>) => ();
  record-guessed-type-region(component, region.body);
end method record-guessed-type-region;

define method record-guessed-type-region
    (component :: <component>, region :: <fer-function-region>) => ();
  record-guessed-type-region(component, region.body);
  unless (region.hidden-references?
	    | values-subtype?(region.result-type, region.guessed-result-type))
    region.result-type := region.guessed-result-type;
    let lit = region.literal;
    if (lit)
      queue-dependents(component, lit);
    end if;
  end unless;
end method record-guessed-type-region;

define method record-guessed-type-region
    (component :: <component>, region :: <exit>) => ();
  // Do nothing.
end method record-guessed-type-region;

