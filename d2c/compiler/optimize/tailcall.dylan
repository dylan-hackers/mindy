module: cheese
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

// Tail call identification.

define function identify-tail-calls (component :: <component>) => ();
  for (function in component.all-function-regions)
    for (return = function.exits then return.next-exit,
	 while: return)
      let results = return.depends-on;
      block (next-return)
	for (dep = results then dep.dependent-next,
	     while: dep)
	  unless (instance?(dep.source-exp, <abstract-variable>))
	    next-return();
	  end;
	end;
	identify-tail-calls-before(component, function, results,
				   return.parent, return);
      end;
    end;
  end;
end;


define generic identify-tail-calls-before
    (component :: <component>, home :: <fer-function-region>,
     results :: false-or(<dependency>), in :: <region>, before :: <region>)
    => ();

define method identify-tail-calls-before
    (component :: <component>, home :: <fer-function-region>,
     results :: false-or(<dependency>), in :: <region>,
     before :: <region>)
    => ();
  identify-tail-calls-before(component, home, results, in.parent, in);
end;

define method identify-tail-calls-before
    (component :: <component>, home :: <fer-function-region>,
     results :: false-or(<dependency>), in :: <compound-region>,
     before :: <region>)
    => ();
  block (return)
    for (subregion in in.regions,
	 prev = #f then subregion)
      if (subregion == before)
	if (prev)
	  identify-tail-calls-in(component, home, results, prev);
	else
          identify-tail-calls-before(component, home, results, in.parent, in); // ### GGR: next-method? FIXME
	end;
	return();
      end;
    end;
  end;
end;

define method identify-tail-calls-before
    (component :: <component>, home :: <fer-function-region>,
     results :: false-or(<dependency>), in :: <if-region>,
     before :: <region>)
    => ();
end;

define method identify-tail-calls-before
    (component :: <component>, home :: <fer-function-region>,
     results :: false-or(<dependency>), in :: <function-region>,
     before :: <region>)
    => ();
end;

define generic identify-tail-calls-in
    (component :: <component>, home :: <fer-function-region>,
     results :: false-or(<dependency>), region :: <region>)
    => ();
  
define method identify-tail-calls-in
    (component :: <component>, home :: <fer-function-region>,
     results :: false-or(<dependency>), region :: <simple-region>)
    => ();
  block (return)
    for (assign = region.last-assign then assign.prev-op,
	 while: assign)
      for (result = results then result.dependent-next,
	   defn = assign.defines then defn.definer-next,
	   while: result | defn)
	if (~result | ~defn | defn.needs-type-check?
	      | ~definition-for?(defn, result.source-exp))
	  return();
	end;
      end;
      let expr = assign.depends-on.source-exp;
      if (instance?(expr, <known-call>))
	let func = expr.depends-on.source-exp;
	if (instance?(func, <function-literal>) & func.main-entry == home)
	  assert(home.literal == func);
	  // It's a self tail call.
	  convert-self-tail-call(component, home, expr);
	elseif (home.literal == func) // this can be removed after a while FIXME
	  // ... or better, make this the primary check.
	  error("home.literal == func, but instance?(func, <function-literal>) & func.main-entry == home failed?");
	elseif (instance?(func, <definition-constant-leaf>))
	  let const-defn :: <abstract-method-definition> = func.const-defn;
	  let ctv = const-defn.ct-value;
	  // now, the ctv is a <ct-function>, but that does not reference the <function-literal>
	  // so we have to go the other way.
	  let lit = home.literal;
	  let lit-ctv = lit.ct-function;
	  if (ctv == lit-ctv & ctv)
	    // It's a self tail call.
	    convert-self-tail-call(component, home, expr);
	  end if;
	elseif (instance?(func, <literal-constant>))
	  let ctv = func.value;
	  let lit = home.literal;
	  if (lit)
	    let lit-ctv = lit.ct-function;
	    if (ctv == lit-ctv & ctv)
	      // It's a self tail call.
	      compiler-warning("DID IT"); // does not seem to occur... FIXME
	      convert-self-tail-call(component, home, expr);
	    end if;
	  end if;
	end;
	return();
      end;
      if (assign.defines
	    & instance?(assign.defines.var-info, <values-cluster-info>))
	// Want to return a cluster.
	if (instance?(expr, <abstract-variable>)
	      & instance?(expr.var-info, <values-cluster-info>))
	  results := assign.depends-on;
	else
	  return();
	end;
      else
	// Want to return a specific number of values.
	if (instance?(expr, <abstract-variable>))
	  if (assign.defines & assign.defines.definer-next == #f
		& ~instance?(expr.var-info, <values-cluster-info>))
	    results := assign.depends-on;
	  else
	    return();
	  end;
	elseif (instance?(expr, <primitive>)
		  & expr.primitive-name == #"values")
	  for (defn = assign.defines then defn.definer-next,
	       dep = expr.depends-on then dep.dependent-next,
	       while: defn & dep)
	  finally
	    if (defn | dep)
	      return();
	    else
	      results := expr.depends-on;
	    end;
	  end;
	else
	  return();
	end;
      end;
    finally
      identify-tail-calls-before(component, home, results,
				 region.parent, region);
    end;
  end;
end;

define method identify-tail-calls-in
    (component :: <component>, home :: <fer-function-region>,
     results :: false-or(<dependency>), region :: <compound-region>)
    => ();
  identify-tail-calls-in(component, home, results, region.regions.last);
end;

define method identify-tail-calls-in
    (component :: <component>, home :: <fer-function-region>,
     results :: false-or(<dependency>), region :: <empty-region>)
    => ();
  identify-tail-calls-before(component, home, results, region.parent, region);
end;

define method identify-tail-calls-in
    (component :: <component>, home :: <fer-function-region>,
     results :: false-or(<dependency>), region :: <if-region>)
    => ();
  identify-tail-calls-in(component, home, results, region.then-region);
  identify-tail-calls-in(component, home, results, region.else-region);
end;
  
define method identify-tail-calls-in
    (component :: <component>, home :: <fer-function-region>,
     results :: false-or(<dependency>), region :: <loop-region>)
    => ();
end;

define method identify-tail-calls-in
    (component :: <component>, home :: <fer-function-region>,
     results :: false-or(<dependency>), region :: <block-region>)
    => ();
  for (exit = region.exits then exit.next-exit,
       while: exit)
    if (home-function-region(exit) == home)
      identify-tail-calls-before(component, home, results,
				 exit.parent, exit);
    end;
  end;
end;

define method identify-tail-calls-in
    (component :: <component>, home :: <fer-function-region>,
     results :: false-or(<dependency>), region :: <unwind-protect-region>)
    => ();
end;

define method identify-tail-calls-in
    (component :: <component>, home :: <fer-function-region>,
     results :: false-or(<dependency>), region :: <exit>)
    => ();
end;


define generic definition-for?
    (defn :: <definition-site-variable>, var :: <abstract-variable>)
    => res :: <boolean>;

define method definition-for?
    (defn :: <ssa-variable>, var :: <abstract-variable>)
    => res :: <boolean>;
  defn == var;
end;

define method definition-for?
    (defn :: <initial-definition>, var :: <abstract-variable>)
    => res :: <boolean>;
  defn.definition-of == var;
end;

define function defined-variables (assignment :: <assignment>)
  for (var = assignment.defines then var.definer-next,
       arg-vars = #() then 
         pair(var, arg-vars),
       while: var)
  finally
    reverse!(arg-vars)
  end for;
end function;

define function operation-arguments(op :: <operation>)
  for (dep = op.depends-on then dep.dependent-next,
       args = #() then pair(dep.source-exp, args),
       while: dep)
  finally
    reverse!(args)
  end for;
end function operation-arguments;

define function copy-defined-variables(builder, assignment :: <assignment>)
  => (vars :: <list>);
  if(assignment.defined-variables)
    map-as(<list>, 
           method(x) make-local-var(builder, x.var-info.debug-name, object-ctype()) end,
           assignment.defined-variables);
  else
    #();
  end if;
end;


define method convert-self-tail-call
    (component :: <component>, func :: <fer-function-region>,
     call :: <abstract-call>)
    => ();

  // Set up the wrapper loop and blocks.
  unless (func.self-call-block)
    let builder = make-builder(component);
    let source = func.source-location;

    // make temps for the function arguments
    let prologue-assignment = func.prologue.dependents.dependent;
    let policy = prologue-assignment.policy;
    let new-vars = copy-defined-variables(builder, prologue-assignment);
    func.self-tail-call-temps := new-vars;

    let old-vars = defined-variables(prologue-assignment);
    let old-assign-dep = func.prologue.dependents;

    // assign result of PROLOGUE primitive to temps
    build-assignment(builder, policy, source,
                     new-vars,
                     func.prologue);

    // in the old prologue assignment, assign from the arg temps instead
    let op = make-operation(builder, <primitive>, new-vars, name: #"values");
    replace-expression(component, old-assign-dep, op);

    
    reoptimize(component, op);
    reoptimize(component, old-assign-dep.dependent);
    reoptimize(component, func.prologue.dependents.dependent);

    build-loop-body(builder, $Default-Policy, source);
    func.self-call-block := build-block-body(builder, $Default-Policy, source);
    func.self-tail-call-temps := new-vars;
    build-region(builder, func.body);
    end-body(builder); // end of the self call block
    end-body(builder); // end of the loop
    replace-subregion(component, func, func.body, builder-result(builder));
  end;

  // replace call to self by assignment to argument temps
  let builder = make-builder(component);
  let assign = call.dependents.dependent;
  let op = make-operation(builder, <primitive>, 
                          operation-arguments(call).tail, name: #"values");
  build-assignment(builder, assign.policy, assign.source-location,
                   func.self-tail-call-temps, op);
  insert-before(component, assign, builder-result(builder));
  insert-exit-after(component, assign, func.self-call-block);
  delete-and-unlink-assignment(component, assign);
  reoptimize(component, op);
  reoptimize(component, op.dependents.dependent);
end;
