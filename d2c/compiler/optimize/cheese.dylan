module: cheese
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/optimize/cheese.dylan,v 1.112 1996/01/04 17:46:53 ram Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.


define variable *do-sanity-checks* = #f;
define method enable-sanity-checks () => (); *do-sanity-checks* := #t; end;
define method disable-sanity-checks () => (); *do-sanity-checks* := #f; end;

define variable *print-shit* = #f;
define method print-shit () => (); *print-shit* := #t; end;
define method dont-print-shit () => (); *print-shit* := #f; end;

define variable *optimize-ncalls* = 0;

define method optimize-component
    (component :: <component>, #key simplify-only) => ();
  reverse-queue(component, #f);
  let done = #f;
  until (done)
    if (*do-sanity-checks*)
      check-sanity(component);
    end;
    if (*print-shit*) dump-fer(component) end;
    if (component.initial-variables)
      if (*print-shit*)
	format(*debug-output*, "\n******** doing trivial ssa conversion\n\n");
      end;
      while (component.initial-variables)
	let init-var = component.initial-variables;
	component.initial-variables := init-var.next-initial-variable;
	init-var.next-initial-variable := #f;
	maybe-convert-to-ssa(component, init-var);
      end;
      if (*print-shit*) dump-fer(component) end;
    end;
    if (component.reoptimize-queue)
      let queueable = component.reoptimize-queue;
      component.reoptimize-queue := queueable.queue-next;
      queueable.queue-next := #"absent";
      if (*print-shit*)
	format(*debug-output*, "\n******** about to optimize %=\n\n",
	       queueable);
      end;
      optimize(component, queueable);
      *optimize-ncalls* := *optimize-ncalls* + 1;
    else
      local method try (function, what)
	      if (what & *print-shit*)
		format(*debug-output*, "\n******** %s\n\n", what);
	      end;
	      function(component);
	      let start-over?
		= component.initial-variables | component.reoptimize-queue;
	      if (start-over? & *print-shit*)
		format(*debug-output*, "\nstarting over...\n");
	      end;
	      start-over?;
	    end;
      (*do-sanity-checks* & try(assure-all-done, #f))
	| try(identify-tail-calls, "finding tail calls")
	| try(cleanup-control-flow, "cleaning up control flow")
	| (simplify-only & (done := #t))
	| try(add-type-checks, "adding type checks")
	| try(replace-placeholders, "replacing placeholders")
	| try(environment-analysis, "running environment analysis")
	| try(build-external-entries, "building external entries")
	| (done := #t);
    end;
  end;
end;


define method reverse-queue
    (component :: <component>, before :: false-or(<queueable-mixin>)) => ();
  if (before)
    unless (before.queue-next == #f
	      | instance?(before.queue-next, <queueable-mixin>))
      error("Can't reverse the part of the queue before %= because it "
	      "isn't in the queue.",
	    before);
    end;
  end;
  let temp = #f;
  for (remaining = component.reoptimize-queue then temp,
       result = before then remaining,
       until: remaining == before)
    temp := remaining.queue-next;
    remaining.queue-next := result;
  finally
    component.reoptimize-queue := result;
  end;
end;



// SSA conversion.

define method maybe-convert-to-ssa
    (component :: <component>, var :: <initial-variable>) => ();
  let defns = var.definitions;
  if (defns ~== #() & defns.tail == #())
    // Single definition -- replace it with an ssa variable.
    let defn = defns.head;
    let assign = defn.definer;
    if (assign)
      let ssa = make(<ssa-variable>,
		     dependents: var.dependents,
		     derived-type: var.derived-type,
		     var-info: var.var-info,
		     definer: assign,
		     definer-next: defn.definer-next,
		     needs-type-check: defn.needs-type-check?);
      // Replace the <initial-definition> with the <ssa-var> in the assignment
      // defines.
      for (other = assign.defines then other.definer-next,
	   prev = #f then other,
	   until: other == defn)
      finally
	if (prev)
	  prev.definer-next := ssa;
	else
	  assign.defines := ssa;
	end;
      end;
      defn.definer := #f;
      // Replace each reference of the <initial-var> with the <ssa-var>.
      for (dep = var.dependents then dep.source-next,
	   while: dep)
	unless (dep.source-exp == var)
	  error("The dependent's source-exp wasn't the var we were trying "
		  "to replace?");
	end;
	dep.source-exp := ssa;
	// Reoptimize the dependent in case they can do something now that
	// they are being given an ssa variable.
	reoptimize(component, dep.dependent);
      end;
      // Reoptimize the defining assignment in case it can now be
      // copy-propagated.
      reoptimize(component, assign);
    end;
  end;
end method maybe-convert-to-ssa;


// Optimizations.

define generic optimize
    (component :: <component>, dependent :: <queueable-mixin>)
    => ();

define method optimize
    (component :: <component>, dependent :: <queueable-mixin>) => ();
  // By default, do nothing.
end;

define method reoptimize
    (component :: <component>, dependent :: <queueable-mixin>) => ();
  if (dependent.queue-next == #"absent")
    add-to-queue(component, dependent);
  end;
end;

define method queue-dependents
    (component :: <component>, expr :: <expression>) => ();
  for (dependency = expr.dependents then dependency.source-next,
       while: dependency)
    reoptimize(component, dependency.dependent);
  end;
end;

define method delete-queueable
    (component :: <component>, queueable :: <queueable-mixin>) => ();
  //
  // If we are queued for reoptimization, belay that.
  unless (queueable.queue-next == #"absent")
    for (q = component.reoptimize-queue then q.queue-next,
	 prev = #f then q,
	 until: q == queueable)
    finally
      if (prev)
	prev.queue-next := q.queue-next;
      else
	component.reoptimize-queue := q.queue-next;
      end;
    end;
  end;
  queueable.queue-next := #"deleted";
end;


// Assignment optimization.

define method expression-flushable? (expr :: <expression>) => res :: <boolean>;
  #f;
end;

define method expression-flushable? (expr :: <primitive>) => res :: <boolean>;
  expr.info.primitive-side-effect-free?;
end;

define method expression-flushable? (expr :: <known-call>) => res :: <boolean>;
  let func = expr.depends-on.source-exp;
  if (instance?(func, <definition-constant-leaf>))
    let defn = func.const-defn;
    instance?(defn, <function-definition>)
      & defn.function-defn-flushable?;
  end;
end;

define method expression-flushable? (expr :: <slot-ref>) => res :: <boolean>;
  #t;
end;

define method expression-flushable? (expr :: <truly-the>) => res :: <boolean>;
  #t;
end;

define method expression-flushable? (var :: <leaf>) => res :: <boolean>;
  #t;
end;



define method expression-movable? (expr :: <expression>)
    => res :: <boolean>;
  #f;
end;

define method expression-movable? (expr :: <primitive>)
    => res :: <boolean>;
  expr.info.primitive-pure?;
end;

define method expression-movable? (expr :: <known-call>)
    => res :: <boolean>;
  function-movable?(expr.depends-on.source-exp);
end;

define method function-movable? (leaf :: <leaf>) => res :: <boolean>;
  #f;
end;

define method function-movable?
    (leaf :: <definition-constant-leaf>) => res :: <boolean>;
  function-movable?(leaf.const-defn);
end;

define method function-movable?
    (leaf :: <literal-constant>) => res :: <boolean>;
  let defn = leaf.value.ct-function-definition;
  defn & function-movable?(defn);
end;

define method function-movable?
    (defn :: <definition>) => res :: <boolean>;
  #f;
end;

define method function-movable?
    (defn :: <function-definition>) => res :: <boolean>;
  defn.function-defn-movable?;
end;

define method expression-movable? (var :: <leaf>)
    => res :: <boolean>;
  #t;
end;

define method expression-movable? (var :: <abstract-variable>)
    => res :: <boolean>;
  ~instance?(var.var-info, <values-cluster-info>);
end;

define method expression-movable? (var :: <initial-variable>)
    => res :: <boolean>;
  #f;
end;


define method trim-unneeded-defines
    (component :: <component>, assignment :: <assignment>) => ();
  local
    method unneeded? (defines)
      if (defines)
	if (unneeded?(defines.definer-next))
	  defines.definer-next := #f;
	  if (define-unneeded?(defines))
	    delete-definition(component, defines);
	    #t;
	  else
	    #f;
	  end;
	else
	  #f;
	end;
      else
	#t;
      end;
    end;
  if (unneeded?(assignment.defines))
    assignment.defines := #f;
  end;
end;

define method define-unneeded? (defn :: <ssa-variable>)
  ~(defn.dependents | defn.needs-type-check?);
end;

define method define-unneeded? (defn :: <initial-definition>)
    => res :: <boolean>;
  ~(defn.definition-of.dependents | defn.needs-type-check?);
end;


define method optimize
    (component :: <component>, assignment :: <assignment>) => ();
  let dependency = assignment.depends-on;
  let source = dependency.source-exp;
  let source-type = source.derived-type;
  trim-unneeded-defines(component, assignment);
  let defines = assignment.defines;
  
  if (source-type == empty-ctype())
    //
    // The source never returns.  Insert an exit to the component and nuke
    // the variables we would have otherwise defined.
    insert-exit-after(component, assignment, component);
    for (defn = defines then defn.definer-next,
	 while: defn)
      delete-definition(component, defn);
    end;
    assignment.defines := #f;

  elseif (defines == #f & expression-flushable?(source))
    //
    // there is no point to this assignment, so nuke it.
    delete-and-unlink-assignment(component, assignment);

  elseif (instance?(source, <abstract-variable>)
	    & instance?(source.var-info, <values-cluster-info>))
    //
    // We are referencing a cluster.
    if (instance?(defines.var-info, <values-cluster-info>))
      // Propagate the type on to the result.
      maybe-restrict-type(component, defines, source-type);
      // We are making a copy of a cluster.  Propagate it if we can.  We can
      // only propagate ssa variables, because we have to preserve the 
      // location of multi-definition variable reads.
      if (instance?(source, <ssa-variable>)
	    & maybe-propagate-copy(component, defines, source))
	// Nuke the assignment, because var is now unused.  We don't want
	// to rely on the regular ununsed assignment deletion code, because
	// we will have two references to source until that stuff runs
	// which will confuse other things in the meantime.
	delete-and-unlink-assignment(component, assignment);
      end;
    else
      // We are extracting some number of values out of a cluster.  Expand
      // the cluster into that number of variables.
      for (nvals from 0,
	   defn = defines then defn.definer-next,
	   names = #() then pair(defn.var-info.debug-name, names),
	   while: defn)
      finally
	let builder = make-builder(component);
	let op = make-operation(builder, <primitive>, list(source),
				name: #"values");
	replace-expression(component, dependency, op);
	expand-cluster(component, source, nvals, names);
      end;
    end;

  elseif (defines & instance?(defines.var-info, <values-cluster-info>))
    //
    // We are defining a cluster.  Propagate the type on though.
    maybe-restrict-type(component, defines, source-type);

  elseif (defines & defines.definer-next == #f & expression-movable?(source)
	    & maybe-propagate-copy(component, defines, source))
    //
    // We are defining a single variable, and we could copy-propagate it
    // out of existance.  So nuke the assignment.
    delete-and-unlink-assignment(component, assignment);

  else
    //
    // We are defining a fixed number of variables.

    // Propagate type information to the defined variables.
    for (var = defines then var.definer-next,
	 index from 0 below source-type.min-values,
	 positionals = source-type.positional-types then positionals.tail,
	 while: var)
      //
      // For the values that are guarenteed to be returned, we have precise
      // type information.
      maybe-restrict-type(component, var, positionals.head);

    finally
      if (var)

	// For the variables that might be defaulted to #f because the value
	// was unsupplied, union in <false>.
	let false-type = dylan-value(#"<false>");
	for (var = var then var.definer-next,
	     positionals = positionals then positionals.tail,
	     until: var == #f | positionals == #())
	  maybe-restrict-type(component, var,
			      ctype-union(positionals.head,
					  false-type));
	finally
	  if (var)

	    let type = source-type.rest-value-type;
	    if (type == empty-ctype())
	      //
	      // We know we will be defaulting this variable to #f, so
	      // use <false> as the type and see if we can propagate the #f
	      // to users of the variable.
	      let false = make-literal-constant(make-builder(component),
						make(<literal-false>));
	      for (var = var then var.definer-next,
		   while: var)
		maybe-restrict-type(component, var, false-type);
		maybe-propagate-copy(component, var, false);
	      end;
	    else
	      //
	      // We might get a value, or we might default it to #f.
	      let type-union-false = ctype-union(type, false-type);
	      for (var = var then var.definer-next,
		   while: var)
		maybe-restrict-type(component, var, type-union-false);
	      end;
	    end;
	  end;
	end;
      end;
    end;
  end;
end;

define method maybe-propagate-copy
    (component :: <component>, var :: <ssa-variable>, value :: <leaf>)
    => no-longer-needed? :: <boolean>;
  unless (var.needs-type-check?)
    // Change all references to this variable to be references to value
    // instead.
    while (var.dependents)
      replace-expression(component, var.dependents, value)
    end;
    // Return that we nuked 'em all.
    #t;
  end;
end;

define method maybe-propagate-copy
    (component :: <component>, var :: <ssa-variable>, value :: <ssa-variable>,
     #next next-method)
    => no-longer-needed? :: <boolean>;
  if (instance?(var.var-info, <lexical-var-info>)
	& ~instance?(value.var-info, <lexical-var-info>))
    // We can't just blindly replace references to var with references to value
    // because they might be in a different function, and value can't be
    // closed over.  So we only replace the references that are homed in
    // value.definer.home.
    let home = value.definer.home-function-region;
    let next = #f;
    for (dep = var.dependents then next,
	 while: dep)
      next := dep.source-next;
      if (dep.dependent.home-function-region == home)
	replace-expression(component, dep, value);
      end;
    end;
    var.dependents == #f;
  else
    next-method();
  end;
end;

define method maybe-propagate-copy
    (component :: <component>, var :: <abstract-variable>,
     value :: <expression>)
    => no-longer-needed? :: <boolean>;
  #f;
end;


// Call optimization.

// <unknown-call> optimization.
//
// Basically, we check the arguments against the signature and try to change
// into a <known-call> if we can and an <error-call> if we have to.
//
define method optimize
    (component :: <component>, call :: <unknown-call>) => ();
  let func-dep = call.depends-on;
  unless (func-dep)
    error("No function in a call?");
  end;
  // Dispatch of the thing we are calling.
  optimize-unknown-call(component, call, func-dep.source-exp);
end;


define method optimize-unknown-call
    (component :: <component>, call :: <unknown-call>, func :: <leaf>)
    => ();
  // Assert that the function is a function.
  assert-type(component, call.dependents.dependent, call.depends-on,
	      if (call.use-generic-entry?)
		specifier-type(#"<method>");
	      else
		function-ctype();
	      end);
end;

define method optimize-unknown-call
    (component :: <component>, call :: <unknown-call>,
     func :: <function-literal>)
    => ();
  maybe-restrict-type(component, call, func.main-entry.result-type);
  maybe-change-to-known-or-error-call(component, call, func.signature,
				      func.main-entry.name);
end;

define method optimize-unknown-call
    (component :: <component>, call :: <unknown-call>,
     func :: <definition-constant-leaf>)
    => ();
  optimize-unknown-call(component, call, func.const-defn);
end;

define method optimize-unknown-call
    (component :: <component>, call :: <unknown-call>,
     func :: <literal-constant>)
    => ();
  let value = func.value;
  if (instance?(value, <ct-function>))
    let defn = value.ct-function-definition;
    if (defn)
      optimize-unknown-call(component, call, defn);
    else
      let sig = value.ct-function-signature;
      maybe-restrict-type(component, call, sig.returns);
      maybe-change-to-known-or-error-call(component, call, sig,
					  value.ct-function-name);
    end;
  else
    // Assert that the function is a function.  It isn't, but this is a handy
    // way of generating an error message.
    assert-type(component, call.dependents.dependent, call.depends-on,
		if (call.use-generic-entry?)
		  specifier-type(#"<method>");
		else
		  function-ctype();
		end);
  end;
end;

define method optimize-unknown-call
    (component :: <component>, call :: <unknown-call>,
     defn :: <abstract-constant-definition>)
    => ();
  // Assert that the function is a function.
  assert-type(component, call.dependents.dependent, call.depends-on,
	      if (call.use-generic-entry?)
		specifier-type(#"<method>");
	      else
		function-ctype();
	      end);
end;

define method optimize-unknown-call
    (component :: <component>, call :: <unknown-call>,
     defn :: <function-definition>)
    => ();
  maybe-restrict-type(component, call, defn.function-defn-signature.returns);
end;

define method optimize-unknown-call
    (component :: <component>, call :: <unknown-call>,
     defn :: <generic-definition>)
    => ();
  let sig = defn.function-defn-signature;
  maybe-restrict-type(component, call, sig.returns);

  if (call.use-generic-entry?)
    error("Trying to pass a generic function next-method information?");
  end;

  let bogus? = #f;
  let arg-leaves = #();
  let arg-types = #();
  block (return)
    for (arg-dep = call.depends-on.dependent-next then arg-dep.dependent-next,
	 count from 0,
	 gf-spec in sig.specializers)
      unless (arg-dep)
	compiler-warning("In %s:\n  not enough arguments in call of %s.\n  "
			   "Wanted %s %d, but only got %d",
			 call.home-function-region.name,
			 defn.defn-name,
			 if (sig.rest-type | sig.key-infos)
			   "at least";
			 else
			   "exactly";
			 end,
			 sig.specializers.size,
			 count);
	bogus? := #t;
	return();
      end;
      let arg-leaf = arg-dep.source-exp;
      let arg-type = arg-leaf.derived-type;
      unless (ctypes-intersect?(arg-type, gf-spec))
	compiler-warning("In %s:\n  wrong type for argument %d in call of "
			   "%s.\n  Wanted %s, but got %s",
			 call.home-function-region.name,
			 count,
			 defn.defn-name,
			 gf-spec,
			 arg-dep.source-exp.derived-type);
	bogus? := #t;
      end;
      arg-leaves := pair(arg-leaf, arg-leaves);
      arg-types := pair(arg-type, arg-types);
    finally
      if (arg-dep)
	unless (sig.key-infos | sig.rest-type)
	  for (arg-dep = arg-dep then arg-dep.dependent-next,
	       have from count,
	       while: arg-dep)
	  finally
	    compiler-warning("In %s:\n  too many arguments in call of %s.\n"
			       "  Wanted exactly %d, but got %d.",
			     call.home-function-region.name,
			     defn.defn-name,
			     count, have);
	    bogus? := #t;
	  end;
	end;
	for (arg-dep = arg-dep then arg-dep.dependent-next,
	     while: arg-dep)
	  arg-leaves := pair(arg-dep.source-exp, arg-leaves);
	end;
      end;
    end;
  end;
  if (bogus?)
    change-call-kind(component, call, <error-call>);
  else
    optimize-generic(component, call, defn, reverse!(arg-types),
		     reverse!(arg-leaves));
  end if;
end method optimize-unknown-call;


define method optimize-generic
    (component :: <component>, call :: <abstract-call>,
     defn :: <generic-definition>, arg-types :: <list>,
     arg-leaves :: <list>)
    => ();
  block (return)
    local
      method maybe-change-to-known ()
	if (~instance?(call, <known-call>) & defn.generic-defn-discriminator)
	  // There is a static descriminator function.  We can change into a
	  // known call.  We don't reference the discriminator directly because
	  // we still want to be able to do method selection if we can derive
	  // a better idea of the argument types.
	  convert-to-known-call
	    (component, defn.generic-defn-discriminator.ct-function-signature,
	     call);
	end if;
	return();
      end method maybe-change-to-known,
      method change-to-error ()
	if (instance?(call, <known-call>))
	  // We can't just change a <known-call> into an <error-call> because
	  // all the #rest args are lumped into a vector.
	  let builder = make-builder(component);
	  let new-call
	    = make-operation(builder, <error-call>,
			     pair(call.depends-on.source-exp, arg-leaves));
	  replace-expression(component, call.dependents, new-call);
	else
	  change-call-kind(component, call, <error-call>);
	end;
	return();
      end method change-to-error;

    let (definitely, maybe) = ct-applicable-methods(defn, arg-types);
    if (definitely == #f)
      maybe-change-to-known();
    end if;

    let applicable = concatenate(definitely, maybe);

    if (applicable == #())
      no-applicable-methods-warning(call, defn, arg-types);
      change-to-error();
    end if;

    // Improve the result type based on the actually applicable methods.
    for (meth in applicable,
	 result-type = wild-ctype()
	   then values-type-union(result-type,
				  meth.function-defn-signature.returns))
    finally
      maybe-restrict-type(component, call, result-type);
    end for;

    // Blow out of here if applicable isn't a valid set of methods.
    unless (maybe == #() | (maybe.tail == #() & definitely == #()))
      return();
    end unless;

    // Compute the set of valid keywords.
    let sig = defn.function-defn-signature;
    let valid-keys
      = if (sig.key-infos)
	  if (sig.all-keys?)
	    #"all";
	  else
	    block (return)
	      reduce(method (keys :: <simple-object-vector>,
			     meth :: <method-definition>)
			 => res :: <simple-object-vector>;
		       let sig = meth.function-defn-signature;
		       if (sig.all-keys?)
			 return(#"all");
		       end if;
		       union(keys, map(key-name, sig.key-infos));
		     end method,
		     #[],
		     applicable);
	    end block;
	  end if;
	else
	  #f;
	end if;

    // Check the keyword arguments for validity.
    if (valid-keys)
      let bogus? = #f;
      for (remaining = arg-leaves then remaining.tail,
	   fixed in arg-types,
	   index from 0)
      finally
	for (remaining = remaining then remaining.tail.tail,
	     index from index by 2,
	     until: remaining == #())
	  let key-leaf = remaining.head;
	  
	  if (~instance?(key-leaf, <literal-constant>))
	    unless (ctypes-intersect?(key-leaf.derived-type,
				      specifier-type(#"<symbol>")))
	      compiler-warning("In %s:\n  bogus keyword as argument "
				 "%d in call of %s",
			       call.home-function-region.name,
			       index,
			       defn.defn-name);
	      bogus? := #t;
	    end;
	  elseif (instance?(key-leaf.value, <literal-symbol>))
	    unless (valid-keys == #"all"
		      | member?(key-leaf.value.literal-value, valid-keys))
	      compiler-warning
		("In %s:\n  invalid keyword (%=) in call of %s",
		 call.home-function-region.name,
		 key-leaf.value.literal-value,
		 defn.defn-name);
	      bogus? := #t;
	    end;
	  else
	    compiler-warning
	      ("In %s:\n  bogus keyword (%s) as argument %d in call of %s",
	       call.home-function-region.name,
	       key-leaf.value,
	       index,
	       defn.defn-name);
	    bogus? := #t;
	  end;

	  if (remaining.tail == #())
	    compiler-warning("In %s:\n  odd number of keyword/value arguments "
			       "in call of %s.",
			     call.home-function-region.name,
			     defn.defn-name);
	    change-to-error();
	  end if;
	end for;
      end for;
      if (bogus?)
	change-to-error();
      end if;
    end if;

    // Sort the applicable methods.
    let (ordered, ambiguous) = sort-methods(applicable, #f);

    if (ordered == #f)
      // We can't tell jack about how to order the methods.  So just change
      // to a known call of the discriminator if possible.
      maybe-change-to-known();
    end if;

    if (ordered == #())
      // It's ambiguous which is the most specific method.  So bitch.
      ambiguous-method-warning(call, defn, ambiguous, arg-types);
      change-to-error();
    end if;
    
    // Change to an unknown call of the most specific method.
    let builder = make-builder(component);
    let assign = call.dependents.dependent;
    let policy = assign.policy;
    let source = assign.source-location;
    let new-func
      = fer-convert-defn-ref(builder, policy, source, ordered.head);
    let next-leaf
      = make-next-method-info-leaf(builder, ordered, ambiguous);
    insert-before(component, assign, builder-result(builder));
    let new-call = make-unknown-call(builder, new-func, next-leaf, arg-leaves);
    replace-expression(component, call.dependents, new-call);
  end block;
end method optimize-generic;




define method ambiguous-method-warning
    (call :: <abstract-call>, defn :: <generic-definition>,
     ambiguous :: <list>, arg-types :: <list>)
    => ();
  let stream = make(<byte-string-output-stream>);
  write("    (", stream);
  for (arg-type in arg-types, first? = #t then #f)
    unless (first?)
      write(", ", stream);
    end;
    print-message(arg-type, stream);
  end;
  write(")", stream);
  let arg-types-string = stream.string-output-stream-string;
  for (meth in ambiguous)
    format(stream, "    %s\n", meth.defn-name);
  end;
  compiler-warning("In %s:\n  can't pick between\n%s\n  when given "
		     "arguments of types:\n%s",
		   call.home-function-region.name,
		   stream.string-output-stream-string,
		   arg-types-string);
end;

define method no-applicable-methods-warning
    (call :: <abstract-call>, defn :: <generic-definition>,
     arg-types :: <list>)
    => ();
  let stream = make(<byte-string-output-stream>);
  write("    (", stream);
  for (arg-type in arg-types, first? = #t then #f)
    unless (first?)
      write(", ", stream);
    end;
    print-message(arg-type, stream);
  end;
  write(")", stream);
  let arg-types-string = stream.string-output-stream-string;
  compiler-warning("In %s:\n  no applicable methods for argument types\n"
		     "%s\n  in call of %s",
		   call.home-function-region.name,
		   arg-types-string,
		   defn.defn-name);
end;

define method make-next-method-info-leaf
    (builder :: <fer-builder>, ordered :: <list>, ambiguous :: <list>)
    => res :: <leaf>;

  let ordered-ctvs = map(ct-value, ordered.tail);
  let ambiguous-ctvs = map(ct-value, ambiguous);

  if (every?(identity, ordered-ctvs) & every?(identity, ambiguous-ctvs))
    make-literal-constant(builder,
			  make(<literal-list>,
			       sharable: #t,
			       contents: ordered-ctvs,
			       tail: if (ordered == #())
				       as(<ct-value>, #());
				     else
				       make(<literal-list>,
					    sharable: #t,
					    contents: ambiguous-ctvs);
				     end));
  else
    error("Can't deal with next method infos that arn't all ctvs.");
    /*
    let var = make-local-var(builder, #"next-method-info",
			     object-ctype());
    let op = make-unknown-call(builder, dylan-defn-leaf(builder, #"list"), #f,
			       map(curry(make-definition-leaf, builder),
				   ordered.tail));
    build-assignment(builder, policy, source, var, op);
    var;
    */
  end;
end;

define method optimize-unknown-call
    (component :: <component>, call :: <unknown-call>,
     defn :: <abstract-method-definition>)
    => ();
  let sig = defn.function-defn-signature;
  maybe-restrict-type(component, call, sig.returns);
  select (compare-unknown-call-against-signature(call, sig, defn.defn-name))
    #"bogus" =>
      if (call.use-generic-entry?)
	error("bogus call w/ next?");
      end;
      change-call-kind(component, call, <error-call>);

    #"valid" =>
      let inline-function = defn.method-defn-inline-function;
      if (inline-function)
	let old-head = component.reoptimize-queue;
	let new-func = clone-function(component, inline-function);
	reverse-queue(component, old-head);
	replace-expression(component, call.depends-on, new-func);
      elseif (~defn.function-defn-hairy?)
	convert-to-known-call(component, sig, call);
      end;

    #"can't tell" =>
      #f;
  end;
end;

define method method-defn-inline-function
    (defn :: <abstract-method-definition>)
    => res :: false-or(<function-literal>);
  if (defn.%method-defn-inline-function == #"not-computed-yet")
    if (defn.method-defn-inline-expansion & ~defn.function-defn-hairy?)
      let component = make(<fer-component>);
      let builder = make-builder(component);
      let lexenv = make(<lexenv>);
      let leaf = fer-convert-method(builder, defn.method-defn-inline-expansion,
				    format-to-string("%s", defn.defn-name),
				    #f, #"local", lexenv, lexenv);
      optimize-component(component, simplify-only: #t);
      defn.%method-defn-inline-function := leaf;
    else
      defn.%method-defn-inline-function := #f;
    end;
  else
    defn.%method-defn-inline-function;
  end;
end;

define method optimize-unknown-call
    (component :: <component>, call :: <unknown-call>,
     func :: <exit-function>)
    => ();
  let builder = make-builder(component);
  let call-dependency = call.dependents;
  let assign = call-dependency.dependent;
  let policy = assign.policy;
  let source = assign.source-location;

  let values = make(<stretchy-vector>);
  for (dep = call.depends-on.dependent-next then dep.dependent-next,
       while: dep)
    add!(values, dep.source-exp);
  end;
  let cluster = make-values-cluster(builder, #"cluster", wild-ctype());
  build-assignment(builder, policy, source, cluster,
		   make-operation(builder, <primitive>, as(<list>, values),
				  name: #"values"));
  insert-before(component, assign, builder-result(builder));
  expand-exit-function(component, call, func, cluster);
end;


define method maybe-change-to-known-or-error-call
    (component :: <component>, call :: <unknown-call>, sig :: <signature>,
     func-name :: type-union(<name>, <string>))
    => ();
  select (compare-unknown-call-against-signature(call, sig, func-name))
    #"bogus" =>
      if (call.use-generic-entry?)
	error("bogus call w/ next?");
      end;
      change-call-kind(component, call, <error-call>);

    #"valid" =>
      convert-to-known-call(component, sig, call);

    #"can't tell" =>
      #f;
  end;
end;


define method compare-unknown-call-against-signature
    (call :: <unknown-call>, sig :: <signature>,
     func-name :: type-union(<name>, <string>))
    => res :: one-of(#"bogus", #"valid", #"can't tell");

  // Find the next-method-info and arguments.
  let (next-method-info, arguments)
    = if (call.use-generic-entry?)
	let dep = call.depends-on.dependent-next;
	values(dep.source-exp, dep.dependent-next);
      else
	values(#f, call.depends-on.dependent-next);
      end;

  let bogus? = #f;
  let valid? = #t;

  block (return)
    for (spec in sig.specializers,
	 arg-dep = arguments then arg-dep.dependent-next,
	 count from 0)
      unless (arg-dep)
	compiler-warning("In %s:\n  not enough arguments in call of %s.\n  "
			   "Wanted %s %d, but only got %d",
			 call.home-function-region.name,
			 func-name,
			 if (sig.rest-type | sig.key-infos)
			   "at least";
			 else
			   "exactly";
			 end,
			 sig.specializers.size,
			 count);
	bogus? := #t;
	return();
      end;
      unless (ctypes-intersect?(arg-dep.source-exp.derived-type, spec))
	compiler-warning("In %s:\n  wrong type for argument %d in call of "
			   "%s.\n  Wanted %s, but got %s",
			 call.home-function-region.name,
			 count,
			 func-name,
			 spec,
			 arg-dep.source-exp.derived-type);
	bogus? := #t;
      end;
    finally
      if (sig.key-infos)
	// Make sure all the supplied keywords are okay.
	for (key-dep = arg-dep then key-dep.dependent-next.dependent-next,
	     count from count by 2,
	     while: key-dep)
	  let val-dep = key-dep.dependent-next;
	  unless (val-dep)
	    compiler-warning("In %s:\n  odd number of keyword/value arguments "
			       "in call of %s.",
			     call.home-function-region.name,
			     func-name);
	    bogus? := #t;
	    return();
	  end;
	  let leaf = key-dep.source-exp;
	  if (~instance?(leaf, <literal-constant>))
	    unless (ctypes-intersect?(leaf.derived-type,
				      specifier-type(#"<symbol>")))
	      compiler-warning("In %s:\n  bogus keyword as argument "
				 "%d in call of %s",
			       call.home-function-region.name,
			       count,
			       func-name);
	      bogus? := #t;
	    end;
	    valid? := #f;
	  elseif (instance?(leaf.value, <literal-symbol>))
	    let key = leaf.value.literal-value;
	    block (found-key)
	      for (keyinfo in sig.key-infos)
		if (keyinfo.key-name == key)
		  unless (ctypes-intersect?(val-dep.source-exp.derived-type,
					    keyinfo.key-type))
		    compiler-warning("In %s:\n  wrong type for keyword "
				       "argument %= in call of "
				       "%s.\n  Wanted %s, but got %s",
				     call.home-function-region.name,
				     key,
				     func-name,
				     keyinfo.key-type,
				     val-dep.source-exp.derived-type);

		    bogus? := #t;
		  end;
		  found-key();
		end;
	      end;
	      unless (sig.all-keys? | call.use-generic-entry?)
		compiler-warning
		  ("In %s:\n  invalid keyword (%=) in call of %s",
		   call.home-function-region.name,
		   key,
		   func-name);
		bogus? := #t;
	      end;
	    end;
	  else
	    compiler-warning
	      ("In %s:\n  bogus keyword (%s) as argument %d in call of %s",
	       call.home-function-region.name,
	       leaf.value,
	       count,
	       func-name);
	    bogus? := #t;
	  end;
	end;
	if (valid?)
	  // Now make sure all the required keywords are supplied.
	  for (keyinfo in sig.key-infos)
	    block (found-key)
	      for (key-dep = arg-dep
		     then key-dep.dependent-next.dependent-next,
		   while: key-dep)
		if (keyinfo.key-name = key-dep.source-exp.value.literal-value)
		  found-key();
		end;
	      end;
	      if (keyinfo.required?)
		compiler-warning
		  ("In %s: required keyword %= missing in call of %s",
		   call.home-function-region.name, keyinfo.key-name,
		   func-name);
		bogus? := #t;
	      end;
	    end;
	  end;
	end;
      elseif (sig.rest-type)
	for (arg-dep = arg-dep then arg-dep.dependent-next,
	     count from count,
	     while: arg-dep)
	  unless (ctypes-intersect?(arg-dep.source-exp.derived-type,
				    sig.rest-type))
	    compiler-warning("In %s:\n  wrong type for argument %d in call of "
			       "%s.\n  Wanted %s, but got %s",
			     call.home-function-region.name,
			     count,
			     func-name,
			     sig.rest-type,
			     arg-dep.source-exp.derived-type);
	    bogus? := #t;
	  end;
	end;
      elseif (arg-dep)
	for (arg-dep = arg-dep then arg-dep.dependent-next,
	     have from count,
	     while: arg-dep)
	finally
	  compiler-warning("In %s:\n  too many arguments in call of %s.\n"
			     "  Wanted exactly %d, but got %d.",
			   call.home-function-region.name,
			   func-name,
			   count, have);
	  bogus? := #t;
	end;
      end;
    end;
  end;

  if (bogus?)
    #"bogus";
  elseif (valid?)
    #"valid";
  else
    #"can't tell";
  end;
end;


define method convert-to-known-call
    (component :: <component>, sig :: <signature>, call :: <unknown-call>)
    => ();
  let (next-method-info, arguments)
    = if (call.use-generic-entry?)
	let dep = call.depends-on.dependent-next;
	values(dep.source-exp, dep.dependent-next);
      else
	values(#f, call.depends-on.dependent-next);
      end;

  let builder = make-builder(component);
  let new-ops = make(<stretchy-vector>);
  // Add the original function to the known-call operands.
  add!(new-ops, call.depends-on.source-exp);
  // Add the fixed parameters.
  let assign = call.dependents.dependent;
  for (spec in sig.specializers,
       arg-dep = arguments then arg-dep.dependent-next)
    // Assert the argument types before adding them to the known-call
    // operands so that the known-call sees the asserted leaves.
    assert-type(component, assign, arg-dep, spec);
    add!(new-ops, arg-dep.source-exp);
  finally
    // If there is a #next parameter, add something for it.
    if (sig.next?)
      if (next-method-info)
	add!(new-ops, next-method-info);
      else
	add!(new-ops, make-literal-constant(builder, as(<ct-value>, #())));
      end;
    end;
    // Need to assert the key types before we build the #rest vector.
    if (sig.key-infos)
      for (key-dep = arg-dep then key-dep.dependent-next.dependent-next,
	   while: key-dep)
	block (next-key)
	  let key = key-dep.source-exp.value.literal-value;
	  for (keyinfo in sig.key-infos)
	    if (keyinfo.key-name == key)
	      assert-type(component, assign, key-dep.dependent-next,
			  keyinfo.key-type);
	      next-key();
	    end;
	  end;
	end;
      end;
    end;
    if (sig.rest-type | (sig.next? & sig.key-infos))
      let rest-args = make(<stretchy-vector>);
      for (arg-dep = arg-dep then arg-dep.dependent-next,
	   while: arg-dep)
	add!(rest-args, arg-dep.source-exp);
      end;
      let rest-temp = make-local-var(builder, #"rest", object-ctype());
      build-assignment
	(builder, assign.policy, assign.source-location, rest-temp,
	 make-operation(builder, <primitive>, as(<list>, rest-args),
			name: #"vector"));
      add!(new-ops, rest-temp);
    end;
    if (sig.key-infos)
      for (keyinfo in sig.key-infos)
	let key = keyinfo.key-name;
	for (key-dep = arg-dep then key-dep.dependent-next.dependent-next,
	     until: key-dep == #f
	       | key-dep.source-exp.value.literal-value == key)
	finally
	  let leaf
	    = if (key-dep)
		key-dep.dependent-next.source-exp;
	      else
		let default = keyinfo.key-default;
		if (default)
		  make-literal-constant(builder, default);
		else
		  make(<uninitialized-value>,
		       derived-type: keyinfo.key-type);
		end;
	      end;
	  add!(new-ops, leaf);
	  if (keyinfo.key-needs-supplied?-var)
	    let supplied? = as(<ct-value>, key-dep & #t);
	    add!(new-ops, make-literal-constant(builder, supplied?));
	  end;
	end;
      end;
    end;
    insert-before(component, assign, builder-result(builder));
    let new-call = make-operation(builder, <known-call>,
				  as(<list>, new-ops));
    replace-expression(component, call.dependents, new-call);
  end;
end;




define method optimize
    (component :: <component>, call :: <known-call>) => ();
  let func-dep = call.depends-on;
  unless (func-dep)
    error("No function in a call?");
  end;
  let func = func-dep.source-exp;
  block (return)
    for (transformer in find-transformers(func))
      if (transformer.transformer-function(component, call))
	return();
      end;
    end;
    // Dispatch of the thing we are calling.
    optimize-known-call(component, call, func);
  end;
end;

define method find-transformers (func :: type-union(<leaf>, <definition>))
    => res :: <list>;
  #();
end;

define method find-transformers (func :: <definition-constant-leaf>)
    => res :: <list>;
  find-transformers(func.const-defn);
end;

define method find-transformers (func :: <literal-constant>)
  let defn = func.value.ct-function-definition;
  if (defn)
    find-transformers(defn);
  else
    #();
  end;
end;

define method find-transformers (defn :: <function-definition>)
    => res :: <list>;
  defn.function-defn-transformers;
end;
  
define method find-transformers (defn :: <generic-definition>)
    => res :: <list>;
  choose(method (transformer)
	   transformer.transformer-specializers == #f;
	 end,
	 defn.function-defn-transformers);
end;


define method optimize-known-call
    (component :: <component>, call :: <known-call>,
     func :: type-union(<leaf>, <definition>))
    => ();
end;

define method optimize-known-call
    (component :: <component>, call :: <known-call>,
     func :: <function-literal>)
    => ();
  maybe-restrict-type(component, call, func.main-entry.result-type);
end;

define method optimize-known-call
    (component :: <component>, call :: <known-call>,
     func :: <literal-constant>)
    => ();
  block (return)
    let ctv = func.value;
    for (lit in component.all-function-literals)
      if (lit.ct-function == ctv)
	replace-expression(component, call.depends-on, lit);
	return();
      end;
    end;
    let defn = ctv.ct-function-definition;
    if (defn)
      optimize-known-call(component, call, defn);
    else
      maybe-restrict-type(component, call, ctv.ct-function-signature.returns);
    end;
  end;
end;

define method optimize-known-call
    (component :: <component>, call :: <known-call>,
     func :: <definition-constant-leaf>)
    => ();
  optimize-known-call(component, call, func.const-defn);
end;

define method optimize-known-call
    (component :: <component>, call :: <known-call>,
     defn :: <function-definition>)
  let sig = defn.function-defn-signature;
  maybe-restrict-type(component, call, sig.returns);
end;

define method optimize-known-call
    (component :: <component>, call :: <known-call>,
     defn :: <generic-definition>)
    => ();
  // We might be able to do a bit more method selection.

  let sig = defn.function-defn-signature;
  maybe-restrict-type(component, call, sig.returns);

  let nfixed = sig.specializers.size;
  for (i from 0 below nfixed,
       arg-leaves = #() then pair(dep.source-exp, arg-leaves),
       arg-types = #() then pair(dep.source-exp.derived-type, arg-types),
       dep = call.depends-on.dependent-next then dep.dependent-next)
  finally
    if (sig.rest-type)
      unless (dep & dep.dependent-next == #f)
	error("Strange number of arguments in a known call to a generic "
		"function?");
      end;
      let rest-var = dep.source-exp;
      unless (instance?(rest-var, <ssa-variable>))
	error("Strange leaf for rest-var in known call to a generic "
		"function");
      end;
      let rest-op = rest-var.definer.depends-on.source-exp;
      unless (instance?(rest-op, <primitive>) & rest-op.name == #"vector")
	error("Strange value for rest-var in known call to a generic "
		"function");
      end;
      for (dep = rest-op.depends-on then dep.dependent-next,
	   while: dep)
	arg-leaves := pair(dep.source-exp, arg-leaves);
      end;
    end;

    optimize-generic(component, call, defn, reverse!(arg-types),
		     reverse!(arg-leaves));
  end for;
end method optimize-known-call;

define method optimize-known-call
    (component :: <component>, call :: <known-call>,
     func :: <getter-method-definition>)     
    => ();
  maybe-restrict-type(component, call, func.function-defn-signature.returns);
  optimize-slot-ref(component, call, func.accessor-method-defn-slot-info,
		    listify-dependencies(call.depends-on.dependent-next));
end;

define method optimize-known-call
    (component :: <component>, call :: <known-call>,
     func :: <setter-method-definition>)     
    => ();
  maybe-restrict-type(component, call, func.function-defn-signature.returns);
  optimize-slot-set(component, call, func.accessor-method-defn-slot-info,
		    listify-dependencies(call.depends-on.dependent-next));
end;


define method listify-dependencies (dependencies :: false-or(<dependency>))
    => res :: <list>;
  for (res = #() then pair(dep.source-exp, res),
       dep = dependencies then dep.dependent-next,
       while: dep)
  finally
    reverse!(res);
  end;
end;


define method optimize-slot-ref
    (component :: <component>, call :: <abstract-call>,
     slot :: <instance-slot-info>, args :: <list>)
    => ();
  let instance = args.first;
  let offset = find-slot-offset(slot, instance.derived-type);
  if (offset)
    let builder = make-builder(component);
    let call-assign = call.dependents.dependent;
    let policy = call-assign.policy;
    let source = call-assign.source-location;
    let init?-slot = slot.slot-initialized?-slot;
    let guaranteed-initialized?
      = slot-guaranteed-initialized?(slot, instance.derived-type);
    if (init?-slot & ~guaranteed-initialized?)
      let init?-offset = find-slot-offset(init?-slot, instance.derived-type);
      unless (init?-offset)
	error("The slot is at a fixed offset, but the initialized flag "
		"isn't?");
      end;
      let temp = make-local-var(builder, #"slot-initialized?", object-ctype());
      build-assignment(builder, policy, source, temp,
		       make-operation(builder, <slot-ref>, list(instance),
				      derived-type: init?-slot.slot-type,
				      slot-info: init?-slot,
				      slot-offset: init?-offset));
      build-if-body(builder, policy, source, temp);
      build-else(builder, policy, source);
      build-assignment
	(builder, policy, source, #(),
	 make-error-operation(builder, policy, source,
			      #"uninitialized-slot-error"));
      end-body(builder);
    end;
    let value = make-local-var(builder, slot.slot-getter.variable-name,
			       slot.slot-type);
    build-assignment(builder, policy, source, value,
		     make-operation(builder, <slot-ref>, args,
				    derived-type: slot.slot-type,
				    slot-info: slot, slot-offset: offset));
    unless (init?-slot | guaranteed-initialized?)
      let temp = make-local-var(builder, #"slot-initialized?", object-ctype());
      build-assignment(builder, policy, source, temp,
		       make-operation(builder, <primitive>, list(value),
				      name: #"initialized?"));
      build-if-body(builder, policy, source, temp);
      build-else(builder, policy, source);
      build-assignment
	(builder, policy, source, #(),
	 make-error-operation(builder, policy, source,
			      #"uninitialized-slot-error"));
      end-body(builder);
    end;
    insert-before(component, call-assign, builder-result(builder));

    let dep = call-assign.depends-on;
    replace-expression(component, dep, value);
  end;
end;

define method optimize-slot-set
    (component :: <component>, call :: <abstract-call>,
     slot :: <instance-slot-info>, args :: <list>)
    => ();
  let instance = args.second;
  let offset = find-slot-offset(slot, instance.derived-type);
  if (offset)
    let new = args.first;
    let builder = make-builder(component);
    let call-assign = call.dependents.dependent;
    let op = make-operation(builder, <slot-set>, args,
			    slot-info: slot, slot-offset: offset);

    build-assignment(builder, call-assign.policy, call-assign.source-location,
		     #(), op);
    begin
      let init?-slot = slot.slot-initialized?-slot;
      if (init?-slot)
	let init?-offset = find-slot-offset(init?-slot, instance.derived-type);
	unless (init?-offset)
	  error("The slot is at a fixed offset, but the initialized flag "
		  "isn't?");
	end;
	let true-leaf = make-literal-constant(builder, make(<literal-true>));
	let init-op = make-operation
	  (builder, <slot-set>, list(true-leaf, instance),
	   slot-info: init?-slot, slot-offset: init?-offset);
	build-assignment(builder, call-assign.policy,
			 call-assign.source-location, #(), init-op);
      end;
    end;
    insert-before(component, call-assign, builder-result(builder));

    let dep = call-assign.depends-on;
    replace-expression(component, dep, new);
  end;
end;



define method change-call-kind
    (component :: <component>, call :: <abstract-call>, new-kind :: <class>,
     #rest make-keyword-args, #all-keys)
    => ();
  let new = apply(make, new-kind, dependents: call.dependents,
		  depends-on: call.depends-on,
		  derived-type: call.derived-type,
		  make-keyword-args);
  for (dep = call.depends-on then dep.dependent-next,
       while: dep)
    dep.dependent := new;
  end;
  for (dep = call.dependents then dep.source-next,
       while: dep)
    dep.source-exp := new;
  end;
  reoptimize(component, new);

  call.depends-on := #f;
  call.dependents := #f;
  delete-dependent(component, call);
end;



// <error-call> optimization.
//
// We only make <error-call>s when we want to give up.
// 
define method optimize (component :: <component>, call :: <error-call>) => ();
end;


// <mv-call> optimization.
//
// If the cluster feeding the mv call has a fixed number of values, then
// convert the <mv-call> into an <unknown-call>.  Otherwise, if the called
// function is an exit function, then convert it into a pitcher.
// 
define method optimize (component :: <component>, call :: <mv-call>) => ();
  let cluster
    = if (call.use-generic-entry?)
	call.depends-on.dependent-next.dependent-next.source-exp;
      else
	call.depends-on.dependent-next.source-exp;
      end;
  if (maybe-expand-cluster(component, cluster))
    change-call-kind(component, call, <unknown-call>,
		     use-generic-entry: call.use-generic-entry?);
  else
    let func = call.depends-on.source-exp;
    if (instance?(func, <exit-function>))
      expand-exit-function(component, call, func, cluster);
    elseif (instance?(func, <literal-constant>)
	      & func.value == dylan-value(#"values"))
      replace-expression(component, call.dependents, cluster);
    end;
  end;
end;


// block/exit related optimizations.

define method optimize (component :: <component>, catch :: <catch>) => ();
  let nlx-info = catch.nlx-info;
  if (~nlx-info.nlx-hidden-references? & nlx-info.nlx-exit-function == #f
	& nlx-info.nlx-throws == #f)
    // We no longer need the exit function and all the throws are gone,
    // which means we only have local exits.  So change the catch into a call
    // and nuke the make-catcher and disable-catchers.
    let builder = make-builder(component);
    replace-expression
      (component, catch.dependents,
       make-unknown-call
	 (builder, catch.depends-on.source-exp, #f,
	  list(make(<uninitialized-value>, 
		    derived-type: specifier-type(#"<raw-pointer>")))));
    let make-catcher-op = nlx-info.nlx-make-catcher;
    if (make-catcher-op)
      replace-expression
	(component, make-catcher-op.dependents,
	 make(<uninitialized-value>, 
	      derived-type: make-catcher-op.derived-type));
    end;
    while (nlx-info.nlx-disable-catchers)
      let disable-catcher-op = nlx-info.nlx-disable-catchers;
      replace-expression
	(component, disable-catcher-op.dependents,
	 make(<uninitialized-value>, 
	      derived-type: disable-catcher-op.derived-type));
    end;
  end;
end;

define method expand-exit-function
    (component :: <component>, call :: <general-call>,
     func :: <exit-function>, cluster :: <abstract-variable>)
    => ();
  if (call.use-generic-entry?)
    error("Trying to call the generic entry for an exit function?");
  end;

  replace-expression(component, call.dependents,
		     make-operation(make-builder(component), <throw>,
				    list(func.depends-on.source-exp, cluster),
				    nlx-info: func.nlx-info));
end;

define method optimize (component :: <component>, op :: <throw>) => ();
  let nlx-info = op.nlx-info;
  let body-region = nlx-info.nlx-make-catcher.home-function-region;
  if (op.home-function-region == body-region)
    let assign = op.dependents.dependent;
    let builder = make-builder(component);
    build-assignment
      (builder, assign.policy, assign.source-location, #(),
       make-operation(builder, <disable-catcher>,
		      list(op.depends-on.source-exp),
		      nlx-info: nlx-info));
    for (region = op.dependents.dependent.region
	   then region.parent,
	 until: region == body-region)
      if (instance?(region, <unwind-protect-region>))
	build-assignment
	  (builder, $Default-Policy, region.source-location, #(),
	   make-unknown-call
	     (builder, region.uwp-region-cleanup-function, #f, #()));
      end;
    end;
    insert-before(component, assign, builder-result(builder));
    insert-return-before(component, assign, body-region,
			 op.depends-on.dependent-next.source-exp);
  end;
end;

define method optimize
    (component :: <component>, region :: <block-region>) => ();
  if (region.exits == #f)
    if (region.body.doesnt-return?)
      delete-stuff-after(component, region.parent, region);
    end;
    replace-subregion(component, region.parent, region, region.body);
    delete-queueable(component, region);
  end;
end;



// Function optimization


define method optimize
    (component :: <component>, function :: <function-literal>)
    => ();

  // For functions that are only locally visible, we can throw them away
  // if we no longer need them.
  if (function.visibility == #"local")
    if (block (return)
	  for (dep = function.dependents then dep.source-next,
	       while: dep)
	    unless (home-function-region(dep.dependent) == function.main-entry)
	      return(#f);
	    end;
	  finally
	    #t;
	  end;
	end)
      // All the references to this function are inside this function, so
      // there is no way any outside party can reference it -- hence we can
      // nuke it.  First, we delete the body.
      local
	method delete-function-region (region)
	  if (region)
	    delete-stuff-in(component, region);
	  end;
	end;
      delete-function-region(function.main-entry);
      delete-function-region(function.general-entry);
      if (instance?(function, <method-literal>))
	delete-function-region(function.generic-entry);
      end;
      // Deleting the body should have flushed the remaining references.
      assert(function.dependents == #f);
      // And then we delete the function literal itself.
      function.visibility := #"deleted";
      remove!(component.all-function-literals, function);
      delete-queueable(component, function);
    elseif (function.dependents.source-next == #f)
      // There is only one reference.  Let convert it if it is a known call.
      // We don't have to check for self calls, because they will have
      // been picked off up above.
      let ref = function.dependents.dependent;
      if (ref.depends-on == function.dependents & instance?(ref, <known-call>))
	let-convert(component, function.main-entry, ref);
      end;
    end;
  end;
end;

define method optimize
    (component :: <component>, function :: <fer-function-region>) => ();
  //
  // If there are hidden references, we can't improve the result type because
  // those hidden references have no way of observing said improvement.
  unless (function.hidden-references?)
    // Compute the result type by unioning all the returned types.  If it is
    // more restrictive than last time, queue all the dependents of this
    // function.
    for (return = function.exits then return.next-exit,
	 type = empty-ctype()
	   then values-type-union(type, return.returned-type),
	 while: return)
    finally
      let old-type = function.result-type;
      if (~values-subtype?(old-type, type) & values-subtype?(type, old-type))
	function.result-type := type;
      end;
    end;
  end;
end;

define method optimize (component :: <component>, prologue :: <prologue>)
    => ();
  maybe-restrict-type(component, prologue,
		      make-values-ctype(prologue.function.argument-types, #f));
end;

define method optimize (component :: <component>, return :: <return>) => ();
  let results = return.depends-on;
  let cluster?
    = (results & instance?(results.source-exp, <abstract-variable>)
	 & instance?(results.source-exp.var-info, <values-cluster-info>));
  let result-type
    = if (cluster?)
	results.source-exp.derived-type;
      else
	let types = make(<stretchy-vector>);
	for (dep = results then dep.dependent-next,
	     while: dep)
	  add!(types, dep.source-exp.derived-type);
	end;
	make-values-ctype(as(<list>, types), #f);
      end;
  let old-type = return.returned-type;
  if (~values-subtype?(old-type, result-type)
	& values-subtype?(result-type, old-type))
    return.returned-type := result-type;
    reoptimize(component, return.block-of);
  end;

  if (cluster?)
    maybe-expand-cluster(component, results.source-exp);
  end;
end;


define method maybe-expand-cluster
    (component :: <component>, cluster :: <abstract-variable>)
    => did-anything? :: <boolean>;
  if (fixed-number-of-values?(cluster.derived-type))
    unless (cluster.dependents)
      error("Trying to expand a cluster that isn't being used?");
    end;
    if (cluster.dependents.source-next)
      error("Trying to expand a cluster that is referenced "
	      "in more than one place?");
    end;
    expand-cluster(component, cluster, cluster.derived-type.min-values, #());
    #t;
  else
    #f;
  end;
end;

define method expand-cluster 
    (component :: <component>, cluster :: <ssa-variable>,
     number-of-values :: <fixed-integer>, names :: <list>)
    => ();
  let cluster-dependency = cluster.dependents;
  let target = cluster-dependency.dependent;
  let assign = cluster.definer;
  let new-defines = #f;
  let new-depends-on = cluster-dependency.dependent-next;
  for (index from number-of-values - 1 to 0 by -1,
       names = names then names.tail)
    let debug-name = if (names == #())
		       as(<symbol>, format-to-string("result%d", index));
		     else
		       names.head;
		     end;
    let var-info = make(<local-var-info>, debug-name: debug-name,
			asserted-type: object-ctype());
    let var = make(<ssa-variable>, var-info: var-info,
		   definer: assign, definer-next: new-defines);
    let dep = make(<dependency>, source-exp: var, source-next: #f,
		   dependent: target, dependent-next: new-depends-on);
    var.dependents := dep;
    new-defines := var;
    new-depends-on := dep;
  end;
  assign.defines := new-defines;
  for (dep = target.depends-on then dep.dependent-next,
       prev = #f then dep,
       until: dep == cluster-dependency)
  finally
    if (prev)
      prev.dependent-next := new-depends-on;
    else
      target.depends-on := new-depends-on;
    end;
  end;
  reoptimize(component, assign);
  let assign-source = assign.depends-on.source-exp;
  if (instance?(assign-source, <primitive>)
	& assign-source.name == #"values")
    reoptimize(component, assign-source);
  end;
end;

define method expand-cluster 
    (component :: <component>, cluster :: <initial-variable>,
     number-of-values :: <fixed-integer>, names :: <list>)
    => ();
  let cluster-dependency = cluster.dependents;
  let target = cluster-dependency.dependent;
  let assigns = map(definer, cluster.definitions);
  let new-defines = make(<list>, size: cluster.definitions.size, fill: #f);
  let new-depends-on = cluster-dependency.dependent-next;
  for (index from number-of-values - 1 to 0 by -1,
       names = names then names.tail)
    let debug-name = if (names == #())
		       as(<symbol>, format-to-string("result%d", index));
		     else
		       names.head;
		     end;
    let var-info = make(<local-var-info>, debug-name: debug-name,
			asserted-type: object-ctype());
    let var = make(<initial-variable>, var-info: var-info,
		   next-initial-variable: component.initial-variables,
		   component: component);
    component.initial-variables := var;
    let defns = map(method (assign, next-define)
		      make(<initial-definition>, var-info: var-info,
			   definition: var, definer: assign,
			   definer-next: next-define);
		    end,
		    assigns, new-defines);
    let dep = make(<dependency>, source-exp: var, source-next: #f,
		   dependent: target, dependent-next: new-depends-on);
    var.dependents := dep;
    new-defines := defns;
    new-depends-on := dep;
  end;
  for (assign in assigns, defn in new-defines)
    assign.defines := defn;
  end;
  for (dep = target.depends-on then dep.dependent-next,
       prev = #f then dep,
       until: dep == cluster-dependency)
  finally
    if (prev)
      prev.dependent-next := new-depends-on;
    else
      target.depends-on := new-depends-on;
    end;
  end;
  for (assign in assigns)
    reoptimize(component, assign);
    let assign-source = assign.depends-on.source-exp;
    if (instance?(assign-source, <primitive>)
	  & assign-source.name == #"values")
      reoptimize(component, assign-source);
    end;
  end;
end;

define method let-convert
    (component :: <component>, function :: <fer-function-region>,
     call :: <known-call>)
    => ();
  let call-assign :: <assignment> = call.dependents.dependent;
  let new-home = home-function-region(call-assign);

  assert(~(new-home == function));

  let builder = make-builder(component);
  let call-policy = call-assign.policy;
  let call-source = call-assign.source-location;

  // Extract the function body and replace it with an empty region.  This is
  // so that when we drop the last reference to the function-literal leaf,
  // that doesn't trigger a deletion walk of the entire body.
  let function-body = function.body;
  replace-subregion(component, function, function-body, make(<empty-region>));

  // Define a bunch of temporaries from the call args and insert it before
  // the call assignment.
  let arg-temps
    = begin
	let temps = make(<stretchy-vector>);
	for (dep = call.depends-on.dependent-next then dep.dependent-next,
	     arg-type in function.argument-types,
	     arg = function.prologue.dependents.dependent.defines
	       then arg & arg.definer-next,
	     index from 0)
	  unless (dep)
	    error("Wrong number of argument in let-convert?");
	  end;
	  let debug-name = if (arg)
			     arg.var-info.debug-name;
			   else
			     as(<symbol>, format-to-string("arg%d", index));
			   end;
	  let temp = make-local-var(builder, debug-name, arg-type);
	  add!(temps, temp);
	  build-assignment(builder, call-policy, call-source,
			   temp, dep.source-exp);
	finally
	  if (dep)
	    error("Wrong number of arguments in let-convert?");
	  end;
	end;
	insert-before(component, call-assign, builder-result(builder));
	as(<list>, temps);
      end;

  // Replace the prologue with the arg-temps.
  replace-expression(component, function.prologue.dependents,
		     make-operation(builder, <primitive>, arg-temps,
				    name: #"values"));

  // For each self-tail-call, change it into an assignment of the arg temps.
  if (function.self-tail-calls)
    // But first, peel off the temps that correspond to closure vars.
    for (closure-var = function.environment.closure-vars
	   then closure-var.closure-next,
	 temps = arg-temps then temps.tail,
	 while: closure-var)
    finally
      for (self-tail-call = function.self-tail-calls
	     then self-tail-call.next-self-tail-call,
	   while: self-tail-call)
	let assign = self-tail-call.dependents.dependent;
	for (dep = self-tail-call.depends-on then dep.dependent-next,
	     args = #() then pair(dep.source-exp, args),
	     while: dep)
	finally
	  let op = make-operation(builder, <primitive>, reverse!(args),
				  name: #"values");
	  build-assignment(builder, assign.policy, assign.source-location,
			   temps, op);
	  insert-before(component, assign, builder-result(builder));
	  delete-and-unlink-assignment(component, assign);
	end;
      end;
    end;
  end;

  // If there are any returns, change them into assignments of a cluster
  // and change the call to a reference to that cluster.
  if (function.exits)
    let results-temp = make-values-cluster(builder, #"results", wild-ctype());

    // Wrap the function body in a block so we can return to it.
    let body-block = build-block-body(builder, call-policy, call-source);
    build-region(builder, function-body);
    end-body(builder);

    // Insert that block before the original call site.
    insert-before(component, call-assign, builder-result(builder));

    // Replace each return with an assignment of the result cluster
    // followed by an exit to the body-block.
    while (function.exits)
      let return = function.exits;
      let source = return.source-location;
      let results = return.depends-on;
      if (results & instance?(results.source-exp, <abstract-variable>)
	    & instance?(results.source-exp.var-info, <values-cluster-info>))
	build-assignment(builder, call-policy, source,
			 results-temp, results.source-exp);
      else
	// Make a values operation stealing the results from the return.
	let op = make(<primitive>, derived-type: return.returned-type,
		      name: #"values", depends-on: results);
	for (dep = results then dep.dependent-next,
	     while: dep)
	  dep.dependent := op;
	end;
	return.depends-on := #f;
	reoptimize(component, op);
	// Assign the result temp with the values call.
	build-assignment(builder, call-policy, source, results-temp, op);
      end;
      build-exit(builder, call-policy, return.source-location, body-block);

      replace-subregion(component, return.parent, return,
			builder-result(builder));
      delete-stuff-in(component, return);
    end;

    // Replace the call with a reference to the result cluster.  We need to
    // do this after replacing the returns because this triggers the deletion
    // of the function while replacing returns will trigger a queueing of
    // the function.
    replace-expression(component, call-assign.depends-on, results-temp);
  else
    // Insert the function body before the call assignment.
    insert-before(component, call-assign, function-body);
    // Insert an exit to the component after the call assignment.
    insert-exit-after(component, call-assign, component);
    // And delete the call assignment.
    delete-and-unlink-assignment(component, call-assign);
  end;

  // Queue the catchers for blocks in the new home that are exited to from
  // the function's body.
  queue-throws(component, new-home, new-home.body);
end;

define method queue-throws
    (component :: <component>, home :: <fer-function-region>,
     region :: <simple-region>)
    => ();
  for (assign = region.first-assign then assign.next-op,
       while: assign)
    if (instance?(assign, <let-assignment>))
      reoptimize(component, assign);
    end;
    let expr = assign.depends-on.source-exp;
    if (instance?(expr, <throw>))
      reoptimize(component, expr);
    end;
  end;
end;

define method queue-throws
    (component :: <component>, home :: <fer-function-region>,
     region :: <compound-region>)
    => ();
  for (subregion in region.regions)
    queue-throws(component, home, subregion);
  end;
end;

define method queue-throws
    (component :: <component>, home :: <fer-function-region>,
     region :: <if-region>)
    => ();
  queue-throws(component, home, region.then-region);
  queue-throws(component, home, region.else-region);
end;

define method queue-throws
    (component :: <component>, home :: <fer-function-region>,
     region :: <body-region>)
    => ();
  queue-throws(component, home, region.body);
end;

define method queue-throws
    (component :: <component>, home :: <fer-function-region>,
     region :: <exit>)
    => ();
end;



// If optimizations.

define method optimize (component :: <component>, if-region :: <if-region>)
    => ();
  let condition = if-region.depends-on.source-exp;
  let condition-type = condition.derived-type;
  let false = dylan-value(#"<false>");
  if (csubtype?(condition-type, false))
    replace-if-with(component, if-region, if-region.else-region);
    delete-stuff-in(component, if-region.then-region);
  elseif (~ctypes-intersect?(false, condition-type))
    replace-if-with(component, if-region, if-region.then-region);
    delete-stuff-in(component, if-region.else-region);
  elseif (instance?(if-region.then-region, <empty-region>)
	    & instance?(if-region.else-region, <empty-region>))
    replace-if-with(component, if-region, make(<empty-region>));
  elseif (instance?(condition, <ssa-variable>))
    let cond-source = condition.definer.depends-on.source-exp;
    if (instance?(cond-source, <primitive>) & cond-source.name == #"not")
      replace-expression(component, if-region.depends-on,
			 cond-source.depends-on.source-exp);
      let then-region = if-region.then-region;
      if-region.then-region := if-region.else-region;
      if-region.else-region := then-region;
    end;
  end;
end;

define method replace-if-with
    (component :: <component>, if-region :: <if-region>, with :: <region>)
    => ();
  let builder = make-builder(component);
  build-assignment(builder, $Default-Policy, if-region.source-location,
		   #(), if-region.depends-on.source-exp);
  build-region(builder, with);
  replace-subregion(component, if-region.parent, if-region,
		    builder-result(builder));
  delete-dependent(component, if-region);
end;



// Type utilities.

define method optimize (component :: <component>, op :: <truly-the>) => ();
  let (intersection, win)
    = ctype-intersection(op.depends-on.source-exp.derived-type,
			 op.guaranteed-type);
  maybe-restrict-type(component, op,
		      if (win)
			intersection;
		      else
			op.guaranteed-type;
		      end);
end;

define method assert-type
    (component :: <component>, before :: <assignment>,
     dependent :: <dependency>, type :: <ctype>)
    => ();
  let source = dependent.source-exp;
  unless (csubtype?(source.derived-type, type))
    let builder = make-builder(component);
    let temp = make-ssa-var(builder, #"temp", type);
    build-assignment(builder, before.policy, before.source-location,
		     temp, source);
    for (dep = source.dependents then dep.source-next,
	 prev = #f then dep,
	 until: dep == dependent)
    finally
      if (prev)
	prev.source-next := dep.source-next;
      else
	source.dependents := dep.source-next;
      end;
    end;
    dependent.source-exp := temp;
    temp.dependents := dependent;
    dependent.source-next := #f;
    insert-before(component, before, builder-result(builder));
  end;
end;

define method maybe-restrict-type
    (component :: <component>, expr :: <expression>, type :: <values-ctype>)
    => ();
  unless (type == wild-ctype())
    let old-type = expr.derived-type;
    if (old-type == wild-ctype() 
	  | (~values-subtype?(old-type, type)
	       & values-subtype?(type, old-type)))
      expr.derived-type := type;
      if (instance?(expr, <initial-definition>))
	let var = expr.definition-of;
	if (instance?(var, <initial-variable>))
	  block (return)
	    let var-type = empty-ctype();
	    for (defn in var.definitions)
	      let (res, win) = values-type-union(var-type, defn.derived-type);
	      if (win)
		var-type := res;
	      else
		return();
	      end;
	    finally
	      maybe-restrict-type(component, var, var-type);
	    end for;
	  end block;
	end if;
      end if;
      queue-dependents(component, expr);
    end if;
  end unless;
end method maybe-restrict-type;


define method maybe-restrict-type
    (component :: <component>, var :: <abstract-variable>,
     type :: <values-ctype>, #next next-method)
    => ();
  let var-info = var.var-info;
  next-method(component, var,
	      if (instance?(var-info, <values-cluster-info>))
		values-type-intersection(type, var-info.asserted-type);
	      else
		ctype-intersection(defaulted-first-type(type),
				   var-info.asserted-type);
	      end);
end;

define method maybe-restrict-type
    (component :: <component>, var :: <definition-site-variable>,
     type :: <values-ctype>, #next next-method)
    => ();
  if (var.needs-type-check?
	& values-subtype?(type, var.var-info.asserted-type))
    var.needs-type-check? := #f;
    reoptimize(component, var.definer);
  end;
  next-method();
end;

define method defaulted-first-type (ctype :: <ctype>) => res :: <ctype>;
  ctype;
end;

define method defaulted-first-type (ctype :: <values-ctype>) => res :: <ctype>;
  let positionals = ctype.positional-types;
  if (positionals == #())
    ctype-union(ctype.rest-value-type, dylan-value(#"<false>"));
  elseif (zero?(ctype.min-values))
    ctype-union(positionals[0], dylan-value(#"<false>"));    
  else
    positionals[0];
  end;
end;

define method fixed-number-of-values? (ctype :: <ctype>) => res :: <boolean>;
  #t;
end;

define method fixed-number-of-values?
    (ctype :: <values-ctype>) => res :: <boolean>;
  ctype.min-values == ctype.positional-types.size
    & ctype.rest-value-type == empty-ctype();
end;


// Tail call identification.


define method identify-tail-calls (component :: <component>) => ();
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
	  identify-tail-calls-before(component, home, results, in.parent, in);
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
	  // It's a self tail call.
	  convert-self-tail-call(component, home, expr);
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
	elseif (instance?(expr, <primitive>) & expr.name == #"values")
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


define method convert-self-tail-call
    (component :: <component>, func :: <fer-function-region>,
     call :: <abstract-call>)
    => ();
  // Set up the wrapper loop and blocks.
  unless (func.self-call-block)
    let builder = make-builder(component);
    let source = func.source-location;
    build-loop-body(builder, $Default-Policy, source);
    func.self-call-block := build-block-body(builder, $Default-Policy, source);
    build-region(builder, func.body);
    end-body(builder); // end of the self call block
    end-body(builder); // end of the loop
    replace-subregion(component, func, func.body, builder-result(builder));
  end;
  // Change the call into a self-tail-call operation.
  let op = make(<self-tail-call>, dependents: call.dependents,
		depends-on: call.depends-on.dependent-next,
		next-self-tail-call: func.self-tail-calls, of: func);
  func.self-tail-calls := op;
  remove-dependency-from-source(component, call.depends-on);
  for (dep = op.depends-on then dep.dependent-next,
       while: dep)
    dep.dependent := op;
  end;
  let assign-dep = call.dependents;
  assign-dep.source-exp := op;
  assert(~assign-dep.source-next);
  // Insert the exit to self-call-block after the self-tail-call assignment.
  let assign = assign-dep.dependent;
  insert-exit-after(component, assign, func.self-call-block);
  // Delete the definitions for the assignment.
  for (defn = assign.defines then defn.definer-next,
       while: defn)
    delete-definition(component, defn);
  end;
  assign.defines := #f;
  // Queue the assignment and self-tail-call operation.
  reoptimize(component, op);
  reoptimize(component, assign);
end;


// Control flow cleanup stuff.

define method cleanup-control-flow (component :: <component>) => ();
  for (function in component.all-function-regions)
    if (cleanup-control-flow-aux(component, function.body) == #f)
      error("control flow drops off the end of %=?", function);
    end;
  end;
end;

define method cleanup-control-flow-aux
    (component :: <component>, region :: <simple-region>)
    => terminating-exit :: type-union(<exit>, <boolean>);
  #f;
end;

define method cleanup-control-flow-aux
    (component :: <component>, region :: <compound-region>)
    => terminating-exit :: type-union(<exit>, <boolean>);
  block (return)
    for (remaining = region.regions then remaining.tail,
	 until: remaining == #())
      let terminating-exit
	= cleanup-control-flow-aux(component, remaining.head);
      if (terminating-exit)
	unless (remaining.tail == #())
	  for (subregion in remaining.tail)
	    delete-stuff-in(component, subregion);
	  end;
	  remaining.tail := #();
	  if (region.regions.tail == #())
	    replace-subregion(component, region.parent, region,
			      region.regions.head);
	  end;
	end;
	return(terminating-exit);
      end;
    end;
    #f;
  end;
end;

define method cleanup-control-flow-aux
    (component :: <component>, region :: <if-region>)
    => terminating-exit :: type-union(<exit>, <boolean>);
  let then-terminating-exit
    = cleanup-control-flow-aux(component, region.then-region);
  let else-terminating-exit
    = cleanup-control-flow-aux(component, region.else-region);
  if (then-terminating-exit & else-terminating-exit)
    for (then-target-ancestor = then-terminating-exit.block-of
	   then then-target-ancestor.parent,
	 else-target-ancestor = else-terminating-exit.block-of
	   then else-target-ancestor.parent,
	 while: then-target-ancestor & else-target-ancestor)
    finally
      if (then-target-ancestor == #f)
	else-terminating-exit;
      else
	then-terminating-exit;
      end;
    end;
  else
    #f;
  end;
end;

define method cleanup-control-flow-aux
    (component :: <component>, region :: <loop-region>)
    => terminating-exit :: type-union(<exit>, <boolean>);
  if (cleanup-control-flow-aux(component, region.body))
    // ### Hm.  Should flush this region, but that will cause all sorts of
    // problems with the iteration in <compound-region> above.
    #f;
  end;
  #f;
end;

define method cleanup-control-flow-aux
    (component :: <component>, region :: <block-region>)
    => terminating-exit :: type-union(<exit>, <boolean>);
  let terminating-exit = cleanup-control-flow-aux(component, region.body);
  if (instance?(terminating-exit, <exit>)
	& terminating-exit.block-of == region)
    delete-stuff-in(component, terminating-exit);
    replace-subregion(component, terminating-exit.parent, terminating-exit,
		      make(<empty-region>));
  end;
  #f;
end;

define method cleanup-control-flow-aux
    (component :: <component>, region :: <unwind-protect-region>)
    => terminating-exit :: type-union(<exit>, <boolean>);
  cleanup-control-flow-aux(component, region.body);
end;

define method cleanup-control-flow-aux
    (component :: <component>, region :: <exit>)
    => terminating-exit :: type-union(<exit>, <boolean>);
  region;
end;



// Cheesy type check stuff.


define method add-type-checks (component :: <component>) => ();
  for (function in component.all-function-regions)
    add-type-checks-aux(component, function);
  end;
end;

define method add-type-checks-aux
    (component :: <component>, region :: <simple-region>) => ();
  let next-assign = #f;
  for (assign = region.first-assign then next-assign,
       while: assign)
    let builder = #f;
    next-assign := assign.next-op;
    for (defn = assign.defines then defn.definer-next,
	 prev = #f then defn,
	 while: defn)
      if (defn.needs-type-check?)
	if (instance?(defn.var-info, <values-cluster-info>))
	  error("values cluster needs a type check?");
	end;
	// Make the builder if we haven't already.
	unless (builder)
	  builder := make-builder(component);
	end;
	// Make a temp to hold the unchecked value.
	let temp = make-ssa-var(builder, #"temp", object-ctype());
	// Link the temp in in place of this definition.
	temp.definer := assign;
	temp.definer-next := defn.definer-next;
	defn.definer-next := #f;
	if (prev)
	  prev.definer-next := temp;
	else
	  assign.defines := temp;
	end;
	// Do the type check.
	let checked = make-ssa-var(builder, #"checked", object-ctype());
	let asserted-type = defn.var-info.asserted-type;
	build-assignment
	  (builder, assign.policy, assign.source-location, checked,
	   make-unknown-call
	     (builder,
	      ref-dylan-defn(builder, assign.policy, assign.source-location,
			     #"%check-type"),
	      #f, list(temp, make-literal-constant(builder, asserted-type))));
	// Assign the type checked value to the real var.
	build-assignment
	  (builder, assign.policy, assign.source-location, defn,
	   make-operation(builder, <truly-the>, list(checked),
			  guaranteed-type: asserted-type));
	// Change defn to temp so that the loop steps correctly.
	defn := temp;
      end;
    end;
    if (builder)
      // We built some type checks, so insert them.
      insert-after(component, assign, builder-result(builder));
      // Queue the assignment for reoptimization.
      reoptimize(component, assign);
    end;
  end;
end;

define method add-type-checks-aux
    (component :: <component>, region :: <compound-region>) => ();
  for (subregion in region.regions)
    add-type-checks-aux(component, subregion);
  end;
end;

define method add-type-checks-aux
    (component :: <component>, region :: <if-region>) => ();
  add-type-checks-aux(component, region.then-region);
  add-type-checks-aux(component, region.else-region);
end;

define method add-type-checks-aux
    (component :: <component>, region :: <body-region>) => ();
  add-type-checks-aux(component, region.body);
end;

define method add-type-checks-aux
    (component :: <component>, region :: <exit>) => ();
end;



// Replacement of placeholder

define method replace-placeholders (component :: <component>) => ();
  for (function in component.all-function-regions)
    replace-placeholders-in(component, function);
  end;
end;

define method replace-placeholders-in
    (component :: <component>, region :: <simple-region>) => ();
  for (assign = region.first-assign then assign.next-op,
       while: assign)
    replace-placeholder(component, assign.depends-on,
			assign.depends-on.source-exp);
  end;
end;

define method replace-placeholders-in
    (component :: <component>, region :: <compound-region>) => ();
  for (subregion in region.regions)
    replace-placeholders-in(component, subregion);
  end;
end;

define method replace-placeholders-in
    (component :: <component>, region :: <if-region>) => ();
  replace-placeholder(component, region.depends-on,
		      region.depends-on.source-exp);
  replace-placeholders-in(component, region.then-region);
  replace-placeholders-in(component, region.else-region);
end;

define method replace-placeholders-in
    (component :: <component>, region :: <body-region>) => ();
  replace-placeholders-in(component, region.body);
end;

define method replace-placeholders-in
    (component :: <component>, region :: <exit>) => ();
end;
  
define method replace-placeholders-in
    (component :: <component>, region :: <return>) => ();
  for (dep = region.depends-on then dep.dependent-next,
       while: dep)
    replace-placeholder(component, dep, dep.source-exp);
  end;
end;
  
define method replace-placeholder
    (component :: <component>, dep :: <dependency>,
     placeholder :: <expression>)
    => ();
end;

define method replace-placeholder
    (component :: <component>, dep :: <dependency>, op :: <operation>)
    => ();
  for (dep = op.depends-on then dep.dependent-next,
       while: dep)
    replace-placeholder(component, dep, dep.source-exp);
  end;
end;

define method replace-placeholder
    (component :: <component>, dep :: <dependency>, op :: <primitive>)
    => ();
  for (dep = op.depends-on then dep.dependent-next,
       while: dep)
    replace-placeholder(component, dep, dep.source-exp);
  end;
  select (op.name)
    #"vector" => 
      let builder = make-builder(component);
      let assign = dep.dependent;
      let policy = assign.policy;
      let source = assign.source-location;
      let len = for (dep = op.depends-on then dep.dependent-next,
		     count from 0,
		     while: dep)
		finally
		  count;
		end;
      let vec = make-local-var(builder, #"vector",
			       specifier-type(#"<simple-object-vector>"));
      build-assignment
	(builder, policy, source, vec,
	 make-unknown-call
	   (builder, ref-dylan-defn(builder, policy, source, #"make"), #f,
	    list(ref-dylan-defn(builder, policy, source,
				#"<simple-object-vector>"),
		 make-literal-constant(builder, as(<ct-value>, size:)),
		 make-literal-constant(builder, as(<ct-value>, len)))));
      for (dep = op.depends-on then dep.dependent-next,
	   index from 0,
	   while: dep)
	build-assignment
	  (builder, policy, source, #(),
	   make-unknown-call
	     (builder,
	      ref-dylan-defn(builder, policy, source, #"%element-setter"),
	      #f,
	      list(dep.source-exp, vec,
		   make-literal-constant(builder, as(<ct-value>, index)))));
      end;
      insert-before(component, assign, builder-result(builder));
      replace-expression(component, dep, vec);

    otherwise => #f;
  end;
end;

define method replace-placeholder
    (component :: <component>, dep :: <dependency>, leaf :: <exit-function>)
    => ();
  leaf.nlx-info.nlx-hidden-references? := #t;
  let builder = make-builder(component);
  let catcher = leaf.depends-on.source-exp;
  let policy = $Default-Policy;
  let source = make(<source-location>);
  let make-exit-fun-leaf = ref-dylan-defn(builder, policy, source,
					  #"make-exit-function");
  let temp = make-local-var(builder, #"exit-function", function-ctype());
  build-assignment
    (builder, policy, source, temp,
     make-unknown-call(builder, make-exit-fun-leaf, #f, list(catcher)));
  insert-before(component, dep.dependent, builder-result(builder));
  replace-expression(component, dep, temp);
end;


define method replace-placeholder
    (component :: <component>, dep :: <dependency>, op :: <make-catcher>)
    => ();
  op.nlx-info.nlx-hidden-references? := #t;
  let builder = make-builder(component);
  let catcher = op.depends-on.source-exp;
  let assign = op.dependents.dependent;
  let func = ref-dylan-defn(builder, assign.policy, assign.source-location,
			    #"make-catcher");
  insert-before(component, assign, builder-result(builder));
  replace-expression(component, dep,
		     make-unknown-call(builder, func, #f, list(catcher)));
end;
  
define method replace-placeholder
    (component :: <component>, dep :: <dependency>, op :: <disable-catcher>)
    => ();
  op.nlx-info.nlx-hidden-references? := #t;
  let builder = make-builder(component);
  let catcher = op.depends-on.source-exp;
  let assign = op.dependents.dependent;
  let func = ref-dylan-defn(builder, assign.policy, assign.source-location,
			    #"disable-catcher");
  insert-before(component, assign, builder-result(builder));
  replace-expression(component, dep,
		     make-unknown-call(builder, func, #f, list(catcher)));
end;
  
define method replace-placeholder
    (component :: <component>, dep :: <dependency>, op :: <throw>) => ();
  op.nlx-info.nlx-hidden-references? := #t;
  let builder = make-builder(component);
  let assign = dep.dependent;
  let catcher = op.depends-on.source-exp;
  let cluster = op.depends-on.dependent-next.source-exp;
  let temp = make-local-var(builder, #"values", object-ctype());
  let zero-leaf = make-literal-constant(builder, as(<ct-value>, 0));
  build-assignment(builder, assign.policy, assign.source-location, temp,
		   make-operation(builder, <primitive>,
				  list(cluster, zero-leaf),
				  name: #"canonicalize-results"));
  let func = ref-dylan-defn(builder, assign.policy, assign.source-location,
			    #"throw");
  insert-before(component, assign, builder-result(builder));
  replace-expression(component, dep,
		     make-unknown-call(builder, func, #f,
				       list(op.depends-on.source-exp, temp)));
end;


// Environment analysis

define method environment-analysis (component :: <component>) => ();
  let lets = component.all-lets;
  component.all-lets := #f;
  for (l = lets then l.let-next, while: l)
    unless (l.queue-next == #"deleted")
      let home = home-function-region(l);
      let next = #f;
      for (var = l.defines then next,
	   while: var)
	next := var.definer-next;
	maybe-close-over(component, var, home);
      end;
      reoptimize(component, l);
    end;
  end;
end;

define method home-function-region (op :: <operation>)
    => home :: <fer-function-region>;
  home-function-region(op.dependents.dependent);
end;

define method home-function-region (assign :: <assignment>)
    => home :: <fer-function-region>;
  home-function-region(assign.region);
end;

define method home-function-region (region :: <region>)
    => home :: <fer-function-region>;
  home-function-region(region.parent);
end;

define method home-function-region (function :: <fer-function-region>)
    => home :: <fer-function-region>;
  function;
end;

define method maybe-close-over
    (component :: <component>, var :: <ssa-variable>,
     home :: <fer-function-region>)
    => ();
  let orig-dependents = var.dependents;
  var.dependents := #f;
  let next = #f;
  for (dep = orig-dependents then next,
       while: dep)
    next := dep.source-next;
    let ref = dep.dependent;
    let ref-function = home-function-region(ref);
    let copy = find-in-environment(component, ref-function, var, home);
    dep.source-next := copy.dependents;
    copy.dependents := dep;
    dep.source-exp := copy;
  end;
end;

define method find-in-environment
    (component :: <component>, function :: <fer-function-region>,
     var :: <ssa-variable>, home :: <fer-function-region>)
    => copy :: <ssa-variable>;
  unless (function == home)
    error("%= can't close over %= because it isn't a lambda.", function, var);
  end;
  var;
end;

define method find-in-environment
    (component :: <component>, function :: <lambda>,
     var :: <ssa-variable>, home :: <fer-function-region>)
    => copy :: <ssa-variable>;
  if (function == home)
    var;
  else
    block (return)
      for (closure = function.environment.closure-vars
	     then closure.closure-next,
	   while: closure)
	if (closure.original-var == var)
	  return(closure.copy-var);
	end;
      end;
      if (function.literal.visibility == #"global")
	error("%= can't close over %= because it has hidden references.",
	      function, var);
      end;
      let prologue = function.prologue;
      let assign = prologue.dependents.dependent;
      let copy = make(<ssa-variable>, var-info: var.var-info, definer: assign,
		      definer-next: assign.defines,
		      derived-type: var.derived-type);
      assign.defines := copy;
      function.environment.closure-vars
	:= make(<closure-var>, original: var, copy: copy,
		next: function.environment.closure-vars);
      function.argument-types
	:= pair(var.derived-type, function.argument-types);
      prologue.derived-type := wild-ctype();
      let next-dep = #f;
      for (ref-dep = function.literal.dependents then next-dep,
	   while: ref-dep)
	next-dep := ref-dep.source-next;
	let ref = ref-dep.dependent;
	let var-at-ref
	  = find-in-environment(component, home-function-region(ref),
				var, home);
	if ((instance?(ref, <known-call>)
	       | (instance?(ref, <primitive>) & ref.name == #"make-closure"))
	      & ref.depends-on == ref-dep)
	  let new-dep = make(<dependency>, source-exp: var-at-ref,
			     source-next: var-at-ref.dependents,
			     dependent: ref,
			     dependent-next: ref-dep.dependent-next);
	  var-at-ref.dependents := new-dep;
	  ref-dep.dependent-next := new-dep;
	  reoptimize(component, ref);
	else
	  let builder = make-builder(component);
	  let op = make-operation(builder, <primitive>,
				  list(function.literal, var-at-ref),
				  name: #"make-closure");
	  let temp = make-local-var(builder, #"closure", function-ctype());
	  build-assignment(builder, $Default-policy, make(<source-location>),
			   temp, op);
	  insert-before(component, ref-dep.dependent, builder-result(builder));
	  replace-expression(component, ref-dep, temp);
	end;
      end;
      reoptimize(component, prologue);
      copy;
    end;
  end;
end;

define method maybe-close-over
    (component :: <component>, defn :: <initial-definition>,
     home :: <fer-function-region>)
    => ();
  let var = defn.definition-of;
  if (block (return)
	for (defn in var.definitions)
	  unless (home-function-region(defn.definer) == home)
	    return(#t);
	  end;
	end;
	for (dep = var.dependents then dep.source-next,
	     while: dep)
	  unless (home-function-region(dep.dependent) == home)
	    return(#t);
	  end;
	end;
	#f;
      end)
    let value-cell-type = dylan-value(#"<value-cell>");
    let builder = make-builder(component);
    let value-cell = make(<ssa-variable>,
			  var-info: make(<lexical-var-info>,
					 debug-name: var.var-info.debug-name,
					 asserted-type: value-cell-type,
					 source-location:
					   var.var-info.source-location),
			  derived-type: value-cell-type);
    for (defn in var.definitions)
      let temp = make-ssa-var(builder, var.var-info.debug-name,
			      defn.derived-type);
      let assign = defn.definer;
      for (other = assign.defines then other.definer-next,
	   prev = #f then other,
	   until: other == defn)
      finally
	if (prev)
	  prev.definer-next := temp;
	else
	  assign.defines := temp;
	end;
	temp.definer-next := other.definer-next;
      end;
      temp.definer := assign;
      select (defn.definer by instance?)
	<let-assignment> =>
	  let make-leaf
	    = ref-dylan-defn(builder, assign.policy, assign.source-location,
			     #"make");
	  let value-cell-type-leaf
	    = make-literal-constant(builder, value-cell-type);
	  let value-keyword-leaf
	    = make-literal-constant(builder,
				    make(<literal-symbol>, value: #"value"));
	  let op
	    = make-unknown-call(builder, make-leaf, #f, 
				list(value-cell-type-leaf, value-keyword-leaf,
				     temp));
	  op.derived-type := value-cell-type;
	  build-assignment
	    (builder, assign.policy, assign.source-location, value-cell, op);
	<set-assignment> =>
	  let value-setter
	    = ref-dylan-defn(builder, assign.policy, assign.source-location,
			     #"value-setter");
	  build-assignment
	    (builder, assign.policy, assign.source-location, #(),
	     make-unknown-call(builder, value-setter, #f,
			       list(temp, value-cell)));
      end;
      insert-after(component, assign, builder-result(builder));
      reoptimize(component, assign);
    end;
    let next = #f;
    for (dep = var.dependents then next,
	 while: dep)
      next := dep.source-next;
      let temp = make-ssa-var(builder, var.var-info.debug-name,
			      var.derived-type);
      dep.source-exp := temp;
      temp.dependents := dep;
      dep.source-next := #f;
      let value
	= ref-dylan-defn(builder, $Default-Policy, make(<source-location>),
			 #"value");
      let op = make-unknown-call(builder, value, #f, list(value-cell));
      op.derived-type := var.derived-type;
      build-assignment(builder, $Default-Policy, make(<source-location>),
		       temp, op);
      insert-before(component, dep.dependent, builder-result(builder));
    end;
    maybe-close-over(component, value-cell, home);
  end;
end;

define-primitive-transformer
  (#"make-closure",
   method (component :: <component>, primitive :: <primitive>) => ();
     let builder = make-builder(component);
     let assign = primitive.dependents.dependent;
     let policy = assign.policy;
     let source = assign.source-location;
     let func = primitive.depends-on.source-exp;
     assert(instance?(func, <function-literal>));
     func.visibility := #"global";
     let closure-size
       = for (dep = primitive.depends-on.dependent-next
		then dep.dependent-next,
	      res from 0,
	      while: dep)
	 finally
	   res;
	 end;
     func.visibility := #"global";
     let ctv
       = (func.ct-function
	    | (func.ct-function
		 := make(if (instance?(func, <method-literal>))
			   <ct-method>;
			 else
			   <ct-function>;
			 end,
			 name: func.main-entry.name,
			 signature: func.signature,
			 closure-var-types:
			   for (var = func.main-entry.environment.closure-vars
				  then var.closure-next,
				results = #()
				  then pair(var.original-var.derived-type,
					    results),
				while: var)
			   finally
			     reverse!(results);
			   end)));
     let var = make-local-var(builder, #"closure", object-ctype());
     build-assignment
       (builder, policy, source, var,
	make-unknown-call
	  (builder, ref-dylan-defn(builder, policy, source, #"make-closure"),
	   #f,
	   list(make-literal-constant(builder, ctv),
		make-literal-constant(builder,
				      as(<ct-value>, closure-size)))));
     let closure-var-setter-leaf
       = ref-dylan-defn(builder, policy, source, #"closure-var-setter");
     for (dep = primitive.depends-on.dependent-next then dep.dependent-next,
	  index from 0,
	  while: dep)
       build-assignment
	 (builder, policy, source, #(),
	  make-unknown-call
	    (builder, closure-var-setter-leaf, #f,
	     list(dep.source-exp,
		  var,
		  make-literal-constant(builder, as(<ct-value>, index)))));
     end;
     insert-before(component, assign, builder-result(builder));
     replace-expression(component, assign.depends-on, var);
   end);


// External entry construction.

define method build-external-entries (component :: <component>) => ();
  for (function in component.all-function-literals)
    if (function.general-entry == #f)
      maybe-build-external-entries-for(component, function);
    end;
  end;
end;

define method maybe-build-external-entries-for
    (component :: <component>, func :: <function-literal>) => ();
  select(func.visibility)
    #"global" =>
      build-external-entries-for(component, func);
    #"local" =>
      block (return)
	for (dep = func.dependents then dep.source-next,
	     while: dep)
	  let dependent = dep.dependent;
	  unless (instance?(dependent, <known-call>)
		    & dependent.depends-on == dep)
	    build-external-entries-for(component, func);
	    return();
	  end;
	end;
      end;
  end;
end;

define method build-external-entries-for
    (component :: <component>, function :: <function-literal>) => ();
  function.general-entry
    := build-xep(component, #f, function, function.signature);
end;

define method build-external-entries-for
    (component :: <component>, function :: <method-literal>) => ();
  let ctv = function.ct-function;
  unless (ctv & ctv.ct-method-hidden?)
    function.general-entry
      := build-xep(component, #f, function, function.signature);
  end;
  unless (function.generic-entry)
    function.generic-entry
      := build-xep(component, #t, function, function.signature);
  end;
end;


define method build-xep
    (component :: <component>, generic-entry? :: <boolean>,
     function :: <function-literal>, signature :: <signature>)
    => xep :: <fer-function-region>;
  let main-entry = function.main-entry;
  let builder = make-builder(component);
  let policy = $Default-Policy;
  let source = make(<source-location>);
  let closure?
    = instance?(main-entry, <lambda>) & main-entry.environment.closure-vars;
  let self-leaf
    = make-local-var(builder, #"self",
		     specifier-type(if (instance?(function, <method-literal>))
				      if (closure?)
					#"<method-closure>";
				      else
					#"<method>";
				      end;
				    else
				      if (closure?)
					#"<raw-closure>";
				      else
					#"<raw-function>";
				      end;
				    end));
  let nargs-leaf = make-local-var(builder, #"nargs", 
				  dylan-value(#"<fixed-integer>"));
  let next-info-leaf
    = generic-entry? & make-local-var(builder, #"next-method-info",
				      dylan-value(#"<list>"));
  let name = format-to-string("%s entry for %s",
			      if (generic-entry?) "Generic" else "General" end,
			      main-entry.name);
  let xep = build-function-body(builder, policy, source, #f, name,
				if (generic-entry?)
				  list(self-leaf, nargs-leaf, next-info-leaf);
				else
				  list(self-leaf, nargs-leaf);
				end,
				wild-ctype(), #t);
  let new-args = make(<stretchy-vector>);
  if (instance?(main-entry, <lambda>) & main-entry.environment.closure-vars)
    let closure-ref-leaf = ref-dylan-defn(builder, policy, source,
					  #"closure-var");
    for (closure-var = main-entry.environment.closure-vars
	   then closure-var.closure-next,
	 index from 0,
	 while: closure-var)
      let copy = closure-var.copy-var;
      let pre-type = make-local-var(builder, copy.var-info.debug-name,
				    object-ctype());
      let index-leaf = make-literal-constant(builder, as(<ct-value>, index));
      build-assignment(builder, policy, source, pre-type,
		       make-unknown-call(builder, closure-ref-leaf, #f,
					 list(self-leaf, index-leaf)));
      let post-type = make-local-var(builder, copy.var-info.debug-name,
				     copy.derived-type);
      build-assignment(builder, policy, source, post-type,
		       make-operation(builder, <truly-the>, list(pre-type),
				      guaranteed-type: copy.derived-type));
      add!(new-args, post-type);
    end;
  end;

  let arg-types = signature.specializers;
  let raw-ptr-type = dylan-value(#"<raw-pointer>");
  let args-leaf = make-local-var(builder, #"args", raw-ptr-type);
  let wanted-leaf
    = make-literal-constant(builder, as(<ct-value>, arg-types.size));
  if (generic-entry?)
    // We don't have to check the number of arguments, we just have to
    // find the arg pointer.
    build-assignment
      (builder, policy, source, args-leaf,
       make-operation(builder, <primitive>,
		      list(if (signature.rest-type | signature.key-infos)
			     nargs-leaf;
			   else
			     wanted-leaf;
			   end),
		      name: #"extract-args"));
  else
    if (signature.rest-type == #f & signature.key-infos == #f)
      let op = make-unknown-call
	(builder, ref-dylan-defn(builder, policy, source, #"=="), #f,
	 list(nargs-leaf, wanted-leaf));
      let temp = make-local-var(builder, #"nargs-okay?", object-ctype());
      build-assignment(builder, policy, source, temp, op);
      build-if-body(builder, policy, source, temp);
      build-else(builder, policy, source);
      build-assignment
	(builder, policy, source, #(),
	 make-error-operation
	   (builder, policy, source, #"wrong-number-of-arguments-error",
	    make-literal-constant(builder, as(<ct-value>, #t)),
	    wanted-leaf, nargs-leaf));
      end-body(builder);
      build-assignment(builder, policy, source, args-leaf,
		       make-operation(builder, <primitive>, list(wanted-leaf),
				      name: #"extract-args"));
    else
      unless (empty?(arg-types))
	let op = make-unknown-call
	  (builder, ref-dylan-defn(builder, policy, source, #"<"), #f,
	   list(nargs-leaf, wanted-leaf));
	let temp = make-local-var(builder, #"nargs-okay?", object-ctype());
	build-assignment(builder, policy, source, temp, op);
	build-if-body(builder, policy, source, temp);
	build-assignment
	  (builder, policy, source, #(),
	   make-error-operation
	   (builder, policy, source, #"wrong-number-of-arguments-error",
	    make-literal-constant(builder, as(<ct-value>, #f)),
	    wanted-leaf, nargs-leaf));
	build-else(builder, policy, source);
	end-body(builder);
      end;
      if (signature.key-infos)
	let func = ref-dylan-defn(builder, policy, source,
				  if (odd?(arg-types.size))
				    #"even?";
				  else
				    #"odd?";
				  end);
	let op = make-unknown-call(builder, func, #f, list(nargs-leaf));
	let temp = make-local-var(builder, #"nkeys-okay?", object-ctype());
	build-assignment(builder, policy, source, temp, op);
	build-if-body(builder, policy, source, temp);
	build-assignment
	  (builder, policy, source, #(),
	   make-error-operation
	     (builder, policy, source,
	      #"odd-number-of-keyword/value-arguments-error"));
	build-else(builder, policy, source);
	end-body(builder);
      end;
      build-assignment(builder, policy, source, args-leaf,
		       make-operation(builder, <primitive>, list(nargs-leaf),
				      name: #"extract-args"));
    end;
  end;

  for (type in arg-types,
       index from 0)
    let temp = make-local-var(builder, #"arg",
			      if (generic-entry?)
				object-ctype();
			      else
				type;
			      end);
    let index-leaf = make-literal-constant(builder, as(<ct-value>, index));
    build-assignment(builder, policy, source, temp,
		     make-operation(builder, <primitive>,
				    list(args-leaf, index-leaf),
				    name: #"extract-arg"));
    if (generic-entry?)
      let post-type = make-local-var(builder, #"arg", type);
      build-assignment(builder, policy, source, post-type,
		       make-operation(builder, <truly-the>, list(temp),
				      guaranteed-type: type));
      add!(new-args, post-type);
    else
      add!(new-args, temp);
    end;
  end;

  if (signature.next?)
    add!(new-args,
	 if (generic-entry?)
	   next-info-leaf;
	 else
	   make-literal-constant(builder, as(<ct-value>, #()));
	 end);
  end;

  if (signature.rest-type | (signature.next? & signature.key-infos))
    let op = make-operation(builder, <primitive>,
			    list(args-leaf, wanted-leaf, nargs-leaf),
			    name: #"make-rest-arg");
    let rest-var = make-local-var(builder, #"rest", object-ctype());
    build-assignment(builder, policy, source, rest-var, op);
    add!(new-args, rest-var);
  end;

  if (signature.key-infos)
    let key-var = make-local-var(builder, #"key", dylan-value(#"<symbol>"));
    let val-var = make-local-var(builder, #"value", object-ctype());
    let key-dispatch-builder = make-builder(builder);
    let unsupplied-flame-builder = make-builder(builder);
    local
      method build-next-key (remaining)
	if (empty?(remaining))
	  unless (generic-entry? | signature.all-keys?)
	    build-assignment
	      (key-dispatch-builder, policy, source, #(),
	       make-error-operation
		 (key-dispatch-builder, policy, source,
		  #"unrecognized-keyword-error", key-var));
	  end;
	else
	  let key-info = remaining.head;
	  let key = key-info.key-name;
	  let var = make-local-var(builder, key, key-info.key-type);
	  let type = key-info.key-type;
	  let default = key-info.key-default;
	  let default-bogus?
	    = default & ~cinstance?(key-info.key-default, type);
	  let needs-supplied?-var? = key-info.key-needs-supplied?-var;
	  let supplied?-var
	    = if (default-bogus? | needs-supplied?-var?)
		make-local-var(builder,
			       as(<symbol>,
				  concatenate(as(<string>, key),
					      "-supplied?")),
			       dylan-value(#"<boolean>"));
	      else
		#f;
	      end;
	  add!(new-args, var);
	  build-assignment
	    (builder, policy, source, var,
	     if (default & ~default-bogus?)
	       make-literal-constant(builder, default);
	     else
	       make(<uninitialized-value>, derived-type: type);
	     end);
	  if (supplied?-var)
	    if (needs-supplied?-var?)
	      add!(new-args, supplied?-var);
	    end;
	    build-assignment
	      (builder, policy, source, supplied?-var,
	       make-literal-constant(builder, as(<ct-value>, #f)));
	  end;
	  let temp = make-local-var(key-dispatch-builder, #"condition",
				    object-ctype());
	  build-assignment
	    (key-dispatch-builder, policy, source, temp,
	     make-unknown-call
	       (key-dispatch-builder,
		ref-dylan-defn(key-dispatch-builder, policy, source, #"=="),
		#f,
		list(key-var,
		     make-literal-constant(key-dispatch-builder,
					   as(<ct-value>, key)))));
	  build-if-body(key-dispatch-builder, policy, source, temp);
	  build-assignment(key-dispatch-builder, policy, source, var, val-var);
	  if (supplied?-var)
	    build-assignment
	      (key-dispatch-builder, policy, source, supplied?-var, 
	       make-literal-constant(key-dispatch-builder,
				     as(<ct-value>, #t)));
	  end;
	  build-else(key-dispatch-builder, policy, source);
	  build-next-key(remaining.tail);
	  end-body(key-dispatch-builder);

	  if (default-bogus?)
	    build-if-body(unsupplied-flame-builder, policy, source,
			  supplied?-var);
	    build-else(unsupplied-flame-builder, policy, source);
	    build-assignment
	      (unsupplied-flame-builder, policy, source, #(),
	       make-error-operation
		 (unsupplied-flame-builder, policy, source,
		  #"type-error",
		  make-literal-constant(unsupplied-flame-builder, default),
		  make-literal-constant(unsupplied-flame-builder, type)));
	    end-body(unsupplied-flame-builder);
	  end;
	end;
      end;

    let index-var = make-local-var(key-dispatch-builder, #"index",
				   dylan-value(#"<fixed-integer>"));
    build-assignment
      (key-dispatch-builder, policy, source, index-var,
       make-unknown-call
	 (key-dispatch-builder,
	  ref-dylan-defn(key-dispatch-builder, policy, source, #"-"),
	  #f,
	  list(nargs-leaf,
	       make-literal-constant
		 (key-dispatch-builder, as(<ct-value>, 2)))));

    let done-block = build-block-body(key-dispatch-builder, policy, source);
    build-loop-body(key-dispatch-builder, policy, source);

    let done-var
      = make-local-var(key-dispatch-builder, #"done?", object-ctype());
    build-assignment
      (key-dispatch-builder, policy, source, done-var,
       make-unknown-call(key-dispatch-builder,
			 ref-dylan-defn(key-dispatch-builder, policy, source,
					#"<"),
			 #f, list(index-var, wanted-leaf)));
    build-if-body(key-dispatch-builder, policy, source, done-var);
    build-exit(key-dispatch-builder, policy, source, done-block);
    build-else(key-dispatch-builder, policy, source);
    begin
      let op = make-operation(key-dispatch-builder, <primitive>,
			      list(args-leaf, index-var),
			      name: #"extract-arg");
      if (generic-entry?)
	let temp = make-local-var(key-dispatch-builder, #"key",
				  object-ctype());
	build-assignment(key-dispatch-builder, policy, source, temp, op);
	op := make-operation(key-dispatch-builder, <truly-the>, list(temp),
			     guaranteed-type: dylan-value(#"<symbol>"));
      end;
      build-assignment(key-dispatch-builder, policy, source, key-var, op);
    end;
    let temp = make-local-var(key-dispatch-builder, #"temp",
			      dylan-value(#"<fixed-integer>"));
    build-assignment
      (key-dispatch-builder, policy, source, temp,
       make-unknown-call(key-dispatch-builder,
			 ref-dylan-defn(key-dispatch-builder, policy, source,
					#"+"),
			 #f,
			 list(index-var,
			      make-literal-constant(key-dispatch-builder,
						    as(<ct-value>, 1)))));
    build-assignment(key-dispatch-builder, policy, source, val-var,
		     make-operation(key-dispatch-builder, <primitive>,
				    list(args-leaf, temp),
				    name: #"extract-arg"));
    build-next-key(signature.key-infos);
    build-assignment
      (key-dispatch-builder, policy, source, index-var,
       make-unknown-call(key-dispatch-builder,
			 ref-dylan-defn(key-dispatch-builder, policy, source,
					#"-"),
			 #f,
			 list(index-var,
			      make-literal-constant(key-dispatch-builder,
						    as(<ct-value>, 2)))));
    end-body(key-dispatch-builder); // if
    end-body(key-dispatch-builder); // loop
    end-body(key-dispatch-builder); // block
    build-region(builder, builder-result(key-dispatch-builder));
    build-region(builder, builder-result(unsupplied-flame-builder));
  end;

  build-assignment(builder, policy, source, #(),
		   make-operation(builder, <primitive>, list(args-leaf),
				  name: #"pop-args"));
  let cluster = make-values-cluster(builder, #"results", wild-ctype());
  build-assignment(builder, policy, source, cluster,
		   make-operation(builder, <known-call>,
				  pair(function, as(<list>, new-args))));
  build-return(builder, policy, source, xep, cluster);
  end-body(builder);

  xep;
end;



// Deletion code.

define method delete-dependent
    (component :: <component>, dependent :: <dependent-mixin>) => ();
  //
  // Remove our dependency from whatever we depend on.
  for (dep = dependent.depends-on then dep.dependent-next,
       while: dep)
    remove-dependency-from-source(component, dep);
  end;
  //
  delete-queueable(component, dependent);
end;

define method delete-dependent
    (component :: <component>, op :: <catch>, #next next-method) => ();
  op.nlx-info.nlx-catch := #f;
  next-method();
end;

define method delete-dependent
    (component :: <component>, op :: <make-catcher>, #next next-method) => ();
  op.nlx-info.nlx-make-catcher := #f;
  next-method();
end;

define method delete-dependent
    (component :: <component>, op :: <disable-catcher>, #next next-method)
    => ();
  let nlx-info = op.nlx-info;
  for (prev = #f then disable,
       disable = nlx-info.nlx-disable-catchers
	 then disable.disable-catcher-next,
       until: disable == op)
  finally
    if (prev)
      prev.disable-catcher-next := op.disable-catcher-next;
    else
      nlx-info.nlx-disable-catchers := op.disable-catcher-next;
    end;
  end;
  next-method();
end;

define method delete-dependent
    (component :: <component>, op :: <throw>, #next next-method) => ();
  let nlx-info = op.nlx-info;
  for (prev = #f then throw,
       throw = nlx-info.nlx-throws then throw.throw-next,
       until: throw == op)
  finally
    if (prev)
      prev.throw-next := op.throw-next;
    else
      nlx-info.nlx-throws := op.throw-next;
    end;
  end;
  if (nlx-info.nlx-catch & ~nlx-info.nlx-hidden-references?
	& nlx-info.nlx-exit-function == #f & nlx-info.nlx-throws == #f)
    reoptimize(component, nlx-info.nlx-catch);
  end;
  next-method();
end;

define method delete-and-unlink-assignment
    (component :: <component>, assignment :: <assignment>) => ();

  // Do everything but the unlinking.
  delete-assignment(component, assignment);

  // Unlink the assignment from region.
  let next = assignment.next-op;
  let prev = assignment.prev-op;
  if (next)
    next.prev-op := prev;
  else
    assignment.region.last-assign := prev;
  end;
  if (prev)
    prev.next-op := next;
  else
    assignment.region.first-assign := next;
  end;
  unless (next | prev)
    // It was the only assignment in the region, so flush the region.  Note:
    // this won't actually do anything if the region is the join-region of
    // some other region.
    let region = assignment.region;
    replace-subregion(component, region.parent, region, make(<empty-region>));
  end;

  // Set the region to #f to indicate that we are a gonner.
  assignment.region := #f;
end;


define method delete-assignment
    (component :: <component>, assignment :: <assignment>) => ();

  // Clean up the dependent aspects.
  delete-dependent(component, assignment);

  // Nuke the definitions.
  for (var = assignment.defines then var.definer-next,
       while: var)
    delete-definition(component, var);
  end;
end;

define method delete-definition
    (component :: <component>, defn :: <ssa-variable>) => ();
  defn.definer := #f;
end;

define method delete-definition
    (component :: <component>, defn :: <initial-definition>) => ();
  defn.definer := #f;
  let var = defn.definition-of;
  var.definitions := remove!(var.definitions, defn);
  // We might be able to ssa convert var now, so add it back to
  // initial-variables.
  unless (empty?(var.definitions))
    var.next-initial-variable := component.initial-variables;
    component.initial-variables := var;
    for (type = empty-ctype()
	   then values-type-union(type, other-defn.derived-type),
	 other-defn in var.definitions)
    finally
      maybe-restrict-type(component, var, type);
    end;
  end;
end;


define method remove-dependency-from-source
    (component :: <component>, dependency :: <dependency>) => ();
  let source = dependency.source-exp;
  for (dep = source.dependents then dep.source-next,
       prev = #f then dep,
       until: dep == dependency)
  finally
    if (prev)
      prev.source-next := dep.source-next;
    else
      source.dependents := dep.source-next;
    end;
  end;

  // Note that we dropped a dependent in case doing so will trigger
  // some optimization based on the number of definers.
  dropped-dependent(component, source);
end;

define method dropped-dependent
    (component :: <component>, expr :: <expression>) => ();
end;

define method dropped-dependent
    (component :: <component>, op :: <operation>) => ();
  if (op.dependents)
    error("%= had more than one dependent?", op);
  end;
  delete-dependent(component, op);
end;

define method dropped-dependent
    (component :: <component>, var :: <ssa-variable>) => ();
  // If the variable ended up with no references and doesn't need a type check,
  // queue it for reoptimization so it gets deleted.  But only if is still
  // actually being defines.
  unless (var.dependents | var.needs-type-check? | var.definer == #f)
    reoptimize(component, var.definer);
  end;
end;

define method dropped-dependent
    (component :: <component>, var :: <initial-variable>) => ();
  // If the variable ended up with no references and doesn't need a type check,
  // queue it for reoptimization so it gets deleted.  But only if is still
  // actually being defines.
  unless (var.dependents)
    for (def in var.definitions)
      unless (def.needs-type-check? | def.definer == #f)
	reoptimize(component, def.definer);
      end;
    end;
  end;
end;

define method dropped-dependent
    (component :: <component>, function :: <function-literal>) => ();
  if (function.visibility == #"local")
    // If we dropped a reference to the function literal, we might be
    // able to nuke it.
    reoptimize(component, function);
  end;
end;

define method dropped-dependent
    (component :: <component>, exit :: <exit-function>) => ();
  // If we dropped the last reference, clear it out.
  unless (exit.dependents)
    let nlx-info = exit.nlx-info;
    nlx-info.nlx-exit-function := #f;

    delete-dependent(component, exit);

    if (nlx-info.nlx-catch & ~nlx-info.nlx-hidden-references?
	  & nlx-info.nlx-exit-function == #f & nlx-info.nlx-throws == #f)
      reoptimize(component, nlx-info.nlx-catch);
    end;
  end;
end;

// insert-exit-after -- internal.
//
// Inserts an exit to the target after the assignment, and deletes everything
// following it in the control flow.  This is the interface to data driven
// dead code deletion.
//
define method insert-exit-after
    (component :: <component>, assignment :: <abstract-assignment>,
     target :: <block-region-mixin>)
    => ();
  if (assignment.next-op)
    let orig-region = assignment.region;
    let orig-parent = orig-region.parent;
    let (before, after) = split-after(assignment);
    replace-subregion(component, orig-parent, orig-region, before);
    after.parent := #f;
    delete-stuff-in(component, after);
  end;

  let orig-region = assignment.region;
  let orig-parent = orig-region.parent;
  unless (exit-useless?(orig-parent, orig-region, target))
    let exit = make(<exit>, block: target, next: target.exits);
    target.exits := exit;
    let new = combine-regions(orig-region, exit);
    replace-subregion(component, orig-parent, orig-region, new);
    delete-stuff-after(component, exit.parent, exit);
  end;
end;

define method insert-return-before
    (component :: <component>, assignment :: <abstract-assignment>,
     target :: <block-region-mixin>, cluster :: <abstract-variable>)
    => ();
  let exit = make(<return>, block: target, next: target.exits);
  target.exits := exit;
  let dep = make(<dependency>, dependent: exit, source-exp: cluster,
		 source-next: cluster.dependents);
  cluster.dependents := dep;
  exit.depends-on := dep;

  let orig-region = assignment.region;
  let orig-parent = orig-region.parent;

  if (assignment.prev-op)
    let (before, after) = split-before(assignment);
    replace-subregion(component, orig-parent, orig-region,
		      combine-regions(before, exit));
    after.parent := #f;
    delete-stuff-in(component, after);
  else
    replace-subregion(component, orig-parent, orig-region, exit);
    orig-region.parent := #f;
    delete-stuff-in(component, orig-region);
  end;
  delete-stuff-after(component, exit.parent, exit);

  target.result-type := wild-ctype();
  reoptimize(component, target);
end;


define generic exit-useless?
    (from :: <region>, after :: <region>, target :: <block-region-mixin>)
    => res :: <boolean>;

define method exit-useless?
    (from :: <compound-region>, after :: <region>,
     target :: <block-region-mixin>)
    => res :: <boolean>;
  for (regions = from.regions then regions.tail,
       second-to-last = #f then last,
       last = #f then regions.head,
       until: regions == #())
  finally
    if (last == after)
      exit-useless?(from.parent, from, target);
    else
      second-to-last == after
	& instance?(last, <exit>)
	& last.block-of == target;
    end;
  end;
end;

define method exit-useless?
    (from :: <region>, after :: <region>, target :: <block-region-mixin>)
    => res :: <boolean>;
  exit-useless?(from.parent, from, target);
end;

define method exit-useless?
    (from :: <loop-region>, after :: <region>, target :: <block-region-mixin>)
    => res :: <boolean>;
  #f;
end;

define method exit-useless?
    (from :: <block-region>, after :: <region>, target :: <block-region-mixin>)
    => res :: <boolean>;
  from == target | exit-useless?(from.parent, from, target);
end;

define method exit-useless?
    (from :: <function-region>, after :: <region>,
     target :: <block-region-mixin>)
    => res :: <boolean>;
  #f;
end;


define method delete-stuff-in
    (component :: <component>, simple-region :: <simple-region>) => ();
  for (assign = simple-region.first-assign then assign.next-op,
       while: assign)
    delete-assignment(component, assign);
    assign.region := #f;
  end;
end;

define method delete-stuff-in
    (component :: <component>, region :: <compound-region>) => ();
  for (subregion in region.regions)
    delete-stuff-in(component, subregion);
  end;
end;

define method delete-stuff-in
    (component :: <component>, region :: <if-region>) => ();
  delete-dependent(component, region);
  delete-stuff-in(component, region.then-region);
  delete-stuff-in(component, region.else-region);
end;

define method delete-stuff-in
    (component :: <component>, region :: <body-region>) => ();
  delete-stuff-in(component, region.body);
end;

define method delete-stuff-in
    (component :: <component>, region :: <block-region>) => ();
  delete-queueable(component, region);
  delete-stuff-in(component, region.body);
end;

define method delete-stuff-in
    (component :: <component>, region :: <exit>) => ();
  let block-region = region.block-of;
  for (scan = block-region.exits then scan.next-exit,
       prev = #f then scan,
       until: scan == region)
  finally
    if (scan)
      let next = region.next-exit;
      if (prev)
	prev.next-exit := next;
      else
	block-region.exits := next;
      end;
    end;
  end;
  unless (instance?(block-region, <component>))
    reoptimize(component, block-region);
  end;
end;

define method delete-stuff-in
    (component :: <component>, return :: <return>, #next next-method) => ();
  delete-dependent(component, return);
  next-method();
end;

define method delete-stuff-in
    (component :: <component>, region :: <fer-function-region>) => ();
  remove!(component.all-function-regions, region);
  delete-queueable(component, region);
  delete-stuff-in(component, region.body);
end;


define method delete-stuff-after
    (component :: <component>, region :: <compound-region>, after :: <region>)
    => ();
  for (remaining = region.regions then remaining.tail,
       until: remaining.head == after)
  finally
    for (subregion in remaining.tail)
      delete-stuff-in(component, subregion);
    end;
    remaining.tail := #();
  end;

  delete-stuff-after(component, region.parent, region);

  if (region.regions.size == 1)
    replace-subregion(component, region.parent, region, region.regions[0]);
  end;
end;

define method delete-stuff-after
    (component :: <component>, region :: <if-region>, after :: <region>)
    => ();
  if (select (after)
	region.then-region => doesnt-return?(region.else-region);
	region.else-region => doesnt-return?(region.then-region);
      end)
    delete-stuff-after(component, region.parent, region);
  end;
end;

define method delete-stuff-after
    (component :: <component>, region :: <loop-region>, after :: <region>)
    => ();
  // There is nothing ``after'' a loop region in the flow of control.
end;

define method delete-stuff-after
    (component :: <component>, region :: <block-region>, after :: <region>)
    => ();
  unless (region.exits)
    delete-stuff-after(component, region.parent, region);
  end;
end;

define method delete-stuff-after
    (component :: <component>, region :: <unwind-protect-region>,
     after :: <region>)
    => ();
  delete-stuff-after(component, region.parent, region);
end;

define method delete-stuff-after
    (component :: <component>, region :: <function-region>, after :: <region>)
    => ();
  // There is nothing after the function.
end;


define method doesnt-return? (region :: <simple-region>) => res :: <boolean>;
  #f;
end;

define method doesnt-return? (region :: <compound-region>) => res :: <boolean>;
  doesnt-return?(region.regions.last);
end;

define method doesnt-return? (region :: <empty-region>) => res :: <boolean>;
  #f;
end;

define method doesnt-return? (region :: <if-region>) => res :: <boolean>;
  doesnt-return?(region.then-region) & doesnt-return?(region.else-region);
end;

define method doesnt-return? (region :: <loop-region>) => res :: <boolean>;
  #t;
end;

define method doesnt-return? (region :: <block-region>) => res :: <boolean>;
  if (region.exits)
    #f;
  else
    doesnt-return?(region.body);
  end;
end;

define method doesnt-return?
    (region :: <unwind-protect-region>) => res :: <boolean>;
  region.body.doesnt-return?;
end;

define method doesnt-return? (region :: <exit>) => res :: <boolean>;
  #t;
end;



// FER editing utilities.

// replace-expression
//
// Replace dep's source-exp with new-exp.  We remove dep from the old exp's
// dependents, set the source-exp, and add dep to the new-exp's dependents.
// We also queue dep's dependent for reoptimization because it probably
// wants to know that one of its operands has changed.
//
define method replace-expression
    (component :: <component>, dep :: <dependency>, new-exp :: <expression>)
    => ();
  remove-dependency-from-source(component, dep);
  dep.source-exp := new-exp;
  dep.source-next := new-exp.dependents;
  new-exp.dependents := dep;
  reoptimize(component, dep.dependent);
end;

// combine-regions -- internal.
//
// Takes two subtrees of FER and combines them into one subtree.  The result
// is interally consistent (i.e. the two input regions will have their
// parent link updated if necessary).  This routine does NOT check the
// first subregion to see if it exits or not (i.e. whether the second subregion
// is actually reachable.
// 
define method combine-regions (#rest stuff) => res :: <region>;
  let results = #();
  local
    method grovel (region)
      if (instance?(region, <compound-region>))
	for (subregion in region.regions)
	  grovel(subregion);
	end;
      elseif (instance?(region, <simple-region>)
		& instance?(results.head, <simple-region>))
	results.head := merge-simple-regions(results.head, region);
      else
	results := pair(region, results);
      end;
    end;
  for (region in stuff)
    grovel(region);
  end;
  if (results == #())
    make(<empty-region>);
  elseif (results.tail == #())
    results.head;
  else
    let results = reverse!(results);
    let new = make(<compound-region>, regions: results);
    for (region in results)
      region.parent := new;
    end;
    new;
  end;
end;

define method merge-simple-regions
    (first :: <simple-region>, second :: <simple-region>)
    => res :: <simple-region>;
  let last-of-first = first.last-assign;
  let first-of-second = second.first-assign;

  last-of-first.next-op := first-of-second;
  first-of-second.prev-op := last-of-first;

  first.last-assign := second.last-assign;

  for (assign = first-of-second then assign.next-op,
       while: assign)
    assign.region := first;
  end;

  first;
end;

// split-after - internal
//
// Splits the region containing the assignment into two regions with the
// split following the assignment.  The assignments in the two result
// regions will have correct region links, but the parent link of the two
// results is undefined.
// 
define method split-after (assign :: <abstract-assignment>)
    => (before :: <linear-region>, after :: <linear-region>);
  let next = assign.next-op;
  let region = assign.region;
  if (next)
    let last = region.last-assign;
    assign.next-op := #f;
    region.last-assign := assign;
    let new = make(<simple-region>);
    new.first-assign := next;
    next.prev-op := #f;
    new.last-assign := last;
    for (foo = next then foo.next-op,
	 while: foo)
      foo.region := new;
    end;
    values(region, new);
  else
    values(region, make(<empty-region>));
  end;
end;

// split-before -- internal
//
// Splits the region containing the assignment into two regions with the
// split preceding the assignment.  The assignments in the two result
// regions will have correct region links, but the parent link of the two
// results is undefined.
// 
define method split-before (assign :: <abstract-assignment>)
    => (before :: <linear-region>, after :: <linear-region>);
  let prev = assign.prev-op;
  if (prev)
    split-after(prev);
  else
    values(make(<empty-region>), assign.region);
  end;
end;

// insert-after -- internal
//
// Insert the region immediate after the assignment.  All appropriate parent
// and region links are updated.
//
define generic insert-after
    (component :: <component>, assign :: <abstract-assignment>,
     insert :: <region>) => ();

define method insert-after
    (component :: <component>, assign :: <abstract-assignment>,
     insert :: <region>) => ();
  let region = assign.region;
  let parent = region.parent;
  let (before, after) = split-after(assign);
  let new = combine-regions(before, insert, after);
  new.parent := parent;
  replace-subregion(component, parent, region, new);
end;
    
define method insert-after
    (component :: <component>, after :: <abstract-assignment>,
     insert :: <simple-region>) => ();
  let region = after.region;
  for (assign = insert.first-assign then assign.next-op,
       while: assign)
    assign.region := region;
  end for;
  if (after.next-op)
    after.next-op.prev-op := insert.last-assign;
  else
    region.last-assign := insert.last-assign;
  end if;
  insert.last-assign.next-op := after.next-op;
  insert.first-assign.prev-op := after;
  after.next-op := insert.first-assign;
end method insert-after;

define method insert-after
    (component :: <component>, assign :: <abstract-assignment>,
     insert :: <empty-region>)
    => ();
end;

// insert-before -- internal
//
// Insert the region immediate before the assignment.  All appropriate parent
// and region links are updated.
//
define generic insert-before
    (component :: <component>, before :: <dependent-mixin>,
     insert :: <region>)
    => ();

define method insert-before
    (component :: <component>, assign :: <abstract-assignment>,
     insert :: <region>)
    => ();
  let region = assign.region;
  let parent = region.parent;
  let (before, after) = split-before(assign);
  let new = combine-regions(before, insert, after);
  new.parent := parent;
  replace-subregion(component, parent, region, new);
end;
    
define method insert-before
    (component :: <component>, before :: <abstract-assignment>,
     insert :: <simple-region>)
    => ();
  let region = before.region;
  for (assign = insert.first-assign then assign.next-op,
       while: assign)
    assign.region := region;
  end for;
  if (before.prev-op)
    before.prev-op.next-op := insert.first-assign;
  else
    region.first-assign := insert.first-assign;
  end if;
  insert.first-assign.prev-op := before.prev-op;
  insert.last-assign.next-op := before;
  before.prev-op := insert.last-assign;
end;
    
define method insert-before
    (component :: <component>, assign :: <abstract-assignment>,
     insert :: <empty-region>)
    => ();
end;

define method insert-before
    (component :: <component>, region :: <if-region>, insert :: <region>)
    => ();
  // Note: the region.parent must be evaluated first because combine-regions
  // is allowed to dick with the parent links.
  replace-subregion(component, region.parent, region,
		    combine-regions(insert, region));
end;

define method insert-before
    (component :: <component>, op :: <operation>, insert :: <region>)
    => ();
  insert-before(component, op.dependents.dependent, insert);
end;

define method insert-before
    (component :: <component>, region :: <return>, insert :: <region>)
    => ();
  // Note: the region.parent must be evaluated first because combine-regions
  // is allowed to dick with the parent links.
  replace-subregion(component, region.parent, region,
		    combine-regions(insert, region));
end;

// replace-subregion -- internal
//
// Replace region's child old with new.  This is NOT a deletion.  None of the
// code associated with old is deleted.  It is assumed that this routine will
// be used to edit the tree structure of regions while keeping the underlying
// assignments the same.  The new region's parent slot is updated.
//
define generic replace-subregion
    (component :: <component>, region :: <body-region>, old :: <region>,
     new :: <region>)
    => ();

define method replace-subregion
    (component :: <component>, region :: <body-region>, old :: <region>,
     new :: <region>)
    => ();
  unless (region.body == old)
    error("Replacing unknown region");
  end;
  region.body := new;
  new.parent := region;
end;

define method replace-subregion
    (component :: <component>, region :: <if-region>, old :: <region>,
     new :: <region>)
    => ();
  if (region.then-region == old)
    region.then-region := new;
  elseif (region.else-region == old)
    region.else-region := new;
  else
    error("Replacing unknown region");
  end;
  new.parent := region;
  if (instance?(region.then-region, <empty-region>)
	& instance?(region.else-region, <empty-region>))
    reoptimize(component, region);
  end;
end;

define method replace-subregion
    (component :: <component>, region :: <compound-region>, old :: <region>,
     new :: <region>)
    => ();
  for (scan = region.regions then scan.tail,
       prev = #f then scan,
       until: scan == #() | scan.head == old)
  finally
    if (scan == #())
      error("Replacing unknown region");
    end;
    let regions
      = if (prev)
	  prev.tail := pair(new, scan.tail);
	  region.regions;
	else
	  pair(new, scan.tail);
	end;

    let parent = region.parent;
    let combo = apply(combine-regions, regions);
    replace-subregion(component, parent, region, combo);
  end;
end;


// Sanity checking code.

define method assure-all-done (component :: <component>) => ();
  for (function in component.all-function-regions)
    assure-all-done-region(component, function);
  end;
end;

define method assure-all-done-region
    (component :: <component>, region :: <simple-region>) => ();
  for (assign = region.first-assign then assign.next-op,
       while: assign)
    assure-all-done-dependent(component, assign);
  end;
end;

define method assure-all-done-region
    (component :: <component>, region :: <compound-region>) => ();
  for (subregion in region.regions)
    assure-all-done-region(component, subregion);
  end;
end;

define method assure-all-done-region
    (component :: <component>, region :: <if-region>) => ();
  assure-all-done-dependent(component, region);
  assure-all-done-region(component, region.then-region);
  assure-all-done-region(component, region.else-region);
end;

define method assure-all-done-region
    (component :: <component>, region :: <body-region>) => ();
  assure-all-done-region(component, region.body);
end;

define method assure-all-done-region
    (component :: <component>, region :: <block-region>) => ();
  assure-all-done-queueable(component, region);
  assure-all-done-region(component, region.body);
end;

define method assure-all-done-region
    (component :: <component>, region :: <exit>) => ();
end;

define method assure-all-done-region
    (component :: <component>, region :: <return>) => ();
  assure-all-done-dependent(component, region);
end;

define method assure-all-done-queueable
    (component :: <component>, queueable :: <queueable-mixin>) => ();
  optimize(component, queueable);
  if (component.initial-variables | component.reoptimize-queue)
    cerror("so what?",
	   "optimizing %= did something, but we thought we were done.",
	   queueable);
  end;
end;

define method assure-all-done-dependent
    (component :: <component>, dependent :: <dependent-mixin>) => ();
  assure-all-done-queueable(component, dependent);
  for (dep = dependent.depends-on then dep.dependent-next,
       while: dep)
    let expr = dep.source-exp;
    if (instance?(expr, <dependent-mixin>))
      assure-all-done-dependent(component, expr);
    end;
  end;
end;


define method check-sanity (component :: <component>) => ();
  //
  // Make sure the component's parent is #f.
  if (component.parent)
    error("Component %= has non-#f parent %=",
	  component, component.parent);
  end;
  //
  // Check the functions.
  for (function in component.all-function-regions)
    check-sanity(function);
  end;
end;

define method check-sanity (reg :: <simple-region>) => ();
  for (assign = reg.first-assign then assign.next-op,
       prev = #f then assign,
       while: assign)
    //
    // Check that the assigment has the correct region.
    unless (assign.region == reg)
      error("assignment %= claims %= as its region instead of %=",
	    assign, assign.region, reg);
    end;
    //
    // Check that the assignment is linked correctly.
    unless (assign.prev-op == prev)
      error("assignment %= claims %= as its predecessor instead of %=",
	    assign, assign.prev-op, prev);
    end;
    //
    // Check the defines.
    for (defn = assign.defines then defn.definer-next,
	 while: defn)
      unless (defn.definer == assign)
	error("assignment %='s result %= claims its definer is %=",
	      assign, defn, defn.definer);
      end;
    end;
    //
    // Check the dependent aspect of this assignment.
    check-dependent(assign, <expression>, #t);
  end;
end;

define method check-sanity (region :: <compound-region>) => ();
  for (subregion in region.regions)
    //
    // Check to make sure the subregion's parent is correct.
    unless (subregion.parent == region)
      error("%= claims %= for its parent instead of %=",
	    subregion, subregion.parent, region);
    end;
    //
    // Check the subregion.
    check-sanity(subregion);
  end;
end;

define method check-sanity (region :: <if-region>) => ();
  //
  // Check the dependent aspects.
  check-dependent(region, <leaf>, #t);
  //
  // Check to make sure the subregion's parent links are correct.
  unless (region.then-region.parent == region)
    error("%= claims %= for its parent instead of %=",
	  region.then-region, region.then-region.parent, region);
  end;
  unless (region.else-region.parent == region)
    error("%= claims %= for its parent instead of %=",
	  region.else-region, region.else-region.parent, region);
  end;
  //
  // Check the sub regions.
  check-sanity(region.then-region);
  check-sanity(region.else-region);
end;

define method check-sanity (region :: <body-region>) => ();
  unless (region.body.parent == region)
    error("%='s body %= claims %= for its parent.",
	  region, region.body, region.body.parent);
  end;
  check-sanity(region.body);
end;

define method check-sanity (exit :: <exit>) => ();
  //
  // Make sure the exit exits to some block above us.
  for (region = exit then region.parent,
       until: region == #f | region == exit.block-of)
    unless (region)
      error("exit %= exits to block %= but that isn't an ancestor",
	    exit, exit.block-of);
    end;
  end;
end;

define method check-sanity (return :: <return>, #next next-method) => ();
  //
  // Check the exit aspects.
  next-method();
  //
  // Check the dependent aspects.
  check-dependent(return, <leaf>, #f);
end;


define method check-expression (expr :: <expression>) => ();
  //
  // Make sure all the dependents refer to this source.
  for (dep = expr.dependents then dep.source-next,
       while: dep)
    unless (dep.source-exp == expr)
      error("%='s dependent %= claims %= for its source-exp",
	    expr, dep, dep.source-exp);
    end;
    //
    // And make sure that dependent depends on us.
    for (other-dep = dep.dependent.depends-on then other-dep.dependent-next,
	 until: other-dep == dep)
      unless (other-dep)
	error("%= lists %= as a dependent, but it isn't in the "
		"depends-on chain.",
	      expr, dep.dependent);
      end;
    end;
  end;
end;

define method check-expression (op :: <operation>, #next next-method) => ();
  //
  // Check the expression aspects of an operation.
  next-method();
  //
  // Check the dependent aspects of an operation.
  check-dependent(op, <leaf>, #f);
end;

define method check-expression
    (op :: <exit-function>, #next next-method) => ();
  //
  // Check the expression aspects of the exit-function.
  next-method();
  //
  // Check the dependent aspects of the exit-function.
  check-dependent(op, <leaf>, #f);
end;

define method check-dependent
    (dep :: <dependent-mixin>, expr-kind :: <class>, one-only? :: <boolean>)
    => ();
  if (one-only?)
    unless (dep.depends-on)
      error("%= doesn't depend on anything.");
    end;
    if (dep.depends-on.dependent-next)
      error("%= depends on more than one thing.", dep);
    end;
  end;

  for (dependency = dep.depends-on then dependency.dependent-next,
       while: dependency)
    //
    // Make sure everything we depend on agrees.
    unless (dependency.dependent == dep)
      error("%='s dependency %= claims %= for its dependent",
	    dep, dependency, dep.dependent);
    end;
    //
    // Make make sure that source is okay.
    unless (instance?(dependency.source-exp, expr-kind))
      error("%='s dependency %= isn't a %=",
	    dep, dependency.source-exp, expr-kind);
    end;
    check-expression(dependency.source-exp);
    //
    // Make sure that source lists us as a dependent.
    for (sources-dependency = dependency.source-exp.dependents
	   then sources-dependency.source-next,
	 until: sources-dependency == dependency)
      unless (sources-dependency)
	error("%= depends on %=, but isn't listed as a dependent.",
	      dep, dependency.source-exp);
      end;
    end;
  end;
end;
