module: cheese
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/optimize/cheese.dylan,v 1.68 1995/05/29 02:08:50 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.


define variable *do-sanity-checks* = #f;
define method enable-sanity-checks () => (); *do-sanity-checks* := #t; end;
define method disable-sanity-checks () => (); *do-sanity-checks* := #f; end;

define variable *print-shit* = #f;
define method print-shit () => (); *print-shit* := #t; end;
define method dont-print-shit () => (); *print-shit* := #f; end;


define method optimize-component (component :: <component>) => ();
  let done = #f;
  until (done)
    if (*do-sanity-checks*)
      check-sanity(component);
    end;
    if (*print-shit*) dump-fer(component) end;
    if (component.initial-definitions)
      let init-defn = component.initial-definitions;
      component.initial-definitions := init-defn.next-initial-definition;
      init-defn.next-initial-definition := #f;
      if (*print-shit*)
	format(*debug-output*,
	       "\n********* considering %= for ssa conversion\n\n",
	       init-defn.definition-of);
      end;
      maybe-convert-to-ssa(component, init-defn);
    elseif (component.reoptimize-queue)
      let dependent = component.reoptimize-queue;
      component.reoptimize-queue := dependent.queue-next;
      dependent.queue-next := #"absent";
      if (*print-shit*)
	format(*debug-output*, "\n********** about to optimize %=\n\n",
	       dependent);
      end;
      optimize(component, dependent);
    else
      local method try (function, what)
	      if (what)
		if (*print-shit*)
		  format(*debug-output*, "\n********** ");
		end;
		format(*debug-output*, "%s\n", what);
		if (*print-shit*)
		  write('\n', *debug-output*);
		end;
	      end;
	      function(component);
	      let start-over?
		= component.initial-definitions | component.reoptimize-queue;
	      if (start-over?)
		format(*debug-output*, "\nstarting over...\n");
	      end;
	      start-over?;
	    end;
      try(propagate-call-results, "propagating call result types")
	| (*do-sanity-checks* & try(assure-all-done, #f))
	| try(identify-tail-calls, "finding tail calls")
	| try(cleanup-control-flow, "cleaning up control flow")
	| try(add-type-checks, "adding type checks")
	| try(replace-placeholders, "replacing placeholders")
	| try(environment-analysis, "running environment analysis")
	| try(build-external-entries, "building external entries")
	| (done := #t);
    end;
  end;
end;



// SSA conversion.

define method maybe-convert-to-ssa
    (component :: <component>, defn :: <initial-definition>) => ();
  let var :: <initial-variable> = defn.definition-of;
  if (var.definitions.size == 1)
    let assign = defn.definer;
    let ssa = make(<ssa-variable>,
		   dependents: var.dependents,
		   derived-type: var.var-info.asserted-type,
		   var-info: var.var-info,
		   definer: assign,
		   definer-next: defn.definer-next);
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
    delete-definition(component, defn);
    for (dep = var.dependents then dep.source-next,
	 while: dep)
      unless (dep.source-exp == var)
	error("The dependent's source-exp wasn't the var we were trying "
		"to replace?");
      end;
      dep.source-exp := ssa;
      reoptimize(component, dep.dependent);
    end;
    reoptimize(component, assign);
  end;
end;


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
    dependent.queue-next := component.reoptimize-queue;
    component.reoptimize-queue := dependent;
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

define method side-effect-free? (expr :: <expression>) => res :: <boolean>;
  #f;
end;

define method side-effect-free? (expr :: <primitive>) => res :: <boolean>;
  expr.info.primitive-side-effect-free?;
end;

define method side-effect-free? (expr :: <known-call>) => res :: <boolean>;
  let func = expr.depends-on.source-exp;
  if (instance?(func, <definition-constant-leaf>))
    let defn = func.const-defn;
    instance?(defn, <function-definition>)
      & defn.function-defn-flushable?;
  end;
end;

define method side-effect-free? (expr :: <slot-ref>) => res :: <boolean>;
  #t;
end;

define method side-effect-free? (expr :: <truly-the>) => res :: <boolean>;
  #t;
end;

define method side-effect-free? (var :: <leaf>) => res :: <boolean>;
  #t;
end;

define method side-effect-free?
    (var :: <global-variable>) => res :: <boolean>;
  if (var.var-info.var-defn.ct-value)
    #t;
  else
    #f;
  end;
end;



define method pure-single-value-expression? (expr :: <expression>)
    => res :: <boolean>;
  #f;
end;

define method pure-single-value-expression? (expr :: <primitive>)
    => res :: <boolean>;
  if (expr.info.primitive-pure?)
    let type = expr.derived-type;
    type.min-values == 1 & type.fixed-number-of-values?;
  end;
end;

define method pure-single-value-expression? (expr :: <known-call>)
    => res :: <boolean>;
  let type = expr.derived-type;
  if (type.min-values == 1 & type.fixed-number-of-values?)
    let func = expr.depends-on.source-exp;
    if (instance?(func, <definition-constant-leaf>))
      let defn = func.const-defn;
      instance?(defn, <function-definition>)
	& defn.function-defn-movable?;
    end;
  end;
end;

define method pure-single-value-expression? (var :: <leaf>)
    => res :: <boolean>;
  #t;
end;

define method pure-single-value-expression? (var :: <abstract-variable>)
    => res :: <boolean>;
  ~instance?(var.var-info, <values-cluster-info>);
end;

define method pure-single-value-expression? (var :: <global-variable>)
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

  elseif (defines == #f & side-effect-free?(source))
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
      // We are making a copy of a cluster.  If the cluster we are copying
      // is an <ssa-variable>, then copy propagate it.
      if (instance?(source, <ssa-variable>))
	maybe-propagate-copy(component, defines, source);
      end;
    else
      // We are extracting some number of values out of a cluster.  Expand
      // the cluster into that number of variables.
      for (nvals from 0, defn = defines then defn.definer-next, while: defn)
      finally
	let builder = make-builder(component);
	let op = make-operation(builder, <primitive>, list(source),
				name: #"values");
	replace-expression(component, dependency, op);
	expand-cluster(component, source, nvals);
      end;
    end;

  elseif (defines & instance?(defines.var-info, <values-cluster-info>))
    //
    // We are defining a cluster.  Propagate the type on though.
    maybe-restrict-type(component, defines, source-type);

  else

    // We are defining some fixed number of variables.
    if (pure-single-value-expression?(source))
      // But we don't have to do it right here, so propagate the value to
      // dependents of whatever we define.
      maybe-propagate-copy(component, defines, source);
    end;

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
	      let type-or-false = ctype-union(type, false-type);
	      for (var = var then var.definer-next,
		   while: var)
		maybe-restrict-type(component, var, type-or-false);
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
    => ();
  unless (var.needs-type-check?)
    // Change all references to this variable to be references to value
    // instead.
    let next = #f;
    for (dep = var.dependents then next,
	 while: dep)
      // Save next, 'cause it gets changed.
      next := dep.source-next;
      // Swing that dependent over to the new value.
      replace-expression(component, dep, value)
    end;
    
    // Queue the vars definer assignment, because the var is now unused.
    reoptimize(component, var.definer);
  end;
end;


define method maybe-propagate-copy
    (component :: <component>, var :: <abstract-variable>,
     value :: <expression>)
    => ();
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
  optimize-unknown-call(component, call, func-dep.source-exp, #f);
end;


define method optimize-unknown-call
    (component :: <component>, call :: <unknown-call>, func :: <leaf>,
     inline-expansion :: false-or(<method-parse>))
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
     func :: <function-literal>,
     inline-expansion :: false-or(<method-parse>))
    => ();
  // First, observe the result type.
  maybe-restrict-type(component, call, func.main-entry.result-type);

  // Next, convert to a known call if possible and an error call if we must.
  let sig = func.signature;

  let bogus? = #f;
  let known? = #t;
  let (next-method-info, arguments)
    = if (call.use-generic-entry?)
	let dep = call.depends-on.dependent-next;
	values(dep.source-exp, dep.dependent-next);
      else
	values(#f, call.depends-on.dependent-next);
      end;

  block (return)
    for (spec in sig.specializers,
	 arg-dep = arguments then arg-dep.dependent-next)
      unless (arg-dep)
	compiler-warning("Not enough arguments.");
	bogus? := #t;
	return();
      end;
      unless (ctypes-intersect?(arg-dep.source-exp.derived-type, spec))
	compiler-warning("wrong type arg.");
	bogus? := #t;
      end;
    finally
      if (sig.key-infos)
	// Make sure all the supplied keywords are okay.
	for (key-dep = arg-dep then key-dep.dependent-next.dependent-next,
	     while: key-dep)
	  let val-dep = key-dep.dependent-next;
	  unless (val-dep)
	    compiler-warning("Odd number of keyword/value arguments.");
	    bogus? := #t;
	    return();
	  end;
	  let leaf = key-dep.source-exp;
	  if (~instance?(leaf, <literal-constant>))
	    known? := #f;
	  elseif (instance?(leaf.value, <literal-symbol>))
	    let key = leaf.value.literal-value;
	    block (found-key)
	      for (keyinfo in sig.key-infos)
		if (keyinfo.key-name == key)
		  unless (ctypes-intersect?(val-dep.source-exp.derived-type,
					    keyinfo.key-type))
		    compiler-warning("wrong type keyword arg.");
		    bogus? := #t;
		  end;
		  found-key();
		end;
	      end;
	      unless (sig.all-keys?)
		compiler-warning("Invalid keyword %=", key);
		bogus? := #t;
	      end;
	    end;
	  else
	    compiler-warning("%= isn't a keyword.", leaf.value);
	    bogus? := #t;
	  end;
	end;
	if (known?)
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
		compiler-warning("Required keyword %= unsupplied.",
				 keyinfo.key-name);
		bogus? := #t;
	      end;
	    end;
	  end;
	end;
      elseif (sig.rest-type)
	for (arg-dep = arg-dep then arg-dep.dependent-next,
	     while: arg-dep)
	  unless (ctypes-intersect?(arg-dep.source-exp.derived-type,
				    sig.rest-type))
	    compiler-warning("wrong type rest arg");
	    bogus? := #t;
	  end;
	end;
      elseif (arg-dep)
	compiler-warning("Too many arguments.");
	bogus? := #t;
      end;
    end;
  end;
  if (bogus?)
    if (next-method-info)
      let call-dep = call.depends-on;
      let next-info-dep = call-dep.dependent-next;
      remove-dependency-from-source(component, next-info-dep);
      call-dep.dependent-next := next-info-dep.dependent-next;
    end;
    change-call-kind(component, call, <error-call>);
  elseif (known?)
    if (inline-expansion)
      let builder = make-builder(component);
      let lexenv = make(<lexenv>);
      let new-func
	= fer-convert-method(builder, inline-expansion, #f,
			     #"local", lexenv, lexenv);
      insert-before(component, call.dependents.dependent,
		    builder-result(builder));
      let func-dep = call.depends-on;
      replace-expression(component, func-dep, new-func);
    else
      convert-to-known-call(component, sig, call);
    end;
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
	  if (keyinfo.key-supplied?-var)
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

define method optimize-unknown-call
    (component :: <component>, call :: <unknown-call>,
     func :: <definition-constant-leaf>,
     inline-expansion :: false-or(<method-parse>))
    => ();
  optimize-unknown-call(component, call, func.const-defn, #f);
end;

define method optimize-unknown-call
    (component :: <component>, call :: <unknown-call>,
     defn :: <abstract-constant-definition>,
     inline-expansion :: false-or(<method-parse>))
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
     defn :: <function-definition>,
     inline-expansion :: false-or(<method-parse>))
    => ();
  let sig = defn.function-defn-signature;
  maybe-restrict-type(component, call, sig.returns);
end;

define method optimize-unknown-call
    (component :: <component>, call :: <unknown-call>,
     defn :: <generic-definition>,
     inline-expansion :: false-or(<method-parse>))
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
	 gf-spec in sig.specializers)
      unless (arg-dep)
	compiler-warning("Not enough arguments.");
	bogus? := #t;
	return();
      end;
      let arg-leaf = arg-dep.source-exp;
      let arg-type = arg-leaf.derived-type;
      unless (ctypes-intersect?(arg-type, gf-spec))
	compiler-warning("Invalid type argument.");
	bogus? := #t;
      end;
      arg-leaves := pair(arg-leaf, arg-leaves);
      arg-types := pair(arg-type, arg-types);
    finally
      if (arg-dep)
	unless (sig.key-infos | sig.rest-type)
	  compiler-warning("Too many arguments.");
	  bogus? := #t;
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
    let meths = ct-sorted-applicable-methods(defn, reverse!(arg-types));
    if (meths == #())
      compiler-warning("No applicable methods.");
      change-call-kind(component, call, <error-call>);
    elseif (meths)
      // ### Need to check the keywords before the ct method selection
      // is valid.
      let builder = make-builder(component);
      let assign = call.dependents.dependent;
      let policy = assign.policy;
      let source = assign.source-location;
      let new-func = make-definition-leaf(builder, meths.head);
      let next-leaf = make-local-var(builder, #"next-method-info",
				     object-ctype());
      let op
	= if (empty?(meths.tail))
	    make-literal-constant(builder, as(<ct-value>, #()));
	  else
	    make-unknown-call(builder, dylan-defn-leaf(builder, #"list"), #f,
			      map(curry(make-definition-leaf, builder),
				  meths.tail));
	  end;
      build-assignment(builder, policy, source, next-leaf, op);
      insert-before(component, assign, builder-result(builder));
      let new-call = make-unknown-call(builder, new-func, next-leaf,
				       reverse!(arg-leaves));
      replace-expression(component, call.dependents, new-call);
    elseif (defn.generic-defn-discriminator-leaf)
      // There is a static descriminator function.  We can change into a
      // known call.  We don't reference the discriminator directly because
      // we still want to be able to do method selection if we can derive
      // a better idea of the argument types.
      convert-to-known-call
	(component, defn.generic-defn-discriminator-leaf.signature, call);
    end;
  end;
end;    

define method optimize-unknown-call
    (component :: <component>, call :: <unknown-call>,
     defn :: <abstract-method-definition>,
     inline-expansion :: false-or(<method-parse>))
    => ();
  let sig = defn.function-defn-signature;
  maybe-restrict-type(component, call, sig.returns);
  let leaf = defn.method-defn-leaf;
  if (leaf)
    optimize-unknown-call(component, call, leaf,
			  defn.method-defn-inline-expansion);
  end;
end;

define method optimize-unknown-call
    (component :: <component>, call :: <unknown-call>,
     func :: <getter-method-definition>,
     inline-expansion :: false-or(<method-parse>))
    => ();
  let sig = func.function-defn-signature;
  maybe-restrict-type(component, call, sig.returns);
  let leaf = func.method-defn-leaf;
  if (leaf)
    optimize-unknown-call(component, call, leaf, #f);
  else
    optimize-slot-ref(component, call, func.accessor-method-defn-slot-info);
  end;
end;

define method optimize-unknown-call
    (component :: <component>, call :: <unknown-call>,
     func :: <setter-method-definition>,
     inline-expansion :: false-or(<method-parse>))
    => ();
  let sig = func.function-defn-signature;
  maybe-restrict-type(component, call, sig.returns);
  let leaf = func.method-defn-leaf;
  if (leaf)
    optimize-unknown-call(component, call, leaf, #f);
  else
    optimize-slot-set(component, call, func.accessor-method-defn-slot-info);
  end;
end;

define method optimize-unknown-call
    (component :: <component>, call :: <unknown-call>,
     func :: <exit-function>, inline-expansion :: false-or(<method-parse>))
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

define method find-transformers (func :: union(<leaf>, <definition>))
    => res :: <list>;
  #();
end;

define method find-transformers (func :: <definition-constant-leaf>)
    => res :: <list>;
  find-transformers(func.const-defn);
end;

define method find-transformers (defn :: <function-definition>)
    => res :: <list>;
  defn.function-defn-transformers;
end;
  
define method find-transformers (defn :: <generic-definition>)
    => res :: <list>;
  #();
end;


define method optimize-known-call
    (component :: <component>, call :: <known-call>,
     func :: union(<leaf>, <definition>))
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
  let nfixed = sig.specializers.size;
  for (i from 0 below nfixed,
       arg-leaves = #() then pair(dep.source-exp, arg-leaves),
       arg-types = #() then pair(dep.source-exp.derived-type, arg-types),
       dep = call.depends-on.dependent-next then dep.dependent-next)
  finally
    let meths = ct-sorted-applicable-methods(defn, reverse!(arg-types));
    if (meths)
      // Okay, we now have enough of a better idea of the types that we can
      // actually select the methods.  We need to extract the original
      // arguments and change into a call of the first applicable method.
      // But we have already extracted the fixed arguments, so just extract
      // the rest args if there are any.
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
      arg-leaves := reverse!(arg-leaves);
      // Okay, we now have all the arguments.  Change into a call of the
      // first method.
      let builder = make-builder(component);
      let new-call
	= if (meths == #())
	    compiler-warning("No applicable methods.");
	    make-operation(builder, <error-call>,
			   pair(call.depends-on.source-exp, arg-leaves));
	  else
	    let assign = call.dependents.dependent;
	    let policy = assign.policy;
	    let source = assign.source-location;
	    let new-func = make-definition-leaf(builder, meths.head);
	    let next-leaf = make-local-var(builder, #"next-method-info",
					   object-ctype());
	    let op
	      = if (empty?(meths.tail))
		  make-literal-constant(builder, as(<ct-value>, #()));
		else
		  make-unknown-call
		    (builder, dylan-defn-leaf(builder, #"list"), #f,
		     map(curry(make-definition-leaf, builder),
			 meths.tail));
		end;
	    build-assignment(builder, policy, source, next-leaf, op);
	    insert-before(component, assign, builder-result(builder));
	    make-unknown-call(builder, new-func, next-leaf, arg-leaves);
	  end;
      replace-expression(component, call.dependents, new-call);
    end;
  end;
end;

define method optimize-known-call
    (component :: <component>, call :: <known-call>,
     defn :: <abstract-method-definition>)
    => ();
  let sig = defn.function-defn-signature;
  maybe-restrict-type(component, call, sig.returns);
  let leaf = defn.method-defn-leaf;
  if (leaf)
    optimize-known-call(component, call, leaf);
  end;
end;

define method optimize-known-call
    (component :: <component>, call :: <known-call>,
     func :: <getter-method-definition>)     
    => ();
  let sig = func.function-defn-signature;
  maybe-restrict-type(component, call, sig.returns);
  optimize-slot-ref(component, call, func.accessor-method-defn-slot-info);
end;

define method optimize-known-call
    (component :: <component>, call :: <known-call>,
     func :: <setter-method-definition>)     
    => ();
  let sig = func.function-defn-signature;
  maybe-restrict-type(component, call, sig.returns);
  optimize-slot-set(component, call, func.accessor-method-defn-slot-info);
end;




define method optimize-slot-ref
    (component :: <component>, call :: <abstract-call>,
     slot :: <instance-slot-info>)
    => ();
  let instance = call.depends-on.dependent-next.source-exp;
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
	 make-error-operation(builder, "Slot is not initialized"));
      end-body(builder);
    end;
    let value = make-local-var(builder, slot.slot-getter.variable-name,
			       slot.slot-type);
    build-assignment(builder, policy, source, value,
		     make-operation(builder, <slot-ref>, list(instance),
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
	 make-error-operation(builder, "Slot is not initialized"));
      end-body(builder);
    end;
    insert-before(component, call-assign, builder-result(builder));

    let dep = call-assign.depends-on;
    replace-expression(component, dep, value);
  end;
end;

define method optimize-slot-set
    (component :: <component>, call :: <abstract-call>,
     slot :: <instance-slot-info>)
    => ();
  let instance = call.depends-on.dependent-next.dependent-next.source-exp;
  let offset = find-slot-offset(slot, instance.derived-type);
  if (offset)
    let new = call.depends-on.dependent-next.source-exp;
    let builder = make-builder(component);
    let call-assign = call.dependents.dependent;
    let op = make-operation(builder, <slot-set>, list(new, instance),
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
    elseif (instance?(func, <definition-constant-leaf>)
	      & func.const-defn == dylan-defn(#"values"))
      replace-expression(component, call.dependents, cluster);
    end;
  end;
end;


// Primitive and other magic operator optimization


define method optimize (component :: <component>, primitive :: <primitive>)
    => ();
  let info = primitive.info;

  let assign = primitive.dependents.dependent;
  local
    method assert-arg-types
	(dep :: false-or(<dependency>), arg-types :: <list>) => ();
      if (arg-types == #())
	if (dep)
	  error("Too many arguments to %%%%primitive %s", primitive.name);
	end;
      else
	let arg-type = arg-types.head;
	if (arg-type == #"rest")
	  let arg-type = arg-types.tail.head;
	  for (dep = dep then dep.dependent-next,
	       while: dep)
	    assert-type(component, assign, dep, arg-type);
	  end;
	elseif (dep == #f)
	  error("Not enough arguments to %%%%primitive %s", primitive.name);
	else
	  if (arg-type == #"cluster")
	    let arg = dep.source-exp;
	    unless (instance?(arg, <abstract-variable>)
		      & instance?(arg.var-info, <values-cluster-info>))
	      error("%%%%primitive %s expected a values cluster but got "
		      "a regular variable.");
	    end;
	  else
	    assert-type(component, assign, dep, arg-type);
	  end;
	  assert-arg-types(dep.dependent-next, arg-types.tail);
	end;
      end;
    end;
  assert-arg-types(primitive.depends-on, info.primitive-arg-types);

  maybe-restrict-type(component, primitive, info.primitive-result-type);

  let transformer = info.primitive-transformer;
  if (transformer)
    transformer(component, primitive);
  end;
end;

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
       for (var = defns then next-var,
	    val-dep = primitive.depends-on
	      then val-dep & val-dep.dependent-next,
	    while: var)
	 next-var := var.definer-next;
	 var.definer-next := #f;
	 build-assignment(builder, assign.policy, assign.source-location, var,
			  if (val-dep)
			    val-dep.source-exp;
			  else
			    make-literal-constant(builder,
						  make(<literal-false>));
			  end);
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
       let nfixed = nfixed-leaf.value.literal-value;
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
	  while: instance?(vec, <ssa-variable>))
     finally
       if (instance?(vec, <primitive>))
	 if (vec.name == #"vector")	 
	   for (value-dep = vec.depends-on then value-dep.dependent-next,
		values = #() then pair(value-dep.source-exp, values),
		while: value-dep)
	   finally
	     replace-expression
	       (component, primitive.dependents,
		make-operation(make-builder(component), <primitive>,
			       reverse!(values), name: #"values"));
	   end;
	 elseif (vec.name == #"canonicalize-results")
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
			 | consumes-cluster?(assign.depends-on))
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
  (#"call-out",
   method (component :: <component>, primitive :: <primitive>) => ();
     let func-dep = primitive.depends-on;
     begin
       let func = func-dep.source-exp;
       unless (instance?(func, <literal-constant>)
		 & instance?(func.value, <literal-string>))
	 compiler-error("The function in call-out isn't a constant string.");
       end;
     end;
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

define method dylan-type-for-c-type (leaf :: <leaf>) => res :: <values-ctype>;
  if (instance?(leaf, <literal-constant>))
    let ct-value = leaf.value;
    if (instance?(ct-value, <literal-symbol>))
      let c-type = ct-value.literal-value;
      select (c-type)
	#"char", #"short", #"int", #"long",
	#"unsigned-char", #"unsigned-short", #"unsigned-int" =>
	  specifier-type(#"<fixed-integer>");
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
       if (instance?(arg-source, <primitive>) & arg-source.name == #"not")
	 let source-source = arg-source.depends-on.source-exp;
	 let op = make-operation(make-builder(component), <primitive>,
				 list(source-source), name: #"as-boolean");
	 replace-expression(component, primitive.dependents, op);
       end;
     end;
   end);

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



// block/exit related optimizations.

define method expand-exit-function
    (component :: <component>, call :: <general-call>,
     func :: <exit-function>, cluster :: <abstract-variable>)
    => ();
  let builder = make-builder(component);
  let call-dependency = call.dependents;
  let assign = call-dependency.dependent;
  let policy = assign.policy;
  let source = assign.source-location;

  if (call.use-generic-entry?)
    error("Trying to call the generic entry for an exit function?");
  end;

  let catcher = func.depends-on.source-exp;
  let from = func.depends-on.dependent-next.source-exp;

  let throw
    = make-operation(builder, <primitive>, list(catcher, from, cluster),
		     name: #"throw", derived-type: empty-ctype());

  replace-expression(component, assign.depends-on, throw);
end;

define-primitive-transformer
  (#"make-catcher",
   method (component :: <component>, primitive :: <primitive>) => ();
     let state-leaf = primitive.depends-on.source-exp;
     if (instance?(state-leaf, <uninitialized-value>))
       replace-expression
	 (component, primitive.dependents,
	  make-literal-constant(make-builder(component),
				as(<ct-value>, #f)));
     end;
   end);

define-primitive-transformer
  (#"disable-catcher",
   method (component :: <component>, primitive :: <primitive>) => ();
     let catcher-leaf = primitive.depends-on.source-exp;
     if (csubtype?(catcher-leaf.derived-type, specifier-type(#"<false>")))
       replace-expression(component, primitive.dependents, catcher-leaf);
     end;
   end);

define-primitive-transformer
  (#"throw",
   method (component :: <component>, primitive :: <primitive>) => ();
     let body-function = primitive.depends-on.dependent-next.source-exp;
     let body-region = body-function.main-entry;
     if (primitive.home-function-region == body-region)
       let assign = primitive.dependents.dependent;
       let builder = make-builder(component);
       let catcher = primitive.depends-on.source-exp;
       assert(instance?(catcher, <ssa-variable>));
       build-assignment
	 (builder, assign.policy, assign.source-location, #(),
	  make-operation(builder, <primitive>, list(catcher),
		    name: #"disable-catcher"));
       for (region = primitive.dependents.dependent.region
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
			    primitive.depends-on.dependent-next
			      .dependent-next.source-exp);
       //
       // Check to see if the only remaining dependent of the body-function
       // is the catch primitive.
       let dep = body-function.dependents;
       if (dep & dep.dependent-next == #f
	     & instance?(dep.dependent, <primitive>)
	     & dep.dependent.name == #"catch")
	 //
	 // Change the catch into a regular call.
	 replace-expression
	   (component, dep.dependent.dependents,
	    make-unknown-call
	      (builder, body-function, #f,
	       list(make(<uninitialized-value>,
			 derived-type: specifier-type(#"<raw-pointer>")))));
       end;
     end;
   end);

define method optimize
    (component :: <component>, region :: <block-region>) => ();
  if (region.exits == #f)
    replace-subregion(component, region.parent, region, region.body);
    delete-queueable(component, region);
  end;
end;


// Function optimization


define method optimize
    (component :: <component>, function :: <function-literal>)
    => ();

  // If there is exactly one reference and that reference is the function
  // in a local call, let convert the function.
  if (function.visibility == #"local")
    let dependents = function.dependents;
    if (dependents & dependents.source-next == #f)
      let dependent = dependents.dependent;
      if (dependent.depends-on == dependents
	    & instance?(dependent, <known-call>))
	let-convert(component, function.main-entry, dependent);
      end;
    end;
  end;
end;

define method optimize
    (component :: <component>, function :: <fer-function-region>) => ();

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
    expand-cluster(component, cluster, cluster.derived-type.min-values);
    #t;
  else
    #f;
  end;
end;

define method expand-cluster 
    (component :: <component>, cluster :: <ssa-variable>,
     number-of-values :: <fixed-integer>)
    => ();
  let cluster-dependency = cluster.dependents;
  let target = cluster-dependency.dependent;
  let assign = cluster.definer;
  let new-defines = #f;
  let new-depends-on = cluster-dependency.dependent-next;
  for (index from number-of-values - 1 to 0 by -1)
    let debug-name = as(<symbol>, format-to-string("result%d", index));
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
     number-of-values :: <fixed-integer>)
    => ();
  let cluster-dependency = cluster.dependents;
  let target = cluster-dependency.dependent;
  let assigns = map(definer, cluster.definitions);
  let new-defines = make(<list>, size: cluster.definitions.size, fill: #f);
  let new-depends-on = cluster-dependency.dependent-next;
  for (index from number-of-values - 1 to 0 by -1)
    let debug-name = as(<symbol>, format-to-string("result%d", index));
    let var-info = make(<local-var-info>, debug-name: debug-name,
			asserted-type: object-ctype());
    let var = make(<initial-variable>, var-info: var-info);
    let defns = map(method (assign, next-define)
		      let defn = make(<initial-definition>, var-info: var-info,
				      definition: var, definer: assign,
				      definer-next: next-define,
				      next-initial-definition:
					component.initial-definitions);
		      component.initial-definitions := defn;
		      defn;
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

  let builder = make-builder(component);
  let call-policy = call-assign.policy;
  let call-source = call-assign.source-location;

  // Define a bunch of temporaries from the call args and insert it before
  // the call assignment.
  let arg-temps
    = begin
	let temps = make(<stretchy-vector>);
	for (dep = call.depends-on.dependent-next then dep.dependent-next,
	     arg-type in function.argument-types)
	  unless (dep)
	    error("Wrong number of argument in let-convert?");
	  end;
	  let temp = make-local-var(builder, #"arg", arg-type);
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
  begin
    let op = make-operation(builder, <primitive>, arg-temps, name: #"values");
    let prologue = function.prologue;
    let dep = prologue.dependents;
    dep.source-exp := op;
    op.dependents := dep;
    delete-dependent(component, prologue);
    reoptimize(component, op);
  end;

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

  // If there are any returns,
  if (function.exits)
    let results-temp = make-values-cluster(builder, #"results", wild-ctype());

    // Start a block for the returns to exit to.
    let body-block = build-block-body(builder, call-policy, call-source);

    // Replace each return with an assignment of the result cluster
    // followed by an exit to the body-block.
    for (return = function.exits then return.next-exit,
	 while: return)
      let builder = make-builder(component);
      let source = return.source-location;
      let results = return.depends-on;
      if (results & instance?(results.source-exp, <abstract-variable>)
	    & instance?(results.source-exp.var-info, <values-cluster-info>))
	build-assignment(builder, call-policy, return.source-location,
			 results-temp, results.source-exp);
      else
	// Make a values operation stealing the results from the return.
	let values-op = make(<primitive>, derived-type: return.returned-type,
			     name: #"values", depends-on: results);
	for (dep = results then dep.dependent-next,
	     while: dep)
	  dep.dependent := values-op;
	end;
	return.depends-on := #f;
	reoptimize(component, values-op);
	// Assign the result temp with the values call.
	build-assignment(builder, call-policy, return.source-location,
			 results-temp, values-op);
      end;
      build-exit(builder, call-policy, return.source-location, body-block);

      replace-subregion(component, return.parent, return,
			builder-result(builder));
      delete-stuff-in(component, return);
    end;
    reoptimize(component, call-assign);
    
    // Insert the body block before the call assignment.
    build-region(builder, function.body);
    end-body(builder);
    insert-before(component, call-assign, builder-result(builder));

    // Replace the call with a reference to the result cluster.
    replace-expression(component, call-assign.depends-on, results-temp);
  else
    // Insert the function body before the call assignment.
    insert-before(component, call-assign, function.body);
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
    let expr = assign.depends-on.source-exp;
    if (instance?(expr, <primitive>) & expr.name == #"throw")
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
  let old-type = expr.derived-type;
  if (~values-subtype?(old-type, type) & values-subtype?(type, old-type))
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
	  end;
	end;
      end;
    end;
    queue-dependents(component, expr);
  end;
end;

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


// Call result propagation.

define method propagate-call-results (component :: <component>) => ();
  for (function in component.all-function-regions)
    propagate-call-results-in(component, function);
  end;
end;

define method propagate-call-results-in
    (component :: <component>, region :: <simple-region>) => ();
  for (assign = region.first-assign then assign.next-op,
       while: assign)
    let source = assign.depends-on.source-exp;
    if (instance?(source, <abstract-call>))
      let func = source.depends-on.source-exp;
      propagate-call-results-of(component, source, func);
    end;
  end;
end;

define method propagate-call-results-in
    (component :: <component>, region :: <compound-region>) => ();
  for (subregion in region.regions)
    propagate-call-results-in(component, subregion);
  end;
end;

define method propagate-call-results-in
    (component :: <component>, region :: <if-region>) => ();
  propagate-call-results-in(component, region.then-region);
  propagate-call-results-in(component, region.else-region);
end;

define method propagate-call-results-in
    (component :: <component>, region :: <body-region>) => ();
  propagate-call-results-in(component, region.body);
end;

define method propagate-call-results-in
    (component :: <component>, region :: <exit>) => ();
end;

define method propagate-call-results-of
    (component :: <component>, call :: <abstract-call>, function :: <leaf>)
    => ();
end;

define method propagate-call-results-of
    (component :: <component>, call :: <abstract-call>,
     function :: <function-literal>)
    => ();
  maybe-restrict-type(component, call, function.main-entry.result-type);
end;

define method propagate-call-results-of
    (component :: <component>, call :: <abstract-call>,
     function :: <definition-constant-leaf>)
    => ();
  propagate-call-results-of(component, call, function.const-defn);
end;

define method propagate-call-results-of
    (component :: <component>, call :: <abstract-call>,
     function :: <abstract-constant-definition>)
    => ();
end;

define method propagate-call-results-of
    (component :: <component>, call :: <abstract-call>,
     function :: <abstract-method-definition>)
    => ();
  let leaf = function.method-defn-leaf;
  if (leaf)
    propagate-call-results-of(component, call, leaf);
  end;
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
	if (find-main-entry(expr.depends-on.source-exp) == home)
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


define method find-main-entry (func :: <leaf>)
    => res :: false-or(<function-region>);
  #f;
end;

define method find-main-entry (func :: <function-literal>)
    => res :: false-or(<function-region>);
  func.main-entry;
end;

define method find-main-entry (func :: <definition-constant-leaf>)
    => res :: false-or(<function-region>);
  find-main-entry(func.const-defn);
end;

define method find-main-entry (defn :: <abstract-constant-definition>)
    => res :: false-or(<function-region>);
  #f;
end;

define method find-main-entry (defn :: <abstract-method-definition>)
    => res :: false-or(<function-region>);
  let leaf = defn.method-defn-leaf;
  leaf & find-main-entry(leaf);
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
  reoptimize(component, assign);
  reoptimize(component, op);
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
    => terminating-exit :: union(<exit>, <boolean>);
  #f;
end;

define method cleanup-control-flow-aux
    (component :: <component>, region :: <compound-region>)
    => terminating-exit :: union(<exit>, <boolean>);
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
    => terminating-exit :: union(<exit>, <boolean>);
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
    => terminating-exit :: union(<exit>, <boolean>);
  if (cleanup-control-flow-aux(component, region.body))
    // ### Hm.  Should flush this region, but that will cause all sorts of
    // problems with the iteration in <compound-region> above.
    #f;
  end;
  #t;
end;

define method cleanup-control-flow-aux
    (component :: <component>, region :: <block-region>)
    => terminating-exit :: union(<exit>, <boolean>);
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
    => terminating-exit :: union(<exit>, <boolean>);
  cleanup-control-flow-aux(component, region.body);
end;

define method cleanup-control-flow-aux
    (component :: <component>, region :: <exit>)
    => terminating-exit :: union(<exit>, <boolean>);
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
	// Make a temp to hold the unchecked value.
	let temp = if (instance?(defn.var-info, <values-cluster-info>))
		     error("values cluster needs a type check?");
		   else
		     make(<ssa-variable>,
			  definer: assign,
			  definer-next: defn.definer-next,
			  var-info: make(<local-var-info>,
					 debug-name: defn.var-info.debug-name,
					 asserted-type: object-ctype()));
		   end;
	// Link the temp in in place of this definition.
	if (prev)
	  prev.definer-next := temp;
	else
	  assign.defines := temp;
	end;
	// Make the builder if we haven't already.
	unless (builder)
	  builder := make-builder(component);
	end;
	// Make the check type operation.
	let asserted-type = defn.var-info.asserted-type;
	let check = make-check-type-operation(builder, temp,
					      make-literal-constant
						(builder, asserted-type));
	// Assign the type checked value to the real var.
	defn.definer-next := #f;
	build-assignment(builder, assign.policy, assign.source-location,
			 defn, check);
	// Seed the derived type of the check-type call.
	let cur-type = assign.depends-on.source-exp.derived-type;
	let cur-type-positionals = cur-type.positional-types;
	let (checked-type, precise?)
	  = ctype-intersection(asserted-type, defaulted-first-type(cur-type));
	maybe-restrict-type(component, check,
			    if (precise?)
			      checked-type;
			    else
			      asserted-type;
			    end);
	// Queue the assignment for reoptimization.
	reoptimize(component, assign);
	// Change defn to temp so that the loop steps correctly.
	defn := temp;
      end;
    end;
    if (builder)
      insert-after(component, assign, builder-result(builder));
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
	   (builder, dylan-defn-leaf(builder, #"make"), #f,
	    list(dylan-defn-leaf(builder, #"<simple-object-vector>"),
		 make-literal-constant(builder, as(<ct-value>, size:)),
		 make-literal-constant(builder, as(<ct-value>, len)))));
      for (dep = op.depends-on then dep.dependent-next,
	   index from 0,
	   while: dep)
	build-assignment
	  (builder, policy, source, #(),
	   make-unknown-call
	     (builder, dylan-defn-leaf(builder, #"element-setter"), #f,
	      list(dep.source-exp, vec,
		   make-literal-constant(builder, as(<ct-value>, index)))));
      end;
      insert-before(component, assign, builder-result(builder));
      replace-expression(component, dep, vec);

    #"make-catcher", #"disable-catcher", #"throw" =>
      let builder = make-builder(component);
      replace-expression
	(component, dep,
	 make-unknown-call
	   (builder, dylan-defn-leaf(builder, op.name), #f,
	    for (dep = op.depends-on then dep.dependent-next,
		 args = #() then pair(dep.source-exp, args),
		 while: dep)
	    finally
	      reverse!(args);
	    end));
    otherwise => #f;
  end;
end;

define method replace-placeholder
    (component :: <component>, dep :: <dependency>, leaf :: <exit-function>)
    => ();
  let builder = make-builder(component);
  let catcher = leaf.depends-on.source-exp;
  let make-exit-fun-leaf = dylan-defn-leaf(builder, #"make-exit-function");
  let temp = make-local-var(builder, #"exit-function", function-ctype());
  build-assignment
    (builder, $Default-Policy, make(<source-location>), temp,
     make-unknown-call(builder, make-exit-fun-leaf, #f, list(catcher)));
  insert-before(component, dep.dependent, builder-result(builder));
  replace-expression(component, dep, temp);
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
      for (ref-dep = function.literal.dependents then ref-dep.source-next,
	   while: ref-dep)
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
	elseif (instance?(ref, <primitive>) & ref.name == #"throw")
	  error("A throw primitive made it through to environment analysis?");
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
    let value-setter = dylan-defn-leaf(builder, #"value-setter");
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
	  let make-leaf = dylan-defn-leaf(builder, #"make");
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
	  build-assignment
	    (builder, assign.policy, assign.source-location, #(),
	     make-unknown-call(builder, value-setter, #f,
			       list(temp, value-cell)));
      end;
      insert-after(component, assign, builder-result(builder));
      reoptimize(component, assign);
    end;
    let value = dylan-defn-leaf(builder, #"value");
    let next = #f;
    for (dep = var.dependents then next,
	 while: dep)
      next := dep.source-next;
      let temp = make-ssa-var(builder, var.var-info.debug-name,
			      var.derived-type);
      dep.source-exp := temp;
      temp.dependents := dep;
      dep.source-next := #f;
      let op = make-unknown-call(builder, value, #f, list(value-cell));
      op.derived-type := var.derived-type;
      build-assignment(builder, $Default-Policy, make(<source-location>),
		       temp, op);
      insert-before(component, dep.dependent, builder-result(builder));
    end;
    maybe-close-over(component, value-cell, home);
  end;
end;



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
  if (instance?(function, <method-literal>))
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
  let self-leaf = make-local-var(builder, #"self", function-ctype());
  let nargs-leaf = make-local-var(builder, #"nargs", 
				  dylan-value(#"<fixed-integer>"));
  let next-info-leaf
    = generic-entry? & make-local-var(builder, #"next-method-info",
				      dylan-value(#"<list>"));
  let name = format-to-string("%s entry for %s",
			      if (generic-entry?) "Generic" else "General" end,
			      main-entry.name);
  let xep = build-function-body(builder, policy, source, name,
				if (generic-entry?)
				  list(self-leaf, nargs-leaf, next-info-leaf);
				else
				  list(self-leaf, nargs-leaf);
				end,
				#"cluster");
  let new-args = make(<stretchy-vector>);
  if (instance?(main-entry, <lambda>) & main-entry.environment.closure-vars)
    let closure-ref-leaf = dylan-defn-leaf(builder, #"%closure-ref");
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
	(builder, dylan-defn-leaf(builder, #"=="), #f,
	 list(nargs-leaf, wanted-leaf));
      let temp = make-local-var(builder, #"nargs-okay?", object-ctype());
      build-assignment(builder, policy, source, temp, op);
      build-if-body(builder, policy, source, temp);
      build-else(builder, policy, source);
      build-assignment
	(builder, policy, source, #(),
	 make-error-operation
	   (builder,
	    "Wrong number of arguments.  Wanted exactly %d but got %d",
	    wanted-leaf, nargs-leaf));
      end-body(builder);
      build-assignment(builder, policy, source, args-leaf,
		       make-operation(builder, <primitive>, list(wanted-leaf),
				      name: #"extract-args"));
    else
      unless (empty?(arg-types))
	let op = make-unknown-call
	  (builder, dylan-defn-leaf(builder, #"<"), #f,
	   list(nargs-leaf, wanted-leaf));
	let temp = make-local-var(builder, #"nargs-okay?", object-ctype());
	build-assignment(builder, policy, source, temp, op);
	build-if-body(builder, policy, source, temp);
	build-assignment
	  (builder, policy, source, #(),
	   make-error-operation
	     (builder,
	      "Wrong number of arguments.  Wanted at least %d but only got %d",
	      wanted-leaf, nargs-leaf));
	build-else(builder, policy, source);
	end-body(builder);
      end;
      if (signature.key-infos)
	let func = dylan-defn-leaf(builder,
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
	     (builder, "Odd number of keyword/value arguments."));
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

  if (signature.rest-type)
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
    local
      method build-next-key (remaining)
	if (empty?(remaining))
	  unless (generic-entry? | signature.all-keys?)
	    build-assignment(key-dispatch-builder, policy, source, #(),
			     make-error-operation(key-dispatch-builder,
						  "Bogus keyword: %=",
						  key-var));
	  end;
	else
	  let key-info = remaining.head;
	  let key = key-info.key-name;
	  let var = make-local-var(builder, key, key-info.key-type);
	  let supplied?-var
	    = if (key-info.key-supplied?-var)
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
	     if (key-info.key-default)
	       make-literal-constant(builder, key-info.key-default);
	     else
	       make(<uninitialized-value>, derived-type: key-info.key-type);
	     end);
	  if (supplied?-var)
	    add!(new-args, supplied?-var);
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
		dylan-defn-leaf(key-dispatch-builder, #"=="),
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
	end;
      end;

    let index-var = make-local-var(key-dispatch-builder, #"index",
				   dylan-value(#"<fixed-integer>"));
    build-assignment
      (key-dispatch-builder, policy, source, index-var,
       make-unknown-call
	 (key-dispatch-builder,
	  dylan-defn-leaf(key-dispatch-builder, #"-"),
	  #f,
	  list(nargs-leaf,
	       make-literal-constant
		 (key-dispatch-builder, as(<ct-value>, 2)))));

    let done-block = build-block-body(key-dispatch-builder, policy, source);
    build-loop-body(key-dispatch-builder, policy, source);

    let more-var
      = make-local-var(key-dispatch-builder, #"more?", object-ctype());
    build-assignment
      (key-dispatch-builder, policy, source, more-var,
       make-unknown-call(key-dispatch-builder,
			 dylan-defn-leaf(key-dispatch-builder, #"<"),
			 #f, list(index-var, wanted-leaf)));
    build-if-body(key-dispatch-builder, policy, source, more-var);
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
			 dylan-defn-leaf(key-dispatch-builder, #"+"),
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
			 dylan-defn-leaf(key-dispatch-builder, #"-"),
			 #f,
			 list(index-var,
			      make-literal-constant(key-dispatch-builder,
						    as(<ct-value>, 2)))));
    end-body(key-dispatch-builder); // if
    end-body(key-dispatch-builder); // loop
    end-body(key-dispatch-builder); // block
    build-region(builder, builder-result(key-dispatch-builder));
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

define method delete-and-unlink-assignment
    (component :: <component>, assignment :: <assignment>) => ();

  // Do everything but the unlinking.
  delete-assignment(component, assignment);

  // Unlink the assignment from region.
  let next = assignment.next-op;
  let prev = assignment.prev-op;
  if (next | prev)
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
  else
    // It was the only assignment in the region, so flush the region.
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
  unless (empty?(var.definitions))
    let remaining-defn = var.definitions.first;
    remaining-defn.next-initial-definition := component.initial-definitions;
    component.initial-definitions := remaining-defn;
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
    (component :: <component>, function :: <function-literal>) => ();
  if (function.visibility == #"local")
    if (function.dependents == #f)
      // Delete the function.
      remove!(component.all-function-literals, function);
      delete-queueable(component, function);
      function.visibility := #"deleted";
      local
	method delete-function-region (region)
	  if (region)
	    remove!(component.all-function-regions, region);
	    delete-queueable(component, region);
	  end;
	end;
      delete-function-region(function.main-entry);
      delete-function-region(function.general-entry);
      if (instance?(function, <method-literal>))
	delete-function-region(function.generic-entry);
      end;
    elseif (function.dependents.source-next == #f)
      // Only one reference left, so queue it for reoptimization so that
      // it can be let converted.
      reoptimize(component, function);
    end;
  end;
end;

define method dropped-dependent
    (component :: <component>, exit :: <exit-function>) => ();
  // If we dropped the last reference, clear it out.
  unless (exit.dependents)
    delete-dependent(component, exit);
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
  let new = combine-regions(combine-regions(before, insert), after);
  new.parent := parent;
  replace-subregion(component, parent, region, new);
end;
    
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
  let new = combine-regions(combine-regions(before, insert), after);
  new.parent := parent;
  replace-subregion(component, parent, region, new);
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
    assure-all-done-expr(component, assign.depends-on.source-exp);
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
    (component :: <component>, region :: <exit>) => ();
end;

define method assure-all-done-region
    (component :: <component>, region :: <return>) => ();
  assure-all-done-dependent(component, region);
end;

define method assure-all-done-dependent
    (component :: <component>, dependent :: <dependent-mixin>) => ();
  optimize(component, dependent);
  if (component.initial-definitions | component.reoptimize-queue)
    error("optimizing %= did something, but we thought we were done.",
	  dependent);
  end;
end;

define method assure-all-done-expr
    (component :: <component>, expr :: <expression>) => ();
end;

define method assure-all-done-expr
    (component :: <component>, op :: <dependent-mixin>) => ();
  assure-all-done-dependent(component, op);
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
