module: cheese
rcs-header: $Header: /scm/cvs/src/d2c/compiler/optimize/cheese.dylan,v 1.1 1998/05/03 19:55:34 andreas Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.


//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
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
//    University.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports, questions, comments, and suggestions should be sent by
// E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
//
//======================================================================

define variable *do-sanity-checks* :: <boolean> = #f;
define method enable-sanity-checks () => (); *do-sanity-checks* := #t; end;
define method disable-sanity-checks () => (); *do-sanity-checks* := #f; end;

define variable *print-shit* :: <boolean> = #f;
define method print-shit () => (); *print-shit* := #t; end;
define method dont-print-shit () => (); *print-shit* := #f; end;

define variable *optimize-ncalls* :: <integer> = 0;

// Note: the simplify-only: keyword is used only during inline
// expansions.  It is not useful in any other situation.
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
	dformat("\n******** doing trivial ssa conversion\n\n");
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
	dformat("\n******** about to optimize %=\n\n", queueable);
      end;
      optimize(component, queueable);
      *optimize-ncalls* := *optimize-ncalls* + 1;
    else
      local method try (function, what)
	      if (what & *print-shit*)
		dformat("\n******** %s\n\n", what);
	      end;
	      function(component);
	      let start-over?
		= component.initial-variables | component.reoptimize-queue;
	      if (start-over? & *print-shit*)
		dformat("\nstarting over...\n");
	      end;
	      start-over?;
	    end;
      (*do-sanity-checks* & try(assure-all-done, #f))
	| try(identify-tail-calls, "finding tail calls")
	| try(cleanup-control-flow, "cleaning up control flow")
	| try(common-subexpression-elimination,
	      "eliminating common sub-expressions")
	| try(propagate-constraints, "propagating constraints")
	| try(optimistic-type-inference, "optimistic type inference")
	| (simplify-only & (done := #t))
	| try(add-type-checks, "adding type checks")
	| try(replace-placeholders, "replacing placeholders")
	| try(environment-analysis, "running environment analysis")
	| try(build-local-xeps, "building external entries for local funs")
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
  if (*print-shit*)
    dformat("queueing %=\n", dependent);
  end if;
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
  expr.primitive-info.priminfo-side-effect-free?;
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
  expr.primitive-info.priminfo-pure?;
end method expression-movable?;

define method expression-movable? (expr :: <known-call>)
    => res :: <boolean>;
  function-movable?(expr.depends-on.source-exp);
end method expression-movable?;

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

define method expression-movable? (var :: <ssa-variable>)
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
      for (nvals :: <integer> from 0,
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
	 index :: <integer> from 0 below source-type.min-values,
	 positionals = source-type.positional-types then positionals.tail,
	 while: var)
      //
      // For the values that are guarenteed to be returned, we have precise
      // type information.
      let type = positionals.head;
      maybe-restrict-type(component, var, type);
      //
      // If it is a singleton type, replace references to the var with
      // references directly to that value.
      let ctv = only-possible-value(type);
      if (ctv)
	maybe-propagate-copy
	  (component, var,
	   make-literal-constant(make-builder(component), ctv));
      end if;

    finally
      if (var)

	// For the variables that might be defaulted to #f because the value
	// was unsupplied, union in <false>.
	let false-type = dylan-value(#"<false>").ctype-extent;
	for (var = var then var.definer-next,
	     positionals = positionals then positionals.tail,
	     until: var == #f | positionals == #())
	  let type = ctype-union(positionals.head, false-type);
	  maybe-restrict-type(component, var, type);
	  //
	  // If it is a singleton type, replace references to the var with
	  // references directly to that value.
	  let ctv = only-possible-value(type);
	  if (ctv)
	    maybe-propagate-copy
	      (component, var,
	       make-literal-constant(make-builder(component), ctv));
	  end if;
	  
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


// maybe-propagate-copy -- internal.
//
// Called by the assignment optimizer whenever it notices an assignment that
// is a potentially useless copy.  The values is guaranteed to be movable,
// but it is up to us to make sure the copy really is useless.
// 
define generic maybe-propagate-copy
    (component :: <component>, var :: <definition-site-variable>,
     value :: <expression>)
    => no-longer-needed? :: <boolean>;

// maybe-propagate-copy{<initial-definition>,<expression>} -- internal.
//
// We can't propagate away assignments to initial definitions, because we can't
// tell which definition the uses of the corresponding initial-variable
// correspond to.
//
define method maybe-propagate-copy
    (component :: <component>, var :: <initial-definition>,
     value :: <expression>)
    => no-longer-needed? :: <boolean>;
  #f;
end;

// maybe-propagate-copy{<ssa-variable>,<leaf>}
//
// For ssa-variables that are being assigned some leaf, just use that leaf
// instead of the ssa-variable.  But not if we need to type-check that leaf.
// 
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

// maybe-propagate-copy{<ssa-variable>,<ssa-variable>}
//
// A copy of another variable needs to be handled specially because only
// some ssa-variables can be closed over (i.e. defined and referenced in
// different functions).
//
define method maybe-propagate-copy
    (component :: <component>, var :: <ssa-variable>, value :: <ssa-variable>)
    => no-longer-needed? :: <boolean>;
  unless (var.needs-type-check?)
    
    if (instance?(var.var-info, <lexical-var-info>)
	  & ~instance?(value.var-info, <lexical-var-info>))
      // We can't just blindly replace references to var with references to
      // value because they might be in a different function, and value can't
      // be closed over.  So we only replace the references that are homed in
      // value.definer.home.
      //
      // let-conversion requeues any lets in the compound result function so
      // we will retry copy propagation whenever the references to var might
      // have been moved into the function that defines var.
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
      //
      // We can just replace references to var with references to value because
      // they are either both lexical variables (and hence value can be closed
      // over just as easily as var) or var isn't a lexical variable (and hence
      // can't be closed over so it doesn't matter that value can't either).
      //
      // Change all references to this variable to be references to value
      // instead.
      while (var.dependents)
	replace-expression(component, var.dependents, value)
      end;
      // Return that we nuked 'em all.
      #t;
    end if;
  end unless;
end method maybe-propagate-copy;


// maybe-propagate-copy{<ssa-variable>,<expression>}
//
// We only squeeze out a copy of some operation when there is a single
// reference to the copy, that reference is an assignment, and that assignment
// is in the same loop or function as the variable's definition.  And when
// we don't need to type-check the result.
// 
define method maybe-propagate-copy
    (component :: <component>, var :: <ssa-variable>, value :: <operation>)
    => no-longer-needed? :: <boolean>;
  unless (var.needs-type-check?)
    let dep = var.dependents;
    assert(dep);
    if (dep.source-next == #f)
      let use = dep.dependent;
      if (instance?(use, <assignment>)
	    & (enclosing-loop-or-function(use.region)
		 == enclosing-loop-or-function(var.definer.region)))
	if (instance?(use.defines, <ssa-variable>))
	  queue-dependents(component, use.defines);
	end if;
	replace-expression(component, dep, value);
	#t;
      end if;
    end if;
  end unless;
end method maybe-propagate-copy;



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
     number-of-values :: <integer>, names :: <list>)
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
	& assign-source.primitive-name == #"values")
    reoptimize(component, assign-source);
  end;
end;

define method expand-cluster 
    (component :: <component>, cluster :: <initial-variable>,
     number-of-values :: <integer>, names :: <list>)
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
	  & assign-source.primitive-name == #"values")
      reoptimize(component, assign-source);
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
		    derived-type:
		      specifier-type(#"<raw-pointer>").ctype-extent))));
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
	let policy = $Default-Policy;
	let source = region.source-location;
	build-assignment
	  (builder, policy, source, #(),
	   make-unknown-call
	     (builder,
	      ref-dylan-defn(builder, policy, source, #"pop-unwind-protect"),
	      #f, #()));
	build-assignment
	  (builder, policy, source, #(),
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
  else
    if (instance?(condition, <ssa-variable>))
      let cond-source = condition.definer.depends-on.source-exp;
      if (instance?(cond-source, <primitive>))
	// We know that these optimizations will always trigger when possible
	// because the only ways that the trigger condition can become true
	// is if someone changes our condition, or if someone changes the
	// condition's definition to be one of the magic primitives.  But
	// the only things that can change the condition's definition to either
	// a not or a make-next-method primitive is copy-propagation (which
	// explicitly queues the dependents of the variable it changes the
	// definition of) and optimization of as-boolean when the argument is
	// a not primitive (which will trigger a copy-propagation, as
	// described above).
	
	select (cond-source.primitive-name)
	  #"not" =>
	    // We have an ``if (~x) foo else bar end''.  So change it into an
	    // ``if (x) bar else foo end''.
	    replace-expression
	      (component, if-region.depends-on,
	       maybe-copy(component, cond-source.depends-on.source-exp,
			  condition.definer, if-region.home-function-region));
	    let then-region = if-region.then-region;
	    if-region.then-region := if-region.else-region;
	    if-region.else-region := then-region;

	  #"make-next-method" =>
	    // We have an ``if (make-next-method(info, orig-args)) ... end''
	    // so change it into an ``if (info ~== #()) ... end''
	    replace-expression
	      (component, if-region.depends-on,
	       expand-next-method-if-ref(component, if-region, cond-source));
	    let then-region = if-region.then-region;
	    if-region.then-region := if-region.else-region;
	    if-region.else-region := then-region;

	  otherwise =>
	    // Something else.  Ignore it.
	    begin end;

	end select;
      end if;
    end if;
    if (anything-after?(if-region.parent, if-region))
      let then-doesnt-return? = if-region.then-region.doesnt-return?;
      let else-doesnt-return? = if-region.else-region.doesnt-return?;
      if (else-doesnt-return?)
	unless (then-doesnt-return?)
	  let after
	    = extract-stuff-after(component, if-region.parent, if-region);
	  replace-subregion
	    (component, if-region, if-region.then-region,
	     combine-regions(component, if-region.then-region, after));
	end unless;
      elseif (then-doesnt-return?)
	let after
	  = extract-stuff-after(component, if-region.parent, if-region);
	replace-subregion
	  (component, if-region, if-region.else-region,
	   combine-regions(component, if-region.else-region, after));
      end if;
    end if;
  end if;
end method optimize;

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


define method expand-next-method-if-ref
    (component :: <component>, ref-site :: <dependent-mixin>,
     next-method-maker :: <primitive>)
    => no-next-method?-leaf :: <leaf>;
  let builder = make-builder(component);
  let source = make(<source-location>);
  let policy = $Default-Policy;

  let empty?-var = make-local-var(builder, #"empty?", object-ctype());
  build-assignment
    (builder, policy, source, empty?-var,
     make-operation
       (builder, <primitive>,
	list(maybe-copy(component, next-method-maker.depends-on.source-exp,
			next-method-maker, ref-site.home-function-region),
	     make-literal-constant(builder, as(<ct-value>, #()))),
	name: #"=="));

  insert-before(component, ref-site, builder-result(builder));

  empty?-var;
end method expand-next-method-if-ref;



// Type utilities.

define method optimize (component :: <component>, op :: <truly-the>) => ();
  let guaranteed = op.guaranteed-type.ctype-extent;
  let (intersection, win)
    = ctype-intersection(op.depends-on.source-exp.derived-type, guaranteed);
  maybe-restrict-type
    (component, op, if (win) intersection else guaranteed end);
end;

define method assert-type
    (component :: <component>, before :: <assignment>,
     dependent :: <dependency>, type :: <ctype>)
    => ();
  let source = dependent.source-exp;
  unless (csubtype?(source.derived-type, type.ctype-extent))
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
		ctype-intersection(defaulted-type(type, 0),
				   var-info.asserted-type);
	      end);
end;

define method maybe-restrict-type
    (component :: <component>, var :: <definition-site-variable>,
     type :: <values-ctype>, #next next-method)
    => ();
  if (var.needs-type-check?
	& values-subtype?(type, var.var-info.asserted-type.ctype-extent))
    var.needs-type-check? := #f;
    reoptimize(component, var.definer);
  end;
  next-method();
end;


define generic defaulted-type (ctype :: <values-ctype>, index :: <integer>)
    => res :: <ctype>;

define method defaulted-type (ctype :: <ctype>, index :: <integer>)
    => res :: <ctype>;
  if (index.zero?)
    ctype;
  else
    specifier-type(#"<false>").ctype-extent;
  end if;
end;

define method defaulted-type
    (ctype :: <multi-value-ctype>, index :: <integer>)
    => res :: <ctype>;
  let positionals = ctype.positional-types;
  if (index < positionals.size)
    let type = positionals[index];
    if (index < ctype.min-values)
      type;
    else
      ctype-union(type, specifier-type(#"<false>").ctype-extent);
    end if;
  else
    ctype-union(ctype.rest-value-type,
		specifier-type(#"<false>").ctype-extent);
  end if;
end method defaulted-type;

define method fixed-number-of-values? (ctype :: <ctype>) => res :: <boolean>;
  #t;
end;

define method fixed-number-of-values?
    (ctype :: <values-ctype>) => res :: <boolean>;
  ctype.min-values == ctype.positional-types.size
    & ctype.rest-value-type == empty-ctype();
end;


// Control flow cleanup stuff.

define method cleanup-control-flow (component :: <component>) => ();
  let postpass-thunks = make(<stretchy-vector>);
  for (function in component.all-function-regions)
    if (cleanup-control-flow-aux
	  (component, function.body, #(), postpass-thunks))
      error("control flow drops off the end of %=?", function);
    end;
  end;
  for (thunk in postpass-thunks)
    thunk();
  end for;
end;

define method cleanup-control-flow-aux
    (component :: <component>, region :: <simple-region>,
     surrounding-blocks :: <list>, postpass-thunks :: <stretchy-vector>)
    => returns? :: <boolean>;
  #t;
end;

define method cleanup-control-flow-aux
    (component :: <component>, region :: <compound-region>,
     surrounding-blocks :: <list>, postpass-thunks :: <stretchy-vector>)
    => returns? :: <boolean>;
  block (return)
    for (remaining = region.regions then remaining.tail,
	 until: remaining == #())
      let last? = remaining.tail == #();
      let returned?
	= cleanup-control-flow-aux(component, remaining.head,
				   if (last?)
				     surrounding-blocks;
				   else
				     #();
				   end,
				   postpass-thunks);
      unless (returned?)
	unless (last?)
	  for (subregion in remaining.tail)
	    delete-stuff-in(component, subregion);
	  end;
	  remaining.tail := #();
	  if (region.regions.tail == #())
	    add!(postpass-thunks,
		 method () => ();
		   replace-subregion(component, region.parent, region,
				     region.regions.head);
		 end method);
	  end if;
	end unless;
	return(#f);
      end unless;
    end for;
    #t;
  end block;
end method cleanup-control-flow-aux;

define method cleanup-control-flow-aux
    (component :: <component>, region :: <if-region>,
     surrounding-blocks :: <list>, postpass-thunks :: <stretchy-vector>)
    => returns? :: <boolean>;
  let then-returns?
    = cleanup-control-flow-aux(component, region.then-region,
			       surrounding-blocks, postpass-thunks);
  let else-returns?
    = cleanup-control-flow-aux(component, region.else-region,
			       surrounding-blocks, postpass-thunks);
  if (then-returns? ~== else-returns? & anything-after?(region.parent, region))
    reoptimize(component, region);
  end if;
  then-returns? | else-returns?;
end method cleanup-control-flow-aux;

define method cleanup-control-flow-aux
    (component :: <component>, region :: <loop-region>,
     surrounding-blocks :: <list>, postpass-thunks :: <stretchy-vector>)
    => returns? :: <boolean>;
  unless (cleanup-control-flow-aux(component, region.body,
				   #(), postpass-thunks))
    add!(postpass-thunks,
	 method () => ();
	   replace-subregion(component, region.parent, region, region.body);
	 end method);
  end unless;
  #f;
end;

define method cleanup-control-flow-aux
    (component :: <component>, region :: <block-region>,
     surrounding-blocks :: <list>, postpass-thunks :: <stretchy-vector>)
    => returns? :: <boolean>;
  cleanup-control-flow-aux(component, region.body,
			   pair(region, surrounding-blocks),
			   postpass-thunks);
  #t;
end;

define method cleanup-control-flow-aux
    (component :: <component>, region :: <unwind-protect-region>,
     surrounding-blocks :: <list>, postpass-thunks :: <stretchy-vector>)
    => returns? :: <boolean>;
  cleanup-control-flow-aux(component, region.body,
			   surrounding-blocks, postpass-thunks);
end;

define method cleanup-control-flow-aux
    (component :: <component>, region :: <exit>,
     surrounding-blocks :: <list>, postpass-thunks :: <stretchy-vector>)
    => returns? :: <boolean>;
  if (member?(region.block-of, surrounding-blocks))
    add!(postpass-thunks,
	 method () => ();
	   delete-stuff-in(component, region);
	   replace-subregion(component, region.parent, region,
			     make(<empty-region>));
	 end method);
  end if;
  #f;
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
  select (op.primitive-name)
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

    #"make-next-method" =>
      let builder = make-builder(component);
      let assign = dep.dependent;
      let policy = assign.policy;
      let source = assign.source-location;

      let make-cookie-leaf
	= ref-dylan-defn(builder, policy, source, #"%make-next-method-cookie");
      insert-before(component, assign, builder-result(builder));
      replace-expression
	(component, dep,
	 make-unknown-call
	   (builder, make-cookie-leaf, #f,
	    listify-dependencies(op.depends-on)));

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

define method replace-placeholder
    (component :: <component>, dep :: <dependency>, op :: <unknown-call>)
    => ();
  for (dep = op.depends-on then dep.dependent-next,
       while: dep)
    replace-placeholder(component, dep, dep.source-exp);
  end;
  replace-placeholder-call-leaf(component, dep, op, op.depends-on.source-exp);
end method replace-placeholder;

define method replace-placeholder-call-leaf
    (component :: <component>, dep :: <dependency>, op :: <unknown-call>,
     func :: <leaf>)
    => ();
end method replace-placeholder-call-leaf;

define method replace-placeholder-call-leaf
    (component :: <component>, dep :: <dependency>, op :: <unknown-call>,
     func :: <literal-constant>)
    => ();
  replace-placeholder-call-ctv(component, dep, op, func.value);
end method replace-placeholder-call-leaf;

define method replace-placeholder-call-leaf
    (component :: <component>, dep :: <dependency>, op :: <unknown-call>,
     func :: <definition-constant-leaf>)
    => ();
  replace-placeholder-call-defn(component, dep, op, func.const-defn);
end method replace-placeholder-call-leaf;

define method replace-placeholder-call-ctv
    (component :: <component>, dep :: <dependency>, op :: <unknown-call>,
     func :: <ct-value>)
    => ();
end method replace-placeholder-call-ctv;

define method replace-placeholder-call-ctv
    (component :: <component>, dep :: <dependency>, op :: <unknown-call>,
     func :: <ct-function>)
    => ();
  let defn = func.ct-function-definition;
  if (defn)
    replace-placeholder-call-defn(component, dep, op, defn);
  end if;
end method replace-placeholder-call-ctv;

define method replace-placeholder-call-defn
    (component :: <component>, dep :: <dependency>, op :: <unknown-call>,
     func :: <definition>)
    => ();
end method replace-placeholder-call-defn;

define method replace-placeholder-call-defn
    (component :: <component>, dep :: <dependency>, op :: <unknown-call>,
     func :: <generic-definition>)
    => ();
  let discriminator = func.generic-defn-discriminator;
  if (discriminator)
    replace-expression(component, op.depends-on,
		       make-literal-constant(make-builder(component),
					     discriminator));
  end if;
end method replace-placeholder-call-defn;


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
      if (function.literal.visibility ~= #"local")
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
	       | (instance?(ref, <primitive>)
		    & ref.primitive-name == #"make-closure"))
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
			  derived-type: value-cell-type.ctype-extent);
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
	  op.derived-type := value-cell-type.ctype-extent;
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
     func.visibility := #"closure";
     let closure-size
       = for (dep = primitive.depends-on.dependent-next
		then dep.dependent-next,
	      res from 0,
	      while: dep)
	 finally
	   res;
	 end;
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
     ctv.has-general-entry? := #t;
     unless (func.general-entry)
       func.general-entry := build-xep(func, #f, component);
     end unless;
     if (instance?(func, <method-literal>) & ~func.generic-entry)
       ctv.has-generic-entry? := #t;
       func.generic-entry := build-xep(func, #t, component);
     end if;
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
	    dep, dependency, dependency.dependent);
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
