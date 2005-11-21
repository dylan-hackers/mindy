module: front
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

// Clone a (top-level) function and everything it references.


define class <clone-state> (<object>)
  //
  // The builder used to build the clone.
  constant slot clone-builder :: <fer-builder>,
    required-init-keyword: builder:;
  //
  // The function-region we are currently building.  Needed for
  // self-tail-calls.
  slot clone-function-region :: false-or(<fer-function-region>),
    init-value: #f;
  //
  // Hash table mapping original things to new things.
  constant slot cloned-stuff :: <object-table> = make(<object-table>);
  //
  // A function to compute the new source-location of a FER feature.
  // It will be called with arguments (original-location, clone-state)
  constant slot relocator :: <function>,
    required-init-keyword: relocator:;
end;


// clone-function -- exported.
//
define method clone-function
    (component :: <fer-component>,
     function :: <function-literal>,
     location-transformer :: <function>)
    => clone :: <function-literal>;
  let state = make(<clone-state>,
		   builder: make-builder(component),
		   relocator: location-transformer);
  state.cloned-stuff[function.main-entry.parent] := component;
  clone-expr(function, state);
end;


define method maybe-clone (thing, state :: <clone-state>, cloner :: <function>)
    => clone;
  element(state.cloned-stuff, thing, default: #f)
    | (element(state.cloned-stuff, thing) := cloner());
end;  


define method clone-expr
    (var :: <ssa-variable>, state :: <clone-state>) => clone :: <ssa-variable>;
  maybe-clone(var, state,
	      method ()
		make(<ssa-variable>,
		     derived-type: var.derived-type,
		     var-info: var.var-info,
                     needs-type-check: var.needs-type-check?);
	      end);
end;

define method clone-expr
    (var :: <initial-variable>, state :: <clone-state>)
    => clone :: <initial-variable>;
  maybe-clone(var, state,
	      method ()
		make-initial-var(state.clone-builder, var.derived-type,
				 var.var-info);
	      end);
end;

define method clone-expr
    (const :: <literal-constant>, state :: <clone-state>)
    => clone :: <literal-constant>;
  make-literal-constant(state.clone-builder, const.value);
end;

define method clone-expr
    (const :: <definition-constant-leaf>, state :: <clone-state>)
    => clone :: <definition-constant-leaf>;
  make-definition-constant(state.clone-builder, const.const-defn);
end;

define method clone-expr
    (expr :: <uninitialized-value>, state :: <clone-state>)
    => clone :: <uninitialized-value>;
  make(<uninitialized-value>, derived-type: expr.derived-type);
end;

define method clone-expr
    (expr :: <primitive>, state :: <clone-state>)
    => clone :: <primitive>;
  make-operation(state.clone-builder, <primitive>,
		 clone-arguments(expr.depends-on, state),
		 derived-type: expr.derived-type,
		 name: expr.primitive-name);
end;

define method clone-expr
    (expr :: <known-call>, state :: <clone-state>)
    => clone :: <known-call>;
  make-operation(state.clone-builder, <known-call>,
		 clone-arguments(expr.depends-on, state),
		 derived-type: expr.derived-type);
end;

define method clone-expr
    (expr :: <unknown-call>, state :: <clone-state>)
    => clone :: <unknown-call>;
  make-operation(state.clone-builder, <unknown-call>,
		 clone-arguments(expr.depends-on, state),
		 derived-type: expr.derived-type,
		 use-generic-entry: expr.use-generic-entry?);
end;

define method clone-expr
    (expr :: <mv-call>, state :: <clone-state>)
    => clone :: <mv-call>;
  make-operation(state.clone-builder, <mv-call>,
		 clone-arguments(expr.depends-on, state),
		 derived-type: expr.derived-type,
		 use-generic-entry: expr.use-generic-entry?);
end;

define method clone-expr
    (expr :: <error-call>, state :: <clone-state>)
    => clone :: <error-call>;
  make-operation(state.clone-builder, <error-call>,
		 clone-arguments(expr.depends-on, state),
		 derived-type: expr.derived-type);
end;

define method clone-expr
    (expr :: <delayed-optimization-call>, state :: <clone-state>)
    => clone :: <delayed-optimization-call>;
  make-operation(state.clone-builder, <delayed-optimization-call>,
		 clone-arguments(expr.depends-on, state),
		 derived-type: expr.derived-type,
		 use-generic-entry: expr.use-generic-entry?);
end;

define method clone-expr
    (expr :: <prologue>, state :: <clone-state>)
    => clone :: <prologue>;
  state.clone-function-region.prologue;
end;

define method clone-expr
    (expr :: <module-var-ref>, state :: <clone-state>)
    => clone :: <module-var-ref>;
  make-operation(state.clone-builder, <module-var-ref>,
		 clone-arguments(expr.depends-on, state),
		 derived-type: expr.derived-type,
		 var: expr.variable);
end;

define method clone-expr
    (expr :: <module-var-set>, state :: <clone-state>)
    => clone :: <module-var-set>;
  make-operation(state.clone-builder, <module-var-set>,
		 clone-arguments(expr.depends-on, state),
		 derived-type: expr.derived-type,
		 var: expr.variable);
end;

define method clone-expr
    (expr :: <heap-slot-ref>, state :: <clone-state>)
    => clone :: <heap-slot-ref>;
  make-operation(state.clone-builder, <heap-slot-ref>,
		 clone-arguments(expr.depends-on, state),
		 derived-type: expr.derived-type,
		 slot-info: expr.slot-info);
end;

define method clone-expr
    (expr :: <data-word-ref>, state :: <clone-state>)
    => clone :: <data-word-ref>;
  make-operation(state.clone-builder, <data-word-ref>,
		 clone-arguments(expr.depends-on, state),
		 derived-type: expr.derived-type,
		 slot-info: expr.slot-info);
end;

define method clone-expr
    (expr :: <heap-slot-set>, state :: <clone-state>)
    => clone :: <heap-slot-set>;
  make-operation(state.clone-builder, <heap-slot-set>,
		 clone-arguments(expr.depends-on, state),
		 slot-info: expr.slot-info);
end;

define method clone-expr
    (expr :: <truly-the>, state :: <clone-state>)
    => clone :: <truly-the>;
  make-operation(state.clone-builder, <truly-the>,
		 clone-arguments(expr.depends-on, state),
		 derived-type: expr.derived-type,
		 guaranteed-type: expr.guaranteed-type);
end;

define method clone-expr
    (expr :: <instance?>, state :: <clone-state>)
    => clone :: <instance?>;
  make-operation(state.clone-builder, <instance?>,
		 clone-arguments(expr.depends-on, state),
		 derived-type: expr.derived-type,
		 type: expr.type);
end;


define method clone-nlx-info
    (info :: <nlx-info>, state :: <clone-state>)
  maybe-clone(info, state, curry(make, <nlx-info>));
end;

define method clone-expr
    (expr :: <catch>, state :: <clone-state>)
    => clone :: <catch>;
  make-operation(state.clone-builder, <catch>,
		 clone-arguments(expr.depends-on, state),
		 derived-type: expr.derived-type,
		 nlx-info: clone-nlx-info(expr.nlx-info, state));
end;

define method clone-expr
    (expr :: <throw>, state :: <clone-state>)
    => clone :: <throw>;
  make-operation(state.clone-builder, <throw>,
		 clone-arguments(expr.depends-on, state),
		 derived-type: expr.derived-type,
		 nlx-info: clone-nlx-info(expr.nlx-info, state));
end;

define method clone-expr
    (expr :: <make-catcher>, state :: <clone-state>)
    => clone :: <make-catcher>;
  make-operation(state.clone-builder, <make-catcher>,
		 clone-arguments(expr.depends-on, state),
		 derived-type: expr.derived-type,
		 nlx-info: clone-nlx-info(expr.nlx-info, state));
end;

define method clone-expr
    (expr :: <disable-catcher>, state :: <clone-state>)
    => clone :: <disable-catcher>;
  make-operation(state.clone-builder, <disable-catcher>,
		 clone-arguments(expr.depends-on, state),
		 derived-type: expr.derived-type,
		 nlx-info: clone-nlx-info(expr.nlx-info, state));
end;

define inline function relocate
    (orig-location :: <source-location>, state :: <clone-state>)
 => new-location :: <source-location>;
  state.relocator(orig-location, state)
end;

define method clone-expr
    (function :: <function-literal>, state :: <clone-state>)
    => clone :: type-union(<function-literal>, <uninitialized-value>);
  maybe-clone
    (function, state,
     method ()
       let placeholder = make(<uninitialized-value>);
       element(state.cloned-stuff, function) := placeholder;
       let orig-region = function.main-entry;
       let region-clone
	 = make(if (instance?(orig-region, <lambda>))
		  <lambda>;
		else
		  <fer-function-region>;
		end,
		source-location: relocate(orig-region.source-location, state),
		name: orig-region.name,
		argument-types: orig-region.argument-types,
		result-type: orig-region.result-type,
		hidden-references: orig-region.hidden-references?,
		calling-convention: orig-region.calling-convention);
       push-body(state.clone-builder, region-clone);
       add!(state.clone-builder.component.all-function-regions, region-clone);
       state.cloned-stuff[orig-region] := region-clone;
       let prev-function-region = state.clone-function-region;
       state.clone-function-region := region-clone;
       clone-region(orig-region.body, state);
       state.clone-function-region := prev-function-region;
       end-body(state.clone-builder);
       if (orig-region.self-call-block)
	 region-clone.self-call-block
	   := state.cloned-stuff[orig-region.self-call-block];
       end;
       let func
	 = make-function-literal(state.clone-builder, #f,
				 select(function by instance?)
				   <callback-literal> => #"callback";
				   <method-literal> => #"method";
				   <function-literal> => #"function";
				 end,
				 function.visibility, function.signature,
				 region-clone);
       element(state.cloned-stuff, function) := func;
       let next = #f;
       for (dep = placeholder.dependents then next,
	    while: dep)
	 next := dep.source-next;
	 dep.source-exp := func;
	 dep.source-next := func.dependents;
	 func.dependents := dep;
       end;
       func;
     end);
end;

define method clone-expr
    (expr :: <exit-function>, state :: <clone-state>)
    => clone :: <exit-function>;
  make-exit-function(state.clone-builder,
		     clone-nlx-info(expr.nlx-info, state),
		     clone-expr(expr.depends-on.source-exp, state));
end;


define generic clone-arguments
    (dep :: false-or(<dependency>), state :: <clone-state>)
    => res :: <list>;

define method clone-arguments (dep :: <dependency>, state :: <clone-state>)
    => res :: <pair>;
  pair(clone-expr(dep.source-exp, state),
       clone-arguments(dep.dependent-next, state));
end;

define method clone-arguments (dep :: <false>, state :: <clone-state>)
    => res :: <empty-list>;
  #();
end;



define method clone-region
    (region :: <simple-region>, state :: <clone-state>) => ();
  for (assign = region.first-assign then assign.next-op,
       while: assign)
    select (assign by instance?)
      <let-assignment> =>
	build-let(state.clone-builder, assign.policy, relocate(assign.source-location, state),
		  clone-defines(assign.defines, state),
		  clone-expr(assign.depends-on.source-exp, state));
      <set-assignment> =>
	build-assignment(state.clone-builder, assign.policy,
			 relocate(assign.source-location, state),
			 clone-defines(assign.defines, state),
			 clone-expr(assign.depends-on.source-exp, state));
    end;
  end;
end;

define generic clone-defines
    (var :: false-or(<definition-site-variable>), state :: <clone-state>)
    => res :: <list>;

define method clone-defines (var :: <ssa-variable>, state :: <clone-state>)
    => res :: <pair>;
  pair(clone-expr(var, state), clone-defines(var.definer-next, state));
end;

define method clone-defines
    (var :: <initial-definition>, state :: <clone-state>)
    => res :: <pair>;
  pair(clone-expr(var.definition-of, state),
       clone-defines(var.definer-next, state));
end;

define method clone-defines
    (var :: <false>, state :: <clone-state>)
    => res :: <empty-list>;
  #();
end;



define method clone-region
    (region :: <compound-region>, state :: <clone-state>) => ();
  for (subregion in region.regions)
    clone-region(subregion, state);
  end;
end;

define method clone-region
    (region :: <if-region>, state :: <clone-state>) => ();
  build-if-body(state.clone-builder, $Default-Policy, relocate(region.source-location, state),
		clone-expr(region.depends-on.source-exp, state));
  clone-region(region.then-region, state);
  build-else(state.clone-builder, $Default-Policy, relocate(region.source-location, state));
  clone-region(region.else-region, state);
  end-body(state.clone-builder);
end;

define method clone-region
    (region :: <block-region>, state :: <clone-state>) => ();
  let clone = build-block-body(state.clone-builder, $Default-Policy,
			       relocate(region.source-location, state));
  state.cloned-stuff[region] := clone;
  clone-region(region.body, state);
  end-body(state.clone-builder);
end;

define method clone-region
    (region :: <loop-region>, state :: <clone-state>) => ();
  build-loop-body(state.clone-builder, $Default-Policy,
		  relocate(region.source-location, state));
  clone-region(region.body, state);
  end-body(state.clone-builder);
end;

define method clone-region
    (region :: <exit>, state :: <clone-state>) => ();
  build-exit(state.clone-builder, $Default-Policy, relocate(region.source-location, state),
	     state.cloned-stuff[region.block-of]);
end;

define method clone-region
    (region :: <return>, state :: <clone-state>) => ();
  build-return(state.clone-builder, $Default-Policy, relocate(region.source-location, state),
	       state.cloned-stuff[region.block-of],
	       clone-arguments(region.depends-on, state));
end;

define method clone-region
    (region :: <unwind-protect-region>, state :: <clone-state>) => ();
  build-unwind-protect-body
    (state.clone-builder, $Default-Policy, relocate(region.source-location, state),
     clone-expr(region.uwp-region-cleanup-function, state));
  clone-region(region.body, state);
  end-body(state.clone-builder);
end;


// Seals for file clone.dylan

// <clone-state> -- subclass of <object>
define sealed domain make(singleton(<clone-state>));
define sealed domain initialize(<clone-state>);
