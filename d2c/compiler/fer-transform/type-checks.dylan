module: fer-transform
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


// Cheesy type check stuff.

// just-add-type-checks -- external.
//
define function just-add-type-checks (component :: <component>) => ();
  for (function in component.all-function-regions)
    add-type-checks-aux(component, function, ignore);
  end;
end;

// add-type-checks-aux -- external.
//
define method add-type-checks-aux
    (component :: <component>, region :: <simple-region>, reoptimize :: <function>) => ();
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
	      #f, list(temp, 
                       make-literal-constant(builder, asserted-type),
                       make-literal-constant(builder, 
                                             format-to-string("%=", assign.source-location)))));
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
      insert-after(component, assign, builder-result(builder), reoptimize);
      // Queue the assignment for reoptimization.
      reoptimize(component, assign);
    end;
  end;
end;

define method add-type-checks-aux
    (component :: <component>, region :: <compound-region>, reoptimize :: <function>) => ();
  for (subregion in region.regions)
    add-type-checks-aux(component, subregion, reoptimize);
  end;
end;

define method add-type-checks-aux
    (component :: <component>, region :: <if-region>, reoptimize :: <function>) => ();
  add-type-checks-aux(component, region.then-region, reoptimize);
  add-type-checks-aux(component, region.else-region, reoptimize);
end;

define method add-type-checks-aux
    (component :: <component>, region :: <body-region>, reoptimize :: <function>) => ();
  add-type-checks-aux(component, region.body, reoptimize);
end;

define method add-type-checks-aux
    (component :: <component>, region :: <exit>, reoptimize :: <function>) => ();
end;
