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

/*
define function debug-reoptimize (component :: <component>, frob) => ()
//  dformat("*** Skipping re-optimization!\n");
end;
*/

// SSA conversion.

// convert-component-to-ssa -- external.
//
define function convert-component-to-ssa (component :: <component>) => ();
  while (component.initial-variables)
    let init-var = component.initial-variables;
    component.initial-variables := init-var.next-initial-variable;
    init-var.next-initial-variable := #f;
    maybe-convert-to-ssa(component, init-var, ignore);
  end;
end function convert-component-to-ssa;

// expand-component-clusters -- external.
//
define function expand-component-clusters (component :: <component>) => ();
  // Massage code to be in a form that maybe-expand-cluster can work with...
  traverse-component(component, <assignment>, fixup-assignment);
//  traverse-component(component, <abstract-variable>, rcurry(maybe-expand-cluster, reoptimize));
  traverse-component(component, <abstract-variable>, rcurry(maybe-expand-cluster, ignore));
end function expand-component-clusters;



// maybe-convert-to-ssa -- external.
//
define method maybe-convert-to-ssa
    (component :: <component>, var :: <initial-variable>, reoptimize :: <function>) => ();
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



// Value-cluster expansion

// maybe-expand-cluster -- internal.
//
define function maybe-expand-cluster
    (component :: <component>, cluster :: <abstract-variable>, reoptimize :: <function>)
    => ();
  if (instance?(cluster.var-info, <values-cluster-info>)
	& fixed-number-of-values?(cluster.derived-type))
    unless (cluster.dependents)
      error("Trying to expand a cluster that isn't being used?");
    end;
    if (cluster.dependents.source-next)
      error("Trying to expand a cluster that is referenced "
	      "in more than one place?");
    end;
    expand-cluster(component, cluster, cluster.derived-type.min-values, #(), reoptimize);
  end if;
end;

// expand-cluster -- external.
//
define generic expand-cluster 
    (component :: <component>, cluster :: <abstract-variable>,
     number-of-values :: <integer>, names :: <list>, reoptimize :: <function>)
    => ();

define method expand-cluster 
    (component :: <component>, cluster :: <ssa-variable>,
     number-of-values :: <integer>, names :: <list>, reoptimize :: <function>)
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
    let var = make(<ssa-variable>, var-info: var-info, definer: assign,
		   definer-next: new-defines, needs-type-check: #f);
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
     number-of-values :: <integer>, names :: <list>, reoptimize :: <function>)
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
			   definer-next: next-define,
			   needs-type-check: #f);
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

// Helper functions for cluster expansion

// fixup-assignment -- internal.
//
define function fixup-assignment
    (component :: <component>, assignment :: <assignment>) => ();
  if (assignment.defines
	& instance?(assignment.defines.var-info, <values-cluster-info>))
    maybe-restrict-type(component, assignment.defines, 
			assignment.depends-on.source-exp.derived-type,
			ignore);
  end if;
end function;

// queue-dependents -- external.
//
define inline function queue-dependents
    (component :: <component>, expr :: <expression>, reoptimize :: <function>) => ();
  for (dependency = expr.dependents then dependency.source-next,
       while: dependency)
    reoptimize(component, dependency.dependent);
  end;
end;

// maybe-restrict-type -- external.
//
define generic maybe-restrict-type
    (component :: <component>, expr :: <expression>, type :: <values-ctype>,
     reoptimize :: <function>)
    => ();

define method maybe-restrict-type
    (component :: <component>, expr :: <expression>, type :: <values-ctype>,
     reoptimize :: <function>)
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
	      maybe-restrict-type(component, var, var-type, reoptimize);
	    end for;
	  end block;
	end if;
      end if;
      queue-dependents(component, expr, reoptimize);
    end if;
  end unless;
end method maybe-restrict-type;


define method maybe-restrict-type
    (component :: <component>, var :: <abstract-variable>, type :: <values-ctype>,
     reoptimize :: <function>, #next next-method)
    => ();
  let var-info = var.var-info;
  next-method(component, var,
	      if (instance?(var-info, <values-cluster-info>))
		values-type-intersection(type, var-info.asserted-type);
	      else
		ctype-intersection(defaulted-type(type, 0),
				   var-info.asserted-type);
	      end,
	      reoptimize);
end;


define method maybe-restrict-type
    (component :: <component>, var :: <definition-site-variable>,
     type :: <values-ctype>,
     reoptimize :: <function>, #next next-method)
    => ();
  if (var.needs-type-check?
	& values-subtype?(type, var.var-info.asserted-type.ctype-extent))
    var.needs-type-check? := #f;
    reoptimize(component, var.definer);
  end;
  next-method();
end;


// defaulted-type -- external.
//
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


// fixed-number-of-values? -- external.
//
define generic fixed-number-of-values? (ctype :: <values-ctype>) => res :: <boolean>;

define method fixed-number-of-values? (ctype :: <ctype>) => res :: <boolean>;
  #t;
end;

define method fixed-number-of-values?
    (ctype :: <values-ctype>) => res :: <boolean>;
  ctype.min-values == ctype.positional-types.size
    & ctype.rest-value-type == empty-ctype();
end;

