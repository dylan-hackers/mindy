module: cheese
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

//
// Constraint Propagation.
//
// This file implements propagate-constraints.  Currently, we only implement
// constraints that can be represented via the type.  But that alone lets us
// squeeze out a lot of type tests.
// 

// propagate-constraints -- internal interface.
//
// Propagate whatever constraints we can.
// 
define method propagate-constraints (component :: <component>)
  for (function in component.all-function-regions)
    propagate-constraints-region
      (component, function.body, make(<renaming-set>));
  end for;
end method propagate-constraints;

// <renaming> -- internal.
//
// Represents the ``renaming'' of one ssa variable to some other leaf.  We
// represent constraints by renaming the constrained variable to something
// else that includes all the new information the constraint imposes.
// 
define class <renaming> (<object>)
  //
  // The variable being renamed.
  constant slot renaming-original :: <ssa-variable>,
    required-init-keyword: original:;
  //
  // What to replace it with.  If this is a <leaf>, then replace the original
  // with it.  If it is a <function>, then call it to produce the <leaf> (and
  // remember the <leaf> for next time).
  slot %renaming-replacement :: type-union(<leaf>, <function>),
    required-init-keyword: replacement:;
end class <renaming>;

define sealed domain make (singleton(<renaming>));
define sealed domain initialize (<renaming>);

// renaming-replacement -- internal.
//
// Wrapper accessor on %renaming-replacement that invokes the function if
// it is one.
// 
define method renaming-replacement (renaming :: <renaming>) => res :: <leaf>;
  let leaf-or-func = renaming.%renaming-replacement;
  if (instance?(leaf-or-func, <function>))
    renaming.%renaming-replacement := leaf-or-func();
  else
    leaf-or-func;
  end if;
end method renaming-replacement;

// <renaming-set> -- internal.
//
// A set of renamings active at some point.  Renaming-sets are layered though
// the inherit-from slot so that we can control the scope of side effects.
// 
define class <renaming-set> (<object>)
  //
  // The set of renamings we subsume.
  constant slot renaming-set-inherit-from :: false-or(<renaming-set>) = #f,
    init-keyword: inherit-from:;
  //
  // Object-table mapping <ssa-variable>s to <renaming>s.
  constant slot renaming-set-renamings :: <object-table>
    = make(<object-table>);
end class <renaming-set>;

define sealed domain make (singleton(<renaming-set>));
define sealed domain initialize (<renaming-set>);

// add-renaming -- internal.
//
// Add another renaming to the set.
// 
define method add-renaming
    (set :: <renaming-set>, original :: <ssa-variable>,
     replacement :: type-union(<leaf>, <function>))
    => ();
  let renaming = make(<renaming>, original: original,
		      replacement: replacement);
  set.renaming-set-renamings[original] := renaming;
end method add-renaming;

define method find-renaming (set :: <renaming-set>, var :: <ssa-variable>)
    => renaming :: false-or(<renaming>);
  element(set.renaming-set-renamings, var, default: #f)
    | (set.renaming-set-inherit-from
	 & find-renaming(set.renaming-set-inherit-from, var));
end method find-renaming;


define method apply-renamings-to-dependencies
    (component :: <component>, dependent :: <dependent-mixin>,
     renamings :: <renaming-set>)
    => ();
  for (dep = dependent.depends-on then dep.dependent-next,
       while: dep)
    apply-renamings(component, dep, dep.source-exp, renamings);
  end for;
end method apply-renamings-to-dependencies;


define generic apply-renamings
    (component :: <component>, dep :: <dependency>, expr :: <expression>,
     renamings :: <renaming-set>)
    => ();
    
define method apply-renamings
    (component :: <component>, dep :: <dependency>, expr :: <expression>,
     renamings :: <renaming-set>)
    => ();
end method apply-renamings;

define method apply-renamings
    (component :: <component>, dep :: <dependency>, var :: <ssa-variable>,
     renamings :: <renaming-set>)
    => ();
  let renaming = find-renaming(renamings, var);
  if (renaming)
    replace-expression(component, dep, renaming.renaming-replacement);
  end if;
end method apply-renamings;

define method apply-renamings
    (component :: <component>, dep :: <dependency>, op :: <operation>,
     renamings :: <renaming-set>)
    => ();
  apply-renamings-to-dependencies(component, op, renamings);
end method apply-renamings;

define method apply-renamings
    (component :: <component>, dep :: <dependency>, op :: <truly-the>,
     renamings :: <renaming-set>)
    => ();
end method apply-renamings;

define method apply-renamings
    (component :: <component>, dep :: <dependency>,
     exit-func :: <exit-function>, renamings :: <renaming-set>)
    => ();
  apply-renamings-to-dependencies(component, exit-func, renamings);
end method apply-renamings;




define generic propagate-constraints-region
    (component :: <component>, region :: <region>, renamings :: <renaming-set>)
    => ();

define method propagate-constraints-region
    (component :: <component>, region :: <simple-region>,
     renamings :: <renaming-set>)
    => ();
  for (assign = region.first-assign then assign.next-op,
       while: assign)
    //
    // Apply any incoming renamings to whatever this assign references.
    apply-renamings-to-dependencies(component, assign, renamings);
    //
    // If this assignment is just a type assertion, then add a renaming so
    // that additional references of this variable will also see the asserted
    // type.
    let def = assign.defines;
    if (def & def.needs-type-check? & instance?(def, <ssa-variable>))
      let src = assign.depends-on.source-exp;
      if (instance?(src, <ssa-variable>)
	    & csubtype?(def.derived-type, src.derived-type))
	add-renaming(renamings, src, def);
      end if;
    end if;
  end for;
end method propagate-constraints-region;

define method propagate-constraints-region
    (component :: <component>, region :: <compound-region>,
     renamings :: <renaming-set>)
    => ();
  for (subregion in region.regions)
    propagate-constraints-region(component, subregion, renamings);
  end for;
end method propagate-constraints-region;

define method propagate-constraints-region
    (component :: <component>, region :: <if-region>,
     renamings :: <renaming-set>)
    => ();
  apply-renamings-to-dependencies(component, region, renamings);
  let then-renamings = make(<renaming-set>, inherit-from: renamings);
  let else-renamings = make(<renaming-set>, inherit-from: renamings);
  extract-if-constraints(component, region, region.depends-on.source-exp,
			 then-renamings, else-renamings);
  propagate-constraints-region(component, region.then-region, then-renamings);
  propagate-constraints-region(component, region.else-region, else-renamings);
end;

define method propagate-constraints-region
    (component :: <component>, region :: <body-region>,
     renamings :: <renaming-set>)
    => ();
  propagate-constraints-region(component, region.body,
			       make(<renaming-set>, inherit-from: renamings));
end;

define method propagate-constraints-region
    (component :: <component>, region :: <exit>,
     renamings :: <renaming-set>)
    => ();
end;

define method propagate-constraints-region
    (component :: <component>, region :: <return>,
     renamings :: <renaming-set>)
    => ();
  apply-renamings-to-dependencies(component, region, renamings);
end;



define generic extract-if-constraints
    (component :: <component>, region :: <if-region>, cond :: <expression>,
     then-renamings :: <renaming-set>, else-renamings :: <renaming-set>)
    => ();

define method extract-if-constraints
    (component :: <component>, region :: <if-region>, cond :: <expression>,
     then-renamings :: <renaming-set>, else-renamings :: <renaming-set>)
    => ();
end method extract-if-constraints;

define method extract-if-constraints
    (component :: <component>, region :: <if-region>, cond :: <ssa-variable>,
     then-renamings :: <renaming-set>, else-renamings :: <renaming-set>)
    => ();
  let cond-type = cond.derived-type;
  if (csubtype?(cond-type, boolean-ctype()))
    add-renaming(then-renamings,
		 cond,
		 method () => new :: <leaf>;
		   make-literal-constant(make-builder(component), #t);
		 end method);
  else
    maybe-constrain-type
      (component, region.then-region, then-renamings, cond,
       ctype-difference(cond-type, specifier-type(#"<false>").ctype-extent));
  end if;
  add-renaming(else-renamings,
	       cond,
	       method () => new :: <leaf>;
		 make-literal-constant(make-builder(component), #f);
	       end method);
  extract-if-constraints(component, region,
			 cond.definer.depends-on.source-exp,
			 then-renamings, else-renamings);
end method extract-if-constraints;

define method extract-if-constraints
    (component :: <component>, region :: <if-region>, cond :: <primitive>, 
     then-renamings :: <renaming-set>, else-renamings :: <renaming-set>)
    => ();
  select (cond.primitive-name)
    #"==", #"fixnum-=", #"single-==", #"double-==", #"extended-==" =>
      let x = cond.depends-on.source-exp;
      let y = cond.depends-on.dependent-next.source-exp;
      constrain-as-identical
	(component, region.then-region, then-renamings, x, y);
      constrain-as-identical
	(component, region.then-region, then-renamings, y, x);
      constrain-as-different
	(component, region.else-region, else-renamings, x, y);
      constrain-as-different
	(component, region.else-region, else-renamings, y, x);
      
    otherwise =>
      #f;
  end select;
end method extract-if-constraints;

define method extract-if-constraints
    (component :: <component>, region :: <if-region>, cond :: <instance?>,
     then-renamings :: <renaming-set>, else-renamings :: <renaming-set>)
    => ();
  let value = cond.depends-on.source-exp;
  if (instance?(value, <ssa-variable>))
    let type = cond.type.ctype-extent;
    maybe-constrain-type(component, region.then-region, then-renamings, value,
			 ctype-intersection(value.derived-type, type));
    maybe-constrain-type(component, region.else-region, else-renamings, value,
			 ctype-difference(value.derived-type, type));
  end if;
end method extract-if-constraints;





define method constrain-as-identical
    (component :: <component>, inside :: <region>, renamings :: <renaming-set>,
     x :: <leaf>, y :: <leaf>)
    => ();
end method constrain-as-identical;

define method constrain-as-identical
    (component :: <component>, inside :: <region>, renamings :: <renaming-set>,
     x :: <ssa-variable>, y :: <leaf>)
    => ();
  maybe-constrain-type(component, inside, renamings, x,
		       ctype-intersection(x.derived-type, y.derived-type));
end method constrain-as-identical;

define method constrain-as-identical
    (component :: <component>, inside :: <region>, renamings :: <renaming-set>,
     x :: <ssa-variable>, y :: <literal-constant>)
    => ();
  add-renaming(renamings, x, y);
end method constrain-as-identical;


define method constrain-as-different
    (component :: <component>, inside :: <region>, renamings :: <renaming-set>,
     x :: <leaf>, y :: <leaf>)
    => ();
end method constrain-as-different;

define method constrain-as-different
    (component :: <component>, inside :: <region>, renamings :: <renaming-set>,
     x :: <ssa-variable>, y :: <literal-constant>)
    => ();
  let ctv = y.value;
  if (instance?(ctv, <eql-ct-value>))
    maybe-constrain-type
      (component, inside, renamings, x,
       ctype-difference
	 (x.derived-type, make(<singleton-ctype>, value: ctv).ctype-extent));
  end if;
end method constrain-as-different;



define method maybe-constrain-type
    (component :: <component>, inside :: <region>, renamings :: <renaming-set>,
     var :: <ssa-variable>, new-type :: <ctype>)
    => ();
  unless (csubtype?(var.derived-type, new-type))
    add-renaming(renamings,
		 var,
		 method () => new :: <leaf>;
		   insert-type-constraint(component, inside, var, new-type);
		 end method);
  end unless;
end method maybe-constrain-type;



define method insert-type-constraint
    (component :: <component>, inside :: <region>,
     var :: <ssa-variable>, new-type :: <ctype>)
    => replacement :: <leaf>;
  let builder = make-builder(component);
  let policy = $Default-Policy;
  let source = make(<unknown-source-location>);

  let temp = make-local-var(builder, var.var-info.debug-name, new-type);
  build-assignment(builder, policy, source, temp,
		   make-operation(builder, <truly-the>, list(var),
				  guaranteed-type: new-type));
  replace-subregion
    (component, inside.parent, inside,
     combine-regions(component, builder-result(builder), inside));
  temp;
end method insert-type-constraint;
