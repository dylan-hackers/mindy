Module: front
Description: implementation of Front-End-Representation builder
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/front/fer-builder.dylan,v 1.2 1994/12/13 13:22:18 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

// The actual implementation slots of all builders, which we mix in to actual
// instantiable builder classes.
//
define abstract class <internal-builder> (<flow-builder>)
  //
  // The component we're building for.
  slot component :: <component>, init-keyword: component:;
  //
  // Entries are either #"else" or a region.
  slot region-stack :: <list>, init-value: #();

  // A list of lists of the bodies being built.  Each sublist is in reverse
  // order.
  slot body-stack :: <list>,
    init-function: method () list(#()) end;
end class;


// Internal utilities:

// Finish up a body that we've been building, making a compound region to hold
// the result if necessary.
//
define method pop-canonical-body(builder, p-region)
 => res :: <region>;
  let stack = builder.body-stack;
  if (stack == #())
    error("Stack empty?");
  end;

  let body = stack.head;
  builder.body-stack := stack.tail;
  if (pair?(body) & (body.tail == #()))
    let res = body.head;
    res.parent := p-region;
    res;
  else
    let regions = reverse!(body);
    let res = make(<compound-region>, regions: regions, parent: p-region,
                   source-location: if (p-region) p-region.source-location
			       else make(<source-location>)
			       end);
    for (reg in regions)
      reg.parent := res;
    end;
    res;
  end;
end method;


// Add a region to the body we're building.
//
define method add-body-region(builder, region) => ();
  let bstack = builder.body-stack;
  bstack.head := pair(region, bstack.head);
end method;


// Add an assignment to the body we're building.  If the last region is
// currently a simple-region, just tack it on.  If not, make a new
// simple-region.
// 
define method add-body-assignment(builder, assign) => ();
  let bstack = builder.body-stack;
  let current = bstack.head;
  let simple
    = if (instance?(current.head, <simple-region>))
        current.head;
      else
        let new = make(<simple-region>, source-location: assign.source-location);
	bstack.head := pair(new, current);
	new;
      end if;

  assign.region := simple;

  let l-assign = simple.last-assign;
  if (l-assign == #f)
    simple.first-assign := assign;
    simple.last-assign := assign;
  else
    l-assign.next-op := assign;
    assign.prev-op := l-assign;
    simple.last-assign := assign;
  end;
end method;


// Given an operation and a list of operands, set up the <dependency> objects
// marking the operands.  The result is returned.
//
define method make-operand-dependencies(result, operands);
  let prev = #f;
  for (op in operands)
    let dep = make(<dependency>, source-exp: op, source-next: op.dependents,
    		   dependent: result);
    if (prev)
      prev.dependent-next := dep;
    else
      result.operands := dep;
    end;
    prev := dep;
    op.dependents := dep;
  end;
  result;
end method;


// Push new entries on region and body stacks (prepare to append to sub-body.)
//
define method push-body(builder, region) => ();
  builder.region-stack := pair(region, builder.region-stack);
  builder.body-stack := pair(#(), builder.body-stack);
end method;


// All-builder (theoretically non-FER specific) interface methods:

// Check if we've ended all of the enclosing regions, and reinitialize.
//
define method builder-result(builder :: <internal-builder>) => res :: <region>;
  let res = pop-canonical-body(builder, #f);
  unless (empty?(builder.region-stack))
    error("Still in the middle of building.")
  end;

  builder.body-stack := list(#());
  res;
end method;


define method build-region(builder :: <internal-builder>, region :: <region>)
 => ();
  add-body-region(builder, region);
end method;


// If "else", pop both bodies, otherwise a body-region with one body.  If the
// region is a method, add it to the component all-methods, otherwise append it
// to the parent body.
//
define method end-body(builder :: <internal-builder>)
 => res :: <region>;
  let region = builder.region-stack.head;
  builder.region-stack := builder.region-stack.tail;

  if (region == #"else")
    let region = builder.region-stack.head;
    builder.region-stack := builder.region-stack.tail;
    region.else-region := pop-canonical-body(builder, region);
    region.then-region := pop-canonical-body(builder, region);
    add-body-region(builder, region);
    region;
  else    
    region.body := pop-canonical-body(builder, region);
    if (instance?(region, <method-region>))
      let comp = builder.component;
      region.parent := comp;
      comp.all-methods := pair(region, comp.all-methods);
    else
      add-body-region(builder, region);
    end;
    region;
  end;
end method;


define method build-if-body
    (builder :: <internal-builder>, policy :: <policy>,
     source :: <source-location>,  predicate-leaf :: <leaf>)
 => ();
  ignore(policy);
  push-body(builder,
  	    make(<if-region>, if-test: predicate-leaf, source-location: source));
end method;


define method build-else
    (builder :: <internal-builder>, policy :: <policy>,
     source :: <source-location>)
 => ();
  ignore(source, policy);
  push-body(builder, #"else");
end method;


define method build-block-body
    (builder :: <internal-builder>, policy :: <policy>,
     source :: <source-location>)
 => res :: <fer-exit-block-region>;
  ignore(policy);
  let res = make(<fer-exit-block-region>, source-location: source);
  push-body(builder, res);
  res;
end method;


define method build-exit
    (builder :: <internal-builder>, policy :: <policy>,
     source :: <source-location>, target :: <block-region>)
 => ();
  ignore(policy);
  let res = make(<exit>, source-location: source, block: target);
  target.exits := pair(res, target.exits);
  add-body-region(builder, res);
  res;
end method;


define method build-loop-body
    (builder :: <internal-builder>, policy :: <policy>,
     source :: <source-location>)
 => ();
  ignore(policy);
  push-body(builder, make(<loop-region>, source-location: source));
end method;


define method build-assignment
    (builder :: <internal-builder>, policy :: <policy>,
     source :: <source-location>,
     target-vars :: type-or(<leaf>, <list>),
     source-exp :: <expression>)
 => ();
  let res = make(<fer-assignment>, source-location: source, policy: policy,
  		 expression: source-exp);
  let dep = make(<dependency>, source-exp: source-exp,
  		 source-next: source-exp.dependents,
		 dependent: res);
  source-exp.dependents := dep;
		 
  if (list?(target-vars))
    let prev = #f;
    for (var in target-vars)
      let new = make(<initial-definition>, var-info: var.var-info,
                     definition: var, definer: res,
		     expression: source-exp);
      if (prev)
        prev.definer-next := new;
      else
        res.defines := new;
      end;
      prev := new;
    end;
  else
    res.defines := make(<initial-definition>, var-info: target-vars.var-info,
                        definition: target-vars, definer: res);
  end;

  add-body-assignment(builder, res);
end method;		


// Make a join operation and a join assignment.  Add the assignment to the
// builder.
//
define method build-join
    (builder :: <internal-builder>, policy :: <policy>,
     source :: <source-location>,
     target-var :: <ssa-variable>,
     source1 :: <leaf>,
     source2 :: <leaf>)
 => ();
  let exp = make-operand-dependencies(make(<join-operation>),
  				      list(source1, source2));
  let res = make(<join-assignment>, source-location: source, policy: policy,
  		 defines: target-var, expression: exp);

  let dep = make(<dependency>, source-exp: exp, source-next: #f,
		 dependent: res);
  exp.dependents := dep;

  target-var.definer := res;
  add-body-assignment(builder, res);
end method;

  
define method make-operation
    (builder :: <internal-builder>, operands :: <list>)
 => res :: <operation>;
  ignore(builder);
  make-operand-dependencies(make(<call>), operands);
end method;



// Actual FER specific stuff:

define class <internal-fer-builder> (<fer-builder>, <internal-builder>)
end;

define method make-builder(wot :: <fer-component>)
 => res :: <internal-fer-builder>;
  make(<internal-fer-builder>, component: wot);
end method;

define method make-builder(wot :: <internal-fer-builder>)
 => res :: <internal-fer-builder>;
  make(<internal-fer-builder>, component: wot.component);
end method;


define method make-mv-operation
    (builder :: <fer-builder>, operands :: <list>)
 => res :: <operation>;
  ignore(builder);
  make-operand-dependencies(make(<mv-call>), operands);
end method;


define method build-let
    (builder :: <fer-builder>, policy :: <policy>,
     source :: <source-location>,
     target-vars :: type-or(<leaf>, <list>),
     source-exp :: <expression>)
 => ();
  // ### lex var create point notation needed.
  build-assignment(builder, policy, source, target-vars, source-exp);
end method;


define method make-literal-constant
    (builder :: <fer-builder>, value :: <ct-value>)
 => res :: <literal-constant>;
  ignore(builder);
  make(<literal-constant>, value: value);
end method;


define method make-definition-leaf
    (builder :: <fer-builder>, defn :: <definition>)
 => res :: <definition-constant-leaf>;
  ignore(builder);
  make(<definition-constant-leaf>, const-defn: defn);
end method;

define method make-definition-leaf
    (builder :: <fer-builder>, defn :: <variable-definition>)
 => res :: <global-variable>;
  ignore(builder);
  make(<global-variable>,
       var-info: make(<module-var-info>,
		      var-defn: defn,
		      asserted-type: defn.defn-type));
end method;


define method make-lexical-var
    (builder :: <fer-builder>, source :: <source-location>, of-type :: <ctype>)
 => res :: <initial-variable>;
  ignore(builder);
  make(<initial-variable>,
       var-info: make(<lexical-var-info>, asserted-type: of-type,
       		      source-location: source));
end method;


define method make-local-var
    (builder :: <fer-builder>, name :: <symbol>, of-type :: <ctype>)
 => res :: <initial-variable>;
  make(<initial-variable>,
       var-info: make(<local-var-info>, asserted-type: of-type,
       		      debug-name: name));
end method;


define method make-values-cluster
    (builder :: <fer-builder>, name :: <symbol>, of-type :: <values-ctype>)
 => res :: <initial-variable>;
  make(<initial-variable>,
       var-info: make(<values-cluster-info>, asserted-type: of-type,
       		      debug-name: name));
end method;


define method copy-variable
    (builder :: <fer-builder>, var :: <initial-variable>)
 => res :: <initial-variable>;

  // ### may need to actually copy var-info...
  make(<initial-variable>, var-info: var.var-info);
end method;


// If we've already made an exit function, return it, otherwise make one.
//
define method make-exit-function
    (builder :: <fer-builder>, target :: <fer-exit-block-region>)
 => res :: <leaf>;
  target.exit-function
    | (target.exit-function
         := make(<exit-function>, source-location: target.source-location,
  		 target-region: target));
end;


// Note that pop-canonical-body places the method in the component all-methods.
//
define method build-method-body
    (builder :: <fer-builder>, policy :: <policy>,
     source :: <source-location>,
     arg-vars :: <list>,
     result-vars :: type-or(<leaf>, <list>))
 => res :: <leaf>;
  ignore(policy);
  let leaf = make(<lambda>, source-location: source, vars: arg-vars,
		  result: if (list?(result-vars))
			    result-vars;
			  else
			    list(result-vars);
			  end);
  push-body(builder, leaf);
  leaf;
end method;
