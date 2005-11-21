module: cheese
copyright: see below


//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
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

// common-subexpression-elimination -- internal to cheese
//
// Eliminate all common sub-expressions in component.
// 
define method common-subexpression-elimination
    (component :: <component>) => ();
  for (function in component.all-function-regions)
    cse-region(component, function.body, make(<operation-table>));
  end for;
end method common-subexpression-elimination;


// Operation tables.

// <operation-table> -- internal.
//
// Used to hold the set of operations currently available for cse.
//
define class <operation-table> (<object>)
  //
  // Set of operations.
  constant slot ot-operations :: <stretchy-vector> = make(<stretchy-vector>);
  //
  // Operation table we inherit from,
  constant slot ot-inherit-from :: false-or(<operation-table>) = #f,
    init-keyword: inherit-from:;
end class <operation-table>;


// find-operation -- internal.
//
// Find an equivalent operation, or return #f if there isn't one.
// 
define method find-operation
    (table :: <operation-table>, op :: <operation>)
    => old-operation :: false-or(<operation>);
  block (return)
    for (old-op in table.ot-operations)
      block (next)
	if (same-operation?(old-op, op))
	  for (old-dep = old-op.depends-on then old-dep.dependent-next,
	       dep = op.depends-on then dep.dependent-next)
	    if (old-dep)
	      if (dep)
		unless (same-leaf?(old-dep.source-exp, dep.source-exp))
		  next();
		end unless;
	      else
		next();
	      end if;
	    else
	      if (dep)
		next();
	      else
		return(old-op);
	      end if;
	    end if;
	  end for;
	end if;
      end block;
    end for;
    table.ot-inherit-from & find-operation(table.ot-inherit-from, op);
  end block;
end method find-operation;


// same-operation? -- internal.
//
// Returns #f iff the two operations are the same.  Is only responsible for
// looking at the operation-type specific stuff.  It is the callers job to
// very that the operands are also the same.
// 
define generic same-operation?
    (op1 :: <operation>, op2 :: <operation>)
    => res :: <boolean>;

// same-operation?{<operation>,<operation>}
//
// Catch-all that just returns #f.
// 
define method same-operation?
    (op1 :: <operation>, op2 :: <operation>)
    => res :: <boolean>;
  #f;
end method same-operation?;

// same-operation?{<primitive>,<primitive>}
//
// Two primitives are the same if they have the same name.
// 
define method same-operation?
    (op1 :: <primitive>, op2 :: <primitive>)
    => res :: <boolean>;
  op1.primitive-name == op2.primitive-name;
end method same-operation?;

// same-operation?{<slot-ref>,<slot-ref>}
//
// Two slot-refs are the same if they are referencing the same slot.
// 
define method same-operation?
    (op1 :: <slot-ref>, slot2 :: <slot-ref>)
    => res :: <boolean>;
  op1.slot-info == slot2.slot-info;
end method same-operation?;

// same-operation?{<truly-the>,<truly-the>}
//
// Two truly-the operations are the same if they are guaranteeing the same
// result type.
// 
define method same-operation?
    (op1 :: <truly-the>, op2 :: <truly-the>)
    => res :: <boolean>;
  op1.guaranteed-type == op2.guaranteed-type;
end method same-operation?;

// same-operation?{<instance?>,<instance?>}
//
// Two instance? operations are the same if they are checking for the same
// type.
// 
define method same-operation?
    (op1 :: <instance?>, op2 :: <instance?>)
    => res :: <boolean>;
  op1.type == op2.type;
end method same-operation?;



// same-leaf? -- internal.
//
// Returns #t iff the two leaves are the same.
//
define generic same-leaf?
    (leaf1 :: <leaf>, leaf2 :: <leaf>)
    => res :: <boolean>;

// same-leaf?{<leaf>}
//
// Currently, we assume that two leaves are the same only if they are ==.
// In theory there could be leaves that are the same but not ==, hence this
// function.  But all of the interesting leaves are == when the same.
// 
define method same-leaf?
    (leaf1 :: <leaf>, leaf2 :: <leaf>)
    => res :: <boolean>;
  leaf1 == leaf2;
end method same-leaf?;

// record-operation -- internal.
//
// Add the operation to the table.
// 
define method record-operation
    (table :: <operation-table>, op :: <operation>) => ();
  add!(table.ot-operations, op);
end method record-operation;


// cse-region -- internal.
//
// Remove the common sub-expressions in the given region.  All the operations
// in the table dominate all the code in this region.  Additionally, side
// effect the table so that it includes all the operations that dominate
// whatever follows this region.
// 
define generic cse-region
    (component :: <component>, region :: <region>, table :: <operation-table>)
    => ();

// cse-region{<simple-region>}
//
// Look for assignments that are using an operation we have already done and
// change them to just use the results of the old operation.
//
define method cse-region
    (component :: <component>, region :: <simple-region>,
     table :: <operation-table>)
    => ();
  for (assign = region.first-assign then assign.next-op,
       while: assign)
    let op = assign.depends-on.source-exp;
    if (expression-cseable?(op))
      let old-op = find-operation(table, op);
      if (old-op)
	let old-assign = old-op.dependents.dependent;
	let builder = make-builder(component);
	let want = values-wanted(assign);
	let vars
	  = block (return)
	      for (var = old-assign.defines then var.definer-next,
		   old-vars = #() then pair(var, old-vars),
		   count from 0 below want)
		unless (instance?(var, <ssa-variable>))
		  //
		  // We can't use the old variables, so make up a batch of new
		  // ones and use them.
		  let temps = max(want, values-wanted(old-assign));
		  for (count from 0 below temps,
		       vars = #()
			 then pair(make-local-var
				     (builder, #"temp", object-ctype()),
				   vars))
		  finally
		    build-assignment
		      (builder, old-assign.policy, old-assign.source-location,
		       vars, old-op);
		    insert-before
		      (component, old-assign, builder-result(builder));
		    replace-expression
		      (component, old-assign.depends-on,
		       make-operation
			 (builder, <primitive>, vars, name: #"values"));
		    return(vars);
		  end for;
		end unless;
	      finally
		reverse(old-vars);
	      end for;
	    end block;
	replace-expression
	  (component, assign.depends-on,
	   make-operation
	     (builder, <primitive>, vars, name: #"values"));
      else
	record-operation(table, op);
      end if;
    end if;
  end for;
end method cse-region;

// cse-region{<compound-region>}
//
// Just call cse-region on each of the sub-regions.  Everything that dominates
// us dominates all the sub-region, and each sub-region dominates everything
// following it.  So we can just pass the table on in.
// 
define method cse-region
    (component :: <component>, region :: <compound-region>,
     table :: <operation-table>)
    => ();
  for (subregion in region.regions)
    cse-region(component, subregion, table);
  end for;
end method cse-region;

// cse-region{<if-region>}
//
// Call cse-region on each of the two sub-regions.  We have to create a new
// tables for each sub-region because neither sub-region dominates the other
// or dominates anything that follows.
// 
define method cse-region
    (component :: <component>, region :: <if-region>,
     table :: <operation-table>)
    => ();
  cse-region(component, region.then-region,
	     make(<operation-table>, inherit-from: table));
  cse-region(component, region.else-region,
	     make(<operation-table>, inherit-from: table));
end method cse-region;

// cse-region{<loop-region>}
//
// Just call cse-region on the body.  We don't copy the table because we don't
// need to: there is nothing after us, so it doesn't matter what happens to the
// table.
// 
define method cse-region
    (component :: <component>, region :: <loop-region>,
     table :: <operation-table>)
    => ();
  cse-region(component, region.body, table);
end method cse-region;

// cse-region{<unwind-protect-region>}
//
// Just call cse-region on the body.  The body dominates whatever follows us,
// so pass in the same table we were passed.
// 
define method cse-region
    (component :: <component>, region :: <unwind-protect-region>,
     table :: <operation-table>)
    => ();
  cse-region(component, region.body, table);
end method cse-region;

// cse-region{<block-region>}
//
// Call cse-region on the body.  We have to make a new table because the body
// does not dominate whatever follows: exits might cause some part of the body
// to be skipped.
//
define method cse-region
    (component :: <component>, region :: <block-region>,
     table :: <operation-table>)
    => ();
  cse-region(component, region.body,
	     make(<operation-table>, inherit-from: table));
end method cse-region;

// cse-region{<exit>}
//
// Exits contain no operations, so there isn't much to do.
//
define method cse-region
    (component :: <component>, region :: <exit>, table :: <operation-table>)
    => ();
end method cse-region;


// expression-cseable? -- internal.
//
// Return #f iff expr is a candidate for CSE.  For it to be a candidate,
// it must be an operation, operation-cseable? must give it the go, and we
// must be able to relocate references to the operands.
//
define method expression-cseable?
    (expr :: <expression>) => res :: <boolean>;
  if (instance?(expr, <operation>) & operation-cseable?(expr))
    block (return)
      for (dep = expr.depends-on then dep.dependent-next,
	   while: dep)
	unless (expression-movable?(dep.source-exp))
	  return(#f);
	end unless;
      end for;
      #t;
    end block;
  else
    #f;
  end if;
end method expression-cseable?;


// operation-cseable? -- internal.
//
// Return #t iff the operation is a candidate for CSE.  This is only
// responsible for checking the operation specific details -- it is the caller
// responsibility to verify that the operands are all movable.
// 
define generic operation-cseable?
    (op :: <operation>) => res :: <boolean>;

// operation-cseable?{<operation>}
//
// By default, just say no.
// 
define method operation-cseable?
    (op :: <operation>) => res :: <boolean>;
  #f;
end method operation-cseable?;

// operation-cseable?{<primitive>}
//
// Primitives are explicitly annotated as cseable or not.
// 
define method operation-cseable?
    (expr :: <primitive>) => res :: <boolean>;
  expr.primitive-info.priminfo-cseable?;
end method operation-cseable?;

// operation-cseable?{<slot-ref>}
//
// Slot references are cseable if the slot is read-only.
// 
define method operation-cseable?
    (expr :: <slot-ref>) => res :: <boolean>;
  expr.slot-info.slot-read-only?;
end method operation-cseable?;

// operation-cseable?{<truly-the>}
//
// Truly-the operations are always available for CSE.
//
define method operation-cseable?
    (expr :: <truly-the>) => res :: <boolean>;
  #t;
end method operation-cseable?;

// operation-cseable?{<instance?>}
//
// Instance? operations are always available for CSE.
//
define method operation-cseable?
    (expr :: <instance?>) => res :: <boolean>;
  #t;
end method operation-cseable?;


// values-wanted -- internal.
//
// Return the number of values wanted by the given assignment.  If the
// assignment is defining a fixed number of variables, ask for that many.
// If the assignment is asking for a cluster, ask for however many the
// operation is producing.  Note: the operation must be producing a fixed
// number of values, or we shouldn't be considering the operation cseable.
// 
define method values-wanted (assign :: <fer-assignment>) => res :: <integer>;
  if (assign.defines
	& instance?(assign.defines.var-info, <values-cluster-info>))
    let type = assign.depends-on.source-exp.derived-type;
    assert(type.fixed-number-of-values?);
    type.min-values;
  else
    for (count from 0,
	 var = assign.defines then var.definer-next,
	 while: var)
    finally
      count;
    end for;
  end if;
end method values-wanted;

