module: fer-transform
rcs-header: $Header: /scm/cvs/src/d2c/compiler/fer-transform/fer-edit.dylan,v 1.6 2003/06/24 21:00:07 andreas Exp $
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


// Routines to edit FER.

// insert-after -- external
//
// Insert the region immediate after the assignment.  All appropriate parent
// and region links are updated.
//
define generic insert-after
    (component :: <component>, assign :: <abstract-assignment>,
     insert :: <region>, reoptimize :: <function>) => ();

define method insert-after
    (component :: <component>, assign :: <abstract-assignment>,
     insert :: <region>, reoptimize :: <function>) => ();
  let region = assign.region;
  let parent = region.parent;
  let (before, after) = split-after(assign);
  let new = combine-regions(component, reoptimize, before, insert, after);
  new.parent := parent;
  replace-subregion(component, parent, region, new, reoptimize);
end;
    
define method insert-after
    (component :: <component>, after :: <abstract-assignment>,
     insert :: <simple-region>, reoptimize :: <function>) => ();
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
     insert :: <empty-region>, reoptimize :: <function>)
    => ();
end;

// split-after - external
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

// combine-regions -- external.
//
// Takes two subtrees of FER and combines them into one subtree.  The result
// is interally consistent (i.e. the two input regions will have their
// parent link updated if necessary).  This routine does NOT check the
// first subregion to see if it exits or not (i.e. whether the second subregion
// is actually reachable.
// 
define generic combine-regions
    (component :: <component>, reoptimize :: <function>, #rest stuff) => res :: <region>;

define method combine-regions
    (component :: <component>, reoptimize :: <function>, #rest stuff) => res :: <region>;
  let results = #();
  local
    method grovel (region)
      if (instance?(region, <compound-region>))
	for (subregion in region.regions)
	  grovel(subregion);
	end;
      elseif (instance?(region, <simple-region>)
		& instance?(results.head, <simple-region>))
	results.head := merge-simple-regions(component, results.head, region, reoptimize);
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

// replace-subregion -- external.
//
// Replace region's child old with new.  This is NOT a deletion.  None of the
// code associated with old is deleted.  It is assumed that this routine will
// be used to edit the tree structure of regions while keeping the underlying
// assignments the same.  The new region's parent slot is updated.
//
define generic replace-subregion
    (component :: <component>, region :: <region>, old :: <region>,
     new :: <region>, reoptimize :: <function>)
    => ();

define method replace-subregion
    (component :: <component>, region :: <body-region>, old :: <region>,
     new :: <region>, reoptimize :: <function>)
    => ();
  unless (region.body == old)
    error("Replacing unknown region");
  end;
  region.body := new;
  new.parent := region;
end;

define method replace-subregion
    (component :: <component>, region :: <if-region>, old :: <region>,
     new :: <region>, reoptimize :: <function>)
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
     new :: <region>, reoptimize :: <function>)
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
    let combo = apply(combine-regions, component, reoptimize, regions);
    replace-subregion(component, parent, region, combo, reoptimize);
  end;
end;

// merge-simple-regions -- internal.
//
define method merge-simple-regions
    (component :: <component>, first :: <simple-region>,
     second :: <simple-region>, reoptimize :: <function>)
    => res :: <simple-region>;
  let last-of-first = first.last-assign;
  let first-of-second = second.first-assign;

  last-of-first.next-op := first-of-second;
  first-of-second.prev-op := last-of-first;

  first.last-assign := second.last-assign;

  for (assign = first-of-second then assign.next-op,
       while: assign)
    assign.region := first;

    // If the operation is a values-sequence of a canonicalize-results
    // in the same region, queue it up for reoptimization because we might
    // be able to squeeze them out.
    let op = assign.depends-on.source-exp;
    if (instance?(op, <primitive>) & op.primitive-name == #"values-sequence")
      let vec = op.depends-on.source-exp;
      if (instance?(vec, <ssa-variable>))
	let vec-definer = vec.definer;
	if (vec-definer.region == first)
	  let vec-defn = vec-definer.depends-on.source-exp;
	  if (instance?(vec-defn, <primitive>)
		& vec-defn.primitive-name == #"canonicalize-results")
	    let nfixed = vec-defn.depends-on.dependent-next.source-exp;
	    if (instance?(nfixed, <literal-constant>)
		  & nfixed.value.literal-value = 0)
	      reoptimize(component, op);
	    end if;
	  end if;
	end if;
      end if;
    end if;
  end for;

  first;
end;


// Deletion routines

// delete-dependent -- external.
//
define method delete-dependent
    (component :: <component>, dependent :: <dependent-mixin>, reoptimize :: <function>) => ();
  //
  // Remove our dependency from whatever we depend on.
  for (dep = dependent.depends-on then dep.dependent-next,
       while: dep)
    remove-dependency-from-source(component, dep, reoptimize);
  end;
  //
  delete-queueable(component, dependent);
end;

define method delete-dependent
    (component :: <component>, op :: <catch>, reoptimize :: <function>, #next next-method) => ();
  op.nlx-info.nlx-catch := #f;
  next-method();
end;

define method delete-dependent
    (component :: <component>, op :: <make-catcher>, reoptimize :: <function>, #next next-method) => ();
  op.nlx-info.nlx-make-catcher := #f;
  next-method();
end;

define method delete-dependent
    (component :: <component>, op :: <disable-catcher>, reoptimize :: <function>, #next next-method)
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
    (component :: <component>, op :: <throw>,
     reoptimize :: <function>, #next next-method) => ();
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


// remove-dependency-from-source -- external.
//
define method remove-dependency-from-source
    (component :: <component>, dependency :: <dependency>, reoptimize :: <function>) => ();
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
  dropped-dependent(component, source, reoptimize);
end;

// dropped-dependent -- external.
//
define generic dropped-dependent
    (component :: <component>, expr :: <expression>,
     reoptimize :: <function>) => ();

define method dropped-dependent
    (component :: <component>, expr :: <expression>,
     reoptimize :: <function>) => ();
end;

define method dropped-dependent
    (component :: <component>, op :: <operation>,
     reoptimize :: <function>) => ();
  //
  // If we dropped the last dependent, delete this operation.
  unless (op.dependents)
    delete-dependent(component, op, reoptimize);
  end unless;
end;

define method dropped-dependent
    (component :: <component>, var :: <ssa-variable>,
     reoptimize :: <function>) => ();
  // If the variable doesn't need a type check and is still being defined,
  // then we might be able to flush the assignment.  We can flush the
  // assignment if the variable is unused (i.e. dependents == #f) or if it
  // is can now be copy-propagated away.
  if (~var.needs-type-check?
	& var.definer ~== #f
	& (var.dependents == #f
	     | (var.dependents.source-next == #f
		  & expression-movable?(var.definer.depends-on.source-exp))))
    reoptimize(component, var.definer);
  end if;
end method dropped-dependent;


define method dropped-dependent
    (component :: <component>, var :: <initial-variable>,
     reoptimize :: <function>) => ();
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
    (component :: <component>, function :: <function-literal>,
     reoptimize :: <function>) => ();
  if (function.visibility == #"local")
    // If we dropped a reference to the function literal, we might be
    // able to nuke it.
    reoptimize(component, function);
  end;
end;

define method dropped-dependent
    (component :: <component>, exit :: <exit-function>, reoptimize :: <function>) => ();
  // If we dropped the last reference, clear it out.
  unless (exit.dependents)
    let nlx-info = exit.nlx-info;
    nlx-info.nlx-exit-function := #f;

    delete-dependent(component, exit, reoptimize);

    if (nlx-info.nlx-catch & ~nlx-info.nlx-hidden-references?
	  & nlx-info.nlx-exit-function == #f & nlx-info.nlx-throws == #f)
      reoptimize(component, nlx-info.nlx-catch);
    end;
  end;
end;


// delete-queueable -- external.
//
define method delete-queueable
    (component :: <component>, queueable :: <queueable-mixin>) => ();
  //
  // If we are queued for reoptimization, belay that.
  unless (queueable.queue-next == #"absent")
    for (q = component.reoptimize-queue then q.queue-next,
	 prev = #f then q,
	 until: q == queueable | ~q)
    finally
      if(q)
        if (prev)
          prev.queue-next := q.queue-next;
        else
          component.reoptimize-queue := q.queue-next;
        end;
      else
        compiler-warning("Queueable %= thinks it's queued, but isn't?",
                         queueable);
      end if;
    end;
  end;
  queueable.queue-next := #"deleted";
end;


// function-movable? -- external.
//
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


// expression-movable? -- external.
//
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
