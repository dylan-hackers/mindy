module: cheese
rcs-header: $Header: /scm/cvs/src/d2c/compiler/optimize/fer-edit.dylan,v 1.1 1998/05/03 19:55:34 andreas Exp $
copyright: Copyright (c) 1996  Carnegie Mellon University
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

// Routines to edit FER.


// Deletion routines

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
  //
  // If we dropped the last dependent, delete this operation.
  unless (op.dependents)
    delete-dependent(component, op);
  end unless;
end;

define method dropped-dependent
    (component :: <component>, var :: <ssa-variable>) => ();
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
    let new = combine-regions(component, orig-region, exit);
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
		      combine-regions(component, before, exit));
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



// Query operations.

define generic doesnt-return? (region :: <region>) => res :: <boolean>;

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


// anything-after? -- internal.
//
// Return #t if there is anything after the given subregion and not
// after anything else.
// 
define generic anything-after? (region :: <region>, after :: <region>)
    => res :: <boolean>;

// anything-after?{<region>}
//
// Flame out because something is wrong.
//
define method anything-after? (region :: <region>, after :: <region>)
    => res :: <boolean>;
  error("%= doesn't have any subregions.", region);
end method anything-after?;

// anything-after?{<compound-region>}
//
// Return #t if the subregion is not the last subregion.  If it is the last
// subregion, then look above this region.
//
define method anything-after? (region :: <compound-region>, after :: <region>)
    => res :: <boolean>;
  for (remaining = region.regions then remaining.tail,
       until: remaining.head == after)
    if (remaining == #())
      error("%= isn't a subregion of %=", after, region);
    end if;
  finally
    if (remaining.tail == #())
      anything-after?(region.parent, region);
    else
      #t;
    end if;
  end for;
end method anything-after?;

// anything-after?{<if-region>}
//
// Return #f.  We don't look above this region because anything up there has to
// be after the other branch of the if.
//
define method anything-after? (region :: <if-region>, after :: <region>)
    => res :: <boolean>;
  unless (after == region.then-region | after == region.else-region)
    error("%= isn't a subregion of %=", after, region);
  end unless;
  #f;
end method anything-after?;

// anything-after?{<body-region>}
//
// Return #f because none of the body regions have anything after them that
// isn't also after something else:
//   function-regions have nothing after them.
//   stuff after block-regions must stay there because of exits.
//   loop regions have nothing after them.
// 
define method anything-after? (region :: <body-region>, after :: <region>)
    => res :: <boolean>;
  unless (after == region.body)
    error("%= isn't a subregion of %=", after, region);
  end unless;
  #f;
end method anything-after?;



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
define method combine-regions
    (component :: <component>, #rest stuff) => res :: <region>;
  let results = #();
  local
    method grovel (region)
      if (instance?(region, <compound-region>))
	for (subregion in region.regions)
	  grovel(subregion);
	end;
      elseif (instance?(region, <simple-region>)
		& instance?(results.head, <simple-region>))
	results.head := merge-simple-regions(component, results.head, region);
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
    (component :: <component>, first :: <simple-region>,
     second :: <simple-region>)
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
  let new = combine-regions(component, before, insert, after);
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
  let new = combine-regions(component, before, insert, after);
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
		    combine-regions(component, insert, region));
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
		    combine-regions(component, insert, region));
end;


// replace-subregion -- internal
//
// Replace region's child old with new.  This is NOT a deletion.  None of the
// code associated with old is deleted.  It is assumed that this routine will
// be used to edit the tree structure of regions while keeping the underlying
// assignments the same.  The new region's parent slot is updated.
//
define generic replace-subregion
    (component :: <component>, region :: <region>, old :: <region>,
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
    let combo = apply(combine-regions, component, regions);
    replace-subregion(component, parent, region, combo);
  end;
end;



// extract-stuff-after

// extract-stuff-after -- internal GF.
//
// Return the stuff after this subregion and not after anything else as a
// region.
// 
define generic extract-stuff-after
    (component :: <component>, region :: <region>, after :: <region>)
    => res :: <region>;

// extract-stuff-after{<region>}
//
// Flame out because something is wrong.
// 
define method extract-stuff-after
    (component :: <component>, region :: <region>, after :: <region>)
    => res :: <region>;
  error("%= doesn't have any subregions.", region);
end method extract-stuff-after;

// extract-stuff-after{<compound-region>}
//
// Split the compound region and append anything after it.
// 
define method extract-stuff-after
    (component :: <component>, region :: <compound-region>, after :: <region>)
    => res :: <region>;
  //
  // Find where to make the split.
  for (remaining = region.regions then remaining.tail,
       until: remaining.head == after)
    if (remaining == #())
      error("%= isn't a subregion of %=", after, region);
    end if;
  finally
    //
    // Lop off the tail.
    let tail = remaining.tail;
    remaining.tail := #();
    //
    // If the stuff after the split is just one region, use that region.
    // Otherwise, make a compound region for that stuff.
    let after
      = if (tail.size ~== 1)
	  let after = make(<compound-region>, regions: tail);
	  for (subregion in tail)
	    subregion.parent := after;
	  end for;
	  after;
	else
	  tail.head;
	end if;
    //
    // If spliting the compound region left just a single subregion before
    // the split, replace the compound region with that subregion.
    let region
      = if (region.regions.size == 1)
	  let subregion = region.regions.head;
	  replace-subregion(component, region.parent, region, subregion);
	  //
	  // We have to change region so that the references below refer to
	  // a valid region.
	  subregion;
	else
	  region;
	end if;
    //
    // Combine the stuff we clipped off with the stuff following this
    // region.
    combine-regions(component, after,
		    extract-stuff-after(component, region.parent, region));
  end for;
end method extract-stuff-after;

// extract-stuff-after{<if-region>}
//
// We don't actually extract anything because the stuff following the if
// region has to follow both subregions.  If one of the two subregions doesn't
// return, then the stuff above us will be moved down into the other when
// *this* if gets optimized.
// 
define method extract-stuff-after
    (component :: <component>, region :: <if-region>, after :: <region>)
    => res :: <region>;
  unless (after == region.then-region | after == region.else-region)
    error("%= isn't a subregion of %=", after, region);
  end unless;
  make(<empty-region>);
end method extract-stuff-after;

// extract-stuff-after{<body-region>}
//
// Just return an empty region because there is nothing extractble following
// any of the different kinds of body regions.
//
define method extract-stuff-after
    (component :: <component>, region :: <body-region>, after :: <region>)
    => res :: <region>;
  unless (after == region.body)
    error("%= isn't a subregion of %=", after, region);
  end unless;
  make(<empty-region>);
end method extract-stuff-after;
