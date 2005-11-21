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

// Function optimizations.
//
// This file contains the stuff to optimize function related things: function
// literals, function regions, and prologues.


define method optimize
    (component :: <component>, function :: <function-literal>)
    => ();

  // For functions that are only locally visible, we can throw them away
  // if we no longer need them.
  if (function.visibility == #"local")
    if (block (return)
	  for (dep = function.dependents then dep.source-next,
	       while: dep)
	    unless (home-function-region(dep.dependent) == function.main-entry)
	      return(#f);
	    end;
	  finally
	    #t;
	  end;
	end)
      // All the references to this function are inside this function, so
      // there is no way any outside party can reference it -- hence we can
      // nuke it.  First, we delete the body.
      local
	method delete-function-region (region)
	  if (region)
	    delete-stuff-in(component, region);
	  end;
	end;
      delete-function-region(function.main-entry);
      delete-function-region(function.general-entry);
      if (instance?(function, <method-literal>))
	delete-function-region(function.generic-entry);
      end;
      if (instance?(function, <callback-literal>))
	delete-function-region(function.callback-entry);
      end;
      // Deleting the body should have flushed the remaining references.
      assert(function.dependents == #f);
      // And then we delete the function literal itself.
      function.visibility := #"deleted";
      remove!(component.all-function-literals, function);
      delete-queueable(component, function);
    elseif (function.dependents.source-next == #f)
      // There is only one reference.  Let convert it if it is a known call.
      // We don't have to check for self calls, because they will have
      // been picked off up above.
      let ref = function.dependents.dependent;
      if (ref.depends-on == function.dependents & instance?(ref, <known-call>))
	let-convert(component, function.main-entry, ref);
      end;
    end;
  end;
end;

define method optimize
    (component :: <component>, function :: <fer-function-region>) => ();
  //
  // If there are hidden references, we can't improve the result type because
  // those hidden references have no way of observing said improvement.
  unless (function.hidden-references?)
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
	if (function.literal)
	  queue-dependents(component, function.literal);
	end if;
      end if;
    end for;
  end unless;
end method optimize;

define method optimize (component :: <component>, prologue :: <prologue>)
    => ();
  maybe-restrict-type
    (component, prologue,
     make-values-ctype(prologue.function.argument-types, #f).ctype-extent);
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


define method let-convert
    (component :: <component>, function :: <fer-function-region>,
     call :: <known-call>)
    => ();
  let call-assign :: <assignment> = call.dependents.dependent;
  let new-home = home-function-region(call-assign);

  assert(~(new-home == function));

  let builder = make-builder(component);
  let call-policy = call-assign.policy;
  let call-source = call-assign.source-location;

  // Extract the function body and replace it with an empty region.  This is
  // so that when we drop the last reference to the function-literal leaf,
  // that doesn't trigger a deletion walk of the entire body.
  let function-body = function.body;
  replace-subregion(component, function, function-body, make(<empty-region>));

  // Define a bunch of temporaries from the call args and insert it before
  // the call assignment.
  let arg-temps
    = begin
	let temps = make(<stretchy-vector>);
	for (dep = call.depends-on.dependent-next then dep.dependent-next,
	     arg-type in function.argument-types,
	     arg = function.prologue.dependents.dependent.defines
	       then arg & arg.definer-next,
	     index from 0)
	  unless (dep)
	    error("Wrong number of argument in let-convert?");
	  end;
	  let debug-name = if (arg)
			     arg.var-info.debug-name;
			   else
			     as(<symbol>, format-to-string("arg%d", index));
			   end;
	  let temp = make-local-var(builder, debug-name, arg-type);
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
  replace-expression(component, function.prologue.dependents,
		     make-operation(builder, <primitive>, arg-temps,
				    name: #"values"));

  // If there are any returns, change them into assignments of a cluster
  // and change the call to a reference to that cluster.
  if (function.exits)
    let results-temp = make-values-cluster(builder, #"results", wild-ctype());

    // Wrap the function body in a block so we can return to it.
    let body-block = build-block-body(builder, call-policy, call-source);
    build-region(builder, function-body);
    end-body(builder);

    // Insert that block before the original call site.
    insert-before(component, call-assign, builder-result(builder));

    // Replace each return with an assignment of the result cluster
    // followed by an exit to the body-block.
    while (function.exits)
      let return = function.exits;
      let source = return.source-location;
      let results = return.depends-on;
      if (results & instance?(results.source-exp, <abstract-variable>)
	    & instance?(results.source-exp.var-info, <values-cluster-info>))
	build-assignment(builder, call-policy, source,
			 results-temp, results.source-exp);
      else
	// Make a values operation stealing the results from the return.
	let op = make(<primitive>, derived-type: return.returned-type,
		      name: #"values", depends-on: results);
	for (dep = results then dep.dependent-next,
	     while: dep)
	  dep.dependent := op;
	end;
	return.depends-on := #f;
	reoptimize(component, op);
	// Assign the result temp with the values call.
	build-assignment(builder, call-policy, source, results-temp, op);
      end;
      build-exit(builder, call-policy, return.source-location, body-block);

      replace-subregion(component, return.parent, return,
			builder-result(builder));
      delete-stuff-in(component, return);
    end;

    // Replace the call with a reference to the result cluster.  We need to
    // do this after replacing the returns because this triggers the deletion
    // of the function while replacing returns will trigger a queueing of
    // the function.
    replace-expression(component, call-assign.depends-on, results-temp);
  else
    // Insert the function body before the call assignment.
    insert-before(component, call-assign, function-body);
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
    if (instance?(assign, <let-assignment>))
      reoptimize(component, assign);
    end;
    let expr = assign.depends-on.source-exp;
    if (instance?(expr, <throw>))
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
