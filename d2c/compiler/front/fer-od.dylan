Module: fer-od
rcs-header: $Header: /scm/cvs/src/d2c/compiler/front/fer-od.dylan,v 1.5 2003/06/24 21:00:08 andreas Exp $
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

// Note that the only official starting point (root) for FER dumping is a
// <function-literal>.  Any other dumpers and loaders expect to be called
// inside of function dumping/loading.  In order to enforce this, we have a
// dump dispatch function for the control-related datastructures (corresponding
// to BUILD-xxx calls in the loader.)  This insures that if we run in to e.g. a
// <region> along the "wrong" path, we will get a "don't know how to dump"
// error.

define generic fer-dump-od
 (obj :: <object>, component :: <fer-component>, buf :: <dump-state>)
 => ();

// The component that we guess we're dumping, since we can't pass it to
// everyone who wants it.  This is shallow-bound to be the current dumping
// component by the lambda dumper, which is supposed to be the entry point into
// this code.
//
define variable *dump-component* = #f;


// Utilities:


// Convert a depends-on thread back into a list of leaf objects, as used by the
// builder.
//
define method depends-list (x :: <dependent-mixin>)
 => res :: type-union(<expression>, <list>);
  let dep-on = x.depends-on;
  if (~dep-on)
    #();
  else
    for (cur = dep-on then cur.dependent-next,
	 res = #() then pair(cur.source-exp, res),
	 while: cur)
    finally reverse!(res)
    end for;
  end if;
end method;


// Regions:

// For simple regions, call fer-dump-od on each assignment.  Note that there is
// only one OD object kind for both simple and compound, but we need different
// dump methods because the subobjects are threaded differently.  We squelch
// prologue operations, since they are reinserted by the builder.
//
define method fer-dump-od 
  (obj :: <simple-region>, component :: <fer-component>, buf :: <dump-state>)
 => ();
  ignore(component);
  let start = buf.current-pos;
  dump-definition-header(#"linear-region", buf, subobjects: #t);
  let fass = obj.first-assign;
  if (instance?(fass.depends-on.source-exp, <prologue>))
    fass := fass.next-op;
  end;
  for (assign = fass then assign.next-op, while: assign)
    fer-dump-od(assign, component, buf);
  end;
  dump-end-entry(start, buf);
end method;

// For compound regions, recurse on the subregions.
//
define method fer-dump-od 
    (obj :: <compound-region>, component :: <fer-component>,
     buf :: <dump-state>)
 => ();
  ignore(component);
  let start = buf.current-pos;
  dump-definition-header(#"linear-region", buf, subobjects: #t);
  for (sub in obj.regions)
    fer-dump-od(sub, component, buf);
  end;
  dump-end-entry(start, buf);
end method;

// At load time, just read the subobjects (for effect.)
//
add-od-loader(*compiler-dispatcher*, #"linear-region",
  method (state :: <load-state>)
    until (load-object-dispatch(state) == $end-object)
    end;
  end method
);


// For an IF, dump component, source, depends-on, then-region, else-region.
//
// ### ignoring join region (currently unused)
define method fer-dump-od 
    (obj :: <if-region>, component :: <fer-component>, buf :: <dump-state>)
 => ();
  let start = buf.current-pos;
  dump-definition-header(#"if-region", buf, subobjects: #t);
  dump-od(component, buf);
  dump-od(obj.source-location, buf);
  dump-od(obj.depends-on.source-exp, buf);
  fer-dump-od(obj.then-region, component, buf);
  fer-dump-od(obj.else-region, component, buf);
  dump-end-entry(start, buf);
end method;

add-od-loader(*compiler-dispatcher*, #"if-region",
  method (state :: <load-state>)
    let builder = load-object-dispatch(state);
    let src = load-object-dispatch(state);
    build-if-body(builder, $default-policy, src, load-object-dispatch(state));
    load-object-dispatch(state);
    build-else(builder, $default-policy, src);
    load-object-dispatch(state);
    assert-end-object(state);
    end-body(builder);
  end method
);


// Like dump-simple-object, except that we do fer-dump-od on the last thing (a
// region.)
//
define method dump-simple-region (name, buf, component, #rest stuff)
 => ();
  let start = buf.current-pos;
  dump-definition-header(name, buf, subobjects: #t);
  dump-od(component, buf);
  for (i from 0 below stuff.size - 1)
    dump-od(stuff[i], buf);
  finally
    fer-dump-od(stuff[stuff.size - 1], component, buf);
  end for;
  dump-end-entry(start, buf);
end method;


// block: component, source, "self", body.
//
define method fer-dump-od
    (obj :: <block-region>, component :: <fer-component>, buf :: <dump-state>)
 => ();
  if (maybe-dump-reference(obj, buf))
    dump-simple-region(#"block-region", buf, component, obj.source-location,
  		       obj, obj.body);
  end if;
end method;

// If we run into a block region somwhere else, we must have already dumped it.
//
define method dump-od (obj :: <block-region>, buf :: <dump-state>) => ();
  assert(~maybe-dump-reference(obj, buf));
end method;


// We use resolve-forward-ref to resolve references to the block before we load
// the code in the block.  This is necessary because exits need to point back
// to the enclosing block.
//
add-od-loader(*compiler-dispatcher*, #"block-region",
  method (state :: <load-state>)
    let builder = load-object-dispatch(state);
    let res = build-block-body(builder, $default-policy,
    			       load-object-dispatch(state));
    let self = load-object-dispatch(state);
    resolve-forward-ref(self, res);
    load-object-dispatch(state);
    assert(res == end-body(builder));
    assert-end-object(state);
    res;
  end method
);


// loop: component, source, body
// ### ignoring join region
define method fer-dump-od
    (obj :: <loop-region>, component :: <fer-component>, buf :: <dump-state>)
 => ();
  dump-simple-region(#"loop-region", buf, component, obj.source-location,
		     obj.body);
end method;

add-od-loader(*compiler-dispatcher*, #"loop-region",
  method (state :: <load-state>)
    let builder = load-object-dispatch(state);
    build-loop-body(builder, $default-policy, load-object-dispatch(state));
    load-object-dispatch(state);
    assert-end-object(state);
    end-body(builder);
  end method
);


// exit: component source, block-of
define method fer-dump-od 
    (obj :: <exit>, component :: <fer-component>, buf :: <dump-state>) => ();
  let bo = obj.block-of;
  dump-simple-object(#"exit-region", buf, component, obj.source-location,
		     if (bo == component) #f else bo end);
end method;

add-od-loader(*compiler-dispatcher*, #"exit-region",
  method (state :: <load-state>)
    let builder = load-object-dispatch(state);
    build-exit(builder, $default-policy, load-object-dispatch(state),
    	       load-object-dispatch(state));
    assert-end-object(state);
  end method
);


// unwind-protect-region: component, source, uwp-region-cleanup-function
define method fer-dump-od 
    (obj :: <unwind-protect-region>, component :: <fer-component>,
     buf :: <dump-state>)
 => ();
  dump-simple-region(#"unwind-protect-region", buf, component,
  		     obj.source-location, obj.uwp-region-cleanup-function,
		     obj.body);
end method;

add-od-loader(*compiler-dispatcher*, #"unwind-protect-region",
  method (state :: <load-state>)
    let builder = load-object-dispatch(state);
    build-unwind-protect-body(
      builder, $default-policy,
      load-object-dispatch(state),
      load-object-dispatch(state)
    );
    load-object-dispatch(state);
    assert-end-object(state);
  end method
);


// return: component, source, block-of, depends-on
//
define method fer-dump-od
    (obj :: <return>, component :: <fer-component>, buf :: <dump-state>) => ();
  dump-simple-object(#"return-region", buf, component, obj.source-location,
  		     obj.block-of, obj.depends-list);
end method;

add-od-loader(*compiler-dispatcher*, #"return-region",
  method (state :: <load-state>)
    let builder = load-object-dispatch(state);
    build-return(builder, $default-policy,
    	         load-object-dispatch(state),
		 load-object-dispatch(state),
		 load-object-dispatch(state));

    assert-end-object(state);
  end method
);


// Assignments:

define method fer-dump-od
    (obj :: <abstract-assignment>, component :: <fer-component>, 
     buf :: <dump-state>)
 => ();
  dump-simple-object(
    select (obj by instance?)
      <set-assignment> => #"set-assignment";
      <let-assignment> => #"let-assignment";
    end select,
    buf,
    component,
    obj.policy,
    obj.source-location,
    for (def = obj.defines then def.definer-next,
	 defs = #()
	   then pair(if (instance?(def, <initial-definition>))
		       def.definition-of;
		     else
		       def;
		     end if,
		     defs),
         until: def == #f)
      finally
	if (defs == #() | defs.tail ~== #())
	  reverse(defs);
	else
	  defs.head;
	end if;
    end for,
    obj.depends-list.head
  );
end method;


for (name in #(#"set-assignment", #"let-assignment"),
     buildfn in list(build-assignment, build-let))
  add-od-loader(*compiler-dispatcher*, name,
    method (state :: <load-state>)
      let builder = load-object-dispatch(state);
      let policy = load-object-dispatch(state);
      let source = load-object-dispatch(state);
      let target-vars = load-object-dispatch(state);
      let source-exp = load-object-dispatch(state);
      assert-end-object(state);
      buildfn(builder, policy, source, target-vars, source-exp);
    end method
  );
end for;



// Variables:

// ssa-variable: derived-type, var-info
// ### needs-type-check???
add-make-dumper(#"ssa-variable", *compiler-dispatcher*, <ssa-variable>,
  list(derived-type, derived-type:, #f,
       var-info, var-info:, #f)
);


// initial-variable: component, derived-type, var-info
//
define method dump-od (obj :: <initial-variable>, buf :: <dump-state>) => ();
  if (maybe-dump-reference(obj, buf))
    dump-simple-object(#"initial-variable", buf, obj.component-of,
  		       obj.derived-type, obj.var-info);
  end if;
end method;

add-od-loader(*compiler-dispatcher*, #"initial-variable",
  method (state :: <load-state>) => res :: <initial-variable>;
    let builder = load-object-dispatch(state);
    let dtype = load-object-dispatch(state);
    let info =  load-object-dispatch(state);
    assert-end-object(state);
    make-initial-var(builder, dtype, info);
  end
);

define constant $debug-named-info-slots =
  list(asserted-type, asserted-type:, #f,
       debug-name, debug-name:, #f);

add-make-dumper(#"values-cluster-info", *compiler-dispatcher*,
		<values-cluster-info>, $debug-named-info-slots);

add-make-dumper(#"local-var-info", *compiler-dispatcher*,
		<local-var-info>, $debug-named-info-slots);

add-make-dumper(#"lexical-var-info", *compiler-dispatcher*,
  <lexical-var-info>, 
  concatenate($debug-named-info-slots,
  	      list(source-location, source-location:, #f))
);


// Operations:

// We don't really want to make the component a slot in all operations, so here
// goes...

define method operation-builder-hack (x)
  ignore(x);
  *dump-component*;
end method;

define constant $operation-slots =
  list(derived-type, derived-type:, #f,
       depends-list, operands:, #f,
       operation-builder-hack, builder:, #f);


add-make-dumper(#"join-operation", *compiler-dispatcher*, <join-operation>,
		$operation-slots);

add-make-dumper(#"prologue-operation", *compiler-dispatcher*, <prologue>,
		concatenate($operation-slots,
			    list(function, function:, function-setter)));

add-make-dumper(#"primitive-operation", *compiler-dispatcher*, <primitive>,
		concatenate($operation-slots,
			    list(primitive-name, name:, #f,
				 primitive-info, info:, #f)));

define constant $abstract-call-slots =
  $operation-slots;


add-make-dumper(#"known-call-operation", *compiler-dispatcher*,
		<known-call>, $abstract-call-slots);

define constant $general-call-slots =
  concatenate($operation-slots,
  	       list(use-generic-entry?, use-generic-entry:, #f));


add-make-dumper(#"unknown-call-operation", *compiler-dispatcher*,
		<unknown-call>, $general-call-slots);

add-make-dumper(#"mv-call-operation", *compiler-dispatcher*,
		<mv-call>, $general-call-slots);

add-make-dumper(#"error-call-operation", *compiler-dispatcher*,
		<error-call>, $abstract-call-slots);

add-make-dumper(#"delayed-optimization-call-operation", *compiler-dispatcher*,
		<delayed-optimization-call>, $general-call-slots);

define constant $module-var-access-slots =
  concatenate($operation-slots, list(variable, var:, #f));

add-make-dumper(#"module-var-ref-operation", *compiler-dispatcher*,
		<module-var-ref>, $module-var-access-slots);

add-make-dumper(#"module-var-set-operation", *compiler-dispatcher*,
		<module-var-set>, $module-var-access-slots);


define constant $slot-access-slots =
  concatenate(
    $operation-slots,
    list(slot-info, slot-info:, #f)
  );

add-make-dumper(#"heap-slot-ref-operation", *compiler-dispatcher*,
		<heap-slot-ref>, $slot-access-slots);

add-make-dumper(#"data-word-ref-operation", *compiler-dispatcher*,
		<data-word-ref>, $slot-access-slots);

add-make-dumper(#"heap-slot-set-operation", *compiler-dispatcher*,
		<heap-slot-set>, $slot-access-slots);


add-make-dumper(#"truly-the-operation", *compiler-dispatcher*,
  <truly-the>,
  concatenate(
    $operation-slots,
    list(guaranteed-type, guaranteed-type:, #f)
  )
);

add-make-dumper(#"instance?-operation", *compiler-dispatcher*,
  <instance?>,
  concatenate(
    $operation-slots,
    list(type, type:, #f)
  )
);

define constant $nlx-operation-slots =
  concatenate($operation-slots,
  	      list(nlx-info, nlx-info:, #f));

add-make-dumper(#"catch-operation", *compiler-dispatcher*,
		<catch>, $nlx-operation-slots);

add-make-dumper(#"throw-operation", *compiler-dispatcher*,
		<throw>, $nlx-operation-slots);

add-make-dumper(#"make-catcher-operation", *compiler-dispatcher*,
		<make-catcher>, $nlx-operation-slots);

add-make-dumper(#"disable-catcher-operation", *compiler-dispatcher*,
		<disable-catcher>, $nlx-operation-slots);


define constant $expression-slots = list(derived-type, derived-type:, #f);

add-make-dumper(#"literal-constant", *compiler-dispatcher*,
		<literal-constant>,
		concatenate($expression-slots, list(value, value:, #f)));

add-make-dumper(#"definition-constant-leaf", *compiler-dispatcher*,
		<definition-constant-leaf>,
		concatenate($expression-slots,
			    list(const-defn, const-defn:, #f)));

add-make-dumper(#"uninitialized-value-leaf", *compiler-dispatcher*,
		<uninitialized-value>, #());

add-make-dumper(#"nlx-info", *compiler-dispatcher*, <nlx-info>,
  list(nlx-hidden-references?, hidden-references:, #f)
);


// Function literals:

define constant $function-literal-slots =
  concatenate(
    $expression-slots,
    list(visibility, visibility:, #f,
         signature, signature:, #f,
	 ct-function, ct-function:, #f,
	 main-entry, main-entry:, #f,
	 general-entry, #f, general-entry-setter)
  );


add-make-dumper(#"function-literal", *compiler-dispatcher*, <function-literal>,
		$function-literal-slots);

add-make-dumper(#"method-literal", *compiler-dispatcher*, <method-literal>,
  concatenate(
    $function-literal-slots,
    list(generic-entry, #f, generic-entry-setter)
  )
);

add-make-dumper(#"callback-literal", *compiler-dispatcher*, <callback-literal>,
  concatenate(
    $function-literal-slots,
    list(callback-entry, #f, callback-entry-setter)
  )
);

add-make-dumper(#"exit-function-literal", *compiler-dispatcher*,
  <exit-function>,
  concatenate(
    $expression-slots,
    list(nlx-info, nlx-info:, #f)
  )
);


// component: source-location, name.
// 
define method dump-od (obj :: <fer-component>, buf :: <dump-state>) => ();
  if (maybe-dump-reference(obj, buf))
    dump-simple-object(#"fer-component", buf, obj.source-location, obj.name);
  end if;
end method;

// A component loads as a builder, providing a convenient way to keep track of
// which builder/component to use for a given method.
//
add-od-loader(*compiler-dispatcher*, #"fer-component", 
  method (state :: <load-state>) => res :: <fer-builder>;
    let src = load-object-dispatch(state);
    let nam = load-object-dispatch(state);
    assert-end-object(state);
    make-builder(make(<fer-component>, name: nam, source-location: src));
  end method
);


// fer-function-region: component, lambda?, source, name, arg-vars, arg-types,
// result-type, hidden-references, "self", body.  We shallow-bind
// *dump-component* so that nested operation dumpers can get at it.
//
define method dump-od (obj :: <fer-function-region>, buf :: <dump-state>)
    => ();
  if (maybe-dump-reference(obj, buf))
    let odc = *dump-component*;
    block ()
      *dump-component* := obj.parent;
      let start-pos = buf.current-pos;
      
      dump-definition-header(#"fer-function-region", buf, subobjects: #t);
      dump-od(obj.parent, buf);
      dump-od(instance?(obj, <lambda>), buf);
      dump-od(obj.source-location, buf);
      dump-od(obj.name, buf);

      for (def = obj.prologue.dependents.dependent.defines
	     then def.definer-next,
	   res = #()
	     then pair(if (instance?(def, <initial-definition>))
			 def.definition-of
		       else
			 def
		       end,
		       res),
	   until: def == #f)

      finally 
	dump-od(reverse!(res), buf);
      end for;

      dump-od(obj.argument-types, buf);
      dump-od(obj.result-type, buf);
      dump-od(obj.hidden-references?, buf);
      dump-od(obj.calling-convention, buf);
      dump-od(obj, buf);

      fer-dump-od(obj.body, obj.parent, buf);
      dump-end-entry(start-pos, buf);

    cleanup
      *dump-component* := odc;
    end block;
  end if;
end method;


// As for blocks, we resolve our self-references before loading the body so
// that enclosed code can directly get at the actual lambda.
//
add-od-loader(*compiler-dispatcher*, #"fer-function-region", 
  method (state :: <load-state>) => res :: <fer-function-region>;
    let builder = load-object-dispatch(state);
    let lambda? = load-object-dispatch(state);
    let source = load-object-dispatch(state);
    let name = load-object-dispatch(state);
    let arg-vars = load-object-dispatch(state);
    let arg-types = load-object-dispatch(state);
    let result-type = load-object-dispatch(state);
    let hidden = load-object-dispatch(state);
    let cconv = load-object-dispatch(state);
    let self = load-object-dispatch(state);
    let res = build-function-body(builder, $default-policy, source, lambda?,
    				  name, arg-vars, result-type, hidden,
				  calling-convention: cconv);
    resolve-forward-ref(self, res);
    res.argument-types := arg-types;
    load-object-dispatch(state);
    assert(end-body(builder) == res);
    assert-end-object(state);
    res;
  end method
);
