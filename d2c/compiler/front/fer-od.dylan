Module: fer-od
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/front/fer-od.dylan,v 1.1 1995/10/30 13:11:40 ram Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


// Note that the only official starting point (root) for FER dumping is a
// <function-literal>.  Any other dumpers and loaders expect to be called
// inside of function dumping/loading.  In order to enforce this, we have a
// dump dispatch function for the control-related datastructures (corresponding
// to BUILD-xxx calls in the loader.)  This insures that if we run in to e.g. a
// <region> along the "wrong" path, we will get a "don't know how to dump"
// error.

define generic fer-dump-od (obj :: <object>, buf :: <dump-state>) => ();


// Utilities:


// Convert a depends-on thread back into the leaf-or-list format used by the
// builder.
//
define method listify-depends (x :: false-or(<dependency>)) 
 => res :: type-or(<expression>, <list>);
  if (~x)
    #();
  elseif (~x.dependent-next)
    x.source-exp;
  else
    for (cur = x then x.dependent-next,
	 res = #() then pair(cur, res),
	 while: cur)
    finally reverse!(res)
    end for;
  end if;
end method;


// We stach the builder we're adding to in the state-stack for the load-state.
//
define method builder-of (state :: <load-state>) => res :: <fer-builder>;
  state.state-stack.head;
end method;



// Regions:

// For simple regions, call fer-dump-od on each assignment.  Note that there is
// only one OD object kind for both simple and compound, but we need different
// dump methods because the subobjects are threaded differently.
//
define method fer-dump-od (obj :: <simple-region>, buf :: <dump-state>) => ();
  let start = buf.current-pos;
  dump-definition-header(#"linear-region", buf, subobjects: #t);
  for (assign = obj.first-assign then assign.next-op, while: assign)
    fer-dump-od(assign, buf);
  end;
  dump-end-entry(start, buf);
end method;

// For compound regions, recurse on the subregions.
//
define method fer-dump-od (obj :: <compound-region>, buf :: <dump-state>) => ();
  let start = buf.current-pos;
  dump-definition-header(#"linear-region", buf, subobjects: #t);
  for (sub in obj.regions)
    fer-dump-od(sub, buf);
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


// For an IF, dump source, depends-on, then-region, else-region.
//
// ### ignoring join region (currently unused)
define method fer-dump-od (obj :: <if-region>, buf :: <dump-state>) => ();
  dump-simple-object(#"if-region", buf, obj.source-location,
  		     obj.depends-on.source-exp,
		     obj.then-region, obj.else-region);
end method;

add-od-loader(*compiler-dispatcher*, #"if-region",
  method (state :: <load-state>)
    let src = load-object-dispatch(state);
    let builder = state.builder-of;
    build-if-body(builder, $default-policy, src, load-object-dispatch(state));
    load-object-dispatch(state);
    build-else(builder, $default-policy, src);
    load-object-dispatch(state);
    assert-end-object(state);
    end-body(builder);
  end method
);


// block: source, body, info.
//
define method fer-dump-od (obj :: <block-region>, buf :: <dump-state>) => ();
  dump-simple-object(#"block-region", buf, obj.source-location, obj.body,
  		     obj.info);
end method;

add-od-loader(*compiler-dispatcher*, #"block-region",
  method (state :: <load-state>)
    let builder = state.builder-of;
    build-block-body(builder, $default-policy, load-object-dispatch(state));
    load-object-dispatch(state);
    let res = end-body(builder);
    res.info := load-object-dispatch(state);
    assert-end-object(state);
  end method
);


// loop: source, body
// ### ignoring join region
define method fer-dump-od (obj :: <loop-region>, buf :: <dump-state>) => ();
  dump-simple-object(buf, #"loop-region", obj.source-location, obj.body);
end method;

add-od-loader(*compiler-dispatcher*, #"loop-region",
  method (state :: <load-state>)
    let builder = state.builder-of;
    build-loop-body(builder, $default-policy, load-object-dispatch(state));
    load-object-dispatch(state);
    assert-end-object(state);
    end-body(builder);
  end method
);


// exit: source, block-of
define method fer-dump-od (obj :: <exit>, buf :: <dump-state>) => ();
  dump-simple-object(buf, #"exit-region", obj.source-location, obj.block-of);
end method;

add-od-loader(*compiler-dispatcher*, #"exit-region",
  method (state :: <load-state>)
    build-exit(state.builder-of, $default-policy, load-object-dispatch(state),
    	       load-object-dispatch(state));
    assert-end-object(state);
  end method
);


// return: source, block-of, depends-on
//
define method fer-dump-od (obj :: <return>, buf :: <dump-state>) => ();
  dump-simple-object(buf, #"return-region", obj.source-location, obj.block-of,
  		     listify-depends(obj.depends-on));
end method;

add-od-loader(*compiler-dispatcher*, #"return-region",
  method (state :: <load-state>)
    build-exit(state.builder-of, $default-policy,
    	       load-object-dispatch(state),
    	       load-object-dispatch(state),
	       load-object-dispatch(state));

    assert-end-object(state);
  end method
);


// Assignments:

define method fer-dump-od (obj :: <abstract-assignment>, buf :: <dump-state>)
 => ();
  dump-simple-object(
    select (obj by instance?)
      <set-assignment> => #"set-assignment";
      <let-assignment> => #"let-assignment";
    end select,
    buf,
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
    listify-depends(obj.depends-on)
  );
end method;


for (name in #(#"set-assignment", #"let-assignment"),
     buildfn in list(build-assignment, build-let))
  add-od-loader(*compiler-dispatcher*, name,
    method (state :: <load-state>)
      let policy = load-object-dispatch(state);
      let source = load-object-dispatch(state);
      let target-vars = load-object-dispatch(state);
      let source-exp = load-object-dispatch(state);
      assert-end-object(state);
      buildfn(state.builder-of, policy, source, target-vars, source-exp);
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


// initial-variable: derived-type, var-info
//
define method dump-od (obj :: <initial-variable>, buf :: <dump-state>) => ();
  dump-simple-object(#"initial-variable", buf, obj.derived-type, obj.var-info);
end method;

add-od-loader(*compiler-dispatcher*, #"initial-variable",
  method (state :: <load-state>) => res :: <initial-variable>;
    let dtype = load-object-dispatch(state);
    let info =  load-object-dispatch(state);
    assert-end-object(state);
    make-initial-var(state.builder-of, dtype, info);
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


/* 
control-flow.dylan:define class <simple-region> (<linear-region>)
control-flow.dylan:define class <compound-region> (<linear-region>)
control-flow.dylan:define class <if-region> (<join-region>, <dependent-mixin>)
control-flow.dylan:define class <block-region>
control-flow.dylan:define class <loop-region> (<body-region>)
control-flow.dylan:define class <exit> (<region>)
control-flow.dylan:define class <return> (<exit>, <dependent-mixin>)
data-flow.dylan:define class <ssa-variable> (<definition-site-variable>)
data-flow.dylan:define class <initial-variable> (<multi-definition-variable>)
front.dylan:define class <values-cluster-info> (<debug-named-info>)
front.dylan:define class <local-var-info> (<debug-named-info>)
front.dylan:define class <lexical-var-info> (<debug-named-info>

front.dylan:define class <let-assignment> (<fer-assignment>)
front.dylan:define class <set-assignment> (<fer-assignment>)

data-flow.dylan:define class <join-operation> (<operation>)

front.dylan:define class <primitive> (<operation>)
front.dylan:define class <unknown-call> (<general-call>)
front.dylan:define class <mv-call> (<general-call>)
front.dylan:define class <prologue> (<operation>)
front.dylan:define class <module-var-access> (<operation>)
front.dylan:define class <module-var-ref> (<module-var-access>)
front.dylan:define class <module-var-set> (<module-var-access>)
front.dylan:define class <self-tail-call> (<operation>)
front.dylan:define class <slot-ref> (<slot-access>)
front.dylan:define class <slot-set> (<slot-access>)
front.dylan:define class <truly-the> (<operation>)
front.dylan:define class <instance?> (<operation>)
front.dylan:define class <nlx-operation> (<operation>)
front.dylan:define class <catch> (<nlx-operation>)
front.dylan:define class <throw> (<nlx-operation>)
front.dylan:define class <make-catcher> (<nlx-operation>)
front.dylan:define class <disable-catcher> (<nlx-operation>)

front.dylan:define class <literal-constant> (<constant>)
front.dylan:define class <definition-constant-leaf> (<constant>)
front.dylan:define class <uninitialized-value> (<constant>)

front.dylan:define class <method-literal> (<function-literal>)

front.dylan:define class <fer-function-region>
front.dylan:define class <lambda> (<fer-function-region>)
front.dylan:define class <fer-component> (<component>)
front.dylan:define class <unwind-protect-region> (<body-region>)

front.dylan:define class <nlx-info> (<object>)

*/
