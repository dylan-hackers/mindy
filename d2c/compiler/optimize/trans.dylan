module: cheese
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/optimize/trans.dylan,v 1.1 1995/05/12 12:31:06 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.


define method extract-args
    (call :: <known-call>, nfixed :: <fixed-integer>, want-next? :: <boolean>,
     rest? :: <boolean>, keys :: false-or(<list>));
  let sig = find-signature(call.depends-on.source-exp);
  unless (sig.specializers.size == nfixed)
    error("Transformer inconsistent w/ function signature.");
  end;
  unless (if (rest?) sig.rest-type else ~sig.rest-type end)
    error("Transformer inconsistent w/ function signature.");
  end;
  if (keys)
    error("Can't extract keyword arguments yet.");
  end;
  local
    method decode-fixed-and-on
	(remaining :: <fixed-integer>, results :: <list>,
	 args :: false-or(<dependency>))
	=> result :: <list>;
      if (zero?(remaining))
	decode-next-and-on(results, args);
      else
	decode-fixed-and-on(remaining - 1,
			    pair(args.source-exp, results),
			    args.dependent-next);
      end;
    end,
    method decode-next-and-on
	(results :: <list>, args :: false-or(<dependency>))
	=> result :: <list>;
      if (sig.next?)
	decode-rest-and-on(if (want-next?)
			     pair(args.source-exp, results);
			   else
			     results;
			   end,
			   args.dependent-next);
      elseif (want-next?)
	error("Transformer inconsistent w/ function signature.");
      else
	decode-rest-and-on(results, args);
      end;
    end,
    method decode-rest-and-on
	(results :: <list>, args :: false-or(<dependency>))
      if (rest?)
	pair(extract-rest-arg(args.source-exp), results);
      else
	results;
      end;
    end;
  apply(values,
	reverse!(decode-fixed-and-on(nfixed, #(),
				     call.depends-on.dependent-next)));
end;

define method find-signature (func :: union(<leaf>, <definition>))
    => res :: <signature>;
  error("Known call of some random leaf?");
end;

define method find-signature (func :: <function-literal>)
    => res :: <signature>;
  func.signature;
end;

define method find-signature (func :: <definition-constant-leaf>)
    => res :: <signature>;
  find-signature(func.const-defn);
end;

define method find-signature (defn :: <function-definition>)
    => res :: <signature>;
  defn.function-defn-signature;
end;


define method extract-rest-arg (expr :: <expression>) => res :: <list>;
  error("Can't extract a rest arg from ~=", expr);
end;

define method extract-rest-arg (expr :: <ssa-variable>) => res :: <list>;
  extract-rest-arg(expr.definer.depends-on.source-exp);
end;

define method extract-rest-arg (expr :: <primitive>) => res :: <list>;
  if (expr.name == #"vector")
    for (arg = expr.depends-on then arg.dependent-next,
	 results = #() then pair(arg.source-exp, results),
	 while: arg)
    finally
      reverse!(results);
    end;
  else
    error("Can't extract a rest arg from ~=", expr);
  end;
end;

define method extract-rest-arg (expr :: <literal-constant>) => res :: <list>;
  if (expr.value = #())
    #();
  else
    error("Can't extract a rest arg from ~=", expr);
  end;
end;    




define method apply-transformer
    (component :: <component>, call :: <known-call>)
    => (did-anything? :: <boolean>);
  let (function, args) = extract-args(call, 1, #f, #t, #f);
  if (empty?(args))
    compiler-warning("Apply must be given at least on argument in addition "
		       "to the function.");
    #f;
  else
    let builder = make-builder(component);
    let assign = call.dependents.dependent;
    let policy = assign.policy;
    let source = assign.source-location;
    let cluster
      = if (args.size > 1)
	  let cluster1 = make-values-cluster(builder, #"fixed-args",
					     wild-ctype());
	  build-assignment
	    (builder, policy, source, cluster1,
	     make-operation(builder, <primitive>,
			    copy-sequence(args, end: args.size - 1),
			    name: #"values"));
	  let cluster2 = make-values-cluster(builder, #"more-args",
					     wild-ctype());
	  build-assignment
	    (builder, policy, source, cluster2,
	     make-unknown-call
	       (builder, dylan-defn-leaf(builder, #"values-sequence"), #f,
		list(args.last)));
	  let cluster3 = make-values-cluster(builder, #"args", wild-ctype());
	  build-assignment
	    (builder, policy, source, cluster3,
	     make-operation(builder, <primitive>, list(cluster1, cluster2),
			    name: #"merge-clusters"));
	  cluster3;
	else
	  let cluster = make-values-cluster(builder, #"args", wild-ctype());
	  build-assignment
	    (builder, policy, source, cluster,
	     make-unknown-call
	       (builder, dylan-defn-leaf(builder, #"values-sequence"), #f,
		args));
	  cluster;
	end;
    let op = make-operation(builder, <mv-call>, list(function, cluster),
			    use-generic-entry: #f);
    insert-before(component, assign, builder-result(builder));
    replace-expression(component, assign.depends-on, op);
    #t;
  end;
end;

define-transformer(#"apply", apply-transformer);



define method list-transformer
    (component :: <component>, call :: <known-call>)
    => (did-anything? :: <boolean>);
  let args = extract-args(call, 0, #f, #t, #f);
  if (empty?(args))
    let builder = make-builder(component);
    replace-expression(component, call.dependents,
		       make-literal-constant(builder, as(<ct-value>, #())));
    #t;
  else
    #f;
  end;
end;

define-transformer(#"list", list-transformer);
