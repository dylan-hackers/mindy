module: cheese
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/optimize/trans.dylan,v 1.3 1995/06/01 14:41:29 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.


define method extract-args
    (call :: <known-call>, nfixed :: <fixed-integer>, want-next? :: <boolean>,
     rest? :: <boolean>, keys :: false-or(<list>))
    => (okay? :: <boolean>, #rest arg :: union(<leaf>, <list>));
  block (return)
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
	  pair(extract-rest-arg(args.source-exp)
		 | return(#f),
	       results);
	else
	  results;
	end;
      end;
    apply(values,
	  #t,
	  reverse!(decode-fixed-and-on(nfixed, #(),
				       call.depends-on.dependent-next)));
  end;
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


define method extract-rest-arg (expr :: <expression>) => res :: <false>;
  #f;
end;

define method extract-rest-arg (expr :: <ssa-variable>)
    => res :: false-or(<list>);
  extract-rest-arg(expr.definer.depends-on.source-exp);
end;

define method extract-rest-arg (expr :: <primitive>)
    => res :: false-or(<list>);
  if (expr.name == #"vector")
    for (arg = expr.depends-on then arg.dependent-next,
	 results = #() then pair(arg.source-exp, results),
	 while: arg)
    finally
      reverse!(results);
    end;
  else
    #f;
  end;
end;

define method extract-rest-arg (expr :: <literal-constant>)
    => res :: false-or(<list>);
  if (expr.value = #() | expr.value = #[])
    #();
  else
    #f;
  end;
end;    


define method apply-transformer
    (component :: <component>, call :: <known-call>)
    => (did-anything? :: <boolean>);
  let (okay?, function, args) = extract-args(call, 1, #f, #t, #f);
  if (~okay?)
    #f;
  elseif (empty?(args))
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

define-transformer(#"apply", #f, apply-transformer);



define method list-transformer
    (component :: <component>, call :: <known-call>)
    => (did-anything? :: <boolean>);
  let (okay?, args) = extract-args(call, 0, #f, #t, #f);
  if (okay? & empty?(args))
    let builder = make-builder(component);
    replace-expression(component, call.dependents,
		       make-literal-constant(builder, as(<ct-value>, #())));
    #t;
  else
    #f;
  end;
end;

define-transformer(#"list", #f, list-transformer);



define method make-transformer
    (component :: <component>, call :: <known-call>)
    => (did-anything? :: <boolean>);
  block (return)
    local method give-up () return(#f) end;
    let (okay?, cclass-leaf, init-keywords)
      = extract-args(call, 1, #f, #t, #f);
    unless (okay? & instance?(cclass-leaf, <literal-constant>))
      give-up();
    end;
    let cclass = cclass-leaf.value;
    if (cclass.abstract?)
      compiler-warning("In %s:\n  trying to make an instance of an abstract "
			 "class %s",
		       call.home-function-region.name,
		       cclass.cclass-name);
      give-up();
    end;
    maybe-restrict-type(component, call, cclass);
    unless (instance?(cclass, <defined-cclass>))
      give-up();
    end;
    let defn = cclass.class-defn;
    unless (defn.class-defn-maker-function)
      give-up();
    end;
    // ### Need to default the keywords.
    // ### Need to validate the keywords.
    let builder = make-builder(component);
    let assign = call.dependents.dependent;
    let policy = assign.policy;
    let source = assign.source-location;
    let instance-var = make-local-var(builder, #"instance", cclass);
    build-assignment
      (builder, policy, source, instance-var,
       make-unknown-call
	 (builder, defn.class-defn-maker-function, #f, init-keywords));
    build-assignment
      (builder, policy, source, #(),
       make-unknown-call
	 (builder, dylan-defn-leaf(builder, #"initialize"), #f,
	  pair(instance-var, init-keywords)));
    insert-before(component, assign, builder-result(builder));
    replace-expression(component, assign.depends-on, instance-var);
    #t;
  end;
end;

define-transformer(#"make", #(#"<class>"), make-transformer);
