module: cheese
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/optimize/trans.dylan,v 1.4 1995/06/04 01:06:30 wlott Exp $
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
  if (okay?)
    let builder = make-builder(component);
    let assign = call.dependents.dependent;
    let policy = assign.policy;
    let source = assign.source-location;
    let pair-leaf = dylan-defn-leaf(builder, #"pair");
    let current-value = make-literal-constant(builder, as(<ct-value>, #()));
    for (arg in reverse!(args))
      let temp = make-local-var(builder, #"temp", object-ctype());
      build-assignment
	(builder, policy, source, temp,
	 make-unknown-call(builder, pair-leaf, #f,
			   list(current-value, arg)));
      current-value := temp;
    end;
    insert-before(component, assign, builder-result(builder));
    replace-expression(component, call.dependents, current-value);
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
	 (builder,
	  make-literal-constant(builder, defn.class-defn-maker-function),
	  #f, init-keywords));
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



// reduce & reduce1 transformers.

define method reduce-transformer
    (component :: <component>, call :: <known-call>)
    => (did-anything? :: <boolean>);
  let (okay?, proc, init-val, collection) = extract-args(call, 3, #f, #f, #f);
  if (okay?)
    let elements = extract-rest-arg(collection);
    if (elements)
      reduce-transformer-aux(component, call, proc, init-val, elements);
    end;
  end;
end;

define-transformer(#"reduce", #(#"<function>", #"<object>", #"<collection>"),
		   reduce-transformer);

define method reduce1-transformer
    (component :: <component>, call :: <known-call>)
    => (did-anything? :: <boolean>);
  let (okay?, proc, collection) = extract-args(call, 3, #f, #f, #f);
  if (okay?)
    let elements = extract-rest-arg(collection);
    if (elements & ~empty?(elements))
      reduce-transformer-aux(component, call, proc,
			     elements.head, elements.tail);
      
    end;
  end;
end;

define-transformer(#"reduce1", #(#"<function>", #"<object>", #"<sequence>"),
		   reduce1-transformer);

define method reduce-transformer-aux
    (component :: <component>, call :: <known-call>, proc :: <leaf>,
     init-value :: <leaf>, elements :: <list>)
    => res :: <true>;
  let builder = make-builder(component);
  let assign = call.dependents.dependent;
  let policy = assign.policy;
  let source = assign.source-location;
  let current-value = init-value;
  for (element in elements)
    let temp = make-local-var(builder, #"temp", object-ctype());
    build-assignment
      (builder, policy, source, temp,
       make-unknown-call(builder, proc, #f, list(current-value, element)));
    current-value := temp;
  end;
  insert-before(component, assign, builder-result(builder));
  replace-expression(component, call.dependents, current-value);
  #t;
end;


// mapping transforms.

define method do-transformer
    (component :: <component>, call :: <known-call>)
    => (did-anything? :: <boolean>);
  let (okay?, proc, collection, more-collections)
    = extract-args(call, 2, #f, #t, #f);
  if (okay?
	& every?(method (leaf)
		   csubtype?(leaf.derived-type, specifier-type(#"<sequence>"));
		 end,
		 more-collections))
    let collections = pair(collection, more-collections);
    let builder = make-builder(component);
    let assign = call.dependents.dependent;
    let policy = assign.policy;
    let source = assign.source-location;

    let iteration-vars-vectors
      = map(curry(iteration-setup, builder, policy, source), collections);
    let block-region = build-block-body(builder, policy, source);
    build-loop-body(builder, policy, source);
    for (iteration-vars in iteration-vars-vectors)
      build-if-body(builder, policy, source,
		    iteration-done(builder, policy, source, iteration-vars));
      build-exit(builder, policy, source, block-region);
      end-body(builder); // if
    end;
    build-assignment(builder, policy, source, #(),
		     make-unknown-call(builder, proc, #f,
				       map(curry(iteration-current-element,
						 builder, policy, source),
					   iteration-vars-vectors)));
    do(curry(iteration-advance, builder, policy, source),
       iteration-vars-vectors);
    end-body(builder); // loop
    end-body(builder); // block
    insert-before(component, assign, builder-result(builder));
    replace-expression(component, call.dependents,
		       make-literal-constant(builder, as(<ct-value>, #f)));
    #t;
  end;
end;


// Utilities for building iterators.

define method iteration-setup
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     coll :: <leaf>)
    => res :: <simple-object-vector>;
  let state = make-lexical-var(builder, #"state", source, object-ctype());
  let limit = make-lexical-var(builder, #"limit", source, object-ctype());
  let next = make-lexical-var(builder, #"next", source, function-ctype());
  let done = make-lexical-var(builder, #"done?", source, function-ctype());
  let curkey = make-lexical-var(builder, #"curkey", source, function-ctype());
  let curel = make-lexical-var(builder, #"curel", source, function-ctype());
  let curel-setter
    = make-lexical-var(builder, #"curel-setter", source, function-ctype());
  let copy-state
    = make-lexical-var(builder, #"copy-state", source, function-ctype());
  build-let(builder, policy, source,
	    list(state, limit, next, done, curkey, curel,
		 curel-setter, copy-state),
	    make-unknown-call(builder,
			      dylan-defn-leaf(builder,
					      #"forward-iteration-protocol"),
			      #f,
			      list(coll)));
  vector(state, limit, next, done, curkey, curel, curel-setter, copy-state);
end;

define method iteration-advance
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     coll :: <leaf>, iteration-vars :: <simple-object-vector>)
  build-assignment(builder, policy, source, iteration-vars[0],
		   make-unknown-call(builder, iteration-vars[2], #f,
				     list(coll, iteration-vars[0])));
end;

define method iteration-done
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     coll :: <leaf>, iteration-vars :: <simple-object-vector>)
    => var :: <leaf>;
  let var = make-local-var(builder, #"done?", object-ctype());
  build-assignment(builder, policy, source, var,
		   make-unknown-call(builder, iteration-vars[3], #f,
				     list(coll,
					  iteration-vars[0],
					  iteration-vars[1])));
  var;
end;

define method iteration-current-key
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     coll :: <leaf>, iteration-vars :: <simple-object-vector>)
    => var :: <leaf>;
  let var = make-local-var(builder, #"current-key", object-ctype());
  build-assignment(builder, policy, source, var,
		   make-unknown-call(builder, iteration-vars[4], #f,
				     list(coll, iteration-vars[0])));
  var;
end;

define method iteration-current-element
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     coll :: <leaf>, iteration-vars :: <simple-object-vector>)
    => var :: <leaf>;
  let var = make-local-var(builder, #"current-element", object-ctype());
  build-assignment(builder, policy, source, var,
		   make-unknown-call(builder, iteration-vars[5], #f,
				     list(coll, iteration-vars[0])));
  var;
end;

define method iteration-current-element-setter
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     new-value :: <leaf>, coll :: <leaf>,
     iteration-vars :: <simple-object-vector>)
    => ();
  build-assignment(builder, policy, source, #(),
		   make-unknown-call(builder, iteration-vars[6], #f,
				     list(new-value, coll,
					  iteration-vars[0])));
end;

define method iteration-copy-state
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     coll :: <leaf>, iteration-vars :: <simple-object-vector>)
    => res :: <simple-object-vector>;
  let var = make-lexical-var(builder, #"state", object-ctype());
  build-assignment(builder, policy, source, var,
		   make-unknown-call(builder, iteration-vars[7], #f,
				     list(coll, iteration-vars[0])));
  let res = shallow-copy(iteration-vars);
  res[0] := var;
  res;
end;

