module: cheese
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/optimize/trans.dylan,v 1.10 1995/07/19 19:41:50 wlott Exp $
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

define method find-signature (func :: <leaf>)
    => res :: <signature>;
  error("Known call of some random leaf?");
end;

define method find-signature (func :: <function-literal>)
    => res :: <signature>;
  func.signature;
end;

define method find-signature (func :: <definition-constant-leaf>)
    => res :: <signature>;
  func.const-defn.function-defn-signature;
end;

define method find-signature (func :: <literal-constant>)
    => res :: <signature>;
  func.value.ct-function-signature;
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


// == stuff.

define method ==-transformer
    (component :: <component>, call :: <known-call>)
    => did-anything? :: <boolean>;
  let (okay?, x, y) = extract-args(call, 2, #f, #f, #f);
  okay? & trivial-==-optimization(component, call, x, y);
end;

define-transformer(#"==", #f, ==-transformer);

define method object-==-transformer
    (component :: <component>, call :: <known-call>)
    => did-anything? :: <boolean>;
  let (okay?, x, y) = extract-args(call, 2, #f, #f, #f);
  if (okay?)
    if (trivial-==-optimization(component, call, x, y))
      #t;
    else
      let x-functional = is-it-functional?(x.derived-type);
      let y-functional = is-it-functional?(y.derived-type);
      if (x-functional == #f | y-functional == #f)
	let builder = make-builder(component);
	replace-expression
	  (component, call.dependents,
	   make-operation(builder, <primitive>, list(x, y),
			  name: #"=="));
	#t;
      elseif (x-functional == #t | y-functional == #t)
	let builder = make-builder(component);
	let assign = call.dependents.dependent;
	let leaf
	  = ref-dylan-defn(builder, assign.policy, assign.source-location,
			    #"functional-==");
	insert-before(component, assign, builder-result(builder));
	replace-expression
	  (component, call.dependents,
	   make-unknown-call(builder, leaf, #f, list(x, y)));
	#t;
      else
	#f;
      end;
    end;
  end;
end;

define generic is-it-functional? (type :: <ctype>)
    => res :: one-of(#t, #f, #"maybe");

define method is-it-functional? (type :: <unknown-ctype>)
    => res :: one-of(#t, #f, #"maybe");
  #"maybe";
end;

define method is-it-functional? (class :: <cclass>)
    => res :: one-of(#t, #f, #"maybe");
  if (class.functional?)
    #t;
  elseif (class.not-functional?)
    #f;
  else
    #"maybe";
  end;
end;

define method is-it-functional? (type :: <limited-ctype>)
    => res :: one-of(#t, #f, #"maybe");
  is-it-functional?(type.base-class);
end;

define method is-it-functional? (type :: <union-ctype>)
    => res :: one-of(#t, #f, #"maybe");
  let members = type.members;
  if (members == #())
    #f;
  else
    let first-functional? = is-it-functional?(members.head);
    block (return)
      for (next-type in members.tail)
	unless (first-functional? == is-it-functional?(next-type))
	  return(#"maybe");
	end;
      end;
      first-functional?;
    end;
  end;
end;

define-transformer(#"==", #(#"<object>", #"<object>"), object-==-transformer);

define method trivial-==-optimization
    (component :: <component>, operation :: <operation>,
     x :: <leaf>, y :: <leaf>)
    => did-anything? :: <boolean>;
  local method origin (thing :: <expression>)
	  if (instance?(thing, <ssa-variable>))
	    let assign = thing.definer;
	    if (thing == assign.defines)
	      origin(assign.depends-on.source-exp);
	    else
	      thing;
	    end;
	  else
	    thing;
	  end;
	end;
  if (always-the-same?(x.origin, y.origin))
    // Same variable or same literal constant.
    replace-expression(component, operation.dependents,
		       make-literal-constant(make-builder(component),
					     as(<ct-value>, #t)));
    #t;
  else
    let x-type = x.derived-type;
    let y-type = y.derived-type;
    if (~ctypes-intersect?(x-type, y-type))
      replace-expression(component, operation.dependents,
			 make-literal-constant(make-builder(component),
					       as(<ct-value>, #f)));
      #t;
    elseif (instance?(x-type, <singleton-ctype>) & x-type == y-type)
      replace-expression(component, operation.dependents,
			 make-literal-constant(make-builder(component),
					       as(<ct-value>, #t)));
      #t;
    else
      #f;
    end;
  end;
end;

define method always-the-same?
    (x :: <expression>, y :: <expression>) => res :: <boolean>;
  x == y;
end;

define method always-the-same?
    (x :: <definition-constant-leaf>, y :: <definition-constant-leaf>)
    => res :: <boolean>;
  x.const-defn.ct-value == y.const-defn.ct-value;
end;

define method always-the-same?
    (x :: <module-var-ref>, y :: <module-var-ref>)
    => res :: <boolean>;
  let defn = x.variable;
  instance?(defn, <abstract-constant-definition>) & defn == y.variable;
end;


define method check-type-transformer
    (component :: <component>, call :: <known-call>)
    => (did-anything? :: <boolean>);
  let (okay?, object-leaf, type-leaf) = extract-args(call, 2, #f, #f, #f);
  if (okay? & instance?(type-leaf, <literal-constant>))
    let type = type-leaf.value;
    if (csubtype?(object-leaf.derived-type, type))
      replace-expression(component, call.dependents, object-leaf);
      #t;
    else
      let builder = make-builder(component);
      let dep = call.dependents;
      let assign = dep.dependent;
      let policy = assign.policy;
      let source = assign.source-location;

      let temp = make-ssa-var(builder, #"cond", specifier-type(#"<boolean>"));
      let res = make-ssa-var(builder, #"result", type);
      build-assignment
	(builder, policy, source, temp,
	 make-unknown-call
	   (builder, ref-dylan-defn(builder, policy, source, #"instance?"),
	    #f, list(object-leaf, type-leaf)));
      build-if-body(builder, policy, source, temp);
      build-assignment
	(builder, policy, source, res,
	 make-operation(builder, <truly-the>, list(object-leaf),
			guaranteed-type: type));
      build-else(builder, policy, source);
      build-assignment
	(builder, policy, source, #(),
	 make-error-operation
	   (builder, policy, source, #"type-error", object-leaf, type-leaf));
      end-body(builder);
      insert-before(component, assign, builder-result(builder));
      replace-expression(component, dep, res);
    end;
  else
    #f;
  end;
end;

define-transformer(#"check-type", #f, check-type-transformer);



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
	       (builder,
		ref-dylan-defn(builder, policy, source, #"values-sequence"),
		#f,
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
	       (builder,
		ref-dylan-defn(builder, policy, source, #"values-sequence"),
		#f,
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
    let pair-leaf = ref-dylan-defn(builder, policy, source, #"pair");
    let current-value = make-literal-constant(builder, as(<ct-value>, #()));
    for (arg in reverse!(args))
      let temp = make-local-var(builder, #"temp", object-ctype());
      build-assignment
	(builder, policy, source, temp,
	 make-unknown-call(builder, pair-leaf, #f,
			   list(arg, current-value)));
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
	 (builder, ref-dylan-defn(builder, policy, source, #"initialize"), #f,
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
      build-else(builder, policy, source);
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

define-transformer(#"do", #(#"<function>", #"<sequence>"), do-transformer);


// Utilities for building iterators.

define class <iteration-vars> (<object>)
  slot iteration-collection-var :: <leaf>,
    required-init-keyword: collection-var:;
  slot iteration-state-var :: <leaf>,
    required-init-keyword: state-var:;
  slot iteration-limit-var :: <leaf>,
    required-init-keyword: limit-var:;
  slot iteration-next-state-func :: <leaf>,
    required-init-keyword: next-state-func:;
  slot iteration-finished?-func :: <leaf>,
    required-init-keyword: finished?-func:;
  slot iteration-current-key-func :: <leaf>,
    required-init-keyword: current-key-func:;
  slot iteration-current-element-func :: <leaf>,
    required-init-keyword: current-element-func:;
  slot iteration-current-element-setter-func :: <leaf>,
    required-init-keyword: current-element-setter-func:;
end;

define method iteration-setup
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     orig-coll :: <leaf>)
    => res :: <iteration-vars>;
  let coll = make-lexical-var(builder, #"collection", source, object-ctype());
  let state = make-lexical-var(builder, #"state", source, object-ctype());
  let limit = make-lexical-var(builder, #"limit", source, object-ctype());
  let next = make-lexical-var(builder, #"next", source, function-ctype());
  let done = make-lexical-var(builder, #"done?", source, function-ctype());
  let curkey = make-lexical-var(builder, #"curkey", source, function-ctype());
  let curel = make-lexical-var(builder, #"curel", source, function-ctype());
  let curel-setter
    = make-lexical-var(builder, #"curel-setter", source, function-ctype());
  build-let(builder, policy, source, coll, orig-coll);
  build-let(builder, policy, source,
	    list(state, limit, next, done, curkey, curel, curel-setter),
	    make-unknown-call(builder,
			      ref-dylan-defn(builder, policy, source,
					     #"forward-iteration-protocol"),
			      #f,
			      list(coll)));
  make(<iteration-vars>,
       collection-var: coll,
       state-var: state,
       limit-var: limit,
       next-state-func: next,
       finished?-func: done,
       current-key-func: curkey,
       current-element-func: curel,
       current-element-setter-func: curel-setter);
end;

define method iteration-advance
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     iteration-vars :: <iteration-vars>)
  build-assignment
    (builder, policy, source, iteration-vars.iteration-state-var,
     make-unknown-call
       (builder, iteration-vars.iteration-next-state-func, #f,
	list(iteration-vars.iteration-collection-var,
	     iteration-vars.iteration-state-var)));
end;

define method iteration-done
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     iteration-vars :: <iteration-vars>)
    => var :: <leaf>;
  let var = make-local-var(builder, #"done?", object-ctype());
  build-assignment
    (builder, policy, source, var,
     make-unknown-call
       (builder, iteration-vars.iteration-finished?-func, #f,
	list(iteration-vars.iteration-collection-var,
	     iteration-vars.iteration-state-var,
	     iteration-vars.iteration-limit-var)));
  var;
end;

define method iteration-current-key
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     iteration-vars :: <iteration-vars>)
    => var :: <leaf>;
  let var = make-local-var(builder, #"current-key", object-ctype());
  build-assignment
    (builder, policy, source, var,
     make-unknown-call
       (builder, iteration-vars.iteration-current-key-func, #f,
	list(iteration-vars.iteration-collection-var,
	     iteration-vars.iteration-state-var)));
  var;
end;

define method iteration-current-element
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     iteration-vars :: <iteration-vars>)
    => var :: <leaf>;
  let var = make-local-var(builder, #"current-element", object-ctype());
  build-assignment
    (builder, policy, source, var,
     make-unknown-call
       (builder, iteration-vars.iteration-current-element-func, #f,
	list(iteration-vars.iteration-collection-var,
	     iteration-vars.iteration-state-var)));
  var;
end;

define method iteration-current-element-setter
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     new-value :: <leaf>, iteration-vars :: <iteration-vars>)
    => ();
  build-assignment
    (builder, policy, source, #(),
     make-unknown-call
       (builder, iteration-vars.iteration-current-element-setter-func, #f,
	list(new-value, iteration-vars.iteration-collection-var,
	     iteration-vars.iteration-state-var)));
end;

