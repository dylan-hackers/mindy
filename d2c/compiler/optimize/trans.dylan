module: cheese
rcs-header: $Header: /scm/cvs/src/d2c/compiler/optimize/trans.dylan,v 1.1 1998/05/03 19:55:34 andreas Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
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

//
// Fer-to-fer magic transformations.
//
// Callopt arranges to call maybe-transform-call when it thinks it would be
// useful.  Maybe-transform-call looks to see if there are any transformers
// for the called function, and if so, gives them each a change at transforming
// the call.
//
// Maybe-transform-call can be called on either <unknown-call>s or
// <known-call>s (<abstract-call> is close enough), it will only be called
// when the call is valid.  We don't restrict it to <known-call> because
// generic functions are never <known-call>ed.
//


// maybe-transform-call
//
// The main entry point to transforming calls.
// 
define method maybe-transform-call
    (component :: <component>, call :: <abstract-call>)
    => did-anything? :: <boolean>;
  block (return)
    for (transformer in call.depends-on.source-exp.find-transformers-leaf)
      if (transformer.transformer-function(component, call))
	return(#t);
      end;
    end;
    #f;
  end;
end method maybe-transform-call;


define generic find-transformers-leaf (func :: <leaf>) => res :: <list>;

define method find-transformers-leaf (func :: <leaf>) => res :: <list>;
  #();
end;

define method find-transformers-leaf (func :: <definition-constant-leaf>)
    => res :: <list>;
  find-transformers-defn(func.const-defn);
end;

define method find-transformers-leaf (func :: <literal-constant>)
    => res :: <list>;
  let defn = func.value.ct-function-definition;
  if (defn)
    find-transformers-defn(defn);
  else
    #();
  end;
end;


define generic find-transformers-defn (func :: <definition>)
    => res :: <list>;

define method find-transformers-defn (func :: <definition>)
    => res :: <list>;
  #();
end;

define method find-transformers-defn (defn :: <function-definition>)
    => res :: <list>;
  defn.function-defn-transformers;
end;
  
define method find-transformers-defn (defn :: <generic-definition>)
    => res :: <list>;
  choose(method (transformer)
	   let specializers = transformer.transformer-specializers;
	   specializers == #f | specializers == #"gf";
	 end,
	 defn.function-defn-transformers);
end;


// extract-args
//
// Extracts the arguments from a call.
// 
define method extract-args
    (call :: <known-call>, nfixed :: <integer>, want-next? :: <boolean>,
     rest? :: <boolean>, keys :: false-or(<list>))
    => (okay? :: <boolean>, #rest arg :: type-union(<leaf>, <list>));
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
    let results = make(<simple-object-vector>,
		       size: nfixed
			 + if (want-next?) 1 else 0 end
			 + if (rest?) 1 else 0 end);
    local
      method decode-fixed-and-on
	  (index :: <integer>, args :: false-or(<dependency>))
	  => ();
	if (index == nfixed)
	  decode-next-and-rest(index, args);
	else
	  results[index] := args.source-exp;
	  decode-fixed-and-on(index + 1, args.dependent-next);
	end;
      end,
      method decode-next-and-rest
	  (index :: <integer>, args :: false-or(<dependency>))
	  => ();
	if (sig.next?)
	  if (want-next?)
	    results[index] := args.source-exp;
	    index := index + 1;
	  end;
	  args := args.dependent-next;
	elseif (want-next?)
	  error("Transformer inconsistent w/ function signature.");
	end if;
	if (rest?)
	  let rest = extract-rest-arg(args.source-exp);
	  unless (rest)
	    return(#f);
	  end unless;
	  results[index] := rest;
	end if;
      end method decode-next-and-rest;
    decode-fixed-and-on(0, call.depends-on.dependent-next);
    apply(values, #t, results);
  end block;
end method extract-args;


define method extract-args
    (call :: <unknown-call>, nfixed :: <integer>, want-next? :: <boolean>,
     rest? :: <boolean>, keys :: false-or(<list>))
    => (okay? :: <boolean>, #rest arg :: type-union(<leaf>, <list>));
  let sig = find-signature(call.depends-on.source-exp);
  unless (sig.specializers.size == nfixed)
    error("Transformer inconsistent w/ function signature.");
  end;
  if (want-next?)
    error("GF transformer wants the next info?");
  end if;
  unless (if (rest?) sig.rest-type else ~sig.rest-type end)
    error("Transformer inconsistent w/ function signature.");
  end;
  if (keys)
    error("Can't extract keyword arguments yet.");
  end;
  let results = make(<simple-object-vector>,
		     size: if (rest?) nfixed + 1 else nfixed end);
  for (dep = call.depends-on.dependent-next then dep.dependent-next,
       i from 0 below nfixed)
    results[i] := dep.source-exp;
  finally
    if (rest?)
      for (dep = dep then dep.dependent-next,
	   rest-args = #() then pair(dep.source-exp, rest-args),
	   while: dep)
      finally
	results[nfixed] := reverse!(rest-args);
      end for;
    end if;
  end for;
  apply(values, #t, results);
end method extract-args;


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
  if (expr.primitive-name == #"vector")
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

define method generic-==-transformer
    (component :: <component>, call :: <unknown-call>)
    => did-anything? :: <boolean>;
  block (return)
    let (okay?, x, y) = extract-args(call, 2, #f, #f, #f);
    unless (okay?)
      return(#f);
    end unless;
    if (trivial-==-optimization(component, call, x, y))
      return(#t);
    end if;
    let x-disjoint? = #t;
    let y-disjoint? = #t;
    for (specifier
	   in #[#"<integer>", #"<extended-integer>", #"<single-float>",
		#"<double-float>", #"<extended-float>", #"<raw-pointer>"])
      let type = specifier-type(specifier);
      if (csubtype?(x.derived-type, type))
	if (csubtype?(y.derived-type, type))
	  // They are both the special type.  That means that method selection
	  // can pick the correct method, so blow out of here.
	  return(#f);
	else
	  // X is one of the special types, but Y isn't.  So we replace
	  // the check with:
	  //   instance?(y, type) & x == truly-the(y, type);
	  replace-==-with-instance?-then-==(component, call, x, y, type);
	  return(#t);
	end if;
      else
	if (csubtype?(y.derived-type, type))
	  // Y is the special type, but X isn't (tested above).
	  replace-==-with-instance?-then-==(component, call, y, x, type);
	  return(#t);
	else
	  if (ctypes-intersect?(x.derived-type, type))
	    x-disjoint? := #f;
	  end if;
	  if (ctypes-intersect?(y.derived-type, type))
	    y-disjoint? := #f;
	  end if;
	end if;
      end if;
    end for;
    if (~x-disjoint? & y-disjoint?)
      // If only one of the two arguments is disjoint from all the special
      // types we know about, we want to put that arugment first.  Doing so
      // will allow method selection to pick the <object>,<object> method.
      // We don't just switch to that method ourselves, in case this code
      // becomes inconsistent with the set of methods on \==.
      replace-expression
	(component, call.dependents,
	 make-unknown-call
	   (make-builder(component), call.depends-on.source-exp, #f,
	    list(y, x)));
      #t;
    else
      #f;
    end if;
  end block;
end method generic-==-transformer;

define-transformer(#"==", #"gf", generic-==-transformer);


define method trivial-==-optimization
    (component :: <component>, operation :: <operation>,
     x :: <leaf>, y :: <leaf>)
    => did-anything? :: <boolean>;
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

define method origin (thing :: <expression>) => origin :: <expression>;
  thing;
end method origin;

define method origin (thing :: <ssa-variable>) => origin :: <expression>;
  let assign = thing.definer;
  if (thing == assign.defines)
    let source = assign.depends-on.source-exp;
    if (source.expression-movable?)
      origin(source);
    else
      thing;
    end if;
  else
    thing;
  end;
end;


define method always-the-same?
    (x :: <expression>, y :: <expression>) => res :: <boolean>;
  x == y;
end;

define method always-the-same?
    (x :: <literal-constant>, y :: <literal-constant>)
    => res :: <boolean>;
  x.value == y.value;
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


define method replace-==-with-instance?-then-==
    (component :: <component>, call :: <abstract-call>,
     x :: <leaf>, y :: <leaf>, type :: <ctype>)
    => ();
  let builder = make-builder(component);
  let assign = call.dependents.dependent;
  let source = assign.source-location;
  let policy = assign.policy;

  let boolean-ctype = specifier-type(#"<boolean>");
  let result-temp = make-local-var(builder, #"result", boolean-ctype);
  let instance?-temp = make-local-var(builder, #"temp", boolean-ctype);
  build-assignment
    (builder, policy, source, instance?-temp,
     make-operation(builder, <instance?>, list(y), type: type));
  build-if-body(builder, policy, source, instance?-temp);
  let typed-temp = make-local-var(builder, #"typed", type);
  build-assignment
    (builder, policy, source, typed-temp,
     make-operation(builder, <truly-the>, list(y), guaranteed-type: type));
  build-assignment
    (builder, policy, source, result-temp,
     make-unknown-call
       (builder, call.depends-on.source-exp, #f, list(x, typed-temp)));
  build-else(builder, policy, source);
  build-assignment
    (builder, policy, source, result-temp,
     make-literal-constant(builder, as(<ct-value>, #f)));
  end-body(builder);

  insert-before(component, assign, builder-result(builder));
  replace-expression(component, call.dependents, result-temp);
end method replace-==-with-instance?-then-==;


define method object-==-transformer
    (component :: <component>, call :: <abstract-call>)
    => did-anything? :: <boolean>;
  block (return)
    let (okay?, x, y) = extract-args(call, 2, #f, #f, #f);
    unless (okay?)
      return(#f);
    end unless;
    if (trivial-==-optimization(component, call, x, y))
      return(#t);
    end if;

    let x-functional = is-it-functional?(x.derived-type);
    let y-functional = is-it-functional?(y.derived-type);
    if (x-functional == #f | y-functional == #f)
      let builder = make-builder(component);
      replace-expression
	(component, call.dependents,
	 make-operation(builder, <primitive>, list(x, y),
			name: #"=="));
      return(#t);
    end if;
    if (x-functional == #t | y-functional == #t)
      let builder = make-builder(component);
      let assign = call.dependents.dependent;
      let leaf
	= ref-dylan-defn(builder, assign.policy, assign.source-location,
			 #"slow-functional-==");
      insert-before(component, assign, builder-result(builder));
      replace-expression
	(component, call.dependents,
	 make-unknown-call(builder, leaf, #f, list(x, y)));
      return(#t);
    end if;

    #f;
  end block;
end method object-==-transformer;

define-transformer(#"==", #(#"<object>", #"<object>"), object-==-transformer);


define generic is-it-functional? (type :: <ctype>)
    => res :: one-of(#t, #f, #"maybe");

define method is-it-functional? (type :: <unknown-ctype>)
    => res :: one-of(#t, #f, #"maybe");
  #"maybe";
end;

define method is-it-functional? (class :: <cclass>)
    => res :: one-of(#t, #f, #"maybe");
  if (~class.sealed?)
    if (class.functional?)
      #t;
    elseif (class.not-functional?)
      #f;
    else
      #"maybe";
    end;
  else
    let subs = find-direct-classes(class);
    if (every?(functional?, subs))
      #t;
    elseif (every?(not-functional?, subs))
      #f;
    else
      #"maybe";
    end if;
  end if;
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


define method slow-functional-==-transformer
    (component :: <component>, call :: <abstract-call>)
    => did-anything? :: <boolean>;
  let (okay?, x, y) = extract-args(call, 2, #f, #f, #f);
  if (okay?)
    if (trivial-==-optimization(component, call, x, y))
      #t;
    else
      let x-classes = find-direct-classes(x.derived-type);
      if (x-classes & x-classes.size == 1)
	replace-with-functional-==(component, call, x-classes.first, x, y);
	#t;
      else
	let y-classes = find-direct-classes(y.derived-type);
	if (y-classes & y-classes.size == 1)
	  replace-with-functional-==(component, call, y-classes.first, y, x);
	  #t;
	else
	  #f;
	end if;
      end if;
    end if;
  else
    #f;
  end if;
end method slow-functional-==-transformer;
    
define-transformer(#"slow-functional-==", #f, slow-functional-==-transformer);


define method replace-with-functional-==
    (component :: <component>, call :: <abstract-call>,
     class :: <cclass>, x :: <leaf>, y :: <leaf>)
    => ();
  let builder = make-builder(component);
  let assign = call.dependents.dependent;
  let source = assign.source-location;
  let policy = assign.policy;

  let type = make(<direct-instance-ctype>, base-class: class);
  let need-instance? = ~csubtype?(y.derived-type, type);

  let boolean-ctype = specifier-type(#"<boolean>");
  let result-temp = make-local-var(builder, #"result", boolean-ctype);

  let typed
    = if (need-instance?)
	let instance?-temp = make-local-var(builder, #"temp", boolean-ctype);
	build-assignment
	  (builder, policy, source, instance?-temp,
	   make-operation(builder, <instance?>, list(y), type: type));
	build-if-body(builder, policy, source, instance?-temp);
	let typed-temp = make-local-var(builder, #"typed", type);
	build-assignment
	  (builder, policy, source, typed-temp,
	   make-operation(builder, <truly-the>, list(y),
			  guaranteed-type: type));
	typed-temp;
      else
	y;
      end if;

  let func-leaf = ref-dylan-defn(builder, policy, source, #"functional-==");
  let class-leaf = make-literal-constant(builder, class);
  build-assignment
    (builder, policy, source, result-temp,
     make-unknown-call(builder, func-leaf, #f, list(class-leaf, x, typed)));

  if (need-instance?)
    build-else(builder, policy, source);
    build-assignment
      (builder, policy, source, result-temp,
       make-literal-constant(builder, as(<ct-value>, #f)));
    end-body(builder);
  end if;

  insert-before(component, assign, builder-result(builder));
  replace-expression(component, call.dependents, result-temp);
end method replace-with-functional-==;



// Type transforms.

// check-type
//
// We transforms calls of check-type where the type is a compile-time constant
// into assignments with a type assertion so the other parts of the compiler
// can more easily identify the type restriction.
// 
define method check-type-transformer
    (component :: <component>, call :: <abstract-call>)
    => (did-anything? :: <boolean>);
  let (okay?, object, type) = extract-args(call, 2, #f, #f, #f);
  if (okay?)
    let type = extract-constant-type(type);
    if (type)
      let builder = make-builder(component);
      let dep = call.dependents;
      let assign = dep.dependent;
      let policy = assign.policy;
      let source = assign.source-location;

      let checked = make-ssa-var(builder, #"checked", type);
      build-assignment(builder, policy, source, checked, object);
      insert-before(component, assign, builder-result(builder));
      replace-expression(component, dep, checked);
      #t;
    else
      #f;
    end;
  else
    #f;
  end;
end;
//
define-transformer(#"check-type", #f, check-type-transformer);

define method extract-constant-type (leaf :: <leaf>)
    => res :: false-or(<ctype>);
  #f;
end;

define method extract-constant-type (leaf :: <literal-constant>)
  leaf.value;
end;

define method extract-constant-type (leaf :: <definition-constant-leaf>)
  leaf.const-defn.ct-value;
end;

define method instance?-transformer
    (component :: <component>, call :: <abstract-call>)
    => (did-anything? :: <boolean>);
  let (okay?, value-leaf, type-leaf) = extract-args(call, 2, #f, #f, #f);
  if (okay?)
    let type = extract-constant-type(type-leaf);
    if (type)
      let derived-type = value-leaf.derived-type;
      let type-extent = type.ctype-extent;
      replace-expression
	(component, call.dependents,
	 if (csubtype?(derived-type, type-extent))
	   make-literal-constant(make-builder(component), as(<ct-value>, #t));
	 elseif (~ctypes-intersect?(derived-type, type-extent))
	   make-literal-constant(make-builder(component), as(<ct-value>, #f));
	 else
	   make-operation
	     (make-builder(component), <instance?>, list(value-leaf),
	      type: type);
	 end);
      #t;
    end;
  end;
end;

define-transformer(#"instance?", #f, instance?-transformer);


define method optimize
    (component :: <component>, op :: <instance?>) => ();
  let value-type = op.depends-on.source-exp.derived-type;
  let test-type = op.type.ctype-extent;
  if (csubtype?(value-type, test-type))
    replace-expression
      (component, op.dependents,
       make-literal-constant(make-builder(component), as(<ct-value>, #t)));
  elseif (~ctypes-intersect?(value-type, test-type))
    replace-expression
      (component, op.dependents,
       make-literal-constant(make-builder(component), as(<ct-value>, #f)));
  end if;
end method optimize;


define method replace-placeholder
    (component :: <component>, dep :: <dependency>, op :: <instance?>)
    => ();
  for (dep = op.depends-on then dep.dependent-next,
       while: dep)
    replace-placeholder(component, dep, dep.source-exp);
  end;

  let builder = make-builder(component);
  let assign = dep.dependent;
  let policy = assign.policy;
  let source = assign.source-location;
  
  let expr = build-instance?(builder, policy, source,
			     op.depends-on.source-exp, op.type);

  insert-before(component, assign, builder-result(builder));
  replace-expression(component, dep, expr);
end;

define generic build-instance?
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     value :: <leaf>, type :: <ctype>)
    => res :: <expression>;

define method build-instance?
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     value :: <leaf>, type :: <ctype>)
    => res :: <expression>;
  make-unknown-call
    (builder, ref-dylan-defn(builder, policy, source, #"%instance?"),
     #f, list(value, make-literal-constant(builder, type)));
end;

define method build-instance?
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     value :: <leaf>, type :: <union-ctype>)
    => res :: <expression>;
  let res = make-local-var(builder, #"temp", specifier-type(#"<boolean>"));
  local
    method repeat (remaining :: <list>)
      if (remaining == #())
	build-assignment(builder, policy, source, res,
			 make-literal-constant(builder, as(<ct-value>, #f)));
      else
	let member-type = remaining.head;
	if (ctypes-intersect?(value.derived-type, member-type))
	  let temp = make-local-var(builder, #"temp",
				    specifier-type(#"<boolean>"));
	  build-assignment(builder, policy, source, temp,
			   build-instance?(builder, policy, source, value,
					   member-type));
	  build-if-body(builder, policy, source, temp);
	  build-assignment(builder, policy, source, res,
			   make-literal-constant(builder, as(<ct-value>, #t)));
	  build-else(builder, policy, source);
	  repeat(remaining.tail);
	  end-body(builder);
	else
	  repeat(remaining.tail);
	end;
      end;
    end;
  repeat(type.members);
  res;
end;

define method build-instance?
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     value :: <leaf>, type :: <singleton-ctype>)
    => res :: <expression>;
  make-unknown-call
    (builder, ref-dylan-defn(builder, policy, source, #"=="),
     #f, list(value, make-literal-constant(builder, type.singleton-value)));
end;

// ### limited integers?

define method build-instance?
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     value :: <leaf>, type :: <byte-character-ctype>)
    => res :: <expression>;
  let res = make-local-var(builder, #"temp", specifier-type(#"<boolean>"));
  let char-ctype = specifier-type(#"<character>");
  let guaranteed-character? = csubtype?(value.derived-type, char-ctype);
  unless (guaranteed-character?)
    let temp = make-local-var(builder, #"temp", specifier-type(#"<boolean>"));
    build-assignment(builder, policy, source, temp,
		     build-instance?(builder, policy, source,
				     value, char-ctype));
    build-if-body(builder, policy, source, temp);
    let new-value = make-local-var(builder, #"char", char-ctype);
    build-assignment(builder, policy, source, new-value,
		     make-operation(builder, <truly-the>, list(value),
				    guaranteed-type: char-ctype));
    value := new-value;
  end;
  let char-code-temp = make-local-var(builder, #"code", object-ctype());
  build-assignment
    (builder, policy, source, char-code-temp,
     make-unknown-call
       (builder, ref-dylan-defn(builder, policy, source, #"as"), #f,
	list(make-literal-constant(builder, specifier-type(#"<integer>")),
	     value)));
  build-assignment
    (builder, policy, source, res,
     make-unknown-call
       (builder, ref-dylan-defn(builder, policy, source, #"<"), #f,
	list(char-code-temp,
	     make-literal-constant(builder, as(<ct-value>, 256)))));
  unless (guaranteed-character?)
    build-else(builder, policy, source);
    build-assignment(builder, policy, source, res,
		     make-literal-constant(builder, as(<ct-value>, #f)));
    end-body(builder);
  end;
  res;
end;

define method build-instance?
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     value :: <leaf>, class :: <cclass>, #next next-method)
    => res :: <expression>;
  if (class.sealed?)
    let res = make-local-var(builder, #"temp", specifier-type(#"<boolean>"));
    if (class == specifier-type(#"<false>"))
      build-assignment
	(builder, policy, source, res,
	 make-unknown-call
	   (builder, ref-dylan-defn(builder, policy, source, #"=="), #f,
	    list(value, make-literal-constant(builder, as(<ct-value>, #f)))));
    elseif (class == specifier-type(#"<true>"))
      build-assignment
	(builder, policy, source, res,
	 make-unknown-call
	   (builder, ref-dylan-defn(builder, policy, source, #"=="), #f,
	    list(value, make-literal-constant(builder, as(<ct-value>, #t)))));
    elseif (class == specifier-type(#"<empty-list>"))
      build-assignment
	(builder, policy, source, res,
	 make-unknown-call
	   (builder, ref-dylan-defn(builder, policy, source, #"=="), #f,
	    list(value, make-literal-constant(builder, as(<ct-value>, #())))));
    else
      let class-temp = make-local-var(builder, #"class", object-ctype());
      build-assignment
	(builder, policy, source, class-temp,
	 make-unknown-call
	   (builder, ref-dylan-defn(builder, policy, source, #"%object-class"),
	    #f, list(value)));
      let direct-classes
	= choose(method (direct-class)
		   ctypes-intersect?(value.derived-type,
				     make(<direct-instance-ctype>,
					  base-class: direct-class));
		 end,
		 find-direct-classes(class));
      if (empty?(direct-classes))
	error("None of the direct classes intersect the derived type but we "
		"didn't optimize the test away?");
      elseif (class.subclass-id-range-min & direct-classes.size > 2)
	// There is a range we can check the unique-id against.
	let id-temp = make-local-var(builder, #"unique-id", object-ctype());
	build-assignment
	  (builder, policy, source, id-temp,
	   make-unknown-call
	     (builder, ref-dylan-defn(builder, policy, source, #"unique-id"),
	      #f, list(class-temp)));
	let test-min? = #f;
	let test-max? = #f;
	let possible-classes = find-direct-classes(value.derived-type);
	if (possible-classes)
	  for (possible-class in possible-classes)
	    let possible-id = possible-class.unique-id;
	    if (~possible-id | possible-id < class.subclass-id-range-min)
	      test-min? := #t;
	    elseif (possible-id > class.subclass-id-range-max)
	      test-max? := #t;
	    end;
	  end;
	  unless (test-min? | test-max?)
	    signal("All of the potential direct classes are subtypes of "
		     "the class against which we are testing.  Someone should "
		     "have noticed before this.");
	  end;
	else
	  test-min? := #t;
	  test-max? := #t;
	end;
	if (test-min?)
	  let temp = make-local-var(builder, #"temp",
				    specifier-type(#"<boolean>"));
	  build-assignment
	    (builder, policy, source, temp,
	     make-unknown-call
	       (builder, ref-dylan-defn(builder, policy, source, #"<"), #f,
		list(id-temp,
		     make-literal-constant
		       (builder,
			as(<ct-value>, class.subclass-id-range-min)))));
	  build-if-body(builder, policy, source, temp);
	  build-assignment(builder, policy, source, res,
			   make-literal-constant(builder, as(<ct-value>, #f)));
	  build-else(builder, policy, source);
	end;
	if (test-max?)
	  let temp = make-local-var(builder, #"temp",
				    specifier-type(#"<boolean>"));
	  build-assignment
	    (builder, policy, source, temp,
	     make-unknown-call
	       (builder, ref-dylan-defn(builder, policy, source, #"<"), #f,
		list(make-literal-constant
		       (builder, as(<ct-value>, class.subclass-id-range-max)),
		     id-temp)));
	  build-if-body(builder, policy, source, temp);
	  build-assignment(builder, policy, source, res,
			   make-literal-constant(builder, as(<ct-value>, #f)));
	  build-else(builder, policy, source);
	end;
	build-assignment(builder, policy, source, res,
			 make-literal-constant(builder, as(<ct-value>, #t)));
	if (test-max?)
	  end-body(builder);
	end;
	if (test-min?)
	  end-body(builder);
	end;
      else
	local
	  method repeat (remaining :: <list>)
	    if (remaining.tail == #())
	      build-assignment
		(builder, policy, source, res,
		 make-unknown-call
		   (builder, ref-dylan-defn(builder, policy, source, #"=="),
		    #f,
		    list(class-temp,
			 make-literal-constant(builder, remaining.head))));
	    else
	      let temp = make-local-var(builder, #"temp",
					specifier-type(#"<boolean>"));
	      build-assignment
		(builder, policy, source, temp,
		 make-unknown-call
		   (builder, ref-dylan-defn(builder, policy, source, #"=="),
		    #f,
		    list(class-temp,
			 make-literal-constant(builder, remaining.head))));
	      build-if-body(builder, policy, source, temp);
	      build-assignment(builder, policy, source, res,
			       make-literal-constant
				 (builder, as(<ct-value>, #t)));
	      build-else(builder, policy, source);
	      repeat(remaining.tail);
	      end-body(builder);
	    end;
	  end;
	repeat(direct-classes);
      end;
    end;
    res;
  else
    next-method();
  end;
end;  

define method build-instance?
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     value :: <leaf>, type :: <direct-instance-ctype>)
    => res :: <expression>;
  let class-temp = make-local-var(builder, #"class", object-ctype());
  build-assignment
    (builder, policy, source, class-temp,
     make-unknown-call
       (builder, ref-dylan-defn(builder, policy, source, #"%object-class"),
	#f, list(value)));
  make-unknown-call
    (builder, ref-dylan-defn(builder, policy, source, #"=="),
     #f, list(class-temp, make-literal-constant(builder, type.base-class)));
end method build-instance?;

define method build-instance?
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     value :: <leaf>, type :: <subclass-ctype>)
    => res :: <expression>;
  local method build-subtype-call ()
	  make-unknown-call
	    (builder, ref-dylan-defn(builder, policy, source, #"subtype?"), #f,
	     list(value, make-literal-constant(builder, type.subclass-of)));
	end method build-subtype-call;
  let class-ctype = specifier-type(#"<class>");
  if (csubtype?(value.derived-type, class-ctype))
    build-subtype-call();
  else
    let temp = make-local-var(builder, #"temp", specifier-type(#"<boolean>"));
    build-assignment
      (builder, policy, source, temp,
       build-instance?(builder, policy, source, value, class-ctype));
    if (csubtype?(ctype-intersection(value.derived-type, class-ctype), type))
      temp;
    else
      let res = make-local-var(builder, #"temp", specifier-type(#"<boolean>"));
      build-if-body(builder, policy, source, temp);
      build-assignment(builder, policy, source, res, build-subtype-call());
      build-else(builder, policy, source);
      build-assignment(builder, policy, source, res,
		       make-literal-constant(builder, as(<ct-value>, #f)));
      end-body(builder);
      res;
    end if;
  end if;
end method build-instance?;



// slot initialized and address functions.

define method slot-initialized?-transformer
    (component :: <component>, call :: <abstract-call>)
    => (did-anything? :: <boolean>);
  block (done)
    let (okay?, instance, getter) = extract-args(call, 2, #f, #f, #f);
    unless (okay?) done(#f) end unless;

    let class = best-idea-of-class(instance.derived-type);
    unless (class) done(#f) end unless;

    let slot = slot-from-getter(class, getter);
    unless (slot) done(#f) end unless;

    let builder = make-builder(component);
    let instance-type = instance.derived-type;
    if (slot-guaranteed-initialized?(slot, instance-type))
      replace-expression(component, call.dependents,
			 make-literal-constant(builder, as(<ct-value>, #t)));
      done(#t);
    end if;

    let init?-slot = slot.slot-initialized?-slot;
    if (init?-slot)
      let offset = find-slot-offset(init?-slot, instance-type);
      unless (offset) done(#f) end unless;

      replace-expression
	(component, call.dependents,
	 make-operation(builder, <heap-slot-ref>,
			list(instance,
			     make-literal-constant(builder,
						   as(<ct-value>, offset))),
			derived-type: init?-slot.slot-type.ctype-extent,
			slot-info: init?-slot));
      done(#t);
    end if;

    let offset = find-slot-offset(slot, instance-type);
    unless (offset) done(#f) end unless;

    let dep = call.dependents;
    let call-assign = dep.dependent;
    let policy = call-assign.policy;
    let source = call-assign.source-location;
    let temp = make-local-var(builder, slot.slot-getter.variable-name,
			      slot.slot-type);
    build-assignment
      (builder, policy, source, temp,
       make-operation(builder, <heap-slot-ref>,
		      list(instance,
			   make-literal-constant(builder,
						 as(<ct-value>, offset))),
		      derived-type: slot.slot-type.ctype-extent,
		      slot-info: slot));
    replace-expression(component, dep,
		       make-operation(builder, <primitive>, list(temp),
				      name: #"initialized?"));
    insert-before(component, call-assign, builder-result(builder));
    done(#t);
  end block;
end method slot-initialized?-transformer;
	    
define-transformer(#"slot-initialized?", #f, slot-initialized?-transformer);


define method slot-from-getter
    (class :: <cclass>, getter :: <literal-constant>)
    => res :: false-or(<slot-info>);
  block (return)
    let ctv = getter.value;
    for (slot in class.all-slot-infos)
      let var = slot.slot-getter;
      if (var)
	let defn = var.variable-definition;
	if (defn & defn.ct-value == ctv)
	  return(slot);
	end;
      end;
    end;
    #f;
  end;
end method slot-from-getter;

define method slot-from-getter
    (class :: <cclass>, getter :: <definition-constant-leaf>)
    => res :: false-or(<slot-info>);
  block (return)
    for (slot in class.all-slot-infos)
      let var = slot.slot-getter;
      if (var & var.variable-definition == getter)
	return(slot);
      end;
    end;
    #f;
  end;
end method slot-from-getter;

define method slot-from-getter
    (class :: <cclass>, getter :: <leaf>) => res :: <false>;
  #f;
end method slot-from-getter;




define method apply-transformer
    (component :: <component>, call :: <abstract-call>)
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
    (component :: <component>, call :: <abstract-call>)
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
    maybe-restrict-type
      (component, call,
       make(<direct-instance-ctype>, base-class: cclass).ctype-extent);
    unless (instance?(cclass, <defined-cclass>))
      give-up();
    end;
    let defn = cclass.class-defn;
    if (defn.class-defn-defered-evaluations-function)
      give-up();
    end;
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


// Seals for file trans.dylan

// <iteration-vars> -- subclass of <object>
define sealed domain make(singleton(<iteration-vars>));
define sealed domain initialize(<iteration-vars>);
