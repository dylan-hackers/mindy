module: cheese
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/optimize/callopt.dylan,v 1.1 1996/02/02 23:19:47 wlott Exp $
copyright: Copyright (c) 1996  Carnegie Mellon University
	   All rights reserved.

// Call optimization.

// <unknown-call> optimization.
//
// Basically, we check the arguments against the signature and try to change
// into a <known-call> if we can and an <error-call> if we have to.
//
define method optimize
    (component :: <component>, call :: <unknown-call>) => ();
  let func-dep = call.depends-on;
  unless (func-dep)
    error("No function in a call?");
  end;
  // Dispatch of the thing we are calling.
  optimize-unknown-call(component, call, func-dep.source-exp);
end;


define method optimize-unknown-call
    (component :: <component>, call :: <unknown-call>, func :: <leaf>)
    => ();
  // Assert that the function is a function.
  assert-type(component, call.dependents.dependent, call.depends-on,
	      if (call.use-generic-entry?)
		specifier-type(#"<method>");
	      else
		function-ctype();
	      end);
end;

define method optimize-unknown-call
    (component :: <component>, call :: <unknown-call>,
     func :: <function-literal>)
    => ();
  maybe-restrict-type(component, call, func.main-entry.result-type);
  maybe-change-to-known-or-error-call(component, call, func.signature,
				      func.main-entry.name);
end;

define method optimize-unknown-call
    (component :: <component>, call :: <unknown-call>,
     func :: <definition-constant-leaf>)
    => ();
  optimize-unknown-call(component, call, func.const-defn);
end;

define method optimize-unknown-call
    (component :: <component>, call :: <unknown-call>,
     func :: <literal-constant>)
    => ();
  let value = func.value;
  if (instance?(value, <ct-function>))
    let defn = value.ct-function-definition;
    if (defn)
      optimize-unknown-call(component, call, defn);
    else
      let sig = value.ct-function-signature;
      maybe-restrict-type(component, call, sig.returns);
      maybe-change-to-known-or-error-call(component, call, sig,
					  value.ct-function-name);
    end;
  else
    // Assert that the function is a function.  It isn't, but this is a handy
    // way of generating an error message.
    assert-type(component, call.dependents.dependent, call.depends-on,
		if (call.use-generic-entry?)
		  specifier-type(#"<method>");
		else
		  function-ctype();
		end);
  end;
end;

define method optimize-unknown-call
    (component :: <component>, call :: <unknown-call>,
     defn :: <abstract-constant-definition>)
    => ();
  // Assert that the function is a function.
  assert-type(component, call.dependents.dependent, call.depends-on,
	      if (call.use-generic-entry?)
		specifier-type(#"<method>");
	      else
		function-ctype();
	      end);
end;

define method optimize-unknown-call
    (component :: <component>, call :: <unknown-call>,
     defn :: <function-definition>)
    => ();
  maybe-restrict-type(component, call, defn.function-defn-signature.returns);
end;

define method optimize-unknown-call
    (component :: <component>, call :: <unknown-call>,
     defn :: <generic-definition>)
    => ();
  let sig = defn.function-defn-signature;
  maybe-restrict-type(component, call, sig.returns);

  if (call.use-generic-entry?)
    error("Trying to pass a generic function next-method information?");
  end;

  let bogus? = #f;
  let arg-leaves = #();
  let arg-types = #();
  block (return)
    for (arg-dep = call.depends-on.dependent-next then arg-dep.dependent-next,
	 count from 0,
	 gf-spec in sig.specializers)
      unless (arg-dep)
	compiler-warning("In %s:\n  not enough arguments in call of %s.\n  "
			   "Wanted %s %d, but only got %d",
			 call.home-function-region.name,
			 defn.defn-name,
			 if (sig.rest-type | sig.key-infos)
			   "at least";
			 else
			   "exactly";
			 end,
			 sig.specializers.size,
			 count);
	bogus? := #t;
	return();
      end;
      let arg-leaf = arg-dep.source-exp;
      let arg-type = arg-leaf.derived-type;
      unless (ctypes-intersect?(arg-type, gf-spec))
	compiler-warning("In %s:\n  wrong type for argument %d in call of "
			   "%s.\n  Wanted %s, but got %s",
			 call.home-function-region.name,
			 count,
			 defn.defn-name,
			 gf-spec,
			 arg-dep.source-exp.derived-type);
	bogus? := #t;
      end;
      arg-leaves := pair(arg-leaf, arg-leaves);
      arg-types := pair(arg-type, arg-types);
    finally
      if (arg-dep)
	unless (sig.key-infos | sig.rest-type)
	  for (arg-dep = arg-dep then arg-dep.dependent-next,
	       have from count,
	       while: arg-dep)
	  finally
	    compiler-warning("In %s:\n  too many arguments in call of %s.\n"
			       "  Wanted exactly %d, but got %d.",
			     call.home-function-region.name,
			     defn.defn-name,
			     count, have);
	    bogus? := #t;
	  end;
	end;
	for (arg-dep = arg-dep then arg-dep.dependent-next,
	     while: arg-dep)
	  arg-leaves := pair(arg-dep.source-exp, arg-leaves);
	end;
      end;
    end;
  end;
  if (bogus?)
    change-call-kind(component, call, <error-call>);
  else
    optimize-generic(component, call, defn, reverse!(arg-types),
		     reverse!(arg-leaves));
  end if;
end method optimize-unknown-call;


define method optimize-generic
    (component :: <component>, call :: <abstract-call>,
     defn :: <generic-definition>, arg-types :: <list>,
     arg-leaves :: <list>)
    => ();
  block (return)
    local
      method maybe-change-to-known ()
	if (~instance?(call, <known-call>) & defn.generic-defn-discriminator)
	  // There is a static descriminator function.  We can change into a
	  // known call.  We don't reference the discriminator directly because
	  // we still want to be able to do method selection if we can derive
	  // a better idea of the argument types.
	  convert-to-known-call
	    (component, defn.generic-defn-discriminator.ct-function-signature,
	     call);
	end if;
	return();
      end method maybe-change-to-known,
      method change-to-error ()
	if (instance?(call, <known-call>))
	  // We can't just change a <known-call> into an <error-call> because
	  // all the #rest args are lumped into a vector.
	  let builder = make-builder(component);
	  let new-call
	    = make-operation(builder, <error-call>,
			     pair(call.depends-on.source-exp, arg-leaves));
	  replace-expression(component, call.dependents, new-call);
	else
	  change-call-kind(component, call, <error-call>);
	end;
	return();
      end method change-to-error;

    let (definitely, maybe) = ct-applicable-methods(defn, arg-types);
    if (definitely == #f)
      maybe-change-to-known();
    end if;

    let applicable = concatenate(definitely, maybe);

    if (applicable == #())
      no-applicable-methods-warning(call, defn, arg-types);
      change-to-error();
    end if;

    // Improve the result type based on the actually applicable methods.
    for (meth in applicable,
	 result-type = wild-ctype()
	   then values-type-union(result-type,
				  meth.function-defn-signature.returns))
    finally
      maybe-restrict-type(component, call, result-type);
    end for;

    // Blow out of here if applicable isn't a valid set of methods.
    unless (maybe == #() | (maybe.tail == #() & definitely == #()))
      return();
    end unless;

    // Compute the set of valid keywords.
    let sig = defn.function-defn-signature;
    let valid-keys
      = if (sig.key-infos)
	  if (sig.all-keys?)
	    #"all";
	  else
	    block (return)
	      reduce(method (keys :: <simple-object-vector>,
			     meth :: <method-definition>)
			 => res :: <simple-object-vector>;
		       let sig = meth.function-defn-signature;
		       if (sig.all-keys?)
			 return(#"all");
		       end if;
		       union(keys, map(key-name, sig.key-infos));
		     end method,
		     #[],
		     applicable);
	    end block;
	  end if;
	else
	  #f;
	end if;

    // Check the keyword arguments for validity.
    if (valid-keys)
      let bogus? = #f;
      for (remaining = arg-leaves then remaining.tail,
	   fixed in arg-types,
	   index from 0)
      finally
	for (remaining = remaining then remaining.tail.tail,
	     index from index by 2,
	     until: remaining == #())
	  let key-leaf = remaining.head;
	  
	  if (~instance?(key-leaf, <literal-constant>))
	    unless (ctypes-intersect?(key-leaf.derived-type,
				      specifier-type(#"<symbol>")))
	      compiler-warning("In %s:\n  bogus keyword as argument "
				 "%d in call of %s",
			       call.home-function-region.name,
			       index,
			       defn.defn-name);
	      bogus? := #t;
	    end;
	  elseif (instance?(key-leaf.value, <literal-symbol>))
	    unless (valid-keys == #"all"
		      | member?(key-leaf.value.literal-value, valid-keys))
	      compiler-warning
		("In %s:\n  invalid keyword (%=) in call of %s",
		 call.home-function-region.name,
		 key-leaf.value.literal-value,
		 defn.defn-name);
	      bogus? := #t;
	    end;
	  else
	    compiler-warning
	      ("In %s:\n  bogus keyword (%s) as argument %d in call of %s",
	       call.home-function-region.name,
	       key-leaf.value,
	       index,
	       defn.defn-name);
	    bogus? := #t;
	  end;

	  if (remaining.tail == #())
	    compiler-warning("In %s:\n  odd number of keyword/value arguments "
			       "in call of %s.",
			     call.home-function-region.name,
			     defn.defn-name);
	    change-to-error();
	  end if;
	end for;
      end for;
      if (bogus?)
	change-to-error();
      end if;
    end if;

    // Sort the applicable methods.
    let (ordered, ambiguous) = sort-methods(applicable, #f);

    if (ordered == #f)
      // We can't tell jack about how to order the methods.  So just change
      // to a known call of the discriminator if possible.
      maybe-change-to-known();
    end if;

    if (ordered == #())
      // It's ambiguous which is the most specific method.  So bitch.
      ambiguous-method-warning(call, defn, ambiguous, arg-types);
      change-to-error();
    end if;
    
    // Change to an unknown call of the most specific method.
    let builder = make-builder(component);
    let assign = call.dependents.dependent;
    let policy = assign.policy;
    let source = assign.source-location;
    let new-func
      = fer-convert-defn-ref(builder, policy, source, ordered.head);
    let next-leaf
      = make-next-method-info-leaf(builder, ordered, ambiguous);
    insert-before(component, assign, builder-result(builder));
    let new-call = make-unknown-call(builder, new-func, next-leaf, arg-leaves);
    replace-expression(component, call.dependents, new-call);
  end block;
end method optimize-generic;




define method ambiguous-method-warning
    (call :: <abstract-call>, defn :: <generic-definition>,
     ambiguous :: <list>, arg-types :: <list>)
    => ();
  let stream = make(<byte-string-output-stream>);
  write("    (", stream);
  for (arg-type in arg-types, first? = #t then #f)
    unless (first?)
      write(", ", stream);
    end;
    print-message(arg-type, stream);
  end;
  write(")", stream);
  let arg-types-string = stream.string-output-stream-string;
  for (meth in ambiguous)
    format(stream, "    %s\n", meth.defn-name);
  end;
  compiler-warning("In %s:\n  can't pick between\n%s\n  when given "
		     "arguments of types:\n%s",
		   call.home-function-region.name,
		   stream.string-output-stream-string,
		   arg-types-string);
end;

define method no-applicable-methods-warning
    (call :: <abstract-call>, defn :: <generic-definition>,
     arg-types :: <list>)
    => ();
  let stream = make(<byte-string-output-stream>);
  write("    (", stream);
  for (arg-type in arg-types, first? = #t then #f)
    unless (first?)
      write(", ", stream);
    end;
    print-message(arg-type, stream);
  end;
  write(")", stream);
  let arg-types-string = stream.string-output-stream-string;
  compiler-warning("In %s:\n  no applicable methods for argument types\n"
		     "%s\n  in call of %s",
		   call.home-function-region.name,
		   arg-types-string,
		   defn.defn-name);
end;

define method make-next-method-info-leaf
    (builder :: <fer-builder>, ordered :: <list>, ambiguous :: <list>)
    => res :: <leaf>;

  let ordered-ctvs = map(ct-value, ordered.tail);
  let ambiguous-ctvs = map(ct-value, ambiguous);

  if (every?(identity, ordered-ctvs) & every?(identity, ambiguous-ctvs))
    make-literal-constant(builder,
			  make(<literal-list>,
			       sharable: #t,
			       contents: ordered-ctvs,
			       tail: if (ordered == #())
				       as(<ct-value>, #());
				     else
				       make(<literal-list>,
					    sharable: #t,
					    contents: ambiguous-ctvs);
				     end));
  else
    error("Can't deal with next method infos that arn't all ctvs.");
    /*
    let var = make-local-var(builder, #"next-method-info",
			     object-ctype());
    let op = make-unknown-call(builder, dylan-defn-leaf(builder, #"list"), #f,
			       map(curry(make-definition-leaf, builder),
				   ordered.tail));
    build-assignment(builder, policy, source, var, op);
    var;
    */
  end;
end;

define method optimize-unknown-call
    (component :: <component>, call :: <unknown-call>,
     defn :: <abstract-method-definition>)
    => ();
  let sig = defn.function-defn-signature;
  maybe-restrict-type(component, call, sig.returns);
  select (compare-unknown-call-against-signature(call, sig, defn.defn-name))
    #"bogus" =>
      if (call.use-generic-entry?)
	error("bogus call w/ next?");
      end;
      change-call-kind(component, call, <error-call>);

    #"valid" =>
      let inline-function = defn.method-defn-inline-function;
      if (inline-function)
	let old-head = component.reoptimize-queue;
	let new-func = clone-function(component, inline-function);
	reverse-queue(component, old-head);
	replace-expression(component, call.depends-on, new-func);
      elseif (~defn.function-defn-hairy?)
	convert-to-known-call(component, sig, call);
      end;

    #"can't tell" =>
      #f;
  end;
end;

define method method-defn-inline-function
    (defn :: <abstract-method-definition>)
    => res :: false-or(<function-literal>);
  if (defn.%method-defn-inline-function == #"not-computed-yet")
    if (defn.method-defn-inline-expansion & ~defn.function-defn-hairy?)
      let component = make(<fer-component>);
      let builder = make-builder(component);
      let lexenv = make(<lexenv>);
      let leaf = fer-convert-method(builder, defn.method-defn-inline-expansion,
				    format-to-string("%s", defn.defn-name),
				    #f, #"local", lexenv, lexenv);
      optimize-component(component, simplify-only: #t);
      defn.%method-defn-inline-function := leaf;
    else
      defn.%method-defn-inline-function := #f;
    end;
  else
    defn.%method-defn-inline-function;
  end;
end;

define method optimize-unknown-call
    (component :: <component>, call :: <unknown-call>,
     func :: <exit-function>)
    => ();
  let builder = make-builder(component);
  let call-dependency = call.dependents;
  let assign = call-dependency.dependent;
  let policy = assign.policy;
  let source = assign.source-location;

  let values = make(<stretchy-vector>);
  for (dep = call.depends-on.dependent-next then dep.dependent-next,
       while: dep)
    add!(values, dep.source-exp);
  end;
  let cluster = make-values-cluster(builder, #"cluster", wild-ctype());
  build-assignment(builder, policy, source, cluster,
		   make-operation(builder, <primitive>, as(<list>, values),
				  name: #"values"));
  insert-before(component, assign, builder-result(builder));
  expand-exit-function(component, call, func, cluster);
end;


define method maybe-change-to-known-or-error-call
    (component :: <component>, call :: <unknown-call>, sig :: <signature>,
     func-name :: type-union(<name>, <string>))
    => ();
  select (compare-unknown-call-against-signature(call, sig, func-name))
    #"bogus" =>
      if (call.use-generic-entry?)
	error("bogus call w/ next?");
      end;
      change-call-kind(component, call, <error-call>);

    #"valid" =>
      convert-to-known-call(component, sig, call);

    #"can't tell" =>
      #f;
  end;
end;


define method compare-unknown-call-against-signature
    (call :: <unknown-call>, sig :: <signature>,
     func-name :: type-union(<name>, <string>))
    => res :: one-of(#"bogus", #"valid", #"can't tell");

  // Find the next-method-info and arguments.
  let (next-method-info, arguments)
    = if (call.use-generic-entry?)
	let dep = call.depends-on.dependent-next;
	values(dep.source-exp, dep.dependent-next);
      else
	values(#f, call.depends-on.dependent-next);
      end;

  let bogus? = #f;
  let valid? = #t;

  block (return)
    for (spec in sig.specializers,
	 arg-dep = arguments then arg-dep.dependent-next,
	 count from 0)
      unless (arg-dep)
	compiler-warning("In %s:\n  not enough arguments in call of %s.\n  "
			   "Wanted %s %d, but only got %d",
			 call.home-function-region.name,
			 func-name,
			 if (sig.rest-type | sig.key-infos)
			   "at least";
			 else
			   "exactly";
			 end,
			 sig.specializers.size,
			 count);
	bogus? := #t;
	return();
      end;
      unless (ctypes-intersect?(arg-dep.source-exp.derived-type, spec))
	compiler-warning("In %s:\n  wrong type for argument %d in call of "
			   "%s.\n  Wanted %s, but got %s",
			 call.home-function-region.name,
			 count,
			 func-name,
			 spec,
			 arg-dep.source-exp.derived-type);
	bogus? := #t;
      end;
    finally
      if (sig.key-infos)
	// Make sure all the supplied keywords are okay.
	for (key-dep = arg-dep then key-dep.dependent-next.dependent-next,
	     count from count by 2,
	     while: key-dep)
	  let val-dep = key-dep.dependent-next;
	  unless (val-dep)
	    compiler-warning("In %s:\n  odd number of keyword/value arguments "
			       "in call of %s.",
			     call.home-function-region.name,
			     func-name);
	    bogus? := #t;
	    return();
	  end;
	  let leaf = key-dep.source-exp;
	  if (~instance?(leaf, <literal-constant>))
	    unless (ctypes-intersect?(leaf.derived-type,
				      specifier-type(#"<symbol>")))
	      compiler-warning("In %s:\n  bogus keyword as argument "
				 "%d in call of %s",
			       call.home-function-region.name,
			       count,
			       func-name);
	      bogus? := #t;
	    end;
	    valid? := #f;
	  elseif (instance?(leaf.value, <literal-symbol>))
	    let key = leaf.value.literal-value;
	    block (found-key)
	      for (keyinfo in sig.key-infos)
		if (keyinfo.key-name == key)
		  unless (ctypes-intersect?(val-dep.source-exp.derived-type,
					    keyinfo.key-type))
		    compiler-warning("In %s:\n  wrong type for keyword "
				       "argument %= in call of "
				       "%s.\n  Wanted %s, but got %s",
				     call.home-function-region.name,
				     key,
				     func-name,
				     keyinfo.key-type,
				     val-dep.source-exp.derived-type);

		    bogus? := #t;
		  end;
		  found-key();
		end;
	      end;
	      unless (sig.all-keys? | call.use-generic-entry?)
		compiler-warning
		  ("In %s:\n  invalid keyword (%=) in call of %s",
		   call.home-function-region.name,
		   key,
		   func-name);
		bogus? := #t;
	      end;
	    end;
	  else
	    compiler-warning
	      ("In %s:\n  bogus keyword (%s) as argument %d in call of %s",
	       call.home-function-region.name,
	       leaf.value,
	       count,
	       func-name);
	    bogus? := #t;
	  end;
	end;
	if (valid?)
	  // Now make sure all the required keywords are supplied.
	  for (keyinfo in sig.key-infos)
	    block (found-key)
	      for (key-dep = arg-dep
		     then key-dep.dependent-next.dependent-next,
		   while: key-dep)
		if (keyinfo.key-name = key-dep.source-exp.value.literal-value)
		  found-key();
		end;
	      end;
	      if (keyinfo.required?)
		compiler-warning
		  ("In %s: required keyword %= missing in call of %s",
		   call.home-function-region.name, keyinfo.key-name,
		   func-name);
		bogus? := #t;
	      end;
	    end;
	  end;
	end;
      elseif (sig.rest-type)
	for (arg-dep = arg-dep then arg-dep.dependent-next,
	     count from count,
	     while: arg-dep)
	  unless (ctypes-intersect?(arg-dep.source-exp.derived-type,
				    sig.rest-type))
	    compiler-warning("In %s:\n  wrong type for argument %d in call of "
			       "%s.\n  Wanted %s, but got %s",
			     call.home-function-region.name,
			     count,
			     func-name,
			     sig.rest-type,
			     arg-dep.source-exp.derived-type);
	    bogus? := #t;
	  end;
	end;
      elseif (arg-dep)
	for (arg-dep = arg-dep then arg-dep.dependent-next,
	     have from count,
	     while: arg-dep)
	finally
	  compiler-warning("In %s:\n  too many arguments in call of %s.\n"
			     "  Wanted exactly %d, but got %d.",
			   call.home-function-region.name,
			   func-name,
			   count, have);
	  bogus? := #t;
	end;
      end;
    end;
  end;

  if (bogus?)
    #"bogus";
  elseif (valid?)
    #"valid";
  else
    #"can't tell";
  end;
end;


define method convert-to-known-call
    (component :: <component>, sig :: <signature>, call :: <unknown-call>)
    => ();
  let (next-method-info, arguments)
    = if (call.use-generic-entry?)
	let dep = call.depends-on.dependent-next;
	values(dep.source-exp, dep.dependent-next);
      else
	values(#f, call.depends-on.dependent-next);
      end;

  let builder = make-builder(component);
  let new-ops = make(<stretchy-vector>);
  // Add the original function to the known-call operands.
  add!(new-ops, call.depends-on.source-exp);
  // Add the fixed parameters.
  let assign = call.dependents.dependent;
  for (spec in sig.specializers,
       arg-dep = arguments then arg-dep.dependent-next)
    // Assert the argument types before adding them to the known-call
    // operands so that the known-call sees the asserted leaves.
    assert-type(component, assign, arg-dep, spec);
    add!(new-ops, arg-dep.source-exp);
  finally
    // If there is a #next parameter, add something for it.
    if (sig.next?)
      if (next-method-info)
	add!(new-ops, next-method-info);
      else
	add!(new-ops, make-literal-constant(builder, as(<ct-value>, #())));
      end;
    end;
    // Need to assert the key types before we build the #rest vector.
    if (sig.key-infos)
      for (key-dep = arg-dep then key-dep.dependent-next.dependent-next,
	   while: key-dep)
	block (next-key)
	  let key = key-dep.source-exp.value.literal-value;
	  for (keyinfo in sig.key-infos)
	    if (keyinfo.key-name == key)
	      assert-type(component, assign, key-dep.dependent-next,
			  keyinfo.key-type);
	      next-key();
	    end;
	  end;
	end;
      end;
    end;
    if (sig.rest-type | (sig.next? & sig.key-infos))
      let rest-args = make(<stretchy-vector>);
      for (arg-dep = arg-dep then arg-dep.dependent-next,
	   while: arg-dep)
	add!(rest-args, arg-dep.source-exp);
      end;
      let rest-temp = make-local-var(builder, #"rest", object-ctype());
      build-assignment
	(builder, assign.policy, assign.source-location, rest-temp,
	 make-operation(builder, <primitive>, as(<list>, rest-args),
			name: #"vector"));
      add!(new-ops, rest-temp);
    end;
    if (sig.key-infos)
      for (keyinfo in sig.key-infos)
	let key = keyinfo.key-name;
	for (key-dep = arg-dep then key-dep.dependent-next.dependent-next,
	     until: key-dep == #f
	       | key-dep.source-exp.value.literal-value == key)
	finally
	  let leaf
	    = if (key-dep)
		key-dep.dependent-next.source-exp;
	      else
		let default = keyinfo.key-default;
		if (default)
		  make-literal-constant(builder, default);
		else
		  make(<uninitialized-value>,
		       derived-type: keyinfo.key-type);
		end;
	      end;
	  add!(new-ops, leaf);
	  if (keyinfo.key-needs-supplied?-var)
	    let supplied? = as(<ct-value>, key-dep & #t);
	    add!(new-ops, make-literal-constant(builder, supplied?));
	  end;
	end;
      end;
    end;
    insert-before(component, assign, builder-result(builder));
    let new-call = make-operation(builder, <known-call>,
				  as(<list>, new-ops));
    replace-expression(component, call.dependents, new-call);
  end;
end;




define method optimize
    (component :: <component>, call :: <known-call>) => ();
  let func-dep = call.depends-on;
  unless (func-dep)
    error("No function in a call?");
  end;
  let func = func-dep.source-exp;
  block (return)
    for (transformer in find-transformers(func))
      if (transformer.transformer-function(component, call))
	return();
      end;
    end;
    // Dispatch of the thing we are calling.
    optimize-known-call(component, call, func);
  end;
end;

define method find-transformers (func :: type-union(<leaf>, <definition>))
    => res :: <list>;
  #();
end;

define method find-transformers (func :: <definition-constant-leaf>)
    => res :: <list>;
  find-transformers(func.const-defn);
end;

define method find-transformers (func :: <literal-constant>)
  let defn = func.value.ct-function-definition;
  if (defn)
    find-transformers(defn);
  else
    #();
  end;
end;

define method find-transformers (defn :: <function-definition>)
    => res :: <list>;
  defn.function-defn-transformers;
end;
  
define method find-transformers (defn :: <generic-definition>)
    => res :: <list>;
  choose(method (transformer)
	   transformer.transformer-specializers == #f;
	 end,
	 defn.function-defn-transformers);
end;


define method optimize-known-call
    (component :: <component>, call :: <known-call>,
     func :: type-union(<leaf>, <definition>))
    => ();
end;

define method optimize-known-call
    (component :: <component>, call :: <known-call>,
     func :: <function-literal>)
    => ();
  maybe-restrict-type(component, call, func.main-entry.result-type);
end;

define method optimize-known-call
    (component :: <component>, call :: <known-call>,
     func :: <literal-constant>)
    => ();
  block (return)
    let ctv = func.value;
    for (lit in component.all-function-literals)
      if (lit.ct-function == ctv)
	replace-expression(component, call.depends-on, lit);
	return();
      end;
    end;
    let defn = ctv.ct-function-definition;
    if (defn)
      optimize-known-call(component, call, defn);
    else
      maybe-restrict-type(component, call, ctv.ct-function-signature.returns);
    end;
  end;
end;

define method optimize-known-call
    (component :: <component>, call :: <known-call>,
     func :: <definition-constant-leaf>)
    => ();
  optimize-known-call(component, call, func.const-defn);
end;

define method optimize-known-call
    (component :: <component>, call :: <known-call>,
     defn :: <function-definition>)
  let sig = defn.function-defn-signature;
  maybe-restrict-type(component, call, sig.returns);
end;

define method optimize-known-call
    (component :: <component>, call :: <known-call>,
     defn :: <generic-definition>)
    => ();
  // We might be able to do a bit more method selection.

  let sig = defn.function-defn-signature;
  maybe-restrict-type(component, call, sig.returns);

  let nfixed = sig.specializers.size;
  for (i from 0 below nfixed,
       arg-leaves = #() then pair(dep.source-exp, arg-leaves),
       arg-types = #() then pair(dep.source-exp.derived-type, arg-types),
       dep = call.depends-on.dependent-next then dep.dependent-next)
  finally
    if (sig.rest-type)
      unless (dep & dep.dependent-next == #f)
	error("Strange number of arguments in a known call to a generic "
		"function?");
      end;
      let rest-var = dep.source-exp;
      unless (instance?(rest-var, <ssa-variable>))
	error("Strange leaf for rest-var in known call to a generic "
		"function");
      end;
      let rest-op = rest-var.definer.depends-on.source-exp;
      unless (instance?(rest-op, <primitive>)
		& rest-op.primitive-name == #"vector")
	error("Strange value for rest-var in known call to a generic "
		"function");
      end;
      for (dep = rest-op.depends-on then dep.dependent-next,
	   while: dep)
	arg-leaves := pair(dep.source-exp, arg-leaves);
      end;
    end;

    optimize-generic(component, call, defn, reverse!(arg-types),
		     reverse!(arg-leaves));
  end for;
end method optimize-known-call;

define method optimize-known-call
    (component :: <component>, call :: <known-call>,
     func :: <getter-method-definition>)     
    => ();
  maybe-restrict-type(component, call, func.function-defn-signature.returns);
  optimize-slot-ref(component, call, func.accessor-method-defn-slot-info,
		    listify-dependencies(call.depends-on.dependent-next));
end;

define method optimize-known-call
    (component :: <component>, call :: <known-call>,
     func :: <setter-method-definition>)     
    => ();
  maybe-restrict-type(component, call, func.function-defn-signature.returns);
  optimize-slot-set(component, call, func.accessor-method-defn-slot-info,
		    listify-dependencies(call.depends-on.dependent-next));
end;


define method listify-dependencies (dependencies :: false-or(<dependency>))
    => res :: <list>;
  for (res = #() then pair(dep.source-exp, res),
       dep = dependencies then dep.dependent-next,
       while: dep)
  finally
    reverse!(res);
  end;
end;


define method optimize-slot-ref
    (component :: <component>, call :: <abstract-call>,
     slot :: <instance-slot-info>, args :: <list>)
    => ();
  let instance = args.first;
  let offset = find-slot-offset(slot, instance.derived-type);
  if (offset)
    let builder = make-builder(component);
    let call-assign = call.dependents.dependent;
    let policy = call-assign.policy;
    let source = call-assign.source-location;
    let init?-slot = slot.slot-initialized?-slot;
    let guaranteed-initialized?
      = slot-guaranteed-initialized?(slot, instance.derived-type);
    if (init?-slot & ~guaranteed-initialized?)
      let init?-offset = find-slot-offset(init?-slot, instance.derived-type);
      unless (init?-offset)
	error("The slot is at a fixed offset, but the initialized flag "
		"isn't?");
      end;
      let temp = make-local-var(builder, #"slot-initialized?", object-ctype());
      build-assignment(builder, policy, source, temp,
		       make-operation(builder, <slot-ref>,
				      list(instance,
					   make-literal-constant
					     (builder,
					      as(<ct-value>, init?-offset))),
				      derived-type: init?-slot.slot-type,
				      slot-info: init?-slot));
      build-if-body(builder, policy, source, temp);
      build-else(builder, policy, source);
      build-assignment
	(builder, policy, source, #(),
	 make-error-operation(builder, policy, source,
			      #"uninitialized-slot-error"));
      end-body(builder);
    end;
    let value = make-local-var(builder, slot.slot-getter.variable-name,
			       slot.slot-type);
    build-assignment(builder, policy, source, value,
		     make-operation(builder, <slot-ref>,
				    pair(instance,
					 pair(make-literal-constant
						(builder,
						 as(<ct-value>, offset)),
					      args.tail)),
				    derived-type: slot.slot-type,
				    slot-info: slot));
    unless (init?-slot | guaranteed-initialized?)
      let temp = make-local-var(builder, #"slot-initialized?", object-ctype());
      build-assignment(builder, policy, source, temp,
		       make-operation(builder, <primitive>, list(value),
				      name: #"initialized?"));
      build-if-body(builder, policy, source, temp);
      build-else(builder, policy, source);
      build-assignment
	(builder, policy, source, #(),
	 make-error-operation(builder, policy, source,
			      #"uninitialized-slot-error"));
      end-body(builder);
    end;
    insert-before(component, call-assign, builder-result(builder));

    let dep = call-assign.depends-on;
    replace-expression(component, dep, value);
  end;
end;

define method optimize-slot-set
    (component :: <component>, call :: <abstract-call>,
     slot :: <instance-slot-info>, args :: <list>)
    => ();
  let instance = args.second;
  let offset = find-slot-offset(slot, instance.derived-type);
  if (offset)
    let new = args.first;
    let builder = make-builder(component);
    let call-assign = call.dependents.dependent;
    let op = make-operation(builder, <slot-set>,
			    pair(args.first,
				 pair(instance,
				      pair(make-literal-constant
					     (builder, as(<ct-value>, offset)),
					   args.tail.tail))),
			    slot-info: slot);
    build-assignment(builder, call-assign.policy, call-assign.source-location,
		     #(), op);
    begin
      let init?-slot = slot.slot-initialized?-slot;
      if (init?-slot)
	let init?-offset = find-slot-offset(init?-slot, instance.derived-type);
	unless (init?-offset)
	  error("The slot is at a fixed offset, but the initialized flag "
		  "isn't?");
	end;
	let true-leaf = make-literal-constant(builder, make(<literal-true>));
	let init-op = make-operation(builder, <slot-set>,
				     list(true-leaf, instance,
					  make-literal-constant
					    (builder,
					     as(<ct-value>, init?-offset))),
				     slot-info: init?-slot);
	build-assignment(builder, call-assign.policy,
			 call-assign.source-location, #(), init-op);
      end;
    end;
    insert-before(component, call-assign, builder-result(builder));

    let dep = call-assign.depends-on;
    replace-expression(component, dep, new);
  end;
end;



define method change-call-kind
    (component :: <component>, call :: <abstract-call>, new-kind :: <class>,
     #rest make-keyword-args, #all-keys)
    => ();
  let new = apply(make, new-kind, dependents: call.dependents,
		  depends-on: call.depends-on,
		  derived-type: call.derived-type,
		  make-keyword-args);
  for (dep = call.depends-on then dep.dependent-next,
       while: dep)
    dep.dependent := new;
  end;
  for (dep = call.dependents then dep.source-next,
       while: dep)
    dep.source-exp := new;
  end;
  reoptimize(component, new);

  call.depends-on := #f;
  call.dependents := #f;
  delete-dependent(component, call);
end;



// <error-call> optimization.
//
// We only make <error-call>s when we want to give up.
// 
define method optimize (component :: <component>, call :: <error-call>) => ();
end;


// <mv-call> optimization.
//
// If the cluster feeding the mv call has a fixed number of values, then
// convert the <mv-call> into an <unknown-call>.  Otherwise, if the called
// function is an exit function, then convert it into a pitcher.
// 
define method optimize (component :: <component>, call :: <mv-call>) => ();
  let cluster
    = if (call.use-generic-entry?)
	call.depends-on.dependent-next.dependent-next.source-exp;
      else
	call.depends-on.dependent-next.source-exp;
      end;
  if (maybe-expand-cluster(component, cluster))
    change-call-kind(component, call, <unknown-call>,
		     use-generic-entry: call.use-generic-entry?);
  else
    let func = call.depends-on.source-exp;
    if (instance?(func, <exit-function>))
      expand-exit-function(component, call, func, cluster);
    elseif (instance?(func, <literal-constant>)
	      & func.value == dylan-value(#"values"))
      replace-expression(component, call.dependents, cluster);
    end;
  end;
end;
