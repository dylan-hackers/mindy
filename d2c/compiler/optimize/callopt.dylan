module: cheese
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/optimize/callopt.dylan,v 1.9 1996/04/13 21:16:53 wlott Exp $
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
  optimize-unknown-call-leaf(component, call, func-dep.source-exp);
end;


define generic optimize-unknown-call-leaf
    (component :: <component>, call :: <unknown-call>, func :: <leaf>)
    => ();

define method optimize-unknown-call-leaf
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

define method optimize-unknown-call-leaf
    (component :: <component>, call :: <unknown-call>,
     func :: <function-literal>)
    => ();
  maybe-restrict-type(component, call, func.main-entry.result-type);
  maybe-change-to-known-or-error-call(component, call, func.signature,
				      func.main-entry.name);
end;

define method optimize-unknown-call-leaf
    (component :: <component>, call :: <unknown-call>,
     func :: <definition-constant-leaf>)
    => ();
  optimize-unknown-call-defn(component, call, func.const-defn);
end;

define method optimize-unknown-call-leaf
    (component :: <component>, call :: <unknown-call>,
     func :: <literal-constant>)
    => ();
  optimize-unknown-call-ctv(component, call, func.value);
end method optimize-unknown-call-leaf;

define method optimize-unknown-call-leaf
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
end method optimize-unknown-call-leaf;



define generic optimize-unknown-call-defn
    (component :: <component>, call :: <unknown-call>,
     defn :: <definition>)
    => ();

define method optimize-unknown-call-defn
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

define method optimize-unknown-call-defn
    (component :: <component>, call :: <unknown-call>,
     defn :: <function-definition>)
    => ();
  maybe-restrict-type(component, call, defn.function-defn-signature.returns);
end method optimize-unknown-call-defn;

define method optimize-unknown-call-defn
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
	compiler-warning-location
	  (call.dependents.dependent,
	   "Not enough arguments in call of %s.\n"
	     "  Wanted %s %d, but only got %d.",
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
	compiler-warning-location
	  (call.dependents.dependent,
	   "Wrong type for %s argument in call of %s.\n"
	     "  Wanted %s, but got %s.",
	   integer-to-english(count + 1, as: #"ordinal"),
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
	    compiler-warning-location
	      (call.dependents.dependent,
	       "Too many arguments in call of %s.\n"
		 "  Wanted exactly %d, but got %d.",
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
end method optimize-unknown-call-defn;

define method optimize-unknown-call-defn
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


define generic optimize-unknown-call-ctv
    (component :: <component>, call :: <unknown-call>, ctv :: <ct-value>)
    => ();

define method optimize-unknown-call-ctv
    (component :: <component>, call :: <unknown-call>, ctv :: <ct-value>)
    => ();
  // Assert that the function is a function.  It isn't, but this is a handy
  // way of generating an error message.
  assert-type(component, call.dependents.dependent, call.depends-on,
	      if (call.use-generic-entry?)
		specifier-type(#"<method>");
	      else
		function-ctype();
	      end);
end method optimize-unknown-call-ctv;

define method optimize-unknown-call-ctv
    (component :: <component>, call :: <unknown-call>, ctv :: <ct-function>)
    => ();
  let defn = ctv.ct-function-definition;
  if (defn)
    optimize-unknown-call-defn(component, call, defn);
  else
    assert(~instance?(ctv, <ct-generic-function>));
    let sig = ctv.ct-function-signature;
    maybe-restrict-type(component, call, sig.returns);
    maybe-change-to-known-or-error-call(component, call, sig,
					ctv.ct-function-name);
  end;
end method optimize-unknown-call-ctv;


/// Generic function optimization

define method optimize-generic
    (component :: <component>, call :: <unknown-call>,
     defn :: <generic-definition>, arg-types :: <list>,
     arg-leaves :: <list>)
    => ();
  block (return)
    if (maybe-transform-call(component, call))
      return();
    end if;
    local
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
      return();
    end if;

    let applicable = concatenate(definitely, maybe);

    if (applicable == #())
      no-applicable-methods-warning(call, defn, arg-types);
      change-to-error();
    end if;

    // Improve the result type based on the actually applicable methods.
    for (meth in applicable,
	 result-type = empty-ctype()
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
	      compiler-warning-location
		(call.dependents.dependent,
		 "Bogus keyword as %s argument in call of %s",
		 integer-to-english(index + 1, as: #"ordinal"),
		 defn.defn-name);
	      bogus? := #t;
	    end;
	  elseif (instance?(key-leaf.value, <literal-symbol>))
	    unless (valid-keys == #"all"
		      | member?(key-leaf.value.literal-value, valid-keys))
	      compiler-warning-location
		(call.dependents.dependent,
		 "Unrecognized keyword (%s) as %s argument in call of %s",
		 key-leaf.value.literal-value,
		 integer-to-english(index + 1, as: #"ordinal"),
		 defn.defn-name);
	      bogus? := #t;
	    end;
	  else
	    compiler-warning-location
	      (call.dependents.dependent,
	       "Bogus keyword (%s) as %s argument in call of %s",
	       key-leaf.value,
	       integer-to-english(index + 1, as: #"ordinal"),
	       defn.defn-name);
	    bogus? := #t;
	  end;

	  if (remaining.tail == #())
	    compiler-warning-location
	      (call.dependents.dependent,
	       "Odd number of keyword/value arguments in call of %s.",
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
      return();
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
    let new-func = build-defn-ref(builder, policy, source, ordered.head);
    let next-leaf = make-next-method-info-leaf(builder, ordered, ambiguous);
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
  compiler-warning-location
    (call.dependents.dependent,
     "Can't pick between\n%s\n  when given arguments of types:\n%s",
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
  compiler-warning-location
    (call.dependents.dependent,
     "No applicable methods for argument types\n%s\n  in call of %s",
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


// Call manipulation utilities.

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
	compiler-warning-location
	  (call.dependents.dependent,
	   "Not enough arguments in call of %s.\n"
	     "  Wanted %s %d, but only got %d",
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
	compiler-warning-location
	  (call.dependents.dependent,
	   "Wrong type for %s argument in call of %s.\n"
	     "  Wanted %s, but got %s",
	   integer-to-english(count + 1, as: #"ordinal"),
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
	    compiler-warning-location
	      (call.dependents.dependent,
	       "Odd number of keyword/value arguments in call of %s.",
	       func-name);
	    bogus? := #t;
	    return();
	  end;
	  let leaf = key-dep.source-exp;
	  if (~instance?(leaf, <literal-constant>))
	    unless (ctypes-intersect?(leaf.derived-type,
				      specifier-type(#"<symbol>")))
	      compiler-warning-location
		(call.dependents.dependent,
		 "Bogus keyword as %s argument in call of %s",
		 integer-to-english(count + 1, as: #"ordinal"),
		 func-name);
	      bogus? := #t;
	    end;
	    valid? := #f;
	  elseif (instance?(leaf.value, <literal-symbol>))
	    let key = leaf.value.literal-value;
	    let found-key? = #f;
	    for (keyinfo in sig.key-infos)
	      if (keyinfo.key-name == key)
		unless (ctypes-intersect?(val-dep.source-exp.derived-type,
					  keyinfo.key-type))
		  compiler-warning-location
		    (call.dependents.dependent,
		     "Wrong type for keyword argument %s in call of %s.\n"
		       "  Wanted %s, but got %s",
		     key,
		     func-name,
		     keyinfo.key-type,
		     val-dep.source-exp.derived-type);
		  bogus? := #t;
		end;
		found-key? := #t;
	      end if;
	    end for;
	    unless (found-key? | sig.all-keys? | call.use-generic-entry?)
	      compiler-warning-location
		(call.dependents.dependent,
		 "Unrecognized keyword (%s) as %s argument in call of %s",
		 key,
		 integer-to-english(count + 1, as: #"ordinal"),
		 func-name);
	      bogus? := #t;
	    end unless;
	  else
	    compiler-warning-location
	      (call.dependents.dependent,
	       "Bogus keyword (%s) as %s argument in call of %s",
	       leaf.value,
	       integer-to-english(count + 1, as: #"ordinal"),
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
		compiler-warning-location
		  (call.dependents.dependent,
		   "Required keyword %s missing in call of %s",
		   keyinfo.key-name,
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
	    compiler-warning-location
	      (call.dependents.dependent,
	       "Wrong type for %s argument in call of %s.\n"
		 "  Wanted %s, but got %s",
	       integer-to-english(count + 1, as: #"ordinal"),
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
	  compiler-warning-location
	    (call.dependents.dependent,
	     "Too many arguments in call of %s.\n"
	       "  Wanted exactly %d, but got %d.",
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
	let key = key-dep.source-exp.value.literal-value;
	for (keyinfo in sig.key-infos)
	  if (keyinfo.key-name == key)
	    assert-type(component, assign, key-dep.dependent-next,
			keyinfo.key-type);
	  end if;
	end for;
      end for;
    end if;
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

define method listify-dependencies (dependencies :: false-or(<dependency>))
    => res :: <list>;
  for (res = #() then pair(dep.source-exp, res),
       dep = dependencies then dep.dependent-next,
       while: dep)
  finally
    reverse!(res);
  end;
end;


// Known call optimization.

define method optimize
    (component :: <component>, call :: <known-call>) => ();
  maybe-transform-call(component, call)
    | optimize-known-call-leaf(component, call, call.depends-on.source-exp);
end;


define generic optimize-known-call-leaf
    (component :: <component>, call :: <known-call>, func :: <leaf>)
    => ();


define method optimize-known-call-leaf
    (component :: <component>, call :: <known-call>, func :: <leaf>)
    => ();
end;

define method optimize-known-call-leaf
    (component :: <component>, call :: <known-call>,
     func :: <function-literal>)
    => ();
  maybe-restrict-type(component, call, func.main-entry.result-type);
end;

define method optimize-known-call-leaf
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
      optimize-known-call-defn(component, call, defn);
    else
      maybe-restrict-type(component, call, ctv.ct-function-signature.returns);
    end;
  end;
end;

define method optimize-known-call-leaf
    (component :: <component>, call :: <known-call>,
     func :: <definition-constant-leaf>)
    => ();
  optimize-known-call-defn(component, call, func.const-defn);
end;

define generic optimize-known-call-defn
    (component :: <component>, call :: <known-call>, func :: <definition>)
    => ();


define method optimize-known-call-defn
    (component :: <component>, call :: <known-call>, func :: <definition>)
    => ();
end;

define method optimize-known-call-defn
    (component :: <component>, call :: <known-call>,
     defn :: <function-definition>)
    => ();
  let sig = defn.function-defn-signature;
  maybe-restrict-type(component, call, sig.returns);
end;

define method optimize-known-call-defn
    (component :: <component>, call :: <known-call>,
     defn :: <generic-definition>)
    => ();
  // We might be able to do a bit more method selection.
  error("known call to a generic function?");
end method optimize-known-call-defn;

define method optimize-known-call-defn
    (component :: <component>, call :: <known-call>,
     func :: <getter-method-definition>)     
    => ();
  maybe-restrict-type(component, call, func.function-defn-signature.returns);
  optimize-slot-ref(component, call, func.accessor-method-defn-slot-info,
		    listify-dependencies(call.depends-on.dependent-next));
end;

define method optimize-known-call-defn
    (component :: <component>, call :: <known-call>,
     func :: <setter-method-definition>)     
    => ();
  maybe-restrict-type(component, call, func.function-defn-signature.returns);
  optimize-slot-set(component, call, func.accessor-method-defn-slot-info,
		    listify-dependencies(call.depends-on.dependent-next));
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
      if (init?-offset == #"data-word")
	error("The init? slot is in the data-word?");
      end if;
      let temp = make-local-var(builder, #"slot-initialized?", object-ctype());
      build-assignment(builder, policy, source, temp,
		       make-operation(builder, <heap-slot-ref>,
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
    build-assignment
      (builder, policy, source, value,
       if (offset == #"data-word")
	 make-operation
	   (builder, <data-word-ref>, list(instance),
	    derived-type: slot.slot-type, slot-info: slot);
       else
	 make-operation
	   (builder, <heap-slot-ref>,
	    pair(instance,
		 pair(make-literal-constant(builder, as(<ct-value>, offset)),
		      args.tail)),
	    derived-type: slot.slot-type,
	    slot-info: slot);
       end);
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
    build-assignment
      (builder, call-assign.policy, call-assign.source-location, #(),
       make-operation
	 (builder, <heap-slot-set>,
	  pair(new,
	       pair(instance,
		    pair(make-literal-constant
			   (builder, as(<ct-value>, offset)),
			 args.tail.tail))),
	  slot-info: slot));
    begin
      let init?-slot = slot.slot-initialized?-slot;
      if (init?-slot)
	let init?-offset = find-slot-offset(init?-slot, instance.derived-type);
	unless (init?-offset)
	  error("The slot is at a fixed offset, but the initialized flag "
		  "isn't?");
	end;
	let true-leaf = make-literal-constant(builder, make(<literal-true>));
	let init-op = make-operation(builder, <heap-slot-set>,
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
