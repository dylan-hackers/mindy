module: cheese
rcs-header: $Header: /scm/cvs/src/d2c/compiler/optimize/callopt.dylan,v 1.12 2004/04/09 15:28:21 andreas Exp $
copyright: see below

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000, 2001  Gwydion Dylan Maintainers
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

// Call optimization.


// General (unknown & mv) call optimization.

// optimize{<unknown-call>}
//
// Dispatch off the kind of function.
// 
define method optimize
    (component :: <component>, call :: <unknown-call>) => ();
  optimize-general-call-leaf(component, call, call.depends-on.source-exp);
end;

// optimize{<mv-call>}
//
// If the cluster feeding the mv call has a fixed number of values, then
// convert the <mv-call> into an <unknown-call>.  Otherwise, dispatch of
// the kind of function.
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
    optimize-general-call-leaf(component, call, call.depends-on.source-exp);
  end if;
end method optimize;


// optimize-general-call-leaf, -defn, and -ctv -- internal.
//
// Improve the call in various ways depending on the kind of function
// being called.  There are three different generic functions to keep
// the number of methods to select between down.
//
define generic optimize-general-call-leaf
    (component :: <component>, call :: <general-call>, func :: <leaf>)
    => ();
//
define generic optimize-general-call-defn
    (component :: <component>, call :: <general-call>, defn :: <definition>)
    => ();
//
define generic optimize-general-call-ctv
    (component :: <component>, call :: <general-call>, ctv :: <ct-value>)
    => ();


// optimize-general-call-leaf{<leaf>}
//
// Just add a type assertion that the function is really a function.
// 
define method optimize-general-call-leaf
    (component :: <component>, call :: <general-call>, func :: <leaf>)
    => ();
  assert-function-type(component, call);
end method optimize-general-call-leaf;

// optimize-general-call-leaf{<ssa-variable>}
//
// If the function is an ssa-variable, check it see if the definition of the
// ssa-variable is a use of the make-next-method primitive.  If so, then
// change this call into code to invoke the next method.
// 
define method optimize-general-call-leaf
    (component :: <component>, call :: <general-call>, func :: <ssa-variable>)
    => ();
  let func = func.definer.depends-on.source-exp;
  if (instance?(func, <primitive>)
	& func.primitive-name == #"make-next-method")
    assert(~call.use-generic-entry?);
    expand-next-method-call-ref
      (component, call.dependents,
       select (call by instance?)
	 <unknown-call> =>
	   listify-dependencies(call.depends-on.dependent-next);
	 <mv-call> =>
	   call.depends-on.dependent-next.source-exp;
       end select,
       func);
  else
    // Assert that the function is a function.
    assert-function-type(component, call);
  end;
end method optimize-general-call-leaf;

// optimize-general-call-leaf{<function-literal>}
//
// Compare the call against function signature and change to a known call if
// possible and an error call if we can tell that it is invalid.
// 
define method optimize-general-call-leaf
    (component :: <component>, call :: <general-call>,
     func :: <function-literal>)
    => ();
  maybe-restrict-type(component, call, func.main-entry.result-type);
  maybe-change-to-known-or-error-call
    (component, call, func.signature, func.main-entry.name, #f, #f);
end;

// optimize-general-call-leaf{<definition-constant-leaf>}
//
// Dispatch off the kind of definition.
// 
define method optimize-general-call-leaf
    (component :: <component>, call :: <general-call>,
     func :: <definition-constant-leaf>)
    => ();
  optimize-general-call-defn(component, call, func.const-defn);
end;

// optimize-general-call-leaf{<unknown-call>,<literal-constant>}
//
// Dispatch off the kind of ctv.
// 
define method optimize-general-call-leaf
    (component :: <component>, call :: <unknown-call>,
     func :: <literal-constant>)
    => ();
  optimize-general-call-ctv(component, call, func.value);
end method optimize-general-call-leaf;

// optimize-general-call-leaf{<mv-call>,<literal-constant>}
//
// If we are mv-calling values, just return the cluster.  This saves the
// optimizer the work of inlining values and then eventually reducing it to
// just the argument cluster.  Otherwise, just dispatch off the kind of
// ctv.
// 
define method optimize-general-call-leaf
    (component :: <component>, call :: <mv-call>, func :: <literal-constant>)
    => ();
  if (func.value == dylan-value(#"values"))
    assert(~call.use-generic-entry?);
    replace-expression
      (component, call.dependents, call.depends-on.dependent-next.source-exp);
  else
    optimize-general-call-ctv(component, call, func.value);
  end if;
end method optimize-general-call-leaf;

// optimize-general-call-leaf{<exit-function>}
//
// Build a cluster out of the arguments (unless they already are a cluster,
// of course) and replace the call with a throw.
// 
define method optimize-general-call-leaf
    (component :: <component>, call :: <general-call>, func :: <exit-function>)
    => ();
  if (call.use-generic-entry?)
    error("Trying to call the generic entry for an exit function?");
  end;

  let builder = make-builder(component);

  let cluster
    = select (call by instance?)
	<unknown-call> =>
	  let assign = call.dependents.dependent;

	  let cluster = make-values-cluster(builder, #"cluster", wild-ctype());
	  build-assignment
	    (builder, assign.policy, assign.source-location, cluster,
	     make-operation
	       (builder, <primitive>,
		listify-dependencies(call.depends-on.dependent-next),
		name: #"values"));
	  insert-before(component, assign, builder-result(builder));
	  cluster;

	<mv-call> =>
	  call.depends-on.dependent-next.source-exp;
      end select;
  
  replace-expression
    (component, call.dependents,
     make-operation
       (builder, <throw>, list(func.depends-on.source-exp, cluster),
	nlx-info: func.nlx-info));
end method optimize-general-call-leaf;

// optimize-general-call-defn{<abstract-constant-definition>}
//
// Assert that the constant is a function.
// 
define method optimize-general-call-defn
    (component :: <component>, call :: <general-call>,
     defn :: <abstract-constant-definition>)
    => ();
  assert-function-type(component, call);
end method optimize-general-call-defn;

// optimize-general-call-defn{<function-definition>}
//
// Propagate the result type.
// 
define method optimize-general-call-defn
    (component :: <component>, call :: <general-call>,
     defn :: <function-definition>)
    => ();
  maybe-restrict-type
    (component, call, defn.function-defn-signature.returns.ctype-extent);
end method optimize-general-call-defn;

// optimize-general-call-defn{<generic-definition>}
//
// Propagate the result type and then defer to optimize-generic to do the
// rest of the optimization.
// 
define method optimize-general-call-defn
    (component :: <component>, call :: <general-call>,
     defn :: <generic-definition>)
    => ();
  optimize-generic(component, call, defn);
end method optimize-general-call-defn;

// optimize-general-call-defn{<abstract-method-definition>}
//
// Change the call to a known or error call based on the argument types.
//
define method optimize-general-call-defn
    (component :: <component>, call :: <general-call>,
     defn :: <abstract-method-definition>)
    => ();
  let sig = defn.function-defn-signature;
  maybe-restrict-type(component, call, sig.returns.ctype-extent);
  maybe-change-to-known-or-error-call
    (component, call, sig, defn.defn-name, defn.inlining-candidate?,
     defn.function-defn-hairy?);
end method optimize-general-call-defn;

// optimize-general-call-ctv{<ct-value>}
//
// Assert that the function is a function.  It isn't, but this is a handy
// way of generating an error message.
//
define method optimize-general-call-ctv
    (component :: <component>, call :: <general-call>, ctv :: <ct-value>)
    => ();
  assert-function-type(component, call);
end method optimize-general-call-ctv;

// optimize-general-call-ctv{<ct-function>}
//
// If there is a definition that corresponds to this generic function, dispatch
// off of it.  Otherwise, try changing into a known or error call.
//
define method optimize-general-call-ctv
    (component :: <component>, call :: <general-call>, ctv :: <ct-function>)
    => ();
  let defn = ctv.ct-function-definition;
  if (defn)
    optimize-general-call-defn(component, call, defn);
  else
    assert(~instance?(ctv, <ct-generic-function>));
    let sig = ctv.ct-function-signature;
    maybe-restrict-type(component, call, sig.returns.ctype-extent);
    maybe-change-to-known-or-error-call
      (component, call, sig, ctv.ct-function-name, #f, #f);
  end;
end method optimize-general-call-ctv;


/// Generic function optimization

// optimize-generic -- internal.
//
// Called by optimize-general-call-defn when given a <generic-definition>.
//
define method optimize-generic
    (component :: <component>, call :: <general-call>,
     defn :: <generic-definition>)
    => ();
  if (call.use-generic-entry?)
    error("Trying to pass a generic function next-method information?");
  end;

  block (return)
    let sig = defn.function-defn-signature;

    let call-is
      = compare-call-against-signature(call, sig, #f, defn.defn-name);

    if (call-is == #"bogus")
      change-call-kind(component, call, <error-call>);
      return();
    end if;

    maybe-restrict-type(component, call, sig.returns.ctype-extent);

    if (call-is == #"valid" & instance?(call, <unknown-call>)
	  & maybe-transform-call(component, call))
      return();
    end if;

    let positional-arg-types
      = extract-positional-arg-types(call, sig.specializers.size);
    let (definitely, maybe)
      = ct-applicable-methods(defn, positional-arg-types);
    if (definitely == #f)
      return();
    end if;

    // If there are no applicable methods, then puke.
    let applicable = concatenate(definitely, maybe);
    if (applicable == #())
      no-applicable-methods-warning(call, defn, positional-arg-types);
      change-call-kind(component, call, <error-call>);
      return();
    end if;

    // Improve the result type based on the actually applicable methods.
    for (meth in applicable,
	 result-type = empty-ctype()
	   then values-type-union(result-type,
				  meth.function-defn-signature.returns))
    finally
      maybe-restrict-type(component, call, result-type.ctype-extent);
    end for;

    // Blow out of here if we can't tell exactly what methods are (or can
    // be) applicable.
    unless (maybe == #() | (maybe.tail == #() & definitely == #()))
      return();
    end unless;

    // Sort the applicable methods.
    let (ordered, ambiguous) = sort-methods(applicable, #f);
    if (ordered)
      if (ordered == #())
	ambiguous-method-warning(call, defn, ambiguous, positional-arg-types);
	change-call-kind(component, call, <error-call>);
	return();
      else
	maybe-restrict-type
	  (component, call,
	   ordered.head.function-defn-signature.returns.ctype-extent);
      end if;
    end if;

    // What we can do from here on depends on whether we are a mv-call or an
    // unknown-call.

    select (call by instance?)
      <mv-call> =>
	
	// The function takes keyword arguments, we have no way to validate
	// them.  And if ordered is #f, we can't tell what method to call.
	// Either way, we can't select a method.
	unless (sig.key-infos | ordered == #f)

	  // Change to an mv-call of the most specific method.
	  let builder = make-builder(component);
	  let assign = call.dependents.dependent;
	  let policy = assign.policy;
	  let source = assign.source-location;
	  let new-func = build-defn-ref(builder, policy, source, ordered.head);
	  let next-leaf
	    = make-next-method-info-leaf(builder, ordered, ambiguous);
	  insert-before(component, assign, builder-result(builder));
	  let cluster = call.depends-on.dependent-next.source-exp;
	  let new-call = make-operation(builder, <mv-call>,
					list(new-func, next-leaf, cluster),
					use-generic-entry: #t);
	  replace-expression(component, call.dependents, new-call);
	end unless;

      <unknown-call> =>

	// If the function takes keywords, we can only select the method if
	// we can tell that all the keywords are in fact keywords.
	if (sig.key-infos)

	  // First, compute the set of recognized keywords.
	  let recognized
	    = if (sig.all-keys?)
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
	  
	  // Now check each keyword/value argument.
	  let bogus? = #f;
	  let cant-tell? = #f;
	  for (dep = call.depends-on.dependent-next then dep.dependent-next,
	       index from 0 below sig.specializers.size)
	  finally
	    for (dep = dep then dep.dependent-next.dependent-next,
		 index from index by 2,
		 while: dep)
	      let key-leaf = dep.source-exp;

	      if (instance?(key-leaf, <literal-constant>))
		let ctv = key-leaf.value;
		assert(instance?(ctv, <literal-symbol>));
		unless (recognized == #"all"
			  | member?(ctv.literal-value, recognized))
		  compiler-error-location
		    (call.dependents.dependent,
		     "Unrecognized keyword (%s) as the %s argument"
		       " in call of %s",
		     key-leaf.value.literal-value,
		     integer-to-english(index + 1, as: #"ordinal"),
		     defn.defn-name);
		  bogus? := #t;
		end unless;
	      else
		cant-tell? := #t;
	      end if;
	    end for;
	  end for;
	  if (bogus?)
	    change-call-kind(component, call, <error-call>);
	    return();
	  elseif (cant-tell?)
	    return();
	  end if;
	end if;
	
      // Change to an unknown call of the most specific method if we know
      // what that method is.
      if (ordered)
	let builder = make-builder(component);
	let assign = call.dependents.dependent;
	let policy = assign.policy;
	let source = assign.source-location;
	let new-func = build-defn-ref(builder, policy, source, ordered.head);
	let next-leaf
	  = make-next-method-info-leaf(builder, ordered, ambiguous);
	insert-before(component, assign, builder-result(builder));
	let new-call
	  = (make-unknown-call
	       (builder, new-func, next-leaf,
		listify-dependencies(call.depends-on.dependent-next)));
	replace-expression(component, call.dependents, new-call);
      end if;
    end select;
  end block;
end method optimize-generic;


define method extract-positional-arg-types
    (call :: <unknown-call>, count :: <integer>) => types :: <list>;
  for (dep = call.depends-on.dependent-next then dep.dependent-next,
       index from 0 below count,
       types = #() then pair(dep.source-exp.derived-type, types))
  finally
    reverse!(types);
  end for;
end method extract-positional-arg-types;

define method extract-positional-arg-types
    (call :: <mv-call>, count :: <integer>) => types :: <list>;
  let cluster-type = call.depends-on.dependent-next.source-exp.derived-type;
  let types = cluster-type.positional-types;
  if (types.size < count)
    concatenate(types,
		make(<list>, size: count - types.size,
		     fill: cluster-type.rest-value-type));
  else
    copy-sequence(types, end: count);
  end if;
end method extract-positional-arg-types;
    


define method ambiguous-method-warning
    (call :: <abstract-call>, defn :: <generic-definition>,
     ambiguous :: <list>, arg-types :: <list>)
    => ();
  let stream = make(<buffered-byte-string-output-stream>);
  write(stream, "    (");
  for (arg-type in arg-types, first? = #t then #f)
    unless (first?)
      write(stream, ", ");
    end;
    print-message(arg-type, stream);
  end;
  write-element(stream, ')');
  let arg-types-string = stream.stream-contents;
  for (meth in ambiguous)
    format(stream, "    %s\n", meth.defn-name);
  end;
  compiler-error-location
    (call.dependents.dependent,
     "Can't order\n%s\n  when given positional arguments of types:\n%s",
     stream.stream-contents,
     arg-types-string);
end;

define method no-applicable-methods-warning
    (call :: <abstract-call>, defn :: <generic-definition>,
     arg-types :: <list>)
    => ();
  let stream = make(<buffered-byte-string-output-stream>);
  write(stream, "    (");
  for (arg-type in arg-types, first? = #t then #f)
    unless (first?)
      write(stream, ", ");
    end;
    print-message(arg-type, stream);
  end;
  write-element(stream, ')');
  let arg-types-string = stream.stream-contents;
  compiler-error-location
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

define method assert-function-type
    (component :: <component>, call :: <general-call>) => ();
  //
  // Assert that the function is a function.
  assert-type(component, call.dependents.dependent, call.depends-on,
	      if (call.use-generic-entry?)
		specifier-type(#"<method>");
	      else
		function-ctype();
	      end);
end method assert-function-type;

define function relocator(call :: <general-call>)
 => relocator :: <function>;
  // change to the data-flow level
  // there must be at least one dependent (the assignment)
  let dep :: <dependency> = call.dependents;
  // expect no more dependents
  let no-other-dep :: #f.singleton = dep.dependent-next;
  // expect it to be an assignment
  let assign :: <abstract-assignment> = dep.dependent;
  assign.source-location.always
end;

define method maybe-change-to-known-or-error-call
    (component :: <component>, call :: <general-call>, sig :: <signature>,
     func-name :: <name>,
     inline-function :: false-or(<function-literal>),
     hairy? :: <boolean>)
    => ();
  select (compare-call-against-signature(call, sig, #t, func-name))
    #"bogus" =>
      if (call.use-generic-entry?)
	error("bogus call w/ next?");
      end;
      change-call-kind(component, call, <error-call>);

    #"valid" =>
      if (inline-function)
	let old-head = component.reoptimize-queue;
	let new-func = clone-function(component, inline-function, call.relocator);
	reverse-queue(component, old-head);
	replace-expression(component, call.depends-on, new-func);
      elseif (~hairy?)
	convert-to-known-call(component, sig, call);
      end;

    #"can't tell" =>
      #f;
  end;
end;

// XXX not really enough information available here to make a good decision.
// Need to analyse the size of the function to be
// inlined in case it is {may,default}-inline
//
define method inlining-candidate?
    (defn :: <abstract-method-definition>)
 => potential-inline-function :: false-or(<function-literal>);
  select (defn.method-defn-inline-type)
    #"not-inline" =>
      #f;
    #"default-inline" =>
      #f;
    #"may-inline" =>
      #f;
    #"inline", #"inline-only" =>
      defn.method-defn-inline-function;
  end select;
end method inlining-candidate?;


define generic compare-call-against-signature
    (call :: <general-call>, sig :: <signature>, check-keywords? :: <boolean>,
     func-name :: <name>)
    => res :: one-of(#"bogus", #"valid", #"can't tell");  

define method compare-call-against-signature
    (call :: <unknown-call>, sig :: <signature>, check-keywords? :: <boolean>,
     func-name :: <name>)
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
	compiler-error-location
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
	compiler-error-location
	  (call.dependents.dependent,
	   "Wrong type for the %s argument in call of %s.\n"
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
	    compiler-error-location
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
	      compiler-error-location
		(call.dependents.dependent,
		 "Bogus keyword as the %s argument in call of %s",
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
		  compiler-error-location
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
	    unless (found-key? | sig.all-keys? | call.use-generic-entry?
		      | ~check-keywords?)
	      compiler-error-location
		(call.dependents.dependent,
		 "Unrecognized keyword (%s) as the %s argument in call of %s",
		 key,
		 integer-to-english(count + 1, as: #"ordinal"),
		 func-name);
	      bogus? := #t;
	    end unless;
	  else
	    compiler-error-location
	      (call.dependents.dependent,
	       "Bogus keyword (%s) as the %s argument in call of %s",
	       leaf.value,
	       integer-to-english(count + 1, as: #"ordinal"),
	       func-name);
	    bogus? := #t;
	  end;
	end;
	if (valid? & check-keywords?)
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
		compiler-error-location
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
	    compiler-error-location
	      (call.dependents.dependent,
	       "Wrong type for the %s argument in call of %s.\n"
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
	  compiler-error-location
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
end method compare-call-against-signature;

define method compare-call-against-signature
    (call :: <mv-call>, sig :: <signature>, check-keywords? :: <boolean>,
     func-name :: <name>)
    => res :: one-of(#"bogus", #"valid", #"can't tell");

  block (return)

    let (next-method-info, cluster)
      = if (call.use-generic-entry?)
	  let dep = call.depends-on.dependent-next;
	  values(dep.source-exp, dep.dependent-next.source-exp);
	else
	  values(#f, call.depends-on.dependent-next.source-exp);
	end;
    let cluster-type = cluster.derived-type;

    let bogus? = #f;
    for (gf-spec in sig.specializers,
	 index from 0)
      let arg-type = element(cluster-type.positional-types, index,
			     default: cluster-type.rest-value-type);
      if (arg-type == empty-ctype())
	// Can't have enough arguments.
	compiler-error-location
	  (call.dependents.dependent,
	   "Not enough arguments in call of %s.\n"
	     "  Wanted %s %d, but got no more than %d.",
	   func-name,
	   if (sig.rest-type | sig.key-infos)
	     "at least";
	   else
	     "exactly";
	   end,
	   sig.specializers.size,
	   index);
	return(#"bogus");
      elseif (~ctypes-intersect?(arg-type, gf-spec))
	compiler-error-location
	  (call.dependents.dependent,
	   "Wrong type for %s argument in call of %s.\n"
	     "  Wanted %s, but got %s.",
	   integer-to-english(index + 1, as: #"ordinal"),
	   func-name,
	   gf-spec,
	   arg-type);
	bogus? := #t;
      end;
    end for;

    if (sig.key-infos & (check-keywords? | ~call.use-generic-entry?))
      //
      // The function takes keyword arguments and we have to either check
      // or validate them.  But we have no way of doing that at compile
      // time as long as the call remains an mv-call.
      if (bogus?)
	return(#"bogus");
      else
	return(#"can't tell");
      end if;
    end if;
    
    if (sig.rest-type)
      //
      // The function takes a variable number of arguments.  Make sure all
      // the arguments past the positionals are of the rest-type.
      unless (sig.rest-type == object-ctype())
	for (index from sig.specializers.size
	       below cluster-type.positional-types.size)
	  let arg-type = cluster-type.positional-types[index];
	  unless (ctypes-intersect?(arg-type, sig.rest-type))
	    compiler-error-location
	      (call.dependents.dependent,
	       "Wrong type for %s argument in call of %s.\n"
		 "  Wanted %s, but got %s.",
	       integer-to-english(index + 1, as: #"ordinal"),
	       func-name,
	       sig.rest-type,
	       arg-type);
	    bogus? := #t;
	  end unless;
	end for;
	unless (ctypes-intersect?(cluster-type.rest-value-type, sig.rest-type))
	  compiler-error-location
	    (call.dependents.dependent,
	     "Wrong type for the %s argument and on in call of %s.\n"
	       "  Wanted %s, but got %s.",
	     integer-to-english
	       (cluster-type.positional-types.size + 1, as: #"ordinal"),
	     func-name,
	     sig.rest-type,
	     cluster-type.rest-value-type);
	  bogus? := #t;
	end unless;
      end unless;

      if (bogus?)
	#"bogus";
      elseif (cluster-type.min-values < sig.specializers.size)
	#"can't tell";
      else
	#"valid";
      end if;
	
    else
      //
      // There are a fixed number of values.  Make sure we don't have too
      // many.
      if (cluster-type.min-values > sig.specializers.size)
	compiler-error-location
	  (call.dependents.dependent,
	   "Too many arguments in call of %s.\n"
	     "  Wanted exactly %d, but got at least %d.",
	   func-name,
	   sig.specializers.size,
	   cluster-type.min-values);
	#"bogus";
      elseif (bogus?)
	#"bogus";
      else
	//
	// The call can't be guaranteed valid, because for it to be valid it
	// would have to have exactly the correct number of arguments.  But
	// if it had exactly the correct number of arguments, the stuff to
	// spread the cluster out into a fixed number of single-value variables
	// would have picked it off.
	#"can't tell";
      end if;
    end if;
  end block;
end method compare-call-against-signature;



// convert-to-known-call -- internal.
//
// Convert the call to a known call.  The call is known to be valid.
// 
define generic convert-to-known-call
    (component :: <component>, sig :: <signature>, call :: <general-call>)
    => ();

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
	add!(new-ops, make-literal-constant(builder, #()));
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
		       derived-type: keyinfo.key-type.ctype-extent);
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

define method convert-to-known-call
    (component :: <component>, sig :: <signature>, call :: <mv-call>)
    => ();
  let (next-method-info, cluster)
    = if (call.use-generic-entry?)
	let dep = call.depends-on.dependent-next;
	values(dep.source-exp, dep.dependent-next.source-exp);
      else
	values(#f, call.depends-on.dependent-next.source-exp);
      end;

  let assign = call.dependents.dependent;
  let policy = assign.policy;
  let source = assign.source-location;

  let builder = make-builder(component);
  

  let vars = make(<stretchy-vector>);
  let args = make(<stretchy-vector>);

  // Add the original function to the arguments.
  add!(args, call.depends-on.source-exp);

  // Add variables for all the fixed arguments.
  for (spec in sig.specializers)
    let var = make-local-var(builder, #"temp", spec);
    add!(vars, var);
    add!(args, var);
  end for;

  // If there is a #next parameter, add something for it.
  if (sig.next?)
    if (next-method-info)
      add!(args, next-method-info);
    else
      add!(args, make-literal-constant(builder, #()));
    end;
  end;

  // There must be a rest var for us to be making a known call out of a
  // mv-call.  Not only that, but it better be <object> because we can't check
  // the types of the rest arguments.
  assert(sig.rest-type == object-ctype());

  // Add a variable for the rest.
  let var = make-local-var(builder, #"temp", object-ctype());
  add!(vars, var);
  add!(args, var);

  // There can't be any keyword arguments.
  assert(sig.key-infos == #f | sig.key-infos.empty?);

  // Use canonicalize-results to split the cluster up.
  build-assignment
    (builder, policy, source, as(<list>, vars),
     make-operation
       (builder, <primitive>,
	list(cluster,
	     make-literal-constant
	       (builder, sig.specializers.size)),
	name: #"canonicalize-results"));

  // Insert the assignment.
  insert-before(component, assign, builder-result(builder));

  // Replace the call.
  replace-expression
    (component, call.dependents,
     make-operation(builder, <known-call>, as(<list>, args)));
end method convert-to-known-call;
    


define method change-call-kind
    (component :: <component>, call :: <abstract-call>, new-kind :: <class>,
     #rest make-keyword-args, #key, #all-keys)
    => ();
  let new = apply(make, new-kind, dependents: call.dependents,
		  depends-on: call.depends-on,
		  derived-type: call.derived-type,
                  ct-source-location: call.ct-source-location,
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
      maybe-restrict-type
	(component, call, ctv.ct-function-signature.returns.ctype-extent);
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
  maybe-restrict-type(component, call, sig.returns.ctype-extent);
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
  maybe-restrict-type
    (component, call, func.function-defn-signature.returns.ctype-extent);
  optimize-slot-ref(component, call, func.accessor-method-defn-slot-info,
		    listify-dependencies(call.depends-on.dependent-next));
end;

define method optimize-known-call-defn
    (component :: <component>, call :: <known-call>,
     func :: <setter-method-definition>)     
    => ();
  maybe-restrict-type
    (component, call, func.function-defn-signature.returns.ctype-extent);
  optimize-slot-set(component, call, func.accessor-method-defn-slot-info,
		    listify-dependencies(call.depends-on.dependent-next));
end;

define method optimize-slot-ref
    (component :: <component>, call :: <abstract-call>,
     class-slot :: <class-slot-info>, args :: <list>)
    => ();
  let orig-instance = args.first;
  let meta-slot = class-slot.associated-meta-slot;
  let meta-instance = meta-slot.slot-introduced-by;
  let offset = find-slot-offset(meta-slot, meta-instance /* instance.derived-type */);
  if (offset)
    let builder = make-builder(component);
    let call-assign = call.dependents.dependent;
    let policy = call-assign.policy;
    let source = call-assign.source-location;
    let init?-slot = meta-slot.slot-initialized?-slot;
    let getter-name = class-slot.slot-getter.variable-name;
    let slot-home
      = build-slot-home(getter-name,
			make-literal-constant(builder, class-slot.slot-introduced-by),
			builder, policy, source);
    let guaranteed-initialized?	// remove this altogether???
      = slot-guaranteed-initialized?(meta-slot, meta-instance /* instance.derived-type */);

    guaranteed-initialized?
      & error("A class slot that is guaranteed initialized?");

    if (init?-slot & ~guaranteed-initialized?)
      let init?-offset = find-slot-offset(init?-slot, meta-instance /* instance.derived-type */);
      unless (init?-offset)
	error("The slot is at a fixed offset, but the initialized flag "
		"isn't?");
      end;
      if (init?-offset == #"data-word")
	error("The init? slot is in the data-word?");
      end if;
      let temp = make-local-var(builder, #"slot-initialized?", specifier-type(#"<boolean>"));
      build-assignment
	(builder, policy, source, temp,
	 make-operation
	   (builder, <heap-slot-ref>,
	    list(slot-home,
		 make-literal-constant(builder, init?-offset)),
	    derived-type: init?-slot.slot-type.ctype-extent,
	    slot-info: init?-slot));
      build-if-body(builder, policy, source, temp);
      build-else(builder, policy, source);
      build-assignment
	(builder, policy, source, #(),
	 make-error-operation
	   (builder, policy, source, #"uninitialized-slot-error-with-location",
	    make-literal-constant(builder, class-slot), 
            orig-instance,
            make-literal-constant(builder, format-to-string("%=", source))));
      end-body(builder);
    end;
    let value = make-local-var(builder, getter-name,
			       meta-slot.slot-type);
    build-assignment
      (builder, policy, source, value,
       if (offset == #"data-word")
	 error("The class slot is in the data-word?");
       else
	 make-operation
	   (builder, <heap-slot-ref>,
	    pair(slot-home,
		 pair(make-literal-constant(builder, offset),
		      args.tail)),
	    derived-type: meta-slot.slot-type.ctype-extent,
	    slot-info: meta-slot);
       end);
    unless (init?-slot | guaranteed-initialized?)
      let temp = make-local-var(builder, #"slot-initialized?", specifier-type(#"<boolean>") /* object-ctype() #elsewhere too!!!##*/);
      build-assignment(builder, policy, source, temp,
		       make-operation(builder, <primitive>, list(value),
				      name: #"initialized?"));
      build-if-body(builder, policy, source, temp);
      build-else(builder, policy, source);
      build-assignment
	(builder, policy, source, #(),
	 make-error-operation
	   (builder, policy, source, #"uninitialized-slot-error-with-location",
	    make-literal-constant(builder, class-slot), 
            orig-instance,
            make-literal-constant(builder, format-to-string("%=", source))));
      end-body(builder);
    end;
    insert-before(component, call-assign, builder-result(builder));

    let dep = call-assign.depends-on;
    replace-expression(component, dep, value);
  end;
end;

define method optimize-slot-ref
    (component :: <component>, call :: <abstract-call>,
     slot :: <each-subclass-slot-info>, args :: <list>)
    => ();
// TBI###
end;

define method optimize-slot-ref
    (component :: <component>, call :: <abstract-call>,
     slot :: <instance-slot-info>, args :: <list>)
    => ();
  let instance = args.first;
  let offset = find-slot-offset(slot, instance.derived-type);

  if (instance?(instance, <literal-constant>)
        & slot.slot-getter
        & ct-value-slot(instance.value, slot.slot-getter.variable-name))
    let builder = make-builder(component);
    let result = ct-value-slot(instance.value, slot.slot-getter.variable-name);
    replace-expression(component, call.dependents,
                       make-literal-constant(builder, result));
  elseif (offset)
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
      build-assignment
	(builder, policy, source, temp,
	 make-operation
	   (builder, <heap-slot-ref>,
	    list(instance,
		 make-literal-constant(builder, init?-offset)),
	    derived-type: init?-slot.slot-type.ctype-extent,
	    slot-info: init?-slot));
      build-if-body(builder, policy, source, temp);
      build-else(builder, policy, source);
      build-assignment
	(builder, policy, source, #(),
	 make-error-operation
	   (builder, policy, source, #"uninitialized-slot-error-with-location",
	    make-literal-constant(builder, slot), 
            instance,
            make-literal-constant(builder, format-to-string("%=", source))));
      end-body(builder);
    end;
    let value = make-local-var(builder, slot.slot-getter.variable-name,
			       slot.slot-type);
    build-assignment
      (builder, policy, source, value,
       if (offset == #"data-word")
	 make-operation
	   (builder, <data-word-ref>, list(instance),
	    derived-type: slot.slot-type.ctype-extent, slot-info: slot);
       else
	 make-operation
	   (builder, <heap-slot-ref>,
	    pair(instance,
		 pair(make-literal-constant(builder, offset),
		      args.tail)),
	    derived-type: slot.slot-type.ctype-extent,
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
	 make-error-operation
	   (builder, policy, source, #"uninitialized-slot-error-with-location",
	    make-literal-constant(builder, slot), 
            instance,
            make-literal-constant(builder, format-to-string("%=", source))));
      end-body(builder);
    end;
    insert-before(component, call-assign, builder-result(builder));

    let dep = call-assign.depends-on;
    replace-expression(component, dep, value);
  end;
end;

define method optimize-slot-set
    (component :: <component>, call :: <abstract-call>,
     class-slot :: <class-slot-info>, args :: <list>)
    => ();
  let orig-instance = args.second;
  let meta-slot = class-slot.associated-meta-slot;
  let meta-instance = meta-slot.slot-introduced-by;
  let offset = find-slot-offset(meta-slot, meta-instance);
  if (offset)
    let new = args.first;
    let builder = make-builder(component);
    let call-assign = call.dependents.dependent;
    let policy = call-assign.policy;
    let source = call-assign.source-location;
    let slot-home
      = build-slot-home(class-slot.slot-getter.variable-name,
			make-literal-constant(builder, class-slot.slot-introduced-by),
			builder, policy, source);
    build-assignment
      (builder, policy, source, #(),
       make-operation
	 (builder, <heap-slot-set>,
	  apply(list,
	  	new,
		slot-home,
		make-literal-constant(builder, offset),
		args.tail.tail),
	  slot-info: meta-slot));
    begin
      let init?-slot = meta-slot.slot-initialized?-slot;
      if (init?-slot)
	let init?-offset = find-slot-offset(init?-slot, meta-instance);
	unless (init?-offset)
	  error("The slot is at a fixed offset, but the initialized flag "
		  "isn't?");
	end;
	let true-leaf = make-literal-constant(builder, #t);
	let init-op = make-operation(builder, <heap-slot-set>,
				     list(true-leaf,
					  slot-home,
					  make-literal-constant
					    (builder, init?-offset)),
				     slot-info: init?-slot);
	build-assignment(builder, policy, source, #(), init-op);
      end;
    end;
    insert-before(component, call-assign, builder-result(builder));

    let dep = call-assign.depends-on;
    replace-expression(component, dep, new);
  end;
end;

define method optimize-slot-set
    (component :: <component>, call :: <abstract-call>,
     slot :: <each-subclass-slot-info>, args :: <list>)
    => ();
// TBI###
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
		    pair(make-literal-constant(builder, offset),
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
	let true-leaf = make-literal-constant(builder, #t);
	let init-op = make-operation(builder, <heap-slot-set>,
				     list(true-leaf, instance,
					  make-literal-constant
					    (builder, init?-offset)),
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


// next-method invocation magic.

define method expand-next-method-call-ref
    (component :: <component>, dep :: <dependency>,
     new-args :: type-union(<list>, <abstract-variable>),
     next-method-maker :: <primitive>)
    => ();
  let builder = make-builder(component);
  let assign = dep.dependent;
  let source = assign.source-location;
  let policy = assign.policy;

  let next-method-info
    = maybe-copy(component, next-method-maker.depends-on.source-exp,
		 next-method-maker, assign.home-function-region);

  let empty?-leaf = make-local-var(builder, #"empty?", object-ctype());
  build-assignment
    (builder, policy, source, empty?-leaf,
     make-operation
       (builder, <primitive>,
	list(next-method-info,
	     make-literal-constant(builder, #())),
	name: #"=="));
  build-if-body(builder, policy, source, empty?-leaf);
  build-assignment
    (builder, policy, source, #(),
     make-error-operation
       (builder, policy, source, "No next method."));
  build-else(builder, policy, source);
  
  local
    method build-nmi-access
	(rt-getter :: <symbol>, ct-getter :: <function>,
	 debug-name :: <symbol>, type :: <ctype>)
	=> leaf :: <leaf>;
      if (instance?(next-method-info, <literal-constant>))
	make-literal-constant(builder, next-method-info.value.ct-getter);
      else
	let temp = make-local-var(builder, rt-getter, object-ctype());
	build-assignment
	  (builder, policy, source, temp,
	   make-unknown-call
	     (builder, ref-dylan-defn(builder, policy, source, rt-getter), #f,
	      list(next-method-info)));

	let var = make-local-var(builder, debug-name, type);
	build-assignment
	  (builder, policy, source, var,
	   make-operation
	     (builder, <truly-the>, list(temp), guaranteed-type: type));

	var;
      end if;
    end method build-nmi-access;
	
  let func-leaf
    = build-nmi-access(#"head", literal-head, #"next",
		       specifier-type(#(union:, #"<pair>", #"<method>")));
  let remaining-infos-leaf
    = build-nmi-access(#"tail", literal-tail, #"remaining-infos",
		       specifier-type(#"<list>"));

  let ambiguous? = make-local-var(builder, #"ambiguous?", object-ctype());
  build-assignment
    (builder, policy, source, ambiguous?,
     make-operation
       (builder, <instance?>, list(func-leaf),
	type: specifier-type(#"<pair>")));
  build-if-body(builder, policy, source, ambiguous?);
  let temp = make-local-var(builder, #"temp", specifier-type(#"<pair>"));
  build-assignment(builder, policy, source, temp, func-leaf);
  build-assignment
    (builder, policy, source, #(),
     make-error-operation
       (builder, policy, source, #"ambiguous-method-error", temp));
  build-else(builder, policy, source);

  let op
    = if (instance?(new-args, <abstract-variable>))
	//
	// We are passing a cluster of values.  Is at least on guaranteed?
	if (new-args.derived-type.min-values == 0)
	  //
	  // No.  We have to convert the cluster into a vector and
	  // check its size before we can tell if we should be using
	  // the new-args or the orig-args.
	  let vector = make-local-var(builder, #"new-args", object-ctype());
	  build-assignment
	    (builder, policy, source, vector,
	     make-operation
	       (builder, <primitive>,
		list(new-args,
		     make-literal-constant(builder, 0)),
		name: #"canonicalize-results"));
	  let args-empty?-leaf
	    = make-local-var(builder, #"empty?", object-ctype());
	  build-assignment
	    (builder, policy, source, args-empty?-leaf,
	     make-unknown-call
	       (builder, ref-dylan-defn(builder, policy, source, #"empty?"),
		#f, list(vector)));

	  let cluster = make-values-cluster(builder, #"args", wild-ctype());

	  build-if-body(builder, policy, source, args-empty?-leaf);

	  let orig-args
	    = maybe-copy(component,
			 next-method-maker.depends-on.dependent-next
			   .source-exp,
			 next-method-maker, assign.home-function-region);
	  build-assignment
	    (builder, policy, source, cluster,
	     make-operation(builder, <primitive>, list(orig-args),
			    name: #"values-sequence"));

	  build-else(builder, policy, source);
	  
	  build-assignment
	    (builder, policy, source, cluster,
	     make-operation(builder, <primitive>, list(vector),
			    name: #"values-sequence"));
	  
	  end-body(builder);

	  make-operation
	    (builder, <mv-call>,
	     list(func-leaf, remaining-infos-leaf, cluster),
	     use-generic-entry: #t);

	else
	  //
	  // We are calling it on a cluster of at least one value.  So use
	  // the new args.
	  make-operation
	    (builder, <mv-call>,
	     list(func-leaf, remaining-infos-leaf, new-args),
	     use-generic-entry: #t);

	end if;
      else
	//
	// We are calling it with a fixed number of arguments.
	if (new-args == #())
	  let orig-args
	    = maybe-copy(component,
			 next-method-maker.depends-on.dependent-next
			   .source-exp,
			 next-method-maker, assign.home-function-region);
	  let cluster = make-values-cluster(builder, #"args", wild-ctype());
	  build-assignment
	    (builder, policy, source, cluster,
	     make-operation(builder, <primitive>, list(orig-args),
			    name: #"values-sequence"));
	  make-operation
	    (builder, <mv-call>,
	     list(func-leaf, remaining-infos-leaf, cluster),
	     use-generic-entry: #t);
	else
	  make-unknown-call
	    (builder, func-leaf, remaining-infos-leaf, new-args);
	end if;
      end if;
  
  end-body(builder); // the ambiguous test.
  end-body(builder); // the no-next-method test.

  insert-before(component, assign, builder-result(builder));
  replace-expression(component, dep, op);
end method expand-next-method-call-ref;

