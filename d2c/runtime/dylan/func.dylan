module: dylan

define abstract class <function> (<object>)
  //
  // The general entry point for this function.  The actual address of the
  // C function.
  slot general-entry :: <raw-pointer>,
    required-init-keyword: general-entry:;
  //
  slot function-specializers :: <simple-object-vector>,
    required-init-keyword: specializers:;
  //
  slot function-rest? :: <boolean>,
    required-init-keyword: rest?:;
  //
  slot function-keywords :: union(<simple-object-vector>, <false>),
    required-init-keyword: keywords:;
  //
  slot function-all-keys? :: <boolean>,
    required-init-keyword: all-keys?:;
  //
  slot function-values :: <simple-object-vector>,
    required-init-keyword: values:;
  //
  slot function-rest-value :: <type>,
    required-init-keyword: rest-value:;
end;
  
seal generic make (singleton(<function>));
seal generic initialize (<function>);

define abstract class <closure> (<function>)
  //
  slot closure-var :: <object>,
    sizer: closure-size, size-init-value: 0, size-init-keyword: closure-size:;
end;

seal generic make(singleton(<closure>));

define class <raw-function> (<function>)
end;

seal generic make(singleton(<raw-function>));

define class <raw-closure> (<raw-function>, <closure>)
end;

seal generic make(singleton(<raw-closure>));

define method make-closure
    (func :: <function>, closure-size :: <fixed-integer>)
    => res :: <raw-closure>;
  make(<raw-closure>,
       general-entry: func.general-entry,
       specializers: func.function-specializers,
       rest?: func.function-rest?,
       keywords: func.function-keywords,
       all-keys?: func.function-all-keys?,
       values: func.function-values,
       rest-value: func.function-rest-value,
       closure-size: closure-size);
end;

define class <method> (<function>)
  //
  // The generic-function entry point for this method.
  slot generic-entry :: <raw-pointer>,
    required-init-keyword: general-entry:;
end;

seal generic make(singleton(<method>));

define class <method-closure> (<method>, <closure>)
end;

seal generic make(singleton(<method-closure>));

define method make-closure
    (func :: <method>, closure-size :: <fixed-integer>)
    => res :: <method-closure>;
  make(<method-closure>,
       general-entry: func.general-entry,
       specializers: func.function-specializers,
       rest?: func.function-rest?,
       keywords: func.function-keywords,
       all-keys?: func.function-all-keys?,
       values: func.function-values,
       rest-value: func.function-rest-value,
       generic-entry: func.generic-entry,
       closure-size: closure-size);
end;

define class <gf-cache> (<object>)
  slot simple :: <boolean>, init-value: #t;
  slot cached-normal :: <list>, init-value: #();
  slot cached-ambiguous :: <list>, init-value: #();
  slot cached-classes :: <simple-object-vector>,
    required-init-keyword: #"classes";
  slot next :: union(<false>, <gf-cache>), required-init-keyword: #"next";
  slot prev :: union(<false>, <gf-cache>), init-value: #f;
end class <gf-cache>;

seal generic make(singleton(<gf-cache>));
seal generic initialize (<gf-cache>);

define class <generic-function> (<function>)
  //
  slot generic-function-methods :: <list>,
    required-init-keyword: methods:;
  slot method-cache :: union(<false>, <gf-cache>), init-value: #f;
end;

seal generic make(singleton(<generic-function>));


// Function call related utilities.

// make-rest-arg -- internal.
//
// The compiler produces references to this function when it needs to
// convert a block of values on the values stack into a rest arg.
//
define constant make-rest-arg
  = method (arg-ptr :: <raw-pointer>, count :: <fixed-integer>)
	=> res :: <simple-object-vector>;
      let res = make(<simple-object-vector>, size: count);
      for (index :: <fixed-integer> from 0 below count)
	%element(res, index) := %%primitive extract-arg (arg-ptr, index);
      end;
      res;
    end;


// Function application and invocation.

// apply -- exported.
//
// Call the function with the arguments.  The last element of arguments is
// itself a sequence of additional arguments.
//
define method apply (function :: <function>, #rest arguments)
  if (empty?(arguments))
    error("Apply must be given at least one argument.");
  end;
  let len-1 = size(arguments) - 1;
  %%mv-call(function,
	    values-sequence(copy-sequence(arguments, end: len-1)),
	    values-sequence(arguments[len-1]));
end;


define constant gf-call
  = method (self :: <generic-function>, nargs :: <fixed-integer>)
      let specializers = self.function-specializers;
      let nfixed = specializers.size;
      if (self.function-rest? | self.function-keywords)
	if (nargs < nfixed)
	  wrong-number-of-arguments-error(#f, nfixed, nargs);
	end;
	if (self.function-keywords & odd?(nargs - nfixed))
	  odd-number-of-keyword/value-arguments-error();
	end;
      else
	if (nargs ~= nfixed)
	  wrong-number-of-arguments-error(#t, nfixed, nargs);
	end;
      end;
      let arg-ptr = %%primitive extract-args(nargs);
      let args = make(<simple-object-vector>, size: nargs);
      for (index :: <fixed-integer> from 0 below nargs)
	// This is a very critical path, and we know that the indices are
	// valid, so use the lower level function
	%element(args, index) := %%primitive extract-arg (arg-ptr, index);
      end;
      %%primitive pop-args(arg-ptr);
      let (ordered, ambiguous)
	= cached-sorted-applicable-methods(self, args);
      if (ambiguous == #())
	if (ordered == #())
	  for (index :: <fixed-integer> from 0 below nfixed)
	    let specializer :: <type> = %element(specializers, index);
	    let arg = %element(args, index);
	    %check-type(arg, specializer);
	  end;
	  no-applicable-methods-error();
	else
	  %%primitive invoke-generic-entry
	    (ordered.head, ordered.tail, values-sequence(args));
	end;
      else
	for (prev :: false-or(<pair>) = #f then remaining,
	     remaining :: <list> = ordered then remaining.tail,
	     until: remaining == #())
	finally
	  if (prev)
	    prev.tail := list(ambiguous);
	    %%primitive invoke-generic-entry
	      (ordered.head, ordered.tail, values-sequence(args));
	  else
	    ambiguous-method-error(ambiguous);
	  end;
	end;
      end;
    end;
    
define method internal-sorted-applicable-methods
    (gf :: <generic-function>, args :: <simple-object-vector>)
    => (ordered :: <list>, ambiguous :: <list>);

  // We have to use low-level stuff here.  It would be bad to do a full
  // generic function call at this point.
  let cache-classes = make(<simple-object-vector>, size: args.size);
  for (i :: <fixed-integer> from 0 below args.size)
    %element(cache-classes, i) := %element(args, i).object-class;
  end for;
  let cache = make(<gf-cache>, next: gf.method-cache, classes: cache-classes);

  // Ordered accumulates the methods we can tell the ordering of.  Each
  // element in this list is a method.
  let ordered = #();

  // Ambiguous accumulates the set of methods of which it is unclear which
  // follows next after ordered.  These methods will all be mutually ambiguous.
  let ambiguous = #();

  for (meth :: <method> in gf.generic-function-methods)
    if (internal-applicable-method?(meth, args, cache))
      block (done-with-method)
	for (remaining :: <list> = ordered then remaining.tail,
	     prev :: false-or(<pair>) = #f then remaining,
	     until: remaining == #())
	  //
	  // Grab the method to compare this method against.
	  let other = remaining.head;
	  select (compare-methods(meth, other, args))
	    //
	    // Our method is more specific, so insert it in the list of ordered
	    // methods and go on to the next method.
	    #"more-specific" =>
	      if (prev)
		prev.tail := pair(meth, remaining);
	      else
		ordered := pair(meth, remaining);
	      end;
	      done-with-method();

	    #"less-specific" =>
	      //
	      // Our method is less specific, so we can't do anything at this
	      // time.
	      #f;

	    #"ambiguous" =>
	      //
	      // We know that the other method is more specific than anything
	      // in the current ambiguous set, so throw it away making a new
	      // ambiguous set.  Taking into account that we might have a set
	      // of equivalent methods on our hands.
	      if (prev)
		prev.tail := #();
	      else
		ordered := #();
	      end;
	      ambiguous := list(meth, remaining.head);
	      done-with-method();
	  end;
	finally
	  //
	  // Our method was less specific than any method in the ordered list.
	  // This either means that our method needs to be tacked onto the end
	  // of the ordered list, added to the ambiguous list, or ignored.
	  // Compare the method against all the methods in the ambiguous list
	  // to figure out which.
	  let ambiguous-with = #();
	  for (remaining :: <list> = ambiguous then remaining.tail,
	       until: remaining == #())
	    select (compare-methods(meth, remaining.head, args))
	      #"more-specific" =>
		#f;
	      #"less-specific" =>
		done-with-method();
	      #"ambiguous" =>
		ambiguous-with := pair(remaining.head, ambiguous-with);
	    end;
	  end;
	  //
	  // Ambiguous-with is only #() if we are more specific than anything
	  // currently in the ambiguous set.  So tack us onto the end of the
	  // ordered set.  Otherwise, set the ambiguous set to us and
	  // everything we are ambiguous with.
	  if (ambiguous-with == #())
	    if (prev)
	      prev.tail := list(meth);
	    else
	      ordered := list(meth);
	    end;
	  else
	    ambiguous := pair(meth, ambiguous-with);
	  end;
	end;
      end;
    end;
  end;

  gf.method-cache := cache;
  cache.cached-normal := ordered;
  cache.cached-ambiguous := ambiguous;

  values(ordered, ambiguous);
end;

define method cached-sorted-applicable-methods
    (gf :: <generic-function>, args :: <simple-object-vector>)
 => (ordered :: <list>, ambiguous :: <list>);
  if (gf.generic-function-methods.empty?)
    values(#(), #());
  else
    block (return)
      for (cache :: union(<false>, <gf-cache>) = gf.method-cache
	     then cache.next,
	   until: cache == #f)
	block (no-match)
	  let cache :: <gf-cache> = cache; // A hint to the type system.

	  let classes :: <simple-object-vector> = cache.cached-classes;
	  if (cache.simple)
	    for (index :: <fixed-integer> from 0 below args.size)
	      let type :: <type> = %element(classes, index);
	      let arg = %element(args, index);
	      unless (type == arg.object-class) no-match() end unless;
	    end for;
	  else
	    for (index :: <fixed-integer> from 0 below args.size)
	      let type :: <type> = %element(classes, index);
	      let arg = %element(args, index);
	      unless (type == arg.object-class
			| (~instance?(type, <class>) & instance?(arg, type)))
		no-match();
	      end unless;
	    end for;
	  end if;

	  unless (gf.method-cache == cache)
	    if (cache.prev) cache.prev.next := cache.next end if;
	    if (cache.next) cache.next.prev := cache.prev end if;
	    cache.next := gf.method-cache;
	    gf.method-cache := cache;
	  end unless;
	  return(cache.cached-normal, cache.cached-ambiguous);
	end block;
      end for;
      internal-sorted-applicable-methods(gf, args);
    end block;
  end if;
end method cached-sorted-applicable-methods;

define method internal-applicable-method?
    (meth :: <method>, args :: <simple-object-vector>, cache :: <gf-cache>)
 => (res :: <boolean>);
  block (return)
    let classes :: <simple-object-vector> = cache.cached-classes;
    for (specializer :: <type> in meth.function-specializers,
	 arg :: <object> in args,
	 index :: <fixed-integer> from 0)
      let arg-type = classes[index];

      // arg-type may be either a singleton, a limited-int, or a class.  This
      // stuff has been worked out on a case by case basis.  It could
      // certainly be made clearer, but this could potentially reduce the
      // efficiency by a large margin.
      if (subtype?(arg-type, specializer))
	// A valid arg -- continue
	#f;
      else
	if (instance?(arg, specializer))
	  // Still a valid arg, but a complicated one.  Therefore we patch up
	  // the cache entry to better identify this particular case.
	  classes[index] := if (~%instance?(specializer, <limited-integer>))
			      singleton(arg);
			    elseif (%instance?(arg-type, <limited-integer>))
			      intersect-limited-ints(arg-type, specializer);
			    else
			      specializer;
			    end if;
	  cache.simple := #f;
	else
	  // At last, we've found something which isn't valid.  We'll return
	  // false, but first we may need to adjust the cache to identify the
	  // subtypes that might still be valid.
	  if (overlap?(arg-type, specializer))
	    classes[index]
	      := if (~%instance?(specializer, <limited-integer>))
		   restrict-type(specializer, arg-type);
		 else
		   restrict-limited-ints(arg, arg-type, specializer);
		 end if;
	    cache.simple := #f;
	  end if;
	  return(#f);
	end if;
      end if;
    end for;
    #t;
  end block;
end;

define method compare-methods
    (meth1 :: <method>, meth2 :: <method>, args :: <simple-object-vector>)
    => res :: one-of(#"more-specific", #"less-specific", #"ambiguous",
		     #"identical");
  block (return)
    let result = #"identical";
    let specializers1 = meth1.function-specializers;
    let specializers2 = meth2.function-specializers;
    for (index :: <fixed-integer> from 0 below specializers1.size)
      let spec1 = specializers1[index];
      let spec2 = specializers2[index];
      if (subtype?(spec1, spec2))
	unless (subtype?(spec2, spec1))
	  if (result == #"less-specific")
	    return(#"ambiguous");
	  else
	    result := #"more-specific";
	  end;
	end;
      elseif (subtype?(spec2, spec1))
	if (result == #"more-specific")
	  return(#"ambiguous");
	else
	  result := #"less-specific";
	end;
      elseif (instance?(spec1, <class>) & instance?(spec2, <class>))
	let arg-class = object-class(args[index]);
	block (found)
	  for (super :: <class> in arg-class.all-superclasses)
	    if (super == spec1)
	      if (result == #"less-specific")
		return(#"ambiguous");
	      else
		result := #"more-specific";
	      end;
	      found();
	    elseif (super == spec2)
	      if (result == #"more-specific")
		return(#"ambiguous");
	      else
		result := #"less-specific";
	      end;
	      found();
	    end;
	  end;
	  error("Specializer not in the CPL even though the "
		  "method was applicable?");
	end;
      else
	return(#"ambiguous");
      end;
    end for;
    result;
  end block;
end method compare-methods;


define method sorted-applicable-methods
    (gf :: <generic-function>, #rest args)
    => res :: <list>;
  unless (args.size == gf.function-specializers.size)
    error("Wrong number of arguments to sorted-applicable-methods.");
  end;
  cached-sorted-applicable-methods(gf, args);
end;


// #next method handling

define method %make-next-method-cookie
    (next-method-info :: <list>, #rest original-args)
    => res :: union(<false>, <function>);
  if (next-method-info == #())
    #f;
  elseif (instance?(next-method-info.head, <pair>))
    let ambiguous = next-method-info.head;
    method (#rest replacement-args)
      ambiguous-method-error(ambiguous);
    end;
  else
    method (#rest replacement-args)
      let args = if (replacement-args.size == 0)
		   original-args;
		 else
		   replacement-args;
		 end;
      %%primitive invoke-generic-entry
	(next-method-info.head, next-method-info.tail, values-sequence(args));
    end;
  end;
end;


// Functional Operations

/* ### not absolutly needed

define inline method compose (function :: <function>, #rest more-functions)
    => res :: <function>;
  reduce(binary-compose, function, more-functions);
end;

define inline method binary-compose
    (function1 :: <function>, function2 :: <function>) => res :: <function>;
  method (#rest arguments)
    function1(apply(function2, arguments));
  end;
end;

define inline method complement (predicate :: <function>)
  compose(\~, predicate);
end;

define inline method disjoin (function :: <function>, #rest more-functions)
    => res :: <function>;
  reduce(binary-disjoin, function, more-functions);
end;

define inline method binary-disjoin
    (function1 :: <function>, function2 :: <function>) => res :: <function>;
  method (#rest arguments)
    apply(function1, arguments) | apply(function2, arguments);
  end;
end;

define inline method conjoin (function :: <function>, #rest more-functions)
    => res :: <function>;
  reduce(binary-conjoin, function, more-functions);
end;

define inline method binary-conjoin 
    (function1 :: <function>, function2 :: <function>) => res :: <function>;
  method (#rest arguments)
    apply(function1, arguments) & apply(function2, arguments);
  end;
end;

*/

define inline method curry (function :: <function>, #rest curried-args)
    => res :: <function>;
  method (#rest more-args)
    %%mv-call(function,
	      values-sequence(curried-args),
	      values-sequence(more-args));
  end;
end;

define inline method rcurry (function :: <function>, #rest curried-args)
    => res :: <function>;
  method (#rest more-args)
    %%mv-call(function,
	      values-sequence(more-args),
	      values-sequence(curried-args));
  end;
end;

/* ### not absolutly needed
define inline method always (object :: <object>) => res :: <function>;
  method (#rest ignore) => object :: <object>;
    object;
  end;
end;

*/
