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

define class <generic-function> (<function>)
  //
  slot generic-function-methods :: <list>,
    required-init-keyword: methods:;
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
      for (specializer :: <type> in specializers,
	   index :: <fixed-integer> from 0)
	args[index] := check-type(%%primitive extract-arg (arg-ptr, index),
				  specializer);
      end;
      for (index :: <fixed-integer> from nfixed below nargs)
	args[index] := %%primitive extract-arg (arg-ptr, index);
      end;
      %%primitive pop-args(arg-ptr);
      let (ordered, ambiguous)
	= internal-sorted-applicable-methods(self, args);
      if (ambiguous == #())
	if (ordered == #())
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

  // Ordered accumulates the methods we can tell the ordering of.  Each
  // element in this list is a method.
  let ordered = #();

  // Ambiguous accumulates the set of methods of which it is unclear which
  // follows next after ordered.  These methods will all be mutually ambiguous.
  let ambiguous = #();

  for (meth :: <method> in gf.generic-function-methods)
    if (internal-applicable-method?(meth, args))
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

  values(ordered, ambiguous);
end;


define method internal-applicable-method?
    (meth :: <method>, args :: <simple-object-vector>) => res :: <boolean>;
  block (return)
    for (spec :: <type> in meth.function-specializers,
	 index :: <fixed-integer> from 0)
      unless (instance?(args[index], spec))
	return(#f);
      end;
    end;
    #t;
  end;
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
  internal-sorted-applicable-methods(gf, args);
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

define inline method always (object :: <object>) => res :: <function>;
  method (#rest ignore) => object :: <object>;
    object;
  end;
end;
