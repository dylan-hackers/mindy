rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/func.dylan,v 1.18 1996/02/09 01:38:48 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

define abstract class <function> (<object>)
  //
  // Some human readable ``name'' for this function.
  slot function-name :: <byte-string>,
    required-init-keyword: function-name:;
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
  slot function-keywords :: type-union(<simple-object-vector>, <false>),
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
    init-value: #f,
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
    (func :: <function>, closure-size :: <integer>)
    => res :: <raw-closure>;
  make(<raw-closure>,
       function-name: func.function-name,
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



define class <accessor-method> (<method>)
  //
  // The <slot-descriptor> this is the acccessor of.
  slot accessor-slot :: <slot-descriptor>,
    required-init-keyword: slot:;
end class <accessor-method>;

seal generic make(singleton(<accessor-method>));



define class <method-closure> (<method>, <closure>)
end;

seal generic make(singleton(<method-closure>));

define method make-closure
    (func :: <method>, closure-size :: <integer>)
    => res :: <method-closure>;
  make(<method-closure>,
       function-name: func.function-name,
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



define class <type-vector> (<simple-vector>)
  sealed slot %element :: <type>,
    init-value: type-union(), init-keyword: fill:,
    sizer: size, size-init-value: 0, size-init-keyword: size:;
end class <type-vector>;

seal generic make (singleton(<type-vector>));
seal generic initialize (<type-vector>);

define sealed inline method element
    (vec :: <type-vector>, index :: <integer>,
     #key default = $not-supplied)
    => element :: <object>;
  if (index >= 0 & index < vec.size)
    %element(vec, index);
  elseif (default == $not-supplied)
    element-error(vec, index);
  else
    default;
  end;
end;

define sealed inline method element-setter
    (new-value :: <type>, vec :: <type-vector>, index :: <integer>)
    => new-value :: <type>;
  if (index >= 0 & index < vec.size)
    %element(vec, index) := new-value;
  else
    element-error(vec, index);
  end;
end;

define class <gf-cache> (<object>)
  slot simple :: <boolean>, init-value: #t;
  slot cached-normal :: <list>, init-value: #();
  slot cached-ambiguous :: <list>, init-value: #();
  slot cached-valid-keywords :: type-union(<simple-object-vector>,
					   one-of(#f, #"all")),
    init-value: #f;
  slot cached-classes :: <type-vector>,
    required-init-keyword: #"classes";
  slot next :: type-union(<false>, <gf-cache>), init-value: #f;
  slot call-count :: <integer>, init-value: 1;
end class <gf-cache>;

seal generic make(singleton(<gf-cache>));
seal generic initialize (<gf-cache>);

define class <generic-function> (<function>)
  //
  slot generic-function-methods :: <list>,
    required-init-keyword: methods:;
  slot method-cache :: type-union(<false>, <gf-cache>), init-value: #f;
end;

seal generic make(singleton(<generic-function>));


// Function information routines.

define inline method generic-function-mandatory-keywords
    (gf :: <generic-function>)
    => keywords :: false-or(<simple-object-vector>);
  gf.function-keywords;
end method generic-function-mandatory-keywords;

define inline method function-arguments (function :: <function>)
    => (required :: <integer>, rest? :: <boolean>,
	keywords :: type-union(<simple-object-vector>, one-of(#f, #"all")));
  values(function.function-specializers.size,
	 function.function-rest?,
	 if (function.function-all-keys?)
	   #"all";
	 else
	   function.function-keywords;
	 end);
end method function-arguments;

define inline method function-return-values (function :: <function>)
    => (return-value-types :: <simple-object-vector>,
	rest-return-value :: false-or(<type>));
  let type = function.function-rest-value;
  values(function.function-values,
	 if (subtype?(type, type-union()))
	   #f;
	 else
	   type;
	 end if);
end method function-return-values;


// Method adding, finding, and removing.

define method add-method
    (gf :: <generic-function>, new :: <method>)
    => (new :: <method>, old :: false-or(<method>));
  error("### runtime add-method not yet implemented.");
end method add-method;

define method find-method
    (gf :: <generic-function>, specializers :: <sequence>)
    => meth :: false-or(<method>);
  error("### runtime find-method not yet implemented.");
end method find-method;

define method remove-method
    (gf :: <generic-function>, meth :: <method>)
    => meth :: <method>;
  error("### runtime remove-method not yet implemented.");
end method remove-method;


// Function call related utilities.

// make-rest-arg -- internal.
//
// The compiler produces references to this function when it needs to
// convert a block of values on the values stack into a rest arg.
//
define constant make-rest-arg
  = method (arg-ptr :: <raw-pointer>, count :: <integer>)
	=> res :: <simple-object-vector>;
      if (count == 0)
	#[];
      else
	let res = make(<simple-object-vector>, size: count);
	for (index :: <integer> from 0 below count)
	  %element(res, index) := %%primitive extract-arg (arg-ptr, index);
	end;
	res;
      end if;
    end;

// verify-keywords -- internal.
//
// The compiler generates calls to this function in static discriminators to
// verify that the supplied keywords are indeed valid keywords.  And it is
// only called if there are an even number of keyword/value arguments, so
// we don't need to check for that case.
//
// We implement it via two different methods because we know that practically
// all calls are going to have the valid-keywords as a constant, and hence
// the compiler will have precise type information.
//
define generic verify-keywords
    (keyword-value-arguments :: <simple-object-vector>,
     valid-keywords :: type-union(singleton(#"all"), <simple-object-vector>))
    => ();
//
define method verify-keywords
    (args :: <simple-object-vector>, valid-keywords == #"all")
    => ();
  for (index :: <integer> from 0 below args.size by 2)
    check-type(args[index], <symbol>);
  end for;
end method verify-keywords;
//
define method verify-keywords
    (args :: <simple-object-vector>, valid-keywords :: <simple-object-vector>)
    => ();
  for (index :: <integer> from 0 below args.size by 2)
    let key :: <symbol> = args[index];
    unless (member?(key, valid-keywords))
      unrecognized-keyword-error(key);
    end;
  end for;
end method verify-keywords;


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


// General entry for methods.

// general-call -- magic.
//
// The compiler uses this as the general entry for methods that it thinks will
// not need a general entry.  It works, but is slower than a custom built
// general entry.
// 
define constant general-call
  = method (self :: <method>, nargs :: <integer>)
      let specializers = self.function-specializers;
      let nfixed = specializers.size;
      let keywords = self.function-keywords;
      if (self.function-rest? | keywords)
	if (nargs < nfixed)
	  wrong-number-of-arguments-error(#f, nfixed, nargs);
	end;
	if (keywords & odd?(nargs - nfixed))
	  odd-number-of-keyword/value-arguments-error();
	end;
      else
	if (nargs ~= nfixed)
	  wrong-number-of-arguments-error(#t, nfixed, nargs);
	end;
      end;
      let arg-ptr :: <raw-pointer> = %%primitive extract-args(nargs);
      for (index :: <integer> from 0,
	   specializer :: <type> in specializers)
	check-type(%%primitive extract-arg (arg-ptr, index), specializer);
      end for;
      if (keywords)
	if (self.function-all-keys?)
	  for (index :: <integer> from nfixed below nargs by 2)
	    check-type(%%primitive extract-arg (arg-ptr, index), <symbol>);
	  end for;
	else
	  for (index :: <integer> from nfixed below nargs by 2)
	    let key :: <symbol> = %%primitive extract-arg (arg-ptr, index);
	    unless (member?(key, keywords))
	      unrecognized-keyword-error(key);
	    end unless;
	  end for;
	end if;
      end if;
      %%primitive invoke-generic-entry
	(self, #(), %%primitive pop-args(arg-ptr));
    end method;


// Generic function dispatch.

// gf-call -- magic.
//
// The compiler uses this for the general-entry to all generic functions.
// 
define constant gf-call
  = method (self :: <generic-function>, nargs :: <integer>)
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
      let arg-ptr :: <raw-pointer> = %%primitive extract-args(nargs);
      let (ordered, ambiguous, valid-keywords)
	= cached-sorted-applicable-methods(self, nfixed, arg-ptr);
      if (valid-keywords)
	for (index :: <integer> from nfixed below nargs by 2)
	  let key :: <symbol> = %%primitive extract-arg (arg-ptr, index);
	  unless (valid-keywords == #"all"
		    | member?(key,
			      check-type(valid-keywords,
					 <simple-object-vector>)))
	    error("Unrecognized keyword: %=", key);
	  end unless;
	end for;
      end if;
      if (ambiguous == #())
	if (ordered == #())
	  for (index :: <integer> from 0 below nfixed)
	    let specializer :: <type> = %element(specializers, index);
	    let arg = %%primitive extract-arg(arg-ptr, index);
	    %check-type(arg, specializer);
	  end;
	  no-applicable-methods-error();
	else
	  %%primitive invoke-generic-entry
	    (ordered.head, ordered.tail, %%primitive pop-args(arg-ptr));
	end;
      else
	for (prev :: false-or(<pair>) = #f then remaining,
	     remaining :: <list> = ordered then remaining.tail,
	     until: remaining == #())
	finally
	  if (prev)
	    prev.tail := list(ambiguous);
	    %%primitive invoke-generic-entry
	      (ordered.head, ordered.tail, %%primitive pop-args(arg-ptr));
	  else
	    ambiguous-method-error(ambiguous);
	  end;
	end;
      end;
    end;
    
define method internal-sorted-applicable-methods
    (gf :: <generic-function>, nargs :: <integer>,
     arg-ptr :: <raw-pointer>)
    => (ordered :: <list>, ambiguous :: <list>,
	valid-keywords
	  :: type-union(<simple-object-vector>, one-of(#f, #"all")));

  // We have to use low-level stuff here.  It would be bad to do a full
  // generic function call at this point.
  let cache-classes = make(<type-vector>, size: nargs);
  for (i :: <integer> from 0 below nargs)
    %element(cache-classes, i)
      := (%%primitive extract-arg(arg-ptr, i)).object-class;
  end for;
  let cache = make(<gf-cache>, classes: cache-classes);

  // Ordered accumulates the methods we can tell the ordering of.  Each
  // element in this list is a method.
  let ordered :: <list> = #();

  // Ambiguous accumulates the set of methods of which it is unclear which
  // follows next after ordered.  These methods will all be mutually ambiguous.
  let ambiguous :: <list> = #();

  // Accumulates the valid keywords.
  let valid-keywords :: type-union(<list>, one-of(#f, #"all"))
    = if (gf.function-keywords)
	if (gf.function-all-keys?) #"all" else #() end if;
      else
	#f;
      end if;

  for (meth :: <method> in gf.generic-function-methods)
    if (internal-applicable-method?(meth, arg-ptr, cache))
      unless (valid-keywords == #f | valid-keywords == #"all")
	if (meth.function-all-keys?)
	  valid-keywords := #"all";
	else
	  for (keyword :: <symbol>
		 in check-type(meth.function-keywords, <simple-object-vector>))
	    unless (member?(keyword, check-type(valid-keywords, <list>)))
	      valid-keywords := pair(keyword, valid-keywords);
	    end unless;
	  end for;
	end if;
      end unless;

      block (done-with-method)
	for (remaining :: <list> = ordered then remaining.tail,
	     prev :: false-or(<pair>) = #f then remaining,
	     until: remaining == #())
	  //
	  // Grab the method to compare this method against.
	  let other = remaining.head;
	  select (compare-methods(meth, other, arg-ptr))
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
	    select (compare-methods(meth, remaining.head, arg-ptr))
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

  let old-cache = gf.method-cache;
  gf.method-cache := cache;
  cache.next := old-cache;
  cache.cached-normal := ordered;
  cache.cached-ambiguous := ambiguous;
  let valid-keywords
    = if (instance?(valid-keywords, <list>))
	let vec = make(<simple-object-vector>,
		       size: check-type(valid-keywords, <list>).size);
	for (index :: <integer> from 0,
	     keyword in check-type(valid-keywords, <list>))
	  vec[index] := keyword;
	end for;
	vec;
      else
	valid-keywords;
      end if;
  cache.cached-valid-keywords := valid-keywords;

  values(ordered, ambiguous, valid-keywords);
end;

define variable *debug-generic-threshold* :: <integer> = -1;

define method cached-sorted-applicable-methods
    (gf :: <generic-function>, nargs :: <integer>,
     arg-ptr :: <raw-pointer>)
 => (ordered :: <list>, ambiguous :: <list>,
     valid-keywords :: type-union(<simple-object-vector>, one-of(#f, #"all")));
  block (return)
    for (prev :: type-union(<false>, <gf-cache>) = #f then cache,
	 cache :: type-union(<false>, <gf-cache>) = gf.method-cache
	   then cache.next,
	 until: cache == #f)
      block (no-match)
	let cache :: <gf-cache> = cache; // A hint to the type system.

	let classes :: <type-vector> = cache.cached-classes;
	if (cache.simple)
	  for (index :: <integer> from 0 below nargs)
	    let type :: <type> = %element(classes, index);
	    let arg = %%primitive extract-arg(arg-ptr, index);
	    unless (type == arg.object-class) no-match() end unless;
	  end for;
	else
	  for (index :: <integer> from 0 below nargs)
	    let type :: <type> = %element(classes, index);
	    let arg = %%primitive extract-arg(arg-ptr, index);
	    unless (type == arg.object-class
		      | (~instance?(type, <class>) & instance?(arg, type)))
	      no-match();
	    end unless;
	  end for;
	end if;

	if (prev)	// not the first entry -- make it so.
	  prev.next := cache.next;
	  cache.next := gf.method-cache;
	  gf.method-cache := cache;
	end if;

	// Debugging code.  This will slow down dispatch by a tiny fraction,
	// but will pay off in the short term for optimization purposes.
	let hits = cache.call-count + 1;
	if (hits == *debug-generic-threshold*)
	  format("\n*** Generic function %= called %= times with types %=\n",
		 gf, hits, classes);
	  cache.call-count := 0;
	else
	  cache.call-count := hits;
	end if;

	return(cache.cached-normal, cache.cached-ambiguous,
	       cache.cached-valid-keywords);
      end block;
    end for;
    internal-sorted-applicable-methods(gf, nargs, arg-ptr);
  end block;
end method cached-sorted-applicable-methods;

define method internal-applicable-method?
    (meth :: <method>, arg-ptr :: <raw-pointer>, cache :: <gf-cache>)
 => (res :: <boolean>);
  block (return)
    let classes :: <type-vector> = cache.cached-classes;
    for (specializer :: <type> in meth.function-specializers,
	 index :: <integer> from 0)
      let arg-type = classes[index];
      let arg :: <object> = %%primitive extract-arg(arg-ptr, index);

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
	  classes[index] 
	    := case
		 (instance?(specializer, <limited-integer>)) =>
		   if (instance?(arg-type, <limited-integer>))
		     intersect-limited-ints(arg-type, specializer);
		   else
		     specializer;
		   end if;
		 (specializer == <byte-character>) =>
		   <byte-character>;
		 otherwise => singleton(arg);
	       end case;
	  cache.simple := #f;
	else
	  // At last, we've found something which isn't valid.  We'll return
	  // false, but first we may need to adjust the cache to identify the
	  // subtypes that might still be valid.
	  if (overlap?(arg-type, specializer))
	    classes[index]
	      := if (instance?(specializer, <limited-integer>))
		   restrict-limited-ints(arg, arg-type, specializer);
		 elseif (specializer == <byte-character>)
		   <non-byte-character>;
		 else
		   restrict-type(arg-type, specializer);
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
    (meth1 :: <method>, meth2 :: <method>, arg-ptr :: <raw-pointer>)
    => res :: one-of(#"more-specific", #"less-specific", #"ambiguous",
		     #"identical");
  block (return)
    let result = #"identical";
    let specializers1 = meth1.function-specializers;
    let specializers2 = meth2.function-specializers;
    for (index :: <integer> from 0 below specializers1.size)
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
	let arg-class = object-class(%%primitive extract-arg(arg-ptr, index));
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


// Accessor methods.

define constant general-rep-getter
  = method (self :: <accessor-method>, nargs :: <integer>, next-info :: <list>)
      // We don't specify a return type because we want to use the
      // unknown-values convention.
      let arg-ptr :: <raw-pointer> = %%primitive extract-args(1);
      let instance = %%primitive extract-arg(arg-ptr, 0);
      %%primitive pop-args(arg-ptr);
      let posn = find-slot-offset(instance.object-class, self.accessor-slot);
      let result = %%primitive ref-slot(instance, #"general", posn);
      unless (%%primitive initialized? (result))
	uninitialized-slot-error();
      end unless;
      result;
    end;

define constant general-rep-setter
  = method (self :: <accessor-method>, nargs :: <integer>, next-info :: <list>)
      // We don't specify a return type because we want to use the
      // unknown-values convention.
      let arg-ptr :: <raw-pointer> = %%primitive extract-args(1);
      let new-value = %%primitive extract-arg(arg-ptr, 0);
      let instance = %%primitive extract-arg(arg-ptr, 1);
      %%primitive pop-args(arg-ptr);
      let posn = find-slot-offset(instance.object-class, self.accessor-slot);
      %%primitive set-slot(new-value, instance, #"general", posn);
      new-value;
    end;


define constant heap-rep-getter
  = method (self :: <accessor-method>, nargs :: <integer>, next-info :: <list>)
      // We don't specify a return type because we want to use the
      // unknown-values convention.
      let arg-ptr :: <raw-pointer> = %%primitive extract-args(1);
      let instance = %%primitive extract-arg(arg-ptr, 0);
      %%primitive pop-args(arg-ptr);
      let posn = find-slot-offset(instance.object-class, self.accessor-slot);
      let result = %%primitive ref-slot(instance, #"heap", posn);
      unless (%%primitive initialized? (result))
	uninitialized-slot-error();
      end unless;
      result;
    end;

define constant heap-rep-setter
  = method (self :: <accessor-method>, nargs :: <integer>, next-info :: <list>)
      // We don't specify a return type because we want to use the
      // unknown-values convention.
      let arg-ptr :: <raw-pointer> = %%primitive extract-args(1);
      let new-value = %%primitive extract-arg(arg-ptr, 0);
      let instance = %%primitive extract-arg(arg-ptr, 1);
      %%primitive pop-args(arg-ptr);
      let posn = find-slot-offset(instance.object-class, self.accessor-slot);
      %%primitive set-slot(new-value, instance, #"heap", posn);
      new-value;
    end;


// Applicability predicates.

define method applicable-method?
    (function :: <function>, #rest sample-arguments)
    => res :: <boolean>;
  unless (sample-arguments.size == function.function-specializers.size)
    error("Wrong number of sample arguments to applicable-method?");
  end;
  error("### runtime applicable-method? not yet implemented");
end;


define method sorted-applicable-methods
    (gf :: <generic-function>, #rest sample-arguments)
    => (sorted :: <list>, unsorted :: <list>);
  unless (sample-arguments.size == gf.function-specializers.size)
    error("Wrong number of sample arguments to sorted-applicable-methods.");
  end;
  error("### runtime sorted-applicable-methods not yet implemented");
end;


// #next method handling

define constant %make-next-method-cookie = method
    (next-method-info :: <list>, #rest original-args)
 => (res :: type-union(<false>, <function>));
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
	(next-method-info.head, next-method-info.tail,
	 values-sequence(args));
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
