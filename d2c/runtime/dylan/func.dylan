rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/func.dylan,v 1.13 2003/02/17 17:36:54 andreas Exp $
copyright: see below
module: dylan-viscera

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
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

define abstract class <function> (<object>)
  //
  // Some human readable ``name'' for this function.
  constant slot function-name :: <byte-string>,
    required-init-keyword: function-name:;
  //
  // The general entry point for this function.  The actual address of the
  // C function.
  slot general-entry :: <raw-pointer>,
    required-init-keyword: general-entry:;
  //
  constant slot function-specializers :: <simple-object-vector>,
    required-init-keyword: specializers:;
  //
  constant slot function-rest? :: <boolean>,
    required-init-keyword: rest?:;
  //
  constant slot function-keywords :: type-union(<simple-object-vector>, <false>),
    required-init-keyword: keywords:;
  //
  constant slot function-all-keys? :: <boolean>,
    required-init-keyword: all-keys?:;
  //
  constant slot function-values :: <simple-object-vector>,
    required-init-keyword: values:;
  //
  constant slot function-rest-value :: <type>,
    required-init-keyword: rest-value:;
end;
  
define sealed domain make (singleton(<function>));
define sealed domain initialize (<function>);



define abstract class <closure> (<function>)
  //
  slot closure-var :: <object>,
    init-value: #f,
    sizer: closure-size, size-init-value: 0, size-init-keyword: closure-size:;
end;

define sealed domain make(singleton(<closure>));



define class <raw-function> (<function>)
end;

define sealed domain make(singleton(<raw-function>));



define class <callback-function> (<function>)
  //
  // The C callback entry point for this method.
  slot callback-entry :: <raw-pointer>,
    required-init-keyword: callback-entry:;
  constant slot callback-signature :: <byte-string>,
    required-init-keyword: callback-signature:;
end;

define sealed domain make(singleton(<callback-function>));



define class <raw-closure> (<raw-function>, <closure>)
end;

define sealed domain make(singleton(<raw-closure>));


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


define class <callback-closure> (<callback-function>, <closure>)
end;

define sealed domain make(singleton(<callback-closure>));

define method make-closure
    (func :: <callback-function>, closure-size :: <integer>)
    => res :: <callback-closure>;
  let closure
    = make(<callback-closure>,
	   function-name: func.function-name,
	   general-entry: func.general-entry,
	   specializers: func.function-specializers,
	   rest?: func.function-rest?,
	   keywords: func.function-keywords,
	   all-keys?: func.function-all-keys?,
	   values: func.function-values,
	   rest-value: func.function-rest-value,
	   closure-size: closure-size,
	   callback-entry: func.callback-entry,
	   callback-signature: func.callback-signature);
  closure.callback-entry
    := call-out("make_trampoline", ptr:,
		ptr: func.callback-entry,
		object: closure,
		int: func.callback-signature.size,
		ptr: %%primitive(vector-elements, func.callback-signature));
  closure;
end;


define class <method> (<function>)
  //
  // The generic-function entry point for this method.
  constant slot generic-entry :: <raw-pointer>,
    required-init-keyword: generic-entry:;
end;

define sealed domain make(singleton(<method>));



define class <dynamic-method> (<method>)
  //
  // The static method to call to do the actual work.
  constant slot real-method :: <function>, required-init-keyword: real-method:;
end class <dynamic-method>;

define sealed domain make (singleton(<dynamic-method>));



define class <accessor-method> (<method>)
  //
  // The <slot-descriptor> this is the acccessor of.
  constant slot accessor-slot :: <slot-descriptor>,
    required-init-keyword: slot:;
end class <accessor-method>;

define sealed domain make(singleton(<accessor-method>));



define class <method-closure> (<method>, <closure>)
end;

define sealed domain make(singleton(<method-closure>));

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

define sealed domain make (singleton(<type-vector>));
define sealed domain initialize (<type-vector>);

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
  slot cached-method :: false-or(<method>) = #f;
  slot cached-next-info :: <list>, init-value: #();
  slot cached-valid-keywords :: type-union(<simple-object-vector>,
					   one-of(#f, #"all")),
    init-value: #f;
  constant slot cached-classes :: <type-vector>,
    required-init-keyword: #"classes";
  slot next :: type-union(<false>, <gf-cache>), init-value: #f;
  slot call-count :: <integer>, init-value: 1;
end class <gf-cache>;

define sealed domain make(singleton(<gf-cache>));
define sealed domain initialize (<gf-cache>);

define class <generic-function> (<function>)
  //
  slot generic-function-methods :: <list> = #(),
    init-keyword: methods:;
  //
  slot method-cache :: type-union(<false>, <gf-cache>) = #f;
end;

define sealed domain make(singleton(<generic-function>));

// make{singleton(<generic-function>)} -- exported GF method.
//
// Make a generic function after massaging the arguments.
// 
define method make (class == <generic-function>, #next next-method,
		    #key name :: <byte-string> = "Anonymous Generic Function",
		         required :: type-union(<integer>, <sequence>),
		         rest? :: <boolean>, key :: false-or(<collection>),
		         all-keys? :: <boolean>, values :: <sequence> = #[],
		         rest-value :: type-union(<type>, <false>)
		           = <object>)
    => res :: <generic-function>;
  let specializers
    = select (required by instance?)
	<integer> =>
	  make(<simple-object-vector>, size: required, fill: <object>);
	<simple-object-vector> =>
	  required;
	<sequence> =>
	  as(<simple-object-vector>, required);
      end select;
  for (specializer in specializers)
    check-type(specializer, <type>);
  end for;
  let keywords
    = select (key by instance?)
	<false> => #f;
	<simple-object-vector> => key;
	<sequence> => as(<simple-object-vector>, key);
	<collection> =>
	  let keywords = make(<simple-object-vector>, size: key.size);
	  for (keyword in key, index from 0)
	    keywords[index] := keyword;
	  end for;
	  keywords;
      end select;
  if (keywords)
    for (keyword in keywords)
      check-type(keyword, <symbol>);
    end for;
  end if;
  if (rest? & keywords)
    error("Generic functions cannot have both #rest and #key parameters.");
  end if;
  if (all-keys? & ~keywords)
    error("Cannot specify #all-keys without #key.");
  end if;
  let values :: <simple-object-vector> = as(<simple-object-vector>, values);
  for (value in values)
    check-type(value, <type>);
  end for;
  next-method(<generic-function>,
	      function-name: name,
	      general-entry: %%primitive(main-entry, gf-call),
	      specializers: specializers,
	      rest?: rest?,
	      keywords: keywords,
	      all-keys?: all-keys?,
	      values: values,
	      rest-value: rest-value);
end method make;


// Runtime method construction and handling.

define method %make-method
    (specializers :: <simple-object-vector>,
     fixed-values :: <simple-object-vector>,
     rest-value :: <type>,
     function :: <method>)
    => result :: <method>;
  local method lassert (condition :: <boolean>) => ();
	  condition | lose("internal assertion inside %make-method failed.");
	end method lassert;
  lassert(specializers.size == function.function-specializers.size);
  do(compose(lassert, subtype?), specializers, function.function-specializers);
  lassert(fixed-values.size == function.function-values.size);
  do(compose(lassert, subtype?), fixed-values, function.function-values);
  lassert(subtype?(rest-value, function.function-rest-value));
  make(<dynamic-method>,
       function-name: function.function-name,
       general-entry: %%primitive(main-entry, general-call),
       specializers: specializers,
       rest?: function.function-rest?,
       keywords: function.function-keywords,
       all-keys?: function.function-all-keys?,
       values: fixed-values,
       rest-value: rest-value,
       generic-entry: %%primitive(main-entry, generic-call),
       real-method: function);
end method %make-method;

define constant generic-call
  = method (self :: <dynamic-method>, nargs :: <integer>, next-info :: <list>)
      let arg-ptr :: <raw-pointer> = %%primitive(extract-args, nargs);
      %%primitive(invoke-generic-entry, self.real-method, next-info,
		  %%primitive(pop-args, arg-ptr));
    end method;


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

// add-method -- exported from Dylan.
//
// Add the method to the generic function, and return the new method and
// any method we happened to displace.  Flames out if the method is not
// congruent with the generic function.
// 
define method add-method
    (gf :: <generic-function>, new :: <method>)
    => (new :: <method>, old :: false-or(<method>));
  //
  // First, check that the method is congruent.
  //
  // The number of specialiers must be the same.
  unless (new.function-specializers.size == gf.function-specializers.size)
    error("%= isn't congruent with %= because %= takes %d required "
	    "argument%s but %= takes %d",
	  new, gf,
	  new, new.function-specializers.size,
	  if (new.function-specializers.size == 1) "" else "s" end,
	  gf, gf.function-specializers.size);
  end unless;
  //
  // Each method specializer must be a subtype of the corresponding
  // gf specialier.
  for (index from 0 below new.function-specializers.size)
    let meth-spec = new.function-specializers[index];
    let gf-spec = gf.function-specializers[index];
    unless (subtype?(meth-spec, gf-spec))
      error("%= isn't congruent with %= because specializer %= isn't a "
	      "subtype of %=",
	    new, gf, meth-spec, gf-spec);
    end unless;
  end for;
  //
  // If the gf takes keywords,
  if (gf.function-keywords)
    //
    // The method must take keywords.
    unless (new.function-keywords)
      error("%= isn't congruent with %= because %= accepts keyword arguments "
	      "but %= does not.",
	    new, gf, gf, new);
    end unless;
    //
    // And must recognize all the mandatory keywords.
    for (keyword in gf.function-keywords)
      unless (member?(keyword, new.function-keywords))
	error("%= isn't congruent with %= because %= does not recognize the "
		"mandatory keyword %=",
	      new, gf, new, keyword);
      end unless;
    end for;
  //
  // The gf does not accept keywords, so the method can't either.
  elseif (new.function-keywords)
    error("%= isn't congruent with %= because %= accepts keyword arguments "
	    "but %= does not.",
	  new, gf, new, gf);
  //
  // If the gf accepts a variable number of arguments,
  elseif (gf.function-rest?)
    //
    // The method must also.
    unless (new.function-rest?)
      error("%= isn't congruent with %= because %= accepts a variable number "
	      "of arguments but %= does not.",
	    new, gf, gf, new);
    end unless;
  //
  // The gf does not accept a variable number of arguments, so the method
  // can't either.
  elseif (new.function-rest?)
    error("%= isn't congruent with %= because %= accepts a variable number "
	    "of arguments but %= does not.",
	  new, gf, new, gf);
  end if;
  //
  // If the gf not not contain a rest-value declaration,
  if (subtype?(gf.function-rest-value, <never-returns>))
    //
    // The method must not either.
    unless (subtype?(new.function-rest-value, <never-returns>))
      error("%= isn't congruent with %= because %= contains a rest value "
	      "declaration but %= does not.",
	    new, gf, new, gf);
    end unless;
    //
    // There must be the same number of fixed values.
    unless (new.function-values.size == gf.function-values.size)
      error("%= isn't congruent with %= because %= returns %d exactly value%s "
	      "but %= returns exactly %d.",
	    new, gf,
	    new,
	    if (new.function-values.size == 1) "" else "s" end,
	    new.function-values.size,
	    gf, gf.function-values.size);
    end unless;
    //
    // And each value returned by the method must be a subtype of the value
    // returned by the generic function.
    for (index from 0 below new.function-values.size)
      let meth-spec = new.function-values[index];
      let gf-spec = gf.function-values[index];
      unless (subtype?(meth-spec, gf-spec))
	error("%= isn't congruent with %= because result value %= isn't a "
		"subtype of %=",
	      new, gf, meth-spec, gf-spec);
      end unless;
    end for;
  else
    //
    // The generic contains a rest-value declaration.  The method must return
    // at least as many fixed values as the generic function.
    unless (new.function-values.size >= gf.function-values.size)
      error("%= isn't congruent with %= because %= can return %d value%s but "
	      "%= returns at least %d.",
	    new, gf,
	    new,
	    if (new.function-values.size == 1) "" else "s" end,
	    new.function-values.size,
	    gf, gf.function-values.size);
    end unless;
    //
    // And the fixed values returned by the method must be subtypes of the
    // corresponding value returned by the generic function.
    for (index from 0 below gf.function-values.size)
      let meth-spec = new.function-values[index];
      let gf-spec = gf.function-values[index];
      unless (subtype?(meth-spec, gf-spec))
	error("%= isn't congruent with %= because result value %= isn't a "
		"subtype of %=",
	      new, gf, meth-spec, gf-spec);
      end unless;
    end for;
    for (index from gf.function-values.size below new.function-values.size)
      let meth-spec = new.function-values[index];
      unless (subtype?(meth-spec, gf.function-rest-value))
	error("%= isn't congruent with %= because result value %= isn't a "
		"subtype of %=",
	      new, gf, meth-spec, gf.function-rest-value);
      end unless;
    end for;
    //
    // And the rest value returned by the method must be a subtype of the
    // rest value returned by the generic function.
    unless (subtype?(new.function-rest-value, gf.function-rest-value))
      error("%= isn't congruent with %= because result value %= isn't a "
	      "subtype of %=",
	    new, gf, new.function-rest-value, gf.function-rest-value);
    end unless;
  end if;
  //
  // Find the old method.
  let (old, prev) = internal-find-method(gf, new.function-specializers);
  //
  // Remove the old method.
  if (old)
    if (prev)
      prev.tail := prev.tail.tail;
    else
      gf.generic-function-methods := gf.generic-function-methods.tail;
    end if;
  end if;
  //
  // Add the new method.
  gf.generic-function-methods := pair(new, gf.generic-function-methods);
  //
  // Clear the cache.
  gf.method-cache := #f;
  //
  // Return the new and old methods.
  values(new, old);
end method add-method;

// find-method -- exported from Dylan.
//
// Find the method matching the given signature and return it.
// 
define method find-method
    (gf :: <generic-function>, specializers :: <sequence>)
    => meth :: false-or(<method>);
  //
  // Convert the specialiers into a simple object vector so we can more
  // effeciently access it.
  let specializers :: <simple-object-vector>
    = as(<simple-object-vector>, specializers);
  //
  // Verify that the correct number of specialiers was supplied.
  unless (gf.function-specializers.size == specializers.size)
    error("Wrong number of specializers in find-method.  %= has %d required "
	    "argument%s, but %d specializers were supplied.",
	  gf, gf.function-specializers.size, specializers.size);
  end unless;
  //
  // Find and return the method.
  internal-find-method(gf, specializers);
end method find-method;

// internal-find-method -- internal
//
// Used by add-method and find-method to find a method that matches the
// given specialiers.  Returns that method and the previous pair so that
// add-method can remove it.
// 
define method internal-find-method
    (gf :: <generic-function>, specializers :: <simple-object-vector>)
    => (result :: false-or(<method>), prev :: false-or(<pair>));
  block (return)
    for (prev = #f then remaining,
	 remaining = gf.generic-function-methods then remaining.tail,
	 until: remaining == #())
      let old :: <method> = remaining.head;
      block (no-match)
	for (old-spec :: <type> in old.function-specializers,
	     new-spec :: <type> in specializers)
	  unless (old-spec == new-spec
		    | (subtype?(old-spec, new-spec)
			 & subtype?(new-spec, old-spec)))
	    no-match();
	  end unless;
	end for;
	return(old, prev);
      end block;
    end for;
    values(#f, #f);
  end block;  
end method internal-find-method;

// remove-method -- exported from Dylan.
//
// Remove the method from the generic function.  Flame out if it isn't there.
// 
define method remove-method
    (gf :: <generic-function>, meth :: <method>)
    => meth :: <method>;
  //
  // Find the method.
  block (return)
    for (prev = #f then remaining,
	 remaining = gf.generic-function-methods then remaining.tail,
	 until: remaining == #())
      let old :: <method> = remaining.head;
      //
      // Remove it.
      if (old == meth)
	if (prev)
	  prev.tail := remaining.tail;
	else
	  gf.generic-function-methods := remaining.tail;
	end if;
      end if;
      //
      // Clear the cache.
      gf.method-cache := #f;
      //
      // Return the removed method.
      return(meth);
    end for;
    //
    // It wasn't found, so flame out.
    error("%= does not contain %=", gf, meth);
  end block;
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
	  %element(res, index) := %%primitive(extract-arg, arg-ptr, index);
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
  mv-call(function,
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
      let arg-ptr :: <raw-pointer> = %%primitive(extract-args, nargs);
      for (index :: <integer> from 0,
	   specializer :: <type> in specializers)
	check-type(%%primitive(extract-arg, arg-ptr, index), specializer);
      end for;
      if (keywords)
	if (self.function-all-keys?)
	  for (index :: <integer> from nfixed below nargs by 2)
	    check-type(%%primitive(extract-arg, arg-ptr, index), <symbol>);
	  end for;
	else
	  for (index :: <integer> from nfixed below nargs by 2)
	    let key :: <symbol> = %%primitive(extract-arg, arg-ptr, index);
	    unless (member?(key, keywords))
	      unrecognized-keyword-error(key);
	    end unless;
	  end for;
	end if;
      end if;
      %%primitive(invoke-generic-entry, self, #(),
		  %%primitive(pop-args, arg-ptr));
    end method;


// Generic function dispatch.

// gf-call and gf-call-lookup -- magic.
//
// The compiler uses gf-call for the general-entry to all generic functions.
// Most uses will now be inlined to direct calls after doing a gf-call-lookup.
// These two functions must be kept in sync with the calls to them from
// compiler/cback/cback.dylan.
// 
define function gf-call-lookup 
    (self :: <generic-function>, 
     nargs :: <integer>, 
     source-location :: <source-location>)
 => (meth :: <method>, next :: <list>);
  let specializers = self.function-specializers;
  let nfixed = specializers.size;
  if (nfixed ~== nargs)
    if (self.function-rest? | self.function-keywords)
      if (nargs < nfixed)
        wrong-number-of-arguments-error(#f, nfixed, nargs, 
                                        source-location: source-location);
      end;
      if (self.function-keywords & odd?(nargs - nfixed))
        odd-number-of-keyword/value-arguments-error
          (source-location: source-location);
      end;
    else
      wrong-number-of-arguments-error(#t, nfixed, nargs,
                                      source-location: source-location);
    end;
  end;
  let arg-ptr :: <raw-pointer> = %%primitive(extract-args, nargs);
  let (meth, next-info, valid-keywords)
    = cached-sorted-applicable-methods(self, nfixed, arg-ptr);
  if (nfixed ~== nargs)
    case 
      (valid-keywords == #f) => #t;
      (valid-keywords == #"all") =>
        for (index :: <integer> from nfixed below nargs by 2)
          check-type(%%primitive(extract-arg, arg-ptr, index), <symbol>);
        end for;
      otherwise =>
        check-type(valid-keywords, <simple-object-vector>);
        for (index :: <integer> from nfixed below nargs by 2)
          let key :: <symbol> = %%primitive(extract-arg, arg-ptr, index);
          block (found)
            for (i :: <integer> from 0 below valid-keywords.size)
              if (%element(valid-keywords, i) == key) found() end if;
            end for;
            error("Unrecognized keyword: %=", key);
          end block;
        end for;
    end case;
  end if;
  if (meth)
    values(meth, next-info);
  elseif (next-info.empty?)
    for (index :: <integer> from 0 below nfixed)
      let specializer :: <type> = %element(specializers, index);
      let arg = %%primitive(extract-arg, arg-ptr, index);
      %check-type(arg, specializer, source-location);
    end;
    no-applicable-methods-error
      (self, mv-call(vector, %%primitive(pop-args, arg-ptr)),
       source-location: source-location);
  else
    // There is no unambiguous "first method"
    ambiguous-method-error(next-info.first,
                           source-location: source-location);
  end if;
end function;

// This now not usually used.  Instead inline C code with the same effect
// is generated in emit-assignment in compiler/cback/chack.dylan
//
define function gf-call (self :: <generic-function>, nargs :: <integer>)
  // clever bit: gf-call-lookup and the called function reuse the same
  // arguments on the orig_sp stack
  let (meth, next-info) = gf-call-lookup(self, nargs, $unknown-source-location);
  %%primitive(invoke-generic-entry, meth, next-info, 
              %%primitive(pop-args, %%primitive(extract-args, nargs)));
end function;

// gf-call-one-arg -- magic.
//
// The compiler uses this for the general-entry to certain simple generic
// functions.  The functions must accept no keywords and take exactly one
// argument.
//
define constant gf-call-one-arg
  = method (self :: <generic-function>, nargs :: <integer>)
      if (nargs ~= 1)
       wrong-number-of-arguments-error(#t, 1, nargs);
      end;
      let arg-ptr :: <raw-pointer> = %%primitive(extract-args, nargs);
      let (meth, next-info, valid-keywords)
       = one-arg-sorted-applicable-methods(self, arg-ptr);
      if (meth)
	%%primitive(invoke-generic-entry, meth, next-info,
		    %%primitive(pop-args, arg-ptr));
      elseif (next-info.empty?)
	let specializers = self.function-specializers;                         
	let specializer :: <type> = %element(specializers, 0);
	let arg = %%primitive(extract-arg, arg-ptr, 0);
	%check-type(arg, specializer, $unknown-source-location);
	no-applicable-methods-error
	  (self, mv-call(vector, %%primitive(pop-args, arg-ptr)));
      else
	// There is no unambiguous "first method"
	ambiguous-method-error(next-info.first);
      end if;
    end method;

define method internal-sorted-applicable-methods
    (gf :: <generic-function>, nargs :: <integer>,
     arg-ptr :: <raw-pointer>)
    => (meth :: false-or(<method>), next-method-info :: <list>, 
	valid-keywords
	  :: type-union(<simple-object-vector>, one-of(#f, #"all")));

  // We have to use low-level stuff here.  It would be bad to do a full
  // generic function call at this point.
  let cache-classes = make(<type-vector>, size: nargs);
  for (i :: <integer> from 0 below nargs)
    %element(cache-classes, i)
      := (%%primitive(extract-arg, arg-ptr, i)).object-class;
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
	  select (%compare-methods(meth, other, arg-ptr))
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
	    select (%compare-methods(meth, remaining.head, arg-ptr))
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
  // In the special case that we have a simple dispatch on one argument, we
  // can use a blazingly fast dispatch function
  if (old-cache == #f & valid-keywords == #f & ~gf.function-rest?
       & cache.simple & nargs == 1)
    gf.general-entry := %%primitive(main-entry, gf-call-one-arg);
  elseif (~cache.simple)
    gf.general-entry := %%primitive(main-entry, gf-call);
  end if;

  gf.method-cache := cache;
  cache.next := old-cache;
  if (ordered.empty?)
    cache.cached-method := #f;
    if (ambiguous.empty?)
      cache.cached-next-info := #();
    else
      cache.cached-next-info := list(ambiguous);
    end if;
  else
    cache.cached-method := ordered.head;
    unless (ambiguous.empty?)
      for (prev :: <pair> = ordered then remaining,
	   remaining :: <list> = ordered.tail then remaining.tail,
	   until: remaining == #())
      finally
	prev.tail := list(ambiguous);
      end for;
    end unless;
    cache.cached-next-info := ordered.tail;
  end if;

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

  values(cache.cached-method, cache.cached-next-info,
	 cache.cached-valid-keywords);
end;

define variable *debug-generic-threshold* :: <integer> = -1;

define inline method cached-sorted-applicable-methods
    (gf :: <generic-function>, nargs :: <integer>,
     arg-ptr :: <raw-pointer>)
 => (function :: false-or(<method>),
     next-method-info :: <list>, 
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
	    let arg = %%primitive(extract-arg, arg-ptr, index);
	    unless (type == arg.object-class) no-match() end unless;
	  end for;
	else
	  for (index :: <integer> from 0 below nargs)
	    let type :: <type> = %element(classes, index);
	    let arg = %%primitive(extract-arg, arg-ptr, index);
	    case
	      // The easy check for matches
	      (type == arg.object-class) => #t;
	      // The above check will have caught all class matches, so
	      // this must not be a match.
	      (instance?(type, <class>)) => no-match();
	      // Singletons are the next most common case, so optimize them.
	      (instance?(type, <singleton>)) =>
		// Trust me.  This special case *does* speed things up.
		unless (arg == type.singleton-object) no-match() end unless;
	      // instance? is slow, but its the last resort
	      otherwise =>
		unless (instance?(arg, type)) no-match() end unless;
	    end case;
	  end for;
	end if;

	if (prev)	// not the first entry -- make it so.
	  prev.next := cache.next;
	  cache.next := gf.method-cache;
	  gf.method-cache := cache;
	end if;

	return(cache.cached-method, cache.cached-next-info,
	       cache.cached-valid-keywords);
      end block;
    end for;
    internal-sorted-applicable-methods(gf, nargs, arg-ptr);
  end block;
end method cached-sorted-applicable-methods;

define inline method one-arg-sorted-applicable-methods
    (gf :: <generic-function>, arg-ptr :: <raw-pointer>)
 => (function :: false-or(<method>),
     next-method-info :: <list>, 
     valid-keywords :: type-union(<simple-object-vector>, one-of(#f, #"all")));
  let arg-class = (%%primitive(extract-arg, arg-ptr, 0)).object-class;
  let cache = gf.method-cache;
  if (~cache)
    internal-sorted-applicable-methods(gf, 1, arg-ptr);
  elseif (%element(cache.cached-classes, 0) == arg-class)
    values(cache.cached-method, cache.cached-next-info, #f);
  else
    block (return)
      for (prev :: <gf-cache> = cache then cache,
          cache :: type-union(<false>, <gf-cache>) = cache.next
            then cache.next,
          until: cache == #f)
       block (no-match)
         let cache :: <gf-cache> = cache; // A hint to the type system.

         let class = %element(cache.cached-classes, 0);
         unless (class == arg-class) no-match() end unless;

         prev.next := cache.next;
         cache.next := gf.method-cache;
         gf.method-cache := cache;
         return(cache.cached-method, cache.cached-next-info, #f);
       end block;
      end for;
      internal-sorted-applicable-methods(gf, 1, arg-ptr);
    end block;
  end if;
end method one-arg-sorted-applicable-methods;

define method internal-applicable-method?
    (meth :: <method>, arg-ptr :: <raw-pointer>, cache :: <gf-cache>)
 => (res :: <boolean>);
  block (return)
    let classes :: <type-vector> = cache.cached-classes;
    for (specializer :: <type> in meth.function-specializers,
	 index :: <integer> from 0)
      let arg-type = classes[index];
      let arg :: <object> = %%primitive(extract-arg, arg-ptr, index);

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
		 // (specializer == <byte-character>) =>
		 // The following code works around a bug in the current
		 // system.  It seems that there is one "<byte-character>"
		 // constant per library, so the equality test above tends to
		 // fail.
		 (instance?(specializer, <byte-character-type>)) =>
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
		 elseif (instance?(specializer, <byte-character-type>))
		   <non-byte-character>;
		 elseif (arg-type = <class>)
		   // This special case is designed to catch "make".
		   // We don't want to create a <none-of> type with
		   // hundreds of exceptions, and are fairly confident
		   // that <class> has a finite number of instances.
		   // Since the latter will *not* be true in general, we
		   // can't generalize further.
		   singleton(arg);
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

define method %compare-methods
    (meth1 :: <method>, meth2 :: <method>, arg-ptr :: <raw-pointer>)
    => res :: one-of(#"more-specific", #"less-specific", #"ambiguous",
		     #"identical");
  block (return)
    let specializers1 = meth1.function-specializers;
    let specializers2 = meth2.function-specializers;
    //
    // Start out assuming that the they are identical.
    let result = #"identical";
    //
    // Compare each specializer.
    for (index :: <integer> from 0 below specializers1.size)
      let spec1 :: <type> = specializers1[index];
      let spec2 :: <type> = specializers2[index];
      block (next)
	let cmp = if (subtype?(spec1, spec2))
		    if (subtype?(spec2, spec1))
		      // The two specializers are identical, so they impose
		      // no additional ordering constraints.
		      next();
		    else
		      // Spec1 is more specific, so the result is more-specific
		      // or ambiguous.
		      #"more-specific";
		    end if;
		  elseif (subtype?(spec2, spec1))
		    // Spec2 is more specific, so the result is less-specific
		    // or ambiguous.
		    #"less-specific";
		  else
		    // The specializers intersect (otherwise we wouldn't have
		    // selected both) but neither is a subtype of the other.
		    // Defer to compare-overlapping-specializers to figure
		    // out their ordering.
		    compare-overlapping-specializers
		      (spec1, spec2, %%primitive(extract-arg, arg-ptr, index));
		  end if;
	// Now merge the constrain imposed by this position with the constrain
	// accumulated from the positions we have already looked at.
	if (result == #"identical")
	  // The order is currently unconstrained, so just use this positions
	  // ordering.
	  result := cmp;
	elseif (cmp == #"more-specific" | cmp == #"less-specific")
	  // This position imposes an ordering, so check to see if that melds
	  // with the constrain previously accumulated.
	  if (result == cmp)
	    // Same as it ever was, so don't change anything now.
	    #f;
	  elseif (result == #"ambiguous")
	    // So far we haven't established an ordering, so use this ordering.
	    result := cmp;
	  else
	    // We've got us an inconsistent ordering from two different
	    // positions, the overall result is ambiguous.
	    return(#"ambiguous");
	  end if;
	end if;
      end block;
    end for;
    result;
  end block;
end method %compare-methods;


// This is the same as %compare-methods above except it uses a
// sequence of arguments instead of a <raw-pointer>
define method compare-methods
    (meth1 :: <method>, meth2 :: <method>, arguments :: <sequence>)
    => res :: one-of(#"more-specific", #"less-specific", #"ambiguous",
		     #"identical");
  block (return)
    let specializers1 = meth1.function-specializers;
    let specializers2 = meth2.function-specializers;
    //
    // Start out assuming that the they are identical.
    let result = #"identical";
    //
    // Compare each specializer.
    for (index :: <integer> from 0 below specializers1.size)
      let spec1 :: <type> = specializers1[index];
      let spec2 :: <type> = specializers2[index];
      block (next)
	let cmp = if (subtype?(spec1, spec2))
		    if (subtype?(spec2, spec1))
		      // The two specializers are identical, so they impose
		      // no additional ordering constraints.
		      next();
		    else
		      // Spec1 is more specific, so the result is more-specific
		      // or ambiguous.
		      #"more-specific";
		    end if;
		  elseif (subtype?(spec2, spec1))
		    // Spec2 is more specific, so the result is less-specific
		    // or ambiguous.
		    #"less-specific";
		  else
		    // The specializers intersect (otherwise we wouldn't have
		    // selected both) but neither is a subtype of the other.
		    // Defer to compare-overlapping-specializers to figure
		    // out their ordering.
		    compare-overlapping-specializers
		      (spec1, spec2, arguments[index]);
		  end if;
	// Now merge the constrain imposed by this position with the constrain
	// accumulated from the positions we have already looked at.
	if (result == #"identical")
	  // The order is currently unconstrained, so just use this positions
	  // ordering.
	  result := cmp;
	elseif (cmp == #"more-specific" | cmp == #"less-specific")
	  // This position imposes an ordering, so check to see if that melds
	  // with the constrain previously accumulated.
	  if (result == cmp)
	    // Same as it ever was, so don't change anything now.
	    #f;
	  elseif (result == #"ambiguous")
	    // So far we haven't established an ordering, so use this ordering.
	    result := cmp;
	  else
	    // We've got us an inconsistent ordering from two different
	    // positions, the overall result is ambiguous.
	    return(#"ambiguous");
	  end if;
	end if;
      end block;
    end for;
    result;
  end block;
end method compare-methods;

define method compare-overlapping-specializers
    (spec1 :: <type>, spec2 :: <type>, arg :: <object>)
    => res :: one-of(#"more-specific", #"less-specific", #"ambiguous");
  #"ambiguous";
end method compare-overlapping-specializers;

define method compare-overlapping-specializers
    (spec1 :: <class>, spec2 :: <class>, arg :: <object>)
    => res :: one-of(#"more-specific", #"less-specific", #"ambiguous");
  compare-overlapping-classes(spec1, spec2, arg.object-class);
end method compare-overlapping-specializers;

define method compare-overlapping-specializers
    (spec1 :: <subclass>, spec2 :: <subclass>, arg :: <object>)
    => res :: one-of(#"more-specific", #"less-specific", #"ambiguous");
  compare-overlapping-classes(spec1.subclass-of, spec2.subclass-of, arg);
end method compare-overlapping-specializers;

define method compare-overlapping-classes
    (spec1 :: <class>, spec2 :: <class>, arg-class :: <class>)
    => res :: one-of(#"more-specific", #"less-specific", #"ambiguous");
  block (return)
    for (super :: <class> in arg-class.all-superclasses)
      if (super == spec1)
	return(#"more-specific");
      elseif (super == spec2)
	return(#"less-specific");
      end;
    end;
    lose("Specializer not in the CPL even though the method was applicable?");
  end block;
end method compare-overlapping-classes;


// Accessor methods.

define constant general-rep-getter
  = method (self :: <accessor-method>, nargs :: <integer>, next-info :: <list>)
      // We don't specify a return type because we want to use the
      // unknown-values convention.
      let arg-ptr :: <raw-pointer> = %%primitive(extract-args, 1);
      let instance = %%primitive(extract-arg, arg-ptr, 0);
      %%primitive(pop-args, arg-ptr);
      let slot = self.accessor-slot;
      let posn = find-slot-offset(instance.object-class, slot);
      let result = %%primitive(ref-slot, instance, #"general", posn);
      unless (%%primitive(initialized?, result))
	uninitialized-slot-error(slot, instance);
      end unless;
      result;
    end;

define constant general-rep-setter
  = method (self :: <accessor-method>, nargs :: <integer>, next-info :: <list>)
      // We don't specify a return type because we want to use the
      // unknown-values convention.
      let arg-ptr :: <raw-pointer> = %%primitive(extract-args, 2);
      let new-value = %%primitive(extract-arg, arg-ptr, 0);
      let instance = %%primitive(extract-arg, arg-ptr, 1);
      %%primitive(pop-args, arg-ptr);
      let posn = find-slot-offset(instance.object-class, self.accessor-slot);
      %%primitive(set-slot, new-value, instance, #"general", posn);
      new-value;
    end;


define constant heap-rep-getter
  = method (self :: <accessor-method>, nargs :: <integer>, next-info :: <list>)
      // We don't specify a return type because we want to use the
      // unknown-values convention.
      let arg-ptr :: <raw-pointer> = %%primitive(extract-args, 1);
      let instance = %%primitive(extract-arg, arg-ptr, 0);
      %%primitive(pop-args, arg-ptr);
      let slot = self.accessor-slot;
      let posn = find-slot-offset(instance.object-class, slot);
      let result = %%primitive(ref-slot, instance, #"heap", posn);
      unless (%%primitive(initialized?, result))
	uninitialized-slot-error(slot, instance);
      end unless;
      result;
    end;

define constant heap-rep-setter
  = method (self :: <accessor-method>, nargs :: <integer>, next-info :: <list>)
      // We don't specify a return type because we want to use the
      // unknown-values convention.
      let arg-ptr :: <raw-pointer> = %%primitive(extract-args, 2);
      let new-value = %%primitive(extract-arg, arg-ptr, 0);
      let instance = %%primitive(extract-arg, arg-ptr, 1);
      %%primitive(pop-args, arg-ptr);
      let posn = find-slot-offset(instance.object-class, self.accessor-slot);
      %%primitive(set-slot, new-value, instance, #"heap", posn);
      new-value;
    end;


// Applicability predicates.

define method applicable-method?
    (meth :: <method>, #rest sample-arguments)
 => res :: <boolean>;
  block (return)
    let (initial-state, limit, next-state, finished-state?, current-key,
	 current-element) = forward-iteration-protocol(sample-arguments);
    for (specializer :: <type> in meth.function-specializers,
	 state = initial-state then next-state(sample-arguments, state))
      if (finished-state?(sample-arguments, state, limit))
	return(#f);
      else
	let arg = current-element(sample-arguments, state);
	if (~instance?(arg, specializer))
	  return(#f);
	end if;
      end if;
    finally
      // Matched all required arguments; look for keywords or #rest
      //
      if (finished-state?(sample-arguments, state, limit))
	#t;
      elseif (meth.function-keywords | meth.function-all-keys?)
	for(state = state then next-state(sample-arguments, state),
	    keyword? = #t then ~keyword?,
	    until: finished-state?(sample-arguments, state, limit))
	  if (keyword?)
	    let key = current-element(sample-arguments, state);
	    if (meth.function-all-keys?)
	      unless (instance?(key, <symbol>))
		return(#f);
	      end;
	    else
	      unless (member?(key, meth.function-keywords))
		return (#f);
	      end;
	    end if;
	  end if;
	finally
	  if(keyword?)
	    #f;
	  else
	    #t;
	  end;
	end for;
      elseif (meth.function-rest?)
	#t;
      end if;
    end for;
  end block;
end method;

define method applicable-method?
    (gf :: <generic-function>, #rest sample-arguments)
  any?(method(meth :: <method>)
	   apply(applicable-method?, meth, sample-arguments);
       end,
       generic-function-methods(gf));
end;

// This is similar to internal-sorted-applicable-methods except that
// it operates on a sequence rather than a <raw-pointer>.
//
define method sorted-applicable-methods
    (gf :: <generic-function>, #rest sample-arguments)
    => (sorted :: <list>, unsorted :: <list>);

  let nargs = gf.function-specializers.size;
  if(sample-arguments.size < nargs)
    error("Not enough sample arguments in sorted-applicable-methods");
  end if;
  
  // Ordered accumulates the methods we can tell the ordering of.  Each
  // element in this list is a method.
  let ordered :: <list> = #();

  // Ambiguous accumulates the set of methods of which it is unclear which
  // follows next after ordered.  These methods will all be mutually ambiguous.
  let ambiguous :: <list> = #();

  for (meth :: <method> in gf.generic-function-methods)
    if (apply(applicable-method?, meth, sample-arguments))
      block (done-with-method)
	for (remaining :: <list> = ordered then remaining.tail,
	     prev :: false-or(<pair>) = #f then remaining,
	     until: remaining == #())
	  //
	  // Grab the method to compare this method against.
	  let other = remaining.head;
	  select (compare-methods(meth, other, sample-arguments))
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
	    select (compare-methods(meth, remaining.head, sample-arguments))
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


// #next method handling

define movable method %make-next-method-cookie
    (next-method-info :: <list>, original-args :: <simple-object-vector>)
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
      %%primitive(invoke-generic-entry,
		  next-method-info.head, next-method-info.tail,
		  values-sequence(args));
    end method;
  end if;
end method %make-next-method-cookie;


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
    mv-call(function,
	    values-sequence(curried-args),
	    values-sequence(more-args));
  end;
end;

define inline method rcurry (function :: <function>, #rest curried-args)
    => res :: <function>;
  method (#rest more-args)
    mv-call(function,
	    values-sequence(more-args),
	    values-sequence(curried-args));
  end;
end;

define inline method always (object :: <object>) => res :: <function>;
  method (#rest ignore) => object :: <object>;
    object;
  end;
end;
