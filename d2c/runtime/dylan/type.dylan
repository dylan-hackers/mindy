rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/type.dylan,v 1.10 1995/12/16 04:25:29 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

// <type> -- exported from Dylan
//
// Abstract superclass of all the various kinds of types.
//
define abstract class <type> (<object>)
end;

// Seal make and initialize for types.
// 
seal generic make (singleton(<type>));
seal generic initialize (<type>);

// limited -- exported from Dylan.
//
// Generic interface to constraining a type in some way.
// 
define generic limited (type :: <type>, #key) => res :: <type>;

// instance? -- exported from Dylan.
//
// The compiler automatically converts visible calls to instance? to calls
// to %instance? (or something better, if possible).  But we still need
// a definition for instance? for uses in non-visible calls.
//
define movable method instance? (object :: <object>, type :: <type>)
    => res :: <boolean>;
  %instance?(object, type);
end;

// %instance? -- internal.
//
// DO NOT CALL THIS.  Call instance? instead.  Otherwise you will just succeed
// in hiding the check from the compiler and keeping it from being able to
// optimize it at all.
//
define movable generic %instance? (object :: <object>, type :: <type>)
    => res :: <boolean>;

// subtype? -- exported from Dylan.
// 
define movable generic subtype? (type1 :: <type>, type2 :: <type>)
    => res :: <boolean>;
//
// We can't tell just by dispatching off of type1 (or some other method would
// have been applicable).  So try dispatching off of type2.
//
define method subtype? (type1 :: <type>, type2 :: <type>)
    => res :: <boolean>;
  subtype?-type2-dispatch(type1, type2);
end;

// subtype?-type2-dispatch -- internal.
//
// Used to dispatch off the second type.  Methods on this should have type1
// just be <type>.  This generic function is necessary to avoid having
// ambiguous methods.
// 
define movable generic subtype?-type2-dispatch
    (type1 :: <type>, type2 :: <type>)
    => res :: <boolean>;
//
// And if nobody pipes up when dispatching off of type2 then the two types
// must be unrelated.
//
define method subtype?-type2-dispatch (type1 :: <type>, type2 :: <type>)
    => res :: <boolean>;
  #f;
end;


// Singletons.

// <singleton> -- exported from Dylan.
// 
define class <singleton> (<type>)
  //
  // The object this is the singleton type of.
  slot singleton-object :: <object>, setter: #f,
    required-init-keyword: object:;
end;

seal generic make (singleton(<singleton>));

// singleton -- exported from Dylan.
//
define inline method singleton (object :: <object>)
    => res :: <singleton>;
  make(<singleton>, object: object);
end;

// one-of -- exported from Extensions.
//
define method one-of (#rest things) => res :: <type>;
  apply(type-union, map(singleton, things));
end;

// instance? -- exported generic function method.
//
// An object is instance? a singleton iff it is (i.e. ==) the singleton's
// object.
// 
define method %instance? (object, type :: <singleton>)
    => res :: <boolean>;
  object == type.singleton-object;
end;

// subtype? -- exported generic function method.
//
// A singleton is only subtype? some other type if the singleton's object
// is instance? the other type.
// 
define method subtype? (type1 :: <singleton>, type2 :: <type>)
    => res :: <boolean>;
  instance?(type1.singleton-object, type2);
end;


// Unions

// <union> -- internal
//
// <union>s are used to represent the union of an arbitrary number of types
// if that union can't be represented directly.
// 
// Actually, <union> types are sorta exported because the constructor is
// exposed, but the <union> class isn't.
//
define class <union> (<type>)
  //
  // List of non-singleton types that make up this union type.  None of these
  // types will be subtype? any of the others and <union> types will be
  // expanded.
  slot union-members :: <list>, setter: #f,
    init-value: #(), init-keyword: members:;
  //
  // List of the singletons that are part of this union type.  Note: these are
  // the actual objects, not the singleton types.
  slot union-singletons :: <list>, setter: #f,
    init-value: #(), init-keyword: singletons:;
end;

seal generic make (singleton(<union>));

// type-union -- exported from Dylan.
//
// N-ary constructor for <union> types.
// 
define method type-union (#rest types)
  //
  // First scan across the types using merge-type to build up the members
  // and singletons list.
  let members = #();
  let singletons = #();
  for (type :: <type> in types)
    let (new-members, new-singletons) = merge-type(members, singletons, type);
    members := new-members;
    singletons := new-singletons;
  end;
  //
  // Now that we have the canonical set of members and singletons, check
  // to see if we actually need to make a union type.
  if (singletons == #())
    if (~(members == #()) & members.tail == #())
      members.head;
    else
      make(<union>, members: members);
    end;
  else
    if (members == #() & singletons.tail == #())
      make(<singleton>, object: singletons.head);
    else
      make(<union>, members: members, singletons: singletons);
    end;
  end;
end;

// merge-type -- internal generic function.
//
// Merge type into the members and singletons lists taking care to combine
// types wherever possible.
// 
define generic merge-type
    (members :: <list>, singletons :: <list>, type :: <type>)
    => (members :: <list>, singletons :: <list>);

// merge-type(<type>) -- internal generic function method.
//
// Nothing special is known about type, so just merge it in.
//
define method merge-type
    (members :: <list>, singletons :: <list>, type :: <type>)
    => (members :: <list>, singletons :: <list>);
  block (return)
    local
      //
      // This local method is used to iterate across the members.
      method scan-members (remaining, prev)
	if (remaining == #())
	  // If we hit the end of the list, then just add this type to the
	  // list of members.
	  pair(type, members);
	else
	  let member = remaining.head;
	  if (subtype?(type, member))
	    // The type is a subtype of one of the pre-existing members, so we
	    // don't have to add anything, and there will be nothing to
	    // remove because we will already have removed any possible
	    // subtypes.  So just return what we started with.
	    return(members, singletons);
	  elseif (subtype?(member, type))
	    // This pre-existing member is a subtype of the type we are adding.
	    // So splice it out and keep going.
	    if (prev)
	      prev.tail := remaining.tail;
	    else
	      members := remaining.tail;
	    end;
	    scan-members(remaining.tail, prev);
	  else
	    // Neither is a subtype of the other, so keep them both.
	    scan-members(remaining.tail, remaining);
	  end;
	end if;
      end method,
      //
      // And this local method is used to iterate over the singletons.  This
      // is only called after we have decided to add the type to the members.
      // So the only thing we have to do is remove useless singletons.
      method scan-singletons (remaining, prev)
	if (remaining == #())
	  // We be done.
	  singletons;
	elseif (instance?(remaining.head, type))
	  // This singleton is an instance of the type, so splice it out.
	  if (prev)
	    prev.tail := remaining.tail;
	  else
	    singletons := remaining.tail;
	  end;
	  scan-singletons(remaining.tail, prev);
	else
	  scan-singletons(remaining.tail, remaining);
	end;
      end method;
    values(scan-members(members, #f), scan-singletons(singletons, #f));
  end;
end;

// merge-type(<singleton>) -- internal generic function method.
//
// Just call merge-singleton on the singleton's object.
//
define method merge-type
    (members :: <list>, singletons :: <list>, type :: <singleton>)
    => (members :: <list>, singletons :: <list>);
  merge-singleton(members, singletons, type.singleton-object);
end;

// merge-type(<union>) -- internal generic function method.
//
// Merge all the members and singletons of this union one at a time.
//
define method merge-type
    (members :: <list>, singletons :: <list>, type :: <union>)
    => (members :: <list>, singletons :: <list>);
  // Merge the members.
  for (member :: <type> in type.union-members)
    let (new-members, new-singletons)
      = merge-type(members, singletons, member);
    members := new-members;
    singletons := new-singletons;
  end;
  // Merge the singletons.
  for (singleton in type.union-singletons)
    let (new-members, new-singletons)
      = merge-singleton(members, singletons, singleton);
    members := new-members;
    singletons := new-singletons;
  end;
  values(members, singletons);
end;

// merge-singleton -- internal function.
//
// Auxiliary routine used by the singleton and union merge-type methods.  If
// the object is a instance of any of the members or is already in the
// singletons, blow it off.  Otherwise, add it.
//
define method merge-singleton
    (members :: <list>, singletons :: <list>, object :: <object>)
    => (members :: <list>, singletons :: <list>);
  if (any?(curry(instance?, object), members) | member?(object, singletons))
    values(members, singletons);
  else
    values(members, pair(object, singletons));
  end;
end;

// false-or(<type>) -- exported from extensions.
//
// Shorthand for type-union(type, <false>).
//
define inline method false-or (type :: <type>) => res :: <type>;
  type-union(type, <false>);
end method false-or;

// instance?(<object>,<union>) -- exported generic function method.
//
// Something is an instance of a union if it is an instance of any of the
// member types or one of the singletons.
//
define method %instance? (object :: <object>, type :: <union>)
    => res :: <boolean>;
  block (return)
    for (member in type.union-members)
      if (instance?(object, member)) return(#t) end if;
    finally
      member?(object, type.union-singletons);
    end for;
  end block;
end;

// subtype?(<union>,<type>) -- exported generic function method.
//
// A union is subtype some other type if all the members are subtype that
// other type and all the singletons are instances that other type.
//
define method subtype? (type1 :: <union>, type2 :: <type>)
    => res :: <boolean>;
  every?(method (t) subtype?(t, type2) end, type1.union-members)
    & every?(method (t) instance?(t, type2) end, type1.union-singletons);
end;
  
// subtype?-type2-dispatch(<type>,<union>) -- exported generic function method
//
// A type is a subtype of a union if it is a subtype of any of the member
// types.  We can ignore the singletons because the only thing that can
// subtype a singleton type is the same singleton type and the
// <singleton>,<type> branch is picked off by the singleton code up
// above.
// 
define method subtype?-type2-dispatch (type1 :: <type>, type2 :: <union>)
    => res :: <boolean>;
  any?(method (t) subtype?(type1, t) end, type2.union-members);
end;


// Limited integers.

// <limited-integer> -- internal
//
// <Limited-integer> types are used to represent some subrange of one
// of the integer classes.
//
// Like unions, limited integers are sorta exported because the constructor
// for them is exported.
//
define class <limited-integer> (<type>)
  slot limited-integer-base-class :: <class>, setter: #f,
    required-init-keyword: base-class:;
  slot limited-integer-minimum :: false-or(<integer>), setter: #f,
    required-init-keyword: min:;
  slot limited-integer-maximum :: false-or(<integer>), setter: #f,
    required-init-keyword: max:;
end;

seal generic make (singleton(<limited-integer>));

// limited(<integer>,...), limited(<extended-integer>) -- exported gf methods
//
define method limited (class :: one-of(<integer>, <extended-integer>),
		       #key min, max)
    => res :: <type>;
  if (max & min & max < min)
    // If the range is empty, return the empty type.
    type-union();
  else
    // Otherwise, make a limited integer.
    make(<limited-integer>, base-class: class, min: min, max: max);
  end;
end;

// limited(<fixed-integer>,...) -- exported generic function method.
//
define method limited (class == <fixed-integer>, #key min, max)
    => res :: <type>;
  block (return)
    // Convert the min into a fixed-integer if possible.
    let min = select (min by instance?)
		<false> => $minimum-fixed-integer;
		<fixed-integer> => min;
		<extended-integer> =>
		  if (min > $maximum-fixed-integer)
		    return(type-union());
		  elseif (min <= $minimum-fixed-integer)
		    $minimum-fixed-integer;
		  else
		    as(<fixed-integer>, min);
		  end;
	      end;
    // Likewise for the max.
    let max = select (max by instance?)
		<false> => $maximum-fixed-integer;
		<fixed-integer> => max;
		<extended-integer> =>
		  if (max < $minimum-fixed-integer)
		    return(type-union());
		  elseif (max >= $maximum-fixed-integer)
		    $maximum-fixed-integer;
		  else
		    as(<fixed-integer>, max);
		  end;
	      end;
    if (max < min)
      // Empty range.
      type-union();
    else
      // Otherwise, make the limited integer.
      make(<limited-integer>, base-class: <fixed-integer>, min: min, max: max);
    end;
  end;
end;

// instance?(<object>,<limited-integer>) -- exported generic function method
//
// Only integers are instance? a limited integer, and they have their own
// method.
// 
define method %instance? (object :: <object>, type :: <limited-integer>)
    => res :: <boolean>;
  #f;
end;

// instance?(<integer>,<limited-integer>) -- exported generic function method
//
// Make sure the integer is the right kind of integer and that it is in the
// given range.
//
define method %instance? (object :: <integer>, type :: <limited-integer>)
    => res :: <boolean>;
  if (instance?(object, type.limited-integer-base-class))
    let min = type.limited-integer-minimum;
    if (~min | object >= min)
      let max = type.limited-integer-maximum;
      ~max | object <= max;
    else
      #f;
    end;
  else
    #f;
  end;
end;

// The general case is too slow (since it must do generic dispatch), but so is
// next-method.  Therefore we duplicate the preceding routine *twice* in order
// to get up to a factor of 20 speedup.
//
define method %instance?
    (object :: <fixed-integer>, type :: <limited-integer>)
 => res :: <boolean>;
  let base-class :: <class> = type.limited-integer-base-class;
  if (base-class == <fixed-integer>)
    // This is appallingly messy, but until we get a better type inference
    // mechanism, it seems to be the only way to get a fast enough inner loop.
    let min = type.limited-integer-minimum;
    let above-bottom :: <boolean>
      = if (min)
	  let min :: <fixed-integer> = min;
	  object >= min;
	else
	  #t;
	end if;
    if (above-bottom)
      let max = type.limited-integer-maximum;
      if (max)
	let max :: <fixed-integer> = max;
	object <= max;
      else
	#t;
      end if;
    end if;
  else
    if (instance?(object, type.limited-integer-base-class))
      let min = type.limited-integer-minimum;
      if (~min | object >= min)
	let max = type.limited-integer-maximum;
	~max | object <= max;
      else
	#f;
      end;
    else
      #f;
    end;
  end;
end;

// subtype?(<limited-integer>,<type>) -- exported generic function method.
//
// Unless one of the other methods steps in, a limited integer is only
// a subtype of some other type if the integers base class is a subtype
// of it.
//
define method subtype? (type1 :: <limited-integer>, type2 :: <type>)
    => res :: <boolean>;
  subtype?(type1.limited-integer-base-class, type2);
end;

// subtype?(<limited-integer>,<limited-integer>) -- exported gf method
//
// One limited integer is a subtype of another limited integer if the
// base class of the first is a subtype of the base class of the second
// and if the range of the first is a subrange of the second.
//
define method subtype? (type1 :: <limited-integer>, type2 :: <limited-integer>)
    => res :: <boolean>;
  if (subtype?(type1.limited-integer-base-class,
	       type2.limited-integer-base-class))
    let min1 = type1.limited-integer-minimum;
    let min2 = type2.limited-integer-minimum;
    if (~min2 | (min1 & min1 >= min2))
      let max1 = type1.limited-integer-maximum;
      let max2 = type2.limited-integer-maximum;
      ~max2 | (max1 & max1 <= max2);
    else
      #f;
    end;
  else
    #f;
  end;
end;

// intersect-limited-ints(<limited-integer>,<limited-integer>) -- internal
//
// Returns either a new limited integer type containing all elements common to
// lim1 and lim2, or false if there are no common elements.
//
define method intersect-limited-ints
    (lim1 :: <limited-integer>, lim2 :: <limited-integer>)
 => (res :: type-union(<false>, <limited-integer>));
  block (return)
    let base1 = lim1.limited-integer-base-class;
    let base2 = lim2.limited-integer-base-class;
    let base = case
		 (base1 == base2) => base1;
		 (base1 == <integer>) => base2;
		 (base2 == <integer>) => base1;
		 otherwise => return(#f);
	       end case;

    let min1 = lim1.limited-integer-minimum;
    let min2 = lim2.limited-integer-minimum;
    let max1 = lim1.limited-integer-maximum;
    let max2 = lim2.limited-integer-maximum;

    let new-min = case
		    ~min1 => min2;
		    ~min2 => min1;
		    otherwise => max(min1, min2);
		  end case;
    let new-max = case
		    ~max1 => max2;
		    ~max2 => max1;
		    otherwise => min(max1, max2);
		  end case;

    if (new-min & new-max & new-max < new-min)
      #f;
    else
      make(<limited-integer>, base-class: base, min: new-min, max: new-max);
    end if;
  end block;
end method intersect-limited-ints;

// restrict-limited-ints(<limited-integer>,<limited-integer>) -- internal.
//
// Return a new limited integer type containing the portion of lim1 which
// contains value but contains no instances of lim2.  Lim1 may be either an
// "<integer>" class or a limited integer type.  We require, as a
// precondition, that value not be an instance of lim2.
//
define method restrict-limited-ints
    (value :: <integer>, lim1 :: <type>, lim2 :: <limited-integer>)
 => (res :: <limited-integer>);
  let (min1, max1)
    = case
	(instance?(lim1, <limited-integer>)) =>
	  values(lim1.limited-integer-minimum, lim1.limited-integer-maximum);
	(subtype?(lim1, <integer>)) =>
	  values(#f, #f);
	otherwise =>
	  error("restrict-limited-ints must be passed two integer types.");
      end case;
  let min2 = lim2.limited-integer-minimum;
  let max2 = lim2.limited-integer-maximum;

  let min = if ((~min2 | min2 < value) & (~min1 | min1 < max2))
	      max2 + 1;
	    else
	      min1;
	    end if;
  let max = if ((~max2 | max2 > value) & (~max1 | max1 > min2))
	      min2 - 1;
	    else
	      max1;
	    end if;
  make(<limited-integer>, base-class: lim2.limited-integer-base-class,
       min: min, max: max);
end method restrict-limited-ints;


// The <byte-character> type.

// <byte-character-type> -- internal.
//
// This is the class of the <byte-character> type.
//
define class <byte-character-type> (<type>)
end;

seal generic make (singleton(<byte-character-type>));

// <byte-character> -- exported from Extensions.
//
// The set of all characters that can be represented by a byte.
//
define constant <byte-character> = make(<byte-character-type>);

// <non-byte-character> -- internal.
//
// We need this special-case type to handle generic function caching for
// character types.
//
define constant <non-byte-character>
  = restrict-type(<character>, <byte-character>);

// instance? -- exported generic function method.
//
// The only instances of <byte-character> are characters that have a character
// code of less than 256.
//
define method %instance? (object, type :: <byte-character-type>)
    => res :: <boolean>;
  #f;
end;
//
define method %instance? (object :: <character>, type :: <byte-character-type>)
    => res :: <boolean>;
  as(<fixed-integer>, object) < 256;
end;

// subtype? -- exported generic funciton method.
//
// In general, <byte-character> is only subtype? some other type if <character>
// is subtype? that other type.  The one exception is <byte-character> itself.
//
// We don't need a subtype?-type2-dispatch method because the only subtypes
// of <byte-character> are singletons, and they are already delt with.
//
define method subtype? (type1 :: <byte-character-type>, type2 :: <type>)
    => res :: <boolean>;
  subtype?(<character>, type2);
end;
//
define method subtype?
    (type1 :: <byte-character-type>, type2 :: <byte-character-type>)
    => res :: <boolean>;
  #t;
end;


// Negation-types

// <none-of> -- internal
//
// This is a funky sort of special purpose type.  It represents objects which
// are instances of a given "base type" but not members of any of a set of
// excluded subtypes.  This concept probably has no conceivable use outside of
// generic function dispatch.
//
define class <none-of> (<type>)
  slot base-type :: <type>, required-init-keyword: #"base";
  slot excluded-types :: <type-vector>,
    required-init-keyword: #"excluded";
end;

seal generic make (singleton(<none-of>));

// restrict-types -- internal.
//
// return a version of "type" which excludes the given value.  If type
// is already a none_of type, simply extend it, else return a new <none-of>
// type.
// 
define method restrict-type (base :: <type>, exclude :: <type>);
  if (instance?(base, <none-of>))
    let old-excluded = base.excluded-types;
    let new-excluded = make(<type-vector>, size: old-excluded.size + 1,
			    fill: exclude);
    for (i :: <fixed-integer> from 0 below old-excluded.size)
      new-excluded[i] := old-excluded[i];
    end for;
    base.excluded-types := new-excluded;
    base;
  else
    make(<none-of>, base: base,
	 excluded: make(<type-vector>, size: 1, fill: exclude));
  end if;
end method restrict-type;

// instance?(<object>,<none-of>) -- exported generic function method.
//
// Something is an instance of a union if it is an instance of any of the
// member types or one of the singletons.
//
define method %instance? (object :: <object>, type :: <none-of>)
    => res :: <boolean>;
//  instance?(object, type.base-type) &
//    ~any?(curry(instance?, object), type.excluded-types));
  block (return)
    instance?(object, type.base-type)
      & for (t :: <type> in type.excluded-types)
	  if (instance?(object, t)) return(#f) end if;
	finally #t;
	end for;
  end block;
end;

// subtype?(<none-of>,<type>) -- exported generic function method.
//
// A type negation is subtype some other type if it's base is a subtype.
// This case is straigtforward, but the (<type>, <none-of>) method is just too
// hair to implement, so we won't.
//
define method subtype? (type1 :: <none-of>, type2 :: <type>)
    => res :: <boolean>;
  subtype?(type1.base-type, type2);
end;
  

// Overlap? -- internal.
//
// This function returns true if it is possible to create an object which is a
// member of both of the given types.  In case of doubt, it is permissible to
// return a spurious true value.
//
define generic overlap? (type1 :: <type>, type2 :: <type>) => res :: <boolean>;

define method overlap? (type1 :: <class>, type2 :: <class>)
 => res :: <boolean>;
  local method sub-overlap (cls :: <class>) => res :: <boolean>;
	   subtype?(cls, type2) | any?(sub-overlap, direct-subclasses(cls));
	end method sub-overlap;
  sub-overlap(type1);
end method overlap?;

define method overlap? (type1 :: <class>, type2 :: <type>) => res :: <boolean>;
  overlap?(type2, type1);
end method overlap?;

define method overlap?
    (type1 :: <singleton>, type2 :: <type>) => res :: <boolean>;
  subtype?(type1, type2);
end method overlap?;

/* ### not absolutly needed

define method overlap? (type1 :: <subclass>, type2 :: <type>)
 => res :: <boolean>;
  local method check-subclass-instances (class :: <class>) => res :: <boolean>;
	  instance?(class, type2)
	    | any?(check-subclass-instances, direct-subclasses(class));
	end method check-subclass-instances;
  check-subclass-instances(type1.subclass-of);
end method overlap?;
*/

define method overlap?
    (type1 :: <limited-integer>, type2 :: <limited-integer>)
 => (res :: <boolean>);
  let min1 = type1.limited-integer-minimum;
  let min2 = type2.limited-integer-minimum;
  let max1 = type1.limited-integer-maximum;
  let max2 = type2.limited-integer-maximum;

  case
    (~overlap?(type1.limited-integer-base-class,
	       type2.limited-integer-base-class)) => #f;
    (max1 & min2 & (max1 > min2)) => #f;
    (max2 & min1 & (max2 > min1)) => #f;
    otherwise => #t;
  end case;
end method overlap?;

define method overlap? (type1 :: <limited-integer>, type2 :: <class>)
 => res :: <boolean>;
  overlap?(type1.limited-integer-base-class, type2);
end method overlap?;

define method overlap? (type1 :: <limited-integer>, type2 :: <type>)
 => res :: <boolean>;
  overlap?(type2, type1);
end method overlap?;

define method overlap? (type1 :: <union>, type2 :: <type>) => res :: <boolean>;
  any?(method (t) overlap?(t , type2) end, type1.union-members)
    | any?(method (t) instance?(t, type2) end, type1.union-singletons);
end method overlap?;

define method overlap?
    (type1 :: <none-of>, type2 :: <type>) => res :: <boolean>;
  overlap?(type1.base-type, type2);
end method overlap?;

define method overlap? (type1 :: <type>, type2 :: <type>) => res :: <boolean>;
  // Default.  If nothing else catches the argument, then there is no overlap.
  #f;
end method overlap?;

