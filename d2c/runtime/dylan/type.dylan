module: dylan

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

// instance? and subtype? -- exported from Dylan.
//
// The two exported type predicates.
//
define generic instance? (object :: <object>, type :: <type>)
    => res :: <boolean>;
define generic subtype? (type1 :: <type>, type2 :: <type>)
    => res :: <boolean>;

// subtype?-type2-dispatch -- internal.
//
// Used to dispatch off the second type.  Methods on this should have type1
// just be <type>.  This generic function is necessary to avoid having
// ambiguous methods.
// 
define generic subtype?-type2-dispatch (type1 :: <type>, type2 :: <type>)
    => res :: <boolean>;
//
// We can't tell just by dispatching off of type1 (or some other method would
// have been applicable).  So try dispatching off of type2.
//
define method subtype? (type1 :: <type>, type2 :: <type>)
    => res :: <boolean>;
  subtype?-type2-dispatch(type1, type2);
end;
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
  apply(type-or, map(singleton, things));
end;

// instance? -- exported generic function method.
//
// An object is instance? a singleton iff it is (i.e. ==) the singleton's
// object.
// 
define method instance? (object, type :: <singleton>)
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

// union -- exported from Dylan.
//
// Cheesy binary constructor for <union> types that is overloaded on top of
// a collection routine.  Man, sometimes Apple can really fuck up.
//
define sealed inline method union (type1 :: <type>, type2 :: <type>, #key)
    => res :: <type>;
  type-or(type1, type2);
end;

// type-or -- exported from Extensions.
//
// N-ary constructor for <union> types.
// 
define method type-or (#rest types)
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

// instance?(<object>,<union>) -- exported generic function method.
//
// Something is an instance of a union if it is an instance of any of the
// member types or one of the singletons.
//
define method instance? (object :: <object>, type :: <union>)
    => res :: <boolean>;
  any?(curry(instance?, object), type.union-members)
    | member?(object, type.union-singletons);
end;

// subtype?(<union>,<type>) -- exported generic function method.
//
// A union is subtype some other type if all the members are subtype that
// other type and all the singletons are instances that other type.
//
define method subtype? (type1 :: <union>, type2 :: <type>)
    => res :: <boolean>;
  every?(rcurry(subtype?, type2), type1.union-members)
    & every?(rcurry(instance?, type2), type1.union-singletons);
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
  any?(curry(subtype?, type1), type2.union-members);
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
    type-or();
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
		    return(type-or());
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
		    return(type-or());
		  elseif (max >= $maximum-fixed-integer)
		    $maximum-fixed-integer;
		  else
		    as(<fixed-integer>, max);
		  end;
	      end;
    if (max < min)
      // Empty range.
      type-or();
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
define method instance? (object :: <object>, type :: <limited-integer>)
    => res :: <boolean>;
  #f;
end;

// instance?(<integer>,<limited-integer>) -- exported generic function method
//
// Make sure the integer is the right kind of integer and that it is in the
// given range.
//
define method instance? (object :: <integer>, type :: <limited-integer>)
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

// instance? -- exported generic function method.
//
// The only instances of <byte-character> are characters that have a character
// code of less than 256.
//
define method instance? (object, type :: <byte-character-type>)
    => res :: <boolean>;
  #f;
end;
//
define method instance? (object :: <character>, type :: <byte-character-type>)
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
