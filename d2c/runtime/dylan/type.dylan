rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/type.dylan,v 1.11 2003/09/30 17:49:01 gabor Exp $
copyright: see below
module: dylan-viscera

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998 - 2003  Gwydion Dylan Maintainers
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

// <type> -- exported from Dylan
//
// Abstract superclass of all the various kinds of types.
//
define abstract class <type> (<object>)
end;

// Seal make and initialize for types.
// 
define sealed domain make (singleton(<type>));
define sealed domain initialize (<type>);

// limited -- exported from Dylan.
//
// Generic interface to constraining a type in some way.
// 
define generic limited (type :: <type>, #key) => res :: <type>;


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
  // Vector of non-singleton types that make up this union type.  None of these
  // types will be subtype? any of the others and <union> types will be
  // expanded.
  constant slot union-members :: <simple-object-vector>,
    init-value: #[], init-keyword: members:;
  //
  // Vector of the singletons that are part of this union type.  Note: these are
  // the actual objects, not the singleton types.
  constant slot union-singletons :: <simple-object-vector>,
    init-value: #[], init-keyword: singletons:;
end;

define sealed domain make (singleton(<union>));

// type-union -- exported from Dylan.
//
// N-ary constructor for <union> types.
// 
define method type-union (#rest types)
  //
  // First scan across the types using merge-type to build up the members
  // and singletons list.
  let members :: <list> = #();
  let singletons :: <list> = #();
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
      make(<union>, members: as(<simple-object-vector>, members));
    end;
  else
    if (members == #() & singletons.tail == #())
      make(<singleton>, object: singletons.head);
    else
      make(<union>, members: as(<simple-object-vector>, members),
	   singletons: as(<simple-object-vector>, singletons));
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
	  if (%subtype?(type, member))
	    // The type is a subtype of one of the pre-existing members, so we
	    // don't have to add anything, and there will be nothing to
	    // remove because we will already have removed any possible
	    // subtypes.  So just return what we started with.
	    return(members, singletons);
	  elseif (%subtype?(member, type))
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



// <direct-instance> -- internal.
//
// A <direct-instance> represents the direct instances of a class.  In other
// words, anything for which foo.object-class == target-class.
//
// Exposed because the constructor is exported.
// 
define class <direct-instance> (<type>)
  //
  // The class this is the direct instances of.
  constant slot direct-instance-of :: <class>, required-init-keyword: of:;
end class <direct-instance>;

define sealed domain make (singleton(<direct-instance>));

// direct-instance -- exported.
//
// Construct and return a direct-instance type for the given class.
// 
define inline method direct-instance (of :: <class>)
    => res :: <direct-instance>;
  make(<direct-instance>, of: of);
end method direct-instance;


// <subclass> -- internal
//
// A <subclass> represents all of the subclasses of a particular class,
// conceptually the same as:
//   apply(type-union, map(singleton,all-subclasses(class)))
// assuming a definition for all-subclasses.  And that no new classes are
// created after the subclass type is.
//
// Exposed because the constructor is exported.
// 
define class <subclass> (<type>)
  //
  // The class this is the subclasses of.
  constant slot subclass-of :: <class>, required-init-keyword: of:;
end;

define sealed domain make (singleton(<subclass>));

// subclass -- exported.
//
// Construct and return the corresponding subclass type.
//
define inline method subclass (of :: <class>) => res :: <subclass>;
  make(<subclass>, of: of);
end;


// <singleton> -- exported from Dylan.
// 
define class <singleton> (<type>)
  //
  // The object this is the singleton type of.
  slot singleton-object :: <object>, setter: #f,
    required-init-keyword: object:;
end;

define sealed domain make (singleton(<singleton>));

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


// <limited-type> -- internal
//
// The superclass of all limited types (i.e. integer and collection)
//
define class <limited-type> (<type>)
  constant slot limited-integer-base-class :: <class>, 
    required-init-keyword: #"base-class";
end class <limited-type>;

// <limited-integer> -- internal
//
// <Limited-integer> types are used to represent some subrange of one
// of the integer classes.
//
// Like unions, limited integers are sorta exported because the constructor
// for them is exported.
//
define class <limited-integer> (<limited-type>)
  slot limited-integer-minimum :: false-or(<general-integer>), setter: #f,
    required-init-keyword: min:;
  slot limited-integer-maximum :: false-or(<general-integer>), setter: #f,
    required-init-keyword: max:;
end;

define sealed domain make (singleton(<limited-integer>));

// limited(<general-integer>,...), limited(<extended-integer>)
//   -- exported gf methods
//
define method limited (class :: one-of(<general-integer>, <extended-integer>),
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

// limited(<integer>,...) -- exported generic function method.
//
define method limited (class == <integer>, #key min, max)
    => res :: <type>;
  block (return)
    // Convert the min into a fixed-integer if possible.
    let min = select (min by instance?)
		<false> => $minimum-integer;
		<integer> => min;
		<extended-integer> =>
		  if (min > $maximum-integer)
		    return(type-union());
		  elseif (min <= $minimum-integer)
		    $minimum-integer;
		  else
		    as(<integer>, min);
		  end;
	      end;
    // Likewise for the max.
    let max = select (max by instance?)
		<false> => $maximum-integer;
		<integer> => max;
		<extended-integer> =>
		  if (max < $minimum-integer)
		    return(type-union());
		  elseif (max >= $maximum-integer)
		    $maximum-integer;
		  else
		    as(<integer>, max);
		  end;
	      end;
    if (max < min)
      // Empty range.
      type-union()
    elseif (max = min)
      // Singleton range.
      singleton(min)
    else
      // Otherwise, make the limited integer.
      make(<limited-integer>, base-class: <integer>, min: min, max: max)
    end;
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
		 (base1 == <general-integer>) => base2;
		 (base2 == <general-integer>) => base1;
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
// "<general-integer>" class or a limited integer type.  We require, as a
// precondition, that value not be an instance of lim2.
//
define method restrict-limited-ints
    (value :: <general-integer>, lim1 :: <type>, lim2 :: <limited-integer>)
 => (res :: <limited-integer>);
  let (min1, max1)
    = case
	(instance?(lim1, <limited-integer>)) =>
	  values(lim1.limited-integer-minimum, lim1.limited-integer-maximum);
	(%subtype?(lim1, <general-integer>)) =>
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

// <bit> -- exported from Extensions

define /* exported */ constant <bit>
  = limited(<integer>, min: 0, max: 1);


// <limited-collection> -- internal
//
// <limited-collection> types are used to represent a collection with
// restricted size and/or element type.
//
define class <limited-collection> (<limited-type>)
  constant slot limited-element-type :: <type> = #f,
    init-keyword: #"of";
  constant slot limited-size-restriction :: false-or(<integer>) = #f,
    init-keyword: #"size";
  constant slot limited-dimensions :: false-or(<sequence>) = #f,
    init-keyword: #"dimensions";
end;

define sealed domain make (singleton(<limited-collection>));

define open generic element-type
    (object :: <object>) => (type :: <type>, indefinite? :: <boolean>);

define sealed inline method element-type
    (class :: one-of(<collection>, <explicit-key-collection>,
		     <mutable-collection>, <stretchy-collection>,
		     <mutable-explicit-key-collection>, <sequence>,
		     <mutable-sequence>, <table>, <object-table>, <array>,
		     <vector>, <simple-vector>, <stretchy-vector>, <deque>,
		     <stretchy-vector>))
 => (type :: <type>, indefinite? :: <boolean>);
  values(<object>, #f);
end method element-type;

define method element-type
    (class :: <class>) => (type :: <type>, indefinite? :: <boolean>);
  values(<object>, #f);
end method element-type;

define method element-type
    (class :: subclass(<range>)) => (type :: <type>, indefinite? :: <boolean>);
  values(<real>, #t);
end method element-type;

define method element-type
    (class :: subclass(<string>))
 => (type :: <type>, indefinite? :: <boolean>);
  values(<character>, #t);
end method element-type;

define sealed inline method element-type
    (class == <byte-string>) => (type :: <type>, indefinite? :: <boolean>);
  values(<byte-character>, #f);
end method element-type;

define sealed inline method element-type
    (class == <unicode-string>) => (type :: <type>, indefinite? :: <boolean>);
  values(<character>, #f);
end method element-type;

define sealed inline method element-type
    (object :: <limited-collection>)
 => (type :: <type>, indefinite? :: <boolean>);
  values(object.limited-element-type, #f);
end method element-type;

define method element-type (type :: <collection>)
 => (type :: <type>, indefinite? :: <boolean>);
  element-type(type.object-class);
end method element-type;

define sealed inline method limited
    (class == <collection>, #key of = <object>, size) => (result :: <type>);
  make(<limited-collection>, base-class: class, of: of, size: size);
end method limited;

define sealed inline method limited
    (class == <explicit-key-collection>, #key of = <object>, size)
 => (result :: <type>);
  make(<limited-collection>, base-class: class, of: of, size: size);
end method limited;

define sealed inline method limited
    (class == <mutable-collection>, #key of = <object>, size)
 => (result :: <type>);
  make(<limited-collection>, base-class: class, of: of, size: size);
end method limited;

define sealed inline method limited
    (class == <stretchy-collection>, #key of = <object>, size)
 => (result :: <type>);
  make(<limited-collection>, base-class: class, of: of, size: size);
end method limited;

define sealed inline method limited
    (class == <mutable-explicit-key-collection>, #key of = <object>, size)
 => (result :: <type>);
  make(<limited-collection>, base-class: class, of: of, size: size);
end method limited;

define sealed inline method limited
    (class == <sequence>, #key of = <object>, size) => (result :: <type>);
  make(<limited-collection>, base-class: class, of: of, size: size);
end method limited;

define sealed inline method limited
    (class == <mutable-sequence>, #key of = <object>, size)
 => (result :: <type>);
  make(<limited-collection>, base-class: class, of: of, size: size);
end method limited;

define sealed inline method limited
    (class == <table>, #key of = <object>) => (result :: <type>);
  make(<limited-collection>, base-class: class, of: of);
end method limited;

define sealed inline method limited
    (class == <object-table>, #key of = <object>) => (result :: <type>);
  make(<limited-collection>, base-class: class, of: of);
end method limited;

define constant size&dimensions-error =
  method () => res :: <never-returns>;
    error("limited(<array>, ...) can't specify both size and dimensions");
  end;

define sealed inline method limited
    (class == <array>, #key of = <object>, size, dimensions)
 => (result :: <type>);
  if (size & dimensions)
    size&dimensions-error();
  end if;
  make(<limited-collection>, base-class: class, of: of, size: size,
       dimensions: dimensions);
end method limited;

define sealed inline method limited
    (class == <vector>, #key of = <object>, size) => (result :: <type>);
  make(<limited-collection>, base-class: class, of: of, size: size);
end method limited;

define sealed inline method limited
    (class == <simple-vector>, #key of = <object>, size) => (result :: <type>);
  make(<limited-collection>, base-class: class, of: of, size: size);
end method limited;

define sealed inline method limited
    (class == <stretchy-vector>, #key of = <object>) => (result :: <type>);
  make(<limited-collection>, base-class: class, of: of);
end method limited;

define sealed inline method limited
    (class == <deque>, #key of = <object>) => (result :: <type>);
  make(<limited-collection>, base-class: class, of: of);
end method limited;

define sealed inline method limited
    (class == <string>, #key of :: subclass(<character>), size)
 => (result :: <type>);
  make(<limited-collection>, base-class: class, of: of, size: size);
end method limited;

define sealed inline method limited
    (class == <range>, #key of = <object>) => (result :: <type>);
  make(<limited-collection>, base-class: class, of: of);
end method limited;


// <byte-character-type> -- internal.
//
// This is the class of the <byte-character> type.
//
define class <byte-character-type> (<type>)
end;

define sealed domain make (singleton(<byte-character-type>));

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


//----------------------------------------------------------------------
// Auxiliary support for limited collections
//----------------------------------------------------------------------
define macro limited-collection-definer
  { define limited-collection ?:name (<vector>) of ?element-type:expression 
      = ?fill:expression }
    => { limited-vector-class(?name, ?element-type, ?fill) }
  { define limited-collection ?:name (<vector>) of ?element-type:expression }
    => { limited-vector-class(?name, ?element-type, #f) }
  { define limited-collection ?:name (<stretchy-vector>) of ?element-type:expression 
      = ?fill:expression }
    => { limited-sv-class(?name, ?element-type, ?fill,
			  "***" ## ?name ## "-internal***") }
  { define limited-collection ?:name (<stretchy-vector>)
      of ?element-type:expression }
    => { limited-sv-class(?name, ?element-type, #f,
			  "***" ## ?name ## "-internal***") }
end macro;

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

define sealed domain make (singleton(<none-of>));

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
    for (i :: <integer> from 0 below old-excluded.size)
      new-excluded[i] := old-excluded[i];
    end for;
    base.excluded-types := new-excluded;
    base;
  else
    make(<none-of>, base: base,
	 excluded: make(<type-vector>, size: 1, fill: exclude));
  end if;
end method restrict-type;


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

// %instance? -- internal gf.
//
// DO NOT CALL THIS.  Call instance? instead.  Otherwise you will just succeed
// in hiding the check from the compiler and keeping it from being able to
// optimize it at all.
//
define movable generic %instance? (object :: <object>, type :: <type>)
    => res :: <boolean>;


// %instance?(<object>,<union>) -- internal gf method.
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

// %instance(<object>,<class>) -- internal gf method.
//
// An object is an instance of a class when the object's class is a subtype?
// of the class.
//
// When the compiler decides to do a runtime type-check, it inserts a call
// to %check-type. After much munging, a certain percentage of those calls
// turn into calls to this method. So this is very a critical code path.
//
// But if '-o inline-instance-checks' has been turned on, the compiler will
// try to use an inline version of this method. You have been warned.
// 
define inline method %instance? (object :: <object>, class :: <class>)
    => res :: <boolean>;
  %subtype?(object.object-class, class);
end;

// %instance?(<object>,<direct-instance>) -- internal gf method.
//
// The object-class must be == the base-class.
// 
define method %instance? (object :: <object>, type :: <direct-instance>)
    => res :: <boolean>;
  object.object-class == type.direct-instance-of;
end method %instance?;

// %instance?(<object>,<singleton> -- internal gf method.
//
// An object is instance? a singleton iff it is (i.e. ==) the singleton's
// object.
// 
define method %instance? (object :: <object>, type :: <singleton>)
    => res :: <boolean>;
  object == type.singleton-object;
end;

// %instance?(<object>,<subclass>) -- internal generic function method.
//
// Nothing but classes (handled below) are instances of subclass types.
//
define method %instance? (object :: <object>, type :: <subclass>)
    => res :: <boolean>;
  #f;
end;

// %instance?(<class>,<subclass>) -- internal generic function method.
//
// A class is a instance of a subclass type iff that class is a subtype of
// of the subclass type's base class.
//
define method %instance? (object :: <class>, type :: <subclass>)
    => res :: <boolean>;
  %subtype?(object, type.subclass-of);
end;

// %instance?(singleton(<byte-character-type>),<subclass>)
//   -- internal generic function method.
//
define method %instance? (object :: <byte-character-type>, type :: <subclass>)
    => res :: <boolean>;
  %subtype?(<character>, type.subclass-of);
end;

// %instance?(<object>,<limited-integer>) -- internal gf method.
//
// Only integers are instance? a limited integer, and they have their own
// method.
// 
define method %instance? (object :: <object>, type :: <limited-integer>)
    => res :: <boolean>;
  #f;
end;

// %instance?(<general-integer>,<limited-integer>) -- internal gf method.
//
// Make sure the integer is the right kind of integer and that it is in the
// given range.
//
define method %instance?
    (object :: <general-integer>, type :: <limited-integer>)
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

// %instance?(<integer>,<limited-integer>) -- internal gf method.
//
// The general case is too slow (since it must do generic dispatch), but so is
// next-method.  Therefore we duplicate the preceding routine *twice* in order
// to get up to a factor of 20 speedup.
//
define method %instance?
    (object :: <integer>, type :: <limited-integer>)
 => res :: <boolean>;
  let base-class :: <class> = type.limited-integer-base-class;
  if (base-class == <integer>)
    // This is appallingly messy, but until we get a better type inference
    // mechanism, it seems to be the only way to get a fast enough inner loop.
    let min = type.limited-integer-minimum;
    let above-bottom :: <boolean>
      = if (min)
	  let min :: <integer> = min;
	  object >= min;
	else
	  #t;
	end if;
    if (above-bottom)
      let max = type.limited-integer-maximum;
      if (max)
	let max :: <integer> = max;
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

// %instance?(<collection>, <limited-collection>) -- internal gf method.
//
define method %instance?
    (object :: <object>, type :: <limited-collection>)
 => (res :: <boolean>);
  block (return)
    unless (instance?(object, type.limited-integer-base-class)) return(#f) end unless;
    unless (object.element-type = type.limited-element-type)
      return(#f);
    end unless;
    if (type.limited-size-restriction)
      if (instance?(object, <stretchy-collection>))	return(#f) end if;
      if (type.limited-size-restriction ~== object.size) return(#f) end if;
    elseif (type.limited-dimensions)
      if (instance?(object, <stretchy-collection>))	return(#f) end if;
      if (type.limited-size-restriction ~== object.size) return(#f) end if;
    end if;
    #t;
  end block;
end method %instance?;

// %instance?(<object>,<byte-character-type>) -- internal gf method.
//
// The only instances of <byte-character> are characters that have a character
// code of less than 256.
//
define method %instance? (object :: <object>, type :: <byte-character-type>)
    => res :: <boolean>;
  #f;
end;

// %instance?(<character>,<byte-character-type>) -- internal gf method.
//
// The only instances of <byte-character> are characters that have a character
// code of less than 256.
//
define method %instance? (object :: <character>, type :: <byte-character-type>)
    => res :: <boolean>;
  as(<integer>, object) < 256;
end;

// %instance?(<object>,<none-of>) -- internal.
//
// Something is an instance of a <none-of> if it is an instance of the base
// type but not an isntance of any of the excluded types.
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

// fast-class-instance?(object :: <object>, type :: <class>) -- exported
//
// for internal use by the compiler. Makes heavy assumptions on the
// structure of classes.
//
define sealed movable flushable inline method fast-class-instance? (object :: <object>, class :: <class>)
    => res :: <boolean>;
  let bucket = class.class-bucket;
//  class1 == class2 |
   %%primitive(fixnum-=, 
	       %element(object.object-class.class-row, bucket), 
	       %element(class.class-row, bucket));
end method fast-class-instance?;



// subtype? -- exported from Dylan.
//
// Returns #t if type1 is a subtype of type2.
// 
define movable generic subtype? (type1 :: <type>, type2 :: <type>)
    => res :: <boolean>;

// subtype?(<type>,<type>) -- exported gf method.
//
// Neither type is a union type, so defer to %subtype?.
//
define inline method subtype? (type1 :: <type>, type2 :: <type>)
    => res :: <boolean>;
  %subtype?(type1, type2);
end method subtype?;

// subtype?(<union>,<type>) -- exported gf method.
//
// A union is subtype some other type if all the members are subtype that
// other type and all the singletons are instances that other type.
//
define method subtype? (type1 :: <union>, type2 :: <type>)
    => res :: <boolean>;
  every?(method (t :: <type>) => res :: <boolean>;
	   %subtype?(t, type2);
	 end,
	 type1.union-members)
    & every?(method (s :: <object>) => res :: <boolean>;
	       instance?(s, type2);
	     end,
	     type1.union-singletons);
end method subtype?;

// subtype?(<type>,<union>) -- exported gf method.
//
// A type is a subtype of a union if it is a subtype of any of the member
// types.  We ignore the singletons, the only type that can be subtype? a
// singleton is the same singleton, which is picked off by the
// <singleton>,<type> method below.
// 
define method subtype? (type1 :: <type>, type2 :: <union>)
    => res :: <boolean>;
  any?(method (t :: <type>) => res :: <boolean>;
	 %subtype?(type1, t)
       end,
       type2.union-members);
end method subtype?;

// subtype?(<singleton>,<type>) -- exported gf method.
//
// A singleton is subtype? of a union if it is an instance of any of the
// union-members or == any of the union-singletons.
// 
define method subtype? (type1 :: <singleton>, type2 :: <union>)
    => res :: <boolean>;
  let object = type1.singleton-object;
  any?(method (t :: <type>) => res :: <boolean>;
	 instance?(object, t);
       end,
       type2.union-members)
    | any?(method (s :: <object>) => res :: <boolean>;
	     object == s;
	   end,
	   type2.union-singletons);
end method subtype?;

// subtype?(<union>,<union>) -- exported gf method.
//
// This method is a conglomeration of the above three methods.  Even if we
// didn't care about the epsilon additional performance this gets us, we would
// have to have a method on <union>,<union> to keep the <type>,<union> and
// <union>,<type> methods from being ambiguous when given two <union>s.
// 
define method subtype? (type1 :: <union>, type2 :: <union>)
    => res :: <boolean>;
  every?(method (t1 :: <type>) => res :: <boolean>;
	   any?(method (t2 :: <type>) => res :: <boolean>;
		  %subtype?(t1, t2)
		end,
		type2.union-members);
	 end,
	 type1.union-members)
    & every?(method (s1 :: <object>) => res :: <boolean>;
	       any?(method (t2 :: <type>) => res :: <boolean>;
		      instance?(s1, t2);
		    end,
		    type2.union-members)
		 | any?(method (s2 :: <object>) => res :: <boolean>;
			  s1 == s2;
			end,
			type2.union-singletons);
	     end,
	     type1.union-singletons);
end method subtype?;

// %subtype? -- internal.
//
// Return #f iff type1 is a subtype of type2.  Neither type will be a union
// type because subtype? deals with unions and then calls %subtype? to handle
// all the other kinds of types.
// 
define movable generic %subtype? (type1 :: <type>, type2 :: <type>)
    => res :: <boolean>;

// %subtype?(<type>,<type>) -- internal.
//
// By default, a type is not subtype? another type.
// 
define method %subtype? (type1 :: <type>, type2 :: <type>)
    => res :: <boolean>;
  #f;
end method %subtype?;

// %subtype(<class>,<class>) -- internal gf method.
//
//<<<<<<< type.dylan
//// The generic case is treated using the type inclusion
//// matrix.
////
//define inline method %subtype? (class1 :: <class>, class2 :: <class>)
//    => res :: <boolean>;
////  class1 == class2 |
//  class1.class-row[class2.class-bucket] == class2.class-row[class2.class-bucket]; 
//=======
// Class1 is a subtype of class2 when class2 is listed in class1's
// all-superclasses.  For effeciency, pick off the case where the two
// classes are ==.
//
// This is speed-critical code--if we're not using inline instance checks
// for open classes, this gets executed for every dynamic type check.
//
// We're doing "packed encoding" type checks, as described by Vitek, et al.
// This would be a *great* algorithm if we tweaked it a bit, followed all
// the recommendations in the paper, and generated inline assembly code.
// As it is, we're calling a Dylan function to do type checks, which is
// extremely suboptimal.
//
// We call %element because we know that we don't need to do any bounds
// checking on class-row. We call the fixnum-= to avoid doing a generic
// function dispatch on \==.
//
// The compiler still inserts bogus typechecks for the arguments to
// fixnum-=. These slow us down by a few instructions, but they don't cause
// any recursion, because <integer> has no subclass, and the compiler
// generates an inline typecheck.
//
define method %subtype? (class1 :: <class>, class2 :: <class>)
    => res :: <boolean>;
  class1 == class2 |
    %%primitive(fixnum-=,
		%element(class1.class-row, class2.class-bucket),
		%element(class2.class-row, class2.class-bucket));
//>>>>>>> 1.4.4.2
end method %subtype?;

// %subtype?(<direct-instance>,<type>) -- internal gf method.
//
// A direct-instance type is a subtype of some type if the direct-instance's
// base class is a subclass of that type.
// 
define method %subtype? (type1 :: <direct-instance>, type2 :: <type>)
    => res :: <boolean>;
  %subtype?(type1.direct-instance-of, type2);
end method %subtype?;

// %subtype?(<direct-instance>,<direct-instance>) -- internal gf method.
//
// A direct-instance type is a subtype of another direct-instance type if the
// base classes are the same.
// 
define method %subtype?
    (type1 :: <direct-instance>, type2 :: <direct-instance>)
    => res :: <boolean>;
  type1.direct-instance-of == type2.direct-instance-of;
end method %subtype?;

// %subtype? -- internal.
//
// A singleton is only subtype? some other type if the singleton's object
// is instance? the other type.
// 
define method %subtype? (type1 :: <singleton>, type2 :: <type>)
    => res :: <boolean>;
  instance?(type1.singleton-object, type2);
end method %subtype?;

// %subtype?(<subclass>,<type>) -- internal gf method.
//
// Unless some more specific method is applicable, a subclass type is a subtype
// of some other type iff the base class's metaclass is a subtype of that
// other type.  We assume that <class> is the only kind of metaclass in
// existance, though.
//
define inline method %subtype? (type1 :: <subclass>, type2 :: <type>)
    => res :: <boolean>;
  %subtype?(<class>, type2);
end;

// %subtype?(<subclass>,<subclass>) -- internal gf method.
//
// One subclass type is a subtype of another subclass type if the first
// one's root class is a subclass of the second one's root class.
//
define method %subtype? (type1 :: <subclass>, type2 :: <subclass>)
    => res :: <boolean>;
  %subtype?(type1.subclass-of, type2.subclass-of);
end;

// %subtype?(<limited-type>,<type>) -- internal.
//
// Unless one of the other methods steps in, a limited type is only a subtype
// of some other type its base class is a subtype of it.
//
define method %subtype? (type1 :: <limited-type>, type2 :: <type>)
    => res :: <boolean>;
  %subtype?(type1.limited-integer-base-class, type2);
end;

// %subtype?(<limited-integer>,<limited-integer>) -- internal.
//
// One limited integer is a subtype of another limited integer if the
// base class of the first is a subtype of the base class of the second
// and if the range of the first is a subrange of the second.
//
define method %subtype?
    (type1 :: <limited-integer>, type2 :: <limited-integer>)
    => res :: <boolean>;
  if (%subtype?(type1.limited-integer-base-class,
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

// %subtype?(<limited-collection>, <limited-collection>) -- internal.
//
// One limited collection is a subtype of another if their element types are
// equal, the first collection's base type is a subtype of the second's, and
// their sizes are "compatible".
//
define method %subtype?
    (type1 :: <limited-collection>, type2 :: <limited-collection>)
    => res :: <boolean>;
  if (%subtype?(type1.limited-integer-base-class, type2.limited-integer-base-class)
	& type1.limited-element-type = type2.limited-element-type)
    if (type2.limited-dimensions)
      type1.limited-dimensions = type2.limited-dimensions;
    elseif (~type2.limited-size-restriction)
      #t;
    elseif (type1.limited-dimensions)
      reduce1(\*, type1.limited-dimensions) == type2.limited-size-restriction;
    else
      type2.limited-size-restriction == type1.limited-size-restriction;
    end if;
  end if;
end method %subtype?;

// %subtype?(<byte-character-type>,<type>) -- internal gf method.
//
// In general, <byte-character> is only subtype? some other type if <character>
// is subtype? that other type.
//
define method %subtype? (type1 :: <byte-character-type>, type2 :: <type>)
    => res :: <boolean>;
  %subtype?(<character>, type2);
end;

// %subtype?(<byte-character-type>,<byte-character-type>) -- internal gf method
//
// <byte-character-type>s are all equivalent, so they are all subtype? each
// other.
// 
define method %subtype?
    (type1 :: <byte-character-type>, type2 :: <byte-character-type>)
    => res :: <boolean>;
  #t;
end;

// %subtype?(<none-of>,<type>) -- internal gf method.
//
// A type negation is subtype some other type if it's base is a subtype.
// This case is straigtforward, but the (<type>, <none-of>) method is just too
// hair to implement, so we won't.
//
define method %subtype? (type1 :: <none-of>, type2 :: <type>)
    => res :: <boolean>;
  // We don't use %subtype, because the base-type might be a union type.
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
  //
  // Note: this would be wrong if we could actually create classes at runtime.
  // Because then we could create a new subclass that inherited from both
  // the classes and then create an instance of that new class.  But we can't
  // make(<class>, ...) yet, so this works.
  local method sub-overlap (cls :: <class>) => res :: <boolean>;
	   subtype?(cls, type2) | any?(sub-overlap, direct-subclasses(cls));
	end method sub-overlap;
  sub-overlap(type1);
end method overlap?;

define method overlap? (type1 :: <class>, type2 :: <type>) => res :: <boolean>;
  overlap?(type2, type1);
end method overlap?;

define method overlap? (type1 :: <singleton>, type2 :: <type>)
    => res :: <boolean>;
  subtype?(type1, type2);
end method overlap?;

define method overlap? (type1 :: <subclass>, type2 :: <type>)
    => res :: <boolean>;
  local method check-subclass-instances (class :: <class>) => res :: <boolean>;
	  instance?(class, type2)
	    | any?(check-subclass-instances, direct-subclasses(class));
	end method check-subclass-instances;
  check-subclass-instances(type1.subclass-of);
end method overlap?;

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

// We ignore size and dimensions specifiers -- they are unlikely to appear
// much in practice, and the omission will not affect correctness.
//
define method overlap?
    (type1 :: <limited-collection>, type2 :: <limited-collection>)
 => (result :: <boolean>);
  type1.limited-element-type = type2.limited-element-type
    & overlap?(type1.limited-integer-base-class, type2.limited-integer-base-class);
end method overlap?;
	       
// Catch comparisions between limited integers and limited collections.
//
define method overlap?
    (type1 :: <limited-type>, type2 :: <limited-type>)
 => (result :: <boolean>);
  #f;
end method overlap?;

define method overlap? (type1 :: <limited-type>, type2 :: <class>)
    => res :: <boolean>;
  overlap?(type1.limited-integer-base-class, type2);
end method overlap?;

define method overlap? (type1 :: <limited-type>, type2 :: <type>)
    => res :: <boolean>;
  overlap?(type2, type1);
end method overlap?;

define method overlap? (type1 :: <union>, type2 :: <type>) => res :: <boolean>;
  any?(method (t) overlap?(t , type2) end, type1.union-members)
    | any?(method (t) instance?(t, type2) end, type1.union-singletons);
end method overlap?;

define method overlap? (type1 :: <none-of>, type2 :: <type>)
    => res :: <boolean>;
  overlap?(type1.base-type, type2);
end method overlap?;

define method overlap? (type1 :: <type>, type2 :: <type>) => res :: <boolean>;
  // Default.  If nothing else catches the argument, then there is no overlap.
  #f;
end method overlap?;

////////////////////////////////////////////////////////////////////////////
// This is a *very* stripped down implementation of limited function
// types -- right now only functions with a fixed number of arguments and 
// return values are supported. These are pure subtypes; that is, one
// type is a subtype of another if every element of the subtype can be
// type-safely substituted when the other type is required.
//
// This means that function arguments specialize contravariantly, and
// return types specialize covariantly. (This is different from the
// method selection rules, but that makes sense -- it's a different
// concept.)
//
// TODO: Add support for #key and #rest arguments. #rest is not too bad, 
//       but correctly adding support for keyword arguments may involve
//       some fairly involved hacking of <function>, because keyword
//       arguments don't currently carry their types with them. 
//
////////////////////////////////////////////////////////////////////////////

define constant <empty> = type-union();

define class <limited-function> (<limited-type>)
  slot lf-specializers :: <simple-object-vector>,
    required-init-keyword: specializers:;
  slot lf-return-types :: <simple-object-vector>,
    required-init-keyword: return-types:;
end class <limited-function>;

define method limited(class == <function>,
		      #key specializers :: <simple-object-vector>,
		           return-types :: <simple-object-vector>)
 => (type :: <limited-function>)
  make(<limited-function>,
       specializers: specializers,
       return-types: return-types,
       base-class: <function>);
end method limited;

define function tuple-subtype?(tuple1 :: <simple-object-vector>,
			       tuple2 :: <simple-object-vector>)
 => (b :: <boolean>)
  if (tuple1.size = tuple2.size)
    block(return)
      for (type1 :: <type> in tuple1, type2 :: <type> in tuple2)
	if (~ subtype?(type1, type2))
	  return(#f);
	end if;
      finally
	#t;
      end for;
    end block;
  else
    #f
  end if;
end function tuple-subtype?;

define method %instance?(function :: <function>, type :: <limited-function>)
 => (b :: <boolean>)
  let (n :: <integer>,
       rest-arg? :: <boolean>,
       kwd) = function.function-arguments;
  let (return-types :: <sequence>,
       return-rest? :: false-or(<type>)) = function.function-return-values;
  if (rest-arg? | kwd | return-rest?)
    #f
  else
    // Note that the types of the argument specializers must be
    // supertypes of the <limited-function>, and the types of the
    // return values must be subtypes of the return types of the
    // <limited-function>.
    tuple-subtype?(as(<vector>, type.lf-specializers),
		   as(<vector>, function.function-specializers))
      & tuple-subtype?(as(<vector>, return-types),
		       as(<vector>, type.lf-return-types));
  end if;
end method %instance?;

define method %subtype?(type1 :: <limited-function>,
			type2 :: <limited-function>) => (b :: <boolean>)
  // Similarly to instance?, the argument types of type1 must be
  // supertypes of the argument types of type2, and the return types
  // of type1 must be subtypes of the argument types of type2.
  //
  tuple-subtype?(type2.lf-specializers, type1.lf-specializers)
  & tuple-subtype?(type1.lf-return-types, type2.lf-return-types);
end method %subtype?;

define method overlap?(type1 :: <limited-function>,
		       type2 :: <limited-function>) => (b :: <boolean>)
  // Conceptually, two function types can overlap only if each of their
  // specializer arguments overlap and each of their return types
  // overlap. Otherwise there is no "room" for a function that belongs
  // to both types.
  //
  type1.lf-specializers.size = type2.lf-specializers.size
  & type2.lf-return-types.size = type2.lf-return-types.size
  & every?(overlap?, type1.lf-specializers, type2.lf-specializers)
  & every?(overlap?, type1.lf-return-types, type2.lf-return-types)
end method overlap?;
