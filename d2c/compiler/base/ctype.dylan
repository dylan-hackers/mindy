Module: ctype
Description: compile-time type system
rcs-header: $Header: /scm/cvs/src/d2c/compiler/base/ctype.dylan,v 1.12 2003/09/30 19:42:45 gabor Exp $
copyright: see below

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

/*

values-ctype [identity-preserving-mixin] {abstract}
    ctype {abstract}
        cclass [eql-ct-value] {abstract} (external)
        union-ctype [ct-value]
        unknown-ctype        
        limited-ctype {abstract}
            limited-integer-type [ct-value]
            limited-collection-type [ct-value]
            singleton-type [ct-value]
            byte-character-type [ct-value]
            direct-instance-ctype [ct-value] (external)
            subclass-ctype [ct-value, identity-preserving-mixin] (external)
    multi-value-ctype

wild-ctype() returns a <multi-value-ctype> with zero or more values of
type <object>. This is the result type of an unconstrained function.

*/

/// Superclass of multi-value types and regular single types.
define abstract class <values-ctype> (<identity-preserving-mixin>)
  //
  // Memo of the extensional version of this type.
  slot %ctype-extent :: false-or(<values-ctype>) = #f;
end class;

define sealed domain make (singleton(<values-ctype>));
define sealed domain initialize (<values-ctype>);


/// Type function memoization:
//
// Our primary approach for getting good performance on the type
// operations is to use memoization, rather than trying to come up
// with clever ways to quickly determine type relationships.  This
// based on the observation that relatively few types are actually in
// use at any given time, and that the compiler does the same
// operations over and over.

// Memoization is based on the type-hash, which is a slot shared by
// all compile-time types.
//
// <ctype> objects are also hash-consed, which (modulo unknown types)
// means that == is type equivalence.


define abstract class <ctype> (<values-ctype>)
  constant slot type-hash :: <integer> = random-bits(),
    init-keyword: type-hash:;
end class;

define sealed domain make (singleton(<ctype>));

// Memoization is done in a vector.  Each entry has four elements: the
// two arg types, the result type and the result precise flag.  All
// elements are initialized to #F, which ensures that empty entries
// will miss (since the arguments are always types.)  This memoization
// is a probablistic cache, not a complete record of all results ever
// computed.
//
define class <memo-entry> (<object>)
  slot memo-type1 :: false-or(<ctype>) = #f;
  slot memo-type2 :: false-or(<ctype>) = #f;
  slot memo-value :: type-union(<ctype>, <boolean>) = #f;
  slot memo-precise :: <boolean> = #f;
end class <memo-entry>;

define sealed domain make(singleton(<memo-entry>));
define sealed domain initialize(<memo-entry>);

define constant $null-memo-entry :: <memo-entry> = make(<memo-entry>);

#if (mindy)

define constant <memo-table> = <simple-object-vector>;

#else

define sealed class <memo-table> (<vector>)
  sealed slot %element :: <memo-entry>,
    init-value: type-union(), init-keyword: fill:,
    sizer: size, size-init-value: 0, size-init-keyword: size:;
end class <memo-table>;

define sealed domain make (singleton(<memo-table>));
define sealed domain initialize (<memo-table>);

define sealed inline method element
    (vec :: <memo-table>, index :: <integer>, #key default = $not-supplied)
 => (element :: <memo-entry>);
  if (index >= 0 & index < vec.size)
    %element(vec, index);
  elseif (default == $not-supplied)
    error("Undefined element: %s[%s]", vec, index);
  else
    default;
  end;
end;

define sealed inline method element-setter
    (new-value :: <memo-entry>, vec :: <memo-table>, index :: <integer>)
    => new-value :: <memo-entry>;
  if (index >= 0 & index < vec.size)
    %element(vec, index) := new-value;
  else
    error("Undefined element: %s[%s]", vec, index);
  end;
end;

#endif

// log2 of the the number of entries in the table.
define constant memo2-bits = 9;

// mask which gives a vector index from a large hash value.  Low zeros
// align to start of an entry.
define constant memo2-size = ash(1, memo2-bits);
define constant memo2-mask = memo2-size - 1;

define function make-memo2-table ()
  make(<memo-table>, size: memo2-size, fill: $null-memo-entry);
end function make-memo2-table;

// some hit rate info, for tuning.
define variable *memo2-hits* :: <integer> = 0;
define variable *memo2-probes* :: <integer> = 0;

// See if Type1 & Type2 are memoized in Table.  If so, the two
// memoized values are returned.  If not, we return #"miss" and #f;
define function memo2-lookup
    (type1 :: <ctype>, type2 :: <ctype>, table :: <memo-table>)
 => (value :: type-union(<ctype>, <boolean>, singleton(#"miss")),
     precise :: <boolean>);
  
  *memo2-probes* := *memo2-probes* + 1;
#if (mindy)
  let base = modulo(type1.type-hash - type2.type-hash, memo2-size);
#else
  let base = logand(type1.type-hash - type2.type-hash, memo2-mask);
#endif
  let entry :: <memo-entry> = table[base];
  if (entry.memo-type1 == type1 & entry.memo-type2 == type2)
    *memo2-hits* := *memo2-hits* + 1;
    values(entry.memo-value, entry.memo-precise);
  else
    values(#"miss", #f);
  end;
end function memo2-lookup;

define function memo2-enter
    (type1 :: <ctype>, type2 :: <ctype>,
     result :: type-union(<ctype>, <boolean>),
     precise :: <boolean>, table :: <memo-table>)
#if (mindy)  
  let base = modulo(type1.type-hash - type2.type-hash, memo2-size);
#else
  let base = logand(type1.type-hash - type2.type-hash, memo2-mask);
#endif
  let entry :: <memo-entry> = table[base];
  if (entry == $null-memo-entry)
    entry := make(<memo-entry>);
    table[base] := entry;
  end if;
  entry.memo-type1 := type1;
  entry.memo-type2 := type2;
  entry.memo-value := result;
  entry.memo-precise := precise;
end function memo2-enter;
 

/// Equality:
//
// Since ctypes are hash-consed, equality/inequality is pretty
// degenerate.  The only problem area is with unknown types (whice we
// could not or elected not to evaluate at compile time.)  Unknown
// types may be spuriously ~==, so to ensure a precise result we must
// test for unknown types.

// If two types are definitely equivalent, return true.  The second
// value indicates whether the first value is definitely correct.
// This should only fail in the presence of Unknown types.
//
define function ctype-eq? (type1 :: <ctype>, type2 :: <ctype>)
 => (result :: <boolean>, precise :: <boolean>);
  
  if (type1 == type2)
    values(#t, #t);
  else
    values(#f, ~(instance?(type1, <unknown-ctype>) 
		   | instance?(type2, <unknown-ctype>)))
  end;
end function ctype-eq?;

// Similar to ctype-eq, but we return true if the types are definitely
// not the same.
//
define function ctype-neq? (type1 :: <ctype>, type2 :: <ctype>)
       => (result :: <boolean>, precise :: <boolean>);
  
  if (type1 == type2)
    values(#f, #t);
  elseif (instance?(type1, <unknown-ctype>)
	    | instance?(type2, <unknown-ctype>))
    values(#f, #f);
  else
    values(#t, #t);
  end;
end function ctype-neq?;


// find-direct-classes  --  exported
//
// Given an arbitrary type, return a list of all the classes that a
// value of that type could possibly be direct instances of.  If we
// can't determine this (because an open class is involved) then
// return #f.  We could potentially return #() if there is no possibly
// non-abstract class.
//
define generic find-direct-classes(type :: <ctype>) => res :: false-or(<list>);


// ctype-extent -- exported.
//
// Return a new ctype that describes the set of all values that could
// be instances of that type (i.e. the extensional interpretation of
// the type).
// 
define method ctype-extent (ctype :: <values-ctype>) => res :: <values-ctype>;
  let extent = ctype.%ctype-extent;
  if (extent)
    extent;
  else
    let extent = ctype-extent-dispatch(ctype);
    ctype.%ctype-extent := extent;
    if (extent.%ctype-extent)
      assert(extent.%ctype-extent == extent);
    else
      extent.%ctype-extent := extent;
    end if;
    extent;
  end if;
end method ctype-extent;

// ctype-extent-dispatch -- internal.
//
// Does the actual work of ctype-extent if we haven't already done it.
// 
define generic ctype-extent-dispatch (ctype :: <values-ctype>)
    => res :: <values-ctype>;



/// CINSTANCE? -- exported
//
define generic cinstance? (ctv :: <ct-value>, ctype :: <ctype>)
    => (result :: <boolean>, precise :: <boolean>);

define method cinstance? (ctv :: <ct-value>, ctype :: <ctype>)
    => (result :: <boolean>, precise :: <boolean>);
  csubtype?(ctv.ct-value-cclass, ctype);
end;

define method cinstance? (ctv :: <eql-ct-value>, ctype :: <ctype>)
    => (result :: <boolean>, precise :: <boolean>);
  csubtype?(make(<singleton-ctype>, value: ctv), ctype);
end;


/// CSUBTYPE?
//
// Ignoring unknown and union types (which are handled specially), we have the
// following cross products:
//
// Singleton with:
//   singleton -- #f, 'cause == singletons are picked off
//   limited-int -- in range?
//   limited-collection -- instance?(sing.value, limited)
//   byte-char -- is the singleton a byte-character?
//   subclass -- subtype?(sing.value, subclass.subclass-of)
//   direct -- singleton.base-class == direct.base-class
//   class -- subtype?(base-class, class)
// limited integer:
//   singleton -- #f, by definition
//   limited integer -- the base classes are subtype? and the ranges are in
//     order
//   limited-collection -- #f, 'cause integers and collections are disjoint
//   byte-char -- #f, 'cause integers and characters are disjoint
//   subclass -- #f, 'cause integers and classes are disjoint
//   direct -- limint.base-class == direct.base-class
//   class -- subtype?(base-class, class)
// limited-collection:
//   singleton -- #f, by definition
//   limited-int -- #f, 'cause integers and collections are disjoint
//   limited-collection -- the base classes are subtype?, the element types 
//     are equal, and the sizes are "compatible"
//   byte-char -- #f, 'cause characters and collections are disjoint
//   subclass -- #f, 'cause collections and classes are disjoint
//   direct -- #f, by fiat
//   class -- subtype?(base-class, class)
// byte-char:
//   singleton -- #f, by definition
//   limited-int -- #f, 'cause characters and integers are disjoint
//   limited-collection -- #f, 'cause characters and collections are disjoint
//   byte-char -- can't happen, cause == types are picked off.
//   subclass -- #f, 'cause characters and classes are disjoint
//   direct -- bchar.base-class == direct.base-class
//   class -- subtype?(bchar.base-class, class)
// subclass:
//   singleton -- #f, by definition
//   limited-int -- #f, 'cause integers and classes are disjoint.
//   limited-collection -- #f, 'cause collections and classes are disjoint
//   byte-char -- #f, 'cause characters and classes are disjoint
//   subclass -- subtype?(t1.subclass-of, t2.subclass-of)
//   direct -- #f, by definition
//   class -- subtype?(base-class, class);
// Direct:
//   singleton -- #f, by definition.
//   limited-int -- #f, by definition.
//   limited-collection -- #f, by definition
//   byte-char -- #f, by definition.
//   subclass -- #f, by definition.
//   direct -- #f, 'cause == direct classes are picked off.
//   class -- subtype?(base-class, class)
// class:
//   singleton -- #f, by definition.
//   limited integer -- #f, by definition.
//   limited-collection -- #f, by definition
//   byte-char -- #f, by definition.
//   subclass -- #f, by definition.
//   direct -- #f, by definition
//   class -- check the class precedence list
//   

// csubtype-dispatch -- internal.
// 
// Handle csubtype? for unequal types other than union and unknown.
// Result is always precise because any vagueness is in the unknown types.
//
define sealed generic csubtype-dispatch(type1 :: <ctype>, type2 :: <ctype>)
       => result :: <boolean>;

// csubtype-dispatch{<ctype>,<ctype>}
// 
// Many cases are false, so make that the default.
// 
define method csubtype-dispatch(type1 :: <ctype>, type2 :: <ctype>)
    => result :: <boolean>;
  #f;
end method;

// $csubtype-memo -- internal.
//
// Memo for use in rememering results of csubtype?.
// 
define constant $csubtype-memo :: <memo-table> = make-memo2-table();

// csubtype? -- exported.
// 
// Like subtype?, but works on ctypes, and returns the second value #F
// if the relation cannot be determined at compile time (due to
// unknown types.)
//
// Check if result is memoized; if not, pick off the unknown & union
// cases before calling the generic function.
// 
define function csubtype? (type1 :: <ctype>, type2 :: <ctype>)
 => (result :: <boolean>, precise :: <boolean>);
  case
    // Makes unknown types be subtypes of themselves, & eliminates the
    // case of equal types from later consideration.  Also speeds up a
    // common case...
    (type1 == type2) => values(#t, #t);
      
    // the only thing an unknown type is surely a subtype of is
    // <object>
    instance?(type1, <unknown-ctype>) =>
      if (type2 == object-ctype()) values(#t,#t) else values(#f, #f) end;
      
    // nothing is a definite subtype of an unknown type (except
    // itself.)
    instance?(type2, <unknown-ctype>) => values(#f, #f);
      
    otherwise =>
      let (memo-val, memo-win) = memo2-lookup(type1, type2, $csubtype-memo);
      if (memo-val == #"miss")
	let val =
	  case
	    instance?(type1, <union-ctype>) =>
	      every?(method(t1) csubtype?(t1, type2) end, type1.members);
	    instance?(type2, <union-ctype>) =>
	      any?(method(t2) csubtype?(type1, t2) end, type2.members);
	    otherwise =>
	      csubtype-dispatch(type1, type2);
	  end case;

	memo2-enter(type1, type2, val, #t, $csubtype-memo);
	values(val, #t);
      else
	values(memo-val, memo-win);
      end;
  end case;
end function csubtype?;


// Intersection:

// Ignoring unknowns and unions (which are handled specially), we have
// the following possible combinations.  Note: we only list half of
// them, because intersection is commutative.
//
// Singleton with:
//   singleton -- empty, 'cause == singletons are picked off.
//   limited-int -- empty, 'cause singleton integers don't exist due to
//     canonicalization.
//   class -- the singleton if subtype?, otherwise empty
//   direct -- the singleton if subtype?, otherwise empty
//   byte-char -- the singleton if subtype?, otherwise empty
// Limited integer with:
//   limited-int -- the intersection of the base classes and range.
//   class -- the intersection of the base class and the other class, 
//     same range
//   direct -- empty, 'cause the direct integer classes are canonicalized
//     into themselves.
//   byte-char -- empty
// Class:
//   class -- the subtype class if one is a subtype of the other, the
//     intersection of their subtypes if both are sealed, or one at
//     random otherwise.
//   direct -- the direct type if it is a subtype of the class, empty
//     otherwise
//   byte-char -- the byte-char type if it is a subtype of the class
// Direct:
//   direct -- the type when they are the same, otherwise empty
//   byte-char -- the byte-char type if it is a subtype of the class
// Byte-char:
//   byte-char -- won't be called, 'cause the == case is picked off
//
// So if we pick off the case where one type is a subtype of the other
// first, we have the following cases left:
//
// Limited integer with:
//   limited-int -- the intersection of the base classes and range.
//   class -- the intersection of the base class and the other class,
//     same range
// Class:
//   class -- the subtype class if one is a subtype of the other, the
//     intersection of their subtypes if both are sealed, or one at
//     random otherwise.
//

// Handle ctype-intersection for unequal types other than union and
// unknown.  Result may be imprecise if we intersect two non-sealed
// classes.
//
define sealed generic ctype-intersection-dispatch
    (type1 :: <ctype>, type2 :: <ctype>)
     => (result :: false-or(<ctype>), precise :: <boolean>);

/// indicates try swapping args, or if that failed, the result is empty.
define method ctype-intersection-dispatch(type1 :: <ctype>, type2 :: <ctype>)
     => (result :: false-or(<ctype>), precise :: <boolean>);
  values(#f, #t);
end method;


define constant $intersection-memo :: <memo-table> = make-memo2-table();

// Return as restrictive a type as we can discover that is no more
// restrictive than the intersection of Type1 and Type2.  The second
// value is true if the result is exact.  At worst, we arbitrarily
// return one of the arguments as the first value (trying not to
// return an unknown type).
//
define function ctype-intersection (type1 :: <ctype>, type2 :: <ctype>)
     => (result :: <ctype>, precise :: <boolean>);

  if (type1 == type2)
    values(type1, #t);
  else
    let (memo-val, memo-win) = memo2-lookup(type1, type2,
					    $intersection-memo);
    if (memo-val == #"miss")
      let (val, win) = compute-ctype-intersection(type1, type2);      
      memo2-enter(type1, type2, val, win, $intersection-memo);
      values(val, win);
    else
      values(memo-val, memo-win);
    end;
  end if;
end function ctype-intersection;

define function compute-ctype-intersection
    (type1 :: <ctype>, type2 :: <ctype>)
 => (ctype-or-false :: type-union(<ctype>, <boolean>),
     precise? :: <boolean>);
  case
    // Makes unknown types intersect with themselves, & eliminates the
    // case of equal types from later consideration.  If one arg is
    // unknown, return the other and #f.
    //
    instance?(type1, <unknown-ctype>) =>
      values(type2, #f);
    instance?(type2, <unknown-ctype>) =>
      values(type1, #f);
      
    // Otherwise, the intersection is the union of the pairwise
    // intersection of the members.  As described above, we try both
    // orders.
    otherwise =>
      compute-ctype-intersection-using-members(type1, type2);
  end case;
end function compute-ctype-intersection;

define function compute-ctype-intersection-using-members
    (type1 :: <ctype>, type2 :: <ctype>)
 => (ctype-or-false :: type-union(<ctype>, <boolean>),
     precise? :: <boolean>);
  let precise? = #t;
  let res-union = empty-ctype();
  for (mem1 in type1.members)
    for (mem2 in type2.members)
      // If either is a subtype of the other, include the subtype.
      if (csubtype?(mem1, mem2))
	res-union := ctype-union(res-union, mem1);
      elseif (csubtype?(mem2, mem1))
	res-union := ctype-union(res-union, mem2);
      else
	// Call the intersection dispatch function.
	let (res12, win12) = ctype-intersection-dispatch(mem1, mem2);
	if (res12)
	  unless (win12) precise? := #f end;
	  res-union := ctype-union(res-union, res12);
	else
	  let (res21, win21) = ctype-intersection-dispatch(mem2, mem1);
	  if (res21)
	    unless (win21) precise? := #f end;
	    res-union := ctype-union(res-union, res21);
	    
	    // else precisely empty, nothing to union.
	  end if;
	end if;
      end if;
    end for;
  end for;
  values(res-union, precise?);
end function compute-ctype-intersection-using-members;

// The first value is true unless the types definitely don't
// intersect.  The second value is true if the first value is
// definitely correct.  empty-ctype is considered to intersect with
// any type.  If either type is <object>, we also return #T, #T.  This
// way we consider unknown types to intersect with <object>.
//
define function ctypes-intersect? (type1 :: <ctype>, type2 :: <ctype>)
 => (result :: <boolean>, precise :: <boolean>);
  if (type1 == empty-ctype() | type2 == empty-ctype())
    values(#t, #t);
  else
    let (res, win) = ctype-intersection(type1, type2);
    if (win)
      values(~(res == empty-ctype()), #t);
    elseif (type1 == object-ctype() | type2 == object-ctype())
      values(#t, #t);
    else
      values(#t, #f);
    end;
  end;
end function ctypes-intersect?;


/// Difference.

define constant $difference-memo :: <memo-table> = make-memo2-table();

// Return our best guess at the type that describes all objects that
// are in Type1 but not in Type2.  If we can't precisely determine
// this type, then return something more inclusive than it, but never
// more inclusive than type1.
//
define method ctype-difference (type1 :: <ctype>, type2 :: <ctype>)
    => (result :: <ctype>, precise? :: <boolean>);
  if (type1 == type2)
    values(empty-ctype(), #t);
  else
    let (memo-val, memo-win) = memo2-lookup(type1, type2, $difference-memo);
    if (memo-val == #"miss")
      let (val, win)
	= block (return)
	    if (instance?(type1, <unknown-ctype>)
		  | instance?(type2, <unknown-ctype>))
	      return(type1, #f);
	    end if;
	    let result = empty-ctype();
	    let precise? = #t;
	    for (member in type1.members)
	      unless (csubtype?(member, type2))
		if (ctypes-intersect?(member, type2))
		  precise? := #f;
		end if;
		result := ctype-union(result, member);
	      end unless;
	    end for;
	    values(result, precise?);
	  end block;
      memo2-enter(type1, type2, val, win, $difference-memo);
      values(val, win);
    else
      values(memo-val, memo-win);
    end if;
  end if;
end method ctype-difference;




//// Union types:

// <union-table> -- internal.
// 
// table used for hash-consing union types.  Key is a list of <ctype>s.
//
define class <union-table> (<table>)
end class;

define sealed domain make (singleton(<union-table>));
define sealed domain initialize (<union-table>);

define method table-protocol(table :: <union-table>)
    => (test :: <function>, hash :: <function>);
  values(\=,
  	 method(key :: <list>, initial-state)
	   for(elt :: <ctype> in key,
	       res :: <integer> = 0 then \+(res, elt.type-hash))
	   finally values(res, initial-state);
	   end for;
	 end method);
end;

define constant $union-table :: <union-table> = make(<union-table>);


define generic members (type :: <ctype>) => members :: <list>;

define class <union-ctype> (<ctype>, <ct-value>)
  // list of ctypes in the union, which can only be classes, limited
  // types or singletons.  Any nested unions are flattened into this
  // one, and the union of anything and an unknown type is itself an
  // unknown type.
  constant slot members :: <list>, required-init-keyword: members:;
end class;

define sealed domain make (singleton(<union-ctype>));

define method make
    (class == <union-ctype>, #next next-method, #key members :: <list>)
    => res :: <union-ctype>;
  let sorted = sort!(copy-sequence(members),
		     test: method (x :: <ctype>, y :: <ctype>)
			       => res :: <boolean>;
			     x.type-hash < y.type-hash;
			   end);
  let found = element($union-table, sorted, default: #f);
  if (found)
    found;
  else
    $union-table[sorted] := next-method(class, members: sorted);
  end;
end method make;


define method print-object (union :: <union-ctype>, stream :: <stream>) => ();
  pprint-logical-block
    (stream,
     prefix: "{",
     body: method (stream)
	     write-class-name(union, stream);
	     write-element(stream, ' ');
	     pprint-indent(#"block", 2, stream);
	     pprint-newline(#"linear", stream);
	     pprint-logical-block
	       (stream,
		prefix: "(",
		body: method (stream)
			for (member in union.members, first? = #t then #f)
			  unless(first?)
			    write(stream, ", ");
			    pprint-newline(#"fill", stream);
			  end;
			  print(member, stream);
			end;
		      end,
		suffix: ")");
	   end,
     suffix: "}");
end;

define method print-message (union :: <union-ctype>, stream :: <stream>) => ();
  write(stream, "type-union(");
  for (member in union.members, first? = #t then #f)
    unless(first?)
      write(stream, ", ");
    end;
    print-message(member, stream);
  end;
  write-element(stream, ')');
end;

// The "members" of any non-union type is a list of the type itself.
//
define method members (type :: <ctype>) => result :: <list>;
  list(type);
end method;


// ctype-extent-dispatch{<union-ctype>}
//
// Just union the extents of the members.
// 
define method ctype-extent-dispatch (type :: <union-ctype>)
    => res :: <ctype>;
  reduce(method (result :: <ctype>, member :: <ctype>)
	     => result :: <ctype>;
	   ctype-union(result, member.ctype-extent);
	 end method,
	 empty-ctype(),
	 type.members);
end method ctype-extent-dispatch;


// find-direct-classes{<union-ctype>}
// 
// Most type ops have a non-generic wrapper that handles unions and
// unknowns.  Not so for find-direct-classes.
//
// Just accumulate the direct classes of all the members.
// 
define method find-direct-classes (type :: <union-ctype>)
    => res :: false-or(<list>);
  let res = #();
  block (done)
    for (mem in type.members)
      let mem-classes = find-direct-classes(mem);
      if (mem-classes)
	res := concatenate(mem-classes, res);
      else
        done(#f);
      end;
    finally
      remove-duplicates(res);
    end;
  end;
end method;


define constant $union-memo :: <memo-table> = make-memo2-table();


// Find a type which includes both types.  The result is an unknown
// type if either of the arguments are unknown; otherwise the result
// is precise.  This result is simplified into the canonical form,
// thus is not a union type unless there is no other way to represent
// the result.
//
// If no members, the result is the empty type.  If one, it is that
// type.  Otherwise, make (or reuse) a union-ctype.
//
define function ctype-union (type1 :: <ctype>, type2 :: <ctype>)
 => value :: <ctype>;
  let (value, precise) = memo2-lookup(type1, type2, $union-memo);
  case 
    ~(value == #"miss") =>
      value;
      
    instance?(type1, <unknown-ctype>) =>
      make(<unknown-ctype>, type-exp: type1.type-exp);
   
    instance?(type2, <unknown-ctype>) =>
      make(<unknown-ctype>, type-exp: type2.type-exp);

    otherwise =>
      compute-ctype-union(type1, type2);
  end case;
end function ctype-union;

define function compute-ctype-union (type1 :: <ctype>, type2 :: <ctype>)
 => value :: <ctype>;
  let new-members :: <list> = copy-sequence(type1.members);
  for (member in type2.members)
    if (instance?(member, <limited-integer-ctype>))
      member := limited-int-union(member, new-members);
    end if;
    block (next-member)
      for (remaining = new-members then remaining.tail,
	   prev = #f then remaining,
	   until: remaining == #())
	let other = remaining.head;
	if (csubtype?(member, other))
	  next-member();
	elseif (csubtype?(other, member))
	  if (prev)
	    prev.tail := remaining.tail;
	  else
	    new-members := remaining.tail;
	  end if;
	end if;
      end for;
      new-members := pair(member, new-members);
    end block;
  end for;
  
  let res = list-to-ctype-union(new-members);
  memo2-enter(type1, type2, res, #t, $union-memo);
  res;
end function compute-ctype-union;

define function list-to-ctype-union(members :: <list>)
  if (members == #())
    empty-ctype();
  elseif (tail(members) == #())
    head(members);
  else
    make(<union-ctype>, members: members);
  end if;
end function list-to-ctype-union;



// <unknown-ctype> represents some random non-compile-time expression
// that ought to be a type.
//
// This should be interpreted as "some type whose meaning is unknown
// because the value of EXP is unknown".  An unknown type is never
// CTYPE-EQ to itself or to any other type.
//
define class <unknown-ctype> (<ctype>)
  //
  // The expression which was of unknown type.  In general, this is
  // only for human context.
  slot type-exp, init-value: #f, init-keyword: type-exp:;
end class;

define sealed domain make (singleton(<unknown-ctype>));

define method print-message (ctype :: <unknown-ctype>, stream :: <stream>)
    => ();
  write(stream, "<unknown>");
end;

define method find-direct-classes(type :: <unknown-ctype>) => res :: <false>;
  ignore(type);
  #f;
end;

define method ctype-extent-dispatch (type :: <unknown-ctype>)
    => res :: <ctype>;
  object-ctype();
end method ctype-extent-dispatch;


/// Limited types:
//
// The <limited-ctype> abstract class is inherited by various
// non-class types where there is a class that is a "tight" supertype
// of the type.  This includes singleton and direct-instance types.
//
define abstract class <limited-ctype> (<ctype>)
  // The most specific class that is a supertype of this type.
  constant slot base-class :: <cclass>, required-init-keyword: base-class:;
end class;

define sealed domain make (singleton(<limited-ctype>));

// find-direct-classes{<limited-ctype>}
//
// Find the direct classes of the base class.
//
define method find-direct-classes (type :: <limited-ctype>)
    => res :: false-or(<list>);
  find-direct-classes(type.base-class);
end;


/// Limited integer types:

// A limited-integer-table is used to keep track of all the limited
// integers we have already allocated, so we can reuse them.

define class <limited-integer-table> (<table>)
end;

define sealed domain make (singleton(<limited-integer-table>));
define sealed domain initialize (<limited-integer-table>);

define method table-protocol (table :: <limited-integer-table>)
    => (test :: <function>, hash :: <function>);
  values(\=,
	 method (key :: <vector>, initial-state)
	   let (min-id, min-state) = equal-hash(key.second, initial-state);
	   let base&min-id
	     = merge-hash-ids(key.first.type-hash, min-id, ordered: #t);
	   let (max-id, max-state) = equal-hash(key.third, min-state);
	   let id = merge-hash-ids(base&min-id, max-id, ordered: #t);
	   values(id, max-state);
	 end);
end;

define constant $limited-integer-table = make(<limited-integer-table>);

define class <limited-integer-ctype> (<limited-ctype>, <ct-value>)
  constant slot low-bound :: false-or(<extended-integer>), 
       required-init-keyword: low-bound:;
  constant slot high-bound :: false-or(<extended-integer>), 
       required-init-keyword:  high-bound:;
end class;

define sealed domain make (singleton(<limited-integer-ctype>));

define method make (class == <limited-integer-ctype>, #next next-method,
		    #key base-class, low-bound, high-bound)
    => res :: <limited-integer-ctype>;
  let key = vector(base-class, low-bound, high-bound);
  element($limited-integer-table, key, default: #f)
    | (element($limited-integer-table, key) := next-method());
end;

define method print-object (limint :: <limited-integer-ctype>,
			    stream :: <stream>)
    => ();
  pprint-logical-block
    (stream,
     prefix: "{",
     body: method (stream)
	     write-class-name(limint, stream);
	     write-element(stream, ' ');
	     pprint-indent(#"block", 2, stream);
	     pprint-newline(#"linear", stream);
	     pprint-logical-block
	       (stream,
		prefix: "(",
		body: method (stream)
			print(limint.base-class, stream);
			if (limint.low-bound)
			  write(stream, ", ");
			  pprint-newline(#"fill", stream);
			  format(stream, "min: %d", limint.low-bound);
			end;
			if (limint.high-bound)
			  write(stream, ", ");
			  pprint-newline(#"fill", stream);
			  format(stream, "max: %d", limint.high-bound);
			end;
		      end,
		suffix: ")");
	   end,
     suffix: "}");
end;

define method print-message
    (limint :: <limited-integer-ctype>, stream :: <stream>)
    => ();
  write(stream, "limited(");
  print-message(limint.base-class, stream);
  if (limint.low-bound)
    format(stream, ", min: %d", limint.low-bound);
  end;
  if (limint.high-bound)
    format(stream, ", max: %d", limint.high-bound);
  end;
  write-element(stream, ')');
end;


// ctype-extent-dispatch{<limited-integer-ctype>}
//
// Find the minimal extent of the limited integer type.
//
define method ctype-extent-dispatch
    (type :: <limited-integer-ctype>)
    => res :: <ctype>;
  // ### Should really make integer sets.
  local
    method make-set (class :: <cclass>) => res :: <ctype>;
      make-canonical-limited-integer
	(class, type.low-bound, type.high-bound);
    end method make-set;
  if (type.base-class == specifier-type(#"<general-integer>"))
    ctype-union(make-set(specifier-type(#"<integer>")),
		make-set(specifier-type(#"<extended-integer>")));
  else
    make-set(type.base-class);
  end if;
end method ctype-extent-dispatch;


define method make-canonical-limited-integer
    (base-class :: <cclass>,
     low-bound :: false-or(<general-integer>),
     high-bound :: false-or(<general-integer>))
    => res :: <ctype>;
  let low-bound = low-bound & as(<extended-integer>, low-bound);
  let high-bound = high-bound & as(<extended-integer>, high-bound);
  
  if (base-class == specifier-type(#"<integer>"))
    let min-int = ash(as(<extended-integer>, -1),
		      *current-target*.platform-integer-length - 1);
    let max-int = lognot(min-int);
    if (~low-bound | low-bound < min-int)
      low-bound := min-int;
    end;
    if (~high-bound | high-bound > max-int)
      high-bound := max-int;
    end;
    if (high-bound < low-bound)
      empty-ctype()
    elseif (high-bound = low-bound)
      make(<singleton-ctype>, value: as(<ct-value>, as(<integer>, low-bound)))
    else
      make(<limited-integer-ctype>, base-class: base-class,
	   low-bound: low-bound, high-bound: high-bound)
    end;
  else
    if (high-bound & low-bound & high-bound < low-bound)
      empty-ctype();
    elseif (high-bound & low-bound & high-bound = low-bound)
      make(<singleton-ctype>, value: as(<ct-value>, low-bound))
    else
      make(<limited-integer-ctype>, base-class: base-class,
	   low-bound: low-bound, high-bound: high-bound);
    end;
  end;
end;

// csubtype-dispatch{<limited-integer-ctype>,<limited-integer-ctype>}
//
// A limited integer type is a subtype of another if the bounds of
// type1 are not wider that type2's (and the base class is a subtype.)
//
define method csubtype-dispatch
    (type1 :: <limited-integer-ctype>, type2 :: <limited-integer-ctype>)
    => result :: <boolean>;
  let L1 = type1.low-bound;  
  let L2 = type2.low-bound;  
  let H1 = type1.high-bound;  
  let H2 = type2.high-bound;

  (L1 == L2 | L2 == #f | (L1 ~= #f & L1 >= L2)) 
    & 
  (H1 == H2 | H2 == #f | (H1 ~= #f & H1 <= H2))
    &
  csubtype?(type1.base-class, type2.base-class);
end method;

define method ctype-intersection-dispatch
    (type1 :: <limited-integer-ctype>, type2 :: <ctype>)
    => (result :: <ctype>, precise :: <true>);
  let base = ctype-intersection(type1.base-class, type2);
  if (base == empty-ctype())
    values(empty-ctype(), #t);
  elseif (instance?(base, <singleton-ctype>)) // tc
    // Should we return make-canonical-limited-integer...?
    values(base, #t);
  elseif (instance?(base, <cclass>))
    values(make-canonical-limited-integer(base, type1.low-bound,
					  type1.high-bound),
	   #t);
  else
    error("Wrong base for type intersection with <limited-integer-ctype>.\n"
	    "Shouldn't happen.");
    #f;
  end;
end;

// The intersection of two limited integer types is the overlap of the
// ranges.  We determine this by maximizing the lower bounds and
// minimizing the upper bounds, returning that range if non-empty.
//
define method ctype-intersection-dispatch
    (type1 :: <limited-integer-ctype>, type2 :: <limited-integer-ctype>)
    => (result :: <ctype>, precise :: <true>);
  local innerize(b1, b2, fun)
    case
      ~b1 => b2;
      ~b2 => b1;
      otherwise => fun(b1, b2);
    end;
  end method;

  let rbase = ctype-intersection(type1.base-class, type2.base-class);
  if (rbase == empty-ctype())
    values(empty-ctype(), #t);
  else
    let L1 = type1.low-bound;
    let L2 = type2.low-bound;
    let H1 = type1.high-bound;
    let H2 = type2.high-bound;
    let nlow = innerize(L1, L2, max);
    let nhigh = innerize(H1, H2, min);
    if (~nlow | ~nhigh | nlow <= nhigh)
      values(make-canonical-limited-integer(rbase, nlow, nhigh), #t);
    else
      values(empty-ctype(), #t);
    end if;
  end;
end method;


// Return a new limited integer type with Int joined to any of the
// types in Others that it intesects.  We don't bother removing the
// overlapped type, since it will be removed by the subtype
// elimination pass later on.
//
define function limited-int-union
    (int :: <limited-integer-ctype>, others :: <list>)
    => res :: <limited-integer-ctype>;
  // Return true if the two types have overlapping or contiguous
  // ranges.  Value is arbitrary if one is a subtype of the other,
  // which doesn't matter here.
  local method adjacent? (type1, type2)
	  let L1 = type1.low-bound;
	  let H1 = type1.high-bound;
	  let L2 = type2.low-bound;
	  let H2 = type2.high-bound;
	  
	  if (L1 = #f | (L2 ~= #f & L1 <= L2))
	    H1 = #f | L2 = #f | L2 <= H1 + 1;
	  else
	    L1 = #f | H2 = #f | L1 <= H2 + 1;
	  end;
	end method;

  let LI = int.low-bound;
  let HI = int.high-bound;
  let base = int.base-class;
  
  for (other in others)
    if (instance?(other, <limited-integer-ctype>))
      let LO = other.low-bound;
      let HO = other.high-bound;
      
      if (base == other.base-class & adjacent?(int, other))
	if (LI ~== #f & (LO == #f | LO < LI))
	  LI := LO;
	end;
	
	if (HI ~== #f & (HO == #f | HO > HI))
	  HI := HO;
	end;
      end;
    end if;
  end for;

  make(<limited-integer-ctype>,
       base-class: base, low-bound: LI, high-bound: HI);
end function limited-int-union;


/// Limited collection types:

// A limited-collection-table is used to keep track of all the limited
// collections we have already allocated, so we can reuse them.

define class <limited-collection-table> (<table>)
end;

define sealed domain make (singleton(<limited-collection-table>));
define sealed domain initialize (<limited-collection-table>);

define method table-protocol (table :: <limited-collection-table>)
    => (test :: <function>, hash :: <function>);
  values(\=,
	 method (key :: <vector> /* of base, element, size/dimensions */,
		 initial-state)
	   let types-id = merge-hash-ids(key.first.type-hash,
					 key.second.type-hash,
					 ordered: #t);
	   let (size-hash, size-state) = equal-hash(key.third, initial-state);
	   let id = merge-hash-ids(types-id, size-hash, ordered: #t);
	   values(id, size-state);
	 end);
end;

define constant $limited-collection-table = make(<limited-collection-table>);

// optimize/limopt.dylan uses this hook to supply implementation classes
// for limited collection types.
define variable *find-limited-collection-implementation* :: <function>
  = method (type :: <limited-collection-ctype>)
     => (cclass :: false-or(<cclass>))
      error("No function supplied for "
	      "*find-limited-collection-implementation*");
    end method;

define class <limited-collection-ctype> (<limited-ctype>, <ct-value>)
  constant slot element-type :: <ctype>, required-init-keyword: element-type:;
  slot size-or-dimension :: type-union(<false>, <integer>, <sequence>) = #f,
       init-keyword: size:;
  // The implementation class, if one exists and we know it. All instances
  // of this type will be instances of this class. (If the type isn't
  // instantiable, this will be #f.)
  slot implementation-class :: false-or(<cclass>) = #f;
end class;

define sealed domain make (singleton(<limited-collection-ctype>));

define method make (class == <limited-collection-ctype>, #next next-method,
		    #key base-class, element-type, size)
    => res :: <limited-collection-ctype>;
  let key = vector(base-class, element-type, size);
  element($limited-collection-table, key, default: #f)
    | (element($limited-collection-table, key) := next-method());
end;

define method initialize
    (instance :: <limited-collection-ctype>, #key, #all-keys) => ()
  instance.implementation-class :=
    *find-limited-collection-implementation*(instance);
end method initialize;

define method print-object (limcol :: <limited-collection-ctype>,
			    stream :: <stream>)
    => ();
  pprint-logical-block
    (stream,
     prefix: "{",
     body: method (stream)
	     write-class-name(limcol, stream);
	     write-element(stream, ' ');
	     pprint-indent(#"block", 2, stream);
	     pprint-newline(#"linear", stream);
	     pprint-logical-block
	       (stream,
		prefix: "(",
		body: method (stream)
			print(limcol.base-class, stream);
			write(stream, ", ");
			pprint-newline(#"fill", stream);
			format(stream, "element-type: %=",
			       limcol.element-type);
			if (limcol.size-or-dimension)
			  write(stream, ", ");
			  pprint-newline(#"fill", stream);
			  format(stream, "size/dim: %=",
				 limcol.size-or-dimension);
			end;
		      end,
		suffix: ")");
	   end,
     suffix: "}");
end;

define method print-message
    (limcol :: <limited-collection-ctype>, stream :: <stream>)
    => ();
  write(stream, "limited(");
  print-message(limcol.base-class, stream);
  format(stream, ", of: %s", limcol.element-type);
  if (limcol.size-or-dimension)
    format(stream, ", size: %d", limcol.size-or-dimension);
  end;
  write-element(stream, ')');
end;

// ctype-extent-dispatch{<limited-collection-ctype>}
//
// Find the minimal extent of the limited collection type.
//
define method ctype-extent-dispatch
    (type :: <limited-collection-ctype>)
    => res :: <ctype>;
  local method build-limited(class)
          make(<limited-collection-ctype>, base-class: class,
               element-type: type.element-type, size: type.size-or-dimension);
        end method;
  let implementation = type.implementation-class;
  if (implementation)
    build-limited(implementation)
  else
    let base-extent = ctype-extent(type.base-class);
    if (instance?(base-extent, <union-ctype>))
      reduce(method (result :: <ctype>, member :: <ctype>)
	      => result :: <ctype>;
	       ctype-union(result, build-limited(member));
	     end method,
	     empty-ctype(),
	     base-extent.members);
    else
      build-limited(base-extent);
    end if;
  end if;
end method ctype-extent-dispatch;

// csubtype-dispatch{<limited-collection-ctype>,<limited-collection-ctype>}
//
// A limited collection type is a subtype of another if the element
// types are identical and the sizes are compatible (and the base
// class is a subtype.)
//
define method csubtype-dispatch
    (type1 :: <limited-collection-ctype>, type2 :: <limited-collection-ctype>)
    => result :: <boolean>;
  csubtype?(type1.base-class, type2.base-class)
    & (type1.element-type = type2.element-type)
    & (~type2.size-or-dimension
	 | type1.size-or-dimension = type2.size-or-dimension
	 | (instance?(type2.size-or-dimension, <sequence>)
	      & (reduce1(\*, type2.size-or-dimension)
		   = type1.size-or-dimension)));
end method csubtype-dispatch;
	 
define method ctype-intersection-dispatch
    (type1 :: <limited-collection-ctype>, type2 :: <limited-collection-ctype>)
    => (result :: <ctype>, precise :: <boolean>);
  let (base, precise) = ctype-intersection(type1.base-class, type2.base-class);
  if (base == empty-ctype() | type1.element-type ~= type2.element-type)
    values(empty-ctype(), precise);
  elseif (~instance?(base, <cclass>))
    error("Wrong base for intersection of <limited-collection-ctype>.\n"
	    "Shouldn't happen.");
    #f;
  else
    let size1 = type1.size-or-dimension;
    let size2 = type2.size-or-dimension;
    let new-size = #t;
    if (~(size1 & size2))
      new-size := (size1 | size2)
    elseif (size1 = size2)
      new-size := size1;
    elseif (instance?(size2, <sequence>))
      if (~instance?(size1, <sequence>) & reduce1(\*, size2) = size1)
	new-size := size1;
      else
	new-size := #t;
      end;
    elseif (instance?(size1, <sequence>) & reduce1(\*, size1) = size2)
      new-size := size2;
    else
      new-size := #t;
    end if;
    if (new-size ~= #t)
      let result = make(<limited-collection-ctype>, base-class: base,
			element-type: type1.element-type, size: new-size);
      values(result, precise);
    else
      values(empty-ctype(), precise);
    end if;
  end if;
end;

define method ctype-intersection-dispatch
    (type1 :: <limited-collection-ctype>, type2 :: <ctype>)
    => (result :: <ctype>, precise :: <boolean>);
  let (base, precise) = ctype-intersection(type1.base-class, type2);
  if (base == empty-ctype())
//    values(empty-ctype(), precise);
    values(base, precise);
  elseif (instance?(base, <cclass>))
    values(make(<limited-collection-ctype>, base-class: base,
		element-type: type1.element-type,
		size: type1.size-or-dimension), precise);
  else
    values(type1, #f);
  end if;
end method;


/// Singleton types:

// <singleton-ctype> -- exported.
//
// We only represent singletons with eql compile-time constant values.
//
define class <singleton-ctype> (<limited-ctype>, <ct-value>)

  // The base-class is the direct class of this object, which can be
  // used interchangably with the object when testing this object for
  // class membership.

  // The value we represent.
  constant slot singleton-value :: <eql-ct-value>,
    required-init-keyword: value:;
end class;

define sealed domain make (singleton(<singleton-ctype>));

define method make
    (class == <singleton-ctype>, #next next-method,
     #key value :: <eql-ct-value>, base-class :: false-or(<cclass>))
    => res :: <singleton-ctype>;
  value.ct-value-singleton
    | (value.ct-value-singleton
	 := next-method(class, value: value,
			base-class: base-class | value.ct-value-cclass));
end method make;


define method print-object (sing :: <singleton-ctype>, stream :: <stream>)
    => ();
  pprint-logical-block
    (stream,
     prefix: "{",
     body: method (stream)
	     write-class-name(sing, stream);
	     write-element(stream, ' ');
	     pprint-indent(#"block", 2, stream);
	     pprint-newline(#"linear", stream);
	     pprint-logical-block
	       (stream,
		prefix: "(",
		body: curry(print, sing.singleton-value),
		suffix: ")");
	   end,
     suffix: "}");
end;

define method print-message (sing :: <singleton-ctype>, stream :: <stream>)
    => ();
  format(stream, "singleton(%s)", sing.singleton-value);
end;


// ctype-extent-dispatch{<singleton-ctype>}
//
// Check to see if the value is one of the ones we would rather
// represent as a member of some set of values.
//
define method ctype-extent-dispatch (type :: <singleton-ctype>)
    => res :: <ctype>;
  let value = type.singleton-value;
  select (value by instance?)
    /* ### Should really return some kind of set.
    <literal-integer> =>
      ###;
    <literal-extended-integer> =>
      ###;
    <literal-character> =>
      ###;
    */
    <literal-general-integer> =>
      let value = value.literal-value;
      make-canonical-limited-integer(type.base-class, value, value);
    otherwise =>
      type;
  end select;
end method ctype-extent-dispatch;


// csubtype-dispatch{<singleton-ctype>,<limited-integer-ctype>}
//
// A singleton is a subtype of a limited integer if it is the right
// kind of integer and if it is inside the bounds.
// 
define method csubtype-dispatch
    (type1 :: <singleton-ctype>, type2 :: <limited-integer-ctype>)
    => res :: <boolean>;
  if (csubtype?(type1.base-class, type2.base-class))
    let value :: <extended-integer> = type1.singleton-value.literal-value;
    (type2.low-bound == #f | type2.low-bound <= value)
      & (type2.high-bound == #f | type2.high-bound >= value);
  end if;
end method csubtype-dispatch;

// csubtype-dispatch{<singleton-ctype>,<limited-collection-ctype>}
//
// Without a compile type model of collection's element-type, we
// cannot determine instance relationships, so just return #f.
// 
define method csubtype-dispatch
    (type1 :: <singleton-ctype>, type2 :: <limited-collection-ctype>)
    => res :: <boolean>;
  #f;
end method csubtype-dispatch;

// csubtype-dispatch{<singleton-ctype>,<byte-character-ctype>}
//
// A singleton is a subtype of <byte-character-ctype> iff the
// singleton is a byte character.
// 
define method csubtype-dispatch
    (type1 :: <singleton-ctype>, type2 :: <byte-character-ctype>)
    => res :: <boolean>;
  let val = type1.singleton-value;
  instance?(val, <literal-character>)
    & instance?(val.literal-value, <byte-character>);
end;



// ct-value-cclass -- exported.
//
// Specify the instance relationship of ct-values.
// Does not take layout or other compiler specific properties
// into account. Generally some class defined in the dylan module
// is returned.
// 
define generic ct-value-cclass (ct-value :: <ct-value>) => res :: <cclass>;

define method ct-value-slot(ct-value :: <ct-value>, slot == #"%object-class")
 => res :: <cclass>;
  ct-value-cclass(ct-value);
end method;

define method ct-value-cclass (object :: <ct-not-supplied-marker>)
    => res :: <cclass>;
  specifier-type(#"<not-supplied-marker>");
end method;

define method ct-value-cclass (object :: <literal-true>) => res :: <cclass>;
  specifier-type(#"<true>");
end method;

define method ct-value-cclass (object :: <literal-false>) => res :: <cclass>;
  specifier-type(#"<false>");
end method;

define method ct-value-cclass (object :: <literal-symbol>) => res :: <cclass>;
  specifier-type(#"<symbol>");
end method;

define method ct-value-cclass (object :: <literal-character>)
    => res :: <cclass>;
  specifier-type(#"<character>");
end method;

define method ct-value-cclass (object :: <literal-empty-list>)
    => res :: <cclass>;
  specifier-type(#"<empty-list>");
end method;

define method ct-value-cclass (object :: <literal-list>) => res :: <cclass>;
  specifier-type(#"<pair>");
end method;

define method ct-value-cclass (object :: <literal-string>) => res :: <cclass>;
  specifier-type(#"<byte-string>");
end method;

define method ct-value-cclass (object :: <literal-simple-object-vector>)
    => res :: <cclass>;
  specifier-type(#"<simple-object-vector>");
end method;

define method ct-value-cclass (object :: <literal-integer>)
    => res :: <cclass>;
  specifier-type(#"<integer>");
end method;

define method ct-value-cclass (object :: <literal-extended-integer>)
    => res :: <cclass>;
  specifier-type(#"<extended-integer>");
end method;

define method ct-value-cclass (object :: <literal-ratio>) => res :: <cclass>;
  specifier-type(#"<ratio>");
end method;

define method ct-value-cclass (object :: <literal-single-float>)
    => res :: <cclass>;
  specifier-type(#"<single-float>");
end method;

define method ct-value-cclass (object :: <literal-double-float>)
    => res :: <cclass>;
  specifier-type(#"<double-float>");
end method;

define method ct-value-cclass (object :: <literal-extended-float>)
    => res :: <cclass>;
  specifier-type(#"<extended-float>");
end method;

define method ct-value-cclass (object :: <union-ctype>) => res :: <cclass>;
  specifier-type(#"<union>");
end;

define method ct-value-cclass (object :: <limited-integer-ctype>)
    => res :: <cclass>;
  specifier-type(#"<limited-integer>");
end;

define method ct-value-cclass (object :: <limited-collection-ctype>)
    => res :: <cclass>;
  specifier-type(#"<limited-collection>");
end;

define method ct-value-cclass (object :: <singleton-ctype>) => res :: <cclass>;
  specifier-type(#"<singleton>");
end;

define method ct-value-cclass (object :: <byte-character-ctype>)
    => res :: <cclass>;
  specifier-type(#"<byte-character-type>");
end;


// <byte-character-ctype>

define class <byte-character-ctype> (<limited-ctype>, <ct-value>)
end;

define sealed domain make (singleton(<byte-character-ctype>));

define variable *byte-character-ctype-memo*
    :: false-or(<byte-character-ctype>)
  = #f;

define method make
    (class == <byte-character-ctype>, #next next-method, #key base-class)
    => res :: <byte-character-ctype>;
  *byte-character-ctype-memo*
    | (*byte-character-ctype-memo*
	 := next-method(class,
			base-class:
			  base-class | specifier-type(#"<character>")));
end;

define method print-message
    (type :: <byte-character-ctype>, stream :: <stream>)
    => ();
  write(stream, "<byte-character>");
end;


// ctype-extent-dispatch{<byte-character-ctype>}
//
// Just return the set of all byte characters.
// 
define method ctype-extent-dispatch (type :: <byte-character-ctype>)
    => reult :: <ctype>;
  // ### Should really be returning a character set.
  type;
end method ctype-extent-dispatch;



/// Multi-values types:

//
// The normal type operations (type-union, csubtype?, etc.) are not
// allowed on multi-value types.  In most situations in the compiler
// (such as with values of variables and slots.), we we are
// manipulating a single value (<ctype> class), and don't have to
// worry about multi-value types.
//
// Instead we provide new operations which are analogous to the
// one-value operations (and delegate to them in the single-value
// case.)  These operations are optimized for utility rather than
// exactness, but it is guaranteed that it will be no smaller (no more
// restrictive) than the precise result.
//
// With values types such as:
//    values(a0, a1)
//    values(b0, b1)
//
// We compute the more useful result:
//    values(OP(a0, b0), OP(a1, b1))
//
// Rather than the precise result:
//    OP(values(a0, a1), values(b0, b1))
//
// This has the virtue of always keeping the values type specifier
// outermost (so that it is easily stripped off or decoded), and
// retains all of the information that is really useful for static
// type analysis.  We want to know what is always true of each value
// independently.  It is worthless to know that IF the first value is
// B0 then the second will be B1.


// <multi-value-ctype> holds information about values for situations
// (function return, etc.) where there are more than one value.  We
// extend the slot accessors to handle the degenerate case of a
// 1-value <ctype>.
//
// In order to allow the result of union or intersection of
// values-types to be represented more precisely, we allow some
// vagueness in the number of "positional" values.  If we union (Y1,
// Y2) and (X1), then the positional types are (Y1 union X1, Y2), and
// the min-values is 1.  Y2 is thus sort of an "optional" result type.
//
define class <multi-value-ctype> (<values-ctype>)
  // Types of each specifically typed value.  Values > than min-values
  // might not actually be returned.
  constant slot positional-types :: <list>, required-init-keyword: positional-types:;

  // The minimum number of values that will ever be returned (<= to
  // positional-types.)
  constant slot min-values :: <integer>, required-init-keyword: min-values:;

  // Type of the rest values; empty-ctype if none.
  constant slot rest-value-type :: <ctype>, required-init-keyword: rest-value-type:;
end class;

define sealed domain make (singleton(<multi-value-ctype>));

define method print-object (type :: <multi-value-ctype>, stream :: <stream>)
    => ();
  pprint-logical-block
    (stream,
     prefix: "{",
     body: method (stream)
	     write-class-name(type, stream);
	     write-element(stream, ' ');
	     pprint-indent(#"block", 2, stream);
	     pprint-newline(#"linear", stream);
	     pprint-logical-block
	       (stream,
		prefix: "(",
		body: method (stream)
			for (pos-type in type.positional-types,
			     count from 0)
			  unless (count == 0)
			    write(stream, ", ");
			    pprint-newline(#"linear", stream);
			  end;
			  if (count == type.min-values)
			    write(stream, "#optional ");
			  end;
			  print(pos-type, stream);
			end;
			unless (type.rest-value-type == empty-ctype())
			  unless (type.positional-types == #())
			    write(stream, ", ");
			    pprint-newline(#"linear", stream);
			  end;
			  write(stream, "#rest ");
			  print(type.rest-value-type, stream);
			end;
		      end,
		suffix: ")");
	   end,
     suffix: "}");
end;

define method print-message (type :: <multi-value-ctype>, stream :: <stream>)
    => ();
  write(stream, "values(");
  for (type in type.positional-types,
       count from 0)
    unless (count == 0)
      write(stream, ", ");
    end;
    if (count == type.min-values)
      write(stream, "#optional ");
    end;
    print-message(type, stream);
  end;
  unless (type.rest-value-type == empty-ctype())
    unless (type.positional-types == #())
      write(stream, ", ");
    end;
    write(stream, "#rest ");
    print-message(type.rest-value-type, stream);
  end;
  write(stream, ")");
end;

// make-values-ctype  --  Exported
//
// Make a potentially multi-value ctype.  If there is only one value,
// just return that.  If #rest object, return wild-ctype().
//
define function make-values-ctype
  (req :: <list>, rest :: false-or(<ctype>)) => res :: <values-ctype>;
  if (member?(empty-ctype(), req))
    empty-ctype();
  else
    let nreq = req.size;
    if (nreq == 1 & (rest == #f | rest == empty-ctype()))
      req.first;
    elseif (nreq == 0 & rest == object-ctype())
      wild-ctype();
    else
      make(<multi-value-ctype>, positional-types: req, min-values: nreq,
	   rest-value-type: rest | empty-ctype());
    end if;
  end if;
end function make-values-ctype;

   
define generic positional-types (type :: <values-ctype>) => res :: <list>;

define method positional-types (type :: <ctype>) => res :: <list>;
  list(type);
end method;

define generic min-values (type :: <values-ctype>) => res :: <integer>;

define method min-values (type :: <ctype>) => res :: <integer>;
  1;
end;

define generic rest-value-type (type :: <values-ctype>) => res :: <ctype>;

define method rest-value-type (type :: <ctype>) => res :: <ctype>;
  empty-ctype();
end method;


// ctype-extent-dispatch{<multi-value-ctype>}
//
// Return a multi-value-ctype that contains the extents of each of the
// component types.
//
define method ctype-extent-dispatch (type :: <multi-value-ctype>)
    => res :: <multi-value-ctype>;
  make(<multi-value-ctype>,
       positional-types: map(ctype-extent, type.positional-types),
       min-values: type.min-values,
       rest-value-type: type.rest-value-type.ctype-extent);
end method ctype-extent-dispatch;



/// Multi-value type operations:

// Fixed-Values-Op  --  Internal
//
// Return a list of Operation applied to the types in Types1 and
// Types2, padding the shorter with the appropriate rest values as
// needed.  The second value is #t if Operation always returned a true
// second value.
//
define function fixed-values-op (types1 :: <list>, types2 :: <list>,
	    rest1 :: <ctype>, rest2 :: <ctype>,
	    operation :: <function>)
	=> (res :: <list>, exact :: <boolean>);
      let exact = #t;
      let result = #();
      for (t1 = types1 then t1.tail,  t2 = types2 then t2.tail,
	   until: t1 == #() & t2 == #())
	let type1 = if (t1 == #()) rest1 else t1.head end;
	let type2 = if (t2 == #()) rest2 else t2.head end;
	let (res, win) = operation(type1, type2);
	unless (win) exact := #f end;
	result := pair(res, result);
      end for;
      values(reverse!(result), exact);
end function;

// Args-Type-Op  --  Internal
//
// If the values count signatures differ, then we produce result with
// the required value count chosen by Min-Fun when applied to the
// number of required values in type1 and type2.
//
// The second value is true if the result is definitely empty or if
// Operation returned true as its second value each time we called it.
// Since we approximate the intersection of values types, the second
// value being true doesn't mean the result is exact.
//
define generic args-type-op
    (type1 :: <values-ctype>, type2 :: <values-ctype>, operation :: <function>,
     min-fun :: <function>)
    => (res :: <values-ctype>, win? :: <boolean>);

define method args-type-op
    (type1 :: <ctype>, type2 :: <ctype>, operation :: <function>,
     min-fun :: <function>)
    => (res :: <values-ctype>, win? :: <boolean>);
  operation(type1, type2);
end;

define method args-type-op
    (type1 :: <values-ctype>, type2 :: <values-ctype>, operation :: <function>,
     min-fun :: <function>)
    => (res :: <values-ctype>, win? :: <boolean>);
  let rest1 = type1.rest-value-type;
  let rest2 = type2.rest-value-type;
  let (rest, rest-exact) = operation(rest1, rest2);
  let (res, res-exact)
    = fixed-values-op(type1.positional-types, type2.positional-types,
		      rest1, rest2, operation);
  let min-values = min-fun(type1.min-values, type2.min-values);
  let empty = empty-ctype();
  if (min-values == 1 & rest == empty & res ~== #() & res.tail == #())
    values(res.first, res-exact & rest-exact);
  else
    let empty-type-posn :: false-or(<integer>)
      = find-key(res, method (x) x == empty end);
    if (empty-type-posn)
      if (empty-type-posn < min-values)
	values(empty-ctype(), #t);
      else
	values(make(<multi-value-ctype>,
		    positional-types: copy-sequence(res, end: empty-type-posn),
		    min-values: min-values, rest-value-type: empty),
	       res-exact);
      end if;
    else
      values(make(<multi-value-ctype>, positional-types: res,
		  min-values: min-values, rest-value-type: rest),
	     res-exact & rest-exact);
    end;
  end;
end method;


// Values-Type-Union, Values-Type-Intersection  --  Interface
//
// Do a union or intersection operation on types that might be values
// types.
//
define function values-type-union
    (type1 :: <values-ctype>, type2 :: <values-ctype>)
    => (res :: <values-ctype>, win :: <boolean>);
  if (type1 == empty-ctype())
    values(type2, #t);
  elseif (type2 == empty-ctype())
    values(type1, #t);
  else
    args-type-op(type1, type2,
		 method (t1, t2)
		   let res = ctype-union(t1, t2);
		   values(res, ~instance?(res, <unknown-ctype>));
		 end,
		 min);
  end;
end function values-type-union;

define function values-type-intersection
    (type1 :: <values-ctype>, type2 :: <values-ctype>)
    => (res :: <values-ctype>, win :: <boolean>);
  if (type1 == empty-ctype() | type2 == empty-ctype())
    values(empty-ctype(), #t);
  else
    args-type-op(type1, type2, ctype-intersection, max);
  end;
end function values-type-intersection;


// Values-Types-Intersect?  --  Interface
//
// Like CTypes-Intersect?, except that it sort of works on values
// types.  Note that due to the semantics of Values-Type-Intersection,
// this might return {T, T} when there isn't really any intersection
// (?).
//
define generic values-types-intersect?
    (type1 :: <values-ctype>, type2 :: <values-ctype>)
    => (result :: <boolean>, precise :: <boolean>);

define method values-types-intersect?
    (type1 :: <ctype>, type2 :: <ctype>)
    => (result :: <boolean>, precise :: <boolean>);
  ctypes-intersect?(type1, type2);
end;

define method values-types-intersect?
    (type1 :: <values-ctype>, type2 :: <values-ctype>)
    => (result :: <boolean>, precise :: <boolean>);
  if (type1 == empty-ctype() | type2 == empty-ctype())
    values(#t, #t);
  else
    let (res, win) = values-type-intersection(type1, type2);
    values(~(res == empty-ctype()), win);
  end;
end method;


// Values-Subtype?  --  Interface
//
// A subtypep-like operation that can be used on any types, including
// values types.  This is something like the result type congruence
// rule.
//
define generic values-subtype?
    (type1 :: <values-ctype>, type2 :: <values-ctype>)
    => (result :: <boolean>, precise :: <boolean>);

define method values-subtype?
    (type1 :: <ctype>, type2 :: <ctype>)
    => (result :: <boolean>, precise :: <boolean>);
  csubtype?(type1, type2);
end;

define method values-subtype?
    (type1 :: <values-ctype>, type2 :: <values-ctype>)
    => (result :: <boolean>, precise :: <boolean>);
  if (type2 == wild-ctype())
    values(#t, #t)
  elseif (~values-types-intersect?(type1, type2))
    values(#f, #t);
  else
    let types1 = type1.positional-types;
    let rest1 = type1.rest-value-type;
    let types2 = type2.positional-types;
    let rest2 = type2.rest-value-type;
    if (type1.min-values < type2.min-values)
      values(#f, #t);
    else
      block (done)
	for (rem1 = types1 then rem1.tail,
	     rem2 = types2 then rem2.tail,
	     until: rem1 == #() & rem2 == #())
	  let t1 = if (rem1 == #()) rest1 else rem1.head end;
	  let t2 = if (rem2 == #()) rest2 else rem2.head end;
	  let (res, win-p) = csubtype?(t1, t2);
	  unless (win-p) done(#f, #f) end;
	  unless (res) done(#f, #t) end;
	end for;
	csubtype?(rest1, rest2);
      end block;
    end;
  end;
end method;


//// Accessors.

define variable *wild-ctype-memo* :: false-or(<multi-value-ctype>) = #f;

define function wild-ctype () => res :: <multi-value-ctype>;
      *wild-ctype-memo*
	| (*wild-ctype-memo*
	     := make(<multi-value-ctype>,
		     positional-types: #(),
		     rest-value-type: object-ctype(),
		     min-values: 0));
end;

define variable *object-ctype-memo* :: false-or(<ctype>) = #f;

define function object-ctype () => res :: <ctype>;
      *object-ctype-memo*
	| (*object-ctype-memo*
	     := dylan-value(#"<object>") | error("<object> undefined?"));
end;

define variable *function-ctype-memo* :: false-or(<ctype>) = #f;

define function function-ctype () => res :: <ctype>;
      *function-ctype-memo*
	| (*function-ctype-memo*
	     := dylan-value(#"<function>") | error("<function> undefined?"));
end;

define variable *class-ctype-memo* :: false-or(<ctype>) = #f;

define function class-ctype () => res :: <ctype>;
      *class-ctype-memo*
	| (*class-ctype-memo*
	     := dylan-value(#"<class>") | error("<class> undefined?"));
end;

define variable *boolean-ctype-memo* :: false-or(<ctype>) = #f;

define function boolean-ctype () => res :: <ctype>;
      *boolean-ctype-memo*
	| (*boolean-ctype-memo*
	     := dylan-value(#"<boolean>") | error("<boolean> undefined?"));
end;

define variable *empty-ctype-memo* :: false-or(<ctype>) = #f;

// The empty-type (bottom) is the union of no members.
define function empty-ctype () => res :: <ctype>;
      *empty-ctype-memo*
	| (*empty-ctype-memo* := make(<union-ctype>,
				      members: #(),
				      type-hash: 0));
end;


/// Type specifiers.

define constant <type-specifier> = type-union(<symbol>, <list>);

define generic specifier-type (spec :: <type-specifier>)
    => res :: <values-ctype>;


define constant $cache-size = 997;  // The 168'th prime

define constant $specifier-type-cache
  = make(<object-table>, size: $cache-size);


define method specifier-type (spec :: <type-specifier>)
    => res :: <values-ctype>;
  let res = element($specifier-type-cache, spec, default: #f);
  if (res)
    res;
  else
    // Fully eval the specializer before updating the cache.
    let new = slow-specifier-type(spec);
    element($specifier-type-cache, spec) := new;
  end;
end;

define generic slow-specifier-type (spec :: <type-specifier>)
    => res :: <values-ctype>;

define method slow-specifier-type (symbol :: <symbol>)
    => res :: <values-ctype>;
  dylan-value(symbol) | error("Type %s is undefined.", symbol);
end;

define method slow-specifier-type (list :: <list>)
    => res :: <values-ctype>;
  slow-specifier-type-list(list.head, list.tail);
end;

define generic slow-specifier-type-list (sym :: <symbol>, args :: <list>)
    => res :: <values-ctype>;

define method slow-specifier-type-list (sym == #"union", args :: <list>)
    => res :: <values-ctype>;
  for (arg in args,
       result = empty-ctype()
	 then values-type-union(result, specifier-type(arg)))
  finally
    result;
  end;
end;

define method slow-specifier-type-list (sym == #"intersection", args :: <list>)
    => res :: <values-ctype>;
  for (arg in args,
       result = object-ctype()
	 then values-type-intersection(result, specifier-type(arg)))
  finally
    result;
  end;
end;

define method slow-specifier-type-list
    (sym :: one-of(#"singleton", #"one-of"), args :: <list>)
    => res :: <values-ctype>;
  for (arg in args,
       result = empty-ctype()
	 then values-type-union(result,
				make(<singleton-ctype>,
				     value: specifier-ct-value(arg))))
  finally
    result;
  end;
end;

define generic specifier-ct-value (arg :: <object>) => res :: <ct-value>;

define method specifier-ct-value (arg :: <integer>) => res :: <ct-value>;
  make(<literal-integer>, value: arg);
end;

define method specifier-ct-value
    (arg :: <extended-integer>) => res :: <ct-value>;
  make(<literal-extended-integer>, value: arg);
end;

define method specifier-ct-value (arg == #f) => res :: <ct-value>;
  make(<literal-false>);
end;

define method specifier-ct-value (arg == #t) => res :: <ct-value>;
  make(<literal-true>);
end;

define method specifier-ct-value (arg == #()) => res :: <ct-value>;
  make(<literal-empty-list>);
end;

define method specifier-ct-value (arg :: <symbol>) => res :: <ct-value>;
  make(<literal-symbol>, value: arg);
end;


define method slow-specifier-type-list (sym == #"values", args :: <list>)
    => res ::<values-ctype>;
  let num-args = args.size;
  if (num-args >= 2 & args[num-args - 2] == #"rest")
    for (arg in args,
	 i from 0 below num-args - 2,
	 positionals = #() then pair(specifier-type(arg), positionals))
    finally
      make-values-ctype(reverse!(positionals),
			specifier-type(args[num-args - 1]));
    end;
  else
    make-values-ctype(map(specifier-type, args), #f);
  end;
end;

