Module: ctype
Description: compile-time type system
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/ctype.dylan,v 1.45 1996/03/20 01:44:03 rgs Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

/*
Todo: 
  subclass types (unknown or union of singletons if sealed)
*/

/// Superclass of multi-value types and regular single types.
define abstract class <values-ctype> (<object>)
end class;


//// Type function memoization:
///
/// Our primary approach for getting good performance on the type operations is
/// to use memoization, rather than trying to come up with clever ways to
/// quickly determine type relationships.  This based on the observation that
/// relatively few types are actually in use at any given time, and that the
/// compiler does the same operations over and over.

/// Memoization is based on the type-hash, which is a slot shared by all
/// compile-time types.
///
/// <ctype> objects are also hash-consed, which (modulo unknown types)
/// means that == is type equivalence.


define abstract class <ctype> (<values-ctype>)
  constant slot type-hash :: <integer> = random-bits(),
    init-keyword: type-hash:;
end class;


/// Memoization is done in a vector.  Each entry has four elements: the two arg
/// types, the result type and the result precise flag.  All elements are
/// initialized to #F, which ensures that empty entries will miss (since the
/// arguments are always types.)  This memoization is a probablistic cache, not
/// a complete record of all results ever computed.
///
/// ### vector could be limited to type-union(<ctype>, one-of(#t, #f));
///
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

#end

/// log2 of the the number of entries in the table.
define constant memo2-bits = 9;

// mask which gives a vector index from a large hash value.  Low zeros align to
// start of an entry.
define constant memo2-size = ash(1, memo2-bits);
define constant memo2-mask = memo2-size - 1;

define constant make-memo2-table = method ()
  make(<memo-table>, size: memo2-size, fill: $null-memo-entry);
end method;

// some hit rate info, for tuning.
define variable *memo2-hits* :: <integer> = 0;
define variable *memo2-probes* :: <integer> = 0;

// See if Type1 & Type2 are memoized in Table.  If so, the two memoized values
// are returned.  If not, we return #"miss" and #f;
define constant memo2-lookup = method
   (type1 :: <ctype>, type2 :: <ctype>, table :: <memo-table>)
    => (value :: type-union(<ctype>, <boolean>, singleton(#"miss")),
        precise :: <boolean>);

  *memo2-probes* := *memo2-probes* + 1;
#if (mindy)
  let base = modulo(type1.type-hash - type2.type-hash, memo2-size);
#else
  let base = logand(type1.type-hash - type2.type-hash, memo2-mask);
#end
  let entry :: <memo-entry> = table[base];
  if (entry.memo-type1 == type1 & entry.memo-type2 == type2)
    *memo2-hits* := *memo2-hits* + 1;
    values(entry.memo-value, entry.memo-precise);
  else
    values(#"miss", #f);
  end;
end method;

define constant memo2-enter = method
   (type1 :: <ctype>, type2 :: <ctype>,
    result :: type-union(<ctype>, <boolean>),
    precise :: <boolean>, table :: <memo-table>)
#if (mindy)  
  let base = modulo(type1.type-hash - type2.type-hash, memo2-size);
#else
  let base = logand(type1.type-hash - type2.type-hash, memo2-mask);
#end
  let entry :: <memo-entry> = table[base];
  if (entry == $null-memo-entry)
    entry := make(<memo-entry>);
    table[base] := entry;
  end if;
  entry.memo-type1 := type1;
  entry.memo-type2 := type2;
  entry.memo-value := result;
  entry.memo-precise := precise;
end method;
 

//// Equality:
///
///  Since ctypes are hash-consed, equality/inequality is pretty degenerate.
/// The only problem area is with unknown types (whice we could not or elected
/// not to evaluate at compile time.)  Unknown types may be spuriously ~==,
/// so to ensure a precise result we must test for unknown types.  

///    If two types are definitely equivalent, return true.  The second value
/// indicates whether the first value is definitely correct.  This should only
/// fail in the presence of Unknown types.
///
define constant ctype-eq? = method (type1 :: <ctype>, type2 :: <ctype>)
       => (result :: <boolean>, precise :: <boolean>);
  
  if (type1 == type2)
    values(#t, #t);
  else
    values(#f, ~(instance?(type1, <unknown-ctype>) 
                 | instance?(type2, <unknown-ctype>)))
  end;
end method;

/// Similar to ctype-eq, but we return true if the types are definitely not the
/// same.
///
define constant ctype-neq? = method (type1 :: <ctype>, type2 :: <ctype>)
       => (result :: <boolean>, precise :: <boolean>);
  
  if (type1 == type2)
    values(#f, #t);
  elseif (instance?(type1, <unknown-ctype>)
	    | instance?(type2, <unknown-ctype>))
    values(#f, #f);
  else
    values(#t, #t);
  end;
end method;


/// find-direct-classes  --  exported
///
///    Given an arbitrary type, return a list of all the classes that a value
/// of that type could possibly be direct instances of.  If we can't determine
/// this (because an open class is involved) then return #f.  We could
/// potentially return #() if there is no possibly non-abstract class.
///
define generic find-direct-classes(type :: <ctype>) => res :: false-or(<list>);


//// CINSTANCE? -- exported
//
define generic cinstance? (ctv :: <ct-value>, ctype :: <ctype>)
    => (result :: <boolean>, precise :: <boolean>);

define method cinstance? (ctv :: <ct-value>, ctype :: <ctype>)
    => (result :: <boolean>, precise :: <boolean>);
  csubtype?(ctv.ct-value-cclass, ctype);
end;

define method cinstance? (ctv :: <eql-ct-value>, ctype :: <ctype>)
    => (result :: <boolean>, precise :: <boolean>);
  csubtype?(ctv.make-canonical-singleton, ctype);
end;


//// CSUBTYPE?
///

/// Ignoring unknown and union types (which are handled specially), we have the
/// following cross products:
///
/// Singleton with:
///   singleton -- #f, 'cause == singletons are picked off
///   limited-int -- #f, 'cause singleton integers don't exist due to
///     canonicalization.
///   class -- subtype?(base-class, class)
///   direct -- singleton.base-class == direct.base-class)
///   byte-char -- is the singleton a byte-character?
///   heap-instance -- subtype?(base-class, heap-inst)
/// limited integer:
///   singleton -- #f, 'cause singleton integers don't exist due to
///     canonicalization.
///   limited integer -- the base classes are subtype? and the ranges are in
///     order
///   class -- subtype?(base-class, class)
///   direct -- limint.base-class == direct.base-class
///   byte-char -- #f
///   heap-instance -- subtype?(base-class, heap-inst)
/// class:
///   singleton -- #f, 'cause #t, #f, and #() don't exist as singletons due to
///     canonicalization.
///   limited integer -- #f, 'cause infinate bound limited integers are
///     canonicalized back to the base class.
///   class -- check the class precedence list
///   direct -- #f, 'cause direct-type(leaf-class) => leaf-class
///   byte-char -- #f
///   heap-instance -- #t if the class is guarenteed to be heap allocated.
/// Direct:
///   singleton -- #f.
///   limited-int -- #f.
///   class -- subtype?(base-class, class)
///   direct -- #f, 'cause == direct classes are picked off.
///   byte-char -- #f
///   heap-instance -- #t iff the base-class is heap allocated.
/// byte-char:
///   singleton -- #f.
///   limited-int -- #f.
///   class -- subtype?(bchar.base-class, class)
///   direct -- bchar.base-class == direct.base-class
///   byte-char -- can't happen, cause == types are picked off.
///   heap-instance -- #t iff the base-class is heap allocated.
/// heap-instance:
///   singleton -- #f;
///   limited-int -- #f;
///   class -- class == <object>
///   direct -- #f;
///   byte-char -- #f;
///   heap-instance -- can't happen, cause == types are picked off.
///   
/// Note that for most of the subtype?(limited,other) cases, it just becomes
/// subtype?(limited.base-class,other).
///

/// Handle csubtype? for unequal types other than union and unknown.
/// Result is always precise because any vagueness is in the unknown types.
///
define sealed generic csubtype-dispatch(type1 :: <ctype>, type2 :: <ctype>)
       => result :: <boolean>;

/// Many cases are false, so make that the default.
define method csubtype-dispatch(type1 :: <ctype>, type2 :: <ctype>)
    => result :: <boolean>;
  #f;
end method;

define constant $csubtype-memo :: <memo-table> = make-memo2-table();

/// Like subtype?, but works on ctypes, and returns the second value #F if the
/// relation cannot be determined at compile time (due to unknown types.)
///
/// Check if result is memoized; if not, pick off the unknown & union cases
/// before calling the generic function.
/// 
define constant csubtype? = method (type1 :: <ctype>, type2 :: <ctype>)
       => (result :: <boolean>, precise :: <boolean>);

  case
    // Makes unknown types be subtypes of themselves, & eliminates the case of
    // equal types from later consideration.  Also speeds up a common case...
    (type1 == type2) => values(#t, #t);
    
    // the only thing an unknown type is surely a subtype of is <object>
    instance?(type1, <unknown-ctype>) =>
      if (type2 == object-ctype()) values(#t,#t) else values(#f, #f) end;
      
    // nothing is a definite subtype of an unknown type (except itself.)
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
end method;


//// Intersection:

/// Ignoring unknowns and unions (which are handled specially), we have the
/// following possible combinations.  Note: we only list half of them, because
/// intersection is commutative.
///
/// Singleton with:
///   singleton -- empty, 'cause == singletons are picked off.
///   limited-int -- empty, 'cause singleton integers don't exist due to
///     canonicalization.
///   class -- the singleton if subtype?, otherwise empty
///   direct -- the singleton if subtype?, otherwise empty
///   byte-char -- the singleton if subtype?, otherwise empty
///   heap-instance -- the singletion if subtype?, otherwise empty
/// Limited integer with:
///   limited-int -- the intersection of the base classes and range.
///   class -- the intersection of the base class and the other class, same
///     range
///   direct -- empty, 'cause the direct integer classes are canonicalized into
///     themselves.
///   byte-char -- empty
///   heap-instance -- intersection of the base class and the other, same range
/// Class:
///   class -- the subtype class if one is a subtype of the other, the
///     intersection of their subtypes if both are sealed, or one at random
///     otherwise.
///   direct -- the direct type if it is a subtype of the class, empty
///     otherwise
///   byte-char -- the byte-char type if it is a subtype of the class
///   heap-instance -- the class,#t if subtype, empty if the class is
///     immediate, otherwise class,#f
/// Direct:
///   direct -- the type when they are the same, otherwise empty
///   byte-char -- the byte-char type if it is a subtype of the class
///   heap-instance -- the direct if the class is heap allocated, empty
///     otherwise
/// Byte-char:
///   byte-char -- won't be called, 'cause the == case is picked off
///   heap-instance -- empty
/// heap-instance
///   heap-instance -- won't be called, 'cause the == case is picked off
///
/// So if we pick off the case where one type is a subtype of the other first,
/// we have the following cases left:
///
/// Limited integer with:
///   limited-int -- the intersection of the base classes and range.
///   class -- the intersection of the base class and the other class, same
///     range
/// Class:
///   class -- the subtype class if one is a subtype of the other, the
///     intersection of their subtypes if both are sealed, or one at random
///     otherwise.
///

/// Handle ctype-intersection for unequal types other than union and unknown.
/// Result may be imprecise if we intersect two non-sealed classes.
///
define sealed generic ctype-intersection-dispatch
    (type1 :: <ctype>, type2 :: <ctype>)
     => (result :: false-or(<ctype>), precise :: <boolean>);

/// indicates try swapping args, or if that failed, the result is empty.
define method ctype-intersection-dispatch(type1 :: <ctype>, type2 :: <ctype>)
     => (result :: false-or(<ctype>), precise :: <boolean>);
  values(#f, #t);
end method;


define constant $intersection-memo :: <memo-table> = make-memo2-table();

///    Return as restrictive a type as we can discover that is no more
/// restrictive than the intersection of Type1 and Type2.  The second value is
/// true if the result is exact.  At worst, we arbitrarily return one of the
/// arguments as the first value (trying not to return an unknown type).
///
define constant ctype-intersection
  = method (type1 :: <ctype>, type2 :: <ctype>)
     => (result :: <ctype>, precise :: <boolean>);

      if (type1 == type2)
	values(type1, #t);
      else
	let (memo-val, memo-win) = memo2-lookup(type1, type2,
						$intersection-memo);
	if (memo-val == #"miss")
	  let (val, win) = 
	    case
	      // Makes unknown types intersect with themselves, & eliminates
	      // the case of equal types from later consideration.
	      // If one arg is unknown, return the other and #f.
	      instance?(type1, <unknown-ctype>) => values(type2, #f);
	      instance?(type2, <unknown-ctype>) => values(type1, #f);
	  
	      // Otherwise, the intersection is the union of the pairwise
	      // intersection of the members.  As described above, we try both
	      // orders. 
	      otherwise =>
		let win-int = #t;
		let res-union = empty-ctype();
		for (mem1 in type1.members)
		  for (mem2 in type2.members)
		    // If either is a subtype of the other, include the
		    // subtype.
		    if (csubtype?(mem1, mem2))
		      res-union := ctype-union(res-union, mem1);
		    elseif (csubtype?(mem2, mem1))
		      res-union := ctype-union(res-union, mem2);
		    else
		      // Call the intersection dispatch function.
		      let (res12, win12) = ctype-intersection-dispatch(mem1, mem2);
		      if (res12)
			unless (win12) win-int := #f end;
			res-union := ctype-union(res-union, res12);
		      else
			let (res21, win21) = ctype-intersection-dispatch(mem2, mem1);
			if (res21)
			  unless (win21) win-int := #f end;
			  res-union := ctype-union(res-union, res21);
			  
			  // else precisely empty, nothing to union.
			end if;
		      end if;
		    end if;
		  end for;
		end for;
		values(res-union, win-int);
	    end case;

	  memo2-enter(type1, type2, val, win, $intersection-memo);
	  values(val, win);
	else
	  values(memo-val, memo-win);
	end;
      end if;
    end method;


/// The first value is true unless the types definitely don't intersect.  The
/// second value is true if the first value is definitely correct.  empty-ctype
/// is considered to intersect with any type.  If either type is <object>, we
/// also return #T, #T.  This way we consider unknown types to intersect with
/// <object>.
///
define constant ctypes-intersect? = method (type1 :: <ctype>, type2 :: <ctype>)
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
end method;


/// Difference.


define constant $difference-memo :: <memo-table> = make-memo2-table();


/// Return our best guess at the type that describes all objects that are in
/// Type1 but not in Type2.  If we can't precisely determine this type, then
/// return something more inclusive than it, but never more inclusive than
/// type1.
///
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

define generic members (type :: <ctype>) => members :: <list>;

define class <union-ctype> (<ctype>, <ct-value>)
  // list of ctypes in the union, which can only be classes, limited types or
  // singletons.  Any nested unions are flattened into this one, and the union
  // of anything and an unknown type is itself an unknown type.
  slot members :: <list>, setter: #f, required-init-keyword: members:;
end class;

define method print-object (union :: <union-ctype>, stream :: <stream>) => ();
  pprint-logical-block
    (stream,
     prefix: "{",
     body: method (stream)
	     write-class-name(union, stream);
	     write(' ', stream);
	     pprint-indent(#"block", 2, stream);
	     pprint-newline(#"linear", stream);
	     pprint-logical-block
	       (stream,
		prefix: "(",
		body: method (stream)
			for (member in union.members, first? = #t then #f)
			  unless(first?)
			    write(", ", stream);
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
  write("type-union(", stream);
  for (member in union.members, first? = #t then #f)
    unless(first?)
      write(", ", stream);
    end;
    print-message(member, stream);
  end;
  write(')', stream);
end;

// The "members" of any non-union type is a list of the type itself.
define method members (type :: <ctype>) => result :: <list>;
  list(type);
end method;


// Most type ops have a non-generic wrapper that handles unions and unknowns.
// Not so for find-direct-classes.
define method find-direct-classes(type :: <union-ctype>)
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
      finally remove-duplicates(res);
    end;
  end;
end method;


// Eliminate subtypes and adjacent integer types.  Result sorted by hash
// code for canonical order.  Does not flatten unions or unknown types.
//
define constant canonicalize-union = method (types :: <list>) => res :: <list>;
  let other = #();
  let ints = #();
  for (elt in types)
    if (instance?(elt, <limited-integer-ctype>))
      // ### This isn't quite correct.  Limited-int-union might return a cclass.
      ints := limited-int-union(elt, ints);
    else
      other := pair(elt, other);
    end;
  end for;

  let res = #();
  let filtered = concatenate(other, ints);
  for (elt in filtered)
    // omit from result if already in result or a proper subtype of some other
    // element.
    unless (member?(elt, res)
            | any?(method (x) csubtype?(elt, x) & ~(x == elt) end,
		   filtered))
      res := pair(elt, res);
    end;
  end for;
    
  sort!(res,
        test: method (x, y)
        	x.type-hash < y.type-hash
	      end);
end method;


// table used for hash-consing union types.  Key is a list of <ctype>s.
define class <union-table> (<table>)
end class;

define method table-protocol(table :: <union-table>)
    => (test :: <function>, hash :: <function>);
  values(\=,
  	 method(key :: <list>)
	   for(elt :: <ctype> in key,
	       res :: <integer> = 0 then \+(res, elt.type-hash))
	   finally values(res, $permanent-hash-state);
	   end for;
	 end method);
end;

define constant $union-table :: <union-table> = make(<union-table>);
define constant $union-memo :: <memo-table> = make-memo2-table();


///    Find a type which includes both types.  The result is an unknown type if
/// either of the arguments are unknown; otherwise the result is precise.  This
/// result is simplified into the canonical form, thus is not a union type
/// unless there is no other way to represent the result.
///
/// If no members, the result is the empty type.  If one, it is that type.
/// Otherwise, check if a union with those members already exists before making
/// a new union type.
///
define constant ctype-union = method (type1 :: <ctype>, type2 :: <ctype>)
    => value :: <ctype>;

  let (value, precise) = memo2-lookup(type1, type2, $union-memo);
  case 
    ~(value == #"miss") => value;

    instance?(type1, <unknown-ctype>) =>
      make(<unknown-ctype>, type-exp: type1.type-exp);
   
    instance?(type2, <unknown-ctype>) =>
      make(<unknown-ctype>, type-exp: type2.type-exp);

    otherwise =>
      local frob(canonical)
	if (canonical == #())
	  empty-ctype()
	elseif (tail(canonical) == #())
	  head(canonical)
	else
	  let found = element($union-table, canonical, default: #f);
	  if (found)
	    found
	  else
	    $union-table[canonical] := make(<union-ctype>, members: canonical);
	  end;
	end;
      end method;

      let res = frob(
                 canonicalize-union(
 		  concatenate(type1.members, type2.members)));
      memo2-enter(type1, type2, res, #t, $union-memo);
      res;
  end;
end method;


/// <unknown-ctype> represents some random non-compile-time expression that
/// ought to be a type.
///
/// This should be interpreted as "some type whose meaning is unknown because
/// the value of EXP is unknown".  An unknown type is never CTYPE-EQ to itself
/// or to any other type.
///
define class <unknown-ctype> (<ctype>)

  // The expression which was of unknown type.  In general, this is only for
  // human context. 
  slot type-exp, init-value: #f, init-keyword: type-exp:;
end class;

define method find-direct-classes(type :: <unknown-ctype>) => res :: <false>;
  ignore(type);
  #f;
end;

define method print-message (ctype :: <unknown-ctype>, stream :: <stream>)
    => ();
  write("<unknown>", stream);
end;


//// Limited types:
///
/// The <limited-ctype> abstract class is inherited by various non-class types
/// where there is a class that is a "tight" supertype of the type.  This
/// includes singleton and direct-instance types.

define abstract class <limited-ctype> (<ctype>)
  // The most specific class that is a supertype of this type.
  slot base-class :: <cclass>, required-init-keyword: base-class:;
end class;

/// In general, a limited type is only a subtype of some other type if the limited
/// class is a subtype of that tpe.
/// 
define method csubtype-dispatch(type1 :: <limited-ctype>, type2 :: <ctype>)
    => result :: <boolean>;
  csubtype?(type1.base-class, type2);
end method;

define method find-direct-classes(type :: <limited-ctype>)
    => res :: false-or(<list>);
  find-direct-classes(type.base-class);
end;


/// Limited integer types:

// A limited-integer-table is used to keep track of all the limited integers we
// have already allocated, so we can reuse them.

define class <limited-integer-table> (<table>)
end;

define method table-protocol (table :: <limited-integer-table>)
    => (test :: <function>, hash :: <function>);
  values(\=,
	 method (key :: <vector>)
	   let (min-id, min-state) = equal-hash(key.second);
	   let (base&min-id, base&min-state)
	     = merge-hash-codes(key.first.type-hash, $permanent-hash-state,
				min-id, min-state, ordered: #t);
	   let (max-id, max-state) = equal-hash(key.third);
	   merge-hash-codes(base&min-id, base&min-state,
			    max-id, max-state, ordered: #t);
	 end);
end;

define constant $limited-integer-table = make(<limited-integer-table>);

define class <limited-integer-ctype> (<limited-ctype>, <ct-value>)
  slot low-bound :: false-or(<extended-integer>), 
       required-init-keyword: low-bound:;

  slot high-bound :: false-or(<extended-integer>), 
       required-init-keyword:  high-bound:;
end class;

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
	     write(' ', stream);
	     pprint-indent(#"block", 2, stream);
	     pprint-newline(#"linear", stream);
	     pprint-logical-block
	       (stream,
		prefix: "(",
		body: method (stream)
			print(limint.base-class, stream);
			if (limint.low-bound)
			  write(", ", stream);
			  pprint-newline(#"fill", stream);
			  format(stream, "min: %d", limint.low-bound);
			end;
			if (limint.high-bound)
			  write(", ", stream);
			  pprint-newline(#"fill", stream);
			  format(stream, "min: %d", limint.high-bound);
			end;
		      end,
		suffix: ")");
	   end,
     suffix: "}");
end;

define method print-message (limint :: <limited-integer-ctype>,
			     stream :: <stream>)
    => ();
  write("limited(", stream);
  print-message(limint.base-class, stream);
  if (limint.low-bound)
    format(stream, ", min: %d", limint.low-bound);
  end;
  if (limint.high-bound)
    format(stream, ", max: %d", limint.high-bound);
  end;
  write(')', stream);
end;

define method make-canonical-limited-integer
    (base-class :: <cclass>,
     low-bound :: false-or(<general-integer>),
     high-bound :: false-or(<general-integer>))
    => res :: <ctype>;
  if (low-bound)
    low-bound := as(<extended-integer>, low-bound);
  end;
  if (high-bound)
    high-bound := as(<extended-integer>, high-bound);
  end;
  
  if (base-class == specifier-type(#"<integer>"))
    if (~low-bound | low-bound < runtime-$minimum-integer)
      low-bound := runtime-$minimum-integer;
    end;
    if (~high-bound | high-bound > runtime-$maximum-integer)
      high-bound := runtime-$maximum-integer;
    end;
    if (high-bound < low-bound)
      empty-ctype();
    elseif (low-bound == runtime-$minimum-integer
	      & high-bound == runtime-$maximum-integer)
      base-class;
    else
      make(<limited-integer-ctype>, base-class: base-class,
	   low-bound: low-bound, high-bound: high-bound);
    end;
  else
    if (~high-bound & ~low-bound)
      base-class;
    elseif (high-bound & low-bound & high-bound < low-bound)
      empty-ctype();
    else
      make(<limited-integer-ctype>, base-class: base-class,
	   low-bound: low-bound, high-bound: high-bound);
    end;
  end;
end;

/// A limited integer type is a subtype of another if the bounds of type1 are
/// not wider that type2's (and the base class is a subtype.)
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
  elseif (instance?(base, <cclass>))
    values(make-canonical-limited-integer(base, type1.low-bound, type1.high-bound),
	   #t);
  else
    error("Shouldn't happen.");
    #f;
  end;
end;

/// The intersection of two limited integer types is the overlap of the ranges.
/// We determine this by maximizing the lower bounds and minimizing the upper
/// bounds, returning that range if non-empty.
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


/// Return a new list of limited integer types with Int joined to any of the
/// types in Others that it intesects.  We don't bother removing the overlapped
/// type, since it will be removed by the subtype elimination pass later on.
///
define constant limited-int-union = method 
    (int :: <limited-integer-ctype>, others :: <list>) => res :: <list>;

  // Return true if the two types have overlapping or contiguous ranges.  Value
  // is arbitrary if one is a subtype of the other, which doesn't matter here.
  local adjacent?(type1, type2)
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
    let LO = other.low-bound;
    let HO = other.high-bound;

    if (base == other.base-class & adjacent?(int, other))
      if (LI ~= #f & (LO = #f | LO < LI))
        LI := LO;
      end;

      if (HI ~= #f & (HO = #f | HO > HI))
        HI := HO;
      end;
    end;
  end for;

  add-new!(others, make-canonical-limited-integer(base, LI, HI));
end method;


//// Direct instance types:

define class <direct-instance-ctype> (<limited-ctype>, <ct-value>)
end class;

define method print-object
    (type :: <direct-instance-ctype>, stream :: <stream>) => ();
  pprint-fields(type, stream, base-class: type.base-class);
end;

define method print-message
    (type :: <direct-instance-ctype>, stream :: <stream>) => ();
  format(stream, "direct-instances-of(%s)", type.base-class);
end;

define method csubtype-dispatch
    (type1 :: <limited-ctype>, type2 :: <direct-instance-ctype>)
    => result :: <boolean>;
  type1.base-class == type2.base-class;
end method csubtype-dispatch;



//// Singleton types:
///
/// We only represents singletons with compile-time constant (non-integer)
/// values.  Integer values are represented by limited integer types.  Note
/// that in Dylan the only compile-time constants are literals, so we are that
/// we are really restricted to float, character, symbol and magic tokens (#t,
/// #f, #()).  We omit the collection literals (string and list) with no real
/// loss, since they aren't meaningfully compared with ==.

/// ### we may also want to hack singletons of classes, e.g. for specializers.
/// singleton-of-class may want to be a seperate type, or maybe we have a magic
/// compile-time cookie representing the class, or something.

define class <singleton-ctype> (<limited-ctype>, <ct-value>)
  // The base-class is the direct class of this object, which can be used
  // interchangably with the object when testing this object for class
  // membership.

  // The value we represent.
  slot singleton-value :: <eql-ct-value>,
    required-init-keyword: singleton-value:;
end class;

define method print-object (sing :: <singleton-ctype>, stream :: <stream>)
    => ();
  pprint-logical-block
    (stream,
     prefix: "{",
     body: method (stream)
	     write-class-name(sing, stream);
	     write(' ', stream);
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


//// make-canonical-singleton:

// Return the ctype equivalent to singleton(object), where object is a
// compile-time value.
define generic make-canonical-singleton (thing :: <ct-value>, #key)
    => res :: <ctype>;

define method make-canonical-singleton (thing :: <ct-value>, #key)
    => res :: <ctype>;
  empty-ctype();
end;

define method make-canonical-singleton
    (thing :: <eql-ct-value>, #key base-class)
    => res :: <ctype>;
  thing.ct-value-singleton
    | (thing.ct-value-singleton
	 := really-make-canonical-singleton(thing, base-class));
end;

define generic really-make-canonical-singleton
    (thing :: <eql-ct-value>, base-class-hint :: false-or(<cclass>))
    => res :: <ctype>;

define method really-make-canonical-singleton
    (thing :: <eql-ct-value>, base-class-hint :: false-or(<cclass>))
    => res :: <ctype>;
  make(<singleton-ctype>,
       base-class: base-class-hint | ct-value-cclass(thing),
       singleton-value: thing);
end;

define method really-make-canonical-singleton
    (thing :: <literal-general-integer>, base-class-hint :: false-or(<cclass>))
    => res :: <ctype>;
  let value = thing.literal-value;
  make(<limited-integer-ctype>,
       base-class: base-class-hint | ct-value-cclass(thing),
       low-bound: value, high-bound: value);
end;

define method really-make-canonical-singleton
    (thing :: type-union(<literal-boolean>, <literal-empty-list>),
     base-class-hint :: false-or(<cclass>))
    => res :: <ctype>;
  base-class-hint | ct-value-cclass(thing);
end;



define generic ct-value-cclass (ct-value :: <ct-value>) => res :: <cclass>;

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

define method ct-value-cclass (object :: <singleton-ctype>) => res :: <cclass>;
  specifier-type(#"<singleton>");
end;

define method ct-value-cclass (object :: <direct-instance-ctype>)
    => res :: <cclass>;
  specifier-type(#"<direct-instance>");
end;

define method ct-value-cclass (object :: <byte-character-ctype>)
    => res :: <cclass>;
  specifier-type(#"<byte-character-type>");
end;



// <byte-character-ctype>

define class <byte-character-ctype> (<limited-ctype>, <ct-value>)
end;

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
  write("<byte-character>", stream);
end;

define method csubtype-dispatch (type1 :: <singleton-ctype>,
				 type2 :: <byte-character-ctype>)
    => res :: <boolean>;
  let val = type1.singleton-value;
  instance?(val, <literal-character>)
    & instance?(val.literal-value, <byte-character>);
end;


//// Multi-values types:

///
///    The normal type operations (type-union, csubtype?, etc.) are not allowed
/// on multi-value types.  In most situations in the compiler (such as with
/// values of variables and slots.), we we are manipulating a single value
/// (<ctype> class), and don't have to worry about multi-value types.
///
/// Instead we provide new operations which are analogous to the one-value
/// operations (and delegate to them in the single-value case.)  These
/// operations are optimized for utility rather than exactness, but it is
/// guaranteed that it will be no smaller (no more restrictive) than the
/// precise result.
///
/// With values types such as:
///    values(a0, a1)
///    values(b0, b1)
///
/// We compute the more useful result:
///    values(OP(a0, b0), OP(a1, b1))
///
/// Rather than the precise result:
///    OP(values(a0, a1), values(b0, b1))
///
/// This has the virtue of always keeping the values type specifier outermost
/// (so that it is easily stripped off or decoded), and retains all of the
/// information that is really useful for static type analysis.  We want to
/// know what is always true of each value independently.  It is worthless to
/// know that IF the first value is B0 then the second will be B1.


/// <multi-value-ctype> holds information about values for situations (function
/// return, etc.) where there are more than one value.  We extend the slot
/// accessors to handle the degenerate case of a 1-value <ctype>.
///
/// In order to allow the result of union or intersection of values-types to be
/// represented more precisely, we allow some vagueness in the number of
/// "positional" values.  If we union (Y1, Y2) and (X1), then the positional
/// types are (Y1 union X1, Y2), and the min-values is 1.  Y2 is thus sort of
/// an "optional" result type.
///
define class <multi-value-ctype> (<values-ctype>)

  // Types of each specifically typed value.  Values > than min-values might
  // not actually be returned.
  slot positional-types :: <list>, required-init-keyword: positional-types:;

  // The minimum number of values that will ever be returned (<= to
  // positional-types.)
  slot min-values :: <integer>, required-init-keyword: min-values:;

  // Type of the rest values; empty-ctype if none.
  slot rest-value-type :: <ctype>, required-init-keyword: rest-value-type:;
end class;

define method print-object (type :: <multi-value-ctype>, stream :: <stream>)
    => ();
  pprint-logical-block
    (stream,
     prefix: "{",
     body: method (stream)
	     write-class-name(type, stream);
	     write(' ', stream);
	     pprint-indent(#"block", 2, stream);
	     pprint-newline(#"linear", stream);
	     pprint-logical-block
	       (stream,
		prefix: "(",
		body: method (stream)
			for (pos-type in type.positional-types,
			     count from 0)
			  unless (count == 0)
			    write(", ", stream);
			    pprint-newline(#"linear", stream);
			  end;
			  if (count == type.min-values)
			    write("#optional ", stream);
			  end;
			  print(pos-type, stream);
			end;
			unless (type.rest-value-type == empty-ctype())
			  unless (type.positional-types == #())
			    write(", ", stream);
			    pprint-newline(#"linear", stream);
			  end;
			  write("#rest ", stream);
			  print(type.rest-value-type, stream);
			end;
		      end,
		suffix: ")");
	   end,
     suffix: "}");
end;

define method print-message (type :: <multi-value-ctype>, stream :: <stream>)
    => ();
  write("values(", stream);
  for (type in type.positional-types,
       count from 0)
    unless (count == 0)
      write(", ", stream);
    end;
    if (count == type.min-values)
      write("#optional ", stream);
    end;
    print-message(type, stream);
  end;
  unless (type.rest-value-type == empty-ctype())
    unless (type.positional-types == #())
      write(", ", stream);
    end;
    write("#rest ", stream);
    print-message(type.rest-value-type, stream);
  end;
  write(")", stream);
end;

// make-values-ctype  --  Exported
//
// Make a potentially multi-value ctype.  If there is only one value, just
// return that.  If #rest object, return wild-ctype().
//
define constant make-values-ctype = method
  (req :: <list>, rest :: false-or(<ctype>)) => res :: <values-ctype>;

 let nreq = req.size;
 if (nreq == 1 & ~rest)
   req.first;
 elseif (nreq == 0 & rest == object-ctype())
   wild-ctype();
 else
   make(<multi-value-ctype>, positional-types: req, min-values: nreq,
        rest-value-type: rest | empty-ctype());
	
 end;
end method;

   
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



//// Multi-value type operations:

/// Fixed-Values-Op  --  Internal
///
///    Return a list of Operation applied to the types in Types1 and Types2,
/// padding the shorter with the appropriate rest values as needed.
/// The second value is #t if Operation always returned a true second
/// value.
///
define constant fixed-values-op
  = method (types1 :: <list>, types2 :: <list>,
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
    end method;

/// Args-Type-Op  --  Internal
///
/// If the values count signatures differ, then we produce result with the
/// required value count chosen by Min-Fun when applied to the number of
/// required values in type1 and type2.
///
/// The second value is true if the result is definitely empty or if Operation
/// returned true as its second value each time we called it.  Since we
/// approximate the intersection of values types, the second value being true
/// doesn't mean the result is exact.
///
define generic args-type-op
    (type1 :: <values-ctype>, type2 :: <values-ctype>, operation :: <function>,
     min-fun :: <function>)
    => (res :: <values-ctype>, win? :: <boolean>);
///
define method args-type-op
    (type1 :: <ctype>, type2 :: <ctype>, operation :: <function>,
     min-fun :: <function>)
    => (res :: <values-ctype>, win? :: <boolean>);
  operation(type1, type2);
end;
///
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


/// Values-Type-Union, Values-Type-Intersection  --  Interface
///
///    Do a union or intersection operation on types that might be values
/// types.
///
define constant values-type-union = method
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
end method;
///
define constant values-type-intersection = method
    (type1 :: <values-ctype>, type2 :: <values-ctype>)
    => (res :: <values-ctype>, win :: <boolean>);
  if (type1 == empty-ctype() | type2 == empty-ctype())
    values(empty-ctype(), #t);
  else
    args-type-op(type1, type2, ctype-intersection, max);
  end;
end method;


/// Values-Types-Intersect?  --  Interface
///
///    Like CTypes-Intersect?, except that it sort of works on values types.
/// Note that due to the semantics of Values-Type-Intersection, this might
/// return {T, T} when there isn't really any intersection (?).
///
define generic values-types-intersect?
    (type1 :: <values-ctype>, type2 :: <values-ctype>)
    => (result :: <boolean>, precise :: <boolean>);
///
define method values-types-intersect?
    (type1 :: <ctype>, type2 :: <ctype>)
    => (result :: <boolean>, precise :: <boolean>);
  ctypes-intersect?(type1, type2);
end;
///
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


/// Values-Subtype?  --  Interface
///
///    A subtypep-like operation that can be used on any types, including
/// values types.  This is something like the result type congruence rule.
///
define generic values-subtype?
    (type1 :: <values-ctype>, type2 :: <values-ctype>)
    => (result :: <boolean>, precise :: <boolean>);
///
define method values-subtype?
    (type1 :: <ctype>, type2 :: <ctype>)
    => (result :: <boolean>, precise :: <boolean>);
  csubtype?(type1, type2);
end;
///
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

define constant wild-ctype
  = method () => res :: <multi-value-ctype>;
      *wild-ctype-memo*
	| (*wild-ctype-memo*
	     := make(<multi-value-ctype>,
		     positional-types: #(),
		     rest-value-type: object-ctype(),
		     min-values: 0));
    end;

define variable *object-ctype-memo* :: false-or(<ctype>) = #f;

define constant object-ctype
  = method () => res :: <ctype>;
      *object-ctype-memo*
	| (*object-ctype-memo*
	     := dylan-value(#"<object>") | error("<object> undefined?"));
    end;

define variable *function-ctype-memo* :: false-or(<ctype>) = #f;

define constant function-ctype
  = method () => res :: <ctype>;
      *function-ctype-memo*
	| (*function-ctype-memo*
	     := dylan-value(#"<function>") | error("<function> undefined?"));
    end;

define variable *class-ctype-memo* :: false-or(<ctype>) = #f;

define constant class-ctype
  = method () => res :: <ctype>;
      *class-ctype-memo*
	| (*class-ctype-memo*
	     := dylan-value(#"<class>") | error("<class> undefined?"));
    end;

define variable *empty-ctype-memo* :: false-or(<ctype>) = #f;

// The empty-type (bottom) is the union of no members.
define constant empty-ctype
    = method () => res :: <ctype>;
      *empty-ctype-memo*
	| (*empty-ctype-memo* := make(<union-ctype>,
				      members: #(),
				      type-hash: 0));
    end;


//// Type specifiers.

define constant <type-specifier> = type-union(<symbol>, <list>);

define generic specifier-type (spec :: <type-specifier>)
    => res :: <values-ctype>;


define constant $cache-size = 997;  // The 168'th prime

define constant $specifier-type-cache
  = make(<vector>, size: ash($cache-size, 1));


define method specifier-type (spec :: <type-specifier>)
    => res :: <values-ctype>;
  let (id, state) = object-hash(spec);
  let index = ash(modulo(id, $cache-size), 1);
  if ($specifier-type-cache[index] == spec)
    $specifier-type-cache[index + 1];
  else
    // Fully eval the specializer before updating the cache.
    let new = slow-specifier-type(spec);
    // If the state is no longer valid, recompute the index.
    // ### Except that we can't do this, because state-valid? isn't exported.
    //unless (state-valid?(state))
    //  index := ash(floor/(object-hash(spec), $cache-size), 1);
    //end;
    $specifier-type-cache[index] := spec;
    $specifier-type-cache[index + 1] := new;
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
				make-canonical-singleton
				  (specifier-ct-value(arg))))
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
