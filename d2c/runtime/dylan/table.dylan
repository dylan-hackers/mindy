module:	    dylan-viscera
Author:	    Nick Kramer (nkramer@cs.cmu.edu)
rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/table.dylan,v 1.1 1998/05/03 19:55:40 andreas Exp $
Synopsis:   Implements <table>, <object-table>, <equal-table>, 
            and <value-table>.

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
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
//    University.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports, questions, comments, and suggestions should be sent by
// E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
//
//======================================================================

// This code is a more or less implementation independent.  Almost all
// of the code that is implementation dependent is in the beginning of
// the file; the rest can be found with a search for "mindy".
//
// Author's note: "ht" is my abbreviation for "hashtable", and is used
// as a parameter quite frequently.


// Exported interface.

define open generic table-protocol (table :: <table>)
    => (key-test :: <function>, key-hash :: <function>);

define open generic value-hash (thing :: <object>)
    => (id :: <integer>, state :: <hash-state>);

define open generic equal-hash (thing :: <object>)
    => (id :: <integer>, state :: <hash-state>);



// -------------------------------------------------------------------
// Compiler-specific code
// -------------------------------------------------------------------

// Later, this will be "false-or(<some-type>)", when we actually have a
// non-conservative garbage collector.  At the moment, $permanent-hash-state
// is the only possible state.  In this case, we will also have to add new
// methods on "merge-hash-states", and update "pointer-hash".
//
define constant <hash-state> :: <type> = <false>;

define inline method state-valid? (state :: <hash-state>)
 => (result :: <true>);
  // At the moment, we only have permanent states, so they're all valid.
  #t;
end method state-valid?;

// Also be sure to verify that equal-hash and value-hash work as
// advertised. They depend on object-hash (which is always defined,
// but might not behave as Mindy's does) and float-hash (which is
// implemented in Mindy but not standard).

define constant $permanent-hash-state :: <hash-state> = #f;

// We use this for implementing a bit rotate.  If we have the wrong
// number of bits, then our hash functions may be less useful, but
// nothing should break
//
define constant *word-bits* = 32;

define inline method merge-hash-ids
    (id1 :: <integer>, id2 :: <integer>, #key ordered = #f )
 => (hash-id :: <integer>);
  if (ordered)
    // We go through a lot of work here to make sure that the ordered
    // merge produces good results regardless of the argument order.
    // Failure to do this caused significant inefficiencies in earlier
    // versions. 
    logxor(ash(id1, 5), ash(id1, 5 - *word-bits*),
	   ash(id2, -2), ash(id2, *word-bits* - 2));
  else
    logxor(id1, id2);
  end if;
end method merge-hash-ids;

define inline method merge-hash-states
    (state1 :: <false>, state2 :: <false>) => (result :: <hash-state>);
  $permanent-hash-state;
end method merge-hash-states;

// At some point, there will be at least one additional method for
// merge-hash-states to deal with non-permanent states.

define inline method merge-hash-codes
    (id1 :: <integer>, state1 :: <hash-state>,
     id2 :: <integer>, state2 :: <hash-state>,
     #key ordered :: <boolean> = #f)
 => (merged-id :: <integer>, merged-state :: <hash-state>);
  values(merge-hash-ids(id1, id2, ordered: ordered),
	 merge-hash-states(state1, state2));
end method merge-hash-codes;

define inline method pointer-hash (object :: <object>)
 => (id :: <integer>, state :: <hash-state>);
  // Translate the address into an integer, and shift away to bits to account
  // for word alignment.
  //
  // If we get a non-moving garbage collector, we will have to return
  // some non-permanent state.
  //
  // We compute the hash state first and *then* the hash id.  If we do
  // it in the more natural order, there could be a GC right after we
  // compute the hash id, and we'd be returning an outdated hash id
  // without even knowing it.  (Ok, granted, this can't happen with
  // our current non-moving GC, but if we ever get a new one...)
  let state = $permanent-hash-state;
  let id = ash(as(<integer>, %%primitive(object-address, object)), -2);
  values(id, state);
end method pointer-hash;
  
// This function is slow, but should work reasonably.  Eventually, we probably
// want to replace it with a clever bit manipulation instead.
//
define method float-hash (object :: <float>)
 => (id :: <integer>, state :: <hash-state>);
  let (int, fraction) = truncate(object);
  let (int-id, int-state) = object-hash(int);
  let fract-id = as(<integer>, truncate(fraction * $really-big-prime));
  merge-hash-codes(int-id, int-state, fract-id, $permanent-hash-state);
end method float-hash;

// -------------------------------------------------------------------
// Portable implementation
// -------------------------------------------------------------------

// Except for $default-table-size, these numbers are expressed as
// percentages.  200 for $expand-when means when there are two objects
// for every bucket, the hash table will grow to $expand-to % of the
// original size.  (Make sure it expands at least 100%, or you'll be
// resizing constantly) Expands only occur when someone adds something
// to the table.
//
// $shrink-when and $shrink-to are handled similarly.  Shrink conditions
// are checked only when someone removes an element, and expand only
// when someone adds an element.  Be careful not to set shrink-when too
// high, because if you do the table could shrink immediately after it
// expands.
//
define constant $starting-table-size :: <integer> =  13;
define constant $expand-when :: <integer> = 200;
define constant $expand-to :: <integer> = 300;
define constant $shrink-when :: <integer> = 10;
define constant $shrink-to :: <integer> = 100;

define class <bucket-entry> (<object>)
  slot entry-key :: <object>, required-init-keyword: #"key";
  slot entry-elt :: <object>, required-init-keyword: #"item";
  slot entry-hash-id :: <integer>, required-init-keyword: #"hash-id";
  slot entry-hash-state :: <hash-state>, required-init-keyword: #"hash-state";
  slot entry-next :: false-or(<bucket-entry>) = #f, init-keyword: #"next";
end class <bucket-entry>;

define sealed domain make (singleton(<bucket-entry>));
define sealed domain initialize (<bucket-entry>);

#if (mindy)

define constant <entry-vector> = <simple-object-vector>;

#else

define class <entry-vector> (<vector>)
  sealed slot %element :: false-or(<bucket-entry>),
    init-value: #f, init-keyword: fill:,
    sizer: size, size-init-value: 0, size-init-keyword: size:;
end class <entry-vector>;
define sealed domain make(singleton(<entry-vector>));
define sealed domain initialize(<entry-vector>);

#endif

define sealed inline method element
    (vec :: <entry-vector>, index :: <integer>,
     #key default = $not-supplied)
    => element :: <object>; // because of default:
  if (index >= 0 & index < vec.size)
    %element(vec, index);
  elseif (default == $not-supplied)
    element-error(vec, index);
  else
    default;
  end;
end;

define sealed inline method element-setter
    (new-value :: false-or(<bucket-entry>), vec :: <entry-vector>,
     index :: <integer>)
    => new-value :: false-or(<bucket-entry>);
  if (index >= 0 & index < vec.size)
    %element(vec, index) := new-value;
  else
    element-error(vec, index);
  end;
end;

define constant $empty-entry-vector :: <entry-vector>
  = make(<entry-vector>, size: 0);
define constant $empty-simple-object-vector :: <simple-object-vector>
  = make(<simple-object-vector>, size: 0);

define open abstract primary class <table>
    (<mutable-explicit-key-collection>, <stretchy-collection>)
  slot table-size :: <integer> = 0;  // Number of keys
  // Each bucket is a chained sequence of <bucket-entry>s
  // HACK: The init values simply fool the compiler into being sure
  // that the slots are defined -- otherwise it will spend lots of
  // time checking.
  slot buckets :: <entry-vector> = $empty-entry-vector;
  // the merged states of each of the buckets
  slot bucket-states :: <simple-object-vector> /* of <hash-state>s */
    = $empty-simple-object-vector;
  slot table-hash-state :: <hash-state> = $permanent-hash-state;
end class <table>;

// Uses == (aka id?) as key comparison
//
define open abstract class <object-table> (<table>)
end class <object-table>;

define sealed class <simple-object-table> (<object-table>) end class;

define sealed domain make (singleton(<simple-object-table>));
define sealed domain initialize (<simple-object-table>);

// Uses = as key comparison
//
define sealed class <equal-table> (<table>)
end class <equal-table>;

define sealed domain make (singleton(<equal-table>));
define sealed domain initialize (<equal-table>);

// Uses a user defined key comparison and hash function, so long as
// the hash function doesn't involve addresses.
//
define open abstract class <value-table> (<table>)
end class <value-table>;

define sealed method make 
    (c == <table>, #rest key-value-pairs, #all-keys)
 =>  table :: <simple-object-table>;
  apply(make, <simple-object-table>, key-value-pairs);
end method make;

define sealed method make 
    (c == <object-table>, #rest key-value-pairs, #all-keys)
 =>  table :: <simple-object-table>;
  apply(make, <simple-object-table>, key-value-pairs);
end method make;

// Table-init does most of the work of initialize, except we move it out
// of initialize to allow remove-all-keys! to call it.

define method table-init (ht :: <table>, #key size: sz = $starting-table-size)
  let sz = if (sz == 0) 1 else sz end if;
  ht.buckets := make(<entry-vector>, size: sz, fill: #f);
  ht.bucket-states := make(<simple-object-vector>, 
			   size: sz, fill: $permanent-hash-state);
end method table-init;

define method initialize
    (ht :: <table>, #next next-method, #key size: sz = $starting-table-size);
  next-method();
  table-init(ht, size: sz)
end method initialize;

define inline method key-test (ht :: <table>) => test :: <function>;
  values(table-protocol(ht));    // drop the second return value
end method key-test;

define sealed domain key-test (<simple-object-table>);

define inline method object-hash (key :: <object>)
 => (id :: <integer>, state :: <hash-state>);
  pointer-hash(key);
end method object-hash;

// The largest <integer> prime.
//
define constant $really-big-prime = 1073741789;

define method object-hash (key :: <integer>)
 => (id :: <integer>, state :: <hash-state>);
  values(modulo(key, $really-big-prime), $permanent-hash-state);
end;

define method object-hash (key :: <extended-integer>)
 => (id :: <integer>, state :: <hash-state>);
  values(as(<integer>, modulo(key, $really-big-prime)),
	 $permanent-hash-state);
end method object-hash;

define method object-hash (key :: <ratio>)
 => (id :: <integer>, state :: <hash-state>);
  values(logxor(as(<integer>, modulo(key.numerator, $really-big-prime)),
		as(<integer>,
		   modulo(key.denominator, $really-big-prime))),
	 $permanent-hash-state);
end method object-hash;

define inline method object-hash (key :: <float>)
 => (id :: <integer>, state :: <hash-state>);
  float-hash(key);
end method object-hash;

define inline method object-hash (key :: <character>)
 => (id :: <integer>, state :: <hash-state>);
  // We could get away with using pointer-hash for <character>s in Mindy,
  // but we don't because there is a trivial hash function that lets us
  // use $permanent-hash-state.
  values(as(<integer>, key), $permanent-hash-state);
end method object-hash;

// equal-hash is used in the table-protocol as the hash-function 
// for equal tables. Calling convention is similar to object-hash.
//
// The default method for objects that don't have any 
// better methods defined. (We can't call object-hash, so what can we do?)
//
define method equal-hash (key :: <object>) 
 => (id :: <integer>, state :: <hash-state>);
  values(42, $permanent-hash-state);
end method equal-hash;

// Call object-hash for characters, integers, symbols, classes,
// functions, and conditions.
//
define method equal-hash (key :: <character>)
 => (id :: <integer>, state :: <hash-state>);
  object-hash(key);
end method equal-hash;

define method equal-hash (key :: <general-integer>)
 => (id :: <integer>, state :: <hash-state>);
  object-hash(key);
end method equal-hash;

define method equal-hash (key :: <float>)
 => (id :: <integer>, state :: <hash-state>);
  float-hash(key);
end method equal-hash;

define method equal-hash (key :: <symbol>)
 => (id :: <integer>, state :: <hash-state>);
  object-hash(key);
end method equal-hash;

define method equal-hash (key :: <class>)
 => (id :: <integer>, state :: <hash-state>);
  object-hash(key);
end method equal-hash;

define method equal-hash (key :: <function>)
 => (id :: <integer>, state :: <hash-state>);
  object-hash(key);
end method equal-hash;

define method equal-hash (key :: <type>)
 => (id :: <integer>, state :: <hash-state>);
  object-hash(key);
end method equal-hash;

define method equal-hash (key :: singleton (#f))
 => (id :: <integer>, state :: <hash-state>);
  object-hash(key);
end method equal-hash;

define method equal-hash (key :: singleton (#t))
 => (id :: <integer>, state :: <hash-state>);
  object-hash(key);
end method equal-hash;

define method equal-hash (key :: <condition>)
 => (id :: <integer>, state :: <hash-state>);
  object-hash(key);
end method equal-hash;

define method equal-hash (col :: <collection>)
 => (id :: <integer>, state :: <hash-state>);
  collection-hash(equal-hash, equal-hash, col);
end method equal-hash;

// Object-hash returns $permanent-hash-state for <fix-num>s. (Yes,
// ignore the "don't call object-hash" warning at the beginning of
// this file. Trust me, this works in *Mindy*) object-hash in Mindy
// does not return $permanent-hash-state for anything else.
//
define sealed method value-hash (key :: <general-integer>)
 => (id :: <integer>, state :: <hash-state>);
  object-hash(key);
end method value-hash;

define sealed method value-hash (key :: <float>)
 => (id :: <integer>, state :: <hash-state>);
  float-hash(key);
end method value-hash;

define sealed method value-hash (key :: <character>)
 => (id :: <integer>, state :: <hash-state>);
  value-hash(as(<integer>, key));
end method value-hash;

define sealed method value-hash (key :: <symbol>)
 => (id :: <integer>, state :: <hash-state>);
  string-hash(as(<string>, key));
end method value-hash;

define sealed method value-hash (key == #f)
 => (id :: <integer>, state :: <hash-state>);
  values(0, $permanent-hash-state);
end method value-hash;

define sealed method value-hash (key == #t)
 => (id :: <integer>, state :: <hash-state>);
  values(1, $permanent-hash-state);
end method value-hash;

// Another function placed here to prevent a circular library definition.
//
// You can't write a more specific method on collections because 
// any two collections with identical key/element pairs are equal. 
// Because of this, you can't merge-hash-codes with ordered: #t, or
// really anything else interesting. In partial compensation, this
// method hashes the keys as well as the elements. (As long as you
// always put the element before the key when you merge hash codes,
// you *can* use ordered: #t for merging them)
//
define method collection-hash
    (key-hash :: <function>, element-hash :: <function>, col :: <collection>,
     #key ordered :: <boolean> = #f)
 => (id :: <integer>, state :: <hash-state>);
  let (current-id, current-state) = values(0, $permanent-hash-state);
  let (state, limit, next, done?, cur-key, cur-elem)
    = forward-iteration-protocol(col);
  for (st = state then next(col, st), until: done?(col, st, limit))
    let elt = cur-elem(col, st);
    let key = cur-key(col, st);

    let (elt-id, elt-state) = element-hash(elt);
    let (key-id, key-state) = key-hash(key);
    let (captured-id1, captured-state1)
      = merge-hash-codes(elt-id, elt-state, key-id, key-state, ordered: #t);
    let (captured-id2, captured-state2) 
      = merge-hash-codes(current-id, current-state, 
			 captured-id1, captured-state1, ordered: ordered);
    current-id := captured-id2;
    current-state := captured-state2;
  end for;
  values(current-id, current-state);
end method collection-hash;

// This is similar to an equal-hash, except that it hashes things with
// ordered: #t and ignores the sequence keys. USE WITH CAUTION: This
// isn't a proper equal-hash because two collections of different types
// but identical key/element pairs won't generate the same hash id,
// even though the two collections are =.
//
define function sequence-hash
    (element-hash :: <function>, seq :: <sequence>)
 => (id :: <integer>, state :: <hash-state>);
  let (current-id, current-state) = values(0, $permanent-hash-state);
  for (elt in seq)
    let (id, state) = element-hash(elt);
    let (captured-id, captured-state) 
      = merge-hash-codes(current-id, current-state, id, state, ordered: #t);
    current-id := captured-id;
    current-state := captured-state;
  end for;
  values(current-id, current-state);
end function sequence-hash;

define sealed inline method table-protocol (ht :: <object-table>) 
 => (key-test :: <function>, key-hash :: <function>);
  values(\==, object-hash);
end method table-protocol;

define sealed domain table-protocol (<simple-object-table>);

define sealed inline method table-protocol (ht :: <equal-table>) 
 => (key-test :: <function>, key-hash :: <function>);
  values(\=, equal-hash);
end method table-protocol;

define constant not-in-ht2 = pair(#f, #f);

// Informally, two hash tables are = if they use the same key test,
// have the same size, and all the elements in the first hash table
// have matching elements in the second hash table.
//
define method \= (ht1 :: <table>, ht2 :: <table>) => answer :: <boolean>;
  let test1 = ht1.key-test;
  let test2 = ht2.key-test;
  (test1 == test2) 
    & ht1.size == ht2.size
    & block (return)
	let (state, limit, next, done?, cur-key, cur-elem)
	  = forward-iteration-protocol(ht1);
	for (st = state then next(ht1, st), until: done?(ht1, st, limit))
	  let elt1 = cur-elem(ht1, st);
	  let key = cur-key(ht1, st);

	  let elt2 = element(ht2, key, default: not-in-ht2);
	  if (elt2 == not-in-ht2 | ~test1(elt1, elt2))
	    return(#f);
	  end if;
	end for;
	#t;
      end block;
end method \=;

// Looks for an entry in the chain which satisfies the key test.  If
// found, reorganize the chain so that element is at the front, and
// return it.  Otherwise, return #f.  Note that you *must* reassign the
// value of the bucket if this returns a true value.
//
define method find-elt
    (chain :: false-or(<bucket-entry>), key :: <object>, key-id :: <integer>,
     key= :: <function>)
 => (whatever :: false-or(<bucket-entry>));
  case
    (~chain) => #f;
    ((chain.entry-hash-id == key-id) & key=(chain.entry-key, key)) => chain;
    otherwise =>
      block (return)
	for (prev :: <bucket-entry> = chain then elem,
	     elem :: false-or(<bucket-entry>) = chain.entry-next then elem.entry-next,
	     while: elem)
	  if ((elem.entry-hash-id == key-id) & key=(elem.entry-key, key))
	    prev.entry-next := elem.entry-next;
	    elem.entry-next := chain;
	    return(elem);
	  end if;
	end for;
      end block;
  end case;
end method find-elt;

// This function looks redundant at times, but it's necessary in order
// to avoid race conditions with the garbage collector.
//
define method table-element
    (ht :: <table>, key :: <object>, key= :: <function>, key-id :: <integer>,
     key-state :: <hash-state>, default)
 => (result :: <object>);
  // We don't yet check for outdated hash states, since the element
  // might match anyway, and the lookup is much cheaper than a rehash.

  let bucket-index :: <integer> = modulo(key-id, ht.buckets.size);
  let bucket = ht.buckets[bucket-index];
  let find-result = find-elt(bucket, key, key-id, key=);
  
  if (find-result)
    ht.buckets[bucket-index] := find-result;
    find-result.entry-elt;

  // If our element wasn't found, see if we needed to rehash the
  // table.  If so, start over.  Even if you check states in the
  // beginning, you need this test anyway: You'd need to hash the
  // element and check the table's state at the same time.
  elseif (~ht.table-hash-state.state-valid? | ~key-state.state-valid?)
    rehash(ht);
    element(ht, key, default: default);
  elseif (default == $not-supplied)
    error("Element not found");
  else 
    default;
  end if;
end method table-element;

define inline method element
    (ht :: <table>, key :: <object>, #key default: default = $not-supplied )
 => (result :: <object>);
  // We don't yet check for outdated hash states, since the element
  // might match anyway, and the lookup is much cheaper than a rehash.

  let (key=, key-hash) = table-protocol(ht);
  let (key-id :: <integer>, key-state :: <hash-state>) = key-hash(key);
  table-element(ht, key, key=, key-id, key-state, default);
end method element;

// This is essentially the same code without the garbage collection stuff
//
//define inline method element
//    (ht :: <value-table>, key, #key default = $not-supplied)
// => (result :: <object>);
//  let (key=, key-hash) = table-protocol(ht);
//  let key-id :: <integer> = key-hash(key);
//  let bucket-index :: <integer> = modulo(key-id, ht.buckets.size);
//  let bucket = ht.buckets[bucket-index];
//  let find-result = find-elt(bucket, key, key-id, key=);
//  
//  if (find-result)
//    ht.buckets[bucket-index] := find-result;
//    find-result.entry-elt;
//  elseif (default == $not-supplied)
//    error ("Element not found");
//  else 
//    default;
//  end if;
//end method element;

define sealed domain element (<table>, <object>);

// Return a size that's "almost prime", i.e., not divisible by any
// prime less than 11.
//
define method find-new-size (target :: <integer>)
 => (result :: <integer>);
  for (num :: <integer>
	 from if (even?(target)) target + 1 else target end if by 2,
       until: (modulo(num, 3) > 0 & modulo(num, 5) > 0 & modulo(num, 7) > 0
		& modulo(num, 11) > 0))
  finally
    num;
  end for;
end method find-new-size;

define method maybe-expand-table (ht :: <table>) => ();
  if (ht.size * 100 > (ht.buckets.size * $expand-when))
    let target = truncate/(ht.buckets.size * $expand-to, 100);
    resize-table(ht, find-new-size(target));
  end if;
end method maybe-expand-table;

// This function looks redundant at times, but it's necessary in order
// to avoid race conditions with the garbage collector.
//
define method element-setter
    (value :: <object>, ht :: <table>, key :: <object>) 
 => value :: <object>;
  let (key=, key-hash) = table-protocol(ht);
  let (key-id :: <integer>, key-state :: <hash-state>) = key-hash(key);
  let bucket-index :: <integer> = modulo(key-id, ht.buckets.size);
  let bucket-entry = find-elt(ht.buckets [bucket-index], key, key-id, key=);

     // Check to see if there was a garbage collection in the middle
     // of this method. If there was, start over.

  if (bucket-entry)
    // If we found an entry, it doesn't matter whether there was a
    // garbage collection.  Just update it (and the hash state).
    ht.buckets[bucket-index] := bucket-entry;
    bucket-entry.entry-key := key;
    bucket-entry.entry-hash-id := key-id;
    bucket-entry.entry-hash-state := key-state;
    bucket-entry.entry-elt := value;
    // Update bucket's merged-hash-state
    ht.bucket-states[bucket-index]
      := merge-hash-states(key-state, ht.bucket-states[bucket-index]);
    // Update table's merged hash codes
    ht.table-hash-state := merge-hash-states(key-state, ht.table-hash-state);
    value;
  elseif (~ht.table-hash-state.state-valid? | ~key-state.state-valid?)
    // Not found, but that *may* be bacause of the invalid state.  Try again.
    rehash(ht);
    element-setter(value, ht, key);
  else
    // There was no garbage collection (and no match), and we're safe.  (If
    // there is a garbage collection between now and the the end of
    // this method, it invalidates the states we're about to write,
    // but we can just re-compute them on the next lookup)
    ht.buckets[bucket-index]
      := make(<bucket-entry>, key: key, item: value, hash-id: key-id, 
	      hash-state: key-state, next: ht.buckets[bucket-index]);
    ht.table-size := ht.table-size + 1;
    maybe-expand-table(ht);
    // Update bucket's merged-hash-state
    ht.bucket-states[bucket-index]
      := merge-hash-states(key-state, ht.bucket-states[bucket-index]);
    // Update table's merged hash codes
    ht.table-hash-state := merge-hash-states(key-state, ht.table-hash-state);
    value;
  end if;
end method element-setter;

// This is exactly the same code without the garbage collection stuff
//
define method element-setter (value :: <object>, ht :: <value-table>, 
			      key :: <object>) => value :: <object>;
  let (key=, key-hash) = table-protocol(ht);
  let key-id :: <integer> = key-hash(key);
  let bucket-index :: <integer> = modulo(key-id, ht.buckets.size);
  let bucket-entry = find-elt(ht.buckets [bucket-index], key, key-id, key=);

  if (bucket-entry)
    // Item WAS found
    ht.buckets[bucket-index] := bucket-entry;
    bucket-entry.entry-key := key;
    bucket-entry.entry-hash-id := key-id;
    bucket-entry.entry-elt := value;
  else
    // If item didn't exist, add it
    bucket-entry
      := make(<bucket-entry>, key: key, item: value, hash-id: key-id, 
	      hash-state: $permanent-hash-state,
	      next: ht.buckets[bucket-index]);
    ht.buckets[bucket-index] := bucket-entry;
    ht.table-size := ht.table-size + 1;

    maybe-expand-table(ht);
  end if;
  value;
end method element-setter;

define method maybe-shrink-table (ht :: <table>) => ();
  if (ht.size * 100 < (ht.buckets.size * $shrink-when))
    let target = truncate/(ht.buckets.size * $shrink-to, 100);
    resize-table(ht, find-new-size(target));
  end if;
end method maybe-shrink-table;

define method remove-key! (ht :: <table>, key) => (found :: <boolean>);
  while (~ht.table-hash-state.state-valid?)
    rehash(ht);
  end while;
  let (key=, key-hash) = table-protocol(ht);
  let (key-id :: <integer>, key-state :: <hash-state>) = key-hash(key);
  let bucket-index :: <integer> = modulo (key-id, ht.buckets.size);
  let bucket = ht.buckets[bucket-index];
  let the-item = find-elt(bucket, key, key-id, key=);

  if (the-item)
    // An item with that key was found.  It doesn't matter if the
    // hash codes are out of date -- this is what we wanted regardless.
    ht.table-size := ht.table-size - 1;
    // Find-elt reorganized the chain so that out desired element is at
    // the front.  Therefore, we simply slip it off, and we've
    // effectively removed the element.
    ht.buckets[bucket-index] := the-item.entry-next;
    maybe-shrink-table(ht);

    // We leave all the merged-states as is. rehash will take care of it
    // if a remove-key! made the merged-state information overly cautious.

    #t;
  elseif (~ht.table-hash-state.state-valid? | ~key-state.state-valid?)
    // If state not valid, goto beginning for a rehash
    remove-key!(ht, key);
  end if;
end method remove-key!;

// This is exactly the same code without the garbage collection stuff
//
define method remove-key! (ht :: <value-table>, key) => (found? :: <boolean>);
  let (key=, key-hash) = table-protocol(ht);
  let key-id :: <integer> = key-hash(key);
  let bucket-index :: <integer> = modulo(key-id, ht.buckets.size);
  let bucket = ht.buckets[bucket-index];

  let the-item = find-elt(bucket, key, key-id, key=);

  if (the-item)       // An item with that key was found
    ht.table-size := ht.table-size - 1;
    ht.buckets[bucket-index] := the-item.entry-next;

    maybe-shrink-table(ht);
    #t;
  end if; // had to remove something
end method remove-key!;

// Modifies collection so that the collection no longer contains any keys
// or elements (i.e. is empty)
//
define open generic remove-all-keys!
    (coll :: <mutable-explicit-key-collection>)
 => (coll :: <mutable-explicit-key-collection>);

// Method on <table> which doesn't use remove-key!
//
define method remove-all-keys! (table :: <table>)
 => (table :: <table>);
  table-init(table);
  table.table-size := 0;
  table.table-hash-state := $permanent-hash-state;
  table;
end method remove-all-keys!;

// Takes a hashtable and mutates it so that it has a different number of
// buckets.
//
define method resize-table (ht :: <table>, numbuckets :: <integer>);
  let new-array = make(<entry-vector>, size: numbuckets, fill: #f);
  let new-state-array = make(<simple-object-vector>,
			     size: numbuckets, fill: $permanent-hash-state);
  local method iterate (entry :: false-or(<bucket-entry>)) => ();
	  if (entry)
	    // eagerly grab this, since	it will be changing
	    let next-entry = entry.entry-next;
	    let index :: <integer> = modulo(entry.entry-hash-id, numbuckets);
	    entry.entry-next := new-array[index];
	    new-array[index] := entry;
	    new-state-array[index]
	      := merge-hash-states(new-state-array[index],
				   entry.entry-hash-state);
	    iterate(next-entry);
	  end if;
	end method iterate;
  for (i :: <integer> from 0 below ht.buckets.size)
    iterate(ht.buckets[i]);
  end for;
  ht.buckets := new-array;
  ht.bucket-states := new-state-array;
end method resize-table;

// This version of resize-table doesn't bother updating any of the
// merged state slots, arrays, etc.
//
define method resize-table (ht :: <value-table>, numbuckets :: <integer>)
  let new-array = make(<entry-vector>, 
		       size: numbuckets,
		       fill: #f);

  local method iterate (entry :: false-or(<bucket-entry>)) => ();
	  if (entry)
	    // eagerly grab this, since	it will be changing
	    let next-entry = entry.entry-next;
	    let index :: <integer> = modulo(entry.entry-hash-id, numbuckets);
	    entry.entry-next := new-array[index];
	    new-array[index] := entry;
	    iterate(next-entry);
	  end if;
	end method iterate;
  for (i :: <integer> from 0 below ht.buckets.size)
    iterate(ht.buckets[i]);
  end for;

  ht.buckets := new-array;
end method resize-table;


// Rehash does its best to bring a table up to date so that all the
// hash-id's in the table are valid. Rehash makes no guarantees about
// its success, however, so one should call it inside an until loop
// to make sure it keeps trying until it succeeds.
//
// Rehash wants to get the merged-hash-states to be as accurate as
// possible without sacraficing too much performance. This might be a
// good function to tune.
//
define method rehash (ht :: <table>) => rehashed-ht :: <table>;
  let (key=, key-hash) = table-protocol(ht);
  let deferred-elements :: false-or(<bucket-entry>) = #f;

  for (i :: <integer> from 0 below ht.buckets.size)
    if (~ht.bucket-states[i].state-valid?)     // rehash bucket
      ht.bucket-states[i] := $permanent-hash-state;
      let bucket = ht.buckets[i];
      ht.buckets[i] := #f;
      
      local method iterate (bucket-entry :: false-or(<bucket-entry>)) => ();
	      if (bucket-entry)
		let next-entry = bucket-entry.entry-next;
		if (bucket-entry.entry-hash-state.state-valid?)
		  // Put it back into the same bucket
		  bucket-entry.entry-next := ht.buckets[i];
		  ht.buckets[i] := bucket-entry;
		  ht.bucket-states[i] :=
		    merge-hash-states(bucket-entry.entry-hash-state,
				      ht.bucket-states[i]);
		else  // state is invalid
		  let (id, state) = key-hash(bucket-entry.entry-key);  
		  bucket-entry.entry-hash-id := id;
		  bucket-entry.entry-hash-state := state;
		  let index = modulo(id, ht.buckets.size);
		  if (index <= i)
		    // Put it back into a previously processed bucket
		    bucket-entry.entry-next := ht.buckets[index];
		    ht.buckets[index] := bucket-entry;
		    ht.bucket-states[index] := 
		      merge-hash-states(state, ht.bucket-states[index]);
		  else
		    // Don't install these yet, or we'll just have to
		    // look at them again.
		    bucket-entry.entry-next := deferred-elements;
		    deferred-elements := bucket-entry;
		  end if;    // If index <= i
		end if;      // If state-valid? (bucket-entry)
		iterate(next-entry);
	      end if;
	    end method iterate;
      iterate(bucket);
    end if;
  end for;

  // Now we can process everything we put off before.
  for (deferred :: false-or(<bucket-entry>) = deferred-elements then remaining,
       remaining = if (deferred-elements) deferred-elements.entry-next end if
	 then if (remaining) remaining.entry-next end if,
       while: deferred)
    let id = deferred.entry-hash-id;
    let state = deferred.entry-hash-state;
    let index = modulo(id, ht.buckets.size);

    // Put it back into a previously processed bucket
    deferred.entry-next := ht.buckets[index];
    ht.buckets[index] := deferred;
    ht.bucket-states[index] := merge-hash-states(state, 
						 ht.bucket-states[index]);
  end for;    // Finished traversing the deferred elements
  ht.table-hash-state := reduce(merge-hash-states,
				$permanent-hash-state,
				ht.bucket-states);
  ht;
end method rehash;

define method size (ht :: <table>) => (result :: <integer>);
  ht.table-size;
end method size;

define method empty? (ht :: <table>) => (result :: <boolean>);
  ht.table-size = 0;
end method empty?;

define class <table-iterator> (<object>)
  slot current :: <integer> = 0, init-keyword: #"current";
  slot entries :: <entry-vector>, required-init-keyword: #"entries";
end class <table-iterator>;
define sealed domain make(singleton(<table-iterator>));
define sealed domain initialize(<table-iterator>);

define method forward-iteration-protocol (ht :: <table>)
 => (initial-state :: <table-iterator>,
     limit :: <integer>,
     next-state :: <function>,
     finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>,
     current-element-setter :: <function>,
     copy-state :: <function>);
  let sz :: <integer> = ht.size;
  let vec = make(<entry-vector>, size: sz);
  let i :: <integer> = 0;
  for (from-index :: <integer> from 0 below ht.buckets.size)
    for (entry :: false-or(<bucket-entry>) = ht.buckets[from-index]
	   then entry.entry-next,
	 while: entry)
      unless (i < sz)
	error("Size botch in forward-iteration-protocol(<table>)");
      end unless;
      %element(vec, i) := entry;
      i := i + 1;
    end for;
  end for;
  unless (i == sz)
    error("Size botch in forward-iteration-protocol(<table>)");
  end unless;

  values (make(<table-iterator>, entries: vec), sz,
	  /* next-table-state */
	  method (ht :: <table>, state :: <table-iterator>) 
	   => (new-state :: <table-iterator>);
	    state.current := state.current + 1;
	    state;
	  end method,
	  /* finished-table-state? */
	  method (ht :: <table>, state :: <table-iterator>, limit :: <integer>)
	    state.current >= limit;
	  end method,
	  /* current-table-key */
	  method (ht :: <table>, state :: <table-iterator>) => key :: <object>;
	    if (state.current >= state.entries.size)
	      error("Botched iteration!")
	    end if;
	    %element(state.entries, state.current).entry-key;
	  end method,
	  /* current-table-element */
	  method (ht :: <table>, state :: <table-iterator>) => elt :: <object>;
	    if (state.current >= state.entries.size)
	      error("Botched iteration!")
	    end if;
	    %element(state.entries, state.current).entry-elt;
	  end method,
	  /* current-table-element-setter */
	  method (value :: <object>, ht :: <table>, state :: <table-iterator>)
	   => value :: <object>;
	    if (state.current >= state.entries.size)
	      error("Botched iteration!")
	    end if;
	    %element(state.entries, state.current).entry-elt := value;
	    value;
	  end method,
	  /* copy-table-state*/
	  method (ht :: <table>, old-state :: <table-iterator>)
	   => new-state :: <table-iterator>;
	    make(<table-iterator>, current: old-state.current,
		 entries: old-state.entries);
	  end method);
end method forward-iteration-protocol;

// A convenient method for hashing strings. Calls sequence-hash 
// and "does the right thing."
//
define method string-hash (s :: <string>)
    => (id :: <integer>, state :: <hash-state>);
  sequence-hash(value-hash, s);
end method string-hash;

// This string-hash method should have the same semantics as the standard
// one, but should be much faster.
//
define method string-hash (s :: <byte-string>)
 => (id :: <integer>, state :: <hash-state>);
  for (id = 0 then merge-hash-codes(id, $permanent-hash-state,
				    as(<integer>, s[i]),
				    $permanent-hash-state,
				    ordered: #t),
       i from 0 below s.size)
  finally
    values(id, $permanent-hash-state);
  end for;
end method string-hash;

// Moved from string-extensions to prevent a circular library definition.
//
define method uppercase? (c :: <character>) => answer :: <boolean>;
  c >= 'A' & c <= 'Z';
end method uppercase?;
