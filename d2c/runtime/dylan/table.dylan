module:	    dylan-viscera
Author:	    Nick Kramer (nkramer@cs.cmu.edu)
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/table.dylan,v 1.6 1996/02/19 20:23:04 rgs Exp $
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
//
// <object-table>s are as defined in the book, operating on pointers and
// using == as a comparator.
//
// <equal-table>s use = as a key test, but since = uses == as a
// default method, <equal-table>s also have to worry about garbage
// collection.
//
// <value-table>s are an abstract class who's hash function never
// involves addresses (ie, always returns $permanent-hash-state).  The
// user defines a subclass of <value-table> and writes a method for
// table-protocol.  This will probably involve writing a new hash
// function to be used on the hash keys.  *Make sure this function does
// not call object-hash*.
//
// For a more in depth explanation, see mindy.doc


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
  // If we get a non-conservative garbage collector, we will have to return
  // some non-permanent state.
  values(ash(as(<integer>, %%primitive object-address(object)), -2),
	 $permanent-hash-state);
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
end class <bucket-entry>;

seal generic make (singleton(<bucket-entry>));
seal generic initialize (<bucket-entry>);

define open abstract primary class <table>
    (<mutable-explicit-key-collection>, <stretchy-collection>)
  slot table-size :: <integer>, init-value: 0;  // Number of keys
  slot buckets :: <simple-object-vector>;  // vector of <bucket-entry>s
  slot bucket-states :: <simple-object-vector>;  // the merged states of each
                                                 // of the buckets
  slot table-hash-state :: <object>, init-value: $permanent-hash-state;
end class <table>;

// Uses == (aka id?) as key comparison
//
define open abstract class <object-table> (<table>)
end class <object-table>;

define sealed class <simple-object-table> (<object-table>) end class;

seal generic make (singleton(<simple-object-table>));
seal generic initialize (<simple-object-table>);

// Uses = as key comparison
//
define sealed class <equal-table> (<table>)
end class <equal-table>;

seal generic make (singleton(<equal-table>));
seal generic initialize (<equal-table>);

// Uses a user defined key comparison and hash function, so long as
// the hash function doesn't involve addresses.
//
define open abstract class <value-table> (<table>)
end class <value-table>;

define sealed method make 
    (c == <table>, #rest key-value-pairs, #all-keys)
 =>  table :: <object-table>;
  apply(make, <simple-object-table>, key-value-pairs);
end method make;

define sealed method make 
    (c == <object-table>, #rest key-value-pairs, #all-keys)
 =>  table :: <object-table>;
  apply(make, <simple-object-table>, key-value-pairs);
end method make;

define method initialize
    (ht :: <table>, #next next-method, #key size: sz = $starting-table-size);
  next-method();
  let sz = if (sz == 0) 1 else sz end if;
  ht.buckets := make(<simple-object-vector>, size: sz, fill: #() );
  ht.bucket-states := make(<simple-object-vector>, 
			   size: sz, fill: $permanent-hash-state);
end method initialize;

define inline method key-test (ht :: <table>) => test :: <function>;
  values(table-protocol(ht));    // drop the second return value
end method key-test;

seal generic key-test (<simple-object-table>);

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
  collection-hash(col, equal-hash, equal-hash);
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

// You can't write a more specific method on collections because 
// any two collections with identical key/element pairs are equal. 
// Because of this, you can't merge-hash-codes with ordered: #t, or
// really anything else interesting. In partial compensation, this
// method hashes the keys as well as the elements. (As long as you
// always put the element before the key when you merge hash codes,
// you *can* use ordered: #t for merging them)
//
define method collection-hash
    (col :: <collection>, key-hash :: <function>, element-hash :: <function>)
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
			 captured-id1, captured-state1, ordered: #f);
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
define method sequence-hash
    (seq :: <sequence>, element-hash :: <function>)
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
end method sequence-hash;

// A convenient method for hashing strings. Calls sequence-hash 
// and "does the right thing."
//
define method string-hash (s :: <string>)
    => (id :: <integer>, state :: <hash-state>);
  sequence-hash(s, value-hash);
end method string-hash;

// This string-hash method should have the same semantics as the standard
// one, but should be much faster.
//
define method string-hash (s :: <byte-string>)
 => (id :: <integer>, state :: <object>);
  for (id = 0 then merge-hash-codes(id, $permanent-hash-state,
				    as(<integer>, s[i]),
				    $permanent-hash-state,
				    ordered: #t),
       i from 0 below s.size)
  finally
    values(id, $permanent-hash-state);
  end for;
end method string-hash;

define method table-protocol (ht :: <object-table>) 
 => (key-test :: <function>, key-hash :: <function>);
  values(\==, object-hash);
end method table-protocol;

seal generic table-protocol (<simple-object-table>);

define sealed method table-protocol (ht :: <equal-table>) 
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

// Returns the first element of the list that satisfies
// test.  Internal use only.
//
define method find-elt
    (list :: <list>, key :: <object>, key-id :: <integer>,
     key= :: <function>)
 => (whatever :: <object>);
  if (list.empty?)
    #f;
  else
    let elem = list.head;
    if ((elem.entry-hash-id == key-id) & key=(elem.entry-key, key))
      elem;
    else
      find-elt(list.tail, key, key-id, key=);
    end if;
  end if;
end method find-elt;

// This function looks redundant at times, but it's necessary in order
// to avoid race conditions with the garbage collector.
//
define method element
    (ht :: <table>, key :: <object>, #key default: default = $not-supplied )
 => (result :: <object>);
  // We don't yet check for outdated hash states, since the element
  // might match anyway, and the lookup is much cheaper than a rehash.

  let (key=, key-hash) = table-protocol(ht);
  let (key-id :: <integer>, key-state :: <hash-state>) = key-hash(key);
  let bucket-index :: <integer> = modulo(key-id, ht.buckets.size);
  let bucket = ht.buckets[bucket-index];
  let find-result = find-elt(bucket, key, key-id, key=);
  
  if (find-result)
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
end method element;

// This is exactly the same code without the garbage collection stuff
//
define method element
    (ht :: <value-table>, key, #key default = $not-supplied)
 => (result :: <object>);
  let (key=, key-hash) = table-protocol(ht);
  let key-id :: <integer> = key-hash(key);
  let bucket-index :: <integer> = modulo(key-id, ht.buckets.size);
  let bucket = ht.buckets[bucket-index];
  let find-result = find-elt(bucket, key, key-id, key=);
  
  if (find-result)
    find-result.entry-elt;
  elseif (default == $not-supplied)
    error ("Element not found");
  else 
    default;
  end if;
end method element;

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

  if (~ht.table-hash-state.state-valid? | ~key-state.state-valid?)
    rehash(ht);
    element-setter(value, ht, key);
    
    // Else, there was no garbage collection, and we're safe.  (If
    // there is a garbage collection between now and the the end of
    // this method, it invalidates the states we're about to write,
    // but we can just re-compute them on the next lookup)
  else
    if (bucket-entry == #f)             // If item didn't exist, add it
      bucket-entry := make(<bucket-entry>, key: key, hash-id: key-id,
			   hash-state: key-state, item: value);
      ht.buckets[bucket-index] := pair(bucket-entry, 
				       ht.buckets[bucket-index]);
      ht.table-size := ht.table-size + 1;
      maybe-expand-table(ht);
    else     // Item WAS found
      bucket-entry.entry-key := key;
      bucket-entry.entry-hash-id := key-id;
      bucket-entry.entry-hash-state := key-state;
      bucket-entry.entry-elt := value;
    end if;

    // Update bucket's merged-hash-state
    ht.bucket-states[bucket-index]
      := merge-hash-states(bucket-entry.entry-hash-state, 
			   ht.bucket-states[bucket-index]);

    // Update table's merged hash codes
    ht.table-hash-state := 
      merge-hash-states(bucket-entry.entry-hash-state, 
			ht.table-hash-state);
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
  let bucket-entry = find-elt(ht.buckets [bucket-index],
				     key, key-id, key=);

  if (bucket-entry == #f)             // If item didn't exist, add it
    bucket-entry := make(<bucket-entry>, key: key, hash-id: key-id, 
			 hash-state: $permanent-hash-state, item: value);
    
    ht.buckets[bucket-index] := 
           pair(bucket-entry, ht.buckets[bucket-index]);
    ht.table-size := ht.table-size + 1;

    maybe-expand-table(ht);
  else     // Item WAS found
    bucket-entry.entry-key        := key;
    bucket-entry.entry-hash-id    := key-id;
    bucket-entry.entry-elt       := value;
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

  if (~ht.table-hash-state.state-valid? | ~key-state.state-valid?)
    remove-key!(ht, key); // If state not valid, goto beginning for a rehash
  else
    if (the-item ~== #f)       // An item with that key was found
      ht.table-size := ht.table-size - 1;

      // Between find-elt and remove!, this traverses the bucket
      // twice. It could be improved, but one has to be careful to
      // avoid race conditions with the garbage collector.

      ht.buckets[bucket-index] := remove!(bucket, the-item);
      maybe-shrink-table(ht);

      // We leave all the merged-states as is. rehash will take care of it
      // if a remove-key! made the merged-state information overly cautious.

      #t;
    end if; // had to remove something
  end if;   // states valid?
end method remove-key!;

// This is exactly the same code without the garbage collection stuff
//
define method remove-key! (ht :: <value-table>, key) => (found? :: <boolean>);
  let (key=, key-hash) = table-protocol(ht);
  let key-id :: <integer> = key-hash(key);
  let bucket-index :: <integer> = modulo(key-id, ht.buckets.size);
  let bucket = ht.buckets[bucket-index];

  let the-item = find-elt(bucket, key, key-id, key=);

  if (the-item ~== #f)       // An item with that key was found
    ht.table-size := ht.table-size - 1;

           // Between find-elt and remove!, this traverses the bucket
           // twice. It could be improved.

    ht.buckets[bucket-index] := remove!(bucket, the-item);

    maybe-shrink-table(ht);
    #t;
  end if; // had to remove something
end method remove-key!;

// Takes a hashtable and mutates it so that it has a different number of
// buckets.
//
define method resize-table (ht :: <table>, numbuckets :: <integer>);
  let new-array = make(<simple-object-vector>, size: numbuckets, fill: #());
  let new-state-array = make(<simple-object-vector>,
			     size: numbuckets, fill: $permanent-hash-state);
  for (bucket in ht.buckets)
    for (entry in bucket)
      let index = modulo(entry.entry-hash-id, numbuckets);
      new-array[index] := pair(entry, new-array[index]);
      new-state-array[index] := merge-hash-states(new-state-array[index],
						   entry.entry-hash-state);
    end for;
  end for;
  ht.buckets := new-array;
  ht.bucket-states := new-state-array;
end method resize-table;

// This version of resize-table doesn't bother updating any of the
// merged state slots, arrays, etc.
//
define method resize-table (ht :: <value-table>, numbuckets :: <integer>)
  let new-array = make(<simple-object-vector>, 
		       size: numbuckets,
		       fill: #()   );

  for (bucket in ht.buckets)
    for (entry in bucket)
      let index = modulo(entry.entry-hash-id, numbuckets);
      new-array[index] := pair(entry, new-array[index]);
    end for;
  end for;

  ht.buckets := new-array;
end method resize-table;


// Rehash does its best to bring a table up to date so that all the
// hash-id's in the table are valid. Rehash makes no guarentees about
// its success, however, so one should call it inside an until loop
// to make sure it keeps trying until it succeeds.
//
// Rehash wants to get the merged-hash-states to be as accurate as
// possible without sacraficing too much performance. This might be a
// good function to tune.
//
define method rehash (ht :: <table>) => rehashed-ht :: <table>;
  let (key=, key-hash) = table-protocol(ht);
  let deferred-elements = #();

  for (i :: <integer> from 0 below ht.buckets.size)
    if (~ht.bucket-states[i].state-valid?)     // rehash bucket
      ht.bucket-states[i] := $permanent-hash-state;
      let bucket = ht.buckets[i];
      ht.buckets[i] := #();
      
      for (remaining = bucket then next,
	   next = bucket.tail then next.tail, // depends on #().tail == #()
	   until: remaining == #())
	let bucket-entry = remaining.head;
	if (bucket-entry.entry-hash-state.state-valid?)
	  // Put it back into the same bucket
	  remaining.tail := ht.buckets[i];
	  ht.buckets[i] := remaining;
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
	    remaining.tail := ht.buckets[index];
	    ht.buckets[index] := remaining;
	    ht.bucket-states[index] := 
	      merge-hash-states(state, ht.bucket-states[index]);
	  else
	    // Don't install these yet, or we'll just have to look at them
	    // again.
	    remaining.tail := deferred-elements;
	    deferred-elements := remaining;
	  end if;    // If index <= i
	end if;      // If state-valid? (bucket-entry)
      end for;    // Finished traversing the bucket
    end if;         // state-valid? (bucket-id-slots)
  end for;

  // Now we can process everything we put off before.
  for (remaining = deferred-elements then next,
       next = deferred-elements.tail then next.tail, //  #().tail == #()
       until: remaining == #())
    let bucket-entry = remaining.head;
    let id = bucket-entry.entry-hash-id;
    let state = bucket-entry.entry-hash-state;
    let index = modulo(id, ht.buckets.size);

    // Put it back into a previously processed bucket
    remaining.tail := ht.buckets[index];
    ht.buckets[index] := remaining;
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

// -------------------------------------------------------------------
//                Iteration protocol stuff
// -------------------------------------------------------------------

// All these things are needed in the state, because many of the functions
// get nothing but a hash table and a state.

// This is the iteration state, not a hash-state
//
define class <table-state> (<object>)
  slot elements-touched-slot :: <integer>,
    required-init-keyword: #"elements-touched";
  slot bucket-index :: <integer>, required-init-keyword: #"index";
  slot bucket-cell :: <list>, required-init-keyword: #"cell";
end class <table-state>;

seal generic make (singleton(<table-state>));

// Limit isn't used by this iteration protocol
//
define constant <limit> = singleton(#f);  

define constant finished-table-state?
  = method (ht :: <table>, state :: <table-state>, limit :: <limit>)
      state.elements-touched-slot >= ht.table-size;
    end method;

define constant next-table-state
  = method (ht :: <table>, state :: <table-state>) 
     => new-state :: <table-state>;
      state.elements-touched-slot := state.elements-touched-slot + 1;
      if (~finished-table-state?(ht, state, #f))
	let new-cell = state.bucket-cell.tail;
	if (new-cell == #())
	  for (i from state.bucket-index + 1,
	       while: ht.buckets[i] == #())
	  finally
	    state.bucket-index := i;
	    state.bucket-cell := ht.buckets[i];
	  end for;
	else
	  state.bucket-cell := new-cell;
	end if;
      end if;             // End of more objects left in hash table?
      state;            // Return the new and improved state object
    end method;

define method get-bucket-entry (ht :: <table>, state :: <table-state>)
 => entry :: <bucket-entry>;
  state.bucket-cell.head;
end method get-bucket-entry;

define constant current-table-key
  = method (ht :: <table>, state :: <table-state>) => key :: <object>;
      let bucket-entry = get-bucket-entry(ht, state);
      bucket-entry.entry-key;
    end method;

define constant current-table-element
  = method (ht :: <table>, state :: <table-state>) => elt :: <object>;
      let bucket-entry = get-bucket-entry(ht, state);
      bucket-entry.entry-elt;
    end method;

define constant current-table-element-setter
  = method (value :: <object>, ht :: <table>, state :: <table-state>)
     => value :: <object>;
      let new-bucket-entry = get-bucket-entry(ht, state);
      new-bucket-entry.entry-elt := value;
      value;
    end method;

define constant copy-table-state
  = method (ht :: <table>, old-state :: <table-state>)
     => new-state :: <table-state>;
      make(<table-state>, elements-touched: old-state.elements-touched-slot,
	   index: old-state.bucket-index, cell: old-state.bucket-cell);
    end method;

// We use this rather than the more object make(<table-state>) when
// creating a brand new <table-state> because we need next-table-state
// to locate the first non-empty bucket.
//
define method make-table-state (ht :: <table>) 
 => table-state :: <table-state>;
  let result = make(<table-state>, elements-touched: -1,
		    index: -1, cell: #()); // Depend on tail(#()) == #()
  next-table-state(ht, result);
end method make-table-state;

define method forward-iteration-protocol (ht :: <table>)
 => (initial-state :: <object>,
     limit :: <object>,
     next-state :: <function>,
     finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>,
     current-element-setter :: <function>,
     copy-state :: <function>);
  values (make-table-state(ht),       // initial hash state
	  #f,             // limit -- isn't actually used by finished-state?
	  next-table-state,
	  finished-table-state?,
	  current-table-key,
	  current-table-element,
	  current-table-element-setter,
	  copy-table-state);
end method forward-iteration-protocol;

// A value table whose keys are strings.
//
define class <string-table> (<value-table>)
end class <string-table>;

define sealed inline method table-protocol (ht :: <string-table>)
 => (key-test :: <function>, key-hash :: <function>);
  values(\=, string-hash);
end method table-protocol;
