module:	    dylan-viscera
Author:	    Nick Kramer (nkramer@cs.cmu.edu)
rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/table.dylan,v 1.6 2001/07/07 08:12:36 bruce Exp $
Synopsis:   Implements <table>, <object-table>, <equal-table>, 
            and <value-table>.

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000, 2001  Gwydion Dylan Maintainers
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

// This code is a more or less implementation independent.  Almost all
// of the code that is implementation dependent is in the beginning of
// the file; the rest can be found with a search for "mindy".
//
// Author's note: "ht" is my abbreviation for "hashtable", and is used
// as a parameter quite frequently.


// Exported interface.

define open generic table-protocol (table :: <table>)
    => (key-test :: <function>, key-hash :: <function>);

define open generic value-hash (thing :: <object>, state :: <hash-state>)
    => (id :: <integer>, state :: <hash-state>);

define open generic equal-hash (thing :: <object>, state :: <hash-state>)
    => (id :: <integer>, state :: <hash-state>);



// -------------------------------------------------------------------
// Compiler-specific code
// -------------------------------------------------------------------

// Later, this will be "false-or(<some-type>)", when we actually have a
// copying garbage collector.  At the moment, $permanent-hash-state
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

define inline method pointer-hash (object :: <object>)
 => (id :: <integer>, state :: <hash-state>);
  // Translate the address into an integer, and shift away two bits to account
  // for word alignment.
  //
  // If we get a copying garbage collector, we will have to return
  // some non-permanent state.

  let id = ash(as(<integer>, %%primitive(object-address, object)), -2);
  values(id, $permanent-hash-state);
end method pointer-hash;
  
// This function is slow, but should work reasonably.  Eventually, we probably
// want to replace it with a clever bit manipulation instead.
//
define method float-hash (object :: <float>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  let (int, fraction) = truncate(object);
  let (int-id, int-state) = object-hash(int, initial-state);
  let fract-id = as(<integer>, truncate(fraction * $really-big-prime));
  let id = merge-hash-ids(int-id, fract-id);
  values(id, int-state);
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

define sealed inline method make 
    (c == <table>, #rest key-value-pairs, #key, #all-keys)
 =>  table :: <simple-object-table>;
  apply(make, <simple-object-table>, key-value-pairs);
end method make;

define sealed inline method make 
    (c == <object-table>, #rest key-value-pairs, #key, #all-keys)
 =>  table :: <simple-object-table>;
  apply(make, <simple-object-table>, key-value-pairs);
end method make;

// Table-init does most of the work of initialize, except we move it out
// of initialize to allow remove-all-keys! to call it.

define method table-init (ht :: <table>, #key size: sz = $starting-table-size)
  let sz = if (sz == 0) 1 else sz end if;
  ht.buckets := make(<entry-vector>, size: sz, fill: #f);
end method table-init;

define method initialize
    (ht :: <table>, #next next-method, #key size: sz = $starting-table-size);
  next-method();
  table-init(ht, size: sz)
end method initialize;

define inline method key-test (ht :: <table>) => test :: <function>;
  values(table-protocol(ht));    // drop the second return value
end method key-test;

define sealed generic object-hash (key :: <object>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);

define inline method object-hash (key :: <object>,
				  initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  let (id, state) = pointer-hash(key);
  values(id, merge-hash-states(initial-state, state));
end method object-hash;

// The largest <integer> prime.
//
define constant $really-big-prime = 1073741789;

define inline method object-hash (key :: <integer>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  values(key, initial-state);
end;

define method object-hash
    (key :: <extended-integer>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  values(as(<integer>, modulo(key, $really-big-prime)), initial-state);
end method object-hash;

define method object-hash (key :: <ratio>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  values(logxor(as(<integer>, modulo(key.numerator, $really-big-prime)),
		as(<integer>,
		   modulo(key.denominator, $really-big-prime))),
	 initial-state);
end method object-hash;

define inline method object-hash
    (key :: <float>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  float-hash(key, initial-state);
end method object-hash;

define inline method object-hash
    (key :: <character>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  // We could get away with using pointer-hash for <character>s in Mindy,
  // but we don't because there is a trivial hash function that lets us
  // preserve the hash state.
  values(as(<integer>, key), initial-state);
end method object-hash;

// equal-hash is used in the table-protocol as the hash-function 
// for equal tables. Calling convention is similar to object-hash.
//
// The default method for objects that don't have any 
// better methods defined. (We can't call object-hash, so what can we do?)
//
define inline method equal-hash (key :: <object>, initial-state :: <hash-state>) 
 => (id :: <integer>, state :: <hash-state>);
  values(42, initial-state);
end method equal-hash;

// Call object-hash for characters, integers, symbols, classes,
// functions, and conditions.
//
define inline method equal-hash (key :: <character>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  object-hash(key, initial-state);
end method equal-hash;

define inline method equal-hash
    (key :: <general-integer>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  object-hash(key, initial-state);
end method equal-hash;

define inline method equal-hash (key :: <float>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  float-hash(key, initial-state);
end method equal-hash;

define inline method equal-hash (key :: <symbol>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  object-hash(key, initial-state);
end method equal-hash;

define inline method equal-hash (key :: <class>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  object-hash(key, initial-state);
end method equal-hash;

define inline method equal-hash (key :: <function>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  object-hash(key, initial-state);
end method equal-hash;

define inline method equal-hash (key :: <type>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  object-hash(key, initial-state);
end method equal-hash;

define inline method equal-hash (key :: singleton (#f), initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  object-hash(key, initial-state);
end method equal-hash;

define inline method equal-hash (key :: singleton (#t), initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  object-hash(key, initial-state);
end method equal-hash;

define inline method equal-hash (key :: <condition>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  object-hash(key, initial-state);
end method equal-hash;

define inline method equal-hash (col :: <collection>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  collection-hash(equal-hash, equal-hash, col, initial-state);
end method equal-hash;

// Object-hash returns the initial hash-state for <fix-num>s. (Yes,
// ignore the "don't call object-hash" warning at the beginning of
// this file. Trust me, this works in *Mindy*) object-hash in Mindy
// does not return $permanent-hash-state for anything else.
//
define inline sealed method value-hash
    (key :: <general-integer>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  object-hash(key, initial-state);
end method value-hash;

define inline sealed method value-hash (key :: <float>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  float-hash(key, initial-state);
end method value-hash;

define inline sealed method value-hash
    (key :: <character>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  value-hash(as(<integer>, key), initial-state);
end method value-hash;

define inline sealed method value-hash
    (key :: <symbol>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  string-hash(as(<string>, key), initial-state);
end method value-hash;

define inline sealed method value-hash (key == #f, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  values(0, initial-state);
end method value-hash;

define inline sealed method value-hash (key == #t, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  values(1, initial-state);
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
     initial-state :: <hash-state>, #key ordered :: <boolean> = #f)
 => (id :: <integer>, state :: <hash-state>);
  let (current-id, current-state) = values(0, initial-state);
  let (state, limit, next, done?, cur-key, cur-elem)
    = forward-iteration-protocol(col);
  for (st = state then next(col, st), until: done?(col, st, limit))
    let elt = cur-elem(col, st);
    let key = cur-key(col, st);

    let (elt-id, elt-state) = element-hash(elt, current-state);
    let (key-id, key-state) = key-hash(key, elt-state);
    let captured-id1
      = merge-hash-ids(elt-id, key-id, ordered: #t);
    let captured-id2
      = merge-hash-ids(current-id, captured-id1, ordered: ordered);
    current-id := captured-id2;
    current-state := key-state;
  end for;
  values(current-id, current-state);
end method collection-hash;

// This is similar to an equal-hash, except that it hashes things with
// ordered: #t and ignores the sequence keys. USE WITH CAUTION: This
// isn't a proper equal-hash because two collections of different types
// but identical key/element pairs won't generate the same hash id,
// even though the two collections are =.
//
define inline function sequence-hash
    (element-hash :: <function>, seq :: <sequence>,
     initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  let (current-id, current-state) = values(0, initial-state);
  for (elt in seq)
    let (id, state) = element-hash(elt, current-state);
    let captured-id = merge-hash-ids(current-id, id, ordered: #t);
    current-id := captured-id;
    current-state := state;
  end for;
  values(current-id, current-state);
end function sequence-hash;

define sealed inline method table-protocol (ht :: <object-table>) 
 => (key-test :: <function>, key-hash :: <function>);
  values(\==, object-hash);
end method table-protocol;

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
     default)
 => (result :: <object>);

  let bucket-index :: <integer> = modulo(key-id, ht.buckets.size);
  let bucket = ht.buckets[bucket-index];
  let find-result = find-elt(bucket, key, key-id, key=);
  
  if (find-result)
    ht.buckets[bucket-index] := find-result;
    find-result.entry-elt;
  elseif (default == $not-supplied)
    error("Element not found");
  else 
    default;
  end if;
end method table-element;

define inline method element
    (ht :: <table>, key :: <object>, #key default: default = $not-supplied )
 => (result :: <object>);
  let (key=, key-hash) = table-protocol(ht);
  let key-id :: <integer> = key-hash(key, $permanent-hash-state);
  table-element(ht, key, key=, key-id, default);
end method element;

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

define method element-setter
    (value :: <object>, ht :: <table>, key :: <object>) 
 => value :: <object>;
  let (key=, key-hash) = table-protocol(ht);
  let key-id :: <integer> = key-hash(key, $permanent-hash-state);
  let bucket-index :: <integer> = modulo(key-id, ht.buckets.size);
  let bucket-entry = find-elt(ht.buckets [bucket-index], key, key-id, key=);

  if (bucket-entry)
    ht.buckets[bucket-index] := bucket-entry;
    bucket-entry.entry-key := key;
    bucket-entry.entry-hash-id := key-id;
    bucket-entry.entry-elt := value;
    value;
  else
    ht.buckets[bucket-index]
      := make(<bucket-entry>, key: key, item: value, hash-id: key-id, 
              next: ht.buckets[bucket-index]);
    ht.table-size := ht.table-size + 1;
    maybe-expand-table(ht);
    value;
  end if;
end method element-setter;


define method maybe-shrink-table (ht :: <table>) => ();
  if (ht.size * 100 < (ht.buckets.size * $shrink-when))
    let target = truncate/(ht.buckets.size * $shrink-to, 100);
    resize-table(ht, find-new-size(target));
  end if;
end method maybe-shrink-table;

define method remove-key! (ht :: <table>, key) => (found :: <boolean>);
  let (key=, key-hash) = table-protocol(ht);
  let key-id :: <integer> = key-hash(key, $permanent-hash-state);
  let bucket-index :: <integer> = modulo (key-id, ht.buckets.size);
  let bucket = ht.buckets[bucket-index];
  let the-item = find-elt(bucket, key, key-id, key=);

  if (the-item)
    ht.table-size := ht.table-size - 1;
    // Find-elt reorganized the chain so that out desired element is at
    // the front.  Therefore, we simply slip it off, and we've
    // effectively removed the element.
    ht.buckets[bucket-index] := the-item.entry-next;
    maybe-shrink-table(ht);

    #t;
  end if;
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
  table;
end method remove-all-keys!;

// Takes a hashtable and mutates it so that it has a different number of
// buckets.
//
define method resize-table (ht :: <table>, numbuckets :: <integer>);
  let new-array = make(<entry-vector>, size: numbuckets, fill: #f);

  local method iter (entry :: false-or(<bucket-entry>)) => ();
          if (entry)
            // eagerly grab this, since	it will be changing
            let next-entry = entry.entry-next;
            let index :: <integer> = modulo(entry.entry-hash-id, numbuckets);
            entry.entry-next := new-array[index];
            new-array[index] := entry;
            iter(next-entry);
          end if;
        end method iter;
  
  for (i :: <integer> from 0 below ht.buckets.size)
    iter(ht.buckets[i]);
  end for;
  ht.buckets := new-array;
end method resize-table;


define inline method size (ht :: <table>) => (result :: <integer>);
  ht.table-size;
end method size;

define inline method empty? (ht :: <table>) => (result :: <boolean>);
  ht.table-size = 0;
end method empty?;

define class <table-iterator> (<object>)
  slot current :: <integer> = 0, init-keyword: #"current";
  slot entries :: <entry-vector>, required-init-keyword: #"entries";
end class <table-iterator>;

define sealed domain make(singleton(<table-iterator>));
define sealed domain initialize(<table-iterator>);

define function botched-iteration-error() => ()
  error("Botched iteration!");
end;

define function make-initial-iteration-state(ht :: <table>)
 => (initial-state :: <table-iterator>)
  let sz :: <integer> = ht.size;
  let vec = make(<entry-vector>, size: sz);
  let i :: <integer> = 0;
  for (from-index :: <integer> from 0 below ht.buckets.size)
    for (entry :: false-or(<bucket-entry>) = ht.buckets[from-index]
	   then entry.entry-next,
	 while: entry)
      %element(vec, i) := entry;
      i := i + 1;
    end for;
  end for;
  unless (i == sz)
    error("Size botch in forward-iteration-protocol(<table>)");
  end unless;
  make(<table-iterator>, entries: vec);
end make-initial-iteration-state;

define function botched-table-iteration-error() => ()
  error("Botched table iteration!");
end;

define inline method forward-iteration-protocol (ht :: <table>)
 => (initial-state :: <table-iterator>,
     limit :: <integer>,
     next-state :: <function>,
     finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>,
     current-element-setter :: <function>,
     copy-state :: <function>);

  values (make-initial-iteration-state(ht), ht.size,
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
            let current-item = state.current;
            let entry-list = state.entries;
            if (current-item >= entry-list.size)
              botched-table-iteration-error();
            end if;
	    %element(entry-list, current-item).entry-key;
	  end method,
	  /* current-table-element */
	  method (ht :: <table>, state :: <table-iterator>) => elt :: <object>;
            let current-item = state.current;
            let entry-list = state.entries;
            if (current-item >= entry-list.size)
              botched-table-iteration-error();
            end if;
	    %element(entry-list, current-item).entry-elt;
	  end method,
	  /* current-table-element-setter */
	  method (value :: <object>, ht :: <table>, state :: <table-iterator>)
	   => value :: <object>;
            let current-item = state.current;
            let entry-list = state.entries;
            if (current-item >= entry-list.size)
              botched-table-iteration-error();
            end if;
	    %element(entry-list, current-item).entry-elt := value;
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
define method string-hash (s :: <string>, initial-state :: <hash-state>)
    => (id :: <integer>, state :: <hash-state>);
  sequence-hash(value-hash, s, initial-state);
end method string-hash;

// This string-hash method should have the same semantics as the standard
// one, but should be much faster.
//
define method string-hash (s :: <byte-string>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  for (id = 0 then merge-hash-ids(id, as(<integer>, s[i]), ordered: #t),
       i from 0 below s.size)
  finally
    values(id, initial-state);
  end for;
end method string-hash;

// Moved from string-extensions to prevent a circular library definition.
//
define method uppercase? (c :: <character>) => answer :: <boolean>;
  c >= 'A' & c <= 'Z';
end method uppercase?;
