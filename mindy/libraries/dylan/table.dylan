module:	    %Hash-Tables
Author:	    Nick Kramer (nkramer@cs.cmu.edu)
rcs-header: $Header: /scm/cvs/src/mindy/libraries/dylan/table.dylan,v 1.1 1998/05/03 19:55:21 andreas Exp $
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
    => (id :: <integer>, state :: <object>);

define open generic equal-hash (thing :: <object>)
    => (id :: <integer>, state :: <object>);


// The class of all objects the DRM describes as a hash state
//
define constant <hash-state> = <object>;

// -------------------------------------------------------------------
// Mindy-specific code
// -------------------------------------------------------------------

// merge-hash-codes is predefined in Mindy. However, at present
// merge-hash-states is not. This calls merge-hash-codes and throws
// away information about the hash ids.
//
define method merge-hash-states (state1 :: <object>, state2 :: <object>) 
          => merged :: <hash-state>;
  let (junk, new-state) = merge-hash-codes (0, state1, 0, state2);
  new-state;
end method merge-hash-states;


// -------------------------------------------------------------------
// Stuff that Mindy takes care of, but other implementations might not:
// -------------------------------------------------------------------

// Also be sure to verify that equal-hash and value-hash work as
// advertised. They depend on object-hash (which is always defined,
// but might not behave as Mindy's does) and float-hash (which is
// implemented in Mindy but not standard).

// define constant $permanent-hash-state = #f;
//
// define constant magic-hash-constant = #x3fffffff;
//         // And'ed with hash id's to keep the size under control 
//         // when <general-integer> is <extended-integer>. This constant
//         // should be as many consecutive 1 bits as will fit into a positive
//         // <integer>.
// 
// define constant shift-dist          = 15;
//         // This should be one half the size of an integer (in bits)
//         // for reason that xor'ing the right shifted with the left
//         // left shifted hash value is less sensical if shift-dist
//         // is not 1/2 int size
// 
// define constant $permanent-hash-state = #f;
// 
// 
// define method merge-hash-ids (id1 :: <integer>, id2 :: <integer>,
// 			      #key ordered: ordered = #f )
//                     => hash-id :: <integer>;
// 
//   if (ordered)
//     logand (magic-hash-constant,
// 	    logxor (logxor (ash (id1, shift-dist),
// 			    ash (id1, -shift-dist)),
// 		    id2));
//   else
//     logand (magic-hash-constant, logxor (id1, id2));
//   end if;
// end method merge-hash-ids;
// 
// 
// define method merge-hash-states (state1, state2)
//   if (state1 ~= $permanent-hash-state)
//          if (state2 ~= $permanent-hash-state)
// 	   min (state1, state2);
// 	 else
// 	   state1;
// 	 end if;
//   else
//          state2;
//   end if;
// end method merge-hash-states;
// 
// 
// define method merge-hash-codes (id1 :: <integer>, state1,
// 				id2 :: <integer>, state2,
// 				#key ordered: ordered = #f )
// 
//   values ( merge-hash-ids (id1, id2, ordered: ordered),
// 	   merge-hash-states (state1, state2)
// 	 );
// end method merge-hash-codes;

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

define abstract class <table>
    (<mutable-explicit-key-collection>, <stretchy-collection>)
  slot table-size :: <integer>, init-value: 0;  // Number of keys
  slot buckets :: <simple-object-vector>;  // vector of <bucket-entry>s
  slot bucket-states :: <simple-object-vector>;  // the merged states of each
                                                 // of the buckets
  slot table-hash-state :: <object>, init-value: $permanent-hash-state;
end class <table>;

// Uses == (aka id?) as key comparison
//
define class <object-table> (<table>)
end class <object-table>;

// Uses = as key comparison
//
define class <equal-table> (<table>)
end class <equal-table>;

// Uses a user defined key comparison and hash function, so long as
// the hash function doesn't involve addresses.
//
define abstract class <value-table> (<table>)
end class <value-table>;

define method make 
    (c == <table>, #rest key-value-pairs, #all-keys)
 =>  table :: <object-table>;
  apply(make, <object-table>, key-value-pairs);
end method make;

// Table init does most of the work of initialize, except we move it out of
// initialize to allow remove-all-keys! to call it.
//
define method table-init (ht :: <table>, #key size: sz = $starting-table-size)
  let sz = if (sz = 0) 1 else sz end if;
  ht.buckets := make(<simple-object-vector>, size: sz, fill: #() );
  ht.bucket-states := make(<simple-object-vector>, 
			   size: sz, fill: $permanent-hash-state);
end method table-init;

define method initialize (ht :: <table>,
			  #next next-method,
			  #key size: sz = $starting-table-size);
  next-method();
  table-init(ht, size: sz);
end method initialize;

define method key-test (ht :: <table>) => test :: <function>;
  let test = table-protocol(ht);    // drop the second return value
  test;
end method key-test;


define method object-hash (key :: <object>)
 => (id :: <integer>, state :: <hash-state>);
  pointer-hash(key);
end method object-hash;

define method object-hash (key :: <symbol>)
 => (id :: <integer>, state :: <hash-state>);
  values(key.symbol-hash, $permanent-hash-state);
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

define method object-hash (key :: <float>)
 => (id :: <integer>, state :: <hash-state>);
  float-hash(key);
end method object-hash;

define method object-hash (key :: <character>)
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
define method value-hash (key :: <general-integer>)
 => (id :: <integer>, state :: <hash-state>);
  object-hash(key);
end method value-hash;

define method value-hash (key :: <float>)
 => (id :: <integer>, state :: <hash-state>);
  float-hash(key);
end method value-hash;

define method value-hash (key :: <character>)
 => (id :: <integer>, state :: <hash-state>);
  object-hash(key);
end method value-hash;

define method value-hash (key :: <symbol>)
 => (id :: <integer>, state :: <hash-state>);
  // We have introduced a special object-hash for symbols which is GC
  // independent.  This may not be true of all implementations, so be
  // careful. 
  object-hash(key);
end method value-hash;

define method value-hash (key == #f)
 => (id :: <integer>, state :: <hash-state>);
  values(0, $permanent-hash-state);
end method value-hash;

define method value-hash (key == #t)
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
define function collection-hash
    (key-hash :: <function>, element-hash :: <function>, col :: <collection>,
     #key ordered :: <boolean> = #f)
 => (id :: <integer>, state :: <hash-state>);
  let (current-id, current-state) = values(0, $permanent-hash-state);
  for (elt keyed-by key in col)
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
end function collection-hash;

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

define method table-protocol (ht :: <object-table>) 
 => (key-test :: <function>, key-hash :: <function>);
  values(\==, object-hash);
end method table-protocol;

define method table-protocol (ht :: <equal-table>) 
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
    & ht1.size = ht2.size
    & block (return)
	for (elt1 keyed-by key in ht1)
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
    (list :: <list>, key :: <object>, key-id :: <object>, key= :: <function>)
 => whatever :: <object>;
  if (list.empty?)
    #f;
  else
    let elem = list.head;
    if ((elem.entry-hash-id = key-id) & key=(elem.entry-key, key))
      elem;
    else
      find-elt(list.tail, key, key-id, key=);
    end if;
  end if;
end method find-elt;

// This function looks redundant at times, but it's necessary in order
// to avoid race conditions with the garbage collector.
//
define method element (ht :: <table>, key :: <object>, 
		       #key default: default = $not-supplied )
 => elt :: <object>;
  // We don't yet check for outdated hash states, since the element
  // might match anyway, and the lookup is much cheaper than a rehash.

  let (key=, key-hash) = table-protocol(ht);
  let (key-id, key-state) = key-hash(key);
  let bucket-index = modulo(key-id, ht.buckets.size);
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
define method element (  ht :: <value-table>, key, 
		         #key default: default = $not-supplied )
 => elt :: <object>;
  let (key=, key-hash)      = table-protocol(ht);
  let key-id                = key-hash(key);
  let bucket-index          = modulo(key-id, ht.buckets.size);
  let bucket                = ht.buckets[bucket-index];
  let find-result           = find-elt(bucket, key, key-id, key=);
  
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
define method find-new-size (target :: <integer>) => (result :: <integer>);
  for (num from if (even?(target)) target + 1 else target end if by 2,
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
  let (key-id, key-state) = key-hash(key);
  let bucket-index = modulo(key-id, ht.buckets.size);
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
    if (bucket-entry = #f)             // If item didn't exist, add it
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
  let (key=, key-hash)    = table-protocol(ht);
  let key-id              = key-hash(key);
  let bucket-index        = modulo(key-id, ht.buckets.size);
  let bucket-entry        = find-elt(ht.buckets [bucket-index],
				     key, key-id, key=);

  if (bucket-entry = #f)             // If item didn't exist, add it
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

define method remove-key! (ht :: <table>, key)
 => removed-anything? :: <boolean>;
  while (~ht.table-hash-state.state-valid?)
    rehash(ht);
  end while;
  let (key=, key-hash)      = table-protocol(ht);
  let (key-id, key-state)   = key-hash(key);
  let bucket-index          = modulo (key-id, ht.buckets.size);
  let bucket                = ht.buckets[bucket-index];
  let the-item = find-elt(bucket, key, key-id, key=);

  if (~ht.table-hash-state.state-valid? | ~key-state.state-valid?)
    remove-key!(ht, key); // If state not valid, goto beginning for a rehash
  else
    if (the-item ~= #f)       // An item with that key was found
      ht.table-size := ht.table-size - 1;

      // Between find-elt and remove!, this traverses the bucket
      // twice. It could be improved, but one has to be careful to
      // avoid race conditions with the garbage collector.

      ht.buckets[bucket-index] := remove!(bucket, the-item);
      maybe-shrink-table(ht);

      // We leave all the merged-states as is. rehash will take care of it
      // if a remove-key! made the merged-state information overly cautious.
    end if; // had to remove something
    the-item ~== #f;  // #t if we removed something, #f otherwise
  end if;   // states valid?
end method remove-key!;

// This is exactly the same code without the garbage collection stuff
//
define method remove-key! (ht :: <value-table>, key)
 => removed-anything? :: <boolean>;
  let (key=, key-hash)      = table-protocol(ht);
  let key-id                = key-hash(key);
  let bucket-index          = modulo(key-id, ht.buckets.size);
  let bucket                = ht.buckets[bucket-index];

  let the-item = find-elt(bucket, key, key-id, key=);

  if (the-item ~= #f)       // An item with that key was found
    ht.table-size := ht.table-size - 1;

           // Between find-elt and remove!, this traverses the bucket
           // twice. It could be improved.

    ht.buckets[bucket-index] := remove!(bucket, the-item);

    maybe-shrink-table(ht);
  end if; // had to remove something
  the-item ~== #f;  // #t if we removed something, #f otherwise
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

  for (i from 0 below ht.buckets.size)
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

define method size (ht :: <table>) => size :: <integer>;
  ht.table-size;
end method size;

define method empty? (ht :: <table>) => answer :: <boolean>;
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
  slot elements-touched-slot, required-init-keyword: #"elements-touched";
  slot bucket-index :: <integer>, required-init-keyword: #"index";
  slot bucket-cell :: <list>, required-init-keyword: #"cell";
end class <table-state>;

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
	       until: ht.buckets[i] ~= #())
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
