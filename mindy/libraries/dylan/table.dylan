module:	    Hash-Tables
Author:	    Nick Kramer (nkramer@cs.cmu.edu)
rcs-header: $Header: /home/housel/work/rcs/gd/src/mindy/libraries/dylan/table.dylan,v 1.15 1995/03/12 22:06:35 rgs Exp $
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

// -------------------------------------------------------------------
// Mindy-specific code
// -------------------------------------------------------------------

// merge-hash-codes is predefined in Mindy. However, at present
// merge-hash-states is not. This calls merge-hash-codes and throws
// away information about the hash ids.
//
define method merge-hash-states (state1 :: <object>, state2 :: <object>) 
          => merged :: <object>;
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
//         // when <integer> is <extended-integer>. This constant
//         // should be as many consecutive 1 bits as will fit into a positive
//         // <fixed-integer>.
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

// These numbers are expressed as percentages.  200 for expand-when means
// when there are two objects for every bucket, the hash table will grow
// to expand-to % of the original size.  (Make sure how-much is greater than
// 100%, or you won't get what you want)
//
// Default-shrink-when and -to are handled similarly.  Shrink conditions
// are checked only when someone removes an element, and expand only
// when someone adds an element.  Be careful not to set shrink-when too
// high, because if you do the table could shrink immediately after it
// expands.
//
define constant default-starting-table-size :: <fixed-integer> =  13;
define constant default-expand-when         :: <fixed-integer> = 200;
define constant default-expand-to           :: <fixed-integer> = 300;
define constant default-shrink-when         :: <fixed-integer> = 10;
define constant default-shrink-to           :: <fixed-integer> = 100;


define class <bucket-entry> (<object>)
  slot key-slot                  , required-init-keyword: key:          ;
  slot hash-id-slot  :: <fixed-integer>, required-init-keyword: hash-id:      ;
  slot hash-state-slot           , required-init-keyword: hash-state:   ;
  slot item-slot                 , required-init-keyword: item:         ;
end class <bucket-entry>;


define abstract class <table> (<dictionary>)
  slot item-count-slot         :: <fixed-integer>;     // Number of keys
  slot bucket-array-slot       :: <vector>;
  slot bucket-count-slot       :: <fixed-integer>;     // size of bucket-array
  slot bucket-states-slot      :: <vector>;
  slot expand-when-slot        :: <fixed-integer>;
  slot expand-to-slot          :: <fixed-integer>;
  slot shrink-when-slot        :: <fixed-integer>;
  slot shrink-to-slot          :: <fixed-integer>;
  slot merged-hash-state-slot  :: <object>;
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


define method make-bucket-entry (key, hash-id :: <fixed-integer>, hash-state, item)
 => entry :: <bucket-entry>;
  make(<bucket-entry>,   
       key:        key, 
       hash-id:    hash-id, 
       hash-state: hash-state,
       item:       item);
end method make-bucket-entry;


define method make (c :: singleton (<table>), #rest key-value-pairs,
		    #all-keys)  =>  table :: <object-table>;
  apply(make, <object-table>, key-value-pairs);
end method make;


define method initialize (ht :: <table>,
			  #next next-method,
			  #key size: size       = default-starting-table-size,
			  expand-when: expand-when = default-expand-when,
			  expand-to:   expand-to   = default-expand-to,
			  shrink-when: shrink-when = default-shrink-when,
			  shrink-to:   shrink-to   = default-shrink-to);

  let size = if (size = 0) 1 else size end if;
  ht.bucket-array-slot    := make(<simple-object-vector>, 
				  size: size,
				  fill: #() );     // filled with empty lists
  ht.bucket-states-slot   := make(<simple-object-vector>,
				  size: size,
				  fill: $permanent-hash-state);
  ht.item-count-slot        := 0;
  ht.bucket-count-slot      := size;
  ht.expand-when-slot       := expand-when;
  ht.expand-to-slot         := expand-to;
  ht.shrink-when-slot       := shrink-when;
  ht.shrink-to-slot         := shrink-to;
  ht.merged-hash-state-slot := $permanent-hash-state;
  next-method();
end method initialize;


define method key-test (ht :: <table>) => test :: <function>;
  let test = table-protocol(ht);    // drop the second return value
  test;
end method key-test;


// equal-hash is used in the table-protocol as the hash-function 
// for equal tables. Calling convention is similar to object-hash.
//
// The default method for objects that don't have any 
// better methods defined. (We can't call object-hash, so what can we do?)
//
define method equal-hash (key :: <object>) 
          => (id :: <fixed-integer>, state :: <object>);
  values(42, $permanent-hash-state);
end method equal-hash;


// Call object-hash for characters, integers, symbols, classes,
// functions, and conditions.
//
define method equal-hash (key :: <character>)
          => (id :: <fixed-integer>, state :: <object>);
  object-hash(key);
end method equal-hash;


define method equal-hash (key :: <integer>)
          => (id :: <fixed-integer>, state :: <object>);
  object-hash(key);
end method equal-hash;


define method equal-hash (key :: <float>)
          => (id :: <fixed-integer>, state :: <object>);
  float-hash(key);
end method equal-hash;


define method equal-hash (key :: <symbol>)
          => (id :: <fixed-integer>, state :: <object>);
  object-hash(key);
end method equal-hash;


define method equal-hash (key :: <class>)
          => (id :: <fixed-integer>, state :: <object>);
  object-hash(key);
end method equal-hash;


define method equal-hash (key :: <function>)
          => (id :: <fixed-integer>, state :: <object>);
  object-hash(key);
end method equal-hash;


define method equal-hash (key :: <type>)
          => (id :: <fixed-integer>, state :: <object>);
  object-hash(key);
end method equal-hash;


define method equal-hash (key :: singleton (#f))
          => (id :: <fixed-integer>, state :: <object>);
  object-hash(key);
end method equal-hash;


define method equal-hash (key :: singleton (#t))
          => (id :: <fixed-integer>, state :: <object>);
  object-hash(key);
end method equal-hash;


define method equal-hash (key :: <condition>)
          => (id :: <fixed-integer>, state :: <object>);
  object-hash(key);
end method equal-hash;


define method equal-hash (col :: <collection>)
          => (id :: <fixed-integer>, state :: <object>);
  collection-hash(col, equal-hash, equal-hash);
end method equal-hash;


// Object-hash returns $permanent-hash-state for <fix-num>s, the only
// type of integer Mindy currently has. (Yes, ignore the "don't call
// object-hash" warning at the beginning of this file. Trust me, this
// works in *Mindy*) object-hash in Mindy does not return
// $permanent-hash-state for anything else.
//
define method value-hash (key :: <integer>)
          => (id :: <fixed-integer>, state :: <object>);
  object-hash(key);
end method value-hash;


define method value-hash (key :: <float>)
          => (id :: <fixed-integer>, state :: <object>);
  float-hash(key);
end method value-hash;


define method value-hash (key :: <character>)
          => (id :: <fixed-integer>, state :: <object>);
  value-hash(as(<integer>, key));
end method value-hash;


define method value-hash (key :: <symbol>)
          => (id :: <fixed-integer>, state :: <object>);
  string-hash(as(<string>, key));
end method value-hash;


define method value-hash (key :: singleton (#f))
          => (id :: <fixed-integer>, state :: <object>);
  values(0, $permanent-hash-state);
end method value-hash;


define method value-hash (key :: singleton (#t))
          => (id :: <fixed-integer>, state :: <object>);
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
define method collection-hash(col :: <collection>, key-hash :: <function>,
			      element-hash :: <function>)
          => (id :: <fixed-integer>, state :: <object>);
  let (current-id, current-state) = values(0, $permanent-hash-state);
  for (elt keyed-by key in col)
    let (elt-id, elt-state)           = element-hash(elt);
    let (key-id, key-state)           = key-hash(key);
    let (captured-id1, captured-state1) = merge-hash-codes(elt-id, elt-state,
							   key-id, key-state,
							   ordered: #t);
    let (captured-id2, captured-state2) = merge-hash-codes(current-id, 
							   current-state, 
							   captured-id1,
							   captured-state1,
							   ordered: #f);
    current-id    := captured-id2;
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
define method sequence-hash(seq :: <sequence>, element-hash :: <function>)
          => (id :: <fixed-integer>, state :: <object>);
  let (current-id, current-state) = values(0, $permanent-hash-state);
  for (elt in seq)
    let (id, state) = element-hash(elt);
    let (captured-id, captured-state) = merge-hash-codes(current-id, 
							 current-state, 
							 id, state,
							 ordered: #t);
    current-id    := captured-id;
    current-state := captured-state;
  end for;
  values(current-id, current-state);
end method sequence-hash;


// A convenient method for hashing strings. Calls sequence-hash 
// and "does the right thing."
//
define method string-hash (s :: <string>)
    => (id :: <fixed-integer>, state :: <object>);
  sequence-hash(s, value-hash);
end method string-hash;


define method table-protocol(ht :: <object-table>) 
         => (key-test :: <function>, key-hash :: <function>);
  values(\==, object-hash);
end method table-protocol;


define method table-protocol(ht :: <equal-table>) 
         => (key-test :: <function>, key-hash :: <function>);
  values(\=, equal-hash);
end method table-protocol;


define constant not-in-ht2 = "not-in-ht2";

// Informally, two hash tables are = if they use the same key test,
// have the same size, and all the elements in the first hash table
// have matching elements in the second hash table.
//
define method \= (ht1 :: <table>, ht2 :: <table>);
  let test1 = key-test (ht1);
  let test2 = key-test (ht2);
  (test1 == test2) 
    & size(ht1) = size(ht2) 
    & block (return)
	for (elt1 keyed-by key in ht1)
	  let elt2 = element (ht2, key, default: not-in-ht2);
	  if (elt2 == not-in-ht2 | ~test1 (elt1, elt2))
	    return(#f);
	  end if;
	end for;
	#t;
      end block;
end method \=;


// Returns the first element of the list that satisfies
// test.  Internal use only.
//
define method find-elt (list :: <list>, key, key-id, key=)
  if (empty?(list))
    #f;
  else
    let elem = head(list);
    if ((elem.hash-id-slot = key-id) & key=(elem.key-slot, key))
      elem;
    else
      find-elt( tail (list), key, key-id, key=);
    end if;
  end if;
end method find-elt;


define constant no-default = list("No default");

// This function looks redundant at times, but it's necessary in order
// to avoid race conditions with the garbage collector.
//
define method element (  ht :: <table>, key, 
		         #key default: default = no-default )
  // We don't yet check for out of date data, since the element might match
  // anyway, and the lookup is much cheaper than a rehash.

  let (key=, key-hash)      = table-protocol(ht);
  let (key-id, key-state)   = key-hash(key);
  let bucket-index          = modulo(key-id, ht.bucket-count-slot);
  let bucket                = ht.bucket-array-slot[bucket-index];
  let find-result           = find-elt(bucket, key, key-id, key=);
  
  if (find-result)
    find-result.item-slot;

  // Check to see if there was a garbage collection in the middle
  // of this method. If there was, start over.
  elseif (~ state-valid?(ht.merged-hash-state-slot)
      | ~ state-valid?(key-state) )
    rehash(ht);
    element(ht, key, default: default);
  elseif (default == no-default)
    error("Element not found");
  else 
    default;
  end if;
end method element;


// This is exactly the same code without the garbage collection stuff
//
define method element (  ht :: <value-table>, key, 
		         #key default: default = no-default )
  let (key=, key-hash)      = table-protocol(ht);
  let key-id                = key-hash(key);
  let bucket-index          = modulo(key-id, ht.bucket-count-slot);
  let bucket                = ht.bucket-array-slot[bucket-index];
  let find-result           = find-elt(bucket, key, key-id, key=);
  
  if (find-result)
    find-result.item-slot;
  elseif (default == no-default)
    error ("Element not found");
  else 
    default;
  end if;
end method element;


define method find-new-size (target :: <integer>) => (result :: <integer>);
  for (num from if (even?(target)) target + 1 else target end if by 2,
       until (modulo(num, 3) > 0 & modulo(num, 5) > 0 & modulo(num, 7) > 0
		& modulo(num, 11) > 0))
  finally
    num;
  end for;
end method find-new-size;

// This function looks redundant at times, but it's necessary in order
// to avoid race conditions with the garbage collector.
//
define method element-setter (value :: <object>, ht :: <table>, 
			      key :: <object>) => value :: <object>;

  let (key=, key-hash)    = table-protocol(ht);
  let (key-id, key-state) = key-hash(key);
  let bucket-index        = modulo(key-id, ht.bucket-count-slot);
  let bucket-entry        = find-elt(ht.bucket-array-slot [bucket-index],
				     key, key-id, key=);

     // Check to see if there was a garbage collection in the middle
     // of this method. If there was, start over.

  if (~ state-valid?(ht.merged-hash-state-slot)
      | ~ state-valid?(key-state) )
    rehash(ht);
    element-setter(value, ht, key);
       
             // Else, there was no garbage collection, and we're safe.
             // (If there is a garbage collection between now and the
             // the end of this method, it invalidates the states we're
             // about to write, but we can just re-compute them on
             // the next lookup)

  else

    if (bucket-entry = #f)             // If item didn't exist, add it
      bucket-entry := make-bucket-entry(key, key-id, key-state, value);

      ht.bucket-array-slot[bucket-index] := 
	     pair(bucket-entry, ht.bucket-array-slot[bucket-index]);
      ht.item-count-slot := ht.item-count-slot + 1;

      if (size(ht) * 100 > (ht.bucket-count-slot * ht.expand-when-slot))
	let target = truncate/(ht.bucket-count-slot * ht.expand-to-slot, 100);
	resize-table(ht, find-new-size(target));
      end if;
    else     // Item WAS found
      bucket-entry.key-slot        := key;
      bucket-entry.hash-id-slot    := key-id;
      bucket-entry.hash-state-slot := key-state;
      bucket-entry.item-slot       := value;
    end if;

          // Update bucket's merged-hash-state
    ht.bucket-states-slot[bucket-index] := 
	         merge-hash-states(bucket-entry.hash-state-slot, 
				   ht.bucket-states-slot [bucket-index]);

    // Update table's merged hash codes
    ht.merged-hash-state-slot := 
      merge-hash-states(bucket-entry.hash-state-slot, 
			ht.merged-hash-state-slot);
    value;
  end if;
end method element-setter;


// This is exactly the same code without the garbage collection stuff
//
define method element-setter (value :: <object>, ht :: <value-table>, 
			      key :: <object>) => value :: <object>;
  let (key=, key-hash)    = table-protocol(ht);
  let key-id              = key-hash(key);
  let bucket-index        = modulo(key-id, ht.bucket-count-slot);
  let bucket-entry        = find-elt(ht.bucket-array-slot [bucket-index],
				     key, key-id, key=);

  if (bucket-entry = #f)             // If item didn't exist, add it
    bucket-entry := make-bucket-entry(key, key-id,
				      $permanent-hash-state, 
				      value);
    
    ht.bucket-array-slot[bucket-index] := 
           pair(bucket-entry, ht.bucket-array-slot[bucket-index]);
    ht.item-count-slot := ht.item-count-slot + 1;

    if (size(ht) * 100 > (ht.bucket-count-slot * ht.expand-when-slot))
      let target = truncate/(ht.bucket-count-slot * ht.expand-to-slot, 100);
      resize-table(ht, find-new-size(target));
    end if;
  else     // Item WAS found
    bucket-entry.key-slot        := key;
    bucket-entry.hash-id-slot    := key-id;
    bucket-entry.item-slot       := value;
  end if;
  value;
end method element-setter;


define method remove-key! (ht :: <table>, key) => new-ht :: <table>;
  until (state-valid?(ht.merged-hash-state-slot))
    rehash(ht);
  end until;

  let (key=, key-hash)      = table-protocol(ht);
  let (key-id, key-state)   = key-hash(key);
  let bucket-index          = modulo (key-id, ht.bucket-count-slot);
  let bucket                = ht.bucket-array-slot[bucket-index];
  let the-item = find-elt(bucket, key, key-id, key=);

  if (~state-valid?(ht.merged-hash-state-slot)
      | ~state-valid?(key-state))
    remove-key!(ht, key);    // If state not valid, goto beginning
			     // for a rehash
  else
    if (the-item ~= #f)       // An item with that key was found
	ht.item-count-slot := ht.item-count-slot - 1;

           // Between find-elt and remove!, this traverses the bucket
           // twice. It could be improved, but one has to be careful 
           // to avoid race conditions with the garbage collector.

	ht.bucket-array-slot[bucket-index] := remove!(bucket, the-item);

        if (size (ht) * 100 < (ht.bucket-count-slot * ht.shrink-when-slot))
	  let target = truncate/(ht.bucket-count-slot * ht.shrink-to-slot,
				 100);
	  resize-table(ht, find-new-size(target));
	end if;

      // We leave all the merged-states as is. rehash will take care of it
      // if a remove-key! made the merged-state information overly cautious.

    end if; // had to remove something
    ht;
  end if;   // states valid?
end method remove-key!;


// This is exactly the same code without the garbage collection stuff
//
define method remove-key! (ht :: <value-table>, key) => new-ht :: <table>;
  let (key=, key-hash)      = table-protocol(ht);
  let key-id                = key-hash(key);
  let bucket-index          = modulo(key-id, ht.bucket-count-slot);
  let bucket                = ht.bucket-array-slot[bucket-index];

  let the-item = find-elt(bucket, key, key-id, key=);

  if (the-item ~= #f)       // An item with that key was found
    ht.item-count-slot := ht.item-count-slot - 1;

           // Between find-elt and remove!, this traverses the bucket
           // twice. It could be improved.

    ht.bucket-array-slot[bucket-index] := remove!(bucket, the-item);

    if (size(ht) * 100 < (ht.bucket-count-slot * ht.shrink-when-slot))
      let target = truncate/(ht.bucket-count-slot * ht.shrink-to-slot, 100);
      resize-table(ht, find-new-size(target));
    end if;
  end if; // had to remove something
  ht;
end method remove-key!;


// Takes a hashtable and mutates it so that it has a different number of
// buckets.
//
define method resize-table (ht :: <table>, numbuckets :: <fixed-integer>);
  let new-array = make(<simple-object-vector>, 
		       size: numbuckets,
		       fill: #()   );
  let new-state-array = make(<simple-object-vector>,
			     size: numbuckets,
			     fill: $permanent-hash-state   );

  for (bucket in ht.bucket-array-slot)
    for (entry in bucket)
      let index = modulo(entry.hash-id-slot, numbuckets);
      new-array[index] := pair(entry, new-array [index]);
      new-state-array[index] := merge-hash-states(new-state-array [index],
						   entry.hash-state-slot);
    end for;
  end for;

  ht.bucket-array-slot  := new-array;
  ht.bucket-states-slot := new-state-array;
  ht.bucket-count-slot  := numbuckets;
end method resize-table;


// This version of resize-table doesn't bother updating any of the
// merged state slots, arrays, etc.
//
define method resize-table (ht :: <value-table>, numbuckets :: <fixed-integer>)
  let new-array = make(<simple-object-vector>, 
		       size: numbuckets,
		       fill: #()   );

  for (bucket in ht.bucket-array-slot)
    for (entry in bucket)
      let index = modulo(entry.hash-id-slot, numbuckets);
      new-array[index] := pair(entry, new-array[index]);
    end for;
  end for;

  ht.bucket-array-slot := new-array;
  ht.bucket-count-slot := numbuckets;
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
  let (key=, key-hash)  =  table-protocol(ht);
  let deferred-elements = #();

  for (i from 0 below ht.bucket-count-slot)
    if (~ state-valid?(ht.bucket-states-slot[i]))     // rehash bucket
      ht.bucket-states-slot[i] := $permanent-hash-state;

      let bucket    = ht.bucket-array-slot[i];
      ht.bucket-array-slot[i] := #();
      
      for (remaining = bucket then next,
	   next = bucket.tail then next.tail, // depends on #().tail == #()
	   until remaining == #())
	let bucket-entry = head(remaining);

	if (state-valid?(bucket-entry.hash-state-slot))
	  // Put it back into the same bucket
	  remaining.tail := ht.bucket-array-slot[i];
	  ht.bucket-array-slot[i] := remaining;
	else  // state is invalid
	  let (id, state) = key-hash(bucket-entry.key-slot);  
	  bucket-entry.hash-id-slot    := id;
	  bucket-entry.hash-state-slot := state;
	  let index = modulo(id, ht.bucket-count-slot);

	  if (index <= i)
	    // Put it back into a previously processed bucket
	    remaining.tail := ht.bucket-array-slot[index];
	    ht.bucket-array-slot[index] := remaining;
	    ht.bucket-states-slot[index] := 
	      merge-hash-states(state, ht.bucket-states-slot[index]);
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
       until remaining == #())
    let bucket-entry = head(remaining);

    let id = bucket-entry.hash-id-slot;
    let state = bucket-entry.hash-state-slot;
    let index = modulo(id, ht.bucket-count-slot);

    // Put it back into a previously processed bucket
    remaining.tail := ht.bucket-array-slot[index];
    ht.bucket-array-slot[index] := remaining;
    ht.bucket-states-slot[index] := 
      merge-hash-states(state, ht.bucket-states-slot[index]);
  end for;    // Finished traversing the deferred elements

  ht.merged-hash-state-slot := reduce(merge-hash-states,
				      $permanent-hash-state,
				      ht.bucket-states-slot);
  ht;
end method rehash;


define method size (ht :: <table>)
  ht.item-count-slot;
end method size;


define method empty? (ht :: <table>)
  ht.item-count-slot = 0;
end method empty?;


// Inherit mapping functions

// -------------------------------------------------------------------
//                Iteration protocol stuff
// -------------------------------------------------------------------

// All these things are needed in the state, because many of the functions
// get nothing but a hash table and a state.


// This is the iteration state, not a hash-state
//
define class <ntable-state> (<object>)
  slot elements-touched-slot, init-keyword: elements-touched: ;
  slot bucket-index :: <fixed-integer>, init-keyword: #"index";
  slot bucket-cell :: <list>, init-keyword: #"cell";
end class <ntable-state>;


define constant finished-table-state?
  = method (ht :: <table>,
	    state :: <ntable-state>,
	    limit)
      state.elements-touched-slot >= ht.item-count-slot;
    end method;


define constant next-table-state
  = method (ht    :: <table>,
	    state :: <ntable-state>) 
     => new-state :: <ntable-state>;
      state.elements-touched-slot := state.elements-touched-slot + 1;
      if (~finished-table-state?(ht, state, #f))
	let new-cell = state.bucket-cell.tail;
	if (new-cell == #())
	  for (i from state.bucket-index + 1,
	       until ht.bucket-array-slot[i] ~= #())
	  finally
	    state.bucket-index := i;
	    state.bucket-cell := ht.bucket-array-slot[i];
	  end for;
	else
	  state.bucket-cell := new-cell;
	end if;
      end if;             // End of more objects left in hash table?
      state;            // Return the new and improved state object
    end method;


define method get-bucket-entry (ht :: <table>, state :: <ntable-state>)
                  => entry :: <bucket-entry>;
  state.bucket-cell.head;
end method get-bucket-entry;


define constant current-table-key
  = method (ht :: <table>, state :: <ntable-state>)
      let bucket-entry = get-bucket-entry(ht, state);
      bucket-entry.key-slot;
    end method;


define constant current-table-element
  = method (ht :: <table>, state :: <ntable-state>)
      let bucket-entry = get-bucket-entry(ht, state);
      bucket-entry.item-slot;
    end method;


define constant current-table-element-setter
  = method (value,
	    ht    :: <table>,
	    state :: <ntable-state>)
      // This argument order isn't mentioned anywhere I can find,
      // but seems to be what is expected

      let new-bucket-entry = get-bucket-entry(ht, state);
      new-bucket-entry.item-slot := value;
      value;
    end method;


define constant copy-table-state
  = method (ht :: <table>, old-state :: <ntable-state>)
      make(<ntable-state>, elements-touched: old-state.elements-touched-slot,
	   index: old-state.bucket-index, cell: old-state.bucket-cell);
    end method;


define method make-table-state (ht :: <table>) 
               => table-state :: <ntable-state>;
  let result = make(<ntable-state>, elements-touched: -1,
		    index: -1, cell: #()); // Depend on tail(#()) == #()
  next-table-state(ht, result);
end method make-table-state;


define method forward-iteration-protocol (ht :: <table>)
  values (make-table-state(ht),       // initial hash state
	  #f,             // limit -- isn't actually used by finished-state?
	  next-table-state,
	  finished-table-state?,
	  current-table-key,
	  current-table-element,
	  current-table-element-setter,
	  copy-table-state);
end method forward-iteration-protocol;
