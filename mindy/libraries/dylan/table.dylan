module:		Dylan
Author:		Nick Kramer (nkramer@cs.cmu.edu)

//////////////////////////////////////////////////////////////////////
//
//  Copyright (C) 1994, Carnegie Mellon University
//  All rights reserved.
//
//  This code was produced by the Gwydion Project at Carnegie Mellon
//  University.  If you are interested in using this code, contact
//  "Scott.Fahlman@cs.cmu.edu" (Internet).
//
//////////////////////////////////////////////////////////////////////
//
//  $Header: /home/housel/work/rcs/gd/src/mindy/libraries/dylan/table.dylan,v 1.5 1994/06/13 13:01:31 nkramer Exp $
//

/* -------------------------------------------------------------------
 *
 *  Implements <table>, <object-table>, <equal-table>, and <value-table>.
 *
 * ------------------------------------------------------------------- */

// Author's note: "ht" is my abbreviation for "hashtable", and is used
// as a parameter quite frequently.

// <object-table>s are as defined in the book, operating on pointers and
// using == as a comparator.

// <equal-table>s use = as a key test, but since = uses == as a
// default method, <equal-table>s also have to worry about garbage
// collection.

// <value-table>s take a user defined key test and key hash function
// THAT DOESN'T INVOLVE ADDRESSES. (ie, the key hash involves values,
// not addresses) The functions must be fully defined on all objects
// that the user will be placing in the value-table. The key-test
// defaults to =, but there is no default key-hash. The user must
// supply a key-hash that always returns $permanent-hash-state. (Note
// that object-hash returns $permanent-hash-state under many
// circumstances)

/* -------------------------------------------------------------------
 * Mindy-specific code
 * ------------------------------------------------------------------- */

// merge-hash-codes is predefined in Mindy. However, at present
// merge-hash-states is not. This calls merge-hash-codes and throws
// away information about the hash ids.
// Hopefully in the future merge-hash-states will also be predefined
// in Mindy.

define method merge-hash-states (state1 :: <object>, state2 :: <object>) 
          => merged :: <object>;
  let (junk, new-state) = merge-hash-codes (0, state1, 0, state2);

  new-state;
end method merge-hash-states;

/* -------------------------------------------------------------------
 * Stuff that Mindy takes care of, but other implementations might not:
 * ------------------------------------------------------------------- */

// define constant $permanent-hash-state = #f;
//
// // Define no-default if it isn't already defined somewhere else.
// define constant no-default = "no-default";
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
// /* ---------------- */
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
// /* ---------------- */
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
// /* ---------------- */
// 
// define method merge-hash-codes (id1 :: <integer>, state1,
// 				id2 :: <integer>, state2,
// 				#key ordered: ordered = #f )
// 
//   values ( merge-hash-ids (id1, id2, ordered: ordered),
// 	   merge-hash-states (state1, state2)
// 	 );
// end method merge-hash-codes;

/* -------------------------------------------------------------------
 * Portable implementation
 * ------------------------------------------------------------------- */

define constant default-starting-table-size :: <integer> =  5;
define constant default-expand-when         :: <integer> = 200;
define constant default-expand-to           :: <integer> = 300;
define constant default-shrink-when         :: <integer> = 10;
define constant default-shrink-to           :: <integer> = 100;

/* These numbers are expressed as percentages. 200 for expand-when means
 * when there are two objects for every bucket, the hash table will grow
 * to expand-to % of the original size. (Make sure how-much is greater than
 * 100%, or you won't get what you want)
 * Default-shrink-when and -to are handled similarly. Shrink conditions
 * are checked only when someone removes an element, and expand only
 * when someone adds an element. Be careful not to set shrink-when too
 * high, because if you do the table could shrink immediately after it
 * expands.
 */

/* ---------------- */

define class <bucket-entry> (<object>)
  slot key-slot                  , required-init-keyword: key:               ;
  slot hash-id-slot  :: <integer>, required-init-keyword: hash-id:           ;
  slot hash-state-slot           , required-init-keyword: hash-state:        ;
  slot item-slot                 , required-init-keyword: item:              ;
end class <bucket-entry>;

/* ---------------- */

define class <table> (<mutable-explicit-key-collection>,
		       <stretchy-collection>)
  slot item-count-slot         :: <integer>;
           // Number of keys
  slot bucket-array-slot       :: <vector>;
  slot bucket-count-slot       :: <integer>;
           // size of bucket-array
  slot bucket-states-slot      :: <vector>;
  slot expand-when-slot        :: <integer>;
  slot expand-to-slot          :: <integer>;
  slot shrink-when-slot        :: <integer>;
  slot shrink-to-slot          :: <integer>;
  slot merged-hash-state-slot  :: <object>;
end class <table>;
	  
/* ---------------- */

// Uses == (aka id?) as key comparison

define class <object-table> (<table>)
end class <object-table>;

/* ---------------- */

// Uses = as key comparison

define class <equal-table> (<table>)
end class <equal-table>;

/* ---------------- */

define class <value-table> (<table>)
  slot key-test-slot :: <function>, init-keyword: key-test: ;
  slot key-hash-slot :: <function>, required-init-keyword: key-hash: ;
end class <value-table>;

/* ---------------- */

define method make-bucket-entry (key, hash-id :: <integer>, hash-state, item)
          => entry :: <bucket-entry>;
  make (<bucket-entry>,   
	key:        key, 
	hash-id:    hash-id, 
	hash-state: hash-state,
	item:       item);
end method make-bucket-entry;

/* ---------------- */

define method make (c :: singleton (<table>), #rest key-value-pairs,
		    #all-keys)  =>  table :: <object-table>;
  apply (make, <object-table>, key-value-pairs);
end method make;

/* ---------------- */

define method initialize (ht :: <table>,
			  #next next-method,
			  #key size: size       = default-starting-table-size,
			  buckets: numbuckets   = default-starting-table-size,
			  expand-when: expand-when = default-expand-when,
			  expand-to:   expand-to   = default-expand-to,
			  shrink-when: shrink-when = default-shrink-when,
			  shrink-to:   shrink-to   = default-shrink-to);

  ht.bucket-array-slot    := make (<simple-object-vector>, 
				   size: numbuckets,
				   fill: #() );     // filled with empty lists

  ht.bucket-states-slot   := make (<simple-object-vector>,
				   size: numbuckets,
				   fill: $permanent-hash-state);

  ht.item-count-slot        := 0;
  ht.bucket-count-slot      := numbuckets;
  ht.expand-when-slot       := expand-when;
  ht.expand-to-slot         := expand-to;
  ht.shrink-when-slot       := shrink-when;
  ht.shrink-to-slot         := shrink-to;
  ht.merged-hash-state-slot := $permanent-hash-state;

  next-method ();
end method initialize;


define method initialize (ht :: <value-table>,
			  #next next-method,
			  #rest key-value-pairs,
			  #key key-test: key-test-function = \=,
			  key-hash: key-hash-function);
  ht.key-test-slot := key-test-function;
  ht.key-hash-slot := key-hash-function;
  apply(next-method, ht, key-value-pairs);
end method initialize;

/* ---------------- */

define method key-test (ht :: <object-table>) => test :: <function>;
  \==;
end method key-test;


define method key-test (ht :: <equal-table>) => test :: <function>;
  \=;
end method key-test;


define method key-test (ht :: <value-table>) => test :: <function>;
  ht.key-test-slot;
end method key-test;

/* ---------------- */

// equal-hash is used in the table-protocol as the hash-function 
// for equal tables. Calling convention is similar to object-hash.

// The default method for objects that don't have any 
// better methods defined. (We can't call object-hash, so what can we do?)

define method equal-hash (key :: <object>) 
          => (id :: <integer>, state :: <object>);
  values (42, $permanent-hash-state);
end method equal-hash;


// Call object-hash for characters, integers, symbols, classes,
// functions, and conditions.

define method equal-hash (key :: <character>)
          => (id :: <integer>, state :: <object>);
  object-hash (key);
end method equal-hash;

define method equal-hash (key :: <integer>)
          => (id :: <integer>, state :: <object>);
  object-hash (key);
end method equal-hash;

define method equal-hash (key :: <symbol>)
          => (id :: <integer>, state :: <object>);
  object-hash (key);
end method equal-hash;

define method equal-hash (key :: <class>)
          => (id :: <integer>, state :: <object>);
  object-hash (key);
end method equal-hash;

define method equal-hash (key :: <function>)
          => (id :: <integer>, state :: <object>);
  object-hash (key);
end method equal-hash;

define method equal-hash (key :: <type>)
          => (id :: <integer>, state :: <object>);
  object-hash (key);
end method equal-hash;

define method equal-hash (key :: singleton (#f))
          => (id :: <integer>, state :: <object>);
  object-hash (key);
end method equal-hash;

define method equal-hash (key :: singleton (#t))
          => (id :: <integer>, state :: <object>);
  object-hash (key);
end method equal-hash;

define method equal-hash (key :: <condition>)
          => (id :: <integer>, state :: <object>);
  object-hash (key);
end method equal-hash;


// key-hash for reals. Doesn't look like Mindy has good support for
// reals.

// define method equal-hash (key :: <real>)
//           => (id :: <integer>, state :: <object>);
//   equal-hash (ht, truncate (abs (key)));
// end method equal-hash;



define method equal-hash (col :: <collection>)
          => (id :: <integer>, state :: <object>);
  collection-hash(col, equal-hash, equal-hash);
end method equal-hash;

/* ---------------- */

// You can't write a more specific method on collections because 
// any two collections with identical key/element pairs are equal. 
// Because of this, you can't merge-hash-codes with ordered: #t, or
// really anything else interesting. In partial compensation, this
// method hashes the keys as well as the elements. (As long as you
// always put the element before the key when you merge hash codes,
// you *can* use ordered: #t for merging them)

define method collection-hash(col :: <collection>, key-hash :: <function>,
			      element-hash :: <function>)
          => (id :: <integer>, state :: <object>);
  let (current-id, current-state) = values (0, $permanent-hash-state);

  for (elt keyed-by key in col)
    let (elt-id, elt-state)           = element-hash (elt);
    let (key-id, key-state)           = key-hash (key);

    let (captured-id1, captured-state1) = merge-hash-codes (elt-id, elt-state,
							    key-id, key-state,
							    ordered: #t);

    let (captured-id2, captured-state2) = merge-hash-codes (current-id, 
							    current-state, 
							    captured-id1,
							    captured-state1,
							    ordered: #f);

    current-id    := captured-id2;
    current-state := captured-state2;
  end for;

  values (current-id, current-state);
end method collection-hash;

/* ---------------- */

// This is similar to an equal-hash, except that it hashes things with
// ordered: #t and ignores the sequence keys. USE WITH CAUTION: This
// isn't a proper equal-hash because two collections of different types
// but identical key/element pairs won't generate the same hash id,
// even though the two collections are =.

define method sequence-hash(seq :: <sequence>, element-hash :: <function>)
          => (id :: <integer>, state :: <object>);
  let (current-id, current-state) = values (0, $permanent-hash-state);

  for (elt in seq)
    let (id, state) = element-hash (elt);

    let (captured-id, captured-state) = merge-hash-codes (current-id, 
							  current-state, 
							  id, state,
							  ordered: #t);

    current-id    := captured-id;
    current-state := captured-state;
  end for;

  values (current-id, current-state);
end method sequence-hash;

/* ---------------- */

// A convenient method for hashing strings. Calls sequence-hash 
// and "does the right thing."

define method string-hash (s :: <string>)
    => (id :: <integer>, state :: <object>);
  sequence-hash(s, object-hash);
end method string-hash;

/* ---------------- */

define method table-protocol(ht :: <object-table>) 
         => (key-test :: <function>, key-hash :: <function>);
  values(\==, object-hash);
end method table-protocol;


define method table-protocol(ht :: <equal-table>) 
         => (key-test :: <function>, key-hash :: <function>);
  values(\=, equal-hash);
end method table-protocol;


define method table-protocol(ht :: <value-table>) 
         => (key-test :: <function>, key-hash :: <function>);
  values(ht.key-test-slot, ht.key-hash-slot);
end method table-protocol;

/* ---------------- */

// Informally, two hash tables are = if they use the same key test,
// have the same size, and all the elements in the first hash table
// have matching elements in the second hash table.

define constant not-in-ht2 = "not-in-ht2";

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

/* ---------------- */

define method find-elt (list :: <list>, test :: <function>,
			#key default: default = #f )
	// Returns the first element of the list that satisfies
	// test.

  if ( empty? (list) )
    default;
  else
    if ( test (head (list)) )
      head (list);
    else
      find-elt ( tail (list), test, default: default);
    end if;
  end if;
end method find-elt;

/* ---------------- */

// This function looks redundant at times, but it's necessary in order
// to avoid race conditions with the garbage collector.

define method element (  ht :: <table>, key, 
		         #key default: default = no-default )
  until (state-valid? (ht.merged-hash-state-slot))
    rehash (ht);
  end until;

  let (key=, key-hash)      = table-protocol(ht);

  let (key-id, key-state)   = key-hash (key);
  let bucket-index          = modulo (key-id, ht.bucket-count-slot);
  let bucket                = ht.bucket-array-slot [bucket-index];
	
  let test = method (entry :: <bucket-entry>)
	       (entry.hash-id-slot = key-id)
		 & key= (entry.key-slot, key);
	     end method;

  let find-result = find-elt (bucket, test);
  
     // Check to see if there was a garbage collection in the middle
     // of this method. If there was, start over.

  if (~ state-valid? (ht.merged-hash-state-slot)
      | ~ state-valid? (key-state) )
    element (ht, key, default: default);
       
    // Else, there was no garbage collection, and we're safe.
  elseif ( find-result )
    find-result.item-slot;
  elseif (default == no-default)
    error ("Element not found");
  else 
    default;
  end if;
end method element;


// This is exactly the same code without the garbage collection stuff

define method element (  ht :: <value-table>, key, 
		         #key default: default = no-default )

  let (key=, key-hash)      = table-protocol(ht);

  let key-id                = key-hash (key);
  let bucket-index          = modulo (key-id, ht.bucket-count-slot);
  let bucket                = ht.bucket-array-slot [bucket-index];
	
  let test = method (entry :: <bucket-entry>)
	       (entry.hash-id-slot = key-id)
		 & key= (entry.key-slot, key);
	     end method;

  let find-result = find-elt (bucket, test);
  
  if ( find-result )
    find-result.item-slot;
  elseif (default == no-default)
    error ("Element not found");
  else 
    default;
  end if;
end method element;

/* ---------------- */

// This function looks redundant at times, but it's necessary in order
// to avoid race conditions with the garbage collector.

define method element-setter (value, ht :: <table>, key)

  until (state-valid? (ht.merged-hash-state-slot))
    rehash (ht);
  end until;

  let (key=, key-hash)    = table-protocol(ht);

  let (key-id, key-state) = key-hash (key);
  let bucket-index        = modulo (key-id, ht.bucket-count-slot);
  
  let test-method         = method (existing-item :: <bucket-entry>)
			      (existing-item.hash-id-slot = key-id)
				& key=(existing-item.key-slot, key);
			    end method;

  let bucket-entry        = find-elt (ht.bucket-array-slot [bucket-index],
				      test-method);

     // Check to see if there was a garbage collection in the middle
     // of this method. If there was, start over.

  if (~ state-valid? (ht.merged-hash-state-slot)
      | ~ state-valid? (key-state) )
    element-setter (value, ht, key);
       
             // Else, there was no garbage collection, and we're safe.
             // (If there is a garbage collection between now and the
             // the end of this method, it invalidates the states we're
             // about to write, but we can just re-compute them on
             // the next lookup)

  else

    if (bucket-entry = #f)             // If item didn't exist, add it
      bucket-entry := make-bucket-entry (key, key-id, key-state, value);

      ht.bucket-array-slot [bucket-index] := 
	     pair (bucket-entry, ht.bucket-array-slot [bucket-index]);
      ht.item-count-slot := ht.item-count-slot + 1;

      if (size (ht) * 100 > (ht.bucket-count-slot * ht.expand-when-slot))
	resize-table (ht, truncate/ (size(ht) * ht.expand-to-slot, 100) + 1);
      end if;
    else     // Item WAS found
      bucket-entry.key-slot        := key;
      bucket-entry.hash-id-slot    := key-id;
      bucket-entry.hash-state-slot := key-state;
      bucket-entry.item-slot       := value;
    end if;

          // Update bucket's merged-hash-state
    ht.bucket-states-slot [bucket-index] := 
	         merge-hash-states (bucket-entry.hash-state-slot, 
				    ht.bucket-states-slot [bucket-index]);

    // Update table's merged hash codes
    ht.merged-hash-state-slot := 
      merge-hash-states (bucket-entry.hash-state-slot, 
			 ht.merged-hash-state-slot);
    value;
  end if;
end method element-setter;


// This is exactly the same code without the garbage collection stuff

define method element-setter (value, ht :: <value-table>, key)
  let (key=, key-hash)    = table-protocol(ht);

  let key-id              = key-hash (key);
  let bucket-index        = modulo (key-id, ht.bucket-count-slot);
  
  let test-method         = method (existing-item :: <bucket-entry>)
			      (existing-item.hash-id-slot = key-id)
				& key=(existing-item.key-slot, key);
			    end method;

  let bucket-entry        = find-elt (ht.bucket-array-slot [bucket-index],
				      test-method);

  if (bucket-entry = #f)             // If item didn't exist, add it
    bucket-entry := make-bucket-entry (key, key-id,
				       $permanent-hash-state, 
				       value);
    
    ht.bucket-array-slot [bucket-index] := 
           pair (bucket-entry, ht.bucket-array-slot [bucket-index]);
    ht.item-count-slot := ht.item-count-slot + 1;

    if (size (ht) * 100 > (ht.bucket-count-slot * ht.expand-when-slot))
      resize-table (ht, truncate/ (size(ht) * ht.expand-to-slot, 100) + 1);
    end if;
  else     // Item WAS found
    bucket-entry.key-slot        := key;
    bucket-entry.hash-id-slot    := key-id;
    bucket-entry.item-slot       := value;
  end if;

  value;
end method element-setter;

/* ---------------- */

define method remove-key! (ht :: <table>, key) => new-ht :: <table>;

  until (state-valid? (ht.merged-hash-state-slot))
    rehash (ht);
  end until;

  let (key=, key-hash)      = table-protocol(ht);

  let (key-id, key-state)   = key-hash (key);
  let bucket-index          = modulo (key-id, ht.bucket-count-slot);
  let bucket                = ht.bucket-array-slot [bucket-index];

  let test = method (existing-item :: <bucket-entry>)
	       (existing-item.hash-id-slot = key-id)
		 & key= (existing-item.key-slot, key);
	     end method;

  let the-item = find-elt (bucket, test);

  if (~ state-valid? (ht.merged-hash-state-slot)
      | ~ state-valid? (key-state) )
    remove-key! (ht, key);    // If state not valid, goto beginning
			      // for a rehash
  else
    if (the-item ~= #f)       // An item with that key was found
	ht.item-count-slot := ht.item-count-slot - 1;

           // Between find-elt and remove!, this traverses the bucket
           // twice. It could be improved, but one has to be careful 
           // to avoid race conditions with the garbage collector.

	ht.bucket-array-slot [bucket-index] := remove! (bucket, the-item);

        if (size (ht) * 100 < (ht.bucket-count-slot * ht.shrink-when-slot))
	  resize-table (ht, truncate/ (size(ht) * ht.shrink-to-slot, 100) + 1);
	end if;

      // We leave all the merged-states as is. rehash will take care of it
      // if a remove-key! made the merged-state information overly cautious.

    end if; // had to remove something

    ht;
  end if;   // states valid?
end method remove-key!;


// This is exactly the same code without the garbage collection stuff

define method remove-key! (ht :: <value-table>, key) => new-ht :: <table>;
  let (key=, key-hash)      = table-protocol(ht);

  let key-id                = key-hash (key);
  let bucket-index          = modulo (key-id, ht.bucket-count-slot);
  let bucket                = ht.bucket-array-slot [bucket-index];

  let test = method (existing-item :: <bucket-entry>)
	       (existing-item.hash-id-slot = key-id)
		 & key= (existing-item.key-slot, key);
	     end method;

  let the-item = find-elt (bucket, test);

  if (the-item ~= #f)       // An item with that key was found
    ht.item-count-slot := ht.item-count-slot - 1;

           // Between find-elt and remove!, this traverses the bucket
           // twice. It could be improved.

    ht.bucket-array-slot [bucket-index] := remove! (bucket, the-item);

    if (size (ht) * 100 < (ht.bucket-count-slot * ht.shrink-when-slot))
      resize-table (ht, truncate/ (size(ht) * ht.shrink-to-slot, 100) + 1);
    end if;
  end if; // had to remove something

  ht;
end method remove-key!;

/* ---------------- */

// Takes a hashtable and mutates it so that it has a different number of
// buckets.

define method resize-table (ht :: <table>, numbuckets :: <integer>)
  let new-array = make (<simple-object-vector>, 
			size: numbuckets,
			fill: #()   );

  for (bucket in ht.bucket-array-slot)
    for (entry in bucket)
      let index = modulo (entry.hash-id-slot, numbuckets);
      new-array [index] := pair (entry, new-array [index]);
    end for;
  end for;

  ht.bucket-array-slot := new-array;
  ht.bucket-count-slot := numbuckets;
end method resize-table;

/* ---------------- */

// Rehash does its best to bring a table up to date so that all the
// hash-id's in the table are valid. Rehash makes no guarentees about
// its success, however, so one should call it inside an until loop
// to make sure it keeps trying until it succeeds.

// Rehash wants to get the merged-hash-states to be as accurate as 
// possible without sacraficing too much performance. This might be a
// good function to tune.

define method rehash (ht :: <table>) => rehashed-ht :: <table>;
  let (key=, key-hash)  =  table-protocol(ht);

  for (i from 0 below ht.bucket-count-slot)

    if (~ state-valid? (ht.bucket-states-slot [i]))     // rehash bucket
      ht.bucket-states-slot [i] := $permanent-hash-state;

      let bucket    = ht.bucket-array-slot [i];
      let prev      = #f;
      let remaining = bucket;
      
             // This until is just like remove!, except that it
	     // rehashes things
      until ( remaining == #() )
	let bucket-entry = head (remaining);
	let index        = i;

	if (state-valid? (bucket-entry.hash-state-slot))
	  prev        := remaining;
	  remaining   := tail (remaining);

	else  // state is invalid

	  let (id, state) = key-hash (bucket-entry.key-slot);  
	  bucket-entry.hash-id-slot    := id;
	  bucket-entry.hash-state-slot := state;

	  index := modulo (id, ht.bucket-count-slot);

	  if (index = i)          // Keep its place in the list
	    prev := remaining;
	    remaining := tail (remaining);
	  else                    // Move entry
	    ht.bucket-array-slot [index] := 
	         pair (bucket-entry, ht.bucket-array-slot [index]);

	          // Now remove it from old bucket
	    if (prev)
	      tail (prev) := tail (remaining);
	      remaining   := tail (remaining);
	    else
	      bucket      := tail (remaining);
	      prev        := #f;
	      remaining   := tail (remaining);
	    end if;  // If prev
	  end if;    // If index = i
	end if;      // If state-valid? (bucket-entry)

	ht.bucket-array-slot [i] := bucket;
	ht.bucket-states-slot [index] := 
	          merge-hash-states (bucket-entry.hash-state-slot,
				     ht.bucket-states-slot [index]);

      end until;    // Finished traversing the bucket
    end if;         // state-valid? (bucket-id-slots)
  end for;

  ht.merged-hash-state-slot := reduce (merge-hash-states,
				       $permanent-hash-state,
				       ht.bucket-states-slot);
  ht;
end method rehash;

/* ---------------- */

define method size (ht :: <table>)
  ht.item-count-slot;
end method size;

/* ---------------- */

define method empty? (ht :: <table>)
  ht.item-count-slot = 0;
end method empty?;

/* ---------------- */

// Inherit mapping functions

/* -------------------------------------------------------------------
 *                Iteration protocol stuff
 * ------------------------------------------------------------------- */

// All these things are needed in the state, because many of the functions
// get nothing but a hash table and a state.


// This is the iteration state, not a hash-state

define class <ntable-state> (<object>)
  slot elements-touched-slot,         init-keyword: elements-touched:      ;

  slot array-state-slot,              init-keyword: array-state:           ;
  slot array-limit-slot,              init-keyword: array-limit:           ;
  slot array-next-state-slot,         init-keyword: array-next-state:      ;
  slot array-finished-state?-slot,    init-keyword: array-finished-state?: ;
  slot array-current-key-slot,        init-keyword: array-current-key:     ;
  slot array-current-element-slot,    init-keyword: array-current-element: ;
  slot array-current-element-setter-slot,   
                          init-keyword: array-current-element-setter:      ;
  slot array-copy-state-slot,         init-keyword: array-copy-state:      ;

  slot bucket-state-slot,             init-keyword: bucket-state:          ;
  slot bucket-limit-slot,             init-keyword: bucket-limit:          ;
  slot bucket-next-state-slot,        init-keyword: bucket-next-state:     ;
  slot bucket-finished-state?-slot,   init-keyword: bucket-finished-state?:;
  slot bucket-current-key-slot,       init-keyword: bucket-current-key:    ;
  slot bucket-current-element-slot,   init-keyword: bucket-current-element:;
  slot bucket-current-element-setter-slot,       
                           init-keyword: bucket-current-element-setter:    ;
  slot bucket-copy-state-slot,        init-keyword: bucket-copy-state:     ;

end class <ntable-state>;

/* ---------------- */

define method finished-table-state? (ht :: <table>,
				     state :: <ntable-state>,
				     limit)
  state.elements-touched-slot >= ht.item-count-slot;
end method finished-table-state?;

/* ---------------- */

define method next-table-state (ht    :: <table>,
			       state :: <ntable-state>) 
               => new-state :: <ntable-state>;

  state.elements-touched-slot := state.elements-touched-slot + 1;

  if ( ~ finished-table-state? (ht, state, #f) )
    let bucket = state.array-current-element-slot (ht.bucket-array-slot,
						   state.array-state-slot);

    state.bucket-state-slot := 
                state.bucket-next-state-slot (bucket, state.bucket-state-slot);

    if (state.bucket-finished-state?-slot  (bucket,
					    state.bucket-state-slot,
					    state.bucket-limit-slot))
      // Then move on to the next bucket
      state.array-state-slot := 
                       state.array-next-state-slot (ht.bucket-array-slot,
						    state.array-state-slot);

      bucket := state.array-current-element-slot (ht.bucket-array-slot,
						  state.array-state-slot);

      while (empty? (bucket))
	state.array-state-slot := 
	             state.array-next-state-slot (ht.bucket-array-slot,
						  state.array-state-slot);

	bucket := state.array-current-element-slot (ht.bucket-array-slot,
						    state.array-state-slot);
      end while;

      let (next-bucket-initial-state,
	   next-bucket-limit,
	   next-bucket-next-state,
	   next-bucket-finished-state?,
	   next-bucket-current-key,
	   next-bucket-current-element,
	   next-bucket-current-element-setter,
	   next-bucket-copy-state) = 
                      forward-iteration-protocol (bucket);

      state.bucket-state-slot                  := next-bucket-initial-state;
      state.bucket-limit-slot                  := next-bucket-limit;
      state.bucket-next-state-slot             := next-bucket-next-state;
      state.bucket-finished-state?-slot        := next-bucket-finished-state?;
      state.bucket-current-key-slot            := next-bucket-current-key;
      state.bucket-current-element-slot        := next-bucket-current-element;
      state.bucket-current-element-setter-slot :=
                            next-bucket-current-element-setter;
      state.bucket-copy-state-slot             := next-bucket-copy-state;    
    end if;           // End of things to do if bucket ran dry
  end if;             // End of more objects left in hash table?

  state;            // Return the new and improved state object
end method next-table-state;

/* ---------------- */

define method get-bucket-entry (ht :: <table>, state :: <ntable-state>)
                  => entry :: <bucket-entry>;

  let bucket = state.array-current-element-slot (ht.bucket-array-slot,
						 state.array-state-slot);

  state.bucket-current-element-slot (bucket, state.bucket-state-slot);
end method get-bucket-entry;

/* ---------------- */

define method current-table-key (ht :: <table>, state :: <ntable-state>)
  let bucket-entry = get-bucket-entry (ht, state);

  bucket-entry.key-slot;
end method current-table-key;

/* ---------------- */

define method current-table-element (ht :: <table>, state :: <ntable-state>)
  let bucket-entry = get-bucket-entry (ht, state);

  bucket-entry.item-slot;
end method current-table-element;

/* ---------------- */

define method current-table-element-setter (value,
					   ht    :: <table>,
					   state :: <ntable-state>)
         // This argument order isn't mentioned anywhere I can find,
         // but seems to be what is expected

  let bucket = state.array-current-element-slot (ht.bucket-array-slot,
						 state.array-state-slot);
  let new-bucket-entry = get-bucket-entry (ht, state);

  new-bucket-entry.item-slot := value;
  state.bucket-current-element-setter-slot (new-bucket-entry,
					    bucket,
					    state.bucket-state-slot);
  // Return value:
  value;
end method current-table-element-setter;

/* ---------------- */

define method copy-table-state (ht :: <table>, old-state :: <ntable-state>)
  let bucket    = old-state.array-current-element-slot (ht.bucket-array-slot,
						  old-state.array-state-slot);
  let new-state = make (<ntable-state>);

  new-state.array-state-slot  :=
    old-state.array-copy-state-slot (ht.bucket-array-slot,
				     old-state.array-state-slot);

  new-state.bucket-state-slot := 
    old-state.bucket-copy-state-slot (bucket, old-state.bucket-state-slot);

  new-state.array-next-state-slot      := old-state.array-next-state-slot;
  new-state.array-copy-state-slot      := old-state.array-copy-state-slot;
  new-state.array-current-key-slot     := old-state.array-current-key-slot;
  new-state.array-finished-state?-slot :=
             old-state.array-finished-state?-slot;
  new-state.array-current-element-slot := 
             old-state.array-current-element-slot;
  new-state.array-current-element-setter-slot :=
             old-state.array-current-element-setter-slot;

  new-state.bucket-next-state-slot      := old-state.bucket-next-state-slot;
  new-state.bucket-copy-state-slot      := old-state.bucket-copy-state-slot;
  new-state.bucket-current-key-slot     := old-state.bucket-current-key-slot;
  new-state.bucket-finished-state?-slot :=
             old-state.bucket-finished-state?-slot;
  new-state.bucket-current-element-slot := 
             old-state.bucket-current-element-slot;
  new-state.bucket-current-element-setter-slot :=
             old-state.bucket-current-element-setter-slot;


  new-state;
end method copy-table-state;

/* ---------------- */

define method make-table-state (ht :: <table>) 
               => table-state :: <ntable-state>;

  let (array-initial-state,
       array-limit,
       array-next-state,
       array-finished-state?,
       array-current-key,
       array-current-element,
       array-current-element-setter,
       array-copy-state) = forward-iteration-protocol (ht.bucket-array-slot);

  let init-state = make (<ntable-state>);

  init-state.elements-touched-slot :=             0;

  init-state.array-state-slot :=                  array-initial-state;
  init-state.array-limit-slot :=                  array-limit;
  init-state.array-next-state-slot :=             array-next-state;
  init-state.array-finished-state?-slot :=        array-finished-state?;
  init-state.array-current-key-slot :=            array-current-key;
  init-state.array-current-element-slot :=        array-current-element;
  init-state.array-current-element-setter-slot := array-current-element-setter;
  init-state.array-copy-state-slot :=             array-copy-state;

  if (ht.item-count-slot > 0)
    let bucket = init-state.array-current-element-slot (ht.bucket-array-slot,
			  	                init-state.array-state-slot);

    while (empty?(bucket))             // Find first non-empty bucket
      init-state.array-state-slot := 
	init-state.array-next-state-slot (ht.bucket-array-slot,
					  init-state.array-state-slot);
      
      bucket := init-state.array-current-element-slot (ht.bucket-array-slot,
						init-state.array-state-slot);
    end while;

          // In the case that the hash table is empty, the bucket states
          // are neither initialized nor needed.

    let (first-bucket-initial-state,
	 first-bucket-limit,
	 first-bucket-next-state,
	 first-bucket-finished-state?,
	 first-bucket-current-key,
	 first-bucket-current-element,
	 first-bucket-current-element-setter,
	 first-bucket-copy-state) = 
                    forward-iteration-protocol (bucket);

    init-state.bucket-state-slot :=              first-bucket-initial-state;
    init-state.bucket-limit-slot :=              first-bucket-limit;
    init-state.bucket-next-state-slot :=         first-bucket-next-state;
    init-state.bucket-finished-state?-slot :=    first-bucket-finished-state?;
    init-state.bucket-current-key-slot :=        first-bucket-current-key;
    init-state.bucket-current-element-slot :=    first-bucket-current-element;
    init-state.bucket-current-element-setter-slot := 
                                         first-bucket-current-element-setter;
    init-state.bucket-copy-state-slot :=         first-bucket-copy-state;
  end if;

  init-state;                        // Return value
end method make-table-state;

/* ---------------- */

// The iteration protocol.

define method forward-iteration-protocol (ht :: <table>)

  values (make-table-state (ht),       // initial hash state
	  #f,             // limit -- isn't actually used by finished-state?
	  next-table-state,
	  finished-table-state?,
	  current-table-key,
	  current-table-element,
	  current-table-element-setter,
	  copy-table-state);
end method forward-iteration-protocol;
