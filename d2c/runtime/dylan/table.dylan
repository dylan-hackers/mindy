module:	    dylan-viscera
Author:	    Nick Kramer (nkramer@cs.cmu.edu)
rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/table.dylan,v 1.11 2002/08/30 09:18:34 bruce Exp $
Synopsis:   Implements <table>, <object-table>, <equal-table>,
            and <value-table>.

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
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

// Design notes 2001/07/07
//
// 
// <table>, <object-table>, <equal-table>
// <string-table> <case-insensitive-string-table>
// 
// element, element-setter, remove-element!
// 
// <integer>, <character>, <single-float>, <double-float>, <symbol>,
// <byte-string>, <unicode-string>
//      
// Compute the hash integer :: depends on table type and key type  (28)
// calculate the bucket number
// check each node in turn
//    key check :: depends on table type (for key-test) and key type (28)
// perform the operation :: depends on operation (3)
//
// I'll combine the first four steps into a single function, find-table-element(),
// which will take the table and key and return a <table-item>.  If the
// key was not found then the return value will be $key-not-found (which
// is a global unused bucket entry).  find-table-element() will also set a
// variable in the table to indicate which bucket chain was searched
// (used by remove-key!). find-table-element() moves the located element to
// the start of its chain.
// 
// All that remains is to perform the actual operation.
// 
// find-table-element() is a generic function created with a macro.  Each of
// the instances is optimal within itself and is called either directly 
// (if the table type and key type are statically known) or else via the
// GF.  Either way, the table and key types are discovered early.

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
    //
    // In our built-in stuff, the accumulator is always the first argument, so
    // we'll bias (slightly) towards that one
    //
    // Merging two items, anything with up to 13 bits of varying is optimally spread
    // (and 16 bit data is spread to 2^29 distinct hash codes)
    //
    // Merging a series of data items (e.g. a string), ASCII characters are well
    // spread.
    //
    // e.g. 7 bit ASCII "abcdef"
    //
    // bbbbbb000000000000000000aaaaaaab
    // cccccc00000000000aaaaaaabbbbbbbc
    // dddddd0000aaaaaaabbbbbbbcccccccd
    // eeeXXXaaaabbbbbbbcccccccddddddde  X = mixture
    // XXXXXXbbbbcccccccdddddddeeeeeXXX
    // etc...
    //
    logxor(ash(id1, 7), ash(id1, 7 - *word-bits*),
           ash(id2, -6), ash(id2, *word-bits* - 6));
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
  // If we get a non-moving garbage collector, we will have to return
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

define constant <maybe-table-item> = false-or(<table-item>);


define class <table-item> (<object>)
  slot entry-key :: <object>, required-init-keyword: #"key";
  slot entry-elt :: <object>, required-init-keyword: #"item";
  slot entry-hash-id :: <integer>, required-init-keyword: #"hash-id";
  slot entry-next :: <maybe-table-item> = #f, init-keyword: #"next";
end class <table-item>;

define sealed domain make (singleton(<table-item>));
define sealed domain initialize (<table-item>);


define class <entry-vector> (<vector>)
  sealed slot %element :: <maybe-table-item>,
    init-value: #f, init-keyword: fill:,
    sizer: size, size-init-value: 0, size-init-keyword: size:;
end class <entry-vector>;

define sealed domain make(singleton(<entry-vector>));
define sealed domain initialize(<entry-vector>);


define sealed inline method element
    (vec :: <entry-vector>, index :: <integer>, #key default = $not-supplied)
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
    (new-value :: <maybe-table-item>, vec :: <entry-vector>,
     index :: <integer>)
 => new-value :: <maybe-table-item>;
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
  slot lower-resize-trigger :: <integer> = 0;
  slot upper-resize-trigger :: <integer> = 0;

  // Each bucket is a chained sequence of <table-item>s
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

define sealed method make
    (c == <table>, #rest key-value-pairs, #key, #all-keys)
 => table :: <simple-object-table>;
  apply(make, <simple-object-table>, key-value-pairs);
end method make;

define sealed method make
    (c == <object-table>, #rest key-value-pairs, #key, #all-keys)
 => table :: <simple-object-table>;
  apply(make, <simple-object-table>, key-value-pairs);
end method make;

define method initialize
    (ht :: <table>, #next next-method, #key size: sz = 0);
  next-method();
  resize-table(ht, sz);
end method initialize;


define inline method size (ht :: <table>) => result :: <integer>;
  ht.table-size;
end method size;

define inline method empty? (ht :: <table>) => result :: <boolean>;
  ht.table-size = 0;
end method empty?;

define sealed domain size(<table>);
define sealed domain empty?(<table>);


define inline method key-test (ht :: <table>) => test :: <function>;
  table-protocol(ht);    // drop the second return value
end method key-test;

define sealed generic object-hash
    (key :: <object>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);

define inline method object-hash
    (key :: <object>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  let (id, state) = pointer-hash(key);
  values(id, merge-hash-states(initial-state, state));
end method object-hash;

define constant $really-big-prime = 1073741789;

define inline method object-hash
    (key :: <integer>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  values(key, initial-state);
end;

define method object-hash
    (key :: <extended-integer>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  values(as(<integer>, modulo(key, $really-big-prime)), initial-state);
end method object-hash;

define method object-hash
    (key :: <ratio>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  values
    (merge-hash-ids(as(<integer>, modulo(key.numerator, $really-big-prime)),
                    as(<integer>, modulo(key.denominator, $really-big-prime)),
                    ordered: #t),
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
  // maybe it's just me, but i don't want characters having
  // the exact same hash code as their ASCII code
  values(lognot(as(<integer>, key)), initial-state);
end method object-hash;

// equal-hash is used in the table-protocol as the hash-function 
// for equal tables. Calling convention is similar to object-hash.
//
// The default method for objects that don't have any 
// better methods defined. (We can't call object-hash, so what can we do?)

define function must-define-equal-hash(obj) => ()
  error(concatenate("Must define equal-hash(", obj.object-class.class-name, ")"));
end function;

define inline method equal-hash
    (key :: <object>, initial-state :: <hash-state>) 
 => (id :: <integer>, state :: <hash-state>);
  must-define-equal-hash(key);
  values(42, initial-state);
end method equal-hash;

// Call object-hash for characters, integers, symbols, classes,
// functions, and conditions.
//
define inline method equal-hash
    (key :: <character>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  object-hash(key, initial-state);
end method equal-hash;

define inline method equal-hash
    (key :: <general-integer>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  object-hash(key, initial-state);
end method equal-hash;

define inline method equal-hash
    (key :: <float>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  float-hash(key, initial-state);
end method equal-hash;

define inline method equal-hash
    (key :: <symbol>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  object-hash(key, initial-state);
end method equal-hash;

define inline method equal-hash
    (key :: <class>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  object-hash(key, initial-state);
end method equal-hash;

define inline method equal-hash
    (key :: <function>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  object-hash(key, initial-state);
end method equal-hash;

define inline method equal-hash
    (key :: <type>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  object-hash(key, initial-state);
end method equal-hash;

define inline method equal-hash
    (key :: singleton (#f), initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  object-hash(key, initial-state);
end method equal-hash;

define inline method equal-hash
    (key :: singleton (#t), initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  object-hash(key, initial-state);
end method equal-hash;

define inline method equal-hash
    (key :: <condition>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  object-hash(key, initial-state);
end method equal-hash;

define inline method equal-hash
    (col :: <collection>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  collection-hash(equal-hash, equal-hash, col, initial-state);
end method equal-hash;

define inline sealed method value-hash
    (key :: <general-integer>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  object-hash(key, initial-state);
end method value-hash;

define inline sealed method value-hash
    (key :: <float>, initial-state :: <hash-state>)
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

define inline sealed method value-hash
    (key == #f, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  values(0, initial-state);
end method value-hash;

define inline sealed method value-hash
    (key == #t, initial-state :: <hash-state>)
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
define function sequence-hash
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
// return it.  Otherwise, return #f.  Save the winning hash-id and
// bucket index in case our caller wants it (element-setter and
// remove-key! do) -- and we might eventually try using it to avoid a
// key-hash next time as well.
//


//////////////////////////////// element ///////////////////////////////

define function find-for-element
    (start-at :: <maybe-table-item>, ht :: <table>, key-id :: <integer>)
 => (item :: <maybe-table-item>)
  let bucket = modulo(key-id, ht.buckets.size);
  local
    method try(item)
      case
        ~item => #f;
        item.entry-hash-id == key-id => item;
        otherwise => try(item.entry-next);
      end
    end;
  try(if (start-at) start-at.entry-next else ht.buckets[bucket] end);
end find-for-element;


define inline sealed method element
    (ht :: <table>, key, #key default: default = $not-supplied)
 => (result :: <object>);
  let (key=, key-hash) = table-protocol(ht);
  let key-id :: <integer> = key-hash(key, $permanent-hash-state);
  local
    method loop(item :: <maybe-table-item>)
      let item = find-for-element(item, ht, key-id);
      case
        ~item    // the if() will collapse at compile-time
          => if (default == $not-supplied)
               table-element-not-found-error();
             else 
               default;
             end if;
        key=(item.entry-key, key)
          => item.entry-elt;
        otherwise
          => loop(item);
      end case;
    end;

  loop(#f);
end method element;

//////////////////////////// element-setter ////////////////////////////

define function find-for-element-setter
    (start-at :: <maybe-table-item>, ht :: <table>, key-id :: <integer>, key, value)
 => (item :: <maybe-table-item>)
  let bucket = modulo(key-id, ht.buckets.size);
  local
    method try(item)
      case
        ~item =>
          // insert a new item
          ht.buckets[bucket]
            := make(<table-item>, key: key, item: value, hash-id: key-id, 
                    next: ht.buckets[bucket]);
          let new-size = ht.table-size + 1;
          ht.table-size := new-size;
          if (new-size > ht.upper-resize-trigger)
            resize-table(ht, new-size);
          end;
          #f;
        item.entry-hash-id == key-id => item;
        otherwise => try(item.entry-next);
      end
    end;

  try(if (start-at) start-at.entry-next else ht.buckets[bucket] end);
end find-for-element-setter;


define inline sealed method element-setter
    (value, ht :: <table>, key) 
 => value;
  let (key=, key-hash) = table-protocol(ht);
  let key-id :: <integer> = key-hash(key, $permanent-hash-state);
  local
    method loop(item :: <maybe-table-item>)
      let item = find-for-element-setter(item, ht, key-id, key, value);
      case
        ~item                     => value;
        key=(item.entry-key, key) => item.entry-elt := value;
        otherwise                 => loop(item);
      end case;
    end;

  loop(#f);
end method element-setter;


///////////////////////////// remove-key! //////////////////////////////
//
// This one is a bit nasty ... when we find an item with the right hash
// code we take it out immediately.  If it turns out to not be key= then
// we put it back!!

define function find-for-remove
    (start-at :: <maybe-table-item>, ht :: <table>, key-id :: <integer>)
 => (item :: <maybe-table-item>)
  let bucket = modulo(key-id, ht.buckets.size);
  local
    method try(item, prev)
      case
        ~item => #f;
        item.entry-hash-id == key-id => 
          if (prev)
            prev.entry-next := item.entry-next;
          else
            ht.buckets[bucket] := item.entry-next;
          end;
          item;
        otherwise => try(item.entry-next, item);
      end
    end;
  if (start-at)
    // shit!  put erroneously deleted item back!
    local
      method search(prev)
        if(prev.entry-next == start-at.entry-next)
          prev.entry-next := start-at;
        else
          search(prev.entry-next);
        end;
      end;
    let prev = ht.buckets[bucket];
    if (prev)
      search(prev);
    else
      ht.buckets[bucket] := start-at;
    end;
  end;
  try(if (start-at) start-at.entry-next else ht.buckets[bucket] end, start-at);
end find-for-remove;


define inline sealed method remove-key! (ht :: <table>, key)
 => (found :: <boolean>);
  let (key=, key-hash) = table-protocol(ht);
  let key-id :: <integer> = key-hash(key, $permanent-hash-state);
  local
    method loop(item :: <maybe-table-item>)
      let item = find-for-element(item, ht, key-id);
      case
        ~item                     => #f;
        key=(item.entry-key, key) => #t;
        otherwise                 => loop(item);
      end case;
    end;

  loop(#f);
end method remove-key!;


////////////////////////////////////////////////////////////////////////



define function table-element-not-found-error() => ()
  error("Element not found");
end;


// This list stolen shamelessly from the C++ standard library
// Both g++ and CodeWarrior use exactly the same list
define constant $prime-table = 
  #[53,         97,         193,       389,       769,
    1543,       3079,       6151,      12289,     24593,
    49157,      98317,      196613,    393241,    786433,
    1572869,    3145739,    6291469,   12582917,  25165843,
    50331653,   100663319,  201326611, 402653189, 805306457, 
    1610612741]; // too big for signed => 3221225473, 4294967291

define function find-new-size (target :: <integer>)  => (result :: <integer>);
  // linear search is best, because resizing big tables will dwarf the
  // time for this search, while small tables will be quicker than a
  // binary search would be.  CodeWarrior's STL unrolls the loop but
  // that's pathetic overkill
  block (return)
    for (sz :: <integer> in $prime-table)
      if (sz >= target)
        return (sz);
      end;
    end;
  end block;
end function find-new-size;


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
  table.buckets := $empty-entry-vector;
  table.table-size := 0;
  resize-table(table, 0);
  table;
end method remove-all-keys!;


// Takes a hashtable and mutates it so that it has a different number of
// buckets.
//
define function resize-table (ht :: <table>, requested :: <integer>) => ();
  let numbuckets = find-new-size(requested);
  let buckets = ht.buckets;
  if (numbuckets ~= buckets.size)
    let new-array = make(<entry-vector>, size: numbuckets, fill: #f);

    local
      method iter (entry)
        if (entry)
          let next-entry = entry.entry-next;
          let index = modulo(entry.entry-hash-id, numbuckets);
          entry.entry-next := %element(new-array, index);
          %element(new-array, index) := entry;
          iter(next-entry);
        end if;
      end method iter;
    
    for (i from 0 below buckets.size)
      iter(%element(buckets, i));
    end for;
    ht.buckets := new-array;
    ht.lower-resize-trigger := truncate/(numbuckets, 10);
    ht.upper-resize-trigger := numbuckets;
  end;
end function resize-table;


define class <table-iterator> (<object>)
  slot current :: <integer> = 0, init-keyword: #"current";
  slot entries :: <entry-vector>, required-init-keyword: #"entries";
end class <table-iterator>;

define sealed domain make(singleton(<table-iterator>));
define sealed domain initialize(<table-iterator>);

define function make-initial-iteration-state(ht :: <table>)
 => (initial-state :: <table-iterator>)
  let sz :: <integer> = ht.table-size;
  let vec = make(<entry-vector>, size: sz);
  let i :: <integer> = 0;
  for (from-index :: <integer> from 0 below ht.buckets.size)
    for (entry :: <maybe-table-item> = %element(ht.buckets, from-index)
           then entry.entry-next,
         while: entry)
      %element(vec, i) := entry;
      i := i + 1;
    end for;
  end for;
  make(<table-iterator>, entries: vec);
end make-initial-iteration-state;


define inline method forward-iteration-protocol (ht :: <table>)
 => (initial-state :: <table-iterator>,
     limit :: <integer>,
     next-state :: <function>,
     finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>,
     current-element-setter :: <function>,
     copy-state :: <function>);

  values
    (make-initial-iteration-state(ht),
     ht.table-size,
     
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
       %element(entry-list, current-item).entry-key;
     end method,
     
     /* current-table-element */
     method (ht :: <table>, state :: <table-iterator>) => elt :: <object>;
       let current-item = state.current;
       let entry-list = state.entries;
       %element(entry-list, current-item).entry-elt;
     end method,
     
     /* current-table-element-setter */
     method (value :: <object>, ht :: <table>, state :: <table-iterator>)
      => value :: <object>;
       let current-item = state.current;
       let entry-list = state.entries;
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
define inline method string-hash (s :: <string>, initial-state :: <hash-state>)
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
