module: Dylan
author: William Lott (wlott@cs.cmu.edu)
rcs-header: $Header: /scm/cvs/src/mindy/libraries/dylan/coll.dylan,v 1.1 1998/05/03 19:55:20 andreas Exp $

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
//
// This file contains the collection support code that isn't built in.
//


// Collection routines

// Generic function definitions borrowed from the new compiler

define open generic forward-iteration-protocol
    (collection :: <collection>)
 => (initial-state :: <object>,
     limit :: <object>,
     next-state :: <function>,
     finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>,
     current-element-setter :: <function>,
     copy-state :: <function>);

define open generic backward-iteration-protocol
    (collection :: <collection>)
 => (initial-state :: <object>,
     limit :: <object>,
     next-state :: <function>,
     finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>,
     current-element-setter :: <function>,
     copy-state :: <function>);

define open generic key-sequence
    (collection :: <collection>) => (keys :: <sequence>);

define open generic empty?
    (obj :: <object>)
    => res :: <boolean>;

define open generic reduce
    (proc :: <function>, initial-value :: <object>, collection :: <collection>)
    => value :: <object>;

define open generic reduce1
    (proc :: <function>, collection :: <collection>)
    => value :: <object>;

define open generic member?
    (value :: <object>, collection :: <collection>, #key test)
    => res :: <boolean>;

define open generic find-key
    (collection :: <collection>, pred :: <function>, #key skip, failure)
    => key :: <object>;

define open generic replace-elements!
    (target :: <mutable-collection>, pred :: <function>,
     new-value-fn :: <function>, #key count)
    => target :: <mutable-collection>;

define open generic fill!
    (target :: <mutable-collection>, value :: <object>, #key start:, end:)
    => target :: <mutable-collection>;

define open generic key-test
    (collection :: <collection>) => (test-function :: <function>);


// Element and element-setter will be implemented for arrays and vectors, but
// we must define a default method for all collections.
//
define method element(coll :: <collection>, key :: <object>,
		      #key default = $not-supplied) => obj :: <object>;
  let (init-state, limit, next-state, done?,
       current-key, current-element) = forward-iteration-protocol(coll);
  let test = key-test(coll);
  block (return)
    for (state = init-state then next-state(coll, state),
	 until: done?(coll, state, limit))
      if (test(current-key(coll, state), key))
	return(current-element(coll, state));
      end if;
    finally
      if (default == $not-supplied)
	error("No such element in %=: %=", coll, key);
      else 
	default;
      end if;
    end for;
  end block;
end method element;


define method element-setter (new-value :: <object>,
			      collection :: <mutable-collection>,
			      key :: <object>) => new-value :: <object>;
  let (init-state, limit, next-state, done?,
       current-key, current-element,
       current-element-setter) = forward-iteration-protocol(collection);
  let test = key-test(collection);
  block (return)
    for (state = init-state then next-state(collection, state),
	 until: done?(collection, state, limit))
      if (test(current-key(collection, state), key))
	current-element(collection, state) := new-value;
	return();
      end if;
    end for;
    error("No such element in %=: %=", collection, key);
  end block;
end method element-setter;


define method shallow-copy(collection :: <collection>) 
 => new-coll :: <collection>;
  map(identity, collection);
end method shallow-copy;


define method as(cls :: limited(<class>, subclass-of: <collection>),
		 coll :: <collection>, #next next-method)
 => new-version :: <object>;
  case
    instance?(coll, cls) =>
      coll;
    otherwise =>
      map-as(cls, identity, coll);
  end case;
end method as;


// Note: This function depends upon a definition of \= for sequences, which
// will be supplied later in this file.
//
define method \= (a :: <collection>, b :: <collection>) => answer :: <boolean>;
  let a-test = key-test(a);
  let b-test = key-test(b);
  
  a-test == b-test
    & key-sequence(a) = key-sequence(b) 
    & every?(a-test, a, b);
end method \=;


define method size (collection :: <collection>) => size :: <integer>;
  for (count from 0, elem in collection)
  finally
    count;
  end for;
end method size;

define generic type-for-copy (obj :: <object>) => type :: <type>;

define method type-for-copy(collection :: <mutable-collection>)
 => cls :: <class>;
  object-class(collection);
end method type-for-copy;


define method empty? (collection :: <collection>) => answer :: <boolean>;
  let (init, limit, next, done?) = forward-iteration-protocol(collection);
  done?(collection, init, limit);
end method empty?;


// Note: the map methods for arbitrary collections depend upon the iteration
// protocol being defined for "rest args" (i.e. vectors).
//
define method do (proc :: <function>, collection :: <collection>,
		 #rest more-collections)
 => answer :: singleton(#f);
  let test1 = key-test(collection);
  if (~ every?( method (c) test1 == key-test(c); end, more-collections ))
    error("Can't do over collections with different key tests");
  elseif (empty?(more-collections))
    for (elem in collection) 
      proc(elem);
    end for;
  else
    let keys = reduce(rcurry(intersection, test: test1),
		      key-sequence(collection),
		      map(key-sequence, more-collections));
    for (key in keys)
      apply(proc, collection[key],
	    map(rcurry(element, key), more-collections));
    end for;
  end if;
  #f;
end method do;


define method map(proc :: <function>, collection :: <collection>,
		  #rest more-collections) => coll :: <collection>;
  apply(map-as, type-for-copy(collection), proc, collection,
	more-collections);
end method map;


// map-as must be given collections with the same key tests, but the
// output collection apparently doesn't have to have the same key test
// as its inputs.
//
define method map-as(cls :: <class>, proc :: <function>,
		     coll :: <collection>, #rest more-collections)
    => coll :: <collection>;
  let test = key-test(coll);
  case
    ~every?(method (c) key-test(c) == test end, more-collections) =>
      error("Can't map over collections with different key tests");
    size(coll) == #f
      & every?(method (s) size(s) == #f end, more-collections) =>
      error("Map-as not applicable to unbounded collections");
    empty?(more-collections) =>
      let result = make(cls, size: size(coll));
      let (init, limit, next, done?, curkey, curelt)
        = forward-iteration-protocol(coll);
      for (state = init then next(coll, state),
	   until: done?(coll, state, limit))
	result[curkey(coll, state)] := proc(curelt(coll, state));
      end for;
      result;
    otherwise => 
      let keys = reduce(rcurry(intersection, test: test),
			key-sequence(coll),
			map(key-sequence, more-collections));
      let result = make(cls, size: size (keys));
      for (key in keys)
	result[key] := apply(proc, element (coll, key),
			     map(rcurry (element, key),
				 more-collections));
      end for;
      result;
  end case;
end method map-as;


// map-into must be given collections with the same key tests, and the
// destination must have the same key test as the sources.
//
define method map-into(destination :: <mutable-collection>, proc :: <function>,
		       coll :: <collection>, #rest more-collections)
    => coll :: <collection>;
  let test1 = key-test(coll);
  if (~ every?(method (c) test1 == key-test(c); end, more-collections ))
    error("Can't map over collections with different key tests");
  elseif (~ (test1 == key-test(destination)))
    error("Can't map into a collection with a different key test than its sources.");
  elseif (empty?(more-collections))
    let keys = intersection(key-sequence(coll), key-sequence(destination),
			    test: test1);
    for (key in keys)
      destination[key] := proc(coll[key]);
    end for;
    destination;
  else
    let keys = intersection(reduce(rcurry(intersection, test: test1),
				   key-sequence(coll),
				   map(key-sequence, more-collections)),
			    key-sequence(destination), test: test1);
    for (key in keys)
      destination[key] := apply(proc, coll[key],
				map(rcurry(element, key), more-collections));
    end for;
    destination;
  end if;
end method map-into;


define method any?(proc :: <function>, collection :: <collection>,
		   #rest more-collections) => answer :: <object>;
  let test1 = key-test(collection);
  if (~ every?( method (c) test1 == key-test(c); end, more-collections))
    error("Can't do collection alignment over collections with different key tests");
  end if;

  block (return)
    if (empty?(more-collections))
      for (elem in collection)
	let result = proc(elem);
	if (result) return(result) end if;
      end for;
    else 
      let keys = reduce(rcurry(intersection, test: test1),
			key-sequence(collection),
			map(key-sequence, more-collections));
      for (key in keys)
	let result = apply(proc, collection[key],
			   map(rcurry(element, key), more-collections));
	if (result) return(result) end if;
      end for;
    end if;
    #f;
  end block;
end method any?;

// Pick off 1-collection list case for efficency.
define method any?(proc :: <function>, collection :: <list>,
		    #next next-method, #rest more-collections)
 => answer :: <object>;
  if (empty?(more-collections))
    block (punt)
      for (cur = collection then cur.tail, until: cur == #())
        let res = proc(cur.head);
	if (res) punt(res) end;
      end for;
      #f;
    end block;
  else
    next-method();
  end;
end method;


define method every?(proc :: <function>, collection :: <collection>,
		   #rest more-collections) => answer :: <boolean>;
  let test1 = key-test(collection);
  if (~ every?( method (c) test1 == key-test(c); end, more-collections ))
    error("Can't do collection alignment over collections with different key tests");
  end if;

  block (return)
    if (empty?(more-collections))
      for (elem in collection)
	unless (proc(elem)) return(#f) end unless;
      end for;
    else
      let keys = reduce(rcurry(intersection, test: test1),
			key-sequence(collection),
			map(key-sequence, more-collections));
      for (key in keys)
	let result = apply(proc, collection[key],
			   map(rcurry(element, key), more-collections));
	unless (result) return(#f) end unless;
      end for;
    end if;
    #t;
  end block;
end method every?;

// Pick off 1-collection list case for efficency.
define method every?(proc :: <function>, collection :: <list>,
		    #next next-method, #rest more-collections)
 => answer :: <boolean>;
  if (empty?(more-collections))
    block (punt)
      for (cur = collection then cur.tail, until: cur == #())
        unless (proc(cur.head))
	  punt(#f)
	end;
      end for;
      #t;
    end block;
  else
    next-method();
  end;
end method;

define method reduce(proc :: <function>, init-val, collection :: <collection>)
 => answer :: <object>;
  for (value = init-val then proc(value, elem),
       elem in collection)
  finally value;
  end for;
end method reduce;


define method reduce1(proc :: <function>, collection :: <collection>)
 => answer :: <object>;
  let (init-state, limit, next-state, done?,
       current-key, current-element) = forward-iteration-protocol(collection);
  if (done?(collection, init-state, limit)) // empty collection
    error("Reduce1 not defined for empty collections.");
  else 
    for (// The computation of "value" must precede the computation of "state",
	 // since "next-state" may invalidate the current state.
	 value = current-element(collection, init-state)
	   then proc(value, current-element(collection, state)),
	 state = next-state(collection, init-state)
	   then next-state(collection, state),
	 until: done?(collection, state, limit))
    finally value;
    end for;
  end if;
end method reduce1;


define method member?(value :: <object>, collection :: <collection>,
		      #key test = \==) => answer :: <boolean>;
  block (return)
    for (element in collection)
      if (test(value, element)) return(#t) end if;
    end for;
  end block;
end method member?;


define method replace-elements!(collection :: <mutable-collection>,
				predicate :: <function>,
				new-value-fn :: <function>,
				#key count: count) 
 => coll ::<mutable-collection>;
  let (init-state, limit, next-state, done?,
       current-key, current-element,
       current-element-setter) = forward-iteration-protocol(collection);
  for (state = init-state then next-state(collection, state),
       until: done?(collection, state, limit) | count == 0)
    let this-element = current-element(collection, state);
    if (predicate(this-element))
      current-element(collection, state) := new-value-fn(this-element);
      if (count) count := count - 1 end if;
    end if;
  end for;
  collection;
end method replace-elements!;


define method fill!(collection :: <mutable-collection>, value :: <object>,
		    #key start: first, end: last) 
 => coll :: <mutable-collection>;
  // ignore keywords, since they aren't meaningful for arbitrary collections.
  let (init-state, limit, next-state, done?,
       current-key, current-element,
       current-element-setter) = forward-iteration-protocol(collection);
  for (state = init-state then next-state(collection, state),
       until: done?(collection, state, limit))
    current-element(collection, state) := value;
  end for;
  collection;
end method fill!;


define method find-key(collection :: <collection>, proc :: <function>,
		       #key skip, failure = #f)
 => key :: <object>;
  let (init-state, limit, next-state, done?,
       current-key, current-element) = forward-iteration-protocol(collection);
  block (return)
    for (state = init-state then next-state(collection, state),
	 until: done?(collection, state, limit))
      if (proc(current-element(collection, state)))
	if (skip & skip > 0)
	  skip := skip - 1;
	else
	  return(current-key(collection, state));
	end if;
      end if;
    finally failure
    end for;
  end block;
end method find-key;


define method key-sequence(collection :: <collection>) 
 => key-seq :: <sequence>;
  let (init-state, limit, next-state, done?,
       current-key, current-element) = forward-iteration-protocol(collection);
  let result = make(<vector>, size: size(collection));
  for (index from 0,
       state = init-state then next-state(collection, state),
       until: done?(collection, state, limit))
    result[index] := current-key(collection, state);
  end for;
  result;
end method key-sequence;


// Sequence routines.

// Generic function definitions borrowed from the new compiler

define open generic add
    (seq :: <sequence>, new-element :: <object>) => (new-seq :: <sequence>);

define open generic add!
    (seq :: <sequence>, new-element :: <object>)
 => (maybe-new-seq :: <sequence>);

define open generic add-new
    (seq :: <sequence>, new-element :: <object>, #key test)
 => (new-seq :: <sequence>);

define open generic add-new!
    (seq :: <sequence>, new-element :: <object>, #key test)
 => (new-seq :: <sequence>);

define open generic remove
    (seq :: <sequence>, value :: <object>, #key test, count)
    => new-seq :: <sequence>;

define open generic remove!
    (seq :: <sequence>, value :: <object>, #key test, count)
    => maybe-new-seq :: <sequence>;

define open generic choose
    (pred :: <function>, seq :: <sequence>)
    => new-seq :: <sequence>;

define open generic choose-by
    (pred :: <function>, test-seq :: <sequence>, value-seq :: <sequence>)
    => new-seq :: <sequence>;

define open generic intersection
    (seq1 :: <sequence>, seq2 :: <sequence>, #key test)
    => new-seq :: <sequence>;

define open generic union
    (seq1 :: <sequence>, seq2 :: <sequence>, #key test)
    => new-seq :: <sequence>;

define open generic remove-duplicates
    (seq :: <sequence>, #key test)
    => new-seq :: <sequence>;

define open generic remove-duplicates!
    (seq :: <sequence>, #key test)
    => new-seq :: <sequence>;

define open generic copy-sequence
    (source :: <sequence>, #key start, end: finis)
    => new-seq :: <sequence>;

define open generic replace-subsequence!
    (seq :: <sequence>, insert-seq :: <sequence>, #key start, end: finis)
    => result-seq :: <sequence>;

define open generic reverse (seq :: <sequence>) => (new-seq :: <sequence>);

define open generic reverse! (seq :: <sequence>) => (new-seq :: <sequence>);

define open generic sort
    (seq :: <sequence>, #key test, stable)
    => new-seq :: <sequence>;

define open generic sort!
    (seq :: <sequence>, #key test, stable)
    => new-seq :: <sequence>;

define open generic last
    (seq :: <sequence>, #key default) => (value :: <object>);

define open generic last-setter
    (new-value :: <object>, seq :: <mutable-sequence>) => (value :: <object>);

define open generic subsequence-position
    (big :: <sequence>, pattern :: <sequence>, #key test, count)
 => (index :: false-or(<integer>));


define method element(sequence :: <sequence>, key :: <integer>,
		      #key default = $not-supplied) => elt :: <object>;
  block (return)
    for (this-key from 0, elem in sequence)
      if (this-key == key) return(elem) end if;
    finally
      if (default == $not-supplied)
	error("No such element in %=: %=", sequence, key);
      else 
	default;
      end if;
    end for;
  end block;
end method element;


define method element-setter (new-value, sequence :: <mutable-sequence>,
			      key :: <integer>) => new-value :: <object>;
  let (init-state, limit, next-state, done?,
       current-key, current-element,
       current-element-setter) = forward-iteration-protocol(sequence);
  block (return)
    for (this-key from 0,
	 state = init-state then next-state(sequence, state),
	 until: done?(sequence, state, limit))
      if (this-key == key)
	current-element(sequence, state) := new-value;
	return();
      end if;
    end for;
    error("No such element in %=: %=", sequence, key);
  end block;
end method element-setter;


define method \=(a :: <sequence>, b :: <sequence>) => answer :: <boolean>;
  let (a-init, a-limit, a-next, a-done?, a-key, a-elem)
    = forward-iteration-protocol(a);
  let (b-init, b-limit, b-next, b-done?, b-key, b-elem)
    = forward-iteration-protocol(b);
  block (return)
    for (a-state = a-init then a-next(a, a-state),
	 b-state = b-init then b-next(b, b-state),
	 until: a-done?(a, a-state, a-limit) | b-done?(b, b-state, b-limit))
      if (a-elem(a, a-state) ~= b-elem(b, b-state))
	return(#f);
      end if;
    finally
      if (~a-done?(a, a-state, a-limit) | ~b-done?(b, b-state, b-limit))
	return(#f);
      end if;
    end for;
    #t;
  end block;
end method \=;


define method key-test (sequence :: <sequence>) => test :: <function>;
  \==;            // Return the function == (id?)
end method key-test;


define method key-sequence(sequence :: <sequence>) => key-seq :: <range>;
  let s = size (sequence);
  if (s)
    range (from: 0, below: s);
  else
    range (from: 0);
  end if;
end method key-sequence;


define constant aux-map-as =
  method (cls :: <class>, proc :: <function>, #rest seqs)
    let finite-lengths = choose (identity, map (size, seqs));
    let length = apply(min, finite-lengths);
    let result = make(cls, size: length);
    let (init, limit, next, done?, key, elem, elem-setter)
      = forward-iteration-protocol(result);
    let seq-count = size(seqs);
    let states = make(<vector>, size: seq-count);
    let vals = make(<vector>, size: seq-count);
    let nexts = make(<vector>, size: seq-count);
    let elems = make(<vector>, size: seq-count);

    for (pos from 0, seq in seqs)
      let (init, limit, next, done?, key, elem)
	= forward-iteration-protocol(seq);
      states[pos] := init;
      nexts[pos] := next;
      elems[pos] := elem;
    end for;

    for (state = init then next(result, state),
	 until: done?(result, state, limit))
      for (i from 0 below seq-count)
	let (this-seq, this-state) = values(seqs[i], states[i]);
	vals[i] := elems[i](this-seq, this-state);
	states[i] := nexts[i](this-seq, this-state);
      end for;
      elem(result, state) := apply(proc, vals);
    end for;

    result;
  end method;


define method map-as(cls :: <class>, proc :: <function>,
		     sequence :: <sequence>,
		     #next next-method, #rest more-sequences)
 => coll :: <collection>;
  case
    size (sequence) == #f
      & every? (method (s) size (s) == #f end, more-sequences) =>
      error ("MAP-AS not applicable to unbounded sequences");
    empty?(more-sequences) =>
      let result = make(cls, size: size(sequence));
      let (res-init, res-limit, res-next, res-done?, res-key, res-elem,
	   res-elem-setter) = forward-iteration-protocol(result);
      for (element in sequence,
	   res-state = res-init then res-next(result, res-state))
	res-elem(result, res-state) := proc(element);
      end for;
      result;
    every?(rcurry(instance?, <sequence>), more-sequences) =>
      apply(aux-map-as, cls, proc, sequence, more-sequences);
    otherwise =>
      next-method();
  end case;
end method map-as;


define method map-into(destination :: <mutable-sequence>, proc :: <function>,
		       sequence :: <sequence>,
		       #next next-method, #rest more-sequences)
 => destination :: <mutable-sequence>;
  if (empty?(more-sequences))
    let (res-init, res-limit, res-next, res-done?, res-key, res-elem,
	 res-elem-setter) = forward-iteration-protocol(destination);
    for (element in sequence,
	 res-state = res-init then res-next(destination, res-state),
	 until: res-done?(destination, res-state, res-limit))
      res-elem(destination, res-state) := proc(element);
    end for;
    destination;
  else
    next-method();
  end if;
end method map-into;


define method fill!(sequence :: <mutable-sequence>, value :: <object>,
		    #next next-method,
		    #key start: first = 0, end: last)
 => seq :: <mutable-sequence>;
  // The "collection" method will likely be faster if there are no keywrds.
  if (first = 0 & ~last) next-method() end if;
    
  let (init-state, limit, next-state, done?,
       current-key, current-element,
       current-element-setter) = forward-iteration-protocol(sequence);
  for (state = init-state then next-state(sequence, state),
       index from 0 below first,
       until: done?(sequence, state, limit))
  finally
    if (last)
      for (state = state then next-state(sequence, state),
	   index from index below last,
	   until: done?(sequence, state, limit))
	current-element(sequence, state) := value;
      end for;
    else
      for (state = state then next-state(sequence, state),
	   until: done?(sequence, state, limit))
	current-element(sequence, state) := value;
      end for;
    end if;
  end for;
  sequence;
end method fill!;


define method find-key(sequence :: <sequence>, proc :: <function>,
		       #key skip, failure = #f)
 => key-or-failure :: <object>;
  let (init-state, limit, next-state, done?,
       current-key, current-element) = forward-iteration-protocol(sequence);
  block (return)
    for (elem in sequence,
	 key from 0)
      if (proc(elem))
	if (skip & skip > 0)
	  skip := skip - 1;
	else
	  return(key);
	end if;
      end if;
    finally failure
    end for;
  end block;
end method find-key;


define method add(sequence :: <sequence>, new-element) => seq :: <sequence>;
  let old-size = size(sequence);
  let result = make(type-for-copy(sequence), size: old-size + 1);
  map-into(result, identity, sequence);
  result[old-size] := new-element;
  result;
end method add;


define method add!(sequence :: <sequence>, new-element) => seq :: <sequence>;
  add(sequence, new-element);
end method add!;


define method add-new(sequence :: <sequence>, new-element,
		      #key test = \==) => seq :: <sequence>;
  if (any?(rcurry(test, new-element), sequence))
    sequence;
  else
    add(sequence, new-element);
  end if;
end method add-new;


define method add-new!(sequence :: <sequence>, new-element,
		      #key test = \==) => seq :: <sequence>;
  if (any?(rcurry(test, new-element), sequence))
    sequence;
  else
    add!(sequence, new-element);
  end if;
end method add-new!;


define method remove(sequence :: <sequence>, value,
		     #key test = \==, count) => seq :: <sequence>;
  for (result = #() then if (count = 0)
			   pair(elem, result);
			 elseif (~test(elem, value))
			   if (count) count := count - 1 end if;
			   pair(elem, result);
			 else result
			 end if,
       elem in sequence)
  finally
    as(type-for-copy(sequence), reverse!(result));
  end for;
end remove;


define method remove!(sequence :: <sequence>, value,
		      #key test = \==, count: count) => seq :: <sequence>;
  remove(sequence, value, test: test, count: count);
end method remove!;


define generic size-setter 
    (new-size :: <object>, thing-changed :: <object>) 
 => new-size :: <object>;

define method shrink!(sequence :: <sequence>, length) => seq :: <sequence>;
  if (applicable-method?(size-setter, length, sequence))
    size(sequence) := length;
  else
    copy-sequence(sequence, end: length);
  end if;
end method;


define method remove! (sequence :: <mutable-sequence>, value,
		       #key test = \==, count: count) => seq :: <sequence>;
  let (init-state, limit, next-state, done?, current-key,
       current-element, current-element-setter,
       copy-state) = forward-iteration-protocol(sequence);
  local method replace (dest-state, src-state,
			replaced :: <integer>, length :: <integer>)
	  case
	    done?(sequence, src-state, limit) =>
	      shrink!(sequence, length);
	    replaced = count =>
	      for (dest-state = dest-state
		     then next-state(sequence, dest-state),
		   src-state = src-state then next-state(sequence, src-state),
		   length from length,
		   until: done?(sequence, src-state, limit))
		current-element(sequence, dest-state)
		  := current-element(sequence, src-state);
	      finally
		shrink!(sequence, length);
	      end for;
	    test(current-element(sequence, src-state), value) =>
	      replace(dest-state, next-state(sequence, src-state),
		      replaced + 1, length);
	    otherwise =>
	      current-element(sequence, dest-state)
	        := current-element(sequence, src-state);
	      replace(next-state(sequence, dest-state),
		      next-state(sequence, src-state), replaced, length + 1);
	  end case;
	end method replace;
  if (count = 0)
    sequence;
  else 
    block (return)
      for (state = init-state then next-state(sequence, state),
	   length from 0,
	   until: done?(sequence, state, limit))
	if (test(current-element(sequence, state), value))
	  return(replace(copy-state(sequence, state),
			 next-state(sequence, state), 1, length));
	end if;
      finally
	sequence;
      end for;
    end block;
  end if;
end method remove!;


define method choose(predicate :: <function>,
		     sequence :: <sequence>) => seq :: <sequence>;
  for (result = #() then if (predicate(elem)) pair(elem, result)
			 else result
			 end if,
       elem in sequence)
  finally as(type-for-copy(sequence), reverse!(result));
  end for;
end choose;


define method choose-by(predicate :: <function>, test-seq :: <sequence>,
			value-seq :: <sequence>) => seq :: <sequence>;
  for (result = #() then if (predicate(test-elem)) pair(value-elem, result)
			 else result
			 end if,
       value-elem in value-seq, test-elem in test-seq)
  finally as(type-for-copy(value-seq), reverse!(result));
  end for;
end method;


define method intersection(sequence1 :: <sequence>, sequence2 :: <sequence>,
			   #key test = \==) => seq :: <sequence>;
  choose(method (item) member?(item, sequence2, test: test) end method,
	 sequence1);
end method intersection;


define method difference(sequence1 :: <sequence>, sequence2 :: <sequence>,
			 #key test = \==) => seq :: <sequence>;
  choose(method (item) ~member?(item, sequence2, test: test) end method,
	 sequence1);
end method difference;


define method union(sequence1 :: <sequence>, sequence2 :: <sequence>,
		    #key test = \==) => seq :: <sequence>;
  concatenate(sequence1, difference(sequence2, sequence1,
				    test: method(a, b) test(b,a) end method));
end method union;


define method remove-duplicates(sequence :: <sequence>,
				#key test = \==) => seq :: <sequence>;
  local method true-test(a, b) test(b, a) end method;
  for (result = #() then if (~member?(element, result, test: true-test))
			   pair(element, result);
			 else result
			 end if,
       element in sequence)
  finally as(type-for-copy(sequence), reverse!(result));
  end for;
end method remove-duplicates;


define method remove-duplicates!(sequence :: <sequence>,
				 #key test = \==) => seq :: <sequence>;
  remove-duplicates(sequence, test: test);
end method remove-duplicates!;


define method copy-sequence(sequence :: <sequence>,
			    #key start: first = 0, end: last) 
 => seq :: <sequence>;
  let last = if (last) min(last, size(sequence)) else size(sequence) end if;
  let start = min(first, last);
  let sz = if (start <= last) 
	     last - start;
	   else
	     error("End: (%=) is smaller than start: (%=)", last, start);
	   end if;
  let result = make(type-for-copy(sequence), size: sz);
  let (init-state, limit, next-state, done?,
       current-key, current-element) = forward-iteration-protocol(sequence);

  for (index from 0 below start,
       state = init-state then next-state(sequence, state))
  finally
    let (res-init, res-limit, res-next, res-done?, res-key,
	 res-elem, res-elem-setter) = forward-iteration-protocol(result);
    for (index from index below last,
	 state = state then next-state(sequence, state),
	 res-state = res-init then res-next(result, res-state))
      res-elem(result, res-state) := current-element(sequence, state);
    end for;
  end for;
  result;
end method copy-sequence;


define method concatenate-as(cls :: <class>, sequence :: <sequence>,
			     #rest more-sequences) => seq :: <sequence>;
  if (size (sequence) == #f
	| any? (method (s) size (s) == #f end, more-sequences))
    error ("CONCATENATE-AS not applicable to unbounded sequences");
  end if;
  let length = reduce(method (int, seq) int + size(seq) end method,
		      size(sequence), more-sequences);
  let result = make(cls, size: length);
  let (init-state, limit, next-state, done?, current-key, current-element,
       current-element-setter) = forward-iteration-protocol(result);
  local method do-copy(state, seq :: <sequence>) // :: state
	  for (state = state then next-state(result, state),
	       elem in seq)
	    current-element(result, state) := elem;
	  finally state;
	  end for;
	end method do-copy;
  reduce(do-copy, do-copy(init-state, sequence), more-sequences);
  result;
end method concatenate-as;


define method concatenate(sequence :: <sequence>,
			  #rest more-sequences) => seq :: <sequence>;
  apply(concatenate-as, type-for-copy(sequence), sequence, more-sequences);
end method concatenate;


define method replace-subsequence!(sequence :: <mutable-sequence>,
				   insert-sequence :: <sequence>,
				   #key start: first = 0,
				        end: last) => seq :: <sequence>;
  let last = last | size(sequence);
  concatenate(copy-sequence(sequence, start: 0, end: first), insert-sequence,
	      copy-sequence(sequence, start: last));
end method replace-subsequence!;


define method reverse(sequence :: <sequence>) => seq :: <sequence>;
  let result = make(type-for-copy(sequence), size: size(sequence));
  let (res-state, res-limit, res-next, res-done?, res-key, res-elem,
       res-elem-setter) = forward-iteration-protocol(result);
  let (source-state, source-limit, source-next, source-done?, source-key,
       source-elem) = forward-iteration-protocol(sequence);
  local method reverse1(res-state, source-state) // :: res-state
	  if (source-done?(sequence, source-state, source-limit))
	    res-state
	  else 
	    let elem = source-elem(sequence, source-state);
	    let new-res-state =
	      reverse1(res-state, source-next(sequence, source-state));
	    res-elem(result, new-res-state) := elem;
	    res-next(result, new-res-state);
	  end if;
	end method reverse1;
  reverse1(res-state, source-state);
  result;
end method;


define method reverse!(sequence :: <sequence>) => seq :: <sequence>;
  reverse(sequence);
end method reverse!;


define method first(sequence :: <sequence>, #rest keys, #key default)
 => first-elt :: <object>;
  apply(element, sequence, 0, keys);
end method first;


define method second(sequence :: <sequence>, #rest keys, #key default)
 => second-elt :: <object>;
  apply(element, sequence, 1, keys);
end method second;


define method third(sequence :: <sequence>, #rest keys, #key default)
 => third-elt :: <object>;
  apply(element, sequence, 2, keys);
end method third;


define method first-setter(value, sequence :: <sequence>)
 => value :: <object>;
  sequence[0] := value;
end method first-setter;


define method second-setter(value, sequence :: <sequence>)
 => value :: <object>;
  sequence[1] := value;
end method second-setter;


define method third-setter(value, sequence :: <sequence>)
 => value :: <object>;
  sequence[2] := value;
end method third-setter;


define method last(sequence :: <sequence>, #rest keys, #key default)
 => last-elt :: <object>;
  apply(element, sequence, size(sequence) - 1, keys);
end method last;

    
define method last-setter(value, sequence :: <mutable-sequence>)
 => value :: <object>;
  sequence[size(sequence) - 1] := value;
end method last-setter;

    
define method subsequence-position(big :: <sequence>, pattern :: <sequence>,
				   #key test = \==, count = 1) 
 => pos :: <integer>;
  let (init-state, limit, next-state, done?,
       current-key, current-element,
       current-element-setter, copy-state) = forward-iteration-protocol(big);
  let (pat-init-state, pat-limit, pat-next-state,
       pat-done?, pat-current-key, pat-current-element,
       pat-current-element-setter,
       pat-copy-state) = forward-iteration-protocol(pattern);
  
  if (empty?(pattern))
    0
  else
    local method search(index, index-state, big-state, pat-state, count)
	    case
	      pat-done?(pattern, pat-state, pat-limit) =>
		// End of pattern -- We found one.
		if (count = 1)
		  index
		else
		  let next = next-state(big, index-state);
		  search(index + 1, next, copy-state(big, next),
			 pat-copy-state(pattern, pat-init-state), count - 1);
		end if;
	      done?(big, big-state, limit) =>
		// End of big sequence -- it's not here.
		#f;
	      test(current-element(big, big-state),
		   pat-current-element(pattern, pat-state)) =>
		// They match -- try one more.
		search(index, index-state, next-state(big, big-state),
		       pat-next-state(pattern, pat-state), count);
	      otherwise =>
		// Don't match -- try one further along.
		let next = next-state(big, index-state);
	        search(index + 1, next, next & copy-state(big, next),
		       pat-copy-state(pattern, pat-init-state), count);
	    end case;
	  end method search;
    search(0, copy-state(big, init-state), copy-state(big, init-state),
	   pat-copy-state(pattern, pat-init-state), count);
  end if;
end method subsequence-position;


define abstract class <stretchy-collection> (<collection>) 
end class <stretchy-collection>;

define method map-into(destination :: <stretchy-collection>,
		       proc :: <function>, coll :: <collection>,
		       #rest more-collections) 
 => stretchy :: <stretchy-collection>;
  let test1 = key-test(coll);
  if (~instance?(destination, <mutable-collection>))
    error("%= is not a mutable collection.", destination);
  elseif (~ every?( method (c) test1 == key-test(c); end, more-collections ))
    error("Can't map over collections with a different key tests");
  elseif (~ (test1 == key-test(destination)))
    error("Can't map into a collection with a different key test than its sources.");
  elseif (empty?(more-collections))
    for (key in key-sequence(destination))
      destination[key] := proc(coll[key]);
    end for;
  else
    let keys = reduce(rcurry(intersection, test: test1), key-sequence(coll),
		      map(key-sequence, more-collections));
    for (key in keys)
      destination[key] := apply(proc, coll[key],
				map(rcurry(element, key), more-collections));
    end for;
  end if;
  destination;
end method map-into;


// We must define this method or the above method will be ambiguous with the
// "<mutable-sequence>" method.
//
define method map-into(destination :: <stretchy-collection>,
		       proc :: <function>, sequence :: <sequence>,
		       #rest more-sequences)
 => stretchy :: <stretchy-collection>;
  let test1 = key-test(sequence);
  if (~instance?(destination, <mutable-collection>))
    error("%= is not a mutable collection.", destination);
  elseif (~ every?( method (c) test1 == key-test(c); end, more-sequences ))
    error("Can't map over collections with a different key tests");
  elseif (~ (test1 == key-test(destination)))
    error("Can't map into a collection with a different key test than its sources.");
  elseif (empty?(more-sequences))
    let (res-init, res-limit, res-next, res-done?, res-key, res-elem,
	 res-elem-setter) = forward-iteration-protocol(destination);
    let (src-init, src-limit, src-next, src-done?, src-key, src-elem)
      = forward-iteration-protocol(sequence);
    for (key from 0,
	 src-state = src-init then src-next(sequence, src-state),
	 res-state = res-init then res-next(destination, res-state),
	 until: src-done?(sequence, src-state, src-limit) |
	   res-done?(destination, res-state, res-limit))
      res-elem(destination, res-state) := proc(src-elem(sequence, src-state));
    finally
      for (key from key,
	   src-state = src-state then src-next(sequence, src-state),
	   until: src-done?(sequence, src-state, src-limit))
	destination[key] := proc(src-elem(sequence, src-state));
      end for;
    end for;
    destination;
  else
    // Duplicated code from "<collection>" method, to avoid next-method
    // ambiguity. 
    let keys = reduce(rcurry(intersection, test: test1), key-sequence(sequence),
		      map(key-sequence, more-sequences));
    for (key in keys)
      destination[key] := apply(proc, sequence[key],
				map(rcurry(element, key), more-sequences));
    end for;
    destination;
  end if;
end method map-into;

define open generic remove-key!
    (c :: <mutable-explicit-key-collection>, key :: <object>) 
 => removed-anything? :: <boolean>;
