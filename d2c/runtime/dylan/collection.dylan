rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/collection.dylan,v 1.1 1998/05/03 19:55:38 andreas Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera


//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
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

// Classes

define abstract open class <collection> (<object>)
end;

define abstract open class <explicit-key-collection> (<collection>)
end;

define abstract open class <stretchy-collection> (<collection>)
end;

define abstract open class <mutable-collection> (<collection>)
end;

define abstract open class <sequence> (<collection>)
end;

define abstract open class <mutable-explicit-key-collection>
    (<mutable-collection>, <explicit-key-collection>)
end;

define abstract open class <mutable-sequence>
    (<mutable-collection>, <sequence>)
end;


// Collection Generics.

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

define open generic element
    (collection :: <collection>, key :: <object>, #key default)
 => (element :: <object>);

define open generic key-sequence
    (collection :: <collection>) => (keys :: <sequence>);

define open generic element-setter
    (new-value :: <object>, collection :: <mutable-collection>,
     key :: <object>)
 => (element :: <object>);

define open generic size (object :: <object>) => (res :: <object>);

define open generic empty?
    (collection :: <collection>)
    => res :: <boolean>;

define sealed generic do
    (proc :: <function>, collection :: <collection>, #rest more-collections)
    => false :: <false>;

define sealed generic map
    (proc :: <function>, collection :: <collection>, #rest more-collections)
    => new-collection :: <collection>;

define sealed generic map-as
    (type :: <type>, proc :: <function>, collection :: <collection>,
     #rest more-collections)
    => new-collection :: <mutable-collection>;

define sealed generic map-into
    (target :: <mutable-collection>, proc :: <function>,
     collection :: <collection>, #rest more-collections)
    => new-collection :: <mutable-collection>;

define sealed generic any?
    (pred :: <function>, collection :: <collection>, #rest more-collections)
    => value;

define sealed generic every?
    (pred :: <function>, collection :: <collection>, #rest more-collections)
    => boolean :: <boolean>;

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
    (target :: <mutable-collection>, value :: <object>, #key)
    => target :: <mutable-collection>;

define open generic key-test
    (collection :: <collection>) => (test-function :: <function>);


define inline method type-for-copy (coll :: <mutable-collection>)
    => res :: <class>;
  coll.object-class;
end method type-for-copy;

// Collection Methods.

define method size (collection :: <collection>) => res :: <integer>;
  for (element in collection,
       result :: <integer> from 0)
  finally
    result;
  end;
end method size;

define inline method empty? (coll :: <collection>) => res :: <boolean>;
  let (state, limit, ignore, finished-state?)
    = forward-iteration-protocol(coll);
  finished-state?(coll, state, limit);
end method empty?;

define method key-intersection
    (collection :: <collection>, more-collections :: <simple-object-vector>)
    => res :: <sequence>;
  let test = key-test(collection);
  let keys = key-sequence(collection);
  for (other-collection :: <collection> in more-collections)
    unless (other-collection.key-test == test)
      error("Can't do over collections with different "
	      "key tests");
    end;
    keys := intersection(keys, key-sequence(other-collection), test: test);
  end;
  keys;
end;

define method do
    (proc :: <function>, collection :: <collection>, #rest more-collections)
    => res :: <false>;
  if (empty?(more-collections))
    for (element in collection)
      proc(element);
    end;
  else
    let keys = key-intersection(collection, more-collections);
    for (key in keys)
      apply(proc, collection[key],
	    map(method (coll) coll[key] end method, more-collections));
    end for;
  end;
  #f;
end method do;

define inline method map
    (proc :: <function>, collection :: <collection>, #rest more-collections)
    => res :: <collection>;
  apply(map-as, type-for-copy(collection), proc, collection,
	more-collections);
end;

define method map-as
    (type :: <type>, proc :: <function>, collection :: <collection>,
     #rest more-collections)
    => res :: <mutable-collection>;
  if (empty?(more-collections))
    let (init, limit, next-state, finished?, current-key, current-element)
      = forward-iteration-protocol(collection);
    let result = make(type, size: collection.size);

    // We can just iterate normally across collection, but we can't
    // iterate across result because we don't know that
    // collection.key-sequence matches result.key-sequence

    for (state = init then next-state(collection, state),
	 until: finished?(collection, state, limit))
      result[current-key(collection, state)]
	:= proc(current-element(collection, state));
    end;
    result;
  else
    let keys = key-intersection(collection, more-collections);
    let result = make(type, size: keys.size);
    for (key in keys)
      result[key]
	:= apply(proc, collection[key],
		 map(method (coll) coll[key] end method, more-collections));
    end for;
    result;
  end;
end;

define method map-into
    (target :: <mutable-collection>, proc :: <function>,
     collection :: <collection>, #rest more-collections)
    => res :: <mutable-collection>;
  let test = key-test(target);
  unless (collection.key-test == test)
    error("Can't do over collections with different key tests");
  end;
  let keys = intersection(key-sequence(target),
			  key-intersection(collection, more-collections),
			  test: test);
  // ### The book says that we are supposed to replace the target's elements
  // with the results of applying proc to the collections.  But how do we
  // flush out the old keys?
  for (key in keys)
    target[key]
      := apply(proc, collection[key],
	       map(method (coll) coll[key] end method, more-collections));
  end for;
  target;
end method map-into;

define inline method any?
    (proc :: <function>, collection :: <collection>, #rest more-collections)
    => res :: <object>;
  block (return)
    apply(do,
	  method (#rest args)
	    let res = apply(proc, args);
	    if (res)
	      return(res);
	    end;
	  end,
	  collection,
	  more-collections);
    #f;
  end block;
end method any?;

define inline method every?
    (proc :: <function>, collection :: <collection>, #rest more-collections)
    => res :: <boolean>;
  block (return)
    apply(do,
	  method (#rest args)
	    unless (apply(proc, args))
	      return(#f);
	    end;
	  end,
	  collection,
	  more-collections);
    #t;
  end block;
end method every?;

// reduce and reduce1 can't be inline, or the compiler transform for them
// won't trigger.
// 
define method reduce
    (proc :: <function>, init-value :: <object>, collection :: <collection>)
    => res :: <object>;
  for (res = init-value then proc(res, element),
       element in collection)
  finally
    res;
  end for;
end method reduce;

define method reduce1
    (proc :: <function>, collection :: <collection>)
    => res :: <object>;
  for (res = #f then if (first?) element else proc(res, element) end,
       first? = #t then #f,
       element in collection)
  finally
    if (first?)
      error("reduce1 must be passed at least one element.");
    else
      res;
    end if;
  end for;
end method reduce1;

define inline method choose
    (predicate :: <function>, sequence :: <sequence>)
 => (result :: <sequence>);
  for (result = #()
	 then if (predicate(elem)) pair(elem, result) else result end if,
       elem in sequence)
  finally
    as(type-for-copy(sequence), reverse!(result));
  end for;
end choose;

define inline method choose-by
    (predicate :: <function>, test-seq :: <sequence>, value-seq :: <sequence>)
 => (result :: <sequence>);
  for (result = #() then if (predicate(test-elem))
			   pair(value-elem, result);
			 else
			   result;
			 end if,
       value-elem in value-seq,
       test-elem in test-seq)
  finally
    as(type-for-copy(value-seq), reverse!(result));
  end for;
end method;

define inline method member?
    (value :: <object>, collection :: <collection>, #key test = \==)
    => res :: <boolean>;
  block (return)
    for (el in collection)
      if (test(value, el))
	return(#t);
      end;
    end;
    #f;
  end;
end method member?;

define method find-key
    (collection :: <collection>, proc :: <function>,
     #key skip :: <integer> = 0, failure = #f)
 => (key-or-failure :: <object>);
  let (init-state, limit, next-state, done?, current-key, current-element)
    = forward-iteration-protocol(collection);
  block (return)
    for (state = init-state then next-state(collection, state),
	 until: done?(collection, state, limit))
      if (proc(current-element(collection, state)))
	if (skip > 0)
	  skip := skip - 1;
	else
	  return(current-key(collection, state));
	end if;
      end if;
    finally
      failure
    end for;
  end block;
end method find-key;

define method replace-elements!
    (collection :: <mutable-collection>, predicate :: <function>,
     new-value-fn :: <function>, #key count :: false-or(<integer>))
    => collection :: <mutable-collection>;
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


define method fill!
    (collection :: <mutable-collection>, value :: <object>, #key)
 => collection :: <mutable-collection>;
  let (init-state, limit, next-state, done?,
       current-key, current-element,
       current-element-setter) = forward-iteration-protocol(collection);
  for (state = init-state then next-state(collection, state),
       until: done?(collection, state, limit))
    current-element(collection, state) := value;
  end for;
  collection;
end method fill!;

define inline method shallow-copy
    (collection :: <collection>) => (result :: <collection>);
  map(identity, collection);
end method shallow-copy;

define method key-sequence (collection :: <collection>)
    => keys :: <sequence>;
  let (init-state, limit, next-state, done?, current-key, current-element)
    = forward-iteration-protocol(collection);
  let result = make(<vector>, size: size(collection));
  for (index :: <integer> from 0,
       state = init-state then next-state(collection, state),
       until: done?(collection, state, limit))
    result[index] := current-key(collection, state);
  end for;
  result;
end method key-sequence;

// No method on key-test for <collection>.


// Explicit-key-collection generics

define open generic remove-key!
    (table :: <mutable-explicit-key-collection>, key :: <object>)
 => (result :: <boolean>);


// Sequence generics

define open generic size-setter (new :: <object>, object :: <object>)
 => (new :: <object>);

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

define sealed generic concatenate-as
    (type :: <type>, seq :: <sequence>, #rest more-sequences)
    => new-seq :: <sequence>;

define sealed generic concatenate
    (seq :: <sequence>, #rest more-sequences)
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

define sealed generic first
    (seq :: <sequence>, #key default) => (value :: <object>);

define sealed generic second
    (seq :: <sequence>, #key default) => (value :: <object>);

define sealed generic third
    (seq :: <sequence>, #key default) => (value :: <object>);

define sealed generic first-setter
    (new-value :: <object>, seq :: <mutable-sequence>) => (value :: <object>);

define sealed generic second-setter
    (new-value :: <object>, seq :: <mutable-sequence>) => (value :: <object>);

define sealed generic third-setter
    (new-value :: <object>, seq :: <mutable-sequence>) => (value :: <object>);

define open generic last
    (seq :: <sequence>, #key default) => (value :: <object>);

define open generic last-setter
    (new-value :: <object>, seq :: <mutable-sequence>) => (value :: <object>);

define open generic subsequence-position
    (big :: <sequence>, pattern :: <sequence>, #key test, count)
 => (index :: false-or(<integer>));


// Sequence methods

define inline method add
    (seq :: <sequence>, new-element :: <object>) => (new-seq :: <sequence>);
  concatenate(seq, vector(new-element));
end method add;

define inline method add!
    (seq :: <sequence>, new-element :: <object>)
 => (maybe-new-seq :: <sequence>);
  add(seq, new-element);
end method add!;

define inline method add-new
    (seq :: <sequence>, new-element :: <object>, #key test :: <function> = \==)
 => (new-seq :: <sequence>);
  if (member?(new-element, seq, test: method (a, b) test(b, a) end method))
    seq;
  else
    add(seq, new-element);
  end if;
end method add-new;

define inline method add-new!
    (seq :: <sequence>, new-element :: <object>, #key test :: <function> = \==)
 => (new-seq :: <sequence>);
  if (member?(new-element, seq, test: method (a, b) test(b, a) end method))
    seq;
  else
    add!(seq, new-element);
  end if;
end method add-new!;

define method remove
    (sequence :: <sequence>, value,
     #key test :: <function> = \==, count :: false-or(<integer>))
 => (result :: <sequence>);
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

define inline method remove!
    (sequence :: <sequence>, value,
     #key test :: <function> = \==, count :: false-or(<integer>))
 => (result :: <sequence>);
  remove(sequence, value, test: test, count: count);
end method remove!;

define method reverse (sequence :: <sequence>) => (result :: <sequence>);
  let result = make(sequence.type-for-copy, size: sequence.size);
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

define inline method reverse! (sequence :: <sequence>)
 => (result :: <sequence>);
  reverse(sequence);
end method reverse!;

define method intersection
    (sequence1 :: <sequence>, sequence2 :: <sequence>,
     #key test :: <function> = \==)
 => (result :: <sequence>);
  choose(method (item) member?(item, sequence2, test: test) end method,
	 sequence1);
end method intersection;

define method difference
    (sequence1 :: <sequence>, sequence2 :: <sequence>,
     #key test :: <function> = \==)
 => (result :: <sequence>);
  choose(method (item) ~member?(item, sequence2, test: test) end method,
	 sequence1);
end method difference;

define method union
    (sequence1 :: <sequence>, sequence2 :: <sequence>,
     #key test :: <function> = \==)
 => (result :: <sequence>);
  concatenate(sequence1, difference(sequence2, sequence1,
				    test: method (a, b) test(b, a) end));
end method union;

define method remove-duplicates
    (sequence :: <sequence>, #key test :: <function> = \==)
 => (result :: <sequence>);
  local method true-test (a, b) test(b, a) end method;
  for (result = #() then if (~member?(element, result, test: true-test))
			   pair(element, result);
			 else
			   result;
			 end if,
       element in sequence)
  finally
    as(type-for-copy(sequence), reverse!(result));
  end for;
end method remove-duplicates;

define inline method remove-duplicates!
    (sequence :: <sequence>, #key test :: <function> = \==)
 => (result :: <sequence>);
  remove-duplicates(sequence, test: test);
end method remove-duplicates!;

define method replace-subsequence!
    (sequence :: <sequence>, insert-sequence :: <sequence>,
     #key start: first :: <integer> = 0,
          end: last :: <integer> = sequence.size)
 => (sequence :: <sequence>);
  concatenate(copy-sequence(sequence, start: 0, end: first), insert-sequence,
	      copy-sequence(sequence, start: last));
end method replace-subsequence!;

define method subsequence-position
    (big :: <sequence>, pattern :: <sequence>,
     #key test :: <function> = \==, count :: <integer> = 1)
 => (result :: false-or(<integer>));
  if (empty?(pattern))
    0
  else
    let (init-state, limit, next-state, done?,
	 current-key, current-element,
	 current-element-setter, copy-state) = forward-iteration-protocol(big);
    let (pat-init-state, pat-limit, pat-next-state,
	 pat-done?, pat-current-key, pat-current-element,
	 pat-current-element-setter,
	 pat-copy-state) = forward-iteration-protocol(pattern);
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

define method do
    (proc :: <function>, sequence :: <sequence>,
     #next next-method, #rest more-collections)
    => res :: <false>;
  if (empty?(more-collections))
    for (element in sequence)
      proc(element);
    end;
  elseif (every?(rcurry(instance?, <sequence>), more-collections))
    let sequences = #();
    let states = #();
    let limits = #();
    let next-states = #();
    let finished-state?s = #();
    let current-elements = #();
    for (index :: <integer> from more-collections.size - 1 to 0 by -1)
      let this-sequence = more-collections[index];
      let (state, limit, next-state, finished-state?, ignore, current-element)
	= forward-iteration-protocol(this-sequence);
      sequences := pair(this-sequence, sequences);
      states := pair(state, states);
      limits := pair(limit, limits);
      next-states := pair(next-state, next-states);
      finished-state?s := pair(finished-state?s, finished-state?);
      current-elements := pair(current-element, current-elements);
    end;
    begin
      let (state, limit, next-state, finished-state?, ignore, current-element)
	= forward-iteration-protocol(sequence);
      sequences := pair(sequence, sequences);
      states := pair(state, states);
      limits := pair(limit, limits);
      next-states := pair(next-state, next-states);
      finished-state?s := pair(finished-state?s, finished-state?);
      current-elements := pair(current-element, current-elements);
    end;
    
    until (any?(method (coll, state, limit, finished-state?)
		  finished-state?(coll, state, limit);
		end,
		sequences, states, limits, finished-state?s))
      apply(proc,
	    map(method (coll, state, current-element)
		  current-element(coll, state);
		end,
		sequences, states, current-elements));
      map-into(states, 
	       method (coll, state, next-state)
		 next-state(coll, state);
	       end,
	       sequence, states, next-states);
    end;
    #f;
  else
    next-method();
  end;
end;

define method map-as
    (type :: <type>, proc :: <function>, collection :: <sequence>,
     #next next-method, #rest more-collections)
    => res :: <mutable-collection>;
  if (subtype?(type, <sequence>)
	& every?(rcurry(instance?, <sequence>), more-collections))
    apply(sequence-map-as, type, proc, collection, more-collections);
  else
    next-method();
  end;
end;

define method sequence-map-as
    (type :: <type>, proc :: <function>, sequence :: <sequence>,
     #rest more-sequences)
    => res :: <mutable-collection>;
  let len = sequence.size;
  for (other-sequence :: <sequence> in more-sequences)
    let other-len = other-sequence.size;
    if (other-len)
      if (len)
	len := min(len, other-len);
      else
	len := other-len;
      end;
    end;
  end;
  unless (len)
    error("At least one argument to map-as must be bounded");
  end;
  apply(sequence-map-into, make(type, size: len), proc, sequence,
	more-sequences);
end;


define method map-into
    (target :: <mutable-sequence>, proc :: <function>,
     collection :: <sequence>, #next next-method, #rest more-collections)
    => res :: <mutable-sequence>;
  if (every?(rcurry(instance?, <sequence>), more-collections))
    apply(sequence-map-into, target, proc, collection, more-collections);
  else
    next-method();
  end;
end method map-into;


define method sequence-map-into
    (target :: <mutable-sequence>, proc :: <function>,
     sequence :: <sequence>, #next next-method, #rest more-sequences)
    => res :: <mutable-sequence>;
  let (target-state, target-limit, target-next-state, target-finished-state?,
       ignore1, ignore2, target-current-element-setter)
    = forward-iteration-protocol(target);

  if (empty?(more-sequences))
    let (seq-state, seq-limit, seq-next-state, seq-finished-state?, ignore,
	 seq-current-element)
      = forward-iteration-protocol(sequence);
    for (target-state = target-state
	   then target-next-state(target, target-state),
	 seq-state = seq-state
	   then seq-next-state(sequence, seq-state),
	 until: target-finished-state?(target, target-state, target-limit)
	   | seq-finished-state?(sequence, seq-state, seq-limit))
      target-current-element(target, target-state)
	:= proc(seq-current-element(sequence, seq-state));
    end;
  else
    // Stuff all the forward-iteration-protocol info into a bunch of
    // parallel vectors..
    let sequences = make(<vector>, size: more-sequences.size + 1);
    let states = make(<vector>, size: more-sequences.size + 1);
    let limits = make(<vector>, size: more-sequences.size + 1);
    let next-states = make(<vector>, size: more-sequences.size + 1);
    let finished-state?s = make(<vector>, size: more-sequences.size + 1);
    let current-elements = make(<vector>, size: more-sequences.size + 1);
    local method remember-fip(this-sequence :: <sequence>, index :: <integer>);
	    let (state, limit, next-state, finished-state?, 
		 ignore, current-element)
	      = forward-iteration-protocol(this-sequence);
	    sequences[index] := this-sequence;
	    states[index] := state;
	    limits[index] := limit;
	    next-states[index] := next-state;
	    finished-state?s[index] := finished-state?;
	    current-elements[index] := current-element;
	  end method remember-fip;

    remember-fip(sequence, 0);
    for (i :: <integer> from 0 below more-sequences.size)
      remember-fip(more-sequences[i], i + 1);
    end for;
    
    until (target-finished-state?(target, target-state, target-limit)
	     | any?(method (coll, state, limit, finished-state?)
		      finished-state?(coll, state, limit);
		    end,
		    sequences, states, limits, finished-state?s))

      // To call the mapped function on the n'th elements of the
      // sequences, we need to collect all the elements.
      //
      let elt-vector = make(<vector>, size: sequences.size);
      for (seq in sequences, state in states, cur-elt in current-elements,
	   i from 0)
	elt-vector[i] := cur-elt(seq, state);
      end for;
      target-current-element(target, target-state) := apply(proc, elt-vector);
      target-state := target-next-state(target, target-state);

      // Now we need to update the states of all the sequences we're
      // iterating over, and we have to do it without using map-into().
      //
      for (i from 0 below sequences.size)
	states[i] := next-states[i](sequences[i], states[i]);
      end for;
      
      // With a sufficiently smart compiler, we might instead write
      //
      // map-into(states, 
      //          method (coll, state, next-state)
      //            next-state(coll, state);
      //          end,
      //          sequences, states, next-states);
    end until;
  end if;
  target;
end method sequence-map-into;

define method find-key
    (sequence :: <sequence>, proc :: <function>, #key skip, failure = #f)
    => key-or-failure :: <object>;
  let (init-state, limit, next-state, done?, current-key, current-element)
    = forward-iteration-protocol(sequence);
  block (return)
    for (elem in sequence,
	 key :: <integer> from 0)
      if (proc(elem))
	if (skip & skip > 0)
	  skip := skip - 1;
	else
	  return(key);
	end if;
      end if;
    finally
      failure
    end for;
  end block;
end method find-key;


define method fill!
    (sequence :: <mutable-sequence>, value :: <object>,
     #key start: first :: <integer> = 0,
          end: last :: false-or(<integer>))
 => (sequence :: <mutable-sequence>);
    
  let (init-state, limit, next-state, done?,
       current-key, current-element,
       current-element-setter) = forward-iteration-protocol(sequence);
  for (state = init-state then next-state(sequence, state),
       index :: <integer> from 0 below first,
       until: done?(sequence, state, limit))
  finally
    if (last)
      for (state = state then next-state(sequence, state),
	   index :: <integer> from index below last,
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

define inline method key-sequence
    (sequence :: <sequence>) => keys :: <range>;
  let s = size(sequence);
  if (s)
    range(from: 0, below: s);
  else
    range(from: 0);
  end if;
end method key-sequence;

define sealed inline method key-test
    (sequence :: <sequence>) => test :: <function>;
  \==;
end method key-test;

define inline method concatenate (sequence :: <sequence>, #rest more-sequences)
    => new-seq :: <sequence>;
  apply(concatenate-as, sequence.type-for-copy, sequence, more-sequences);
end;

define method concatenate-as(type :: <type>, sequence :: <sequence>,
			     #rest more-sequences) => result :: <sequence>;
  if (size (sequence) == #f
	| any? (method (s) size (s) == #f end, more-sequences))
    error ("CONCATENATE-AS not applicable to unbounded sequences");
  end if;
  let length = for (total = 0 then total + seq.size,
		    seq in more-sequences)
	       finally total + sequence.size;
	       end for;
		 
  let result = make(type, size: length);
  let (init-state, limit, next-state, done?, current-key, current-element,
       current-element-setter) = forward-iteration-protocol(result);

  let new-state = for (elem in sequence,
		       state = init-state then next-state(result, state))
		    current-element(result, state) := elem;
		  finally state;
		  end for;
  for (result-state = new-state
	 then for (elem in sequence,
		   state = result-state then next-state(result, state))
		current-element(result, state) := elem;
	      finally state;
	      end for,
       sequence in more-sequences)
  end for;
  result;
end method concatenate-as;

define inline method first
    (sequence :: <sequence>, #rest keys, #key default) => value :: <object>;
  apply(element, sequence, 0, keys);
end;

define inline method first-setter
    (new-value :: <object>, sequence :: <mutable-sequence>)
 => (new-value :: <object>);
  sequence[0] := new-value;
end;

define inline method second
    (sequence :: <sequence>, #rest keys, #key default) => value :: <object>;
  apply(element, sequence, 1, keys);
end;

define inline method second-setter
    (new-value :: <object>, sequence :: <mutable-sequence>)
 => (new-value :: <object>);
  sequence[1] := new-value;
end;

define inline method third
    (sequence :: <sequence>, #rest keys, #key default) => value :: <object>;
  apply(element, sequence, 2, keys);
end;

define inline method third-setter
    (new-value :: <object>, sequence :: <mutable-sequence>)
 => (new-value :: <object>);
  sequence[2] := new-value;
end;

define method copy-sequence
    (sequence :: <sequence>, #key start :: <integer> = 0, end: last)
 => (result :: <sequence>);
  let seq-size :: <integer> = sequence.size;
  let last :: <integer> = last | seq-size;
  case
    (last > seq-size) => error("End: (%=) out of range.", last);
    (start < 0) => error("Start: (%=) out of range.", start);
    (start > last) => error("Start: (%=) > End: (%=).", start, last);
  end case;

  let sz :: <integer> = last - start;
  let result = make(type-for-copy(sequence), size: sz);
  let (init-state, limit, next-state, done?,
       current-key, current-element) = forward-iteration-protocol(sequence);

  for (index :: <integer> from 0 below start,
       state = init-state then next-state(sequence, state))
  finally
    let (res-init, res-limit, res-next, res-done?, res-key,
	 res-elem, res-elem-setter) = forward-iteration-protocol(result);
    for (index :: <integer> from index below last,
	 state = state then next-state(sequence, state),
	 res-state = res-init then res-next(result, res-state))
      res-elem(result, res-state) := current-element(sequence, state);
    end for;
  end for;
  result;
end method copy-sequence;

define inline method last
    (seq :: <sequence>, #rest keys, #key default) => value :: <object>;
  apply(element, seq, seq.size - 1, keys);
end method last;

define inline method last-setter
    (new-value :: <object>, seq :: <mutable-sequence>) => value :: <object>;
  element(seq, seq.size - 1) := new-value;
end method last-setter;

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

define method \= (a :: <sequence>, b :: <sequence>) => answer :: <boolean>;
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

// Key-exists -- Exported
//
// If the given key is present in the collection, return #t and the value
// associated with the key.  Otherwise, return #f and an undefined value.
//
// Can't use $not-supplied, because we're passing undefined as an
// argument to element(), which itself probably uses $not-supplied..
//
define constant undefined = pair(#f, #f);

define method key-exists? (coll :: <collection>, key :: <object>)
 => (result :: <boolean>, value :: <object>);
  let value = element(coll, key, default: undefined);
  values(value ~= undefined, value);
end method key-exists?;

