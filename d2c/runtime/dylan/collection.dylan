rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/collection.dylan,v 1.6 1995/12/05 03:34:32 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera


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
    => element :: <object>;

define open generic key-sequence
    (collection :: <collection>)
    => keys :: <sequence>;

define open generic element-setter
    (new-value :: <object>, collection :: <collection>, key :: <object>)
    => element :: <object>;

define open generic size
    (collection :: <collection>)
    => res :: false-or(<integer>);

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
    (class :: <class>, proc :: <function>, collection :: <collection>,
     #rest more-collections)
    => new-collection :: <collection>;

define sealed generic map-into
    (target :: <mutable-collection>, proc :: <function>,
     collection :: <collection>, #rest more-collections)
    => new-collection :: <collection>;

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
    (collection :: <collection>)
    => test-function :: <function>;


define inline method class-for-copy (coll :: <mutable-collection>)
    => res :: <class>;
  coll.object-class;
end method class-for-copy;

// Collection Methods.

/* ### not absolutly needed
define method size (collection :: <collection>) => res :: <fixed-integer>;
  for (element in collection,
       result :: <fixed-integer> from 0)
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
	    map(rcurry(element, key), more-collections));
    end for;
  end;
  #f;
end method do;

*/

define inline method map
    (proc :: <function>, collection :: <collection>, #rest more-collections)
    => res :: <collection>;
  apply(map-as, class-for-copy(collection), proc, collection,
	more-collections);
end;

/* ### not absolutly needed

define method map-as
    (class :: <class>, proc :: <function>, collection :: <collection>,
     #rest more-collections)
    => res :: <collection>;
  if (empty?(more-collections))
    let (init, limit, next-state, finished?, current-key, current-element)
      = forward-iteration-protocol(collection);
    let result = make(class, size: collection.size);
    for (state = init then next-state(collection, state),
	 until: finished?(collection, state, limit))
      result[current-key(collection, state)]
	:= current-element(collection, state);
    end;
    result;
  else
    let keys = key-intersection(collection, more-collections);
    let result = make(class, size: keys.size);
    for (key in keys)
      result[key]
	:= apply(proc, collection[key],
		 map(rcurry(element, key), more-collections));
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
	       map(rcurry(element, key), more-collections));
  end for;
  target;
end method map-into;

*/

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

/* ### not absolutly needed
define method find-key
    (collection :: <collection>, proc :: <function>, #key skip, failure = #f)
    => key-or-failure :: <object>;
  let (init-state, limit, next-state, done?, current-key, current-element)
    = forward-iteration-protocol(collection);
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
    finally
      failure
    end for;
  end block;
end method find-key;

define method replace-elements!
    (collection :: <mutable-collection>, predicate :: <function>,
     new-value-fn :: <function>, #key count)
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

define method key-sequence (collection :: <collection>)
    => keys :: <sequence>;
  let (init-state, limit, next-state, done?, current-key, current-element)
    = forward-iteration-protocol(collection);
  let result = make(<vector>, size: size(collection));
  for (index :: <fixed-integer> from 0,
       state = init-state then next-state(collection, state),
       until: done?(collection, state, limit))
    result[index] := current-key(collection, state);
  end for;
  result;
end method key-sequence;

// No method on key-test for <collection>.

*/


// Sequence generics

define open generic size-setter
    (new :: <integer>, seq :: <stretchy-collection>)
    => new :: <integer>;

define open generic add
    (seq :: <sequence>, new-element :: <object>)
    => new-seq :: <sequence>;

define open generic add!
    (seq :: <sequence>, new-element :: <object>)
    => maybe-new-seq :: <sequence>;

define open generic add-new
    (seq :: <sequence>, new-element :: <object>, #key test)
    => new-seq :: <sequence>;

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
    (seq1 :: <sequence>, seq2 :: <sequence>, #key)
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
    (class :: <class>, seq :: <sequence>, #rest more-sequences)
    => new-seq :: <sequence>;

define sealed generic concatenate
    (seq :: <sequence>, #rest more-sequences)
    => new-seq :: <sequence>;

define open generic replace-subsequence!
    (seq :: <sequence>, insert-seq :: <sequence>, #key start, end: finis)
    => result-seq :: <sequence>;

define open generic reverse
    (seq :: <sequence>)
    => new-seq :: <sequence>;

define open generic reverse!
    (seq :: <sequence>)
    => new-seq :: <sequence>;

define open generic sort
    (seq :: <sequence>, #key test, stable)
    => new-seq :: <sequence>;

define open generic sort!
    (seq :: <sequence>, #key test, stable)
    => new-seq :: <sequence>;

define sealed generic first
    (seq :: <sequence>, #key default)
    => value :: <object>;

define sealed generic second
    (seq :: <sequence>, #key default)
    => value :: <object>;

define sealed generic third
    (seq :: <sequence>, #key default)
    => value :: <object>;

define sealed generic first-setter
    (new-value :: <object>, seq :: <sequence>)
    => value :: <object>;

define sealed generic second-setter
    (new-value :: <object>, seq :: <sequence>)
    => value :: <object>;

define sealed generic third-setter
    (new-value :: <object>, seq :: <sequence>)
    => value :: <object>;

define open generic last
    (seq :: <sequence>, #key default)
    => value :: <object>;

define open generic last-setter
    (new-value :: <object>, seq :: <sequence>)
    => value :: <object>;

define open generic subsequence-position
    (big :: <sequence>, pattern :: <sequence>, #key test, count)
    => index :: <integer>;


// Sequence methods

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
    for (index :: <fixed-integer> from more-collections.size - 1 to 0 by -1)
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
    (class :: <class>, proc :: <function>, collection :: <sequence>,
     #next next-method, #rest more-collections)
    => res :: <collection>;
  if (subtype?(class, <sequence>)
	& every?(rcurry(instance?, <sequence>), more-collections))
    apply(sequence-map-as, class, proc, collection, more-collections);
  else
    next-method();
  end;
end;

define method sequence-map-as
    (class :: <class>, proc :: <function>, sequence :: <sequence>,
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
  apply(sequence-map-into, make(class, size: len), proc, sequence,
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
    let sequences = #();
    let states = #();
    let limits = #();
    let next-states = #();
    let finished-state?s = #();
    let current-elements = #();
    for (index :: <fixed-integer> from more-sequences.size - 1 to 0 by -1)
      let this-sequence = more-sequences[index];
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
    
    until (target-finished-state?(target, target-state, target-limit)
	     | any?(method (coll, state, limit, finished-state?)
		      finished-state?(coll, state, limit);
		    end,
		    sequences, states, limits, finished-state?s))
      target-current-element(target, target-state)
	:= apply(proc,
		 map(method (coll, state, current-element)
		       current-element(coll, state);
		     end,
		     sequences, states, current-elements));
      target-state := target-next-state(target, target-state);
      map-into(states, 
	       method (coll, state, next-state)
		 next-state(coll, state);
	       end,
	       sequence, states, next-states);
    end until;
  end;
  target;
end method sequence-map-into;

/* ### not absolutly needed

define method find-key
    (sequence :: <sequence>, proc :: <function>, #key skip, failure = #f)
    => key-or-failure :: <object>;
  let (init-state, limit, next-state, done?, current-key, current-element)
    = forward-iteration-protocol(sequence);
  block (return)
    for (elem in sequence,
	 key :: <fixed-integer> from 0)
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
     #key start: first = 0, end: last)
    => sequence :: <mutable-sequence>;
    
  let (init-state, limit, next-state, done?,
       current-key, current-element,
       current-element-setter) = forward-iteration-protocol(sequence);
  for (state = init-state then next-state(sequence, state),
       index :: <fixed-integer> from 0 below first,
       until: done?(sequence, state, limit))
  finally
    if (last)
      for (state = state then next-state(sequence, state),
	   index :: <fixed-integer> from index below last,
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

/*
define method key-sequence(sequence :: <sequence>)
    => keys :: <range>;
  let s = size(sequence);
  if (s)
    range(from: 0, below: s);
  else
    range(from: 0);
  end if;
end method key-sequence;
*/

define sealed inline method key-test
    (sequence :: <sequence>) => test :: <function>;
  \==;
end method key-test;

define method concatenate (sequence :: <sequence>, #rest more-sequences)
    => new-seq :: <sequence>;
  apply(concatenate-as, sequence.class-for-copy, sequence, more-sequences);
end;

/*
define method concatenate-as
    (class :: <class>, sequence :: <sequence>, #rest more-sequences)
    => new-seq :: <sequence>;
*/


define inline method first
    (sequence :: <sequence>, #rest keys, #key default) => value :: <object>;
  apply(element, 0, keys);
end;

define inline method first-setter
    (new-value :: <object>, sequence :: <sequence>) => new-value :: <object>;
  sequence[0] := new-value;
end;

define inline method second
    (sequence :: <sequence>, #rest keys, #key default) => value :: <object>;
  apply(element, 1, keys);
end;

define inline method second-setter
    (new-value :: <object>, sequence :: <sequence>) => new-value :: <object>;
  sequence[1] := new-value;
end;

define inline method third
    (sequence :: <sequence>, #rest keys, #key default) => value :: <object>;
  apply(element, 2, keys);
end;

define inline method third-setter
    (new-value :: <object>, sequence :: <sequence>) => new-value :: <object>;
  sequence[2] := new-value;
end;

*/

define method copy-sequence
    (sequence :: <sequence>, #key start :: <fixed-integer> = 0, end: last)
 => (result :: <sequence>);
  let seq-size :: <fixed-integer> = sequence.size;
  let last :: <fixed-integer> = last | seq-size;
  case
    (last > seq-size) => error("End: (%=) out of range.", last);
    (start < 0) => error("Start: (%=) out of range.", start);
    (start > last) => error("Start: (%=) > End: (%=).", start, last);
  end case;

  let sz :: <fixed-integer> = last - start;
  let result = make(class-for-copy(sequence), size: sz);
  let (init-state, limit, next-state, done?,
       current-key, current-element) = forward-iteration-protocol(sequence);

  for (index :: <fixed-integer> from 0 below start,
       state = init-state then next-state(sequence, state))
  finally
    let (res-init, res-limit, res-next, res-done?, res-key,
	 res-elem, res-elem-setter) = forward-iteration-protocol(result);
    for (index :: <fixed-integer> from index below last,
	 state = state then next-state(sequence, state),
	 res-state = res-init then res-next(result, res-state))
      res-elem(result, res-state) := current-element(sequence, state);
    end for;
  end for;
  result;
end method copy-sequence;

define inline method last
    (seq :: <sequence>, #key default) => value :: <object>;
  element(seq, seq.size - 1);
end method last;

define inline method last-setter
    (new-value :: <object>, seq :: <sequence>) => value :: <object>;
  element(seq, seq.size - 1) := new-value;
end method last-setter;

// Experiment -- this is the same source as the general method, but more
// specific type info.  How will the result differ?
define method copy-sequence
    (sequence :: <simple-object-vector>, #key start :: <fixed-integer> = 0, end: last)
 => (result :: <simple-object-vector>);
  let seq-size :: <fixed-integer> = sequence.size;
  let last :: <fixed-integer> = last | seq-size;
  case
    (last > seq-size) => error("End: (%=) out of range.", last);
    (start < 0) => error("Start: (%=) out of range.", start);
    (start > last) => error("Start: (%=) > End: (%=).", start, last);
  end case;

  let sz :: <fixed-integer> = last - start;
  let result-cls :: <class> = class-for-copy(sequence);
  let result :: result-cls = make(result-cls, size: sz);
  let (init-state, limit, next-state, done?,
       current-key, current-element) = forward-iteration-protocol(sequence);

  for (index :: <fixed-integer> from 0 below start,
       state = init-state then next-state(sequence, state))
  finally
    let (res-init, res-limit, res-next, res-done?, res-key,
	 res-elem, res-elem-setter) = forward-iteration-protocol(result);
    for (index :: <fixed-integer> from index below last,
	 state = state then next-state(sequence, state),
	 res-state = res-init then res-next(result, res-state))
      res-elem(result, res-state) := current-element(sequence, state);
    end for;
  end for;
  result;
end method copy-sequence;

