comment: -*- Module: DYLAN -*-
module: Dylan

######################################################################
##
##  Copyright (C) 1994, Carnegie Mellon University
##  All rights reserved.
##
##  This code was produced by the Gwydion Project at Carnegie Mellon
##  University.  If you are interested in using this code, contact
##  "Scott.Fahlman@cs.cmu.edu" (Internet).
##
######################################################################
##
##  $Header: /home/housel/work/rcs/gd/src/mindy/libraries/dylan/coll.dylan,v 1.1 1994/03/24 21:49:52 wlott Exp $
##
## This file contains the collection support code that isn't built in.
##

define constant no_default :: <pair> = pair(#f, #f);


## Collection routines

## We inherit the iteration protocol from the subclasses, which must define
## it. 
## define generic forward-iteration-protocol(collection);

## Element and element-setter will be implemented for arrays and vectors, but
## we must define a default method for all collections.
define method element(coll :: <collection>, key :: <object>,
		      #key default: default (no_default)) :: <object>;
  let (init_state, limit, next_state, done?,
       current_key, current_element) = forward-iteration-protocol(coll);
  block (return)
    for (state = init_state then next_state(coll, state),
	 until done?(coll, state, limit))
      if (current_key(coll, state) = key)
	return(current_element(coll, state));
      end if;
    finally
      if (default == no_default)
	error("No such element in ~S: ~S", collection, key);
      else 
	default;
      end if;
    end for;
  end block;
end method element;

define method element-setter (new_value, collection :: <mutable-collection>,
			      key :: <integer>)
  let (init_state, limit, next_state, done?,
       current_key, current_element,
       current_element-setter) = forward-iteration-protocol(collection);
  block (return)
    for (state = init_state then next_state(collection, state),
	 until done?(collection, state, limit))
      if (current_key(collection, state) = key)
	current_element(collection, state) := new_value;
	return();
      end if;
    end for;
    error("No such element in ~S: ~S", collection, key);
  end block;
end method element-setter;

define method shallow-copy(collection :: <collection>) :: <collection>;
  map(identity, collection);
end method shallow-copy;

define method as(cls :: <class>, collection :: <collection>,
		 #next next-method) :: <object>;
  case
    ~subtype?(cls, <collection>) =>
      if (next-method) next-method();
      else error("Cannot convert ~S to ~S", collection, cls);
      end if;
    instance?(collection, cls) =>
      collection;
    otherwise =>
      map-as(cls, identity, collection);
  end case;
end method as;

## Note: This function depends upon a definition of `=` for sequences, which
## will be supplied later in this file.
define method `=`(a :: <collection>, b :: <collection>) :: <object>;
  key-sequence(a) = key-sequence(b) & every?(`=`, a, b);
end method `=`;

define method size(collection :: <collection>) :: <integer>;
  for (count from 0, elem in collection)
  finally
    count;
  end for;
end method size;

define method class-for-copy(collection :: <mutable-collection>) :: <class>;
  object-class(collection);
end method class-for-copy;

define method empty?(collection :: <collection>) :: <object>;
  let (init, limit, next, done?) = forward-iteration-protocol(collection);
  done?(collection, init, limit);
end method empty?;

## Note: the map methods for arbitrary collections depend upon the iteration
## protocol being defined for "rest args" (i.e. vectors).
define method do(proc :: <function>, collection :: <collection>,
		 #rest more_collections)
  if (empty?(more_collections))
    for (elem in collection) proc(elem) end for;
  else
    let keys = reduce(rcurry(intersection, test: `=`),
		      key-sequence(collection),
		      map(key-sequence, more_collections));
    for (key in keys)
      apply(proc, collection[key],
	    map(rcurry(element, key), more_collections));
    end for;
  end if;
end method do;

define method map(proc :: <function>, collection :: <collection>,
		  #rest more_collections) :: <collection>;
  apply(map-as, class-for-copy(collection), proc, collection,
	more_collections);
end method map;

define method map-as(cls :: <class>, proc :: <function>,
		     coll :: <collection>, #rest more_collections)
    :: <collection>;
  if (empty?(more_collections))
    let result = make(cls, size: size(coll));
    let (init_state, limit, next_state, done?,
	 current_key, current_element) = forward-iteration-protocol(coll);
    for (state = init_state then next_state(coll, state),
	 until done?(coll, state, limit))
      result[current_key(coll, state)] := current_element(coll, state);
    end for;
    result;
  else 
    let keys = reduce(rcurry(intersection, test: `=`),
		      key-sequence(coll),
		      map(key-sequence, more_collections));
    let result = make(cls, size: size(keys));
    for (key in keys)
      result[key] := apply(proc, coll[key],
			   map(rcurry(element, key), more_collections));
    end for;
    result;
  end if;
end method map-as;

define method map-into(destination :: <mutable-collection>, proc :: <function>,
		       coll :: <collection>, #rest more_collections)
    :: <collection>;
  if (empty?(more_collections))
    let keys = intersection(key-sequence(coll), key-sequence(destination),
			    test: `=`);
    for (key in keys)
      destination[key] := proc(coll[key]);
    end for;
  else
    let keys = intersection(reduce(rcurry(intersection, test: `=`),
				   key-sequence(coll),
				   map(key-sequence, more_collections)),
			    key-sequence(destination), test: `=`);
    for (key in keys)
      destination[key] := apply(proc, coll[key],
				map(rcurry(element, key), more_collections));
    end for;
  end if;
  destination;
end method map-into;

define method any?(proc :: <function>, collection :: <collection>,
		   #rest more_collections) :: <object>;
  block (return)
    if (empty?(more_collections))
      for (elem in collection)
	let result = proc(elem);
	if (result) return(result) end if;
      end for;
    else 
      let keys = reduce(rcurry(intersection, test: `=`),
			key-sequence(collection),
			map(key-sequence, more_collections));
      for (key in keys)
	let result = apply(proc, collection[key],
			   map(rcurry(element, key), more_collections));
	if (result) return(result) end if;
      end for;
    end if;
    #f;
  end block;
end method any?;

define method every?(proc :: <function>, collection :: <collection>,
		   #rest more_collections) :: <object>;
  block (return)
    if (empty?(more_collections))
      for (elem in collection)
	unless (proc(elem)) return(#f) end unless;
      end for;
    else
      let keys = reduce(rcurry(intersection, test: `=`),
			key-sequence(collection),
			map(key-sequence, more_collections));
      for (key in keys)
	let result = apply(proc, collection[key],
			   map(rcurry(element, key), more_collections));
	unless (result) return(#f) end unless;
      end for;
    end if;
    #t;
  end block;
end method every?;

define method reduce(proc :: <function>, init_val, collection :: <collection>)
  for (value = init_val then proc(value, elem),
       elem in collection)
  finally value;
  end for;
end method reduce;

define method reduce1(proc :: <function>, collection :: <collection>)
  let (init_state, limit, next_state, done?,
       current_key, current_element) = forward-iteration-protocol(collection);
  if (done?(collection, state, limit)) ## empty collection
    error("Reduce1 not defined for empty collections.");
  else 
    for (state = next_state(collection, init_state)
	   then next_state(collection, state),
	 value = current_element(collection, init_state)
	   then proc(value, current_element(collection, init_state)),
	 until done?(collection, state, limit))
    finally value;
    end for;
  end if;
end method reduce1;

define method member?(value :: <object>, collection :: <collection>,
		      #key test: test (`==`)) :: <object>;
  block (return)
    for (element in collection)
      if (test(value, element)) return(#t) end if;
    end for;
  end block;
end method member?;

define method replace-elements!(collection :: <mutable-collection>,
				predicate :: <function>,
				new_value_fn :: <function>,
				#key count: count) :: <mutable-collection>;
  let (init_state, limit, next_state, done?,
       current_key, current_element,
       current_element-setter) = forward-iteration-protocol(collection);
  for (state = init_state then next_state(collection, state),
       until done?(collection, state, limit) | count == 0)
    let this_element = current_element(collection, state);
    if (predicate(this_element))
      current_element(collection, state) := new_value_fn(this_element);
      if (count) count := count - 1 end if;
    end if;
  end for;
  collection;
end method replace-elements!;

define method fill!(collection :: <mutable-collection>, value :: <object>,
		    #key start: first, end: last) :: <mutable-collection>;
  ## ignore keywords, since they aren't meaningful for arbitrary collections.
  let (init_state, limit, next_state, done?,
       current_key, current_element,
       current_element-setter) = forward-iteration-protocol(collection);
  for (state = init_state then next_state(collection, state),
       until done?(collection, state, limit))
    current_element(collection, state) := value;
  end for;
  collection;
end method fill!;

define method find-key(collection :: <collection>, proc :: <function>,
		       #key skip: skip, failure: failure (#f))
  let (init_state, limit, next_state, done?,
       current_key, current_element) = forward-iteration-protocol(collection);
  block (return)
    for (state = init_state then next_state(collection, state),
	 until done?(collection, state, limit))
      if (proc(current_element(collection, state)))
	if (skip & skip > 0)
	  skip := skip - 1;
	else
	  return(current_key(collection, state));
	end if;
      end if;
    finally failure
    end for;
  end block;
end method find-key;

define method key-sequence(collection :: <collection>) :: <collection>;
  let (init_state, limit, next_state, done?,
       current_key, current_element) = forward-iteration-protocol(collection);
  let result = make(<vector>, size: size(collection));
  for (index from 0,
       state = init_state then next_state(collection, state),
       until done?(collection, state, limit))
    result[index] := current_key(collection, state);
  end for;
  result;
end method key-sequence;

## Sequence routines.

define method element(sequence :: <sequence>, key :: <integer>,
		      #key default: default (no_default)) :: <object>;
  block (return)
    for (this_key from 0, elem in sequence)
      if (this_key == key) return(elem) end if;
    finally
      if (default == no_default)
	error("No such element in ~S: ~S", sequence, key);
      else 
	default;
      end if;
    end for;
  end block;
end method element;

define method element-setter (new_value, sequence :: <mutable-sequence>,
			      key :: <integer>)
  let (init_state, limit, next_state, done?,
       current_key, current_element,
       current_element-setter) = forward-iteration-protocol(sequence);
  block (return)
    for (this_key from 0,
	 state = init_state then next_state(sequence, state),
	 until done?(sequence, state, limit))
      if (this_key == key)
	current_element(sequence, state) := new_value;
	return();
      end if;
    end for;
    error("No such element in ~S: ~S", sequence, key);
  end block;
end method element-setter;

define method `=`(a :: <sequence>, b :: <sequence>) :: <object>;
  let (a_init, a_limit, a_next, a_done?, a_key, a_elem)
    = forward-iteration-protocol(a);
  let (b_init, b_limit, b_next, b_done?, b_key, b_elem)
    = forward-iteration-protocol(b);
  block (return)
    for (a_state = a_init then a_next(a, a_state),
	 b_state = b_init then b_next(b, b_state),
	 until a_done?(a, a_state, a_limit) | b_done?(b, b_state, b_limit))
      if (a_elem(a, a_state) /= b_elem(b, b_state))
	return(#f);
      end if;
    finally
      if (~a_done?(a, a_state, a_limit) | ~b_done?(b, b_state, b_limit))
	return(#f);
      end if;
    end for;
    #t;
  end block;
end method `=`;

### This will be a good and a useful thing once we have written ranges.  Till
### then, it doesn't do much good.
## define method key-sequence(sequence :: <sequence>) :: <range>;
##   make(<range>, from: 0, below: size(sequence));
## end method key-sequence;

define constant aux_map_as =
  method (cls :: <class>, proc :: <function>, #rest seqs)
    let length = apply(min, map(size, seqs));
    let result = make(cls, size: length);
    let (init, limit, next, done?, key, elem, elem-setter)
      = forward-iteration-protocol(result);
    let seq_count = size(seqs);
    let states = make(<vector>, size: seq_count);
    let vals = make(<vector>, size: seq_count);
    let nexts = make(<vector>, size: seq_count);
    let elems = make(<vector>, size: seq_count);

    for (pos from 0, seq in seqs)
      let (init, limit, next, done?, key, elem)
	= forward-iteration-protocol(seq);
      states[pos] := init;
      nexts[pos] := next;
      elems[pos] := elem;
    end for;

    for (state = init then next(result, state),
	 until done?(result, state, limit))
      for (i from 0 below seq_count)
	let (this_seq, this_state) = values(seqs[i], states[i]);
	vals[i] := elems[i](this_seq, this_state);
	states[i] := nexts[i](this_seq, this_state);
      end for;
      elem(result, state) := apply(proc, vals);
    end for;

    result;
  end method;

define method map-as(cls :: <class>, proc :: <function>,
		     sequence :: <sequence>,
		     #next next-method, #rest more_sequences)
  case
    empty?(more_sequences) =>
      let result = make(cls, size: size(sequence));
      let (res_init, res_limit, res_next, res_done?, res_key, res_elem,
	   res_elem-setter) = forward-iteration-protocol(result);
      for (element in sequence,
	   res_state = res_init then res_next(result, res_state))
	res_elem(result, res_state) := proc(element);
      end for;
      result;
    every?(rcurry(instance?, <sequence>), more_sequences) =>
      apply(aux_map_as, cls, proc, sequence, more_sequences);
    otherwise =>
      next-method();
  end case;
end method map-as;

define method map-into(destination :: <mutable-sequence>, proc :: <function>,
		       sequence :: <sequence>,
		       #next next-method, #rest more_sequences)
  if (empty?(more_sequences))
      let (res_init, res_limit, res_next, res_done?, res_key, res_elem,
	   res_elem-setter) = forward-iteration-protocol(destination);
      for (element in sequence,
	   res_state = res_init then res_next(destination, res_state),
	   until res_done?(destination, res_state, res_limit))
	res_elem(destination, res_state) := proc(element);
      end for;
      destination;
  else
    next_method();
  end if;
end method map-into;

define method fill!(sequence :: <mutable-sequence>, value :: <object>,
		    #next next-method,
		    #key start: first (0), end: last) :: <mutable-sequence>;
  ## The "collection" method will likely be faster if there are no keywrds.
  if (first = 0 & ~last) next-method() end if;
    
  let (init_state, limit, next_state, done?,
       current_key, current_element,
       current_element-setter) = forward-iteration-protocol(sequence);
  for (state = init_state then next_state(sequence, state),
       index from 0 below first,
       until done?(sequence, state, limit))
  finally
    if (last)
      for (state = state then next_state(sequence, state),
	   index from index below last,
	   until done?(sequence, state, limit))
	current_element(sequence, state) := value;
      end for;
    else
      for (state = state then next_state(sequence, state),
	   until done?(sequence, state, limit))
	current_element(sequence, state) := value;
      end for;
    end if;
  end for;
  sequence;
end method fill!;

define method find-key(sequence :: <sequence>, proc :: <function>,
		       #key skip: skip, failure: failure (#f))
  let (init_state, limit, next_state, done?,
       current_key, current_element) = forward-iteration-protocol(sequence);
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

define method add(sequence :: <sequence>, new_element) :: <sequence>;
  let old_size = size(sequence);
  let result = make(class-for-copy(sequence), size: old_size + 1);
  map-into(result, identity, sequence);
  result[old_size] := new_element;
  result;
end method add;

define method add!(sequence :: <sequence>, new_element) :: <sequence>;
  add(sequence, new_element);
end method add!;

define method add-new(sequence :: <sequence>, new_element,
		      #key test: test (`==`)) :: <sequence>;
  if (any?(rcurry(test, new_element), sequence))
    sequence;
  else
    add(sequence, new_element);
  end if;
end method add-new;

define method add-new!(sequence :: <sequence>, new_element,
		      #key test: test (`==`)) :: <sequence>;
  if (any?(rcurry(test, new_element), sequence))
    sequence;
  else
    add!(sequence, new_element);
  end if;
end method add-new!;

define method remove(sequence :: <sequence>, value,
		     #key test: test (`==`), count: count) :: <sequence>;
  let result = make(<deque>);
  for (elem in sequence)
    if (count = 0 | ~test(elem, value))
      push-last(result, elem);
      if (count) count := count - 1 end if;
    end if;
  end for;
  as(class-for-copy(sequence), result);
end remove;

define method remove!(sequence :: <sequence>, value,
		      #key test: test (`==`), count: count) :: <sequence>;
  remove(sequence, value, test: test, count: count);
end method remove!;

define generic size-setter(length, collection);

define method shrink!(sequence :: <sequence>, length) :: <sequence>;
  if (empty?(applicable-method?(size-setter, length, sequence)))
    copy-sequence(sequence, end: length);
  else
    size(sequence) := length;
  end if;
end method;

define method remove! (sequence :: <mutable-sequence>, value,
		       #key test: test (`==`), count: count) :: <sequence>;
  let (init_state, limit, next_state, done?, current_key,
       current_element, current_element-setter,
       copy_state) = forward-iteration-protocol(sequence);
  local method replace (dest_state, src_state,
			replaced :: <integer>, length :: <integer>)
	  case
	    done?(sequence, src_state, limit) =>
	      shrink!(sequence, length);
	    replaced = count =>
	      for (dest_state = dest_state
		     then next_state(sequence, dest_state),
		   src_state = src_state then next_state(sequence, src_state),
		   length from length,
		   until done?(sequence, src_state, limit))
		current_element(sequence, dest_state)
		  := current_element(sequence, src_state);
	      finally
		shrink!(sequence, length);
	      end for;
	    test(current_element(sequence, src_state), value) =>
	      replace(dest_state, next_state(sequence, src_state),
		      replaced + 1, length);
	    otherwise =>
	      current_element(sequence, dest_state)
	        := current_element(sequence, src_state);
	      replace(next_state(sequence, dest_state),
		      next_state(sequence, src_state), replaced, length + 1);
	  end case;
	end method replace;
  if (count = 0)
    sequence;
  else 
    block (return)
      for (state = init_state then next_state(sequence, state),
	   length from 0,
	   until done?(sequence, state, limit))
	if (test(current_element(sequence, state), value))
	  return(replace(copy_state(sequence, state),
			 next_state(sequence, state), 1, length));
	end if;
      finally
	sequence;
      end for;
    end block;
  end if;
end method remove!;

define method choose(predicate :: <function>,
		     sequence :: <sequence>) :: <sequence>;
  let result = make(<deque>);
  for (elem in sequence)
    if (predicate(elem)) push-last(result, elem) end if;
  end for;
  as(class-for-copy(sequence), result);
end choose;

define method choose-by(predicate :: <function>, test_seq :: <sequence>,
			value_seq :: <sequence>) :: <sequence>;
  let result = make(<deque>);
  for (value_elem in value_seq, test_elem in test_seq)
    if (predicate(test_elem)) push-last(result, value_elem) end if;
  end for;
  as(class-for-copy(sequence), result);
end method;

define method intersection(sequence1 :: <sequence>, sequence2 :: <sequence>,
			   #key test: test (`==`)) :: <sequence>;
  choose(method (item) member?(item, sequence2, test: test) end method,
	 sequence1);
end method intersection;

define method difference(sequence1 :: <sequence>, sequence2 :: <sequence>,
			 #key test: test (`==`)) :: <sequence>;
  choose(method (item) ~member?(item, sequence2, test: test) end method,
	 sequence1);
end method difference;

define method difference(sequence1 :: <sequence>, sequence2 :: <sequence>,
			 #key test: test (`==`)) :: <sequence>;
  concatenate(sequence1, difference(sequence2, sequence1,
				    test: method(a, b) test(b,a) end method));
end method difference;

define method remove-duplicates(sequence :: <sequence>,
				#key test: test (`==`)) :: <sequence>;
  let result = make(<deque>);
  for (element in sequence)
    if (~member?(element, result, test: method (a, b) test(b, a) end method))
      push-last(result, element);
    end if;
  end for;
  as(class-for-copy(sequence), result);
end method remove-duplicates;

define method remove-duplicates!(sequence :: <sequence>,
				 #key test: test (`==`)) :: <sequence>;
  remove-duplicates(sequence, test: test);
end method remove-duplicates!;

define method copy-sequence(sequence :: <sequence>,
			    #key start: first (0), end: last) :: <sequence>;
  let last = if (last) min(last, size(sequence)) else size(sequence) end if;
  let start = min(first, last);
  let sz = last - start;
  let result = make(class-for-copy(sequence), size: sz);
  let (init_state, limit, next_state, done?,
       current_key, current_element) = forward-iteration-protocol(sequence);

  for (index from 0 below start,
       state = init_state then next_state(sequence, state))
  finally
    let (res_init, res_limit, res_next, res_done?, res_key,
	 res_elem, res_elem-setter) = forward-iteration-protocol(result);
    for (index from index below last,
	 state = state then next_state(sequence, state),
	 res_state = res_init then res_next(result, res_state))
      res_elem(result, res_state) := current_element(sequence, state);
    end for;
  end for;
  result;
end method copy-sequence;

define method concatenate-as(cls :: <class>, sequence :: <sequence>,
			     #rest more_sequences) :: <sequence>;
  let length = reduce(method (int, seq) int + size(seq) end method,
		      size(sequence), more_sequences);
  let result = make(cls, size: length);
  let (init_state, limit, next_state, done?, current_key, current_element,
       current_element-setter) = forward-iteration-protocol(result);
  local method do_copy(state, seq :: <sequence>) ## :: state
	  for (state = state then next_state(result, state),
	       elem in seq)
	    current_element(result, state) := elem;
	  finally state;
	  end for;
	end method do_copy;
  reduce(do_copy, do_copy(init_state, sequence), more_sequences);
  result;
end method concatenate-as;

define method concatenate(sequence :: <sequence>,
			  #rest more_sequences) :: <sequence>;
  apply(concatenate-as, class-for-copy(sequence), sequence, more_sequences);
end method concatenate;

define method replace-subsequence!(sequence :: <mutable-sequence>,
				   insert_sequence :: <sequence>,
				   #key start: first (0),
				        end: last) :: <sequence>;
  let last = last | size(sequence);
  concatenate(copy-sequence(sequence, start: 0, end: first), insert_sequence,
	      copy-sequence(sequence, start: last));
end method replace-subsequence!;

define method reverse(sequence :: <sequence>) :: <sequence>;
  let result = make(class-for-copy(sequence), size: size(sequence));
  let (res-state, res-limit, res-next, res-done?, res-key, res-elem,
       res-elem-setter) = forward-iteration-protocol(result);
  let (source-state, source-limit, source-next, source-done?, source-key,
       source-elem) = forward-iteration-protocol(sequence);
  local method reverse1(res-state, source-state) ## :: res-state
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

define method reverse!(sequence :: <sequence>) :: <sequence>;
  reverse(sequence);
end method reverse!;

define method first(sequence :: <sequence>, #rest keys)
  apply(element, sequence, 0, keys);
end method first;

define method second(sequence :: <sequence>, #rest keys)
  apply(element, sequence, 1, keys);
end method second;

define method third(sequence :: <sequence>, #rest keys)
  apply(element, sequence, 2, keys);
end method third;

define method first-setter(value, sequence :: <sequence>)
  sequence[0] := value;
end method first-setter;

define method second-setter(value, sequence :: <sequence>)
  sequence[1] := value;
end method second-setter;

define method third-setter(value, sequence :: <sequence>)
  sequence[2] := value;
end method third-setter;

define method last(sequence :: <sequence>, #rest keys)
  apply(element, sequence, size(sequence) - 1, keys);
end method last;
    
define method last-setter(value, sequence :: <sequence>)
  sequence[size(sequence) - 1] := value;
end method last-setter;
    
define method subsequence-position(big :: <sequence>, pattern :: <sequence>,
				   #key test: test (`==`),
				        count: count (1))

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
		## End of pattern -- We found one.
		if (count = 1)
		  index
		else
		  let next = next-state(big, index-state);
		  search(index + 1, next, copy-state(big, next),
			 pat-copy-state(pattern, pat-init-state), count - 1);
		end if;
	      done?(big, big-state, limit) =>
		## End of big sequence -- it's not here.
		#f;
	      test(current-element(big, big-state),
		   pat-current-element(pattern, pat-state)) =>
		## They match -- try one more.
		search(index, index-state, next-state(big, big-state),
		       pat-next-state(pattern, pat-state), count);
	      otherwise =>
		## Don't match -- try one further along.
		let next = next-state(big, index-state);
	        search(index + 1, next, next & copy-state(big, next),
		       pat-copy-state(pattern, pat-init-state), count);
	    end case;
	  end method search;
    search(0, copy-state(big, init-state), copy-state(big, init-state),
	   pat-copy-state(pattern, pat-init-state), count);
  end if;
end method subsequence-position;
