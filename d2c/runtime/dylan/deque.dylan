copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera
author: David Pierce (dpierce@cs.cmu.edu)
rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/deque.dylan,v 1.1 1998/05/03 19:55:40 andreas Exp $

//======================================================================
//
// Copyright (c) 1994, 1995  Carnegie Mellon University
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
// This file contains definitions of classes and functions for the Dylan
// deque collection class.  The data structure used for deque is a
// doubly-linked list of element objects.  Element objects each contain a
// piece of data and previous and next element pointers.
//
// Written by David Pierce
//  Translated to infix and optimized by Robert Stockton
//



//; Dylan Deque Class Definition

// The <deque> class defines double ended queues.  A deque allows accesses
// from both ends of the queue.  Used with PUSH and POP, a deque may be
// treated as a stack.  When PUSH-LAST and POP-LAST are used, more
// complicated things can be done.  And the regular sequence and
// collection operations allow one to work with the deque as a whole.
// This implementation of deques uses element objects with pointers to
// represent the deque.

define open abstract class <deque> (<mutable-sequence>, <stretchy-collection>)
//  keyword size:, init-value: 0;
//  keyword fill:, init-value: #f;
end class <deque>;

define sealed inline method make
    (cls == <deque>, #rest rest, #all-keys) => (res :: <simple-object-deque>) 
  apply(make, <simple-object-deque>, rest);
end method make;


// Note: In the code that follows, the terms "state" and "element" will be
// used interchangeably to denote <deque-element> objects.  The choice of
// term used will usually depend on the context.  If the object is used as
// a pointer into the deque structure, the term "state" will probably be
// used.  If it is used as containing a piece of deque data, the term
// "element" may be used.

// Deque operations -- public
//
// These are the main functions to use with deques.
//
define generic push (deque :: <deque>, new) => (new :: <object>);
define generic pop (deque :: <deque>) => (result :: <object>);
define generic push-last (deque :: <deque>, new) => (new :: <object>);
define generic pop-last (deque :: <deque>) => (result :: <object>);

// <deque-element> -- internal
//
// Each <deque-element> has a DEQUE-ELEMENT-DATA slot and slots for the
// prvious and next deque elements.  If there is no PREV-DEQUE-ELEMENT or
// NEXT-DEQUE-ELEMENT, the marker #f should be used in these slots.
//
define sealed class <deque-element> (<object>)
  sealed slot deque-element-data, required-init-keyword: #"data" ;
  sealed slot prev-deque-element :: false-or(<deque-element>), init-value: #f;
  sealed slot next-deque-element :: false-or(<deque-element>), init-value: #f;
end class <deque-element>;

define sealed domain make (singleton(<deque-element>));

// <simple-object-deque> -- public
//
// Deque objects have slots for the head and tail of the queue.  The
// DEQUE-HEAD and DEQUE-TAIL of a deque must always be instances of
// <deque-element>; except when the deque is empty, in which case
// DEQUE-HEAD and DEQUE-TAIL must both be #f.  This invariant must always
// be preserved by the deque functions.
//
define sealed class <simple-object-deque> (<deque>)
  sealed slot deque-head :: false-or(<deque-element>), init-value: #f;
  sealed slot deque-tail :: false-or(<deque-element>), init-value: #f;
  sealed slot size :: <integer>,
    setter: deque-size-setter,
    init-value: 0;
end class <simple-object-deque>;

// initialize -- interface
//
// This initialize method implements the keys specified for deques.  The
// SIZE key indicates the number of elements the deque should initially
// have, and the FILL key indicates what the values of these initial
// elements should be.  I added another key DATA which gives the entire
// contents to the new deque in a sequence.
//
define method initialize (deque :: <simple-object-deque>,
			  #key data, size :: <integer> = 0,
			       fill = #f)
  if (data)
    for (element in data)
      push-last(deque, element);
    end for;
  else
    for (s :: <integer> from 0 below size)
      push-last(deque, fill);
    end for;
  end if;
end method initialize;

define sealed domain make (singleton(<simple-object-deque>));
define sealed domain initialize (<simple-object-deque>);


//; Iteration Protocol

// States in the iteration protocol are deque-elements from the deque that
// you are working with.  The initial state is the first deque element and
// the final state is the last deque element.  The next state is found by
// looking at the NEXT-DEQUE-ELEMENT slot in the deque-element.  The
// current element is the data found in the DEQUE-ELEMENT-DATA slot of the
// deque-element.

define sealed inline method forward-iteration-protocol
    (deque :: <simple-object-deque>)
 => (initial-state :: <object>,
     limit :: <object>,
     next-state :: <function>,
     finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>,
     current-element-setter :: <function>,
     copy-state :: <function>);
  values(deque-head(deque), #f,
	 method (deque :: <simple-object-deque>, state :: <deque-element>)
	   next-deque-element(state);
	 end method,
	 method (deque :: <simple-object-deque>, state, limit)
	   ~state;
	 end method,
	 method (deque :: <simple-object-deque>, state :: <deque-element>)
	  => (result :: <integer>) ;
	   for (count from -1,
		deque_elem = state then prev-deque-element(deque_elem),
		while: deque_elem)
	   finally
	     count;
	   end for;
	 end method,
	 method (deque :: <simple-object-deque>, state :: <deque-element>)
	   deque-element-data(state);
	 end method,
	 method (value, deque :: <simple-object-deque>,
		 state :: <deque-element>)
	   deque-element-data(state) := value;
	 end method,
	 method (deque :: <simple-object-deque>, state :: <deque-element>)
	  => (result :: <deque-element>);
	   state;
	 end method);
end method forward-iteration-protocol;

define sealed inline method backward-iteration-protocol
    (deque :: <simple-object-deque>)
 => (initial-state :: <object>,
     limit :: <object>,
     next-state :: <function>,
     finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>,
     current-element-setter :: <function>,
     copy-state :: <function>);
  values(deque-tail(deque), #f,
	 method (deque :: <simple-object-deque>, state :: <deque-element>)
	   prev-deque-element(state);
	 end method,
	 method (deque :: <simple-object-deque>, state, limit)
	   ~state;
	 end method,
	 method (deque :: <simple-object-deque>, state :: <deque-element>)
	  => (result :: <integer>) ;
	   for (count from -1,
		deque_elem = state then prev-deque-element(deque_elem),
		while: deque_elem)
	   finally
	     count;
	   end for;
	 end method,
	 method (deque :: <simple-object-deque>, state :: <deque-element>)
	   deque-element-data(state);
	 end method,
	 method (value, deque :: <simple-object-deque>,
		 state :: <deque-element>)
	   deque-element-data(state) := value;
	 end method,
	 method (deque :: <simple-object-deque>, state :: <deque-element>)
	  => (result :: <deque-element>);
	   state;
	 end method);
end method backward-iteration-protocol;

// drop! -- internal
//
// DROP! is a nice utility function that is used in the functions below.
// It works with a current deque-element in the deque.  DROP! splices the
// deque-element before and after the given element so that the given
// element is no longer attached to the deque.  In the special case that
// the element is the head or tail (or both in the case of one element
// deque) DROP! adjusts DEQUE-HEAD or DEQUE-TAIL.
//
define method drop! (deque :: <simple-object-deque>,
		     element :: <deque-element>)
  case
    element == deque-head(deque) & element == deque-tail(deque) =>
      // Zero or one elements in deque
      deque-head(deque) := #f;
      deque-tail(deque) := #f;
    element == deque-head(deque) =>
      // ELEMENT is the head of the deque
      deque-head(deque) := next-deque-element(element);
      prev-deque-element(deque-head(deque)) := #f;
    element == deque-tail(deque) =>
      // ELEMENT is the tail of the deque
      deque-tail(deque) := prev-deque-element(element);
      next-deque-element(deque-tail(deque)) := #f;
    otherwise =>
      // ELEMENT is a normal element in the middle of the deque
      next-deque-element(prev-deque-element(element)) :=
        next-deque-element(element);
      prev-deque-element(next-deque-element(element)) :=
	 prev-deque-element(element);
  end case;
  deque.deque-size := size(deque) - 1;
  deque;
end method drop!;


//; Deque Functions

// push -- public
//
// Creates a new deque-element with data new and places it at the
// DEQUE-HEAD of the deque.  If the deque is empty, both DEQUE-HEAD and
// DEQUE-TAIL must be set to the new element.
//
define method push (deque :: <simple-object-deque>, new)
 => (result :: <object>);
  let new-element :: <deque-element> = make(<deque-element>, data: new);
  case
    empty?(deque) =>
      deque-head(deque) := new-element;
      deque-tail(deque) := new-element;
    otherwise =>
      next-deque-element(new-element) := deque-head(deque);
      prev-deque-element(deque-head(deque)) := new-element;
      deque-head(deque) := new-element;
  end case;
  deque.deque-size := size(deque) + 1;
  new;
end method push;

// pop -- public
//
// Removes the first deque-element and returns its DEQUE-ELEMENT-DATA.  If
// the deque is empty, an error is signalled.
//
define method pop (deque :: <simple-object-deque>) => (result :: <object>);
  case
    empty?(deque) =>
      error("POP:  deque empty.");
    deque-head(deque) == deque-tail(deque) =>
      let first-element = deque-head(deque);
      deque.deque-size := 0;
      deque-head(deque) := #f;
      deque-tail(deque) := #f;
      deque-element-data(first-element);
    otherwise =>
      let first-element = deque-head(deque);
      deque.deque-size := size(deque) - 1;
      deque-head(deque) := next-deque-element(first-element);
      prev-deque-element(deque-head(deque)) := #f;
      deque-element-data(first-element);
  end case;
end method pop;

// push-last -- public
//
// Creates a new deque-element and places it at the DEQUE-TAIL of the
// deque.
//
define method push-last (deque :: <simple-object-deque>, new)
 => (result :: <object>);
  let new-element :: <deque-element> = make(<deque-element>, data: new);
  case
    empty?(deque) =>
      deque-head(deque) := new-element;
      deque-tail(deque) := new-element;
    otherwise =>
      prev-deque-element(new-element) := deque-tail(deque);
      next-deque-element(deque-tail(deque)) := new-element;
      deque-tail(deque) := new-element;
  end case;
  deque.deque-size := size(deque) + 1;
  new;
end method push-last;

// pop-last -- public
//
// Removes the last deque-element and returns its DEQUE-ELEMENT-DATA.
//
define method pop-last (deque :: <simple-object-deque>) =>
    (result :: <object>);
  case
    empty?(deque) =>
      error("POP-LAST:  deque empty.");
    deque-head(deque) == deque-tail(deque) =>
      let last-element = deque-tail(deque);
      deque.deque-size := 0;
      deque-head(deque) := #f;
      deque-tail(deque) := #f;
      deque-element-data(last-element);
    otherwise =>
      let last-element = deque-tail(deque);
      deque.deque-size := size(deque) - 1;
      deque-tail(deque) := prev-deque-element(last-element);
      next-deque-element(deque-tail(deque)) := #f;
      deque-element-data(last-element);
  end case;
end method pop-last;



//; Collection Function Methods

// Most of the methods for functions on general collections have been left
// as the default.  This works well because these functions are
// implemented primarily in terms of the iteration protocol.
//
// The collection functions that must be implemented (besides the
// iteration protocol functions) are TYPE-FOR-COPY and MAP-AS.
// Definitions for these are below.

// size-setter -- public
//
// If N is larger than the size of the deque, extra copies of
// #F are pushed to the end until the size is N.  If N
// is smaller than the size of the deque, elements are popped from the end
// until the size is N.
//
define sealed method size-setter
    (n :: <integer>, deque :: <simple-object-deque>)
 => (result :: <integer>);
  let s = size(deque);
  case
    (n == s) => #f;
    (n == 0) => deque.deque-head := deque.deque-tail := #f;
    (n > s) =>
      for (i :: <integer> from 0 below n - s)
	push-last(deque, #f)
      end for;
    (n + n < s) =>		// closer to front
      for (i :: <integer> from 0 below n - 1,
	   state :: false-or(<deque-element>) = deque.deque-head
	     then state.next-deque-element)
      finally
	state.next-deque-element := #f;
	deque.deque-tail := state;
      end for;
    otherwise =>		// closer to back
      for (i :: <integer> from n below s,
	   state :: false-or(<deque-element>) = deque.deque-tail
	     then state.prev-deque-element)
      finally
	state.next-deque-element := #f;
	deque.deque-tail := state;
      end for;
  end case;
  deque.deque-size := n;
end method size-setter;

// ### not absolutly needed
// type-for-copy -- public
//
// Return the class for copy of deques (<deque>).
//
//define method type-for-copy (deque :: <deque>) :: 
//  <deque>;
//end method type-for-copy;

// Since we can traverse from either end, we check to see which end is closer
// to the desired element and take that as our starting point.
//
define sealed method element
    (deque :: <simple-object-deque>, key :: <integer>,
     #key default = $not-supplied) => (result :: <object>);
  let deque-size :: <integer> = deque.size;
  if (key < 0 | key >= deque-size)
    if (default == $not-supplied)
      error("No such element in %=: %d", deque, key)
    else
      default
    end if;
  elseif (key + key > deque-size)	// closer to end than start
    for (cur_key :: <integer> from deque-size - 1 above key by -1,
	 state :: false-or(<deque-element>) = deque.deque-tail
	   then state.prev-deque-element)
    finally state.deque-element-data
    end for;
  else
    for (cur_key :: <integer> from 0 below key,
	 state :: false-or(<deque-element>) = deque.deque-head
	   then state.next-deque-element)
    finally state.deque-element-data
    end for;
  end if;
end method element;

define sealed method element-setter
    (value, deque :: <simple-object-deque>, key :: <integer>)
 => (result :: <object>);
  let sz :: <integer> = deque.size;
  if (key < 0)
    error("No such element in %=: %d", deque, key)
  elseif (key >= sz)
    size(deque) := key + 1;
  end if;
    
  if (key + key > sz)		// closer to end than start
    for (cur_key :: <integer> from sz - 1 above key by -1,
	 state :: false-or(<deque-element>) = deque.deque-tail
	   then state.prev-deque-element)
    finally state.deque-element-data := value;
    end for;
  else
    for (cur_key :: <integer> from 0 below key,
	 state :: false-or(<deque-element>) = deque.deque-head
	   then state.next-deque-element)
    finally state.deque-element-data := value;
    end for;
  end if;
end method element-setter;

// add! -- public
//
// Add a NEW element to a deque destructively.  This is effectively
// the same as "push", but returns a different value.
//
define sealed inline method add! (deque :: <deque>, new)
 => (result :: <deque>);
  push(deque, new);
  deque;
end method add!;

// remove! -- public
//
// Remove up to COUNT copies of VALUE from the deque.  If COUNT is not
// given, remove all copies of VALUE.  The deque is destructively
// modified.  The TEST key allows the equality test to be specified.
//
// The helping function SCAN! runs down the deque, DROP!ping elements
// which match VALUE.  When COUNT runs out it quits.
//
define method remove! (deque :: <simple-object-deque>, value,
		       #key test = \==, count: count)
 => (result :: <simple-object-deque>);
  let count = count | size(deque);
  local method scan!(state :: false-or(<deque-element>),
		     count :: <integer>)
	  case
	    count <= 0 | ~state =>
	      #t;
	    test(deque-element-data(state), value) =>
	      drop!(deque, state);
	      scan!(next-deque-element(state), count - 1);
	    otherwise =>
	      scan!(next-deque-element(state), count);
	  end case;
	end method scan!;
  scan!(deque-head(deque), count);
  deque;
end method remove!;

// last -- public
//
// Returns the last element of the duque.  This is more efficient because
// the last element of a deque can be accessed directly.
//
define sealed method last
    (deque :: <simple-object-deque>, #key default = $not-supplied)
 => (result :: <object>);
  let deque-tail = deque-tail(deque);
  case
    deque-tail =>
      deque-element-data(deque-tail);
    default == $not-supplied =>
      error("No such element in %=:  last.", deque);
    otherwise =>
      default;
  end case;
end method last;

define sealed inline method empty? (deque :: <simple-object-deque>)
 => (result :: <boolean>);
  deque.size == 0;
end method empty?;

// ### not absolutly needed -- (to end of file)
// // map-as -- public
// //
// // This is done by finding the intersection of the key sequences of the
// // collections, and applying the function to each keyed element.  The
// // result of this is pushed onto the end of a result deque.  (Everything
// // is pushed onto the end so that order will be preserved.)
// //
// // It is unclear what this should do when you are mapping from tables, so best
// // we keep it undefined for now.           -rgs
// //
// // define method map-as(cls == <deque>, proc :: <function>,
// //		     collection :: <collection>, #rest more-collections);
// //   let result = make(<deque>);
// //   let keys = reduce(intersection, key-sequence(collection),
// // 		    map(key-sequence, more-collections));
// //   for (key in keys)
// //     push-last(result, apply(proc, collection[key],
// // 			    map(rcurry(element, key), more-collections)));
// //   end for;
// //   result;
// // end map-as;
// 
// //
// // A specialized version of MAP-AS for mapping only sequences into a
// // deque.  Iterates using iteration states and CURRENT-ELEMENT instead of
// // the key sequences.
// //
// // Cut down to avoid hassles with iteration protocol across multiple
// // collections              -rgs
// //
// define method map-as (cls == <deque>, proc :: <function>,
// 		      sequence :: <sequence>,
// 		      #next next-method, #rest more-sequences) => (result ::<deque>);
//   if (empty?(more-sequences))
//     let result = make(<deque>);
//     for (element in sequence)
//       push-last(result, proc(element));
//     end for;
//     result;
//   else 
//     next-method();
//   end if;
// end method map-as;
// 
// //
// // A specialized version of MAP-AS for mapping only deques into a deque.
// // Iterates along the structure of the deques rather than using
// // CURRENT-ELEMENT as an accessor.
// //
// // Modified heavily for efficiency.  -- rgs
// //
// define method map-as (cls == <deque>, proc :: <function>,
// 		     deque :: <deque>, #next next-method, #rest more-deques)
//   case
//     empty?(more-deques) =>
//       let result = make(<deque>);
//       for (element = deque-head(deque) then next-deque-element(element),
// 	   while: element)
// 	push-last(result, proc(deque-element-data(element)));
//       end for;
//       result;
//     every?(rcurry(instance?,<deque>), more-deques) =>
//       let result = make(<deque>);
//       let more-vals = make(<vector>, size: size(more-deques));
//       for (element = deque-head(deque) then next-deque-element(element),
// 	   more-elements = map-as(<vector>, deque-head, more-deques)
// 	     then map-into(more-elements, next-deque-element, more-elements),
// 	   while: element & every?(identity, more-elements))
// 	map-into(more-vals, deque-element-data, more-elements);
// 	push-last(result, apply(proc, deque-element-data(element), more-vals));
//       end for;
//       result;
//     otherwise =>
//       next-method();
//   end case;
// end map-as;
// 
// define method map-into (destination :: <deque>,
// 			proc :: <function>, sequence :: <sequence>,
// 			#next next_method, #rest more_sequences)
//   if (empty?(more_sequences))
//     for (elem in sequence,
// 	 state = destination.deque-head then state & state.next-deque-element)
//       if (state) state.deque-element-data := proc(elem)
//       else push-last(destination, proc(elem))
//       end if;
//     end for;
//     destination;
//   else
//     next_method();
//   end if;
// end method map-into;
// 
// 
// 
// // Sequence Function Methods
// 
// // More of the sequence functions have been specialized for deques because
// // they are improved more than the general collection functions by
// // knowledge of the data structure.
// //
// // Especially useful are the specializations of the mutator functions
// // (ones that end in !) which are allowed to change the data structure.
// // These can be made very space and time efficient for deques.  However,
// // the code for these functions also looks messier because it grunges
// // around with the structure of the deque object.
// //
// // Other functions will usually use either recursive helper functions
// // (especially those with funny keys for counting the number of things to
// // remove and so forth, because it is easier to deal with things that way)
// // or more abstract forms such as for-each.
// //
// // Some of the sequence functions have been specialized not for
// // efficiency, but for order stability.  Since deques are by nature an
// // ordered data structure, order should be preserved in deque operations,
// // and this cannot be guaranteed in the general sequence methods.
// 
// // add -- public
// //
// // Create a new deque with the NEW element added to it.  Add must
// // function similarly to add!, so this adds to the front of the
// // deque.
// //
// define method add (deque :: <deque>, new) => (result :: <deque>);
//   let new-deque = copy-sequence(deque);
//   push(new-deque, new);
// end method add;
// 

// // remove -- public
// //
// // Create a new deque with COUNT copies of element VALUE removed from it.
// // If COUNT is not given, remove all copies of VALUE from the new deque.
// // The TEST key always the equality test to be specified.
// //
// // The helper function COPY runs down the deque creating a copy, but
// // skipping the element VALUE as long as COUNT does not run out.
// //
// define method remove (deque :: <deque>, value,
// 		      #key test = \==, count) => (result :: <deque>);
//   let count = count | size(deque);
//   local method copy(state :: false-or(<deque-element>),
// 		    count :: <integer>) => (result :: <deque>);
// 	  case
// 	    ~state =>
// 	      make(<deque>);
// 	    count <= 0 | ~test(deque-element-data(state), value) =>
// 	      push(copy(next-deque-element(state), count),
// 		   deque-element-data(state));
// 	    otherwise =>
// 	      copy(next-deque-element(state), count - 1);
// 	  end case;
// 	end method;
//   copy(deque-head(deque), count);
// end method remove;
// 
// // choose -- public
// //
// // Creates a new deque containing only those elements which satisfy
// // PREDICATE.  Uses FOR-EACH to check every element of DEQUE and appends
// // those which satisfy to the new RESULT deque.
// //
// define method choose (predicate :: <function>, deque :: <deque>) => (result :: <deque>);
//   let result = make(<deque>);
//   for (element in deque)
//     if (predicate(element)) push-last(result, element) end if;
//   end for;
//   result;
// end method choose;
// 
// // choose-by -- public
// //
// // Creates a new deque containing only those elements of VALUE-DEQUE which
// // correspond to elements in TEST-SEQUENCE which satisfy PREDICATE.  Uses
// // FOR-EACH to check each element of DEQUE and append good ones to RESULT.
// //
// define method choose-by (predicate :: <function>, test-sequence :: <sequence>,
// 			 value-deque :: <deque>) => (result :: <deque>);
//   let result = make(<deque>);
//   for (test-element in test-sequence,
//        value-element in value-deque)
//     if (predicate(test-element)) push-last(result, value-element) end if;
//   end for;
//   result;
// end method choose-by;
// 
// // remove-duplicates -- public
// //
// // Remove duplicate items from DEQUE.  The test for duplicates may be
// // specified by the TEST keyword.
// //
// // The helper function MEMBER? checks whether a value occurs further down
// // the deque from a particular state.  The helper function COPY returns a
// // copy of the deque, but whenever it encounters a value that occurs later,
// // it is skipped.
// //
// define method remove-duplicates (deque :: <deque>,
// 				 #key test = \==) => (result :: <deque>);
//   local method member?(value, state)
// 	  for (state = state then next-deque-element(state),
// 	       while: state & ~test(value, deque-element-data(state)))
// 	  finally
// 	    state;
// 	  end for;
// 	end method member?,
//         method copy(state)
// 	  case
// 	    ~state =>
// 	      make(<deque>);
// 	    member?(deque-element-data(state), next-deque-element(state)) =>
// 	      copy(next-deque-element, state);
// 	    otherwise =>
// 	      push(copy(next-deque-element(state)), deque-element-data(state));
// 	  end case;
// 	end method copy;
//   copy(deque-head(deque));
// end method remove-duplicates;
// 
// // remove-duplicates! -- public
// //
// // Remove duplicate items from DEQUE.  The test for duplicates may be
// // specified by the TEST keyword.  The deque is destructively modified by
// // REMOVE-DUPLICATES!.
// //
// //The helper function MEMBER? checks whether a value occurs further down
// // the deque from a particular state.  The helper function SCAN! runs down
// // the deque, and whenever it encounters a value that occurs later, it
// // drops the element.
// //
// define method remove-duplicates! (deque :: <deque>,
// 				  #key test = \==) => (result :: <deque>);
//   local method member?(value, state)
// 	  for (state = state then next-deque-element(state),
// 	       while: state & ~test(value, deque-element-data(state)))
// 	  finally state;
// 	  end for;
// 	end method member?,
//         method scan!(state)
// 	  case
// 	    ~state =>
// 	      #t;
// 	    member?(deque-element-data(state), next-deque-element(state)) =>
// 	      drop!(deque, state);
// 	      scan!(next-deque-element(state));
// 	    otherwise =>
// 	      scan!(next-deque-element(state));
// 	  end case;
// 	end method scan!;
//   scan!(deque-head(deque));
//   deque;
// end method remove-duplicates!;
// 
// // copy-sequence -- public
// //
// // Returns a copy of the deque which shares no structure with the
// // original.  The keyword START gives an inclusive beginning to the copy;
// // it defaults to 0.  The keyword END gives an exclusive end to the copy;
// // it defaults to the length of the deque.
// //
// // The helper function COPY runs down the deque pushing elements onto a
// // copy of the original.  It starts pushing elements at the STARTth
// // element and stops before the ENDth element or at the end of the deque.
// //
// define method copy-sequence (source :: <deque>,
// 			     #key start: first = 0, end: last) => (result :: <deque>);
//   let last = last | size(source);
//   if (first > last) 
//     error("End: (%=) is smaller than start: (%=)", last, first);
//   end if;
// 
//   local method copy(state, first, last)
// 	  case
// 	    ~state =>
// 	      make(<deque>);
// 	    first > 0 =>
// 	      copy(next-deque-element(state), first - 1, last - 1);
// 	    last > 0 =>
// 	      push(copy(next-deque-element(state), first, last - 1),
// 		   deque-element-data(state));
// 	    otherwise =>
// 	      make(<deque>);
// 	  end case;
// 	end method copy;
//   copy(deque-head(source), first, last);
// end method copy-sequence;
// 
// // concatenate-as -- public
// //
// // A new deque is made, each sequence is taken one at a time, and each
// // element of the sequence is pushed onto the end of the new deque.  The
// // deque is returned.
// //
// define method concatenate-as (cls == <deque>, sequence :: <sequence>,
// 			      #rest more-sequences)
//   let result = make(<deque>);
//   for (element in sequence) push-last(result, element) end for;
//   for (sequence in more-sequences)
//     for (element in sequence) push-last(result, element) end for;
//   end for;
//   result;
// end method concatenate-as;
// 
// // replace-subsequence! -- public
// //
// // Uses the default method.
// 
// // reverse -- public
// //
// // Creates a new deque which is the reversal of DEQUE.  Uses FOR-EACH to
// // push each element onto the RESULT deque.  Since PUSH is used, the
// // resulting deque is backwards.
// //
// define method reverse (deque :: <deque>) => (result :: <deque>);
//   let result = make(<deque>);
//   for (element in deque) push(result, element) end for;
//   result;
// end method reverse;
// 
// // reverse! -- public
// //
// // Reverses DEQUE destructively.  Uses a FOR loop to run down the deque.
// // Each element's NEXT-DEQUE-ELEMENT and PREV-DEQUE-ELEMENT pointers are
// // swapped.  Finally, the DEQUE-HEAD and DEQUE-TAIL of the deque are
// // swapped.  This reverses the deque using the original deque elements.
// //
// define method reverse! (deque :: <deque>) => (result :: <deque>);
//   for (state = deque-head(deque) then prev-deque-element(state),
//        while: state)
//     let (prev, next) = values(prev-deque-element(state),
// 			      next-deque-element(state));
//     prev-deque-element(state) := next;
//     next-deque-element(state) := prev;
//   end for;
//   let (head, tail) = values(deque-head(deque), deque-tail(deque));
//   deque-head(deque) := tail;
//   deque-tail(deque) := head;
//   deque;
// end method reverse!;
//
// // last-setter -- Set last element of deque.
// //
// // Corresponding efficient implementation for the setter of LAST.
// //
// define method last-setter(new, deque :: <deque>)
//   let deque-tail = deque-tail(deque);
//   if (deque-tail)
//     deque-element-data(deque-tail) := new;
//   else
//     error("No such element in %=:  last.", deque);
//   end if;
// end method last-setter;
// 
