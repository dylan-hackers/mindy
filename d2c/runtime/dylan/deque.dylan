module: dylan-viscera
copyright: see below

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998 - 2004       Gwydion Dylan Maintainers
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

//  This file implements deques.


// <deque>

define open abstract class <deque> (<stretchy-sequence>, <mutable-sequence>)
end class <deque>;


// Deque operations -- public
//
// These are the main functions to use with deques.
//
define open generic push (deque :: <deque>, new) => (new :: <object>);
define open generic pop (deque :: <deque>) => (result :: <object>);
define open generic push-last (deque :: <deque>, new) => (new :: <object>);
define open generic pop-last (deque :: <deque>) => (result :: <object>);


define open abstract class <builtin-deque> (<deque>)
  //
  // The current size of the deque.
  sealed slot deq-current-size :: <integer> = 0;
  sealed slot deq-current-offset :: <integer> = 0;

  // masked against offset into deq-data to implement wrap
  sealed slot deq-data-mask :: <integer> = 0;
end class <builtin-deque>;


define open generic deq-data (dq :: <builtin-deque>);


define open generic deq-data-setter
    (value :: <object>, dq :: <builtin-deque>);


define sealed inline method make
    (class == <deque>,
     #key size :: <integer> = 0, capacity = #f, fill = #f)
    => res :: <object-deque>;
  make(<object-deque>, size: size, capacity: capacity, fill: fill);
end method;


define sealed inline method size (deq :: <builtin-deque>)
    => size :: <integer>;
  deq.deq-current-size;
end method size;



// <object-deque>

// Invariants:
//   1. deq-current-size <= deq-data.size
//   2. elements [deq-current-offset .. deq-current-offset + deq-current-size-1]
//      modulo deq-data.size contain user-supplied data
//   3. all other elements contain #f (forced so deleted elements can be GC'd)
//
define sealed class <object-deque> (<builtin-deque>)
  //
  // A <simple-object-vector> holding the vector elements.  Obviously
  // at least as long as the deque.
  sealed slot deq-data :: <simple-object-vector> = #[];
end class <object-deque>;

define sealed domain make(singleton(<object-deque>));


define function calc-deque-size(new :: <integer>)
 => new :: <integer>;
  if (new < 0)
    error("size: can't be negative.");
  end;
  for (new-len = 4 then new-len * 2,
       until: new <= new-len)
  finally
    // *must* be a power of two so that we can use masking
    new-len
  end for;
end calc-deque-size;


define sealed inline method initialize
    (object :: <object-deque>,
     #key size :: <integer> = 0, capacity = #f, fill = #f)
 => ();
  let data-size = calc-deque-size
    ((capacity & capacity > size & capacity) | size);

  // The "fill:" keyword assures that elements above deq-current-size
  // will be #f...
  let data = make(<simple-object-vector>, size: data-size);

  if (fill)
    for (i from 0 below size)
      %element(data, i) := fill;
    end;
    for (i from size below data-size)
      %element(data, i) := #f;
    end;
  else
    for (i from 0 below data-size)
      %element(data, i) := #f;
    end;
  end;

  object.deq-data := data;
  object.deq-current-size := size;
  object.deq-data-mask := data-size - 1;
end method initialize;


define method size-setter
    (new :: <integer>, deq :: <object-deque>)
    => new :: <integer>;
  let current = deq.deq-current-size;
  let data = deq.deq-data;
  if (new > current)
    let len = data.size;
    if (new > len)
      let new-len = calc-deque-size(new);
      let new-data = make(<simple-object-vector>, size: new-len);
      let offset = deq.deq-current-offset;
      let mask = deq.deq-data-mask;
      for (i from 0 below current)
	%element(new-data, i) := %element(data, logand(i + offset, mask));
      end for;
      for (i from current below new-len)
        %element(new-data, i) := #f;
      end;
      deq.deq-data := new-data;
      deq.deq-current-offset := 0;
      deq.deq-data-mask := new-len - 1;
    else
      let offset = deq.deq-current-offset;
      let mask = deq.deq-data-mask;
      for (i from len below new)
        %element(data, logand(i + offset, mask)) := #f;
      end;
    end if;
  else
    let offset = deq.deq-current-offset;
    let mask = deq.deq-data-mask;
    for (i from new below current)
      %element(data, logand(i + offset, mask)) := #f;
    end;
  end if;
  deq.deq-current-size := new;
end method size-setter;


define sealed inline method element
    (deq :: <object-deque>, key :: <integer>,
     #key default = $not-supplied)
    => result :: <object>;
  case
    (key >= 0 & key < deq.size) =>
      // Warning: unchecked reference.  However, if we're *sure* we
      // satisfy the invariants, we're be safe.
      %element(deq.deq-data, logand(key + deq.deq-current-offset, deq.deq-data-mask));
    (default == $not-supplied) =>
      element-error(deq, key);
    otherwise =>
      default;
  end case;
end method element;


define sealed inline method element-setter
    (value, deq :: <object-deque>, key :: <integer>)
    => value :: <object>;
  if (key < 0)
    element-error(deq, key);
  else
    if (key >= deq.size)
      deq.size := key + 1;
    end if;
    // Warning: unchecked reference.  However, if we're *sure* we
    // satisfy the invariants, we're be safe.
    %element(deq.deq-data,
             logand(key + deq.deq-current-offset, deq.deq-data-mask))
      := value;
  end if;
end method element-setter;


define inline method %element
    (v :: <object-deque>,
     i :: <integer>)
 => (obj :: <object>);
  %element(v.deq-data, logand(i + v.deq-current-offset, v.deq-data-mask));
end;


define inline method %element-setter
    (newVal,
     v :: <object-deque>,
     i :: <integer>)
 => (obj :: <object>);
  %element(v.deq-data, logand(i + v.deq-current-offset, v.deq-data-mask))
    := newVal;
end;


// This method is identical to the one in "array.dylan", except that it
// is more tightly specialized to a single sealed class.  If you need to 
// make a general change, you should probably grep for "outlined-iterator" 
// and change all matching locations.
//
define inline method forward-iteration-protocol
    (array :: <object-deque>)
    => (initial-state :: <integer>,
	limit :: <integer>,
	next-state :: <function>,
	finished-state? :: <function>,
	current-key :: <function>,
	current-element :: <function>,
	current-element-setter :: <function>,
	copy-state :: <function>);
  values(0,
	 array.size,
	 method (array :: <object-deque>, state :: <integer>)
	     => new-state :: <integer>;
	   state + 1;
	 end,
	 method (array :: <object-deque>, state :: <integer>,
		 limit :: <integer>)
	     => done? :: <boolean>;
	   // We use >= instead of == so that the constraint propagation
	   // stuff can tell that state is < limit if this returns #f.
	   state >= limit;
	 end,
	 method (array :: <object-deque>, state :: <integer>)
	     => key :: <integer>;
	   state;
	 end,
	 method (array :: <object-deque>, state :: <integer>)
	     => element :: <object>;
	   element(array, state);
	 end,
	 method (new-value :: <object>, array :: <object-deque>,
		 state :: <integer>)
	     => new-value :: <object>;
	   element(array, state) := new-value;
	 end,
	 method (array :: <object-deque>, state :: <integer>)
	     => state-copy :: <integer>;
	   state;
	 end);
end;


define inline method empty? (deq :: <object-deque>)
 => res :: <boolean>;
  deq.size == 0;
end method;



//; Deque Functions

define sealed method grow-deque(deque :: <object-deque>)
 => (new-data :: <simple-object-vector>);
  let data = deque.deq-data;
  let current = deque.size;
  unless (current == data.size)
    error("grow-deque called when deque not full");
  end;

  let offset = deque.deq-current-offset;
  let mask = deque.deq-data-mask;
  let new-size = current * 2;
  let new-data = make(<simple-object-vector>, size: new-size);
  for (i from 0 below current)
    %element(new-data, i) := %element(data, logand(i + offset, mask));
  end;
  deque.deq-data := new-data;
  deque.deq-current-offset := 0;
  deque.deq-data-mask := new-size - 1;
  new-data;
end method grow-deque;


// push -- public
//
// Creates a new deque-element with data new and places it at the
// DEQUE-HEAD of the deque.  If the deque is empty, both DEQUE-HEAD and
// DEQUE-TAIL must be set to the new element.
//
define sealed inline method push (deque :: <object-deque>, new)
 => (result :: <object>);
  let data = deque.deq-data;
  let current = deque.size;
  if (current == data.size)
    data := grow-deque(deque);
  end;
  let new-offset = logand(deque.deq-current-offset - 1, deque.deq-data-mask);
  %element(data, new-offset) := new;
  deque.deq-current-offset := new-offset;
  deque.deq-current-size := current + 1;
  new;
end method push;


// push-last -- public
//
// Creates a new deque-element and places it at the DEQUE-TAIL of the
// deque.
//
define sealed inline method push-last (deque :: <object-deque>, new)
 => (result :: <object>);
  let data = deque.deq-data;
  let current = deque.size;
  if (current == data.size)
    data := grow-deque(deque);
  end;
  let offset = logand(deque.deq-current-offset + current, deque.deq-data-mask);
  %element(data, offset) := new;
  deque.deq-current-size := current + 1;
  new;
end method push-last;


// pop -- public
//
// Removes the first deque-element and returns its DEQUE-ELEMENT-DATA.  If
// the deque is empty, an error is signalled.
//
define sealed inline method pop (deque :: <object-deque>)
 => (result :: <object>);
  let current = deque.size;
  if (current = 0)
    error("POP:  deque empty.");
  end;
  let data = deque.deq-data;
  let offset = deque.deq-current-offset;
  let elt = %element(data, offset);
  %element(data, offset) := #f;
  deque.deq-current-offset := logand(offset + 1, deque.deq-data-mask);
  deque.deq-current-size := current - 1;
  elt;
end method pop;


// pop-last -- public
//
// Removes the last deque-element and returns its DEQUE-ELEMENT-DATA.
//
define sealed inline method pop-last (deque :: <object-deque>)
 => (result :: <object>);
  let current = deque.size;
  if (current = 0)
    error("POP-LAST:  deque empty.");
  end;
  let data = deque.deq-data;
  let offset = logand(deque.deq-current-offset + current - 1,
                      deque.deq-data-mask);
  let elt = %element(data, offset);
  %element(data, offset) := #f;
  deque.deq-current-size := current - 1;
  elt;
end method pop-last;


// Add a NEW element to a deque destructively.  This is effectively
// the same as "push", but returns a different value.
//
define sealed inline method add! (deque :: <deque>, new)
 => (result :: <deque>);
  push(deque, new);
  deque;
end method add!;


define sealed inline method add! (deque :: <object-deque>, new)
 => (result :: <object-deque>);
  push(deque, new);
  deque;
end method add!;


define method remove! (deq :: <object-deque>, elem,
                       #key test :: false-or(<function>) = \==,
                       count :: false-or(<integer>))
 => deq :: <object-deque>;
  unless (count & (count == 0))
    let data = deq.deq-data;
    let sz = deq.size;
    let offset = deq.deq-current-offset;
    let mask = deq.deq-data-mask;
    local
      method copy (src :: <integer>, dst :: <integer>,
		   deleted :: <integer>)
	  => ();
	case
	  src == sz =>
	    deq.deq-current-size := sz - deleted;
          otherwise =>
            %element(data, logand(dst + offset, mask))
              := %element(data, logand(src + offset, mask));
	    copy(src + 1, dst + 1, deleted);
	end case;
      end method copy,
      method search-and-copy (src :: <integer>, dst :: <integer>,
			      deleted :: <integer>)
	  => ();
	if (src == sz)
	  deq.deq-current-size := sz - deleted;
	else 
	  let this-element = %element(data, logand(src + offset, mask));
	  case
	    test(this-element, elem) =>
	      let deleted = deleted + 1;
	      if (count & (deleted == count))
		copy(src + 1, dst, deleted);
	      else
		search-and-copy(src + 1, dst, deleted);
	      end if;
            otherwise =>
              %element(data, logand(dst + offset, mask))
                := %element(data, logand(src + offset, mask));
	      search-and-copy(src + 1, dst + 1, deleted);
	  end case;
	end if;
      end method search-and-copy,
      method search (src :: <integer>) => ();
	unless (src == sz)
	  let this-element = %element(data, logand(src + offset, mask));
	  if (test(this-element, elem))
	    if (count & (count == 1))
	      copy(src + 1, src, 1);
	    else 
	      search-and-copy(src + 1, src, 1);
	    end if;
	  else
	    search(src + 1);
	  end if;
	end unless;
      end method search;
    search(0);
  end unless;
  deq;
end method remove!;


define sealed method concatenate!
    (deq :: <object-deque>, #rest more-sequences)
 => (deq :: <object-deque>)
  let current = deq.size;

  let new-size = current;
  for(sequence in more-sequences)
    let seq-size :: <integer> = sequence.size;
    new-size := new-size + seq-size;
  end;
    
  if (new-size >= deq.deq-data.size)
    let new-size = calc-deque-size(new-size);
    let data = deq.deq-data;
    let offset = deq.deq-current-offset;
    let mask = deq.deq-data-mask;
    let new-data = make(<simple-object-vector>, size: new-size);
    for (i from 0 below current)
      %element(new-data, i) := %element(data, logand(i + offset, mask));
    end;
    deq.deq-data := new-data;
    deq.deq-current-offset := 0;
    deq.deq-data-mask := new-size - 1;
  end if;

  let data = deq.deq-data;
  let offset = deq.deq-current-offset;
  let mask = deq.deq-data-mask;
  for(sequence in more-sequences,
      outer-index = current
        then for(item in sequence, index from outer-index)
               %element(data, logand(index + offset, mask)) := item;
             finally
               index;
             end)
  end;
  
  deq.deq-current-size := new-size;
  deq;
end method concatenate!;


define sealed method copy-sequence
    (sequence :: <object-deque>, #key start :: <integer> = 0, end: last)
 => (result :: <object-deque>);
  let seq-size :: <integer> = sequence.size;
  let last :: <integer> = last | seq-size;
  case
    (last > seq-size) => error("End: (%=) out of range.", last);
    (start < 0) => error("Start: (%=) out of range.", start);
    (start > last) => error("Start: (%=) > End: (%=).", start, last);
  end case;

  let sz :: <integer> = last - start;
  let result = make(<object-deque>, size: sz);

  let from-data = sequence.deq-data;
  let from-mask = sequence.deq-data-mask;
  let from-start = start + sequence.deq-current-offset;
  let to-data = result.deq-data;

  for (i from 0 below sz)
    %element(to-data, i) :=
      %element(from-data, logand(from-start + i, from-mask));
  end;

  result;
end method copy-sequence;


define sealed method reverse! (deq :: <object-deque>)
 => (res :: <object-deque>);
  let current = deq.size;
  let data = deq.deq-data;
  let offset = deq.deq-current-offset;
  let mask = deq.deq-data-mask;
  for (left from offset,
       right from current + offset - 1 by -1,
       while: left < right)
    let l = logand(left, mask);
    let r = logand(right, mask);
    let ltmp = %element(data, l);
    let rtmp = %element(data, r);
    %element(data, l) := rtmp;
    %element(data, r) := ltmp;
  end;
  deq;
end method reverse!;


define inline sealed method as
    (class == <deque>, collection :: <collection>)
 => (res :: <object-deque>);
  let res = make(<object-deque>, size: collection.size);
  let data = res.deq-data;
  for (index from 0, elt in collection)
    %element(data, index) := elt;
  end;
  res;
end;


define inline method as
    (class == <deque>, deq :: <deque>)
 => (res :: <deque>);
  deq;
end;


define inline method map-into (destination :: <object-deque>,
                               proc :: <function>, sequence :: <sequence>,
                               #next next-method, #rest more-sequences)
    => res :: <deque>;
  if (empty?(more-sequences))
    let sz = size(sequence);
    if (sz == #f)
      error("Cannot map unbounded sequences into deques.");
    elseif (sz > size(destination))
      size(destination) := sz
    end if;
    let data = destination.deq-data;
    let offset = destination.deq-current-offset;
    let mask = destination.deq-data-mask;
    for (key :: <integer> from 0, elem in sequence)
      %element(data, logand(key + offset, mask)) := proc(elem);
    end for;
    destination;
  else
    next-method();
  end if;
end method map-into;
