module: dylan
rcs-header: $Header: /scm/cvs/src/mindy/libraries/dylan/list.dylan,v 1.1 1998/05/03 19:55:21 andreas Exp $

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
//  This file contains the support for lists that isn't built in.
//



//// Construction.

define method make
    (cls == <list>, #rest keys, #key size = 0, fill = #f) 
 => l :: <list>;
  let result = for (i from 0 below size,
		    list = #() then pair(fill, list))
	       finally
		 list;
	       end for;
  apply(initialize, result, keys);
  result;
end method make;


// Note: list(...) is built into Mindy.



//// Iteration protocol.

define constant list_fip_next_state =
  method (list :: <list>, state :: <list>) => next :: <list>;
    tail(state);
  end method;

define constant list_fip_finished-state? =
  method (list :: <list>, state :: <list>, limit)
    state == #();
  end method;

define constant list_fip_current_key =
  method (list :: <list>, state :: <list>) => cur-key :: <integer>;
    for (key from 0,
	 scan = list then tail(scan),
	 until: scan == state)
      if (scan == #())
	error("State not part of list?");
      end;
    finally
      key;
    end for;
  end method;


define constant list_fip_current_element =
  method (list :: <list>, state :: <list>) => cur-elt :: <object>;
    head(state);
  end method;

define constant list_fip_current_element-setter =
  method (value :: <object>, list :: <list>, state :: <list>)
   => value :: <object>;
    head(state) := value;
  end method;

define constant list_fip_copy_state =
  method (list :: <list>, state :: <list>) => state :: <list>;
    state;
  end method;

define method forward-iteration-protocol (list :: <list>)
 => (initial-state :: <object>,
     limit :: <object>,
     next-state :: <function>,
     finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>,
     current-element-setter :: <function>,
     copy-state :: <function>);
  values(list, #f, list_fip_next_state, list_fip_finished-state?,
	 list_fip_current_key, list_fip_current_element,
	 list_fip_current_element-setter, list_fip_copy_state);
end method forward-iteration-protocol;


//// Collection routines.

// Note: size(<list>) is built into Mindy.

define method empty?(list :: <empty-list>) => answer :: <boolean>;
  #t;
end method empty?;

define method empty?(list :: <pair>) => answer :: <boolean>;
  #f;
end method empty?;

define method type-for-copy(list :: <list>) => type :: <class>;
  <list>;
end method type-for-copy;

define method member? (value, l :: <list>, #key test: test = \==)
 => answer :: <boolean>;
  let done        = #f;
  let lapped-slow = #f;                // Has fast lapped slow?

  block (return)
    for (slow = l        then tail (slow),
	 fast = tail (l) then if (lapped-slow) fast;
			      else tail (tail (fast))
			      end if,
	 until: done | slow == #() )

      if (test (value, head (slow)))
	return(#t);
      elseif (fast == slow)
	done   := lapped-slow;    // Since fast goes twice the speed,
	                          // need to give slow a chance to
	                          // catch up.
	lapped-slow := #t;
      end if;
    end for;

    #f;     // If we've gotten this far, the for loop didn't find the element
  end block;
end method member?;

define method map (proc :: <function>, 
		   collection :: <empty-list>, 
		   #rest more) => l :: <list>;
  #();
end method map;

define method map-as (a_class :: singleton (<list>), proc :: <function>,
		      l :: <list>, #next next-method, #rest more-lists)
 => result :: <list>;
  if (every? (rcurry ( instance?, <list> ), more-lists))
    for (l          = l          then tail (l),
	 more-lists = more-lists then map (tail, more-lists),
	 result     = #()        then pair (apply (proc, head (l),
						   map (head, more-lists)),
					    result),
	 until: ( l == #() ) | any? (rcurry (\==, #()), more-lists))
    finally
      reverse! (result);
    end for;

  else
    next-method ();
  end if;
end method map-as;

define method any?   (proc :: <function>, l :: <empty-list>, #rest more)
 => answer :: <boolean>;
  #f;
end method any?;

define method every? (proc :: <function>, l :: <empty-list>, #rest more)
 => answer :: <boolean>;
  #t;
end method every?;


//// Sequence routines.

define method add  (l :: <list>, new) => new-list :: <list>;
  pair(new, l);
end method add;

define method add! (l :: <list>, new) => new-list :: <list>;
  pair (new, l);
end method add!;

define method remove  (l :: <list>, value, #key test: test = \==,
		       count: count) => new-l :: <list>;
  let result    = #();
  let remaining = l;

  until ( remaining == #() )
    if ( (count ~= 0) & test (head (remaining), value) )
      remaining := tail (remaining);
      count     := count & (count - 1);         // False if undefined,
			                        // count - 1 otherwise
    else
      result    := pair (head (remaining), result);
      remaining := tail (remaining);
    end if;
  end until;

  reverse! (result);
end method remove;
      
define method remove! (l :: <list>, value, #key test: test = \==,
		       count: count) => new-l :: <list>;
  let result    = l;
  let prev      = #f;
  let remaining = l;

  until ( remaining == #() )
    if (count = 0 | ~ (test (head (remaining), value)))
      prev      := remaining;
      remaining := tail (remaining);
    elseif (prev)
      tail (prev) := tail (remaining);
      remaining   := tail (remaining);
      count       := count & (count - 1);
    else
      result      := tail (remaining);
      prev        := #f;
      remaining   := tail (remaining);
      count       := count & (count - 1);
    end if;
  end until;

  result;
end method remove!;

// If there are duplicates, this returns the LAST identical element,
// and not the first like the example on page 107 would indicate.

define method remove-duplicates (l :: <list>, #key test: test = \== )
 => new-l :: <list>;
  let result    = #();
  let prev      = #f;
  let remaining = l;

  until ( remaining == #() )
    if (member? (head (remaining), tail (remaining), test: test))
      remaining   := tail (remaining);
    elseif (prev)
      let next = list (head (remaining));
      tail (prev) := next;
      prev        := next;
      remaining   := tail (remaining);
    else
      let new = list (head (remaining));
      result      := new;
      prev        := new;
      remaining   := tail (remaining);
    end if;
  end until;

  result;
end method remove-duplicates;

define method remove-duplicates! ( l :: <list>, #key test: test = \== )
 => new-l :: <list>;
  let result    = l;
  let prev      = #f;
  let remaining = l;
  
  until ( remaining == #() )
    if ( ~ member? (head (remaining), tail (remaining), test: test))
      prev        := remaining;
      remaining   := tail (remaining);
    elseif (prev)
      tail (prev) := tail (remaining);
      remaining   := tail (remaining);
    else
      result      := tail (remaining);
      prev        := #f;
      remaining   := tail (remaining);
    end if;
  end until;

  result;
end method remove-duplicates!;

define method replace-subsequence! (l :: <list>, seq :: <sequence>,
				    #key start: start = 0, end: stop)
 => new-l :: <list>;
  let result = pair (#f, l);
  let prev   = result;

  for (i from 1 to start)
    prev := tail (prev);
  end for;

  if (~ stop)
    stop := start + size (seq);
  end if;

  let after-hole = for (after-hole = tail (prev) then tail (after-hole),
			index = start then index + 1,
			until: index = stop)
		   finally after-hole;
		   end for;

  for (elt in seq)
    let next = pair (elt, #f);
    tail (prev) := next;
    prev        := next;
  end for;
  
  tail (prev) := after-hole;
  tail (result);
end method replace-subsequence!;

define method reverse  (l :: <list>) => l :: <list>;
  let result = #();
  let remaining = l;

  until ( remaining == #() )
    result := pair (head (remaining), result);
    remaining := tail (remaining);
  end until;

  result;
end method reverse;

define method reverse! (l :: <list>) => l :: <list>;
  let result    = #();
  let remaining = l;

  until ( remaining == #() )
    let t = tail (remaining);
    tail (remaining) := result;
    result           := remaining;
    remaining        := t;
  end until;

  result;
end method reverse!;


//// =

// We have to define a method on <list>/<list>, because = is defined to
// work on dotted lists, and the sequence version of = will try calling
// every?, which will flame out on dotted lists.

// Will be called when you compare an <empty-list> to a <pair>
// or vice versa.
define method \= (a :: <list>, b :: <list>) => answer :: <boolean>;
  #f;
end method \=;


define method \= (a :: <empty-list>, b :: <empty-list>) => answer :: <boolean>;
  #t;
end method \=;


define method \= (a :: <pair>, b :: <pair>) => answer :: <boolean>;
  ( head (a) = head (b) )  &  ( tail (a) = tail (b) );
end method \=;
