module: Dylan-viscera
rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/range.dylan,v 1.4 2003/06/03 02:11:32 housel Exp $

//======================================================================
//
// Copyright (c) 1994, 1995  Carnegie Mellon University
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
//
// This file contains definitions of classes and functions for the
// Dylan range collection class.  Ranges represent linear arithmetic
// sequences, which may be infinitely long.
//



/* Dylan Range Class Definition

   Objects of the class <range> represent linear arithmetic sequences
   (here sequence is the mathematical term as well as the collection
   term).  Ranges are special collections because they may be
   infinitely long.

   A range is defined by six keyword arguments to the constructor
   function RANGE -- from:, by:, to:, above:, below:, and size:.  Any
   of these may be given or omitted; the behavior of the range depends
   on the combination of keywords given.  The FROM and BY keywords
   have default values 0 and 1 respectively.  The range created begins
   at FROM and increases by an increment of BY.

   The endpoint of the range is determined by the combination of the
   to:, above:, below:, and size: keywords.  TO is an inclusive bound
   independent of the direction of the range.  ABOVE is an exclusive
   lower bound and BELOW is and exclusive upper bound.  The range will
   have no more than SIZE elements.

   The range representation used in this code is simplified so that
   only the from, by, and a size value need to be stored.  The
   original representation (using TO, ABOVE, or whatever) is
   translated to this representation by the function
   COMPUTE-RANGE-SIZE.

*/

// <range> -- public
// 
// The <range> abstract class represents ranges (linear arithmetic
// sequences).  The class has slots to store the FROM and BY
// parameters of the range and a virtual slot RANGE-DIRECTION.
// 
// The concrete subclasses that implement the range protocol are
// <bounded-range> and <unbounded-range>.
//
define open abstract primary class <range> (<sequence>) end class;

define sealed class <builtin-range> (<range>)
   slot range-from :: <real>,
      init-value: 0,
      init-keyword: #"from";
   slot range-by :: <real>,
      init-value: 1,
      init-keyword: #"by";
//   virtual slot range-direction,
//      setter: #f;
end class;

// range-direction -- internal
// 
// This implements the virtual slot RANGE-DIRECTION.  Returns the
// direction of the range.  If the range increment BY is positive, the
// range has the direction #"increasing", if negative, #"decreasing",
// and if zero, #"none".
//
define method range-direction (range :: <builtin-range>)
      => direction :: <symbol>;
   let r-by = range.range-by;
   case
      r-by = 0 => #"none";
      r-by > 0 => #"increasing";
      r-by < 0 => #"decreasing";
   end case;
end method;


// <unbounded-range> -- extremely internal
// 
// Class to represent unbounded (infinite) ranges.
// 
// MAKE should never be called on <unbounded-range> except for the few
// places in the range constructor.  Please use RANGE instead.
//
define sealed class <unbounded-range> (<builtin-range>) end class;


// <bounded-range> -- extremely internal
// 
// Class to represent bounded (finite) ranges.  This class adds a size
// slot to the <builtin-range> class.
// 
// MAKE should never be called on <bounded-range> except for the few
// places in the range constructor.  Please use RANGE instead.
//
define sealed class <bounded-range> (<builtin-range>)
   slot size :: <integer>,
      required-init-keyword: #"size";
end class;

define sealed domain make (singleton(<unbounded-range>));
define sealed domain make (singleton(<bounded-range>));


/* Range Utility Functions

   This section contains functions that are used to do the computation
   needed to set up a range.  Such computations include figuring out
   what the size of a range should be given its FROM, BY, TO, ABOVE,
   BELOW, and SIZE parameters.

*/

// compute-range-size -- internal
// 
// This function translates the (from, by, to, above, below, size)
// representation of the user to the (from, by, size) (bounded) or
// (from, by) (unbounded) internal representation.
// 
// The size returned by COMPUTE-RANGE-SIZE is the smallest range size
// such that:
// 1) the first (if any) element of the range is FROM and its
//    increment is BY
// 2) the range has no element less than ABOVE or greater than BELOW
// 3) the range has no element greater than TO + BY if BY is positive,
//    or no element less than TO + BY if BY is negative
// 4) the size of the range is no greater than SIZE
// 
// Size limitations for each of the arguments are computed.  Valid
// sizes (sizes not #f) are taken.
// 
// If there are no valid sizes, #f is returned.  (Everywhere in this
// implementation of ranges, a size of #f denotes an unbounded range.)
// If valid sizes exists the maximum of 0 and the minimum of the valid
// sizes is returned.
//
define method compute-range-size
    (r-from :: <real>, r-by :: <real>, r-to :: false-or(<real>),
     r-above :: false-or(<real>), r-below :: false-or(<real>),
     r-size :: false-or(<integer>))
 => (size :: false-or(<integer>));
   let to-size = r-to & compute-to-size (r-from, r-by, r-to);
   let above-size = r-above & compute-above-size (r-from, r-by, r-above);
   let below-size = r-below & compute-below-size (r-from, r-by, r-below);
   let size-size = r-size;

  if (to-size | above-size | below-size | size-size)
    let min = $maximum-integer;
    let min = if (to-size & (to-size < min)) to-size else min end if;
    let min = if (above-size & (above-size < min)) above-size else min end if;
    let min = if (below-size & (below-size < min)) below-size else min end if;
    let min = if (size-size & (size-size < min)) size-size else min end if;
    if (min > 0) min else 0 end if;
  end if;
end method;


// compute-to-size -- internal
// 
// Computes the limiting size of a TO argument to RANGE.  This size is
// one plus the nearest integer larger than
// 
//		 (BOUND - START) / INCREMENT
// 
// (See also APPROXIMATE-RANGE-KEY.  The TO size limit is essentially
// the larger approximate key for BOUND (plus 1).)
// 
// (The <integer> method is slightly optimized for case where the
// increment is +1 or -1.)
//
define method compute-to-size (start :: <integer>,
			       increment :: <integer>,
			       bound :: <integer>)
      => to-size :: false-or(<integer>);
   select (increment by \=)
      0 =>
	 #f;
      1 =>
	 bound - start + 1;
      -1 =>
	 -(bound - start) + 1;
      otherwise =>
	 ceiling/ (bound - start, increment) + 1;
   end select;
end method;
//
define method compute-to-size (start :: <real>,
			       increment :: <real>,
			       bound :: <real>)
      => to-size :: false-or(<integer>);
   select (increment by \=)
      0 =>
	 #f;
      otherwise =>
	 ceiling/ (bound - start, increment) + 1;
   end select;
end method;


// compute-above-size -- internal
// 
// Computes the limiting size of an ABOVE argument to RANGE.  This
// size is the nearest integer larger than
// 
//		 (BOUND - START) / INCREMENT
// 
// if the increment is negative (the range is decreasing toward the
// ABOVE bound.)
// 
// If the range is not decreasing, then if START if above ABOVE, #f is
// returned (no limiting size).  But if START is below ABOVE, 0 is
// returned.
//
define method compute-above-size (start :: <integer>,
				  increment :: <integer>,
				  bound :: <integer>)
      => above-size :: false-or(<integer>);
   if (negative? (increment))
      if (increment = -1)
	 -(bound - start)
      else
	 ceiling/ (bound - start, increment)
      end if;
   else
      if (bound < start)
	 #f
      else
	 0
      end if;
   end if;
end method;
//
define method compute-above-size (start :: <real>,
				  increment :: <real>,
				  bound :: <real>)
      => above-size :: false-or(<integer>);
   if (negative? (increment))
      ceiling/ (bound - start, increment)
   else
      if (bound < start)
	 #f
      else
	 0
      end if;
   end if;
end method;


// compute-below-size -- internal
//
// Computes the limiting size of an BELOW argument to RANGE.  This size is
// the nearest integer larger than
// 
//		 (BOUND - START) / INCREMENT
// 
// if the increment is positive (the range is increasing toward the
// BELOW bound.)
// 
// If the range is not increasing, then if START if below BELOW, #f is
// returned (no limiting size).  But if START is above BELOW, 0 is
// returned.
//
define method compute-below-size (start :: <integer>,
				  increment :: <integer>,
				  bound :: <integer>)
      => below-size :: false-or(<integer>);
   if (positive? (increment))
      if (increment = 1)
	 bound - start
      else
	 ceiling/ (bound - start, increment)
      end if;
   else
      if (bound > start)
	 #f
      else
	 0
      end if;
   end if;
end method;
//
define method compute-below-size (start :: <real>,
				  increment :: <real>,
				  bound :: <real>)
      => below-size :: false-or(<integer>);
   if (positive? (increment))
      if (increment = 1)
	 bound - start
      else
	 ceiling/ (bound - start, increment)
      end if;
   else
      if (bound > start)
	 #f
      else
	 0
      end if;
   end if;
end method;


// approximate-range-key -- internal
//
// Returns the key of the element of RANGE nearest to ELEMENT.  The
// approximate key for a number N is the integer nearest
// 
//			 (N - FROM) / BY
//
define method approximate-range-key
    (range :: <builtin-range>, element :: <real>)
 => (key :: <integer>);
  round/ (element - range.range-from, range.range-by)
end method;



/* Range Functions

   This section includes the special range constructor RANGE, and
   other functions special to the implementation of ranges, such as
   ELEMENT, and the method for BINARY=.

*/

// range -- public
// 
// RANGE is the constructor for ranges.  It accepts six keywords --
// from:, by:, to:, above:, below:, and size:.  It uses
// COMPUTE-RANGE-SIZE to find the appropriate size for the new range.
// If this size is #f an unbounded range is created, otherwise a
// bounded range is made.
//
define sealed method range 
    (#key from: r-from = 0, by: r-by = 1,
          to: r-to = #f, above: r-above = #f, below: r-below = #f,
	  size: r-size = #f)
 => new-range :: <builtin-range>;
  let range-size =
    compute-range-size (r-from, r-by, r-to, r-above, r-below, r-size);
  if (range-size)
    make (<bounded-range>, from: r-from, by: r-by, size: range-size);
  else
    make (<unbounded-range>, from: r-from, by: r-by);
  end if;
end method;


// make -- public
// 
// The MAKE method for abstract class <builtin-range> applies RANGE, the range
// constructor, to the keyword arguments.  This produces an instance
// of one of the concrete subclasses <bounded-range> or
// <unbounded-range>.
//
define inline method make
    (class-to-make == <range>, #rest keys, #key, #all-keys)
 => (new-range :: <builtin-range>);
   apply (range, keys);
end method;


// element -- public
// 
// Returns the element of the range corresponding to KEY.  This
// element is found using FROM + KEY * BY.  If KEY is out of the
// bounds of the range, the default is returned or an error is
// signalled.
//
define sealed method element (range :: <bounded-range>, key :: <integer>,
                       #key default = $not-supplied)
      => range-element :: <real>;
   case
      (key >= 0) & (key < range.size) =>
         range.range-from + (key * range.range-by);
      (default == $not-supplied) =>
         error ("No such element in %=: %d", range, key);
      otherwise =>
         default;
   end case;
end method;
//
define sealed method element
    (range :: <unbounded-range>, key :: <integer>,
     #key default = $not-supplied)
 => range-element :: <real>;
   case
      (key >= 0) =>
         range.range-from + (key * range.range-by);
      (default == $not-supplied) =>
         error ("No such element in %=: %d", range, key);
      otherwise =>
         default;
   end case;
end method;


// = -- public
// 
// Ranges are = if their beginning points, increments, and sizes are
// equal.
//
define sealed method \= (range1 :: <builtin-range>, range2 :: <builtin-range>)
      => equal? :: <boolean>;
  (range1.empty? & range2.empty?)
    | (range1.range-from = range2.range-from
         & range1.range-by = range2.range-by
         & range1.size = range2.size);
end method;



/* Iteration Protocol

   Iteration states for ranges are simply the integer keys, since we
   have an efficient way of calculating any element of the range.

   For bounded ranges we have to check the state against the size of the
   range.  Iteration over unbounded ranges does not terminate (i.e.
   NEXT-STATE never returns #f).

*/

// forward-iteration-protocol -- public
// 
define sealed inline method forward-iteration-protocol (range :: <bounded-range>)
      => (initial-state :: <object>, limit :: <object>,
	  next-state :: <function>, finished-state? :: <function>,
	  current-key :: <function>, current-element :: <function>,
	  current-element-setter :: <function>, copy-state? :: <function>);
   let initial-state = 0;
   let limit = range.size;
   local method next-state (r :: <builtin-range>, s :: <integer>)
	    s + 1
	 end method;
   local method finished-state? (r :: <builtin-range>, s :: <integer>,
				 l :: <integer>)
	    s = l
	 end method;
   local method current-key (r :: <builtin-range>, s :: <integer>)
	    s
	 end method;
   local method current-element (r :: <builtin-range>, s :: <integer>)
	    r[s];
	 end method;
   local method current-element-setter (r :: <builtin-range>, s :: <integer>,
					value)
            error ("CURRENT-ELEMENT-SETTER not applicable for <builtin-range>");
	 end method;
   local method copy-state (r :: <builtin-range>, s :: <integer>)
	    s
	 end method;
   values (initial-state, limit, next-state, finished-state?, current-key,
	   current-element, current-element-setter, copy-state);
end method;
//
define sealed inline method forward-iteration-protocol (range :: <unbounded-range>)
      => (initial-state :: <object>, limit :: <object>,
	  next-state :: <function>, finished-state? :: <function>,
	  current-key :: <function>, current-element :: <function>,
	  current-element-setter :: <function>, copy-state? :: <function>);
   let initial-state = 0;
   let limit = #f;
   local method next-state (r :: <builtin-range>, s :: <integer>)
	    s + 1
	 end method;
   local method finished-state? (r :: <builtin-range>, s :: <integer>, l)
	    #f
	 end method;
   local method current-key (r :: <builtin-range>, s :: <integer>)
	    s
	 end method;
   local method current-element (r :: <builtin-range>, s :: <integer>)
	    r[s];
	 end method;
   local method current-element-setter (r :: <builtin-range>, s :: <integer>, 
					value)
            error ("CURRENT-ELEMENT-SETTER not applicable for <builtin-range>");
	 end method;
   local method copy-state (r :: <builtin-range>, s :: <integer>)
	    s
	 end method;
   values (initial-state, limit, next-state, finished-state?, current-key,
	   current-element, current-element-setter, copy-state);
end method;



/* Collection Function Methods

   The collection functions which have methods specialized for ranges
   are SIZE, TYPE-FOR-COPY, EMPTY?, and MEMBER?.  These methods are
   defined in this section.

   Ranges use the default methods for the collection functions DO,
   MAP, ANY?, EVERY?, and FIND-KEY.

   Ranges have no methods for SIZE-SETTER because they are not
   stretchy.  Ranges do not have methods for MAP-AS, MAP-INTO,
   REPLACE-ELEMENTS!, and FILL! because they are not mutable.

   The methods for REDUCE and REDUCE1 for unbounded ranges signal an
   error, since reduction over unbounded ranges will not terminate.

   Note that using some of the default methods on unbounded ranges may
   cause infinite loops.  For example, uses of DO, MAP, ANY?, or
   EVERY? on unbounded ranges may never terminate.  (On the other
   hand, they might terminate, so we do not make this an error.)

*/

// size -- public
// 
// SIZE for unbounded ranges returns #f.  Size for bounded methods is taken
// from the slot
//
define sealed inline method size
    (range :: <unbounded-range>) => (result :: <false>);
  #f;
end method;


// type-for-copy -- public
// 
define sealed inline method type-for-copy
    (range :: <builtin-range>) => (result :: <class>);
  <list>;
end method;


// empty? -- public
// 
// A bounded range is empty if the size is zero.  An unbounded range
// can never be empty.
//
define sealed inline method empty?
    (range :: <bounded-range>) => (result :: <boolean>);
  range.size = 0
end method;
//
define sealed inline method empty?
    (range :: <unbounded-range>) => (result :: <false>);
  #f;
end method;


// reduce reduce1
// 
// Trying to reduce an unbounded range will not terminate.
//
define sealed inline method reduce
    (procedure :: <function>, initial-value, range :: <unbounded-range>)
 => (result :: <object>);
   error ("REDUCE not applicable for unbounded <range>");
end method;
//
define sealed inline method reduce1
    (procedure :: <function>, range :: <unbounded-range>)
 => (result :: <object>);
   error ("REDUCE1 not applicable for unbounded <range>");
end method;


// member? -- public
// 
// MEMBER? for must terminate if test is ==, even if the range is unbounded.
// We can do this test by deterministic arithmetic methods by insuring that
// the number is between the endpoints of the range and is it is an exact
// multiple of "by:" away from "from:".
//
define sealed method member?
    (value :: <object>, range :: <builtin-range>, #next next,
     #key test :: <function> = \==)
 => (result :: <boolean>);
  case
    (test ~= \==) => next();
    (~instance?(value, <real>)) => #f;
    otherwise =>
      let (count, remainder)
	= floor/(value - range.range-from, range.range-by);
      zero?(remainder) & (count >= 0)
	& (~instance?(range, <bounded-range>) | count < range.size);
  end case;
end method member?;


/* Sequence Function Methods

   The sequence functions which have methods specialized for ranges
   are INTERSECTION, COPY-SEQUENCE, REVERSE, and LAST.  These methods
   are defined in this section.

   Ranges use the default methods for the sequence functions ADD(!),
   ADD-NEW(!), REMOVE(!), CHOOSE, CHOOSE-BY, UNION,
   REMOVE-DUPLICATES(!), CONCATENATE, REPLACE-SUBSEQUENCE!, SORT(!),
   FIRST, SECOND, THIRD, and SUBSEQUENCE-POSITION.

   Ranges do not have methods for CONCATENATE-AS, and FIRST- SECOND-
   THIRD- LAST-SETTER because they are not mutable.

   The methods for ADD, ADD-NEW, CHOOSE, REMOVE-DUPLICATES, REVERSE,
   SORT, and LAST for unbounded ranges signal an error, since any of
   these over unbounded ranges will not terminate.

   Note that using some of the default methods on unbounded ranges may
   cause infinite loops.  For example, uses of CHOOSE-BY, UNION,
   CONCATENATE, and REPLACE-SUBSEQUENCE! on unbounded ranges may never
   terminate.

*/

// add
//
define sealed inline method add
    (range :: <unbounded-range>, new) => (result :: <sequence>);
  error ("ADD not applicable for unbounded <range>");
end method;


// add-new
//
define sealed inline method add-new
    (range :: <unbounded-range>, new, #key test) => (result :: <sequence>);
  error ("ADD-NEW not applicable for unbounded <range>");
end method;


// choose
//
define sealed inline method choose
    (predicate :: <function>, range :: <unbounded-range>)
 => (result :: <sequence>);
   error ("CHOOSE not applicable for unbounded <range>");
end method;


// intersection -- public
// 
// Range intersection is quite complicated, so the implementation is
// included in its own section below.


// remove-duplicates
//
define sealed inline method remove-duplicates
    (range :: <unbounded-range>, #key test) => (result :: <sequence>);
  error ("REMOVE-DUPLICATES not applicable for unbounded <range>");
end method;


// copy-sequence -- public
// 
// Returns a range which is a copy of the source range.  The START and
// END keywords specify at which elements of the range copying should
// begin and end.
// 
// For bounded ranges, correct values for COPY-START and COPY-END are
// found with respect to the range, and RANGE is called with the right
// length and other parameters from the original range.
// 
// For unbounded ranges, a bounded range is returned if END is
// supplied, and an unbounded range if not.
//
define sealed method copy-sequence
    (source :: <bounded-range>, #key start: copy-start = 0, end: copy-end)
 => (result :: <bounded-range>);
   let r-size = source.size;
   let r-from = source.range-from;
   let r-by = source.range-by;
   let copy-start = if (copy-start >= 0)
		       copy-start
		    else
		       0
		    end if;
   let copy-end = if (copy-end)
		     copy-end
		  else
		     r-size
		  end if;
   if (copy-start > copy-end) 
     error("End: (%=) is smaller than start: (%=)", copy-start, copy-end);
   end if;

   case
      copy-start >= r-size =>
	 range (size: 0);
      copy-end >= r-size =>
	 range (from: source[copy-start], by: r-by,
		size: r-size - copy-start);
      otherwise =>
	 range (from: source[copy-start], by: r-by,
		size: copy-end - copy-start);
   end case;
end method;
//
define sealed method copy-sequence
    (source :: <unbounded-range>, #key start: copy-start = 0, end: copy-end)
 => (result :: <builtin-range>);
   let r-from = source.range-from;
   let r-by = source.range-by;
   let copy-start = if (copy-start >= 0)
		       copy-start
		    else
		       0
		    end if;
   if (copy-end)
      range (from: source[copy-start], by: r-by,
	     size: copy-end - copy-start);
   else
      range (from: source[copy-start], by: r-by);
   end if;
end method;


// reverse -- public
// 
// For bounded ranges REVERSE returns a new range from: the last
// element of the original range, by: the negative of the original by,
// with size: the size of the original range.
// 
// Unbounded ranges cannot be reversed.p
//
define sealed method reverse
    (range-to-reverse :: <bounded-range>) => (result :: <bounded-range>);
   range (from: last (range-to-reverse, default: range-to-reverse.range-from),
	  by: negative (range-to-reverse.range-by),
	  size: range-to-reverse.size);
end method;
//
define sealed inline method reverse
    (range :: <unbounded-range>) => (result :: <sequence>);
  error ("REVERSE not applicable for unbounded <range>");
end method;


// reverse! -- public
// 
// For bounded ranges, REVERSE! sets RANGE-FROM to the last element of
// the range and RANGE-BY to the negative of the original by, and
// returns the range.
// 
// Unbounded ranges cannot be REVERSED!.
//
define sealed method reverse!
    (range :: <bounded-range>) => (range :: <bounded-range>);
  range.range-from := last (range, default: range.range-from);
  range.range-by := negative (range.range-by);
  range
end method;
//
define sealed inline method reverse!
    (range :: <unbounded-range>) => (result :: <sequence>);
  error ("REVERSE! not applicable for unbounded <range>");
end method;


// sort
//
define sealed inline method sort
    (range :: <unbounded-range>, #key test, stable) => (result :: <sequence>);
  error ("SORT not applicable for unbounded <range>");
end method;

define sealed inline method sort!
    (range :: <unbounded-range>, #key test, stable) => (result :: <sequence>);
  error ("SORT! not applicable for unbounded <range>");
end method;

// last -- public
// 
// Returns the element at SIZE - 1.  Signals an error for
// unbounded ranges.
//
define sealed inline method last
    (range :: <bounded-range>, #rest keys, #key default)
 => (result :: <object>);
   apply(element, range, range.size - 1, keys)
end method;
//
define sealed inline method last
    (range :: <unbounded-range>, #key default) => (result :: <object>);
  error ("LAST not applicable for unbounded <range>");
end method;



/*
			  Range Intersection

   INTERSECTION for ranges is required to return even for unbounded
   ranges.  So the algorithm used for range intersection must be able
   to find an intersection for unbounded ranges.  Fortunately this is
   not too hard with the representation of ranges used here.

   The steps of finding the intersection of two ranges are:

   1) Find the interval in which the two ranges must intersect.  This
      interval may be infinitely long in one direction.

   2) If the interval is finite, find the finite intersection of the
      two ranges within that interval.

      If the interval is infinite, find the unbounded increasing or
      decreasing (one or the other) intersection of the two ranges
      within that interval.

   The functions to do these steps are defined below.  Step 1 is
   performed by INTERSECTION-INTERVAL.  Step 2 is performed by one of
   FINITE-INTERSECTION, INCREASING-INTERSECTION, or
   DECREASING-INTERSECTION.

*/

// intersection -- public
// 
// The method on sequence intersection for ranges.  If the TEST is ==
// or =, INTERSECTION will produce a range as its result.  If not,
// then the sequence produced is the result of the default sequence
// method for ranges.
//
define sealed inline method intersection (range1 :: <builtin-range>, range2 :: <builtin-range>,
			    #next next-method, #key test = \==)
      => sequence :: <sequence>;
   if (test == \== | test == \=)
      range-intersection (range1, range2, test: test);
   else
      next-method ();
   end if;
end method;


// range-intersection -- internal
// 
// Return a new range which is the intersection of the two ranges.
// 
// This is done by finding the interval of intersection of the two
// ranges, and calculating the either finite, infinite increasing, or
// infinite decreasing intersection withing the interval.
//
define method range-intersection (range1 :: <builtin-range>, range2 :: <builtin-range>,
				  #key test)
      => range :: <builtin-range>;
   let (x-from, x-to) = intersection-interval (range1, range2);
   case
      ~ x-from =>
	 decreasing-intersection (range1, range2, test: test);
      ~ x-to =>
	 increasing-intersection (range1, range2, test: test);
      otherwise =>
	 finite-intersection (range1, range2, test: test);
   end case;
end method;


// finite-intersection -- internal
// 
// Returns a bounded range containing the intersection of the two
// ranges.  The keys in RANGE1 of the bounds of the intersection
// interval are computed.  Then all the elements of RANGE1 between
// these keys which are also elements of RANGE2 are found.  A new
// range beginning at the first element (if any) of elements and
// ending at the last with the increment of the second - the first is
// returned.
//
define method finite-intersection (range1 :: <builtin-range>, range2 :: <builtin-range>,
				   #key test)
      => range :: <bounded-range>;
   let (x-from, x-to) = intersection-interval (range1, range2);
   let from-key = approximate-range-key (range1, x-from);
   let to-key = approximate-range-key (range1, x-to);
   let intersection =
      if (range1.range-direction == #"increasing")
	 choose (rcurry (member?, range2, test: test),
		 copy-sequence (range1, start: from-key, end: to-key + 1));
      else
	 choose (rcurry (member?, range2, test: test),
		 copy-sequence (range1, start: to-key, end: from-key + 1));
      end if;
   select (intersection.size by \=)
      0 =>
	 range (size: 0);
      1 =>
         range (from: intersection.first, size: 1);
      otherwise =>
         range (from: intersection.first, to: intersection.last,
		by: intersection.second - intersection.first);
   end select;
end method;


// increasing-intersection -- internal
// 
// Returns an unbounded increasing range containing the intersection
// of the two ranges.  BY is taken to be the least common multiple of
// the BYs of RANGE1 and RANGE2.  The key in RANGE1 of the lower
// intersection interval bound is found, and the upper key is taken to
// be the key of the lower bound + BY (because the intersection
// interval has no upper bound).  (If the intersection has any
// elements, there must be one within BY of the bottom of the
// intersection interval.)
// 
// The elements of RANGE1 between these keys which are also elements
// of RANGE2 are found, and a new range beginning with the first of
// these (if any) and with an increment of BY is returned.
//
define method increasing-intersection (range1 :: <unbounded-range>,
				       range2 :: <unbounded-range>,
				       #key test)
      => range :: <unbounded-range>;
   let (x-from, x-to) = intersection-interval (range1, range2);
   let x-by = lcm (range1.range-by, range2.range-by);
   let from-key = approximate-range-key (range1, x-from);
   let to-key = approximate-range-key (range1, x-from + 2 * x-by);
   let intersection =
      choose (rcurry (member?, range2, test: test),
	      copy-sequence (range1, start: from-key, end: to-key));
   if (empty? (intersection))
      range (size: 0);
   else
      range (from: intersection.first, by: x-by);
   end if;
end method;


// decreasing-intersection -- internal
// 
// Returns an unbounded decreasing range containing the intersection
// of the two ranges.  BY is taken to be the least common multiple of
// the BYs of RANGE1 and RANGE2.  The key in RANGE1 of the upper
// intersection interval bound is found, and the lower key is taken to
// be the key of the upper bound + BY (because the intersection
// interval has no lower bound).  (If the intersection has any
// elements, there must be one within BY of the top of the
// intersection interval.)
// 
// The elements of RANGE1 between these keys which are also elements
// of RANGE2 are found, and a new range beginning with the first of
// these (if any) and with an increment of BY is returned.
//
define method decreasing-intersection (range1 :: <unbounded-range>,
				       range2 :: <unbounded-range>,
				       #key test)
      => range :: <unbounded-range>;
   let (x-from, x-to) = intersection-interval (range1, range2);
   let x-by = -lcm (-range1.range-by, -range2.range-by);
   let from-key = approximate-range-key (range1, x-to + 2 * x-by);
   let to-key = approximate-range-key (range1, x-to);
   let intersection =
      choose (rcurry (member?, range2, test: test),
	      copy-sequence (range1, start: to-key, end: from-key));
   if (empty? (intersection))
      range (size: 0);
   else
      range (from: intersection.first, by: x-by);
   end if;
end method;


// range-directions -- internal
// 
// Returns a symbol denoting the respective directions of RANGE1 and
// RANGE2.
//
define method range-directions (range1 :: <builtin-range>, range2 :: <builtin-range>)
      => direction :: <symbol>;
   if (range1.range-direction == #"increasing")
      if (range2.range-direction == #"increasing")
	 #"increasing-increasing"
      else
	 #"increasing-decreasing"
      end if;
   else
      if (range2.range-direction == #"increasing")
	 #"decreasing-increasing"
      else
	 #"decreasing-decreasing"
      end if;
   end if;
end method;


// intersection-interval -- internal
// 
// Returns the lower and upper bounds of the interval in which the two
// ranges intersect.
// 
// For any intersection with a bounded range, the intersection
// interval will be finite.  The first number returned is always lower
// than the second.
// 
// For two unbounded ranges, the interval of intersection may be
// infinitely long in one direction or the other.  In this case one of
// the bounds will be #f (using the convention in this code that #f
// represents an unbounded size).
//
define method intersection-interval (range1 :: <bounded-range>,
				     range2 :: <bounded-range>)
      => (x-from :: false-or(<integer>), x-to :: false-or(<integer>));
   let from1 = range1.range-from;
   let to1 = range1.last;
   let from2 = range2.range-from;
   let to2 = range2.last;
   select (range-directions (range1, range2))
      #"increasing-increasing" =>
	 values (max (from1, from2), min (to1, to2));
      #"increasing-decreasing" =>
	 values (max (from1, to2), min (to1, from2));
      #"decreasing-increasing" =>
	 values (max (to1, from2), min (from1, to2));
      #"decreasing-decreasing" =>
	 values (max (to1, to2), min (from1, from2));
   end select;
end method;
//
define method intersection-interval (range1 :: <bounded-range>,
				     range2 :: <unbounded-range>)
      => (x-from :: false-or(<integer>), x-to :: false-or(<integer>));
   let from1 = range1.range-from;
   let to1 = range1.last;
   let from2 = range2.range-from;
   select (range-directions (range1, range2))
      #"increasing-increasing" =>
	 values (max (from1, from2), to1);
      #"increasing-decreasing" =>
	 values (from1, min (to1, from2));
      #"decreasing-increasing" =>
	 values (max (to1, from2), from1);
      #"decreasing-decreasing" =>
	 values (to1, min (from1, from2));
   end select;
end method;
//
define method intersection-interval (range1 :: <unbounded-range>,
				     range2 :: <bounded-range>)
      => (x-from :: false-or(<integer>), x-to :: false-or(<integer>));
   let from1 = range1.range-from;
   let from2 = range2.range-from;
   let to2 = range2.last;
   select (range-directions (range1, range2))
      #"increasing-increasing" =>
	 values (max (from1, from2), to2);
      #"increasing-decreasing" =>
	 values (max (from1, to2), from2);
      #"decreasing-increasing" =>
	 values (from2, min (from1, to2));
      #"decreasing-decreasing" =>
	 values (to2, min (from1, from2));
   end select;
end method;
//
define method intersection-interval (range1 :: <unbounded-range>,
				     range2 :: <unbounded-range>)
      => (x-from :: false-or(<integer>), x-to :: false-or(<integer>));
   let from1 = range1.range-from;
   let from2 = range2.range-from;
   select (range-directions (range1, range2))
      #"increasing-increasing" =>
	 values (max (from1, from2), #f);
      #"increasing-decreasing" =>
	 values (from1, from2);
      #"decreasing-increasing" =>
	 values (from2, from1);
      #"decreasing-decreasing" =>
	 values (#f, min (from1, from2));
   end select;
end method;
