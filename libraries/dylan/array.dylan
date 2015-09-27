module:   dylan
language: infix-dylan
author:   Nick Kramer (nkramer@cs.cmu.edu)

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

// This is an array implementation that depends upon vectors being
// already implemented.

// GFs borrowed from the new compiler

define open generic dimensions (array :: <array>) => dims :: <sequence>;

define open generic rank (array :: <array>) => rank :: <integer>;

define open generic row-major-index
    (array :: <array>, #rest subscripts) => index :: <integer>;

define open generic aref (array :: <array>, #rest indices)
    => element :: <object>;

define open generic aref-setter
    (new-value :: <object>, array :: <array>, #rest indices)
    => new-value :: <object>;

define open generic dimension (array :: <array>, axis :: <integer>)
    => dimension :: <integer>;


define class <multiD-array> (<array>)
  slot dimensions-slot  :: <simple-object-vector>;  // Sequence of integers
  slot contents-slot    :: <simple-object-vector>;
  slot size-slot        :: <integer>;
end class <multiD-array>;

// General array methods

define method make (c == <array>, 
		    #key dimensions: dimensions :: <sequence> = $not-supplied, 
		    fill = #f)
 => array :: <array>;
  if (dimensions == $not-supplied)
    error("Need the dimensions or a size for an array");
  elseif (size(dimensions) = 1)
    make(<vector>, fill: fill, size: head(dimensions));
  else
    make(<multiD-array>, dimensions: dimensions, fill: fill);
  end if;
end method make;


define method row-major-index (array :: <array>, #rest indices)
 => index :: <integer>;
  let dims = dimensions(array);
  let sum = 0;
  if (size(indices) ~= size(dims))
    error("Number of indices not equal to rank. Got %=, wanted %d indices",
	  indices, size(dims));
  else
    for (index in indices,
	 dim   in dims)
      if (index < 0 | index >= dim)
	error("Array index out of bounds: %= in %=", index, indices);
      else
	sum := (sum * dim) + index;
      end if;
    end for;
    sum;
  end if;
end method row-major-index;	       


define method aref (array :: <array>, #rest indices)
 => elt :: <object>;
  let index = apply(row-major-index, array, indices);
  array[index];             // Call element
end method aref;


define method aref-setter (value :: <object>, array :: <array>, 
			   #rest indices) => value :: <object>;
  let index = apply(row-major-index, array, indices);
  array [index] := value;    // Call element-setter
end method aref-setter;


// rank -- the number of dimensions
//
define method rank (array :: <array>) => the-rank-of-array :: <integer>;
  size(dimensions(array));
end method rank;


// Also defined below on multiD-arrays
//
define method size (array :: <array>) => size :: <integer>;
  reduce(\*, 1, dimensions(array));
end method size;


define method dimension (array :: <array>, axis :: <integer>) 
 => dim-of-that-axis :: <integer>;
  element(dimensions(array), axis);
end method dimension;


define method forward-iteration-protocol (array :: <array>)
  => (initial-state          :: <integer>,
      limit                  :: <integer>,
      next-state             :: <function>,  finished-state? :: <function>,
      current-key            :: <function>,  current-element :: <function>,
      current-element-setter :: <function>,  copy-state      :: <function>);
  values(0,                 // initial state
	 size(array),       // limit 

	      // next-state
	 method (array :: <array>, state :: <integer>) 
	  => next-state :: <integer>;
	   state + 1;
	 end method,

	      // finished-state?
	 method (array :: <array>, state :: <integer>,
		 limit :: <integer>)
	  => answer :: <boolean>;
	   state = limit;
	 end method,

	     // current-key
	 method (array :: <array>, state :: <integer>)
	     => key :: <integer>;
	   state;
	 end method,

	     // current-element
	 method (array :: <array>, state :: <integer>)
	   array[state];
	 end method,

	    // current-element-setter
	 method (value, array :: <array>, state :: <integer>)
	   array [state] := value;
	 end method,

	    // copy-state
	 method (array :: <array>, state :: <integer>) 
	  => new-state :: <integer>;
	   state;
	 end method);
end method forward-iteration-protocol;


define method backward-iteration-protocol (array :: <array>)
  => (final-state            :: <integer>,
      limit                  :: <integer>,
      previous-state         :: <function>,  finished-state? :: <function>,
      current-key            :: <function>,  current-element :: <function>,
      current-element-setter :: <function>,  copy-state      :: <function>);

  values(size (array) - 1,                 // final state
	 -1,                               // limit 

	     // next-state
	 method (array :: <array>, state :: <integer>)    
	   state - 1;
	 end method,

	     // Everything else the same as forward-iteration-protocol

	     // finished-state?
	 method (array :: <array>, state :: <integer>,
		 limit :: <integer>)
	   state = limit;
	 end method,

	     // current-key
	 method (array :: <array>, state :: <integer>)
	     => key :: <integer>;
	   state;
	 end method,

	     // current-element
	 method (array :: <array>, state :: <integer>)
	   array [state];
	 end method,

	    // current-element-setter
	 method (value, array :: <array>, state :: <integer>)
	   array [state] := value;
	 end method,

	    // copy-state
	 method (array :: <array>, state :: <integer>) 
	  => new-state :: <integer>;
	   state;
	 end method);
end method backward-iteration-protocol;


// multiD-array code


define method initialize (array :: <multiD-array>, 
			  #key dimensions: dimensions :: <sequence>,
			  fill: fill = #f);

  if (size(dimensions) == 1 )
    // This code should never be executed unless someone calls
    // make on a <multiD-array> instead of make (<array>)

    error("Can't make a <multiD-array> with 1 dimension");
  end if;
  array.dimensions-slot := as(<simple-object-vector>, dimensions);
  let total-size = reduce(\*, 1, array.dimensions-slot);
  array.size-slot := total-size;
  array.contents-slot := make(<simple-object-vector>, 
			      size: total-size, fill: fill);
end method initialize;


define method element (array :: <multiD-array>, index :: <integer>,
		       #key default: default = $not-supplied)
  => elt :: <object>;
  if (default == $not-supplied)
    array.contents-slot[index];
  else
    element(array.contents-slot, index, default: default);
  end if;
end method element;


define method element-setter (value, array :: <multiD-array>, 
			      index :: <integer>)
  => value :: <object>;
  array.contents-slot[index] := value;
end method element-setter;


define method size (array :: <multiD-array>) => size :: <integer>;
  array.size-slot;
end method size;


define method shallow-copy (array :: <multiD-array>)
 => new-array :: <multiD-array>;
  let new-array = make(<multiD-array>, dimensions: array.dimensions);
  map-into(new-array, identity, array);
end method shallow-copy;


define method dimensions (array :: <multiD-array>) => dimensions :: <sequence>;
  array.dimensions-slot;
end method dimensions;
