rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/array.dylan,v 1.1 1998/05/03 19:55:37 andreas Exp $
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

// <array> and generics

define open abstract class <array> (<mutable-sequence>)
end;

// make(<array>) -- exported GF method
//
// The <array> make method either makes a <simple-object-vector> or a
// <simple-object-array> depending on the number of dimensions.
// 
define sealed method make
    (class == <array>, #key fill, dimensions :: <sequence>)
    => res :: <array>;
  let rank :: <integer> = dimensions.size;
  if (rank == 1)
    make(<simple-object-vector>, size: dimensions.first, fill: fill);
  else
    for (size :: <integer> = 1 then size * dimension,
	 dimension :: <integer> in dimensions)
    finally
      let result = make(<simple-object-array>,
			data-vector: make(<simple-object-vector>,
					  size: size, fill: fill),
			rank: rank);
      for (index :: <integer> from 0,
	   dimension :: <integer> in dimensions)
	%dimension(result, index) := dimension;
      end;
      result;
    end for;
  end if;
end method make;

define sealed inline method as (class == <array>, collection :: <collection>)
    => res :: <simple-object-vector>;
  as(<simple-object-vector>, collection);
end;

define inline method as (class == <array>, array :: <array>)
    => res :: <array>;
  array;
end;

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


// Default methods.

// This method is duplicated in several places, except that the duplicates
// are more tightly specialized to a single sealed class.  If you need to 
// make a general change, you should probably grep for "outlined-iterator" 
// and change all matching locations.
//
define inline method forward-iteration-protocol (array :: <array>)
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
	 method (array :: <array>, state :: <integer>)
	     => new-state :: <integer>;
	   state + 1;
	 end,
	 method (array :: <array>, state :: <integer>,
		 limit :: <integer>)
	     => done? :: <boolean>;
	   // We use >= instead of == so that the constraint propagation
	   // stuff can tell that state is < limit if this returns #f.
	   state >= limit;
	 end,
	 method (array :: <array>, state :: <integer>)
	     => key :: <integer>;
	   state;
	 end,
	 method (array :: <array>, state :: <integer>)
	     => element :: <object>;
	   element(array, state);
	 end,
	 method (new-value :: <object>, array :: <array>,
		 state :: <integer>)
	     => new-value :: <object>;
	   element(array, state) := new-value;
	 end,
	 method (array :: <array>, state :: <integer>)
	     => state-copy :: <integer>;
	   state;
	 end);
end;

define inline method backward-iteration-protocol (array :: <array>)
    => (initial-state :: <integer>,
	limit :: <integer>,
	next-state :: <function>,
	finished-state? :: <function>,
	current-key :: <function>,
	current-element :: <function>,
	current-element-setter :: <function>,
	copy-state :: <function>);
  values(array.size - 1,
	 -1,
	 method (array :: <array>, state :: <integer>)
	     => next-state :: <integer>;
	   state - 1;
	 end,
	 method (array :: <array>, state :: <integer>,
		 limit :: <integer>)
	     => done :: <boolean>;
	   state == limit;
	 end,
	 method (array :: <array>, state :: <integer>)
	     => key :: <integer>;
	   state;
	 end,
	 method (array :: <array>, state :: <integer>)
	     => element :: <object>;
	   element(array, state);
	 end,
	 method (new-value :: <object>, array :: <array>,
		 state :: <integer>)
	     => new-value :: <object>;
	   element(array, state) := new-value;
	 end,
	 method (array :: <array>, state :: <integer>)
	     => state-copy :: <integer>;
	   state;
	 end);
end;

define inline method size (array :: <array>) => size :: <integer>;
  reduce(\*, 1, array.dimensions);
end;

define inline method rank (array :: <array>) => rank :: <integer>;
  array.dimensions.size;
end;

define method row-major-index (array :: <array>, #rest indices)
    => index :: <integer>;
  let dims = dimensions(array);
  if (size(indices) ~== size(dims))
    error("Number of indices not equal to rank. Got %=, wanted %d indices",
	  indices, size(dims));
  else
    for (index :: <integer> in indices,
	 dim :: <integer>   in dims,
	 sum :: <integer> = 0 then (sum * dim) + index)
      if (index < 0 | index >= dim)
	error("Array index out of bounds: %= in %=", index, indices);
      end if;
    finally
      sum;
    end for;
  end if;
end;

define inline method aref (array :: <array>, #rest indices)
    => element :: <object>;
  element(array, apply(row-major-index, array, indices));
end;

define inline method aref-setter
    (new-value :: <object>, array :: <array>, #rest indices)
    => new-value :: <object>;
  element(array, apply(row-major-index, array, indices)) := new-value;
end;

define inline method dimension (array :: <array>, axis :: <integer>)
    => dimension :: <integer>;
  array.dimensions[axis];
end;


// <simple-object-array>s

// <simple-object-array> -- sorta exported.
//
// <simple-object-array>s are the multi-dimensional analogue to
// <simple-object-vector>s.  They can only elements of type <object>
// and cannot be resized once created.
// 
// The functionality of <simple-object-array> is exported via make(<array>),
// but the name <simple-object-array> is not.
//
define class <simple-object-array> (<array>)
  slot data-vector :: <simple-object-vector>,
    required-init-keyword: data-vector:;
  slot %dimension :: <integer>,
    init-value: 0, sizer: rank, required-size-init-keyword: rank:;
end;

define sealed domain make (singleton(<simple-object-array>));

// dimensions(<simple-object-array>) -- exported gf method.
//
// Make a vector, fill it in with the dimensions, and return it.
// 
define method dimensions (array :: <simple-object-array>)
    => dims :: <simple-object-vector>;
  let rank = array.rank;
  let dims = make(<simple-object-vector>, size: rank);
  for (index :: <integer> from 0 below rank)
    dims[index] := %dimension(array, index);
  end;
  dims;
end;

// row-major-index(<simple-object-array>) -- exported gf method.
//
// Similar to the default <array> method, but we don't both computing a
// dimensions sequence because we can just access each dimension directly.
//
define method row-major-index
    (array :: <simple-object-array>, #rest indices)
    => index :: <integer>;
  if (indices.size ~== array.rank)
    error("Number of indices not equal to rank. Got %=, wanted %d indices",
	  indices, array.rank);
  else
    let sum :: <integer> = 0;
    for (i :: <integer> from 0,
	 index :: <integer> in indices)
      let dim = %dimension(array, i);
      if (index < 0 | index >= dim)
	error("Array index out of bounds: %= in %=", index, indices);
      end if;
      sum := sum * dim + index;
    end for;
    sum;
  end if;
end;

// aref(<simple-object-array>) -- exported gf method.
//
// Identical to the inherited method, but repeated here so that the body
// can be compiled more effeciently.
//
define inline method aref
    (array :: <simple-object-array>, #rest indices)
    => element :: <object>;
  element(array, apply(row-major-index, array, indices));
end;

// aref-setter(<simple-object-array>) -- exported gf method.
//
// Identical to the inherited method, but repeated here so that the body
// can be compiled more effeciently.
//
define inline method aref-setter
    (new-value :: <object>, array :: <simple-object-array>, #rest indices)
    => new-value :: <object>;
  element(array, apply(row-major-index, array, indices)) := new-value;
end;

// dimension(<simple-object-array>) -- exported gf method.
//
// Just check that the axis is in bounds and then extract the dimension.
//
define inline method dimension
    (array :: <simple-object-array>, axis :: <integer>)
    => res :: <integer>;
  if (axis < 0 | axis >= array.rank)
    error("Invalid axis in %=: %=", array, axis);
  end;
  %dimension(array, axis);
end;

// element(<simple-object-array>) -- exported gf method.
//
// Just access the data vector.  We don't do any bounds checking because
// calling element on the data vector will do that for us.
//
define inline method element
    (array :: <simple-object-array>, index :: <integer>,
     #key default = $not-supplied)
    => res :: <object>;
  // We rely on the <simple-object-vector> element method using $not-supplied
  // to indicate an unsupplied default.
  element(array.data-vector, index, default: default);
end;

// element-setter(<simple-object-array>) -- exported gf method.
//
// Just access the data vector.  We don't do any bounds checking because
// calling element-setter on the data vector will do that for us.
//
define inline method element-setter
    (new-value :: <object>, array :: <simple-object-array>,
     index :: <integer>)
    => new-value :: <object>;
  array.data-vector[index] := new-value;
end;

// shallow-copy(<simple-object-array>) -- exported gf method.
//
// We can't use the default collection method, because it will try to
// call make(type-for-copy(array), size: array.size) which just won't do.
//
define method shallow-copy (array :: <simple-object-array>)
    => res :: <simple-object-array>;
  let rank = array.rank;
  let res = make(<simple-object-array>, rank: rank,
		 data-vector: shallow-copy(array.data-vector));
  for (index :: <integer> from 0 below rank)
    %dimension(res, index) := %dimension(array, index);
  end;
  res;
end;
