rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/vector.dylan,v 1.12 1996/03/20 01:44:03 rgs Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera


// Abstract vector stuff.

define open abstract class <vector> (<array>)
end;

define inline sealed method make (class == <vector>, #key size = 0, fill)
    => res :: <simple-object-vector>;
  make(<simple-object-vector>, size: size, fill: fill);
end;

define sealed inline method as
    (class == <vector>, collection :: <collection>)
    => res :: <vector>;
  as(<simple-object-vector>, collection);
end;

define sealed inline method as
    (class == <vector>, vector :: <vector>)
    => res :: <vector>;
  vector;
end;

define inline method dimensions (vec :: <vector>) => res :: <sequence>;
  vector(vec.size);
end;

define inline method rank (vec :: <vector>) => res :: <integer>;
  1;
end;

define inline method row-major-index (vec :: <vector>, #rest indices)
    => index :: <integer>;
  if (indices.size ~== 1)
    error("Number of indices not equal to rank. Got %=, wanted one index",
	  indices);
  end if;
  let index :: <integer> = indices[0];
  if (index < 0 | index > vec.size)
    error("Vector index out of bounds: %=", index);
  end;
  index;
end;


// ???

define constant element-error
  = method (coll :: <sequence>, index :: <integer>)
	=> res :: <never-returns>;
      error("No element %d in %=", index, coll);
    end method;


// <simple-vector>s

define sealed abstract class <simple-vector> (<vector>)
end;

define inline sealed method make
    (class == <simple-vector>, #key size = 0, fill)
    => res :: <simple-object-vector>;
  make(<simple-object-vector>, size: size, fill: fill);
end;

define sealed inline method as
    (class == <simple-vector>, collection :: <collection>)
    => res :: <simple-vector>;
  as(<simple-object-vector>, collection);
end;

define sealed inline method as
    (class == <simple-vector>, vector :: <simple-vector>)
    => res :: <simple-vector>;
  vector;
end;


// <simple-object-vector>s

define  inline method vector (#rest args) => res :: <simple-object-vector>;
  args;
end;

define class <simple-object-vector> (<simple-vector>)
  sealed slot %element,
    init-value: #f, init-keyword: fill:,
    sizer: size, size-init-value: 0, size-init-keyword: size:;
end;

define sealed domain make (singleton(<simple-object-vector>));

define sealed inline method element
    (vec :: <simple-object-vector>, index :: <integer>,
     #key default = $not-supplied)
    => element :: <object>;
  if (index >= 0 & index < vec.size)
    %element(vec, index);
  elseif (default == $not-supplied)
    element-error(vec, index);
  else
    default;
  end;
end;

define sealed inline method element-setter
    (new-value :: <object>, vec :: <simple-object-vector>,
     index :: <integer>)
    => new-value :: <object>;
  if (index >= 0 & index < vec.size)
    %element(vec, index) := new-value;
  else
    element-error(vec, index);
  end;
end;

// This method is identical to the one in "array.dylan", except that it
// is more tightly specialized to a single sealed class.  If you need to 
// make a general change, you should probably grep for "outlined-iterator" 
// and change all matching locations.
//
define inline method forward-iteration-protocol
    (array :: <simple-object-vector>)
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
	 method (array :: <simple-object-vector>, state :: <integer>)
	     => new-state :: <integer>;
	   state + 1;
	 end,
	 method (array :: <simple-object-vector>, state :: <integer>,
		 limit :: <integer>)
	     => done? :: <boolean>;
	   state == limit;
	 end,
	 method (array :: <simple-object-vector>, state :: <integer>)
	     => key :: <integer>;
	   state;
	 end,
	 method (array :: <simple-object-vector>, state :: <integer>)
	     => element :: <object>;
	   element(array, state);
	 end,
	 method (new-value :: <object>, array :: <simple-object-vector>,
		 state :: <integer>)
	     => new-value :: <object>;
	   element(array, state) := new-value;
	 end,
	 method (array :: <simple-object-vector>, state :: <integer>)
	     => state-copy :: <integer>;
	   state;
	 end);
end;

define sealed inline method fill!
    (vec :: <simple-vector>, value :: <object>,
     #key start :: <integer> = 0, end: end-index :: <integer> = vec.size)
 => (vec :: <simple-vector>);
  for (index :: <integer> from start below end-index)
    vec[index] := value;
  end for;
  vec;
end method fill!;

define sealed inline method as
    (class == <simple-object-vector>, vector :: <simple-object-vector>)
    => res :: <simple-object-vector>;
  vector;
end;

// Perhaps this should be inlined eventually, but at present it
// tickles a bug in stack analysis
//
define sealed method as
    (class == <simple-object-vector>, collection :: <collection>)
    => res :: <simple-object-vector>;
  let res = make(<simple-object-vector>, size: collection.size);
  for (index :: <integer> from 0, element in collection)
    res[index] := element;
  end;
  res;
end method as;

// This method looks to be unduly specific, but the compiler will
// generate this case whenever you "apply" a function to a list
//
define sealed method as
    (class == <simple-object-vector>, collection :: <list>)
    => res :: <simple-object-vector>;
  let res = make(<simple-object-vector>, size: collection.size);
  for (index :: <integer> from 0, element in collection)
    res[index] := element;
  end;
  res;
end method as;

// This method looks to be unduly specific, but the compiler will
// generate this case whenever you "apply" a function to a strechy vector
//
define sealed method as
    (class == <simple-object-vector>, collection :: <stretchy-object-vector>)
 => (res :: <simple-object-vector>);
  let sz = collection.size;
  let res = make(<simple-object-vector>, size: sz);
  for (index :: <integer> from 0 below sz)
    res[index] := collection[index];
  end;
  res;
end;

