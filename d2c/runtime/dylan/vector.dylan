rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/vector.dylan,v 1.9 1996/03/02 19:21:08 rgs Exp $
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

seal generic make (singleton(<simple-object-vector>));

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

define sealed method as
    (class == <simple-object-vector>, collection :: <collection>)
    => res :: <simple-object-vector>;
  let res = make(<simple-object-vector>, size: collection.size);
  for (index :: <integer> from 0, element in collection)
    res[index] := element;
  end;
  res;
end method as;

define inline method as
    (class == <simple-object-vector>, vector :: <simple-object-vector>)
    => res :: <simple-object-vector>;
  vector;
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
