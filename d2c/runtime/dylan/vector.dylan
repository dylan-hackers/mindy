rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/vector.dylan,v 1.4 2003/06/03 02:12:16 housel Exp $
copyright: see below
module: dylan-viscera


//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
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


// out of line error functions, to minimize calling code size
// element-error is actually used throughout the runtime library

define constant element-error = method (coll :: <sequence>, index :: <integer>)
 => res :: <never-returns>;
  error("No element %d in %=", index, coll);
end method;

define constant row-major-index-error = method (index :: <integer>)
 => res :: <never-returns>;
  error("Vector index out of bounds: %=", index);
end method;

define constant vector-rank-error = method (indices)
 => res :: <never-returns>;
  error("Number of indices not equal to rank. Got %=, wanted one index",
	indices);
end method;




define inline method dimensions (vec :: <vector>) => res :: <sequence>;
  vector(vec.size);
end;

define inline method rank (vec :: <vector>) => res :: <integer>;
  1;
end;

define inline method row-major-index (vec :: <vector>, #rest indices)
    => index :: <integer>;
  if (indices.size ~== 1)
    vector-rank-error(indices);
  end if;
  let index :: <integer> = indices[0];
  if (index < 0 | index >= vec.size)
    row-major-index-error(index);
  end;
  index;
end;


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
	   // We use >= instead of == so that the constraint propagation
	   // stuff can tell that state is < limit if this returns #f.
	   state >= limit;
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


define open generic %elem (vec :: <vector>, index :: <integer>) 
 => (result :: <object>);
define open generic %elem-setter
    (value :: <object>, vec :: <vector>, index :: <integer>) 
 => (result :: <object>);

define macro limited-vector-class
  { limited-vector-class(?:name, ?element-type:expression, ?fill:expression) }
    => { begin
	   define sealed class ?name (<vector>)
	     sealed slot %elem :: ?element-type,
	       init-value: ?fill, init-keyword: fill:, sizer: size,
	       size-init-value: 0, size-init-keyword: size:;
	   end class;
           define sealed domain make (singleton(?name));
	   define sealed inline method element-type
	       (class :: subclass(?name))
	    => (type :: <type>, indefinite? :: <false>);
	     values(?element-type, #f);
	   end method element-type;
           define sealed inline method element
	       (vec :: ?name, index :: <integer>,
		#key default = $not-supplied)
	    => element :: <object>; // because of default:
	     if (index >= 0 & index < vec.size)
	       %elem(vec, index);
	     elseif (default == $not-supplied)
	       element-error(vec, index);
	     else
	       default;
	     end;
	   end;
           define sealed inline method element-setter
	       (new-value :: ?element-type, vec :: ?name,
		index :: <integer>)
	    => new-value :: ?element-type;
	     if (index >= 0 & index < vec.size)
	       %elem(vec, index) := new-value;
	     else
	       element-error(vec, index);
	     end;
	   end;
           // This method is identical to the one in "array.dylan", except
           // that it is more tightly specialized to a single sealed class.
           // If you need to make a general change, you should probably grep
           // for "outlined-iterator" and change all matching locations.
           //
           define sealed inline method forward-iteration-protocol (array :: ?name)
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
		    method (array :: ?name, state :: <integer>)
		     => new-state :: <integer>;
		      state + 1;
		    end,
		    method (array :: ?name, state :: <integer>,
			    limit :: <integer>)
		     => done? :: <boolean>;
		      // We use >= instead of == so that the constraint propagation
		      // stuff can tell that state is < limit if this returns #f.
		      state >= limit;
		    end,
		    method (array :: ?name, state :: <integer>)
		     => key :: <integer>;
		      state;
		    end,
		    method (array :: ?name, state :: <integer>)
		     => element :: ?element-type;
		      element(array, state);
		    end,
		    method (new-value :: ?element-type, array :: ?name,
			    state :: <integer>)
		     => new-value :: ?element-type;
		      element(array, state) := new-value;
		    end,
		    method (array :: ?name, state :: <integer>)
		     => state-copy :: <integer>;
		      state;
		    end);
	   end;
         end; }
end macro;

