rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/vector.dylan,v 1.3 1995/11/13 23:09:07 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

define open abstract class <vector> (<array>)
end;

define generic vector (#rest args) => res :: <vector>;

define inline method dimensions (vec :: <vector>) => res :: <sequence>;
  vector(vec.size);
end;

define inline sealed method make (class == <vector>, #rest keys, #all-keys)
    => res :: <simple-object-vector>;
  apply(make, <simple-object-vector>, keys);
end;


// Shared support for builtin vectors.

define open abstract class <builtin-vector> (<vector>, <builtin-array>)
end;

// Builtin vectors are built around size, %element, and %element-setter.
// %element and %element-setter are just like element and element-setter,
// except they don't deal with the default keyword (and currently don't do
// any bounds checking).

define open generic %element
    (vec :: <builtin-vector>, index :: <fixed-integer>)
    => res :: <object>;

define open generic %element-setter
    (new :: <object>, vec :: <builtin-vector>, index :: <fixed-integer>)
    => res :: <object>;

// By making this a separate non-inline routine, we avoid inlining calls to
// error, which are annoyingly large.
//
define constant element-error
  = method (coll :: <sequence>, index :: <fixed-integer>) => res :: type-or();
      error("No element %d in %=", index, coll);
    end method;

define sealed inline method element
    (vec :: <builtin-vector>, index :: <fixed-integer>,
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
    (new-value :: <object>, vec :: <builtin-vector>, index :: <fixed-integer>)
    => new-value :: <object>;
  if (index >= 0 & index < vec.size)
    %element(vec, index) := new-value;
  else
    element-error(vec, index);
  end;
end;

define sealed inline method empty? (vec :: <builtin-vector>)
    => res :: <boolean>;
  vec.size == 0;
end;



// Making new Limited vectors.

/*

define variable *limited-vectors* = #();

define class <limited-vector-info> (<object>)
  slot lvi-type :: <type>,
    required-init-keyword: type:;
  slot lvi-size :: union(<false>, <fixed-integer>),
    required-init-keyword: size:;
  slot lvi-class :: <class>,
    required-init-keyword: class:;
  slot lvi-next :: union(<limited-vector-info>, <false>),
    required-init-keyword: next:;
end;

define method limited (class == <vector>, #key of :: <type>, fill, size: len)
  block (return)
    for (entry = *limited-vectors* then entry.lvi-next)
      if (subtype?(of, entry.lvi-type) & subtype?(entry.lvi-type, of)
	    & len == entry.lvi-size)
	return(entry.class);
      end;
    end;
    let slot = concatenate(vector(getter: %element, setter: element-setter,
				  type: type, init-keyword: fill:,
				  init-value: fill, size: size),
			   if (len)
			     vector(size-init-value: len);
			   else
			     vector(size-init-keyword: size:));
    let new = make(<class>, superclasses: <builtin-vector>,
		   slots: vector(slot));
    *limited-vectors* :=
      make(<limited-vector-info>, type: of, size: len, class: new,
	   next: *limited-vectors*);
    new;
  end;
end;

*/


// <simple-object-vector>s

/* 
define constant <simple-object-vector>
  = limited(<vector>, of: <object>);
*/

define class <simple-object-vector> (<builtin-vector>)
  sealed slot %element,
    init-value: #f, init-keyword: fill:,
    sizer: size, required-size-init-keyword: size:;
end;

seal generic make (singleton(<simple-object-vector>));
seal generic initialize (<simple-object-vector>);

define inline method vector (#rest args) => res :: <simple-object-vector>;
  args;
end;

define sealed inline method element
    (vec :: <simple-object-vector>, index :: <fixed-integer>,
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
    (new-value :: <object>, vec :: <simple-object-vector>, index :: <fixed-integer>)
    => new-value :: <object>;
  if (index >= 0 & index < vec.size)
    %element(vec, index) := new-value;
  else
    element-error(vec, index);
  end;
end;

