RCS-Header: $Header: /scm/cvs/src/d2c/runtime/dylan/limited-collection.dylan,v 1.4 2002/07/13 00:57:03 bruce Exp $
Module: dylan-viscera
Copyright: See below.
Synopsis: Runtime support for limited collections.
Author: Eric Kidd <eric.kidd@pobox.com>

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

// XXX - We should rename this slot on <limited-type>, but we can't do that
// without modifying cback's heap-dumping code and incrementing the
// bootstrap counter.
define constant limited-type-base-class = limited-integer-base-class;


//=========================================================================
//  make(singleton(<limited-collection>))
//=========================================================================
//  We pick apart the limited collection type and do (another) dispatch.
//
//  To add a new limited collection type, define an appropriate method on
//  make-limited-collection. Then, go to compiler/optimize/limopt.dylan
//  and add your implementation class to limited-collection-implementation.
//  You'll also need to export your class from bootstrap.dylan and the
//  'magic' module of exports.dylan so you can refer to it from inside
//  the compiler.
//
//  This function should *not* be inlined. We define a transformer that
//  shadows this function and generates better (but equivalent) code.
//  If you inline this function, the transformer breaks.

define sealed  method make
    (type :: <limited-collection>, #rest supplied-keys, #key, #all-keys)
 => (instance :: <collection>)
  apply(make-limited-collection,
	type.limited-type-base-class,
	type.limited-element-type,
	type, supplied-keys);
end method make;

// Note that we should specify tight return types on the individual methods,
// preferably in terms of 'limited(<foo>, of: <known-type>)' when possible.
// This will cause a typecheck to happen in make-limited-collection, but
// will allow better type inference in user code.
// XXX - Actually, the above statements may be wrong. Investigate this.
// Eventually, we'll need to add a special-purpose transformer to clean
// up the type of this generic in the compiler itself.
define generic make-limited-collection
    (base-class :: <class>,
     element-type :: <type>,
     collection-type :: <limited-collection>,
     #key, #all-keys)
 => (instance :: <collection>);


//=========================================================================
//  <limited-collection-mixin>
//=========================================================================
//  All limited collection types[*] should inherit from this class. We use
//  it to tie into type introspection and cheap IO.
//  [*] Except the <simple-object-something> types, which aren't really
//  limited in any way.
//
//  XXX - We should be called <limited-collection>, but that name is
//  already taken by a subclass of <limited-type>. Investigate doing this
//  renaming someday--it will require changes to cback and possibly other
//  parts of the compiler.

define class <limited-collection-mixin> (<collection>)
  // What limited type was used to create this collection?
  slot %limited-collection-type :: <limited-collection>,
    required-init-keyword: collection-type:;
end class;

define sealed domain initialize (<limited-collection-mixin>);

define method element-type (collection :: <limited-collection-mixin>)
 => (type :: <type>, indefinite? :: <boolean>);
  element-type(collection.%limited-collection-type);
end method element-type;


//=========================================================================
//  Packable Element Types
//=========================================================================
//  For sufficiently important limited collection types, we should have
//  packed storage representations for each of the types below. This
//  list should vaguely match the types in the C-Representation module.
//
//  We don't support unsigned 32-bit integers because they don't work
//  correctly in the current version of d2c. Oh, well.
//
//  XXX - Do we want to get this from configure, hypothetically speaking?
//  XXX - This code is partially duplicated in optimizer/limopt.dylan.

// These types are declared elsewhere.
// <integer> - This gives us 32-bit signed values with our current compiler.
// <single-float>
// <double-float>
// <extended-float>
// <boolean>

// From /usr/include/limits.h.
define constant $CHAR_MIN  = -128;
define constant $CHAR_MAX  = 127;
define constant $UCHAR_MAX = 255;
define constant $SHRT_MIN  = -32768;
define constant $SHRT_MAX  = 32767;
define constant $USHRT_MAX = 65535;

// Limited integer types.
// (These don't *need* to be exported anywhere; the user can just use the
// limited integer types instead, and the optimizer will automagically do
// the Right Thing<tm>.)
define constant <%u-char>  = limited(<integer>,min: 0, max: $UCHAR_MAX);
define constant <%s-char>  = limited(<integer>,min: $CHAR_MIN, max: $CHAR_MAX);
define constant <%u-short> = limited(<integer>,min: 0, max: $USHRT_MAX);
define constant <%s-short> = limited(<integer>,min: $SHRT_MIN, max: $SHRT_MAX);

// A little help for selecting one of several classes based on a limited
// integer type. The 'five-classes' vector should contain an element for
// each of <%u-char>, <%s-char>, <%u-short>, <%s-short> and a default value,
// in that order.
define function look-up-class-by-limited-integer-type
    (type :: <limited-integer>, five-classes :: <simple-object-vector>)
 => (class :: <class>)
  let min = type.limited-integer-minimum;
  let max = type.limited-integer-maximum;
  let choice =
    case
      (min == 0) =>
	select (max by \==)
	  $UCHAR_MAX => 0;
	  $USHRT_MAX => 2;
	  otherwise  => 4;
	end select;
      (min == $CHAR_MIN & max == $CHAR_MAX) => 1;
      (min == $SHRT_MIN & max == $SHRT_MAX) => 3;
      otherwise => 4;
    end case;
  five-classes[choice];
end function look-up-class-by-limited-integer-type;


//=========================================================================
//  Limited <object-table> Types
//=========================================================================
//  We declare one subclass of <object-table> and override element-setter.

// limited(<object-table>, of: <object>)
define method make-limited-collection
    (base-class :: type-union(singleton(<table>), singleton(<object-table>)),
     element-type == <object>,
     collection-type :: <limited-collection>,
     #rest supplied-keys, #key, #all-keys)
 => (instance :: <simple-object-table>)
  // The DRM discussion of '<simple-object-vector>' and
  // 'limited(<simple-vector>, of: <object>)' on page 223
  // implies that we should do this exactly so.
  apply(make, <simple-object-table>, supplied-keys);
end method make-limited-collection;

define class <limited-object-table>
    (<object-table>, <limited-collection-mixin>)
end class <limited-object-table>;

define sealed domain make (singleton(<limited-object-table>));

// limited(<object-table>, of: ...)
define method make-limited-collection
    (base-class :: type-union(singleton(<table>), singleton(<object-table>)),
     element-type :: <type>,
     collection-type :: <limited-collection>,
     #rest supplied-keys, #key, #all-keys)
 => (instance :: <limited-object-table>)
  apply(make, <limited-object-table>,
	collection-type: collection-type,
	supplied-keys);
end method make-limited-collection;

// This isn't especially efficient--we need to inline it and implement
// a constant folder to get the element-type.
define method element-setter
    (new-value :: <object>, collection :: <limited-object-table>,
     key :: <object>, #next next-method)
 => (element :: <object>)
  check-type(new-value,
	     collection.%limited-collection-type.limited-element-type);
  next-method();
end method element-setter;


//=========================================================================
//  Limited <simple-vector> Support Code
//=========================================================================
//  Some macros and support functions so we can declare all our subclasses
//  of <simple-vector> easily.

// Implement the DRM rules (plus an extension) for handling keyword
// arguments to 'make'.
define function process-simple-vector-keys
    (collection-type :: <limited-collection>,
     size :: type-union(<integer>, singleton($not-supplied)),
     fill :: <object>)
 => (requested-size :: <integer>)
  // Bruce wants us to default 'size:' in the useful fashion.
  let restricted-size = collection-type.limited-size-restriction;
  let requested-size :: <integer> =
    case
      instance?(size, <integer>) =>
	size;
      restricted-size =>
	restricted-size;
      otherwise =>
	0;
    end case;
  
  // XXX - These may not be the right errors to signal. Oh, well.
  if (restricted-size & requested-size ~= restricted-size)
    error("Requested vector size %= does not match size of %=",
	  size, collection-type);
  end if;
  if (requested-size > 0 & ~instance?(fill, collection-type.element-type))
    error("Cannot fill %= with %=", collection-type, fill);
  end if;
  requested-size;
end function process-simple-vector-keys;

//  This is a slightly more intelligent version of limited-vector-class.
//  It defines types which are properly hooked into the runtime type model.
//  We define methods on 'element-setter' and 'make-limited-collection' in
//  another macro below.
//  I nuked 'element-type' from this list, since we implement a general
//  version for <limited-collection-mixin>.
define macro %limited-simple-vector-class
  { %limited-simple-vector-class(?:name,
				 ?element-type:expression,
				 ?fill:expression) }
    => { begin
	   define sealed class ?name (<simple-vector>,
				      <limited-collection-mixin>)
	     sealed slot %elem :: ?element-type,
	       init-value: ?fill, init-keyword: fill:, sizer: size,
	       size-init-value: 0, size-init-keyword: size:;
	   end class;
           define sealed domain make (singleton(?name));
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

define macro %limited-simple-vector-setter
  { %limited-simple-vector-setter(?:name,
				  ?element-type:expression,
				  ?fill:expression) }
    => { begin
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
         end; }
end macro;

define macro %limited-simple-vector-maker
  { %limited-simple-vector-maker(?:name,
				 ?element-type:expression,
				 ?fill:expression) }
    => { begin
           define method make-limited-collection
               (base-class :: type-union(singleton(<vector>),
					 singleton(<simple-vector>)),
		element-type == ?element-type,
		collection-type :: <limited-collection>,
		#rest supplied-keys,
		#key fill = ?fill, size = $not-supplied, #all-keys)
	    => (instance :: limited(<simple-vector>, of: ?element-type))
	     let requested-size =
	       process-simple-vector-keys(collection-type, size, fill);
	     apply(make, ?name,
		   collection-type: collection-type,
		   size: requested-size, fill: fill,
		   supplied-keys);
	   end method make-limited-collection;
         end; }
end macro;

    
//=========================================================================
//  Limited <simple-vector> Types
//=========================================================================
//  We use a handy macro to declare our limited vector classes. It, in
//  turn, uses funky compiler magic. ;-)

// limited(<simple-vector>, of: <object>)
define method make-limited-collection
    (base-class :: type-union(singleton(<vector>),
			      singleton(<simple-vector>)),
     element-type == <object>,
     collection-type :: <limited-collection>,
     #rest supplied-keys,
     #key size = $not-supplied, fill, #all-keys)
 => (instance :: <simple-object-vector>)
  let requested-size = process-simple-vector-keys(collection-type, size, fill);
  // The DRM discussion of '<simple-object-vector>' and
  // 'limited(<simple-vector>, of: <object>)' on page 223
  // requires us to this exactly so.
  apply(make, <simple-object-vector>,
	size: requested-size, fill: fill,
	supplied-keys);
end method make-limited-collection;

// Simple integer types with compact representations.
%limited-simple-vector-class(<simple-uchar-vector>, <%u-char>, 0);
%limited-simple-vector-setter(<simple-uchar-vector>, <%u-char>, 0);
%limited-simple-vector-class(<simple-schar-vector>, <%s-char>, 0);
%limited-simple-vector-setter(<simple-schar-vector>, <%s-char>, 0);
%limited-simple-vector-class(<simple-ushort-vector>, <%u-short>, 0);
%limited-simple-vector-setter(<simple-ushort-vector>, <%u-short>, 0);
%limited-simple-vector-class(<simple-sshort-vector>, <%s-short>, 0);
%limited-simple-vector-setter(<simple-sshort-vector>, <%s-short>, 0);

// Classes to use with look-up-class-by-limited-integer-type.
define constant $lsvli-classes =
  vector(<simple-uchar-vector>, <simple-schar-vector>,
	 <simple-ushort-vector>, <simple-sshort-vector>,
	 <limited-simple-vector>);

// limited(<simple-vector>, of: limited(<integer>, ...))
// This is pretty slow, unfortunately.
define method make-limited-collection
    (base-class :: type-union(singleton(<vector>), singleton(<simple-vector>)),
     element-type :: <limited-integer>,
     collection-type :: <limited-collection>,
     #rest supplied-keys,
     #key fill, size = $not-supplied, #all-keys)
 => (instance :: <simple-vector>)
  let requested-size = process-simple-vector-keys(collection-type, size, fill);
  let class =
    look-up-class-by-limited-integer-type(element-type, $lsvli-classes);
  apply(make, class,
	collection-type: collection-type,
	size: requested-size, fill: fill,
	supplied-keys);
end method make-limited-collection;

// Standard optimized types with compact representations.
%limited-simple-vector-class(<simple-integer-vector>, <integer>, 0);
%limited-simple-vector-setter(<simple-integer-vector>, <integer>, 0);
%limited-simple-vector-maker(<simple-integer-vector>, <integer>, 0);

%limited-simple-vector-class(<simple-double-vector>, <double-float>, 0.0);
%limited-simple-vector-setter(<simple-double-vector>, <double-float>, 0.0);
%limited-simple-vector-maker(<simple-double-vector>, <double-float>, 0.0);

// limited(<simple-vector>, of: ...)
%limited-simple-vector-class(<limited-simple-vector>, <object>, 0);

// This isn't especially efficient--we need to inline it and implement
// a constant folder to get the element-type.
define sealed method element-setter
    (new-value :: <object>, vec :: <limited-simple-vector>, index :: <integer>)
 => new-value :: <object>;
  check-type(new-value, vec.%limited-collection-type.limited-element-type);
  if (index >= 0 & index < vec.size)
    %elem(vec, index) := new-value;
  else
    element-error(vec, index);
  end;
end;

define method make-limited-collection
    (base-class :: type-union(singleton(<vector>), singleton(<simple-vector>)),
     element-type :: <type>,
     collection-type :: <limited-collection>,
     #rest supplied-keys,
     #key fill, size = $not-supplied, #all-keys)
 => (instance :: <limited-simple-vector>)
  let requested-size = process-simple-vector-keys(collection-type, size, fill);
  apply(make, <limited-simple-vector>,
	collection-type: collection-type,
	size: requested-size, fill: fill,
	supplied-keys);
end method make-limited-collection;
