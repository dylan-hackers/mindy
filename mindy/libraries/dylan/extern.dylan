module: extern
rcs-header: $Header: /home/housel/work/rcs/gd/src/mindy/libraries/dylan/extern.dylan,v 1.3 1995/03/27 22:31:47 rgs Exp $

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
// This file contains definitions useful for calling native C functions from
// within Mindy.  Many of these are intended to support the code produced by
// Melange rather than being explicitly referenced by users.
//

define constant gcf-unbound = pair(#f, #f); // hack

// This is potentially useful, but will probably be overshadowed by Melange.
// It combines the functionality of "find-c-function" and
// "constrain-c-function" to get usable function in one step.
//
define method get-c-function
    (name :: <string>,
     #key args, rest = ~args, result = <object>, file = gcf-unbound)
 => (result :: <c-function>);
  let real-args = if (args) as(<list>, args) else #() end if;
  let real-result = if (instance?(result, <sequence>)) as(<list>, result)
		    else list(result)
		    end if;
  let fun = if (file == gcf-unbound)
	      find-c-function(name)
	    else
	      find-c-function(name, file: file);
	    end if;
  fun & constrain-c-function(fun, real-args, rest, real-result);
end method get-c-function;

// These will be used by "make" and "destroy".
//
define constant malloc = get-c-function("malloc", args: list(<integer>),
					result: <statically-typed-pointer>);
define constant calloc = get-c-function("calloc",
					args: list(<integer>, <integer>),
					result: <statically-typed-pointer>);
define constant c-free = get-c-function("free",
					args: list(<statically-typed-pointer>),
					result: #());

// Uses "free" to deallocate a native C pointer.
//
define generic destroy (ptr :: <statically-typed-pointer>) => ();

// Generic "dereference" operation.  Methods will be defined for individual
// pointer types.
//
define open generic pointer-value
    (ptr :: <statically-typed-pointer>, #key index);
define open generic pointer-value-setter
    (value :: <object>, ptr :: <statically-typed-pointer>, #key index);

// Generic function which will be extended with methods that describe the
// sizes of particular pointer types.  If the size is unknown, it will return
// 0.  "Structure-size" is defined as an alias for compatibility with Creole.
//
define open generic content-size
    (cls :: limited(<class>, subclass-of: <statically-typed-pointer>))
 => (result :: <integer>);

define constant structure-size = content-size;

// The import-value and export-value functions can be extended to add
// user-defined type mapping to Melange.  Default methods are provided for
// <statically-typed-pointer> and for <boolean>.
//
// Note that these functions are not compatible with the identically named
// functions in Creole.
//
define open generic export-value
    (low-level-class :: <class>, high-level-value :: <object>)
 => (low-level-value :: <object>);

define open generic import-value
    (high-level-class :: <class>, low-level-value :: <object>)
 => (high-level-value :: <object>);

define method export-value
    (low-level-class :: <class>, high-level-value :: <object>)
 => (low-level-value :: <object>);
  as(low-level-class, high-level-value);
end method export-value;

define method import-value
    (high-level-class :: <class>, low-level-value :: <object>)
 => (high-level-value :: <object>);
  as(high-level-class, low-level-value);
end method import-value;

// Default make method for pointer values.  You may create a vector by
// specifying "element-count:" and may adjust the size explicitly by
// specifying "extra-bytes:".
//
define method make
    (cls :: limited(<class>, subclass-of: <statically-typed-pointer>),
     #rest rest,
     #key extra-bytes :: <integer> = 0,
          pointer,
          element-count :: <integer> = 1,
     #all-keys)
 => (result :: <statically-typed-pointer>);
  if (pointer)
    select (pointer by instance?)
      <statically-typed-pointer>,
      <integer> =>
	as(cls, pointer);
      otherwise =>
	error("Invalid pointer: keyword in make: %=", pointer);
    end select;
  else
    if (element-count < 1)
      error("Bad element-count: in make: %=", element-count);
    end if;

    let ptr = as(cls,
		 calloc((content-size(cls) + extra-bytes), element-count));
    if (ptr = null-pointer) error("Make failed to allocate memory.") end if;

    apply(initialize, ptr, rest);

    ptr;
  end if;
end method make;

// Explicitly destroys C pointers, since we cannot garbage collect them.
//
define method destroy (ptr :: <statically-typed-pointer>) => ();
  c-free(ptr);
end method destroy;

define class <machine-pointer> (<statically-typed-pointer>) end class;

// <C-string> corresponds to C's native "char *" type.  We provide basic
// functions to that it obeys the protocol of <string>.
//
define class <c-string> (<string>, <statically-typed-pointer>) 
end class <c-string>;

// We come up with an ambiguity in this special case, so define a method which
// resolves it.
// 
define method as
    (cls == <c-string>, value :: <c-string>) => (result :: <c-string>);
  value;
end method as;

// We might as well use the native c library routines when they do the right
// thing anyway.
//
define constant strcmp
  = get-c-function("strcmp", args: list(<c-string>, <c-string>),
		   result: <integer>);
define constant strlen
  = get-c-function("strlen", args: list(<c-string>), result: <integer>);

define method class-for-copy(string :: <c-string>)
  <byte-string>;
end method class-for-copy;

define method make(cls :: limited(<class>, subclass-of: <c-string>),
		   #next next, #key size: sz = 0, fill = ' ')
  let result = next(cls, element-count: sz + 1);
  let fill-byte = as(<integer>, fill);
  for (i from 0 below sz)
    unsigned-byte-at(result, offset: i) := fill-byte;
  end for;
  unsigned-byte-at(result, offset: sz) := 0;
  result;
end method make;

define method forward-iteration-protocol(str :: <c-string>)
  values(0, #f,
	 method (str, state) state + 1 end method,
	 method (str, state, limit)
	   unsigned-byte-at(str, offset: state) == 0;
	 end method,
	 method (str, state) state end method,
	 method (str, state)
	   as(<character>, unsigned-byte-at(str, offset: state));
	 end method,
	 method (value :: <character>, str, state)
	   unsigned-byte-at(str, offset: state) := as(<integer>, value);
	 end method,
	 method (str, state) state end method);
end method forward-iteration-protocol;

define method \<
    (str1 :: <c-string>, str2 :: <c-string>)
 => result :: <object>;
  strcmp(str1, str2) < 0;
end method \<;

define method size (string :: <c-string>)
 => result :: <integer>;
  strlen(string);
end method size;

// This is a very common operation, so let's make it fast.
//
define method as (cls == <c-string>, str :: <byte-string>)
  let sz = str.size;
  let result = as(<c-string>, malloc(sz + 1));
  for (i from 0 below sz)
    unsigned-byte-at(result, offset: i) := as(<integer>, str[i]);
  end for;
  unsigned-byte-at(result, offset: sz) := 0;
  result;
end method as;

// This is a very common operation, so let's make it fast.
//
define method as (cls == <byte-string>, str :: <c-string>)
  let sz = str.size;
  let result = make(<string>, size: sz);
  for (i from 0 below sz)
    result[i] := as(<character>, unsigned-byte-at(str, offset: i));
  end for;
  result;
end method as;

define method export-value (cls == <integer>, value :: <boolean>)
 => (result :: <integer>);
  if (value) 1 else 0 end if;
end method export-value;

define method import-value (cls == <boolean>, value :: <integer>)
 => (result :: <boolean>);
  value ~= 0;
end method import-value;

// Normal statically typed pointers can be regarded as vectors of length 1.
// However, C sometimes allows you to access more than the first element, so
// we do no bounds checking.
//
define method element
    (vec :: <statically-typed-pointer>, index :: <integer>, #key default)
 => (result :: <object>);
  pointer-value(vec, index: index);
end method element;

// For "normal" ponters, the size is wired in as "1".  However, subtypes can
// redefine this to higher values and the iteration protocol will still work.
//
define method size
    (vec :: <statically-typed-pointer>) => (result :: <integer>);
  1;
end method size;

// Straightforward vector FIP.  We duplicate it here to avoid problems with
// possible recursive definitions of element.
//
define method forward-iteration-protocol (vec :: <statically-typed-pointer>);
  values(0, vec.size,
	 method (c, s) s + 1 end,	// next-state
	 method (c, s, l) s >= l end, // finished-state?
	 method (c, s) s end,	// current-key
	 method (c, s) pointer-value(c, index: s) end, // current-element
	 method (v, c, s) pointer-value(c, index: s) := v end, // ""-setter
	 method (c, s) s end); // copy-state
end method forward-iteration-protocol;
