module: melange-support
rcs-header: $Header: /scm/cvs/src/d2c/runtime/melange/melange.dylan,v 1.1 1998/05/03 19:55:51 andreas Exp $

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
// d2c generated code.  Many of these are intended to support the code
// produced by Melange rather than being explicitly referenced by users.
//

// Usage: c-variable-ref(int: "&variable") { := expression }
//
define macro c-variable-ref
  { c-variable (?result-type:expression, ?:expression) }
    => { pointer-deref(?result-type, c-expr(ptr: ?expression), 0) }
  { c-variable (?result-type:token ?:expression) }
    => { pointer-deref(?result-type, c-expr(ptr: ?expression), 0) }
end;

define macro c-variable-ref-setter
  { c-variable (?value:expression, ?result-type:expression, ?:expression) }
    => { pointer-deref-setter(?value, ?result-type,
			      c-expr(ptr: ?expression), 0) }
  { c-variable (?value:expression, ?result-type:token ?:expression) }
    => { pointer-deref-setter(?value, ?result-type,
			      c-expr(ptr: ?expression), 0) }
end;

define open primary functional class <statically-typed-pointer> (<object>)
  slot raw-value :: <raw-pointer>, required-init-keyword: #"pointer";
end class;

define sealed method functional-==
    (class :: subclass(<statically-typed-pointer>),
     arg1 :: <statically-typed-pointer>, arg2 :: <statically-typed-pointer>)
 => (result :: <boolean>);
  let int1 :: <integer> = as(<integer>, arg1);
  let int2 :: <integer> = as(<integer>, arg2);
  int1 == int2;
end method functional-==;

// Default make method for pointer values.  You may create a vector by
// specifying "element-count:" and may adjust the size explicitly by
// specifying "extra-bytes:".
//
define method make
    (cls :: subclass(<statically-typed-pointer>), #next next,
     #rest rest, 
     #key extra-bytes :: <integer> = 0,
          pointer,
          element-count :: <integer> = 1,
     #all-keys)
 => (result :: <statically-typed-pointer>);
  if (pointer)
    select (pointer by instance?)
      <raw-pointer> =>
	next(cls, pointer: pointer);
      <statically-typed-pointer> =>
	next(cls, pointer: pointer.raw-value);
      <integer> =>
	next(cls, pointer: as(<raw-pointer>, pointer));
      otherwise =>
	error("Invalid pointer: keyword in make: %=", pointer);
    end select;
  else
    if (element-count < 1)
      error("Bad element-count: in make: %=", element-count);
    end if;

    let rawptr = call-out("GC_malloc", ptr:,
			  unsigned-int:
			    (content-size(cls) + extra-bytes) * element-count);
    let ptr = next(cls, pointer: rawptr);
    if (ptr = null-pointer) error("Make failed to allocate memory.") end if;

    apply(initialize, ptr, rest);

    ptr;
  end if;
end method make;

define method as
    (cls :: subclass(<statically-typed-pointer>),
     obj :: <statically-typed-pointer>)
 => (result :: <statically-typed-pointer>);
  make(cls, pointer: obj.raw-value);
end method as;

define method as
    (cls :: subclass(<statically-typed-pointer>), obj :: <raw-pointer>)
 => (result :: <statically-typed-pointer>);
  make(cls, pointer: obj);
end method as;

define method as
    (cls :: subclass(<statically-typed-pointer>), obj :: <integer>)
 => (result :: <statically-typed-pointer>);
  make(cls, pointer: as(<raw-pointer>, obj));
end method as;

define sealed method as
    (class == <integer>, obj :: <statically-typed-pointer>)
 => (result :: <integer>);
  as(<integer>, obj.raw-value);
end method as;

define sealed method as
    (class == <raw-pointer>, obj :: <statically-typed-pointer>)
 => (result :: <raw-pointer>);
  obj.raw-value;
end method as;

define constant null-pointer :: <statically-typed-pointer>
  = as(<statically-typed-pointer>, 0);

define sealed inline method signed-byte-at
    (ptr :: <statically-typed-pointer>, #key offset :: <integer> = 0)
 => (result :: <integer>);
  pointer-deref(char:, ptr.raw-value, offset);
end method signed-byte-at;

define sealed inline method signed-byte-at-setter
    (new :: <integer>,
     ptr :: <statically-typed-pointer>, #key offset :: <integer> = 0)
 => (result :: <integer>);
  pointer-deref(char:, ptr.raw-value, offset) := new;
end method signed-byte-at-setter;

define sealed inline method unsigned-byte-at
    (ptr :: <statically-typed-pointer>, #key offset :: <integer> = 0)
 => (result :: <integer>);
  pointer-deref(unsigned-char:, ptr.raw-value, offset);
end method unsigned-byte-at;

define sealed inline method unsigned-byte-at-setter
    (new :: <integer>,
     ptr :: <statically-typed-pointer>, #key offset :: <integer> = 0)
 => (result :: <integer>);
  pointer-deref(unsigned-char:, ptr.raw-value, offset) := new;
end method unsigned-byte-at-setter;

define sealed inline method signed-short-at
    (ptr :: <statically-typed-pointer>, #key offset :: <integer> = 0)
 => (result :: <integer>);
  pointer-deref(short:, ptr.raw-value, offset);
end method signed-short-at;

define sealed inline method signed-short-at-setter
    (new :: <integer>,
     ptr :: <statically-typed-pointer>, #key offset :: <integer> = 0)
 => (result :: <integer>);
  pointer-deref(short:, ptr.raw-value, offset) := new;
end method signed-short-at-setter;

define sealed inline method unsigned-short-at
    (ptr :: <statically-typed-pointer>, #key offset :: <integer> = 0)
 => (result :: <integer>);
  pointer-deref(unsigned-short:, ptr.raw-value, offset);
end method unsigned-short-at;

define sealed inline method unsigned-short-at-setter
    (new :: <integer>,
     ptr :: <statically-typed-pointer>, #key offset :: <integer> = 0)
 => (result :: <integer>);
  pointer-deref(unsigned-short:, ptr.raw-value, offset) := new;
end method unsigned-short-at-setter;

define sealed inline method signed-long-at
    (ptr :: <statically-typed-pointer>, #key offset :: <integer> = 0)
 => (result :: <integer>);
  pointer-deref(long:, ptr.raw-value, offset);
end method signed-long-at;

define sealed inline method signed-long-at-setter
    (new :: <integer>,
     ptr :: <statically-typed-pointer>, #key offset :: <integer> = 0)
 => (result :: <integer>);
  pointer-deref(long:, ptr.raw-value, offset) := new;
end method signed-long-at-setter;

define sealed inline method unsigned-long-at
    (ptr :: <statically-typed-pointer>, #key offset :: <integer> = 0)
 => (result :: <integer>);
  pointer-deref(long:, ptr.raw-value, offset);
end method unsigned-long-at;

define sealed inline method unsigned-long-at-setter
    (new :: <integer>,
     ptr :: <statically-typed-pointer>, #key offset :: <integer> = 0)
 => (result :: <integer>);
  pointer-deref(long:, ptr.raw-value, offset) := new;
end method unsigned-long-at-setter;

define sealed inline method longlong-at
    (ptr :: <statically-typed-pointer>, #key offset :: <integer> = 0)
 => (result :: <integer>);
  error("accesses to 'long long' data not currently supported");
end method longlong-at;

define sealed inline method longlong-at-setter
    (new :: <integer>,
     ptr :: <statically-typed-pointer>, #key offset :: <integer> = 0)
 => (result :: <integer>);
  error("accesses to 'long long' data not currently supported");
end method longlong-at-setter;

define sealed inline method pointer-at
    (ptr :: <statically-typed-pointer>,
     #key offset :: <integer> = 0, class)
 => (result :: <statically-typed-pointer>);
  as(class, pointer-deref(ptr:, ptr.raw-value, offset));
end method pointer-at;

define sealed inline method pointer-at-setter
    (new :: <statically-typed-pointer>,
     ptr :: <statically-typed-pointer>,
     #key offset :: <integer> = 0,
          class :: <class> = <statically-typed-pointer>)
 => (result :: <statically-typed-pointer>);
  pointer-deref(ptr:, ptr.raw-value, offset) := new.raw-value;
end method pointer-at-setter;

define sealed domain \= (<statically-typed-pointer>, <object>);
define sealed domain \= (<object>, <statically-typed-pointer>);

define inline method \=
    (ptr1 :: <statically-typed-pointer>, ptr2 :: <statically-typed-pointer>)
 => (result :: <boolean>);
  ptr1.raw-value == ptr2.raw-value;
end method;

define inline method \< 
    (ptr1 :: <statically-typed-pointer>, ptr2 :: <statically-typed-pointer>)
 => (res :: <boolean>);
  ptr1.raw-value < ptr2.raw-value;
end;

define sealed domain \< (<statically-typed-pointer>, <object>);
define sealed domain \< (<object>, <statically-typed-pointer>);

define sealed inline method \+
    (ptr :: <statically-typed-pointer>, bytes :: <integer>)
 => (res :: <statically-typed-pointer>);
  as(ptr.object-class, ptr.raw-value + bytes);
end;

define sealed inline method \+
    (bytes :: <integer>, ptr :: <statically-typed-pointer>)
 => (res :: <statically-typed-pointer>);
  as(ptr.object-class, ptr.raw-value + bytes);
end;

define sealed inline method \-
    (ptr1 :: <statically-typed-pointer>, ptr2 :: <statically-typed-pointer>)
 => (res :: <integer>);
  ptr1.raw-value = ptr2.raw-value;
end;

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
//    (cls :: limited(<class>, subclass-of: <statically-typed-pointer>))
    (cls :: <class>)
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
    (low-level-class :: <type>, high-level-value :: <object>)
 => (low-level-value :: <object>);

define open generic import-value
    (high-level-class :: <type>, low-level-value :: <object>)
 => (high-level-value :: <object>);

define method export-value
    (low-level-class :: <type>, high-level-value :: <object>)
 => (low-level-value :: <object>);
  as(low-level-class, high-level-value);
end method export-value;

define method import-value
    (high-level-class :: <type>, low-level-value :: <object>)
 => (high-level-value :: <object>);
  as(high-level-class, low-level-value);
end method import-value;

// Explicitly destroys C pointers, since we cannot garbage collect them.
//
define method destroy (ptr :: <statically-typed-pointer>) => ();
  call-out("GC_free", void:, ptr: ptr.raw-value);
end method destroy;

define functional class <machine-pointer> (<statically-typed-pointer>) 
end class;

// <C-string> corresponds to C's native "char *" type.  We provide basic
// functions so that it obeys the protocol of <string>.
//
define open functional class <c-string>
    (<stretchy-collection>, <string>, <statically-typed-pointer>) 
end class <c-string>;

// The above class has to be open for versatility.  This one can be
// sealed for efficiency.
define sealed functional class <sealed-c-string> (<c-string>) end class;
  
// We come up with an ambiguity in this special case, so define a method which
// resolves it.
// 
define method as
    (cls == <c-string>, value :: <c-string>) => (result :: <c-string>);
  value;
end method as;

define method type-for-copy (string :: <c-string>) => type :: <type>;
  <byte-string>;
end method type-for-copy;

define method make
    (cls :: one-of(<c-string>, <sealed-c-string>),
     #next next, #key size: sz = 0, fill = ' ', pointer)
 => (result :: <sealed-c-string>);
  if (pointer)
    next(<sealed-c-string>, pointer: pointer);
  else
    let result = next(<sealed-c-string>, element-count: sz + 1);
    let fill-byte = as(<integer>, fill);
    for (i from 0 below sz)
      unsigned-byte-at(result, offset: i) := fill-byte;
    end for;
    unsigned-byte-at(result, offset: sz) := 0;
    result;
  end if;
end method make;

define sealed inline method forward-iteration-protocol (str :: <c-string>)
 => (initial-state :: <object>,
     limit :: <object>,
     next-state :: <function>,
     finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>,
     current-element-setter :: <function>,
     copy-state :: <function>);
  values(0, #f,
	 method (str, state) state + 1 end method,
	 method (str, state, limit)
	   str = null-pointer | unsigned-byte-at(str, offset: state) == 0;
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

define method pointer-value (ptr :: <c-string>, #key index = 0)
  as(<character>, unsigned-byte-at(ptr, offset: index));
end method pointer-value;

define method pointer-value-setter
    (char :: <character>, ptr :: <c-string>, #key index = 0);
  unsigned-byte-at(ptr, offset: index) := as(<integer>, char);
end method pointer-value-setter;

define sealed method content-size
    (value :: subclass(<c-string>))
 => (result :: <integer>);
  1;
end method content-size;

define sealed  method size (string :: <c-string>)
 => result :: <integer>;
  case
    (string = null-pointer) => 0;
    otherwise => call-out("strlen", int:, ptr: string.raw-value);
  end case;
end method size;

define sealed  method empty? (string :: <c-string>) => (result :: <boolean>);
  string = null-pointer | unsigned-byte-at(string) = 0;
end method empty?;

define constant space-byte = as(<integer>, ' ');
define sealed  method size-setter (value :: <integer>, string :: <c-string>)
 => value :: <integer>;
  let sz = string.size;
  case
    value = null-pointer =>
      error("Cannot set size of null <c-string>s");
    value == sz =>
      #f;
    value > sz =>
      for (i from sz below value)
	unsigned-byte-at(string, offset: i) := space-byte;
      end for;
      unsigned-byte-at(string, offset: value) := 0;
    value < 0 =>
      error("Cannot set size below zero.");
    value < sz =>
      unsigned-byte-at(string, offset: value) := 0;
  end case;
end method size-setter;

// These methods actually attempt to avoid writing past the end of a string.
// This makes them somewhat slow.  If faster (unsafe) access is required, you
// should use pointer-value instead.
//
define sealed  method element
    (vec :: <c-string>, index :: <integer>, #key default = $not-supplied)
 => (result :: <character>);
  let sz = vec.size;
  case
    index >= 0 & index < sz =>
      pointer-value(vec, index: index);
    default == $not-supplied =>
      error("No such element in %=: %=", vec, index);
    otherwise =>
      default;
  end case;
end method element;
    
define sealed  method element-setter
    (char :: <byte-character>, vec :: <c-string>, index :: <integer>)
 => char :: <byte-character>;
  let sz = vec.size;
  if (index < 0) error("Negative keys not allowed in strings.") end if;
  if (index >= sz) vec.size := index + 1 end if;
  pointer-value(vec, index: index) := char;
end method element-setter;

define sealed  method \<
    (str1 :: <c-string>, str2 :: <c-string>)
 => result :: <boolean>;
  case
    (str1.empty?) => ~str2.empty?;
    (str2.empty?) => #f;
    otherwise =>
      call-out("strcmp", int:, ptr: str1.raw-value, ptr: str2.raw-value) < 0;
  end case;
end method \<;

define sealed  method \=
    (str1 :: <c-string>, str2 :: <c-string>)
 => result :: <boolean>;
  let empty1 = str1.empty?;
  let empty2 = str2.empty?;
  case
    empty1 & empty2 => #t;
    empty1 | empty2 => #f;
    otherwise =>
      call-out("strcmp", int:, ptr: str1.raw-value, ptr: str2.raw-value) == 0;
  end case;
end method \=;

// This is a very common operation, so let's make it fast.
//
define sealed  method as (cls == <c-string>, str :: <byte-string>)
 => string :: <c-string>;
  let sz = str.size;
  let result
    = as(<c-string>, call-out("GC_malloc", ptr:, unsigned-int: sz + 1));
  for (i from 0 below sz)
    unsigned-byte-at(result, offset: i) := as(<integer>, str[i]);
  end for;
  unsigned-byte-at(result, offset: sz) := 0;
  result;
end method as;

// This is a very common operation, so let's make it fast.
//
define sealed  method as (cls == <byte-string>, str :: <c-string>)
 => string :: <byte-string>;
  let sz = str.size;
  let result = make(<string>, size: sz);
  for (i from 0 below sz)
    result[i] := as(<character>, unsigned-byte-at(str, offset: i));
  end for;
  result;
end method as;

//------------------------------------------------------------------------

define method export-value (cls == <integer>, value :: <boolean>)
 => (result :: <integer>);
  if (value) 1 else 0 end if;
end method export-value;

define method import-value (cls == <boolean>, value :: <integer>)
 => (result :: <boolean>);
  value ~= 0;
end method import-value;

//------------------------------------------------------------------------

// <c-vector> can correspond to either true C vectors or to C pointers which
// are treated as vectors.  In the latter case, we won't know the size, so we
// will define it as "#f" (just like infinite lists have) and make sure that
// the iteration protocol can handle this.
//
// C vector types are automatically made members of <c-vector> by Melange.
// Pointer types will have to explicitly declare membership via a
// "superclasses:" option.
//
define open functional class <c-vector>
    (<vector>, <statically-typed-pointer>) 
end class;

// C does not do bounds checking on vectors, and many of the interface files
// make use of this weakngess.  Therefore, we do no bounds checking either.
// Caveat emptor.
//
define method element
    (vec :: <c-vector>, index :: <integer>, #key default)
 => (result :: <object>);
  pointer-value(vec, index: index);
end method element;

// C does not do bounds checking on vectors, and many of the interface files
// make use of this weakngess.  Therefore, we do no bounds checking either.
// Caveat emptor.
//
define method element-setter
    (value :: <object>, vec :: <c-vector>, index :: <integer>)
 => value :: <object>;
  pointer-value(vec, index: index) := value;
end method element-setter;

// For "normal" ponters, the size is "#f", indicating that the size is unknown
// and potentially infinite.  However, subtypes can redefine this to higher
// values and the iteration protocol will still work.
//
define method size (vec :: <c-vector>) => (result :: <false>);
  #f;
end method size;

// Straightforward vector FIP.  We duplicate it here to avoid problems with
// possible recursive definitions of element.
//
define method forward-iteration-protocol (vec :: <c-vector>)
 => (initial-state :: <object>,
     limit :: <object>,
     next-state :: <function>,
     finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>,
     current-element-setter :: <function>,
     copy-state :: <function>);
  values(0, vec.size,
	 method (c, s) s + 1 end,	// next-state
	 method (c, s, l) s == l end, // finished-state?
	 method (c, s) s end,	// current-key
	 method (c, s) pointer-value(c, index: s) end, // current-element
	 method (v, c, s) pointer-value(c, index: s) := v end, // ""-setter
	 method (c, s) s end); // copy-state
end method forward-iteration-protocol;

//------------------------------------------------------------------------

// <Function-pointer>s correspond to generic pointers to any function.
// For the nonce, all function types are equated to this type, so that
// we don't need to deal with canonicalizing signatures.
//
define open functional class <function-pointer>
    (<statically-typed-pointer>) 
end class <function-pointer>;
