documented: #t
module: c-declarations
copyright: Copyright (C) 1994, Carnegie Mellon University
           All rights reserved
           This code was produced by the Gwydion Project at Carnegie Mellon
           University.  If you are interested in using this code, contact
           "Scott.Fahlman@cs.cmu.edu" (Internet).
rcs-header: $Header:

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
// All rights reserved.
//
//======================================================================

//======================================================================
// c-decl.dylan encapsulates definitions and "standard" functions for the
// <declaration> class.  Other files in the c-declarations model handle the
// interface to the parser (c-decl-state.dylan) and write out dylan code
// corresponding to the declarations (c-decl-write.dylan).
//
// The operations defined in this file are designed to be called from
// "define-interface" in a set order.  This ordering is shown in the exports
// list below.
//======================================================================

define module c-declarations
  use dylan;
  use extensions, exclude: {format, <string-table>};
  use regular-expressions;
  use streams;
  use format;

  // We completely encapsulate "c-parse" and only pass out the very few 
  // objects that will be needed by "define-interface".  Note that the 
  // classes are actually defined within this module but are exported
  // from c-parse.
  use c-parse, export: {<declaration>, <parse-state>, parse, parse-type,
			constant-value, true-type};

  use c-lexer;			// Tokens are used in process-type-list and
				// make-struct-type
  use portability;              // constants for size of C data types

  export
    // Basic type declarations
    <function-declaration>, <structured-type-declaration>,
    <struct-declaration>, <union-declaration>, <variable-declaration>,
    <constant-declaration>, <typedef-declaration>, <pointer-declaration>,
    <vector-declaration>,

    // Preliminary "set declaration properties phase"
    ignored?-setter, find-result, find-parameter, find-slot,
    argument-direction-setter, constant-value-setter, getter-setter,
    setter-setter, read-only-setter, sealed-string-setter, excluded?-setter,
    exclude-slots, equate, remap, rename, superclasses-setter, pointer-equiv,
    dylan-name,

    // "Import declarations phase" 
    declaration-closure, // also calls compute-closure

    // "Name computation phase"
    apply-options, apply-container-options, // also calls find-dylan-name,
					    // compute-dylan-name

    // "Write declaration phase"
    write-declaration, 
    write-file-load, write-mindy-includes,

    // Miscellaneous
    getter, setter, sealed-string, excluded?,
    canonical-name,declarations;
end module c-declarations;


// Returns the number of bytes required to store instances of some C type.
// This was written on HP, but should be valid on all *nix platforms.
//
define generic unix-type-size (type :: <type-declaration>);

// Returns the number of bytes of alignment of a particular type.
// This was written on HP, but should be valid for all *nix platforms.
//
define generic unix-type-alignment (type :: <type-declaration>);

define constant c-type-size = unix-type-size;
define constant c-type-alignment = unix-type-alignment;
define constant $default-alignment :: <integer> = 4;


define method aligned-slot-position
    (prev-slot-end :: <integer>, slot-type :: <type-declaration>,
     #key alignment: alignment :: <integer> = $default-alignment)
 => (this-slot-end :: <integer>, this-slot-start :: <integer>);
  if (instance?(slot-type, <typedef-declaration>))
    aligned-slot-position(prev-slot-end, slot-type.type);
  else
    let size =
      select (slot-type by instance?)
	<predefined-type-declaration>, <function-type-declaration>,
	<pointer-declaration>, <enum-declaration>, <struct-declaration>,
	<union-declaration>, <vector-declaration> =>
	  let sz = c-type-size(slot-type);
	  sz;
	otherwise =>
	  error("Unhandled c type in aligned-slot-position");
      end select;
    let align-temp = prev-slot-end + alignment - 1;
    let slot-start = align-temp - remainder(align-temp, alignment);
    values(slot-start + size, slot-start);
  end if;
end method aligned-slot-position;


// Methods for unix-type-alignment

define method unix-type-alignment (type :: <type-declaration>)
 => size :: <integer>;
  0; // This is probably an error
end method unix-type-alignment;

define method unix-type-alignment (decl :: <union-declaration>)
 => size :: <integer>;
  if (decl.members)
    reduce(method (al :: <integer>, member)
	     max(al, unix-type-alignment(member.type)) end method,
	   1, decl.members);
  else
    1;
  end if;
end method unix-type-alignment;

define method unix-type-alignment (decl :: <struct-declaration>)
  => size :: <integer>;
  if (decl.members)
    reduce(method (al :: <integer>, member)
	     max(al, unix-type-alignment(member.type)) end method,
	   1, decl.members);
  else
    1;
  end if;
end method unix-type-alignment;

define method unix-type-alignment (decl :: <enum-declaration>)
  => size :: <integer>;
  $enum-size;   // Same as data structure that represents enum, and alignment
                // is the same as size for atomic data on the HP.
end method unix-type-alignment;

define method unix-type-alignment (decl :: <pointer-declaration>)
 => size :: <integer>;
  $pointer-size;
end method unix-type-alignment;

define method unix-type-alignment (vector :: <vector-declaration>)
 => size :: <integer>;
  // The first expression is incorrect.  Change it.
  if(vector.length)
    unix-type-alignment(vector.pointer-equiv.referent);
  else
    1;
  end if;
end method unix-type-alignment;

define method unix-type-alignment (decl :: <function-type-declaration>)
 => size :: <integer>;
  $function-pointer-size;
end method unix-type-alignment;

define method unix-type-alignment (typedef :: <typedef-declaration>)
 => size :: <integer>;
  unix-type-alignment(typedef.type);
end method unix-type-alignment;

define method unix-type-alignment (decl :: <predefined-type-declaration>)
 => size :: <integer>;
  decl.unix-type-size;    // I believe that this is true, in general.
end method unix-type-alignment;


// Methods for unix-type-size

define method unix-type-size (type :: <type-declaration>)
 => size :: <integer>;
  0;
end method unix-type-size;

define method unix-type-size (decl :: <union-declaration>)
 => size :: <integer>;
  if (decl.members)
    reduce(method (sz :: <integer>, member)
	     max(sz, unix-type-size(member.type)) end method,
	   0, decl.members);
  else
    0;
  end if;
end method unix-type-size;

define method unix-type-size (decl :: <struct-declaration>)
 => size :: <integer>;
  if (decl.members)
    reduce(method (sz :: <integer>, member)
	     aligned-slot-position(sz, member.type,
				   alignment: unix-type-alignment(decl))
	   end method,
	   0, decl.members);
  else
    0;
  end if;
end method unix-type-size;

define method unix-type-size (type :: <enum-declaration>) => size :: <integer>;
  $enum-size;
end method unix-type-size;

define method unix-type-size (pointer :: <pointer-declaration>)
 => size :: <integer>;
  $pointer-size;
end method unix-type-size;

define method unix-type-size (vector :: <vector-declaration>)
 => size :: <integer>;
  // Portability note: Might some compilers do alignment of elements?
  vector.pointer-equiv.referent.unix-type-size * (vector.length | 0);
end method unix-type-size;

define method unix-type-size (type :: <function-type-declaration>)
 => size :: <integer>;
  $function-pointer-size;
end method unix-type-size;

define method unix-type-size (type :: <predefined-type-declaration>)
 => size :: <integer>;
  type-size-slot(type);
end method unix-type-size;

define method unix-type-size (typedef :: <typedef-declaration>)
 => size :: <integer>;
  unix-type-size(typedef.type);
end method unix-type-size;
