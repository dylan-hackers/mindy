documented: #t
module: c-declarations
copyright: see below
           This code was produced by the Gwydion Project at Carnegie Mellon
           University.  If you are interested in using this code, contact
           "Scott.Fahlman@cs.cmu.edu" (Internet).

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

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
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
// "define-interface" in a set order.  This ordering is shown in the list
// in "exports.dylan".
//======================================================================

// Returns the number of bytes required to store instances of some C type.
// This was written on HP, but seems valid on all *nix platforms.
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
     #key alignment: unused :: <integer> = $default-alignment)
 => (this-slot-end :: <integer>, this-slot-start :: <integer>);
  if (instance?(slot-type, <typedef-declaration>))
    aligned-slot-position(prev-slot-end, slot-type.type);
  else
    let (size, alignment) =
      select (slot-type by instance?)
	<predefined-type-declaration>, <function-type-declaration>,
	<pointer-declaration>, <enum-declaration>, <struct-declaration>,
	<union-declaration>, <vector-declaration> =>
	  let sz = c-type-size(slot-type);
	  let align = c-type-alignment(slot-type);
	  values (sz, align);
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

// do-coalesce-members -- internal
//
// Initializes the "coalesced-members" field.  See c-decl.dylan for
// more details. 
//
// Assumptions:  Adjacent bitfields are all grouped together so long
// as they fit within a single word.  This can yield different
// alignments than (for example) packing into individual bytes.  More
// study will be required to determine what different compilers do.
//
define function do-coalesce-members (decl :: <structured-type-declaration>)
 => (members :: <sequence>)
  let result :: <list> = #();
  for (member :: <declaration> in decl.members)
    let decl-type = member.type;
    if (instance?(decl-type, <bitfield-declaration>))
      let composite = first(result, default: #f);
      if (~instance?(composite, <coalesced-bitfields>)
	    | (composite.bit-size + decl-type.bits-in-field
		 > $default-alignment * 8))
		    let name = anonymous-name();
				composite := make(<coalesced-bitfields>, name: name, dylan-name: name);
				result := pair(composite, result);
      end if;
      decl-type.composite-field := composite;
      decl-type.start-bit := composite.bit-size;
      composite.fields := add!(composite.fields, member);
      let size = composite.bit-size + decl-type.bits-in-field;
      if (size > composite.type.unix-type-size)
	case
	  (size <= unsigned-char-type.unix-type-size) =>
	    composite.type := unsigned-char-type;
	  (size <= unsigned-short-type.unix-type-size) =>
	    composite.type := unsigned-short-type;
	  (size <= unsigned-int-type.unix-type-size) =>
	    composite.type := unsigned-int-type;
	  (size <= unsigned-long-type.unix-type-size) =>
	    composite.type := unsigned-long-type;
	end case;
      end if;
      composite.bit-size := size;
    else
      result := pair(member, result);
    end if;
  end for;
  decl.%coalesced-members := reverse!(result);
end function do-coalesce-members;

define method unix-type-alignment (decl :: <struct-declaration>)
  => size :: <integer>;
  if (decl.members)
    reduce(method (al :: <integer>, member)
	     max(al, unix-type-alignment(member.type)) end method,
	   1, decl.coalesced-members);
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
    let base-size =
      reduce(method (sz :: <integer>, member)
	       max(sz, unix-type-size(member.type)) end method,
	     0, decl.members);
    let align = unix-type-alignment(decl);
    let rem = remainder(base-size, align);
    if (rem = 0)
      base-size;
    else
      (truncate/(base-size, align) * align) + align;
    end if;
  else
    0;
  end if;
end method unix-type-size;

define method unix-type-size (decl :: <struct-declaration>)
 => size :: <integer>;
  if (decl.members)
    let base-size =
      reduce(method (sz :: <integer>, member)
	       aligned-slot-position(sz, member.type); end method,
	     0, decl.coalesced-members);
    let align = unix-type-alignment(decl);
    let rem = remainder(base-size, align);
    if (rem = 0)
      base-size;
    else
      (truncate/(base-size, align) * align) + align;
    end if;
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
