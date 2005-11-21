module: classes
copyright: see below

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


// Compile-time representation of <designator-class>
// 
define abstract class <cdclass> (<cclass>)
  //
  // How big the designated type is, in C sizeof units
  slot size-of :: <integer>,
    init-value: 0, init-keyword: size:;
  //
  // Type's required alignment, a power of 2 in C sizeof units
  slot alignment-of :: <integer>,
    init-value: 0, init-keyword: alignment:;
  //
  // Representation of the FFI type
  slot designated-representation :: false-or(<representation>),
    init-value: #f, init-keyword: representation:;
  //
  // Type that a pointer designator type refers to
  slot referenced-type :: false-or(<cdclass>),
    init-value: #f, init-keyword: referenced-type:;
  //
  // Pointer designator type that refers to this type, if any
  slot pointer-type :: false-or(<cdclass>),
    init-value: #f, init-keyword: pointer-type:;
  //
  // Superclass for generating the pointer-type, if it doesn't exist
  // and is needed
  slot pointer-type-superclass :: false-or(<cclass>),
    init-value: #f, init-keyword: pointer-type-superclass:;
  //
  // Corresponding Dylan import and export types
  slot import-type :: false-or(<ctype>),
    init-value: #f, init-keyword: import-type:;
  slot export-type :: false-or(<ctype>),
    init-value: #f, init-keyword: export-type:;
  //
  // Slots of designated FFI structures
  slot struct-slot-infos :: <simple-object-vector> = #[],
    init-keyword: struct-slots:;
  //
  // Functions for indirecting through an instance of the pointer-type
  slot indirect-getter :: false-or(<ct-value>),
    init-value: #f, init-keyword: indirect-getter:;
  slot indirect-setter :: false-or(<ct-value>),
    init-value: #f, init-keyword: indirect-setter:;
end class;

define sealed domain make (singleton(<cdclass>));
define sealed domain initialize (<cdclass>);

define method initialize
    (class :: <cdclass>, #next next-method, #key, #all-keys)
    => ();
  next-method();

end method;

define class <defined-cdclass> (<cdclass>, <defined-cclass>)
  // no additional slots
end class;

define sealed domain make (singleton(<defined-cdclass>));

define method ct-value-cclass (object :: <cdclass>) => res :: <cclass>;
  dylan-value(#"<designator-class>");
end;

define class <struct-slot-info> (<eql-ct-value>, <identity-preserving-mixin>)
  //
  // The slot's computed offset
  slot struct-slot-offset :: <integer>, init-value: 0;
  //
  // The field's C type
  slot struct-slot-c-type :: <cdclass>,
    required-init-keyword: c-type:;
  //
  // The original C name of the object
  slot struct-slot-c-name :: false-or(<byte-string>),
    init-keyword: c-name:, init-value: #f;
  //
  // The getter generic function definition
  slot struct-slot-getter :: <variable>,
    required-init-keyword: getter:;
  //
  // The setter generic function definition, or #f if the slot is read-only
  slot struct-slot-setter :: false-or(<variable>),
    init-keyword: setter:, init-value: #f;
  //
  // The address-getter generic function definition, or #f if none is defined
  slot struct-slot-address-getter :: false-or(<variable>),
    init-keyword: address-getter:, init-value: #f;
  //
  // The array dimensions of the slot, or #f if it is not an array
  slot struct-slot-dimensions :: false-or(type-union(<sequence>, <integer>)),
    init-keyword: dimensions:, init-value: #f;
  //
  // The bitfield width of the slot, or #f if it is not a bitfield
  slot struct-slot-bitfield-width :: false-or(<integer>),
    init-keyword: width:, init-value: #f;
end class;

define sealed domain make (singleton(<struct-slot-info>));

define method ct-value-cclass
    (object :: <struct-slot-info>)
 => (res :: <cclass>);
  dylan-value(#"<struct-slot-descriptor>");
end;


