module: dylan-user
RCS-header: $Header: /scm/cvs/src/d2c/runtime/melange/exports.dylan,v 1.12.8.1 2004/10/12 01:37:16 gabor Exp $

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998 - 2004  Gwydion Dylan Maintainers
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

define library melange-support
  use dylan;
  export melange-support;
end library melange-support;

define module melange-support
  use dylan;
  use extensions, export: {subclass, <double-integer>};
  use system, export: {call-out, c-include, c-system-include, c-decl, c-local-decl,
                       c-expr, c-struct-field, c-struct-field-setter,
                       callback-method, callback-entry};

  use magic, exclude: {\without-bounds-checks };

  export
    c-variable-ref, c-variable-ref-setter, // deprecated for new work, will be removed after gd 2.4
    c-variable, c-variable-setter, // use these for development
    <statically-typed-pointer>, raw-value, null-pointer,
    signed-byte-at, signed-byte-at-setter,
    unsigned-byte-at, unsigned-byte-at-setter, signed-short-at,
    signed-short-at-setter, unsigned-short-at, unsigned-short-at-setter,
    signed-long-at, signed-long-at-setter, unsigned-long-at,
    unsigned-long-at-setter, longlong-at, longlong-at-setter,
    unsigned-longlong-at, unsigned-longlong-at-setter,
    float-at, float-at-setter, double-at, double-at-setter,
    long-double-at, long-double-at-setter, pointer-at, pointer-at-setter,

    destroy, pointer-value, pointer-value-setter, content-size,
    structure-size, export-value, import-value, <machine-pointer>,
    <c-string>, <c-vector>, <function-pointer>,

    $null-pointer, <void>,

    c-struct-members;
end module melange-support;
