module: dylan-user
RCS-header: $Header: /scm/cvs/src/d2c/runtime/melange/exports.dylan,v 1.1 1998/05/03 19:55:51 andreas Exp $

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
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

define library melange-support
  use dylan;
  export melange-support;
end library melange-support;

define module melange-support
  use dylan;
  use extensions, export: {subclass};
  use system, export: {call-out, c-include, c-decl, c-expr};

  export
    c-variable-ref, c-variable-ref-setter,
    <statically-typed-pointer>, raw-value, null-pointer,
    signed-byte-at, signed-byte-at-setter,
    unsigned-byte-at, unsigned-byte-at-setter, signed-short-at,
    signed-short-at-setter, unsigned-short-at, unsigned-short-at-setter,
    signed-long-at, signed-long-at-setter, unsigned-long-at,
    unsigned-long-at-setter, longlong-at, longlong-at-setter,
    pointer-at, pointer-at-setter,

    destroy, pointer-value, pointer-value-setter, content-size,
    structure-size, export-value, import-value, <machine-pointer>,
    <c-string>, <c-vector>, <function-pointer>;
end module melange-support;
