module: dylan-user
rcs-header: $Header: /scm/cvs/src/d2c/compiler/fer-transform/fer-transform-exports.dylan,v 1.3 2001/03/17 03:43:32 bruce Exp $
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

define library compiler-fer-transform
  use dylan;
  use compiler-base;
  use compiler-front;

  export
    fer-transform,
    null-optimizer;
end library compiler-fer-transform;

define module fer-transform
  use common;
  use utils;
  use errors;
  use compile-time-values;
  use platform,
    import: {*current-target*, platform-integer-length};
  use names;
  use definitions;
  use variables, exclude: {<renaming>};
  use flow;
  use front;
  use ctype;
  use classes;
  use signature-interface;
  use source;
  use builder-interface;
  use policy;
  use primitives;
  use transformers;
  use compile-time-functions;
  use function-definitions;
  
  export 
    add-type-checks, 
    convert-component-to-ssa,
    traverse-component;
end module fer-transform;

define module null-optimizer
  use common;
  use flow, import: {<component>};
  use front, import: {dump-fer};
  use utils, import: {dformat};
  use xep-tools, import: {build-local-xeps};
  use abstract-optimizer;
  use fer-transform;

  export
    <null-optimizer>;
end module null-optimizer;
