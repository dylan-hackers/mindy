module: dylan-user
rcs-header: $Header: /scm/cvs/src/d2c/compiler/fer-transform/fer-transform-exports.dylan,v 1.8 2003/06/24 21:00:07 andreas Exp $
copyright: see below


//======================================================================
//
// Copyright (c) 2000, 2001  Gwydion Dylan Maintainers
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
  use compile-time-values;
  use definitions;
  use flow;
  use front;
  use ctype;
  use source;
  use builder-interface;
  use policy;
  use primitives;
  use compile-time-functions;
  use errors;
  
  export 
    just-add-type-checks, 
    convert-component-to-ssa,
    expand-component-clusters,
    maybe-convert-to-ssa,
    traverse-component,
    add-type-checks-aux,
    delete-dependent,
    remove-dependency-from-source,
    dropped-dependent,
    delete-queueable,
    queue-dependents,
    function-movable?,
    expression-movable?,
    fixed-number-of-values?,
    insert-after,
    split-after,
    combine-regions,
    replace-subregion,
    defaulted-type,
    maybe-restrict-type,
    expand-cluster;
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
