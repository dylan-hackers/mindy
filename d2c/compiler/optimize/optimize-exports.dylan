module: dylan-user
rcs-header: $Header: /scm/cvs/src/d2c/compiler/optimize/optimize-exports.dylan,v 1.8 2003/06/24 21:00:08 andreas Exp $
copyright: see below


//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000, 2001  Gwydion Dylan Maintainers
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

define library compiler-optimize
  use Dylan;
  use compiler-base;
  use compiler-front;
  use compiler-fer-transform;

  export
    cheese;
end library compiler-optimize;


define module cheese
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
  use fer-transform,
    rename:
    {
      replace-subregion => fer-replace-subregion,
      combine-regions => fer-combine-regions,
      delete-dependent => fer-delete-dependent,
      insert-after => fer-insert-after,
      remove-dependency-from-source => fer-remove-dependency-from-source,
      expand-cluster => fer-expand-cluster,
      queue-dependents => fer-queue-dependents,
      maybe-restrict-type => fer-maybe-restrict-type,
      add-type-checks-aux => fer-add-type-checks
    };
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
  use abstract-optimizer;
  use xep-tools;

  export
    <cmu-optimizer>,
    *optimize-ncalls*,
    enable-sanity-checks,
    disable-sanity-checks;
end;
