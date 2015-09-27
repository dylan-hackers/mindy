module:     dylan-user
author:     Russell M. Schaaf (rsbe@cs.cmu.edu) and
            Nick Kramer (nkramer@cs.cmu.edu)
synopsis:   Contains the library and module definitions for the X-inspector
            library.
copyright: see below

//======================================================================
//
// Copyright (c) 1995, 1996 Carnegie Mellon University
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

define library X-inspector
  use dylan;
  use string-extensions;
  use regular-expressions;
  use tk;
  use inspector-base;
  export
    x-inspector;
end library X-inspector;

define module internals
  create
    <inspector-state>, state-lock, state-done?, state-done?-setter,
    state-done, state-all-windows,
    xinspect-one-object;
end module internals;

define module class-diagram
  use dylan;
  use extensions;
  use threads;
  use introspection;
  use tk;
  use tk-extension;
  use internals;
  export 
    view-class-hierarchy;
end module class-diagram;

define module X-inspector
  use dylan;
  use extensions;
  use threads;
  use regular-expressions, import: { split };
  use substring-search, import: { substring-position };
  use tk;
  use tk-extension;
  use inspector-base, export: { *show-elements*, $all-libraries };
  use class-diagram;
  use internals;

  export xinspect;
end module X-inspector;
