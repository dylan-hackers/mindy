module: null-optimizer
rcs-header: $Header: /scm/cvs/src/d2c/compiler/fer-transform/null-optimizer.dylan,v 1.1 2000/06/11 21:20:36 emk Exp $
copyright: see below


//======================================================================
//
// Copyright (c) 2000  Gwydion Dylan Maintainers
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

// The minimum possible optimizer.
//
define class <null-optimizer> (<abstract-optimizer>)
end class <null-optimizer>;

// Perform only those "optimizations" required by the back end.
//
define method optimize-component
    (optimizer :: <null-optimizer>,
     component :: <component>,
     #key simplify-only? :: <boolean>)
 => ()
  unless (simplify-only?)
    maybe-dump-fer(optimizer, component);
    // XXX - We'll generate incorrect code until this function is fully
    // implemented. 

    // - SSA Convert
    // - Add Type Checks
    // - Replace Placeholders
    // - Environmental Analysis

    dformat("\n******** Building external entry points for local funs");
    build-local-xeps(component);
    maybe-dump-fer(optimizer, component);    
  end unless;
end method optimize-component;
