module: null-optimizer
rcs-header: $Header: /scm/cvs/src/d2c/compiler/fer-transform/null-optimizer.dylan,v 1.4.4.1 2004/10/12 02:33:24 gabor Exp $
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
    debug-message(optimizer, "Null-optimizing new component");
    debug-dump(optimizer, component);

    debug-message(optimizer, "Doing trivial SSA conversion");
    convert-component-to-ssa(component);
    debug-dump(optimizer, component);

    debug-message(optimizer, "Expanding clusters");
    expand-component-clusters(component);
    debug-dump(optimizer, component);

    debug-message(optimizer, "Adding type checks");
    just-add-type-checks(component);
    debug-dump(optimizer, component);

    // XXX - We'll generate incorrect code until this function is fully
    // implemented. 
    // - Replace Placeholders
    // - Environmental Analysis

    debug-message(optimizer, "Building external entry points for local funs");
    build-local-xeps(component);
    debug-dump(optimizer, component);    
  end unless;
end method optimize-component;

// If we're debugging the optimizer, dump out the current representation of
// the code.
//
define function debug-dump
    (optimizer :: <abstract-optimizer>,
     component :: <component>)
 => ()
  if (optimizer.debug-optimizer > 1)
    dump-fer(component);
  end;
end;

// If we're debugging the optimizer, dump out the current representation of
// the code.
//
define function debug-message
    (optimizer :: <abstract-optimizer>,
     message :: <string>)
 => ()
  if (optimizer.debug-optimizer > 0)
    dformat("\n******** %s\n\n", message);
  end;
end;

