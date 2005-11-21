module: abstract-optimizer
copyright: See below.


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

// <abstract-optimizer>
//
// This class represents an modular optimizer. Among other things, instances
// of this class are responsible for removing certain artifacts from the
// front-end representation before it gets passed to cback.
//
// The entry points to this class will probably change as we clean up the
// the optimizer.
//
define abstract open primary class <abstract-optimizer> (<object>)
  // Should we print debugging output after each optimization stage?
  // (If this is true, subclasses should call dump-fer frequently.)
  // the higher the value, the more to print
  slot debug-optimizer :: <integer> = 0,
    init-keyword: debug-optimizer:;
  slot optimizer-options :: <table> = make(<table>),
    init-keyword: options:
end class <abstract-optimizer>;


// optimize-component
// 
// Optimize a component. Right now, this routine must perform a minimum of
// five steps:
//   1) SSA Conversion
//   2) Addition of Type Checks
//   3) Environment Analysis
//   4) Placeholder Removeal
//   5) XEP Construction
// Once this routine has run, it should be possible to pass the component to
// the C back end.
//
// The simplify-only? keyword is used during inline expansions. If it is true,
// the above optimizations must not be performed.
//
define open generic optimize-component
    (optimizer :: <abstract-optimizer>,
     component :: <component>,
     #key simplify-only? :: <boolean>)
 => ();

// XXX - This global variable is evil. It used to be implicit, hidden away
// in the old generic functions 'optimize-component' and
// 'build-xep-component'. Now we can at least *see* where it lives.
define variable *current-optimizer* :: false-or(<abstract-optimizer>) = #f;
