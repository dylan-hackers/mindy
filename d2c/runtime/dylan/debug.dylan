rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/debug.dylan,v 1.1 1998/05/03 19:55:39 andreas Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

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

// <debugger> -- exported from ???
//
// Abstract superclass of all the different kinds of debuggers.
//
define primary abstract open class <debugger> (<object>)
end class <debugger>;

// invoke-debugger -- exported from ???
//
// Called by the condition system on *debugger* and the condition when it
// wants to invoke the debugger.  And values returned are passed back on out
// to the original caller of signal when appropriate.
// 
define open generic invoke-debugger
    (debugger :: <debugger>, condition :: <condition>)
    => (#rest values);

// *debugger* -- exported from ???
//
// Value passed in to invoke-debugger when the condition system needs to
// invoke the debugger.
// 
define variable *debugger* :: <debugger> = make(<null-debugger>);


// The null debugger.

// <null-debugger> -- internal.
//
define class <null-debugger> (<debugger>)
end class <null-debugger>;

// invoke-debugger(<null-debugger>) -- exported gf method
//
// The null debugger doesn't do much: it just prints the condition and then
// aborts.
// 
define sealed method invoke-debugger
    (debugger :: <null-debugger>, condition :: <condition>)
    => res :: <never-returns>;
  format("%s\n", condition);
  c-expr(void: "fflush(stdout)");
  call-out("abort", void:);
end;
