rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/debug.dylan,v 1.1 1995/11/16 03:42:45 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

// We don't actually include a debugger in the Dylan library.  Instead,
// we define a suite of variables that encapsulate the interface to the
// debugger.  This way, applications can link in their own debugger that
// uses their own streams library, etc.
//
// For bootstrapping purposes, there are defaults for these variables that
// supply the null debugger.  It just prints a message and flames out.

// *format-function* -- exported from ???
//
// Called by the condition system when it wants to do format like stuff.
// The stream argument is either *debug-output* or whatever was passed to
// report-condition.
//
define variable *format-function* :: <function> =
  method (stream, string :: <string>, #rest arguments) => ();
    apply(format, string, arguments);
  end;

// *debug-output* -- exported from ???
//
// Passed in as the first argument to *format-function* when we have nothing
// better to pass in.
//
define variable *debug-output* = #f;

// *debugger* -- exported from ???
//
// Invoke the debugger to resolve the supplied condition.  Any values it
// returns are passed back to whoever invoked the debugger.
//
define variable *debugger* :: <function>
  = method (condition :: <condition>) => res :: <never-returns>;
      *format-function*(*debug-output*, "%s\n", condition);
      %%primitive call-out ("abort", void:);
    end;
