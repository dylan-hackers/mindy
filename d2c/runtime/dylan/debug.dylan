rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/debug.dylan,v 1.3 1996/03/17 00:11:23 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

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
