rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/condition.dylan,v 1.13 2003/02/17 17:36:54 andreas Exp $
copyright: see below
module: dylan-viscera


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

// Classes

// <condition> -- exported from Dylan
//
define open abstract class <condition> (<object>)
end class <condition>;

// <serious-condition> -- exported from Dylan
// 
define open abstract class <serious-condition> (<condition>)
end class <serious-condition>;

// <error> -- exported from Dylan
// 
define open abstract class <error> (<serious-condition>)
end class <error>;

// <format-string-condition> -- exported from Extensions
//
// This class mixes in the format string and arguments used by the various
// simple-mumble conditions.
// 
define abstract open class <format-string-condition> (<condition>)

  // The format string.  Exported from Dylan.
  constant slot condition-format-string :: <string>,
    required-init-keyword: format-string:;

  // The format arguments.  Exported from Dylan.
  constant slot condition-format-arguments :: <sequence>,
    init-keyword: format-arguments:,
    init-value: #();
end class <format-string-condition>;

// interposed this class to conform to the common-extensions
// in the "core" document from Functional Objects, Inc.
define open abstract class <simple-condition> (<format-string-condition>)
end class <simple-condition>;

// <simple-error> -- exported from Dylan
// 
define class <simple-error> (<error>, <simple-condition>)
end class <simple-error>;

define sealed domain make(singleton(<simple-error>));
define sealed domain initialize(<simple-error>);

define constant <source-location> = <string>; // until we know better
define constant $unknown-source-location :: <source-location>
  = "unknown source location.";

// <type-error> -- exported from Dylan
//
define class <type-error> (<error>)
  //
  // The object that is of the wrong type.
  slot type-error-value :: <object>, required-init-keyword: value:;
  //
  // The type the object is supposed to be.
  slot type-error-expected-type :: <type>, required-init-keyword: type:;
  //
  // The origin of the type check, if known.
  slot type-error-location :: <source-location> = $unknown-source-location, 
    init-keyword: source-location:;
end class <type-error>;

define sealed domain make(singleton(<type-error>));
define sealed domain initialize(<type-error>);

// <sealed-object-error> -- exported from Dylan
//
define class <sealed-object-error> (<error>)
end class <sealed-object-error>;

define sealed domain make(singleton(<sealed-object-error>));
define sealed domain initialize(<sealed-object-error>);

define method make(class == <sealed-object-error>, #key, #all-keys)
 => (obj :: subclass(<sealed-object-error>));
  error("Can't make instances of <sealed-object-error> because the DRM says so.");
end method make;

// <warning> -- exported from Dylan
//
define abstract open class <warning> (<condition>)
end class <warning>;

// <simple-warning> -- exported from Dylan
// 
define class <simple-warning> (<warning>, <simple-condition>)
end class <simple-warning>;

define sealed domain make(singleton(<simple-warning>));
define sealed domain initialize(<simple-warning>);

// <restart> -- exported from Dylan
// 
define open abstract class <restart> (<condition>)
end class <restart>;

// <simple-restart> -- exported from Dylan
//
define class <simple-restart> (<restart>, <simple-condition>)
end class <simple-restart>;

define sealed domain make(singleton(<simple-restart>));
define sealed domain initialize(<simple-restart>);

// <abort> -- exported from Dylan
//
define class <abort> (<restart>)
  slot abort-description :: <byte-string>,
    init-keyword: description:,
    init-value: "<abort>";
end class <abort>;

define sealed domain make(singleton(<abort>));
define sealed domain initialize(<abort>);


// IO abstraction.

// condition-format -- exported from Extensions
//
// Serves as a firewall between the condition system and streams.
// Report-condition methods should call this routine to do their formatting
// and streams libraries should define methods on it to pick off their
// kinds of streams and call their particular format utility.
// 
define open generic condition-format
    (stream :: <object>, control-string :: <string>, #rest arguments) => ();

// condition-format(#"cheap-IO") -- internal.
//
// Bootstrap method for condition-format that just calls the cheap-IO format.
// 23 Nov 98 - emk - added support for #"Cheap-Err", too.
//
define sealed method condition-format
    (stream :: <symbol>, control-string :: <string>, #rest arguments) => ();
  apply(cheap-format, stream, control-string, arguments);
end;

// condition-force-output
//
// Just like condition-format, except performs a general force-output function.
// 
define open generic condition-force-output (stream :: <object>) => ();

// condition-force-output(#"cheap-IO") -- internal.
//
// Bootstrap method for condition-format that just calls the cheap-IO fflush.
//
define sealed method condition-force-output (stream :: <symbol>) => ();
  cheap-force-output(stream);
end;


// *warning-output* -- exported from Extensions
//
// The ``stream'' to which warnings report when signaled (and not handled).
//
define variable *warning-output* :: <object> = #"Cheap-Err";


// Condition reporting.

// report-condition -- exported from Extensions.
//
// Generate a human readable report of the condition on stream.  We restrict
// the stream to <object> because we have no idea what the underlying output
// system is going to use for streams.
//
define open generic report-condition
    (condition :: <condition>, stream :: <object>) => ();


// report-condition(<condition>) -- exported gf method.
//
// Default method for all conditions.  Just print the condition object
// to the stream.
// 
define method report-condition (condition :: <condition>, stream) => ();
  condition-format(stream, "%=", condition);
end method report-condition;

// report-condition(<format-string-condition>) -- exported gf method.
//
// All simple conditions report the same: we just pass the format string
// and arguments to format.
// 
define sealed method report-condition
    (condition :: <format-string-condition>, stream)
    => ();
  apply(condition-format, stream,
	condition.condition-format-string,
	condition.condition-format-arguments);
end method report-condition;

// report-condition(<type-error>) -- exported gf method.
//
// Type errors just complain about the object not being of the correct type.
//
define sealed method report-condition (condition :: <type-error>, stream)
    => ();
  condition-format(stream, "Expected an instance of %=, but got %=, at\n%s",
		   condition.type-error-expected-type,
		   condition.type-error-value,
                   condition.type-error-location);
end method report-condition;

// report-condition(<sealed-object-error>) -- exported gf method.
//
// Until we actually use a sealed object error or two, I haven't much idea
// about how they should report.  So I'll just duplicate the <condition>
// method as a placeholder.
// 
define sealed method report-condition
    (condition :: <sealed-object-error>, stream)
    => ();
  condition-format(stream, "%=", condition);
end method report-condition;

// report-condition(<abort>) -- exported gf method.
//
// Just print the supplied description.
// 
define sealed method report-condition (condition :: <abort>, stream) => ();
  condition-format(stream, "%s", condition.abort-description);
end method report-condition;


// Condition signaling

// signal -- exported from Dylan.
//
// Signal the condition.
//
define generic signal (condition, #rest noise);

// signal(<condition>) -- exported gf method.
// 
// Search the handlers for one that wants to handle this condition and then
// give it a shot.
// 
define method signal (cond :: <condition>, #rest noise)
  unless (empty?(noise))
    error("Can only supply format arguments when supplying a format string.");
  end;
  local
    method search (h :: false-or(<handler>))
      if (h)
	let remaining = h.handler-prev;
	if (instance?(cond, h.handler-type))
	  let test = h.handler-test;
	  if (~test | test(cond))
	    h.handler-function(cond, method () search(remaining) end);
	  else
	    search(remaining);
	  end if;
	else
	  search(remaining);
	end if;
      else
	default-handler(cond);
      end if;
    end method search;
  search(this-thread().cur-handler);
end method signal;

// signal(<string>) -- exported gf method.
//
// Make a <simple-warning> out of the string and arugments and re-signal
// it.
// 
define method signal (string :: <string>, #rest arguments)
  signal(make(<simple-warning>,
	      format-string: string,
	      format-arguments: arguments));
end method signal;

// error -- exported from Dylan
//
// Just like signal but never returns.
// 
define generic error (condition, #rest noise) => res :: <never-returns>;

// error(<condition>) -- exported gf method.
//
// Signal the condition.  If we get control back, immediately invoke the
// debugger.
// 
define method error (cond :: <condition>, #rest noise)
    => res :: <never-returns>;
  unless (empty?(noise))
    error("Can only supply format arguments when supplying a format string.");
  end;
  signal(cond);
  invoke-debugger(*debugger*,
		  make(<simple-error>,
		       format-string:
			 "Attempt to return from a call to error"));
end method error;

// error(<string>) -- exported gf method.
//
// Signals a simple error built from the string and arguments.
//
define method error (string :: <string>, #rest arguments)
    => res :: <never-returns>;
  error(make(<simple-error>,
	     format-string: string,
	     format-arguments: arguments));
end method error;

// cerror -- exported from Dylan
//
// Signals and error after first establishing a handler for <simple-error>s.
//
define method cerror
    (restart-description :: <string>,
     condition-or-string :: type-union(<string>, <condition>),
     #rest arguments)
    => res :: <false>;
  block ()
    apply(error, condition-or-string, arguments);
  exception (<simple-restart>,
	     init-arguments: list(format-string: restart-description,
				  format-arguments: arguments))
    #f;
  end block;
end method cerror;

// check-type -- exported from Dylan
//
// Verify that object is an instance of type.  If so, return it.  If not,
// signal a type error.
//
// check-type is split into two pieces, check-type and %check-type in order
// to help the compiler do its job.  When the compiler notices a call to
// check-type, it replaces it with an internal type assertion.  After doing
// all the type analysis it can, if it cannot prove that the type assertion
// is held, then it inserts a call to %check-type.  If it inserted a call to
// check-type, then it would forever repeat itself, and the compiler is slow
// enough already.
//
define method check-type
    (object :: <object>, type :: <type>) => object :: <object>;
  %check-type(object, type, $unknown-source-location);
end;
//
define inline method %check-type
    (object :: <object>, type :: <type>, source :: <source-location>) 
 => object :: <object>;
  if (instance?(object, type))
    object;
  else
    type-error-with-location(object, type, source);
  end;
end;

// check-types -- internal.
//
// Verify that all the objects in the vector are instances of type and
// signal a type-error if not.  Calls to this are inserted by the compiler
// to type-check #rest result types.
// 
define method check-types
    (objects :: <simple-object-vector>, type :: <type>)
    => checked :: <simple-object-vector>;
  for (object in objects)
    check-type(object, type);
  end for;
  objects;
end method check-types;

// type-error -- internal.
//
// Signals a <type-error> complaining that value is not of the correct type.
//
define function type-error (value :: <object>, type :: <type>)
 => res :: <never-returns>;
  error(make(<type-error>, value: value, type: type));
end function;

define function type-error-with-location (value :: <object>, type :: <type>, 
                                          source :: <source-location>)
 => res :: <never-returns>;
  error(make(<type-error>, value: value, type: type, source-location: source));
end function;

// abort -- exported from Dylan
//
// Aborts and never returns.
// 
define function abort () => res :: <never-returns>;
  error(make(<abort>));
end;


// Condition handling.

// default-handler -- exported from Dylan.
//
// Called if no dynamic handler handles a condition.
// 
define open generic default-handler (condition :: <condition>);

// default-handler(<condition>) -- exported gf method.
//
// Just does nothing.
// 
define method default-handler (condition :: <condition>)
  #f;
end method default-handler;

// default-handler(<serious-condition>) -- exported gf method.
//
// Invoke the debugger.
//
define method default-handler (condition :: <serious-condition>)
  invoke-debugger(*debugger*, condition);
end method default-handler;

// default-handler(<warning>) -- exported gf method.
//
// Report the warning and then just return nothing.
// 
define method default-handler (condition :: <warning>)
  condition-format(*warning-output*, "%s\n", condition);
  #f;
end method default-handler;

// default-handler(<restart>) -- exported gf method.
//
// Complain that the restart wasn't handled.
// 
define method default-handler (restart :: <restart>)
  error("No restart handler for %=", restart);
end method default-handler;


// restart-query -- exported from Dylan.
// 
// This function is called by debuggers and the like after making a
// restart and before signaling it.  Methods can then query the
// interactive user in order to fill in the restart.  The return
// values are not at all meaningful, but Apple wasn't smart enough to
// declare this as not returning anything.
//
define open generic restart-query (restart :: <restart>);

// restart-query(<restart>) -- exported gf method.
//
// Default method for restart-query that just does nothing.
// 
define method restart-query (restart :: <restart>) => ();
end method restart-query;


// Returning.

// return-allowed? -- exported from Dylan.
//
// Called by debuggers and the like to determine if returning is allowed
// for a particular condition.
// 
define open generic return-allowed? (cond :: <condition>) => res :: <boolean>;

// return-allowed?(<condition>) -- exported gf method.
//
// By default, returning is not allowed.
//
define method return-allowed? (cond :: <condition>) => res :: <false>;
  #f;
end method return-allowed?;


// return-description -- exported from Dylan.
//
// Called by debuggers and the like once return-allowed? has okayed returning
// in order to get a description of what returning will do.
//
define open generic return-description (cond :: <condition>)
    => res :: type-union(<false>, <string>, <restart>);


// return-query -- exported from Dylan.
//
// Called by handlers (after return-allowed? has okayed returning) in order
// to propt the user for the values to return.
// 
define open generic return-query (condition :: <condition>);



// Breakpoints.

// <breakpoint> -- internal.
//
// Special kind of <simple-warning> signaled by break.  We don't just use
// <simple-warning> because we want to be able to return from breakpoints.
// 
define class <breakpoint> (<simple-warning>)
end class <breakpoint>;


// return-allowed?(<breakpoint>) -- gf method.
//
// Returning is allowed for breakpoings.
// 
define method return-allowed? (cond :: <breakpoint>) => res :: <true>;
  #t;
end method return-allowed?;


// return-query(<breakpoint>) -- gf method
//
// When invoked interactively, just return #f.
//
define method return-query (cond :: <breakpoint>)
  #f;
end method return-query;


// return-description(<breakpoint>) -- gf method
//
// Describes what returning from a breakpoint will do.
//
define method return-description (cond :: <breakpoint>) => res :: <string>;
  "Return #f";
end method return-description;


// break -- exported from Dylan
//
// Cause a breakpoint.  Split into two functions so the first argument can be
// optional and dispatched off of if supplied.
// 
define method break (#rest arguments) => res :: <false>;
  if (empty?(arguments))
    %break("Break.");
  else
    apply(%break, arguments);
  end if;
end method break;
//
define method %break (string :: <string>, #rest arguments) => res :: <false>;
  %break(make(<breakpoint>,
	      format-string: string,
	      format-arguments: arguments));
end method %break;
//
define method %break (cond :: <condition>, #rest noise) => res :: <false>;
  unless (empty?(noise))
    error("Can only supply format arguments when supplying a format string.");
  end unless;
  block ()
    invoke-debugger(*debugger*, cond);
  exception (<simple-restart>,
	     init-arguments: list(format-string: "Continue from break"))
  end block;
  #f;
end method %break;



// Lose.

// lose -- internal.
//
// Called when real problems are noticed with the runtime system.  Should never
// actually happen.  But that would be assuming that we don't have any bugs
// in our code.
// 
define method lose
    (str :: <byte-string>, #rest args) => res :: <never-returns>;
  fputs("internal error: ", #"Cheap-Err");
  apply(cheap-format, #"Cheap-Err", str, args);
  cheap-format(#"Cheap-Err", "\n");
  call-out("abort", void:);
end;


// Internal errors.

// The compiler inserts calls to these routines because a call to a routine
// with a fixed number of arguments is much more concise than a call to
// error, which takes a variable number of arguments.


// uninitialized-slot-error -- magic
//
// This called whenever we try to access an uninitialized slot, both directly
// by the acccessor standins in func.dylan and by compiler generated code.
// It just signals an <uninitialized-slot-error>.
// 
define function uninitialized-slot-error
    (slot :: <slot-descriptor>, instance :: <object>)
    => res :: <never-returns>;
  error(make(<uninitialized-slot-error>, 
             slot: slot, 
             instance: instance));
end;

define function uninitialized-slot-error-with-location
    (slot :: <slot-descriptor>, instance :: <object>, 
     source-location :: <source-location>)
    => res :: <never-returns>;
  error(make(<uninitialized-slot-error>, 
             slot: slot, 
             instance: instance,
             source-location: source-location));
end;

// <uninitialized-slot-error> -- internal.
//
// The error condition that is signaled whenever we try to access an
// uninitialized slot.
//
define class <uninitialized-slot-error> (<error>)
  //
  // The slot that was uninitialized.
  constant slot uninitialized-slot-error-slot :: <slot-descriptor>,
    required-init-keyword: slot:;
  //
  // The instance whos slot was uninitialized.
  constant slot uninitialized-slot-error-instance :: <object>,
    required-init-keyword: instance:;

  constant slot uninitialized-slot-error-source-location :: <source-location>
    = $unknown-source-location, init-keyword: source-location:;
end class <uninitialized-slot-error>;

define sealed domain make (singleton(<uninitialized-slot-error>));
define sealed domain initialize (<uninitialized-slot-error>);

// report-condition -- method on exported GF.
//
// If the slot has a name, include it in the report.  If not, just print
// the instance.
// 
define sealed method report-condition
    (condition :: <uninitialized-slot-error>, stream)
    => ();
  let name = condition.uninitialized-slot-error-slot.slot-name;
  let instance = condition.uninitialized-slot-error-instance;
  let source-location = condition.uninitialized-slot-error-source-location;
  if (name)
    condition-format(stream, "Accessing uninitialized slot %s in %=, at\n%=",
		     name, instance, source-location);
  else
    condition-format(stream, "Accessing uninitialized slot in %=, at\n%=", 
                     instance, source-location);
  end if;
end method report-condition;



define function missing-required-init-keyword-error
    (keyword :: <symbol>, class :: <class>, 
     #key source-location :: <source-location> = $unknown-source-location) 
 => res :: <never-returns>;
  error("Missing required-init-keyword %= in make of %= at\n%s", 
        keyword, class, source-location);
end;

define function wrong-number-of-arguments-error
    (fixed? :: <boolean>, wanted :: <integer>, got :: <integer>,
     #key source-location :: <source-location> = $unknown-source-location) 
    => res :: <never-returns>;
  error("Wrong number of arguments.  Wanted %s %d but got %d, at\n%s",
	if (fixed?) "exactly" else "at least" end,
	wanted, got, source-location);
end;

define function odd-number-of-keyword/value-arguments-error 
    (#key source-location :: <source-location> = $unknown-source-location) 
 => res :: <never-returns>;
  error("Odd number of keyword/value arguments, at\n%s", source-location);
end;

define function unrecognized-keyword-error
    (key :: <symbol>, 
     #key source-location :: <source-location> = $unknown-source-location) 
 => res :: <never-returns>;
  error("Unrecognized keyword: %= at\n=s", key, source-location);
end;

define function no-applicable-methods-error 
    (function :: <generic-function>, 
     arguments :: <simple-object-vector>,
     #key source-location :: <source-location> = $unknown-source-location) 
 => res :: <never-returns>;
  error("No applicable methods in call of %= when given arguments:\n  %=, at\n%s",
	function, arguments, source-location);
end;

define function ambiguous-method-error 
    (methods :: <list>,
     #key source-location :: <source-location> = $unknown-source-location) 
 => res :: <never-returns>;
  error("It is ambiguous which of these methods is most specific:\n  %=, at\n%s",
	methods, source-location);
end;

define function select-error 
    (target,
     #key source-location :: <source-location> = $unknown-source-location) 
 => res :: <never-returns>;
  error("select error: %= does not match any of the keys, at\n%s", 
        target, source-location);
end;



// GDB debugging hooks.
// These are included in this file, because they will thus be more or
// less guaranteed to be linked in, and because we are bootstrapping
// off of the "condition-format" stuff.
//
// Note that these routines rely upon a magical knowledge of compiler
// internals.  There is no guarantee that they will continue to work
// in the future.

// XXX - *gdb-output* is evil and should go away when we get a new
// streams-protocol. It should be bound to whatever the current program
// is using for standard output.
//
define variable *gdb-output* = #"Cheap-IO";

define method gdb-print-object (obj :: <object>) => ();
  block ()
  condition-format(*gdb-output*, "%=\n", obj);
  condition-force-output(*gdb-output*);
  exception (error :: <error>)
    #f;
  end block;
end method gdb-print-object;

// WRETCHED HACK: By putting the function in an <object> variable, we
// guarantee that it will be dumped on the heap and that it will have a
// general representation.
//
define variable apply-safely-fun :: <object> = apply-safely;

// This debugging support routine does a normal apply, but also traps
// all errors (sending the error message to the standard error
// output).  The debugger can thus call this function without worrying
// about an unexpected error messing up the call stack.  
//
define method apply-safely (fun :: <function>, #rest arguments)
  block ()
    apply(fun, arguments);
  exception (error :: <error>)
    condition-format(*gdb-output*, "%s\n", error);
    condition-force-output(*gdb-output*);
  end block;
end method apply-safely;

define function seg-fault-error () => res :: <never-returns>;
  error("GDB encountered a seg fault -- invalid data.");
end;

// WRETCHED HACK: We put this in an <object> variable so that we will have a
// sample of an integer in the general representation.
//
define variable gdb-integer-value :: <object> = 1;
