module: tk-internal
author: Robert Stockton (rgs@cs.cmu.edu)

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
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
//
// This file provides utilities for retrieving values from the WISH
// interpreter, calling Dylan functions from tk, and imitating tk's "active"
// variables.
//
//======================================================================

// Concatenates the input into a tk command, executes it, and returns the
// result as a string.  Pauses execution of the current thread until the tk
// function returns.
//
// You may need to call "tk-as", "tk-unquote", or "parse-tk-list" on the
// result depending upon the expected result type.
//
define generic call-tk-function (#rest pieces) => (result :: <object>);

// Creates a string which, when executed by the tk interpreter which will
// force execution of the given "function" (which may either be a Dylan
// function or a tk command string).
//
define generic function-string (function :: <object>) => (command :: <string>);

// Interprets a "callback" line printed by the tk interpreter and executes
// the appropriate function.  Callback lines are distinguished by the fact
// that they start with an exclamation point.
//
define generic do-callback (line :: <string>) => ();

// <active-variable>s implement an analogue to "active variables" as
// implemented in tk.  These may be used as the values of "buttons", or as
// "textvariable" values.  They basically only support the operations "value",
// "value-setter", and, of course, "make".
//
// If you define a "class:" option to "make", then "value" will return values
// of that type.  "value-setter" will still accept almost any value and
// attempt to convert it to the appropriate type.
//
define class <active-variable> (<object>)
  virtual slot value;
  slot index :: <string>;
  slot internal-value :: <object>,  // Will be stored as a member of "cls"
    required-init-keyword: #"value";
  slot cls :: <class>, init-keyword: #"class", init-value: <string>;
  slot command :: false-or(<function>),
    init-keyword: #"command", init-value: #f;
end class <active-variable>;


//==========================================================================
//			       Active variables
//==========================================================================

// Support variables for creating and storing active values.  We maintain an
// array of <active-variable>s on this side and an array (with corresponding
// indices) of actual values on the tk/WISH side.
//
define variable tk-var-count = 0;
define variable tk-var-array = make(<stretchy-vector>);

// Basic initialization.  Since it must be called after the WISH interpreter
// is started, we define a function to be called by tk-init.
//
define method init-active-variables ()
  put-tk-line("proc dylan-trace {name element op} ",
	       "{upvar #0 dylan-vars($element) x;",
	       "dylan-put \"!V${element}!\" $x}");
  put-tk-line("trace variable dylan-vars w dylan-trace");
end method init-active-variables;

define method initialize (object :: <active-variable>, #key value, #all-keys)
  let idx = tk-var-count := tk-var-count + 1;
  tk-var-array[idx] := object;
  
  object.index := tk-as(<string>, idx);
  object.value := value;
end method initialize;

define method value (var :: <active-variable>) => (result :: <object>);
  var.internal-value;
end method value;

define method value-setter (value :: <object>, var :: <active-variable>)
  block (return)
    let handler <simple-error>
      = method (error, next-handler)
	  // Make this into a less serious error.
	  apply(signal, concatenate("Error in setting active variable:\n  ",
				    condition-format-string(error)),
		condition-format-arguments(error));
	  return();
	end method;
    let new-value = tk-as(var.cls, value);
    if (var.command & new-value ~= var.internal-value)
      var.command(new-value, var.internal-value)
    end if;
    var.internal-value := new-value;
    let string-value = tk-quote(value);
    put-tk-line("set dylan-vars(", var.index, ") {", string-value, "}");
  end block;
end method value-setter;

define method tk-as
    (cls == <string>, value :: <active-variable>) => (result :: <string>);
  concatenate("dylan-vars(", value.index, ")");
end method tk-as;

//==========================================================================
//			      Event handler loop
//==========================================================================
// Because tk doesn't know about Dylan functions, we generate strings to
// identify the functions and store them in a dictionary.  Value callbacks are
// "one-shots", so they are deleted as soon as the function is executed.
//
// These functions are heavily multi-threaded, so there are a variety of
// threads, locks, events and such:
//   call-back-{table, lock, index}
//     This table is used to store methods corresponding to "call back keys"
//     passed in from Tcl/TK.  Several threads may try to update or read this
//     table simultaneously, so we must protect *all* acceses via
//     "call-back-lock"
//   exec-thread-{proc, event, lock, loop, running}
//     Because callbacks can pause for value return, we can't just invoke them
//     directly from the listener thread.  On the other hand, we still want
//     sequential execution semantics.  Therefore we maintain an "execution
//     thread" which runs continuously.  Each time a new callback event
//     arrives we simply add it to a queue of functions waiting to be
//     executed.  We use exec-thread-lock to protect access to the queue and
//     exec-thread-event to notify the thread that its queue is non-empty.
//==========================================================================

// Because several threads may be processing or creating call-backs at the
// same time, all accesses to "call-back-table" must be protected via
// "call-back-lock".
//

// Performance trade-off.  If we are only doing "function-return" callbacks,
// self-organizing lists are probably more efficient.  Unfortunately, this
// can cause massive slow-downs if we have many callbacks.  At some point, we
// should probably use separate tables, but for now, we'll just use tables.
define constant call-back-table = make(<object-table>);

define constant call-back-lock = make(<lock>);
define variable call-back-index = 1;

define variable exec-thread-running :: <boolean> = #f;
define variable exec-thread-procs :: <deque> = make(<deque>);
define variable exec-thread-event :: <event> = make(<event>);
define variable exec-thread-lock :: <lock> = make(<lock>);

//==========================================================================

define method exec-thread-loop ();
  grab-lock(exec-thread-lock);
  while (empty?(exec-thread-procs))
    wait-for-event(exec-thread-event, exec-thread-lock);
    grab-lock(exec-thread-lock);
  end while;
  let proc = pop(exec-thread-procs);
  release-lock(exec-thread-lock);

  proc();
  exec-thread-loop();
end method exec-thread-loop;

// This function interprets three different types of "callbacks".  We have
// "!V" for setting active variables, "!R" for returning results from
// call-tk-function, and "!N" for "normal" callbacks from Tcl/Tk widgets.
//
// Value returns and variable setting are simple processes that happen
// asynchronously.  Normal callbacks, on the other hand, may have to pause to
// wait for value returns and therefore must run in a separate thread.
//
define method do-callback (line :: <string>) => ();
//write-line(*standard, line-output*);
//force-output(*standard-output*);
  // R for return value, V for variable setter, or N for normal call
  let callback-type = line[1];
  let index = 2;

  let sep = find-first-key(line, curry(\=, '!'), start: index);
  let frst = min(sep + 2, line.size); // skip over "! "

  if (callback-type == 'V')
    // Special callback for "set variable"
    block (return)
      let handler <simple-error>
	= method (error, next-handler)
	    // Make this into a less serious error.
	    apply(signal, concatenate("Error in setting active variable:  ",
				      condition-format-string(error)),
		  condition-format-arguments(error));
	    return();
	  end method;
      let index = tk-as(<integer>,
			copy-sequence(line, start: index, end: sep));
      let var = tk-var-array[index];
      let new-value
	= tk-as(var.cls, parse-tk-list(line, start: frst,
				       depth: 1, unquote: #t).first);
      if (var.command & new-value ~= var.internal-value)
	// Give the execution thread a new function to execute.  It may be
	// waiting for us to give it to us, or it may be too busy executing a
	// previous function, in which case it will be deferred.
	if (~exec-thread-running)
	  spawn-thread("exec", exec-thread-loop);
	  exec-thread-running := #t;
	end if;

	grab-lock(exec-thread-lock);
	push-last(exec-thread-procs,
		  curry(var.command, new-value, var.internal-value));
	signal-event(exec-thread-event);
	release-lock(exec-thread-lock);
      end if;
      var.internal-value := new-value;
    end block;
  else
    let key = as(<symbol>, copy-sequence(line, start: index, end: sep));
    grab-lock(call-back-lock);
    let callback = call-back-table[key];
    release-lock(call-back-lock);

    if (callback-type == 'R')
      // Simple value return
      apply(callback, parse-tk-list(line, start: frst, depth: 1));
    elseif (callback-type == 'N')
      // We don't create execution threads till we need them.  Often we find
      // that we don't.
      if (~exec-thread-running)
	spawn-thread("exec", exec-thread-loop);
	exec-thread-running := #t;
      end if;

      // Give the execution thread a new function to execute.  It may be
      // waiting for us to give it to us, or it may be too busy executing a
      // previous function, in which case it will be deferred.
      grab-lock(exec-thread-lock);
      push-last(exec-thread-procs,
		curry(apply, callback,
		      parse-tk-list(line, start: frst, depth: 1)));
      signal-event(exec-thread-event);
      release-lock(exec-thread-lock);
    else
      error("Unrecognized callback.");
    end if;
  end if;
end method do-callback;


//==========================================================================
//			      tk function call
//==========================================================================


// Removes a value return callback from the table.  This mostly requires us to
// wrap a lock around the table accesses to preserve concurrent safety.
//
define method delete-callback (key :: <symbol>) => ();
  grab-lock(call-back-lock);
  remove-key!(call-back-table, key);
  release-lock(call-back-lock);
end method delete-callback;

// Creates a new value return callback and enters it in the table.
//
define method value-return-string
    (function :: <function>, tk-command :: <string>)
 => (command :: <string>, key :: false-or(<symbol>));
  let key = tk-as(<string>, call-back-index := call-back-index + 1);
  let key-symbol = as(<symbol>, key);

  grab-lock(call-back-lock);
  call-back-table[as(<symbol>, key)] := function;
  release-lock(call-back-lock);

  values(concatenate("dylan-put {!R",  key, "!} [", tk-command, "]"),
	 key-symbol);
end method value-return-string;

// See description of the generic above.
//
define method call-tk-function (#rest strings) => (result :: <sequence>);
  // We get a value return by sending a magic string to Tcl/TK, and then
  // pausing the current thread till the event loop to gets a "value return"
  // back from Tcl/Tk.
  let return-event :: <event> = make(<event>);
  let return-lock :: <lock> = make(<lock>);
  grab-lock(return-lock);
  let return-value = #f;

  let (command-line, callback-key)
    = value-return-string(method (result :: <string>)
			    grab-lock(return-lock);
			    return-value := result;
			    signal-event(return-event);
			    release-lock(return-lock);
			  end method,
			  apply(concatenate, map(curry(tk-as, <string>),
						 strings)));
  put-tk-line(command-line);

  while (~return-value)
    wait-for-event(return-event, return-lock);
    grab-lock(return-lock);
  end while;
  release-lock(return-lock);
  delete-callback(callback-key);

  return-value;
end method call-tk-function;


//==========================================================================
//			      Callback creation
//==========================================================================


// See description of the generic above.
//
define method function-string
    (function :: <function>) => (command :: <string>);
  let key = tk-as(<string>, call-back-index := call-back-index + 1);
  let key-symbol = as(<symbol>, key);

  grab-lock(call-back-lock);
  call-back-table[as(<symbol>, key)] := function;
  release-lock(call-back-lock);

  values(concatenate("dylan-put {!N",  key, "!} "), key-symbol);
end method function-string;

define method tk-as
    (cls == <string>, value :: <function>) => (result :: <string>);
  function-string(value);
end method tk-as;
  
