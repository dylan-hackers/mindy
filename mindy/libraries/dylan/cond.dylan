module: Dylan
author: William Lott (wlott@cs.cmu.edu)
rcs-header: $Header: /scm/cvs/src/mindy/libraries/dylan/cond.dylan,v 1.1 1998/05/03 19:55:20 andreas Exp $

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
// This file implements the condition system.
//


// Classes

define class <condition> (<object>)
end class <condition>;


define class <serious-condition> (<condition>)
end class <serious-condition>;


define class <error> (<serious-condition>)
end class <error>;


define abstract open class <format-string-condition> (<condition>)
  slot condition-format-string,
    required-init-keyword: format-string:;
  slot condition-format-arguments,
    init-keyword: format-arguments:,
    init-value: #();
end class <format-string-condition>;


define class <simple-error> (<error>, <format-string-condition>)
end class <simple-error>;


define class <type-error> (<error>)
  slot type-error-value, init-keyword: value:;
  slot type-error-expected-type, init-keyword: type:;
end class <type-error>;


define class <warning> (<condition>)
end class <warning>;


define class <simple-warning> (<warning>, <format-string-condition>)
end class <simple-warning>;


define class <restart> (<condition>)
end class <restart>;


define class <simple-restart> (<restart>, <format-string-condition>)
end class <simple-restart>;


define class <abort> (<restart>)
  slot abort-description :: <byte-string>,
    init-keyword: description:,
    init-value: "<abort>";
end class <abort>;


// Condition reporting.

// condition-format
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
// Bootstrap method for condition-format that just calls the cheap-IO format
// to handle everything except %s on condition objects, which are handled
// directly.
//
define sealed method condition-format
    (stream == #"cheap-IO", control-string :: <string>, #rest arguments)
    => ();
  let length = control-string.size;
  local
    method scan (str-start, str-index, args-start, args-index)
      if (str-index < length)
	let char = control-string[str-index];
	if (char == '%')
	  let next-char = control-string[str-index + 1];
	  if (next-char == '%')
	    scan(str-start, str-index + 2, args-start, args-index);
	  elseif (next-char == 's' | next-char == 'S')
	    let next-arg = arguments[args-index];
	    if (instance?(next-arg, <condition>))
	      apply(format,
		    copy-sequence
		      (control-string, start: str-start, end: str-index),
		    copy-sequence
		      (arguments, start: args-start, end: args-index));
	      report-condition(next-arg, stream);
	      let next-str-index = str-index + 2;
	      let next-args-index = args-index + 1;
	      scan(next-str-index, next-str-index,
		   next-args-index, next-args-index);
	    else
	      scan(str-start, str-index + 2, args-start, args-index + 1);
	    end if;
	  else
	    scan(str-start, str-index + 2, args-start, args-index + 1);
	  end if;
	else
	  scan(str-start, str-index + 1, args-start, args-index);
	end if;
      else
	apply(format,
	      copy-sequence(control-string, start: str-start),
	      copy-sequence(arguments, start: args-start));
      end if;
    end method scan;
  scan(0, 0, 0, 0);
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
define sealed method condition-force-output (stream == #"cheap-IO") => ();
  fflush();
end;

// report-condition
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

define method report-condition (condition :: <format-string-condition>, stream)
    => ();
  apply(condition-format, stream,
	condition.condition-format-string,
	condition.condition-format-arguments);
end method report-condition;


define method report-condition (condition :: <type-error>, stream) => ();
  condition-format(stream,
		    "%= is not of type %=",
		    condition.type-error-value,
		    condition.type-error-expected-type);
end method report-condition;


define method report-condition (condition :: <abort>, stream) => ();
  condition-format(stream, "%s", condition.abort-description);
end method report-condition;


// Condition signaling

define method signal (string :: <string>, #rest arguments)
  signal(make(<simple-warning>,
	      format-string: string,
	      format-arguments: arguments));
end method signal;


define method signal (cond :: <condition>, #rest noise)
  unless (empty?(noise))
    error("Can only supply format arguments when supplying a format string.");
  end;
  local
    method search (h)
      if (h)
	if (instance?(cond, h.handler-type))
	  let test = h.handler-test;
	  if (~test | test(cond))
	    let remaining = h.handler-next;
	    h.handler-function(cond, method () search(remaining) end);
	  else
	    search(h.handler-next);
	  end if;
	else
	  search(h.handler-next);
	end if;
      else
	default-handler(cond);
      end if;
    end method search;
  search(current-handler());
end method signal;


define method error (string :: <string>, #rest arguments)
  error(make(<simple-error>,
	     format-string: string,
	     format-arguments: arguments));
end method error;


define method error (cond :: <condition>, #rest noise)
  unless (empty?(noise))
    error("Can only supply format arguments when supplying a format string.");
  end;
  signal(cond);
  invoke-debugger(make(<simple-error>,
		       format-string:
			 "Attempt to return from a call to error"));
end method error;


define method cerror (restart-descr, cond-or-string, #rest arguments)
  block ()
    apply(error, cond-or-string, arguments);
  exception (<simple-restart>,
	     init-arguments: list(format-string: restart-descr,
				  format-arguments: arguments))
    #f;
  end block;
end method cerror;


define method type-error (value, type)
  error(make(<type-error>, value: value, type: type));
end method type-error;


define method check-type (value, type)
  if (instance?(value, type))
    value;
  else
    type-error(value, type);
  end if;
end method check-type;

define method abort ()
  error(make(<abort>));
end method abort;

define generic default-handler (condition :: <condition>);

define method default-handler (condition :: <condition>)
 => return-val :: singleton(#f);
  #f;
end method default-handler;


define method default-handler (condition :: <serious-condition>)
  invoke-debugger(condition);
end method default-handler;


define variable *warning-output* = #"Cheap-IO";

define method default-handler (condition :: <warning>)
 => return-val :: singleton(#f);
  condition-format(*warning-output*, "%s\n", condition);
  #f;
end method default-handler;


define method default-handler (restart :: <restart>)
  error("No restart handler for %=", restart);
end method default-handler;



// Breakpoints.

define class <breakpoint> (<simple-warning>)
end class <breakpoint>;


define generic return-allowed? (cond :: <condition>)
 => answer :: <boolean>;

define method return-allowed? (cond :: <breakpoint>) => answer :: <boolean>;
  #t;
end method return-allowed?;


define method return-query (cond :: <breakpoint>)
  #f;
end method return-query;

define generic return-description (cond :: <condition>)
 => descr :: type-union(singleton(#f), <string>, <restart>);

define method return-description (cond :: <breakpoint>)
 => descr :: <string>;
  "Return #f";
end method return-description;


define method %break (string :: <string>, #rest arguments)
  %break(make(<breakpoint>,
	      format-string: string,
	      format-arguments: arguments));
end method %break;


define method %break (cond :: <condition>, #rest noise)
  unless (empty?(noise))
    error("Can only supply format arguments when supplying a format string.");
  end unless;
  block ()
    invoke-debugger(cond);
  exception (<simple-restart>,
	     init-arguments: list(format-string: "Continue from break"))
    #f;
  end block;
end method %break;


define method break (#rest arguments)
  if (empty?(arguments))
    %break("Break.");
  else
    apply(%break, arguments);
  end if;
end method break;



// Introspection.

define method do-handlers (function :: <function>)
  for (h = current-handler() then h.handler-next,
       while: h)
    function(h.handler-type,
	     h.handler-test | method (x) #t end,
	     h.handler-function,
	     h.handler-init-args);
  end for;
end method do-handlers;


define method return-allowed? (cond :: <condition>) => answer :: <boolean>;
  #f;
end method return-allowed?;



// Interactive handling.

define method restart-query (restart :: <restart>)
  #f;
end method restart-query;


define generic return-query (condition);

