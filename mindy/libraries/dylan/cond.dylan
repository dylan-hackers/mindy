module: Dylan

//////////////////////////////////////////////////////////////////////
//
//  Copyright (C) 1994, Carnegie Mellon University
//  All rights reserved.
//
//  This code was produced by the Gwydion Project at Carnegie Mellon
//  University.  If you are interested in using this code, contact
//  "Scott.Fahlman@cs.cmu.edu" (Internet).
//
//////////////////////////////////////////////////////////////////////
//
//  $Header: /home/housel/work/rcs/gd/src/mindy/libraries/dylan/cond.dylan,v 1.4 1994/05/31 18:12:31 nkramer Exp $
//
//  This file does whatever.
//


// Classes

define class <condition> (<object>)
end;

define class <serious-condition> (<condition>)
end;

define class <error> (<serious-condition>)
end;

define class <simple-condition> (<condition>)
  slot condition-format-string,
    required-init-keyword: format-string:;
  slot condition-format-arguments,
    init-keyword: format-arguments:,
    init-value: #();
end;

define class <simple-error> (<error>, <simple-condition>)
end;

define class <type-error> (<error>)
  slot type-error-value, init-keyword: value:;
  slot type-error-expected-type, init-keyword: type:;
end;

define class <warning> (<condition>)
end;

define class <simple-warning> (<warning>, <simple-condition>)
end;

define class <restart> (<condition>)
end;

define class <simple-restart> (<restart>)
end;

define class <abort> (<restart>)
end;


// Condition reporting.

define method report-condition (condition :: <condition>)
  prin1(condition);
end;

define method report-condition (condition :: <simple-condition>)
  apply(format,
	condition.condition-format-string,
	condition.condition-format-arguments);
end;

define method report-condition (condition :: <type-error>)
  format("%= is not of type %=",
	 condition.type-error-value,
	 condition.type-error-expected-type);
end;


// Condition signaling

define method signal (string :: <string>, #rest arguments)
  signal(make(<simple-warning>,
	      format-string: string,
	      format-arguments: arguments));
end;

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
	  end;
	else
	  search(h.handler-next);
	end;
      else
	default-handler(cond);
      end;
    end;
  search(current-handler());
end;

define method error (string :: <string>, #rest arguments)
  error(make(<simple-error>,
	     format-string: string,
	     format-arguments: arguments));
end;

define method error (cond :: <condition>, #rest noise)
  unless (empty?(noise))
    error("Can only supply format arguments when supplying a format string.");
  end;
  signal(cond);
  invoke-debugger(make(<simple-error>,
		       format-string:
			 "Attempt to return from a call to error"));
end;

define method cerror (restart-descr, cond-or-string, #rest arguments)
  block ()
    apply(error, cond-or-string, arguments);
  exception (<simple-restart>, description: restart-descr)
    #f;
  end;
end;

define method type-error (value, type)
  error(make(<type-error>, value: value, type: type));
end;

define method check-type (value, type)
  if (instance?(value, type))
    value;
  else
    type-error(value, type);
  end;
end;

define method abort ()
  error(make(<abort>));
end;

define method default-handler (condition :: <condition>)
  #f;
end;

define method default-handler (condition :: <serious-condition>)
  invoke-debugger(condition);
end;

define method default-handler (condition :: <warning>)
  report-condition(condition);
  #f;
end;

define method default-handler (restart :: <restart>)
  error("No restart handler for %=", restart);
end;


// Breakpoints.

define class <breakpoint> (<simple-warning>)
end;

define method return-allowed? (cond :: <breakpoint>)
  #t;
end;

define method return-query (cond :: <breakpoint>)
  #f;
end;

define method return-description (cond :: <breakpoint>)
  "Return #f";
end;

define method %break (string :: <string>, #rest arguments)
  %break(make(<breakpoint>,
	      format-string: string,
	      format-arguments: arguments));
end;

define method %break (cond :: <condition>, #rest noise)
  unless (empty?(noise))
    error("Can only supply format arguments when supplying a format string.");
  end;
  block ()
    invoke-debugger(cond);
  exception (<simple-restart>, description: "Continue from break")
    #f;
  end;
end;

define method break (#rest arguments)
  if (empty?(arguments))
    %break("Break.");
  else
    apply(%break, arguments);
  end;
end;


// Introspection.

define method do-handlers (function :: <function>)
  for (h = current-handler() then h.handler-next,
       while h)
    function(h.handler-type,
	     h.handler-test | method (x) #t end,
	     h.handler-function,
	     h.handler-description);
  end;
end;

define method return-allowed? (cond :: <condition>)
  #f;
end;

define generic return-description (cond);


// Interactive handling.

define method restart-query (restart :: <restart>)
  #f;
end;

define generic return-query (condition);

