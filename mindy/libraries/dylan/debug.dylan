module: dylan
author: William Lott (wlott@cs.cmu.edu)
rcs-header: $Header: /home/housel/work/rcs/gd/src/mindy/libraries/dylan/debug.dylan,v 1.12 1996/02/13 20:43:17 nkramer Exp $

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
// This file contains the support routines used by the debugger.
//

define variable *debug-output* = #f;

define method report-problem (problem)
  block ()
    report-condition(problem, *debug-output*);
  exception <error>
    *format-function*(*debug-output*,
			"\nproblem reporting problem... giving up");
  end block;
end method report-problem;


define constant debug-variables = make(<stretchy-vector>);


define method debugger-flush ()
  debug-variables.size := 0;
  puts("Flushed all debugger variables.\n");
  values();
end method debugger-flush;


define method eval-debugger-expr (expr, num-debug-vars)
  select (head(expr))
    debug-var: =>
      let var = tail(expr);
      block ()
	if (var < 0)
	  debug-variables[num-debug-vars + var];
	else
	  debug-variables[var];
	end if;
      exception <error>
	error("No debug variable $%=", var);
      end block;
    literal: => tail(expr);
    funcall: =>
      apply(method (func, #rest args) apply(func, args) end,
	    map(rcurry(eval-debugger-expr, num-debug-vars), tail(expr)));
  end select;
end method eval-debugger-expr;


define method debugger-eval (expr)
  block ()
    block ()
      let (#rest results) = eval-debugger-expr(expr, debug-variables.size);
      values(#t, results);
    exception (problem :: <error>)
      *format-function*(*debug-output*, "invocation failed:\n  ");
      report-problem(problem);
      *format-function*(*debug-output*, "\n");
      *force-output-function*(*debug-output*);
      #f;
    end block;
  exception (<error>)
    puts("Could not recover from earlier error.\n");
    #f;
  end block;
end method debugger-eval;


define method eval-and-print (expr, num-debug-vars)
  let (#rest results) = eval-debugger-expr(expr, num-debug-vars);
  if (empty?(results))
    *format-function*(*debug-output*, "[0 values returned]");
  else
    for (first = #t then #f,
	 result in results)
      unless (first)
	*format-function*(*debug-output*, ", ");
      end;
      *format-function*(*debug-output*, "$%==%=",
			debug-variables.size, result);
      add!(debug-variables, result);
    end for;
  end if;
  *format-function*(*debug-output*, "\n");
  *force-output-function*(*debug-output*);
end method eval-and-print;


define method debugger-call (exprs)
  let num-debug-vars = debug-variables.size;
  block ()
    for (expr in exprs)
      eval-and-print(expr, num-debug-vars);
    end for;
  exception (<abort>, init-arguments: list(description: "Blow off call"))
    #f;
  end block;
end method debugger-call;


define method debugger-print (exprs)
  block ()
    let num-debug-vars = debug-variables.size;
    for (expr in exprs)
      block ()
	eval-and-print(expr, num-debug-vars);
      exception (problem :: <error>)
	*format-function*(*debug-output*, "invocation failed:\n  ");
	report-problem(problem);
	*format-function*(*debug-output*, "\n");
	*force-output-function*(*debug-output*);
      end block;
    end for;
  exception (<abort>, init-arguments: list(description: "Blow off print"))
    #f;
  exception (<error>)
    puts("Could not recover from earlier error.\n");
  end block;
end method debugger-print;


define method debugger-report-condition (cond)
  block ()
    *format-function*(*debug-output*, "\n");
    block ()
      report-condition(cond, *debug-output*);
    exception (problem :: <error>)
      *format-function*(*debug-output*, "problem reporting condition:\n  ");
      report-problem(problem);
    end block;
    *format-function*(*debug-output*, "\n\n");
    *force-output-function*(*debug-output*);
  exception <error>
    puts("\nCould not recover from earlier errors.\n\n");
  end block;
end method debugger-report-condition;


define method debugger-abort ()
  block ()
    block ()
      signal(make(<abort>));
    exception (problem :: <error>)
      *format-function*(*debug-output*,
			"problem signaling abort restart:\n  ");
      report-problem(problem);
      *format-function*(*debug-output*, "\n");
      *force-output-function*(*debug-output*);
    end block;
  exception <error>
    puts("Could not recover from earlier errors.\n");
  end block;
end method debugger-abort;


define method debugger-describe-restarts (cond)
  block ()
    block ()
      let index = 0;
      for (h = current-handler() then h.handler-next, while: h)
	let type = h.handler-type;
	if (instance?(type, <class>) & subtype?(type, <restart>))
	  block ()
	    *format-function*(*debug-output*, "%= [%=]: ", index, type);
	    report-condition(apply(make, type, h.handler-init-args),
			     *debug-output*);
	  exception (problem :: <error>)
	    *format-function*(*debug-output*,
			      "\nproblem describing restart:\n  ");
	    report-problem(problem);
	  end block;
	  *format-function*(*debug-output*, "\n");
	  index := index + 1;
	end if;
      end for;
      if (zero?(index))
	*format-function*(*debug-output*, "No active restarts.\n");
      end if;
    exception (problem :: <error>)
      *format-function*(*debug-output*, "\nproblem describing restarts:\n  ");
      report-problem(problem);
      *format-function*(*debug-output*, "\n");
    end block;
    block ()
      if (instance?(cond, <condition>) & return-allowed?(cond))
	block ()
	  *format-function*(*debug-output*, "\nReturning is allowed");
	  let description = return-description(cond);
	  select (description by instance?)
	    singleton(#f) =>
	      #f;
	    <byte-string> =>
	      *format-function*(*debug-output*, ":\n  %s", description);
	    <restart> =>
	      *format-function*(*debug-output*, ":\n  ");
	      report-condition(description, *debug-output*);
	  end select;
	  *force-output-function*(*debug-output*);
	exception (problem :: <error>)
	  *format-function*(*debug-output*,
			    "\nproblem describing return convention:\n  ");
	  report-problem(problem);
	end block;
	*format-function*(*debug-output*, "\n");
      end if;
      *force-output-function*(*debug-output*);
    exception (problem :: <error>)
      *format-function*(*debug-output*,
			"\nproblem checking on return contention:\n  ");
      report-problem(problem);
      *format-function*(*debug-output*, "\n");
      *force-output-function*(*debug-output*);
    end block;
  exception <error>
    puts("\nCould not recover from earlier errors.\n");
  end block;
end method debugger-describe-restarts;


define method debugger-restart (cond, index)
  block (return)
    let count = 0;
    for (h = current-handler() then h.handler-next, while: h)
      let type = h.handler-type;
      let test = h.handler-test;
      if (instance?(type, <class>) & subtype?(type, <restart>))
	if (count == index)
	  block ()
	    let restart = apply(make, type, h.handler-init-args);
	    restart-query(restart);
	    unless (~test | test(h))
	      *format-function*(*debug-output*,
				"The restart handler refused to "
				  "handle the restart.\n");
	      *force-output-function*(*debug-output*);
	      return(#f);
	    end unless;
	    local
	      method next-handler ()
		*format-function*(*debug-output*,
				  "The restart handler declined "
				    "to handle the restart.\n");
		*force-output-function*(*debug-output*);
		return(#f);
	      end method next-handler;
	    let (#rest values) = h.handler-function(restart, next-handler);
	    if (instance?(cond, <condition>) & return-allowed?(cond))
	      return(#t, values);
	    else
	      *format-function*(*debug-output*,
				"The restart handler tried to return, but "
				  "returning is not allowed\n");
	      *force-output-function*(*debug-output*);
	      return(#f);
	    end if;
	  exception (problem :: <error>)
	    *format-function*(*debug-output*,
			      "Problem while attempting to restart:\n  ");
	    report-problem(problem);
	    *format-function*(*debug-output*, "\n");
	    *force-output-function*(*debug-output*);
	    return(#f);
	  end block;
	else
	  count := count + 1;
	end if;
      end if;
    end for;
    if (zero?(count))
      *format-function*(*debug-output*, "No active restarts.\n");
    else
      *format-function*(*debug-output*,
			"Invalid restart number, should be less than %d\n",
			count);
    end if;
    *force-output-function*(*debug-output*);
    #f;
  exception <error>
    puts("Could not recover from earlier errors.\n");
    #f;
  end block;
end method debugger-restart;
      
    
define method debugger-return (cond)
  block (return)
    block ()
      if (instance?(cond, <condition>) & return-allowed?(cond))
	block ()
	  let (#rest values) = return-query(cond);
	  return(#t, values);
	exception (problem :: <error>)
	  *format-function*(*debug-output*,
			    "problem quering for values to return:\n  ");
	  report-problem(problem);
	  *format-function*(*debug-output*, "\n");
	  *force-output-function*(*debug-output*);
	  return(#f);
	end block;
      else
	*format-function*(*debug-output*, "Returning is not allowed\n");
	*force-output-function*(*debug-output*);
	return(#f);
      end if;
    exception (problem :: <error>)
      *format-function*(*debug-output*,
			"problem checking to see if "
			  "returning is allowed:\n  ");
      report-problem(problem);
      *format-function*(*debug-output*, "\n");
      *force-output-function*(*debug-output*);
      return(#f);
    end block;
  exception <error>
    puts("Could not recover from earlier errors.\n");
    #f;
  end block;
end method debugger-return;



// Now that we have the dylan helper routines defined, enable the error system.
//
enable-error-system();
