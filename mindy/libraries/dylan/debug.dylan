module: dylan
rcs-header: $Header: /home/housel/work/rcs/gd/src/mindy/libraries/dylan/debug.dylan,v 1.7 1994/06/27 17:10:21 wlott Exp $

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

define method report-problem (problem)
  block ()
    report-condition(problem);
  exception <error>
    puts("\nproblem reporting problem... giving up");
  end;
end;


define constant debug-variables = make(<stretchy-vector>);


define method debugger-flush ()
  debug-variables.size := 0;
  puts("Flushed all debugger variables.\n");
  values();
end;



define method eval-debugger-expr (expr, num-debug-vars)
  select (head(expr))
    debug-var: =>
      let var = tail(expr);
      block ()
	if (var < 0)
	  debug-variables[num-debug-vars + var];
	else
	  debug-variables[var];
	end;
      exception <error>
	error("No debug variable $%=", var);
      end;
    literal: => tail(expr);
    funcall: =>
      apply(method (func, #rest args) apply(func, args) end,
	    map(rcurry(eval-debugger-expr, num-debug-vars), tail(expr)));
  end;
end method;


define method debugger-eval (expr)
  block ()
    block ()
      let (#rest results) = eval-debugger-expr(expr, debug-variables.size);
      values(#t, results);
    exception (problem :: <error>)
      puts("invocation failed:\n  ");
      report-problem(problem);
      putc('\n');
      #f;
    end;
  exception (<error>)
    puts("Could not recover from earlier error.\n");
    #f;
  end;
end;
    

define method eval-and-print (expr, num-debug-vars)
  let (#rest results) = eval-debugger-expr(expr, num-debug-vars);
  if (empty?(results))
    puts("[0 values returned]");
  else
    for (first = #t then #f,
	 result in results)
      unless (first)
	puts(", ");
      end;
      format("$%==%=", debug-variables.size, result);
      add!(debug-variables, result);
    end;
  end;
  putc('\n');
end method;

define method debugger-call (exprs)
  let num-debug-vars = debug-variables.size;
  for (expr in exprs)
    block ()
      eval-and-print(expr, num-debug-vars);
    exception (<abort>, init-arguments: list(description: "Blow off call"))
      #f;
    end;
  end;
end;

define method debugger-print (exprs)
  block ()
    let num-debug-vars = debug-variables.size;
    for (expr in exprs)
      block ()
	eval-and-print(expr, num-debug-vars);
      exception (problem :: <error>)
	puts("invocation failed:\n  ");
	report-problem(problem);
	putc('\n');
      end;
    end;
  exception (<error>)
    puts("Could not recover from earlier error.\n");
  end;
end;

define method debugger-report-condition (cond)
  block ()
    putc('\n');
    block ()
      report-condition(cond);
    exception (problem :: <error>)
      puts("problem reporting condition:\n  ");
      report-problem(problem);
    end;
    puts("\n\n");
  exception <error>
    puts("\nCould not recover from earlier errors.\n\n");
  end;
end;
  

define method debugger-abort ()
  block ()
    block ()
      signal(make(<abort>));
    exception (problem :: <error>)
      puts("problem signaling abort restart:\n  ");
      report-problem(problem);
      putc('\n');
    end;
  exception <error>
    puts("Could not recover from earlier errors.\n");
  end block;
end;


define method debugger-describe-restarts (cond)
  block ()
    block ()
      let index = 0;
      for (h = current-handler() then h.handler-next, while h)
	let type = h.handler-type;
	if (instance?(type, <class>) & subtype?(type, <restart>))
	  block ()
	    format("%= [%=]: ", index, type);
	    report-condition(apply(make, type, h.handler-init-args));
	  exception (problem :: <error>)
	    puts("\nproblem describing restart:\n  ");
	    report-problem(problem);
	  end;
	  putc('\n');
	  index := index + 1;
	end if;
      end for;
      if (zero?(index))
	puts("No active restarts.\n");
      end;
    exception (problem :: <error>)
      puts("\nproblem describing restarts:\n  ");
      report-problem(problem);
      putc('\n');
    end;
    block ()
      if (instance?(cond, <condition>) & return-allowed?(cond))
	block ()
	  puts("\nReturning is allowed");
	  let description = return-description(cond);
	  select (description by instance?)
	    singleton(#f) =>
	      #f;
	    <byte-string> =>
	      puts(":\n  ");
	      puts(description);
	    <restart> =>
	      puts(":\n  ");
	      report-condition(description);
	  end;
	exception (problem :: <error>)
	  puts("\nproblem describing return convention:\n  ");
	  report-problem(problem);
	end block;
	putc('\n');
      end if;
    exception (problem :: <error>)
      puts("\nproblem checking on return contention:\n  ");
      report-problem(problem);
      putc('\n');
    end block;
  exception <error>
    puts("\nCould not recover from earlier errors.\n");
  end block;
end method;

define method debugger-restart (cond, index)
  block (return)
    let count = 0;
    for (h = current-handler() then h.handler-next, while h)
      let type = h.handler-type;
      let test = h.handler-test;
      if (instance?(type, <class>) & subtype?(type, <restart>))
	if (count == index)
	  block ()
	    let restart = apply(make, type, h.handler-init-args);
	    restart-query(restart);
	    unless (~test | test(h))
	      puts("The restart handler refused to handle the restart.\n");
	      return(#f);
	    end;
	    local
	      method next-handler ()
		puts("The restart handler declined to handle the restart.\n");
		return(#f);
	      end;
	    let (#rest values) = h.handler-function(restart, next-handler);
	    if (instance?(cond, <condition>) & return-allowed?(cond))
	      return(#t, values);
	    else
	      puts("The restart handler tried to return, but returning "
		     "is not allowed\n");
	      return(#f);
	    end if;
	  exception (problem :: <error>)
	    puts("Problem while attempting to restart:\n  ");
	    report-problem(problem);
	    putc('\n');
	    return(#f);
	  end block;
	else
	  count := count + 1;
	end if;
      end if;
    end for;
    if (zero?(count))
      puts("No active restarts.\n");
    else
      format("Invalid restart number, should be less than %d\n", count);
    end if;
    #f;
  exception <error>
    puts("Could not recover from earlier errors.\n");
    #f;
  end block;
end method;
      
    
define method debugger-return (cond)
  block (return)
    block ()
      if (instance?(cond, <condition>) & return-allowed?(cond))
	block ()
	  let (#rest values) = return-query(cond);
	  return(#t, values);
	exception (problem :: <error>)
	  puts("problem quering for values to return:\n  ");
	  report-problem(problem);
	  putc('\n');
	  return(#f);
	end;
      else
	puts("Returning is not allowed\n");
	return(#f);
      end;
    exception (problem :: <error>)
      puts("problem checking to see if returning is allowed:\n  ");
      report-problem(problem);
      putc('\n');
      return(#f);
    end;
  exception <error>
    puts("Could not recover from earlier errors.\n");
    #f;
  end block;
end method;



// Now that we have the dylan helper routines defined, enable the error system.
enable-error-system();
