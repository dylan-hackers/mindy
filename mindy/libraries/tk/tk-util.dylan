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
// This file provides basic utilities for the Dylan/Tk implementation.  This
// includes the "anonymous-name", "make-option", "join-tk-args" and
// "parse-tk-list" functions, but it also include initialization and thread
// routines which are automatically invoked as a result of loading the
// library.  These provide much of the real Dylan/Tk functionality.
//
//======================================================================

// Generates a unique new (but unexciting) name for a window.
//
define generic anonymous-name (#key prefix) => (result :: <string>);

// Creates a tk "option" switch out of option name and an arbitrary value.
// The value may be an <integer>, <string>, or <symbol>, or <true>.  If the
// option is #f, then an empty string (i.e. no option) will be returned.
//
define generic make-option (option, value) => (option :: <string>);

// Creates a string containing the tk representation (via tk-as) of each
// argument, separated by spaces.
//
define generic join-tk-args (#rest object) => (result :: <string>);

// Takes a string returned by a tk function and converts it into a sequence
// of values.  Values will either be 'words' or subsequences, depending upon
// whether the string contains list grouping (i.e. '{' and '}') characters.
// If depth is not #f, all groupings below the given depth will be returned as
// simple strings.  If "unquote" is true, then result strings will be
// filtered through "unquote".
//
define generic parse-tk-list
    (string :: <string>, #key depth, start, end:, unquote)
 => (result :: <sequence>);


//======================================================================
//			      Utility functions
//======================================================================

// Auxiliary variable for "anonymous-name".
define variable anonymous-counter = 0;

// Generates a unique new (but unexciting) name for a window.
//
define method anonymous-name (#key prefix = "w") => (result :: <string>);
  concatenate(prefix,
	      tk-as(<string>, anonymous-counter := anonymous-counter + 1))
end method anonymous-name;

// Creates a tk "option" switch out of option name and an arbitrary value.
// The value may be an <integer>, <string>, or <symbol>, or <true>.  If the
// option is #f, then an empty string (i.e. no option) will be returned.
//
define method make-option (option, value) => (option :: <string>);
  if (value)
    concatenate(" -", tk-as(<string>, option),
		" \"", tk-quote(value), "\" ");
  else
    "";
  end if;
end method make-option;

// See description of the generic above.
//
define method join-tk-args (#rest objects) => (result :: <string>);
  let stream = make(<byte-string-stream>, direction: #"output");
  for (elem in objects,
       dummy = #f then write-element(stream, ' '))
    write(stream, tk-as(<string>, elem));
  end for;
  stream-contents(stream);
end method join-tk-args;

//==========================================================================

// Takes a string returned by a tk function and converts it into a sequence
// of values.  Values will either be 'words' or subsequences, depending upon
// whether the string contains list grouping (i.e. '{' and '}') characters.
// If depth is not #f, all groupings below the given depth will be returned as
// simple strings.  If "unquote" is not #f, then result strings will be
// filtered through "unquote".
//
define method parse-tk-list
    (string :: <string>,
     #key depth, start = 0, end: last = string.size, unquote)
 => (result :: <sequence>);
  local method skip-spaces (string, start)
	  for (i from start below string.size, until: string[i] ~= ' ')
	  finally i;
	  end for;
	end method;
  local method balance-open-brace (string, start, last)
	  for (count = 1 then select (string[i])
				'{' => count + increment;
				'}' => count - increment;
				otherwise => count;
			      end select,
	       // only change count if we're not quoted
	       increment = 1
		 then if (string[i] == '\\') 1 - increment else 1 end if,
	       i from start below last, until: count == 0)
	  finally
	    i - 1;		// I is the character after the close brace.
	  end for;
	end method balance-open-brace;
  local method word-break (string, start, last)
	  for (quoted = #f then (string[i] == '\\'),
	       i from start below last,
	       until: ~quoted & (string[i] == ' '))
	  finally
	    i;
	  end for;
	end method word-break;
  local method parse-elements (string, first, last, depth);
	  if (depth == 0)
	    let result = copy-sequence(string, start: first, end: last);
	    if (unquote) tk-unquote(result) else result end if;
	  else
	    let first = skip-spaces(string, first);
	    let list = #();
	    until (first >= last | string[first] == '}')
	      if (string[first] == '{')
		let rbrace = balance-open-brace(string, first + 1, last);
		let sublist = parse-elements(string, first + 1,
					     rbrace, depth & (depth - 1));
		list := pair(sublist, list);
		first := skip-spaces(string, rbrace + 1);
	      else 
		let index = word-break(string, first, last);
		let word = copy-sequence(string, start: first, end: index);
		list := pair(if (unquote) tk-unquote(word) else word end,
			     list);
		first := skip-spaces(string, index);
	      end if;
	    end until;
	    values(reverse!(list), first);
	  end if;
	end method parse-elements;

  parse-elements(string, start, last, depth);
end method parse-tk-list;


//==========================================================================
//			   Library initializations
//==========================================================================

// This is the main tk execution loop.  It captures all output from the
// interpreter and appropriate 'callbacks' as necessary, and prints everything
// else to stdout.
//
define method tk-input-loop () => ();
  block ()
    for (line = read-tk-line() then read-tk-line(),
	 while: line)
      if (empty?(line))
	write-line(*standard-output*, line);
	force-output(*standard-output*);
      elseif (first(line) == '!')
	// !E! is a magic line that the process sends to confim we are still
	// alive.  This is a wretched hack forced upon us by the inflexibility
	// of Tcl/Tk.
	if (line ~= "!E!")
	  do-callback(line);
	end if;
      else
	write-line(*standard-output*, line);
	force-output(*standard-output*);
      end if;
    end for;
    exit();
  end block;
end tk-input-loop;

//==========================================================================

// Starts up a copy of "wish", opens streams connected to it, and adds in some
// useful definitions.
//
define method tk-init () => ();
  let (fd-in, fd-out) = fd-exec("wish -name MindyTk");
  if (~fd-in)
    error("Could not spawn 'wish' process -- "
	    "please check your PATH environment.");
  end if;
  tk-in := make(<fd-stream>, fd: fd-in, direction: #"output");
  tk-out := make(<fd-stream>, fd: fd-out);

  // The following two tk lines are used to force the WISH process to
  // periodically send a magic string.  If the parent process has exited, then
  // this will generate an error and it will itself exit.  This is a wretched
  // hack, but it was the best I could find.
  put-tk-line("proc tkerror {msg}",
	       " {if {$msg == {error flushing \"stdout\": Broken pipe}}",
	       " {exit} else {error {Unhandled error.}}}");
  put-tk-line("proc checkexit {} {puts stdout !E!;flush stdout;"
		 "after 3000 checkexit};checkexit");

  put-tk-line("wm withdraw .");
  put-tk-line("proc dylan-put args ",
	       "{puts stdout \"$args\";flush stdout}");
  put-tk-line("wm minsize . 1 1");
  init-active-variables();

  // This sticks around forever and does all of our event dispatch.
  spawn-thread("listen", tk-input-loop);
end method tk-init;

tk-init();
