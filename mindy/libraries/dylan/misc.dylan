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
//  $Header: /home/housel/work/rcs/gd/src/mindy/libraries/dylan/misc.dylan,v 1.5 1994/06/11 02:13:43 wlott Exp $
//
//  This file does whatever.
//

define method identity (x)
  x;
end;

define method as (c :: <class>, thing)
  if (instance?(thing, c))
    thing;
  else
    error("%= cannot be converted to %=", thing, c);
  end;
end;

// The built-in "as(<symbol>...)" method only handles byte-strings.
define method as(cls == <symbol>, str :: <string>)
  as(<symbol>, as(<byte-string>, str));
end method as;


define constant \| =
  method (#rest ignore)
    error("| is syntax only and can't be used as a function.");
  end;

define constant \& =
  method (#rest ignore)
    error("& is syntax only and can't be used as a function.");
  end;

define constant \:= =
  method (#rest ignore)
    error(":= is syntax only and can't be used as a function.");
  end;

define method make (c == <generic-function>,
		    #key debug-name, required: req, rest?, key, all-keys?)
  let req = select (req by instance?)
	      <integer> =>
		if (req < 0)
		  error("required: can't be negative: %d",
			req);
		end;
	        req;
	      <sequence> =>
		do(rcurry(check-type, <type>), req);
	        size(req);
	    end;
  if (instance?(key, <collection>))
    do(rcurry(check-type, <symbol>), key);
    if (rest?)
      error("rest?: cannot be true when keywords are supplied.");
    end;
  elseif (key)
    error("bogus value for key:, must be either #f or a "
	    "collection of symbols.");
  elseif (all-keys?)
    error("all-keys?: cannot be true as long as key: is #f.");
  end;
  make-generic-function(debug-name, req, rest?, key, all-keys?,
			#(), <object>);
end;
