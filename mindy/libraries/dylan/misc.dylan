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
//  $Header: /home/housel/work/rcs/gd/src/mindy/libraries/dylan/misc.dylan,v 1.4 1994/06/03 00:40:13 wlott Exp $
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

