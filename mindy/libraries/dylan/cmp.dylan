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
//  $Header: /home/housel/work/rcs/gd/src/mindy/libraries/dylan/cmp.dylan,v 1.4 1994/04/14 16:22:44 rgs Exp $
//
//  This file does whatever.
//


// Default methods for non-primitive compares.

define method \<= (x :: <object>, y :: <object>)
  ~(y < x);
end;

define method \~= (x :: <object>, y :: <object>)
  ~(x = y);
end;

define constant \>= =
  begin
    local method \>= (x :: <object>, y :: <object>)
	    ~(x < y);
	  end;
    \>=;
  end;

define constant \> =
  begin
    local method \> (x :: <object>, y :: <object>)
	    y < x;
	  end;
    \>;
  end;
