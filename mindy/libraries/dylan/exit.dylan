module: dylan

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
//  $Header: /home/housel/work/rcs/gd/src/mindy/libraries/dylan/exit.dylan,v 1.1 1994/04/30 15:03:52 wlott Exp $
//
//  This file does whatever.
//

define constant *on-exit* = make(<deque>);

define method exit (exit-code :: <integer>)
  while (~empty?(*on-exit*))
    pop(*on-exit*)();
  end;
  raw-exit(exit-code);
end;

define method on-exit(fun :: <function>)
  push-last(*on-exit*, fun);
end;
