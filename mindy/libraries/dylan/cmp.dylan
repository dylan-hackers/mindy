module: Dylan
rcs-header: $Header: /home/housel/work/rcs/gd/src/mindy/libraries/dylan/cmp.dylan,v 1.5 1994/06/27 17:10:16 wlott Exp $

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
// This file contains the comparisons that arn't built in.
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
