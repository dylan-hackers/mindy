module: Dylan-User
rcs-header: $Header: /home/housel/work/rcs/gd/src/demos/hello-world/hello-world.dylan,v 1.5 1996/02/17 18:04:18 nkramer Exp $

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
// This is the canonical ``hello, world'' demo.
//
// Mindy invokes the generic function main with the command line arguments
// as strings.  Given that we don't care what they are, we just ignore them.
//
// We just put this in the Dylan-User module, because there is no reason to
// define out own module for something this simplistic.


define method main (argv0 :: <byte-string>, #rest noise)
  puts("Hello, World.\n");
end;
