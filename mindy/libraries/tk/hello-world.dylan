module: test

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
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

define library test
  use dylan;
  use tk;
end library test;

define module test
  use dylan;
  use extensions;
  use tk;
end module test;

define method main (program-name :: <string>, #rest args);
  let b1 = make(<button>, text: "Okay", relief: "raised",
		command: curry(destroy-window, *root-window*),
		expand: #t, in: *root-window*);
  make(<message>, text: "Hello, world!", aspect: 500,
       side: "top", before: b1);
  map-window(*root-window*);
end method main;
