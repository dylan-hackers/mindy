module: format-out-test
author: David Watson, Nick Kramer
synopsis: Test for the format-out library.
copyright: See below.
rcs-header: $Header: /home/housel/work/rcs/gd/src/tests/format-out-test.dylan,v 1.2 1996/07/30 19:37:50 bfw Exp $

//======================================================================
//
// Copyright (c) 1996  Carnegie Mellon University
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

define library format-out-test
  use Dylan;
  use Standard-io;
  use Format-out;
  use streams;
end library format-out-test;

define module format-out-test
  use Dylan;
  use Extensions;
  use Format-out;
  use streams;
  use Standard-io;
  use Format;
end module format-out-test;

define method main (argv0 :: <byte-string>, #rest ignored)
  format(*standard-output*, "\nRegression test for the streams library.\n\n");
  format-out("All Format-out tests pass.\n");
  force-output(*standard-output*);
end method main;
