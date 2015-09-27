module: random-test
author: David Watson, Nick Kramer
synopsis: Test for the random library.
copyright: See below.

//======================================================================
//
// Copyright (c) 1996  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
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
//    University, and the Gwydion Dylan Maintainers.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
// Also, see http://www.gwydiondylan.org/ for updates and documentation. 
//
//======================================================================

define variable has-errors = #f;

define method run-several-tests (test-name :: <string>, 
                                 test :: <function>)
 => ();
  format("%s ... ", test-name);
  let temp-has-errors = has-errors;
  has-errors := #f;
  test();
  if (has-errors == #f)
    format("ok.\n");
  end if;
  has-errors := temp-has-errors | has-errors;
end method run-several-tests;

define method run-test (input, expected-result, test-name :: <string>)
 => passed? :: <boolean>;
  if (input ~= expected-result)
    has-errors := #t;
    format("Failed!\n", test-name);
    format("     Got %=\n", input);
    format("     when we expected %=\n", expected-result);
    #f;
  else
    #t;
  end if;
end method run-test;

define method random-test () => ();
  let random-1 :: <integer> = random(20);
  let random-2 :: <integer> = random-bits();
#if (mindy)
  let random-3 :: <float> = random-float(30);
  let random-4 :: <float> = random-gaussian();
  let random-5 :: <float> = random-exponential();
#endif
end method random-test;

define method main (argv0, #rest ignored)
  format("\nRegression test for the random library.\n\n");
  run-several-tests("random", random-test);
  if (has-errors)
    format("\n********* Warning!  Regression test failed! ***********\n");
  else
    format("All random tests pass.\n");
  end if;
end method main;
