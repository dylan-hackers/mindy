module: format-test
author: David Watson, Nick Kramer
synopsis: Test for the format library.
copyright: See below.
rcs-header: $Header: /scm/cvs/src/tests/format-test.dylan,v 1.1 1998/05/03 19:54:58 andreas Exp $

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

define variable has-errors = #f;

define method run-several-tests (test-name :: <string>, 
                                 test :: <function>)
 => ();
  cheap-io-format("%s ... ", test-name);
  let temp-has-errors = has-errors;
  has-errors := #f;
  test();
  if (has-errors == #f)
    cheap-io-format("ok.\n");
  end if;
  has-errors := temp-has-errors | has-errors;
end method run-several-tests;

define method run-test (input, expected-result, test-name :: <string>)
 => passed? :: <boolean>;
  if (input ~= expected-result)
    has-errors := #t;
    cheap-io-format("Failed %s!\n", test-name);
    cheap-io-format("     Got %=\n", input);
    cheap-io-format("     when we expected %=\n", expected-result);
    #f;
  else
    #t;
  end if;
end method run-test;

define method format-test () => ();
  let string
    = format-to-string("%S%=%C%D%B%O%X%%", #"Hello", 32, 'a', 67, 2, 8, 16);
  run-test(string, "Hello32a67101010%", "format-to-string");
end method format-test;

define method main (argv0, #rest ignored)
  cheap-io-format("\nRegression test for the format library.\n\n");
  run-several-tests("format", format-test);
  if (has-errors)
    cheap-io-format("\n********* Warning!  Regression test failed! ***********\n");
  else
    cheap-io-format("All format tests pass.\n");
  end if;
end method main;
