module: coll-ext-test
author: David Watson, Nick Kramer
synopsis: Test for the collection-extensions library.
copyright: See below.
rcs-header: $Header: /scm/cvs/src/tests/coll-ext-test.dylan,v 1.1 1998/05/03 19:54:58 andreas Exp $

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
    format("Failed %s!\n", test-name);
    format("     Got %=\n", input);
    format("     when we expected %=\n", expected-result);
    #f;
  else
    #t;
  end if;
end method run-test;

define method sol-test () => ();
  let sol = make(<self-organizing-list>);
  sol[42] := 54;
  run-test(sol[42], 54, "self-organizing-list");
end method sol-test;

define method subseq-test () => ();
  let vector = #[3, 4, 1, 2, 3, 4, 7, 8];
  let subseq = subsequence(vector, start: 2, end: 6);
  run-test(subseq, #[1, 2, 3, 4], "subsequence");
end method subseq-test;

define method vect-search-test () => ();
  let vector = #[1, 2, 1, 5, 6, 5, 7, 9];
  run-test(find-first-key(vector, curry(\==, 5)), 3, "find-first-key");
  run-test(find-last-key(vector, curry(\==, 5)), 5, "find-last-key");
end method vect-search-test;

define method main (argv0, #rest ignored)
  format("\nRegression test for the Collection-extensions library.\n\n");
  run-several-tests("Self-Organizing-Lists", sol-test);
  run-several-tests("Subsequences", subseq-test);
  run-several-tests("Vector-Search", vect-search-test);
  if (has-errors)
    format("\n********* Warning!  Regression test failed! ***********\n");
  else
    format("All Collection-extensions tests pass.\n");
  end if;
end method main;
