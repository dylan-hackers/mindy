module: table-ext-test
author: David Watson, Nick Kramer
synopsis: Test for the table-extensions library.
copyright: See below.
rcs-header: $Header: /scm/cvs/src/tests/table-ext-test.dylan,v 1.1 1998/05/03 19:54:58 andreas Exp $

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
    format("Failed!\n", test-name);
    format("     Got %=\n", input);
    format("     when we expected %=\n", expected-result);
    #f;
  else
    #t;
  end if;
end method run-test;

define method string-table-test () => ();
  let string-table = make(<string-table>);
  string-table["Fred"] := 42;
  run-test(string-table["Fred"], 42, "string-table-test");
end method string-table-test;

define method hash-function-test () => ();
  // Test for collection-hash
  let collection = #[5, 3, 2, 6];
  let (hash-id :: <integer>, hash-state :: <hash-state>)
    = collection-hash(object-hash, object-hash, collection);

  // Test for sequence-hash
  let (hash-id :: <integer>, hash-state :: <hash-state>)
    = sequence-hash(object-hash, collection);

  // Test for values-hash
  let (hash-id :: <integer>, hash-state :: <hash-state>)
    = values-hash(object-hash, 4, 5, 6, 7);

  // Test for string-hash
  let (hash-id :: <integer>, hash-state :: <hash-state>)
    = string-hash("Bob");
end method hash-function-test;

define method case-ins-test () => ();
  // Test for case-insensitive-string-hash
  let (id1 :: <integer>, hs1 :: <hash-state>)
    = case-insensitive-string-hash("HelLo WORld");
  let (id2 :: <integer>, hs2 :: <hash-state>)
    = case-insensitive-string-hash("helLO wOrLd");
  run-test(pair(id1, hs1), pair(id2, hs2),
	   "case-insensitive-string-hash test");

  // Test for case-insensitive-equal
  run-test(case-insensitive-equal("boB", "BoB"), #t,
	   "case-insensitive-equal-test");
end method case-ins-test;

define method remove-all-keys-test () => ();
  // Test case for remove-all-keys!
  let string-table = make(<string-table>);
  string-table["Fred"] := 42;  
  remove-all-keys!(string-table);
  run-test(member?(42, string-table), #f, "remove-all-keys! test");
end method remove-all-keys-test;

define method main (argv0, #rest ignored)
  format("\nRegression test for the table-extensions library.\n\n");
  run-several-tests("string tables", string-table-test);
  run-several-tests("case-insensitive functions", case-ins-test);
  run-several-tests("hash functions", hash-function-test);
  run-several-tests("remove-all-keys!", remove-all-keys-test);
  if (has-errors)
    format("\n********* Warning!  Regression test failed! ***********\n");
  else
    format("All table-extension tests pass.\n");
  end if;
end method main;
