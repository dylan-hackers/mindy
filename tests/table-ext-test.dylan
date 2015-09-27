module: table-ext-test
author: David Watson, Nick Kramer
synopsis: Test for the table-extensions library.
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

define method string-table-test () => ();
  let string-table = make(<string-table>);
  string-table["Fred"] := 42;
  run-test(string-table["Fred"], 42, "string-table-test");
end method string-table-test;

define method case-ins-test () => ();
  let case-ins-string-table = make(<case-insensitive-string-table>);
  case-ins-string-table["DeRf"] := 42;
  run-test(case-ins-string-table["dErF"], 42, "case-ins-string-table-test");

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
  run-several-tests("remove-all-keys!", remove-all-keys-test);
  if (has-errors)
    format("\n********* Warning!  Regression test failed! ***********\n");
  else
    format("All table-extension tests pass.\n");
  end if;
end method main;
