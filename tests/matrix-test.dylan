module: matrix-test
author: David Watson, Nick Kramer
synopsis: Test for the matrix library.
copyright: See below.
rcs-header: $Header: /scm/cvs/src/tests/matrix-test.dylan,v 1.1 1998/05/03 19:54:58 andreas Exp $

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

define method arith-test () => ();
  let matrix-a :: <matrix> = make(<matrix>, dimensions: #[2, 2]);
  let matrix-b :: <matrix> = make(<matrix>, dimensions: #[2, 2]);
  let matrix-c :: <matrix> = make(<matrix>, dimensions: #[2, 2]);
  let matrix-d :: <matrix> = make(<matrix>, dimensions: #[2, 2]);
  let matrix-e :: <matrix> = make(<matrix>, dimensions: #[2, 2]);

  matrix-a[0, 0] := 1;
  matrix-a[0, 1] := 3;
  matrix-a[1, 0] := 4;
  matrix-a[1, 1] := 7;

  matrix-b[0, 0] := 4;
  matrix-b[0, 1] := 9;
  matrix-b[1, 0] := 2;
  matrix-b[1, 1] := 1;

  matrix-c[0, 0] := 5;
  matrix-c[0, 1] := 12;
  matrix-c[1, 0] := 6;
  matrix-c[1, 1] := 8;

  matrix-d[0, 0] := 10;
  matrix-d[0, 1] := 12;
  matrix-d[1, 0] := 30;
  matrix-d[1, 1] := 43;

  matrix-e[0, 0] := 5;
  matrix-e[0, 1] := 15;
  matrix-e[1, 0] := 20;
  matrix-e[1, 1] := 35;

  run-test(matrix-a + matrix-b, matrix-c, "matrix addition");
  run-test(matrix-a + matrix-b - matrix-a, matrix-b, "matrix subtraction");
  run-test(matrix-a * matrix-b, matrix-d, "matrix multiplication");
  run-test(matrix-a * 5, matrix-e, "scalar multiplication");
  run-test(5 * matrix-a, matrix-e, "scalar multiplication");
end method arith-test;

define method lin-alg-test () => ();
  let matrix-a :: <matrix> = make(<matrix>, dimensions: #[2, 3]);
  let matrix-b :: <matrix> = make(<matrix>, dimensions: #[2, 1]);
  let matrix-c :: <matrix> = make(<matrix>, dimensions: #[2, 2]);
  let matrix-d :: <matrix> = make(<matrix>, dimensions: #[2, 2]);
  let matrix-e :: <matrix> = make(<matrix>, dimensions: #[2, 2]);
  
  matrix-a[0, 0] := 3;
  matrix-a[0, 1] := 4;
  matrix-a[0, 2] := 10;
  matrix-a[1, 0] := 2;
  matrix-a[1, 1] := -3;
  matrix-a[1, 2] := 1;

  matrix-b[0, 0] := 2;
  matrix-b[1, 0] := 1;

  matrix-c[0, 0] := 1;
  matrix-c[0, 1] := 2;
  matrix-c[1, 0] := 1;
  matrix-c[1, 1] := 1;

  matrix-d[0, 0] := -1;
  matrix-d[0, 1] := 2;
  matrix-d[1, 0] := 1;
  matrix-d[1, 1] := -1;

  matrix-e[0, 0] := 1;
  matrix-e[0, 1] := 1;
  matrix-e[1, 0] := 2;
  matrix-e[1, 1] := 1;

  run-test(gauss-jordan(matrix-a), matrix-b, "gauss-jordan");
  run-test(inverse(matrix-c), matrix-d, "inverse");
  run-test(det(matrix-c), -1, "determinant");
  run-test(transpose(matrix-c), matrix-e, "transpose");
end method lin-alg-test;

define method misc-test () => ();
  let matrix-a :: <matrix> = make(<matrix>, dimensions: #[2, 2]);
  let matrix-b :: <matrix> = make(<matrix>, dimensions: #[2, 4]);
  
  matrix-a[0, 0] := 1;
  matrix-a[1, 1] := 1;

  matrix-b[0, 0] := 1;
  matrix-b[1, 1] := 1;
  matrix-b[0, 2] := 1;
  matrix-b[1, 3] := 1;

  run-test(identity-matrix(dimensions: #[2, 2]), matrix-a, "identity-matrix");
  run-test(augment-matrix(matrix-a, matrix-a), matrix-b, "augment-matrix");
end method misc-test;

define method main (argv0, #rest ignored)
  format("\nRegression test for the matrix library.\n\n");
  run-several-tests("Arithmetic", arith-test);
  run-several-tests("Linear Algebra", lin-alg-test);
  run-several-tests("Misc", misc-test);
  if (has-errors)
    format("\n********* Warning!  Regression test failed! ***********\n");
  else
    format("All matrix tests pass.\n");
  end if;
  force-output(*standard-output*);
end method main;