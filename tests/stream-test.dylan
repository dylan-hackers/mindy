module: stream-test
author: Ben Folk-Williams, Nick Kramer
synopsis: Test for the streams library.
copyright: See below.

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

define method write-test ();
  let s = make(<byte-string-stream>, contents: make(<byte-string>),
	       direction: #"output");
  write(s, "foo");
  run-test(s.stream-contents, "foo", "write test");
  new-line(s);
  #if (newlines-are-CRLF)
     run-test(s.stream-contents, "\r\n", "new-line test");
     write-line(s, "poo");
     run-test(s.stream-contents, "poo\r\n", "write-line test");
  #else
     run-test(s.stream-contents, "\n", "new-line test");
     write-line(s, "poo");
     run-test(s.stream-contents, "poo\n", "write-line test");
  #endif
  write-element(s, 'P');
  write(s, "oop");
  run-test(s.stream-contents, "Poop", "write-element test");
  close(s);
end method write-test;

define method read-test ();
  let s = make(<byte-string-stream>, contents: "Hi Joe.\nFoo");
  run-test(read-line(s), "Hi Joe.", "read-line test");
  run-test(peek(s), 'F', " peek test");
  run-test(read-element(s), 'F', "read-element test");
  run-test(read(s, 2), "oo", "read test");
  close(s);
end method read-test;

define method buffered-write-test ();
  // Except for the call to make, this is exactly the same as write-test.
  // Make edits to both.
  let s = make(<file-stream>, direction: #"output", locator: ".delete.me~");
  // Included ~ in the file name so that 'make clean' will zap it.

  write(s, "foo");
  run-test(s.stream-contents, "foo", "write test");
  new-line(s);
  #if (newlines-are-CRLF)
     run-test(s.stream-contents, "\r\n", "new-line test");
     write-line(s, "poo");
     run-test(s.stream-contents, "poo\r\n", "write-line test");
  #else
     run-test(s.stream-contents, "\n", "new-line test");
     write-line(s, "poo");
     run-test(s.stream-contents, "poo\n", "write-line test");
  #endif
  write-element(s, 'P');
  write(s, "oop");
  run-test(s.stream-contents, "Poop", "write-element test");
  close(s);
end method buffered-write-test;

define method buffered-read-test ();
  let s1 = make(<file-stream>, direction: #"output", locator: ".delete.me~");
  // Included ~ in the file name so that 'make clean' will zap it.

  write(s1, "Hi Joe.");
  new-line(s1);
  write(s1, "Foo");
  // s1.stream-position := #"start";
  close(s1);
  let s = make(<file-stream>, direction: #"input", locator: ".delete.me~");
  // The rest is identical to read-test. Make edits to both.
  run-test(read-line(s), "Hi Joe.", "read-line test");
  run-test(peek(s), 'F', " peek test");
  run-test(read-element(s), 'F', "read-element test");
  run-test(read(s, 2), "oo", "read test");
  close(s);
end method buffered-read-test;

define method main (argv0, #rest ignored)
  format("\nRegression test for the streams library.\n\n");
  run-several-tests("Writing", write-test);
  run-several-tests("Reading", read-test);
  run-several-tests("Buffered writing", buffered-write-test);
  run-several-tests("Buffered reading", buffered-read-test);
  if (has-errors)
    format("\n********* Warning!  Regression test failed! ***********\n");
  else
    format("All streams tests pass.\n");
  end if;
end method main;
