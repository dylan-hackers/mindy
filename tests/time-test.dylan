module: time-test
author: David Watson, Nick Kramer
synopsis: Test for the time library.
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
    format("Failed %s!\n", test-name);
    format("     Got %=\n", input);
    format("     when we expected %=\n", expected-result);
    #f;
  else
    #t;
  end if;
end method run-test;

define method \=(a :: <decoded-time>, b :: <decoded-time>)
 => (result :: <boolean>);
  (a.seconds = b.seconds
     & a.minutes = b.minutes
     & a.hours = b.hours
     & a.day-of-week = b.day-of-week
     & a.day-of-month = b.day-of-month
     & a.month = b.month
     & a.year = b.year
     & a.timezone = b.timezone)
end method;
     
define method time-test () => ();
  let universal-time ::  <universal-time> = get-universal-time();
  let decoded-time :: <decoded-time> = get-decoded-time();

  let old-u-time :: <decoded-time> = decode-time(universal-time);
  let old-d-time :: <universal-time> = encode-time(decoded-time);

  let new-u-time :: <universal-time> = encode-time(old-u-time);
  let new-d-time :: <decoded-time> = decode-time(old-d-time);

  run-test(new-u-time, universal-time, "universal-time");
  run-test(new-d-time, decoded-time, "decoded-time");
end method time-test;

define method time-io-test () => ();
  let decoded-time
    = parse-time(make(<string-stream>, contents: "19 June 1996, 12:34 pm"),
		 "%d %B %Y, %I:%M %p");

  // Now, we make up a value for seconds.  We never print it, but
  // format-time demands that it be specified because it wants to
  // convert the whole <decoded-time> into a C struct tm.
  let decoded-time = make(<decoded-time>, default-from: decoded-time, 
			  seconds: 0);

  let string-stream = make(<buffered-byte-string-output-stream>);
  format-time(string-stream, "%A %d %B %Y, %I:%M %p", decoded-time);
  let string = string-stream.stream-contents;

  run-test(string, "Wednesday 19 June 1996, 12:34 PM", "time-io");
end method time-io-test;

define method main (argv0, #rest ignored)
  format("\nRegression test for the time library.\n\n");
  run-several-tests("time", time-test);
  run-several-tests("time-io", time-io-test);
  if (has-errors)
    format("\n********* Warning!  Regression test failed! ***********\n");
  else
    format("All time tests pass.\n");
  end if;
  force-output(*standard-output*);
end method main;
