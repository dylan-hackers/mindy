module: regression-test
author: Nick Kramer (nkramer@cs.cmu.edu)
copyright:  Copyright (C) 1994, Carnegie Mellon University.
            All rights reserved.
synopsis: A regression test for the string-extensions library.
rcs-header: $Header: /home/housel/work/rcs/gd/src/tests/regexp-test.dylan,v 1.6 1994/11/09 22:41:44 nkramer Exp $

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
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

define library regression-test
  use dylan;
  use string-extensions;
end library regression-test;

define module regression-test
  use dylan;
  use extensions;            // need main
  use regular-expressions;
  use substring-search;
  use string-hacking;
  use cheap-io;
end module regression-test;

define variable has-errors = #f;

define method main (argv0, #rest ignored)
  format("Regression test for the string-extensions library.\n\n");
  positioner-test();
  replace-test();
  translate-test();
  split-test();
  substring-search-test();
//  case-insensitive-equal-test();   // Takes a really long time
  join-test();
  if (has-errors)
    format("\n********* Warning!  Regression test failed! ***********\n");
  else
    format("All tests pass.\n");
  end if;
end method main;

define method run-test (input, expected-result, test-name :: <string>)
 => passed? :: <boolean>;
  if (input ~= expected-result)
    has-errors := #t;
    format("Failed %s\n", test-name);
    format("     Got %=\n", input);
    format("     when we expected %=\n", expected-result);
    #f;
  else
    #t;
  end if;
end method run-test;

define method join-test ();
  format("join\n");
  run-test(join(", ", "dirty word", "clean word",
		"computer word", "spanish word"),
	   "dirty word, clean word, computer word, spanish word",
	   "Join test 1");
  run-test(join(", "), "", "Join test 2");
end method join-test;


// Try all 256x256 combinations to make sure our fancy definition of
// case-insensitive-equal works.
//
define method case-insensitive-equal-test ()
  format("Case insensitive equal\n");
  for (c1 = as(<character>, 0) then successor(c1), 
       until c1 = as(<character>, 255))
    for (c2 = as(<character>, 0) then successor(c2), 
	 until c2 = as(<character>, 255))
      run-test(case-insensitive-equal(c1, c2), 
	       as-lowercase(c1) = as-lowercase(c2),
	       "Case-insensitive equal");
    end for;
  end for;
end method case-insensitive-equal-test;


define method substring-search-test ()
  let big-string = "The rain in spain and some other text";
  run-test(substring-position(big-string, "spain"), 12, "substring-position");
  let positioner = make-substring-positioner("spain");
  run-test(positioner(big-string), 12, "make-substring-positioner");
  run-test(substring-replace(big-string, "spain", "Pittsburgh"),
	   "The rain in Pittsburgh and some other text",
	   "substring-replace");
  let replacer = make-substring-replacer("spain");
  run-test(replacer(big-string, "Pittsburgh"),
	   "The rain in Pittsburgh and some other text",
	   "make-substring-replacer");   
end method substring-search-test;


define method replace-test ()
  format("regexp-replace\n");
  let big-string = "The rain in spain and some other text";
  run-test(regexp-replace(big-string, "the (.*) in (\\w*\\b)",
			  "\\2 has it's \\1"),
	   "spain has it's rain and some other text",
	   "regexp-replace #1");
  run-test(regexp-replace(big-string, "in", "out"),
	   "The raout out spaout and some other text",
	   "regexp-replace #2");
  run-test(regexp-replace(big-string, "in", "out", count: 2),
	   "The raout out spain and some other text",
	   "regexp-replace #3");
  run-test(regexp-replace(big-string, "in", "out", start: 8, end: 15),
	   "The rain out spain and some other text",
	   "regexp-replace #4");

  format("Make-regexp-replacer\n");
  let replacer1 = make-regexp-replacer("the (.*) in (\\w*\\b)");
  run-test(replacer1(big-string, "\\2 has it's \\1"),
	   "spain has it's rain and some other text",
	   "make-regexp-replacer #1");
  let replacer2 = make-regexp-replacer("the (.*) in ([\\w]*\\b)", 
				       replace-with: "\\2 has it's \\1");
  run-test(replacer2(big-string),
	   "spain has it's rain and some other text",
	   "make-regexp-replacer #2");
end method replace-test;


define method translate-test ()
  let big-string = "The rain in spain and some other text";
  format("translate\n");
  run-test(translate(big-string, "a-z", "A-Z"),
	   "THE RAIN IN SPAIN AND SOME OTHER TEXT",
	   "translate test 1");
  run-test(translate(big-string, "a-zA-Z", "A-Za-z"),
	   "tHE RAIN IN SPAIN AND SOME OTHER TEXT",
	   "translate test 2");
  run-test(translate(big-string, "a-z ", "", delete: #t),
	   "T", 
	   "translate test 3");
  run-test(translate(big-string, "a-z", "A-Z", start: 5, end: 10),
	   "The rAIN In spain and some other text",
	   "translate test 4");
  
  format("make-translator\n");
  let translator1 = make-translator("a-z", "A-Z");
  run-test(translator1(big-string),
	   "THE RAIN IN SPAIN AND SOME OTHER TEXT",
	   "make-translator #1");
  let translator2 = make-translator("a-zA-Z", "A-Za-z");
  run-test(translator2(big-string),
	   "tHE RAIN IN SPAIN AND SOME OTHER TEXT",
	   "make-translator #2");
  let translator3 = make-translator("a-z ", "", delete: #t);
  run-test(translator3(big-string),
	   "T",
	   "make-translator #3");
  let translator4 = make-translator("a-z", "A-Z");
  run-test(translator4(big-string, start: 5, end: 10),
	   "The rAIN In spain and some other text",
	   "make-translator #4");
end method translate-test;


define method split-test ()
  let big-string = "The rain in spain and some other text";
  block ()
    format("split\n");
    let (#rest test1) = split("\\s", big-string);
    run-test(test1, 
	     #("The", "rain", "in", "spain", "and", "some", "other", "text"),
	     "split #1");
    let (#rest test2) = split("\\s", big-string, count: 3);
    run-test(test2, #("The", "rain", "in spain and some other text"),
	     "split #2");
    let (#rest test3) = split("\\s", big-string, start: 12);
    run-test(test3, #("The rain in spain", "and", "some", "other", "text"),
	     "split #3");
    let (#rest test4) = split("\\s", " Some   text with   lots of spaces  ", 
			      count: 3);
    run-test(test4, #("Some", "text", "with   lots of spaces  "),
	     "split #4");
    let (#rest test5) = split("\\s", " Some   text with   lots of spaces  ", 
			      count: 3, remove-empty-items: #f);
    run-test(test5, #("", "Some", "  text with   lots of spaces  "),
	     "split #5");
  end block;

  block ()
    format("make-splitter\n");
    let splitter1 = make-splitter("\\s");
    let (#rest test1) = splitter1(big-string);
    run-test(test1, 
	     #("The", "rain", "in", "spain", "and", "some", "other", "text"),
	     "make-splitter #1");
    let splitter2 = make-splitter("\\s");
    let (#rest test2) = splitter2(big-string, count: 3);
    run-test(test2, #("The", "rain", "in spain and some other text"),
	     "make-splitter #2");
    let splitter3 = make-splitter("\\s");
    let (#rest test3) = splitter3(big-string, start: 12);
    run-test(test3, #("The rain in spain", "and", "some", "other", "text"),
	     "make-splitter #3");
    let splitter4 = make-splitter("\\s");
    let (#rest test4) = splitter4(" Some   text with   lots of spaces  ", 
			      count: 3);
    run-test(test4, #("Some", "text", "with   lots of spaces  "),
	     "make-splitter #4");
    let splitter5 = make-splitter("\\s");
    let (#rest test5) = splitter5(" Some   text with   lots of spaces  ", 
			      count: 3, remove-empty-items: #f);
    run-test(test5, #("", "Some", "  text with   lots of spaces  "),
	     "make-splitter #5");
  end block;
end method split-test;

define method positioner-test ();
  format("regexp-positioner\n");
  test-regexp("a*", "aaaaaaaaaa", #[0, 10]);
  test-regexp("a*", "aaaaabaaaa", #[0, 5]);
  test-regexp("ab*(cd|e)", "acd", #[0, 3, 1, 3]);
  test-regexp("ab*(cd|e)", "abbbbe", #[0, 6, 5, 6]);
  test-regexp("ab*(cd|e)", "ab", #f);
    
  test-regexp("^a$", "aaaaaaaaaaaaaa", #f);
  test-regexp("^a$", "a", #[0, 1]);
  test-regexp("(^a$)|aba", "abba", #f);
  test-regexp("(^a$)|aba", "aba", #[0, 3, #f, #f]);

  test-regexp("\\bthe rain (in){1,5} spain$", "the rain in spain", 
	      #[0, 17, 9, 11]);
  test-regexp("\\bthe rain (in){1,5} spain$", "the rain spain",
	      #f);
  test-regexp("\\bthe rain (in){1,5} spain$", "the rain ininin spain",
	      #[0, 21, 13, 15]);
  test-regexp("\\bthe rain (in){1,5} spain$", "bork the rain in spain",
	      #[5, 22, 14, 16]);
  test-regexp("\\bthe rain (in){1,5} spain$", "the rain in spainland", #f);
  test-regexp("\\bthe rain (in){1,5} spain$", "blathe rain in spain", #f);
  test-regexp("\\bthe rain (in){1,5} spain$", "the rain ininininin spain",
	      #[0, 25, 17, 19]);
  test-regexp("\\bthe rain (in){1,5} spain$", "the rain inininininin spain",
	      #f);
  test-regexp("((a*)|(b*))*c", "aaabbabbacbork", 
	      #[0, 10, 8, 9, 8, 9, 6, 8]);
                   // The real test is whether or not this terminates
  test-regexp("a*", "aaaaa", #[0, 5]);
  test-regexp("a*", "a", #[0, 1]);
  test-regexp("a*", "", #[0, 0]);
  test-regexp("bba*c", "bbc", #[0, 3]);
  test-regexp("a", "bbbb", #f);
  test-regexp("a*", "aaaaa", #[3, 4], start: 3, end: 4);
  test-regexp("^a*", "aaaaa", #[2, 5], start: 2);
  test-regexp("^a*", "baaaaa", #[2, 6], start: 2);
  test-regexp("^a+", "bbbaaaaa", #f, start: 2);
  test-regexp("a+", "AAaAA", #[0, 5]);
  test-regexp("a+", "AAaAA", #[2, 3], case-sensitive: #t);
  test-regexp("[a-f]+", "SdFbIeNvI", #[1, 4]);
  test-regexp("[a-f]+", "SdFbIeNvI", #[1, 2], case-sensitive: #t);
end method positioner-test;


define method test-regexp(regexp :: <string>, input :: <string>,
			  right-marks, #key output: output = #t,
			  start: start, end: input-end = #f,
			  case-sensitive: case? = #f)
    => passed? :: <boolean>;
  let (#rest marks) = if (~start & ~input-end) 
			regexp-position(input, regexp, case-sensitive: case?);
		      elseif (start & input-end)
			regexp-position(input, regexp, case-sensitive: case?, 
					start: start, end: input-end);
		      elseif (start)
			regexp-position(input, regexp, case-sensitive: case?,
					start: start);
		      else
			regexp-position(input, regexp, case-sensitive: case?,
					end: input-end);
		      end if;
  let answer = marks[0];
  if (marks = right-marks | answer = right-marks)
    #f;   // do nothing
  elseif (answer ~= #f & right-marks ~= #f)
    format("Failed marks: regexp-position on\n");
    format("     regexp=%=, big=%=\n", regexp, input);
    format("     returned %=\n", marks);
  else
    format("Failed both: regexp-position on\n");
    format("     regexp=%=, big=%=\n", regexp, input);
    format("     returned %=\n", marks);
  end if;
end method test-regexp;
