module: regression-test
author: Nick Kramer (nkramer@cs.cmu.edu)
copyright:  Copyright (C) 1994, Carnegie Mellon University.
            All rights reserved.
rcs-header: $Header: /home/housel/work/rcs/gd/src/tests/regexp-test.dylan,v 1.2 1994/09/11 11:29:34 nkramer Exp $

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
end module regression-test;


define method main (#rest ignored)
  positioner-test(#t);
  replace-test();
  translate-test();
  split-test();
  substring-search-test();
  case-insensitive-equal-test();
  join-test();
end method main;

define method join-test ();
  print("Join test");
  if (join(", ", "dirty word", "clean word", "computer word", "spanish word")
	~= "dirty word, clean word, computer word, spanish word")
    print("Failed join test");
    print(join(", ", "dirty word", "clean word", "computer word", "spanish word"));
  end if;
  if (join(", ") ~= "")
    print("Failed join test #2");
  end if;
end method join-test;


// Try all 256x256 combinations to make sure our fancy definition of
// case-insensitive-equal works.
//
define method case-insensitive-equal-test ()
  print("Case insensitive equal");
  for (c1 = as(<character>, 0) then successor(c1), 
       until c1 = as(<character>, 255))
    for (c2 = as(<character>, 0) then successor(c2), 
	 until c2 = as(<character>, 255))
      if (case-insensitive-equal(c1, c2)
	    ~= (as-lowercase(c1) = as-lowercase(c2)))
	format("Failed! : c1=%c (%d), c2=%c (%d)\n", c1, as(<integer>, c1),
	       c2, as(<integer>, c2));
      end if;
    end for;
  end for;
end method case-insensitive-equal-test;


define method substring-search-test ()
  let big-string = "The rain in spain and some other text";
  print("substring-search");
  let test1 = substring-position(big-string, "spain");
  if (test1 ~= 12)
    format("Failed test 1: %=\n", test1);
  end if;
  print("make-substring-positioner");
  let positioner = make-substring-positioner("spain");
  let test2 = positioner(big-string);
  if (test2 ~= 12)
    format("Failed test 2: %=\n", test1);
  end if;
  print("substring-replace");
  let test3 = substring-replace(big-string, "spain", "Pittsburgh");
  if (test3 ~= "The rain in Pittsburgh and some other text")
    format("Failed test 3: %=\n", test3);
  end if;
  print("make-substring-replacer");
  let replacer = make-substring-replacer("spain");
  let test4 = replacer(big-string, "Pittsburgh");
  if (test4 ~= "The rain in Pittsburgh and some other text")
    format("Failed test 4: %=\n", test1);
  end if;
end method substring-search-test;

define method replace-test ()
  let big-string = "The rain in spain and some other text";
  block ()
    print("regexp-replace");
    let test1 = regexp-replace(big-string, "the (.*) in (\\w*\\b)",
			       "\\2 has it's \\1");
    if (test1 ~= "spain has it's rain and some other text")
      format("Failed test 1: %s\n", test1);
    end if;
    let test2 = regexp-replace(big-string, "in", "out");
    if (test2 ~= "The raout out spaout and some other text")
      format("Failed test 2: %s\n", test2);
    end if;
    let test3 = regexp-replace(big-string, "in", "out", count: 2);
    if (test3 ~= "The raout out spain and some other text")
      format("Failed test 3: %s\n", test3);
    end if;
    let test4 = regexp-replace(big-string, "in", "out", start: 8, end: 15);
    if (test4 ~= "The rain out spain and some other text")
      format("Failed test 4: %s\n", test4);
    end if;
  end block;

  block ()
    print("Make-regexp-replacer");
    let replacer1 = make-regexp-replacer("the (.*) in (\\w*\\b)");

    let test1 = replacer1(big-string, "\\2 has it's \\1");
    if (test1 ~= "spain has it's rain and some other text")
      format("Failed test 1: %s\n", test1);
    end if;
    let replacer2 = make-regexp-replacer("the (.*) in ([\\w]*\\b)", 
					 replace-with: "\\2 has it's \\1");
    let test2 = replacer2(big-string);
    if (test2 ~= "spain has it's rain and some other text")
      format("Failed test 2: %s\n", test2);
    end if;
  end block;
end method replace-test;


define method translate-test ()
  let big-string = "The rain in spain and some other text";
  block ()
    print("translate");
    let test1 = translate(big-string, "a-z", "A-Z");
    if (test1 ~= "THE RAIN IN SPAIN AND SOME OTHER TEXT")
      format("Failed test 1: %s\n", test1);
    end if;
    let test2 = translate(big-string, "a-zA-Z", "A-Za-z");
    if (test2 ~= "tHE RAIN IN SPAIN AND SOME OTHER TEXT")
      format("Failed test 2: %s\n", test2);
    end if;
    let test3 = translate(big-string, "a-z ", "", delete: #t);
    if (test3 ~= "T")
      format("Failed test 3: %s\n", test3);
    end if;
    let test4 = translate(big-string, "a-z", "A-Z", start: 5, end: 10);
    if (test4 ~= "The rAIN In spain and some other text")
      format("Failed test 4: %s\n", test4);
    end if;
  end block;
  
  block ()
    print("make-translator");
    let translator1 = make-translator("a-z", "A-Z");
    let test1 = translator1(big-string);
    if (test1 ~= "THE RAIN IN SPAIN AND SOME OTHER TEXT")
      format("Failed test 1: %s\n", test1);
    end if;
    let translator2 = make-translator("a-zA-Z", "A-Za-z");
    let test2 = translator2(big-string);
    if (test2 ~= "tHE RAIN IN SPAIN AND SOME OTHER TEXT")
      format("Failed test 2: %s\n", test2);
    end if;
    let translator3 = make-translator("a-z ", "", delete: #t);
    let test3 = translator3(big-string);
    if (test3 ~= "T")
      format("Failed test 3: %s\n", test3);
    end if;
    let translator4 = make-translator("a-z", "A-Z");
    let test4 = translator4(big-string, start: 5, end: 10);
    if (test4 ~= "The rAIN In spain and some other text")
      format("Failed test 4: %s\n", test4);
    end if;
  end block;
end method translate-test;


define method split-test ()
  let big-string = "The rain in spain and some other text";
  block ()
    print("split");
    let (#rest test1) = split("\\s", big-string);
    if (test1 ~= #("The", "rain", "in", "spain", 
		   "and", "some", "other", "text"))
      format("Failed test 1: %=\n", test1);
    end if;
    let (#rest test2) = split("\\s", big-string, count: 3);
    if (test2 ~= #("The", "rain", "in spain and some other text"))
      format("Failed test 2: %=\n", test2);
    end if;
    let (#rest test3) = split("\\s", big-string, start: 12);
    if (test3 ~= #("The rain in spain", "and", "some", "other", "text"))
      format("Failed test 3: %=\n", test3);
    end if;
    let (#rest test4) = split("\\s", " Some   text with   lots of spaces  ", 
			      count: 3);
    if (test4 ~= #("Some", "text", "with   lots of spaces  "))
      format("Failed test 4: %=\n", test4);
    end if;
    let (#rest test5) = split("\\s", " Some   text with   lots of spaces  ", 
			      count: 3, remove-empty-items: #f);
    if (test5 ~= #("", "Some", "  text with   lots of spaces  "))
      format("Failed test 5: %=\n", test5);
    end if;
  end block;

  block ()
    print("make-splitter");
    let splitter1 = make-splitter("\\s");
    let (#rest test1) = splitter1(big-string);
    if (test1 ~= #("The", "rain", "in", "spain", 
		   "and", "some", "other", "text"))
      format("Failed test 1: %=\n", test1);
    end if;
    let splitter2 = make-splitter("\\s");
    let (#rest test2) = splitter2(big-string, count: 3);
    if (test2 ~= #("The", "rain", "in spain and some other text"))
      format("Failed test 2: %=\n", test2);
    end if;
    let splitter3 = make-splitter("\\s");
    let (#rest test3) = splitter3(big-string, start: 12);
    if (test3 ~= #("The rain in spain", "and", "some", "other", "text"))
      format("Failed test 3: %=\n", test3);
    end if;
    let splitter4 = make-splitter("\\s");
    let (#rest test4) = splitter4(" Some   text with   lots of spaces  ", 
			      count: 3);
    if (test4 ~= #("Some", "text", "with   lots of spaces  "))
      format("Failed test 4: %=\n", test4);
    end if;
    let splitter5 = make-splitter("\\s");
    let (#rest test5) = splitter5(" Some   text with   lots of spaces  ", 
			      count: 3, remove-empty-items: #f);
    if (test5 ~= #("", "Some", "  text with   lots of spaces  "))
      format("Failed test 5: %=\n", test5);
    end if;
  end block;
end method split-test;

// Currently this ignores whether it should produce output, but some day
// when I care I'll put that in
//
define method positioner-test (output?);
  let results = 
    vector(test-regexp("a*", "aaaaaaaaaa", #[0, 10]),
	   test-regexp("a*", "aaaaabaaaa", #[0, 5]),
	   test-regexp("ab*(cd|e)", "acd", #[0, 3, 1, 3]),
	   test-regexp("ab*(cd|e)", "abbbbe", #[0, 6, 5, 6]),
	   test-regexp("ab*(cd|e)", "ab", #f),

	   test-regexp("^a$", "aaaaaaaaaaaaaa", #f),
	   test-regexp("^a$", "a", #[0, 1]),
	   test-regexp("(^a$)|aba", "abba", #f),
	   test-regexp("(^a$)|aba", "aba", #[0, 3, #f, #f]),

  test-regexp("\\bthe rain (in){1,5} spain$", "the rain in spain", 
	      #[0, 17, 9, 11]),
  test-regexp("\\bthe rain (in){1,5} spain$", "the rain spain",
	      #f),
  test-regexp("\\bthe rain (in){1,5} spain$", "the rain ininin spain",
	      #[0, 21, 13, 15]),
  test-regexp("\\bthe rain (in){1,5} spain$", "fuck the rain in spain",
	      #[5, 22, 14, 16]),
  test-regexp("\\bthe rain (in){1,5} spain$", "the rain in spainland", #f),
  test-regexp("\\bthe rain (in){1,5} spain$", "blathe rain in spain", #f),
  test-regexp("\\bthe rain (in){1,5} spain$", "the rain ininininin spain",
	      #[0, 25, 17, 19]),
  test-regexp("\\bthe rain (in){1,5} spain$", "the rain inininininin spain",
	      #f),
  test-regexp("((a*)|(b*))*c", "aaabbabbacfuck", 
	      #[0, 10, 8, 9, 8, 9, 6, 8]),
                   // The real test is whether or not this terminates
  test-regexp("a*", "aaaaa", #[0, 5]),
  test-regexp("a*", "a", #[0, 1]),
  test-regexp("a*", "", #[0, 0]),
  test-regexp("bba*c", "bbc", #[0, 3]),
  test-regexp("a", "bbbb", #f),
  test-regexp("a*", "aaaaa", #[3, 4], start: 3, end: 4),
  test-regexp("^a*", "aaaaa", #[2, 5], start: 2),
  test-regexp("^a*", "baaaaa", #[2, 6], start: 2),
  test-regexp("^a+", "bbbaaaaa", #f, start: 2),
  test-regexp("a+", "AAaAA", #[0, 5]),
  test-regexp("a+", "AAaAA", #[2, 3], case-sensitive: #t),
  test-regexp("[a-f]+", "SdFbIeNvI", #[1, 4]),
  test-regexp("[a-f]+", "SdFbIeNvI", #[1, 2], case-sensitive: #t),
	   

	#t);

  print(results);
  if (~member?(#f, results))
    format("\nEverything passed.\n");
  else
    format("\n---------------\nWarning: Some tests failed.\n");
  end if;
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
  if (output) 
    format("%s on %s\n", regexp, input); 
    format("%= %= : should be %=", answer, marks, right-marks);
    format("---- %s\n\n", 
	   if (marks = right-marks | answer = right-marks)
	     "passed";
	   elseif (answer ~= #f & right-marks ~= #f)
	     "failed marks";
	   else
	     "failed both";
	   end if);
  end if;
  marks = right-marks | answer = right-marks;
end method test-regexp;


define method test-matcher (matcher :: <function>, input :: <string>, 
			    right-answer);
  let answer = matcher(input);
  format("Matcher on %s\n", input); 
  format("%=      ---- %s\n\n", answer, 
	 if (answer = right-answer)
	   "passed";
	 else
	   "failed both";
	 end if);
  answer = right-answer;
end method test-matcher;
