Module: dylan-test
author: Roger Critchlow (rec@elf.org)
synopsis: A regression test for core Dylan.

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
//
// This program runs a set of simple minded tests past the compiler
// and interpreter.  Lots more tests could be added, obviously, but
// even these few have turned up some problems.  A large block are copied
// from the DIRM examples.
//

define constant buggy? = #f;		// not bugs, features!

define constant tautologies =
  #(#"booleans",
    #"comparisons",
    #"numbers",
    #"characters",
    #"symbols",
    #"collections",
#if (~mindy)
    #"limited collections",  // Mindy doesn't support limited collections
#endif
    #"sequences",
    #"arrays",
    #"deques",
    #"lists",
    #"ranges",
    #"stretchy vectors",
    #"strings",
    #"tables",
    #"vectors");

define method tautology(arg == #"booleans")
  (#t)				| signal("#t is not true!\n");
  (#T)				| signal("#T is not true!\n");
  (#f)				& signal("#f is not false!\n");
  (#F)				& signal("#F is not false!\n");
  (#t & #t)			| signal("#t & #t is not true!\n");
  (#t | #t)			| signal("#t | #t is not true!\n");
  (#f & #f)			& signal("#f & #f is not false!\n");
  (#f | #f)			& signal("#f | #f is not false!\n");
  (#t & #f)			& signal("#t & #f is not false!\n");
  (#t | #f)			| signal("#t | #f is not true!\n");
  (#f & #t)			& signal("#f & #t is not false!\n");
  (#f | #t)			| signal("#f | #t is not true!\n");
end method;

define method tautology(arg == #"comparisons")
  (1 = 1)			| signal("1 is not equal to 1!\n");
  (1 == 1)			| signal("1 is not really equal to 1!\n");
  (1 ~= 1)			& signal("1 is not equal to 1!\n");
  (1 < 2)			| signal("1 is not less than 2!\n");
  (1 >= 2)			& signal("1 is greater than or equal to 2!\n");
  (1 <= 2)			| signal("1 is not less than or equal to 2!\n");
  (1 > 2)			& signal("1 is greater than 2!\n");
  ('a' < 'b')			| signal("'a' is greater than 'b'!\n");
  ("A" < "B")			| signal("\"A\" is greater than \"B\"!\n");
end method;

define method tautology(arg == #"numbers")
  instance?(1, <number>)		| signal("1 is not a <number>");
  instance?(1, <real>)			| signal("1 is not a <real>!\n");
  instance?(1, <float>)			& signal("1 is a <float>!\n");
  instance?(1, <single-float>)		& signal("1 is a <single-float>!\n");
  instance?(1, <double-float>)		& signal("1 is a <double-float>!\n");
  instance?(1, <extended-float>)	& signal("1 is a <extended-float>!\n");
  instance?(1, <rational>)		| signal("1 is not a <rational>!\n");
  instance?(1, <integer>)		| signal("1 is not a <integer>!\n");
  instance?(1, <complex>)		| signal("1 is not a <complex>!\n");
  instance?(1.0, <number>)		| signal("1.0 is not a <number>");
  instance?(1.0, <real>)		| signal("1.0 is not a <real>!\n");
  instance?(1.0, <float>)		| signal("1.0 is not a <float>!\n");
  instance?(1.0s0, <single-float>)	| signal("1.0s0 is not a <single-float>!\n");
  instance?(1.0d0, <double-float>)	| signal("1.0d0 is not a <double-float>!\n");
  instance?(1.0x0, <extended-float>)	| signal("1.0x0 is not a <extended-float>!\n");
  instance?(1.0, <rational>)		& signal("1.0 is a <rational>!\n");
  instance?(1.0, <integer>)		& signal("1.0 is a <integer>!\n");
  instance?(1.0, <complex>)		| signal("1.0 is not a <complex>!\n");
  odd?(1)				| signal("1 is not odd!\n");
  even?(2)				| signal("2 is not even!\n");
  zero?(0)				| signal("0 is not zero!\n");
#if (mindy)
  positive?(+1)				| signal("+1 is not positive!\n");
#else
  positive?(1)				| signal("+1 is not positive!\n");
#endif
  negative?(-1)				| signal("-1 is not negative!\n");
#if (mindy)
  integral?(+1)				| signal("+1 is not integral!\n");
#else
  integral?(1)				| signal("+1 is not integral!\n");
#endif
  integral?(0)				| signal("0 is not integral!\n");
  integral?(-1) 			| signal("-1 is not integral!\n");
  (1 + 1 = 2)				| signal("1 + 1 is not 2!\n");
  (2 * 2 = 4)				| signal("2 * 2 is not 4!\n");
  (1 - 1 = 0)				| signal("1 - 1 is not 0!\n");
  (4.0 / 2.0 = 2.0)			| signal("4 / 2 is not 2!\n");
  (negative(1) = -1)			| signal("negative(1) is not -1!\n");
  (floor(3.14) = 3)			| signal("floor(3.14) is not 3 but %=\n", floor(3.14));
  (ceiling(3.14) = 4)			| signal("ceiling(3.14) is not 4 but %=\n", ceiling(3.14));
  (round(3.14) = 3)			| signal("round(3.14) is not 3 but %=!\n", round(3.14));
  (truncate(3.14) = 3)			| signal("truncate(3.14) is not 3 but %=!\n", truncate(3.14));
  //floor/
  //ceiling/
  //round/
  //truncate/
  //modulo
  //remainder

  (abs(1) = 1)				| signal("abs(1) is not 1!: it's %=\n", abs(1));
  (abs(-1) = 1)				| signal("abs(-1) is not 1!: it's %=\n", abs(-1));
  (logior(1,2) = 3)			| signal("logior(1,2) is not 3!: it's %=\n", logior(1,2));
  (logxor(1,3) = 2)			| signal("logxor(1,3) is not 2!: it's %=\n", logxor(1,3));
  (logand(1,3) = 1)			| signal("logand(1,3) is not 1!: it's %=\n", logand(1,3));
#if (mindy)
  (lognot(#x1234) = #xffffedcb)		| signal("lognot(#x1234) is not #xffffedcb!: it's %x\n", lognot(#x1234));
#else
  (lognot(#x1234) = -#x1235)		| signal("lognot(#x1234) is not #x-1235!: it's %x\n", lognot(#x1234));
#endif
  logbit?(15,#x8000) 			| signal("logbit?(15,#x8000) is not true!\n");
  (ash(1,3) = 8)			| signal("ash(1,3) is not 8!: it's %=\n", ash(1,3));
  (lcm(6,8) = 24)			| signal("lcm(6,8) is not 24!: it's %=\n", lcm(6,8));
  (gcd(6,8) = 2)			| signal("gcd(6,8) is not 2!: it's %=\n", gcd(6,8));
  (min(1,2) = 1)			| signal("min(1,2) is not 1!: it's %=\n", min(1,2));
  (max(1,2) = 2)			| signal("max(1,2) is not 2!: it's %=\n", max(1,2));
  if (buggy?)
    // NB - rationals may not be part of the language
    instance?(1, <ratio>)
      | signal("1 is not a <ratio>!\n");
    // Unbound variable: <ratio>
    instance?(1.0, <ratio>)
      & signal("1.0 is a <ratio>!\n");
    // Unbound variable: <ratio>
    (4 / 2 = 2)
      | signal("4 / 2 is not 2!\n");
    // No applicable methods for / with arguments #[4, 2]
//    format("rationalize(1,2) is %=\n", rationalize(1,2));
    // Unbound variable: rationalize
//    format("numerator(rationalize(1,2)) is %=\n", numerator(rationalize(1,2)));
    // Unbound variable: numerator
//    format("denominator(rationalize(1,2)) is %=\n", denominator(rationalize(1,2)));
    // Unbound variable: denominator
  end;
end method;

define method tautology(arg == #"characters")
  instance?('a', <character>)		| signal("'a' is not a <character>!\n");
  (as-uppercase('a') = 'A')		| signal("as-uppercase('a') is not 'A'!\n");
  (as-lowercase('A') = 'a')		| signal("as-lowercase('A') is not 'a'!\n");
  (as(<integer>, ' ') = 32)		| signal("as(<integer>, ' ') is not 32!\n");
  (as(<character>, 32) = ' ')		| signal("as(<character>, 32) is not ' '!\n");
end method;

define method tautology(arg == #"symbols")
  instance?(#"foo", <symbol>)		| signal("instance?(#\"foo\", <symbol>) is false!\n");
  instance?(#"foo", <symbol>)		| signal("instance?(foo:, <symbol>) is false!\n");
#if (mindy)
  (#"foo" = #"FOO")			| signal("#\"foo\" is not FOO:!\n");
#endif
  (as(<symbol>, "FOO") = #"foo")	| signal("as(<symbol>, \"FOO\") is not foo:!\n");
  (as(<string>, #"Foo") = "foo")	| signal("as(<string>, Foo:) is not \"foo\"! It's %=\n",
						 as(<string>, Foo:));
end method;

define method tautology(arg == #"collections")
  (size(#()) = 0)
    | signal("size(#()) is not zero!\n");
  (size(#[]) = 0)
    | signal("size(#[]) is not zero!\n");
  empty?(#())
    | signal("#() is not empty!\n");
  empty?(#[])
    | signal("#[] is not empty!\n");
  (size(list()) == 0)
    | signal("size(list()) is not zero!\n");
  (size(vector()) == 0)
    | signal("size(vector()) is not zero!\n");
  empty?(list())
    | signal("list() is not empty!\n");
  empty?(vector())
    | signal("vector() is not empty!\n");
  do(\+, #(1,2), #(3, 2))
    & signal("do returned #t!\n");
  (map(\+, #(100, 100, 200, 200), #(1, 2, 3, 4)) = #(101, 102, 203, 204))
    | signal("map(\\+, #(100, 100, 200, 200), #(1, 2, 3, 4)) is not #(101, 102, 203, 204)! It's %=\n",
	     map(\+, #(100, 100, 200, 200), #(1, 2, 3, 4)));
  (map(\+, #(1,2), #(3,2)) = #(4,4))
    | signal("map(\\+, #(1,2), #(3,2)) is not equal to #(4,4)! It's %=\n",
	     map(\+, #(1,2), #(3,2)));
  (map-as(<vector>, \+, #(1,2), #(3,2)) = #[4,4])
    | signal("map-as(<vector>, \\+, #(1,2), #(3,2)) is not equal to #[4,4]! It's %=\n",
	     map-as(<vector>, \+, #(1,2), #(3,2)));
  // NB - the DIRM example for map-into is in error.
  let x = list(100, 100, 200, 200);
  (map-into(x, \+, #(100, 100, 200, 200), #(1, 2, 3, 4)) = #(101, 102, 203, 204))
    | signal("map-into (x, \\+, #(100, 100, 200, 200), #(1, 2, 3, 4)) is not equal to #(101, 102, 203, 204)! It's %=\n",
	     map-into (x, \+, x, #(1, 2, 3, 4)));
  (x = #(101, 102, 203, 204))
    | signal("map-into'ed x is not equal to #(101, 102, 203, 204)! It's %=\n", x);
  any?(\>, #(1,2,3,4), #(5,4,3,2))
    | signal("any?(\\>, #(1,2,3,4), #(5,4,3,2)) is not true!\n");
  any?(even?, #(1, 3, 5, 7))
    & signal("any?(even?, #(1, 3, 5, 7)) is not false!\n");
  every?(\>, #(1,2,3,4), #(5,4,3,2))
    & signal("every?(\\>, #(1,2,3,4), #(5,4,3,2)) is true!\n");
  every? (odd?, #(1, 3, 5, 7))
    | signal("every? (odd?, #(1, 3, 5, 7)) is false!\n");
  let high-score = 10;
  (reduce (max, high-score, #(3, 1, 4, 1, 5, 9)) = 10)
    | signal("reduce(max, high-score, #(3, 1, 4, 1, 5, 9)) is not 10! It's %=\n",
	     reduce(max, high-score, #(3, 1, 4, 1, 5, 9)));
  (reduce(max, high-score, #(3, 12, 9, 8, 8, 6)) = 12)
    | signal(" reduce(max, high-score, #(3, 12, 9, 8, 8, 6)) is not 12! It's %=\n",
	     reduce(max, high-score, #(3, 12, 9, 8, 8, 6)));
  (reduce1(\+, #(1, 2, 3, 4, 5)) = 15)
    | signal("reduce1(\\+, #(1, 2, 3, 4, 5)) is not 15! It's %=\n",
	     reduce1(\+, #(1, 2, 3, 4, 5)));
  let flavors = #(#"vanilla", #"pistachio", #"ginger");
  member?(#"vanilla", flavors)
    | signal("member?(#\"vanilla\", flavors) is false!\n");
  member?(#"banana", flavors)
    & signal("member?(#\"banana\", flavors) is true!\n");
  local method has-nuts?(flavor) member?(flavor, #(#"pistachio")) end;
  (find-key(flavors, has-nuts?) = 1)
    | signal("find-key(flavors, has-nuts?) is not 1! It's %=\n",
	   find-key(flavors, has-nuts?));
  local method double(n) 2 * n end;
  let numbers = list (10, 13, 16, 19);
  (replace-elements!(numbers, odd?, double) = #(10, 26, 16, 38))
    | signal("replace-elements!(numbers, odd?, double) is not #(10, 26, 16, 38)! It's %=\n",
	     replace-elements!(numbers, odd?, double));
  (fill!(numbers, 3, start: 2) = #(10, 26, 3, 3))
    | signal("fill! (numbers, 3, start: 2) is not #(10, 26, 3, 3)!  It's %=\n",
	     fill! (numbers, 3, start: 2));
  key-test(list())
    | signal("no key-test for list()!\n");
  key-test(vector())
    | signal("no key-test for vector()!\n");
end method;

#if (~mindy)

// Mindy doesn't support limited collections

define constant <type1> = limited(<vector>, of: <byte-character>);
define constant <type2> = limited(<vector>, of: <byte-character>, size: 3);
define constant <type3> = limited(<vector>, of: <integer>);
define constant <type4> = limited(<stretchy-vector>, of: <byte-character>);
define limited-collection <int-vector> (<vector>) of <integer> = 0;
define limited-collection <stretchy-chars> (<stretchy-vector>)
  of <byte-character> = ' ';
define class <stretchy-byte-string> (<stretchy-chars>, <string>)
end class <stretchy-byte-string>;

define method test-limited-coll (coll :: <type1>)
  <type1>;
end method test-limited-coll;

define method test-limited-coll (coll :: <type2>)
  <type2>;
end method test-limited-coll;

define method test-limited-coll (coll :: <type3>)
  <type3>;
end method test-limited-coll;

define method test-limited-int (int :: limited(<integer>, min: 0, max: 3))
  "foo";
end method;

define method test-limited-int (int :: limited(<integer>, min: 5, max: 10))
  "bar";
end method;

define method tautology(arg == #"limited collections")
  let int-vec = as(<int-vector>, #[5, 3, 2]);
  (test-limited-int(8) = "bar"
    | signal("function-dispacth on 8 yields: %=\n", test-limited-int(8)));
  (instance?("foo", <type1>)
     | signal("instance?(\"foo\", %=) is false!\n", <type1>));
  (instance?("foo", <type2>)
     | signal("instance?(\"foo\", %=) is false!\n", <type2>));
  (instance?("foobar", <type2>)
     & signal("instance?(\"foobar\", %=) is true!\n", <type2>));
  (instance?("foo", <type3>)
     & signal("instance?(\"foo\", %=) is true!\n", <type3>));
  (instance?(int-vec, <type3>)
     | signal("instance?(%=, %=) is false!\n", int-vec, <type3>));
  (instance?(int-vec, <type1>)
     & signal("instance?(%=, %=) is true!\n", int-vec, <type1>));
  (subtype?(<type2>, <type1>)
     | signal("subtype?(%=, %=) is false!\n", <type2>, <type1>));
  (subtype?(<type1>, <type2>)
     & signal("subtype?(%=, %=) is true!\n", <type1>, <type2>));
  (subtype?(<type3>, <type1>)
     & signal("subtype?(%=, %=) is true!\n", <type3>, <type1>));
  (test-limited-coll("foo") ~== <type2>
     & signal("function dispatch on \"foo\" yields %=\n",
	      test-limited-coll("foo")));
  (test-limited-coll("foobar") ~== <type1>
     & signal("function dispatch on \"foobar\" yields %=\n",
	      test-limited-coll("foobar")));
  let stretchy :: <stretchy-byte-string> = as(<stretchy-byte-string>, "foo");
  (instance?(stretchy, <type1>)
     | signal("instance?(%=,  %=) is false!\n", stretchy, <type1>));
  // Stretchy collections can't match a "size" restriction
  (instance?(stretchy, <type2>)
     & signal("instance?(%=, %=) is not false!\n", stretchy, <type2>));
  add!(stretchy, 'd');
  (stretchy = "food" | signal("%d ~= \"food\"\n"));
  (instance?(stretchy, <type2>)
     & signal("instance?(%=, %=) is true!\n", stretchy, <type2>));
  (instance?(stretchy, <type4>)
     | signal("instance?(%=, %=) is false!\n", stretchy, <type4>));
end method tautology;

#endif

define method tautology(arg == #"sequences")
  let numbers = #(3, 4, 5);
  (add(numbers, 1) = #(1, 3, 4, 5))
    | signal("add(numbers, 1) is not #(1, 3, 4, 5))!  It's %=\n", add(numbers, 1));
  let numbers = list (3, 4, 5);
  (add!(numbers, 1) = #(1, 3, 4, 5))
    | signal("add!(numbers, 1) is not #(1, 3, 4, 5))!  It's %=\n", add!(numbers, 1));
  (add-new(#(3, 4, 5), 1) = #(1, 3, 4, 5))
    | signal("add-new (#(3, 4, 5), 1) is not #(1, 3, 4, 5)!  It's %=\n", add-new (#(3, 4, 5), 1));
  (add-new(#(3, 4, 5), 4) = #(3, 4, 5))
    | signal("add-new (#(3, 4, 5), 4) is not #(3, 4, 5)!  It's %=\n", add-new (#(3, 4, 5), 4));
  (add-new!(list (3, 4, 5), 1) = #(1, 3, 4, 5))
    | signal("add-new! (list (3, 4, 5), 1) is not #(1, 3, 4, 5)!  It's %=\n", add-new! (list (3, 4, 5), 1));
  (remove(#(3, 1, 4, 1, 5, 9), 1) = #(3, 4, 5, 9))
    | signal("remove (#(3, 1, 4, 1, 5, 9), 1) is not #(3, 4, 5, 9)! It's %=\n", remove (#(3, 1, 4, 1, 5, 9), 1));
  (remove!(list(3, 1, 4, 1, 5, 9), 1) = #(3, 4, 5, 9))
    | signal("remove! (list(3, 1, 4, 1, 5, 9), 1) is not #(3, 4, 5, 9)! It's %=\n", remove! (list(3, 1, 4, 1, 5, 9), 1));
  (choose(even?, #(3, 1, 4, 1, 5, 9)) = #(4))
    | signal("choose (even?, #(3, 1, 4, 1, 5, 9)) is not #(4)!  It's %=\n", choose (even?, #(3, 1, 4, 1, 5, 9)));
  (choose-by(even?, range (from: 1), #("a", "b", "c", "d", "e", "f", "g", "h", "i")) =  #("b", "d", "f", "h"))
    | signal("choose-by(even?, range (from: 1), #(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\", \"g\", \"h\", \"i\")) is %=!\n",
	     choose-by(even?, range (from: 1), #("a", "b", "c", "d", "e", "f", "g", "h", "i")));
  let b = #("john", "paul", "george", "ringo");
  let c = #("richard", "george", "edward", "charles");
  (intersection(b, c, test: \=) = #("george"))
    | signal("intersection(b, c, test: \\=) is not #(\"george\")!  It's %=\n", intersection (b, c, test: \=));
  let a = #("butter", "flour", "sugar", "salt", "eggs");
  let b = #("eggs", "butter", "mushrooms", "onions", "salt");
  let c = #("salt", "butter", "flour", "sugar", "eggs", "mushrooms", "onions");
  (sort(union(a, b, test: \=)) = sort(c))
    | signal("union(a, b, test: \\=) is c! It's %=\n", sort(union(a, b, test: \=)));
  let a = #("spam", "eggs", "spam", "sausage", "spam", "spam");
  let b = #("spam", "eggs", "sausage");
  (sort(remove-duplicates(a, test: \=)) = sort(b))
    | signal("remove-duplicates(a, test: \\=) is not b!  It's %=\n", sort(remove-duplicates(a, test: \=)));
  let a = list("spam", "eggs", "spam", "sausage", "spam", "spam");
  (sort(remove-duplicates!(a, test: \=)) = sort(b))
    | signal("remove-duplicates!(a, test: \\=) is not b!  It's %=\n", sort(remove-duplicates!(a, test: \=)));
  let hamlet = #("to", "be", "or", "not", "to", "be");
  (copy-sequence(hamlet) == hamlet)
    & signal("copy-sequence(hamlet) is identical to hamlet!\n");
  (copy-sequence(hamlet, start: 2, end: 4) = #("or", "not"))
    | signal("copy-sequence(hamlet, start: 2, end: 4) is not #(\"or\", \"not\")!  It's %=\n",
	     copy-sequence(hamlet, start: 2, end: 4));
  (concatenate-as(<string>, #('n', 'o', 'n'), #('f', 'a', 't')) = "nonfat")
    | signal("concatenate-as(<string>, #('n', 'o', 'n'), #('f', 'a', 't')) is not \"nonfat\"! It's %=\n",
	     concatenate-as(<string>, #('n', 'o', 'n'), #('f', 'a', 't')));
  (concatenate("low-", "calorie") = "low-calorie")
    | signal("concatenate(\"low-\", \"calorie\") is not \"low-calorie\"!  It's %=\n",
	     concatenate("low-", "calorie"));
  let x = list("a", "b", "c", "d", "e");
  let abcde = replace-subsequence!(x, #("x", "y", "z"), end: 1);
  (abcde = #("x", "y", "z", "b", "c", "d", "e"))
    | signal("abcde is not #(\"x\", \"y\", \"z\", \"b\", \"c\", \"d\", \"e\")!  It's %=\n", abcde);
  let x = #("bim", "bam", "boom");
  (reverse(x) = #("boom", "bam", "bim"))
    | signal("reverse(x) is not #(\"boom\", \"bam\", \"bim\")! It's %=\n", reverse(x));
  let y = reverse!(x);
  (y = #("boom", "bam", "bim"))
    | signal("reverse!(x) is not #(\"boom\", \"bam\", \"bim\")! It's %=\n", y);
  let numbers = #(3, 1, 4, 1, 5, 9);
  (sort(numbers) = #(1, 1, 3, 4, 5, 9))
    | signal("sort(numbers) is not #(1, 1, 3, 4, 5, 9)!  It's %=\n", sort(numbers));
  let y = sort!(numbers);
  (y = #(1, 1, 3, 4, 5, 9))
    | signal("sort!(numbers) is not #(1, 1, 3, 4, 5, 9)!  It's %=\n", y);
  let numbers = #(3, 1, 4, 1, 5, 9);
  (first(numbers) = 3)
    | signal("first(numbers) is not 3!  It's %=\n", first(numbers));
  (second(numbers) = 1)
    | signal("second(numbers) is not 1!  It's %=\n", second(numbers));
  (third(numbers) = 4)
    | signal("third(numbers) is not 4!  It's %=\n", third(numbers));
  (first-setter(1, numbers) = 1)
    | signal("first-setter(1, numbers) is not 1!  It's %=\n", first-setter(1, numbers));
  (second-setter(2, numbers) = 2)
    | signal("second-setter(2, numbers) is not 2!  It's %=\n", second-setter(2, numbers));
  (third-setter(3, numbers) = 3)
    | signal("third-setter(3, numbers) is not 3!  It's %=\n", third-setter(3, numbers));
  (last (#("emperor", "of", "china")) = "china")
    | signal("last (#(\"emperor\", \"of\", \"china\")) is not \"china\"!  It's %=\n",
	     last (#("emperor", "of", "china")));
  let my-list = list (1, 2, 3);
  (my-list = #(1, 2, 3))
    | signal("my-list is not #(1, 2, 3)!  It's %=\n", my-list);
  ((last (my-list) := 4) = 4)
    | signal("last(my-list) := 4 is not 4! It's %=\n", (last (my-list) := 4));
  (subsequence-position ("Ralph Waldo Emerson", "Waldo") = 6)
    | signal("subsequence-position (\"Ralph Waldo Emerson\", \"Waldo\") is not 6!  It's %=\n",
	     subsequence-position ("Ralph Waldo Emerson", "Waldo"));
  (#(1, 2, 3) = #[1, 2, 3])
    | signal("#(1, 2, 3) is not equal to #[1, 2, 3])!\n");
end method;

define method tautology(arg == #"arrays")
  let a = make(<array>, dimensions: #(4, 4));
  (dimensions (a) = #(4, 4))
    | signal("dimensions (a) are not #(4, 4)!  They're %=\n", dimensions (a));
  (size(a) = 16)
    | signal("size(a) is not 16!  It's %=\n", size(a));
  for (i from 0 below 4)
    for (j from 0 below 4)
      a[i,j] := i * 4 + j;
    end;
  end;
  (aref(a, 1, 1) = 5)
    | signal("aref(a, 1, 1) is not 5! It's %=\n", aref(a, 1, 1));
  (aref-setter(128, a, 1, 1) = 128)
    | signal("aref-setter(128, a, 1, 1) is not 128! It's %=\n", aref-setter(128, a, 1, 1));
  if (buggy?)
    (rank(a) = 2)
      | signal("rank(a) is not 2!  It's %=\n", rank(a));
    //  Unbound variable: rank
    (row-major-index(a, 1, 1) = 5)
      | signal("row-major-index (a, 1, 1) is not 5!  It's %=\n", row-major-index(a, 1, 1));
    //  Unbound variable: row-major-index
    (dimension(a, 1) = 4)
      | signal("dimension(a, 1) is not 4!  It's %=\n", dimension(a, 1));
    //  Unbound variable: dimension
  end;
end method;

define method tautology(arg == #"deques")
  let d = make(<deque>);
  for (i from 5 to 1 by -1)
    push(d, i);
  end;
  for (i from 6 to 10)
    push-last(d, i);
  end;
  let p = pop(d);
  (p = 1)
    | signal("first pop(d) is not 1!  It's %=\n", p);
  let p = pop-last(d);
  (p = 10)
    | signal("first pop-last(d) is not 10!  It's %=\n", p);
  if (buggy?)
    //  this should be the same as push() but is maybe push-last()
    d := add!(d, 1);
    let p = pop(d);
    (p = 1)
      | signal("second pop(d) is not 1!  It's %=\n", p);
    //  this fails with a message about scan!()
    d := remove!(d, 9);
    let p = pop-last(d);
    (p = 8)
      | signal("second pop-last(d) is not 8!  It's %=\n", p);
  end;
end method;

define method tautology(arg == #"lists")
  (pair(1, 2) = #(1 . 2))
    | signal("pair(1, 2) is not #(1 . 2)!  It's %=\n", pair(1, 2));
  (pair(1, #(2, 3, 4, 5)) = #(1, 2, 3, 4, 5))
    | signal("pair(1, #(2, 3, 4, 5)) is not #(1, 2, 3, 4, 5)!  It's %=\n",
	     pair(1, #(2, 3, 4, 5)));
  (list(1, 2, 3) = #(1, 2, 3))
    | signal("list(1, 2, 3) is not #(1, 2, 3)!  It's %=\n", list (1, 2, 3));
  (list(4 + 3, 4 - 3) =  #(7, 1))
    | signal("list(4 + 3, 4 - 3) is not #(7, 1)!  It's %=\n", list (4 + 3, 4 - 3));
  (head(#(4, 5, 6)) = 4)
    | signal("head(#(4, 5, 6)) is not 4!  It's %=\n", head(#(4, 5, 6)));
  (head(#()) = #())
    | signal("head(#()) is not #()!  It's %=\n", head (#()));
  (tail(#(4, 5, 6)) = #(5, 6))
    | signal("tail (#(4, 5, 6)) is not #(5, 6)!  It's %=\n", tail (#(4, 5, 6)));
  let x = list (4, 5, 6);
  ((head(x) := 9) = 9)
    | signal("(head(x) := 9) is not 9!\n");
  (x = #(9, 5, 6))
    | signal("x is not #(9, 5, 6)!  It's %=\n", x);
  ((tail(x) := #(9, 8, 7)) = #(9, 8, 7))
    | signal("(tail(x) := #(9, 8, 7)) is not #(9, 8, 7)!\n");
  (x = #(9, 9, 8, 7))
    | signal("x is not #(9, 9, 8, 7)!  It's %=\n", x);
  let x = add!(x, 1);
  (x = #(1, 9, 9, 8, 7))
    | signal("x is not #(1, 9, 9, 8, 7)!  It's %=\n", x);
  let x = remove!(x, 9);
  (x = #(1, 8, 7))
    | signal("x is not #(1, 8, 7)!  It's %=\n", x);
  (size(x) = 3)
    | signal("size(x) is not 3!  It's %=\n", size(x));
end method;

define method tautology(arg == #"ranges")
  let a = make(<range>, from: 0, to: 10);
  let b = make(<range>, from: 5, to: 15);
//  format("\na is %=\nb is %=\n", a.object-class, b.object-class);
  (first(a) = 0)	| signal("first(a) is not 0! It's %=\n", first(a));
  (first(b) = 5)	| signal("first(b) is not 5! It's %=\n", first(b));
  (last(a) = 10)	| signal("last(a) is not 10! It's %=\n", last(a));
  (last(b) = 15)	| signal("last(b) is not 15! It's %=\n", last(b));
  member?(3, a)		| signal("member?(3, a) is not true!\n");
  member?(12, a)	& signal("member?(12, a) is not false!\n");
  member?(3, b)		& signal("member?(3, b) is not false!\n");
  member?(12, b)	| signal("member?(12, b) is not true!\n");
  (size(a) = 11)	| signal("size(a) is not 11!  It's %=\n", size(a));
//  format("checkpoint 1\n");
  (size(b) = 11)	| signal("size(b) is not 11!  It's %=\n", size(b));
  let c = intersection(a, b);
  (first(c) = 5)	| signal("first(c) is not 5!  It's %=\n", first(c));
  (last(c) = 10)	| signal("last(c) is not 10!  It's %=\n", last(c));
  member?(7, c)		| signal("member?(7, c) is not true!\n");
  member?(12, c)	& signal("member?(12, c) is not false!\n");
//  format("checkpoint 2\n");
  (size(c) = 6)		| signal("size(c) is not 6!  It's %=\n", size(c));
  let d = reverse(c);
  (first(d) = 10)	| signal("first(d) is not 10!  It's %=\n", first(d));
  (last(d) = 5)		| signal("last(d) is not 5!  It's %=\n", last(d));
  let e = copy-sequence(d);
  (d = e)		| signal("d is not equal to e!\n");
  let f = reverse!(reverse!(d));
  (d = f)		| signal("d is not equal to f!\n");
end method;

define method tautology(arg == #"stretchy vectors")
  let a = make(<stretchy-vector>);
end method;

define method tautology(arg == #"strings")
  let a = make(<byte-string>);
end method;

define method tautology(arg == #"tables")
  let a = make(<table>);
end method;

define method tautology(arg == #"vectors")
  let a = make(<vector>);
end method;

define method tautology(arg :: <sequence>) => <integer>;
  let warnings = 0;
  local method warning(e :: <simple-warning>, next-handler)
	  apply(format, e.condition-format-string, e.condition-format-arguments);
	  warnings := warnings + 1;
	  #f;
	end method;
  let fatals = 0;
  local method fatal(e :: <simple-error>, next-handler)
	  apply(format, e.condition-format-string, e.condition-format-arguments);
	  fatals := fatals + 1;
	  #f;
	end method;
  let handler <simple-warning> = warning;
  for (arg in arg)
    if (arg)
      format("Tautologies on %s ... ", as(<string>, arg));
      let error-count = fatals;
      tautology(arg);
      if (error-count = fatals)    // last test had no errors
	format("ok.\n");
      end if;
    end if;
  end for;
  format("Tautology completed with %d warnings and %d fatal errors\n",
	 warnings, fatals);
  warnings + fatals;
end method;

define method main(argv0, #rest args)
#if (mindy)
  if (empty?(args))
    exit(exit-code: tautology(tautologies));
  else
    let args = map(curry(as, <symbol>), args);
    if (every?(rcurry(member?, tautologies), args))
      exit(exit-code: tautology(args));
#else
  if (argv0 <= 1)
    tautology(tautologies);
  else
    // Need to figure out how to convert a raw pointer...
    let args = #[];
    if (every?(rcurry(member?, tautologies), args))
      tautology(args);
#endif
    else
      format("usage: tautologies [package ...]\n");
      for (arg in tautologies)
	format("\t%s\n", as(<string>, arg));
      end for;
#if (mindy)
      exit(exit-code: -1);
#endif
    end if;
  end if;
//  force-output(*standard-output*);
end method;
