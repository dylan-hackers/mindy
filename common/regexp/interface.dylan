module:   regular-expressions
author:   Nick Kramer (nkramer@cs.cmu.edu)
synopsis: This provides a useable interface for users. Functions 
	  defined outside this file are really too strange and quirky 
          to be of use to people.
copyright:  Copyright (C) 1994, Carnegie Mellon University.
            All rights reserved.
rcs-header: $Header: /home/housel/work/rcs/gd/src/common/regexp/interface.dylan,v 1.4 1996/03/26 22:46:21 nkramer Exp $

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

// Functions that aren't exported are marked as such.  Everything else
// is exported.

// Find the position of a regular expression inside a string.  If the
// regexp is not found, return #f, otherwise return a variable number
// of marks.
//
define open generic regexp-position
    (big :: <string>, regexp :: <string>, #key start,
     end: the-end, case-sensitive)
 => (regexp-start :: false-or(<integer>), #rest marks :: false-or(<integer>));

// This will be faster than the default == method because it has more
// type information.
//
define constant char-==
    = method (ch1 :: <character>, ch2 :: <character>) => (result :: <boolean>);
	ch1 == ch2;
      end method;

// Type-specific version of case-insensitive-equal.
define constant letter-==
    = method (ch1 :: <character>, ch2 :: <character>) => (result :: <boolean>);
	case-insensitive-equal(ch1, ch2);
      end method;

define method regexp-position
    (big :: <string>, regexp :: <string>, #key start: big-start = 0,
     end: big-end = #f, case-sensitive = #f)
 => (regexp-start :: false-or(<integer>), #rest marks :: false-or(<integer>));
  let substring = make(<substring>, string: big, start: big-start,
		       end: big-end | big.size);
  let comparison = if (case-sensitive) char-== else letter-== end;
  let char-set-class = if (case-sensitive) 
			 <case-sensitive-character-set>;
		       else
			 <case-insensitive-character-set>;
		       end if;
  let (parsed-regexp, last-group) = parse(regexp, char-set-class);

  let (matched, marks)
    = if (parsed-regexp.is-anchored?)
	anchored-match-root?(parsed-regexp, substring, comparison,
			     last-group + 1, #f);
      else
	let initial = parsed-regexp.initial-substring;
	let searcher = ~initial.empty?
	  & make-substring-positioner(initial, case-sensitive: case-sensitive);
	match-root?(parsed-regexp, substring, comparison, last-group + 1,
		    searcher);
      end if;
  if (matched)  
    apply(values, marks);
  else
    #f  
  end if;
end method regexp-position;

// Returns an appropriate matcher function that acts just like
// regexp-position curried.  If the user can deal with a positioner
// that works only on byte strings and doesn't return any marks, and
// doesn't mind the extra wait while the thing compiles, he can get a
// positioner that executes considerably faster.
//
define open generic make-regexp-positioner
    (regexp :: <string>, #key byte-characters-only,
     need-marks, maximum-compile, case-sensitive)
 => regexp-positioner :: <function>;

define method make-regexp-positioner
    (regexp :: <string>, 
     #key byte-characters-only = #f, need-marks = #t, maximum-compile = #f,
     case-sensitive = #f)
 => regexp-positioner :: <function>;
  let comparison = if (case-sensitive) char-== else letter-== end;
  let char-set-class = if (case-sensitive) 
			 <case-sensitive-character-set>;
		       else
			 <case-insensitive-character-set>;
		       end if;
  let (parsed-regexp, last-group, has-backrefs, alternatives, quantifiers) 
    = parse(regexp, char-set-class);
  let match-root-function = if (parsed-regexp.is-anchored?)
			      anchored-match-root?
			    else
			      match-root?;
			    end if;
  let initial = parsed-regexp.initial-substring;
  let searcher = ~initial.empty?
    & make-substring-positioner(initial, case-sensitive: case-sensitive);

  if (~maximum-compile | has-backrefs | ~byte-characters-only | need-marks)
    method (big :: <string>, #key start: big-start = 0,
	    end: big-end = #f)
     => (regexp-start :: false-or(<integer>), #rest marks :: false-or(<integer>));
      let substring = make(<substring>, string: big, start: big-start,
			   end: big-end | big.size);
      let (matched, marks)
	= match-root-function(parsed-regexp, substring, comparison,
			      last-group + 1, searcher);
      if (matched)  
	apply(values, marks);
      else
	#f  
      end if;
    end method;
  else
    let (nfa-begin, nfa-end) = build-nfa(parsed-regexp);
       // Now modify the DFA to accomodate a substring match rather
       // than matching the entire string
    let dot = make(<set-atom>, set: any-char);
    let star = make(<e-state>, next-state: dot, other-next-state: nfa-begin);
    dot.next-state := star;
    let dfa = nfa-to-dfa(star, nfa-end, comparison);
    method (big :: <string>, #key start: big-start = 0,
	    end: big-end = #f)
        => answer :: <boolean>;
      sim-dfa(dfa, make(<substring>, string: big, start: big-start,
			end: big-end | big.size));
    end method;
  end if;
end method make-regexp-positioner;

define open generic regexp-replace
    (input :: <string>, regexp :: <string>, new-substring :: <string>,
     #key count, case-sensitive, start, end: the-end)
 => changed-string :: <string>;

define method regexp-replace
    (input :: <string>, regexp :: <string>, new-substring :: <string>,
     #key count = #f, case-sensitive = #f, start = 0, end: input-end = #f)
 => changed-string :: <string>;
  let positioner
    = make-regexp-positioner(regexp, case-sensitive: case-sensitive);
  do-replacement(positioner, new-substring, input, start, 
		 input-end, count, #t);
end method regexp-replace;

define open generic make-regexp-replacer
    (regexp :: <string>, #key replace-with, case-sensitive)
 => replacer :: <function>;

define method make-regexp-replacer 
    (regexp :: <string>, #key replace-with, case-sensitive = #f)
 => replacer :: <function>;
  let positioner
    = make-regexp-positioner(regexp, case-sensitive: case-sensitive);
  if (replace-with)
    method (input :: <string>, #key count: count, 
	    start = 0, end: input-end = #f)
     => string :: <string>;
      do-replacement(positioner, replace-with, input, start, 
		     input-end, count, #t);
    end method;
  else
    method (input :: <string>, new-substring :: <string>, 
	    #key count = #f, case-sensitive = #f,
	    start = 0, end: input-end = #f)
     => string :: <string>;
      do-replacement(positioner, new-substring, input, 
		     start, input-end, count, #t);
    end method;
  end if;
end method make-regexp-replacer;

// equivalent of Perl's tr.  Does a character by character translation.
//
define open generic translate
    (input :: <string>, from-set :: <string>, to-set :: <string>, 
     #key delete, start, end: the-end)
 => output :: <string>;

//The existing methods only work on byte-strings.
//
define method translate
    (input :: <byte-string>, from-set :: <byte-string>,
     to-set :: <byte-string>,
     #key delete: delete = #f, start = 0, end: input-end = #f)
 => output :: <byte-string>;
  let table = make-translation-table(from-set, to-set, delete: delete);
  run-translator(input, table, start, input-end | size(input));
end method translate;

define open generic make-translator
    (from-set :: <string>, to-set :: <string>, #key delete)
 => translator :: <function>;

// Again, only byte-strings handled here
//
define method make-translator
    (from-set :: <byte-string>, to-set :: <byte-string>,
     #key delete: delete = #f)
 => translator :: <function>;
  let table = make-translation-table(from-set, to-set, delete: delete);
  method (input :: <byte-string>, #key start = 0, end: input-end = #f)
   => output :: <byte-string>;
    run-translator(input, table, start, input-end | size(input));
  end method;
end method make-translator;

// Used by translate.  Not exported.
//
define method make-translation-table
    (from-set :: <byte-string>, to-set :: <byte-string>,
     #key delete: delete = #f)
 => table :: <byte-character-table>;
  let from-index = 0;
  let to-index = 0;
  let previous-from = #f;
  let previous-to = #f;

     // These local methods are identical except for the 
     // choice of variable names and next-from-character signals end of
     // string rather than repeating the last character indefinitely like
     // next-to-character does.
  local method next-from-character ()
	  if (from-index >= size(from-set))
	    #f;
	  elseif (from-set[from-index] = '\\')
	    from-index := from-index + 2;
	    previous-from := from-set[from-index - 1];
	  elseif (from-set[from-index] = '-')
	    if (previous-from = from-set[from-index + 1])
	      from-index := from-index + 1;
	      from-set[from-index];
	    else
	      previous-from := successor(previous-from); 
	              // and return that value
	    end if;
	  else
	    from-index := from-index + 1;
	    previous-from := from-set[from-index - 1];
	  end if;
	end method next-from-character;

  local method next-to-character ()
	  if (to-index >= size(to-set))
	    if (delete)  #f  else  last(to-set)  end;
	  elseif (to-set[to-index] = '\\')
	    to-index := to-index + 2;
	    previous-to := to-set[to-index - 1];
	  elseif (to-set[to-index] = '-')
	    if (previous-to = to-set[to-index + 1])
	      to-index := to-index + 1;
	      to-set[to-index];
	    else
	      previous-to := successor(previous-to); 
	              // and return that value
	    end if;
	  else
	    to-index := to-index + 1;
	    previous-to := to-set[to-index - 1];
	  end if;
	end method next-to-character;

  let table = make(<byte-character-table>);
  // Wish I had keyed-by
  let (state, limit, next, done?, cur-key, cur-elem)
    = forward-iteration-protocol(table);
  for (st = state then next(table, st), until: done?(table, st, limit))
    let c = cur-key(table, st);
    table[c] := c;
  end for;

  for (from-char = next-from-character() then next-from-character(),
       to-char = next-to-character() then next-to-character(),
       until: from-char = #f)
    table[from-char] := to-char;
  end for;

  table;
end method make-translation-table;

// Used by translate.  Not exported.
//
define method run-translator
    (source :: <byte-string>, table :: <byte-character-table>, 
     start-index :: <integer>, end-index :: <integer>)
 => output :: <byte-string>;
  let dest-string = copy-sequence(source);
  let dest-index = start-index;
  for (source-index from start-index below end-index)
    let char = source[source-index];
    if (table[char] ~== #f)
      dest-string[dest-index] := table[char];
      dest-index := dest-index + 1;
    end if;
  end for;

      // Now resize dest-string, because deleting characters in the
      // translation would make dest-string shorter than we've
      // allocated.
  if (dest-index = end-index)
    dest-string;
  else
    replace-subsequence!(dest-string, "", start: dest-index, end: end-index);
  end if;
end method run-translator;

// Like Perl's split function
//
define open generic split
    (pattern :: <string>, input :: <string>, 
     #key count, remove-empty-items, start, end: the-end);
// => #rest whole-bunch-of-strings :: <string>;

define method split
    (pattern :: <string>, input :: <string>, 
     #key count = #f, remove-empty-items = #t, start = 0, end: input-end = #f)
 => (#rest whole-bunch-of-strings :: <string>);
  let positioner = make-regexp-positioner(pattern);
  split-string(positioner, input, start, input-end | size(input),
	       count, remove-empty-items);
end method split;

define open generic make-splitter
    (pattern :: <string>) => splitter :: <function>;

define method make-splitter
    (pattern :: <string>) => splitter :: <function>;
  let positioner = make-regexp-positioner(pattern);
  method (string :: <string>, #key count = #f,
	  remove-empty-items = #t, start = 0, end: input-end = #f)
   => (#rest whole-bunch-of-strings :: <string>);
    split-string(positioner, string, start, input-end | size(string), 
		 count, remove-empty-items);
  end method;
end method make-splitter;

// Used by split.  Not exported.
//
define method split-string
    (positioner :: <function>, input :: <string>, start :: <integer>, 
     input-end :: <integer>, count :: false-or(<integer>), 
     remove-empty-items :: <object>)
 => (#rest whole-bunch-of-strings :: <string>);
  let strings = make(<deque>);
  block (done)
    let end-of-last-match = 0;
    let start-of-where-to-look = start;
    let string-number = 1;    // Since count: starts at 1, so 
                              // should string-number
    while (#t)
      let (substring-start, substring-end)
	= positioner(input, start: start-of-where-to-look, end: input-end);
      if (~substring-start | (count & (count <= string-number)))
	push-last(strings, copy-sequence(input, start: end-of-last-match));
	done(); 
      elseif ((substring-start = start-of-where-to-look)
		&  remove-empty-items)
	      // delimited item is empty
	end-of-last-match := substring-end;
	start-of-where-to-look := end-of-last-match;
      else
	let new-string = copy-sequence(input, start: end-of-last-match, 
				       end: substring-start);
	if (~new-string.empty? | ~remove-empty-items)
	  push-last(strings, new-string);
	  string-number := string-number + 1;
	  end-of-last-match := substring-end;
	  start-of-where-to-look := end-of-last-match;
	end if;
      end if;
    end while;
  end block;
  if (remove-empty-items)
    apply(values, remove!(strings, "", test: \=));
  else
    apply(values, strings);
  end if;
end method split-string;

// join--like Perl's join
//
define open generic join (delimiter :: <string>, #rest strings)
 => big-string :: <string>;

// This is not really any more efficient than concatenate-as, but it's
// more convenient.
//
define method join (delimiter :: <byte-string>, #rest strings)
 => big-string :: <byte-string>;
  let length = max(0, (strings.size - 1 ) * delimiter.size);
  for (string in strings)
    length := length + string.size;
  end for;
  let big-string = make(<byte-string>, size: length);
  let big-index = 0;
  for (i from 0 to strings.size - 2)  // Don't iterate over the last string
    let string = strings[i];
    let new-index = big-index + string.size;
    big-string := replace-subsequence!(big-string, string, 
				       start: big-index, end: new-index);
    big-index := new-index;
    let new-index = big-index + delimiter.size;
    big-string := replace-subsequence!(big-string, delimiter, 
				       start: big-index, end: new-index);
    big-index := new-index;
  end for;
  if (strings.size > 0)
    big-string 
      := replace-subsequence!(big-string, strings.last, 
			      start: big-index, end: big-string.size);
  end if;
  big-string;
end method join;
