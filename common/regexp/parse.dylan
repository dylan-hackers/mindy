module: regular-expressions
author: Nick Kramer (nkramer@cs.cmu.edu)
copyright:  Copyright (C) 1994, Carnegie Mellon University.
            All rights reserved.
rcs-header: $Header: /home/housel/work/rcs/gd/src/common/regexp/parse.dylan,v 1.1 1996/02/17 16:12:26 nkramer Exp $

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

// This is a program to parse regular expressions. The grammar I'm using is:
//
//      <regexp> ::= <alternative> | <alternative>|<regexp>
//
//      <alternative> ::= <quantified-atom> | <quantified-atom><alternative>
//
//      <quantified-atom> ::= <atom> | <atom><quantifier>
//
//      <quantifier> ::= * | + | ? | {n} | {n,} | {n, m}
//            (where n and m are decimal integers)
//
//      <atom> ::= (<regexp>) | <extended-character>
//
// See "Programming perl", p. 103-104 for more details.
//
// Because an assertion is a type of <extended-character>, this will
// parse a "quantified assertion", which really isn't a legal regular
// expression component.  Match.dylan could go into an infinite loop
// if given this.

define abstract class <parsed-regexp> (<object>)
end class <parsed-regexp>;

define class <mark> (<parsed-regexp>)
  slot child :: <parsed-regexp>,  required-init-keyword: #"child";
  slot group-number :: <integer>, required-init-keyword: #"group";
end class <mark>;

define class <union> (<parsed-regexp>)          //    |
  slot left  :: <parsed-regexp>, required-init-keyword: #"left";
  slot right :: <parsed-regexp>, required-init-keyword: #"right";
end class <union>;

define class <alternative> (<parsed-regexp>)    // concatenation
  slot left :: <parsed-regexp>,  required-init-keyword: #"left";
  slot right :: <parsed-regexp>, required-init-keyword: #"right";
end class <alternative>;

define class <parsed-assertion> (<parsed-regexp>)
  slot asserts :: <symbol>, required-init-keyword: #"assertion";
end class <parsed-assertion>;

define class <quantified-atom> (<parsed-regexp>)
  slot atom :: <parsed-regexp>, required-init-keyword: #"atom";
  slot min-matches :: <integer>,  init-value: 0,  init-keyword: #"min";
  slot max-matches :: false-or(<integer>), 
    init-value: #f, init-keyword: #"max";
end class <quantified-atom>;

define abstract class <parsed-atom> (<parsed-regexp>)
end class <parsed-atom>;

define class <parsed-character> (<parsed-atom>)
  slot character :: <character>, required-init-keyword: #"character";
end class <parsed-character>;

define class <parsed-string> (<parsed-atom>)
  slot string :: <string>, required-init-keyword: #"string";
end class <parsed-string>;

define class <parsed-set> (<parsed-atom>)
  slot char-set :: <character-set>, required-init-keyword: #"set";
end class <parsed-set>;

define class <parsed-backreference> (<parsed-atom>)
  slot group-number :: <integer>, required-init-keyword: #"group"; 
end class <parsed-backreference>;

// <parse-info> contains some information about the current regexp
// being parsed.  Using a structure is slightly nicer than having
// global variables..
//
define class <parse-info> (<object>)
  slot backreference-used :: <boolean>, init-value: #f;
     // Whether or not the function includes \1, \2, etc in the regexp.
     // This is different from return-marks, which determines whether the
     // user wants to know about the marks.
  slot has-alternatives :: <boolean>, init-value: #f;
  slot has-quantifiers :: <boolean>, init-value: #f;
  slot current-group-number :: <integer>, init-value: 0;
  slot set-type :: <class>, required-init-keyword: #"set-type";
end class <parse-info>;

define method parse (s :: <string>, character-set-type :: <class>);
  let parse-info = make(<parse-info>, set-type: character-set-type);
  let parse-string = make(<parse-string>, string: s);
  let parse-tree = make(<mark>, group: 0, 
			child: parse-regexp(parse-string, parse-info));
  values(optimize(parse-tree),
	 parse-info.current-group-number,
	 parse-info.backreference-used,
	 parse-info.has-alternatives,
	 parse-info.has-quantifiers);
end method parse;

define method parse-regexp (s :: <parse-string>, info :: <parse-info>)
 => parsed-regexp :: <parsed-regexp>;
  let alternative = parse-alternative(s, info);
  if (lookahead(s) = '|')
    info.has-alternatives := #t;
    make(<union>, left: alternative, right: parse-regexp(consume(s), info));
  else
    alternative;
  end if;
end method parse-regexp;

define method parse-alternative (s :: <parse-string>, info :: <parse-info>)
 => parsed-regexp :: <parsed-regexp>;
  let term = parse-quantified-atom(s, info);
  if (member?(lookahead(s), #(#f, '|', ')')))
    term;
  else
    make(<alternative>, left: term, right: parse-alternative(s, info));
  end if;
end method parse-alternative;

define method parse-quantified-atom (s :: <parse-string>, info :: <parse-info>)
 => parsed-regexp :: <parsed-regexp>;
  let atom = parse-atom(s, info);
  let char = lookahead(s);
  select (char by \=)
    '*' =>
      info.has-quantifiers := #t;
      consume(s);
      make(<quantified-atom>, min: 0, atom: atom);

    '+' =>
      info.has-quantifiers := #t;
      consume(s);
      make(<quantified-atom>, min: 1, atom: atom);

    '?' =>
      info.has-quantifiers := #t;
      consume(s);
      make(<quantified-atom>, min: 0, max: 1, atom: atom);

    '{' =>
      info.has-quantifiers := #t;
      consume(s);
      let first-string = make(<deque>);
      let second-string = make(<deque>);
      let has-comma = #f;
      for (c = lookahead(s) then lookahead(s), until: c = '}')
	consume(s);
	if (c = ',')  
	  has-comma := #t;
	elseif (has-comma)  
	  push-last(second-string, c);
	else 
	  push-last(first-string, c);
	end if;
      end for;
      consume(s);         // Eat closing brace
      make(<quantified-atom>, atom: atom, 
	   min: string-to-integer(first-string),
	   max:  if (~has-comma)    
		   string-to-integer(first-string)
		 elseif (empty?(second-string))   
		   #f
		 else
		   string-to-integer(second-string) 
		 end if);

    otherwise =>
      atom;
  end select;
end method parse-quantified-atom;

define method parse-atom (s :: <parse-string>, info :: <parse-info>)
 => parsed-regexp :: <parsed-regexp>;
  let char = lookahead(s);
  select (char)
    '(' =>
      consume(s);   // Consume beginning paren
      info.current-group-number := info.current-group-number + 1;
      let this-group = info.current-group-number;
      let regexp = parse-regexp(s, info);
      if (lookahead(s) ~= ')')
	error("Unbalanced parens in regexp");
      end if;
      consume(s);   // Consume end paren
      make(<mark>, child: regexp, group: this-group);

    ')' =>
      #f;             // Need something to terminate upon seeing a close paren

    #f  =>
      #f;   // Signal error?  (end of stream)

    '*', '|', '+' =>
      #f;

    '\\' =>
      consume(s);        // Consume the backslash
      parse-escaped-character(s, info);

    '[' =>
      consume(s);        // Eat the opening brace
      let set-string = make(<deque>);      // Need something that'll 
                                           // preserve the right ordering
      for (char = lookahead(s) then lookahead(s), until: char = ']')
	consume(s);                    // eat char
	push-last(set-string, char);
	if (char = '\\')
	  push-last(set-string, lookahead(s));
	  consume(s);     // eat thing after backslash
	end if;
      end for;
      consume(s);     // Eat ending brace
      make(<parsed-set>, set: make(info.set-type, description: set-string));

    '.' =>
      consume(s);
      dot;

    '^' =>
      consume(s);
      make(<parsed-assertion>, assertion: #"beginning-of-string");

    '$' =>
      consume(s);
      make(<parsed-assertion>, assertion: #"end-of-string");
  
      // Insert more special characters here

    otherwise =>
      let char = lookahead(s);
      consume(s);
      make(<parsed-character>, character: char);
  end select;
end method parse-atom;

define constant any-char 
  = make(<case-sensitive-character-set>, description: "^\n");

// The useful definitions of all these is in as(<character-set>)
//
define constant digit-chars
  = make(<case-sensitive-character-set>, description: "\\d");
define constant not-digit-chars
  = make(<case-sensitive-character-set>, description: "^\\d");
define constant word-chars
  = make(<case-sensitive-character-set>, description: "\\w");
define constant not-word-chars
  = make(<case-sensitive-character-set>, description: "^\\w");
define constant whitespace-chars
  = make(<case-sensitive-character-set>, description: "\\s");
define constant not-whitespace-chars
  = make(<case-sensitive-character-set>, description: "^\\s");

define constant dot = make(<parsed-set>, set: any-char);
define constant dot-star = make(<quantified-atom>, min: 0, max: #f,
				atom: dot);

// This only handles escaped characters *outside* of a character
// set. Inside of a character set is a whole different story.
//
define method parse-escaped-character 
    (s :: <parse-string>, info :: <parse-info>)
 => parsed-regexp :: <parsed-regexp>;
  let next-char = lookahead(s);
  consume(s);
  select (next-char)
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' =>
      info.backreference-used := #t;
      make(<parsed-backreference>, group: digit-to-integer(next-char));

    'n' =>   make(<parsed-character>, character: '\n');   // Newline
    't' =>   make(<parsed-character>, character: '\t');   // Tab
    'f' =>   make(<parsed-character>, character: '\f');   // Formfeed
    'r' =>   make(<parsed-character>, character: '\r');   // Carriage return

    'b' =>   make(<parsed-assertion>, assertion: #"word-boundary");
    'B' =>   make(<parsed-assertion>, assertion: #"not-word-boundary");
       // Beginning and end of string are not escaped

    'd' =>   make(<parsed-set>, set: digit-chars);
    'D' =>   make(<parsed-set>, set: not-digit-chars);
    'w' =>   make(<parsed-set>, set: word-chars);
    'W' =>   make(<parsed-set>, set: not-word-chars);
    's' =>   make(<parsed-set>, set: whitespace-chars);
    'S' =>   make(<parsed-set>, set: not-whitespace-chars);

    // Insert more escaped characters here

    otherwise =>
      make(<parsed-character>, character: next-char);
  end select;
end method parse-escaped-character;

define method is-anchored? (regexp :: <parsed-regexp>)
 => (result :: <boolean>);
  select (regexp by instance?)
    <mark> => is-anchored?(regexp.child);
    <alternative> => is-anchored?(regexp.left);
    <parsed-assertion> => regexp.asserts == #"beginning-of-string";
    otherwise => #f;
  end select;
end method is-anchored?;

define method initial-substring (regexp :: <parsed-regexp>)
 => (result :: <string>);
  let result = make(<deque>);
  local method init (regexp :: <parsed-regexp>, result :: <deque>)
	  select (regexp by instance?)
	    <alternative> =>
	      init(regexp.left, result) & init(regexp.right, result);
	    <parsed-character> =>
	      push-last(result, regexp.character);
	    <parsed-string> =>
	      for (ch in regexp.string) push-last(result, ch) end for;
	    <mark> =>
	      init(regexp.child, result);
	    <parsed-assertion> =>
	      #t;
	    otherwise =>
	      #f;
	  end select;
	end method init;
  init(regexp, result);
  as(<byte-string>, result);
end method initial-substring;

// Optimize converts a parse tree into an "optimized" parse tree.
// Currently the only optimization is merging adjacent characters into
// a string.
//
define method optimize (regexp :: <parsed-regexp>)
 => (regexp :: <parsed-regexp>);
  select (regexp by instance?)
    <mark> =>
      regexp.child := optimize(regexp.child);
      regexp;
    <alternative> =>
      if (instance?(regexp.left, <parsed-character>))
	let result-str = make(<deque>);
	push-last(result-str, regexp.left.character);
	for (next = regexp.right then next.right,
	     while: (instance?(next, <alternative>)
		       & instance?(next.left, <parsed-character>)))
	  push-last(result-str, next.left.character)
	finally
	  if (instance?(next, <parsed-character>))
	    push-last(result-str, next.character);
	    make(<parsed-string>, string: as(<string>, result-str));
	  elseif (result-str.size = 1)
	    regexp.right := optimize(regexp.right);
	    regexp;
	  else
	    make(<alternative>,
		 left: make(<parsed-string>, string: as(<string>, result-str)),
		 right: optimize(next));
	  end if;
	end for;
      else
	regexp.left := optimize(regexp.left);
	regexp.right := optimize(regexp.right);
	regexp;
      end if;
    <union> =>
      regexp.left := optimize(regexp.left);
      regexp.right := optimize(regexp.right);
      regexp;
    <quantified-atom> =>
      regexp.atom := optimize(regexp.atom);
      regexp;
    otherwise =>
      regexp;
  end select;
end method optimize;
