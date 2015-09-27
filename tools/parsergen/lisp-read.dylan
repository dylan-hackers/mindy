module: lisp-read
copyright: see below

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
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

// ### This file is almost identical to the top of lisp2dylan.dylan,
// but not quite.  The main difference is that the other version was
// kluged to handle comments (by spewing them to stdout as we read
// them), and the other version doesn't include the lisp-read()
// function.

// The purpose of this file is to implement a simple Lisp parser.
// This file does *not* implement the full functionality of the real
// Lisp reader; rather, I implemented only what was required for me to
// accomplish the tasks at hand (reading parsergen input files, and
// running lisp2dylan over parsergen.lisp).

// There are probably also bugs in what I do claim to implement; I
// don't really know Lisp very well.  (Token classes like
// <macro-thingy> ought to give that away pretty quick)



define abstract class <token> (<object>)
end class <token>;

define class <identifier> (<token>)
  constant slot id-name :: <string>, required-init-keyword: #"name";
end class <identifier>;

define class <string-literal> (<token>)
  constant slot string-literal :: <string>, required-init-keyword: #"string";
end class <string-literal>;

define class <character-literal> (<token>)
  constant slot character-literal :: <character>,
    required-init-keyword: #"character";
end class <character-literal>;

define class <keyword> (<token>)
  constant slot keyword-string :: <string>, required-init-keyword: #"keyword";
end class <keyword>;

// Has something to do with macros, but I'm not sure what
//
define class <macro-thingy> (<token>)
  constant slot looks-like :: <string>, required-init-keyword: #"looks-like";
end class <macro-thingy>;

define class <lparen> (<token>) end class <lparen>;
define class <rparen> (<token>) end class <rparen>;
// '( ) is <list-start> $rparen
define class <list-start> (<token>) end class <list-start>;

define constant $lparen = make(<lparen>);
define constant $rparen = make(<rparen>);
define constant $list-start = make(<list-start>);

define method print-object (token :: <identifier>, stream :: <stream>) => ();
  write(stream, token.id-name);
end method print-object;

define method print-object (token :: <string-literal>, stream :: <stream>) 
 => ();
  write(stream, concatenate("\"", token.string-literal, "\""));
end method print-object;

define method print-object (token :: <character-literal>, stream :: <stream>) 
 => ();
  write(stream, 
	concatenate("'", as(<string>, token.character-literal), "'"));
end method print-object;

define method print-object (token :: <keyword>, stream :: <stream>) 
 => ();
  write(stream, concatenate(token.keyword-string, ":"));
end method print-object;

define method print-object (token :: <macro-thingy>, stream :: <stream>) 
 => ();
  write(stream, token.looks-like);
end method print-object;

define method print-object (token :: <lparen>, stream :: <stream>) 
 => ();
  write(stream, "(");
end method print-object;

define method print-object (token :: <rparen>, stream :: <stream>) 
 => ();
  write(stream, ")");
end method print-object;

define method print-object (token :: <list-start>, stream :: <stream>) 
 => ();
  write(stream, "'(");
end method print-object;


// get-lex() -- Reads a lexeme from input.
//
define method get-lex (input :: <stream>) => lexeme :: <token>;
  let c = peek(input);
  if (c.whitespace?)
    read-element(input); 
    get-lex(input);
  else
    select (c)
      ';' => lex-comment(input); get-lex(input);
      '"' => lex-string(input);
      '(' => read-element(input); $lparen;
      ')' => read-element(input); $rparen;
      '&' => read-element(input); make(<macro-thingy>, looks-like: "&");
      ',' => read-element(input); make(<macro-thingy>, looks-like: ",");
      '`' => read-element(input); make(<macro-thingy>, looks-like: "`");
      '@' => read-element(input); make(<macro-thingy>, looks-like: "@");
      '\'' => 
	read-element(input);  // consume '
	if (peek(input) == '(')
	  read-element(input);
	  $list-start;
	else
	  get-lex(input);  // Ignore the '
	end if;
      ':' => 
	read-element(input);  // consume :
	let fake-identifier = lex-identifier(input);
	make(<keyword>, keyword: fake-identifier.id-name);
      '#' =>
	read-element(input);  // consume #
	let second-char = read-element(input);
	if (second-char == '\'')
	  lex(input);   // ignore #'
	elseif (second-char == '\\')
	  make(<character-literal>, character: read-element(input));
	else
	  error("Unknown lexeme starting with #");
	end if;
      '|' => 
	// quoted identifier
	read-element(input);
	let res = lex-quoted-symbol(input);
	let last-char = read-element(input);
	assert(last-char == '|');
	res;
      otherwise => 
	if (c.identifier-character?)
	  lex-identifier(input);
	else
	  error("Unknown lexeme starting with char %=", c);
	end if;
    end select;
  end if;
end method get-lex;

// A one-lookahead.  #f if lookahead is empty, otherwise whatever the
// lookahead is.
//
define variable *next-lexeme* :: false-or(<token>) = #f;

define function peek-lex (input :: <stream>) => lexeme :: <token>;
  *next-lexeme* | (*next-lexeme* := get-lex(input));
end function peek-lex;

// lex() -- Returns a lexeme.  However, if it encounters a newline,
// it'll output it.  Similarly, comments are translated by lex.
//
define function lex (input :: <stream>) => lexeme :: <token>;
  let res = block ()
	      let lexeme = *next-lexeme*;
	      *next-lexeme* := #f;
	      lexeme | get-lex(input);
	    end block;
//  format(*standard-output*, "%= ", res);
//  force-output(*standard-output*);
  res;
end function lex;

define function identifier-character? (c :: <character>)
 => answer :: <boolean>;
  c.alphanumeric? 
    | c == ':'  // I'm not sure about this one...
    | c == '%'
    | c == '+' | c == '-' | c == '*' | c == '/'
    | c == '=' | c == '<' | c == '>';
end function identifier-character?;

define method lex-identifier (input :: <stream>) 
 => id :: <identifier>;
  let res-vector = make(<stretchy-vector>);
  block (break)
    while (#t)
      let c = peek(input);
      if (~c.identifier-character?)
	break();
      end if;
      add!(res-vector, c);
      read-element(input);  // consume what we've already peeked at
    end while;
  end block;
  make(<identifier>, name: as(<string>, res-vector));
end method lex-identifier;

define method lex-quoted-symbol (input :: <stream>) 
 => id :: <identifier>;
  let res-vector = make(<stretchy-vector>);
  block (break)
    while (#t)
      let c = peek(input);
      if (c == '|')
	break();
      end if;
      add!(res-vector, c);
      read-element(input);  // consume what we've already peeked at
    end while;
  end block;
  make(<identifier>, name: as(<string>, res-vector));
end method lex-quoted-symbol;

define method lex-comment (input :: <stream>) => ();
  // Skip all the ;'s -- ";;" and ";;;" comments are common in Lisp
  read-while(input, method (c) c == ';' end);
  let line = read-line(input);
  // ### Moby kluge ahead: If we encounter a comment, just dump it to
  // std-out.  Given the way the converter is currently arranged, this
  // will have the effect of dumping all comments inside a top-level
  // form before the Dylan version of the form.  This, of course, is
  // better than not translating the comment at all, which is our
  // other choice...
  #if (lisp2dylan)
    write(*standard-output*, "/" "/");  // If I type "//", it screws up
                                        // the emacs mode
    write-line(*standard-output*, line);
  #endif
end method lex-comment;

define method lex-string (input :: <stream>) => string :: <string-literal>;
  let quote = read-element(input);
  if (quote ~== '"') 
    error("You told me this was a string!\n");
  end if;
  let res-vector = make(<stretchy-vector>);
  block (break)
    while (#t)
      let c = read-element(input);
      if (c == '"')
	break();
      else
	add!(res-vector, c);
	if (c == '\\')
	  let c2 = read-element(input);
	  add!(res-vector, c2);
	end if;
      end if;
    end while;
  end block;
  make(<string-literal>, string: as(<string>, res-vector));
end method lex-string;

// Read-to consumes the character that sets off the test.  We don't
// want that.  (This function could probably be implemented as read-to
// followed by unread, but this should work too)
//
define function read-while (input :: <stream>, test :: <function>) => ();
  block (break)
    for (c = peek(input) then peek(input))
      if (~test(c))
	break();
      end if;
      read-element(input);
    end for;
  end block;
end function read-while;


// A not-even-close-to-complete implementation of Lisp's read function
//
define function lisp-read (stream :: <stream>) => obj :: <object>;
  let token = lex(stream);
  select (token by instance?)
    <identifier> => as(<symbol>, token.id-name);
    <string-literal> => token.string-literal;
    <character-literal> => token.character-literal;
    <keyword> => as(<symbol>, token.keyword-string);
    <lparen> => 
      let vec = make(<stretchy-vector>);
      while (peek-lex(stream) ~== $rparen)
	add!(vec, lisp-read(stream));
      end while;
      assert(lex(stream) == $rparen);
      as(<list>, vec);
  end select;
end function lisp-read;
