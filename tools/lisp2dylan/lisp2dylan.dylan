module: lisp2dylan
author: Nick Kramer
copyright: see below
	   This code was produced by the Gwydion Project at Carnegie Mellon
	   University.  If you are interested in using this code, contact
	   "Scott.Fahlman@cs.cmu.edu" (Internet).

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

//======================================================================
//
// Copyright (c) 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
// All rights reserved.
//
//======================================================================

// This is a quick-and-dirty Lisp to Dylan translator.  It has tons
// and tons of problems.  I'm releasing it only because if I don't,
// I'm sure someone else will try exactly the same thing.  All things
// considered, I'd rather someone use my broken wheel than reinvent
// the broken wheel.

// This translator is quite simple-minded.  It is not really even a
// syntax-driven translator; I'd have to call it a lexically driven
// translator.  (The translator functions can't look more than one
// token ahead of what they are supposed to translate) It certainly
// has no real understanding of Lisp.  My original goal was to write
// something that only moved parenthesis around in function calls (ie,
// (func x y) to func(x, y)), but I soon added handling for a variety
// of macros and functions which annoyed me enough.

// Some of the more notable problems of this program include:

// 1. While some of the most common functions are translated (car =>
// head, etc), most are not.  Some of those that are not translated
// have no Dylan equivalent.
//
// 2. The parser/lexer isn't really all that good, because I barely
// know Lisp.
//
// 3. The generated Dylan code is unindented and missing many line
// breaks.  The dylan-mode for Emacs can indent the code for you, but
// you'll have to add line breaks yourself.
//
// 4. For similar reasons, via a wretched kluge all comments inside a
// top-level form (like a defun) are dumped in front of the translated
// form.  This is better than the alternative, not translating
// comments at all.
//
// 5. User-defined macros are not dealt with at all.
//
// 6. t is converted to #t, but nil is left as is.  I consider this a
// feature--there's simply no way for a computer to guess whether the
// author meant #f or #() when nil is written.
//
// 7. In Lisp, the empty list (nil) is the false value.  In Dylan, it
// isn't.  Thus, "if (~foo)" is not equivalent to "if (foo == #())",
// as many Lisp programs assume it is.
//
// 8. Let statements introduce a begin/end block in order to delimit
// the scope of the declared variables.  This is almost always
// unnecessary, but in this case I figured, "Correctness before
// beauty."  (In other situations, I made the opposite choice...)
//
// 9. Looping constructs do not have implicit return statements.  At
// one time I generated "return" non-local exits (via the block
// statement) for "loop" but not dotimes and other looping constructs.
// This turned out to be quite dangerous, though, because if the
// original Lisp program called return inside a dotimes inside a loop,
// the generated code would be subtly incorrect.  Another choice would
// have been to provide a return block for all looping constructs;
// this seemed far too ugly.  Providing none at all seemed like the
// best answer.
//
// 10. Type declarations for function parameters are not translated.
// This can't be done with a lexically-driven translator, because it
// requires too much lookahead.
//
// 11. Some defstruct options, like :print-function and :constructor,
// are silently dropped on the floor.  Usually, I tried to either
// signal an error or put the untranslatable code in a comment, but I
// messed up with defstruct options.

// Some of these problems, like dropping parameter type declarations,
// could be fixed if we went to a syntax-driven translator.  Others,
// like unindented output, could probably be fixed by using an
// intermediate representation for the generated Dylan code.  Still
// other problems, like missing return exits, could be fixed by adding
// semantic understanding of Lisp.  And some problems, like nil -> #f
// or #(), can never be automated.


// A note on implementation: Lexers and converters follow some
// conventions that help us maintain our sanity.  

// (convention 1) The first convention is that when one converter or
// lexer passes something off to a sub-convert or a sub-lexer, it does
// not consume the input that led it to choose that sub-translator.
// We shall call this initial input the dispatching input, because
// that's usually how it is used--the lexer/converter peeks at the
// dispatching input, then uses it to choose a sub-lexer or
// sub-converter.  (This convention is occasionally violated by
// converters, but it will be obvious because the dispatching input
// will be passed as a parameter to the helper function)

// (convention 2) Lexers follow the convention that they will
// completely read their token.  lex-string, for instance, would be
// expected to read the opening quote (convention 1), the contents of
// the string literal, and the closing quote (convention 2).

// (convention 3) Parser/converters follow the convention that
// delimiters are not consumed by the converter that is delimited.
// For instance, if our token stream looks like "(if test branch1
// branch2)", then convert-parenthesized will consume the open
// parenthesis, peek at the "if", and then call convert-if.
// Convert-if will read the "if" token (usually verifying that the
// token is indeed "if" in order to make sure that it wasn't called
// eroneously).  Then convert-if will convert the guts of the if
// statement.  Finally, it will peek at but not read the closing
// parenthesis.  Convert-if returns, and convert-parenthesized will
// consume the closing parenthesis.

// (convention 4) Assert is abused quite liberally throughout this
// file.  We frequently use the convention "assert(lex(input) ==
// $rparen)", which has the side effect of consuming the next token.
// This convention also doesn't make for great error handling
// ("assertion failed").  But, as I said, this program has problems...


// Token classes.  The token classes plainly reflect the fact that I
// don't know Lisp.  I didn't work off any sort of lexical
// specification of Common Lisp; I just made it up as I went along.

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

// ### Has something to do with macros, but I'm not sure what
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


// get-lex() -- Reads a lexeme from input.  Use lex(), not this
// function.
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
// lookahead is.  Used only by peek-lex and lex.
//
define variable *next-lexeme* :: false-or(<token>) = #f;

define function peek-lex (input :: <stream>) => lexeme :: <token>;
  *next-lexeme* | (*next-lexeme* := get-lex(input));
end function peek-lex;

// lex() -- Returns a lexeme.  Like get-lex(), except it deals with
// tokens that have already been peeked at.  (get-lex() always reads a
// new token from the stream, even if there's a perfectly good token
// sitting in *next-lexeme*)
//
define function lex (input :: <stream>) => lexeme :: <token>;
  let res = block ()
	      let lexeme = *next-lexeme*;
	      *next-lexeme* := #f;
	      lexeme | get-lex(input);
	    end block;
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
  write(*standard-output*, "/" "/");  // If I type "//", it screws up
                                      // the emacs mode
  write-line(*standard-output*, line);
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


// Converter/parsers

// See section on conventions towards the top of the file.  To recap,
// we peek at the first lexeme, then call the proper function to read
// it.  They are expected to actually read the first and last lexeme.
//
define method convert (input :: <stream>) => converted :: <string>;
  let lexeme = peek-lex(input);
  if (lexeme == $lparen)
    convert-parenthesized(input);
  elseif (lexeme == $rparen)
    error("Unbalanced paren");
  elseif (lexeme == $list-start)
    lex(input);  // consume $list-start
    let res = convert-funcall("list", input);
    assert(lex(input) == $rparen);
    res;
  else  // only simple lexemes left
    let lexeme = lex(input);
    if (instance?(lexeme, <string-literal>))
      concatenate("\"", lexeme.string-literal, "\"");
    elseif (instance?(lexeme, <character-literal>))
      concatenate("'\\", as(<string>, lexeme.character-literal), "'");
    elseif (instance?(lexeme, <identifier>))
      // query-replace t #t is just too obnoxious to do...
      // We intentionally don't translate nil because it is unclear 
      // whether the user meant #f or #()
      if (lexeme.id-name = "t")	"#t" else lexeme.id-name end if;
    elseif (instance?(lexeme, <keyword>))
      concatenate("#\"", lexeme.keyword-string, "\"");
    elseif (instance?(lexeme, <macro-thingy>))
      "";  // we'll ignore them and hope they go away
    else
      error("Unknown lexeme %=", lexeme);
    end if;
  end if;
end method convert;

// Convert-parenthesized owns the start and end parentheses
//
define method convert-parenthesized (input :: <stream>) 
 => converted :: <string>;
  assert(lex(input) == $lparen);
  let lexeme = peek-lex(input);
  let res 
    = if (instance?(lexeme, <identifier>))
	select (lexeme.id-name by \=)
	  "defstruct" => convert-defstruct(input);
	  "if" => convert-if(input);
	  "defmacro" => convert-ignored(input);
	  "defun" => convert-defun(input);
	  "let" => convert-let(input);
	  "let*" => convert-let(input);
	  "setq" => convert-setf(input);
	  "setf" => convert-setf(input);
	  "eq" => convert-equal(input);
	  "eql" => convert-equal(input);
	  "equal" => convert-equal(input);
	  "=" => convert-equal(input);
	  "when" => convert-test-with-body(input);
	  "unless" => convert-test-with-body(input);
	  "dotimes" => convert-dotimes(input);
	  "dolist" => convert-dolist(input);
	  "loop" => convert-loop(input);
	  "case" => convert-case(input);
	  "ecase" => convert-case(input);
	  "typecase" => convert-typecase(input);
	  "etypecase" => convert-typecase(input);
	  "cond" => convert-cond(input);
	  "lambda" => convert-lambda(input);
	  "labels" => convert-labels(input);
	  "aref" => convert-aref(input);
	  "gethash" => convert-gethash(input);
	  "1+" => convert-plus1(input);
	  "cons" => lex(input); convert-funcall("pair", input);
	  "car" => lex(input); convert-funcall("head", input);
	  "cdr" => lex(input); convert-funcall("tail", input);
	  "length" => lex(input); convert-funcall("size", input);
	  "and" => lex(input); convert-binop("&", input);
	  "or" => lex(input); convert-binop("|", input);
	  // amazingly, + - * / don't appear in parsergen.lisp, 
	  // so we don't do them
	  otherwise => convert-funcall(convert(input), input);
	end select;
      else
	convert-funcall(format-to-string("(%s)", convert(input)), input);
      end if;
  assert(lex(input) == $rparen);
  res;
end method convert-parenthesized;

define method convert-if (input :: <stream>) => converted :: <string>;
  assert(lex(input).id-name = "if");
  let res-stream = make(<buffered-byte-string-output-stream>);
  let test-clause = convert(input);
  let then-clause = convert(input);
  if (peek-lex(input) ~== $rparen)
    let else-clause = convert(input);
    format(res-stream, "if (%s)\n  %s\nelse\n  %s\nend if",
	   test-clause, then-clause, else-clause);
  else
    format(res-stream, "if (%s)\n  %s\nend if",
	   test-clause, then-clause);
  end if;
  if (peek-lex(input) ~== $rparen)
    error("Else clause too damn long");
  end if;
  res-stream.stream-contents;
end method convert-if;

define method convert-defun (input :: <stream>) => converted :: <string>;
  assert(lex(input).id-name = "defun");
  let name = convert(input);  // first token after "defstruct"
  assert(lex(input) == $lparen);
  let params = convert-param-list(input);
  assert(lex(input) == $rparen);
  let body = convert-body(input);
  format-to-string("define function %s (%s)\n%send function %s;\n",
		   name, params, body, name);
end method convert-defun;

// Sticks a ";\n" after each statement in the body
//
define method convert-body (input :: <stream>) => converted :: <string>;
  let res-stream = make(<buffered-byte-string-output-stream>);
  for (lexeme = peek-lex(input) then peek-lex(input),
       until: lexeme == $rparen)
    format(res-stream, "%s;\n", convert(input));
  end for;
  res-stream.stream-contents;
end method convert-body;

// Basically, an unnamed "defun"
//
define method convert-lambda (input :: <stream>) => converted :: <string>;
  assert(lex(input).id-name = "lambda");
  assert(lex(input) == $lparen);
  let params = convert-param-list(input);
  assert(lex(input) == $rparen);
  let body = convert-body(input);
  format-to-string("method (%s)\n%send method", params, body);
end method convert-lambda;

define method convert-setf (input :: <stream>) => converted :: <string>;
  let set-word = lex(input).id-name;
  assert(set-word = "setf" | set-word = "setq");
  let lvalue = convert(input);
  let rvalue = convert(input);
  assert(peek-lex(input) == $rparen);
  format-to-string("%s := %s", lvalue, rvalue);
end method convert-setf;

define method convert-let (input :: <stream>) => converted :: <string>;
  let let-word = lex(input).id-name;
  assert(let-word = "let" | let-word = "let*");
  let res-stream = make(<buffered-byte-string-output-stream>);
  format(res-stream, "begin\n");
  assert(lex(input) == $lparen);
  // Get the "let foo = bar" part
  while (peek-lex(input) ~== $rparen)
    assert(lex(input) == $lparen);
    let var = convert(input);  // probably should check to make sure it
                               // really is a simple name, but we won't
    let val = convert(input);
    assert(lex(input) == $rparen);
    format(res-stream, "let %s = %s;\n", var, val);
  end while;
  assert(lex(input) == $rparen);
  let body = convert-body(input);
  format(res-stream, "%send", body);
  res-stream.stream-contents;
end method convert-let;

// Similar to flet, which we don't handle
//
define method convert-labels (input :: <stream>) => converted :: <string>;
  assert(lex(input).id-name = "labels");
  let res-stream = make(<buffered-byte-string-output-stream>);
  format(res-stream, "begin\n");
  assert(lex(input) == $lparen);
  // Loop through the functions
  while (peek-lex(input) ~== $rparen)
    assert(lex(input) == $lparen);
    let fun-name = convert(input);
    assert(lex(input) == $lparen);
    let params = convert-param-list(input);
    assert(lex(input) == $rparen);
    let body = convert-body(input);
    assert(lex(input) == $rparen);
    format(res-stream, "local method %s (%s)\n%send method %s;\n", 
	   fun-name, params, body, fun-name);
  end while;
  assert(lex(input) == $rparen);
  let main-body = convert-body(input);
  format(res-stream, "%send", main-body);
  res-stream.stream-contents;
end method convert-labels;

define method convert-dotimes (input :: <stream>) => converted :: <string>;
  assert(lex(input).id-name = "dotimes");
  assert(lex(input) == $lparen);
  let var = convert(input);
  let count = convert(input);
  assert(lex(input) == $rparen);  // we don't do resultforms in dotimes
  let body = convert-body(input);
  format-to-string("for (%s from 0 below %s)\n%send for", var, count, body);
end method convert-dotimes;

define method convert-dolist (input :: <stream>) => converted :: <string>;
  assert(lex(input).id-name = "dolist");
  assert(lex(input) == $lparen);
  let var = convert(input);
  let listform = convert(input);
  let resultform = (peek-lex(input) ~== $rparen) & convert(input);
  assert(lex(input) == $rparen);
  let body = convert-body(input);
  if (resultform)
    format-to-string("for (%s in %s)\n%sfinally\n%s;\nend for",
		     var, listform, body, resultform);
  else
    format-to-string("for (%s in %s)\n%send for",
		     var, listform, body);
  end if;
end method convert-dolist;

define method convert-loop (input :: <stream>) => converted :: <string>;
  assert(lex(input).id-name = "loop");
  let body = convert-body(input);
  // We used to add a "block (return)" automatically, but that was
  // extremely dangerous because other looping constructs didn't
  format-to-string("block (return)\nwhile (#t)\n%send while;\nend block",
		   body);
end method convert-loop;

// handles only clauses with single keys
//
define method convert-case (input :: <stream>) => converted :: <string>;
  let macro-name = lex(input).id-name;
  assert(macro-name = "case" | macro-name = "ecase");
  let val = convert(input);
  let res-stream = make(<buffered-byte-string-output-stream>);
  format(res-stream, "select (%s)\n", val);
  while (peek-lex(input) ~== $rparen)
    assert(lex(input) == $lparen);
    let match-val = convert(input);
    let what-to-do = convert-body(input);
    assert(lex(input) == $rparen);
    format(res-stream, "%s =>\n%s", match-val, what-to-do);
  end while;
  format(res-stream, "end select");
  res-stream.stream-contents;
end method convert-case;

define method convert-typecase (input :: <stream>) => converted :: <string>;
  let macro-name = lex(input).id-name;
  assert(macro-name = "typecase" | macro-name = "etypecase");
  let val = convert(input);
  let res-stream = make(<buffered-byte-string-output-stream>);
  format(res-stream, "select (%s by instance?)\n", val);
  while (peek-lex(input) ~== $rparen)
    assert(lex(input) == $lparen);
    let match-val = convert-type(input);
    let what-to-do = convert-body(input);
    assert(lex(input) == $rparen);
    format(res-stream, "%s =>\n%s", match-val, what-to-do);
  end while;
  format(res-stream, "end select");
  res-stream.stream-contents;
end method convert-typecase;

define method convert-cond (input :: <stream>) => converted :: <string>;
  assert(lex(input).id-name = "cond");
  let res-stream = make(<buffered-byte-string-output-stream>);
  format(res-stream, "case\n");
  while (peek-lex(input) ~== $rparen)
    assert(lex(input) == $lparen);
    let test = convert(input);
    let what-to-do = convert-body(input);
    assert(lex(input) == $rparen);
    format(res-stream, "(%s) =>\n%s", test, what-to-do);
  end while;
  format(res-stream, "end case");
  res-stream.stream-contents;
end method convert-cond;

define method convert-plus1 (input :: <stream>) => converted :: <string>;
  assert(lex(input).id-name = "1+");
  let arg = convert(input);
  assert(peek-lex(input) == $rparen);
  format-to-string("1 + %s", arg);
end method convert-plus1;

define method convert-equal (input :: <stream>) => converted :: <string>;
  let eq-word = lex(input).id-name;
  let dylan-equiv = select (eq-word by \=)
		      "eq" => "==";
		      "eql" => "==";
		      "equal" => "=";
		      "=" => "=";       // ### not so sure about this
		      otherwise => error("Unknown eq-word %=", eq-word);
		    end select;
  let arg1 = convert(input);
  let arg2 = convert(input);
  assert(peek-lex(input) == $rparen);
  format-to-string("%s %s %s", arg1, dylan-equiv, arg2);
end method convert-equal;

// when, unless, etc
//
define method convert-test-with-body (input :: <stream>) 
 => converted :: <string>;
  let macro-name = lex(input).id-name;
  let dylan-equiv = select (macro-name by \=)
		      "when" => "if";
		      "unless" => "unless";
		      otherwise => error("Unknown test-with-body %=", 
					 macro-name);
		    end select;
  let test = convert(input);
  let body = convert-body(input);
  format-to-string("%s (%s)\n%send %s", dylan-equiv, test, body, dylan-equiv);
end method convert-test-with-body;

define method convert-defstruct (input :: <stream>) => converted :: <string>;
  assert(lex(input).id-name = "defstruct");
  let res-stream = make(<buffered-byte-string-output-stream>);
  let classname = convert-defstruct-name(input);
  format(res-stream, "define class <%s> (<object>)\n", classname);
  for (lexeme = peek-lex(input) then peek-lex(input), 
       until: lexeme == $rparen)
    format(res-stream, convert-slot(input));
  end for;
  format(res-stream, "end class <%s>;\n", classname);
  res-stream.stream-contents;
end method convert-defstruct;

define method convert-defstruct-name (input :: <stream>)
 => converted :: <string>;
  if (instance?(peek-lex(input), <identifier>))
    convert(input);
  else
    assert(lex(input) == $lparen);
    let res = convert(input);  // Assumed a struct name
    ignore-up-to-end-paren(input);
    assert(lex(input) == $rparen);
    res;
  end if;
end method convert-defstruct-name;

// Consumes the end-paren which corresponds to a start paren this
// function never sees
//
define method ignore-up-to-end-paren (input :: <stream>)
 => ignored-text :: <string>;
  let res-stream = make(<buffered-byte-string-output-stream>);
  block (break)
    let paren-count = 1;
    while (#t)
      let lexeme = peek-lex(input);
      format(res-stream, "%= ", lexeme);
      if (lexeme == $lparen)
	paren-count := paren-count + 1;
      elseif (lexeme == $rparen)
	paren-count := paren-count - 1;
	if (paren-count == 0)
	  break();  // don't consume last end-paren
	end if;
      end if;
//      format(*standard-output*, "ignoring ");
      force-output(*standard-output*);
      lex(input);
    end while;
  end block;
  res-stream.stream-contents;
end method ignore-up-to-end-paren;
  

define method convert-slot (input :: <stream>) => converted :: <string>;
  assert(lex(input) == $lparen);
  let res-stream = make(<buffered-byte-string-output-stream>);
  let slotname = lex(input);
  assert(instance?(slotname, <identifier>));
  let default-value = convert(input);
  let res 
    = if (peek-lex(input) ~== $rparen)
	let keyword :: <keyword> = lex(input);
	assert(keyword.keyword-string = "type");
	let type = convert-type(input);
	if (peek-lex(input) ~== $rparen)
	  let ignored-text = ignore-up-to-end-paren(input);
	  format-to-string("  slot %s :: %s = %s; /" "/ %s\n",
			   slotname.id-name, type, default-value,
			   ignored-text);
	else
	  format-to-string("  slot %s :: %s = %s;\n",
			   slotname.id-name, type, default-value);
	end if;
      else
	format-to-string("  slot %s = %s;\n", slotname, default-value);
      end if;
  assert(lex(input) == $rparen);
  res;
end method convert-slot;

define method convert-type (input :: <stream>) => converted :: <string>;
  let lexeme = lex(input);
  if (lexeme == $lparen)
    let lexeme = lex(input);
    if (lexeme.id-name = "member")
      let res = convert-funcall("one-of", input);
      assert(lex(input) == $rparen);
      res;
    elseif (lexeme.id-name = "or")
      let res = convert-funcall("type-union", input);
      assert(lex(input) == $rparen);
      res;
    else
      assert(lexeme.id-name = "integer");
      ignore-up-to-end-paren(input);
      assert(lex(input) == $rparen);
      "<integer>";
    end if;
  else
    concatenate("<", lexeme.id-name, ">");
  end if;
end method convert-type;

// binop has already been read, and operator is the Dylan equivalent
//
define method convert-binop (operator :: <string>, input :: <stream>)
 => converted :: <string>;
  let parameters = make(<stretchy-vector>);
  for (lexeme = peek-lex(input) then peek-lex(input),
       until: lexeme == $rparen)
    add!(parameters, convert(input));
  end for;
  let glue-string = concatenate(" ", operator, " ");
  let joined-params = apply(join, glue-string, parameters);
  concatenate("(", joined-params, ")");
end method convert-binop;

// aref is also used on vectors in Lisp
//
define method convert-aref (input :: <stream>) => converted :: <string>;
  assert(lex(input).id-name = "aref");
  let sequence = convert(input);
  assert(peek-lex(input) ~== $rparen);
  format-to-string("%s[%s]", sequence, convert-param-list(input));
end method convert-aref;

// doesn't handle defaults for gethash
//
define method convert-gethash (input :: <stream>) => converted :: <string>;
  assert(lex(input).id-name = "gethash");
  let key = convert(input);
  let table = convert(input);
  format-to-string("%s[%s]", table, key);
end method convert-gethash;

define method convert-funcall (function :: <string>, input :: <stream>)
 => converted :: <string>;
  format-to-string("%s(%s)", function, convert-param-list(input));
end method convert-funcall;

// As usual, opening paren has already been consumed
//
define method convert-param-list (input :: <stream>) => converted :: <string>;
  let parameters = make(<stretchy-vector>);
  for (lexeme = peek-lex(input) then peek-lex(input),
       until: lexeme == $rparen)
    if (instance?(lexeme, <keyword>))
      lex(input);  // consume keyword
      if (peek-lex(input) ~== $rparen)
	add!(parameters, concatenate(lexeme.keyword-string, ": ", 
				     convert(input)))
      else
	add!(parameters, concatenate("#\"", lexeme.keyword-string, "\""));
      end if;
    else
      add!(parameters, convert(input));
    end if;
  end for;
  apply(join, ", ", parameters);
end method convert-param-list;

define method convert-ignored (input :: <stream>) => converted :: <string>;
  let comment = concatenate("/" "/ ", ignore-up-to-end-paren(input));
  "";
end method convert-ignored;

// ### We use our own assert, because we're pretty naughty about how
// we use it.  Specifically, the expression inside the assert()
// usually has side-effects.
//
define function assert (value) => ();
  unless (value)
    error("Assertion failed.");
  end;
end function assert;

define method main (ignored, #rest args)
  if (args.size ~== 1)
    format(*standard-error*, "Usage: lisp2dylan file.lisp > output.dylan\n");
    force-output(*standard-error*);
    exit(exit-code: 1);
  end if;
  let input = make(<file-stream>, locator: args.first);
  block ()
    while (#t)
      let output = convert(input);
      format(*standard-output*, "\n\n%s\n\n", output);
      force-output(*standard-output*);
    end while;
  exception (<end-of-stream-error>)
    // ### We assume nothing of significance is pending when we hit
    // eof.  If there was, it probably wouldn't be a legal Lisp
    // program.
    format(*standard-output*, "All done.\n");
    force-output(*standard-output*);
  end block;
  exit(exit-code: 0);
end method main;
