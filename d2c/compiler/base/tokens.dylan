module: tokens
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/tokens.dylan,v 1.9 1996/01/10 14:59:26 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


// token classes.

// <token> -- exported.
// 
// All the different tokens returned by the tokenizer are all
// (indirect) instances the class <token>.  The different syntactic
// categories for tokens are represent by the direct class of the
// token.  Tokens that fall into more than one syntactic category (for
// example, -, which is punctuation, a unary operator, and a binary
// operator) are implemented by using multiple inheritance.
//
define primary abstract class <token> (<source-location-mixin>)
end;


// <eof-token> -- exported.
//
// And, of course, we have to somehow represent the end of the file.
//
define class <eof-token> (<token>)
end;

// <error-token> -- exported.
//
// Used when the lexer fails to figure anything better out.  Will
// only happen if strange characters (non-whitespace control chars or
// chars with the high bit set for example) show up in the input.
// 
define class <error-token> (<token>)
end;

// <symbol-token> -- exported.
//
// Any symbol based token.
//
define abstract class <symbol-token> (<token>)
  //
  // The word as a symbol.
  slot token-symbol :: <symbol>, required-init-keyword: symbol:;
end;

// token-symbol -- exported.
//
// Return the word this token is for as a symbol.
//
define generic token-symbol (token :: <symbol-token>) => res :: <symbol>;

// <identifier-token> -- exported.
// 
// Tokens that may need to be used to identify variables.
//
define abstract class <identifier-token> (<symbol-token>)
  //
  // The module this name should be looked up in if interpreted as a free
  // reference, or #f if a generated name (and hence, not really from any
  // module).
  slot token-module :: false-or(<module>),
    init-value: #f, init-keyword: module:;
  //
  // A uniquifier.
  slot token-uniquifier :: false-or(<uniquifier>),
    init-value: #f, init-keyword: uniquifier:;
end;
  
define method print-object (id :: <identifier-token>, stream :: <stream>)
    => ();
  let mod = id.token-module;
  let uniq = id.token-uniquifier;
  pprint-fields(id, stream,
		symbol: id.token-symbol,
		if (mod) module: end, mod,
		if (uniq) uniquifier: end, uniq);
end;

// token-module -- exported.
//
// Return the module (or #f if none) the name token should be looked
// up in when used as a free reference.
//
define generic token-module (token :: <identifier-token>)
 => res :: false-or(<module>);

// token-uniquifier -- exported.
//
define generic token-uniquifier (token :: <identifier-token>)
    => res :: false-or(<uniquifier>);

// <uniquifier> -- exported.
//
define class <uniquifier> (<object>)
end;

// same-id? -- exported.
// 
define method same-id? (id1 :: <identifier-token>, id2 :: <identifier-token>)
  id1.token-symbol == id2.token-symbol
    & id1.token-module == id2.token-module
    & id1.token-uniquifier == id2.token-uniquifier;
end;

define abstract class <word-token> (<identifier-token>)
end;

define abstract class <name-token> (<word-token>)
end;

define method make (c == <name-token>, #rest keys, #all-keys)
    => res :: <name-token>;
  apply(make, <simple-name-token>, keys);
end;

define class <simple-name-token> (<name-token>)
end;

define class <quoted-name-token> (<simple-name-token>)
end;

define class <begin-word-token> (<word-token>)
end;

define abstract class <define-word-token> (<word-token>)
end;

define method make (c == <define-word-token>, #rest keys, #all-keys)
    => res :: <define-word-token>;
  apply(make, <define-word-and-name-token>, keys);
end;

define abstract class <define-bindings-word-token> (<word-token>)
end;

define method make (c == <define-bindings-word-token>, #rest keys, #all-keys)
    => res :: <define-bindings-word-token>;
  apply(make, <define-bindings-word-and-name-token>, keys);
end;

define class <define-word-and-name-token> (<define-word-token>, <name-token>)
end;

define class <define-bindings-word-and-name-token>
    (<define-bindings-word-token>, <name-token>)
end;

define class <define-and-begin-word-token>
    (<define-word-token>, <begin-word-token>)
end;

define class <define-bindings-and-begin-word-token>
    (<define-bindings-word-token>, <begin-word-token>)
end;

// <constrained-name-token> -- exported.
//
// A constrained name, used by the macro system.
//
define class <constrained-name-token> (<symbol-token>)
  //
  // The constraint, as a symbol.
  slot token-constraint :: <symbol>, required-init-keyword: constraint:;
end;

define method print-object (token :: <constrained-name-token>,
			    stream :: <stream>)
    => ();
  pprint-fields(token, stream,
		symbol: token.token-symbol,
		constraint: token.token-constraint);
end;

// <core-word-token>, etc. -- all exported.
//
// The various core words.
//
define abstract class <core-word-token> (<symbol-token>)
end;

define method print-object (token :: <core-word-token>, stream :: <stream>)
    => ();
  pprint-fields(token, stream, symbol: token.token-symbol);
end;

define class <begin-token> (<core-word-token>) end;
define class <bind-exit-token> (<core-word-token>) end;
define class <class-token> (<core-word-token>) end;
define class <cleanup-token> (<core-word-token>) end;
define class <constant-token> (<core-word-token>) end;
define class <create-token> (<core-word-token>) end;
define class <define-token> (<core-word-token>) end;
define class <else-token> (<core-word-token>) end;
define class <end-token> (<core-word-token>) end;
define class <export-token> (<core-word-token>) end;
define class <finally-token> (<core-word-token>) end;
define class <for-token> (<core-word-token>) end;
define class <from-token> (<core-word-token>) end;
define class <generic-token> (<core-word-token>) end;
define class <handler-token> (<core-word-token>) end;
define class <if-token> (<core-word-token>) end;
define class <in-token> (<core-word-token>) end;
define class <let-token> (<core-word-token>) end;
define class <library-token> (<core-word-token>) end;
define class <local-token> (<core-word-token>) end;
define class <macro-token> (<core-word-token>) end;
define class <method-token> (<core-word-token>) end;
define class <module-token> (<core-word-token>) end;
define class <mv-call-token> (<core-word-token>) end;
define class <otherwise-token> (<core-word-token>) end;
define class <primitive-token> (<core-word-token>) end;
define class <seal-token> (<core-word-token>) end;
define class <set-token> (<core-word-token>) end;
define class <use-token> (<core-word-token>) end;
define class <uwp-token> (<core-word-token>) end;
define class <variable-token> (<core-word-token>) end;
define class <while-token> (<core-word-token>) end;

// <abstract-literal-token> -- exported.
//
// Some kind of literal.
//
define class <abstract-literal-token> (<token>)
  //
  // The literal this is a literal token of.
  slot token-literal :: <literal>, required-init-keyword: literal:;
end;

define method print-object (token :: <abstract-literal-token>,
			    stream :: <stream>)
    => ();
  pprint-fields(token, stream, literal: token.token-literal);
end;

// token-literal -- exported.
//
define generic token-literal (token :: <abstract-literal-token>) => result;

// <keyword-token> -- exported.
//
// A keyword, either foo: or #"foo" syntax.
// 
define class <keyword-token> (<abstract-literal-token>)
end;

// <literal-token> -- exported.
//
// Random literal things that don't need to be distinguished by the parser.
// In other words, numbers and characters.
// 
define class <literal-token> (<abstract-literal-token>)
end;

// <string-token> -- exported.
//
// A string.  Seperate from <literal-token> because the grammar allows
// two strings to be juxtaposed.
// 
define class <string-token> (<abstract-literal-token>)
end;

// <operator-token>, <unary-operator-token>, <binary-operator-token>
//   -- all exported.
//
// An operator, either binary or unary.
// 
define abstract class <operator-token> (<identifier-token>)
end;

define abstract class <binary-operator-token> (<operator-token>)
  slot operator-precedence :: <integer>;
  slot operator-left-associative? :: <boolean>;
end;

define method make (wot == <binary-operator-token>, #rest keys, #key)
    => res :: <binary-operator-token>;
  apply(make, <simple-binary-operator-token>, keys);
end;

define constant $operator-info
  = begin
      let table = make(<self-organizing-list>);
      table[#"^"] := #(5 . #t);
      table[#"*"] := #(4 . #t);
      table[#"/"] := #(4 . #t);
      table[#"+"] := #(3 . #t);
      table[#"-"] := #(3 . #t);
      table[#"="] := #(2 . #t);
      table[#"=="] := #(2 . #t);
      table[#"~="] := #(2 . #t);
      table[#"~=="] := #(2 . #t);
      table[#"<"] := #(2 . #t);
      table[#">"] := #(2 . #t);
      table[#"<="] := #(2 . #t);
      table[#">="] := #(2 . #t);
      table[#"&"] := #(1 . #f);
      table[#"|"] := #(1 . #f);
      table[#":="] := #(0 . #f);
      table;
    end;

define method initialize
    (binop :: <binary-operator-token>, #next next-method, #key) => ();
  next-method();
  let info = $operator-info[binop.token-symbol];
  binop.operator-precedence := head(info);
  binop.operator-left-associative? := tail(info);
end;

define class <simple-binary-operator-token> (<binary-operator-token>)
end;

define abstract class <unary-operator-token> (<operator-token>)
end;

define class <tilde-token> (<unary-operator-token>)
end;

// <punctuation-token>, etc. -- all exported.
//
// The various different kinds of punctuation.  Again, these are all
// different classes because the parser needs to know about them.
// 
define abstract class <punctuation-token> (<token>)
end;

define class <left-paren-token> (<punctuation-token>)
end;

define class <right-paren-token> (<punctuation-token>)
end;

define class <comma-token> (<punctuation-token>)
end;

define class <dot-token> (<punctuation-token>)
end;

define class <semicolon-token> (<punctuation-token>)
end;

define class <left-bracket-token> (<punctuation-token>)
end;

define class <right-bracket-token> (<punctuation-token>)
end;

define class <left-brace-token> (<punctuation-token>)
end;

define class <right-brace-token> (<punctuation-token>)
end;

define class <double-colon-token> (<punctuation-token>)
end;

define class <arrow-token> (<punctuation-token>)
end;

define class <sharp-paren-token> (<punctuation-token>)
end;

define class <sharp-bracket-token> (<punctuation-token>)
end;

define class <question-token> (<punctuation-token>)
end;

define class <double-question-token> (<punctuation-token>)
end;

define class <ellipsis-token> (<punctuation-token>)
end;

// <minus-token>, <equal-token>, <double-equal-token> -- all exported.
//
// These all fall into multiple syntactic categories.  Luckily, we
// were able to tweek the grammar enough that the lack of disjointness
// doesn't cause any ambiguities.
// 
define class <minus-token>
    (<punctuation-token>, <binary-operator-token>, <unary-operator-token>)
end;

define class <equal-token> (<punctuation-token>, <binary-operator-token>)
end;

define class <double-equal-token>
    (<punctuation-token>, <binary-operator-token>)
end;

// <sharp-word-token>, etc. -- all exported.
//
// The various different #sharp words.  If it were just up to me, I
// would have called all these punctuation, but Apple called them
// something different, so I maintained that seperation here.  It
// doesn't matter at all, because the parser never looks at either
// <punctunation-token> or <sharp-word-token>, just the different
// specific kinds of punctuation or sharp words.
// 
define abstract class <sharp-word-token> (<token>)
end;

define class <true-token> (<sharp-word-token>)
end;

define class <false-token> (<sharp-word-token>)
end;

define class <next-token> (<sharp-word-token>)
end;

define class <rest-token> (<sharp-word-token>)
end;

define class <key-token> (<sharp-word-token>)
end;

define class <all-keys-token> (<sharp-word-token>)
end;


// Syntax Tables.

define constant $word-categories$
  = vector(<name-token>,
	   <begin-word-token>,
	   <define-word-token>,
	   <define-bindings-word-token>,
	   <define-word-and-name-token>,
	   <define-bindings-word-and-name-token>,
	   <define-and-begin-word-token>,
	   <define-bindings-and-begin-word-token>);

define method merge-category(table :: <table>,
			     name :: <symbol>,
			     category :: <class>)
  let current = element(table, name, default: category);
  if (subtype?(current, category))
    current;
  else
    block (return)
      for (test in $word-categories$)
	if (subtype?(test, category) & subtype?(test, current))
	  return(test);
	end;
      finally
	#f;
      end;
    end;
  end;
end;


// Tokenizer interface.

define primary abstract class <tokenizer> (<object>)
end;

define generic get-token (tokenizer :: <tokenizer>) => token :: <token>;

define generic unget-token (tokenizer :: <tokenizer>, token :: <token>) => ();

