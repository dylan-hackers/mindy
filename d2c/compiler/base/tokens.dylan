module: tokens
rcs-header: $Header: /scm/cvs/src/d2c/compiler/base/tokens.dylan,v 1.1 1998/05/03 19:55:30 andreas Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
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

// token classes.

// <token> -- exported.
// 
// All the different tokens returned by the tokenizer are all
// instances the class <token>.  The different kinds of tokens are
// represented via an enumeration of integer constants.
//
define class <token> (<source-location-mixin>)
  //
  // Integer indicating the syntactic category for this token.
  constant slot token-kind :: <integer>,
    required-init-keyword: #"kind";
end;

define sealed domain make (singleton(<token>));
define sealed domain initialize (<token>);

define sealed method print-object
    (token :: <token>, stream :: <stream>) => ();
  pprint-fields(token, stream, kind: token.token-kind);
end method print-object;

// The token-kind values.  Note: if you change these values in any way,
// you *must* update the set of tokens in parser/parser.input correspondingly.
//
define constant $eof-token = 0;
define constant $error-token = 1;
//
define constant $left-paren-token = 2;
define constant $right-paren-token = 3;
define constant $comma-token = 4;
define constant $dot-token = 5;
define constant $semicolon-token = 6;
define constant $left-bracket-token = 7;
define constant $right-bracket-token = 8;
define constant $left-brace-token = 9;
define constant $right-brace-token = 10;
define constant $double-colon-token = 11;
define constant $minus-token = 12;
define constant $equal-token = 13;
define constant $double-equal-token = 14;
define constant $arrow-token = 15;
define constant $sharp-paren-token = 16;
define constant $sharp-bracket-token = 17;
define constant $double-sharp-token = 18;
define constant $question-token = 19;
define constant $double-question-token = 20;
define constant $question-equal-token = 21;
define constant $ellipsis-token = 22;
//
define constant $true-token = 23;
define constant $false-token = 24;
define constant $next-token = 25;
define constant $rest-token = 26;
define constant $key-token = 27;
define constant $all-keys-token = 28;
define constant $include-token = 29;
//
define constant $define-token = 30;
define constant $end-token = 31;
define constant $handler-token = 32;
define constant $let-token = 33;
define constant $local-token = 34;
define constant $macro-token = 35;
define constant $otherwise-token = 36; 
//
define constant $raw-ordinary-word-token = 37;
define constant $raw-begin-word-token = 38;
define constant $raw-function-word-token = 39;
define constant $ordinary-define-body-word-token = 40;
define constant $begin-and-define-body-word-token = 41;
define constant $function-and-define-body-word-token = 42;
define constant $ordinary-define-list-word-token = 43;
define constant $begin-and-define-list-word-token = 44;
define constant $function-and-define-list-word-token = 45;
define constant $quoted-name-token = 46;
//
define constant $constrained-name-token = 47;
//
define constant $tilde-token = 48;
define constant $other-binary-operator-token = 49;
//
define constant $literal-token = 50;
define constant $string-token = 51;
define constant $symbol-token = 52;
//
define constant $parsed-definition-macro-call-token = 53;
define constant $parsed-special-definition-token = 54;
define constant $parsed-local-declaration-token = 55;
define constant $parsed-expression-token = 56;
define constant $parsed-constant-token = 57;
define constant $parsed-macro-call-token = 58;
define constant $parsed-parameter-list-token = 59;
define constant $parsed-variable-list-token = 60;
//
define constant $feature-if-token = 61;
define constant $feature-elseif-token = 62;
define constant $feature-else-token = 63;
define constant $feature-endif-token = 64;

// <symbol-token> -- exported.
//
// The various tokens that have a symbol name.
//
define class <symbol-token> (<token>)
  //
  // The symbol name for the token.
  constant slot token-symbol :: <symbol>,
    required-init-keyword: symbol:;
end class <symbol-token>;

define sealed domain make (singleton(<symbol-token>));

define sealed method print-object
    (token :: <symbol-token>, stream :: <stream>) => ();
  pprint-fields(token, stream, kind: token.token-kind,
		symbol: token.token-symbol);
end method print-object;

// <identifier-token> -- exported.
//
// Tokens that can be used as identifiers.
//
define class <identifier-token> (<symbol-token>)
  //
  constant slot token-module :: false-or(<module>) = #f,
    init-keyword: module:;
  //
  constant slot token-uniquifier :: false-or(<uniquifier>) = #f,
    init-keyword: uniquifier:;
end class <identifier-token>;

define sealed domain make (singleton(<identifier-token>));

define sealed method print-object
    (token :: <identifier-token>, stream :: <stream>) => ();
  let mod = token.token-module;
  let uniq = token.token-uniquifier;
  pprint-fields(token, stream,
		kind: token.token-kind,
		symbol: token.token-symbol,
		if (mod) module: end, mod,
		if (uniq) uniquifier: end, uniq);
end;


// <uniquifier> -- exported.
//
define class <uniquifier> (<identity-preserving-mixin>)
end;

define sealed domain make (singleton(<uniquifier>));
define sealed domain initialize (<uniquifier>);


// same-id? -- exported.
// 
define method same-id? (id1 :: <identifier-token>, id2 :: <identifier-token>)
    => res :: <boolean>;
  id1.token-symbol == id2.token-symbol
    & id1.token-module == id2.token-module
    & id1.token-uniquifier == id2.token-uniquifier;
end;


define class <operator-token> (<identifier-token>)
  //
  // The precedence of this operator.  The higher the number, the tighter
  // the binding.
  slot operator-precedence :: <integer> = 0;
  //
  // The associativity of the operator, #"left" or #"right".
  slot operator-associativity :: one-of(#"left", #"right") = #"left";
end class <operator-token>;

define sealed domain make (singleton(<operator-token>));

define sealed method print-object
    (token :: <operator-token>, stream :: <stream>) => ();
  let mod = token.token-module;
  let uniq = token.token-uniquifier;
  pprint-fields(token, stream,
		kind: token.token-kind,
		symbol: token.token-symbol,
		if (mod) module: end, mod,
		if (uniq) uniquifier: end, uniq,
		precedence: token.operator-precedence,
		associativity: token.operator-associativity);
end method print-object;

define constant $operator-info :: <self-organizing-list>
  = begin
      let table = make(<self-organizing-list>);
      table[#"^"] := #(5 . #"left");
      table[#"*"] := #(4 . #"left");
      table[#"/"] := #(4 . #"left");
      table[#"+"] := #(3 . #"left");
      table[#"-"] := #(3 . #"left");
      table[#"="] := #(2 . #"left");
      table[#"=="] := #(2 . #"left");
      table[#"~="] := #(2 . #"left");
      table[#"~=="] := #(2 . #"left");
      table[#"<"] := #(2 . #"left");
      table[#">"] := #(2 . #"left");
      table[#"<="] := #(2 . #"left");
      table[#">="] := #(2 . #"left");
      table[#"&"] := #(1 . #"right");
      table[#"|"] := #(1 . #"right");
      table[#":="] := #(0 . #"right");
      table;
    end;

define method initialize (op :: <operator-token>, #key symbol) => ();
  let info = $operator-info[symbol];
  op.operator-precedence := info.head;
  op.operator-associativity := info.tail;
end method initialize;


// <constrained-name-token> -- exported.
//
// A constrained name, used by the macro system.
//
define class <constrained-name-token> (<symbol-token>)
  //
  // The constraint, as a symbol.
  constant slot token-constraint :: <symbol>,
    required-init-keyword: constraint:;
end;

define sealed domain make (singleton(<constrained-name-token>));

define sealed method print-object
    (token :: <constrained-name-token>, stream :: <stream>) => ();
  pprint-fields(token, stream,
		kind: token.token-kind,
		symbol: token.token-symbol,
		constraint: token.token-constraint);
end method print-object;


// <literal-token> -- exported.
//
// A literal value, e.g. a string, character, number, or symbol.
//
define class <literal-token> (<token>)
  //
  // The literal this token is.
  constant slot token-literal :: <literal>,
    required-init-keyword: literal:;
end class <literal-token>;

define sealed domain make (singleton(<literal-token>));

define sealed method print-object
    (token :: <literal-token>, stream :: <stream>) => ();
  pprint-fields(token, stream,
		kind: token.token-kind,
		literal: token.token-literal);
end method print-object;


// <pre-parsed-token> -- exported.
//
define class <pre-parsed-token> (<token>)
  //
  // The piece of parse tree this token represents.
  slot token-parse-tree :: <object>,
    required-init-keyword: parse-tree:;
end class <pre-parsed-token>;

define sealed domain make (singleton(<pre-parsed-token>));

define sealed method print-object
    (token :: <pre-parsed-token>, stream :: <stream>) => ();
  pprint-fields(token, stream,
		kind: token.token-kind,
		parse-tree: token.token-parse-tree);
end method print-object;


// Print-message for tokens.

define sealed method print-message
    (wot :: <token>, stream :: <stream>) => ();
  select (wot.token-kind)
    $eof-token => write(stream, "EOF");
    $error-token => write(stream, "bogus token");

    $left-paren-token => write(stream, "left parenthesis");
    $right-paren-token => write(stream, "right parenthesis");
    $comma-token => write(stream, "comma");
    $dot-token => write(stream, "dot");
    $semicolon-token => write(stream, "semicolon");
    $left-bracket-token => write(stream, "left bracket");
    $right-bracket-token => write(stream, "right bracket");
    $left-brace-token => write(stream, "left brace");
    $right-brace-token => write(stream, "right brace");
    $double-colon-token => write(stream, "double colon");
    $minus-token => write(stream, "minus");
    $equal-token => write(stream, "equal");
    $double-equal-token => write(stream, "double equal");
    $arrow-token => write(stream, "arrow");
    $sharp-paren-token => write(stream, "sharp paren");
    $sharp-bracket-token => write(stream, "sharp bracket");
    $double-sharp-token => write(stream, "double sharp");
    $question-token => write(stream, "question mark");
    $double-question-token => write(stream, "double question mark");
    $question-equal-token => write(stream, "question mark equal");
    $ellipsis-token => write(stream, "ellipsis");

    $true-token => write(stream, "#t");
    $false-token => write(stream, "#f");
    $next-token => write(stream, "#next");
    $rest-token => write(stream, "#rest");
    $key-token => write(stream, "#key");
    $all-keys-token => write(stream, "#all-keys");
    $include-token => write(stream, "#include");

    $define-token => write(stream, "core word ``define''");
    $end-token => write(stream, "core word ``end''");
    $handler-token => write(stream, "core word ``handler''");
    $let-token => write(stream, "core word ``let''");
    $local-token => write(stream, "core word ``local''");
    $macro-token => write(stream, "core word ``macro''");
    $otherwise-token => write(stream, "core word ``otherwise''");

    $raw-ordinary-word-token =>
      format(stream, "ordinary word ``%s''", wot.token-symbol);
    $raw-begin-word-token =>
      format(stream, "begin word ``%s''", wot.token-symbol);
    $raw-function-word-token =>
      format(stream, "function word ``%s''", wot.token-symbol);
    $ordinary-define-body-word-token =>
      format(stream, "ordinary define body word ``%s''", wot.token-symbol);
    $begin-and-define-body-word-token =>
      format(stream, "begin and define body word ``%s''", wot.token-symbol);
    $function-and-define-body-word-token =>
      format(stream, "function and define body word ``%s''", wot.token-symbol);
    $ordinary-define-list-word-token =>
      format(stream, "ordinary define list word ``%s''", wot.token-symbol);
    $begin-and-define-list-word-token =>
      format(stream, "begin and define list word ``%s''", wot.token-symbol);
    $function-and-define-list-word-token =>
      format(stream, "function and define list word ``%s''", wot.token-symbol);
    $quoted-name-token =>
      format(stream, "quoted name ``%s''", wot.token-symbol);

    $constrained-name-token =>
      format(stream, "constrained name ``%s:%s''",
	     wot.token-symbol, wot.token-constraint);

    $tilde-token =>
      write(stream, "tilde");
    $other-binary-operator-token =>
      format(stream, "binary operator ``%s''", wot.token-symbol);

    $literal-token =>
      format(stream, "literal ``%s''", wot.token-literal);
    $string-token =>
      format(stream, "string literal ``%s''", wot.token-literal);
    $symbol-token =>
      format(stream, "symbol literal ``%s''", wot.token-literal);

    $parsed-definition-macro-call-token =>
      write(stream, "parsed definition macro call");
    $parsed-special-definition-token =>
      write(stream, "parsed definition");
    $parsed-local-declaration-token =>
      write(stream, "parsed local declaration");
    $parsed-expression-token =>
      write(stream, "parsed expression");
    $parsed-constant-token =>
      write(stream, "parsed-constant");
    $parsed-macro-call-token =>
      write(stream, "parsed macro call");
    $parsed-parameter-list-token =>
      write(stream, "parsed parameter list");
    $parsed-variable-list-token =>
      write(stream, "parsed variable list");

    $feature-if-token => write(stream, "#if");
    $feature-elseif-token => write(stream, "#elseif");
    $feature-else-token => write(stream, "#else");
    $feature-endif-token => write(stream, "#endif");

    otherwise =>
      error("Unknown token kind.");
  end select;
end method print-message;



// Syntax Tables.

define constant <word-category>
  = one-of(#"core", #"ordinary", #"begin", #"function",
	   #"define-body", #"define-list");

define class <word-info> (<object>)
  //
  // Vector of symbols describing the categories this word drops into.
  constant slot word-info-categories :: <simple-object-vector>,
    required-init-keyword: categories:;
  //
  // token-kind for this kind of word.
  constant slot word-info-token-kind :: <integer>,
    required-init-keyword: kind:;
  //
  // Self-organizing-list mapping additional categories to the <word-info>
  // for words in that category plus all of this words categories.
  constant slot word-info-sub-infos :: <self-organizing-list>
    = make(<self-organizing-list>);
end class <word-info>;
  
define sealed domain make (singleton(<word-info>));
define sealed domain initialize (<word-info>);

define constant $default-word-info :: <word-info>
  = make(<word-info>, categories: #[], kind: $raw-ordinary-word-token);

begin
  local
    method add-sub-category
	(to :: <word-info>, category :: <word-category>,
	 kind :: <integer>)
	=> (sub-category :: <word-info>);
      let categories = add(to.word-info-categories, category);
      let sub-info = make(<word-info>, categories: categories, kind: kind);
      to.word-info-sub-infos[category] := sub-info;
      for (category in categories)
	sub-info.word-info-sub-infos[category] := sub-info;
      end for;
      sub-info;
    end method add-sub-category;
  for (category in #[#"ordinary", #"begin", #"function"],
       kind from $raw-ordinary-word-token)
    let sub-info = add-sub-category($default-word-info, category, kind);
    for (sub-category in #[#"define-body", #"define-list"],
	 delta from 3 by 3)
      add-sub-category(sub-info, sub-category, kind + delta);
    end for;
  end for;
  for (category in #[#"define-body", #"define-list"],
       kind from $ordinary-define-body-word-token by 3)
    let sub-info = add-sub-category($default-word-info, category, kind);
    for (sub-category in #[#"ordinary", #"begin", #"function"],
	 delta from 0)
      add-sub-category(sub-info, sub-category, kind + delta);
    end for;
  end for;
end;

define class <core-word-info> (<word-info>)
  //
  // The core-word this is the info for.
  slot core-word-info-word :: <symbol>, required-init-keyword: word:;
end class <core-word-info>;

define sealed domain make (singleton(<core-word-info>));

define constant $core-word-infos :: <simple-object-vector>
  = map-as(<simple-object-vector>,
	   method (core-word :: <symbol>, kind :: <integer>)
	       => res :: <core-word-info>;
	     make(<core-word-info>,
		  categories: #[#"core"],
		  kind: kind,
		  word: core-word);
	   end method,
	   #[#"define", #"end", #"handler", #"let",
	     #"local", #"macro", #"otherwise"],
	   make(<range>, from: $define-token));


define class <syntax-table> (<object>)
  //
  // object-table mapping symbols to word-infos.
  constant slot syntax-table-entries :: <object-table>
    = make(<object-table>);
end class <syntax-table>;

define sealed domain make (singleton(<syntax-table>));
define sealed domain initialize (<syntax-table>);

define method initialize (table :: <syntax-table>, #key) => ();
  for (info in $core-word-infos)
    table.syntax-table-entries[info.core-word-info-word] := info;
  end for;
end method initialize;


// syntax-for-name -- exported.
//
// Return the token kind and set of categories for given name.
// 
define method syntax-for-name (table :: <syntax-table>, name :: <symbol>)
    => (kind :: <integer>, categories :: <simple-object-vector>);
  let entry = element(table.syntax-table-entries, name,
		      default: $default-word-info);
  values(entry.word-info-token-kind, entry.word-info-categories);
end method syntax-for-name;


// problem-with-category-merge -- exported.
//
// Return the category that would clashe with new category if we were to
// try to merge them, or #f if the merge is okay.
// 
define method problem-with-category-merge
    (table :: <syntax-table>, word :: <symbol>, category :: <word-category>)
    => problem :: false-or(<word-category>);
  let current = element(table.syntax-table-entries, word,
			default: $default-word-info);
  let current-categories = current.word-info-categories;
  let new = element(current.word-info-sub-infos, category, default: #f);
  if (new)
    #f;
  else
    if (current-categories.size == 1)
      current-categories.first;
    else
      block (return)
	let just-new = $default-word-info.word-info-sub-infos[category];
	for (current-category in current-categories)
	  unless (element(just-new.word-info-sub-infos, current-category,
			  default: #f))
	    return(current-category);
	  end unless;
	end for;
	error("Can't merge %s with %=, but can't tell why.",
	      category, current-categories);
      end block;
    end if;
  end if;
end method problem-with-category-merge;


// merge-category -- exported.
//
// Note that word is also of the given category.
// 
define method merge-category
    (table :: <syntax-table>, word :: <symbol>, category :: <word-category>)
    => ();
  let current = element(table.syntax-table-entries, word,
			default: $default-word-info);
  table.syntax-table-entries[word] := current.word-info-sub-infos[category];
end method merge-category;



// Library dump support.

define constant $token-slots
  = list(source-location, source-location:, #f,
	 token-kind, kind:, #f);

add-make-dumper(#"token", *compiler-dispatcher*, <token>,
		$token-slots);

define constant $symbol-token-slots
  = concatenate($token-slots,
		list(token-symbol, symbol:, #f));

define constant $identifier-token-slots
  = concatenate($symbol-token-slots,
		list(token-module, module:, #f,
		     token-uniquifier, uniquifier:, #f));

add-make-dumper(#"identifier-token", *compiler-dispatcher*, <identifier-token>,
		$identifier-token-slots);

add-make-dumper(#"uniquifier", *compiler-dispatcher*, <uniquifier>, #(),
		load-external: #t);

// We don't need to dump the precedence and associativity of operators because
// they are a static property of the symbol.  We only have them as slots at
// all so that they can be more effeciently accessed.
//
add-make-dumper(#"operator-token", *compiler-dispatcher*, <operator-token>,
		$identifier-token-slots);

add-make-dumper(#"constrained-name-token", *compiler-dispatcher*,
		<constrained-name-token>,
		concatenate($symbol-token-slots,
			    list(token-constraint, constraint:, #f)));

add-make-dumper(#"literal-token", *compiler-dispatcher*, <literal-token>,
		concatenate($token-slots,
			    list(token-literal, literal:, #f)));

add-make-dumper(#"pre-parsed-token", *compiler-dispatcher*, <pre-parsed-token>,
		concatenate($token-slots,
			    list(token-parse-tree, parse-tree:, #f)));
