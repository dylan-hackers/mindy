documented: #t
module: c-parse
author:  Robert Stockton (rgs@cs.cmu.edu)
copyright: Copyright (C) 1994, Carnegie Mellon University
	   All rights reserved.
	   This code was produced by the Gwydion Project at Carnegie Mellon
	   University.  If you are interested in using this code, contact
	   "Scott.Fahlman@cs.cmu.edu" (Internet).
rcs-header: $Header: 

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
// All rights reserved.
//
//======================================================================

//======================================================================
// Module c-parse handles parsing of native C header files.  Theoretically, we
// could have arbitrary C code in these files.  However, in practice we only
// need to be able to parse about half of the C language.
//
// The actual "source" for the c parser is contained in "c-parse.input", which
// is an input file for a lisp-based Dylan parser generator.  (This is a
// logical route to follow for bootstrapping purposes.  It would clearly be
// useful to re-implement the generator in Dylan at some future date.)
// "C-parse.dylan" is the pure Dylan output of this generator, and should
// *not* be considered human readable code.
//
// The "parse-tree" is a very ad-hoc structure, because it is intended to be
// quite ephemeral.  The final goal is to add an ordered sequence of
// <declaration>s to the <parse-state> which both controls the parsing process
// and returns the results of the parse.  This approach allows us to do a
// single pass parse of the data, and also support the feedback required to
// handle C's "type names", which are context sensitive.
//
// The token stream provided by the tokenizer may actually correspond to
// several different "included" files.  We can detect transitions into and out
// of recursively included files by watching for <begin-include-token> and
// <end-include-token>.  We make the simplifying assumption that declarations
// will not be split across include files.  This assumption should be
// justifiable on the basis that any exception would be unbearably ugly.
//
// The raw parse engine has been fitted with a number of different front ends
// to allow several different types of data to be parsed.  The following
// functions are exported:
//   parse(filename, #key defines, undefines) => result :: <parse-file-state>
//     This function processes an entire include file, leaving a series of
//     declarations in the returned parse state.
//   parse-type(alien-name :: <string>, old-state :: <parse-file-state>)
//   => result :: <declaration>
//     This function parses the contents of the given string and tries to
//     interpret it as the name of an object or type declared in "old-state".
//     Parse-type will signal an error if no such declaration is found.
//   parse-macro(cpp-name :: <string>, old-state :: <parse-file-state>)
//   => result :: constant-value or <declaration>
//     This function tries to evaluate a preprocessor constant in hopes that
//     it will either evaluate to a type or object name or to a constant
//     compile time value.  It returns the matched declaration or value, or it
//     signals an error. 
//   cpp-parse(tokenizer :: <tokenizer>) => result :: <integer>
//     This function evaluates a line of CPP input according to a limited set
//     of C operators and an odd set of evaluation rules which make undefined
//     identifiers into integers.  (Note that this function is used by the
//     tokenizer, but also recursively uses the tokenizer by specifying a few
//     magic keywords to avoid infinite recursion.)  This function consumes
//     one line's worth of tokens from the tokenizer and then leaves it in a
//     consistent state for further processing by a different parser.
//
// The input file is messier than it might be, because we have retained
// productions corresponding to the entire C language -- simply commenting out
// the ones which are not needed for header files.  This should simplify
// future expansion.
//======================================================================

//----------------------------------------------------------------------
// Simple parser support 
//----------------------------------------------------------------------

// *HACK*: We temporarily use a global variable to figure out whether a given
// declaration is an object declaration or a typedef declaration.  This
// appears to be simpler than trying to propogate detailed information
// upwards.
//
define variable *typedef-flag* = #f;

// This function checks to see whether the given object can be interpreted as
// an integer.  If so, it returns the integer value.  Otherwise, it raises an
// error.  This function is used to evaluate compile time constants.
//
define generic int-value (value :: <object>, state :: <parse-state>);

define method int-value (value :: <integer>, state :: <parse-state>)
  value;
end method;

define method int-value (value :: <token>, state :: <parse-state>)
  parse-error(value, "Value in constant expression must be an integer.");
end method;

define method int-value (token :: <integer-token>, state :: <parse-state>)
  token.value;
end method;

define method int-value (value :: <object>, state :: <parse-state>)
  error(value, "Value in constant expression must be an integer.");
end method;

// This method will only be called if we are evaluating an expression in a CPP
// line.  It is not called for arbitrary identifiers.
//
define method int-value
    (value :: <identifier-token>, state :: <parse-cpp-state>)
  let expansion = element(value.generator.cpp-table,
                          value.string-value, default: #f);
  case
    expansion == #f => 
      // The C preprocessor blithely accepts undefined identifiers as "0"
      0;
    empty?(expansion) =>
      1;
    expansion.size > 1 =>
      int-value(parse-macro(value.string-value, state), state);
    otherwise =>
      int-value(head(expansion), state);
  end case;
end method;

// Evaluate identifiers in the case where we are *not* evaluating an
// expression in a CPP line.
//
define method int-value
    (value :: <identifier-token>, state :: <parse-state>)
  let object-decl = element(state.objects, value.string-value, default: #f);
  if (instance?(object-decl, <enum-slot-declaration>))
    object-decl.constant-value;
  else
    parse-error(value, "Value in constant expression must be an integer.");
  end if;
end method int-value;

//----------------------------------------------------------------------
// Generic parser boilerplate.
//----------------------------------------------------------------------

define class <action> (<object>)
  slot on :: <class>, required-init-keyword: on:, setter: #f;
end;

define class <shift> (<action>)
  slot state :: <integer>, required-init-keyword: state:, setter: #f;
end;

define method \= (action1 :: <shift>, action2 :: <shift>) => eq? :: <boolean>;
  action1.state == action2.state;
end;

define class <reduce> (<action>)
  slot production :: <integer>, required-init-keyword: production:, setter: #f;
end;

define method \= (action1 :: <reduce>, action2 :: <reduce>)
 => eq? :: <boolean>;
  action1.production == action2.production;
end;

define class <accept> (<action>)
end;

define method \= (action1 :: <accept>, action2 :: <accept>)
 => eq? :: <boolean>;
  #t;
end;

define method make-action-table(#rest actions)
  let result = make(<self-organizing-list>);
  for (action in actions)
    local
      method process (clas :: <class>)
	result[clas] := action;
	for (sub in clas.direct-subclasses)
	  process(sub);
	end for;
      end method process;
    process(action.on);
  end for;
  result;
end method make-action-table;

//----------------------------------------------------------------------
// "Magic" tokens which provide alternate entry points to the parser
//----------------------------------------------------------------------

define class <alien-name-token> (<token>) end class;
define class <macro-parse-token> (<token>) end class;
define class <cpp-parse-token> (<token>) end class;

//----------------------------------------------------------------------
// The actual productions.  The format is
//  production (sub-production-or-<token> .....)
//    Arbitrary dylan code -- variables $r1 - $rn correspond to the
//    sub-productions, and $state is a <parse-state> which is passed into each
//    action routine for record-keeping purposes.
//  %
//----------------------------------------------------------------------

define constant *action-table* = make(<vector>, size: 218);
define constant *production-table* = make(<vector>, size: 136);

*action-table*[0] :=
  // S-PRIME -> * FILE
  make-action-table(make(<shift>, on: <ALIEN-NAME-TOKEN>, state: 204),
                    make(<shift>, on: <BEGIN-INCLUDE-TOKEN>, state: 2),
                    make(<shift>, on: <CONST-TOKEN>, state: 8),
                    make(<shift>, on: <CPP-PARSE-TOKEN>, state: 214),
                    make(<shift>, on: <END-INCLUDE-TOKEN>, state: 3),
                    make(<shift>, on: <ENUM-TOKEN>, state: 15),
                    make(<shift>, on: <EOF-TOKEN>, state: 1),
                    make(<shift>, on: <EXTERN-TOKEN>, state: 5),
                    make(<shift>, on: <MACRO-PARSE-TOKEN>, state: 209),
                    make(<shift>, on: <STRUCT-TOKEN>, state: 10),
                    make(<shift>, on: <TYPE-NAME-TOKEN>, state: 33),
                    make(<shift>, on: <TYPE-SPECIFIER-TOKEN>, state: 7),
                    make(<shift>, on: <TYPEDEF-TOKEN>, state: 4),
                    make(<shift>, on: <UNION-TOKEN>, state: 11),
                    make(<shift>, on: <VOLATILE-TOKEN>, state: 9));

*action-table*[1] :=
  // FILE -> <EOF-TOKEN> *
  make-action-table(make(<reduce>, on: <EOF-TOKEN>, production: 1));

*action-table*[2] :=
  // EXTERNAL-DEFINITION -> <BEGIN-INCLUDE-TOKEN> *
  make-action-table(make(<reduce>, on: <BEGIN-INCLUDE-TOKEN>, production: 132),
                    make(<reduce>, on: <CONST-TOKEN>, production: 132),
                    make(<reduce>, on: <END-INCLUDE-TOKEN>, production: 132),
                    make(<reduce>, on: <ENUM-TOKEN>, production: 132),
                    make(<reduce>, on: <EOF-TOKEN>, production: 132),
                    make(<reduce>, on: <EXTERN-TOKEN>, production: 132),
                    make(<reduce>, on: <STRUCT-TOKEN>, production: 132),
                    make(<reduce>, on: <TYPE-NAME-TOKEN>, production: 132),
                    make(<reduce>, on: <TYPE-SPECIFIER-TOKEN>, production: 132),
                    make(<reduce>, on: <TYPEDEF-TOKEN>, production: 132),
                    make(<reduce>, on: <UNION-TOKEN>, production: 132),
                    make(<reduce>, on: <VOLATILE-TOKEN>, production: 132));

*action-table*[3] :=
  // EXTERNAL-DEFINITION -> <END-INCLUDE-TOKEN> *
  make-action-table(make(<reduce>, on: <BEGIN-INCLUDE-TOKEN>, production: 133),
                    make(<reduce>, on: <CONST-TOKEN>, production: 133),
                    make(<reduce>, on: <END-INCLUDE-TOKEN>, production: 133),
                    make(<reduce>, on: <ENUM-TOKEN>, production: 133),
                    make(<reduce>, on: <EOF-TOKEN>, production: 133),
                    make(<reduce>, on: <EXTERN-TOKEN>, production: 133),
                    make(<reduce>, on: <STRUCT-TOKEN>, production: 133),
                    make(<reduce>, on: <TYPE-NAME-TOKEN>, production: 133),
                    make(<reduce>, on: <TYPE-SPECIFIER-TOKEN>, production: 133),
                    make(<reduce>, on: <TYPEDEF-TOKEN>, production: 133),
                    make(<reduce>, on: <UNION-TOKEN>, production: 133),
                    make(<reduce>, on: <VOLATILE-TOKEN>, production: 133));

*action-table*[4] :=
  // STORAGE-CLASS-SPECIFIER -> <TYPEDEF-TOKEN> *
  make-action-table(make(<reduce>, on: <CONST-TOKEN>, production: 66),
                    make(<reduce>, on: <ENUM-TOKEN>, production: 66),
                    make(<reduce>, on: <EXTERN-TOKEN>, production: 66),
                    make(<reduce>, on: <IDENTIFIER-TOKEN>, production: 66),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 66),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 66),
                    make(<reduce>, on: <STAR-TOKEN>, production: 66),
                    make(<reduce>, on: <STRUCT-TOKEN>, production: 66),
                    make(<reduce>, on: <TYPE-NAME-TOKEN>, production: 66),
                    make(<reduce>, on: <TYPE-SPECIFIER-TOKEN>, production: 66),
                    make(<reduce>, on: <TYPEDEF-TOKEN>, production: 66),
                    make(<reduce>, on: <UNION-TOKEN>, production: 66),
                    make(<reduce>, on: <VOLATILE-TOKEN>, production: 66));

*action-table*[5] :=
  // STORAGE-CLASS-SPECIFIER -> <EXTERN-TOKEN> *
  make-action-table(make(<reduce>, on: <CONST-TOKEN>, production: 67),
                    make(<reduce>, on: <ENUM-TOKEN>, production: 67),
                    make(<reduce>, on: <EXTERN-TOKEN>, production: 67),
                    make(<reduce>, on: <IDENTIFIER-TOKEN>, production: 67),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 67),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 67),
                    make(<reduce>, on: <STAR-TOKEN>, production: 67),
                    make(<reduce>, on: <STRUCT-TOKEN>, production: 67),
                    make(<reduce>, on: <TYPE-NAME-TOKEN>, production: 67),
                    make(<reduce>, on: <TYPE-SPECIFIER-TOKEN>, production: 67),
                    make(<reduce>, on: <TYPEDEF-TOKEN>, production: 67),
                    make(<reduce>, on: <UNION-TOKEN>, production: 67),
                    make(<reduce>, on: <VOLATILE-TOKEN>, production: 67));

*action-table*[6] :=
  // DECLARATION-SPECIFIERS -> STORAGE-CLASS-SPECIFIER *
  // DECLARATION-SPECIFIERS -> STORAGE-CLASS-SPECIFIER * DECLARATION-SPECIFIERS
  make-action-table(make(<shift>, on: <CONST-TOKEN>, state: 8),
                    make(<shift>, on: <ENUM-TOKEN>, state: 15),
                    make(<shift>, on: <EXTERN-TOKEN>, state: 5),
                    make(<reduce>, on: <IDENTIFIER-TOKEN>, production: 59),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 59),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 59),
                    make(<reduce>, on: <STAR-TOKEN>, production: 59),
                    make(<shift>, on: <STRUCT-TOKEN>, state: 10),
                    make(<shift>, on: <TYPE-NAME-TOKEN>, state: 33),
                    make(<shift>, on: <TYPE-SPECIFIER-TOKEN>, state: 7),
                    make(<shift>, on: <TYPEDEF-TOKEN>, state: 4),
                    make(<shift>, on: <UNION-TOKEN>, state: 11),
                    make(<shift>, on: <VOLATILE-TOKEN>, state: 9));

*action-table*[7] :=
  // TYPE-SPECIFIER -> <TYPE-SPECIFIER-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 68),
                    make(<reduce>, on: <CONST-TOKEN>, production: 68),
                    make(<reduce>, on: <ENUM-TOKEN>, production: 68),
                    make(<reduce>, on: <EOF-TOKEN>, production: 68),
                    make(<reduce>, on: <EXTERN-TOKEN>, production: 68),
                    make(<reduce>, on: <IDENTIFIER-TOKEN>, production: 68),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 68),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 68),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 68),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 68),
                    make(<reduce>, on: <STAR-TOKEN>, production: 68),
                    make(<reduce>, on: <STRUCT-TOKEN>, production: 68),
                    make(<reduce>, on: <TYPE-NAME-TOKEN>, production: 68),
                    make(<reduce>, on: <TYPE-SPECIFIER-TOKEN>, production: 68),
                    make(<reduce>, on: <TYPEDEF-TOKEN>, production: 68),
                    make(<reduce>, on: <UNION-TOKEN>, production: 68),
                    make(<reduce>, on: <VOLATILE-TOKEN>, production: 68));

*action-table*[8] :=
  // TYPE-SPECIFIER -> <CONST-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 69),
                    make(<reduce>, on: <CONST-TOKEN>, production: 69),
                    make(<reduce>, on: <ENUM-TOKEN>, production: 69),
                    make(<reduce>, on: <EOF-TOKEN>, production: 69),
                    make(<reduce>, on: <EXTERN-TOKEN>, production: 69),
                    make(<reduce>, on: <IDENTIFIER-TOKEN>, production: 69),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 69),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 69),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 69),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 69),
                    make(<reduce>, on: <STAR-TOKEN>, production: 69),
                    make(<reduce>, on: <STRUCT-TOKEN>, production: 69),
                    make(<reduce>, on: <TYPE-NAME-TOKEN>, production: 69),
                    make(<reduce>, on: <TYPE-SPECIFIER-TOKEN>, production: 69),
                    make(<reduce>, on: <TYPEDEF-TOKEN>, production: 69),
                    make(<reduce>, on: <UNION-TOKEN>, production: 69),
                    make(<reduce>, on: <VOLATILE-TOKEN>, production: 69));

*action-table*[9] :=
  // TYPE-SPECIFIER -> <VOLATILE-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 70),
                    make(<reduce>, on: <CONST-TOKEN>, production: 70),
                    make(<reduce>, on: <ENUM-TOKEN>, production: 70),
                    make(<reduce>, on: <EOF-TOKEN>, production: 70),
                    make(<reduce>, on: <EXTERN-TOKEN>, production: 70),
                    make(<reduce>, on: <IDENTIFIER-TOKEN>, production: 70),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 70),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 70),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 70),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 70),
                    make(<reduce>, on: <STAR-TOKEN>, production: 70),
                    make(<reduce>, on: <STRUCT-TOKEN>, production: 70),
                    make(<reduce>, on: <TYPE-NAME-TOKEN>, production: 70),
                    make(<reduce>, on: <TYPE-SPECIFIER-TOKEN>, production: 70),
                    make(<reduce>, on: <TYPEDEF-TOKEN>, production: 70),
                    make(<reduce>, on: <UNION-TOKEN>, production: 70),
                    make(<reduce>, on: <VOLATILE-TOKEN>, production: 70));

*action-table*[10] :=
  // STRUCT-OR-UNION -> <STRUCT-TOKEN> *
  make-action-table(make(<reduce>, on: <LCURLY-TOKEN>, production: 77),
                    make(<reduce>, on: <NAME-TOKEN>, production: 77));

*action-table*[11] :=
  // STRUCT-OR-UNION -> <UNION-TOKEN> *
  make-action-table(make(<reduce>, on: <LCURLY-TOKEN>, production: 78),
                    make(<reduce>, on: <NAME-TOKEN>, production: 78));

*action-table*[12] :=
  // STRUCT-OR-UNION-SPECIFIER -> STRUCT-OR-UNION * <NAME-TOKEN> <LCURLY-TOKEN> STRUCT-DECLARATION-LIST <RCURLY-TOKEN>
  // STRUCT-OR-UNION-SPECIFIER -> STRUCT-OR-UNION * <LCURLY-TOKEN> STRUCT-DECLARATION-LIST <RCURLY-TOKEN>
  // STRUCT-OR-UNION-SPECIFIER -> STRUCT-OR-UNION * <NAME-TOKEN>
  make-action-table(make(<shift>, on: <LCURLY-TOKEN>, state: 13),
                    make(<shift>, on: <NAME-TOKEN>, state: 184));

*action-table*[13] :=
  // STRUCT-OR-UNION-SPECIFIER -> STRUCT-OR-UNION <LCURLY-TOKEN> * STRUCT-DECLARATION-LIST <RCURLY-TOKEN>
  make-action-table(make(<shift>, on: <CONST-TOKEN>, state: 8),
                    make(<shift>, on: <ENUM-TOKEN>, state: 15),
                    make(<shift>, on: <STRUCT-TOKEN>, state: 10),
                    make(<shift>, on: <TYPE-NAME-TOKEN>, state: 33),
                    make(<shift>, on: <TYPE-SPECIFIER-TOKEN>, state: 7),
                    make(<shift>, on: <UNION-TOKEN>, state: 11),
                    make(<shift>, on: <VOLATILE-TOKEN>, state: 9));

*action-table*[14] :=
  // TYPE-SPECIFIER -> STRUCT-OR-UNION-SPECIFIER *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 71),
                    make(<reduce>, on: <CONST-TOKEN>, production: 71),
                    make(<reduce>, on: <ENUM-TOKEN>, production: 71),
                    make(<reduce>, on: <EOF-TOKEN>, production: 71),
                    make(<reduce>, on: <EXTERN-TOKEN>, production: 71),
                    make(<reduce>, on: <IDENTIFIER-TOKEN>, production: 71),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 71),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 71),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 71),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 71),
                    make(<reduce>, on: <STAR-TOKEN>, production: 71),
                    make(<reduce>, on: <STRUCT-TOKEN>, production: 71),
                    make(<reduce>, on: <TYPE-NAME-TOKEN>, production: 71),
                    make(<reduce>, on: <TYPE-SPECIFIER-TOKEN>, production: 71),
                    make(<reduce>, on: <TYPEDEF-TOKEN>, production: 71),
                    make(<reduce>, on: <UNION-TOKEN>, production: 71),
                    make(<reduce>, on: <VOLATILE-TOKEN>, production: 71));

*action-table*[15] :=
  // ENUM-SPECIFIER -> <ENUM-TOKEN> * <LCURLY-TOKEN> ENUMERATOR-LIST <RCURLY-TOKEN>
  // ENUM-SPECIFIER -> <ENUM-TOKEN> * <NAME-TOKEN> <LCURLY-TOKEN> ENUMERATOR-LIST <RCURLY-TOKEN>
  // ENUM-SPECIFIER -> <ENUM-TOKEN> * <NAME-TOKEN>
  make-action-table(make(<shift>, on: <LCURLY-TOKEN>, state: 166),
                    make(<shift>, on: <NAME-TOKEN>, state: 16));

*action-table*[16] :=
  // ENUM-SPECIFIER -> <ENUM-TOKEN> <NAME-TOKEN> *
  // ENUM-SPECIFIER -> <ENUM-TOKEN> <NAME-TOKEN> * <LCURLY-TOKEN> ENUMERATOR-LIST <RCURLY-TOKEN>
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 88),
                    make(<reduce>, on: <CONST-TOKEN>, production: 88),
                    make(<reduce>, on: <ENUM-TOKEN>, production: 88),
                    make(<reduce>, on: <EOF-TOKEN>, production: 88),
                    make(<reduce>, on: <EXTERN-TOKEN>, production: 88),
                    make(<reduce>, on: <IDENTIFIER-TOKEN>, production: 88),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 88),
                    make(<shift>, on: <LCURLY-TOKEN>, state: 17),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 88),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 88),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 88),
                    make(<reduce>, on: <STAR-TOKEN>, production: 88),
                    make(<reduce>, on: <STRUCT-TOKEN>, production: 88),
                    make(<reduce>, on: <TYPE-NAME-TOKEN>, production: 88),
                    make(<reduce>, on: <TYPE-SPECIFIER-TOKEN>, production: 88),
                    make(<reduce>, on: <TYPEDEF-TOKEN>, production: 88),
                    make(<reduce>, on: <UNION-TOKEN>, production: 88),
                    make(<reduce>, on: <VOLATILE-TOKEN>, production: 88));

*action-table*[17] :=
  // ENUM-SPECIFIER -> <ENUM-TOKEN> <NAME-TOKEN> <LCURLY-TOKEN> * ENUMERATOR-LIST <RCURLY-TOKEN>
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18));

*action-table*[18] :=
  // IDENTIFIER -> <IDENTIFIER-TOKEN> *
  make-action-table(make(<reduce>, on: <AMPERSAND-TOKEN>, production: 135),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 135),
                    make(<reduce>, on: <ASSIGN-TOKEN>, production: 135),
                    make(<reduce>, on: <BAR-TOKEN>, production: 135),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 135),
                    make(<reduce>, on: <COLON-TOKEN>, production: 135),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 135),
                    make(<reduce>, on: <EOF-TOKEN>, production: 135),
                    make(<reduce>, on: <EQ-OP-TOKEN>, production: 135),
                    make(<reduce>, on: <GE-OP-TOKEN>, production: 135),
                    make(<reduce>, on: <GT-TOKEN>, production: 135),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 135),
                    make(<reduce>, on: <LE-OP-TOKEN>, production: 135),
                    make(<reduce>, on: <LEFT-OP-TOKEN>, production: 135),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 135),
                    make(<reduce>, on: <LT-TOKEN>, production: 135),
                    make(<reduce>, on: <MINUS-TOKEN>, production: 135),
                    make(<reduce>, on: <NE-OP-TOKEN>, production: 135),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 135),
                    make(<reduce>, on: <PERCENT-TOKEN>, production: 135),
                    make(<reduce>, on: <PLUS-TOKEN>, production: 135),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 135),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 135),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 135),
                    make(<reduce>, on: <RIGHT-OP-TOKEN>, production: 135),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 135),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 135),
                    make(<reduce>, on: <SLASH-TOKEN>, production: 135),
                    make(<reduce>, on: <STAR-TOKEN>, production: 135));

*action-table*[19] :=
  // ENUMERATOR -> IDENTIFIER *
  // ENUMERATOR -> IDENTIFIER * <ASSIGN-TOKEN> CONSTANT-EXPR
  make-action-table(make(<shift>, on: <ASSIGN-TOKEN>, state: 20),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 91),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 91));

*action-table*[20] :=
  // ENUMERATOR -> IDENTIFIER <ASSIGN-TOKEN> * CONSTANT-EXPR
  make-action-table(make(<shift>, on: <BANG-TOKEN>, state: 28),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 22),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 38),
                    make(<shift>, on: <MINUS-TOKEN>, state: 26),
                    make(<shift>, on: <SIZEOF-TOKEN>, state: 30),
                    make(<shift>, on: <TILDE-TOKEN>, state: 27));

*action-table*[21] :=
  // PRIMARY-EXPR -> IDENTIFIER *
  make-action-table(make(<reduce>, on: <AMPERSAND-TOKEN>, production: 10),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 10),
                    make(<reduce>, on: <BAR-TOKEN>, production: 10),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 10),
                    make(<reduce>, on: <COLON-TOKEN>, production: 10),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 10),
                    make(<reduce>, on: <EOF-TOKEN>, production: 10),
                    make(<reduce>, on: <EQ-OP-TOKEN>, production: 10),
                    make(<reduce>, on: <GE-OP-TOKEN>, production: 10),
                    make(<reduce>, on: <GT-TOKEN>, production: 10),
                    make(<reduce>, on: <LE-OP-TOKEN>, production: 10),
                    make(<reduce>, on: <LEFT-OP-TOKEN>, production: 10),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 10),
                    make(<reduce>, on: <LT-TOKEN>, production: 10),
                    make(<reduce>, on: <MINUS-TOKEN>, production: 10),
                    make(<reduce>, on: <NE-OP-TOKEN>, production: 10),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 10),
                    make(<reduce>, on: <PERCENT-TOKEN>, production: 10),
                    make(<reduce>, on: <PLUS-TOKEN>, production: 10),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 10),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 10),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 10),
                    make(<reduce>, on: <RIGHT-OP-TOKEN>, production: 10),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 10),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 10),
                    make(<reduce>, on: <SLASH-TOKEN>, production: 10),
                    make(<reduce>, on: <STAR-TOKEN>, production: 10));

*action-table*[22] :=
  // PRIMARY-EXPR -> <INTEGER-TOKEN> *
  make-action-table(make(<reduce>, on: <AMPERSAND-TOKEN>, production: 11),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 11),
                    make(<reduce>, on: <BAR-TOKEN>, production: 11),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 11),
                    make(<reduce>, on: <COLON-TOKEN>, production: 11),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 11),
                    make(<reduce>, on: <EOF-TOKEN>, production: 11),
                    make(<reduce>, on: <EQ-OP-TOKEN>, production: 11),
                    make(<reduce>, on: <GE-OP-TOKEN>, production: 11),
                    make(<reduce>, on: <GT-TOKEN>, production: 11),
                    make(<reduce>, on: <LE-OP-TOKEN>, production: 11),
                    make(<reduce>, on: <LEFT-OP-TOKEN>, production: 11),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 11),
                    make(<reduce>, on: <LT-TOKEN>, production: 11),
                    make(<reduce>, on: <MINUS-TOKEN>, production: 11),
                    make(<reduce>, on: <NE-OP-TOKEN>, production: 11),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 11),
                    make(<reduce>, on: <PERCENT-TOKEN>, production: 11),
                    make(<reduce>, on: <PLUS-TOKEN>, production: 11),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 11),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 11),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 11),
                    make(<reduce>, on: <RIGHT-OP-TOKEN>, production: 11),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 11),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 11),
                    make(<reduce>, on: <SLASH-TOKEN>, production: 11),
                    make(<reduce>, on: <STAR-TOKEN>, production: 11));

*action-table*[23] :=
  // POSTFIX-EXPR -> PRIMARY-EXPR *
  make-action-table(make(<reduce>, on: <AMPERSAND-TOKEN>, production: 13),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 13),
                    make(<reduce>, on: <BAR-TOKEN>, production: 13),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 13),
                    make(<reduce>, on: <COLON-TOKEN>, production: 13),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 13),
                    make(<reduce>, on: <EOF-TOKEN>, production: 13),
                    make(<reduce>, on: <EQ-OP-TOKEN>, production: 13),
                    make(<reduce>, on: <GE-OP-TOKEN>, production: 13),
                    make(<reduce>, on: <GT-TOKEN>, production: 13),
                    make(<reduce>, on: <LE-OP-TOKEN>, production: 13),
                    make(<reduce>, on: <LEFT-OP-TOKEN>, production: 13),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 13),
                    make(<reduce>, on: <LT-TOKEN>, production: 13),
                    make(<reduce>, on: <MINUS-TOKEN>, production: 13),
                    make(<reduce>, on: <NE-OP-TOKEN>, production: 13),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 13),
                    make(<reduce>, on: <PERCENT-TOKEN>, production: 13),
                    make(<reduce>, on: <PLUS-TOKEN>, production: 13),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 13),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 13),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 13),
                    make(<reduce>, on: <RIGHT-OP-TOKEN>, production: 13),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 13),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 13),
                    make(<reduce>, on: <SLASH-TOKEN>, production: 13),
                    make(<reduce>, on: <STAR-TOKEN>, production: 13));

*action-table*[24] :=
  // POSTFIX-EXPR -> POSTFIX-EXPR * <LPAREN-TOKEN> ARGUMENT-EXPR-LIST <RPAREN-TOKEN>
  // UNARY-EXPR -> POSTFIX-EXPR *
  make-action-table(make(<reduce>, on: <AMPERSAND-TOKEN>, production: 16),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 16),
                    make(<reduce>, on: <BAR-TOKEN>, production: 16),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 16),
                    make(<reduce>, on: <COLON-TOKEN>, production: 16),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 16),
                    make(<reduce>, on: <EOF-TOKEN>, production: 16),
                    make(<reduce>, on: <EQ-OP-TOKEN>, production: 16),
                    make(<reduce>, on: <GE-OP-TOKEN>, production: 16),
                    make(<reduce>, on: <GT-TOKEN>, production: 16),
                    make(<reduce>, on: <LE-OP-TOKEN>, production: 16),
                    make(<reduce>, on: <LEFT-OP-TOKEN>, production: 16),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 25),
                    make(<reduce>, on: <LT-TOKEN>, production: 16),
                    make(<reduce>, on: <MINUS-TOKEN>, production: 16),
                    make(<reduce>, on: <NE-OP-TOKEN>, production: 16),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 16),
                    make(<reduce>, on: <PERCENT-TOKEN>, production: 16),
                    make(<reduce>, on: <PLUS-TOKEN>, production: 16),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 16),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 16),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 16),
                    make(<reduce>, on: <RIGHT-OP-TOKEN>, production: 16),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 16),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 16),
                    make(<reduce>, on: <SLASH-TOKEN>, production: 16),
                    make(<reduce>, on: <STAR-TOKEN>, production: 16));

*action-table*[25] :=
  // POSTFIX-EXPR -> POSTFIX-EXPR <LPAREN-TOKEN> * ARGUMENT-EXPR-LIST <RPAREN-TOKEN>
  make-action-table(make(<shift>, on: <BANG-TOKEN>, state: 28),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 22),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 38),
                    make(<shift>, on: <MINUS-TOKEN>, state: 26),
                    make(<shift>, on: <SIZEOF-TOKEN>, state: 30),
                    make(<shift>, on: <TILDE-TOKEN>, state: 27));

*action-table*[26] :=
  // UNARY-OPERATOR -> <MINUS-TOKEN> *
  make-action-table(make(<reduce>, on: <BANG-TOKEN>, production: 19),
                    make(<reduce>, on: <IDENTIFIER-TOKEN>, production: 19),
                    make(<reduce>, on: <INTEGER-TOKEN>, production: 19),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 19),
                    make(<reduce>, on: <MINUS-TOKEN>, production: 19),
                    make(<reduce>, on: <SIZEOF-TOKEN>, production: 19),
                    make(<reduce>, on: <TILDE-TOKEN>, production: 19));

*action-table*[27] :=
  // UNARY-OPERATOR -> <TILDE-TOKEN> *
  make-action-table(make(<reduce>, on: <BANG-TOKEN>, production: 20),
                    make(<reduce>, on: <IDENTIFIER-TOKEN>, production: 20),
                    make(<reduce>, on: <INTEGER-TOKEN>, production: 20),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 20),
                    make(<reduce>, on: <MINUS-TOKEN>, production: 20),
                    make(<reduce>, on: <SIZEOF-TOKEN>, production: 20),
                    make(<reduce>, on: <TILDE-TOKEN>, production: 20));

*action-table*[28] :=
  // UNARY-OPERATOR -> <BANG-TOKEN> *
  make-action-table(make(<reduce>, on: <BANG-TOKEN>, production: 21),
                    make(<reduce>, on: <IDENTIFIER-TOKEN>, production: 21),
                    make(<reduce>, on: <INTEGER-TOKEN>, production: 21),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 21),
                    make(<reduce>, on: <MINUS-TOKEN>, production: 21),
                    make(<reduce>, on: <SIZEOF-TOKEN>, production: 21),
                    make(<reduce>, on: <TILDE-TOKEN>, production: 21));

*action-table*[29] :=
  // UNARY-EXPR -> UNARY-OPERATOR * CAST-EXPR
  make-action-table(make(<shift>, on: <BANG-TOKEN>, state: 28),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 22),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 38),
                    make(<shift>, on: <MINUS-TOKEN>, state: 26),
                    make(<shift>, on: <SIZEOF-TOKEN>, state: 30),
                    make(<shift>, on: <TILDE-TOKEN>, state: 27));

*action-table*[30] :=
  // UNARY-EXPR -> <SIZEOF-TOKEN> * <LPAREN-TOKEN> TYPE-NAME <RPAREN-TOKEN>
  make-action-table(make(<shift>, on: <LPAREN-TOKEN>, state: 31));

*action-table*[31] :=
  // UNARY-EXPR -> <SIZEOF-TOKEN> <LPAREN-TOKEN> * TYPE-NAME <RPAREN-TOKEN>
  make-action-table(make(<shift>, on: <CONST-TOKEN>, state: 8),
                    make(<shift>, on: <ENUM-TOKEN>, state: 15),
                    make(<shift>, on: <STRUCT-TOKEN>, state: 10),
                    make(<shift>, on: <TYPE-NAME-TOKEN>, state: 33),
                    make(<shift>, on: <TYPE-SPECIFIER-TOKEN>, state: 7),
                    make(<shift>, on: <UNION-TOKEN>, state: 11),
                    make(<shift>, on: <VOLATILE-TOKEN>, state: 9));

*action-table*[32] :=
  // TYPE-SPECIFIER -> ENUM-SPECIFIER *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 72),
                    make(<reduce>, on: <CONST-TOKEN>, production: 72),
                    make(<reduce>, on: <ENUM-TOKEN>, production: 72),
                    make(<reduce>, on: <EOF-TOKEN>, production: 72),
                    make(<reduce>, on: <EXTERN-TOKEN>, production: 72),
                    make(<reduce>, on: <IDENTIFIER-TOKEN>, production: 72),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 72),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 72),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 72),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 72),
                    make(<reduce>, on: <STAR-TOKEN>, production: 72),
                    make(<reduce>, on: <STRUCT-TOKEN>, production: 72),
                    make(<reduce>, on: <TYPE-NAME-TOKEN>, production: 72),
                    make(<reduce>, on: <TYPE-SPECIFIER-TOKEN>, production: 72),
                    make(<reduce>, on: <TYPEDEF-TOKEN>, production: 72),
                    make(<reduce>, on: <UNION-TOKEN>, production: 72),
                    make(<reduce>, on: <VOLATILE-TOKEN>, production: 72));

*action-table*[33] :=
  // TYPE-SPECIFIER -> <TYPE-NAME-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 73),
                    make(<reduce>, on: <CONST-TOKEN>, production: 73),
                    make(<reduce>, on: <ENUM-TOKEN>, production: 73),
                    make(<reduce>, on: <EOF-TOKEN>, production: 73),
                    make(<reduce>, on: <EXTERN-TOKEN>, production: 73),
                    make(<reduce>, on: <IDENTIFIER-TOKEN>, production: 73),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 73),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 73),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 73),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 73),
                    make(<reduce>, on: <STAR-TOKEN>, production: 73),
                    make(<reduce>, on: <STRUCT-TOKEN>, production: 73),
                    make(<reduce>, on: <TYPE-NAME-TOKEN>, production: 73),
                    make(<reduce>, on: <TYPE-SPECIFIER-TOKEN>, production: 73),
                    make(<reduce>, on: <TYPEDEF-TOKEN>, production: 73),
                    make(<reduce>, on: <UNION-TOKEN>, production: 73),
                    make(<reduce>, on: <VOLATILE-TOKEN>, production: 73));

*action-table*[34] :=
  // TYPE-SPECIFIER-LIST -> TYPE-SPECIFIER *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 106),
                    make(<reduce>, on: <CONST-TOKEN>, production: 106),
                    make(<reduce>, on: <ENUM-TOKEN>, production: 106),
                    make(<reduce>, on: <EOF-TOKEN>, production: 106),
                    make(<reduce>, on: <IDENTIFIER-TOKEN>, production: 106),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 106),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 106),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 106),
                    make(<reduce>, on: <STAR-TOKEN>, production: 106),
                    make(<reduce>, on: <STRUCT-TOKEN>, production: 106),
                    make(<reduce>, on: <TYPE-NAME-TOKEN>, production: 106),
                    make(<reduce>, on: <TYPE-SPECIFIER-TOKEN>, production: 106),
                    make(<reduce>, on: <UNION-TOKEN>, production: 106),
                    make(<reduce>, on: <VOLATILE-TOKEN>, production: 106));

*action-table*[35] :=
  // TYPE-NAME -> TYPE-SPECIFIER-LIST *
  // TYPE-SPECIFIER-LIST -> TYPE-SPECIFIER-LIST * TYPE-SPECIFIER
  // TYPE-NAME -> TYPE-SPECIFIER-LIST * ABSTRACT-DECLARATOR
  make-action-table(make(<shift>, on: <CONST-TOKEN>, state: 8),
                    make(<shift>, on: <ENUM-TOKEN>, state: 15),
                    make(<reduce>, on: <EOF-TOKEN>, production: 118),
                    make(<shift>, on: <LBRACKET-TOKEN>, state: 36),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 101),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 118),
                    make(<shift>, on: <STAR-TOKEN>, state: 143),
                    make(<shift>, on: <STRUCT-TOKEN>, state: 10),
                    make(<shift>, on: <TYPE-NAME-TOKEN>, state: 33),
                    make(<shift>, on: <TYPE-SPECIFIER-TOKEN>, state: 7),
                    make(<shift>, on: <UNION-TOKEN>, state: 11),
                    make(<shift>, on: <VOLATILE-TOKEN>, state: 9));

*action-table*[36] :=
  // ABSTRACT-DECLARATOR2 -> <LBRACKET-TOKEN> * <RBRACKET-TOKEN>
  // ABSTRACT-DECLARATOR2 -> <LBRACKET-TOKEN> * CONSTANT-EXPR <RBRACKET-TOKEN>
  make-action-table(make(<shift>, on: <BANG-TOKEN>, state: 28),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 22),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 38),
                    make(<shift>, on: <MINUS-TOKEN>, state: 26),
                    make(<shift>, on: <RBRACKET-TOKEN>, state: 100),
                    make(<shift>, on: <SIZEOF-TOKEN>, state: 30),
                    make(<shift>, on: <TILDE-TOKEN>, state: 27));

*action-table*[37] :=
  // CAST-EXPR -> UNARY-EXPR *
  make-action-table(make(<reduce>, on: <AMPERSAND-TOKEN>, production: 22),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 22),
                    make(<reduce>, on: <BAR-TOKEN>, production: 22),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 22),
                    make(<reduce>, on: <COLON-TOKEN>, production: 22),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 22),
                    make(<reduce>, on: <EOF-TOKEN>, production: 22),
                    make(<reduce>, on: <EQ-OP-TOKEN>, production: 22),
                    make(<reduce>, on: <GE-OP-TOKEN>, production: 22),
                    make(<reduce>, on: <GT-TOKEN>, production: 22),
                    make(<reduce>, on: <LE-OP-TOKEN>, production: 22),
                    make(<reduce>, on: <LEFT-OP-TOKEN>, production: 22),
                    make(<reduce>, on: <LT-TOKEN>, production: 22),
                    make(<reduce>, on: <MINUS-TOKEN>, production: 22),
                    make(<reduce>, on: <NE-OP-TOKEN>, production: 22),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 22),
                    make(<reduce>, on: <PERCENT-TOKEN>, production: 22),
                    make(<reduce>, on: <PLUS-TOKEN>, production: 22),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 22),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 22),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 22),
                    make(<reduce>, on: <RIGHT-OP-TOKEN>, production: 22),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 22),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 22),
                    make(<reduce>, on: <SLASH-TOKEN>, production: 22),
                    make(<reduce>, on: <STAR-TOKEN>, production: 22));

*action-table*[38] :=
  // PRIMARY-EXPR -> <LPAREN-TOKEN> * EXPR <RPAREN-TOKEN>
  // CAST-EXPR -> <LPAREN-TOKEN> * TYPE-NAME <RPAREN-TOKEN> CAST-EXPR
  make-action-table(make(<shift>, on: <BANG-TOKEN>, state: 28),
                    make(<shift>, on: <CONST-TOKEN>, state: 8),
                    make(<shift>, on: <ENUM-TOKEN>, state: 15),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 22),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 38),
                    make(<shift>, on: <MINUS-TOKEN>, state: 26),
                    make(<shift>, on: <SIZEOF-TOKEN>, state: 30),
                    make(<shift>, on: <STRUCT-TOKEN>, state: 10),
                    make(<shift>, on: <TILDE-TOKEN>, state: 27),
                    make(<shift>, on: <TYPE-NAME-TOKEN>, state: 33),
                    make(<shift>, on: <TYPE-SPECIFIER-TOKEN>, state: 7),
                    make(<shift>, on: <UNION-TOKEN>, state: 11),
                    make(<shift>, on: <VOLATILE-TOKEN>, state: 9));

*action-table*[39] :=
  // CAST-EXPR -> <LPAREN-TOKEN> TYPE-NAME * <RPAREN-TOKEN> CAST-EXPR
  make-action-table(make(<shift>, on: <RPAREN-TOKEN>, state: 40));

*action-table*[40] :=
  // CAST-EXPR -> <LPAREN-TOKEN> TYPE-NAME <RPAREN-TOKEN> * CAST-EXPR
  make-action-table(make(<shift>, on: <BANG-TOKEN>, state: 28),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 22),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 38),
                    make(<shift>, on: <MINUS-TOKEN>, state: 26),
                    make(<shift>, on: <SIZEOF-TOKEN>, state: 30),
                    make(<shift>, on: <TILDE-TOKEN>, state: 27));

*action-table*[41] :=
  // CAST-EXPR -> <LPAREN-TOKEN> TYPE-NAME <RPAREN-TOKEN> CAST-EXPR *
  make-action-table(make(<reduce>, on: <AMPERSAND-TOKEN>, production: 23),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 23),
                    make(<reduce>, on: <BAR-TOKEN>, production: 23),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 23),
                    make(<reduce>, on: <COLON-TOKEN>, production: 23),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 23),
                    make(<reduce>, on: <EOF-TOKEN>, production: 23),
                    make(<reduce>, on: <EQ-OP-TOKEN>, production: 23),
                    make(<reduce>, on: <GE-OP-TOKEN>, production: 23),
                    make(<reduce>, on: <GT-TOKEN>, production: 23),
                    make(<reduce>, on: <LE-OP-TOKEN>, production: 23),
                    make(<reduce>, on: <LEFT-OP-TOKEN>, production: 23),
                    make(<reduce>, on: <LT-TOKEN>, production: 23),
                    make(<reduce>, on: <MINUS-TOKEN>, production: 23),
                    make(<reduce>, on: <NE-OP-TOKEN>, production: 23),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 23),
                    make(<reduce>, on: <PERCENT-TOKEN>, production: 23),
                    make(<reduce>, on: <PLUS-TOKEN>, production: 23),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 23),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 23),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 23),
                    make(<reduce>, on: <RIGHT-OP-TOKEN>, production: 23),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 23),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 23),
                    make(<reduce>, on: <SLASH-TOKEN>, production: 23),
                    make(<reduce>, on: <STAR-TOKEN>, production: 23));

*action-table*[42] :=
  // MULTIPLICATIVE-EXPR -> CAST-EXPR *
  make-action-table(make(<reduce>, on: <AMPERSAND-TOKEN>, production: 24),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 24),
                    make(<reduce>, on: <BAR-TOKEN>, production: 24),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 24),
                    make(<reduce>, on: <COLON-TOKEN>, production: 24),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 24),
                    make(<reduce>, on: <EOF-TOKEN>, production: 24),
                    make(<reduce>, on: <EQ-OP-TOKEN>, production: 24),
                    make(<reduce>, on: <GE-OP-TOKEN>, production: 24),
                    make(<reduce>, on: <GT-TOKEN>, production: 24),
                    make(<reduce>, on: <LE-OP-TOKEN>, production: 24),
                    make(<reduce>, on: <LEFT-OP-TOKEN>, production: 24),
                    make(<reduce>, on: <LT-TOKEN>, production: 24),
                    make(<reduce>, on: <MINUS-TOKEN>, production: 24),
                    make(<reduce>, on: <NE-OP-TOKEN>, production: 24),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 24),
                    make(<reduce>, on: <PERCENT-TOKEN>, production: 24),
                    make(<reduce>, on: <PLUS-TOKEN>, production: 24),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 24),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 24),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 24),
                    make(<reduce>, on: <RIGHT-OP-TOKEN>, production: 24),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 24),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 24),
                    make(<reduce>, on: <SLASH-TOKEN>, production: 24),
                    make(<reduce>, on: <STAR-TOKEN>, production: 24));

*action-table*[43] :=
  // MULTIPLICATIVE-EXPR -> MULTIPLICATIVE-EXPR * <STAR-TOKEN> CAST-EXPR
  // MULTIPLICATIVE-EXPR -> MULTIPLICATIVE-EXPR * <SLASH-TOKEN> CAST-EXPR
  // MULTIPLICATIVE-EXPR -> MULTIPLICATIVE-EXPR * <PERCENT-TOKEN> CAST-EXPR
  // ADDITIVE-EXPR -> MULTIPLICATIVE-EXPR *
  make-action-table(make(<reduce>, on: <AMPERSAND-TOKEN>, production: 28),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 28),
                    make(<reduce>, on: <BAR-TOKEN>, production: 28),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 28),
                    make(<reduce>, on: <COLON-TOKEN>, production: 28),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 28),
                    make(<reduce>, on: <EOF-TOKEN>, production: 28),
                    make(<reduce>, on: <EQ-OP-TOKEN>, production: 28),
                    make(<reduce>, on: <GE-OP-TOKEN>, production: 28),
                    make(<reduce>, on: <GT-TOKEN>, production: 28),
                    make(<reduce>, on: <LE-OP-TOKEN>, production: 28),
                    make(<reduce>, on: <LEFT-OP-TOKEN>, production: 28),
                    make(<reduce>, on: <LT-TOKEN>, production: 28),
                    make(<reduce>, on: <MINUS-TOKEN>, production: 28),
                    make(<reduce>, on: <NE-OP-TOKEN>, production: 28),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 28),
                    make(<shift>, on: <PERCENT-TOKEN>, state: 44),
                    make(<reduce>, on: <PLUS-TOKEN>, production: 28),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 28),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 28),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 28),
                    make(<reduce>, on: <RIGHT-OP-TOKEN>, production: 28),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 28),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 28),
                    make(<shift>, on: <SLASH-TOKEN>, state: 46),
                    make(<shift>, on: <STAR-TOKEN>, state: 48));

*action-table*[44] :=
  // MULTIPLICATIVE-EXPR -> MULTIPLICATIVE-EXPR <PERCENT-TOKEN> * CAST-EXPR
  make-action-table(make(<shift>, on: <BANG-TOKEN>, state: 28),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 22),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 38),
                    make(<shift>, on: <MINUS-TOKEN>, state: 26),
                    make(<shift>, on: <SIZEOF-TOKEN>, state: 30),
                    make(<shift>, on: <TILDE-TOKEN>, state: 27));

*action-table*[45] :=
  // MULTIPLICATIVE-EXPR -> MULTIPLICATIVE-EXPR <PERCENT-TOKEN> CAST-EXPR *
  make-action-table(make(<reduce>, on: <AMPERSAND-TOKEN>, production: 27),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 27),
                    make(<reduce>, on: <BAR-TOKEN>, production: 27),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 27),
                    make(<reduce>, on: <COLON-TOKEN>, production: 27),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 27),
                    make(<reduce>, on: <EOF-TOKEN>, production: 27),
                    make(<reduce>, on: <EQ-OP-TOKEN>, production: 27),
                    make(<reduce>, on: <GE-OP-TOKEN>, production: 27),
                    make(<reduce>, on: <GT-TOKEN>, production: 27),
                    make(<reduce>, on: <LE-OP-TOKEN>, production: 27),
                    make(<reduce>, on: <LEFT-OP-TOKEN>, production: 27),
                    make(<reduce>, on: <LT-TOKEN>, production: 27),
                    make(<reduce>, on: <MINUS-TOKEN>, production: 27),
                    make(<reduce>, on: <NE-OP-TOKEN>, production: 27),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 27),
                    make(<reduce>, on: <PERCENT-TOKEN>, production: 27),
                    make(<reduce>, on: <PLUS-TOKEN>, production: 27),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 27),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 27),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 27),
                    make(<reduce>, on: <RIGHT-OP-TOKEN>, production: 27),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 27),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 27),
                    make(<reduce>, on: <SLASH-TOKEN>, production: 27),
                    make(<reduce>, on: <STAR-TOKEN>, production: 27));

*action-table*[46] :=
  // MULTIPLICATIVE-EXPR -> MULTIPLICATIVE-EXPR <SLASH-TOKEN> * CAST-EXPR
  make-action-table(make(<shift>, on: <BANG-TOKEN>, state: 28),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 22),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 38),
                    make(<shift>, on: <MINUS-TOKEN>, state: 26),
                    make(<shift>, on: <SIZEOF-TOKEN>, state: 30),
                    make(<shift>, on: <TILDE-TOKEN>, state: 27));

*action-table*[47] :=
  // MULTIPLICATIVE-EXPR -> MULTIPLICATIVE-EXPR <SLASH-TOKEN> CAST-EXPR *
  make-action-table(make(<reduce>, on: <AMPERSAND-TOKEN>, production: 26),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 26),
                    make(<reduce>, on: <BAR-TOKEN>, production: 26),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 26),
                    make(<reduce>, on: <COLON-TOKEN>, production: 26),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 26),
                    make(<reduce>, on: <EOF-TOKEN>, production: 26),
                    make(<reduce>, on: <EQ-OP-TOKEN>, production: 26),
                    make(<reduce>, on: <GE-OP-TOKEN>, production: 26),
                    make(<reduce>, on: <GT-TOKEN>, production: 26),
                    make(<reduce>, on: <LE-OP-TOKEN>, production: 26),
                    make(<reduce>, on: <LEFT-OP-TOKEN>, production: 26),
                    make(<reduce>, on: <LT-TOKEN>, production: 26),
                    make(<reduce>, on: <MINUS-TOKEN>, production: 26),
                    make(<reduce>, on: <NE-OP-TOKEN>, production: 26),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 26),
                    make(<reduce>, on: <PERCENT-TOKEN>, production: 26),
                    make(<reduce>, on: <PLUS-TOKEN>, production: 26),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 26),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 26),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 26),
                    make(<reduce>, on: <RIGHT-OP-TOKEN>, production: 26),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 26),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 26),
                    make(<reduce>, on: <SLASH-TOKEN>, production: 26),
                    make(<reduce>, on: <STAR-TOKEN>, production: 26));

*action-table*[48] :=
  // MULTIPLICATIVE-EXPR -> MULTIPLICATIVE-EXPR <STAR-TOKEN> * CAST-EXPR
  make-action-table(make(<shift>, on: <BANG-TOKEN>, state: 28),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 22),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 38),
                    make(<shift>, on: <MINUS-TOKEN>, state: 26),
                    make(<shift>, on: <SIZEOF-TOKEN>, state: 30),
                    make(<shift>, on: <TILDE-TOKEN>, state: 27));

*action-table*[49] :=
  // MULTIPLICATIVE-EXPR -> MULTIPLICATIVE-EXPR <STAR-TOKEN> CAST-EXPR *
  make-action-table(make(<reduce>, on: <AMPERSAND-TOKEN>, production: 25),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 25),
                    make(<reduce>, on: <BAR-TOKEN>, production: 25),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 25),
                    make(<reduce>, on: <COLON-TOKEN>, production: 25),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 25),
                    make(<reduce>, on: <EOF-TOKEN>, production: 25),
                    make(<reduce>, on: <EQ-OP-TOKEN>, production: 25),
                    make(<reduce>, on: <GE-OP-TOKEN>, production: 25),
                    make(<reduce>, on: <GT-TOKEN>, production: 25),
                    make(<reduce>, on: <LE-OP-TOKEN>, production: 25),
                    make(<reduce>, on: <LEFT-OP-TOKEN>, production: 25),
                    make(<reduce>, on: <LT-TOKEN>, production: 25),
                    make(<reduce>, on: <MINUS-TOKEN>, production: 25),
                    make(<reduce>, on: <NE-OP-TOKEN>, production: 25),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 25),
                    make(<reduce>, on: <PERCENT-TOKEN>, production: 25),
                    make(<reduce>, on: <PLUS-TOKEN>, production: 25),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 25),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 25),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 25),
                    make(<reduce>, on: <RIGHT-OP-TOKEN>, production: 25),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 25),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 25),
                    make(<reduce>, on: <SLASH-TOKEN>, production: 25),
                    make(<reduce>, on: <STAR-TOKEN>, production: 25));

*action-table*[50] :=
  // ADDITIVE-EXPR -> ADDITIVE-EXPR * <PLUS-TOKEN> MULTIPLICATIVE-EXPR
  // ADDITIVE-EXPR -> ADDITIVE-EXPR * <MINUS-TOKEN> MULTIPLICATIVE-EXPR
  // SHIFT-EXPR -> ADDITIVE-EXPR *
  make-action-table(make(<reduce>, on: <AMPERSAND-TOKEN>, production: 31),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 31),
                    make(<reduce>, on: <BAR-TOKEN>, production: 31),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 31),
                    make(<reduce>, on: <COLON-TOKEN>, production: 31),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 31),
                    make(<reduce>, on: <EOF-TOKEN>, production: 31),
                    make(<reduce>, on: <EQ-OP-TOKEN>, production: 31),
                    make(<reduce>, on: <GE-OP-TOKEN>, production: 31),
                    make(<reduce>, on: <GT-TOKEN>, production: 31),
                    make(<reduce>, on: <LE-OP-TOKEN>, production: 31),
                    make(<reduce>, on: <LEFT-OP-TOKEN>, production: 31),
                    make(<reduce>, on: <LT-TOKEN>, production: 31),
                    make(<shift>, on: <MINUS-TOKEN>, state: 51),
                    make(<reduce>, on: <NE-OP-TOKEN>, production: 31),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 31),
                    make(<shift>, on: <PLUS-TOKEN>, state: 53),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 31),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 31),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 31),
                    make(<reduce>, on: <RIGHT-OP-TOKEN>, production: 31),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 31),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 31));

*action-table*[51] :=
  // ADDITIVE-EXPR -> ADDITIVE-EXPR <MINUS-TOKEN> * MULTIPLICATIVE-EXPR
  make-action-table(make(<shift>, on: <BANG-TOKEN>, state: 28),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 22),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 38),
                    make(<shift>, on: <MINUS-TOKEN>, state: 26),
                    make(<shift>, on: <SIZEOF-TOKEN>, state: 30),
                    make(<shift>, on: <TILDE-TOKEN>, state: 27));

*action-table*[52] :=
  // MULTIPLICATIVE-EXPR -> MULTIPLICATIVE-EXPR * <STAR-TOKEN> CAST-EXPR
  // MULTIPLICATIVE-EXPR -> MULTIPLICATIVE-EXPR * <SLASH-TOKEN> CAST-EXPR
  // MULTIPLICATIVE-EXPR -> MULTIPLICATIVE-EXPR * <PERCENT-TOKEN> CAST-EXPR
  // ADDITIVE-EXPR -> ADDITIVE-EXPR <MINUS-TOKEN> MULTIPLICATIVE-EXPR *
  make-action-table(make(<reduce>, on: <AMPERSAND-TOKEN>, production: 30),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 30),
                    make(<reduce>, on: <BAR-TOKEN>, production: 30),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 30),
                    make(<reduce>, on: <COLON-TOKEN>, production: 30),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 30),
                    make(<reduce>, on: <EOF-TOKEN>, production: 30),
                    make(<reduce>, on: <EQ-OP-TOKEN>, production: 30),
                    make(<reduce>, on: <GE-OP-TOKEN>, production: 30),
                    make(<reduce>, on: <GT-TOKEN>, production: 30),
                    make(<reduce>, on: <LE-OP-TOKEN>, production: 30),
                    make(<reduce>, on: <LEFT-OP-TOKEN>, production: 30),
                    make(<reduce>, on: <LT-TOKEN>, production: 30),
                    make(<reduce>, on: <MINUS-TOKEN>, production: 30),
                    make(<reduce>, on: <NE-OP-TOKEN>, production: 30),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 30),
                    make(<shift>, on: <PERCENT-TOKEN>, state: 44),
                    make(<reduce>, on: <PLUS-TOKEN>, production: 30),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 30),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 30),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 30),
                    make(<reduce>, on: <RIGHT-OP-TOKEN>, production: 30),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 30),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 30),
                    make(<shift>, on: <SLASH-TOKEN>, state: 46),
                    make(<shift>, on: <STAR-TOKEN>, state: 48));

*action-table*[53] :=
  // ADDITIVE-EXPR -> ADDITIVE-EXPR <PLUS-TOKEN> * MULTIPLICATIVE-EXPR
  make-action-table(make(<shift>, on: <BANG-TOKEN>, state: 28),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 22),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 38),
                    make(<shift>, on: <MINUS-TOKEN>, state: 26),
                    make(<shift>, on: <SIZEOF-TOKEN>, state: 30),
                    make(<shift>, on: <TILDE-TOKEN>, state: 27));

*action-table*[54] :=
  // MULTIPLICATIVE-EXPR -> MULTIPLICATIVE-EXPR * <STAR-TOKEN> CAST-EXPR
  // MULTIPLICATIVE-EXPR -> MULTIPLICATIVE-EXPR * <SLASH-TOKEN> CAST-EXPR
  // MULTIPLICATIVE-EXPR -> MULTIPLICATIVE-EXPR * <PERCENT-TOKEN> CAST-EXPR
  // ADDITIVE-EXPR -> ADDITIVE-EXPR <PLUS-TOKEN> MULTIPLICATIVE-EXPR *
  make-action-table(make(<reduce>, on: <AMPERSAND-TOKEN>, production: 29),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 29),
                    make(<reduce>, on: <BAR-TOKEN>, production: 29),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 29),
                    make(<reduce>, on: <COLON-TOKEN>, production: 29),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 29),
                    make(<reduce>, on: <EOF-TOKEN>, production: 29),
                    make(<reduce>, on: <EQ-OP-TOKEN>, production: 29),
                    make(<reduce>, on: <GE-OP-TOKEN>, production: 29),
                    make(<reduce>, on: <GT-TOKEN>, production: 29),
                    make(<reduce>, on: <LE-OP-TOKEN>, production: 29),
                    make(<reduce>, on: <LEFT-OP-TOKEN>, production: 29),
                    make(<reduce>, on: <LT-TOKEN>, production: 29),
                    make(<reduce>, on: <MINUS-TOKEN>, production: 29),
                    make(<reduce>, on: <NE-OP-TOKEN>, production: 29),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 29),
                    make(<shift>, on: <PERCENT-TOKEN>, state: 44),
                    make(<reduce>, on: <PLUS-TOKEN>, production: 29),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 29),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 29),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 29),
                    make(<reduce>, on: <RIGHT-OP-TOKEN>, production: 29),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 29),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 29),
                    make(<shift>, on: <SLASH-TOKEN>, state: 46),
                    make(<shift>, on: <STAR-TOKEN>, state: 48));

*action-table*[55] :=
  // SHIFT-EXPR -> SHIFT-EXPR * <LEFT-OP-TOKEN> ADDITIVE-EXPR
  // SHIFT-EXPR -> SHIFT-EXPR * <RIGHT-OP-TOKEN> ADDITIVE-EXPR
  // RELATIONAL-EXPR -> SHIFT-EXPR *
  make-action-table(make(<reduce>, on: <AMPERSAND-TOKEN>, production: 34),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 34),
                    make(<reduce>, on: <BAR-TOKEN>, production: 34),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 34),
                    make(<reduce>, on: <COLON-TOKEN>, production: 34),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 34),
                    make(<reduce>, on: <EOF-TOKEN>, production: 34),
                    make(<reduce>, on: <EQ-OP-TOKEN>, production: 34),
                    make(<reduce>, on: <GE-OP-TOKEN>, production: 34),
                    make(<reduce>, on: <GT-TOKEN>, production: 34),
                    make(<reduce>, on: <LE-OP-TOKEN>, production: 34),
                    make(<shift>, on: <LEFT-OP-TOKEN>, state: 58),
                    make(<reduce>, on: <LT-TOKEN>, production: 34),
                    make(<reduce>, on: <NE-OP-TOKEN>, production: 34),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 34),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 34),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 34),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 34),
                    make(<shift>, on: <RIGHT-OP-TOKEN>, state: 56),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 34),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 34));

*action-table*[56] :=
  // SHIFT-EXPR -> SHIFT-EXPR <RIGHT-OP-TOKEN> * ADDITIVE-EXPR
  make-action-table(make(<shift>, on: <BANG-TOKEN>, state: 28),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 22),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 38),
                    make(<shift>, on: <MINUS-TOKEN>, state: 26),
                    make(<shift>, on: <SIZEOF-TOKEN>, state: 30),
                    make(<shift>, on: <TILDE-TOKEN>, state: 27));

*action-table*[57] :=
  // ADDITIVE-EXPR -> ADDITIVE-EXPR * <PLUS-TOKEN> MULTIPLICATIVE-EXPR
  // ADDITIVE-EXPR -> ADDITIVE-EXPR * <MINUS-TOKEN> MULTIPLICATIVE-EXPR
  // SHIFT-EXPR -> SHIFT-EXPR <RIGHT-OP-TOKEN> ADDITIVE-EXPR *
  make-action-table(make(<reduce>, on: <AMPERSAND-TOKEN>, production: 33),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 33),
                    make(<reduce>, on: <BAR-TOKEN>, production: 33),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 33),
                    make(<reduce>, on: <COLON-TOKEN>, production: 33),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 33),
                    make(<reduce>, on: <EOF-TOKEN>, production: 33),
                    make(<reduce>, on: <EQ-OP-TOKEN>, production: 33),
                    make(<reduce>, on: <GE-OP-TOKEN>, production: 33),
                    make(<reduce>, on: <GT-TOKEN>, production: 33),
                    make(<reduce>, on: <LE-OP-TOKEN>, production: 33),
                    make(<reduce>, on: <LEFT-OP-TOKEN>, production: 33),
                    make(<reduce>, on: <LT-TOKEN>, production: 33),
                    make(<shift>, on: <MINUS-TOKEN>, state: 51),
                    make(<reduce>, on: <NE-OP-TOKEN>, production: 33),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 33),
                    make(<shift>, on: <PLUS-TOKEN>, state: 53),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 33),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 33),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 33),
                    make(<reduce>, on: <RIGHT-OP-TOKEN>, production: 33),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 33),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 33));

*action-table*[58] :=
  // SHIFT-EXPR -> SHIFT-EXPR <LEFT-OP-TOKEN> * ADDITIVE-EXPR
  make-action-table(make(<shift>, on: <BANG-TOKEN>, state: 28),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 22),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 38),
                    make(<shift>, on: <MINUS-TOKEN>, state: 26),
                    make(<shift>, on: <SIZEOF-TOKEN>, state: 30),
                    make(<shift>, on: <TILDE-TOKEN>, state: 27));

*action-table*[59] :=
  // ADDITIVE-EXPR -> ADDITIVE-EXPR * <PLUS-TOKEN> MULTIPLICATIVE-EXPR
  // ADDITIVE-EXPR -> ADDITIVE-EXPR * <MINUS-TOKEN> MULTIPLICATIVE-EXPR
  // SHIFT-EXPR -> SHIFT-EXPR <LEFT-OP-TOKEN> ADDITIVE-EXPR *
  make-action-table(make(<reduce>, on: <AMPERSAND-TOKEN>, production: 32),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 32),
                    make(<reduce>, on: <BAR-TOKEN>, production: 32),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 32),
                    make(<reduce>, on: <COLON-TOKEN>, production: 32),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 32),
                    make(<reduce>, on: <EOF-TOKEN>, production: 32),
                    make(<reduce>, on: <EQ-OP-TOKEN>, production: 32),
                    make(<reduce>, on: <GE-OP-TOKEN>, production: 32),
                    make(<reduce>, on: <GT-TOKEN>, production: 32),
                    make(<reduce>, on: <LE-OP-TOKEN>, production: 32),
                    make(<reduce>, on: <LEFT-OP-TOKEN>, production: 32),
                    make(<reduce>, on: <LT-TOKEN>, production: 32),
                    make(<shift>, on: <MINUS-TOKEN>, state: 51),
                    make(<reduce>, on: <NE-OP-TOKEN>, production: 32),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 32),
                    make(<shift>, on: <PLUS-TOKEN>, state: 53),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 32),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 32),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 32),
                    make(<reduce>, on: <RIGHT-OP-TOKEN>, production: 32),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 32),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 32));

*action-table*[60] :=
  // RELATIONAL-EXPR -> RELATIONAL-EXPR * <LT-TOKEN> SHIFT-EXPR
  // RELATIONAL-EXPR -> RELATIONAL-EXPR * <GT-TOKEN> SHIFT-EXPR
  // RELATIONAL-EXPR -> RELATIONAL-EXPR * <LE-OP-TOKEN> SHIFT-EXPR
  // RELATIONAL-EXPR -> RELATIONAL-EXPR * <GE-OP-TOKEN> SHIFT-EXPR
  // EQUALITY-EXPR -> RELATIONAL-EXPR *
  make-action-table(make(<reduce>, on: <AMPERSAND-TOKEN>, production: 39),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 39),
                    make(<reduce>, on: <BAR-TOKEN>, production: 39),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 39),
                    make(<reduce>, on: <COLON-TOKEN>, production: 39),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 39),
                    make(<reduce>, on: <EOF-TOKEN>, production: 39),
                    make(<reduce>, on: <EQ-OP-TOKEN>, production: 39),
                    make(<shift>, on: <GE-OP-TOKEN>, state: 61),
                    make(<shift>, on: <GT-TOKEN>, state: 65),
                    make(<shift>, on: <LE-OP-TOKEN>, state: 63),
                    make(<shift>, on: <LT-TOKEN>, state: 67),
                    make(<reduce>, on: <NE-OP-TOKEN>, production: 39),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 39),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 39),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 39),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 39),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 39),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 39));

*action-table*[61] :=
  // RELATIONAL-EXPR -> RELATIONAL-EXPR <GE-OP-TOKEN> * SHIFT-EXPR
  make-action-table(make(<shift>, on: <BANG-TOKEN>, state: 28),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 22),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 38),
                    make(<shift>, on: <MINUS-TOKEN>, state: 26),
                    make(<shift>, on: <SIZEOF-TOKEN>, state: 30),
                    make(<shift>, on: <TILDE-TOKEN>, state: 27));

*action-table*[62] :=
  // SHIFT-EXPR -> SHIFT-EXPR * <LEFT-OP-TOKEN> ADDITIVE-EXPR
  // SHIFT-EXPR -> SHIFT-EXPR * <RIGHT-OP-TOKEN> ADDITIVE-EXPR
  // RELATIONAL-EXPR -> RELATIONAL-EXPR <GE-OP-TOKEN> SHIFT-EXPR *
  make-action-table(make(<reduce>, on: <AMPERSAND-TOKEN>, production: 38),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 38),
                    make(<reduce>, on: <BAR-TOKEN>, production: 38),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 38),
                    make(<reduce>, on: <COLON-TOKEN>, production: 38),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 38),
                    make(<reduce>, on: <EOF-TOKEN>, production: 38),
                    make(<reduce>, on: <EQ-OP-TOKEN>, production: 38),
                    make(<reduce>, on: <GE-OP-TOKEN>, production: 38),
                    make(<reduce>, on: <GT-TOKEN>, production: 38),
                    make(<reduce>, on: <LE-OP-TOKEN>, production: 38),
                    make(<shift>, on: <LEFT-OP-TOKEN>, state: 58),
                    make(<reduce>, on: <LT-TOKEN>, production: 38),
                    make(<reduce>, on: <NE-OP-TOKEN>, production: 38),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 38),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 38),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 38),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 38),
                    make(<shift>, on: <RIGHT-OP-TOKEN>, state: 56),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 38),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 38));

*action-table*[63] :=
  // RELATIONAL-EXPR -> RELATIONAL-EXPR <LE-OP-TOKEN> * SHIFT-EXPR
  make-action-table(make(<shift>, on: <BANG-TOKEN>, state: 28),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 22),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 38),
                    make(<shift>, on: <MINUS-TOKEN>, state: 26),
                    make(<shift>, on: <SIZEOF-TOKEN>, state: 30),
                    make(<shift>, on: <TILDE-TOKEN>, state: 27));

*action-table*[64] :=
  // SHIFT-EXPR -> SHIFT-EXPR * <LEFT-OP-TOKEN> ADDITIVE-EXPR
  // SHIFT-EXPR -> SHIFT-EXPR * <RIGHT-OP-TOKEN> ADDITIVE-EXPR
  // RELATIONAL-EXPR -> RELATIONAL-EXPR <LE-OP-TOKEN> SHIFT-EXPR *
  make-action-table(make(<reduce>, on: <AMPERSAND-TOKEN>, production: 37),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 37),
                    make(<reduce>, on: <BAR-TOKEN>, production: 37),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 37),
                    make(<reduce>, on: <COLON-TOKEN>, production: 37),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 37),
                    make(<reduce>, on: <EOF-TOKEN>, production: 37),
                    make(<reduce>, on: <EQ-OP-TOKEN>, production: 37),
                    make(<reduce>, on: <GE-OP-TOKEN>, production: 37),
                    make(<reduce>, on: <GT-TOKEN>, production: 37),
                    make(<reduce>, on: <LE-OP-TOKEN>, production: 37),
                    make(<shift>, on: <LEFT-OP-TOKEN>, state: 58),
                    make(<reduce>, on: <LT-TOKEN>, production: 37),
                    make(<reduce>, on: <NE-OP-TOKEN>, production: 37),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 37),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 37),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 37),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 37),
                    make(<shift>, on: <RIGHT-OP-TOKEN>, state: 56),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 37),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 37));

*action-table*[65] :=
  // RELATIONAL-EXPR -> RELATIONAL-EXPR <GT-TOKEN> * SHIFT-EXPR
  make-action-table(make(<shift>, on: <BANG-TOKEN>, state: 28),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 22),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 38),
                    make(<shift>, on: <MINUS-TOKEN>, state: 26),
                    make(<shift>, on: <SIZEOF-TOKEN>, state: 30),
                    make(<shift>, on: <TILDE-TOKEN>, state: 27));

*action-table*[66] :=
  // SHIFT-EXPR -> SHIFT-EXPR * <LEFT-OP-TOKEN> ADDITIVE-EXPR
  // SHIFT-EXPR -> SHIFT-EXPR * <RIGHT-OP-TOKEN> ADDITIVE-EXPR
  // RELATIONAL-EXPR -> RELATIONAL-EXPR <GT-TOKEN> SHIFT-EXPR *
  make-action-table(make(<reduce>, on: <AMPERSAND-TOKEN>, production: 36),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 36),
                    make(<reduce>, on: <BAR-TOKEN>, production: 36),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 36),
                    make(<reduce>, on: <COLON-TOKEN>, production: 36),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 36),
                    make(<reduce>, on: <EOF-TOKEN>, production: 36),
                    make(<reduce>, on: <EQ-OP-TOKEN>, production: 36),
                    make(<reduce>, on: <GE-OP-TOKEN>, production: 36),
                    make(<reduce>, on: <GT-TOKEN>, production: 36),
                    make(<reduce>, on: <LE-OP-TOKEN>, production: 36),
                    make(<shift>, on: <LEFT-OP-TOKEN>, state: 58),
                    make(<reduce>, on: <LT-TOKEN>, production: 36),
                    make(<reduce>, on: <NE-OP-TOKEN>, production: 36),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 36),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 36),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 36),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 36),
                    make(<shift>, on: <RIGHT-OP-TOKEN>, state: 56),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 36),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 36));

*action-table*[67] :=
  // RELATIONAL-EXPR -> RELATIONAL-EXPR <LT-TOKEN> * SHIFT-EXPR
  make-action-table(make(<shift>, on: <BANG-TOKEN>, state: 28),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 22),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 38),
                    make(<shift>, on: <MINUS-TOKEN>, state: 26),
                    make(<shift>, on: <SIZEOF-TOKEN>, state: 30),
                    make(<shift>, on: <TILDE-TOKEN>, state: 27));

*action-table*[68] :=
  // SHIFT-EXPR -> SHIFT-EXPR * <LEFT-OP-TOKEN> ADDITIVE-EXPR
  // SHIFT-EXPR -> SHIFT-EXPR * <RIGHT-OP-TOKEN> ADDITIVE-EXPR
  // RELATIONAL-EXPR -> RELATIONAL-EXPR <LT-TOKEN> SHIFT-EXPR *
  make-action-table(make(<reduce>, on: <AMPERSAND-TOKEN>, production: 35),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 35),
                    make(<reduce>, on: <BAR-TOKEN>, production: 35),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 35),
                    make(<reduce>, on: <COLON-TOKEN>, production: 35),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 35),
                    make(<reduce>, on: <EOF-TOKEN>, production: 35),
                    make(<reduce>, on: <EQ-OP-TOKEN>, production: 35),
                    make(<reduce>, on: <GE-OP-TOKEN>, production: 35),
                    make(<reduce>, on: <GT-TOKEN>, production: 35),
                    make(<reduce>, on: <LE-OP-TOKEN>, production: 35),
                    make(<shift>, on: <LEFT-OP-TOKEN>, state: 58),
                    make(<reduce>, on: <LT-TOKEN>, production: 35),
                    make(<reduce>, on: <NE-OP-TOKEN>, production: 35),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 35),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 35),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 35),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 35),
                    make(<shift>, on: <RIGHT-OP-TOKEN>, state: 56),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 35),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 35));

*action-table*[69] :=
  // EQUALITY-EXPR -> EQUALITY-EXPR * <EQ-OP-TOKEN> RELATIONAL-EXPR
  // EQUALITY-EXPR -> EQUALITY-EXPR * <NE-OP-TOKEN> RELATIONAL-EXPR
  // AND-EXPR -> EQUALITY-EXPR *
  make-action-table(make(<reduce>, on: <AMPERSAND-TOKEN>, production: 42),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 42),
                    make(<reduce>, on: <BAR-TOKEN>, production: 42),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 42),
                    make(<reduce>, on: <COLON-TOKEN>, production: 42),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 42),
                    make(<reduce>, on: <EOF-TOKEN>, production: 42),
                    make(<shift>, on: <EQ-OP-TOKEN>, state: 72),
                    make(<shift>, on: <NE-OP-TOKEN>, state: 70),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 42),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 42),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 42),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 42),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 42),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 42));

*action-table*[70] :=
  // EQUALITY-EXPR -> EQUALITY-EXPR <NE-OP-TOKEN> * RELATIONAL-EXPR
  make-action-table(make(<shift>, on: <BANG-TOKEN>, state: 28),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 22),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 38),
                    make(<shift>, on: <MINUS-TOKEN>, state: 26),
                    make(<shift>, on: <SIZEOF-TOKEN>, state: 30),
                    make(<shift>, on: <TILDE-TOKEN>, state: 27));

*action-table*[71] :=
  // RELATIONAL-EXPR -> RELATIONAL-EXPR * <LT-TOKEN> SHIFT-EXPR
  // RELATIONAL-EXPR -> RELATIONAL-EXPR * <GT-TOKEN> SHIFT-EXPR
  // RELATIONAL-EXPR -> RELATIONAL-EXPR * <LE-OP-TOKEN> SHIFT-EXPR
  // RELATIONAL-EXPR -> RELATIONAL-EXPR * <GE-OP-TOKEN> SHIFT-EXPR
  // EQUALITY-EXPR -> EQUALITY-EXPR <NE-OP-TOKEN> RELATIONAL-EXPR *
  make-action-table(make(<reduce>, on: <AMPERSAND-TOKEN>, production: 41),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 41),
                    make(<reduce>, on: <BAR-TOKEN>, production: 41),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 41),
                    make(<reduce>, on: <COLON-TOKEN>, production: 41),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 41),
                    make(<reduce>, on: <EOF-TOKEN>, production: 41),
                    make(<reduce>, on: <EQ-OP-TOKEN>, production: 41),
                    make(<shift>, on: <GE-OP-TOKEN>, state: 61),
                    make(<shift>, on: <GT-TOKEN>, state: 65),
                    make(<shift>, on: <LE-OP-TOKEN>, state: 63),
                    make(<shift>, on: <LT-TOKEN>, state: 67),
                    make(<reduce>, on: <NE-OP-TOKEN>, production: 41),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 41),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 41),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 41),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 41),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 41),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 41));

*action-table*[72] :=
  // EQUALITY-EXPR -> EQUALITY-EXPR <EQ-OP-TOKEN> * RELATIONAL-EXPR
  make-action-table(make(<shift>, on: <BANG-TOKEN>, state: 28),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 22),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 38),
                    make(<shift>, on: <MINUS-TOKEN>, state: 26),
                    make(<shift>, on: <SIZEOF-TOKEN>, state: 30),
                    make(<shift>, on: <TILDE-TOKEN>, state: 27));

*action-table*[73] :=
  // RELATIONAL-EXPR -> RELATIONAL-EXPR * <LT-TOKEN> SHIFT-EXPR
  // RELATIONAL-EXPR -> RELATIONAL-EXPR * <GT-TOKEN> SHIFT-EXPR
  // RELATIONAL-EXPR -> RELATIONAL-EXPR * <LE-OP-TOKEN> SHIFT-EXPR
  // RELATIONAL-EXPR -> RELATIONAL-EXPR * <GE-OP-TOKEN> SHIFT-EXPR
  // EQUALITY-EXPR -> EQUALITY-EXPR <EQ-OP-TOKEN> RELATIONAL-EXPR *
  make-action-table(make(<reduce>, on: <AMPERSAND-TOKEN>, production: 40),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 40),
                    make(<reduce>, on: <BAR-TOKEN>, production: 40),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 40),
                    make(<reduce>, on: <COLON-TOKEN>, production: 40),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 40),
                    make(<reduce>, on: <EOF-TOKEN>, production: 40),
                    make(<reduce>, on: <EQ-OP-TOKEN>, production: 40),
                    make(<shift>, on: <GE-OP-TOKEN>, state: 61),
                    make(<shift>, on: <GT-TOKEN>, state: 65),
                    make(<shift>, on: <LE-OP-TOKEN>, state: 63),
                    make(<shift>, on: <LT-TOKEN>, state: 67),
                    make(<reduce>, on: <NE-OP-TOKEN>, production: 40),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 40),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 40),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 40),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 40),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 40),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 40));

*action-table*[74] :=
  // AND-EXPR -> AND-EXPR * <AMPERSAND-TOKEN> EQUALITY-EXPR
  // EXCLUSIVE-OR-EXPR -> AND-EXPR *
  make-action-table(make(<shift>, on: <AMPERSAND-TOKEN>, state: 75),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 44),
                    make(<reduce>, on: <BAR-TOKEN>, production: 44),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 44),
                    make(<reduce>, on: <COLON-TOKEN>, production: 44),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 44),
                    make(<reduce>, on: <EOF-TOKEN>, production: 44),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 44),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 44),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 44),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 44),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 44),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 44));

*action-table*[75] :=
  // AND-EXPR -> AND-EXPR <AMPERSAND-TOKEN> * EQUALITY-EXPR
  make-action-table(make(<shift>, on: <BANG-TOKEN>, state: 28),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 22),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 38),
                    make(<shift>, on: <MINUS-TOKEN>, state: 26),
                    make(<shift>, on: <SIZEOF-TOKEN>, state: 30),
                    make(<shift>, on: <TILDE-TOKEN>, state: 27));

*action-table*[76] :=
  // EQUALITY-EXPR -> EQUALITY-EXPR * <EQ-OP-TOKEN> RELATIONAL-EXPR
  // EQUALITY-EXPR -> EQUALITY-EXPR * <NE-OP-TOKEN> RELATIONAL-EXPR
  // AND-EXPR -> AND-EXPR <AMPERSAND-TOKEN> EQUALITY-EXPR *
  make-action-table(make(<reduce>, on: <AMPERSAND-TOKEN>, production: 43),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 43),
                    make(<reduce>, on: <BAR-TOKEN>, production: 43),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 43),
                    make(<reduce>, on: <COLON-TOKEN>, production: 43),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 43),
                    make(<reduce>, on: <EOF-TOKEN>, production: 43),
                    make(<shift>, on: <EQ-OP-TOKEN>, state: 72),
                    make(<shift>, on: <NE-OP-TOKEN>, state: 70),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 43),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 43),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 43),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 43),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 43),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 43));

*action-table*[77] :=
  // EXCLUSIVE-OR-EXPR -> EXCLUSIVE-OR-EXPR * <CARAT-TOKEN> AND-EXPR
  // INCLUSIVE-OR-EXPR -> EXCLUSIVE-OR-EXPR *
  make-action-table(make(<reduce>, on: <AND-OP-TOKEN>, production: 46),
                    make(<reduce>, on: <BAR-TOKEN>, production: 46),
                    make(<shift>, on: <CARAT-TOKEN>, state: 78),
                    make(<reduce>, on: <COLON-TOKEN>, production: 46),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 46),
                    make(<reduce>, on: <EOF-TOKEN>, production: 46),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 46),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 46),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 46),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 46),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 46),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 46));

*action-table*[78] :=
  // EXCLUSIVE-OR-EXPR -> EXCLUSIVE-OR-EXPR <CARAT-TOKEN> * AND-EXPR
  make-action-table(make(<shift>, on: <BANG-TOKEN>, state: 28),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 22),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 38),
                    make(<shift>, on: <MINUS-TOKEN>, state: 26),
                    make(<shift>, on: <SIZEOF-TOKEN>, state: 30),
                    make(<shift>, on: <TILDE-TOKEN>, state: 27));

*action-table*[79] :=
  // AND-EXPR -> AND-EXPR * <AMPERSAND-TOKEN> EQUALITY-EXPR
  // EXCLUSIVE-OR-EXPR -> EXCLUSIVE-OR-EXPR <CARAT-TOKEN> AND-EXPR *
  make-action-table(make(<shift>, on: <AMPERSAND-TOKEN>, state: 75),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 45),
                    make(<reduce>, on: <BAR-TOKEN>, production: 45),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 45),
                    make(<reduce>, on: <COLON-TOKEN>, production: 45),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 45),
                    make(<reduce>, on: <EOF-TOKEN>, production: 45),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 45),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 45),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 45),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 45),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 45),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 45));

*action-table*[80] :=
  // INCLUSIVE-OR-EXPR -> INCLUSIVE-OR-EXPR * <BAR-TOKEN> EXCLUSIVE-OR-EXPR
  // LOGICAL-AND-EXPR -> INCLUSIVE-OR-EXPR *
  make-action-table(make(<reduce>, on: <AND-OP-TOKEN>, production: 48),
                    make(<shift>, on: <BAR-TOKEN>, state: 81),
                    make(<reduce>, on: <COLON-TOKEN>, production: 48),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 48),
                    make(<reduce>, on: <EOF-TOKEN>, production: 48),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 48),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 48),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 48),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 48),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 48),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 48));

*action-table*[81] :=
  // INCLUSIVE-OR-EXPR -> INCLUSIVE-OR-EXPR <BAR-TOKEN> * EXCLUSIVE-OR-EXPR
  make-action-table(make(<shift>, on: <BANG-TOKEN>, state: 28),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 22),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 38),
                    make(<shift>, on: <MINUS-TOKEN>, state: 26),
                    make(<shift>, on: <SIZEOF-TOKEN>, state: 30),
                    make(<shift>, on: <TILDE-TOKEN>, state: 27));

*action-table*[82] :=
  // EXCLUSIVE-OR-EXPR -> EXCLUSIVE-OR-EXPR * <CARAT-TOKEN> AND-EXPR
  // INCLUSIVE-OR-EXPR -> INCLUSIVE-OR-EXPR <BAR-TOKEN> EXCLUSIVE-OR-EXPR *
  make-action-table(make(<reduce>, on: <AND-OP-TOKEN>, production: 47),
                    make(<reduce>, on: <BAR-TOKEN>, production: 47),
                    make(<shift>, on: <CARAT-TOKEN>, state: 78),
                    make(<reduce>, on: <COLON-TOKEN>, production: 47),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 47),
                    make(<reduce>, on: <EOF-TOKEN>, production: 47),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 47),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 47),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 47),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 47),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 47),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 47));

*action-table*[83] :=
  // LOGICAL-AND-EXPR -> LOGICAL-AND-EXPR * <AND-OP-TOKEN> INCLUSIVE-OR-EXPR
  // LOGICAL-OR-EXPR -> LOGICAL-AND-EXPR *
  make-action-table(make(<shift>, on: <AND-OP-TOKEN>, state: 84),
                    make(<reduce>, on: <COLON-TOKEN>, production: 50),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 50),
                    make(<reduce>, on: <EOF-TOKEN>, production: 50),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 50),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 50),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 50),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 50),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 50),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 50));

*action-table*[84] :=
  // LOGICAL-AND-EXPR -> LOGICAL-AND-EXPR <AND-OP-TOKEN> * INCLUSIVE-OR-EXPR
  make-action-table(make(<shift>, on: <BANG-TOKEN>, state: 28),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 22),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 38),
                    make(<shift>, on: <MINUS-TOKEN>, state: 26),
                    make(<shift>, on: <SIZEOF-TOKEN>, state: 30),
                    make(<shift>, on: <TILDE-TOKEN>, state: 27));

*action-table*[85] :=
  // INCLUSIVE-OR-EXPR -> INCLUSIVE-OR-EXPR * <BAR-TOKEN> EXCLUSIVE-OR-EXPR
  // LOGICAL-AND-EXPR -> LOGICAL-AND-EXPR <AND-OP-TOKEN> INCLUSIVE-OR-EXPR *
  make-action-table(make(<reduce>, on: <AND-OP-TOKEN>, production: 49),
                    make(<shift>, on: <BAR-TOKEN>, state: 81),
                    make(<reduce>, on: <COLON-TOKEN>, production: 49),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 49),
                    make(<reduce>, on: <EOF-TOKEN>, production: 49),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 49),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 49),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 49),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 49),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 49),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 49));

*action-table*[86] :=
  // CONDITIONAL-EXPR -> LOGICAL-OR-EXPR *
  // LOGICAL-OR-EXPR -> LOGICAL-OR-EXPR * <OR-OP-TOKEN> LOGICAL-AND-EXPR
  // CONDITIONAL-EXPR -> LOGICAL-OR-EXPR * <QUESTION-TOKEN> LOGICAL-OR-EXPR <COLON-TOKEN> CONDITIONAL-EXPR
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 52),
                    make(<reduce>, on: <EOF-TOKEN>, production: 52),
                    make(<shift>, on: <OR-OP-TOKEN>, state: 91),
                    make(<shift>, on: <QUESTION-TOKEN>, state: 87),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 52),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 52),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 52),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 52));

*action-table*[87] :=
  // CONDITIONAL-EXPR -> LOGICAL-OR-EXPR <QUESTION-TOKEN> * LOGICAL-OR-EXPR <COLON-TOKEN> CONDITIONAL-EXPR
  make-action-table(make(<shift>, on: <BANG-TOKEN>, state: 28),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 22),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 38),
                    make(<shift>, on: <MINUS-TOKEN>, state: 26),
                    make(<shift>, on: <SIZEOF-TOKEN>, state: 30),
                    make(<shift>, on: <TILDE-TOKEN>, state: 27));

*action-table*[88] :=
  // LOGICAL-OR-EXPR -> LOGICAL-OR-EXPR * <OR-OP-TOKEN> LOGICAL-AND-EXPR
  // CONDITIONAL-EXPR -> LOGICAL-OR-EXPR <QUESTION-TOKEN> LOGICAL-OR-EXPR * <COLON-TOKEN> CONDITIONAL-EXPR
  make-action-table(make(<shift>, on: <COLON-TOKEN>, state: 89),
                    make(<shift>, on: <OR-OP-TOKEN>, state: 91));

*action-table*[89] :=
  // CONDITIONAL-EXPR -> LOGICAL-OR-EXPR <QUESTION-TOKEN> LOGICAL-OR-EXPR <COLON-TOKEN> * CONDITIONAL-EXPR
  make-action-table(make(<shift>, on: <BANG-TOKEN>, state: 28),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 22),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 38),
                    make(<shift>, on: <MINUS-TOKEN>, state: 26),
                    make(<shift>, on: <SIZEOF-TOKEN>, state: 30),
                    make(<shift>, on: <TILDE-TOKEN>, state: 27));

*action-table*[90] :=
  // CONDITIONAL-EXPR -> LOGICAL-OR-EXPR <QUESTION-TOKEN> LOGICAL-OR-EXPR <COLON-TOKEN> CONDITIONAL-EXPR *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 53),
                    make(<reduce>, on: <EOF-TOKEN>, production: 53),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 53),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 53),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 53),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 53));

*action-table*[91] :=
  // LOGICAL-OR-EXPR -> LOGICAL-OR-EXPR <OR-OP-TOKEN> * LOGICAL-AND-EXPR
  make-action-table(make(<shift>, on: <BANG-TOKEN>, state: 28),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 22),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 38),
                    make(<shift>, on: <MINUS-TOKEN>, state: 26),
                    make(<shift>, on: <SIZEOF-TOKEN>, state: 30),
                    make(<shift>, on: <TILDE-TOKEN>, state: 27));

*action-table*[92] :=
  // LOGICAL-AND-EXPR -> LOGICAL-AND-EXPR * <AND-OP-TOKEN> INCLUSIVE-OR-EXPR
  // LOGICAL-OR-EXPR -> LOGICAL-OR-EXPR <OR-OP-TOKEN> LOGICAL-AND-EXPR *
  make-action-table(make(<shift>, on: <AND-OP-TOKEN>, state: 84),
                    make(<reduce>, on: <COLON-TOKEN>, production: 51),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 51),
                    make(<reduce>, on: <EOF-TOKEN>, production: 51),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 51),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 51),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 51),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 51),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 51),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 51));

*action-table*[93] :=
  // ASSIGNMENT-EXPR -> CONDITIONAL-EXPR *
  make-action-table(make(<reduce>, on: <RPAREN-TOKEN>, production: 54));

*action-table*[94] :=
  // EXPR -> ASSIGNMENT-EXPR *
  make-action-table(make(<reduce>, on: <RPAREN-TOKEN>, production: 55));

*action-table*[95] :=
  // PRIMARY-EXPR -> <LPAREN-TOKEN> EXPR * <RPAREN-TOKEN>
  make-action-table(make(<shift>, on: <RPAREN-TOKEN>, state: 96));

*action-table*[96] :=
  // PRIMARY-EXPR -> <LPAREN-TOKEN> EXPR <RPAREN-TOKEN> *
  make-action-table(make(<reduce>, on: <AMPERSAND-TOKEN>, production: 12),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 12),
                    make(<reduce>, on: <BAR-TOKEN>, production: 12),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 12),
                    make(<reduce>, on: <COLON-TOKEN>, production: 12),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 12),
                    make(<reduce>, on: <EOF-TOKEN>, production: 12),
                    make(<reduce>, on: <EQ-OP-TOKEN>, production: 12),
                    make(<reduce>, on: <GE-OP-TOKEN>, production: 12),
                    make(<reduce>, on: <GT-TOKEN>, production: 12),
                    make(<reduce>, on: <LE-OP-TOKEN>, production: 12),
                    make(<reduce>, on: <LEFT-OP-TOKEN>, production: 12),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 12),
                    make(<reduce>, on: <LT-TOKEN>, production: 12),
                    make(<reduce>, on: <MINUS-TOKEN>, production: 12),
                    make(<reduce>, on: <NE-OP-TOKEN>, production: 12),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 12),
                    make(<reduce>, on: <PERCENT-TOKEN>, production: 12),
                    make(<reduce>, on: <PLUS-TOKEN>, production: 12),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 12),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 12),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 12),
                    make(<reduce>, on: <RIGHT-OP-TOKEN>, production: 12),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 12),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 12),
                    make(<reduce>, on: <SLASH-TOKEN>, production: 12),
                    make(<reduce>, on: <STAR-TOKEN>, production: 12));

*action-table*[97] :=
  // CONSTANT-EXPR -> CONDITIONAL-EXPR *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 56),
                    make(<reduce>, on: <EOF-TOKEN>, production: 56),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 56),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 56),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 56));

*action-table*[98] :=
  // ABSTRACT-DECLARATOR2 -> <LBRACKET-TOKEN> CONSTANT-EXPR * <RBRACKET-TOKEN>
  make-action-table(make(<shift>, on: <RBRACKET-TOKEN>, state: 99));

*action-table*[99] :=
  // ABSTRACT-DECLARATOR2 -> <LBRACKET-TOKEN> CONSTANT-EXPR <RBRACKET-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 125),
                    make(<reduce>, on: <EOF-TOKEN>, production: 125),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 125),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 125),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 125));

*action-table*[100] :=
  // ABSTRACT-DECLARATOR2 -> <LBRACKET-TOKEN> <RBRACKET-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 124),
                    make(<reduce>, on: <EOF-TOKEN>, production: 124),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 124),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 124),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 124));

*action-table*[101] :=
  // ABSTRACT-DECLARATOR2 -> <LPAREN-TOKEN> * ABSTRACT-DECLARATOR <RPAREN-TOKEN>
  // ABSTRACT-DECLARATOR2 -> <LPAREN-TOKEN> * <RPAREN-TOKEN>
  // ABSTRACT-DECLARATOR2 -> <LPAREN-TOKEN> * PARAMETER-TYPE-LIST <RPAREN-TOKEN>
  make-action-table(make(<shift>, on: <CONST-TOKEN>, state: 8),
                    make(<shift>, on: <ENUM-TOKEN>, state: 15),
                    make(<shift>, on: <LBRACKET-TOKEN>, state: 36),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 101),
                    make(<shift>, on: <RPAREN-TOKEN>, state: 142),
                    make(<shift>, on: <STAR-TOKEN>, state: 143),
                    make(<shift>, on: <STRUCT-TOKEN>, state: 10),
                    make(<shift>, on: <TYPE-NAME-TOKEN>, state: 33),
                    make(<shift>, on: <TYPE-SPECIFIER-TOKEN>, state: 7),
                    make(<shift>, on: <UNION-TOKEN>, state: 11),
                    make(<shift>, on: <VOLATILE-TOKEN>, state: 9));

*action-table*[102] :=
  // PARAMETER-DECLARATION -> TYPE-SPECIFIER-LIST * DECLARATOR
  // TYPE-NAME -> TYPE-SPECIFIER-LIST *
  // TYPE-SPECIFIER-LIST -> TYPE-SPECIFIER-LIST * TYPE-SPECIFIER
  // TYPE-NAME -> TYPE-SPECIFIER-LIST * ABSTRACT-DECLARATOR
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 118),
                    make(<shift>, on: <CONST-TOKEN>, state: 8),
                    make(<shift>, on: <ENUM-TOKEN>, state: 15),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <LBRACKET-TOKEN>, state: 36),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 121),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 118),
                    make(<shift>, on: <STAR-TOKEN>, state: 143),
                    make(<shift>, on: <STRUCT-TOKEN>, state: 10),
                    make(<shift>, on: <TYPE-NAME-TOKEN>, state: 33),
                    make(<shift>, on: <TYPE-SPECIFIER-TOKEN>, state: 7),
                    make(<shift>, on: <UNION-TOKEN>, state: 11),
                    make(<shift>, on: <VOLATILE-TOKEN>, state: 9));

*action-table*[103] :=
  // ABSTRACT-DECLARATOR2 -> ABSTRACT-DECLARATOR2 * <LBRACKET-TOKEN> <RBRACKET-TOKEN>
  // ABSTRACT-DECLARATOR2 -> ABSTRACT-DECLARATOR2 * <LBRACKET-TOKEN> CONSTANT-EXPR <RBRACKET-TOKEN>
  // ABSTRACT-DECLARATOR2 -> ABSTRACT-DECLARATOR2 * <LPAREN-TOKEN> <RPAREN-TOKEN>
  // ABSTRACT-DECLARATOR2 -> ABSTRACT-DECLARATOR2 * <LPAREN-TOKEN> PARAMETER-TYPE-LIST <RPAREN-TOKEN>
  // ABSTRACT-DECLARATOR -> ABSTRACT-DECLARATOR2 *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 121),
                    make(<reduce>, on: <EOF-TOKEN>, production: 121),
                    make(<shift>, on: <LBRACKET-TOKEN>, state: 114),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 104),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 121));

*action-table*[104] :=
  // ABSTRACT-DECLARATOR2 -> ABSTRACT-DECLARATOR2 <LPAREN-TOKEN> * PARAMETER-TYPE-LIST <RPAREN-TOKEN>
  // ABSTRACT-DECLARATOR2 -> ABSTRACT-DECLARATOR2 <LPAREN-TOKEN> * <RPAREN-TOKEN>
  make-action-table(make(<shift>, on: <CONST-TOKEN>, state: 8),
                    make(<shift>, on: <ENUM-TOKEN>, state: 15),
                    make(<shift>, on: <RPAREN-TOKEN>, state: 105),
                    make(<shift>, on: <STRUCT-TOKEN>, state: 10),
                    make(<shift>, on: <TYPE-NAME-TOKEN>, state: 33),
                    make(<shift>, on: <TYPE-SPECIFIER-TOKEN>, state: 7),
                    make(<shift>, on: <UNION-TOKEN>, state: 11),
                    make(<shift>, on: <VOLATILE-TOKEN>, state: 9));

*action-table*[105] :=
  // ABSTRACT-DECLARATOR2 -> ABSTRACT-DECLARATOR2 <LPAREN-TOKEN> <RPAREN-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 130),
                    make(<reduce>, on: <EOF-TOKEN>, production: 130),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 130),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 130),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 130));

*action-table*[106] :=
  // PARAMETER-DECLARATION -> TYPE-NAME *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 117),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 117));

*action-table*[107] :=
  // PARAMETER-LIST -> PARAMETER-DECLARATION *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 114),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 114));

*action-table*[108] :=
  // PARAMETER-TYPE-LIST -> PARAMETER-LIST *
  // PARAMETER-LIST -> PARAMETER-LIST * <COMMA-TOKEN> PARAMETER-DECLARATION
  // PARAMETER-TYPE-LIST -> PARAMETER-LIST * <COMMA-TOKEN> <ELIPSIS-TOKEN>
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 109),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 112));

*action-table*[109] :=
  // PARAMETER-TYPE-LIST -> PARAMETER-LIST <COMMA-TOKEN> * <ELIPSIS-TOKEN>
  // PARAMETER-LIST -> PARAMETER-LIST <COMMA-TOKEN> * PARAMETER-DECLARATION
  make-action-table(make(<shift>, on: <CONST-TOKEN>, state: 8),
                    make(<shift>, on: <ELIPSIS-TOKEN>, state: 111),
                    make(<shift>, on: <ENUM-TOKEN>, state: 15),
                    make(<shift>, on: <STRUCT-TOKEN>, state: 10),
                    make(<shift>, on: <TYPE-NAME-TOKEN>, state: 33),
                    make(<shift>, on: <TYPE-SPECIFIER-TOKEN>, state: 7),
                    make(<shift>, on: <UNION-TOKEN>, state: 11),
                    make(<shift>, on: <VOLATILE-TOKEN>, state: 9));

*action-table*[110] :=
  // PARAMETER-LIST -> PARAMETER-LIST <COMMA-TOKEN> PARAMETER-DECLARATION *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 115),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 115));

*action-table*[111] :=
  // PARAMETER-TYPE-LIST -> PARAMETER-LIST <COMMA-TOKEN> <ELIPSIS-TOKEN> *
  make-action-table(make(<reduce>, on: <RPAREN-TOKEN>, production: 113));

*action-table*[112] :=
  // ABSTRACT-DECLARATOR2 -> ABSTRACT-DECLARATOR2 <LPAREN-TOKEN> PARAMETER-TYPE-LIST * <RPAREN-TOKEN>
  make-action-table(make(<shift>, on: <RPAREN-TOKEN>, state: 113));

*action-table*[113] :=
  // ABSTRACT-DECLARATOR2 -> ABSTRACT-DECLARATOR2 <LPAREN-TOKEN> PARAMETER-TYPE-LIST <RPAREN-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 131),
                    make(<reduce>, on: <EOF-TOKEN>, production: 131),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 131),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 131),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 131));

*action-table*[114] :=
  // ABSTRACT-DECLARATOR2 -> ABSTRACT-DECLARATOR2 <LBRACKET-TOKEN> * CONSTANT-EXPR <RBRACKET-TOKEN>
  // ABSTRACT-DECLARATOR2 -> ABSTRACT-DECLARATOR2 <LBRACKET-TOKEN> * <RBRACKET-TOKEN>
  make-action-table(make(<shift>, on: <BANG-TOKEN>, state: 28),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 22),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 38),
                    make(<shift>, on: <MINUS-TOKEN>, state: 26),
                    make(<shift>, on: <RBRACKET-TOKEN>, state: 115),
                    make(<shift>, on: <SIZEOF-TOKEN>, state: 30),
                    make(<shift>, on: <TILDE-TOKEN>, state: 27));

*action-table*[115] :=
  // ABSTRACT-DECLARATOR2 -> ABSTRACT-DECLARATOR2 <LBRACKET-TOKEN> <RBRACKET-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 126),
                    make(<reduce>, on: <EOF-TOKEN>, production: 126),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 126),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 126),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 126));

*action-table*[116] :=
  // ABSTRACT-DECLARATOR2 -> ABSTRACT-DECLARATOR2 <LBRACKET-TOKEN> CONSTANT-EXPR * <RBRACKET-TOKEN>
  make-action-table(make(<shift>, on: <RBRACKET-TOKEN>, state: 117));

*action-table*[117] :=
  // ABSTRACT-DECLARATOR2 -> ABSTRACT-DECLARATOR2 <LBRACKET-TOKEN> CONSTANT-EXPR <RBRACKET-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 127),
                    make(<reduce>, on: <EOF-TOKEN>, production: 127),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 127),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 127),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 127));

*action-table*[118] :=
  // TYPE-NAME -> TYPE-SPECIFIER-LIST ABSTRACT-DECLARATOR *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 119),
                    make(<reduce>, on: <EOF-TOKEN>, production: 119),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 119));

*action-table*[119] :=
  // TYPE-SPECIFIER-LIST -> TYPE-SPECIFIER-LIST TYPE-SPECIFIER *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 107),
                    make(<reduce>, on: <CONST-TOKEN>, production: 107),
                    make(<reduce>, on: <ENUM-TOKEN>, production: 107),
                    make(<reduce>, on: <EOF-TOKEN>, production: 107),
                    make(<reduce>, on: <IDENTIFIER-TOKEN>, production: 107),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 107),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 107),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 107),
                    make(<reduce>, on: <STAR-TOKEN>, production: 107),
                    make(<reduce>, on: <STRUCT-TOKEN>, production: 107),
                    make(<reduce>, on: <TYPE-NAME-TOKEN>, production: 107),
                    make(<reduce>, on: <TYPE-SPECIFIER-TOKEN>, production: 107),
                    make(<reduce>, on: <UNION-TOKEN>, production: 107),
                    make(<reduce>, on: <VOLATILE-TOKEN>, production: 107));

*action-table*[120] :=
  // DECLARATOR2 -> IDENTIFIER *
  make-action-table(make(<reduce>, on: <COLON-TOKEN>, production: 95),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 95),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 95),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 95),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 95),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 95));

*action-table*[121] :=
  // ABSTRACT-DECLARATOR2 -> <LPAREN-TOKEN> * ABSTRACT-DECLARATOR <RPAREN-TOKEN>
  // ABSTRACT-DECLARATOR2 -> <LPAREN-TOKEN> * <RPAREN-TOKEN>
  // ABSTRACT-DECLARATOR2 -> <LPAREN-TOKEN> * PARAMETER-TYPE-LIST <RPAREN-TOKEN>
  // DECLARATOR2 -> <LPAREN-TOKEN> * DECLARATOR <RPAREN-TOKEN>
  make-action-table(make(<shift>, on: <CONST-TOKEN>, state: 8),
                    make(<shift>, on: <ENUM-TOKEN>, state: 15),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <LBRACKET-TOKEN>, state: 36),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 121),
                    make(<shift>, on: <RPAREN-TOKEN>, state: 142),
                    make(<shift>, on: <STAR-TOKEN>, state: 143),
                    make(<shift>, on: <STRUCT-TOKEN>, state: 10),
                    make(<shift>, on: <TYPE-NAME-TOKEN>, state: 33),
                    make(<shift>, on: <TYPE-SPECIFIER-TOKEN>, state: 7),
                    make(<shift>, on: <UNION-TOKEN>, state: 11),
                    make(<shift>, on: <VOLATILE-TOKEN>, state: 9));

*action-table*[122] :=
  // DECLARATOR2 -> DECLARATOR2 * <LBRACKET-TOKEN> <RBRACKET-TOKEN>
  // DECLARATOR2 -> DECLARATOR2 * <LBRACKET-TOKEN> CONSTANT-EXPR <RBRACKET-TOKEN>
  // DECLARATOR2 -> DECLARATOR2 * <LPAREN-TOKEN> <RPAREN-TOKEN>
  // DECLARATOR2 -> DECLARATOR2 * <LPAREN-TOKEN> PARAMETER-TYPE-LIST <RPAREN-TOKEN>
  // DECLARATOR2 -> DECLARATOR2 * <LPAREN-TOKEN> PARAMETER-IDENTIFIER-LIST <RPAREN-TOKEN>
  // DECLARATOR -> DECLARATOR2 *
  make-action-table(make(<reduce>, on: <COLON-TOKEN>, production: 93),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 93),
                    make(<shift>, on: <LBRACKET-TOKEN>, state: 134),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 123),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 93),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 93));

*action-table*[123] :=
  // DECLARATOR2 -> DECLARATOR2 <LPAREN-TOKEN> * PARAMETER-IDENTIFIER-LIST <RPAREN-TOKEN>
  // DECLARATOR2 -> DECLARATOR2 <LPAREN-TOKEN> * PARAMETER-TYPE-LIST <RPAREN-TOKEN>
  // DECLARATOR2 -> DECLARATOR2 <LPAREN-TOKEN> * <RPAREN-TOKEN>
  make-action-table(make(<shift>, on: <CONST-TOKEN>, state: 8),
                    make(<shift>, on: <ENUM-TOKEN>, state: 15),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <RPAREN-TOKEN>, state: 124),
                    make(<shift>, on: <STRUCT-TOKEN>, state: 10),
                    make(<shift>, on: <TYPE-NAME-TOKEN>, state: 33),
                    make(<shift>, on: <TYPE-SPECIFIER-TOKEN>, state: 7),
                    make(<shift>, on: <UNION-TOKEN>, state: 11),
                    make(<shift>, on: <VOLATILE-TOKEN>, state: 9));

*action-table*[124] :=
  // DECLARATOR2 -> DECLARATOR2 <LPAREN-TOKEN> <RPAREN-TOKEN> *
  make-action-table(make(<reduce>, on: <COLON-TOKEN>, production: 99),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 99),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 99),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 99),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 99),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 99));

*action-table*[125] :=
  // DECLARATOR2 -> DECLARATOR2 <LPAREN-TOKEN> PARAMETER-TYPE-LIST * <RPAREN-TOKEN>
  make-action-table(make(<shift>, on: <RPAREN-TOKEN>, state: 126));

*action-table*[126] :=
  // DECLARATOR2 -> DECLARATOR2 <LPAREN-TOKEN> PARAMETER-TYPE-LIST <RPAREN-TOKEN> *
  make-action-table(make(<reduce>, on: <COLON-TOKEN>, production: 100),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 100),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 100),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 100),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 100),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 100));

*action-table*[127] :=
  // IDENTIFIER-LIST -> IDENTIFIER *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 110),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 110));

*action-table*[128] :=
  // PARAMETER-IDENTIFIER-LIST -> IDENTIFIER-LIST *
  // IDENTIFIER-LIST -> IDENTIFIER-LIST * <COMMA-TOKEN> IDENTIFIER
  // PARAMETER-IDENTIFIER-LIST -> IDENTIFIER-LIST * <COMMA-TOKEN> <ELIPSIS-TOKEN>
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 129),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 108));

*action-table*[129] :=
  // PARAMETER-IDENTIFIER-LIST -> IDENTIFIER-LIST <COMMA-TOKEN> * <ELIPSIS-TOKEN>
  // IDENTIFIER-LIST -> IDENTIFIER-LIST <COMMA-TOKEN> * IDENTIFIER
  make-action-table(make(<shift>, on: <ELIPSIS-TOKEN>, state: 131),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18));

*action-table*[130] :=
  // IDENTIFIER-LIST -> IDENTIFIER-LIST <COMMA-TOKEN> IDENTIFIER *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 111),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 111));

*action-table*[131] :=
  // PARAMETER-IDENTIFIER-LIST -> IDENTIFIER-LIST <COMMA-TOKEN> <ELIPSIS-TOKEN> *
  make-action-table(make(<reduce>, on: <RPAREN-TOKEN>, production: 109));

*action-table*[132] :=
  // DECLARATOR2 -> DECLARATOR2 <LPAREN-TOKEN> PARAMETER-IDENTIFIER-LIST * <RPAREN-TOKEN>
  make-action-table(make(<shift>, on: <RPAREN-TOKEN>, state: 133));

*action-table*[133] :=
  // DECLARATOR2 -> DECLARATOR2 <LPAREN-TOKEN> PARAMETER-IDENTIFIER-LIST <RPAREN-TOKEN> *
  make-action-table(make(<reduce>, on: <COLON-TOKEN>, production: 101),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 101),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 101),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 101),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 101),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 101));

*action-table*[134] :=
  // DECLARATOR2 -> DECLARATOR2 <LBRACKET-TOKEN> * CONSTANT-EXPR <RBRACKET-TOKEN>
  // DECLARATOR2 -> DECLARATOR2 <LBRACKET-TOKEN> * <RBRACKET-TOKEN>
  make-action-table(make(<shift>, on: <BANG-TOKEN>, state: 28),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 22),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 38),
                    make(<shift>, on: <MINUS-TOKEN>, state: 26),
                    make(<shift>, on: <RBRACKET-TOKEN>, state: 135),
                    make(<shift>, on: <SIZEOF-TOKEN>, state: 30),
                    make(<shift>, on: <TILDE-TOKEN>, state: 27));

*action-table*[135] :=
  // DECLARATOR2 -> DECLARATOR2 <LBRACKET-TOKEN> <RBRACKET-TOKEN> *
  make-action-table(make(<reduce>, on: <COLON-TOKEN>, production: 97),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 97),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 97),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 97),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 97),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 97));

*action-table*[136] :=
  // DECLARATOR2 -> DECLARATOR2 <LBRACKET-TOKEN> CONSTANT-EXPR * <RBRACKET-TOKEN>
  make-action-table(make(<shift>, on: <RBRACKET-TOKEN>, state: 137));

*action-table*[137] :=
  // DECLARATOR2 -> DECLARATOR2 <LBRACKET-TOKEN> CONSTANT-EXPR <RBRACKET-TOKEN> *
  make-action-table(make(<reduce>, on: <COLON-TOKEN>, production: 98),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 98),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 98),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 98),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 98),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 98));

*action-table*[138] :=
  // DECLARATOR2 -> <LPAREN-TOKEN> DECLARATOR * <RPAREN-TOKEN>
  make-action-table(make(<shift>, on: <RPAREN-TOKEN>, state: 139));

*action-table*[139] :=
  // DECLARATOR2 -> <LPAREN-TOKEN> DECLARATOR <RPAREN-TOKEN> *
  make-action-table(make(<reduce>, on: <COLON-TOKEN>, production: 96),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 96),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 96),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 96),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 96),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 96));

*action-table*[140] :=
  // ABSTRACT-DECLARATOR2 -> <LPAREN-TOKEN> PARAMETER-TYPE-LIST * <RPAREN-TOKEN>
  make-action-table(make(<shift>, on: <RPAREN-TOKEN>, state: 141));

*action-table*[141] :=
  // ABSTRACT-DECLARATOR2 -> <LPAREN-TOKEN> PARAMETER-TYPE-LIST <RPAREN-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 129),
                    make(<reduce>, on: <EOF-TOKEN>, production: 129),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 129),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 129),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 129));

*action-table*[142] :=
  // ABSTRACT-DECLARATOR2 -> <LPAREN-TOKEN> <RPAREN-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 128),
                    make(<reduce>, on: <EOF-TOKEN>, production: 128),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 128),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 128),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 128));

*action-table*[143] :=
  // POINTER -> <STAR-TOKEN> *
  // POINTER -> <STAR-TOKEN> * TYPE-SPECIFIER-LIST
  // POINTER -> <STAR-TOKEN> * POINTER
  // POINTER -> <STAR-TOKEN> * TYPE-SPECIFIER-LIST POINTER
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 102),
                    make(<shift>, on: <CONST-TOKEN>, state: 8),
                    make(<shift>, on: <ENUM-TOKEN>, state: 15),
                    make(<reduce>, on: <EOF-TOKEN>, production: 102),
                    make(<reduce>, on: <IDENTIFIER-TOKEN>, production: 102),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 102),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 102),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 102),
                    make(<shift>, on: <STAR-TOKEN>, state: 143),
                    make(<shift>, on: <STRUCT-TOKEN>, state: 10),
                    make(<shift>, on: <TYPE-NAME-TOKEN>, state: 33),
                    make(<shift>, on: <TYPE-SPECIFIER-TOKEN>, state: 7),
                    make(<shift>, on: <UNION-TOKEN>, state: 11),
                    make(<shift>, on: <VOLATILE-TOKEN>, state: 9));

*action-table*[144] :=
  // POINTER -> <STAR-TOKEN> POINTER *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 104),
                    make(<reduce>, on: <EOF-TOKEN>, production: 104),
                    make(<reduce>, on: <IDENTIFIER-TOKEN>, production: 104),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 104),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 104),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 104));

*action-table*[145] :=
  // POINTER -> <STAR-TOKEN> TYPE-SPECIFIER-LIST * POINTER
  // TYPE-SPECIFIER-LIST -> TYPE-SPECIFIER-LIST * TYPE-SPECIFIER
  // POINTER -> <STAR-TOKEN> TYPE-SPECIFIER-LIST *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 103),
                    make(<shift>, on: <CONST-TOKEN>, state: 8),
                    make(<shift>, on: <ENUM-TOKEN>, state: 15),
                    make(<reduce>, on: <EOF-TOKEN>, production: 103),
                    make(<reduce>, on: <IDENTIFIER-TOKEN>, production: 103),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 103),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 103),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 103),
                    make(<shift>, on: <STAR-TOKEN>, state: 143),
                    make(<shift>, on: <STRUCT-TOKEN>, state: 10),
                    make(<shift>, on: <TYPE-NAME-TOKEN>, state: 33),
                    make(<shift>, on: <TYPE-SPECIFIER-TOKEN>, state: 7),
                    make(<shift>, on: <UNION-TOKEN>, state: 11),
                    make(<shift>, on: <VOLATILE-TOKEN>, state: 9));

*action-table*[146] :=
  // POINTER -> <STAR-TOKEN> TYPE-SPECIFIER-LIST POINTER *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 105),
                    make(<reduce>, on: <EOF-TOKEN>, production: 105),
                    make(<reduce>, on: <IDENTIFIER-TOKEN>, production: 105),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 105),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 105),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 105));

*action-table*[147] :=
  // DECLARATOR -> POINTER * DECLARATOR2
  // ABSTRACT-DECLARATOR -> POINTER *
  // ABSTRACT-DECLARATOR -> POINTER * ABSTRACT-DECLARATOR2
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 120),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <LBRACKET-TOKEN>, state: 36),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 121),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 120));

*action-table*[148] :=
  // ABSTRACT-DECLARATOR2 -> ABSTRACT-DECLARATOR2 * <LBRACKET-TOKEN> <RBRACKET-TOKEN>
  // ABSTRACT-DECLARATOR2 -> ABSTRACT-DECLARATOR2 * <LBRACKET-TOKEN> CONSTANT-EXPR <RBRACKET-TOKEN>
  // ABSTRACT-DECLARATOR2 -> ABSTRACT-DECLARATOR2 * <LPAREN-TOKEN> <RPAREN-TOKEN>
  // ABSTRACT-DECLARATOR2 -> ABSTRACT-DECLARATOR2 * <LPAREN-TOKEN> PARAMETER-TYPE-LIST <RPAREN-TOKEN>
  // ABSTRACT-DECLARATOR -> POINTER ABSTRACT-DECLARATOR2 *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 122),
                    make(<reduce>, on: <EOF-TOKEN>, production: 122),
                    make(<shift>, on: <LBRACKET-TOKEN>, state: 114),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 104),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 122));

*action-table*[149] :=
  // DECLARATOR2 -> DECLARATOR2 * <LBRACKET-TOKEN> <RBRACKET-TOKEN>
  // DECLARATOR2 -> DECLARATOR2 * <LBRACKET-TOKEN> CONSTANT-EXPR <RBRACKET-TOKEN>
  // DECLARATOR2 -> DECLARATOR2 * <LPAREN-TOKEN> <RPAREN-TOKEN>
  // DECLARATOR2 -> DECLARATOR2 * <LPAREN-TOKEN> PARAMETER-TYPE-LIST <RPAREN-TOKEN>
  // DECLARATOR2 -> DECLARATOR2 * <LPAREN-TOKEN> PARAMETER-IDENTIFIER-LIST <RPAREN-TOKEN>
  // DECLARATOR -> POINTER DECLARATOR2 *
  make-action-table(make(<reduce>, on: <COLON-TOKEN>, production: 94),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 94),
                    make(<shift>, on: <LBRACKET-TOKEN>, state: 134),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 123),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 94),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 94));

*action-table*[150] :=
  // ABSTRACT-DECLARATOR2 -> <LPAREN-TOKEN> ABSTRACT-DECLARATOR * <RPAREN-TOKEN>
  make-action-table(make(<shift>, on: <RPAREN-TOKEN>, state: 151));

*action-table*[151] :=
  // ABSTRACT-DECLARATOR2 -> <LPAREN-TOKEN> ABSTRACT-DECLARATOR <RPAREN-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 123),
                    make(<reduce>, on: <EOF-TOKEN>, production: 123),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 123),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 123),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 123));

*action-table*[152] :=
  // PARAMETER-DECLARATION -> TYPE-SPECIFIER-LIST DECLARATOR *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 116),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 116));

*action-table*[153] :=
  // ABSTRACT-DECLARATOR -> POINTER *
  // ABSTRACT-DECLARATOR -> POINTER * ABSTRACT-DECLARATOR2
  make-action-table(make(<reduce>, on: <EOF-TOKEN>, production: 120),
                    make(<shift>, on: <LBRACKET-TOKEN>, state: 36),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 101),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 120));

*action-table*[154] :=
  // UNARY-EXPR -> <SIZEOF-TOKEN> <LPAREN-TOKEN> TYPE-NAME * <RPAREN-TOKEN>
  make-action-table(make(<shift>, on: <RPAREN-TOKEN>, state: 155));

*action-table*[155] :=
  // UNARY-EXPR -> <SIZEOF-TOKEN> <LPAREN-TOKEN> TYPE-NAME <RPAREN-TOKEN> *
  make-action-table(make(<reduce>, on: <AMPERSAND-TOKEN>, production: 18),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 18),
                    make(<reduce>, on: <BAR-TOKEN>, production: 18),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 18),
                    make(<reduce>, on: <COLON-TOKEN>, production: 18),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 18),
                    make(<reduce>, on: <EOF-TOKEN>, production: 18),
                    make(<reduce>, on: <EQ-OP-TOKEN>, production: 18),
                    make(<reduce>, on: <GE-OP-TOKEN>, production: 18),
                    make(<reduce>, on: <GT-TOKEN>, production: 18),
                    make(<reduce>, on: <LE-OP-TOKEN>, production: 18),
                    make(<reduce>, on: <LEFT-OP-TOKEN>, production: 18),
                    make(<reduce>, on: <LT-TOKEN>, production: 18),
                    make(<reduce>, on: <MINUS-TOKEN>, production: 18),
                    make(<reduce>, on: <NE-OP-TOKEN>, production: 18),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 18),
                    make(<reduce>, on: <PERCENT-TOKEN>, production: 18),
                    make(<reduce>, on: <PLUS-TOKEN>, production: 18),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 18),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 18),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 18),
                    make(<reduce>, on: <RIGHT-OP-TOKEN>, production: 18),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 18),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 18),
                    make(<reduce>, on: <SLASH-TOKEN>, production: 18),
                    make(<reduce>, on: <STAR-TOKEN>, production: 18));

*action-table*[156] :=
  // UNARY-EXPR -> UNARY-OPERATOR CAST-EXPR *
  make-action-table(make(<reduce>, on: <AMPERSAND-TOKEN>, production: 17),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 17),
                    make(<reduce>, on: <BAR-TOKEN>, production: 17),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 17),
                    make(<reduce>, on: <COLON-TOKEN>, production: 17),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 17),
                    make(<reduce>, on: <EOF-TOKEN>, production: 17),
                    make(<reduce>, on: <EQ-OP-TOKEN>, production: 17),
                    make(<reduce>, on: <GE-OP-TOKEN>, production: 17),
                    make(<reduce>, on: <GT-TOKEN>, production: 17),
                    make(<reduce>, on: <LE-OP-TOKEN>, production: 17),
                    make(<reduce>, on: <LEFT-OP-TOKEN>, production: 17),
                    make(<reduce>, on: <LT-TOKEN>, production: 17),
                    make(<reduce>, on: <MINUS-TOKEN>, production: 17),
                    make(<reduce>, on: <NE-OP-TOKEN>, production: 17),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 17),
                    make(<reduce>, on: <PERCENT-TOKEN>, production: 17),
                    make(<reduce>, on: <PLUS-TOKEN>, production: 17),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 17),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 17),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 17),
                    make(<reduce>, on: <RIGHT-OP-TOKEN>, production: 17),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 17),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 17),
                    make(<reduce>, on: <SLASH-TOKEN>, production: 17),
                    make(<reduce>, on: <STAR-TOKEN>, production: 17));

*action-table*[157] :=
  // ARGUMENT-EXPR-LIST -> ASSIGNMENT-EXPR *
  make-action-table(make(<reduce>, on: <RPAREN-TOKEN>, production: 15));

*action-table*[158] :=
  // POSTFIX-EXPR -> POSTFIX-EXPR <LPAREN-TOKEN> ARGUMENT-EXPR-LIST * <RPAREN-TOKEN>
  make-action-table(make(<shift>, on: <RPAREN-TOKEN>, state: 159));

*action-table*[159] :=
  // POSTFIX-EXPR -> POSTFIX-EXPR <LPAREN-TOKEN> ARGUMENT-EXPR-LIST <RPAREN-TOKEN> *
  make-action-table(make(<reduce>, on: <AMPERSAND-TOKEN>, production: 14),
                    make(<reduce>, on: <AND-OP-TOKEN>, production: 14),
                    make(<reduce>, on: <BAR-TOKEN>, production: 14),
                    make(<reduce>, on: <CARAT-TOKEN>, production: 14),
                    make(<reduce>, on: <COLON-TOKEN>, production: 14),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 14),
                    make(<reduce>, on: <EOF-TOKEN>, production: 14),
                    make(<reduce>, on: <EQ-OP-TOKEN>, production: 14),
                    make(<reduce>, on: <GE-OP-TOKEN>, production: 14),
                    make(<reduce>, on: <GT-TOKEN>, production: 14),
                    make(<reduce>, on: <LE-OP-TOKEN>, production: 14),
                    make(<reduce>, on: <LEFT-OP-TOKEN>, production: 14),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 14),
                    make(<reduce>, on: <LT-TOKEN>, production: 14),
                    make(<reduce>, on: <MINUS-TOKEN>, production: 14),
                    make(<reduce>, on: <NE-OP-TOKEN>, production: 14),
                    make(<reduce>, on: <OR-OP-TOKEN>, production: 14),
                    make(<reduce>, on: <PERCENT-TOKEN>, production: 14),
                    make(<reduce>, on: <PLUS-TOKEN>, production: 14),
                    make(<reduce>, on: <QUESTION-TOKEN>, production: 14),
                    make(<reduce>, on: <RBRACKET-TOKEN>, production: 14),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 14),
                    make(<reduce>, on: <RIGHT-OP-TOKEN>, production: 14),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 14),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 14),
                    make(<reduce>, on: <SLASH-TOKEN>, production: 14),
                    make(<reduce>, on: <STAR-TOKEN>, production: 14));

*action-table*[160] :=
  // ENUMERATOR -> IDENTIFIER <ASSIGN-TOKEN> CONSTANT-EXPR *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 92),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 92));

*action-table*[161] :=
  // ENUMERATOR-LIST -> ENUMERATOR *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 89),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 89));

*action-table*[162] :=
  // ENUMERATOR-LIST -> ENUMERATOR-LIST * <COMMA-TOKEN> ENUMERATOR
  // ENUM-SPECIFIER -> <ENUM-TOKEN> <NAME-TOKEN> <LCURLY-TOKEN> ENUMERATOR-LIST * <RCURLY-TOKEN>
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 164),
                    make(<shift>, on: <RCURLY-TOKEN>, state: 163));

*action-table*[163] :=
  // ENUM-SPECIFIER -> <ENUM-TOKEN> <NAME-TOKEN> <LCURLY-TOKEN> ENUMERATOR-LIST <RCURLY-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 87),
                    make(<reduce>, on: <CONST-TOKEN>, production: 87),
                    make(<reduce>, on: <ENUM-TOKEN>, production: 87),
                    make(<reduce>, on: <EOF-TOKEN>, production: 87),
                    make(<reduce>, on: <EXTERN-TOKEN>, production: 87),
                    make(<reduce>, on: <IDENTIFIER-TOKEN>, production: 87),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 87),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 87),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 87),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 87),
                    make(<reduce>, on: <STAR-TOKEN>, production: 87),
                    make(<reduce>, on: <STRUCT-TOKEN>, production: 87),
                    make(<reduce>, on: <TYPE-NAME-TOKEN>, production: 87),
                    make(<reduce>, on: <TYPE-SPECIFIER-TOKEN>, production: 87),
                    make(<reduce>, on: <TYPEDEF-TOKEN>, production: 87),
                    make(<reduce>, on: <UNION-TOKEN>, production: 87),
                    make(<reduce>, on: <VOLATILE-TOKEN>, production: 87));

*action-table*[164] :=
  // ENUMERATOR-LIST -> ENUMERATOR-LIST <COMMA-TOKEN> * ENUMERATOR
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18));

*action-table*[165] :=
  // ENUMERATOR-LIST -> ENUMERATOR-LIST <COMMA-TOKEN> ENUMERATOR *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 90),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 90));

*action-table*[166] :=
  // ENUM-SPECIFIER -> <ENUM-TOKEN> <LCURLY-TOKEN> * ENUMERATOR-LIST <RCURLY-TOKEN>
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18));

*action-table*[167] :=
  // ENUMERATOR-LIST -> ENUMERATOR-LIST * <COMMA-TOKEN> ENUMERATOR
  // ENUM-SPECIFIER -> <ENUM-TOKEN> <LCURLY-TOKEN> ENUMERATOR-LIST * <RCURLY-TOKEN>
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 164),
                    make(<shift>, on: <RCURLY-TOKEN>, state: 168));

*action-table*[168] :=
  // ENUM-SPECIFIER -> <ENUM-TOKEN> <LCURLY-TOKEN> ENUMERATOR-LIST <RCURLY-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 86),
                    make(<reduce>, on: <CONST-TOKEN>, production: 86),
                    make(<reduce>, on: <ENUM-TOKEN>, production: 86),
                    make(<reduce>, on: <EOF-TOKEN>, production: 86),
                    make(<reduce>, on: <EXTERN-TOKEN>, production: 86),
                    make(<reduce>, on: <IDENTIFIER-TOKEN>, production: 86),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 86),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 86),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 86),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 86),
                    make(<reduce>, on: <STAR-TOKEN>, production: 86),
                    make(<reduce>, on: <STRUCT-TOKEN>, production: 86),
                    make(<reduce>, on: <TYPE-NAME-TOKEN>, production: 86),
                    make(<reduce>, on: <TYPE-SPECIFIER-TOKEN>, production: 86),
                    make(<reduce>, on: <TYPEDEF-TOKEN>, production: 86),
                    make(<reduce>, on: <UNION-TOKEN>, production: 86),
                    make(<reduce>, on: <VOLATILE-TOKEN>, production: 86));

*action-table*[169] :=
  // TYPE-SPECIFIER-LIST -> TYPE-SPECIFIER-LIST * TYPE-SPECIFIER
  // STRUCT-DECLARATION -> TYPE-SPECIFIER-LIST * STRUCT-DECLARATOR-LIST <SEMICOLON-TOKEN>
  make-action-table(make(<shift>, on: <CONST-TOKEN>, state: 8),
                    make(<shift>, on: <ENUM-TOKEN>, state: 15),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 170),
                    make(<shift>, on: <STAR-TOKEN>, state: 143),
                    make(<shift>, on: <STRUCT-TOKEN>, state: 10),
                    make(<shift>, on: <TYPE-NAME-TOKEN>, state: 33),
                    make(<shift>, on: <TYPE-SPECIFIER-TOKEN>, state: 7),
                    make(<shift>, on: <UNION-TOKEN>, state: 11),
                    make(<shift>, on: <VOLATILE-TOKEN>, state: 9));

*action-table*[170] :=
  // DECLARATOR2 -> <LPAREN-TOKEN> * DECLARATOR <RPAREN-TOKEN>
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 170),
                    make(<shift>, on: <STAR-TOKEN>, state: 143));

*action-table*[171] :=
  // DECLARATOR -> POINTER * DECLARATOR2
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 170));

*action-table*[172] :=
  // STRUCT-DECLARATOR -> DECLARATOR *
  // STRUCT-DECLARATOR -> DECLARATOR * <COLON-TOKEN> CONSTANT-EXPR
  make-action-table(make(<shift>, on: <COLON-TOKEN>, state: 173),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 84),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 84));

*action-table*[173] :=
  // STRUCT-DECLARATOR -> DECLARATOR <COLON-TOKEN> * CONSTANT-EXPR
  make-action-table(make(<shift>, on: <BANG-TOKEN>, state: 28),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 22),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 38),
                    make(<shift>, on: <MINUS-TOKEN>, state: 26),
                    make(<shift>, on: <SIZEOF-TOKEN>, state: 30),
                    make(<shift>, on: <TILDE-TOKEN>, state: 27));

*action-table*[174] :=
  // STRUCT-DECLARATOR -> DECLARATOR <COLON-TOKEN> CONSTANT-EXPR *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 85),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 85));

*action-table*[175] :=
  // STRUCT-DECLARATOR-LIST -> STRUCT-DECLARATOR *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 82),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 82));

*action-table*[176] :=
  // STRUCT-DECLARATOR-LIST -> STRUCT-DECLARATOR-LIST * <COMMA-TOKEN> STRUCT-DECLARATOR
  // STRUCT-DECLARATION -> TYPE-SPECIFIER-LIST STRUCT-DECLARATOR-LIST * <SEMICOLON-TOKEN>
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 178),
                    make(<shift>, on: <SEMICOLON-TOKEN>, state: 177));

*action-table*[177] :=
  // STRUCT-DECLARATION -> TYPE-SPECIFIER-LIST STRUCT-DECLARATOR-LIST <SEMICOLON-TOKEN> *
  make-action-table(make(<reduce>, on: <CONST-TOKEN>, production: 81),
                    make(<reduce>, on: <ENUM-TOKEN>, production: 81),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 81),
                    make(<reduce>, on: <STRUCT-TOKEN>, production: 81),
                    make(<reduce>, on: <TYPE-NAME-TOKEN>, production: 81),
                    make(<reduce>, on: <TYPE-SPECIFIER-TOKEN>, production: 81),
                    make(<reduce>, on: <UNION-TOKEN>, production: 81),
                    make(<reduce>, on: <VOLATILE-TOKEN>, production: 81));

*action-table*[178] :=
  // STRUCT-DECLARATOR-LIST -> STRUCT-DECLARATOR-LIST <COMMA-TOKEN> * STRUCT-DECLARATOR
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 170),
                    make(<shift>, on: <STAR-TOKEN>, state: 143));

*action-table*[179] :=
  // STRUCT-DECLARATOR-LIST -> STRUCT-DECLARATOR-LIST <COMMA-TOKEN> STRUCT-DECLARATOR *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 83),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 83));

*action-table*[180] :=
  // STRUCT-DECLARATION-LIST -> STRUCT-DECLARATION *
  make-action-table(make(<reduce>, on: <CONST-TOKEN>, production: 79),
                    make(<reduce>, on: <ENUM-TOKEN>, production: 79),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 79),
                    make(<reduce>, on: <STRUCT-TOKEN>, production: 79),
                    make(<reduce>, on: <TYPE-NAME-TOKEN>, production: 79),
                    make(<reduce>, on: <TYPE-SPECIFIER-TOKEN>, production: 79),
                    make(<reduce>, on: <UNION-TOKEN>, production: 79),
                    make(<reduce>, on: <VOLATILE-TOKEN>, production: 79));

*action-table*[181] :=
  // STRUCT-DECLARATION-LIST -> STRUCT-DECLARATION-LIST * STRUCT-DECLARATION
  // STRUCT-OR-UNION-SPECIFIER -> STRUCT-OR-UNION <LCURLY-TOKEN> STRUCT-DECLARATION-LIST * <RCURLY-TOKEN>
  make-action-table(make(<shift>, on: <CONST-TOKEN>, state: 8),
                    make(<shift>, on: <ENUM-TOKEN>, state: 15),
                    make(<shift>, on: <RCURLY-TOKEN>, state: 182),
                    make(<shift>, on: <STRUCT-TOKEN>, state: 10),
                    make(<shift>, on: <TYPE-NAME-TOKEN>, state: 33),
                    make(<shift>, on: <TYPE-SPECIFIER-TOKEN>, state: 7),
                    make(<shift>, on: <UNION-TOKEN>, state: 11),
                    make(<shift>, on: <VOLATILE-TOKEN>, state: 9));

*action-table*[182] :=
  // STRUCT-OR-UNION-SPECIFIER -> STRUCT-OR-UNION <LCURLY-TOKEN> STRUCT-DECLARATION-LIST <RCURLY-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 75),
                    make(<reduce>, on: <CONST-TOKEN>, production: 75),
                    make(<reduce>, on: <ENUM-TOKEN>, production: 75),
                    make(<reduce>, on: <EOF-TOKEN>, production: 75),
                    make(<reduce>, on: <EXTERN-TOKEN>, production: 75),
                    make(<reduce>, on: <IDENTIFIER-TOKEN>, production: 75),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 75),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 75),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 75),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 75),
                    make(<reduce>, on: <STAR-TOKEN>, production: 75),
                    make(<reduce>, on: <STRUCT-TOKEN>, production: 75),
                    make(<reduce>, on: <TYPE-NAME-TOKEN>, production: 75),
                    make(<reduce>, on: <TYPE-SPECIFIER-TOKEN>, production: 75),
                    make(<reduce>, on: <TYPEDEF-TOKEN>, production: 75),
                    make(<reduce>, on: <UNION-TOKEN>, production: 75),
                    make(<reduce>, on: <VOLATILE-TOKEN>, production: 75));

*action-table*[183] :=
  // STRUCT-DECLARATION-LIST -> STRUCT-DECLARATION-LIST STRUCT-DECLARATION *
  make-action-table(make(<reduce>, on: <CONST-TOKEN>, production: 80),
                    make(<reduce>, on: <ENUM-TOKEN>, production: 80),
                    make(<reduce>, on: <RCURLY-TOKEN>, production: 80),
                    make(<reduce>, on: <STRUCT-TOKEN>, production: 80),
                    make(<reduce>, on: <TYPE-NAME-TOKEN>, production: 80),
                    make(<reduce>, on: <TYPE-SPECIFIER-TOKEN>, production: 80),
                    make(<reduce>, on: <UNION-TOKEN>, production: 80),
                    make(<reduce>, on: <VOLATILE-TOKEN>, production: 80));

*action-table*[184] :=
  // STRUCT-OR-UNION-SPECIFIER -> STRUCT-OR-UNION <NAME-TOKEN> *
  // STRUCT-OR-UNION-SPECIFIER -> STRUCT-OR-UNION <NAME-TOKEN> * <LCURLY-TOKEN> STRUCT-DECLARATION-LIST <RCURLY-TOKEN>
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 76),
                    make(<reduce>, on: <CONST-TOKEN>, production: 76),
                    make(<reduce>, on: <ENUM-TOKEN>, production: 76),
                    make(<reduce>, on: <EOF-TOKEN>, production: 76),
                    make(<reduce>, on: <EXTERN-TOKEN>, production: 76),
                    make(<reduce>, on: <IDENTIFIER-TOKEN>, production: 76),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 76),
                    make(<shift>, on: <LCURLY-TOKEN>, state: 185),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 76),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 76),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 76),
                    make(<reduce>, on: <STAR-TOKEN>, production: 76),
                    make(<reduce>, on: <STRUCT-TOKEN>, production: 76),
                    make(<reduce>, on: <TYPE-NAME-TOKEN>, production: 76),
                    make(<reduce>, on: <TYPE-SPECIFIER-TOKEN>, production: 76),
                    make(<reduce>, on: <TYPEDEF-TOKEN>, production: 76),
                    make(<reduce>, on: <UNION-TOKEN>, production: 76),
                    make(<reduce>, on: <VOLATILE-TOKEN>, production: 76));

*action-table*[185] :=
  // STRUCT-OR-UNION-SPECIFIER -> STRUCT-OR-UNION <NAME-TOKEN> <LCURLY-TOKEN> * STRUCT-DECLARATION-LIST <RCURLY-TOKEN>
  make-action-table(make(<shift>, on: <CONST-TOKEN>, state: 8),
                    make(<shift>, on: <ENUM-TOKEN>, state: 15),
                    make(<shift>, on: <STRUCT-TOKEN>, state: 10),
                    make(<shift>, on: <TYPE-NAME-TOKEN>, state: 33),
                    make(<shift>, on: <TYPE-SPECIFIER-TOKEN>, state: 7),
                    make(<shift>, on: <UNION-TOKEN>, state: 11),
                    make(<shift>, on: <VOLATILE-TOKEN>, state: 9));

*action-table*[186] :=
  // STRUCT-DECLARATION-LIST -> STRUCT-DECLARATION-LIST * STRUCT-DECLARATION
  // STRUCT-OR-UNION-SPECIFIER -> STRUCT-OR-UNION <NAME-TOKEN> <LCURLY-TOKEN> STRUCT-DECLARATION-LIST * <RCURLY-TOKEN>
  make-action-table(make(<shift>, on: <CONST-TOKEN>, state: 8),
                    make(<shift>, on: <ENUM-TOKEN>, state: 15),
                    make(<shift>, on: <RCURLY-TOKEN>, state: 187),
                    make(<shift>, on: <STRUCT-TOKEN>, state: 10),
                    make(<shift>, on: <TYPE-NAME-TOKEN>, state: 33),
                    make(<shift>, on: <TYPE-SPECIFIER-TOKEN>, state: 7),
                    make(<shift>, on: <UNION-TOKEN>, state: 11),
                    make(<shift>, on: <VOLATILE-TOKEN>, state: 9));

*action-table*[187] :=
  // STRUCT-OR-UNION-SPECIFIER -> STRUCT-OR-UNION <NAME-TOKEN> <LCURLY-TOKEN> STRUCT-DECLARATION-LIST <RCURLY-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 74),
                    make(<reduce>, on: <CONST-TOKEN>, production: 74),
                    make(<reduce>, on: <ENUM-TOKEN>, production: 74),
                    make(<reduce>, on: <EOF-TOKEN>, production: 74),
                    make(<reduce>, on: <EXTERN-TOKEN>, production: 74),
                    make(<reduce>, on: <IDENTIFIER-TOKEN>, production: 74),
                    make(<reduce>, on: <LBRACKET-TOKEN>, production: 74),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 74),
                    make(<reduce>, on: <RPAREN-TOKEN>, production: 74),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 74),
                    make(<reduce>, on: <STAR-TOKEN>, production: 74),
                    make(<reduce>, on: <STRUCT-TOKEN>, production: 74),
                    make(<reduce>, on: <TYPE-NAME-TOKEN>, production: 74),
                    make(<reduce>, on: <TYPE-SPECIFIER-TOKEN>, production: 74),
                    make(<reduce>, on: <TYPEDEF-TOKEN>, production: 74),
                    make(<reduce>, on: <UNION-TOKEN>, production: 74),
                    make(<reduce>, on: <VOLATILE-TOKEN>, production: 74));

*action-table*[188] :=
  // DECLARATION-SPECIFIERS -> TYPE-SPECIFIER *
  // DECLARATION-SPECIFIERS -> TYPE-SPECIFIER * DECLARATION-SPECIFIERS
  make-action-table(make(<shift>, on: <CONST-TOKEN>, state: 8),
                    make(<shift>, on: <ENUM-TOKEN>, state: 15),
                    make(<shift>, on: <EXTERN-TOKEN>, state: 5),
                    make(<reduce>, on: <IDENTIFIER-TOKEN>, production: 61),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 61),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 61),
                    make(<reduce>, on: <STAR-TOKEN>, production: 61),
                    make(<shift>, on: <STRUCT-TOKEN>, state: 10),
                    make(<shift>, on: <TYPE-NAME-TOKEN>, state: 33),
                    make(<shift>, on: <TYPE-SPECIFIER-TOKEN>, state: 7),
                    make(<shift>, on: <TYPEDEF-TOKEN>, state: 4),
                    make(<shift>, on: <UNION-TOKEN>, state: 11),
                    make(<shift>, on: <VOLATILE-TOKEN>, state: 9));

*action-table*[189] :=
  // DECLARATION-SPECIFIERS -> TYPE-SPECIFIER DECLARATION-SPECIFIERS *
  make-action-table(make(<reduce>, on: <IDENTIFIER-TOKEN>, production: 62),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 62),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 62),
                    make(<reduce>, on: <STAR-TOKEN>, production: 62));

*action-table*[190] :=
  // DECLARATION-SPECIFIERS -> STORAGE-CLASS-SPECIFIER DECLARATION-SPECIFIERS *
  make-action-table(make(<reduce>, on: <IDENTIFIER-TOKEN>, production: 60),
                    make(<reduce>, on: <LPAREN-TOKEN>, production: 60),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 60),
                    make(<reduce>, on: <STAR-TOKEN>, production: 60));

*action-table*[191] :=
  // DECLARATION -> DECLARATION-SPECIFIERS * <SEMICOLON-TOKEN>
  // DECLARATION -> DECLARATION-SPECIFIERS * INIT-DECLARATOR-LIST <SEMICOLON-TOKEN>
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 170),
                    make(<shift>, on: <SEMICOLON-TOKEN>, state: 198),
                    make(<shift>, on: <STAR-TOKEN>, state: 143));

*action-table*[192] :=
  // INIT-DECLARATOR -> DECLARATOR *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 65),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 65));

*action-table*[193] :=
  // INIT-DECLARATOR-LIST -> INIT-DECLARATOR *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 63),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 63));

*action-table*[194] :=
  // INIT-DECLARATOR-LIST -> INIT-DECLARATOR-LIST * <COMMA-TOKEN> INIT-DECLARATOR
  // DECLARATION -> DECLARATION-SPECIFIERS INIT-DECLARATOR-LIST * <SEMICOLON-TOKEN>
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 196),
                    make(<shift>, on: <SEMICOLON-TOKEN>, state: 195));

*action-table*[195] :=
  // DECLARATION -> DECLARATION-SPECIFIERS INIT-DECLARATOR-LIST <SEMICOLON-TOKEN> *
  make-action-table(make(<reduce>, on: <BEGIN-INCLUDE-TOKEN>, production: 58),
                    make(<reduce>, on: <CONST-TOKEN>, production: 58),
                    make(<reduce>, on: <END-INCLUDE-TOKEN>, production: 58),
                    make(<reduce>, on: <ENUM-TOKEN>, production: 58),
                    make(<reduce>, on: <EOF-TOKEN>, production: 58),
                    make(<reduce>, on: <EXTERN-TOKEN>, production: 58),
                    make(<reduce>, on: <STRUCT-TOKEN>, production: 58),
                    make(<reduce>, on: <TYPE-NAME-TOKEN>, production: 58),
                    make(<reduce>, on: <TYPE-SPECIFIER-TOKEN>, production: 58),
                    make(<reduce>, on: <TYPEDEF-TOKEN>, production: 58),
                    make(<reduce>, on: <UNION-TOKEN>, production: 58),
                    make(<reduce>, on: <VOLATILE-TOKEN>, production: 58));

*action-table*[196] :=
  // INIT-DECLARATOR-LIST -> INIT-DECLARATOR-LIST <COMMA-TOKEN> * INIT-DECLARATOR
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 170),
                    make(<shift>, on: <STAR-TOKEN>, state: 143));

*action-table*[197] :=
  // INIT-DECLARATOR-LIST -> INIT-DECLARATOR-LIST <COMMA-TOKEN> INIT-DECLARATOR *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 64),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 64));

*action-table*[198] :=
  // DECLARATION -> DECLARATION-SPECIFIERS <SEMICOLON-TOKEN> *
  make-action-table(make(<reduce>, on: <BEGIN-INCLUDE-TOKEN>, production: 57),
                    make(<reduce>, on: <CONST-TOKEN>, production: 57),
                    make(<reduce>, on: <END-INCLUDE-TOKEN>, production: 57),
                    make(<reduce>, on: <ENUM-TOKEN>, production: 57),
                    make(<reduce>, on: <EOF-TOKEN>, production: 57),
                    make(<reduce>, on: <EXTERN-TOKEN>, production: 57),
                    make(<reduce>, on: <STRUCT-TOKEN>, production: 57),
                    make(<reduce>, on: <TYPE-NAME-TOKEN>, production: 57),
                    make(<reduce>, on: <TYPE-SPECIFIER-TOKEN>, production: 57),
                    make(<reduce>, on: <TYPEDEF-TOKEN>, production: 57),
                    make(<reduce>, on: <UNION-TOKEN>, production: 57),
                    make(<reduce>, on: <VOLATILE-TOKEN>, production: 57));

*action-table*[199] :=
  // EXTERNAL-DEFINITION -> DECLARATION *
  make-action-table(make(<reduce>, on: <BEGIN-INCLUDE-TOKEN>, production: 134),
                    make(<reduce>, on: <CONST-TOKEN>, production: 134),
                    make(<reduce>, on: <END-INCLUDE-TOKEN>, production: 134),
                    make(<reduce>, on: <ENUM-TOKEN>, production: 134),
                    make(<reduce>, on: <EOF-TOKEN>, production: 134),
                    make(<reduce>, on: <EXTERN-TOKEN>, production: 134),
                    make(<reduce>, on: <STRUCT-TOKEN>, production: 134),
                    make(<reduce>, on: <TYPE-NAME-TOKEN>, production: 134),
                    make(<reduce>, on: <TYPE-SPECIFIER-TOKEN>, production: 134),
                    make(<reduce>, on: <TYPEDEF-TOKEN>, production: 134),
                    make(<reduce>, on: <UNION-TOKEN>, production: 134),
                    make(<reduce>, on: <VOLATILE-TOKEN>, production: 134));

*action-table*[200] :=
  // FILE1 -> EXTERNAL-DEFINITION *
  make-action-table(make(<reduce>, on: <BEGIN-INCLUDE-TOKEN>, production: 8),
                    make(<reduce>, on: <CONST-TOKEN>, production: 8),
                    make(<reduce>, on: <END-INCLUDE-TOKEN>, production: 8),
                    make(<reduce>, on: <ENUM-TOKEN>, production: 8),
                    make(<reduce>, on: <EOF-TOKEN>, production: 8),
                    make(<reduce>, on: <EXTERN-TOKEN>, production: 8),
                    make(<reduce>, on: <STRUCT-TOKEN>, production: 8),
                    make(<reduce>, on: <TYPE-NAME-TOKEN>, production: 8),
                    make(<reduce>, on: <TYPE-SPECIFIER-TOKEN>, production: 8),
                    make(<reduce>, on: <TYPEDEF-TOKEN>, production: 8),
                    make(<reduce>, on: <UNION-TOKEN>, production: 8),
                    make(<reduce>, on: <VOLATILE-TOKEN>, production: 8));

*action-table*[201] :=
  // FILE1 -> FILE1 * EXTERNAL-DEFINITION
  // FILE -> FILE1 * <EOF-TOKEN>
  make-action-table(make(<shift>, on: <BEGIN-INCLUDE-TOKEN>, state: 2),
                    make(<shift>, on: <CONST-TOKEN>, state: 8),
                    make(<shift>, on: <END-INCLUDE-TOKEN>, state: 3),
                    make(<shift>, on: <ENUM-TOKEN>, state: 15),
                    make(<shift>, on: <EOF-TOKEN>, state: 202),
                    make(<shift>, on: <EXTERN-TOKEN>, state: 5),
                    make(<shift>, on: <STRUCT-TOKEN>, state: 10),
                    make(<shift>, on: <TYPE-NAME-TOKEN>, state: 33),
                    make(<shift>, on: <TYPE-SPECIFIER-TOKEN>, state: 7),
                    make(<shift>, on: <TYPEDEF-TOKEN>, state: 4),
                    make(<shift>, on: <UNION-TOKEN>, state: 11),
                    make(<shift>, on: <VOLATILE-TOKEN>, state: 9));

*action-table*[202] :=
  // FILE -> FILE1 <EOF-TOKEN> *
  make-action-table(make(<reduce>, on: <EOF-TOKEN>, production: 2));

*action-table*[203] :=
  // FILE1 -> FILE1 EXTERNAL-DEFINITION *
  make-action-table(make(<reduce>, on: <BEGIN-INCLUDE-TOKEN>, production: 9),
                    make(<reduce>, on: <CONST-TOKEN>, production: 9),
                    make(<reduce>, on: <END-INCLUDE-TOKEN>, production: 9),
                    make(<reduce>, on: <ENUM-TOKEN>, production: 9),
                    make(<reduce>, on: <EOF-TOKEN>, production: 9),
                    make(<reduce>, on: <EXTERN-TOKEN>, production: 9),
                    make(<reduce>, on: <STRUCT-TOKEN>, production: 9),
                    make(<reduce>, on: <TYPE-NAME-TOKEN>, production: 9),
                    make(<reduce>, on: <TYPE-SPECIFIER-TOKEN>, production: 9),
                    make(<reduce>, on: <TYPEDEF-TOKEN>, production: 9),
                    make(<reduce>, on: <UNION-TOKEN>, production: 9),
                    make(<reduce>, on: <VOLATILE-TOKEN>, production: 9));

*action-table*[204] :=
  // FILE -> <ALIEN-NAME-TOKEN> * TYPE-NAME <EOF-TOKEN>
  // FILE -> <ALIEN-NAME-TOKEN> * IDENTIFIER <EOF-TOKEN>
  make-action-table(make(<shift>, on: <CONST-TOKEN>, state: 8),
                    make(<shift>, on: <ENUM-TOKEN>, state: 15),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <STRUCT-TOKEN>, state: 10),
                    make(<shift>, on: <TYPE-NAME-TOKEN>, state: 33),
                    make(<shift>, on: <TYPE-SPECIFIER-TOKEN>, state: 7),
                    make(<shift>, on: <UNION-TOKEN>, state: 11),
                    make(<shift>, on: <VOLATILE-TOKEN>, state: 9));

*action-table*[205] :=
  // FILE -> <ALIEN-NAME-TOKEN> IDENTIFIER * <EOF-TOKEN>
  make-action-table(make(<shift>, on: <EOF-TOKEN>, state: 206));

*action-table*[206] :=
  // FILE -> <ALIEN-NAME-TOKEN> IDENTIFIER <EOF-TOKEN> *
  make-action-table(make(<reduce>, on: <EOF-TOKEN>, production: 4));

*action-table*[207] :=
  // FILE -> <ALIEN-NAME-TOKEN> TYPE-NAME * <EOF-TOKEN>
  make-action-table(make(<shift>, on: <EOF-TOKEN>, state: 208));

*action-table*[208] :=
  // FILE -> <ALIEN-NAME-TOKEN> TYPE-NAME <EOF-TOKEN> *
  make-action-table(make(<reduce>, on: <EOF-TOKEN>, production: 3));

*action-table*[209] :=
  // FILE -> <MACRO-PARSE-TOKEN> * TYPE-NAME <EOF-TOKEN>
  // FILE -> <MACRO-PARSE-TOKEN> * CONSTANT-EXPR <EOF-TOKEN>
  make-action-table(make(<shift>, on: <BANG-TOKEN>, state: 28),
                    make(<shift>, on: <CONST-TOKEN>, state: 8),
                    make(<shift>, on: <ENUM-TOKEN>, state: 15),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 22),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 38),
                    make(<shift>, on: <MINUS-TOKEN>, state: 26),
                    make(<shift>, on: <SIZEOF-TOKEN>, state: 30),
                    make(<shift>, on: <STRUCT-TOKEN>, state: 10),
                    make(<shift>, on: <TILDE-TOKEN>, state: 27),
                    make(<shift>, on: <TYPE-NAME-TOKEN>, state: 33),
                    make(<shift>, on: <TYPE-SPECIFIER-TOKEN>, state: 7),
                    make(<shift>, on: <UNION-TOKEN>, state: 11),
                    make(<shift>, on: <VOLATILE-TOKEN>, state: 9));

*action-table*[210] :=
  // FILE -> <MACRO-PARSE-TOKEN> CONSTANT-EXPR * <EOF-TOKEN>
  make-action-table(make(<shift>, on: <EOF-TOKEN>, state: 211));

*action-table*[211] :=
  // FILE -> <MACRO-PARSE-TOKEN> CONSTANT-EXPR <EOF-TOKEN> *
  make-action-table(make(<reduce>, on: <EOF-TOKEN>, production: 6));

*action-table*[212] :=
  // FILE -> <MACRO-PARSE-TOKEN> TYPE-NAME * <EOF-TOKEN>
  make-action-table(make(<shift>, on: <EOF-TOKEN>, state: 213));

*action-table*[213] :=
  // FILE -> <MACRO-PARSE-TOKEN> TYPE-NAME <EOF-TOKEN> *
  make-action-table(make(<reduce>, on: <EOF-TOKEN>, production: 5));

*action-table*[214] :=
  // FILE -> <CPP-PARSE-TOKEN> * CONSTANT-EXPR <EOF-TOKEN>
  make-action-table(make(<shift>, on: <BANG-TOKEN>, state: 28),
                    make(<shift>, on: <IDENTIFIER-TOKEN>, state: 18),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 22),
                    make(<shift>, on: <LPAREN-TOKEN>, state: 38),
                    make(<shift>, on: <MINUS-TOKEN>, state: 26),
                    make(<shift>, on: <SIZEOF-TOKEN>, state: 30),
                    make(<shift>, on: <TILDE-TOKEN>, state: 27));

*action-table*[215] :=
  // FILE -> <CPP-PARSE-TOKEN> CONSTANT-EXPR * <EOF-TOKEN>
  make-action-table(make(<shift>, on: <EOF-TOKEN>, state: 216));

*action-table*[216] :=
  // FILE -> <CPP-PARSE-TOKEN> CONSTANT-EXPR <EOF-TOKEN> *
  make-action-table(make(<reduce>, on: <EOF-TOKEN>, production: 7));

*action-table*[217] :=
  // S-PRIME -> FILE *
  make-action-table(make(<accept>, on: <EOF-TOKEN>));

*production-table*[1] :=
  method (state-stack, symbol-stack, #key $state)
    // FILE -> <EOF-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(217,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[2] :=
  method (state-stack, symbol-stack, #key $state)
    // FILE -> FILE1 <EOF-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(217,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[3] :=
  method (state-stack, symbol-stack, #key $state)
    // FILE -> <ALIEN-NAME-TOKEN> TYPE-NAME <EOF-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(217,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      $r2;
                  end,
                  temp1);
           end);
  end;

*production-table*[4] :=
  method (state-stack, symbol-stack, #key $state)
    // FILE -> <ALIEN-NAME-TOKEN> IDENTIFIER <EOF-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(217,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      element($state.objects, $r2.string-value, default: #f)
                        | parse-error($r2, "Unknown identifier: %s", $r2.string-value);
                  end,
                  temp1);
           end);
  end;

*production-table*[5] :=
  method (state-stack, symbol-stack, #key $state)
    // FILE -> <MACRO-PARSE-TOKEN> TYPE-NAME <EOF-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(217,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      $r2;
                  end,
                  temp1);
           end);
  end;

*production-table*[6] :=
  method (state-stack, symbol-stack, #key $state)
    // FILE -> <MACRO-PARSE-TOKEN> CONSTANT-EXPR <EOF-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(217,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      if (instance?($r2, <identifier-token>))
                        element($state.objects, $r2.string-value, default: #f)
                          | parse-error($r2, "Unknown identifier: %s", $r2.string-value);
                      else
                        $r2;
                      end if;
                  end,
                  temp1);
           end);
  end;

*production-table*[7] :=
  method (state-stack, symbol-stack, #key $state)
    // FILE -> <CPP-PARSE-TOKEN> CONSTANT-EXPR <EOF-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(217,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      int-value($r2, $state);
                  end,
                  temp1);
           end);
  end;

*production-table*[8] :=
  method (state-stack, symbol-stack, #key $state)
    // FILE1 -> EXTERNAL-DEFINITION
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(201,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[9] :=
  method (state-stack, symbol-stack, #key $state)
    // FILE1 -> FILE1 EXTERNAL-DEFINITION
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(201,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[10] :=
  method (state-stack, symbol-stack, #key $state)
    // PRIMARY-EXPR -> IDENTIFIER
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(23,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      // We allow arbitrary identifiers only because they are occasionally
                      // permitted by CPP (and treated as integers).  This will also allow a
                      // single identifier to pass as a constant expr.  Users of "constant-expr"
                      // should be aware of this and call "int-value" to insure that it is in fact
                      // a constant value.  "parse-macro" takes advantage of this property of
                      // "constant-expr" to allow defined identifiers to be parsed.
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[11] :=
  method (state-stack, symbol-stack, #key $state)
    // PRIMARY-EXPR -> <INTEGER-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(23,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1.value;
                  end,
                  temp1);
           end);
  end;

*production-table*[12] :=
  method (state-stack, symbol-stack, #key $state)
    // PRIMARY-EXPR -> <LPAREN-TOKEN> EXPR <RPAREN-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(23,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      $r2;
                  end,
                  temp1);
           end);
  end;

*production-table*[13] :=
  method (state-stack, symbol-stack, #key $state)
    // POSTFIX-EXPR -> PRIMARY-EXPR
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(24,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[14] :=
  method (state-stack, symbol-stack, #key $state)
    // POSTFIX-EXPR -> POSTFIX-EXPR <LPAREN-TOKEN> ARGUMENT-EXPR-LIST <RPAREN-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(state-stack))));
             pair(24,
                  poped-state-stack);
           end,
           begin
             let $r4 = head(symbol-stack);
             let temp4 = tail(symbol-stack);
             let $r3 = head(temp4);
             let temp3 = tail(temp4);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      // There aren't any compile-time functions which we are prepared to
                      // evaluate in general.  However, if we are evaluating an expression on a
                      // CPP line, we should be prepared to evaluate the pseudo-function
                      // "defined".
                      //
                      if (~instance?($state, <parse-cpp-state>) | $r1.string-value ~= "defined")
                        parse-error($state,"Function calls not allowed in constant expressions.");
                      elseif (element($r3.generator.cpp-table, $r3.string-value, default: #f))
                        1;
                      else
                        0;
                      end if;
                  end,
                  temp1);
           end);
  end;

*production-table*[15] :=
  method (state-stack, symbol-stack, #key $state)
    // ARGUMENT-EXPR-LIST -> ASSIGNMENT-EXPR
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(158,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[16] :=
  method (state-stack, symbol-stack, #key $state)
    // UNARY-EXPR -> POSTFIX-EXPR
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(37,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[17] :=
  method (state-stack, symbol-stack, #key $state)
    // UNARY-EXPR -> UNARY-OPERATOR CAST-EXPR
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(37,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      select ($r1 by instance?)
                        <minus-token> => -int-value($r2, $state);
                        <tilde-token> => lognot(int-value($r2, $state));
                        <bang-token> => if (int-value($r2, $state) == 0) 1 else 0 end if;
                      end select;
                  end,
                  temp1);
           end);
  end;

*production-table*[18] :=
  method (state-stack, symbol-stack, #key $state)
    // UNARY-EXPR -> <SIZEOF-TOKEN> <LPAREN-TOKEN> TYPE-NAME <RPAREN-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(state-stack))));
             pair(37,
                  poped-state-stack);
           end,
           begin
             let $r4 = head(symbol-stack);
             let temp4 = tail(symbol-stack);
             let $r3 = head(temp4);
             let temp3 = tail(temp4);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      c-type-size($r3);
                  end,
                  temp1);
           end);
  end;

*production-table*[19] :=
  method (state-stack, symbol-stack, #key $state)
    // UNARY-OPERATOR -> <MINUS-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(29,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[20] :=
  method (state-stack, symbol-stack, #key $state)
    // UNARY-OPERATOR -> <TILDE-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(29,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[21] :=
  method (state-stack, symbol-stack, #key $state)
    // UNARY-OPERATOR -> <BANG-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(29,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[22] :=
  method (state-stack, symbol-stack, #key $state)
    // CAST-EXPR -> UNARY-EXPR
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    29 => 156;
                    40 => 41;
                    44 => 45;
                    46 => 47;
                    48 => 49;
                    OTHERWISE => 42;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[23] :=
  method (state-stack, symbol-stack, #key $state)
    // CAST-EXPR -> <LPAREN-TOKEN> TYPE-NAME <RPAREN-TOKEN> CAST-EXPR
    values(begin
             let poped-state-stack = tail(tail(tail(tail(state-stack))));
             pair(select (head(poped-state-stack))
                    29 => 156;
                    40 => 41;
                    44 => 45;
                    46 => 47;
                    48 => 49;
                    OTHERWISE => 42;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r4 = head(symbol-stack);
             let temp4 = tail(symbol-stack);
             let $r3 = head(temp4);
             let temp3 = tail(temp4);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      let result = int-value($r4, $state);
                      if (instance?(result, <integer>)
                           & instance?($r2.true-type, <integer-type-declaration>))
                        result;
                      else
                        parse-error
                          ($state,
                           "Melange only handles compile time casts from integer to integer");
                      end if;
                  end,
                  temp1);
           end);
  end;

*production-table*[24] :=
  method (state-stack, symbol-stack, #key $state)
    // MULTIPLICATIVE-EXPR -> CAST-EXPR
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    51 => 52;
                    53 => 54;
                    OTHERWISE => 43;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[25] :=
  method (state-stack, symbol-stack, #key $state)
    // MULTIPLICATIVE-EXPR -> MULTIPLICATIVE-EXPR <STAR-TOKEN> CAST-EXPR
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    51 => 52;
                    53 => 54;
                    OTHERWISE => 43;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      int-value($r1, $state) * int-value($r3, $state);
                  end,
                  temp1);
           end);
  end;

*production-table*[26] :=
  method (state-stack, symbol-stack, #key $state)
    // MULTIPLICATIVE-EXPR -> MULTIPLICATIVE-EXPR <SLASH-TOKEN> CAST-EXPR
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    51 => 52;
                    53 => 54;
                    OTHERWISE => 43;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      truncate/(int-value($r1, $state), int-value($r3, $state));
                  end,
                  temp1);
           end);
  end;

*production-table*[27] :=
  method (state-stack, symbol-stack, #key $state)
    // MULTIPLICATIVE-EXPR -> MULTIPLICATIVE-EXPR <PERCENT-TOKEN> CAST-EXPR
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    51 => 52;
                    53 => 54;
                    OTHERWISE => 43;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      remainder(int-value($r1, $state), int-value($r3, $state));
                  end,
                  temp1);
           end);
  end;

*production-table*[28] :=
  method (state-stack, symbol-stack, #key $state)
    // ADDITIVE-EXPR -> MULTIPLICATIVE-EXPR
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    56 => 57;
                    58 => 59;
                    OTHERWISE => 50;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[29] :=
  method (state-stack, symbol-stack, #key $state)
    // ADDITIVE-EXPR -> ADDITIVE-EXPR <PLUS-TOKEN> MULTIPLICATIVE-EXPR
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    56 => 57;
                    58 => 59;
                    OTHERWISE => 50;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      int-value($r1, $state) + int-value($r3, $state);
                  end,
                  temp1);
           end);
  end;

*production-table*[30] :=
  method (state-stack, symbol-stack, #key $state)
    // ADDITIVE-EXPR -> ADDITIVE-EXPR <MINUS-TOKEN> MULTIPLICATIVE-EXPR
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    56 => 57;
                    58 => 59;
                    OTHERWISE => 50;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      int-value($r1, $state) - int-value($r3, $state);
                  end,
                  temp1);
           end);
  end;

*production-table*[31] :=
  method (state-stack, symbol-stack, #key $state)
    // SHIFT-EXPR -> ADDITIVE-EXPR
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    61 => 62;
                    63 => 64;
                    65 => 66;
                    67 => 68;
                    OTHERWISE => 55;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[32] :=
  method (state-stack, symbol-stack, #key $state)
    // SHIFT-EXPR -> SHIFT-EXPR <LEFT-OP-TOKEN> ADDITIVE-EXPR
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    61 => 62;
                    63 => 64;
                    65 => 66;
                    67 => 68;
                    OTHERWISE => 55;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      ash(int-value($r1, $state), int-value($r3, $state))
                  end,
                  temp1);
           end);
  end;

*production-table*[33] :=
  method (state-stack, symbol-stack, #key $state)
    // SHIFT-EXPR -> SHIFT-EXPR <RIGHT-OP-TOKEN> ADDITIVE-EXPR
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    61 => 62;
                    63 => 64;
                    65 => 66;
                    67 => 68;
                    OTHERWISE => 55;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      ash(int-value($r1, $state), -int-value($r3, $state))
                  end,
                  temp1);
           end);
  end;

*production-table*[34] :=
  method (state-stack, symbol-stack, #key $state)
    // RELATIONAL-EXPR -> SHIFT-EXPR
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    70 => 71;
                    72 => 73;
                    OTHERWISE => 60;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[35] :=
  method (state-stack, symbol-stack, #key $state)
    // RELATIONAL-EXPR -> RELATIONAL-EXPR <LT-TOKEN> SHIFT-EXPR
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    70 => 71;
                    72 => 73;
                    OTHERWISE => 60;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      if (int-value($r1, $state) < int-value($r3, $state)) 1 else 0 end if;
                  end,
                  temp1);
           end);
  end;

*production-table*[36] :=
  method (state-stack, symbol-stack, #key $state)
    // RELATIONAL-EXPR -> RELATIONAL-EXPR <GT-TOKEN> SHIFT-EXPR
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    70 => 71;
                    72 => 73;
                    OTHERWISE => 60;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      if (int-value($r1, $state) > int-value($r3, $state)) 1 else 0 end if;
                  end,
                  temp1);
           end);
  end;

*production-table*[37] :=
  method (state-stack, symbol-stack, #key $state)
    // RELATIONAL-EXPR -> RELATIONAL-EXPR <LE-OP-TOKEN> SHIFT-EXPR
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    70 => 71;
                    72 => 73;
                    OTHERWISE => 60;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      if (int-value($r1, $state) <= int-value($r3, $state)) 1 else 0 end if;
                  end,
                  temp1);
           end);
  end;

*production-table*[38] :=
  method (state-stack, symbol-stack, #key $state)
    // RELATIONAL-EXPR -> RELATIONAL-EXPR <GE-OP-TOKEN> SHIFT-EXPR
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    70 => 71;
                    72 => 73;
                    OTHERWISE => 60;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      if (int-value($r1, $state) >= int-value($r3, $state)) 1 else 0 end if;
                  end,
                  temp1);
           end);
  end;

*production-table*[39] :=
  method (state-stack, symbol-stack, #key $state)
    // EQUALITY-EXPR -> RELATIONAL-EXPR
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    75 => 76;
                    OTHERWISE => 69;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[40] :=
  method (state-stack, symbol-stack, #key $state)
    // EQUALITY-EXPR -> EQUALITY-EXPR <EQ-OP-TOKEN> RELATIONAL-EXPR
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    75 => 76;
                    OTHERWISE => 69;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      if (int-value($r1, $state) == int-value($r3, $state)) 1 else 0 end if;
                  end,
                  temp1);
           end);
  end;

*production-table*[41] :=
  method (state-stack, symbol-stack, #key $state)
    // EQUALITY-EXPR -> EQUALITY-EXPR <NE-OP-TOKEN> RELATIONAL-EXPR
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    75 => 76;
                    OTHERWISE => 69;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      if (int-value($r1, $state) ~= int-value($r3, $state)) 1 else 0 end if;
                  end,
                  temp1);
           end);
  end;

*production-table*[42] :=
  method (state-stack, symbol-stack, #key $state)
    // AND-EXPR -> EQUALITY-EXPR
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    78 => 79;
                    OTHERWISE => 74;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[43] :=
  method (state-stack, symbol-stack, #key $state)
    // AND-EXPR -> AND-EXPR <AMPERSAND-TOKEN> EQUALITY-EXPR
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    78 => 79;
                    OTHERWISE => 74;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      logand(int-value($r1, $state), int-value($r3, $state))
                  end,
                  temp1);
           end);
  end;

*production-table*[44] :=
  method (state-stack, symbol-stack, #key $state)
    // EXCLUSIVE-OR-EXPR -> AND-EXPR
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    81 => 82;
                    OTHERWISE => 77;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[45] :=
  method (state-stack, symbol-stack, #key $state)
    // EXCLUSIVE-OR-EXPR -> EXCLUSIVE-OR-EXPR <CARAT-TOKEN> AND-EXPR
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    81 => 82;
                    OTHERWISE => 77;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      logxor(int-value($r1, $state), int-value($r3, $state));
                  end,
                  temp1);
           end);
  end;

*production-table*[46] :=
  method (state-stack, symbol-stack, #key $state)
    // INCLUSIVE-OR-EXPR -> EXCLUSIVE-OR-EXPR
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    84 => 85;
                    OTHERWISE => 80;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[47] :=
  method (state-stack, symbol-stack, #key $state)
    // INCLUSIVE-OR-EXPR -> INCLUSIVE-OR-EXPR <BAR-TOKEN> EXCLUSIVE-OR-EXPR
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    84 => 85;
                    OTHERWISE => 80;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      logior(int-value($r1, $state), int-value($r3, $state));
                  end,
                  temp1);
           end);
  end;

*production-table*[48] :=
  method (state-stack, symbol-stack, #key $state)
    // LOGICAL-AND-EXPR -> INCLUSIVE-OR-EXPR
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    91 => 92;
                    OTHERWISE => 83;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[49] :=
  method (state-stack, symbol-stack, #key $state)
    // LOGICAL-AND-EXPR -> LOGICAL-AND-EXPR <AND-OP-TOKEN> INCLUSIVE-OR-EXPR
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    91 => 92;
                    OTHERWISE => 83;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      if (int-value($r1, $state) == 0) $r1 else $r3 end if;
                  end,
                  temp1);
           end);
  end;

*production-table*[50] :=
  method (state-stack, symbol-stack, #key $state)
    // LOGICAL-OR-EXPR -> LOGICAL-AND-EXPR
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    87 => 88;
                    OTHERWISE => 86;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[51] :=
  method (state-stack, symbol-stack, #key $state)
    // LOGICAL-OR-EXPR -> LOGICAL-OR-EXPR <OR-OP-TOKEN> LOGICAL-AND-EXPR
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    87 => 88;
                    OTHERWISE => 86;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      if (int-value($r1, $state) == 0) $r3 else $r1 end if;
                  end,
                  temp1);
           end);
  end;

*production-table*[52] :=
  method (state-stack, symbol-stack, #key $state)
    // CONDITIONAL-EXPR -> LOGICAL-OR-EXPR
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    25 => 93;
                    38 => 93;
                    89 => 90;
                    OTHERWISE => 97;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[53] :=
  method (state-stack, symbol-stack, #key $state)
    // CONDITIONAL-EXPR -> LOGICAL-OR-EXPR <QUESTION-TOKEN> LOGICAL-OR-EXPR <COLON-TOKEN> CONDITIONAL-EXPR
    values(begin
             let poped-state-stack = tail(tail(tail(tail(tail(state-stack)))));
             pair(select (head(poped-state-stack))
                    25 => 93;
                    38 => 93;
                    89 => 90;
                    OTHERWISE => 97;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r5 = head(symbol-stack);
             let temp5 = tail(symbol-stack);
             let $r4 = head(temp5);
             let temp4 = tail(temp5);
             let $r3 = head(temp4);
             let temp3 = tail(temp4);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      if (int-value($r1, $state) == 0) $r5 else $r3 end if;
                  end,
                  temp1);
           end);
  end;

*production-table*[54] :=
  method (state-stack, symbol-stack, #key $state)
    // ASSIGNMENT-EXPR -> CONDITIONAL-EXPR
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    25 => 157;
                    OTHERWISE => 94;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[55] :=
  method (state-stack, symbol-stack, #key $state)
    // EXPR -> ASSIGNMENT-EXPR
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(95,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[56] :=
  method (state-stack, symbol-stack, #key $state)
    // CONSTANT-EXPR -> CONDITIONAL-EXPR
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    20 => 160;
                    36 => 98;
                    114 => 116;
                    134 => 136;
                    173 => 174;
                    209 => 210;
                    OTHERWISE => 215;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      // In general, constant expr will return an integer value.  However, for
                      // obscure reasons, we also allow it to return a single identifier.
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[57] :=
  method (state-stack, symbol-stack, #key $state)
    // DECLARATION -> DECLARATION-SPECIFIERS <SEMICOLON-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(199,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      *typedef-flag* := #f;
                      process-type-list($r1, $state);
                  end,
                  temp1);
           end);
  end;

*production-table*[58] :=
  method (state-stack, symbol-stack, #key $state)
    // DECLARATION -> DECLARATION-SPECIFIERS INIT-DECLARATOR-LIST <SEMICOLON-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(199,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      let type = process-type-list($r1, $state);
                      let names = reverse!($r2);
                      declare-objects($state, type, names, *typedef-flag*);
                      *typedef-flag* := #f;
                  end,
                  temp1);
           end);
  end;

*production-table*[59] :=
  method (state-stack, symbol-stack, #key $state)
    // DECLARATION-SPECIFIERS -> STORAGE-CLASS-SPECIFIER
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    6 => 190;
                    188 => 189;
                    OTHERWISE => 191;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      // Storage class must be extern; unspecified type must be "int"
                      list(make(<int-token>, generator: $r1.generator, string: "int"));
                  end,
                  temp1);
           end);
  end;

*production-table*[60] :=
  method (state-stack, symbol-stack, #key $state)
    // DECLARATION-SPECIFIERS -> STORAGE-CLASS-SPECIFIER DECLARATION-SPECIFIERS
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(select (head(poped-state-stack))
                    6 => 190;
                    188 => 189;
                    OTHERWISE => 191;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      // Storage class must be extern -- no need to note it
                      $r2;
                  end,
                  temp1);
           end);
  end;

*production-table*[61] :=
  method (state-stack, symbol-stack, #key $state)
    // DECLARATION-SPECIFIERS -> TYPE-SPECIFIER
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    6 => 190;
                    188 => 189;
                    OTHERWISE => 191;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      list($r1);
                  end,
                  temp1);
           end);
  end;

*production-table*[62] :=
  method (state-stack, symbol-stack, #key $state)
    // DECLARATION-SPECIFIERS -> TYPE-SPECIFIER DECLARATION-SPECIFIERS
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(select (head(poped-state-stack))
                    6 => 190;
                    188 => 189;
                    OTHERWISE => 191;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair($r1, $r2);
                  end,
                  temp1);
           end);
  end;

*production-table*[63] :=
  method (state-stack, symbol-stack, #key $state)
    // INIT-DECLARATOR-LIST -> INIT-DECLARATOR
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(194,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      list($r1);
                  end,
                  temp1);
           end);
  end;

*production-table*[64] :=
  method (state-stack, symbol-stack, #key $state)
    // INIT-DECLARATOR-LIST -> INIT-DECLARATOR-LIST <COMMA-TOKEN> INIT-DECLARATOR
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(194,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair($r3, $r1);
                  end,
                  temp1);
           end);
  end;

*production-table*[65] :=
  method (state-stack, symbol-stack, #key $state)
    // INIT-DECLARATOR -> DECLARATOR
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    191 => 193;
                    OTHERWISE => 197;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[66] :=
  method (state-stack, symbol-stack, #key $state)
    // STORAGE-CLASS-SPECIFIER -> <TYPEDEF-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(6,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      *typedef-flag* := #t;
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[67] :=
  method (state-stack, symbol-stack, #key $state)
    // STORAGE-CLASS-SPECIFIER -> <EXTERN-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(6,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[68] :=
  method (state-stack, symbol-stack, #key $state)
    // TYPE-SPECIFIER -> <TYPE-SPECIFIER-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    0 => 188;
                    6 => 188;
                    35 => 119;
                    102 => 119;
                    145 => 119;
                    169 => 119;
                    188 => 188;
                    201 => 188;
                    OTHERWISE => 34;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[69] :=
  method (state-stack, symbol-stack, #key $state)
    // TYPE-SPECIFIER -> <CONST-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    0 => 188;
                    6 => 188;
                    35 => 119;
                    102 => 119;
                    145 => 119;
                    169 => 119;
                    188 => 188;
                    201 => 188;
                    OTHERWISE => 34;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[70] :=
  method (state-stack, symbol-stack, #key $state)
    // TYPE-SPECIFIER -> <VOLATILE-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    0 => 188;
                    6 => 188;
                    35 => 119;
                    102 => 119;
                    145 => 119;
                    169 => 119;
                    188 => 188;
                    201 => 188;
                    OTHERWISE => 34;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[71] :=
  method (state-stack, symbol-stack, #key $state)
    // TYPE-SPECIFIER -> STRUCT-OR-UNION-SPECIFIER
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    0 => 188;
                    6 => 188;
                    35 => 119;
                    102 => 119;
                    145 => 119;
                    169 => 119;
                    188 => 188;
                    201 => 188;
                    OTHERWISE => 34;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[72] :=
  method (state-stack, symbol-stack, #key $state)
    // TYPE-SPECIFIER -> ENUM-SPECIFIER
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    0 => 188;
                    6 => 188;
                    35 => 119;
                    102 => 119;
                    145 => 119;
                    169 => 119;
                    188 => 188;
                    201 => 188;
                    OTHERWISE => 34;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[73] :=
  method (state-stack, symbol-stack, #key $state)
    // TYPE-SPECIFIER -> <TYPE-NAME-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    0 => 188;
                    6 => 188;
                    35 => 119;
                    102 => 119;
                    145 => 119;
                    169 => 119;
                    188 => 188;
                    201 => 188;
                    OTHERWISE => 34;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $state.objects[$r1.value];
                  end,
                  temp1);
           end);
  end;

*production-table*[74] :=
  method (state-stack, symbol-stack, #key $state)
    // STRUCT-OR-UNION-SPECIFIER -> STRUCT-OR-UNION <NAME-TOKEN> <LCURLY-TOKEN> STRUCT-DECLARATION-LIST <RCURLY-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(tail(state-stack)))));
             pair(14,
                  poped-state-stack);
           end,
           begin
             let $r5 = head(symbol-stack);
             let temp5 = tail(symbol-stack);
             let $r4 = head(temp5);
             let temp4 = tail(temp5);
             let $r3 = head(temp4);
             let temp3 = tail(temp4);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      make-struct-type($r2.value, $r4, $r1, $state);
                  end,
                  temp1);
           end);
  end;

*production-table*[75] :=
  method (state-stack, symbol-stack, #key $state)
    // STRUCT-OR-UNION-SPECIFIER -> STRUCT-OR-UNION <LCURLY-TOKEN> STRUCT-DECLARATION-LIST <RCURLY-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(state-stack))));
             pair(14,
                  poped-state-stack);
           end,
           begin
             let $r4 = head(symbol-stack);
             let temp4 = tail(symbol-stack);
             let $r3 = head(temp4);
             let temp3 = tail(temp4);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      make-struct-type(#f, $r3, $r1, $state);
                  end,
                  temp1);
           end);
  end;

*production-table*[76] :=
  method (state-stack, symbol-stack, #key $state)
    // STRUCT-OR-UNION-SPECIFIER -> STRUCT-OR-UNION <NAME-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(14,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      make-struct-type($r2.value, #f, $r1, $state);
                  end,
                  temp1);
           end);
  end;

*production-table*[77] :=
  method (state-stack, symbol-stack, #key $state)
    // STRUCT-OR-UNION -> <STRUCT-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(12,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[78] :=
  method (state-stack, symbol-stack, #key $state)
    // STRUCT-OR-UNION -> <UNION-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(12,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[79] :=
  method (state-stack, symbol-stack, #key $state)
    // STRUCT-DECLARATION-LIST -> STRUCT-DECLARATION
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    13 => 181;
                    OTHERWISE => 186;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[80] :=
  method (state-stack, symbol-stack, #key $state)
    // STRUCT-DECLARATION-LIST -> STRUCT-DECLARATION-LIST STRUCT-DECLARATION
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(select (head(poped-state-stack))
                    13 => 181;
                    OTHERWISE => 186;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      // Creates list in normal order.
                      concatenate($r1, $r2);
                  end,
                  temp1);
           end);
  end;

*production-table*[81] :=
  method (state-stack, symbol-stack, #key $state)
    // STRUCT-DECLARATION -> TYPE-SPECIFIER-LIST STRUCT-DECLARATOR-LIST <SEMICOLON-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    13 => 180;
                    185 => 180;
                    OTHERWISE => 183;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      let type = process-type-list(reverse!($r1), $state);
                      let return = #();
                      for (declarator in $r2)
                        let (type, name) = process-declarator(type, declarator, $state);
                        return := pair(pair(name.value, type), return);
                      end for;
                      return;
                  end,
                  temp1);
           end);
  end;

*production-table*[82] :=
  method (state-stack, symbol-stack, #key $state)
    // STRUCT-DECLARATOR-LIST -> STRUCT-DECLARATOR
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(176,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      list($r1);
                  end,
                  temp1);
           end);
  end;

*production-table*[83] :=
  method (state-stack, symbol-stack, #key $state)
    // STRUCT-DECLARATOR-LIST -> STRUCT-DECLARATOR-LIST <COMMA-TOKEN> STRUCT-DECLARATOR
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(176,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      // Creates list in reverse order.
                      pair($r3, $r1);
                  end,
                  temp1);
           end);
  end;

*production-table*[84] :=
  method (state-stack, symbol-stack, #key $state)
    // STRUCT-DECLARATOR -> DECLARATOR
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    169 => 175;
                    OTHERWISE => 179;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[85] :=
  method (state-stack, symbol-stack, #key $state)
    // STRUCT-DECLARATOR -> DECLARATOR <COLON-TOKEN> CONSTANT-EXPR
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    169 => 175;
                    OTHERWISE => 179;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(#"bitfield", pair(int-value($r3, $state), $r1));
                  end,
                  temp1);
           end);
  end;

*production-table*[86] :=
  method (state-stack, symbol-stack, #key $state)
    // ENUM-SPECIFIER -> <ENUM-TOKEN> <LCURLY-TOKEN> ENUMERATOR-LIST <RCURLY-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(state-stack))));
             pair(32,
                  poped-state-stack);
           end,
           begin
             let $r4 = head(symbol-stack);
             let temp4 = tail(symbol-stack);
             let $r3 = head(temp4);
             let temp3 = tail(temp4);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      make-struct-type(#f, reverse!($r3), $r1, $state);
                  end,
                  temp1);
           end);
  end;

*production-table*[87] :=
  method (state-stack, symbol-stack, #key $state)
    // ENUM-SPECIFIER -> <ENUM-TOKEN> <NAME-TOKEN> <LCURLY-TOKEN> ENUMERATOR-LIST <RCURLY-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(tail(state-stack)))));
             pair(32,
                  poped-state-stack);
           end,
           begin
             let $r5 = head(symbol-stack);
             let temp5 = tail(symbol-stack);
             let $r4 = head(temp5);
             let temp4 = tail(temp5);
             let $r3 = head(temp4);
             let temp3 = tail(temp4);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      make-struct-type($r2.value, reverse!($r4), $r1, $state);
                  end,
                  temp1);
           end);
  end;

*production-table*[88] :=
  method (state-stack, symbol-stack, #key $state)
    // ENUM-SPECIFIER -> <ENUM-TOKEN> <NAME-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(32,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      make-struct-type($r2.value, #f, $r1, $state);
                  end,
                  temp1);
           end);
  end;

*production-table*[89] :=
  method (state-stack, symbol-stack, #key $state)
    // ENUMERATOR-LIST -> ENUMERATOR
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    17 => 162;
                    OTHERWISE => 167;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      list(make-enum-slot($r1.head, $r1.tail, #f, $state));
                  end,
                  temp1);
           end);
  end;

*production-table*[90] :=
  method (state-stack, symbol-stack, #key $state)
    // ENUMERATOR-LIST -> ENUMERATOR-LIST <COMMA-TOKEN> ENUMERATOR
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    17 => 162;
                    OTHERWISE => 167;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      // HACK: We depend here on the fact that this parser generator evaluates
                      // the subtrees left-to-right.
                      //
                      // We do all of this on the fly because some (i.e. Apple's) C compilers
                      // let later enum values be computed based upon those earlier in the 
                      // same enum declaration.
                      pair(make-enum-slot($r3.head, $r3.tail, $r1.head, $state), $r1);
                  end,
                  temp1);
           end);
  end;

*production-table*[91] :=
  method (state-stack, symbol-stack, #key $state)
    // ENUMERATOR -> IDENTIFIER
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    164 => 165;
                    OTHERWISE => 161;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      pair($r1.value, #f);
                  end,
                  temp1);
           end);
  end;

*production-table*[92] :=
  method (state-stack, symbol-stack, #key $state)
    // ENUMERATOR -> IDENTIFIER <ASSIGN-TOKEN> CONSTANT-EXPR
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    164 => 165;
                    OTHERWISE => 161;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair($r1.value, int-value($r3, $state));
                  end,
                  temp1);
           end);
  end;

*production-table*[93] :=
  method (state-stack, symbol-stack, #key $state)
    // DECLARATOR -> DECLARATOR2
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    102 => 152;
                    121 => 138;
                    169 => 172;
                    170 => 138;
                    178 => 172;
                    OTHERWISE => 192;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[94] :=
  method (state-stack, symbol-stack, #key $state)
    // DECLARATOR -> POINTER DECLARATOR2
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(select (head(poped-state-stack))
                    102 => 152;
                    121 => 138;
                    169 => 172;
                    170 => 138;
                    178 => 172;
                    OTHERWISE => 192;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair($r1, $r2);
                  end,
                  temp1);
           end);
  end;

*production-table*[95] :=
  method (state-stack, symbol-stack, #key $state)
    // DECLARATOR2 -> IDENTIFIER
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    147 => 149;
                    171 => 149;
                    OTHERWISE => 122;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[96] :=
  method (state-stack, symbol-stack, #key $state)
    // DECLARATOR2 -> <LPAREN-TOKEN> DECLARATOR <RPAREN-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    147 => 149;
                    171 => 149;
                    OTHERWISE => 122;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      $r2
                  end,
                  temp1);
           end);
  end;

*production-table*[97] :=
  method (state-stack, symbol-stack, #key $state)
    // DECLARATOR2 -> DECLARATOR2 <LBRACKET-TOKEN> <RBRACKET-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    147 => 149;
                    171 => 149;
                    OTHERWISE => 122;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(#"vector", pair(#f, $r1));
                  end,
                  temp1);
           end);
  end;

*production-table*[98] :=
  method (state-stack, symbol-stack, #key $state)
    // DECLARATOR2 -> DECLARATOR2 <LBRACKET-TOKEN> CONSTANT-EXPR <RBRACKET-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(state-stack))));
             pair(select (head(poped-state-stack))
                    147 => 149;
                    171 => 149;
                    OTHERWISE => 122;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r4 = head(symbol-stack);
             let temp4 = tail(symbol-stack);
             let $r3 = head(temp4);
             let temp3 = tail(temp4);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(#"vector", pair(int-value($r3, $state), $r1));
                  end,
                  temp1);
           end);
  end;

*production-table*[99] :=
  method (state-stack, symbol-stack, #key $state)
    // DECLARATOR2 -> DECLARATOR2 <LPAREN-TOKEN> <RPAREN-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    147 => 149;
                    171 => 149;
                    OTHERWISE => 122;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(#"function", pair(list(make(<varargs-declaration>,
                                                       name: "", type: unknown-type)), $r1));
                  end,
                  temp1);
           end);
  end;

*production-table*[100] :=
  method (state-stack, symbol-stack, #key $state)
    // DECLARATOR2 -> DECLARATOR2 <LPAREN-TOKEN> PARAMETER-TYPE-LIST <RPAREN-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(state-stack))));
             pair(select (head(poped-state-stack))
                    147 => 149;
                    171 => 149;
                    OTHERWISE => 122;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r4 = head(symbol-stack);
             let temp4 = tail(symbol-stack);
             let $r3 = head(temp4);
             let temp3 = tail(temp4);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(#"function", pair(reverse!($r3), $r1));
                  end,
                  temp1);
           end);
  end;

*production-table*[101] :=
  method (state-stack, symbol-stack, #key $state)
    // DECLARATOR2 -> DECLARATOR2 <LPAREN-TOKEN> PARAMETER-IDENTIFIER-LIST <RPAREN-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(state-stack))));
             pair(select (head(poped-state-stack))
                    147 => 149;
                    171 => 149;
                    OTHERWISE => 122;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r4 = head(symbol-stack);
             let temp4 = tail(symbol-stack);
             let $r3 = head(temp4);
             let temp3 = tail(temp4);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(#"function", pair(reverse!($r3), $r1));
                  end,
                  temp1);
           end);
  end;

*production-table*[102] :=
  method (state-stack, symbol-stack, #key $state)
    // POINTER -> <STAR-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    35 => 153;
                    101 => 153;
                    102 => 147;
                    121 => 147;
                    143 => 144;
                    145 => 146;
                    OTHERWISE => 171;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      list(#"pointer");
                  end,
                  temp1);
           end);
  end;

*production-table*[103] :=
  method (state-stack, symbol-stack, #key $state)
    // POINTER -> <STAR-TOKEN> TYPE-SPECIFIER-LIST
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(select (head(poped-state-stack))
                    35 => 153;
                    101 => 153;
                    102 => 147;
                    121 => 147;
                    143 => 144;
                    145 => 146;
                    OTHERWISE => 171;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      // They can put "const" in some screwy places.  Ignore it.
                      list(#"pointer");
                  end,
                  temp1);
           end);
  end;

*production-table*[104] :=
  method (state-stack, symbol-stack, #key $state)
    // POINTER -> <STAR-TOKEN> POINTER
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(select (head(poped-state-stack))
                    35 => 153;
                    101 => 153;
                    102 => 147;
                    121 => 147;
                    143 => 144;
                    145 => 146;
                    OTHERWISE => 171;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(#"pointer", $r2);
                  end,
                  temp1);
           end);
  end;

*production-table*[105] :=
  method (state-stack, symbol-stack, #key $state)
    // POINTER -> <STAR-TOKEN> TYPE-SPECIFIER-LIST POINTER
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    35 => 153;
                    101 => 153;
                    102 => 147;
                    121 => 147;
                    143 => 144;
                    145 => 146;
                    OTHERWISE => 171;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      // They can put "const" in some screwy places.  Ignore it.
                      pair(#"pointer", $r2);
                  end,
                  temp1);
           end);
  end;

*production-table*[106] :=
  method (state-stack, symbol-stack, #key $state)
    // TYPE-SPECIFIER-LIST -> TYPE-SPECIFIER
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(#[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 169, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 35, 0, 0, 0, 0, 0, 0, 35, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 102, 0, 0, 102, 0, 0, 0, 0, 102, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 102, 0, 102, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 145, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 169, 0, 0, 0, 169, 169, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 35, 0, 0, 0, 0, 35][head(poped-state-stack)],
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      list($r1);
                  end,
                  temp1);
           end);
  end;

*production-table*[107] :=
  method (state-stack, symbol-stack, #key $state)
    // TYPE-SPECIFIER-LIST -> TYPE-SPECIFIER-LIST TYPE-SPECIFIER
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(#[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 169, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 35, 0, 0, 0, 0, 0, 0, 35, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 102, 0, 0, 102, 0, 0, 0, 0, 102, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 102, 0, 102, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 145, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 169, 0, 0, 0, 169, 169, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 35, 0, 0, 0, 0, 35][head(poped-state-stack)],
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      // Produces a list in reverse order
                      pair($r2, $r1);
                  end,
                  temp1);
           end);
  end;

*production-table*[108] :=
  method (state-stack, symbol-stack, #key $state)
    // PARAMETER-IDENTIFIER-LIST -> IDENTIFIER-LIST
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(132,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[109] :=
  method (state-stack, symbol-stack, #key $state)
    // PARAMETER-IDENTIFIER-LIST -> IDENTIFIER-LIST <COMMA-TOKEN> <ELIPSIS-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(132,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(make(<varargs-declaration>, name: "", type: unknown-type), $r1);
                  end,
                  temp1);
           end);
  end;

*production-table*[110] :=
  method (state-stack, symbol-stack, #key $state)
    // IDENTIFIER-LIST -> IDENTIFIER
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(128,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      list(make(<arg-declaration>, name: $r1.string-value, type: unknown-type));
                  end,
                  temp1);
           end);
  end;

*production-table*[111] :=
  method (state-stack, symbol-stack, #key $state)
    // IDENTIFIER-LIST -> IDENTIFIER-LIST <COMMA-TOKEN> IDENTIFIER
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(128,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      // Produces list in reverse order.
                      pair(make(<arg-declaration>, name: $r3.string-value, type: unknown-type),
                           $r1);
                  end,
                  temp1);
           end);
  end;

*production-table*[112] :=
  method (state-stack, symbol-stack, #key $state)
    // PARAMETER-TYPE-LIST -> PARAMETER-LIST
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    104 => 112;
                    123 => 125;
                    OTHERWISE => 140;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[113] :=
  method (state-stack, symbol-stack, #key $state)
    // PARAMETER-TYPE-LIST -> PARAMETER-LIST <COMMA-TOKEN> <ELIPSIS-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    104 => 112;
                    123 => 125;
                    OTHERWISE => 140;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(make(<varargs-declaration>, name: "", type: unknown-type), $r1);
                  end,
                  temp1);
           end);
  end;

*production-table*[114] :=
  method (state-stack, symbol-stack, #key $state)
    // PARAMETER-LIST -> PARAMETER-DECLARATION
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(108,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      list($r1);
                  end,
                  temp1);
           end);
  end;

*production-table*[115] :=
  method (state-stack, symbol-stack, #key $state)
    // PARAMETER-LIST -> PARAMETER-LIST <COMMA-TOKEN> PARAMETER-DECLARATION
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(108,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      // Produces lists in reverse order.
                      pair($r3, $r1);
                  end,
                  temp1);
           end);
  end;

*production-table*[116] :=
  method (state-stack, symbol-stack, #key $state)
    // PARAMETER-DECLARATION -> TYPE-SPECIFIER-LIST DECLARATOR
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(select (head(poped-state-stack))
                    109 => 110;
                    OTHERWISE => 107;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      let type = process-type-list(reverse!($r1), $state);
                      let (type, name) = process-declarator(type, $r2, $state);
                      make(<arg-declaration>, name: name.string-value, type: type);
                  end,
                  temp1);
           end);
  end;

*production-table*[117] :=
  method (state-stack, symbol-stack, #key $state)
    // PARAMETER-DECLARATION -> TYPE-NAME
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    109 => 110;
                    OTHERWISE => 107;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      make(<arg-declaration>, name: "", type: $r1);
                  end,
                  temp1);
           end);
  end;

*production-table*[118] :=
  method (state-stack, symbol-stack, #key $state)
    // TYPE-NAME -> TYPE-SPECIFIER-LIST
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    31 => 154;
                    38 => 39;
                    204 => 207;
                    209 => 212;
                    OTHERWISE => 106;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      process-type-list(reverse!($r1), $state);
                  end,
                  temp1);
           end);
  end;

*production-table*[119] :=
  method (state-stack, symbol-stack, #key $state)
    // TYPE-NAME -> TYPE-SPECIFIER-LIST ABSTRACT-DECLARATOR
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(select (head(poped-state-stack))
                    31 => 154;
                    38 => 39;
                    204 => 207;
                    209 => 212;
                    OTHERWISE => 106;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      let type = process-type-list(reverse!($r1), $state);
                      let (type, name) = process-declarator(type, $r2, $state);
                      type;
                  end,
                  temp1);
           end);
  end;

*production-table*[120] :=
  method (state-stack, symbol-stack, #key $state)
    // ABSTRACT-DECLARATOR -> POINTER
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    35 => 118;
                    102 => 118;
                    OTHERWISE => 150;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      list($r1);
                  end,
                  temp1);
           end);
  end;

*production-table*[121] :=
  method (state-stack, symbol-stack, #key $state)
    // ABSTRACT-DECLARATOR -> ABSTRACT-DECLARATOR2
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    35 => 118;
                    102 => 118;
                    OTHERWISE => 150;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[122] :=
  method (state-stack, symbol-stack, #key $state)
    // ABSTRACT-DECLARATOR -> POINTER ABSTRACT-DECLARATOR2
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(select (head(poped-state-stack))
                    35 => 118;
                    102 => 118;
                    OTHERWISE => 150;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair($r1, $r2);
                  end,
                  temp1);
           end);
  end;

*production-table*[123] :=
  method (state-stack, symbol-stack, #key $state)
    // ABSTRACT-DECLARATOR2 -> <LPAREN-TOKEN> ABSTRACT-DECLARATOR <RPAREN-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    147 => 148;
                    153 => 148;
                    OTHERWISE => 103;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      $r2;
                  end,
                  temp1);
           end);
  end;

*production-table*[124] :=
  method (state-stack, symbol-stack, #key $state)
    // ABSTRACT-DECLARATOR2 -> <LBRACKET-TOKEN> <RBRACKET-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(select (head(poped-state-stack))
                    147 => 148;
                    153 => 148;
                    OTHERWISE => 103;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      list(#"vector", #f);
                  end,
                  temp1);
           end);
  end;

*production-table*[125] :=
  method (state-stack, symbol-stack, #key $state)
    // ABSTRACT-DECLARATOR2 -> <LBRACKET-TOKEN> CONSTANT-EXPR <RBRACKET-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    147 => 148;
                    153 => 148;
                    OTHERWISE => 103;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      list(#"vector", int-value($r2, $state));
                  end,
                  temp1);
           end);
  end;

*production-table*[126] :=
  method (state-stack, symbol-stack, #key $state)
    // ABSTRACT-DECLARATOR2 -> ABSTRACT-DECLARATOR2 <LBRACKET-TOKEN> <RBRACKET-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    147 => 148;
                    153 => 148;
                    OTHERWISE => 103;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(#"vector", pair(#f, $r1));
                  end,
                  temp1);
           end);
  end;

*production-table*[127] :=
  method (state-stack, symbol-stack, #key $state)
    // ABSTRACT-DECLARATOR2 -> ABSTRACT-DECLARATOR2 <LBRACKET-TOKEN> CONSTANT-EXPR <RBRACKET-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(state-stack))));
             pair(select (head(poped-state-stack))
                    147 => 148;
                    153 => 148;
                    OTHERWISE => 103;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r4 = head(symbol-stack);
             let temp4 = tail(symbol-stack);
             let $r3 = head(temp4);
             let temp3 = tail(temp4);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(#"vector", pair(int-value($r3, $state), $r1));
                  end,
                  temp1);
           end);
  end;

*production-table*[128] :=
  method (state-stack, symbol-stack, #key $state)
    // ABSTRACT-DECLARATOR2 -> <LPAREN-TOKEN> <RPAREN-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(select (head(poped-state-stack))
                    147 => 148;
                    153 => 148;
                    OTHERWISE => 103;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      list(#"function",
                           list(make(<varargs-declaration>, name: "", type: unknown-type)));
                  end,
                  temp1);
           end);
  end;

*production-table*[129] :=
  method (state-stack, symbol-stack, #key $state)
    // ABSTRACT-DECLARATOR2 -> <LPAREN-TOKEN> PARAMETER-TYPE-LIST <RPAREN-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    147 => 148;
                    153 => 148;
                    OTHERWISE => 103;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      list(#"function", reverse!($r2));
                  end,
                  temp1);
           end);
  end;

*production-table*[130] :=
  method (state-stack, symbol-stack, #key $state)
    // ABSTRACT-DECLARATOR2 -> ABSTRACT-DECLARATOR2 <LPAREN-TOKEN> <RPAREN-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    147 => 148;
                    153 => 148;
                    OTHERWISE => 103;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r3 = head(symbol-stack);
             let temp3 = tail(symbol-stack);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(#"function", pair(list(make(<varargs-declaration>,
                                                       name: "", type: unknown-type)), $r1));
                  end,
                  temp1);
           end);
  end;

*production-table*[131] :=
  method (state-stack, symbol-stack, #key $state)
    // ABSTRACT-DECLARATOR2 -> ABSTRACT-DECLARATOR2 <LPAREN-TOKEN> PARAMETER-TYPE-LIST <RPAREN-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(state-stack))));
             pair(select (head(poped-state-stack))
                    147 => 148;
                    153 => 148;
                    OTHERWISE => 103;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r4 = head(symbol-stack);
             let temp4 = tail(symbol-stack);
             let $r3 = head(temp4);
             let temp3 = tail(temp4);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(#"function", pair(reverse!($r3), $r1));
                  end,
                  temp1);
           end);
  end;

*production-table*[132] :=
  method (state-stack, symbol-stack, #key $state)
    // EXTERNAL-DEFINITION -> <BEGIN-INCLUDE-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    0 => 200;
                    OTHERWISE => 203;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      if ($state.verbose)
                        write-element(*standard-error*, '[');
                        force-output(*standard-error*);
                      end if;
                      push-include-level($state, $r1.string-value);
                  end,
                  temp1);
           end);
  end;

*production-table*[133] :=
  method (state-stack, symbol-stack, #key $state)
    // EXTERNAL-DEFINITION -> <END-INCLUDE-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    0 => 200;
                    OTHERWISE => 203;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      if ($state.verbose)
                        write-element(*standard-error*, ']');
                        force-output(*standard-error*);
                      end if;
                      do(curry(add-cpp-declaration, $state), $r1.value);
                      pop-include-level($state);
                  end,
                  temp1);
           end);
  end;

*production-table*[134] :=
  method (state-stack, symbol-stack, #key $state)
    // EXTERNAL-DEFINITION -> DECLARATION
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    0 => 200;
                    OTHERWISE => 203;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      if ($state.verbose)
                        write-element(*standard-error*, '.');
                        force-output(*standard-error*);
                      end if;
                      $r1;
                  end,
                  temp1);
           end);
  end;

*production-table*[135] :=
  method (state-stack, symbol-stack, #key $state)
    // IDENTIFIER -> <IDENTIFIER-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(#[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 19, 0, 0, 21, 0, 0, 0, 0, 21, 0, 0, 0, 21, 0, 0, 0, 0, 0, 0, 21, 0, 21, 0, 21, 0, 0, 0, 21, 0, 21, 0, 21, 0, 0, 21, 0, 21, 0, 0, 21, 0, 21, 0, 0, 21, 0, 21, 0, 21, 0, 21, 0, 0, 21, 0, 21, 0, 0, 21, 0, 0, 21, 0, 0, 21, 0, 0, 21, 0, 0, 21, 0, 21, 0, 21, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 120, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 21, 0, 0, 0, 0, 0, 0, 120, 0, 127, 0, 0, 0, 0, 0, 130, 0, 0, 0, 0, 21, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 120, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 19, 0, 19, 0, 0, 120, 120, 120, 0, 21, 0, 0, 0, 0, 120, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 120, 0, 0, 0, 0, 120, 0, 0, 0, 0, 0, 0, 0, 205, 0, 0, 0, 0, 21, 0, 0, 0, 0, 21][head(poped-state-stack)],
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1;
                  end,
                  temp1);
           end);
  end;


//----------------------------------------------------------------------
// More parser boilerplate
//----------------------------------------------------------------------

define method find-action (table, token)
  let action = element(table, token.object-class, default: #f);
  if (action)
    action;
  else
    parse-error(token, "Parse error at or before %=.", token.string-value);
  end;
end;

define method aux-get-token
    (parse-state :: <parse-state>) => (result :: <token>);
  get-token(parse-state.tokenizer);
end method aux-get-token;

define method aux-get-token
    (parse-state :: <parse-cpp-state>) => (result :: <token>);
  get-token(parse-state.tokenizer, cpp-line: #t, expand: #f);
end method aux-get-token;

define method parse-loop
    (parse-state :: <parse-state>) => (result :: <object>);
  local method step
	    (state-stack :: <list>, symbol-stack :: <list>,
	     token :: <token>, parse-state :: <parse-state>)
#if (~mindy)
	 => (result :: <object>);
#endif
	  let action = find-action(*action-table*[state-stack.head], token);
	  select (action by instance?)
	    <shift> =>
	      step(pair(action.state, state-stack), pair(token, symbol-stack),
		   aux-get-token(parse-state), parse-state);
	    <reduce> =>
	      let (new-state-stack, new-symbol-stack)
		= (*production-table*[action.production]
		     (state-stack, symbol-stack, $state: parse-state));
	      step(new-state-stack, new-symbol-stack, token, parse-state);
	    <accept> =>
	      unget-token(parse-state.tokenizer, token);
	      if (symbol-stack.size ~= 1)
		parse-error(token,
			    "Symbol-stack didn't get reduced all the way?");
	      end;
	      symbol-stack.head;
	  end select;
	end method step;
  step(#(0), #(), aux-get-token(parse-state), parse-state);
end;

//----------------------------------------------------------------------
// External interfaces to the parsing engine.
//----------------------------------------------------------------------

// This function processes an entire include file, leaving a series of
// declarations in the returned parse state.
//
define method parse
    (files :: <sequence> /* of <string> */, #key defines, verbose)
 => (result :: <parse-state>);
  let tokenizer
    = if (files.size == 1)	
        make(<tokenizer>, source: files.first, defines: defines);
      else
        let stream = make(<byte-string-stream>, contents: "",
                          direction: #"input-output");
        for (file in files)
          format(stream, "#include \"%s\"\n", file);
        end for;
	stream.stream-position := #"start";
        make(<tokenizer>, name: "<top-level>", source: stream,
	     defines: defines);
      end if;

  let parse-state = make(<parse-file-state>, tokenizer: tokenizer);
  parse-state.verbose := verbose;

  parse-loop(parse-state);
  if (tokenizer.cpp-decls)
    do(curry(add-cpp-declaration, parse-state), tokenizer.cpp-decls)
  end if;
  parse-state;
end;

// This function parses the contents of the given string and tries to
// interpret it as the name of an object or type declared in "old-state".
// Parse-type will signal an error if no such declaration is found.
//
define method parse-type
    (type :: <string>, old-state :: <parse-file-state>)
 => (result :: <declaration>);
  let tokenizer = make(<tokenizer>, name: type,
		       typedefs-from: old-state.tokenizer,
                       source: make(<byte-string-stream>, contents: type));
  unget-token(tokenizer, make(<alien-name-token>,
			      generator: tokenizer, string: ""));
  let parse-state
    = make(<parse-type-state>, tokenizer: tokenizer, parent: old-state);
  parse-loop(parse-state);
end;

// This function tries to evaluate a preprocessor constant in hopes that
// it will either evaluate to a type or object name or to a constant
// compile time value.  It returns the matched declaration or value, or it
// signals an error. 
//
define method parse-macro
    (name :: <string>, old-state :: <parse-state>)
 => (result :: <object>);
  let old-tokenizer = old-state.tokenizer;
  let tokenizer = make(<tokenizer>, name: name, parent: old-tokenizer,
                       source: make(<byte-string-stream>, contents: " "));
  if (check-cpp-expansion(name, tokenizer))
    unget-token(tokenizer, make(<macro-parse-token>,
				generator: tokenizer, string: ""));
    let parse-state
      = make(<parse-macro-state>, tokenizer: tokenizer, parent: old-state);
    parse-loop(parse-state);
  else
    error("Macro not defined in 'parse-macro'.");
  end if;
end;

// This function evaluates a line of CPP input according to a limited set of C
// operators and an odd set of evaluation rules which make undefined
// identifiers into integers.  (Note that this function is used by the
// tokenizer, but also recursively uses the tokenizer by specifying a few
// magic keywords to avoid infinite recursion.)  This function consumes one
// line's worth of tokens from the tokenizer and then leaves it in a
// consistent state for further processing by a different parser.
//
define method cpp-parse (tokenizer :: <tokenizer>) => result :: <integer>;
  block ()
    let parse-state
      = make(<parse-cpp-state>, tokenizer: tokenizer);
    unget-token(tokenizer, make(<cpp-parse-token>,
				generator: tokenizer, string: ""));
    parse-loop(parse-state);
  cleanup 
    get-token(tokenizer); // un-unget the <eof-token>, since we may want to
                          // continue with this tokenizer using a different
                          // lexer
  end block;
end;

// Seals for file c-parse.dylan

// <action> -- subclass of <object>
define sealed domain make(singleton(<action>));
define sealed domain initialize(<action>);
// <shift> -- subclass of <action>
define sealed domain make(singleton(<shift>));
// <reduce> -- subclass of <action>
define sealed domain make(singleton(<reduce>));
// <accept> -- subclass of <action>
define sealed domain make(singleton(<accept>));
// <alien-name-token> -- subclass of <token>
define sealed domain make(singleton(<alien-name-token>));
define sealed domain initialize(<alien-name-token>);
// <macro-parse-token> -- subclass of <token>
define sealed domain make(singleton(<macro-parse-token>));
define sealed domain initialize(<macro-parse-token>);
// <cpp-parse-token> -- subclass of <token>
define sealed domain make(singleton(<cpp-parse-token>));
define sealed domain initialize(<cpp-parse-token>);
