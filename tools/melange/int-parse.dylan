documented: #t
module: int-parse
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
// Module int-parse handles parsing of define interface forms in interace
// files.  It makes no attempt to handle any other part of the Dylan language
// at this time, since it will eventually be merged into the native parser for
// the Gwydion compiler.
//
// The actual "source" for this parser is contained in "int-parse.input",
// which is an input file for a lisp-based Dylan parser generator.  (This is a
// logical route to follow for bootstrapping purposes.  It would clearly be
// useful to re-implement the generator in Dylan at some future date.)
// "Int-parse.dylan" is the pure Dylan output of this generator, and should
// *not* be considered human readable code.
//
// The "parse-tree" is a very ad-hoc structure, because it is intended to be
// quite ephemeral.  The final goal is to add annotations to the "parse-state"
// which describe the C header file to be processed and the transformations to
// apply to that header file's declarations.
//
// The external interface to the parser is the function "parse", which has the
// following interface:
//   parse(parse-state, #key defines, undefines) => ()
//     This function processes a single "define interface" form, using the
//     tokenizer in "parse-state", and annotates the parse state with the
//     information acquired.
// <parse-state> is defined below, and contains both global modifiers such as
// "include file" or "object file" and "clauses" which describe the processing
// of a single object.
//
// Note that this parser is atypical in that it does not try to consume all of
// the input.  It simply goes until it reaches the end of the "define
// interface" form, and the stops.  Since this is foreign to the parser
// generator's internal model, <eof-token> has been redefined to encompass all
// possible tokens.  Although this works, it is probably inherently unstable.
// Some extra code has been added to the generic "make-action-table" to insure
// that the omnipresence of <eof-token> doesn't screw up the tables.
//======================================================================

define module int-parse
  use dylan;
  use extensions;
  use self-organizing-list;
  use int-lexer;
  export
    parse, <parse-state>, include-file, object-files, mindy-include-file,
    container-options, macro-defines, macro-undefines, clauses,
    <container-options>, name-mapper, imports, prefix, exclude, rename,
    mappings, equates, read-only, seal-string, <clause>, <function-clause>,
    <struct-clause>, <union-clause>, <pointer-clause>,
    <constant-clause>, <variable-clause>,
    name, options, <undefined>, undefined;
end module int-parse;

//----------------------------------------------------------------------
// Simple parser support 
//----------------------------------------------------------------------

// Designates an "optional" string.
//
define constant <string?> = type-union(<string>, <false>);

// Undefined values are useful for values that may need to be defaulted.  This
// is superious do simply filling in the default value at the start, since it
// allows us to merge several <container-option>s before doing the defaulting.
//
define class <undefined> (<object>) end class;
define constant undefined = make(<undefined>);

// Container options can either describe global modifications to be applied to
// all objects or can describe a set of modifications for some single object.
// Each of the fields corresponds pretty closely to a single "container
// option" in the Creole/Melange specs.
//
define class <container-options> (<object>)
  slot name-mapper :: type-union(<symbol>, <undefined>), init-value: undefined;
  // each element of imports is either #"all" or an import list
  slot imports :: <sequence>, init-value: #();
  slot prefix :: type-union(<string>, <undefined>), init-value: undefined;
  slot exclude :: <sequence>, init-value: #();
  slot rename :: <sequence>, init-value: #();
  slot mappings :: <sequence>, init-value: #();
  slot equates :: <sequence>, init-value: #();
  slot read-only :: type-union(<boolean>, <undefined>), init-value: undefined;
  slot seal-string :: type-union(<string>, <undefined>), init-value: undefined;
end class <container-options>;

// This structure contains the tokenizer which defines the input to the
// parser, and also contains slot which will be filled in based upon the
// results of the parse.  It includes slots for various options which are
// unique to "#include" clauses, a <container-options> slot which corresponds
// to container options that have been globally applied in the "#include"
// clause, and a sequence of "<clause>"s (defined below) in no particular
// order. 
//
define class <parse-state> (<object>)
  slot tokenizer :: <tokenizer>, required-init-keyword: #"tokenizer";
  slot include-file :: <string?>, init-value: #f;
  slot object-files :: <deque>, init-function: curry(make, <deque>);
  slot mindy-include-file :: <string?>, init-value: #f;
  slot macro-defines :: <deque>, init-function: curry(make, <deque>);
  slot macro-undefines :: <deque>, init-function: curry(make, <deque>);
  slot container-options :: <container-options>;
  slot clauses :: <sequence>, init-value: #();
end class <parse-state>;

// This function fills in slots of a <container-option> based upon the raw
// "parse-tree" returned by some production.  This stuff is very ad-hoc, since
// there is little use in carefully defining the structure of an object that
// will last for milliseconds or less.
//
define method process-container-options 
    (parsed-clauses :: <list>)
 => (result :: <container-options>, rest :: <list>);
  let result = make(<container-options>);
  let left = #();
  for (item in parsed-clauses)
    select (item.head)
      #"name-mapper" =>
        if (result.name-mapper ~= undefined)
          error("Multiple name mappers in one #include clause");
        else
          result.name-mapper := item.tail;
        end if;
      #"import" =>
        result.imports := pair(item.tail, result.imports);
      #"prefix" =>
        if (result.prefix ~= undefined)
          error("Multiple prefixes in one #include clause");
        else
          result.prefix := item.tail;
        end if;
      #"exclude" =>
        result.exclude := reduce(method(a,b) pair(b,a) end method,
                                 result.exclude, item.tail);
      #"rename" =>
        result.rename := reduce(method(a,b) pair(b,a) end method,
                                result.rename, item.tail);
      #"mapping" =>
        result.mappings := reduce(method(a,b) pair(b,a) end method,
                                  result.mappings, item.tail);
      #"equate" =>
        result.equates := reduce(method(a,b) pair(b,a) end method,
                      	         result.equates, item.tail);
      #"read-only" =>
        result.read-only := item.tail;
      #"seal" =>
        result.seal-string := item.tail;
      otherwise =>
        left := pair(item, left);
    end select;
  end for;
  values(result, reverse!(left));
end method process-container-options;

// This corresponds to any "clause" in the interface definition which is not a
// "#include".  Most of them are simply typed "boxes" for sequences of
// "options".  At present the options consist of a sequence labelled by an
// initial symbol.  At some later time they may be fleshed out in more detail.
// Some clauses also contain a <container-options> structure which breaks down
// some of the options in a more convenient format.
//
define class <clause> (<object>) 
  slot name :: <string>, required-init-keyword: #"name";
  slot options :: <sequence>, required-init-keyword: #"options";
end class;

define class <function-clause> (<clause>) end class;
define class <variable-clause> (<clause>) end class;
define class <constant-clause> (<clause>) end class;
define class <pointer-clause> (<clause>) end class;

define class <container-clause> (<clause>)
  slot container-options :: <container-options>;
end class <container-clause>;

define class <struct-clause> (<container-clause>) end class;
define class <union-clause> (<container-clause>) end class;

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

// Because <eof-token> encompasses all other token classes, we must keep
// actions for <eof-token> from overwriting other valid actions.  This is an
// ugly special case, but allows us to use the parser generator for an
// application beyond its normal capabilities -- i.e. parsing only a portion
// of a file.  (In practice, the extra code is probably superfluous, as the
// states which acceof <eof-token> don't accept anything else -- but better
// safe than sorry.
//
define method make-action-table(#rest actions)
  let result = make(<self-organizing-list>);
  for (action in actions)
    local
      method process (clas :: <class>, is-eof)
	unless (is-eof & key-exists?(result, clas))
	  result[clas] := action;
	  for (sub in clas.direct-subclasses)
	    process(sub, is-eof);
	  end;
	end unless;
      end;
    process(action.on, action.on == <eof-token>);
  end;
  result;
end;

//----------------------------------------------------------------------
// The actual productions.  The format is
//  production (sub-production-or-<token> .....)
//    Arbitrary dylan code -- variables $r1 - $rn correspond to the
//    sub-productions, and $state is a <parse-state> which is passed into each
//    action routine for record-keeping purposes.
//  %
//----------------------------------------------------------------------

define constant *action-table* = make(<vector>, size: 185);
define constant *production-table* = make(<vector>, size: 95);

*action-table*[0] :=
  // S-PRIME -> * PARSE-ROOT
  make-action-table(make(<shift>, on: <DEFINE-TOKEN>, state: 1));

*action-table*[1] :=
  // INTERFACE-DEF -> <DEFINE-TOKEN> * <INTERFACE-TOKEN> INTERFACE-CLAUSE-LIST <END-TOKEN> <INTERFACE-TOKEN>
  // INTERFACE-DEF -> <DEFINE-TOKEN> * <INTERFACE-TOKEN> INTERFACE-CLAUSE-LIST <END-TOKEN>
  make-action-table(make(<shift>, on: <INTERFACE-TOKEN>, state: 2));

*action-table*[2] :=
  // INTERFACE-DEF -> <DEFINE-TOKEN> <INTERFACE-TOKEN> * INTERFACE-CLAUSE-LIST <END-TOKEN>
  // INTERFACE-DEF -> <DEFINE-TOKEN> <INTERFACE-TOKEN> * INTERFACE-CLAUSE-LIST <END-TOKEN> <INTERFACE-TOKEN>
  make-action-table(make(<shift>, on: <CONSTANT-TOKEN>, state: 145),
                    make(<shift>, on: <FUNCTION-TOKEN>, state: 79),
                    make(<shift>, on: <INCLUDE-TOKEN>, state: 3),
                    make(<shift>, on: <POINTER-TOKEN>, state: 136),
                    make(<shift>, on: <STRUCT-TOKEN>, state: 114),
                    make(<shift>, on: <UNION-TOKEN>, state: 130),
                    make(<shift>, on: <VARIABLE-TOKEN>, state: 155));

*action-table*[3] :=
  // FILE-CLAUSE -> <INCLUDE-TOKEN> * <STRING-LITERAL-TOKEN> FILE-OPTION-LIST
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 4));

*action-table*[4] :=
  // FILE-CLAUSE -> <INCLUDE-TOKEN> <STRING-LITERAL-TOKEN> * FILE-OPTION-LIST
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 11),
                    make(<reduce>, on: <END-TOKEN>, production: 11),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 11));

*action-table*[5] :=
  // FILE-OPTION-LIST -> FILE-OPTION-LIST * <COMMA-TOKEN> FILE-OPTION
  // FILE-CLAUSE -> <INCLUDE-TOKEN> <STRING-LITERAL-TOKEN> FILE-OPTION-LIST *
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 6),
                    make(<reduce>, on: <END-TOKEN>, production: 10),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 10));

*action-table*[6] :=
  // FILE-OPTION-LIST -> FILE-OPTION-LIST <COMMA-TOKEN> * FILE-OPTION
  make-action-table(make(<shift>, on: <DEFINE-MACRO-TOKEN>, state: 11),
                    make(<shift>, on: <EQUATE-TOKEN>, state: 65),
                    make(<shift>, on: <EXCLUDE-TOKEN>, state: 46),
                    make(<shift>, on: <IMPORT-TOKEN>, state: 32),
                    make(<shift>, on: <MAPPING-TOKEN>, state: 61),
                    make(<shift>, on: <MINDY-INC-TOKEN>, state: 9),
                    make(<shift>, on: <NAME-MAPPER-TOKEN>, state: 30),
                    make(<shift>, on: <OBJECT-FILE-TOKEN>, state: 7),
                    make(<shift>, on: <PREFIX-TOKEN>, state: 44),
                    make(<shift>, on: <READ-ONLY-TOKEN>, state: 74),
                    make(<shift>, on: <RENAME-TOKEN>, state: 53),
                    make(<shift>, on: <SEAL-FUNCTIONS-TOKEN>, state: 69),
                    make(<shift>, on: <UNDEFINE-TOKEN>, state: 22));

*action-table*[7] :=
  // FILE-OPTION -> <OBJECT-FILE-TOKEN> * <STRING-LITERAL-TOKEN>
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 8));

*action-table*[8] :=
  // FILE-OPTION -> <OBJECT-FILE-TOKEN> <STRING-LITERAL-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 13),
                    make(<reduce>, on: <END-TOKEN>, production: 13),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 13));

*action-table*[9] :=
  // FILE-OPTION -> <MINDY-INC-TOKEN> * <STRING-LITERAL-TOKEN>
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 10));

*action-table*[10] :=
  // FILE-OPTION -> <MINDY-INC-TOKEN> <STRING-LITERAL-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 14),
                    make(<reduce>, on: <END-TOKEN>, production: 14),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 14));

*action-table*[11] :=
  // FILE-OPTION -> <DEFINE-MACRO-TOKEN> * <LBRACE-TOKEN> MACRO-DEFINITIONS <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <LBRACE-TOKEN>, state: 12));

*action-table*[12] :=
  // FILE-OPTION -> <DEFINE-MACRO-TOKEN> <LBRACE-TOKEN> * MACRO-DEFINITIONS <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 13));

*action-table*[13] :=
  // MACRO-DEFINITION -> <STRING-LITERAL-TOKEN> *
  // MACRO-DEFINITION -> <STRING-LITERAL-TOKEN> * <ARROW-TOKEN> <STRING-LITERAL-TOKEN>
  // MACRO-DEFINITION -> <STRING-LITERAL-TOKEN> * <ARROW-TOKEN> <INTEGER-TOKEN>
  make-action-table(make(<shift>, on: <ARROW-TOKEN>, state: 14),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 20),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 20));

*action-table*[14] :=
  // MACRO-DEFINITION -> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> * <INTEGER-TOKEN>
  // MACRO-DEFINITION -> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> * <STRING-LITERAL-TOKEN>
  make-action-table(make(<shift>, on: <INTEGER-TOKEN>, state: 16),
                    make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 15));

*action-table*[15] :=
  // MACRO-DEFINITION -> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <STRING-LITERAL-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 21),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 21));

*action-table*[16] :=
  // MACRO-DEFINITION -> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <INTEGER-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 22),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 22));

*action-table*[17] :=
  // MACRO-DEFINITIONS -> MACRO-DEFINITION *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 18),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 18));

*action-table*[18] :=
  // MACRO-DEFINITIONS -> MACRO-DEFINITIONS * <COMMA-TOKEN> MACRO-DEFINITION
  // FILE-OPTION -> <DEFINE-MACRO-TOKEN> <LBRACE-TOKEN> MACRO-DEFINITIONS * <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 20),
                    make(<shift>, on: <RBRACE-TOKEN>, state: 19));

*action-table*[19] :=
  // FILE-OPTION -> <DEFINE-MACRO-TOKEN> <LBRACE-TOKEN> MACRO-DEFINITIONS <RBRACE-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 15),
                    make(<reduce>, on: <END-TOKEN>, production: 15),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 15));

*action-table*[20] :=
  // MACRO-DEFINITIONS -> MACRO-DEFINITIONS <COMMA-TOKEN> * MACRO-DEFINITION
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 13));

*action-table*[21] :=
  // MACRO-DEFINITIONS -> MACRO-DEFINITIONS <COMMA-TOKEN> MACRO-DEFINITION *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 19),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 19));

*action-table*[22] :=
  // FILE-OPTION -> <UNDEFINE-TOKEN> * <LBRACE-TOKEN> MACRO-UNDEFINITIONS <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <LBRACE-TOKEN>, state: 23));

*action-table*[23] :=
  // FILE-OPTION -> <UNDEFINE-TOKEN> <LBRACE-TOKEN> * MACRO-UNDEFINITIONS <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 24));

*action-table*[24] :=
  // MACRO-UNDEFINITION -> <STRING-LITERAL-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 25),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 25));

*action-table*[25] :=
  // MACRO-UNDEFINITIONS -> MACRO-UNDEFINITION *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 23),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 23));

*action-table*[26] :=
  // MACRO-UNDEFINITIONS -> MACRO-UNDEFINITIONS * <COMMA-TOKEN> MACRO-UNDEFINITION
  // FILE-OPTION -> <UNDEFINE-TOKEN> <LBRACE-TOKEN> MACRO-UNDEFINITIONS * <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 28),
                    make(<shift>, on: <RBRACE-TOKEN>, state: 27));

*action-table*[27] :=
  // FILE-OPTION -> <UNDEFINE-TOKEN> <LBRACE-TOKEN> MACRO-UNDEFINITIONS <RBRACE-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 16),
                    make(<reduce>, on: <END-TOKEN>, production: 16),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 16));

*action-table*[28] :=
  // MACRO-UNDEFINITIONS -> MACRO-UNDEFINITIONS <COMMA-TOKEN> * MACRO-UNDEFINITION
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 24));

*action-table*[29] :=
  // MACRO-UNDEFINITIONS -> MACRO-UNDEFINITIONS <COMMA-TOKEN> MACRO-UNDEFINITION *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 24),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 24));

*action-table*[30] :=
  // CONTAINER-OPTION -> <NAME-MAPPER-TOKEN> * <IDENTIFIER-TOKEN>
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 31));

*action-table*[31] :=
  // CONTAINER-OPTION -> <NAME-MAPPER-TOKEN> <IDENTIFIER-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 26),
                    make(<reduce>, on: <END-TOKEN>, production: 26),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 26));

*action-table*[32] :=
  // CONTAINER-OPTION -> <IMPORT-TOKEN> * <ALL-TOKEN>
  // CONTAINER-OPTION -> <IMPORT-TOKEN> * <LBRACE-TOKEN> IMPORT-LIST <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <ALL-TOKEN>, state: 43),
                    make(<shift>, on: <LBRACE-TOKEN>, state: 33));

*action-table*[33] :=
  // CONTAINER-OPTION -> <IMPORT-TOKEN> <LBRACE-TOKEN> * IMPORT-LIST <RBRACE-TOKEN>
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 29),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 29),
                    make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 34));

*action-table*[34] :=
  // IMPORT -> <STRING-LITERAL-TOKEN> *
  // RENAMING -> <STRING-LITERAL-TOKEN> * <ARROW-TOKEN> <IDENTIFIER-TOKEN>
  make-action-table(make(<shift>, on: <ARROW-TOKEN>, state: 35),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 32),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 32));

*action-table*[35] :=
  // RENAMING -> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> * <IDENTIFIER-TOKEN>
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 36));

*action-table*[36] :=
  // RENAMING -> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 34),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 34));

*action-table*[37] :=
  // IMPORT -> RENAMING *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 33),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 33));

*action-table*[38] :=
  // IMPORT-LIST -> IMPORT *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 31),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 31));

*action-table*[39] :=
  // IMPORT-LIST -> IMPORT-LIST * <COMMA-TOKEN> IMPORT
  // CONTAINER-OPTION -> <IMPORT-TOKEN> <LBRACE-TOKEN> IMPORT-LIST * <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 41),
                    make(<shift>, on: <RBRACE-TOKEN>, state: 40));

*action-table*[40] :=
  // CONTAINER-OPTION -> <IMPORT-TOKEN> <LBRACE-TOKEN> IMPORT-LIST <RBRACE-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 28),
                    make(<reduce>, on: <END-TOKEN>, production: 28),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 28));

*action-table*[41] :=
  // IMPORT-LIST -> IMPORT-LIST <COMMA-TOKEN> * IMPORT
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 34));

*action-table*[42] :=
  // IMPORT-LIST -> IMPORT-LIST <COMMA-TOKEN> IMPORT *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 30),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 30));

*action-table*[43] :=
  // CONTAINER-OPTION -> <IMPORT-TOKEN> <ALL-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 27),
                    make(<reduce>, on: <END-TOKEN>, production: 27),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 27));

*action-table*[44] :=
  // CONTAINER-OPTION -> <PREFIX-TOKEN> * <STRING-LITERAL-TOKEN>
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 45));

*action-table*[45] :=
  // CONTAINER-OPTION -> <PREFIX-TOKEN> <STRING-LITERAL-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 35),
                    make(<reduce>, on: <END-TOKEN>, production: 35),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 35));

*action-table*[46] :=
  // CONTAINER-OPTION -> <EXCLUDE-TOKEN> * <LBRACE-TOKEN> EXCLUDE-LIST <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <LBRACE-TOKEN>, state: 47));

*action-table*[47] :=
  // CONTAINER-OPTION -> <EXCLUDE-TOKEN> <LBRACE-TOKEN> * EXCLUDE-LIST <RBRACE-TOKEN>
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 37),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 37),
                    make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 48));

*action-table*[48] :=
  // EXCLUDE-LIST -> <STRING-LITERAL-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 39),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 39));

*action-table*[49] :=
  // EXCLUDE-LIST -> EXCLUDE-LIST * <COMMA-TOKEN> <STRING-LITERAL-TOKEN>
  // CONTAINER-OPTION -> <EXCLUDE-TOKEN> <LBRACE-TOKEN> EXCLUDE-LIST * <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 51),
                    make(<shift>, on: <RBRACE-TOKEN>, state: 50));

*action-table*[50] :=
  // CONTAINER-OPTION -> <EXCLUDE-TOKEN> <LBRACE-TOKEN> EXCLUDE-LIST <RBRACE-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 36),
                    make(<reduce>, on: <END-TOKEN>, production: 36),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 36));

*action-table*[51] :=
  // EXCLUDE-LIST -> EXCLUDE-LIST <COMMA-TOKEN> * <STRING-LITERAL-TOKEN>
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 52));

*action-table*[52] :=
  // EXCLUDE-LIST -> EXCLUDE-LIST <COMMA-TOKEN> <STRING-LITERAL-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 38),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 38));

*action-table*[53] :=
  // CONTAINER-OPTION -> <RENAME-TOKEN> * <LBRACE-TOKEN> RENAMING-LIST <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <LBRACE-TOKEN>, state: 54));

*action-table*[54] :=
  // CONTAINER-OPTION -> <RENAME-TOKEN> <LBRACE-TOKEN> * RENAMING-LIST <RBRACE-TOKEN>
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 41),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 41),
                    make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 55));

*action-table*[55] :=
  // RENAMING -> <STRING-LITERAL-TOKEN> * <ARROW-TOKEN> <IDENTIFIER-TOKEN>
  make-action-table(make(<shift>, on: <ARROW-TOKEN>, state: 35));

*action-table*[56] :=
  // RENAMING-LIST -> RENAMING *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 43),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 43));

*action-table*[57] :=
  // RENAMING-LIST -> RENAMING-LIST * <COMMA-TOKEN> RENAMING
  // CONTAINER-OPTION -> <RENAME-TOKEN> <LBRACE-TOKEN> RENAMING-LIST * <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 59),
                    make(<shift>, on: <RBRACE-TOKEN>, state: 58));

*action-table*[58] :=
  // CONTAINER-OPTION -> <RENAME-TOKEN> <LBRACE-TOKEN> RENAMING-LIST <RBRACE-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 40),
                    make(<reduce>, on: <END-TOKEN>, production: 40),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 40));

*action-table*[59] :=
  // RENAMING-LIST -> RENAMING-LIST <COMMA-TOKEN> * RENAMING
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 55));

*action-table*[60] :=
  // RENAMING-LIST -> RENAMING-LIST <COMMA-TOKEN> RENAMING *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 42),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 42));

*action-table*[61] :=
  // CONTAINER-OPTION -> <MAPPING-TOKEN> * <LBRACE-TOKEN> RENAMING-LIST <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <LBRACE-TOKEN>, state: 62));

*action-table*[62] :=
  // CONTAINER-OPTION -> <MAPPING-TOKEN> <LBRACE-TOKEN> * RENAMING-LIST <RBRACE-TOKEN>
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 41),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 41),
                    make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 55));

*action-table*[63] :=
  // RENAMING-LIST -> RENAMING-LIST * <COMMA-TOKEN> RENAMING
  // CONTAINER-OPTION -> <MAPPING-TOKEN> <LBRACE-TOKEN> RENAMING-LIST * <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 59),
                    make(<shift>, on: <RBRACE-TOKEN>, state: 64));

*action-table*[64] :=
  // CONTAINER-OPTION -> <MAPPING-TOKEN> <LBRACE-TOKEN> RENAMING-LIST <RBRACE-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 44),
                    make(<reduce>, on: <END-TOKEN>, production: 44),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 44));

*action-table*[65] :=
  // CONTAINER-OPTION -> <EQUATE-TOKEN> * <LBRACE-TOKEN> RENAMING-LIST <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <LBRACE-TOKEN>, state: 66));

*action-table*[66] :=
  // CONTAINER-OPTION -> <EQUATE-TOKEN> <LBRACE-TOKEN> * RENAMING-LIST <RBRACE-TOKEN>
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 41),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 41),
                    make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 55));

*action-table*[67] :=
  // RENAMING-LIST -> RENAMING-LIST * <COMMA-TOKEN> RENAMING
  // CONTAINER-OPTION -> <EQUATE-TOKEN> <LBRACE-TOKEN> RENAMING-LIST * <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 59),
                    make(<shift>, on: <RBRACE-TOKEN>, state: 68));

*action-table*[68] :=
  // CONTAINER-OPTION -> <EQUATE-TOKEN> <LBRACE-TOKEN> RENAMING-LIST <RBRACE-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 45),
                    make(<reduce>, on: <END-TOKEN>, production: 45),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 45));

*action-table*[69] :=
  // CONTAINER-OPTION -> <SEAL-FUNCTIONS-TOKEN> * SEALING
  make-action-table(make(<shift>, on: <INLINE-TOKEN>, state: 72),
                    make(<shift>, on: <OPEN-TOKEN>, state: 71),
                    make(<shift>, on: <SEALED-TOKEN>, state: 70));

*action-table*[70] :=
  // SEALING -> <SEALED-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 46),
                    make(<reduce>, on: <END-TOKEN>, production: 46),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 46));

*action-table*[71] :=
  // SEALING -> <OPEN-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 47),
                    make(<reduce>, on: <END-TOKEN>, production: 47),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 47));

*action-table*[72] :=
  // SEALING -> <INLINE-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 48),
                    make(<reduce>, on: <END-TOKEN>, production: 48),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 48));

*action-table*[73] :=
  // CONTAINER-OPTION -> <SEAL-FUNCTIONS-TOKEN> SEALING *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 49),
                    make(<reduce>, on: <END-TOKEN>, production: 49),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 49));

*action-table*[74] :=
  // CONTAINER-OPTION -> <READ-ONLY-TOKEN> * <BOOLEAN-TOKEN>
  make-action-table(make(<shift>, on: <BOOLEAN-TOKEN>, state: 75));

*action-table*[75] :=
  // CONTAINER-OPTION -> <READ-ONLY-TOKEN> <BOOLEAN-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 50),
                    make(<reduce>, on: <END-TOKEN>, production: 50),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 50));

*action-table*[76] :=
  // FILE-OPTION -> CONTAINER-OPTION *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 17),
                    make(<reduce>, on: <END-TOKEN>, production: 17),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 17));

*action-table*[77] :=
  // FILE-OPTION-LIST -> FILE-OPTION-LIST <COMMA-TOKEN> FILE-OPTION *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 12),
                    make(<reduce>, on: <END-TOKEN>, production: 12),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 12));

*action-table*[78] :=
  // INTERFACE-CLAUSE -> FILE-CLAUSE *
  make-action-table(make(<reduce>, on: <END-TOKEN>, production: 9),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 9));

*action-table*[79] :=
  // INTERFACE-CLAUSE -> <FUNCTION-TOKEN> * <STRING-LITERAL-TOKEN> FUNCTION-OPTION-LIST
  // INTERFACE-CLAUSE -> <FUNCTION-TOKEN> * <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> FUNCTION-OPTION-LIST
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 80));

*action-table*[80] :=
  // INTERFACE-CLAUSE -> <FUNCTION-TOKEN> <STRING-LITERAL-TOKEN> * <ARROW-TOKEN> <IDENTIFIER-TOKEN> FUNCTION-OPTION-LIST
  // INTERFACE-CLAUSE -> <FUNCTION-TOKEN> <STRING-LITERAL-TOKEN> * FUNCTION-OPTION-LIST
  make-action-table(make(<shift>, on: <ARROW-TOKEN>, state: 111),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 53),
                    make(<reduce>, on: <END-TOKEN>, production: 53),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 53));

*action-table*[81] :=
  // FUNCTION-OPTION-LIST -> FUNCTION-OPTION-LIST * <COMMA-TOKEN> FUNCTION-OPTION
  // INTERFACE-CLAUSE -> <FUNCTION-TOKEN> <STRING-LITERAL-TOKEN> FUNCTION-OPTION-LIST *
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 82),
                    make(<reduce>, on: <END-TOKEN>, production: 51),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 51));

*action-table*[82] :=
  // FUNCTION-OPTION-LIST -> FUNCTION-OPTION-LIST <COMMA-TOKEN> * FUNCTION-OPTION
  make-action-table(make(<shift>, on: <EQUATE-ARGUMENT-TOKEN>, state: 98),
                    make(<shift>, on: <EQUATE-RESULT-TOKEN>, state: 85),
                    make(<shift>, on: <IGNORE-RESULT-TOKEN>, state: 87),
                    make(<shift>, on: <INPUT-ARGUMENT-TOKEN>, state: 104),
                    make(<shift>, on: <INPUT-OUTPUT-ARGUMENT-TOKEN>, state: 106),
                    make(<shift>, on: <MAP-ARGUMENT-TOKEN>, state: 89),
                    make(<shift>, on: <MAP-RESULT-TOKEN>, state: 83),
                    make(<shift>, on: <OUTPUT-ARGUMENT-TOKEN>, state: 108));

*action-table*[83] :=
  // FUNCTION-OPTION -> <MAP-RESULT-TOKEN> * <IDENTIFIER-TOKEN>
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 84));

*action-table*[84] :=
  // FUNCTION-OPTION -> <MAP-RESULT-TOKEN> <IDENTIFIER-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 55),
                    make(<reduce>, on: <END-TOKEN>, production: 55),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 55));

*action-table*[85] :=
  // FUNCTION-OPTION -> <EQUATE-RESULT-TOKEN> * <IDENTIFIER-TOKEN>
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 86));

*action-table*[86] :=
  // FUNCTION-OPTION -> <EQUATE-RESULT-TOKEN> <IDENTIFIER-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 56),
                    make(<reduce>, on: <END-TOKEN>, production: 56),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 56));

*action-table*[87] :=
  // FUNCTION-OPTION -> <IGNORE-RESULT-TOKEN> * <BOOLEAN-TOKEN>
  make-action-table(make(<shift>, on: <BOOLEAN-TOKEN>, state: 88));

*action-table*[88] :=
  // FUNCTION-OPTION -> <IGNORE-RESULT-TOKEN> <BOOLEAN-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 57),
                    make(<reduce>, on: <END-TOKEN>, production: 57),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 57));

*action-table*[89] :=
  // FUNCTION-OPTION -> <MAP-ARGUMENT-TOKEN> * <LBRACE-TOKEN> ARGUMENT <ARROW-TOKEN> <IDENTIFIER-TOKEN> <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <LBRACE-TOKEN>, state: 90));

*action-table*[90] :=
  // FUNCTION-OPTION -> <MAP-ARGUMENT-TOKEN> <LBRACE-TOKEN> * ARGUMENT <ARROW-TOKEN> <IDENTIFIER-TOKEN> <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 92),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 91),
                    make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 93));

*action-table*[91] :=
  // ARGUMENT -> <INTEGER-TOKEN> *
  make-action-table(make(<reduce>, on: <ARROW-TOKEN>, production: 63),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 63),
                    make(<reduce>, on: <END-TOKEN>, production: 63),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 63));

*action-table*[92] :=
  // ARGUMENT -> <IDENTIFIER-TOKEN> *
  make-action-table(make(<reduce>, on: <ARROW-TOKEN>, production: 64),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 64),
                    make(<reduce>, on: <END-TOKEN>, production: 64),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 64));

*action-table*[93] :=
  // ARGUMENT -> <STRING-LITERAL-TOKEN> *
  make-action-table(make(<reduce>, on: <ARROW-TOKEN>, production: 65),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 65),
                    make(<reduce>, on: <END-TOKEN>, production: 65),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 65));

*action-table*[94] :=
  // FUNCTION-OPTION -> <MAP-ARGUMENT-TOKEN> <LBRACE-TOKEN> ARGUMENT * <ARROW-TOKEN> <IDENTIFIER-TOKEN> <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <ARROW-TOKEN>, state: 95));

*action-table*[95] :=
  // FUNCTION-OPTION -> <MAP-ARGUMENT-TOKEN> <LBRACE-TOKEN> ARGUMENT <ARROW-TOKEN> * <IDENTIFIER-TOKEN> <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 96));

*action-table*[96] :=
  // FUNCTION-OPTION -> <MAP-ARGUMENT-TOKEN> <LBRACE-TOKEN> ARGUMENT <ARROW-TOKEN> <IDENTIFIER-TOKEN> * <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <RBRACE-TOKEN>, state: 97));

*action-table*[97] :=
  // FUNCTION-OPTION -> <MAP-ARGUMENT-TOKEN> <LBRACE-TOKEN> ARGUMENT <ARROW-TOKEN> <IDENTIFIER-TOKEN> <RBRACE-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 58),
                    make(<reduce>, on: <END-TOKEN>, production: 58),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 58));

*action-table*[98] :=
  // FUNCTION-OPTION -> <EQUATE-ARGUMENT-TOKEN> * <LBRACE-TOKEN> ARGUMENT <ARROW-TOKEN> <IDENTIFIER-TOKEN> <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <LBRACE-TOKEN>, state: 99));

*action-table*[99] :=
  // FUNCTION-OPTION -> <EQUATE-ARGUMENT-TOKEN> <LBRACE-TOKEN> * ARGUMENT <ARROW-TOKEN> <IDENTIFIER-TOKEN> <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 92),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 91),
                    make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 93));

*action-table*[100] :=
  // FUNCTION-OPTION -> <EQUATE-ARGUMENT-TOKEN> <LBRACE-TOKEN> ARGUMENT * <ARROW-TOKEN> <IDENTIFIER-TOKEN> <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <ARROW-TOKEN>, state: 101));

*action-table*[101] :=
  // FUNCTION-OPTION -> <EQUATE-ARGUMENT-TOKEN> <LBRACE-TOKEN> ARGUMENT <ARROW-TOKEN> * <IDENTIFIER-TOKEN> <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 102));

*action-table*[102] :=
  // FUNCTION-OPTION -> <EQUATE-ARGUMENT-TOKEN> <LBRACE-TOKEN> ARGUMENT <ARROW-TOKEN> <IDENTIFIER-TOKEN> * <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <RBRACE-TOKEN>, state: 103));

*action-table*[103] :=
  // FUNCTION-OPTION -> <EQUATE-ARGUMENT-TOKEN> <LBRACE-TOKEN> ARGUMENT <ARROW-TOKEN> <IDENTIFIER-TOKEN> <RBRACE-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 59),
                    make(<reduce>, on: <END-TOKEN>, production: 59),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 59));

*action-table*[104] :=
  // FUNCTION-OPTION -> <INPUT-ARGUMENT-TOKEN> * ARGUMENT
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 92),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 91),
                    make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 93));

*action-table*[105] :=
  // FUNCTION-OPTION -> <INPUT-ARGUMENT-TOKEN> ARGUMENT *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 60),
                    make(<reduce>, on: <END-TOKEN>, production: 60),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 60));

*action-table*[106] :=
  // FUNCTION-OPTION -> <INPUT-OUTPUT-ARGUMENT-TOKEN> * ARGUMENT
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 92),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 91),
                    make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 93));

*action-table*[107] :=
  // FUNCTION-OPTION -> <INPUT-OUTPUT-ARGUMENT-TOKEN> ARGUMENT *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 61),
                    make(<reduce>, on: <END-TOKEN>, production: 61),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 61));

*action-table*[108] :=
  // FUNCTION-OPTION -> <OUTPUT-ARGUMENT-TOKEN> * ARGUMENT
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 92),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 91),
                    make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 93));

*action-table*[109] :=
  // FUNCTION-OPTION -> <OUTPUT-ARGUMENT-TOKEN> ARGUMENT *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 62),
                    make(<reduce>, on: <END-TOKEN>, production: 62),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 62));

*action-table*[110] :=
  // FUNCTION-OPTION-LIST -> FUNCTION-OPTION-LIST <COMMA-TOKEN> FUNCTION-OPTION *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 54),
                    make(<reduce>, on: <END-TOKEN>, production: 54),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 54));

*action-table*[111] :=
  // INTERFACE-CLAUSE -> <FUNCTION-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> * <IDENTIFIER-TOKEN> FUNCTION-OPTION-LIST
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 112));

*action-table*[112] :=
  // INTERFACE-CLAUSE -> <FUNCTION-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> * FUNCTION-OPTION-LIST
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 53),
                    make(<reduce>, on: <END-TOKEN>, production: 53),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 53));

*action-table*[113] :=
  // FUNCTION-OPTION-LIST -> FUNCTION-OPTION-LIST * <COMMA-TOKEN> FUNCTION-OPTION
  // INTERFACE-CLAUSE -> <FUNCTION-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> FUNCTION-OPTION-LIST *
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 82),
                    make(<reduce>, on: <END-TOKEN>, production: 52),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 52));

*action-table*[114] :=
  // INTERFACE-CLAUSE -> <STRUCT-TOKEN> * <STRING-LITERAL-TOKEN> STRUCTURE-OPTION-LIST
  // INTERFACE-CLAUSE -> <STRUCT-TOKEN> * <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> STRUCTURE-OPTION-LIST
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 115));

*action-table*[115] :=
  // INTERFACE-CLAUSE -> <STRUCT-TOKEN> <STRING-LITERAL-TOKEN> * <ARROW-TOKEN> <IDENTIFIER-TOKEN> STRUCTURE-OPTION-LIST
  // INTERFACE-CLAUSE -> <STRUCT-TOKEN> <STRING-LITERAL-TOKEN> * STRUCTURE-OPTION-LIST
  make-action-table(make(<shift>, on: <ARROW-TOKEN>, state: 127),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 70),
                    make(<reduce>, on: <END-TOKEN>, production: 70),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 70));

*action-table*[116] :=
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST * <COMMA-TOKEN> CONTAINER-OPTION
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST * <COMMA-TOKEN> SUPERCLASS-OPTION
  // INTERFACE-CLAUSE -> <STRUCT-TOKEN> <STRING-LITERAL-TOKEN> STRUCTURE-OPTION-LIST *
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 117),
                    make(<reduce>, on: <END-TOKEN>, production: 66),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 66));

*action-table*[117] :=
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST <COMMA-TOKEN> * SUPERCLASS-OPTION
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST <COMMA-TOKEN> * CONTAINER-OPTION
  make-action-table(make(<shift>, on: <EQUATE-TOKEN>, state: 65),
                    make(<shift>, on: <EXCLUDE-TOKEN>, state: 46),
                    make(<shift>, on: <IMPORT-TOKEN>, state: 32),
                    make(<shift>, on: <MAPPING-TOKEN>, state: 61),
                    make(<shift>, on: <NAME-MAPPER-TOKEN>, state: 30),
                    make(<shift>, on: <PREFIX-TOKEN>, state: 44),
                    make(<shift>, on: <READ-ONLY-TOKEN>, state: 74),
                    make(<shift>, on: <RENAME-TOKEN>, state: 53),
                    make(<shift>, on: <SEAL-FUNCTIONS-TOKEN>, state: 69),
                    make(<shift>, on: <SUPERCLASS-TOKEN>, state: 119));

*action-table*[118] :=
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST <COMMA-TOKEN> CONTAINER-OPTION *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 71),
                    make(<reduce>, on: <END-TOKEN>, production: 71),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 71));

*action-table*[119] :=
  // SUPERCLASS-OPTION -> <SUPERCLASS-TOKEN> * <LBRACE-TOKEN> SUPERCLASS-LIST <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <LBRACE-TOKEN>, state: 120));

*action-table*[120] :=
  // SUPERCLASS-OPTION -> <SUPERCLASS-TOKEN> <LBRACE-TOKEN> * SUPERCLASS-LIST <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 121));

*action-table*[121] :=
  // SUPERCLASS-LIST -> <IDENTIFIER-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 72),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 72));

*action-table*[122] :=
  // SUPERCLASS-LIST -> SUPERCLASS-LIST * <COMMA-TOKEN> <IDENTIFIER-TOKEN>
  // SUPERCLASS-OPTION -> <SUPERCLASS-TOKEN> <LBRACE-TOKEN> SUPERCLASS-LIST * <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 124),
                    make(<shift>, on: <RBRACE-TOKEN>, state: 123));

*action-table*[123] :=
  // SUPERCLASS-OPTION -> <SUPERCLASS-TOKEN> <LBRACE-TOKEN> SUPERCLASS-LIST <RBRACE-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 74),
                    make(<reduce>, on: <END-TOKEN>, production: 74),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 74));

*action-table*[124] :=
  // SUPERCLASS-LIST -> SUPERCLASS-LIST <COMMA-TOKEN> * <IDENTIFIER-TOKEN>
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 125));

*action-table*[125] :=
  // SUPERCLASS-LIST -> SUPERCLASS-LIST <COMMA-TOKEN> <IDENTIFIER-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 73),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 73));

*action-table*[126] :=
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST <COMMA-TOKEN> SUPERCLASS-OPTION *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 75),
                    make(<reduce>, on: <END-TOKEN>, production: 75),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 75));

*action-table*[127] :=
  // INTERFACE-CLAUSE -> <STRUCT-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> * <IDENTIFIER-TOKEN> STRUCTURE-OPTION-LIST
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 128));

*action-table*[128] :=
  // INTERFACE-CLAUSE -> <STRUCT-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> * STRUCTURE-OPTION-LIST
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 70),
                    make(<reduce>, on: <END-TOKEN>, production: 70),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 70));

*action-table*[129] :=
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST * <COMMA-TOKEN> CONTAINER-OPTION
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST * <COMMA-TOKEN> SUPERCLASS-OPTION
  // INTERFACE-CLAUSE -> <STRUCT-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> STRUCTURE-OPTION-LIST *
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 117),
                    make(<reduce>, on: <END-TOKEN>, production: 67),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 67));

*action-table*[130] :=
  // INTERFACE-CLAUSE -> <UNION-TOKEN> * <STRING-LITERAL-TOKEN> STRUCTURE-OPTION-LIST
  // INTERFACE-CLAUSE -> <UNION-TOKEN> * <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> STRUCTURE-OPTION-LIST
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 131));

*action-table*[131] :=
  // INTERFACE-CLAUSE -> <UNION-TOKEN> <STRING-LITERAL-TOKEN> * <ARROW-TOKEN> <IDENTIFIER-TOKEN> STRUCTURE-OPTION-LIST
  // INTERFACE-CLAUSE -> <UNION-TOKEN> <STRING-LITERAL-TOKEN> * STRUCTURE-OPTION-LIST
  make-action-table(make(<shift>, on: <ARROW-TOKEN>, state: 133),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 70),
                    make(<reduce>, on: <END-TOKEN>, production: 70),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 70));

*action-table*[132] :=
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST * <COMMA-TOKEN> CONTAINER-OPTION
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST * <COMMA-TOKEN> SUPERCLASS-OPTION
  // INTERFACE-CLAUSE -> <UNION-TOKEN> <STRING-LITERAL-TOKEN> STRUCTURE-OPTION-LIST *
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 117),
                    make(<reduce>, on: <END-TOKEN>, production: 68),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 68));

*action-table*[133] :=
  // INTERFACE-CLAUSE -> <UNION-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> * <IDENTIFIER-TOKEN> STRUCTURE-OPTION-LIST
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 134));

*action-table*[134] :=
  // INTERFACE-CLAUSE -> <UNION-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> * STRUCTURE-OPTION-LIST
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 70),
                    make(<reduce>, on: <END-TOKEN>, production: 70),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 70));

*action-table*[135] :=
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST * <COMMA-TOKEN> CONTAINER-OPTION
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST * <COMMA-TOKEN> SUPERCLASS-OPTION
  // INTERFACE-CLAUSE -> <UNION-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> STRUCTURE-OPTION-LIST *
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 117),
                    make(<reduce>, on: <END-TOKEN>, production: 69),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 69));

*action-table*[136] :=
  // INTERFACE-CLAUSE -> <POINTER-TOKEN> * <STRING-LITERAL-TOKEN> POINTER-OPTION-LIST
  // INTERFACE-CLAUSE -> <POINTER-TOKEN> * <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> POINTER-OPTION-LIST
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 137));

*action-table*[137] :=
  // INTERFACE-CLAUSE -> <POINTER-TOKEN> <STRING-LITERAL-TOKEN> * <ARROW-TOKEN> <IDENTIFIER-TOKEN> POINTER-OPTION-LIST
  // INTERFACE-CLAUSE -> <POINTER-TOKEN> <STRING-LITERAL-TOKEN> * POINTER-OPTION-LIST
  make-action-table(make(<shift>, on: <ARROW-TOKEN>, state: 142),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 70),
                    make(<reduce>, on: <END-TOKEN>, production: 78),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 78));

*action-table*[138] :=
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST * <COMMA-TOKEN> CONTAINER-OPTION
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST * <COMMA-TOKEN> SUPERCLASS-OPTION
  // POINTER-OPTION-LIST -> STRUCTURE-OPTION-LIST * <COMMA-TOKEN> SUPERCLASS-OPTION
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 139));

*action-table*[139] :=
  // POINTER-OPTION-LIST -> STRUCTURE-OPTION-LIST <COMMA-TOKEN> * SUPERCLASS-OPTION
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST <COMMA-TOKEN> * SUPERCLASS-OPTION
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST <COMMA-TOKEN> * CONTAINER-OPTION
  make-action-table(make(<shift>, on: <EQUATE-TOKEN>, state: 65),
                    make(<shift>, on: <EXCLUDE-TOKEN>, state: 46),
                    make(<shift>, on: <IMPORT-TOKEN>, state: 32),
                    make(<shift>, on: <MAPPING-TOKEN>, state: 61),
                    make(<shift>, on: <NAME-MAPPER-TOKEN>, state: 30),
                    make(<shift>, on: <PREFIX-TOKEN>, state: 44),
                    make(<shift>, on: <READ-ONLY-TOKEN>, state: 74),
                    make(<shift>, on: <RENAME-TOKEN>, state: 53),
                    make(<shift>, on: <SEAL-FUNCTIONS-TOKEN>, state: 69),
                    make(<shift>, on: <SUPERCLASS-TOKEN>, state: 119));

*action-table*[140] :=
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST <COMMA-TOKEN> SUPERCLASS-OPTION *
  // POINTER-OPTION-LIST -> STRUCTURE-OPTION-LIST <COMMA-TOKEN> SUPERCLASS-OPTION *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 75),
                    make(<reduce>, on: <END-TOKEN>, production: 79),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 79));

*action-table*[141] :=
  // INTERFACE-CLAUSE -> <POINTER-TOKEN> <STRING-LITERAL-TOKEN> POINTER-OPTION-LIST *
  make-action-table(make(<reduce>, on: <END-TOKEN>, production: 76),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 76));

*action-table*[142] :=
  // INTERFACE-CLAUSE -> <POINTER-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> * <IDENTIFIER-TOKEN> POINTER-OPTION-LIST
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 143));

*action-table*[143] :=
  // INTERFACE-CLAUSE -> <POINTER-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> * POINTER-OPTION-LIST
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 70),
                    make(<reduce>, on: <END-TOKEN>, production: 78),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 78));

*action-table*[144] :=
  // INTERFACE-CLAUSE -> <POINTER-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> POINTER-OPTION-LIST *
  make-action-table(make(<reduce>, on: <END-TOKEN>, production: 77),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 77));

*action-table*[145] :=
  // INTERFACE-CLAUSE -> <CONSTANT-TOKEN> * <STRING-LITERAL-TOKEN> CONSTANT-OPTION-LIST
  // INTERFACE-CLAUSE -> <CONSTANT-TOKEN> * <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> CONSTANT-OPTION-LIST
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 146));

*action-table*[146] :=
  // INTERFACE-CLAUSE -> <CONSTANT-TOKEN> <STRING-LITERAL-TOKEN> * <ARROW-TOKEN> <IDENTIFIER-TOKEN> CONSTANT-OPTION-LIST
  // INTERFACE-CLAUSE -> <CONSTANT-TOKEN> <STRING-LITERAL-TOKEN> * CONSTANT-OPTION-LIST
  make-action-table(make(<shift>, on: <ARROW-TOKEN>, state: 152),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 82),
                    make(<reduce>, on: <END-TOKEN>, production: 82),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 82));

*action-table*[147] :=
  // CONSTANT-OPTION-LIST -> CONSTANT-OPTION-LIST * <COMMA-TOKEN> CONSTANT-OPTION
  // INTERFACE-CLAUSE -> <CONSTANT-TOKEN> <STRING-LITERAL-TOKEN> CONSTANT-OPTION-LIST *
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 148),
                    make(<reduce>, on: <END-TOKEN>, production: 80),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 80));

*action-table*[148] :=
  // CONSTANT-OPTION-LIST -> CONSTANT-OPTION-LIST <COMMA-TOKEN> * CONSTANT-OPTION
  make-action-table(make(<shift>, on: <VALUE-TOKEN>, state: 149));

*action-table*[149] :=
  // CONSTANT-OPTION -> <VALUE-TOKEN> * <LITERAL-TOKEN>
  make-action-table(make(<shift>, on: <LITERAL-TOKEN>, state: 150));

*action-table*[150] :=
  // CONSTANT-OPTION -> <VALUE-TOKEN> <LITERAL-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 84),
                    make(<reduce>, on: <END-TOKEN>, production: 84),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 84));

*action-table*[151] :=
  // CONSTANT-OPTION-LIST -> CONSTANT-OPTION-LIST <COMMA-TOKEN> CONSTANT-OPTION *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 83),
                    make(<reduce>, on: <END-TOKEN>, production: 83),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 83));

*action-table*[152] :=
  // INTERFACE-CLAUSE -> <CONSTANT-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> * <IDENTIFIER-TOKEN> CONSTANT-OPTION-LIST
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 153));

*action-table*[153] :=
  // INTERFACE-CLAUSE -> <CONSTANT-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> * CONSTANT-OPTION-LIST
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 82),
                    make(<reduce>, on: <END-TOKEN>, production: 82),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 82));

*action-table*[154] :=
  // CONSTANT-OPTION-LIST -> CONSTANT-OPTION-LIST * <COMMA-TOKEN> CONSTANT-OPTION
  // INTERFACE-CLAUSE -> <CONSTANT-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> CONSTANT-OPTION-LIST *
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 148),
                    make(<reduce>, on: <END-TOKEN>, production: 81),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 81));

*action-table*[155] :=
  // INTERFACE-CLAUSE -> <VARIABLE-TOKEN> * <STRING-LITERAL-TOKEN> VARIABLE-OPTION-LIST
  // INTERFACE-CLAUSE -> <VARIABLE-TOKEN> * <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> VARIABLE-OPTION-LIST
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 156));

*action-table*[156] :=
  // INTERFACE-CLAUSE -> <VARIABLE-TOKEN> <STRING-LITERAL-TOKEN> * <ARROW-TOKEN> <IDENTIFIER-TOKEN> VARIABLE-OPTION-LIST
  // INTERFACE-CLAUSE -> <VARIABLE-TOKEN> <STRING-LITERAL-TOKEN> * VARIABLE-OPTION-LIST
  make-action-table(make(<shift>, on: <ARROW-TOKEN>, state: 171),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 87),
                    make(<reduce>, on: <END-TOKEN>, production: 87),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 87));

*action-table*[157] :=
  // VARIABLE-OPTION-LIST -> VARIABLE-OPTION-LIST * <COMMA-TOKEN> VARIABLE-OPTION
  // INTERFACE-CLAUSE -> <VARIABLE-TOKEN> <STRING-LITERAL-TOKEN> VARIABLE-OPTION-LIST *
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 158),
                    make(<reduce>, on: <END-TOKEN>, production: 85),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 85));

*action-table*[158] :=
  // VARIABLE-OPTION-LIST -> VARIABLE-OPTION-LIST <COMMA-TOKEN> * VARIABLE-OPTION
  make-action-table(make(<shift>, on: <EQUATE-TOKEN>, state: 165),
                    make(<shift>, on: <GETTER-TOKEN>, state: 167),
                    make(<shift>, on: <MAPPING-TOKEN>, state: 163),
                    make(<shift>, on: <READ-ONLY-TOKEN>, state: 161),
                    make(<shift>, on: <SETTER-TOKEN>, state: 159));

*action-table*[159] :=
  // VARIABLE-OPTION -> <SETTER-TOKEN> * <IDENTIFIER-TOKEN>
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 160));

*action-table*[160] :=
  // VARIABLE-OPTION -> <SETTER-TOKEN> <IDENTIFIER-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 90),
                    make(<reduce>, on: <END-TOKEN>, production: 90),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 90));

*action-table*[161] :=
  // VARIABLE-OPTION -> <READ-ONLY-TOKEN> * <BOOLEAN-TOKEN>
  make-action-table(make(<shift>, on: <BOOLEAN-TOKEN>, state: 162));

*action-table*[162] :=
  // VARIABLE-OPTION -> <READ-ONLY-TOKEN> <BOOLEAN-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 91),
                    make(<reduce>, on: <END-TOKEN>, production: 91),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 91));

*action-table*[163] :=
  // VARIABLE-OPTION -> <MAPPING-TOKEN> * <IDENTIFIER-TOKEN>
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 164));

*action-table*[164] :=
  // VARIABLE-OPTION -> <MAPPING-TOKEN> <IDENTIFIER-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 92),
                    make(<reduce>, on: <END-TOKEN>, production: 92),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 92));

*action-table*[165] :=
  // VARIABLE-OPTION -> <EQUATE-TOKEN> * <IDENTIFIER-TOKEN>
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 166));

*action-table*[166] :=
  // VARIABLE-OPTION -> <EQUATE-TOKEN> <IDENTIFIER-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 93),
                    make(<reduce>, on: <END-TOKEN>, production: 93),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 93));

*action-table*[167] :=
  // VARIABLE-OPTION -> <GETTER-TOKEN> * <IDENTIFIER-TOKEN>
  // VARIABLE-OPTION -> <GETTER-TOKEN> * SEALING
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 169),
                    make(<shift>, on: <INLINE-TOKEN>, state: 72),
                    make(<shift>, on: <OPEN-TOKEN>, state: 71),
                    make(<shift>, on: <SEALED-TOKEN>, state: 70));

*action-table*[168] :=
  // VARIABLE-OPTION -> <GETTER-TOKEN> SEALING *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 94),
                    make(<reduce>, on: <END-TOKEN>, production: 94),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 94));

*action-table*[169] :=
  // VARIABLE-OPTION -> <GETTER-TOKEN> <IDENTIFIER-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 89),
                    make(<reduce>, on: <END-TOKEN>, production: 89),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 89));

*action-table*[170] :=
  // VARIABLE-OPTION-LIST -> VARIABLE-OPTION-LIST <COMMA-TOKEN> VARIABLE-OPTION *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 88),
                    make(<reduce>, on: <END-TOKEN>, production: 88),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 88));

*action-table*[171] :=
  // INTERFACE-CLAUSE -> <VARIABLE-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> * <IDENTIFIER-TOKEN> VARIABLE-OPTION-LIST
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 172));

*action-table*[172] :=
  // INTERFACE-CLAUSE -> <VARIABLE-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> * VARIABLE-OPTION-LIST
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 87),
                    make(<reduce>, on: <END-TOKEN>, production: 87),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 87));

*action-table*[173] :=
  // VARIABLE-OPTION-LIST -> VARIABLE-OPTION-LIST * <COMMA-TOKEN> VARIABLE-OPTION
  // INTERFACE-CLAUSE -> <VARIABLE-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> VARIABLE-OPTION-LIST *
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 158),
                    make(<reduce>, on: <END-TOKEN>, production: 86),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 86));

*action-table*[174] :=
  // INTERFACE-CLAUSE-LIST1 -> INTERFACE-CLAUSE *
  make-action-table(make(<reduce>, on: <END-TOKEN>, production: 8),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 8));

*action-table*[175] :=
  // INTERFACE-CLAUSE-LIST -> INTERFACE-CLAUSE-LIST1 * <SEMICOLON-TOKEN>
  // INTERFACE-CLAUSE-LIST1 -> INTERFACE-CLAUSE-LIST1 * <SEMICOLON-TOKEN> INTERFACE-CLAUSE
  // INTERFACE-CLAUSE-LIST -> INTERFACE-CLAUSE-LIST1 *
  make-action-table(make(<reduce>, on: <END-TOKEN>, production: 6),
                    make(<shift>, on: <SEMICOLON-TOKEN>, state: 176));

*action-table*[176] :=
  // INTERFACE-CLAUSE-LIST1 -> INTERFACE-CLAUSE-LIST1 <SEMICOLON-TOKEN> * INTERFACE-CLAUSE
  // INTERFACE-CLAUSE-LIST -> INTERFACE-CLAUSE-LIST1 <SEMICOLON-TOKEN> *
  make-action-table(make(<shift>, on: <CONSTANT-TOKEN>, state: 145),
                    make(<reduce>, on: <END-TOKEN>, production: 5),
                    make(<shift>, on: <FUNCTION-TOKEN>, state: 79),
                    make(<shift>, on: <INCLUDE-TOKEN>, state: 3),
                    make(<shift>, on: <POINTER-TOKEN>, state: 136),
                    make(<shift>, on: <STRUCT-TOKEN>, state: 114),
                    make(<shift>, on: <UNION-TOKEN>, state: 130),
                    make(<shift>, on: <VARIABLE-TOKEN>, state: 155));

*action-table*[177] :=
  // INTERFACE-CLAUSE-LIST1 -> INTERFACE-CLAUSE-LIST1 <SEMICOLON-TOKEN> INTERFACE-CLAUSE *
  make-action-table(make(<reduce>, on: <END-TOKEN>, production: 7),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 7));

*action-table*[178] :=
  // INTERFACE-DEF -> <DEFINE-TOKEN> <INTERFACE-TOKEN> INTERFACE-CLAUSE-LIST * <END-TOKEN> <INTERFACE-TOKEN>
  // INTERFACE-DEF -> <DEFINE-TOKEN> <INTERFACE-TOKEN> INTERFACE-CLAUSE-LIST * <END-TOKEN>
  make-action-table(make(<shift>, on: <END-TOKEN>, state: 179));

*action-table*[179] :=
  // INTERFACE-DEF -> <DEFINE-TOKEN> <INTERFACE-TOKEN> INTERFACE-CLAUSE-LIST <END-TOKEN> *
  // INTERFACE-DEF -> <DEFINE-TOKEN> <INTERFACE-TOKEN> INTERFACE-CLAUSE-LIST <END-TOKEN> * <INTERFACE-TOKEN>
  make-action-table(make(<shift>, on: <INTERFACE-TOKEN>, state: 180),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 4),
                    make(<reduce>, on: <TRUE-EOF-TOKEN>, production: 4));

*action-table*[180] :=
  // INTERFACE-DEF -> <DEFINE-TOKEN> <INTERFACE-TOKEN> INTERFACE-CLAUSE-LIST <END-TOKEN> <INTERFACE-TOKEN> *
  make-action-table(make(<reduce>, on: <SEMICOLON-TOKEN>, production: 3),
                    make(<reduce>, on: <TRUE-EOF-TOKEN>, production: 3));

*action-table*[181] :=
  // PARSE-ROOT -> INTERFACE-DEF * <SEMICOLON-TOKEN>
  // PARSE-ROOT -> INTERFACE-DEF * <TRUE-EOF-TOKEN>
  make-action-table(make(<shift>, on: <SEMICOLON-TOKEN>, state: 183),
                    make(<shift>, on: <TRUE-EOF-TOKEN>, state: 182));

*action-table*[182] :=
  // PARSE-ROOT -> INTERFACE-DEF <TRUE-EOF-TOKEN> *
  make-action-table(make(<reduce>, on: <EOF-TOKEN>, production: 2));

*action-table*[183] :=
  // PARSE-ROOT -> INTERFACE-DEF <SEMICOLON-TOKEN> *
  make-action-table(make(<reduce>, on: <EOF-TOKEN>, production: 1));

*action-table*[184] :=
  // S-PRIME -> PARSE-ROOT *
  make-action-table(make(<accept>, on: <EOF-TOKEN>));

*production-table*[1] :=
  method (state-stack, symbol-stack, #key $state)
    // PARSE-ROOT -> INTERFACE-DEF <SEMICOLON-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(184,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      // dispose of semi if it exists, but accept EOF instead
                      #t;
                  end,
                  temp1);
           end);
  end;

*production-table*[2] :=
  method (state-stack, symbol-stack, #key $state)
    // PARSE-ROOT -> INTERFACE-DEF <TRUE-EOF-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(184,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      #t;
                  end,
                  temp1);
           end);
  end;

*production-table*[3] :=
  method (state-stack, symbol-stack, #key $state)
    // INTERFACE-DEF -> <DEFINE-TOKEN> <INTERFACE-TOKEN> INTERFACE-CLAUSE-LIST <END-TOKEN> <INTERFACE-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(tail(state-stack)))));
             pair(181,
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
                  end,
                  temp1);
           end);
  end;

*production-table*[4] :=
  method (state-stack, symbol-stack, #key $state)
    // INTERFACE-DEF -> <DEFINE-TOKEN> <INTERFACE-TOKEN> INTERFACE-CLAUSE-LIST <END-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(state-stack))));
             pair(181,
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
                  end,
                  temp1);
           end);
  end;

*production-table*[5] :=
  method (state-stack, symbol-stack, #key $state)
    // INTERFACE-CLAUSE-LIST -> INTERFACE-CLAUSE-LIST1 <SEMICOLON-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(178,
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

*production-table*[6] :=
  method (state-stack, symbol-stack, #key $state)
    // INTERFACE-CLAUSE-LIST -> INTERFACE-CLAUSE-LIST1
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(178,
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

*production-table*[7] :=
  method (state-stack, symbol-stack, #key $state)
    // INTERFACE-CLAUSE-LIST1 -> INTERFACE-CLAUSE-LIST1 <SEMICOLON-TOKEN> INTERFACE-CLAUSE
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(175,
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
                      pair($r3, $r1)
                  end,
                  temp1);
           end);
  end;

*production-table*[8] :=
  method (state-stack, symbol-stack, #key $state)
    // INTERFACE-CLAUSE-LIST1 -> INTERFACE-CLAUSE
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(175,
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

*production-table*[9] :=
  method (state-stack, symbol-stack, #key $state)
    // INTERFACE-CLAUSE -> FILE-CLAUSE
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    2 => 174;
                    OTHERWISE => 177;
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

*production-table*[10] :=
  method (state-stack, symbol-stack, #key $state)
    // FILE-CLAUSE -> <INCLUDE-TOKEN> <STRING-LITERAL-TOKEN> FILE-OPTION-LIST
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(78,
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
                      if ($state.include-file) 
                        parse-error($state.tokenizer,
                                    "More than one #include in interface definition.")
                      end if;
                      $state.include-file := $r2.value;
                      $state.container-options := process-container-options($r3);
                  end,
                  temp1);
           end);
  end;

*production-table*[11] :=
  method (state-stack, symbol-stack, #key $state)
    // FILE-OPTION-LIST -> epsilon
    values(begin
             let poped-state-stack = state-stack;
             pair(5,
                  poped-state-stack);
           end,
           begin
             pair(begin
                      #();
                  end,
                  symbol-stack);
           end);
  end;

*production-table*[12] :=
  method (state-stack, symbol-stack, #key $state)
    // FILE-OPTION-LIST -> FILE-OPTION-LIST <COMMA-TOKEN> FILE-OPTION
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(5,
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

*production-table*[13] :=
  method (state-stack, symbol-stack, #key $state)
    // FILE-OPTION -> <OBJECT-FILE-TOKEN> <STRING-LITERAL-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(77,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      push-last($state.object-files, $r2.value);
                      pair(#"object-file", $r2.value);
                  end,
                  temp1);
           end);
  end;

*production-table*[14] :=
  method (state-stack, symbol-stack, #key $state)
    // FILE-OPTION -> <MINDY-INC-TOKEN> <STRING-LITERAL-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(77,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      if ($state.mindy-include-file) 
                        parse-error($state.tokenizer,
                                    "More than one mindy-include-file: in interface definition.")
                      end if;
                      pair(#"mindy-file", $state.mindy-include-file := $r2.value);
                  end,
                  temp1);
           end);
  end;

*production-table*[15] :=
  method (state-stack, symbol-stack, #key $state)
    // FILE-OPTION -> <DEFINE-MACRO-TOKEN> <LBRACE-TOKEN> MACRO-DEFINITIONS <RBRACE-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(state-stack))));
             pair(77,
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
                      pair(#"macro-definitions", $r3);
                  end,
                  temp1);
           end);
  end;

*production-table*[16] :=
  method (state-stack, symbol-stack, #key $state)
    // FILE-OPTION -> <UNDEFINE-TOKEN> <LBRACE-TOKEN> MACRO-UNDEFINITIONS <RBRACE-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(state-stack))));
             pair(77,
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
                      pair(#"macro-undefines", $r3);
                  end,
                  temp1);
           end);
  end;

*production-table*[17] :=
  method (state-stack, symbol-stack, #key $state)
    // FILE-OPTION -> CONTAINER-OPTION
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(77,
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

*production-table*[18] :=
  method (state-stack, symbol-stack, #key $state)
    // MACRO-DEFINITIONS -> MACRO-DEFINITION
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(18,
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

*production-table*[19] :=
  method (state-stack, symbol-stack, #key $state)
    // MACRO-DEFINITIONS -> MACRO-DEFINITIONS <COMMA-TOKEN> MACRO-DEFINITION
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(18,
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

*production-table*[20] :=
  method (state-stack, symbol-stack, #key $state)
    // MACRO-DEFINITION -> <STRING-LITERAL-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    12 => 17;
                    OTHERWISE => 21;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      push-last($state.macro-defines, pair($r1.value, 1));
                  end,
                  temp1);
           end);
  end;

*production-table*[21] :=
  method (state-stack, symbol-stack, #key $state)
    // MACRO-DEFINITION -> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <STRING-LITERAL-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    12 => 17;
                    OTHERWISE => 21;
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
                      push-last($state.macro-defines, pair($r1.value, $r3.value));
                  end,
                  temp1);
           end);
  end;

*production-table*[22] :=
  method (state-stack, symbol-stack, #key $state)
    // MACRO-DEFINITION -> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <INTEGER-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    12 => 17;
                    OTHERWISE => 21;
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
                      push-last($state.macro-defines, pair($r1.value, $r3.value));
                  end,
                  temp1);
           end);
  end;

*production-table*[23] :=
  method (state-stack, symbol-stack, #key $state)
    // MACRO-UNDEFINITIONS -> MACRO-UNDEFINITION
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(26,
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

*production-table*[24] :=
  method (state-stack, symbol-stack, #key $state)
    // MACRO-UNDEFINITIONS -> MACRO-UNDEFINITIONS <COMMA-TOKEN> MACRO-UNDEFINITION
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(26,
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

*production-table*[25] :=
  method (state-stack, symbol-stack, #key $state)
    // MACRO-UNDEFINITION -> <STRING-LITERAL-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    23 => 25;
                    OTHERWISE => 29;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      push-last($state.macro-undefines, $r1.value);
                  end,
                  temp1);
           end);
  end;

*production-table*[26] :=
  method (state-stack, symbol-stack, #key $state)
    // CONTAINER-OPTION -> <NAME-MAPPER-TOKEN> <IDENTIFIER-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(select (head(poped-state-stack))
                    6 => 76;
                    OTHERWISE => 118;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(#"name-mapper", $r2.value);
                  end,
                  temp1);
           end);
  end;

*production-table*[27] :=
  method (state-stack, symbol-stack, #key $state)
    // CONTAINER-OPTION -> <IMPORT-TOKEN> <ALL-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(select (head(poped-state-stack))
                    6 => 76;
                    OTHERWISE => 118;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(#"import", #t);
                  end,
                  temp1);
           end);
  end;

*production-table*[28] :=
  method (state-stack, symbol-stack, #key $state)
    // CONTAINER-OPTION -> <IMPORT-TOKEN> <LBRACE-TOKEN> IMPORT-LIST <RBRACE-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(state-stack))));
             pair(select (head(poped-state-stack))
                    6 => 76;
                    OTHERWISE => 118;
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
                      pair(#"import", $r3);
                  end,
                  temp1);
           end);
  end;

*production-table*[29] :=
  method (state-stack, symbol-stack, #key $state)
    // IMPORT-LIST -> epsilon
    values(begin
             let poped-state-stack = state-stack;
             pair(39,
                  poped-state-stack);
           end,
           begin
             pair(begin
                      #();
                  end,
                  symbol-stack);
           end);
  end;

*production-table*[30] :=
  method (state-stack, symbol-stack, #key $state)
    // IMPORT-LIST -> IMPORT-LIST <COMMA-TOKEN> IMPORT
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(39,
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

*production-table*[31] :=
  method (state-stack, symbol-stack, #key $state)
    // IMPORT-LIST -> IMPORT
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(39,
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

*production-table*[32] :=
  method (state-stack, symbol-stack, #key $state)
    // IMPORT -> <STRING-LITERAL-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    33 => 38;
                    OTHERWISE => 42;
                  end,
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

*production-table*[33] :=
  method (state-stack, symbol-stack, #key $state)
    // IMPORT -> RENAMING
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    33 => 38;
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

*production-table*[34] :=
  method (state-stack, symbol-stack, #key $state)
    // RENAMING -> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    33 => 37;
                    41 => 37;
                    59 => 60;
                    OTHERWISE => 56;
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
                      pair($r1.value, $r3.value);
                  end,
                  temp1);
           end);
  end;

*production-table*[35] :=
  method (state-stack, symbol-stack, #key $state)
    // CONTAINER-OPTION -> <PREFIX-TOKEN> <STRING-LITERAL-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(select (head(poped-state-stack))
                    6 => 76;
                    OTHERWISE => 118;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(#"prefix", $r2.value);
                  end,
                  temp1);
           end);
  end;

*production-table*[36] :=
  method (state-stack, symbol-stack, #key $state)
    // CONTAINER-OPTION -> <EXCLUDE-TOKEN> <LBRACE-TOKEN> EXCLUDE-LIST <RBRACE-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(state-stack))));
             pair(select (head(poped-state-stack))
                    6 => 76;
                    OTHERWISE => 118;
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
                      pair(#"exclude", $r3);
                  end,
                  temp1);
           end);
  end;

*production-table*[37] :=
  method (state-stack, symbol-stack, #key $state)
    // EXCLUDE-LIST -> epsilon
    values(begin
             let poped-state-stack = state-stack;
             pair(49,
                  poped-state-stack);
           end,
           begin
             pair(begin
                      #();
                  end,
                  symbol-stack);
           end);
  end;

*production-table*[38] :=
  method (state-stack, symbol-stack, #key $state)
    // EXCLUDE-LIST -> EXCLUDE-LIST <COMMA-TOKEN> <STRING-LITERAL-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(49,
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
                      pair($r3.value, $r1);
                  end,
                  temp1);
           end);
  end;

*production-table*[39] :=
  method (state-stack, symbol-stack, #key $state)
    // EXCLUDE-LIST -> <STRING-LITERAL-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(49,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      list($r1.value);
                  end,
                  temp1);
           end);
  end;

*production-table*[40] :=
  method (state-stack, symbol-stack, #key $state)
    // CONTAINER-OPTION -> <RENAME-TOKEN> <LBRACE-TOKEN> RENAMING-LIST <RBRACE-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(state-stack))));
             pair(select (head(poped-state-stack))
                    6 => 76;
                    OTHERWISE => 118;
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
                      pair(#"rename", $r3);
                  end,
                  temp1);
           end);
  end;

*production-table*[41] :=
  method (state-stack, symbol-stack, #key $state)
    // RENAMING-LIST -> epsilon
    values(begin
             let poped-state-stack = state-stack;
             pair(select (head(poped-state-stack))
                    54 => 57;
                    62 => 63;
                    OTHERWISE => 67;
                  end,
                  poped-state-stack);
           end,
           begin
             pair(begin
                      #();
                  end,
                  symbol-stack);
           end);
  end;

*production-table*[42] :=
  method (state-stack, symbol-stack, #key $state)
    // RENAMING-LIST -> RENAMING-LIST <COMMA-TOKEN> RENAMING
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    54 => 57;
                    62 => 63;
                    OTHERWISE => 67;
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
                      pair($r3, $r1);
                  end,
                  temp1);
           end);
  end;

*production-table*[43] :=
  method (state-stack, symbol-stack, #key $state)
    // RENAMING-LIST -> RENAMING
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    54 => 57;
                    62 => 63;
                    OTHERWISE => 67;
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

*production-table*[44] :=
  method (state-stack, symbol-stack, #key $state)
    // CONTAINER-OPTION -> <MAPPING-TOKEN> <LBRACE-TOKEN> RENAMING-LIST <RBRACE-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(state-stack))));
             pair(select (head(poped-state-stack))
                    6 => 76;
                    OTHERWISE => 118;
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
                      pair(#"mapping", $r3);
                  end,
                  temp1);
           end);
  end;

*production-table*[45] :=
  method (state-stack, symbol-stack, #key $state)
    // CONTAINER-OPTION -> <EQUATE-TOKEN> <LBRACE-TOKEN> RENAMING-LIST <RBRACE-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(state-stack))));
             pair(select (head(poped-state-stack))
                    6 => 76;
                    OTHERWISE => 118;
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
                      pair(#"equate", $r3);
                  end,
                  temp1);
           end);
  end;

*production-table*[46] :=
  method (state-stack, symbol-stack, #key $state)
    // SEALING -> <SEALED-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    69 => 73;
                    OTHERWISE => 168;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1.string-value
                  end,
                  temp1);
           end);
  end;

*production-table*[47] :=
  method (state-stack, symbol-stack, #key $state)
    // SEALING -> <OPEN-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    69 => 73;
                    OTHERWISE => 168;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      $r1.string-value
                  end,
                  temp1);
           end);
  end;

*production-table*[48] :=
  method (state-stack, symbol-stack, #key $state)
    // SEALING -> <INLINE-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    69 => 73;
                    OTHERWISE => 168;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      parse-error($r1, "'inline' not handled at present.")
                  end,
                  temp1);
           end);
  end;

*production-table*[49] :=
  method (state-stack, symbol-stack, #key $state)
    // CONTAINER-OPTION -> <SEAL-FUNCTIONS-TOKEN> SEALING
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(select (head(poped-state-stack))
                    6 => 76;
                    OTHERWISE => 118;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(#"seal", $r2);
                  end,
                  temp1);
           end);
  end;

*production-table*[50] :=
  method (state-stack, symbol-stack, #key $state)
    // CONTAINER-OPTION -> <READ-ONLY-TOKEN> <BOOLEAN-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(select (head(poped-state-stack))
                    6 => 76;
                    OTHERWISE => 118;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(#"read-only", $r2.value);
                  end,
                  temp1);
           end);
  end;

*production-table*[51] :=
  method (state-stack, symbol-stack, #key $state)
    // INTERFACE-CLAUSE -> <FUNCTION-TOKEN> <STRING-LITERAL-TOKEN> FUNCTION-OPTION-LIST
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    2 => 174;
                    OTHERWISE => 177;
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
                      $state.clauses := add!($state.clauses,
                                             make(<function-clause>, 
                                                  name: $r2.value, options: $r3));
                  end,
                  temp1);
           end);
  end;

*production-table*[52] :=
  method (state-stack, symbol-stack, #key $state)
    // INTERFACE-CLAUSE -> <FUNCTION-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> FUNCTION-OPTION-LIST
    values(begin
             let poped-state-stack = tail(tail(tail(tail(tail(state-stack)))));
             pair(select (head(poped-state-stack))
                    2 => 174;
                    OTHERWISE => 177;
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
                      $state.container-options.rename :=
                         pair(pair($r2.value, $r4.value), $state.container-options.rename);
                      $state.clauses := add!($state.clauses,
                                             make(<function-clause>,
                                                  name: $r2.value, options: $r5));
                  end,
                  temp1);
           end);
  end;

*production-table*[53] :=
  method (state-stack, symbol-stack, #key $state)
    // FUNCTION-OPTION-LIST -> epsilon
    values(begin
             let poped-state-stack = state-stack;
             pair(select (head(poped-state-stack))
                    80 => 81;
                    OTHERWISE => 113;
                  end,
                  poped-state-stack);
           end,
           begin
             pair(begin
                      #();
                  end,
                  symbol-stack);
           end);
  end;

*production-table*[54] :=
  method (state-stack, symbol-stack, #key $state)
    // FUNCTION-OPTION-LIST -> FUNCTION-OPTION-LIST <COMMA-TOKEN> FUNCTION-OPTION
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    80 => 81;
                    OTHERWISE => 113;
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
                      pair($r3, $r1);
                  end,
                  temp1);
           end);
  end;

*production-table*[55] :=
  method (state-stack, symbol-stack, #key $state)
    // FUNCTION-OPTION -> <MAP-RESULT-TOKEN> <IDENTIFIER-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(110,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(#"map-result", $r2.string-value);
                  end,
                  temp1);
           end);
  end;

*production-table*[56] :=
  method (state-stack, symbol-stack, #key $state)
    // FUNCTION-OPTION -> <EQUATE-RESULT-TOKEN> <IDENTIFIER-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(110,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(#"equate-result", $r2.string-value);
                  end,
                  temp1);
           end);
  end;

*production-table*[57] :=
  method (state-stack, symbol-stack, #key $state)
    // FUNCTION-OPTION -> <IGNORE-RESULT-TOKEN> <BOOLEAN-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(110,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(#"ignore-result", $r2.value);
                  end,
                  temp1);
           end);
  end;

*production-table*[58] :=
  method (state-stack, symbol-stack, #key $state)
    // FUNCTION-OPTION -> <MAP-ARGUMENT-TOKEN> <LBRACE-TOKEN> ARGUMENT <ARROW-TOKEN> <IDENTIFIER-TOKEN> <RBRACE-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(tail(tail(state-stack))))));
             pair(110,
                  poped-state-stack);
           end,
           begin
             let $r6 = head(symbol-stack);
             let temp6 = tail(symbol-stack);
             let $r5 = head(temp6);
             let temp5 = tail(temp6);
             let $r4 = head(temp5);
             let temp4 = tail(temp5);
             let $r3 = head(temp4);
             let temp3 = tail(temp4);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(#"map-arg", pair($r3.value, $r5.string-value));
                  end,
                  temp1);
           end);
  end;

*production-table*[59] :=
  method (state-stack, symbol-stack, #key $state)
    // FUNCTION-OPTION -> <EQUATE-ARGUMENT-TOKEN> <LBRACE-TOKEN> ARGUMENT <ARROW-TOKEN> <IDENTIFIER-TOKEN> <RBRACE-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(tail(tail(state-stack))))));
             pair(110,
                  poped-state-stack);
           end,
           begin
             let $r6 = head(symbol-stack);
             let temp6 = tail(symbol-stack);
             let $r5 = head(temp6);
             let temp5 = tail(temp6);
             let $r4 = head(temp5);
             let temp4 = tail(temp5);
             let $r3 = head(temp4);
             let temp3 = tail(temp4);
             let $r2 = head(temp3);
             let temp2 = tail(temp3);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(#"equate-arg", pair($r3.value, $r5.string-value));
                  end,
                  temp1);
           end);
  end;

*production-table*[60] :=
  method (state-stack, symbol-stack, #key $state)
    // FUNCTION-OPTION -> <INPUT-ARGUMENT-TOKEN> ARGUMENT
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(110,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(#"in", $r2.value);
                  end,
                  temp1);
           end);
  end;

*production-table*[61] :=
  method (state-stack, symbol-stack, #key $state)
    // FUNCTION-OPTION -> <INPUT-OUTPUT-ARGUMENT-TOKEN> ARGUMENT
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(110,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(#"in-out", $r2.value);
                  end,
                  temp1);
           end);
  end;

*production-table*[62] :=
  method (state-stack, symbol-stack, #key $state)
    // FUNCTION-OPTION -> <OUTPUT-ARGUMENT-TOKEN> ARGUMENT
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(110,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(#"out", $r2.value);
                  end,
                  temp1);
           end);
  end;

*production-table*[63] :=
  method (state-stack, symbol-stack, #key $state)
    // ARGUMENT -> <INTEGER-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    90 => 94;
                    99 => 100;
                    104 => 105;
                    106 => 107;
                    OTHERWISE => 109;
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

*production-table*[64] :=
  method (state-stack, symbol-stack, #key $state)
    // ARGUMENT -> <IDENTIFIER-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    90 => 94;
                    99 => 100;
                    104 => 105;
                    106 => 107;
                    OTHERWISE => 109;
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

*production-table*[65] :=
  method (state-stack, symbol-stack, #key $state)
    // ARGUMENT -> <STRING-LITERAL-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    90 => 94;
                    99 => 100;
                    104 => 105;
                    106 => 107;
                    OTHERWISE => 109;
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
    // INTERFACE-CLAUSE -> <STRUCT-TOKEN> <STRING-LITERAL-TOKEN> STRUCTURE-OPTION-LIST
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    2 => 174;
                    OTHERWISE => 177;
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
                      let (container-opts, rest) = process-container-options($r3);
                      let new-clause = make(<struct-clause>, name: $r2.value, options: rest);
                      new-clause.container-options := container-opts;
                      $state.clauses := add!($state.clauses, new-clause);
                  end,
                  temp1);
           end);
  end;

*production-table*[67] :=
  method (state-stack, symbol-stack, #key $state)
    // INTERFACE-CLAUSE -> <STRUCT-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> STRUCTURE-OPTION-LIST
    values(begin
             let poped-state-stack = tail(tail(tail(tail(tail(state-stack)))));
             pair(select (head(poped-state-stack))
                    2 => 174;
                    OTHERWISE => 177;
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
                      $state.container-options.rename :=
                         pair(pair($r2.value, $r4.value), $state.container-options.rename);
                      let (container-opts, rest) = process-container-options($r5);
                      let new-clause = make(<struct-clause>, name: $r2.value, options: rest);
                      new-clause.container-options := container-opts;
                      $state.clauses := add!($state.clauses, new-clause);
                  end,
                  temp1);
           end);
  end;

*production-table*[68] :=
  method (state-stack, symbol-stack, #key $state)
    // INTERFACE-CLAUSE -> <UNION-TOKEN> <STRING-LITERAL-TOKEN> STRUCTURE-OPTION-LIST
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    2 => 174;
                    OTHERWISE => 177;
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
                      let (container-opts, rest) = process-container-options($r3);
                      let new-clause = make(<union-clause>, name: $r2.value, options: rest);
                      new-clause.container-options := container-opts;
                      $state.clauses := add!($state.clauses, new-clause);
                  end,
                  temp1);
           end);
  end;

*production-table*[69] :=
  method (state-stack, symbol-stack, #key $state)
    // INTERFACE-CLAUSE -> <UNION-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> STRUCTURE-OPTION-LIST
    values(begin
             let poped-state-stack = tail(tail(tail(tail(tail(state-stack)))));
             pair(select (head(poped-state-stack))
                    2 => 174;
                    OTHERWISE => 177;
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
                      $state.container-options.rename :=
                         pair(pair($r2.value, $r4.value), $state.container-options.rename);
                      let (container-opts, rest) = process-container-options($r5);
                      let new-clause = make(<union-clause>, name: $r2.value, options: rest);
                      new-clause.container-options := container-opts;
                      $state.clauses := add!($state.clauses, new-clause);
                  end,
                  temp1);
           end);
  end;

*production-table*[70] :=
  method (state-stack, symbol-stack, #key $state)
    // STRUCTURE-OPTION-LIST -> epsilon
    values(begin
             let poped-state-stack = state-stack;
             pair(select (head(poped-state-stack))
                    115 => 116;
                    128 => 129;
                    131 => 132;
                    134 => 135;
                    OTHERWISE => 138;
                  end,
                  poped-state-stack);
           end,
           begin
             pair(begin
                      #();
                  end,
                  symbol-stack);
           end);
  end;

*production-table*[71] :=
  method (state-stack, symbol-stack, #key $state)
    // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST <COMMA-TOKEN> CONTAINER-OPTION
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    115 => 116;
                    128 => 129;
                    131 => 132;
                    134 => 135;
                    OTHERWISE => 138;
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
                      pair($r3, $r1);
                  end,
                  temp1);
           end);
  end;

*production-table*[72] :=
  method (state-stack, symbol-stack, #key $state)
    // SUPERCLASS-LIST -> <IDENTIFIER-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(122,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      list($r1.string-value);
                  end,
                  temp1);
           end);
  end;

*production-table*[73] :=
  method (state-stack, symbol-stack, #key $state)
    // SUPERCLASS-LIST -> SUPERCLASS-LIST <COMMA-TOKEN> <IDENTIFIER-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(122,
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
                      pair($r3.string-value, $r1);
                  end,
                  temp1);
           end);
  end;

*production-table*[74] :=
  method (state-stack, symbol-stack, #key $state)
    // SUPERCLASS-OPTION -> <SUPERCLASS-TOKEN> <LBRACE-TOKEN> SUPERCLASS-LIST <RBRACE-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(state-stack))));
             pair(select (head(poped-state-stack))
                    117 => 126;
                    OTHERWISE => 140;
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
                      pair(#"superclass", reverse!($r3));
                  end,
                  temp1);
           end);
  end;

*production-table*[75] :=
  method (state-stack, symbol-stack, #key $state)
    // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST <COMMA-TOKEN> SUPERCLASS-OPTION
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    115 => 116;
                    128 => 129;
                    131 => 132;
                    134 => 135;
                    OTHERWISE => 138;
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
                      pair($r3, $r1);
                  end,
                  temp1);
           end);
  end;

*production-table*[76] :=
  method (state-stack, symbol-stack, #key $state)
    // INTERFACE-CLAUSE -> <POINTER-TOKEN> <STRING-LITERAL-TOKEN> POINTER-OPTION-LIST
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    2 => 174;
                    OTHERWISE => 177;
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
                      let new-clause = make(<pointer-clause>, name: $r2.value, options: $r3);
                      $state.clauses := add!($state.clauses, new-clause);
                  end,
                  temp1);
           end);
  end;

*production-table*[77] :=
  method (state-stack, symbol-stack, #key $state)
    // INTERFACE-CLAUSE -> <POINTER-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> POINTER-OPTION-LIST
    values(begin
             let poped-state-stack = tail(tail(tail(tail(tail(state-stack)))));
             pair(select (head(poped-state-stack))
                    2 => 174;
                    OTHERWISE => 177;
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
                      $state.container-options.rename :=
                         pair(pair($r2.value, $r4.value), $state.container-options.rename);
                      let new-clause = make(<pointer-clause>, name: $r2.value, options: $r5);
                      $state.clauses := add!($state.clauses, new-clause);
                  end,
                  temp1);
           end);
  end;

*production-table*[78] :=
  method (state-stack, symbol-stack, #key $state)
    // POINTER-OPTION-LIST -> epsilon
    values(begin
             let poped-state-stack = state-stack;
             pair(select (head(poped-state-stack))
                    137 => 141;
                    OTHERWISE => 144;
                  end,
                  poped-state-stack);
           end,
           begin
             pair(begin
                      #();
                  end,
                  symbol-stack);
           end);
  end;

*production-table*[79] :=
  method (state-stack, symbol-stack, #key $state)
    // POINTER-OPTION-LIST -> STRUCTURE-OPTION-LIST <COMMA-TOKEN> SUPERCLASS-OPTION
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    137 => 141;
                    OTHERWISE => 144;
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
                      pair($r3, $r1);
                  end,
                  temp1);
           end);
  end;

*production-table*[80] :=
  method (state-stack, symbol-stack, #key $state)
    // INTERFACE-CLAUSE -> <CONSTANT-TOKEN> <STRING-LITERAL-TOKEN> CONSTANT-OPTION-LIST
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    2 => 174;
                    OTHERWISE => 177;
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
                      $state.clauses := add!($state.clauses,
                                             make(<constant-clause>, 
                                                  name: $r2.value, options: $r3));
                  end,
                  temp1);
           end);
  end;

*production-table*[81] :=
  method (state-stack, symbol-stack, #key $state)
    // INTERFACE-CLAUSE -> <CONSTANT-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> CONSTANT-OPTION-LIST
    values(begin
             let poped-state-stack = tail(tail(tail(tail(tail(state-stack)))));
             pair(select (head(poped-state-stack))
                    2 => 174;
                    OTHERWISE => 177;
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
                      $state.container-options.rename :=
                         pair(pair($r2.value, $r4.value), $state.container-options.rename);
                      $state.clauses := add!($state.clauses,
                                             make(<constant-clause>,
                                                  name: $r2.value, options: $r5));
                  end,
                  temp1);
           end);
  end;

*production-table*[82] :=
  method (state-stack, symbol-stack, #key $state)
    // CONSTANT-OPTION-LIST -> epsilon
    values(begin
             let poped-state-stack = state-stack;
             pair(select (head(poped-state-stack))
                    146 => 147;
                    OTHERWISE => 154;
                  end,
                  poped-state-stack);
           end,
           begin
             pair(begin
                      #();
                  end,
                  symbol-stack);
           end);
  end;

*production-table*[83] :=
  method (state-stack, symbol-stack, #key $state)
    // CONSTANT-OPTION-LIST -> CONSTANT-OPTION-LIST <COMMA-TOKEN> CONSTANT-OPTION
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    146 => 147;
                    OTHERWISE => 154;
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
                      pair($r3, $r1);
                  end,
                  temp1);
           end);
  end;

*production-table*[84] :=
  method (state-stack, symbol-stack, #key $state)
    // CONSTANT-OPTION -> <VALUE-TOKEN> <LITERAL-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(151,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(#"value", $r2.string-value);
                  end,
                  temp1);
           end);
  end;

*production-table*[85] :=
  method (state-stack, symbol-stack, #key $state)
    // INTERFACE-CLAUSE -> <VARIABLE-TOKEN> <STRING-LITERAL-TOKEN> VARIABLE-OPTION-LIST
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    2 => 174;
                    OTHERWISE => 177;
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
                      $state.clauses := add!($state.clauses,
                                             make(<variable-clause>, 
                                                  name: $r2.value, options: $r3));
                  end,
                  temp1);
           end);
  end;

*production-table*[86] :=
  method (state-stack, symbol-stack, #key $state)
    // INTERFACE-CLAUSE -> <VARIABLE-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> VARIABLE-OPTION-LIST
    values(begin
             let poped-state-stack = tail(tail(tail(tail(tail(state-stack)))));
             pair(select (head(poped-state-stack))
                    2 => 174;
                    OTHERWISE => 177;
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
                      $state.container-options.rename :=
                         pair(pair($r2.value, $r4.value), $state.container-options.rename);
                      $state.clauses := add!($state.clauses,
                                             make(<variable-clause>,
                                                  name: $r2.value, options: $r5));
                  end,
                  temp1);
           end);
  end;

*production-table*[87] :=
  method (state-stack, symbol-stack, #key $state)
    // VARIABLE-OPTION-LIST -> epsilon
    values(begin
             let poped-state-stack = state-stack;
             pair(select (head(poped-state-stack))
                    156 => 157;
                    OTHERWISE => 173;
                  end,
                  poped-state-stack);
           end,
           begin
             pair(begin
                      #();
                  end,
                  symbol-stack);
           end);
  end;

*production-table*[88] :=
  method (state-stack, symbol-stack, #key $state)
    // VARIABLE-OPTION-LIST -> VARIABLE-OPTION-LIST <COMMA-TOKEN> VARIABLE-OPTION
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    156 => 157;
                    OTHERWISE => 173;
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
                      pair($r3, $r1);
                  end,
                  temp1);
           end);
  end;

*production-table*[89] :=
  method (state-stack, symbol-stack, #key $state)
    // VARIABLE-OPTION -> <GETTER-TOKEN> <IDENTIFIER-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(170,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(#"getter", $r2.string-value);
                  end,
                  temp1);
           end);
  end;

*production-table*[90] :=
  method (state-stack, symbol-stack, #key $state)
    // VARIABLE-OPTION -> <SETTER-TOKEN> <IDENTIFIER-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(170,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(#"setter", $r2.string-value);
                  end,
                  temp1);
           end);
  end;

*production-table*[91] :=
  method (state-stack, symbol-stack, #key $state)
    // VARIABLE-OPTION -> <READ-ONLY-TOKEN> <BOOLEAN-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(170,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(#"read-only", $r2.value);
                  end,
                  temp1);
           end);
  end;

*production-table*[92] :=
  method (state-stack, symbol-stack, #key $state)
    // VARIABLE-OPTION -> <MAPPING-TOKEN> <IDENTIFIER-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(170,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(#"map", $r2.string-value);
                  end,
                  temp1);
           end);
  end;

*production-table*[93] :=
  method (state-stack, symbol-stack, #key $state)
    // VARIABLE-OPTION -> <EQUATE-TOKEN> <IDENTIFIER-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(170,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(#"equate", $r2.string-value);
                  end,
                  temp1);
           end);
  end;

*production-table*[94] :=
  method (state-stack, symbol-stack, #key $state)
    // VARIABLE-OPTION -> <GETTER-TOKEN> SEALING
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(170,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      pair(#"seal", $r2);
                  end,
                  temp1);
           end);
  end;


//----------------------------------------------------------------------
// More parser boilerplate
//----------------------------------------------------------------------

define method do-action (action :: <accept>, state-stack :: <list>,
			 symbol-stack :: <list>, token :: <token>,
			 parse-state :: <parse-state>)
  unget-token(parse-state.tokenizer, token);
  if (symbol-stack.size ~= 1)
    parse-error(token, "Symbol-stack didn't get reduced all the way?");
  end;
  symbol-stack.head;
end;

define method do-action (action :: <shift>, state-stack :: <list>,
			 symbol-stack :: <list>, token :: <token>,
			 parse-state :: <parse-state>)
  parse-loop(pair(action.state, state-stack),
	     pair(token, symbol-stack),
	     get-token(parse-state.tokenizer),
	     parse-state);
end;

define method do-action (action :: <reduce>, state-stack :: <list>,
			 symbol-stack :: <list>, token :: <token>,
			 parse-state :: <parse-state>)
  let (new-state-stack, new-symbol-stack)
    = *production-table*[action.production](state-stack, symbol-stack,
                                            $state: parse-state);
  parse-loop(new-state-stack, new-symbol-stack, token, parse-state);
end;

define method find-action (table, token)
  let action = element(table, token.object-class, default: #f);
  if (action)
    action;
  else
    parse-error(token, "Parse error at or before %=.", token.string-value);
  end;
end;

define method parse-loop (state-stack :: <list>, symbol-stack :: <list>,
			  token :: <token>, parse-state :: <parse-state>)
  do-action(find-action(*action-table*[state-stack.head], token),
	    state-stack, symbol-stack, token, parse-state);
end;

//----------------------------------------------------------------------
// External interfaces to the parsing engine.
//----------------------------------------------------------------------

// This function processes a single "define interface" form, using the
// tokenizer in "parse-state", and annotates the parse state with the
// information acquired.
//
define method parse (parse-state :: <parse-state>)
  parse-loop(#(0), #(), get-token(parse-state.tokenizer), parse-state);
end;
