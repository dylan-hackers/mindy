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
  slot global-import-mode
    :: one-of(#"all", #"all-recursive", #"none", #"undecided") = #"undecided";
  slot global-imports :: <sequence> = make(<stretchy-vector>);
  slot file-import-modes = make(<string-table>) /* of #"all", #"none" */;
  slot file-imports = make(<string-table>) /* of <sequence> */;
  slot prefix :: type-union(<string>, <undefined>), init-value: undefined;
  constant slot exclude = make(<deque>);
  constant slot excluded-files = make(<deque>);
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
  slot include-files :: false-or(<sequence>) = #f;
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
    select (item.first)
      #"name-mapper" =>
        if (result.name-mapper ~= undefined)
          error("Multiple name mappers in one #include clause");
        else
          result.name-mapper := item.second;
        end if;
      #"import" =>
	let value = item.second;
        if (instance?(value, <sequence>))
          for (import in value)
            add!(result.global-imports, import);
          end for;
	  if (result.global-import-mode == #"undecided")
            result.global-import-mode := #"none";
          end if;
        else
          result.global-import-mode := value;
        end if;
      #"import-file" =>
        let (file, stream) = open-in-include-path(item.second.value);
        if (stream)
          close(stream);
        else
          parse-error(item.second, "File does not exist in path.");
        end if;
	let value = item.third;
        if (instance?(value, <sequence>))
	  let imports = (element(result.file-imports, file, default: #f)
                          | (result.file-imports[file]
			       := make(<stretchy-vector>)));
	  for (import in value)
	    add!(imports, import);
          end for;
	  unless (element(result.file-import-modes, file, default: #f))
            result.file-import-modes[file] := #"none";
          end unless;
        else
          result.file-import-modes[file] := value;
        end if;
      #"prefix" =>
        if (result.prefix ~= undefined)
          error("Multiple prefixes in one #include clause");
        else
          result.prefix := item.second;
        end if;
      #"exclude" =>
        // exclude lists contain tokens
 	for (elem in item.second) add!(result.exclude, elem.value) end for;
      #"exclude-file" =>
        // exclude lists contain tokens
	for (elem in item.second) 
          let (file, stream) = open-in-include-path(elem.value);
          if (stream)
	    add!(result.excluded-files, file);
            close(stream);
          else
            parse-error(elem, "File does not exist in path.");
          end if;
        end for;
      #"rename" =>
        result.rename := reduce(method(a,b) pair(b,a) end method,
                                result.rename, item.second);
      #"mapping" =>
        result.mappings := reduce(method(a,b) pair(b,a) end method,
                                  result.mappings, item.second);
      #"equate" =>
        result.equates := reduce(method(a,b) pair(b,a) end method,
                      	         result.equates, item.second);
      #"read-only" =>
        result.read-only := item.second;
      #"seal" =>
        result.seal-string := item.second;
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

define method \= (action1 :: <reduce>, action2 :: <reduce>) => eq? :: <boolean>;
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

define constant *action-table* = make(<vector>, size: 205);
define constant *production-table* = make(<vector>, size: 104);

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
  make-action-table(make(<shift>, on: <CONSTANT-TOKEN>, state: 165),
                    make(<shift>, on: <FUNCTION-TOKEN>, state: 99),
                    make(<shift>, on: <INCLUDE-TOKEN>, state: 3),
                    make(<shift>, on: <POINTER-TOKEN>, state: 156),
                    make(<shift>, on: <STRUCT-TOKEN>, state: 134),
                    make(<shift>, on: <UNION-TOKEN>, state: 150),
                    make(<shift>, on: <VARIABLE-TOKEN>, state: 175));

*action-table*[3] :=
  // FILE-CLAUSE -> <INCLUDE-TOKEN> * <STRING-LITERAL-TOKEN> FILE-OPTION-LIST
  // FILE-CLAUSE -> <INCLUDE-TOKEN> * <LBRACE-TOKEN> INCLUDE-FILE-LIST <RBRACE-TOKEN> FILE-OPTION-LIST
  make-action-table(make(<shift>, on: <LBRACE-TOKEN>, state: 4),
                    make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 96));

*action-table*[4] :=
  // FILE-CLAUSE -> <INCLUDE-TOKEN> <LBRACE-TOKEN> * INCLUDE-FILE-LIST <RBRACE-TOKEN> FILE-OPTION-LIST
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 5));

*action-table*[5] :=
  // INCLUDE-FILE-LIST -> <STRING-LITERAL-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 12),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 12));

*action-table*[6] :=
  // INCLUDE-FILE-LIST -> INCLUDE-FILE-LIST * <COMMA-TOKEN> <STRING-LITERAL-TOKEN>
  // FILE-CLAUSE -> <INCLUDE-TOKEN> <LBRACE-TOKEN> INCLUDE-FILE-LIST * <RBRACE-TOKEN> FILE-OPTION-LIST
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 94),
                    make(<shift>, on: <RBRACE-TOKEN>, state: 7));

*action-table*[7] :=
  // FILE-CLAUSE -> <INCLUDE-TOKEN> <LBRACE-TOKEN> INCLUDE-FILE-LIST <RBRACE-TOKEN> * FILE-OPTION-LIST
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 14),
                    make(<reduce>, on: <END-TOKEN>, production: 14),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 14));

*action-table*[8] :=
  // FILE-OPTION-LIST -> FILE-OPTION-LIST * <COMMA-TOKEN> FILE-OPTION
  // FILE-CLAUSE -> <INCLUDE-TOKEN> <LBRACE-TOKEN> INCLUDE-FILE-LIST <RBRACE-TOKEN> FILE-OPTION-LIST *
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 9),
                    make(<reduce>, on: <END-TOKEN>, production: 11),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 11));

*action-table*[9] :=
  // FILE-OPTION-LIST -> FILE-OPTION-LIST <COMMA-TOKEN> * FILE-OPTION
  make-action-table(make(<shift>, on: <DEFINE-MACRO-TOKEN>, state: 14),
                    make(<shift>, on: <EQUATE-TOKEN>, state: 81),
                    make(<shift>, on: <EXCLUDE-FILE-TOKEN>, state: 64),
                    make(<shift>, on: <EXCLUDE-TOKEN>, state: 57),
                    make(<shift>, on: <IMPORT-TOKEN>, state: 35),
                    make(<shift>, on: <MAPPING-TOKEN>, state: 77),
                    make(<shift>, on: <MINDY-INC-TOKEN>, state: 12),
                    make(<shift>, on: <NAME-MAPPER-TOKEN>, state: 33),
                    make(<shift>, on: <OBJECT-FILE-TOKEN>, state: 10),
                    make(<shift>, on: <PREFIX-TOKEN>, state: 55),
                    make(<shift>, on: <READ-ONLY-TOKEN>, state: 90),
                    make(<shift>, on: <RENAME-TOKEN>, state: 69),
                    make(<shift>, on: <SEAL-FUNCTIONS-TOKEN>, state: 85),
                    make(<shift>, on: <UNDEFINE-TOKEN>, state: 25));

*action-table*[10] :=
  // FILE-OPTION -> <OBJECT-FILE-TOKEN> * <STRING-LITERAL-TOKEN>
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 11));

*action-table*[11] :=
  // FILE-OPTION -> <OBJECT-FILE-TOKEN> <STRING-LITERAL-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 16),
                    make(<reduce>, on: <END-TOKEN>, production: 16),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 16));

*action-table*[12] :=
  // FILE-OPTION -> <MINDY-INC-TOKEN> * <STRING-LITERAL-TOKEN>
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 13));

*action-table*[13] :=
  // FILE-OPTION -> <MINDY-INC-TOKEN> <STRING-LITERAL-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 17),
                    make(<reduce>, on: <END-TOKEN>, production: 17),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 17));

*action-table*[14] :=
  // FILE-OPTION -> <DEFINE-MACRO-TOKEN> * <LBRACE-TOKEN> MACRO-DEFINITIONS <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <LBRACE-TOKEN>, state: 15));

*action-table*[15] :=
  // FILE-OPTION -> <DEFINE-MACRO-TOKEN> <LBRACE-TOKEN> * MACRO-DEFINITIONS <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 16));

*action-table*[16] :=
  // MACRO-DEFINITION -> <STRING-LITERAL-TOKEN> *
  // MACRO-DEFINITION -> <STRING-LITERAL-TOKEN> * <ARROW-TOKEN> <STRING-LITERAL-TOKEN>
  // MACRO-DEFINITION -> <STRING-LITERAL-TOKEN> * <ARROW-TOKEN> <INTEGER-TOKEN>
  make-action-table(make(<shift>, on: <ARROW-TOKEN>, state: 17),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 23),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 23));

*action-table*[17] :=
  // MACRO-DEFINITION -> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> * <INTEGER-TOKEN>
  // MACRO-DEFINITION -> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> * <STRING-LITERAL-TOKEN>
  make-action-table(make(<shift>, on: <INTEGER-TOKEN>, state: 19),
                    make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 18));

*action-table*[18] :=
  // MACRO-DEFINITION -> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <STRING-LITERAL-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 24),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 24));

*action-table*[19] :=
  // MACRO-DEFINITION -> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <INTEGER-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 25),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 25));

*action-table*[20] :=
  // MACRO-DEFINITIONS -> MACRO-DEFINITION *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 21),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 21));

*action-table*[21] :=
  // MACRO-DEFINITIONS -> MACRO-DEFINITIONS * <COMMA-TOKEN> MACRO-DEFINITION
  // FILE-OPTION -> <DEFINE-MACRO-TOKEN> <LBRACE-TOKEN> MACRO-DEFINITIONS * <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 23),
                    make(<shift>, on: <RBRACE-TOKEN>, state: 22));

*action-table*[22] :=
  // FILE-OPTION -> <DEFINE-MACRO-TOKEN> <LBRACE-TOKEN> MACRO-DEFINITIONS <RBRACE-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 18),
                    make(<reduce>, on: <END-TOKEN>, production: 18),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 18));

*action-table*[23] :=
  // MACRO-DEFINITIONS -> MACRO-DEFINITIONS <COMMA-TOKEN> * MACRO-DEFINITION
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 16));

*action-table*[24] :=
  // MACRO-DEFINITIONS -> MACRO-DEFINITIONS <COMMA-TOKEN> MACRO-DEFINITION *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 22),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 22));

*action-table*[25] :=
  // FILE-OPTION -> <UNDEFINE-TOKEN> * <LBRACE-TOKEN> MACRO-UNDEFINITIONS <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <LBRACE-TOKEN>, state: 26));

*action-table*[26] :=
  // FILE-OPTION -> <UNDEFINE-TOKEN> <LBRACE-TOKEN> * MACRO-UNDEFINITIONS <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 27));

*action-table*[27] :=
  // MACRO-UNDEFINITION -> <STRING-LITERAL-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 28),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 28));

*action-table*[28] :=
  // MACRO-UNDEFINITIONS -> MACRO-UNDEFINITION *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 26),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 26));

*action-table*[29] :=
  // MACRO-UNDEFINITIONS -> MACRO-UNDEFINITIONS * <COMMA-TOKEN> MACRO-UNDEFINITION
  // FILE-OPTION -> <UNDEFINE-TOKEN> <LBRACE-TOKEN> MACRO-UNDEFINITIONS * <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 31),
                    make(<shift>, on: <RBRACE-TOKEN>, state: 30));

*action-table*[30] :=
  // FILE-OPTION -> <UNDEFINE-TOKEN> <LBRACE-TOKEN> MACRO-UNDEFINITIONS <RBRACE-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 19),
                    make(<reduce>, on: <END-TOKEN>, production: 19),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 19));

*action-table*[31] :=
  // MACRO-UNDEFINITIONS -> MACRO-UNDEFINITIONS <COMMA-TOKEN> * MACRO-UNDEFINITION
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 27));

*action-table*[32] :=
  // MACRO-UNDEFINITIONS -> MACRO-UNDEFINITIONS <COMMA-TOKEN> MACRO-UNDEFINITION *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 27),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 27));

*action-table*[33] :=
  // CONTAINER-OPTION -> <NAME-MAPPER-TOKEN> * <IDENTIFIER-TOKEN>
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 34));

*action-table*[34] :=
  // CONTAINER-OPTION -> <NAME-MAPPER-TOKEN> <IDENTIFIER-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 29),
                    make(<reduce>, on: <END-TOKEN>, production: 29),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 29));

*action-table*[35] :=
  // CONTAINER-OPTION -> <IMPORT-TOKEN> * <ALL-TOKEN>
  // CONTAINER-OPTION -> <IMPORT-TOKEN> * <ALL-RECURSIVE-TOKEN>
  // CONTAINER-OPTION -> <IMPORT-TOKEN> * <LBRACE-TOKEN> IMPORT-LIST <RBRACE-TOKEN>
  // CONTAINER-OPTION -> <IMPORT-TOKEN> * <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <ALL-TOKEN>
  // CONTAINER-OPTION -> <IMPORT-TOKEN> * <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <NONE-TOKEN>
  // CONTAINER-OPTION -> <IMPORT-TOKEN> * <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <LBRACE-TOKEN> IMPORT-LIST <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <ALL-RECURSIVE-TOKEN>, state: 53),
                    make(<shift>, on: <ALL-TOKEN>, state: 54),
                    make(<shift>, on: <LBRACE-TOKEN>, state: 50),
                    make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 36));

*action-table*[36] :=
  // CONTAINER-OPTION -> <IMPORT-TOKEN> <STRING-LITERAL-TOKEN> * <ARROW-TOKEN> <LBRACE-TOKEN> IMPORT-LIST <RBRACE-TOKEN>
  // CONTAINER-OPTION -> <IMPORT-TOKEN> <STRING-LITERAL-TOKEN> * <ARROW-TOKEN> <NONE-TOKEN>
  // CONTAINER-OPTION -> <IMPORT-TOKEN> <STRING-LITERAL-TOKEN> * <ARROW-TOKEN> <ALL-TOKEN>
  make-action-table(make(<shift>, on: <ARROW-TOKEN>, state: 37));

*action-table*[37] :=
  // CONTAINER-OPTION -> <IMPORT-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> * <ALL-TOKEN>
  // CONTAINER-OPTION -> <IMPORT-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> * <NONE-TOKEN>
  // CONTAINER-OPTION -> <IMPORT-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> * <LBRACE-TOKEN> IMPORT-LIST <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <ALL-TOKEN>, state: 49),
                    make(<shift>, on: <LBRACE-TOKEN>, state: 38),
                    make(<shift>, on: <NONE-TOKEN>, state: 48));

*action-table*[38] :=
  // CONTAINER-OPTION -> <IMPORT-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <LBRACE-TOKEN> * IMPORT-LIST <RBRACE-TOKEN>
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 36),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 36),
                    make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 39));

*action-table*[39] :=
  // IMPORT -> <STRING-LITERAL-TOKEN> *
  // RENAMING -> <STRING-LITERAL-TOKEN> * <ARROW-TOKEN> <IDENTIFIER-TOKEN>
  make-action-table(make(<shift>, on: <ARROW-TOKEN>, state: 40),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 39),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 39));

*action-table*[40] :=
  // RENAMING -> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> * <IDENTIFIER-TOKEN>
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 41));

*action-table*[41] :=
  // RENAMING -> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 41),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 41));

*action-table*[42] :=
  // IMPORT -> RENAMING *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 40),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 40));

*action-table*[43] :=
  // IMPORT-LIST -> IMPORT *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 38),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 38));

*action-table*[44] :=
  // IMPORT-LIST -> IMPORT-LIST * <COMMA-TOKEN> IMPORT
  // CONTAINER-OPTION -> <IMPORT-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <LBRACE-TOKEN> IMPORT-LIST * <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 46),
                    make(<shift>, on: <RBRACE-TOKEN>, state: 45));

*action-table*[45] :=
  // CONTAINER-OPTION -> <IMPORT-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <LBRACE-TOKEN> IMPORT-LIST <RBRACE-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 35),
                    make(<reduce>, on: <END-TOKEN>, production: 35),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 35));

*action-table*[46] :=
  // IMPORT-LIST -> IMPORT-LIST <COMMA-TOKEN> * IMPORT
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 39));

*action-table*[47] :=
  // IMPORT-LIST -> IMPORT-LIST <COMMA-TOKEN> IMPORT *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 37),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 37));

*action-table*[48] :=
  // CONTAINER-OPTION -> <IMPORT-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <NONE-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 34),
                    make(<reduce>, on: <END-TOKEN>, production: 34),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 34));

*action-table*[49] :=
  // CONTAINER-OPTION -> <IMPORT-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <ALL-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 33),
                    make(<reduce>, on: <END-TOKEN>, production: 33),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 33));

*action-table*[50] :=
  // CONTAINER-OPTION -> <IMPORT-TOKEN> <LBRACE-TOKEN> * IMPORT-LIST <RBRACE-TOKEN>
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 36),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 36),
                    make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 39));

*action-table*[51] :=
  // IMPORT-LIST -> IMPORT-LIST * <COMMA-TOKEN> IMPORT
  // CONTAINER-OPTION -> <IMPORT-TOKEN> <LBRACE-TOKEN> IMPORT-LIST * <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 46),
                    make(<shift>, on: <RBRACE-TOKEN>, state: 52));

*action-table*[52] :=
  // CONTAINER-OPTION -> <IMPORT-TOKEN> <LBRACE-TOKEN> IMPORT-LIST <RBRACE-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 32),
                    make(<reduce>, on: <END-TOKEN>, production: 32),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 32));

*action-table*[53] :=
  // CONTAINER-OPTION -> <IMPORT-TOKEN> <ALL-RECURSIVE-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 31),
                    make(<reduce>, on: <END-TOKEN>, production: 31),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 31));

*action-table*[54] :=
  // CONTAINER-OPTION -> <IMPORT-TOKEN> <ALL-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 30),
                    make(<reduce>, on: <END-TOKEN>, production: 30),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 30));

*action-table*[55] :=
  // CONTAINER-OPTION -> <PREFIX-TOKEN> * <STRING-LITERAL-TOKEN>
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 56));

*action-table*[56] :=
  // CONTAINER-OPTION -> <PREFIX-TOKEN> <STRING-LITERAL-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 42),
                    make(<reduce>, on: <END-TOKEN>, production: 42),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 42));

*action-table*[57] :=
  // CONTAINER-OPTION -> <EXCLUDE-TOKEN> * <LBRACE-TOKEN> EXCLUDE-LIST <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <LBRACE-TOKEN>, state: 58));

*action-table*[58] :=
  // CONTAINER-OPTION -> <EXCLUDE-TOKEN> <LBRACE-TOKEN> * EXCLUDE-LIST <RBRACE-TOKEN>
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 44),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 44),
                    make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 59));

*action-table*[59] :=
  // EXCLUDE-LIST -> <STRING-LITERAL-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 46),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 46));

*action-table*[60] :=
  // EXCLUDE-LIST -> EXCLUDE-LIST * <COMMA-TOKEN> <STRING-LITERAL-TOKEN>
  // CONTAINER-OPTION -> <EXCLUDE-TOKEN> <LBRACE-TOKEN> EXCLUDE-LIST * <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 62),
                    make(<shift>, on: <RBRACE-TOKEN>, state: 61));

*action-table*[61] :=
  // CONTAINER-OPTION -> <EXCLUDE-TOKEN> <LBRACE-TOKEN> EXCLUDE-LIST <RBRACE-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 43),
                    make(<reduce>, on: <END-TOKEN>, production: 43),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 43));

*action-table*[62] :=
  // EXCLUDE-LIST -> EXCLUDE-LIST <COMMA-TOKEN> * <STRING-LITERAL-TOKEN>
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 63));

*action-table*[63] :=
  // EXCLUDE-LIST -> EXCLUDE-LIST <COMMA-TOKEN> <STRING-LITERAL-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 45),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 45));

*action-table*[64] :=
  // CONTAINER-OPTION -> <EXCLUDE-FILE-TOKEN> * <STRING-LITERAL-TOKEN>
  // CONTAINER-OPTION -> <EXCLUDE-FILE-TOKEN> * <LBRACE-TOKEN> EXCLUDE-LIST <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <LBRACE-TOKEN>, state: 65),
                    make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 68));

*action-table*[65] :=
  // CONTAINER-OPTION -> <EXCLUDE-FILE-TOKEN> <LBRACE-TOKEN> * EXCLUDE-LIST <RBRACE-TOKEN>
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 44),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 44),
                    make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 59));

*action-table*[66] :=
  // EXCLUDE-LIST -> EXCLUDE-LIST * <COMMA-TOKEN> <STRING-LITERAL-TOKEN>
  // CONTAINER-OPTION -> <EXCLUDE-FILE-TOKEN> <LBRACE-TOKEN> EXCLUDE-LIST * <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 62),
                    make(<shift>, on: <RBRACE-TOKEN>, state: 67));

*action-table*[67] :=
  // CONTAINER-OPTION -> <EXCLUDE-FILE-TOKEN> <LBRACE-TOKEN> EXCLUDE-LIST <RBRACE-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 48),
                    make(<reduce>, on: <END-TOKEN>, production: 48),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 48));

*action-table*[68] :=
  // CONTAINER-OPTION -> <EXCLUDE-FILE-TOKEN> <STRING-LITERAL-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 47),
                    make(<reduce>, on: <END-TOKEN>, production: 47),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 47));

*action-table*[69] :=
  // CONTAINER-OPTION -> <RENAME-TOKEN> * <LBRACE-TOKEN> RENAMING-LIST <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <LBRACE-TOKEN>, state: 70));

*action-table*[70] :=
  // CONTAINER-OPTION -> <RENAME-TOKEN> <LBRACE-TOKEN> * RENAMING-LIST <RBRACE-TOKEN>
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 50),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 50),
                    make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 71));

*action-table*[71] :=
  // RENAMING -> <STRING-LITERAL-TOKEN> * <ARROW-TOKEN> <IDENTIFIER-TOKEN>
  make-action-table(make(<shift>, on: <ARROW-TOKEN>, state: 40));

*action-table*[72] :=
  // RENAMING-LIST -> RENAMING *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 52),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 52));

*action-table*[73] :=
  // RENAMING-LIST -> RENAMING-LIST * <COMMA-TOKEN> RENAMING
  // CONTAINER-OPTION -> <RENAME-TOKEN> <LBRACE-TOKEN> RENAMING-LIST * <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 75),
                    make(<shift>, on: <RBRACE-TOKEN>, state: 74));

*action-table*[74] :=
  // CONTAINER-OPTION -> <RENAME-TOKEN> <LBRACE-TOKEN> RENAMING-LIST <RBRACE-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 49),
                    make(<reduce>, on: <END-TOKEN>, production: 49),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 49));

*action-table*[75] :=
  // RENAMING-LIST -> RENAMING-LIST <COMMA-TOKEN> * RENAMING
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 71));

*action-table*[76] :=
  // RENAMING-LIST -> RENAMING-LIST <COMMA-TOKEN> RENAMING *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 51),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 51));

*action-table*[77] :=
  // CONTAINER-OPTION -> <MAPPING-TOKEN> * <LBRACE-TOKEN> RENAMING-LIST <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <LBRACE-TOKEN>, state: 78));

*action-table*[78] :=
  // CONTAINER-OPTION -> <MAPPING-TOKEN> <LBRACE-TOKEN> * RENAMING-LIST <RBRACE-TOKEN>
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 50),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 50),
                    make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 71));

*action-table*[79] :=
  // RENAMING-LIST -> RENAMING-LIST * <COMMA-TOKEN> RENAMING
  // CONTAINER-OPTION -> <MAPPING-TOKEN> <LBRACE-TOKEN> RENAMING-LIST * <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 75),
                    make(<shift>, on: <RBRACE-TOKEN>, state: 80));

*action-table*[80] :=
  // CONTAINER-OPTION -> <MAPPING-TOKEN> <LBRACE-TOKEN> RENAMING-LIST <RBRACE-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 53),
                    make(<reduce>, on: <END-TOKEN>, production: 53),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 53));

*action-table*[81] :=
  // CONTAINER-OPTION -> <EQUATE-TOKEN> * <LBRACE-TOKEN> RENAMING-LIST <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <LBRACE-TOKEN>, state: 82));

*action-table*[82] :=
  // CONTAINER-OPTION -> <EQUATE-TOKEN> <LBRACE-TOKEN> * RENAMING-LIST <RBRACE-TOKEN>
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 50),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 50),
                    make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 71));

*action-table*[83] :=
  // RENAMING-LIST -> RENAMING-LIST * <COMMA-TOKEN> RENAMING
  // CONTAINER-OPTION -> <EQUATE-TOKEN> <LBRACE-TOKEN> RENAMING-LIST * <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 75),
                    make(<shift>, on: <RBRACE-TOKEN>, state: 84));

*action-table*[84] :=
  // CONTAINER-OPTION -> <EQUATE-TOKEN> <LBRACE-TOKEN> RENAMING-LIST <RBRACE-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 54),
                    make(<reduce>, on: <END-TOKEN>, production: 54),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 54));

*action-table*[85] :=
  // CONTAINER-OPTION -> <SEAL-FUNCTIONS-TOKEN> * SEALING
  make-action-table(make(<shift>, on: <INLINE-TOKEN>, state: 88),
                    make(<shift>, on: <OPEN-TOKEN>, state: 87),
                    make(<shift>, on: <SEALED-TOKEN>, state: 86));

*action-table*[86] :=
  // SEALING -> <SEALED-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 55),
                    make(<reduce>, on: <END-TOKEN>, production: 55),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 55));

*action-table*[87] :=
  // SEALING -> <OPEN-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 56),
                    make(<reduce>, on: <END-TOKEN>, production: 56),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 56));

*action-table*[88] :=
  // SEALING -> <INLINE-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 57),
                    make(<reduce>, on: <END-TOKEN>, production: 57),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 57));

*action-table*[89] :=
  // CONTAINER-OPTION -> <SEAL-FUNCTIONS-TOKEN> SEALING *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 58),
                    make(<reduce>, on: <END-TOKEN>, production: 58),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 58));

*action-table*[90] :=
  // CONTAINER-OPTION -> <READ-ONLY-TOKEN> * <BOOLEAN-TOKEN>
  make-action-table(make(<shift>, on: <BOOLEAN-TOKEN>, state: 91));

*action-table*[91] :=
  // CONTAINER-OPTION -> <READ-ONLY-TOKEN> <BOOLEAN-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 59),
                    make(<reduce>, on: <END-TOKEN>, production: 59),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 59));

*action-table*[92] :=
  // FILE-OPTION -> CONTAINER-OPTION *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 20),
                    make(<reduce>, on: <END-TOKEN>, production: 20),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 20));

*action-table*[93] :=
  // FILE-OPTION-LIST -> FILE-OPTION-LIST <COMMA-TOKEN> FILE-OPTION *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 15),
                    make(<reduce>, on: <END-TOKEN>, production: 15),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 15));

*action-table*[94] :=
  // INCLUDE-FILE-LIST -> INCLUDE-FILE-LIST <COMMA-TOKEN> * <STRING-LITERAL-TOKEN>
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 95));

*action-table*[95] :=
  // INCLUDE-FILE-LIST -> INCLUDE-FILE-LIST <COMMA-TOKEN> <STRING-LITERAL-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 13),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 13));

*action-table*[96] :=
  // FILE-CLAUSE -> <INCLUDE-TOKEN> <STRING-LITERAL-TOKEN> * FILE-OPTION-LIST
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 14),
                    make(<reduce>, on: <END-TOKEN>, production: 14),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 14));

*action-table*[97] :=
  // FILE-OPTION-LIST -> FILE-OPTION-LIST * <COMMA-TOKEN> FILE-OPTION
  // FILE-CLAUSE -> <INCLUDE-TOKEN> <STRING-LITERAL-TOKEN> FILE-OPTION-LIST *
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 9),
                    make(<reduce>, on: <END-TOKEN>, production: 10),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 10));

*action-table*[98] :=
  // INTERFACE-CLAUSE -> FILE-CLAUSE *
  make-action-table(make(<reduce>, on: <END-TOKEN>, production: 9),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 9));

*action-table*[99] :=
  // INTERFACE-CLAUSE -> <FUNCTION-TOKEN> * <STRING-LITERAL-TOKEN> FUNCTION-OPTION-LIST
  // INTERFACE-CLAUSE -> <FUNCTION-TOKEN> * <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> FUNCTION-OPTION-LIST
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 100));

*action-table*[100] :=
  // INTERFACE-CLAUSE -> <FUNCTION-TOKEN> <STRING-LITERAL-TOKEN> * <ARROW-TOKEN> <IDENTIFIER-TOKEN> FUNCTION-OPTION-LIST
  // INTERFACE-CLAUSE -> <FUNCTION-TOKEN> <STRING-LITERAL-TOKEN> * FUNCTION-OPTION-LIST
  make-action-table(make(<shift>, on: <ARROW-TOKEN>, state: 131),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 62),
                    make(<reduce>, on: <END-TOKEN>, production: 62),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 62));

*action-table*[101] :=
  // FUNCTION-OPTION-LIST -> FUNCTION-OPTION-LIST * <COMMA-TOKEN> FUNCTION-OPTION
  // INTERFACE-CLAUSE -> <FUNCTION-TOKEN> <STRING-LITERAL-TOKEN> FUNCTION-OPTION-LIST *
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 102),
                    make(<reduce>, on: <END-TOKEN>, production: 60),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 60));

*action-table*[102] :=
  // FUNCTION-OPTION-LIST -> FUNCTION-OPTION-LIST <COMMA-TOKEN> * FUNCTION-OPTION
  make-action-table(make(<shift>, on: <EQUATE-ARGUMENT-TOKEN>, state: 118),
                    make(<shift>, on: <EQUATE-RESULT-TOKEN>, state: 105),
                    make(<shift>, on: <IGNORE-RESULT-TOKEN>, state: 107),
                    make(<shift>, on: <INPUT-ARGUMENT-TOKEN>, state: 124),
                    make(<shift>, on: <INPUT-OUTPUT-ARGUMENT-TOKEN>, state: 126),
                    make(<shift>, on: <MAP-ARGUMENT-TOKEN>, state: 109),
                    make(<shift>, on: <MAP-RESULT-TOKEN>, state: 103),
                    make(<shift>, on: <OUTPUT-ARGUMENT-TOKEN>, state: 128));

*action-table*[103] :=
  // FUNCTION-OPTION -> <MAP-RESULT-TOKEN> * <IDENTIFIER-TOKEN>
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 104));

*action-table*[104] :=
  // FUNCTION-OPTION -> <MAP-RESULT-TOKEN> <IDENTIFIER-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 64),
                    make(<reduce>, on: <END-TOKEN>, production: 64),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 64));

*action-table*[105] :=
  // FUNCTION-OPTION -> <EQUATE-RESULT-TOKEN> * <IDENTIFIER-TOKEN>
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 106));

*action-table*[106] :=
  // FUNCTION-OPTION -> <EQUATE-RESULT-TOKEN> <IDENTIFIER-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 65),
                    make(<reduce>, on: <END-TOKEN>, production: 65),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 65));

*action-table*[107] :=
  // FUNCTION-OPTION -> <IGNORE-RESULT-TOKEN> * <BOOLEAN-TOKEN>
  make-action-table(make(<shift>, on: <BOOLEAN-TOKEN>, state: 108));

*action-table*[108] :=
  // FUNCTION-OPTION -> <IGNORE-RESULT-TOKEN> <BOOLEAN-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 66),
                    make(<reduce>, on: <END-TOKEN>, production: 66),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 66));

*action-table*[109] :=
  // FUNCTION-OPTION -> <MAP-ARGUMENT-TOKEN> * <LBRACE-TOKEN> ARGUMENT <ARROW-TOKEN> <IDENTIFIER-TOKEN> <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <LBRACE-TOKEN>, state: 110));

*action-table*[110] :=
  // FUNCTION-OPTION -> <MAP-ARGUMENT-TOKEN> <LBRACE-TOKEN> * ARGUMENT <ARROW-TOKEN> <IDENTIFIER-TOKEN> <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 112),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 111),
                    make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 113));

*action-table*[111] :=
  // ARGUMENT -> <INTEGER-TOKEN> *
  make-action-table(make(<reduce>, on: <ARROW-TOKEN>, production: 72),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 72),
                    make(<reduce>, on: <END-TOKEN>, production: 72),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 72));

*action-table*[112] :=
  // ARGUMENT -> <IDENTIFIER-TOKEN> *
  make-action-table(make(<reduce>, on: <ARROW-TOKEN>, production: 73),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 73),
                    make(<reduce>, on: <END-TOKEN>, production: 73),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 73));

*action-table*[113] :=
  // ARGUMENT -> <STRING-LITERAL-TOKEN> *
  make-action-table(make(<reduce>, on: <ARROW-TOKEN>, production: 74),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 74),
                    make(<reduce>, on: <END-TOKEN>, production: 74),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 74));

*action-table*[114] :=
  // FUNCTION-OPTION -> <MAP-ARGUMENT-TOKEN> <LBRACE-TOKEN> ARGUMENT * <ARROW-TOKEN> <IDENTIFIER-TOKEN> <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <ARROW-TOKEN>, state: 115));

*action-table*[115] :=
  // FUNCTION-OPTION -> <MAP-ARGUMENT-TOKEN> <LBRACE-TOKEN> ARGUMENT <ARROW-TOKEN> * <IDENTIFIER-TOKEN> <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 116));

*action-table*[116] :=
  // FUNCTION-OPTION -> <MAP-ARGUMENT-TOKEN> <LBRACE-TOKEN> ARGUMENT <ARROW-TOKEN> <IDENTIFIER-TOKEN> * <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <RBRACE-TOKEN>, state: 117));

*action-table*[117] :=
  // FUNCTION-OPTION -> <MAP-ARGUMENT-TOKEN> <LBRACE-TOKEN> ARGUMENT <ARROW-TOKEN> <IDENTIFIER-TOKEN> <RBRACE-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 67),
                    make(<reduce>, on: <END-TOKEN>, production: 67),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 67));

*action-table*[118] :=
  // FUNCTION-OPTION -> <EQUATE-ARGUMENT-TOKEN> * <LBRACE-TOKEN> ARGUMENT <ARROW-TOKEN> <IDENTIFIER-TOKEN> <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <LBRACE-TOKEN>, state: 119));

*action-table*[119] :=
  // FUNCTION-OPTION -> <EQUATE-ARGUMENT-TOKEN> <LBRACE-TOKEN> * ARGUMENT <ARROW-TOKEN> <IDENTIFIER-TOKEN> <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 112),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 111),
                    make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 113));

*action-table*[120] :=
  // FUNCTION-OPTION -> <EQUATE-ARGUMENT-TOKEN> <LBRACE-TOKEN> ARGUMENT * <ARROW-TOKEN> <IDENTIFIER-TOKEN> <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <ARROW-TOKEN>, state: 121));

*action-table*[121] :=
  // FUNCTION-OPTION -> <EQUATE-ARGUMENT-TOKEN> <LBRACE-TOKEN> ARGUMENT <ARROW-TOKEN> * <IDENTIFIER-TOKEN> <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 122));

*action-table*[122] :=
  // FUNCTION-OPTION -> <EQUATE-ARGUMENT-TOKEN> <LBRACE-TOKEN> ARGUMENT <ARROW-TOKEN> <IDENTIFIER-TOKEN> * <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <RBRACE-TOKEN>, state: 123));

*action-table*[123] :=
  // FUNCTION-OPTION -> <EQUATE-ARGUMENT-TOKEN> <LBRACE-TOKEN> ARGUMENT <ARROW-TOKEN> <IDENTIFIER-TOKEN> <RBRACE-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 68),
                    make(<reduce>, on: <END-TOKEN>, production: 68),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 68));

*action-table*[124] :=
  // FUNCTION-OPTION -> <INPUT-ARGUMENT-TOKEN> * ARGUMENT
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 112),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 111),
                    make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 113));

*action-table*[125] :=
  // FUNCTION-OPTION -> <INPUT-ARGUMENT-TOKEN> ARGUMENT *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 69),
                    make(<reduce>, on: <END-TOKEN>, production: 69),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 69));

*action-table*[126] :=
  // FUNCTION-OPTION -> <INPUT-OUTPUT-ARGUMENT-TOKEN> * ARGUMENT
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 112),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 111),
                    make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 113));

*action-table*[127] :=
  // FUNCTION-OPTION -> <INPUT-OUTPUT-ARGUMENT-TOKEN> ARGUMENT *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 70),
                    make(<reduce>, on: <END-TOKEN>, production: 70),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 70));

*action-table*[128] :=
  // FUNCTION-OPTION -> <OUTPUT-ARGUMENT-TOKEN> * ARGUMENT
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 112),
                    make(<shift>, on: <INTEGER-TOKEN>, state: 111),
                    make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 113));

*action-table*[129] :=
  // FUNCTION-OPTION -> <OUTPUT-ARGUMENT-TOKEN> ARGUMENT *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 71),
                    make(<reduce>, on: <END-TOKEN>, production: 71),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 71));

*action-table*[130] :=
  // FUNCTION-OPTION-LIST -> FUNCTION-OPTION-LIST <COMMA-TOKEN> FUNCTION-OPTION *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 63),
                    make(<reduce>, on: <END-TOKEN>, production: 63),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 63));

*action-table*[131] :=
  // INTERFACE-CLAUSE -> <FUNCTION-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> * <IDENTIFIER-TOKEN> FUNCTION-OPTION-LIST
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 132));

*action-table*[132] :=
  // INTERFACE-CLAUSE -> <FUNCTION-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> * FUNCTION-OPTION-LIST
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 62),
                    make(<reduce>, on: <END-TOKEN>, production: 62),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 62));

*action-table*[133] :=
  // FUNCTION-OPTION-LIST -> FUNCTION-OPTION-LIST * <COMMA-TOKEN> FUNCTION-OPTION
  // INTERFACE-CLAUSE -> <FUNCTION-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> FUNCTION-OPTION-LIST *
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 102),
                    make(<reduce>, on: <END-TOKEN>, production: 61),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 61));

*action-table*[134] :=
  // INTERFACE-CLAUSE -> <STRUCT-TOKEN> * <STRING-LITERAL-TOKEN> STRUCTURE-OPTION-LIST
  // INTERFACE-CLAUSE -> <STRUCT-TOKEN> * <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> STRUCTURE-OPTION-LIST
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 135));

*action-table*[135] :=
  // INTERFACE-CLAUSE -> <STRUCT-TOKEN> <STRING-LITERAL-TOKEN> * <ARROW-TOKEN> <IDENTIFIER-TOKEN> STRUCTURE-OPTION-LIST
  // INTERFACE-CLAUSE -> <STRUCT-TOKEN> <STRING-LITERAL-TOKEN> * STRUCTURE-OPTION-LIST
  make-action-table(make(<shift>, on: <ARROW-TOKEN>, state: 147),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 79),
                    make(<reduce>, on: <END-TOKEN>, production: 79),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 79));

*action-table*[136] :=
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST * <COMMA-TOKEN> CONTAINER-OPTION
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST * <COMMA-TOKEN> SUPERCLASS-OPTION
  // INTERFACE-CLAUSE -> <STRUCT-TOKEN> <STRING-LITERAL-TOKEN> STRUCTURE-OPTION-LIST *
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 137),
                    make(<reduce>, on: <END-TOKEN>, production: 75),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 75));

*action-table*[137] :=
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST <COMMA-TOKEN> * SUPERCLASS-OPTION
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST <COMMA-TOKEN> * CONTAINER-OPTION
  make-action-table(make(<shift>, on: <EQUATE-TOKEN>, state: 81),
                    make(<shift>, on: <EXCLUDE-FILE-TOKEN>, state: 64),
                    make(<shift>, on: <EXCLUDE-TOKEN>, state: 57),
                    make(<shift>, on: <IMPORT-TOKEN>, state: 35),
                    make(<shift>, on: <MAPPING-TOKEN>, state: 77),
                    make(<shift>, on: <NAME-MAPPER-TOKEN>, state: 33),
                    make(<shift>, on: <PREFIX-TOKEN>, state: 55),
                    make(<shift>, on: <READ-ONLY-TOKEN>, state: 90),
                    make(<shift>, on: <RENAME-TOKEN>, state: 69),
                    make(<shift>, on: <SEAL-FUNCTIONS-TOKEN>, state: 85),
                    make(<shift>, on: <SUPERCLASS-TOKEN>, state: 139));

*action-table*[138] :=
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST <COMMA-TOKEN> CONTAINER-OPTION *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 80),
                    make(<reduce>, on: <END-TOKEN>, production: 80),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 80));

*action-table*[139] :=
  // SUPERCLASS-OPTION -> <SUPERCLASS-TOKEN> * <LBRACE-TOKEN> SUPERCLASS-LIST <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <LBRACE-TOKEN>, state: 140));

*action-table*[140] :=
  // SUPERCLASS-OPTION -> <SUPERCLASS-TOKEN> <LBRACE-TOKEN> * SUPERCLASS-LIST <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 141));

*action-table*[141] :=
  // SUPERCLASS-LIST -> <IDENTIFIER-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 81),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 81));

*action-table*[142] :=
  // SUPERCLASS-LIST -> SUPERCLASS-LIST * <COMMA-TOKEN> <IDENTIFIER-TOKEN>
  // SUPERCLASS-OPTION -> <SUPERCLASS-TOKEN> <LBRACE-TOKEN> SUPERCLASS-LIST * <RBRACE-TOKEN>
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 144),
                    make(<shift>, on: <RBRACE-TOKEN>, state: 143));

*action-table*[143] :=
  // SUPERCLASS-OPTION -> <SUPERCLASS-TOKEN> <LBRACE-TOKEN> SUPERCLASS-LIST <RBRACE-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 83),
                    make(<reduce>, on: <END-TOKEN>, production: 83),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 83));

*action-table*[144] :=
  // SUPERCLASS-LIST -> SUPERCLASS-LIST <COMMA-TOKEN> * <IDENTIFIER-TOKEN>
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 145));

*action-table*[145] :=
  // SUPERCLASS-LIST -> SUPERCLASS-LIST <COMMA-TOKEN> <IDENTIFIER-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 82),
                    make(<reduce>, on: <RBRACE-TOKEN>, production: 82));

*action-table*[146] :=
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST <COMMA-TOKEN> SUPERCLASS-OPTION *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 84),
                    make(<reduce>, on: <END-TOKEN>, production: 84),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 84));

*action-table*[147] :=
  // INTERFACE-CLAUSE -> <STRUCT-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> * <IDENTIFIER-TOKEN> STRUCTURE-OPTION-LIST
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 148));

*action-table*[148] :=
  // INTERFACE-CLAUSE -> <STRUCT-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> * STRUCTURE-OPTION-LIST
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 79),
                    make(<reduce>, on: <END-TOKEN>, production: 79),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 79));

*action-table*[149] :=
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST * <COMMA-TOKEN> CONTAINER-OPTION
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST * <COMMA-TOKEN> SUPERCLASS-OPTION
  // INTERFACE-CLAUSE -> <STRUCT-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> STRUCTURE-OPTION-LIST *
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 137),
                    make(<reduce>, on: <END-TOKEN>, production: 76),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 76));

*action-table*[150] :=
  // INTERFACE-CLAUSE -> <UNION-TOKEN> * <STRING-LITERAL-TOKEN> STRUCTURE-OPTION-LIST
  // INTERFACE-CLAUSE -> <UNION-TOKEN> * <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> STRUCTURE-OPTION-LIST
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 151));

*action-table*[151] :=
  // INTERFACE-CLAUSE -> <UNION-TOKEN> <STRING-LITERAL-TOKEN> * <ARROW-TOKEN> <IDENTIFIER-TOKEN> STRUCTURE-OPTION-LIST
  // INTERFACE-CLAUSE -> <UNION-TOKEN> <STRING-LITERAL-TOKEN> * STRUCTURE-OPTION-LIST
  make-action-table(make(<shift>, on: <ARROW-TOKEN>, state: 153),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 79),
                    make(<reduce>, on: <END-TOKEN>, production: 79),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 79));

*action-table*[152] :=
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST * <COMMA-TOKEN> CONTAINER-OPTION
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST * <COMMA-TOKEN> SUPERCLASS-OPTION
  // INTERFACE-CLAUSE -> <UNION-TOKEN> <STRING-LITERAL-TOKEN> STRUCTURE-OPTION-LIST *
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 137),
                    make(<reduce>, on: <END-TOKEN>, production: 77),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 77));

*action-table*[153] :=
  // INTERFACE-CLAUSE -> <UNION-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> * <IDENTIFIER-TOKEN> STRUCTURE-OPTION-LIST
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 154));

*action-table*[154] :=
  // INTERFACE-CLAUSE -> <UNION-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> * STRUCTURE-OPTION-LIST
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 79),
                    make(<reduce>, on: <END-TOKEN>, production: 79),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 79));

*action-table*[155] :=
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST * <COMMA-TOKEN> CONTAINER-OPTION
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST * <COMMA-TOKEN> SUPERCLASS-OPTION
  // INTERFACE-CLAUSE -> <UNION-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> STRUCTURE-OPTION-LIST *
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 137),
                    make(<reduce>, on: <END-TOKEN>, production: 78),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 78));

*action-table*[156] :=
  // INTERFACE-CLAUSE -> <POINTER-TOKEN> * <STRING-LITERAL-TOKEN> POINTER-OPTION-LIST
  // INTERFACE-CLAUSE -> <POINTER-TOKEN> * <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> POINTER-OPTION-LIST
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 157));

*action-table*[157] :=
  // INTERFACE-CLAUSE -> <POINTER-TOKEN> <STRING-LITERAL-TOKEN> * <ARROW-TOKEN> <IDENTIFIER-TOKEN> POINTER-OPTION-LIST
  // INTERFACE-CLAUSE -> <POINTER-TOKEN> <STRING-LITERAL-TOKEN> * POINTER-OPTION-LIST
  make-action-table(make(<shift>, on: <ARROW-TOKEN>, state: 162),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 79),
                    make(<reduce>, on: <END-TOKEN>, production: 87),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 87));

*action-table*[158] :=
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST * <COMMA-TOKEN> CONTAINER-OPTION
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST * <COMMA-TOKEN> SUPERCLASS-OPTION
  // POINTER-OPTION-LIST -> STRUCTURE-OPTION-LIST * <COMMA-TOKEN> SUPERCLASS-OPTION
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 159));

*action-table*[159] :=
  // POINTER-OPTION-LIST -> STRUCTURE-OPTION-LIST <COMMA-TOKEN> * SUPERCLASS-OPTION
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST <COMMA-TOKEN> * SUPERCLASS-OPTION
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST <COMMA-TOKEN> * CONTAINER-OPTION
  make-action-table(make(<shift>, on: <EQUATE-TOKEN>, state: 81),
                    make(<shift>, on: <EXCLUDE-FILE-TOKEN>, state: 64),
                    make(<shift>, on: <EXCLUDE-TOKEN>, state: 57),
                    make(<shift>, on: <IMPORT-TOKEN>, state: 35),
                    make(<shift>, on: <MAPPING-TOKEN>, state: 77),
                    make(<shift>, on: <NAME-MAPPER-TOKEN>, state: 33),
                    make(<shift>, on: <PREFIX-TOKEN>, state: 55),
                    make(<shift>, on: <READ-ONLY-TOKEN>, state: 90),
                    make(<shift>, on: <RENAME-TOKEN>, state: 69),
                    make(<shift>, on: <SEAL-FUNCTIONS-TOKEN>, state: 85),
                    make(<shift>, on: <SUPERCLASS-TOKEN>, state: 139));

*action-table*[160] :=
  // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST <COMMA-TOKEN> SUPERCLASS-OPTION *
  // POINTER-OPTION-LIST -> STRUCTURE-OPTION-LIST <COMMA-TOKEN> SUPERCLASS-OPTION *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 84),
                    make(<reduce>, on: <END-TOKEN>, production: 88),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 88));

*action-table*[161] :=
  // INTERFACE-CLAUSE -> <POINTER-TOKEN> <STRING-LITERAL-TOKEN> POINTER-OPTION-LIST *
  make-action-table(make(<reduce>, on: <END-TOKEN>, production: 85),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 85));

*action-table*[162] :=
  // INTERFACE-CLAUSE -> <POINTER-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> * <IDENTIFIER-TOKEN> POINTER-OPTION-LIST
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 163));

*action-table*[163] :=
  // INTERFACE-CLAUSE -> <POINTER-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> * POINTER-OPTION-LIST
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 79),
                    make(<reduce>, on: <END-TOKEN>, production: 87),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 87));

*action-table*[164] :=
  // INTERFACE-CLAUSE -> <POINTER-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> POINTER-OPTION-LIST *
  make-action-table(make(<reduce>, on: <END-TOKEN>, production: 86),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 86));

*action-table*[165] :=
  // INTERFACE-CLAUSE -> <CONSTANT-TOKEN> * <STRING-LITERAL-TOKEN> CONSTANT-OPTION-LIST
  // INTERFACE-CLAUSE -> <CONSTANT-TOKEN> * <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> CONSTANT-OPTION-LIST
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 166));

*action-table*[166] :=
  // INTERFACE-CLAUSE -> <CONSTANT-TOKEN> <STRING-LITERAL-TOKEN> * <ARROW-TOKEN> <IDENTIFIER-TOKEN> CONSTANT-OPTION-LIST
  // INTERFACE-CLAUSE -> <CONSTANT-TOKEN> <STRING-LITERAL-TOKEN> * CONSTANT-OPTION-LIST
  make-action-table(make(<shift>, on: <ARROW-TOKEN>, state: 172),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 91),
                    make(<reduce>, on: <END-TOKEN>, production: 91),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 91));

*action-table*[167] :=
  // CONSTANT-OPTION-LIST -> CONSTANT-OPTION-LIST * <COMMA-TOKEN> CONSTANT-OPTION
  // INTERFACE-CLAUSE -> <CONSTANT-TOKEN> <STRING-LITERAL-TOKEN> CONSTANT-OPTION-LIST *
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 168),
                    make(<reduce>, on: <END-TOKEN>, production: 89),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 89));

*action-table*[168] :=
  // CONSTANT-OPTION-LIST -> CONSTANT-OPTION-LIST <COMMA-TOKEN> * CONSTANT-OPTION
  make-action-table(make(<shift>, on: <VALUE-TOKEN>, state: 169));

*action-table*[169] :=
  // CONSTANT-OPTION -> <VALUE-TOKEN> * <LITERAL-TOKEN>
  make-action-table(make(<shift>, on: <LITERAL-TOKEN>, state: 170));

*action-table*[170] :=
  // CONSTANT-OPTION -> <VALUE-TOKEN> <LITERAL-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 93),
                    make(<reduce>, on: <END-TOKEN>, production: 93),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 93));

*action-table*[171] :=
  // CONSTANT-OPTION-LIST -> CONSTANT-OPTION-LIST <COMMA-TOKEN> CONSTANT-OPTION *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 92),
                    make(<reduce>, on: <END-TOKEN>, production: 92),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 92));

*action-table*[172] :=
  // INTERFACE-CLAUSE -> <CONSTANT-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> * <IDENTIFIER-TOKEN> CONSTANT-OPTION-LIST
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 173));

*action-table*[173] :=
  // INTERFACE-CLAUSE -> <CONSTANT-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> * CONSTANT-OPTION-LIST
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 91),
                    make(<reduce>, on: <END-TOKEN>, production: 91),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 91));

*action-table*[174] :=
  // CONSTANT-OPTION-LIST -> CONSTANT-OPTION-LIST * <COMMA-TOKEN> CONSTANT-OPTION
  // INTERFACE-CLAUSE -> <CONSTANT-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> CONSTANT-OPTION-LIST *
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 168),
                    make(<reduce>, on: <END-TOKEN>, production: 90),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 90));

*action-table*[175] :=
  // INTERFACE-CLAUSE -> <VARIABLE-TOKEN> * <STRING-LITERAL-TOKEN> VARIABLE-OPTION-LIST
  // INTERFACE-CLAUSE -> <VARIABLE-TOKEN> * <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> VARIABLE-OPTION-LIST
  make-action-table(make(<shift>, on: <STRING-LITERAL-TOKEN>, state: 176));

*action-table*[176] :=
  // INTERFACE-CLAUSE -> <VARIABLE-TOKEN> <STRING-LITERAL-TOKEN> * <ARROW-TOKEN> <IDENTIFIER-TOKEN> VARIABLE-OPTION-LIST
  // INTERFACE-CLAUSE -> <VARIABLE-TOKEN> <STRING-LITERAL-TOKEN> * VARIABLE-OPTION-LIST
  make-action-table(make(<shift>, on: <ARROW-TOKEN>, state: 191),
                    make(<reduce>, on: <COMMA-TOKEN>, production: 96),
                    make(<reduce>, on: <END-TOKEN>, production: 96),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 96));

*action-table*[177] :=
  // VARIABLE-OPTION-LIST -> VARIABLE-OPTION-LIST * <COMMA-TOKEN> VARIABLE-OPTION
  // INTERFACE-CLAUSE -> <VARIABLE-TOKEN> <STRING-LITERAL-TOKEN> VARIABLE-OPTION-LIST *
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 178),
                    make(<reduce>, on: <END-TOKEN>, production: 94),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 94));

*action-table*[178] :=
  // VARIABLE-OPTION-LIST -> VARIABLE-OPTION-LIST <COMMA-TOKEN> * VARIABLE-OPTION
  make-action-table(make(<shift>, on: <EQUATE-TOKEN>, state: 185),
                    make(<shift>, on: <GETTER-TOKEN>, state: 187),
                    make(<shift>, on: <MAPPING-TOKEN>, state: 183),
                    make(<shift>, on: <READ-ONLY-TOKEN>, state: 181),
                    make(<shift>, on: <SETTER-TOKEN>, state: 179));

*action-table*[179] :=
  // VARIABLE-OPTION -> <SETTER-TOKEN> * <IDENTIFIER-TOKEN>
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 180));

*action-table*[180] :=
  // VARIABLE-OPTION -> <SETTER-TOKEN> <IDENTIFIER-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 99),
                    make(<reduce>, on: <END-TOKEN>, production: 99),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 99));

*action-table*[181] :=
  // VARIABLE-OPTION -> <READ-ONLY-TOKEN> * <BOOLEAN-TOKEN>
  make-action-table(make(<shift>, on: <BOOLEAN-TOKEN>, state: 182));

*action-table*[182] :=
  // VARIABLE-OPTION -> <READ-ONLY-TOKEN> <BOOLEAN-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 100),
                    make(<reduce>, on: <END-TOKEN>, production: 100),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 100));

*action-table*[183] :=
  // VARIABLE-OPTION -> <MAPPING-TOKEN> * <IDENTIFIER-TOKEN>
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 184));

*action-table*[184] :=
  // VARIABLE-OPTION -> <MAPPING-TOKEN> <IDENTIFIER-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 101),
                    make(<reduce>, on: <END-TOKEN>, production: 101),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 101));

*action-table*[185] :=
  // VARIABLE-OPTION -> <EQUATE-TOKEN> * <IDENTIFIER-TOKEN>
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 186));

*action-table*[186] :=
  // VARIABLE-OPTION -> <EQUATE-TOKEN> <IDENTIFIER-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 102),
                    make(<reduce>, on: <END-TOKEN>, production: 102),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 102));

*action-table*[187] :=
  // VARIABLE-OPTION -> <GETTER-TOKEN> * <IDENTIFIER-TOKEN>
  // VARIABLE-OPTION -> <GETTER-TOKEN> * SEALING
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 189),
                    make(<shift>, on: <INLINE-TOKEN>, state: 88),
                    make(<shift>, on: <OPEN-TOKEN>, state: 87),
                    make(<shift>, on: <SEALED-TOKEN>, state: 86));

*action-table*[188] :=
  // VARIABLE-OPTION -> <GETTER-TOKEN> SEALING *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 103),
                    make(<reduce>, on: <END-TOKEN>, production: 103),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 103));

*action-table*[189] :=
  // VARIABLE-OPTION -> <GETTER-TOKEN> <IDENTIFIER-TOKEN> *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 98),
                    make(<reduce>, on: <END-TOKEN>, production: 98),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 98));

*action-table*[190] :=
  // VARIABLE-OPTION-LIST -> VARIABLE-OPTION-LIST <COMMA-TOKEN> VARIABLE-OPTION *
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 97),
                    make(<reduce>, on: <END-TOKEN>, production: 97),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 97));

*action-table*[191] :=
  // INTERFACE-CLAUSE -> <VARIABLE-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> * <IDENTIFIER-TOKEN> VARIABLE-OPTION-LIST
  make-action-table(make(<shift>, on: <IDENTIFIER-TOKEN>, state: 192));

*action-table*[192] :=
  // INTERFACE-CLAUSE -> <VARIABLE-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> * VARIABLE-OPTION-LIST
  make-action-table(make(<reduce>, on: <COMMA-TOKEN>, production: 96),
                    make(<reduce>, on: <END-TOKEN>, production: 96),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 96));

*action-table*[193] :=
  // VARIABLE-OPTION-LIST -> VARIABLE-OPTION-LIST * <COMMA-TOKEN> VARIABLE-OPTION
  // INTERFACE-CLAUSE -> <VARIABLE-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> VARIABLE-OPTION-LIST *
  make-action-table(make(<shift>, on: <COMMA-TOKEN>, state: 178),
                    make(<reduce>, on: <END-TOKEN>, production: 95),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 95));

*action-table*[194] :=
  // INTERFACE-CLAUSE-LIST1 -> INTERFACE-CLAUSE *
  make-action-table(make(<reduce>, on: <END-TOKEN>, production: 8),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 8));

*action-table*[195] :=
  // INTERFACE-CLAUSE-LIST -> INTERFACE-CLAUSE-LIST1 * <SEMICOLON-TOKEN>
  // INTERFACE-CLAUSE-LIST1 -> INTERFACE-CLAUSE-LIST1 * <SEMICOLON-TOKEN> INTERFACE-CLAUSE
  // INTERFACE-CLAUSE-LIST -> INTERFACE-CLAUSE-LIST1 *
  make-action-table(make(<reduce>, on: <END-TOKEN>, production: 6),
                    make(<shift>, on: <SEMICOLON-TOKEN>, state: 196));

*action-table*[196] :=
  // INTERFACE-CLAUSE-LIST1 -> INTERFACE-CLAUSE-LIST1 <SEMICOLON-TOKEN> * INTERFACE-CLAUSE
  // INTERFACE-CLAUSE-LIST -> INTERFACE-CLAUSE-LIST1 <SEMICOLON-TOKEN> *
  make-action-table(make(<shift>, on: <CONSTANT-TOKEN>, state: 165),
                    make(<reduce>, on: <END-TOKEN>, production: 5),
                    make(<shift>, on: <FUNCTION-TOKEN>, state: 99),
                    make(<shift>, on: <INCLUDE-TOKEN>, state: 3),
                    make(<shift>, on: <POINTER-TOKEN>, state: 156),
                    make(<shift>, on: <STRUCT-TOKEN>, state: 134),
                    make(<shift>, on: <UNION-TOKEN>, state: 150),
                    make(<shift>, on: <VARIABLE-TOKEN>, state: 175));

*action-table*[197] :=
  // INTERFACE-CLAUSE-LIST1 -> INTERFACE-CLAUSE-LIST1 <SEMICOLON-TOKEN> INTERFACE-CLAUSE *
  make-action-table(make(<reduce>, on: <END-TOKEN>, production: 7),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 7));

*action-table*[198] :=
  // INTERFACE-DEF -> <DEFINE-TOKEN> <INTERFACE-TOKEN> INTERFACE-CLAUSE-LIST * <END-TOKEN> <INTERFACE-TOKEN>
  // INTERFACE-DEF -> <DEFINE-TOKEN> <INTERFACE-TOKEN> INTERFACE-CLAUSE-LIST * <END-TOKEN>
  make-action-table(make(<shift>, on: <END-TOKEN>, state: 199));

*action-table*[199] :=
  // INTERFACE-DEF -> <DEFINE-TOKEN> <INTERFACE-TOKEN> INTERFACE-CLAUSE-LIST <END-TOKEN> *
  // INTERFACE-DEF -> <DEFINE-TOKEN> <INTERFACE-TOKEN> INTERFACE-CLAUSE-LIST <END-TOKEN> * <INTERFACE-TOKEN>
  make-action-table(make(<shift>, on: <INTERFACE-TOKEN>, state: 200),
                    make(<reduce>, on: <SEMICOLON-TOKEN>, production: 4),
                    make(<reduce>, on: <TRUE-EOF-TOKEN>, production: 4));

*action-table*[200] :=
  // INTERFACE-DEF -> <DEFINE-TOKEN> <INTERFACE-TOKEN> INTERFACE-CLAUSE-LIST <END-TOKEN> <INTERFACE-TOKEN> *
  make-action-table(make(<reduce>, on: <SEMICOLON-TOKEN>, production: 3),
                    make(<reduce>, on: <TRUE-EOF-TOKEN>, production: 3));

*action-table*[201] :=
  // PARSE-ROOT -> INTERFACE-DEF * <SEMICOLON-TOKEN>
  // PARSE-ROOT -> INTERFACE-DEF * <TRUE-EOF-TOKEN>
  make-action-table(make(<shift>, on: <SEMICOLON-TOKEN>, state: 203),
                    make(<shift>, on: <TRUE-EOF-TOKEN>, state: 202));

*action-table*[202] :=
  // PARSE-ROOT -> INTERFACE-DEF <TRUE-EOF-TOKEN> *
  make-action-table(make(<reduce>, on: <EOF-TOKEN>, production: 2));

*action-table*[203] :=
  // PARSE-ROOT -> INTERFACE-DEF <SEMICOLON-TOKEN> *
  make-action-table(make(<reduce>, on: <EOF-TOKEN>, production: 1));

*action-table*[204] :=
  // S-PRIME -> PARSE-ROOT *
  make-action-table(make(<accept>, on: <EOF-TOKEN>));

*production-table*[1] :=
  method (state-stack, symbol-stack, #key $state)
    // PARSE-ROOT -> INTERFACE-DEF <SEMICOLON-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(204,
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
             pair(204,
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
             pair(201,
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
             pair(201,
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
             pair(198,
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
             pair(198,
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
             pair(195,
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
             pair(195,
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
                    2 => 194;
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

*production-table*[10] :=
  method (state-stack, symbol-stack, #key $state)
    // FILE-CLAUSE -> <INCLUDE-TOKEN> <STRING-LITERAL-TOKEN> FILE-OPTION-LIST
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(98,
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
                      if ($state.include-files) 
                        parse-error($r1, "More than one #include in interface definition.")
                      end if;
                      $state.include-files := vector($r2.value);
                      $state.container-options := process-container-options($r3);
                  end,
                  temp1);
           end);
  end;

*production-table*[11] :=
  method (state-stack, symbol-stack, #key $state)
    // FILE-CLAUSE -> <INCLUDE-TOKEN> <LBRACE-TOKEN> INCLUDE-FILE-LIST <RBRACE-TOKEN> FILE-OPTION-LIST
    values(begin
             let poped-state-stack = tail(tail(tail(tail(tail(state-stack)))));
             pair(98,
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
                      if ($state.include-files) 
                        parse-error($r1, "More than one #include in interface definition.")
                      end if;
                      $state.include-files := as(<simple-object-vector>, $r3);
                      $state.container-options := process-container-options($r5);
                  end,
                  temp1);
           end);
  end;

*production-table*[12] :=
  method (state-stack, symbol-stack, #key $state)
    // INCLUDE-FILE-LIST -> <STRING-LITERAL-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(6,
                  poped-state-stack);
           end,
           begin
             let $r1 = head(symbol-stack);
             let temp1 = tail(symbol-stack);
             pair(begin
                      make(<stretchy-vector>, size: 1, fill: $r1.value);
                  end,
                  temp1);
           end);
  end;

*production-table*[13] :=
  method (state-stack, symbol-stack, #key $state)
    // INCLUDE-FILE-LIST -> INCLUDE-FILE-LIST <COMMA-TOKEN> <STRING-LITERAL-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(6,
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
                      add!($r1, $r3.value);
                  end,
                  temp1);
           end);
  end;

*production-table*[14] :=
  method (state-stack, symbol-stack, #key $state)
    // FILE-OPTION-LIST -> epsilon
    values(begin
             let poped-state-stack = state-stack;
             pair(select (head(poped-state-stack))
                    7 => 8;
                    OTHERWISE => 97;
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

*production-table*[15] :=
  method (state-stack, symbol-stack, #key $state)
    // FILE-OPTION-LIST -> FILE-OPTION-LIST <COMMA-TOKEN> FILE-OPTION
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    7 => 8;
                    OTHERWISE => 97;
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

*production-table*[16] :=
  method (state-stack, symbol-stack, #key $state)
    // FILE-OPTION -> <OBJECT-FILE-TOKEN> <STRING-LITERAL-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(93,
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

*production-table*[17] :=
  method (state-stack, symbol-stack, #key $state)
    // FILE-OPTION -> <MINDY-INC-TOKEN> <STRING-LITERAL-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(93,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      if ($state.mindy-include-file) 
                        parse-error($r1,
                                    "More than one mindy-include-file: in interface definition.")
                      end if;
                      pair(#"mindy-file", $state.mindy-include-file := $r2.value);
                  end,
                  temp1);
           end);
  end;

*production-table*[18] :=
  method (state-stack, symbol-stack, #key $state)
    // FILE-OPTION -> <DEFINE-MACRO-TOKEN> <LBRACE-TOKEN> MACRO-DEFINITIONS <RBRACE-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(state-stack))));
             pair(93,
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

*production-table*[19] :=
  method (state-stack, symbol-stack, #key $state)
    // FILE-OPTION -> <UNDEFINE-TOKEN> <LBRACE-TOKEN> MACRO-UNDEFINITIONS <RBRACE-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(state-stack))));
             pair(93,
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

*production-table*[20] :=
  method (state-stack, symbol-stack, #key $state)
    // FILE-OPTION -> CONTAINER-OPTION
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(93,
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
    // MACRO-DEFINITIONS -> MACRO-DEFINITION
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(21,
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

*production-table*[22] :=
  method (state-stack, symbol-stack, #key $state)
    // MACRO-DEFINITIONS -> MACRO-DEFINITIONS <COMMA-TOKEN> MACRO-DEFINITION
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(21,
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

*production-table*[23] :=
  method (state-stack, symbol-stack, #key $state)
    // MACRO-DEFINITION -> <STRING-LITERAL-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    15 => 20;
                    OTHERWISE => 24;
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

*production-table*[24] :=
  method (state-stack, symbol-stack, #key $state)
    // MACRO-DEFINITION -> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <STRING-LITERAL-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    15 => 20;
                    OTHERWISE => 24;
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

*production-table*[25] :=
  method (state-stack, symbol-stack, #key $state)
    // MACRO-DEFINITION -> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <INTEGER-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    15 => 20;
                    OTHERWISE => 24;
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

*production-table*[26] :=
  method (state-stack, symbol-stack, #key $state)
    // MACRO-UNDEFINITIONS -> MACRO-UNDEFINITION
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(29,
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

*production-table*[27] :=
  method (state-stack, symbol-stack, #key $state)
    // MACRO-UNDEFINITIONS -> MACRO-UNDEFINITIONS <COMMA-TOKEN> MACRO-UNDEFINITION
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(29,
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

*production-table*[28] :=
  method (state-stack, symbol-stack, #key $state)
    // MACRO-UNDEFINITION -> <STRING-LITERAL-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    26 => 28;
                    OTHERWISE => 32;
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

*production-table*[29] :=
  method (state-stack, symbol-stack, #key $state)
    // CONTAINER-OPTION -> <NAME-MAPPER-TOKEN> <IDENTIFIER-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(select (head(poped-state-stack))
                    9 => 92;
                    OTHERWISE => 138;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      list(#"name-mapper", $r2.value);
                  end,
                  temp1);
           end);
  end;

*production-table*[30] :=
  method (state-stack, symbol-stack, #key $state)
    // CONTAINER-OPTION -> <IMPORT-TOKEN> <ALL-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(select (head(poped-state-stack))
                    9 => 92;
                    OTHERWISE => 138;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      list(#"import", #"all");
                  end,
                  temp1);
           end);
  end;

*production-table*[31] :=
  method (state-stack, symbol-stack, #key $state)
    // CONTAINER-OPTION -> <IMPORT-TOKEN> <ALL-RECURSIVE-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(select (head(poped-state-stack))
                    9 => 92;
                    OTHERWISE => 138;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      list(#"import", #"all-recursive");
                  end,
                  temp1);
           end);
  end;

*production-table*[32] :=
  method (state-stack, symbol-stack, #key $state)
    // CONTAINER-OPTION -> <IMPORT-TOKEN> <LBRACE-TOKEN> IMPORT-LIST <RBRACE-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(state-stack))));
             pair(select (head(poped-state-stack))
                    9 => 92;
                    OTHERWISE => 138;
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
                      list(#"import", $r3);
                  end,
                  temp1);
           end);
  end;

*production-table*[33] :=
  method (state-stack, symbol-stack, #key $state)
    // CONTAINER-OPTION -> <IMPORT-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <ALL-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(state-stack))));
             pair(select (head(poped-state-stack))
                    9 => 92;
                    OTHERWISE => 138;
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
                      list(#"import-file", $r2, #"all");
                  end,
                  temp1);
           end);
  end;

*production-table*[34] :=
  method (state-stack, symbol-stack, #key $state)
    // CONTAINER-OPTION -> <IMPORT-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <NONE-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(state-stack))));
             pair(select (head(poped-state-stack))
                    9 => 92;
                    OTHERWISE => 138;
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
                      list(#"import-file", $r2, #"none");
                  end,
                  temp1);
           end);
  end;

*production-table*[35] :=
  method (state-stack, symbol-stack, #key $state)
    // CONTAINER-OPTION -> <IMPORT-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <LBRACE-TOKEN> IMPORT-LIST <RBRACE-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(tail(tail(state-stack))))));
             pair(select (head(poped-state-stack))
                    9 => 92;
                    OTHERWISE => 138;
                  end,
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
                      list(#"import-file", $r2, $r5);
                  end,
                  temp1);
           end);
  end;

*production-table*[36] :=
  method (state-stack, symbol-stack, #key $state)
    // IMPORT-LIST -> epsilon
    values(begin
             let poped-state-stack = state-stack;
             pair(select (head(poped-state-stack))
                    38 => 44;
                    OTHERWISE => 51;
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

*production-table*[37] :=
  method (state-stack, symbol-stack, #key $state)
    // IMPORT-LIST -> IMPORT-LIST <COMMA-TOKEN> IMPORT
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    38 => 44;
                    OTHERWISE => 51;
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

*production-table*[38] :=
  method (state-stack, symbol-stack, #key $state)
    // IMPORT-LIST -> IMPORT
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    38 => 44;
                    OTHERWISE => 51;
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

*production-table*[39] :=
  method (state-stack, symbol-stack, #key $state)
    // IMPORT -> <STRING-LITERAL-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    46 => 47;
                    OTHERWISE => 43;
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

*production-table*[40] :=
  method (state-stack, symbol-stack, #key $state)
    // IMPORT -> RENAMING
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    46 => 47;
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

*production-table*[41] :=
  method (state-stack, symbol-stack, #key $state)
    // RENAMING -> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    38 => 42;
                    46 => 42;
                    50 => 42;
                    75 => 76;
                    OTHERWISE => 72;
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

*production-table*[42] :=
  method (state-stack, symbol-stack, #key $state)
    // CONTAINER-OPTION -> <PREFIX-TOKEN> <STRING-LITERAL-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(select (head(poped-state-stack))
                    9 => 92;
                    OTHERWISE => 138;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      list(#"prefix", $r2.value);
                  end,
                  temp1);
           end);
  end;

*production-table*[43] :=
  method (state-stack, symbol-stack, #key $state)
    // CONTAINER-OPTION -> <EXCLUDE-TOKEN> <LBRACE-TOKEN> EXCLUDE-LIST <RBRACE-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(state-stack))));
             pair(select (head(poped-state-stack))
                    9 => 92;
                    OTHERWISE => 138;
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
                      list(#"exclude", $r3);
                  end,
                  temp1);
           end);
  end;

*production-table*[44] :=
  method (state-stack, symbol-stack, #key $state)
    // EXCLUDE-LIST -> epsilon
    values(begin
             let poped-state-stack = state-stack;
             pair(select (head(poped-state-stack))
                    58 => 60;
                    OTHERWISE => 66;
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

*production-table*[45] :=
  method (state-stack, symbol-stack, #key $state)
    // EXCLUDE-LIST -> EXCLUDE-LIST <COMMA-TOKEN> <STRING-LITERAL-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    58 => 60;
                    OTHERWISE => 66;
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

*production-table*[46] :=
  method (state-stack, symbol-stack, #key $state)
    // EXCLUDE-LIST -> <STRING-LITERAL-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    58 => 60;
                    OTHERWISE => 66;
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

*production-table*[47] :=
  method (state-stack, symbol-stack, #key $state)
    // CONTAINER-OPTION -> <EXCLUDE-FILE-TOKEN> <STRING-LITERAL-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(select (head(poped-state-stack))
                    9 => 92;
                    OTHERWISE => 138;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      list(#"exclude-file", list($r2));
                  end,
                  temp1);
           end);
  end;

*production-table*[48] :=
  method (state-stack, symbol-stack, #key $state)
    // CONTAINER-OPTION -> <EXCLUDE-FILE-TOKEN> <LBRACE-TOKEN> EXCLUDE-LIST <RBRACE-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(state-stack))));
             pair(select (head(poped-state-stack))
                    9 => 92;
                    OTHERWISE => 138;
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
                      list(#"exclude-file", $r3);
                  end,
                  temp1);
           end);
  end;

*production-table*[49] :=
  method (state-stack, symbol-stack, #key $state)
    // CONTAINER-OPTION -> <RENAME-TOKEN> <LBRACE-TOKEN> RENAMING-LIST <RBRACE-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(state-stack))));
             pair(select (head(poped-state-stack))
                    9 => 92;
                    OTHERWISE => 138;
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
                      list(#"rename", $r3);
                  end,
                  temp1);
           end);
  end;

*production-table*[50] :=
  method (state-stack, symbol-stack, #key $state)
    // RENAMING-LIST -> epsilon
    values(begin
             let poped-state-stack = state-stack;
             pair(select (head(poped-state-stack))
                    70 => 73;
                    78 => 79;
                    OTHERWISE => 83;
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

*production-table*[51] :=
  method (state-stack, symbol-stack, #key $state)
    // RENAMING-LIST -> RENAMING-LIST <COMMA-TOKEN> RENAMING
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    70 => 73;
                    78 => 79;
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
                      pair($r3, $r1);
                  end,
                  temp1);
           end);
  end;

*production-table*[52] :=
  method (state-stack, symbol-stack, #key $state)
    // RENAMING-LIST -> RENAMING
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    70 => 73;
                    78 => 79;
                    OTHERWISE => 83;
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

*production-table*[53] :=
  method (state-stack, symbol-stack, #key $state)
    // CONTAINER-OPTION -> <MAPPING-TOKEN> <LBRACE-TOKEN> RENAMING-LIST <RBRACE-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(state-stack))));
             pair(select (head(poped-state-stack))
                    9 => 92;
                    OTHERWISE => 138;
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
                      list(#"mapping", $r3);
                  end,
                  temp1);
           end);
  end;

*production-table*[54] :=
  method (state-stack, symbol-stack, #key $state)
    // CONTAINER-OPTION -> <EQUATE-TOKEN> <LBRACE-TOKEN> RENAMING-LIST <RBRACE-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(state-stack))));
             pair(select (head(poped-state-stack))
                    9 => 92;
                    OTHERWISE => 138;
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
                      list(#"equate", $r3);
                  end,
                  temp1);
           end);
  end;

*production-table*[55] :=
  method (state-stack, symbol-stack, #key $state)
    // SEALING -> <SEALED-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    85 => 89;
                    OTHERWISE => 188;
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

*production-table*[56] :=
  method (state-stack, symbol-stack, #key $state)
    // SEALING -> <OPEN-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    85 => 89;
                    OTHERWISE => 188;
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

*production-table*[57] :=
  method (state-stack, symbol-stack, #key $state)
    // SEALING -> <INLINE-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    85 => 89;
                    OTHERWISE => 188;
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

*production-table*[58] :=
  method (state-stack, symbol-stack, #key $state)
    // CONTAINER-OPTION -> <SEAL-FUNCTIONS-TOKEN> SEALING
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(select (head(poped-state-stack))
                    9 => 92;
                    OTHERWISE => 138;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      list(#"seal", $r2);
                  end,
                  temp1);
           end);
  end;

*production-table*[59] :=
  method (state-stack, symbol-stack, #key $state)
    // CONTAINER-OPTION -> <READ-ONLY-TOKEN> <BOOLEAN-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(select (head(poped-state-stack))
                    9 => 92;
                    OTHERWISE => 138;
                  end,
                  poped-state-stack);
           end,
           begin
             let $r2 = head(symbol-stack);
             let temp2 = tail(symbol-stack);
             let $r1 = head(temp2);
             let temp1 = tail(temp2);
             pair(begin
                      list(#"read-only", $r2.value);
                  end,
                  temp1);
           end);
  end;

*production-table*[60] :=
  method (state-stack, symbol-stack, #key $state)
    // INTERFACE-CLAUSE -> <FUNCTION-TOKEN> <STRING-LITERAL-TOKEN> FUNCTION-OPTION-LIST
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    2 => 194;
                    OTHERWISE => 197;
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

*production-table*[61] :=
  method (state-stack, symbol-stack, #key $state)
    // INTERFACE-CLAUSE -> <FUNCTION-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> FUNCTION-OPTION-LIST
    values(begin
             let poped-state-stack = tail(tail(tail(tail(tail(state-stack)))));
             pair(select (head(poped-state-stack))
                    2 => 194;
                    OTHERWISE => 197;
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

*production-table*[62] :=
  method (state-stack, symbol-stack, #key $state)
    // FUNCTION-OPTION-LIST -> epsilon
    values(begin
             let poped-state-stack = state-stack;
             pair(select (head(poped-state-stack))
                    100 => 101;
                    OTHERWISE => 133;
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

*production-table*[63] :=
  method (state-stack, symbol-stack, #key $state)
    // FUNCTION-OPTION-LIST -> FUNCTION-OPTION-LIST <COMMA-TOKEN> FUNCTION-OPTION
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    100 => 101;
                    OTHERWISE => 133;
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

*production-table*[64] :=
  method (state-stack, symbol-stack, #key $state)
    // FUNCTION-OPTION -> <MAP-RESULT-TOKEN> <IDENTIFIER-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(130,
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

*production-table*[65] :=
  method (state-stack, symbol-stack, #key $state)
    // FUNCTION-OPTION -> <EQUATE-RESULT-TOKEN> <IDENTIFIER-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(130,
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

*production-table*[66] :=
  method (state-stack, symbol-stack, #key $state)
    // FUNCTION-OPTION -> <IGNORE-RESULT-TOKEN> <BOOLEAN-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(130,
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

*production-table*[67] :=
  method (state-stack, symbol-stack, #key $state)
    // FUNCTION-OPTION -> <MAP-ARGUMENT-TOKEN> <LBRACE-TOKEN> ARGUMENT <ARROW-TOKEN> <IDENTIFIER-TOKEN> <RBRACE-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(tail(tail(state-stack))))));
             pair(130,
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

*production-table*[68] :=
  method (state-stack, symbol-stack, #key $state)
    // FUNCTION-OPTION -> <EQUATE-ARGUMENT-TOKEN> <LBRACE-TOKEN> ARGUMENT <ARROW-TOKEN> <IDENTIFIER-TOKEN> <RBRACE-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(tail(tail(state-stack))))));
             pair(130,
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

*production-table*[69] :=
  method (state-stack, symbol-stack, #key $state)
    // FUNCTION-OPTION -> <INPUT-ARGUMENT-TOKEN> ARGUMENT
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(130,
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

*production-table*[70] :=
  method (state-stack, symbol-stack, #key $state)
    // FUNCTION-OPTION -> <INPUT-OUTPUT-ARGUMENT-TOKEN> ARGUMENT
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(130,
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

*production-table*[71] :=
  method (state-stack, symbol-stack, #key $state)
    // FUNCTION-OPTION -> <OUTPUT-ARGUMENT-TOKEN> ARGUMENT
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(130,
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

*production-table*[72] :=
  method (state-stack, symbol-stack, #key $state)
    // ARGUMENT -> <INTEGER-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    110 => 114;
                    119 => 120;
                    124 => 125;
                    126 => 127;
                    OTHERWISE => 129;
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
    // ARGUMENT -> <IDENTIFIER-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    110 => 114;
                    119 => 120;
                    124 => 125;
                    126 => 127;
                    OTHERWISE => 129;
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

*production-table*[74] :=
  method (state-stack, symbol-stack, #key $state)
    // ARGUMENT -> <STRING-LITERAL-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(select (head(poped-state-stack))
                    110 => 114;
                    119 => 120;
                    124 => 125;
                    126 => 127;
                    OTHERWISE => 129;
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

*production-table*[75] :=
  method (state-stack, symbol-stack, #key $state)
    // INTERFACE-CLAUSE -> <STRUCT-TOKEN> <STRING-LITERAL-TOKEN> STRUCTURE-OPTION-LIST
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    2 => 194;
                    OTHERWISE => 197;
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

*production-table*[76] :=
  method (state-stack, symbol-stack, #key $state)
    // INTERFACE-CLAUSE -> <STRUCT-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> STRUCTURE-OPTION-LIST
    values(begin
             let poped-state-stack = tail(tail(tail(tail(tail(state-stack)))));
             pair(select (head(poped-state-stack))
                    2 => 194;
                    OTHERWISE => 197;
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

*production-table*[77] :=
  method (state-stack, symbol-stack, #key $state)
    // INTERFACE-CLAUSE -> <UNION-TOKEN> <STRING-LITERAL-TOKEN> STRUCTURE-OPTION-LIST
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    2 => 194;
                    OTHERWISE => 197;
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

*production-table*[78] :=
  method (state-stack, symbol-stack, #key $state)
    // INTERFACE-CLAUSE -> <UNION-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> STRUCTURE-OPTION-LIST
    values(begin
             let poped-state-stack = tail(tail(tail(tail(tail(state-stack)))));
             pair(select (head(poped-state-stack))
                    2 => 194;
                    OTHERWISE => 197;
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

*production-table*[79] :=
  method (state-stack, symbol-stack, #key $state)
    // STRUCTURE-OPTION-LIST -> epsilon
    values(begin
             let poped-state-stack = state-stack;
             pair(select (head(poped-state-stack))
                    135 => 136;
                    148 => 149;
                    151 => 152;
                    154 => 155;
                    OTHERWISE => 158;
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

*production-table*[80] :=
  method (state-stack, symbol-stack, #key $state)
    // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST <COMMA-TOKEN> CONTAINER-OPTION
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    135 => 136;
                    148 => 149;
                    151 => 152;
                    154 => 155;
                    OTHERWISE => 158;
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

*production-table*[81] :=
  method (state-stack, symbol-stack, #key $state)
    // SUPERCLASS-LIST -> <IDENTIFIER-TOKEN>
    values(begin
             let poped-state-stack = tail(state-stack);
             pair(142,
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

*production-table*[82] :=
  method (state-stack, symbol-stack, #key $state)
    // SUPERCLASS-LIST -> SUPERCLASS-LIST <COMMA-TOKEN> <IDENTIFIER-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(142,
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

*production-table*[83] :=
  method (state-stack, symbol-stack, #key $state)
    // SUPERCLASS-OPTION -> <SUPERCLASS-TOKEN> <LBRACE-TOKEN> SUPERCLASS-LIST <RBRACE-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(tail(tail(state-stack))));
             pair(select (head(poped-state-stack))
                    137 => 146;
                    OTHERWISE => 160;
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

*production-table*[84] :=
  method (state-stack, symbol-stack, #key $state)
    // STRUCTURE-OPTION-LIST -> STRUCTURE-OPTION-LIST <COMMA-TOKEN> SUPERCLASS-OPTION
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    135 => 136;
                    148 => 149;
                    151 => 152;
                    154 => 155;
                    OTHERWISE => 158;
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

*production-table*[85] :=
  method (state-stack, symbol-stack, #key $state)
    // INTERFACE-CLAUSE -> <POINTER-TOKEN> <STRING-LITERAL-TOKEN> POINTER-OPTION-LIST
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    2 => 194;
                    OTHERWISE => 197;
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

*production-table*[86] :=
  method (state-stack, symbol-stack, #key $state)
    // INTERFACE-CLAUSE -> <POINTER-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> POINTER-OPTION-LIST
    values(begin
             let poped-state-stack = tail(tail(tail(tail(tail(state-stack)))));
             pair(select (head(poped-state-stack))
                    2 => 194;
                    OTHERWISE => 197;
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

*production-table*[87] :=
  method (state-stack, symbol-stack, #key $state)
    // POINTER-OPTION-LIST -> epsilon
    values(begin
             let poped-state-stack = state-stack;
             pair(select (head(poped-state-stack))
                    157 => 161;
                    OTHERWISE => 164;
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
    // POINTER-OPTION-LIST -> STRUCTURE-OPTION-LIST <COMMA-TOKEN> SUPERCLASS-OPTION
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    157 => 161;
                    OTHERWISE => 164;
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
    // INTERFACE-CLAUSE -> <CONSTANT-TOKEN> <STRING-LITERAL-TOKEN> CONSTANT-OPTION-LIST
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    2 => 194;
                    OTHERWISE => 197;
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

*production-table*[90] :=
  method (state-stack, symbol-stack, #key $state)
    // INTERFACE-CLAUSE -> <CONSTANT-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> CONSTANT-OPTION-LIST
    values(begin
             let poped-state-stack = tail(tail(tail(tail(tail(state-stack)))));
             pair(select (head(poped-state-stack))
                    2 => 194;
                    OTHERWISE => 197;
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

*production-table*[91] :=
  method (state-stack, symbol-stack, #key $state)
    // CONSTANT-OPTION-LIST -> epsilon
    values(begin
             let poped-state-stack = state-stack;
             pair(select (head(poped-state-stack))
                    166 => 167;
                    OTHERWISE => 174;
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

*production-table*[92] :=
  method (state-stack, symbol-stack, #key $state)
    // CONSTANT-OPTION-LIST -> CONSTANT-OPTION-LIST <COMMA-TOKEN> CONSTANT-OPTION
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    166 => 167;
                    OTHERWISE => 174;
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

*production-table*[93] :=
  method (state-stack, symbol-stack, #key $state)
    // CONSTANT-OPTION -> <VALUE-TOKEN> <LITERAL-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(171,
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

*production-table*[94] :=
  method (state-stack, symbol-stack, #key $state)
    // INTERFACE-CLAUSE -> <VARIABLE-TOKEN> <STRING-LITERAL-TOKEN> VARIABLE-OPTION-LIST
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    2 => 194;
                    OTHERWISE => 197;
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

*production-table*[95] :=
  method (state-stack, symbol-stack, #key $state)
    // INTERFACE-CLAUSE -> <VARIABLE-TOKEN> <STRING-LITERAL-TOKEN> <ARROW-TOKEN> <IDENTIFIER-TOKEN> VARIABLE-OPTION-LIST
    values(begin
             let poped-state-stack = tail(tail(tail(tail(tail(state-stack)))));
             pair(select (head(poped-state-stack))
                    2 => 194;
                    OTHERWISE => 197;
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

*production-table*[96] :=
  method (state-stack, symbol-stack, #key $state)
    // VARIABLE-OPTION-LIST -> epsilon
    values(begin
             let poped-state-stack = state-stack;
             pair(select (head(poped-state-stack))
                    176 => 177;
                    OTHERWISE => 193;
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

*production-table*[97] :=
  method (state-stack, symbol-stack, #key $state)
    // VARIABLE-OPTION-LIST -> VARIABLE-OPTION-LIST <COMMA-TOKEN> VARIABLE-OPTION
    values(begin
             let poped-state-stack = tail(tail(tail(state-stack)));
             pair(select (head(poped-state-stack))
                    176 => 177;
                    OTHERWISE => 193;
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

*production-table*[98] :=
  method (state-stack, symbol-stack, #key $state)
    // VARIABLE-OPTION -> <GETTER-TOKEN> <IDENTIFIER-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(190,
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

*production-table*[99] :=
  method (state-stack, symbol-stack, #key $state)
    // VARIABLE-OPTION -> <SETTER-TOKEN> <IDENTIFIER-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(190,
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

*production-table*[100] :=
  method (state-stack, symbol-stack, #key $state)
    // VARIABLE-OPTION -> <READ-ONLY-TOKEN> <BOOLEAN-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(190,
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

*production-table*[101] :=
  method (state-stack, symbol-stack, #key $state)
    // VARIABLE-OPTION -> <MAPPING-TOKEN> <IDENTIFIER-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(190,
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

*production-table*[102] :=
  method (state-stack, symbol-stack, #key $state)
    // VARIABLE-OPTION -> <EQUATE-TOKEN> <IDENTIFIER-TOKEN>
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(190,
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

*production-table*[103] :=
  method (state-stack, symbol-stack, #key $state)
    // VARIABLE-OPTION -> <GETTER-TOKEN> SEALING
    values(begin
             let poped-state-stack = tail(tail(state-stack));
             pair(190,
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
