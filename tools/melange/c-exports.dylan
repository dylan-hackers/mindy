module: dylan-user
copyright: Copyright (C) 1994, 1996, Carnegie Mellon University
	   All rights reserved.
	   This code was produced by the Gwydion Project at Carnegie Mellon
	   University.  If you are interested in using this code, contact
	   "Scott.Fahlman@cs.cmu.edu" (Internet).
rcs-header: $Header: 

//======================================================================
//
// Copyright (c) 1994, 1996  Carnegie Mellon University
// All rights reserved.
//
//======================================================================

define library melange-c
  use dylan;
  use string-extensions;
  use collection-extensions;
  use regular-expressions;
  use table-extensions;
  use streams;
  use standard-io;
  use format;
  export c-lexer, c-declarations, portability;
end library melange-c;

define module c-lexer
  use dylan;
  use extensions;
  use table-extensions, exclude: {<string-table>};
  use self-organizing-list;
  use string-conversions;
  use regular-expressions;
  use substring-search;
  use character-type;
  use streams;
  create cpp-parse;
  export
    include-path, open-in-include-path, check-cpp-expansion, <tokenizer>,
    get-token, unget-token, add-typedef, cpp-table, cpp-decls, <token>, value,
    string-value, generator, parse-error, <error-token>, <identifier-token>,
    <integer-token>, <eof-token>, <begin-include-token>, <end-include-token>,
    <reserved-word-token>, <struct-token>, <typedef-token>, <name-token>,
    <int-token>, <short-token>, <long-token>, <signed-token>,
    <unsigned-token>, <char-token>, <float-token>, <double-token>,
    // <const-token>, <volatile-token>,
    <void-token>, <inline-token>,
    <extern-token>, <static-token>, <auto-token>, <register-token>,
    <type-name-token>, <union-token>, <enum-token>, <elipsis-token>,
    <sizeof-token>, <dec-op-token>, <inc-op-token>, <ptr-op-token>,
    <literal-token>, <string-literal-token>, <constant-token>,
    <mul-assign-token>, <div-assign-token>, <mod-assign-token>,
    <add-assign-token>, <sub-assign-token>, <left-assign-token>,
    <right-assign-token>, <and-assign-token>, <xor-assign-token>,
    <or-assign-token>, <semicolon-token>, <comma-token>, <lparen-token>,
    <rparen-token>, <lbracket-token>, <rbracket-token>, <dot-token>,
    <ampersand-token>, <star-token>, <slash-token>, <plus-token>,
    <minus-token>, <tilde-token>, <bang-token>, <percent-token>, <lt-token>,
    <gt-token>, <carat-token>, <bar-token>, <question-token>, <colon-token>,
    <eq-op-token>, <assign-token>, <ge-op-token>, <le-op-token>,
    <ne-op-token>, <and-op-token>, <or-op-token>, <left-op-token>,
    <right-op-token>, <lcurly-token>, <rcurly-token>, <type-specifier-token>,
    <nc-punctuation-token>, *handle-//-comments*;
end module c-lexer;

define module portability
  use dylan;
  use c-lexer, import: {include-path, *handle-//-comments*};
  use system, import: {getenv};  // win32 only
  use regular-expressions;       // win32 only			  
  export
    $default-defines,
    $enum-size,
    $pointer-size, $function-pointer-size,
    $integer-size, $short-int-size,
    $long-int-size, $char-size,
    $float-size, $double-float-size,
    $long-double-size;
end module portability;

define module c-parse
  use dylan;
  use extensions;
  use self-organizing-list;
  use c-lexer;
  use streams;
  use format;
  use standard-io;
  create
    <parse-state>, <parse-file-state>, <parse-type-state>, <parse-cpp-state>,
    <parse-macro-state>, tokenizer, verbose, verbose-setter,
    push-include-level, pop-include-level, objects, process-type-list,
    process-declarator, declare-objects, make-struct-type, c-type-size,
    add-cpp-declaration, unknown-type, <declaration>, <arg-declaration>,
    <varargs-declaration>, <enum-slot-declaration>, constant-value,
    <integer-type-declaration>, true-type, make-enum-slot;
  export
    parse, parse-type, parse-macro;
end module c-parse;

define module c-declarations
  use dylan;
  use extensions, exclude: {format};
  use table-extensions, exclude: {<string-table>};
  use regular-expressions;
  use streams;
  use format;

  // We completely encapsulate "c-parse" and only pass out the very few 
  // objects that will be needed by "define-interface".  Note that the 
  // classes are actually defined within this module but are exported
  // from c-parse.
  use c-parse, export: {<declaration>, <parse-state>, parse, parse-type,
			constant-value, true-type};

  use c-lexer;			// Tokens are used in process-type-list and
				// make-struct-type
  use portability;              // constants for size of C data types

  export
    // Basic type declarations
    <function-declaration>, <structured-type-declaration>,
    <struct-declaration>, <union-declaration>, <variable-declaration>,
    <constant-declaration>, <typedef-declaration>, <pointer-declaration>,
    <vector-declaration>,

    // Preliminary "set declaration properties phase"
    ignored?-setter, find-result, find-parameter, find-slot,
    argument-direction-setter, constant-value-setter, getter-setter,
    setter-setter, read-only-setter, sealed-string-setter, excluded?-setter,
    exclude-slots, equate, remap, rename, superclasses-setter, pointer-equiv,
    dylan-name, exclude-decl,

    // "Import declarations phase" 
    declaration-closure, // also calls compute-closure

    // "Name computation phase"
    apply-options, apply-container-options, // also calls find-dylan-name,
					    // compute-dylan-name

    // "Write declaration phase"
    write-declaration, 
    write-file-load, write-mindy-includes,

    // Miscellaneous
    getter, setter, sealed-string, excluded?,
    canonical-name,declarations,
    melange-target;
end module c-declarations;
