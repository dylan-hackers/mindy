Module:   dylan-user
Synopsis: Dylan-based Mindy Compiler
Author:   Peter S. Housel

define module forwards
  create
    <library>, <module>;
//  <ctype>,
//  <cclass>,
//  <abstract-variable>,
//  <ct-function>;
end;

define module literals
  use common-dylan, exclude: { format-to-string };
  use streams;
  use format;
  use print;
  use pprint;

  export
    <literal>, literal-value, <eql-literal>,
    <literal-number>, <literal-real>, <literal-rational>,
    <literal-general-integer>, <literal-integer>,
/*
     <literal-extended-integer>,
     <literal-ratio>,
*/
    <literal-float>, <literal-single-float>, <literal-double-float>,
    <literal-extended-float>, <literal-symbol>, <literal-character>,
    <literal-boolean>, <literal-true>, <literal-false>,
    <literal-sequence>,
    <literal-list>,
    <literal-pair>, literal-head, literal-tail,
    <literal-empty-list>,
    <literal-vector>,
    <literal-simple-object-vector>,
    <literal-string>, concat-strings;
end;

define module source
  use common-dylan, exclude: { format-to-string };
  use streams;
  use format;
  use pprint;

  export
    <source-location-mixin>, source-location,
    <source-location>, describe-source-location,
    <unknown-source-location>,

    <source>,
    <source-file>, contents, full-file-name, 

    <known-source-location>, source,
    start-posn, start-line, start-column,
    end-posn, end-line, end-column,

    extract-string;
end;

define module tokens
  use common-dylan, exclude: { format-to-string };
  use streams;
  use format;
  use print;
  use pprint;

  use source;
  use literals;
  use forwards, import: {<module>};

  export

    $eof-token,
    $error-token,
    $left-paren-token,
    $right-paren-token,
    $comma-token,
    $dot-token,
    $semicolon-token,
    $left-bracket-token,
    $right-bracket-token,
    $left-brace-token,
    $right-brace-token,
    $double-colon-token,
    $minus-token,
    $equal-token,
    $double-equal-token,
    $arrow-token,
    $sharp-paren-token,
    $sharp-bracket-token,
    $double-sharp-token,
    $question-token,
    $double-question-token,
    $question-equal-token,
    $ellipsis-token,
    $true-token,
    $false-token,
    $next-token,
    $rest-token,
    $key-token,
    $all-keys-token,
    $include-token,
    $define-token,
    $end-token,
    $handler-token,
    $let-token,
    $local-token,
    $macro-token,
    $otherwise-token,
    $raw-ordinary-word-token,
    $raw-begin-word-token,
    $raw-function-word-token,
    $ordinary-define-body-word-token,
    $begin-and-define-body-word-token,
    $function-and-define-body-word-token,
    $ordinary-define-list-word-token,
    $begin-and-define-list-word-token,
    $function-and-define-list-word-token,
    $quoted-name-token,
    $constrained-name-token,
    $tilde-token,
    $other-binary-operator-token,
    $literal-token,
    $string-token,
    $symbol-token,
    $parsed-definition-macro-call-token,
    $parsed-special-definition-token,
    $parsed-local-declaration-token,
    $parsed-expression-token,
    $parsed-constant-token,
    $parsed-macro-call-token,
    $parsed-parameter-list-token,
    $parsed-variable-list-token,
    $feature-if-token,
    $feature-elseif-token,
    $feature-else-token,
    $feature-endif-token,

    <token>, token-kind, 
    <symbol-token>, token-symbol, 
    <identifier-token>, token-module, token-uniquifier,
    <uniquifier>, same-id?,
    <operator-token>, operator-precedence, operator-associativity,
    <constrained-name-token>, token-constraint,
    <literal-token>, token-literal,
    <pre-parsed-token>, token-parse-tree,

    <syntax-table>, syntax-for-name,
    problem-with-category-merge, merge-category;
end;

define module names
  use common-dylan, exclude: { format-to-string };
  use format;
  use streams;
  use pprint;

  use forwards, import: {<module>};
  use tokens;
  use source;

  export
    <name>,
    <basic-name>, id-name, name-symbol, name-module,
    <derived-name>, derived-name-base, derived-name-how,
    <internal-name>, internal-name-symbol, internal-name-base,
    <anonymous-name>, anonymous-name-location,
    <method-name>, method-name-generic-function, method-name-specializers,
    name-unique?;
end;

define module errors
  use common-dylan, exclude: { format-to-string };
  use format;
  use streams;
  use standard-io;
  use pprint;

  use source;
  use standard-io;
  use tokens;
  export
    <compiler-condition>, condition-at,
    <compiler-warning>, <compiler-error>, *break-on-compiler-errors*,
    <fatal-error-recovery-restart>,
    note-context, end-of-context,
    compiler-warning, compiler-warning-location, *warnings*,
    compiler-error, compiler-error-location, *errors*,
    compiler-fatal-error, compiler-fatal-error-location,
    extract-source;
end module;

define module definitions
  use common-dylan, exclude: { format-to-string };
  use format;
  use streams;
  use standard-io;
  use pprint;

  use tokens;
  use source;
  use names;
  use forwards, import: {<library>};
  use errors;

  export
    <definition>, defn-name, defn-library, defn-dynamic?,
    *defn-dynamic-default*,
    definition-syntax-info, definition-kind,
    <abstract-constant-definition>, <abstract-variable-definition>,
    <implicit-definition>,
    <class-definition>,

    <function-definition>;
end;

define module variables
  use common-dylan, exclude: { format-to-string };
  use format;
  use streams;
  use standard-io;
  use pprint;

  use source;
  use errors;
  use tokens;
  use names;
  use definitions;

  use forwards, import: {<library>, <module>}, export: all;
  export
    $Dylan-Library, $Dylan-Module, *Current-Library*, *Current-Module*,

    $Bootstrap-Module, add-bootstrap-export, define-bootstrap-module,

    find-library, library-name, note-library-definition,
    find-module, module-name, module-syntax-table,
    note-module-definition, deferred-importers,
    <variable>, find-variable, variable-name, variable-definition,
    variable-transformers, variable-transformers-setter,
    variable-ct-evaluator, variable-ct-evaluator-setter,
    variable-fragment-expander, variable-fragment-expander-setter,
    note-variable-definition,
    <use>, <all-marker>, <renaming>, renaming-orig-name, renaming-new-name,

    module-home, variable-home,
    name-inherited-or-exported?,

    dylan-var, dylan-defn;
end;

define module header
  use common-dylan, exclude: { format-to-string };
  use format;
  use streams;
  use standard-io;
  use pprint;

  use source;

  export
    <header>, parse-header, find-newline,
    header-add, header-add-new, header-concatenate;
end;

define module tokenize
  use common-dylan, exclude: { format-to-string };

  use tokens;
  use source;

  export
    <tokenizer>, get-token, unget-token, note-potential-end-point;
end module tokenize;

define module lexer
  use common-dylan, exclude: { format-to-string };
  use format;
  use streams;
  use standard-io;
  use print;
  use pprint;

  use errors;
  use source;
  use literals;
  use tokens;
  use variables;
  use tokenize;

  export
    <lexer>,

    add-feature, remove-feature;
end;

define module source-utilities
  use common-dylan, exclude: { format-to-string };
  use format;
  use streams;
  use pprint;

  use source;

  export
    <macro-source>, macro-source-description,

    <section-marker>,

    <macro-source-location>, macro-srcloc-source,

    <simple-macro-source-location>, macro-srcloc-came-from,
    macro-srcloc-token, macro-srcloc-section,

    <compound-macro-source-location>, macro-srcloc-first, macro-srcloc-last,

    source-location-before, source-location-after,
    source-location-between, source-location-spanning, 
    simplify-source-location;
end module source-utilities;

define module fragments
  use common-dylan, exclude: { format-to-string };
  use format;
  use streams;
  use print;
  use pprint;

  use source;
  use tokens;
  use tokenize;
  use source-utilities;

  export
    <fragment>,
    <empty-fragment>,
    <compound-fragment>, fragment-head, fragment-tail,
    <elementary-fragment>, fragment-prev, fragment-next,
    <token-fragment>, fragment-token,
    <bracketed-fragment>, fragment-left-token, fragment-left-srcloc,
    fragment-contents, fragment-right-token, fragment-right-srcloc,

    copy-fragment, append-fragments!,

    <fragment-tokenizer>, tokenizer-potential-end-point;
end;

define module parse-tree
  use common-dylan, exclude: { format-to-string };
  use format;
  use streams;
  use print;
  use pprint;

  use tokens;
  use fragments;
  use source;
  use literals;

  export

    <method-parse>, method-name, method-name-setter, method-parameters,
    method-returns, method-body,

    <callback-method-parse>,

    <variable-list>, varlist-fixed, varlist-fixed-setter, varlist-rest,
    varlist-rest-setter,

    <parameter-list>, paramlist-next, paramlist-next-setter, paramlist-keys,
    paramlist-all-keys?,

    <parameter>, param-name, param-type,

    <keyword-parameter>, param-keyword, param-default,

    <bindings-parse>, bindings-variables, bindings-expression,

    <constituent-parse>,

    <macro-call-parse>, macro-call-word, macro-call-fragment,

    <definition-parse>,

    <definition-macro-call-parse>, definition-modifiers,

    <body-style-definition-macro-call-parse>,

    <list-style-definition-macro-call-parse>,

    <local-declaration-parse>,

    <let-parse>, let-variables, let-expression,

    <handler-parse>, handler-type, handler-options, handler-expression,

    <local-parse>, local-methods,

    <expression-parse>,

    <literal-ref-parse>, litref-literal,

    <funcall-parse>, funcall-function, funcall-arguments,

    <dot-parse>, dot-operand, dot-name,

    <varref-parse>, varref-id,

    <varset-parse>, varset-id, varset-value,

    <statement-parse>,

    <function-macro-call-parse>,

    <body-parse>, body-parts,

    <bind-exit-parse>, exit-name, exit-body,

    <if-parse>, if-condition, if-consequent, if-alternate,

    <method-ref-parse>, method-ref-method, method-ref-options,

    <callback-method-ref-parse>,

    <primitive-parse>, primitive-name, primitive-operands,

    <unwind-protect-parse>, uwp-body, uwp-cleanup,

    <define-macro-parse>, defmacro-name, defmacro-main-rule-set,
    defmacro-auxiliary-rule-sets,

    <rule-set>, rule-set-rules,

    <main-rule-set>,

    <auxiliary-rule-set>, rule-set-name, rule-set-body-variable?,
    rule-set-body-variable?-setter, rule-set-processed-intermediate-words?,
    rule-set-processed-intermediate-words?-setter,

    <rule>, rule-pattern, rule-pattern-setter, rule-template,

    <main-rule>, main-rule-name,

    <define-rule>, define-rule-modifiers-pattern,
    define-rule-modifiers-pattern-setter,

    <body-style-define-rule>,

    <list-style-define-rule>,

    <statement-rule>,

    <function-rule>,

    <auxiliary-rule>,

    <pattern>,

    <empty-pattern>,

    <binary-pattern>, pattern-left, pattern-right, pattern-last?,

    <semicolon-pattern>,

    <comma-pattern>,

    <sequential-pattern>,

    <simple-pattern>,

    <bracketed-pattern>, pattern-left-token, pattern-guts, pattern-right-token,

    <variable-pattern>, variable-name-pattern, variable-type-pattern,

    <bindings-pattern>, bindings-variables-pattern, bindings-value-pattern,

    <name-pattern>, pattern-name,

    <arrow-pattern>,

    <pattern-variable>, patvar-name, patvar-constraint,
    patvar-constraint-setter, patvar-at-end?, patvar-at-end?-setter,

    <property-list-pattern>, plistpat-rest, plistpat-rest-setter,
    plistpat-keys, plistpat-all-keys?,

    <pattern-keyword>, patkey-default, patkey-all?,

    <template>,

    <procedural-template>, template-name, template-arguments,

    <literal-template>, template-elements,

    <bracketed-element>, bracketed-element-left-token, bracketed-element-guts,
    bracketed-element-right-token,

    <pattern-variable-reference>, patvarref-name,

    <simple-pattern-variable-reference>,

    <ellipsis-pattern-variable-reference>,

    <concatenating-pattern-variable-reference>,
    patvarref-prefix, patvarref-suffix,

    <sequence-pattern-variable-reference>, patvarref-separator,

    <unhygienic-pattern-variable-reference>,

    <property>, prop-comma, prop-comma-srcloc,
    prop-keyword, prop-keyword-srcloc, prop-value;
end;

define module parser
  use common-dylan, exclude: { format-to-string };
  use format;
  use streams;
  use print;
  use pprint;
  use standard-io;

  use source;
  use literals;
  use errors;
  use tokens;
  use variables;
  use fragments;
  use tokenize;
  use parse-tree;
  use source-utilities;

  export
    make-parsed-fragment,

    process-top-level-form,

    parse-source-record,
    parse-expression,
    parse-variable,
    parse-bindings,
    parse-body,
    parse-case-body,
    parse-property-list,
    parse-parameter-list,
    parse-variable-list,
    parse-macro-call;
end;

define module macros
  use common-dylan, exclude: { format-to-string };
  use format;
  use streams;
  use print;
  use pprint;

  use source;
  use literals;
  use errors;
  use tokens;
  use tokenize;
  use names;
  use definitions;
  use variables;
  use fragments;
  use parse-tree;
  use parser;
  use source-utilities;

  export
    <macro-definition>, macro-expand, recursively-macro-expand,
    macro-expansion-tokenizer,

    <expansion-generator>, generate-token-source-location, generate-fragment,
      generator-call, generator-source,
    define-procedural-expander;
end;

define module convert
  use common-dylan, exclude: { format-to-string };
  use format;
  use streams;
  use print;
  use pprint;
  use standard-io;

  use source;
  use errors;
  use literals;
  use names;
  use tokens;
  use parse-tree;
  use variables;
  use fragments;
  use macros;
  use parser;
end;

define module convert-macros
  use common-dylan, exclude: { format-to-string };
  use format;
  use streams;
  use print;
  use pprint;

  use errors;
  use tokens;
  use source;
  use definitions;
  use variables;

  use parse-tree;
  use parser;
  use macros;
end module;

define module dbmc
  use common-dylan, exclude: { format-to-string };
  use format;
  use format-out;
  use standard-io;
  use streams;
  use file-system;

  use source;
  use errors;
  use variables;
  use header;
  use tokens;
  use tokenize;
  use lexer;
  use parser;

  export main;
end module dbmc;
