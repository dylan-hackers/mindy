module: dylan-user
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/Attic/exports.dylan,v 1.2 1994/12/12 21:21:53 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

define library compiler
  use Dylan;
  use Collection-Extensions;
  use Random;
  use Streams;
  use Print;
  use Format;
  use Debugger-Format;
end;

define module common
  use Dylan, export: all;
  use Extensions, export: all;
  use Streams, export: all;
  use Print, export: all;
  use PPrint, export: all;
  use Format, export: all;
end;

define module utils
  use common;
  use standard-io;
  use Introspection;
  use System;

  // Stuff defined in utils
  export
    write-class-name, write-address, pprint-fields, assert,
    key-of, list?, pair?;
end;

define module forwards
  create
    <module>,
    <ctype>,
    <abstract-variable>;
end;

define module source
  use common;
  use System;
  use File-Descriptors;

  use utils;

  export
    <source-location>, source-location-span,
    <source-location-mixin>, source-location,

    <source-file>, contents,
    <file-source-location>, source-file,
    start-posn, start-line, start-column,
    end-posn, end-line, end-column,

    extract-string;
end;

define module header
  use common;
  use System;

  use utils;
  use source;

  export
    <header>, parse-header;
end;

define module tokens
  use common;
  use self-organizing-list;

  use utils;
  use source;
  use forwards, import: {<module>};

  export
    <token>, <eof-token>, <error-token>, <word-token>,
    <identifier-token>, <name-token>, <simple-name-token>,
    <quoted-name-token>, <begin-word-token>, <define-word-token>,
    <define-bindings-word-token>, <constrained-name-token>,
    <begin-token>, <bind-exit-token>, <class-token>, <cleanup-token>,
    <constant-token>, <create-token>, <define-token>, <else-token>,
    <end-token>, <export-token>, <finally-token>, <for-token>,
    <from-token>, <generic-token>, <handler-token>, <if-token>,
    <in-token>, <let-token>, <library-token>, <local-token>,
    <macro-token>, <module-token>, <method-token>, <mv-call-token>,
    <otherwise-token>, <set-token>, <use-token>, <uwp-token>,
    <variable-token>, <while-token>, <keyword-token>,
    <abstract-literal-token>, <literal-token>, <string-token>,
    <operator-token>, <binary-operator-token>, <unary-operator-token>,
    <punctuation-token>, <left-paren-token>, <right-paren-token>,
    <comma-token>, <dot-token>, <semicolon-token>,
    <left-bracket-token>, <right-bracket-token>, <left-brace-token>,
    <right-brace-token>, <double-colon-token>, <minus-token>,
    <tilde-token>, <equal-token>, <double-equal-token>, <arrow-token>,
    <sharp-paren-token>, <sharp-bracket-token>, <question-token>,
    <double-question-token>, <ellipsis-token>, <sharp-word-token>,
    <true-token>, <false-token>, <next-token>, <rest-token>,
    <key-token>, <all-keys-token>,

    token-symbol, token-constraint, token-module,
    token-uniquifier, token-literal,

    operator-precedence, operator-left-associative?,
    operator-precedence-setter, operator-left-associative?-setter,

    <uniquifier>, same-id?,

    merge-category,

    <tokenizer>, get-token, unget-token;

end;

define module signature
  create
    <signature>, specializers, rest-type, key-infos, all-keys?,
    returns, returns-rest-type,

    <key-info>, key-name, key-type, required?;
end;

define module names
  use common;
  use forwards, import: {<module>};
  use utils;
  use signature;

  export
    <name>,
    <basic-name>, name-symbol, name-module,
    <type-cell-name>, type-cell-name-base,
    <method-name>, method-name-generic-function, method-name-signature;
end;

define module definitions
  use common;

  use utils;
  use tokens;
  use names;

  export
    <definition>, defn-name,
    check-syntax-table-additions, make-syntax-table-additions,
    <abstract-constant-definition>,
    <implicit-definition>;

  create
    <class-definition>,
    <constant-definition>,
    <variable-definition>, var-defn-type-defn;

end;

define module variables
  use common;

  use utils;
  use tokens;
  use names;
  use definitions;

  use forwards, import: {<module>}, export: all;
  export
    $Dylan-Library, $Dylan-Module, *Current-Library*, *Current-Module*,

    <library>, find-library, library-name, note-library-definition,
    find-module, module-name, module-syntax-table,
    note-module-definition,
    <variable>, find-variable, variable-name, variable-definition,
    note-variable-definition,
    <use>, <renaming>,

    done-initializing-module-system,

    dylan-var, dylan-defn;
end;

define module lexer
  use common;
  use System;

  use utils;
  use source;
  use tokens;
  use variables;

  export
    <lexer>;
end;

define module fragments
  use common;

  use utils;
  use tokens;

  export
    <fragment>, fragment-head, fragment-head-setter, fragment-tail,
    fragment-tail-setter,
    <piece>, piece-prev, piece-prev-setter, piece-next, piece-next-setter,
    piece-token,
    <balanced-piece>, piece-other, piece-other-setter,
    prepend-piece, postpend-piece, append-fragments,
    <fragment-tokenizer>;
end;

define module parse-tree
  use common;
  use utils;
  use tokens;
  use fragments;

  export
    <property>, prop-keyword, prop-value,
    <bindings>, bindings-parameter-list, bindings-expression,
    <parameter-list>, paramlist-required-vars, paramlist-required-vars-setter,
    paramlist-rest, paramlist-rest-setter, paramlist-next,
    paramlist-next-setter, paramlist-keys, paramlist-all-keys?,
    <parameter>, param-name, param-type,
    <keyword-parameter>, param-keyword, param-default,
    <method-parse>, method-name, method-name-setter, method-param-list,
    method-returns, method-body,
    <case-clause>, case-label, case-body, case-body-setter,
    <property-set>, property-set-members,
    <use-clause>, use-name, use-import, use-exclude, use-prefix, use-rename,
    use-export,
    <export-clause>, export-names,
    <create-clause>, create-names,
    <for-clause>, <for-while-clause>, for-clause-condition,
    <for-var-clause>, for-clause-variable,
    <for-in-clause>, for-clause-collection,
    <for-step-clause>, for-clause-init, for-clause-step,
    <for-from-clause>, for-clause-from, for-clause-by, for-clause-kind,
    for-clause-bound,
    <classopt>, classopt-kind, classopt-name, classopt-plist,

    <constituent>,
    <defining-form>, define-modifiers, define-modifiers-setter,
    <define-class-parse>, defclass-name, defclass-supers, defclass-options,
    <define-constant-parse>, defconst-bindings,
    <define-generic-parse>, defgen-name, defgen-name-setter, defgen-param-list,
    defgen-returns, defgen-plist, defgen-plist-setter,
    <define-library-parse>, deflibrary-name, deflibrary-clauses,
    <define-method-parse>, defmethod-method,
    <define-module-parse>, defmodule-name, defmodule-clauses,
    <define-variable-parse>, defvar-bindings,
    <define-parse>, define-word, define-word-setter, define-name,
    define-fragment,
    <define-bindings-parse>, define-bindings,
    <define-macro-parse>, defmacro-name, defmacro-main-rule-set,
    defmacro-auxiliary-rule-sets,
    <define-define-macro-parse>, <define-define-bindings-macro-parse>,
    <define-statement-macro-parse>, <define-function-macro-parse>,
    <local-declaration>,
    <let>, let-bindings,
    <let-handler>, handler-type, handler-plist, handler-expression,
    <local>, local-methods,
    <expression>,
    <literal>, lit-value,
    <binop-series>, binop-series-operands, binop-series-operators,
    <funcall>, funcall-function, funcall-arguments,
    <dot>, dot-operand, dot-name,
    <varref>, varref-name,
    <macro-statement>, statement-begin-word, statement-fragment,
    <assignment>, assignment-place, assignment-value,
    <begin>, begin-body,
    <bind-exit>, exit-name, exit-body,
    <for>, for-header, for-body, for-finally,
    <if>, if-condition, if-consequent, if-alternate,
    <method-ref>, method-ref-method,
    <mv-call>, mv-call-operands,
    <uwp>, uwp-body, uwp-cleanup,
    <rule>, rule-pattern, rule-template,
    <abstract-define-rule>, define-rule-modifiers-pattern,
    define-rule-modifiers-pattern-setter,
    <define-rule>, <define-bindings-rule>,
    <statement-rule>, <function-rule>,
    <pattern>, pattern-pieces,
    <pattern-list>, pattern-list-pieces,
    <pattern-sequence>, pattern-sequence-pieces,pattern-sequence-pieces-setter,
    <simple-pattern>,
    <variable-pattern>, variable-name-pattern, variable-type-pattern,
    <bound-variable-pattern>, bound-variable-variable, bound-variable-value,
    <identifier-pattern>, pattern-identifier,
    <literal-pattern>, pattern-literal, <otherwise-pattern>, <arrow-pattern>,
    <details-pattern>, pattern-sub-pattern,
    <pattern-variable>, patvar-name, patvar-name-setter,
    patvar-constraint, patvar-wildcard?, patvar-wildcard?-setter,
    <property-list-pattern>, plistpat-rest, plistpat-rest-setter,
    plistpat-keys, plistpat-all-keys?,
    <pattern-keyword>, patkey-name, patkey-default, patkey-all?,
    <auxiliary-rule-set>, rule-set-name, rule-set-rules,
    rule-set-body-variable?, rule-set-body-variable?-setter,
    rule-set-processed-intermediate-words?,
    rule-set-processed-intermediate-words?-setter,
    <auxiliary-rule>,
    <template>, template-parts,
    <paren-template>, template-left-token, template-right-token,
    <pattern-variable-reference>, patvarref-name, patvarref-separator;

end;

define module top-level-forms
  use common;
  use utils;
  use tokens;
  use parse-tree;
  use definitions;
  use builder-interface, import: {<fer-builder>};

  export
    <top-level-form>,
    <define-tlf>, <simple-define-tlf>, tlf-defn,
    $Top-Level-Forms,

    process-top-level-form, finalize-top-level-form, convert-top-level-form,

    extract-modifiers, extract-properties;
end;

define module policy
  use common;
  use utils;

  export <policy>, $Default-Policy;
end;

define module lexenv
  use common;
  use utils;
  use tokens;
  use policy;
  use forwards, import: {<abstract-variable>};

  export
    <lexenv>, lexenv-policy, lexenv-policy-setter,
    <binding>, binding-name, binding-var, binding-type-var,
    add-binding, find-binding;
end;

define module compile-time-values
  use common;

  use utils;
  use forwards, import: {<ctype>};
  use variables;
  use definitions;

  export
    <ct-value>,
    <ct-unbound-marker>, $Unbound-Marker-CT-Value,
    <eql-ct-value>, ct-value-singleton,
    <ct-literal>, ct-literal-value,
    <eql-ct-literal>,

    ct-value, dylan-value;
end;

define module ctype
  use common;
  use Introspection, import: {class-name};
  use Random;

  use utils;
  use compile-time-values;
  use names;

  use forwards, import: {<ctype>}, export: all;
  export
    <values-ctype>, csubtype?, ctype-union, ctype-intersection,
    ctype-difference, ctypes-intersect?, ctype-eq?, ctype-neq?, <union-ctype>,
    <singleton-ctype>, <unknown-ctype>, <limited-ctype>,
    <limited-integer-ctype>, <limited-collection-ctype>,
    <direct-instance-ctype>, 
    make-canonical-singleton, singleton-value, type-exp, base-class,
    low-bound, high-bound, element-limit, size-limit, <cclass>,
    <primitive-cclass>, <defined-cclass>,
    precedence-list, subclasses, sealed?, abstract?, primary?, slot-infos,
    wild-ctype, empty-ctype, object-ctype, function-ctype,
    find-direct-classes;
end;

define module compile-time-eval
  use common;

  use utils;
  use variables;
  use definitions;
  use compile-time-values;
  use ctype;
  use tokens;
  use parse-tree;
  use expand;
  use lexenv;

  export
    ct-eval, ct-mv-eval;
end;

define module expand
  use common;
  use utils;
  use tokens;
  use parse-tree;
  use variables;

  export
    expand;
end;

define module parser
  use common;
  use self-organizing-list;
  use utils;
  use tokens;
  use variables;
  use fragments;
  use parse-tree;
  use top-level-forms;

  export
    parse-program, parse-expression, parse-variable, parse-body,
    parse-case-body, parse-property-list, parse-type;
end;

define module macros
  use common;
  use utils;
  use source;
  use tokens;
  use names;
  use definitions;
  use variables;
  use fragments;
  use parse-tree;
  use top-level-forms;
  use parser;

  export
    <define-macro-definition>, <define-bindings-macro-definition>,
    <statement-macro-definition>, <function-macro-definition>;
end;

define module define-libraries-and-modules
  use common;
  use utils;
  use tokens;
  use parse-tree;
  use variables;
  use top-level-forms;
end;

define module define-functions
  use common;
  use utils;
  use tokens;
  use names;
  use definitions;
  use variables;
  use parse-tree;
  use top-level-forms;
  use compile-time-values;
  use builder-interface;
  use signature;
  use ctype;
  use compile-time-eval;
  use lexenv;

  export
    <function-definition>,
    <generic-definition>,
    <method-definition>,
    <define-generic-tlf>,
    <define-method-tlf>;
end;

define module define-classes
  use common;
  use utils;
  use tokens;
  use names;
  use definitions;
  use variables;
  use lexenv;
  use parse-tree;
  use top-level-forms;
  use compile-time-values;
  use ctype;
  use compile-time-eval;
  use define-functions;
  use builder-interface;
end;

define module builder-interface
  create
    <flow-builder>, make-builder, builder-result, end-body, build-region,
    build-if-body, build-else, build-block-body, build-exit, build-loop-body,
    build-assignment, build-join, make-operation, <fer-builder>, build-let,
    make-mv-operation, make-literal-constant, make-definition-leaf,
    make-lexical-var, make-local-var, make-values-cluster, copy-variable,
    make-exit-function, build-method-body,

    <fer-component>;
end;

define module flow
  use common;
  use utils;
  use ctype;
  use source;
  use forwards, import: {<abstract-variable>}, export: all;
  export 
    <region>, <linear-region>, <simple-region>, <compound-region>,
    <join-region>, <if-region>, <body-region>, <block-region-mixin>,
    <block-region>, <method-region>, <loop-region>, <exit>, <call-site>,
    <component>,

    parent, parent-setter, first-assign, first-assign-setter, last-assign,
    last-assign-setter, regions, regions-setter, join-region,
    join-region-setter, if-test, if-test-setter, then-region,
    then-region-setter, else-region, else-region-setter, body, body-setter,
    exits, exits-setter, call-sites, call-sites-setter, block-of,
    block-of-setter, method-of, method-of-setter, all-methods,
    all-methods-setter,

    <expression>, <dependency>, <leaf>, <variable-info>,
    <definition-site-variable>, <ssa-variable>, <initial-definition>,
    <multi-definition-variable>, <initial-variable>, <global-variable>,
    <operation>, <join-operation>, <abstract-assignment>, <assignment>,
    <join-assignment>,

    dependents, derived-type, source-exp, source-next, dependent,
    dependent-next, var-info, asserted-type, definer, definer-next, queue-next,
    definition-of, definitions, operands, defines, region, next-op, prev-op,
    expression,

    dependents-setter, derived-type-setter, source-exp-setter,
    source-next-setter, dependent-setter, dependent-next-setter,
    var-info-setter, asserted-type-setter, definer-setter, definer-next-setter,
    queue-next-setter, definition-of-setter, definitions-setter,
    operands-setter, defines-setter, region-setter, next-op-setter,
    prev-op-setter, expression-setter;

end;

define module front
  use common;
  use utils;
  use compile-time-values;
  use definitions;
  use flow;
  use ctype;
  use signature;
  use source;
  use builder-interface;
  use policy;
end;

define module fer-convert
  use common;

  use utils;
  use source;
  use tokens;
  use compile-time-values;
  use compile-time-eval;
  use definitions;
  use variables;
  use parse-tree;
  use expand;
  use flow,
    exclude: {<expression>, <assignment>},
    export: {<leaf>};
  use builder-interface;
  use ctype;
  use lexenv;
  use policy;

  export
    fer-convert, build-hairy-method-body;
end;

define module define-constants-and-variables
  use common;
  use utils;
  use compile-time-values;
  use compile-time-eval;
  use lexenv;
  use parse-tree;
  use top-level-forms;
  use names;
  use definitions;
  use ctype;
  use variables;
  use tokens;
  use builder-interface;
  use fer-convert;
end;

define module top-level-expressions
  use common;
  use utils;
  use parse-tree;
  use top-level-forms;
  use lexenv;
  use builder-interface;
  use fer-convert;
end;

define module init
  use common;
  use utils;
  use tokens;
  use variables;
  use lexer;
  use parser;
  use source;
  use header;
end;

define module dump
  use common;

  use utils;
  use tokens;
  use variables;
  use parse-tree;
  use fragments;
  use source;

  export
    dump;
end;

define module main
  use common;
  use utils;
  use define-classes;
  use variables;
  use parser;
  use format;
  use lexer;
  use header;
  use source;
  use top-level-forms;
  use flow;
  use builder-interface;
  use fer-convert;
end;
