module: dylan-user
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/Attic/exports.dylan,v 1.78 1995/06/07 19:38:13 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

define library compiler
  use Dylan;
  use Collection-Extensions, exclude: {heap};
  use Random;
  use Streams;
  use Print;
  use Format;
  use Debugger-Format;
end;

define module params
  use Dylan;
  use Extensions, import: {<extended-integer>};

  export $minimum-fixed-integer, $maximum-fixed-integer;
end;

define module common
  use Dylan, export: all;
  use Extensions, export: all;
  use Streams, export: all;
  use Print, export: all;
  use PPrint, export: all;
  use Format, export: all;
  use Params, prefix: "runtime-", export: all;
end;

define module utils
  use common;
  use standard-io;
  use Introspection;
  use System;

  // Stuff defined in utils
  export
    write-class-name, write-address, pprint-fields,
    find-in, size-in,
    dformat, assert,
    <annotatable>, info, info-setter,
    compiler-warning, compiler-error, key-of, list?, pair?,
    symcat;
end;

define module forwards
  create
    <module>,
    <ctype>,
    <cclass>,
    <abstract-variable>,
    expand;
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

define module compile-time-values
  use common;

  use utils;
  use forwards, import: {<ctype>};

  export
    <ct-value>,
    <eql-ct-value>, ct-value-singleton, ct-value-singleton-setter,
    <literal>, literal-value, <eql-literal>,
    <literal-number>, <literal-real>, <literal-rational>, <literal-integer>,
    <literal-fixed-integer>, <literal-extended-integer>, <literal-ratio>,
    <literal-float>, <literal-single-float>, <literal-double-float>,
    <literal-extended-float>, <literal-symbol>, <literal-character>,
    <literal-boolean>, <literal-true>, <literal-false>,
    <literal-sequence>,
    <literal-list>,
    <literal-pair>, literal-head, literal-tail,
    <literal-empty-list>,
    <literal-vector>,
    <literal-simple-object-vector>, literal-contents,
    <literal-string>;
end;


define module tokens
  use common;
  use self-organizing-list;

  use utils;
  use source;
  use compile-time-values;
  use forwards, import: {<module>};

  export

    <token>, <eof-token>, <error-token>, <symbol-token>,
    <identifier-token>, <word-token>, <name-token>,
    <simple-name-token>, <quoted-name-token>, <begin-word-token>,
    <define-word-token>, <define-bindings-word-token>,
    <constrained-name-token>, <core-word-token>, <begin-token>,
    <bind-exit-token>, <class-token>, <cleanup-token>,
    <constant-token>, <create-token>, <define-token>, <else-token>,
    <end-token>, <export-token>, <finally-token>, <for-token>,
    <from-token>, <generic-token>, <handler-token>, <if-token>,
    <in-token>, <let-token>, <library-token>, <local-token>,
    <macro-token>, <module-token>, <method-token>, <mv-call-token>,
    <otherwise-token>, <primitive-token>, <seal-token>, <set-token>,
    <use-token>, <uwp-token>, <variable-token>, <while-token>,
    <keyword-token>, <abstract-literal-token>, <literal-token>,
    <string-token>, <operator-token>, <binary-operator-token>,
    <simple-binary-operator-token>, <unary-operator-token>,
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

define module signature-interface
  create
    <signature>, specializers, next?, rest-type, key-infos, all-keys?,
    returns,

    <key-info>, key-name, key-type, required?, key-default,
    key-supplied?-var, key-supplied?-var-setter;
end;

define module names
  use common;
  use forwards, import: {<module>};
  use utils;
  use tokens;
  use signature-interface;

  export
    <name>,
    <basic-name>, id-name, name-symbol, name-module,
    <type-cell-name>, type-cell-name-base,
    <method-name>, method-name-generic-function, method-name-specializers;
end;

define module definitions
  use common;

  use utils;
  use tokens;
  use compile-time-values;
  use names;
  use forwards, import: {<ctype>};

  export
    <definition>, defn-name, defn-type, ct-value,
    check-syntax-table-additions, make-syntax-table-additions,
    <abstract-constant-definition>,
    <implicit-definition>;

  create
    <class-definition>,
    <function-definition>, function-defn-signature, function-defn-hairy?,
    function-defn-transformers, function-defn-transformers-setter,
    function-defn-movable?, function-defn-flushable?,
    <abstract-method-definition>,
    <bindings-definition>, defn-init-value,
    <constant-definition>,
    <constant-method-definition>,
    <variable-definition>, var-defn-type-defn;

end;

define module variables
  use common;

  use utils;
  use compile-time-values;
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
    variable-transformers, variable-transformers-setter,
    note-variable-definition,
    <use>, <renaming>,

    done-initializing-module-system,

    dylan-var, dylan-defn, dylan-value;
end;

define module lexer
  use common;
  use System;

  use utils;
  use source;
  use compile-time-values;
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
    bindings-expression-setter,
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
    <seal-generic-parse>, sealgen-name, sealgen-type-exprs,
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
    <literal-ref>, litref-literal,
    <binop-series>, binop-series-operands, binop-series-operators,
    <funcall>, funcall-function, funcall-arguments,
    <dot>, dot-operand, dot-name,
    <varref>, varref-id,
    <macro-statement>, statement-begin-word, statement-fragment,
    <assignment>, assignment-place, assignment-value,
    <begin>, begin-body,
    <bind-exit>, exit-name, exit-body,
    <for>, for-header, for-body, for-finally,
    <if>, if-condition, if-consequent, if-alternate,
    <method-ref>, method-ref-method,
    <mv-call>, mv-call-operands,
    <primitive>, primitive-name, primitive-operands,
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
    patvar-at-end?, patvar-at-end?-setter,
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
  use compile-time-values;
  use parse-tree;
  use definitions;
  use builder-interface, import: {<fer-builder>};

  export
    <top-level-form>,
    <define-tlf>, <simple-define-tlf>, tlf-defn, tlf-defn-setter,
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

define module ctype
  use common;
  use Introspection, import: {class-name};
  use Random;

  use utils;
  use compile-time-values;
  use names;
  use variables;
  use forwards, import: {<cclass>};

  use forwards, import: {<ctype>}, export: all;
  export
    // The various types, their accessors and constructors.
    <values-ctype>,
    <multi-value-ctype>, make-values-ctype, min-values, positional-types,
	rest-value-type, first-value,
    // <ctype> is picked from from the forwards.
    <unknown-ctype>, type-exp,
    <union-ctype>, members,
    <limited-ctype>, base-class,
    <singleton-ctype>, make-canonical-singleton, singleton-value,
    <limited-integer-ctype>, make-canonical-limited-integer,
    low-bound, high-bound,
    <direct-instance-ctype>,
    <byte-character-ctype>, 

    // Operations on types.
    values-subtype?, values-types-intersect?, values-type-intersection,
    values-type-union, csubtype?, ctype-union, ctype-intersection,
    ctype-difference, ctypes-intersect?, ctype-eq?, ctype-neq?, 
    find-direct-classes,

    // Shorthand constructor functions.
    ct-value-cclass, wild-ctype, object-ctype, function-ctype, empty-ctype,

    // Type specifiers.
    <type-specifier>, specifier-type,

    // Ctype extension generic functions.
    csubtype-dispatch, ctype-intersection-dispatch;
end;

define module transformers
  use common;

  use utils;
  use variables;
  use ctype;

  export
    <transformer>, transformer-name, transformer-specializers,
    transformer-function, define-transformer;
end;

define module representation
  use common;

  use utils;
  use variables;
  use ctype;

  export
    <representation>, pick-representation, representation-alignment,
    representation-size, representation-has-bottom-value?;
end;

define module classes
  use common;

  use utils;
  use names;
  use definitions;
  use variables;
  use compile-time-values;
  use ctype;
  use representation;

  use forwards, import: {<cclass>}, export: all;

  export
    cclass-name, closest-primary-superclass, precedence-list, subclasses,
    sealed?, abstract?, primary?, functional?, not-functional?,
    all-slot-infos, new-slot-infos, override-infos, unique-id, direct-type,
    space-representation, space-representation-setter,
    speed-representation, speed-representation-setter,
    instance-slots-layout, vector-slot,
    <defined-cclass>, class-defn,

    <slot-allocation>, <slot-info>, slot-introduced-by,
    slot-type, slot-type-setter, slot-getter, slot-read-only?,
    slot-guaranteed-initialized?, slot-init-value, slot-init-value-setter,
    slot-init-function, slot-init-function-setter, slot-init-keyword,
    slot-init-keyword-required?, slot-overrides,

    <instance-slot-info>, slot-representation, slot-initialized?-slot,
    slot-positions, find-slot-offset,

    <vector-slot-info>, slot-size-slot,

    <virtual-slot-info>, <constant-slot-info>, <class-slot-info>,
    <each-subclass-slot-info>,

    <override-info>, override-introduced-by, override-getter, override-slot,
    override-init-value, override-init-value-setter,
    override-init-function, override-init-function-setter,

    <layout-table>, layout-length, layout-holes,

    <proxy>, proxy-for,

    inherit-slots, inherit-overrides, assign-unique-ids,
    assign-slot-representations, layout-instance-slots;
end;

define module c-representation
  use common;

  use utils;
  use variables;
  use ctype;
  use representation;
  use classes;

  export
    seed-representations,

    <c-representation>, more-general-representation, representation-depth,
    representation-to-more-general, representation-from-more-general,
    representation-c-type,

    <general-representation>, <immediate-representation>,

    <data-word-representation>, representation-class,
    representation-data-word-member,

    $general-rep, $heap-rep, $boolean-rep,
    *long-rep*, *int-rep*, *uint-rep*, *short-rep*, *ushort-rep*,
    *byte-rep*, *ubyte-rep*, *ptr-rep*,
    *float-rep*, *double-rep*, *long-double-rep*;
end;

define module compile-time-functions
  use common;

  use utils;
  use compile-time-values;
  use signature-interface;
  use definitions;
  use ctype;
  use classes;

  export
    <ct-function>, ct-function-name, ct-function-signature,
    ct-function-definition, ct-function-closure-var-types,

    <ct-generic-function>,

    <ct-method>, ct-method-hidden?,

    <ct-entry-point>, ct-entry-point-for, ct-entry-point-kind;
end;

define module compile-time-eval
  use common;

  use utils;
  use names;
  use variables;
  use definitions;
  use compile-time-values;
  use ctype;
  use classes;
  use tokens;
  use parse-tree;
  use forwards, import: {expand};
  use lexenv;

  export
    ct-eval, ct-mv-eval;
end;

define module expand
  use common;
  use utils;
  use compile-time-values;
  use tokens;
  use lexenv;
  use parse-tree;
  use variables;
  use compile-time-eval;
  use ctype;

  use forwards, import: {expand}, export: all;
end;

define module parser
  use common;
  use self-organizing-list;
  use utils;
  use compile-time-values;
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
  use compile-time-values;
  use tokens;
  use names;
  use definitions;
  use variables;
  use fragments;
  use parse-tree;
  use top-level-forms;
  use parser;
  use expand;
  use lexenv, exclude: {add-binding};

  export
    <define-macro-definition>, <define-bindings-macro-definition>,
    <statement-macro-definition>, <function-macro-definition>;
end;

define module define-libraries-and-modules
  use common;
  use utils;
  use compile-time-values;
  use tokens;
  use parse-tree;
  use variables;
  use top-level-forms;
end;

define module builder-interface
  create
    <flow-builder>, make-builder, builder-result, end-body, build-region,
    build-if-body, build-else, build-block-body, build-exit, build-return,
    build-loop-body, build-assignment, build-join, make-operation,
    <fer-builder>, build-let, make-unknown-call, make-literal-constant,
    make-definition-leaf, make-lexical-var, make-ssa-var, make-local-var,
    make-values-cluster, copy-variable, make-exit-function,
    build-unwind-protect-body, build-function-body, make-function-literal, 

    <fer-component>, <nlx-info>;
end;

define module flow
  use common;
  use utils;
  use ctype;
  use source;
  use forwards, import: {<abstract-variable>}, export: all;
  export 
    <region>, <linear-region>, <simple-region>, <compound-region>,
    <empty-region>,
    <join-region>, <if-region>, <body-region>, <block-region-mixin>,
    <block-region>, <function-region>, <loop-region>, <exit>, <return>,
    <component>,

    parent, parent-setter, first-assign, first-assign-setter, last-assign,
    last-assign-setter, regions, regions-setter, join-region,
    join-region-setter, then-region,
    then-region-setter, else-region, else-region-setter, body, body-setter,
    exits, exits-setter, block-of, block-of-setter,
    next-exit, next-exit-setter, returned-type, returned-type-setter,
    initial-variables, initial-variables-setter,
    reoptimize-queue, reoptimize-queue-setter,
    add-to-queue, all-function-regions,

    <expression>, <dependency>, <queueable-mixin>, <dependent-mixin>,
    <leaf>, <variable-info>, <definition-site-variable>,
    <ssa-variable>, <initial-definition>, <multi-definition-variable>,
    <initial-variable>, <global-variable>, <operation>,
    <join-operation>, <abstract-assignment>, <assignment>,
    <join-assignment>,

    dependents, derived-type, source-exp, source-next, dependent,
    dependent-next, var-info, asserted-type, definer, definer-next,
    needs-type-check?, queue-next, definition-of, definitions,
    next-initial-variable, next-initial-variable-setter,
    defines, region, next-op, prev-op, depends-on,

    dependents-setter, derived-type-setter, source-exp-setter,
    source-next-setter, dependent-setter, dependent-next-setter,
    var-info-setter, asserted-type-setter, definer-setter, definer-next-setter,
    needs-type-check?-setter,
    queue-next-setter, definition-of-setter, definitions-setter,
    defines-setter, region-setter, next-op-setter, depends-on-setter,
    prev-op-setter;

end;

define module signature
  use signature-interface;
  use compile-time-values;
  use common;
  use utils;
  use ctype;
  use definitions;
end;

define module primitives
  use common;
  use utils;
  use ctype;

  export
    <primitive-info>, define-primitive, primitive-info-or-lose,
    primitive-name, primitive-arg-types, primitive-result-type,
    primitive-side-effect-free?, primitive-pure?,
    primitive-transformer, primitive-emitter,


    define-primitive-transformer, define-primitive-emitter;
end;

define module front
  use common;
  use utils;
  use compile-time-values;
  use names;
  use definitions;
  use variables;
  use flow;
  use ctype;
  use classes;
  use signature-interface;
  use source;
  use builder-interface, export: {<fer-component>, <nlx-info>};
  use policy;
  use primitives;
  use compile-time-functions;

  export
    dump-fer, id, reset-ids,

    <fer-assignment>, policy,
    <let-assignment>, let-next, <set-assignment>,

    <abstract-call>, <known-call>, <error-call>,
    <general-call>, use-generic-entry?, <unknown-call>, <mv-call>,
    <primitive>, name,
    <prologue>, function,
    <set>, variable,
    <self-tail-call>, self-tail-call-of, next-self-tail-call,
    <slot-access>, slot-info, slot-offset, <slot-ref>, <slot-set>,
    <truly-the>, guaranteed-type,
    nlx-info, <catch>, <throw>, throw-next, throw-next-setter, <make-catcher>,
    <disable-catcher>, disable-catcher-next, disable-catcher-next-setter,

    <constant>, <literal-constant>, value,
    <definition-constant-leaf>, const-defn,
    <uninitialized-value>,

    <debug-named-info>, debug-name,
    <values-cluster-info>, <local-var-info>, <lexical-var-info>,
    <module-var-info>, var-defn, <module-almost-constant-var-info>,

    <abstract-function-literal>,
    <function-literal>, visibility, visibility-setter, <function-visibility>,
    name, signature, ct-function, ct-function-setter, main-entry,
    general-entry, general-entry-setter,
    <method-literal>, generic-entry, generic-entry-setter,
    <exit-function>,

    <fer-function-region>, prologue, argument-types, argument-types-setter,
    result-type, result-type-setter, hidden-references?,
    self-call-block, self-call-block-setter,
    self-tail-calls, self-tail-calls-setter,
    <lambda>, literal, environment,

    // <fer-component> is picked up via a create
    all-function-literals, all-lets, all-lets-setter, name,

    <unwind-protect-region>, uwp-region-cleanup-function,

    <environment>, closure-vars, closure-vars-setter,
    <closure-var>, original-var, copy-var, closure-next, closure-next-setter,

    // <nlx-info> is picked up via a create.
    nlx-hidden-references?, nlx-hidden-references?-setter,
    nlx-catch, nlx-catch-setter, nlx-make-catcher, nlx-make-catcher-setter,
    nlx-exit-function, nlx-exit-function-setter, nlx-disable-catchers,
    nlx-disable-catchers-setter, nlx-throws, nlx-throws-setter;
end;

define module fer-convert
  use common;

  use utils;
  use source;
  use tokens;
  use names;
  use compile-time-values;
  use compile-time-eval;
  use signature-interface;
  use definitions;
  use variables;
  use parse-tree;
  use expand;
  use flow,
    exclude: {<assignment>},
    rename: {<expression> => <fer-expression>},
    export: all;
  use front,
    rename: {<primitive> => <fer-primitive>, <mv-call> => <fer-mv-call>},
    import: {<function-literal>, <method-literal>, <set>,
	       <function-visibility>, <catch>, <disable-catcher>,
	       <make-catcher>};
  use builder-interface, export: all;
  use ctype;
  use lexenv, export: all;
  use policy, export: all;
  use representation;
  use compile-time-functions;

  export
    fer-convert-method, fer-convert, fer-convert-body,
    dylan-defn-leaf, make-check-type-operation, make-error-operation;
end;

define module define-functions
  use common;
  use utils;
  use tokens;
  use names;
  use definitions;
  use variables;
  use parse-tree,
    exclude: {<mv-call>};
  use top-level-forms;
  use compile-time-values;
  use builder-interface;
  use fer-convert;
  use signature-interface;
  use ctype;
  use classes;
  use transformers;
  use compile-time-eval;
  use lexenv;
  use source;
  use front,
    import: {<function-literal>, <method-literal>, <truly-the>,
	       <set>, <mv-call>};
  use compile-time-functions;

  export
    compute-signature,
    function-defn-signature-setter, function-defn-hairy?-setter,
    function-defn-ct-value,
    <generic-definition>, generic-defn-discriminator,
    add-seal, ct-sorted-applicable-methods,
    method-defn-inline-expansion,
    <method-definition>, method-defn-of,
    <accessor-method-definition>, accessor-method-defn-slot-info,
    <getter-method-definition>, <setter-method-definition>,
    <define-generic-tlf>,
    <define-method-tlf>,
    implicitly-define-generic;
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
  use source;
  use front, import: {<method-literal>, <set>};
  use define-functions;
  use expand;
  use compile-time-functions;

  export
    <define-bindings-tlf>, tlf-required-defns, tlf-rest-defn;
end;

define module define-classes
  use common;
  use utils;
  use compile-time-values;
  use tokens;
  use names;
  use definitions;
  use variables;
  use lexenv;
  use parse-tree,
    exclude: {<primitive>};
  use top-level-forms;
  use ctype;
  use classes;
  use compile-time-eval;
  use define-functions;
  use builder-interface;
  use fer-convert;
  use signature-interface;
  use source;
  use expand;
  use front,
    import: {<slot-ref>, <slot-set>, <uninitialized-value>, <primitive>,
	       <function-literal>, <method-literal>};
  use representation;
  use c-representation;
  use compile-time-functions;

  export
    class-defn-defered-evaluations-function, class-defn-maker-function;
end;

define module top-level-expressions
  use common;
  use utils;
  use parse-tree;
  use top-level-forms;
  use lexenv;
  use builder-interface;
  use fer-convert;
  use expand;
end;

define module cheese
  use common;
  use utils;
  use compile-time-values;
  use names;
  use definitions;
  use variables;
  use flow;
  use front;
  use ctype;
  use classes;
  use signature-interface;
  use source;
  use builder-interface;
  use policy;
  use define-functions;
  use define-classes;
  use parse-tree, import: {<method-parse>};
  use lexenv;
  use fer-convert;
  use primitives;
  use transformers;
  use compile-time-functions;

  export
    optimize-component, *optimize-ncalls*;
end;

define module stack-analysis
  use common;
  use utils;
  use flow;
  use front;
  use ctype;
  use signature-interface;
  use definitions;
  use compile-time-functions;

  export
    analize-stack-usage;
end;


define module cback
  use common;
  use utils;
  use compile-time-values;
  use names;
  use variables;
  use definitions;
  use representation;
  use c-representation;
  use flow;
  use front;
  use top-level-forms;
  use define-functions;
  use define-constants-and-variables;
  use ctype;
  use classes;
  use stack-analysis;
  use primitives;
  use compile-time-functions;
  use signature-interface;

  export
    <output-info>, output-info-init-roots,
    emit-prologue, emit-tlf-gunk, emit-component, emit-epilogue,
    entry-point-c-name;
end;

define module heap
  use common;
  use utils;
  use names;
  use compile-time-values;
  use variables;
  use representation;
  use c-representation;
  use ctype;
  use classes;
  use compile-time-functions;
  use definitions;
  use define-functions;
  use define-classes;
  use cback;

  export
    build-initial-heap;
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
    dump-parse;
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
  use front;
  use dump;
  use classes;
  use c-representation;
  use cback;
  use heap;
  use compile-time-functions;
  use signature-interface;
  use ctype;
  use cheese;
end;
