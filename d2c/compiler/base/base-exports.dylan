module: dylan-user
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/base-exports.dylan,v 1.16 1996/02/09 18:05:56 ram Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

define library compiler-base
  use Dylan;
  use Collection-Extensions,
    import: {self-organizing-list}, export: all;
  use Random;
  use Streams, export: all;
  use Print, export: all;
  use Format, export: all;
#if (mindy)
  use Debugger-Format;
  use String-extensions, export: all;
#end
  
  export c-representation;
  export classes;
  export common;
  export compile-time-eval;
  export compile-time-functions;
  export compile-time-values;
  export ctype;
  export definitions;
  export errors;
  export expand;
  export flow;
  export forwards;
  export fragments;
  export header;
  export lexenv;
  export names;
  export od-format;
  export parse-tree;
  export policy;
  export representation;
  export signature-interface;
  export source;
  export tokens;
  export transformers;
  export utils;
  export variables-dumper-vars;
  export variables;

end;

define module params
  use Dylan;
  use Extensions, import: {<extended-integer>};

  export $minimum-integer, $maximum-integer;
end;

define module common
  use Dylan,
    exclude: {direct-superclasses, direct-subclasses},
    export: all;
  use Extensions,
    import: {<general-integer>, <extended-integer>,
	     $maximum-integer, ratio,
	     false-or, one-of, <false>, <true>, ignore,
	     $minimum-integer, <byte-character>,
#if (mindy)
             <boolean>, *debug-output*, main, <dictionary>, key-exists?,
#else
             <ratio>, numerator, denominator,
#end
	     <equal-table>, <string-table>, equal-hash},
#if (mindy)
    rename: {type-or => type-union},
#end
    export: all;
  use Streams, export: all;
  use Print, export: all;
  use PPrint, export: all;
  use Format, export: all;
  use Params, prefix: "runtime-", export: all;
#if (~mindy)
  create
     *debug-output*;
#end
end;

define module utils
  use common;
  use standard-io;
#if (mindy)
  use Introspection, import: {object-address, class-name};
  use System, import: {copy-bytes};
#else
  use Introspection, import: {class-name};
  use System, import: {object-address, copy-bytes};
#end

  // Stuff defined in utils
  export
    write-class-name, write-address, pprint-fields,
    find-in, size-in,
    dformat, assert,
    <annotatable>, info, info-setter,
    key-of, list?, pair?,
    symcat, stringify;
end;

define module od-format
  use common;
#if (mindy)
  use system, import: {get-time-of-day};
#else
  use system, import: {call-out};
#end
  use standard-io;
  use introspection, import: {function-name};
  use utils;
  use self-organizing-list;
  export
    $odf-header-flag,
    $odf-etype-mask,
    $odf-object-definition-etype,
    $odf-end-entry-etype,
    $odf-local-reference-etype,
    $odf-external-reference-etype,
    $odf-subobjects-flag,
    $odf-raw-format-mask,
    $odf-no-raw-data-format,
    $odf-byte-raw-data-format,
    $odf-16bit-raw-data-format,
    $odf-32bit-raw-data-format,
    $odf-64bit-raw-data-format,
    $odf-untranslatable-raw-data-format,
    $odf-word-raw-data-format,
    $od-format-major-version,
    $od-format-minor-version,
    $like-an-hp-platform-characteristics,
    $library-summary-unit-type,
    $word-bytes,
    $word-bits,
    buffer-word,
    <dump-buffer>,
    current-pos,
    <dump-state>,
    dump-od,
    begin-dumping,
    end-dumping,
    dump-word,
    dump-raw-data,
    dump-definition-header,
    dump-end-entry,
    dump-simple-object,
    new-local-id,
    label-next-object,
    dump-local-reference,
    <load-state>,
    od-stream,
    od-buffer,
    od-next,
    od-next-setter,
    od-end,
    <dispatcher>,
    *default-dispatcher*,
    add-od-loader,
    find-data-unit,
    $end-object,
    load-object-dispatch,
    fill-at-least,
    load-raw-data,
    load-subobjects-vector,
    load-sole-subobject,
    assert-end-object,
    <forward-ref>,
    actual-obj,
    obj-resolved?,
    request-backpatch,
    resolve-forward-ref,
    <identity-preserving-mixin>,
    maybe-dump-reference,
    load-external-definition,
    add-make-dumper,
    invert-registry,
    *Data-Unit-Search-Path*;

end;

define module dylan-dump
  use common;
  use standard-io;
  use utils;
  use od-format;
end;

define module forwards
  create
    <library>, <module>,
    <ctype>,
    <cclass>,
    <abstract-variable>,
    <ct-function>,
    expand;
end;

define module compile-time-values
  use common;

  use utils;
  use forwards, import: {<ctype>};
  use od-format;

  export
    <ct-value>, ct-value-heap-labels, ct-value-heap-labels-setter,
    <eql-ct-value>, ct-value-singleton, ct-value-singleton-setter,
    <literal>, literal-value, <eql-literal>,
    <ct-not-supplied-marker>,
    <literal-number>, <literal-real>, <literal-rational>, <literal-general-integer>,
    <literal-integer>, <literal-extended-integer>, <literal-ratio>,
    <literal-float>, <literal-single-float>, <literal-double-float>,
    <literal-extended-float>, <literal-symbol>, <literal-character>,
    <literal-boolean>, <literal-true>, <literal-false>,
    <literal-sequence>,
    <literal-list>,
    <literal-pair>, literal-head, literal-tail,
    <literal-empty-list>,
    <literal-vector>,
    <literal-simple-object-vector>,
    <literal-string>, concat-strings,
    *compiler-dispatcher*;
end;

define module source
  use common;
  use System, import: {copy-bytes};
  use utils;
  use od-format;
  use compile-time-values;

  export
    <source-location>, source-location-span,
    <source-location-mixin>, source-location,
    <unknown-source-location>,

    <source-file>, contents, <file-contents>,
    <file-source-location>, source-file,
    start-posn, start-line, start-column,
    end-posn, end-line, end-column,
    file-name,

    extract-string;
end;

define module errors
  use common;
  use utils;
  use source;
  use standard-io;
  export
    compiler-warning, *warnings*, compiler-error,
    compiler-warning-location, compiler-error-location,
    extract-source;
end module;

define module header
  use common;
  use System, import: {copy-bytes};

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
  use compile-time-values;
  use forwards, import: {<module>};
  use od-format;

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
    key-needs-supplied?-var;
end;


define module names
  use common;
  use forwards, import: {<module>};
  use utils;
  use tokens;
  use signature-interface;
  use od-format;
  use compile-time-values;

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
  use od-format;
  use forwards, import: {<library>, <ctype>, <ct-function>};
  use signature-interface;

  export
    <definition>, defn-name, defn-library, defn-type, ct-value,
    $definition-slots,
    check-syntax-table-additions, make-syntax-table-additions,
    <abstract-constant-definition>,
    <implicit-definition>,
    <class-definition>,

    <function-definition>,
    function-defn-signature, function-defn-signature-setter,
    function-defn-hairy?, function-defn-hairy?-setter,
    function-defn-ct-value, function-defn-ct-value-setter,
    function-defn-transformers, function-defn-transformers-setter,
    function-defn-movable?, function-defn-flushable?;

end;

define module variables-dumper-vars
  create
    name-used, imports, prefix, excludes, renamings, exports,
    orig-name, new-name;
end;

define module variables
  use common;

  use utils;
  use errors;
  use compile-time-values;
  use tokens;
  use names;
  use definitions;
  use variables-dumper-vars;
  use od-format;

  use forwards, import: {<library>, <module>}, export: all;
  export
    $Dylan-Library, $Dylan-Module, *Current-Library*, *Current-Module*,

    find-library, library-name, note-library-definition,
    find-module, use-module, module-name, module-syntax-table,
    note-module-definition,
    <variable>, find-variable, variable-name, variable-definition,
    variable-transformers, variable-transformers-setter,
    variable-ct-evaluator, variable-ct-evaluator-setter,
    note-variable-definition,
    <use>, <renaming>,

    module-home, variable-home,
    name-inherited-or-exported?,

    dylan-var, dylan-defn, dylan-value;
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
  use od-format;
  use source, import: { source-location };
  use compile-time-values, import: { *compiler-dispatcher* };

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
    patvar-constraint, patvar-constraint-setter,
    patvar-wildcard?, patvar-wildcard?-setter,
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

define module policy
  use common;
  use utils;
  use od-format;
  use compile-time-values;

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
    <body-lexenv>, lexenv-handlers, lexenv-handlers-setter,
    <binding>, binding-name, binding-var, binding-type-var,
    add-binding, find-binding;
end;

define module ctype
  use common;
  use Introspection, import: {class-name};
  use Random, import: {random-bits};

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
    values-type-union, cinstance?, csubtype?, ctype-union, ctype-intersection,
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
  use errors;
  use names;
  use definitions;
  use variables;
  use compile-time-values;
  use ctype;
  use representation;
  use od-format;

  use forwards, import: {<cclass>}, export: all;

  export
    cclass-name, loaded?, direct-superclasses,
    closest-primary-superclass, closest-primary-superclass-setter,
    precedence-list, subclasses, direct-subclasses, sealed?,
    abstract?, primary?, functional?, not-functional?, all-slot-infos,
    all-slot-infos-setter, new-slot-infos, new-slot-infos-setter,
    override-infos, override-infos-setter, unique-id,
    set-and-record-unique-id, subclass-id-range-min,
    subclass-id-range-max, direct-type, space-representation,
    space-representation-setter, speed-representation,
    speed-representation-setter, instance-slots-layout, vector-slot,
    vector-slot-setter, class-heap-fields, class-heap-fields-setter,
    <defined-cclass>, class-defn, class-defn-setter,

    <slot-allocation>, <slot-info>, slot-introduced-by,
    slot-type, slot-type-setter, slot-getter, slot-read-only?,
    slot-guaranteed-initialized?, slot-init-value, slot-init-value-setter,
    slot-init-function, slot-init-function-setter, slot-init-keyword,
    slot-init-keyword-required?, slot-overrides,

    <instance-slot-info>, slot-representation, slot-initialized?-slot,
    slot-positions, find-slot-offset, best-idea-of-class,

    <vector-slot-info>, slot-size-slot, slot-size-slot-setter,

    <virtual-slot-info>, <class-slot-info>, <each-subclass-slot-info>,

    <override-info>, override-introduced-by, override-introduced-by-setter, 
    override-getter, override-slot,
    override-init-value, override-init-value-setter,
    override-init-function, override-init-function-setter,

    <layout-table>, layout-length, layout-holes,

    <proxy>, proxy-for,

    inherit-slots, inherit-overrides, assign-unique-ids,
    assign-slot-representations, layout-instance-slots,

    // For dumper...
    <limited-cclass>, each-subclass-slots-count;
end;

define module type-dump
  use common;
  use standard-io;
  use utils;
  use od-format;
  use compile-time-values;
  use ctype;
  use classes;
end;

define module c-representation
  use common;

  use utils;
  use variables;
  use ctype;
  use representation;
  use classes;
  use od-format;
  use compile-time-values;

  export
    seed-representations,

    <c-representation>, more-general-representation, representation-depth,
    representation-to-more-general, representation-from-more-general,
    representation-c-type,

    <general-representation>,
    <heap-representation>,
    <immediate-representation>,

    <data-word-representation>, representation-class,
    representation-data-word-member,

    *general-rep*, *heap-rep*, *boolean-rep*,
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
  use od-format;
  use forwards, import: {<ct-function>}, export: all;

  export
    ct-function-name, ct-function-signature,
    ct-function-definition, ct-function-closure-var-types,

    <ct-generic-function>, <ct-open-generic>, <ct-sealed-generic>,

    <ct-method>, ct-method-hidden?,
    <ct-accessor-method>, ct-accessor-method-slot-info,
    ct-accessor-standin, ct-accessor-standin-setter,

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


define module flow
  use common;
  use utils;
  use ctype;
  use source;
  use od-format;
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
    <initial-variable>, <operation>,
    <join-operation>, <abstract-assignment>, <assignment>,
    <join-assignment>,

    dependents, derived-type, source-exp, source-next, dependent,
    dependent-next, var-info, asserted-type, definer, definer-next,
    needs-type-check?, queue-next, definition-of, definitions,
    next-initial-variable, next-initial-variable-setter,
    defines, region, next-op, prev-op, depends-on, component-of,

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
  use representation;
  use od-format;
end;

