module: dylan-user
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/front/front-exports.dylan,v 1.2 1996/01/11 18:54:50 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

define library compiler-front
  use Dylan;
  use compiler-base;

  export cheese;
  export dump;
  export fer-convert;
  export fer-od;
  export front;
  export misc-dump;
  export primitives;
  export top-level-expressions;
  export top-level-forms;
  export builder-interface;
  export define-functions;
  export define-constants-and-variables;
  export define-libraries-and-modules;
  export define-classes;
  export forward-defn-classes;
end library;

define module forward-defn-classes
  create
    <abstract-method-definition>,
    <bindings-definition>,
    defn-init-value,
    <constant-definition>,
    <constant-method-definition>,
    <variable-definition>, var-defn-type-defn;
end module;


define module builder-interface
  create
    <flow-builder>, make-builder, builder-result, end-body, build-region,
    build-if-body, build-else, build-block-body, build-exit, build-return,
    build-loop-body, build-assignment, build-join, make-operation,
    <fer-builder>, build-let, make-unknown-call, make-literal-constant,
    make-definition-constant, make-lexical-var, make-ssa-var, make-local-var,
    make-values-cluster, copy-variable, make-exit-function,
    build-unwind-protect-body, build-function-body, make-function-literal,
    make-initial-var,

    <fer-component>, <nlx-info>;
end;


define module top-level-forms
  use common;
  use utils;
  use tokens;
  use compile-time-values;
  use parse-tree;
  use variables;
  use definitions;
  use builder-interface, import: {<fer-builder>};
  use od-format;

  export
    <top-level-form>,
    <define-tlf>, <simple-define-tlf>, tlf-defn, tlf-defn-setter,
    *Top-Level-Forms*,

    process-top-level-form, finalize-top-level-form, convert-top-level-form,

    extract-modifiers, extract-properties;
end;


define module define-libraries-and-modules
  use common;
  use utils;
  use names;
  use compile-time-values;
  use tokens;
  use parse-tree;
  use variables;
  use top-level-forms;
  use od-format;
  use builder-interface, import: {<fer-builder>};
end;

define module primitives
  use common;
  use utils;
  use ctype;
  use od-format;
  use compile-time-values;

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
  use od-format;

  export
    dump-fer, id, reset-ids,

    clone-function,

    <fer-assignment>, policy,
    <let-assignment>, let-next, <set-assignment>,

    <abstract-call>, <known-call>, <error-call>,
    <general-call>, use-generic-entry?, <unknown-call>, <mv-call>,
    <primitive>, name,
    <prologue>, function, function-setter,
    <module-var-set>, <module-var-ref>, variable,
    <self-tail-call>, self-tail-call-of, next-self-tail-call,
    <slot-access>, slot-info, <slot-ref>, <slot-set>,
    <truly-the>, guaranteed-type,
    <instance?>, type,
    nlx-info, <catch>, <throw>, throw-next, throw-next-setter, <make-catcher>,
    <disable-catcher>, disable-catcher-next, disable-catcher-next-setter,

    <constant>, <literal-constant>, value,
    <definition-constant-leaf>, const-defn,
    <uninitialized-value>,

    <debug-named-info>, debug-name,
    <values-cluster-info>, <local-var-info>, <lexical-var-info>,

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
  use forward-defn-classes;
  use variables;
  use parse-tree;
  use expand;
  use flow,
    exclude: {<assignment>},
    rename: {<expression> => <fer-expression>},
    export: all;
  use front,
    rename: {<primitive> => <fer-primitive>, <mv-call> => <fer-mv-call>},
    import: {<function-literal>, <method-literal>,
	       <module-var-ref>, <module-var-set>,
	       <function-visibility>, <catch>, <disable-catcher>,
	       <make-catcher>};
  use builder-interface, export: all;
  use ctype;
  use lexenv, export: all;
  use policy, export: all;
  use representation;
  use compile-time-functions;
  use primitives,
    import: {primitive-info-or-lose, primitive-arg-types};

  export
    fer-convert-method, fer-convert, fer-convert-body,
    fer-convert-defn-ref, fer-convert-defn-set,
    ref-dylan-defn, make-check-type-operation, make-error-operation;
end;


define module define-functions
  use common;
  use utils;
  use tokens;
  use names;
  use definitions;
  use forward-defn-classes;
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
    import: {<function-literal>, <method-literal>, <truly-the>, <mv-call>};
  use compile-time-functions;
  use od-format;

  export
    compute-signature,
    <generic-definition>, generic-defn-discriminator, generic-defn-methods,
    add-seal, ct-add-method, ct-applicable-methods, sort-methods,
    method-defn-inline-expansion, method-defn-inline-function,
    %method-defn-inline-function, %method-defn-inline-function-setter,
    <method-definition>, method-defn-of,
    <accessor-method-definition>, accessor-method-defn-slot-info,
    <getter-method-definition>, <setter-method-definition>,
    <define-generic-tlf>,
    <define-method-tlf>,
    implicitly-define-generic,

    $abstract-method-definition-slots;
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
  use forward-defn-classes;
  use ctype;
  use variables;
  use tokens;
  use builder-interface;
  use fer-convert;
  use source;
  use front, import: {<method-literal>};
  use define-functions;
  use expand;
  use compile-time-functions;
  use od-format;

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
  use od-format;

  export
    class-defn-defered-evaluations-function, class-defn-maker-function;
end;

define module top-level-expressions
  use common;
  use utils;
  use tokens;
  use parse-tree;
  use top-level-forms;
  use lexenv;
  use builder-interface;
  use fer-convert;
  use expand;
  use od-format;

  export <magic-interal-primitives-placeholder>;
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

define module misc-dump
  use common;
  use standard-io;
  use utils;
  use od-format;
  use compile-time-values;
  use ctype;
  use classes;
  use variables;
  use variables-dumper-vars;
  use names;
end;

define module fer-od
  use common;
  use standard-io;
  use utils;
  use source;
  use policy;
  use od-format;
  use compile-time-values;
  use ctype;
  use classes;
  use variables;
  use names;
  use front;
  use flow;
  use builder-interface;
  
end;

define module cheese
  use common;
  use utils;
  use compile-time-values;
  use names;
  use definitions;
  use forward-defn-classes;
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

