module: dylan-user
rcs-header: $Header: /scm/cvs/src/d2c/compiler/front/front-exports.dylan,v 1.12 2003/06/24 21:00:08 andreas Exp $
copyright: see below

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000, 2001, 2002  Gwydion Dylan Maintainers
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
//    University, and the Gwydion Dylan Maintainers.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
// Also, see http://www.gwydiondylan.org/ for updates and documentation. 
//
//======================================================================

define library compiler-front
  use Dylan;
  use compiler-base;

  export fer-od;
  export front;
  export primitives;
  export builder-interface;
  export function-definitions;
  export variable-definitions;
  export top-level-forms;
  export abstract-optimizer;
  export xep-tools;
end library;


define module builder-interface
  create
    <flow-builder>, make-builder, builder-result, end-body, build-region,
    build-if-body, build-else, build-block-body, build-exit, build-return,
    build-loop-body, build-assignment, build-join, make-operation,
    <fer-builder>, build-let, make-unknown-call, make-literal-constant,
    make-definition-constant, make-lexical-var, make-ssa-var, make-local-var,
    make-values-cluster, copy-variable, make-exit-function,
    build-unwind-protect-body, build-function-body, make-function-literal,
    make-initial-var, build-slot-home, add-body-assignment,

    build-defn-ref, build-defn-set, ref-dylan-defn,
    make-check-type-operation, make-error-operation,

    <fer-component>, <nlx-info>;
end;


define module primitives
  use common;
  use utils;
  use ctype;
  use od-format;
  use compile-time-values;

  export
    <primitive-info>, define-primitive, primitive-info-or-lose,
    priminfo-name, priminfo-arg-types, priminfo-result-type,
    priminfo-side-effect-free?, priminfo-pure?, priminfo-cseable?,
    priminfo-type-deriver, priminfo-transformer, priminfo-emitter,


    define-primitive-type-deriver,
    define-primitive-transformer,
    define-primitive-emitter;
end;


define module variable-definitions
  use common;

  use compile-time-values;
  use ctype;
  use definitions;

  export
    <bindings-definition>, defn-type-setter, %defn-init-value,
    defn-init-value, defn-init-value-setter,
    <variable-definition>, var-defn-type-defn, var-defn-type-defn-setter;
end module variable-definitions;


define module front
  use common;
  use utils;
  use compile-time-values;
  use names;
  use definitions;
  use variables;
  use variable-definitions, import: {<bindings-definition>};
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

    <abstract-call>, <known-call>, <error-call>, <delayed-optimization-call>,
    <general-call>, use-generic-entry?, ct-source-location, <unknown-call>, <mv-call>,
    <primitive>, primitive-name, primitive-info,
    <prologue>, function, function-setter,
    preferred-names, preferred-names-setter,
    <module-var-set>, <module-var-ref>, variable,
    <slot-access>, slot-info,
    <slot-ref>, <heap-slot-ref>, <data-word-ref>,
    <heap-slot-set>,
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
    <callback-literal>, callback-entry, callback-entry-setter,
    <exit-function>,

    <fer-function-region>, prologue, argument-types, argument-types-setter,
    result-type, result-type-setter, guessed-result-type,
    guessed-result-type-setter, hidden-references?,
    self-call-block, self-call-block-setter,
    self-tail-call-temps, self-tail-call-temps-setter,
    calling-convention, calling-convention-setter,
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

define module function-definitions
  use common;

  use utils;
  use od-format;
  use source;
  use errors;
  use compile-time-values;
  use compile-time-functions;
  use ctype;
  use classes;
  use names;
  use definitions;
  use variables;
  use signature-interface;
  use transformers;

  use front;

  export
    <generic-definition>, generic-defn-sealed?, generic-defn-sealed?-setter,
    generic-defn-methods, generic-defn-discriminator,
    %generic-defn-discriminator, %generic-defn-discriminator-setter,

    <implicit-generic-definition>,

    <abstract-method-definition>, method-defn-inline-function,
    %method-defn-inline-function-setter,
    <inline-type>, method-defn-inline-type,

    <method-definition>, method-defn-congruent?, method-defn-of,

    <accessor-method-definition>, accessor-method-defn-slot-info,
    accessor-method-defn-slot-info-setter,

    <getter-method-definition>,
    <setter-method-definition>,

    add-seal, <seal-info>, seal-types, 

    ct-add-method, ct-applicable-methods, sort-methods,
    static-next-method-info,

    $abstract-method-definition-slots, dump-queued-methods;
    
end module function-definitions;


define module top-level-forms
  use common;

  use utils;
  use od-format;
  use source;
  use errors;
  use tokens;
  use compile-time-values;
  use variables;
  use definitions;

  use builder-interface, import: {<fer-builder>};
  use variable-definitions;

  export
    *Top-Level-Forms*,
    <top-level-form>, finalize-top-level-form, convert-top-level-form,

    <define-tlf>, <simple-define-tlf>, tlf-defn, tlf-defn-setter,

    <define-generic-tlf>,

    <define-method-tlf>,

    <define-bindings-tlf>, tlf-required-defns, tlf-rest-defn,

    <define-class-tlf>, tlf-init-function-defns,

    <magic-interal-primitives-placeholder>;
end;

define module abstract-optimizer
  use common;
  use flow, import: {<component>};

  export
    <abstract-optimizer>,
      debug-optimizer, debug-optimizer-setter, optimizer-options,
    optimize-component,
    *current-optimizer*;
end module abstract-optimizer;

// XXX - Move xep someplace more logical someday.
// This code is pretty weird--it's needed by both the optimizers and cback,
// but it doesn't rely on anything that isn't declared in front. This was
// moved in here by emk for three reasons:
//   1) Build performance when working on the optimizers.
//   2) Everybody who needs this code already uses this library.
//   3) No lower library already imports precisely right libraries.
// This code originally lived in optimizer/xep.dylan.
//
define module xep-tools
  use common;
  use utils;
  use errors;
  use compile-time-values;
  use names;
  use definitions;
  use variables, exclude: {<renaming>};
  use flow;
  use front;
  use ctype;
  use classes;
  use signature-interface;
  use source;
  use builder-interface;
  use policy;
  use primitives;
  use transformers;
  use compile-time-functions;
  use function-definitions;
  use abstract-optimizer;

  export
    build-xep,
    build-local-xeps,
    build-callback-xep,
    build-xep-component;
end module xep-tools;
