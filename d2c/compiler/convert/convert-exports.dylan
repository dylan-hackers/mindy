module: dylan-user
rcs-header: $Header: /scm/cvs/src/d2c/compiler/convert/convert-exports.dylan,v 1.11 2003/07/06 03:50:00 housel Exp $
copyright: see below


//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000, 2001  Gwydion Dylan Maintainers
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

define library compiler-convert
  use Dylan;
  use compiler-base;
  use compiler-front;
  use compiler-parser;

  export
    define-classes, define-libraries-and-modules, define-functions,
    fer-convert, top-level-expressions,
    
    // for browser support:
    define-macros, define-constants-and-variables;
end library compiler-convert;


define module lexenv
  use common;

  use utils;
  use policy;
  use flow, import: {<abstract-variable>};
  use variables, import: {<variable>, find-variable};
  use names;
  use tokens;
  use source;
  use top-level-forms;

  export
    <lexenv>, lexenv-policy,
    <body-lexenv>, lexenv-handlers, lexenv-handlers-setter,
    lexenv-method-name,
    <top-level-binding>,
    <binding>, binding-name, binding-var, binding-type-var,
    add-binding, find-binding, local-binding?, lexenv-for-tlf;
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
  use macros;

  use lexenv;

  export
    ct-eval, ct-mv-eval;
end;


define module expanders
  use common;
  use utils;
  use source;
  use errors;
  use tokens;
  use variables;
  use compile-time-values;

  use fragments;
  use parse-tree;
  use parser;
  use macros;

  use compile-time-eval;

  export
    split-fragment-at-commas, expression-from-fragment,
    extract-name, extract-boolean, extract-identifier-or-false,
    extract-identifier, extract-properties, make-magic-fragment;

end module expanders;


define module fer-convert
  use common;

  use utils;
  use source, exclude: {source};
  use errors;
  use tokens;
  use names;
  use compile-time-values;
  use compile-time-functions;
  use ctype;
  use signature-interface;
  use definitions;
  use variables;
  use representation;
  use policy, export: all;
  use flow, export: all;

  use parse-tree;
  use macros;

  use front,
    import: {<primitive>, <function-literal>, <method-literal>,
         <module-var-ref>, <module-var-set>,
         <function-visibility>, <catch>, <disable-catcher>,
         <make-catcher>};
  use builder-interface, export: all;
  use primitives,
    import: {primitive-info-or-lose, priminfo-arg-types};
  use variable-definitions;

  use lexenv, export: all;
  use compile-time-eval;

  export
    fer-convert-method, fer-convert;
end;


define module define-macros
  use common;
  use utils;
  use errors;
  use tokens;
  use source;
  use definitions;
  use variables;

  use parse-tree;
  use parser;
  use macros;

  use builder-interface;

  use top-level-forms;

  export
    <define-macro-tlf>; // browser support
end module define-macros;


define module define-libraries-and-modules
  use common;
  use utils;
  use errors;
  use names;
  use compile-time-values;
  use tokens;
  use parse-tree;
  use variables;
  use top-level-forms;
  use od-format;
  use builder-interface, import: {<fer-builder>};
  use fragments;
  use macros;
  use expanders;
  use parser;

  export
    <define-library-tlf>,
    define-library-name, define-library-library,
    define-library-uses, define-library-exports,
    <define-module-tlf>,
    define-module-name, define-module-module,
    define-module-uses, define-module-exports, define-module-creates;
end;


define module define-functions
  use common;

  use utils;
  use od-format;
  use source;
  use errors;
  use tokens;
  use names;
  use definitions;
  use variables;
  use compile-time-values;
  use compile-time-functions;
  use ctype;
  use classes;
  use transformers;
  use signature-interface;

  use fragments;
  use parse-tree;
  use parser;
  use macros;

  use builder-interface;
  use function-definitions;
  use front,
    import: {<function-literal>, <method-literal>, <truly-the>, <mv-call>,
             <literal-constant>, value};
  use top-level-forms;

  use expanders;
  use lexenv;
  use compile-time-eval;
  use fer-convert;

  use abstract-optimizer;

  export
    *implicitly-define-next-method*,
    compute-signature,
    expand-inline-function,
    implicitly-define-generic;
end;

define module define-constants-and-variables
  use common;
  use utils;
  use od-format;
  use source;
  use errors;
  use compile-time-values;
  use names;
  use definitions;
  use ctype;
  use variables;
  use tokens;
  use compile-time-functions;

  use builder-interface;
  use front, import: {<method-literal>, <primitive>};
  use function-definitions;
  use variable-definitions;
  use top-level-forms;

  use fragments;
  use parse-tree;
  use parser;
  use macros;

  use lexenv;
  use compile-time-eval;
  use expanders;
  use fer-convert;
  use define-functions;

  export
    expand-until-method-ref,
    
    // for browser support:
    <define-constant-tlf>, <define-variable-tlf>, tlf-variables,
    <define-constant-method-tlf>, <constant-definition>;
end;

define module define-classes
  use common;
  use utils;
  use source;
  use errors;
  use compile-time-values;
  use tokens;
  use names;
  use definitions;
  use variables;
  use ctype;
  use classes;
  use compile-time-functions;
  use od-format;
  use representation;

  use top-level-forms;
  use builder-interface;
  use signature-interface;
  use front,
    import: {<heap-slot-ref>, <data-word-ref>, <heap-slot-set>,
       <uninitialized-value>, <primitive>, <fer-function-region>,
       <function-literal>, <method-literal>};
  use c-representation;
  use function-definitions;

  use source-utilities;
  use fragments;
  use parse-tree;
  use parser;
  use macros;

  use lexenv;
  use compile-time-eval;
  use expanders;
  use fer-convert;
  use define-constants-and-variables;
  use define-functions;

  use abstract-optimizer;

  export
    class-defn-overrides, class-defn-slots,

    override-defn-info,

    slot-defn-info, slot-defn-allocation, slot-defn-getter, slot-defn-setter,
    
    // for browser support:
    <slot-defn>, slot-defn-getter-name,
    class-defn-cclass,
    <local-class-definition>;
end;

define module top-level-expressions
  use common;
  use utils;
  use source;
  use od-format;
  use errors;
  use tokens;

  use builder-interface;
  use top-level-forms;

  use parse-tree;
  use parser;
  use macros;

  use lexenv;
  use fer-convert;

  export <expression-tlf>, tlf-expression;
end;


