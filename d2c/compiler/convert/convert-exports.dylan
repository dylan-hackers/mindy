module: dylan-user
rcs-header: $Header: /scm/cvs/src/d2c/compiler/convert/convert-exports.dylan,v 1.1 1998/05/03 19:55:36 andreas Exp $
copyright: Copyright (c) 1996  Carnegie Mellon University
	   All rights reserved.


//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
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
//    University.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports, questions, comments, and suggestions should be sent by
// E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
//
//======================================================================

define library compiler-convert
  use Dylan;
  use compiler-base;
  use compiler-front;
  use compiler-parser;

  export
    define-classes, define-functions;
end library compiler-convert;


define module lexenv
  use common;

  use utils;
  use policy;
  use flow, import: {<abstract-variable>};
  use names;
  use tokens;
  use source;
  use top-level-forms;

  export
    <lexenv>, lexenv-policy, lexenv-policy-setter,
    <body-lexenv>, lexenv-handlers, lexenv-handlers-setter,
    lexenv-method-name, lexenv-method-name-setter,
    <binding>, binding-name, binding-var, binding-type-var,
    add-binding, find-binding, lexenv-for-tlf;
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
  // use definitions;
  use variables;
  use compile-time-values;

  use fragments;
  use parse-tree;
  use parser;
  use macros;

  use compile-time-eval;

  export
    split-fragment-at-commas, expression-from-fragment,
    extract-name, extract-boolean, extract-properties,
    make-magic-fragment;

end module expanders;


define module fer-convert
  use common;

  use utils;
  use source;
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
  use definitions;
  use variables;

  use parse-tree;
  use parser;
  use macros;

  use builder-interface;

  use top-level-forms;

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
	     <literal-constant>, value, optimize-component};
  use top-level-forms;

  use expanders;
  use lexenv;
  use compile-time-eval;
  use fer-convert;

  export
    *implicitly-define-next-method*,
    compute-signature,
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
    expand-until-method-ref;
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
	     <function-literal>, <method-literal>, optimize-component};
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

  export
    class-defn-overrides, class-defn-slots,

    override-defn-info,

    slot-defn-info, slot-defn-allocation, slot-defn-getter, slot-defn-setter;
    
end;

define module top-level-expressions
  use common;

  use utils;
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
end;


