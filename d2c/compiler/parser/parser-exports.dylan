module: dylan-user
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/parser/parser-exports.dylan,v 1.3 1996/03/17 00:54:29 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

define library compiler-parser
  use Dylan;
  use compiler-base;

  export lexer;
  export parse-tree;
  export fragments;
  export parser;
  export macros;
end;

define module lexer
  use common;

  use utils;
  use errors;
  use source;
  use compile-time-values;
  use tokens;
  use variables;

  export
    <lexer>,

    add-feature, remove-feature;
end;

define module fragments
  use common;

  use utils;
  use tokens;

  export
    <fragment>,
    <empty-fragment>,
    <compound-fragment>, fragment-head, fragment-tail,
    <elementary-fragment>, fragment-prev, fragment-next,
    <token-fragment>, fragment-token,
    <bracketed-fragment>, fragment-left-token, fragment-contents,
    fragment-right-token,

    copy-fragment, append-fragments!,

    <fragment-tokenizer>, tokenizer-potential-end-point;
end;

define module parse-tree
  use common;
  use utils;
  use tokens;
  use fragments;
  use od-format;
  use source;
  use compile-time-values;

  export

    <method-parse>, method-name, method-name-setter, method-parameters,
    method-returns, method-body,

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

    <method-ref-parse>, method-ref-method,

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

    <property>, prop-comma, prop-keyword, prop-value;
end;

define module parser
  use common;
  use utils;
  use source;
  use errors;
  use compile-time-values;
  use tokens;
  use variables;
  use fragments;
  use parse-tree;

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
  use common;
  use utils;
  use source;
  use errors;
  use compile-time-values;
  use tokens;
  use names;
  use definitions;
  use variables;
  use fragments;
  use parse-tree;
  use parser;
  use od-format;

  export
    <macro-definition>, macro-expand, recursively-macro-expand,

    define-procedural-expander;
end;

