module: parse-tree
copyright: see below

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
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

// Parse tree classes.
//
//  <method-parse>
//
//  <callback-method-parse>
//
//  <variable-list>
//	<parameter-list>
//
//  <parameter>
//	<keyword-parameter>
//
//  <bindings-parse>
//
//  <constituent-parse> {abstract, open}
//	<macro-call-parse> {abstract}
//	<definition-parse> {abstract, open}
//	    <definition-macro-call-parse> [macro-call-parse] {abstract}
//		<body-style-definition-macro-call-parse>
//		<list-style-definition-macro-call-parse>
//	    <define-macro-parse>
//	<local-declaration-parse> {abstract}
//	    <let-parse>
//	    <handler-parse>
//	    <local-parse>
//	<expression-parse> {abstract}
//	    <literal-ref-parse>
//	    <varref-parse>
//	    <varset-parse>
//	    <funcall-parse>
//	    <dot-parse>
//	    <statement-parse> [macro-call-parse]
//	    <function-macro-call-parse> [macro-call-parse]
//	    <body-parse>
//	    <bind-exit-parse>
//	    <if-parse>
//	    <method-ref-parse>
//	    <callback-method-ref-parse>
//	    <primitive-parse>
//	    <unwind-protect-parse>
//	    
//  <rule-set> {abstract}
//	<main-rule-set>
//	<auxiliary-rule-set>
//
//  <rule>
//      <main-rule> {abstract}
//          <define-rule>
//		<body-style-define-rule>
//		<list-style-define-rule>
//	    <statement-rule>
//	    <function-rule>
//
//  <pattern>
//	<empty-pattern>
//	<binary-pattern>
//	    <semicolon-patter>
//	    <comma-pattern>
//	    <sequential-pattern>
//	<simple-pattern>
//	    <bracketed-pattern>
//	    <variable-pattern>
//	    <bindings-pattern>
//	    <name-pattern>
//	    <arrow-pattern>
//	    <pattern-variable>
//		<pattern-keyword>
//	<property-list-pattern>
//
//  <template>
//	<procedural-template>
//	<literal-template>
//
//  <bracketed-element>
//  <pattern-variable-reference>
//	<simple-pattern-variable-reference>
//	<ellipsis-pattern-variable-reference>
//	<concatenating-pattern-variable-reference>
//	<sequence-pattern-variable-reference>
//	<unhygienic-pattern-variable-reference>
//
//  <property>
//


// Method Definitions.

// <method-parse> -- exported.
//
// Encapsulation of the parse of a method.
// 
define class <method-parse> (<source-location-mixin>)
  //
  // The name of this method, if there is one.  Settable because that makes
  // the parser a tad bit simpler.
  slot method-name :: false-or(<identifier-token>),
    init-value: #f, init-keyword: name:;
  //
  // The parameter list.
  constant slot method-parameters :: <parameter-list>,
    required-init-keyword: parameters:;
  //
  // Parameter list describing the return values.
  constant slot method-returns :: <variable-list>
    = make(<variable-list>,
	   rest: make(<parameter>,
		      name: make(<identifier-token>,
				 kind: $raw-ordinary-word-token,
				 symbol: #"results"))),
    init-keyword: returns:;
  //
  // The body.
  constant slot method-body :: <expression-parse>,
    required-init-keyword: body:;
end class <method-parse>;

define sealed domain make (singleton(<method-parse>));
define sealed domain initialize (<method-parse>);

/*
define sealed method print-object
    (meth :: <method-parse>, stream :: <stream>) => ();
  pprint-fields(meth, stream,
		if (meth.method-name) name: end, meth.method-name,
		parameters: meth.method-parameters,
		returns: meth.method-returns,
		body: meth.method-body);
end method print-object;
*/


// <callback-method-parse> -- exported.
//
// Encapsulation of the parse of a callback method.
// 
define class <callback-method-parse> (<method-parse>)

end class <callback-method-parse>;

define sealed domain make (singleton(<callback-method-parse>));
define sealed domain initialize (<callback-method-parse>);



// <variable-list> -- exported.
//
// Used to represent simple parameter lists (i.e. lets & result values).
//
define class <variable-list> (<object>)
  //
  // Vector of <parameter>s.  Assignable, because that makes parsing a tad
  // bit easier.
  slot varlist-fixed :: <simple-object-vector>,
    init-value: #[], init-keyword: fixed:;
  //
  // Either #f or the name of the #rest parameter.  Again, assignable because
  // that makes parsing a tad bit easier.
  slot varlist-rest :: false-or(<parameter>),
    init-value: #f, init-keyword: rest:;
end class <variable-list>;

define sealed domain make (singleton(<variable-list>));
define sealed domain initialize (<variable-list>);

/*
define sealed method print-object
    (paramlist :: <variable-list>, stream :: <stream>) => ();
  pprint-fields(paramlist, stream,
		fixed: paramlist.varlist-fixed,
		if (paramlist.varlist-rest) #"#rest" end,
		paramlist.varlist-rest);
end method print-object;
*/


// <parameter-list> -- exported.
//
// Used to represent hairy parameter lists (i.e. method argument parameter
// lists).
// 
define class <parameter-list> (<variable-list>)
  //
  // Either #f or the name of the #next parameter.  Assignable because that
  // makes parsing a tad bit easier.
  slot paramlist-next :: false-or(<identifier-token>),
    init-value: #f, init-keyword: next:;
  //
  // Either #f or a (possibly empty) vector of <keyword-parameter>s.
  constant slot paramlist-keys :: false-or(<simple-object-vector>),
    init-value: #f, init-keyword: keys:;
  //
  // #t if #all-keys was supplied, #f otherwise.
  constant slot paramlist-all-keys? :: <boolean>,
    init-value: #f, init-keyword: all-keys:;
end class <parameter-list>;

define sealed domain make (singleton(<parameter-list>));
define sealed domain initialize (<parameter-list>);

/*
define sealed method print-object
    (paramlist :: <parameter-list>, stream :: <stream>) => ();
  pprint-fields(paramlist, stream,
		fixed: paramlist.varlist-fixed,
		if (paramlist.varlist-rest) #"#rest" end,
		  paramlist.varlist-rest,
		if (paramlist.paramlist-next) #"#next" end,
		  paramlist.paramlist-next,
		if (paramlist.paramlist-keys) #"#key" end,
		  paramlist.paramlist-keys,
		if (paramlist.paramlist-all-keys?) #"#all-keys" end,
		  paramlist.paramlist-all-keys?);
end method print-object;
*/

// <parameter> -- exported.
//
// Used whenever we need to represent a (potentially) typed variable.
//
define class <parameter> (<object>)
  //
  // The name.
  constant slot param-name :: <identifier-token>,
    required-init-keyword: name:;
  //
  // The type expression if there is one, or #f if not.
  constant slot param-type :: false-or(<expression-parse>) = #f,
    init-keyword: type:;
end class <parameter>;

define sealed domain make (singleton(<parameter>));
define sealed domain initialize (<parameter>);

/*
define sealed method print-object
    (param :: <parameter>, stream :: <stream>) => ();
  pprint-fields(param, stream,
		name: param.param-name,
		if (param.param-type) type: end, param.param-type);
end method print-object;
*/

// <keyword-parameter> -- exported.
//
// Like a <parameter>, but also has a keyword and optional default.
//
define class <keyword-parameter> (<parameter>)
  //
  constant slot param-keyword :: <symbol>,
    required-init-keyword: keyword:;
  //
  constant slot param-default :: false-or(<expression-parse>),
    required-init-keyword: default:;
end class <keyword-parameter>;

define sealed domain make (singleton(<keyword-parameter>));

/*
define sealed method pprint-object
    (param :: <keyword-parameter>, stream :: <stream>) => ();
  pprint-fields(param, stream,
		keyword: param.param-keyword,
		name: param.param-name,
		if (param.param-type) type: end, param.param-type,
		if (param.param-default) default: end, param.param-default);
end method pprint-object;
*/


// <bindings-parse> -- exported.
//
// The results of parsing a bindings form.
//
define class <bindings-parse> (<object>)
  //
  // The parameters being bound.
  constant slot bindings-variables :: <variable-list>,
    required-init-keyword: variables:;
  //
  // The expression they are being bound to.
  constant slot bindings-expression :: <expression-parse>,
    required-init-keyword: expression:;
end;

define sealed domain make (singleton(<bindings-parse>));
define sealed domain initialize (<bindings-parse>);

/*
define sealed method print-object
    (bindings :: <bindings-parse>, stream :: <stream>) => ();
  pprint-fields(bindings, stream,
		param-list: bindings.bindings-variables,
		expression: bindings.bindings-expression);
end;
*/



// Constituents: definitions, local-declarations, and expressions.

// <constituent-parse> -- exported.
//
// The various different kinds of constituent inherit from this.
//
define open abstract class <constituent-parse> (<source-location-mixin>)
end class <constituent-parse>;

define sealed domain make (singleton(<constituent-parse>));
define sealed domain initialize (<constituent-parse>);

// <macro-call-parse> -- exported.
//
// Abstract superclass for all the different kinds of macro calls.
// 
define abstract class <macro-call-parse> (<constituent-parse>)
  //
  // The word (DEFINE-BODY, DEFINE-LIST, STATEMENT, or FUNCTION) that names
  // the macro being called.
  constant slot macro-call-word :: <identifier-token>,
    required-init-keyword: word:;
  //
  // The fragment that makes up the body of the macro call.
  constant slot macro-call-fragment :: <fragment>,
    required-init-keyword: fragment:;
end class <macro-call-parse>;

define sealed domain make (singleton(<macro-call-parse>));

/*
define sealed method print-object
    (call :: <macro-call-parse>, stream :: <stream>) => ();
  pprint-fields(call, stream,
		word: call.macro-call-word,
		fragment: call.macro-call-fragment);
end method print-object;
*/

// <definition-parse> -- exported.
//
// Shared by the various defining forms.
// 
define open abstract class <definition-parse> (<constituent-parse>)
end class <definition-parse>;

define sealed domain make (singleton(<definition-parse>));

// <definition-macro-call-parse> -- exported.
//
// A call to some kind of definition macro.
//
define abstract class <definition-macro-call-parse>
    (<macro-call-parse>, <definition-parse>)
  //
  // Vector of <token>s for the modifiers.
  constant slot definition-modifiers :: <simple-object-vector>,
    required-init-keyword: modifiers:;
end class <definition-macro-call-parse>;

define sealed domain make (singleton(<definition-macro-call-parse>));

/*
define sealed method print-object
    (object :: <definition-macro-call-parse>, stream :: <stream>) => ();
  pprint-fields(object, stream,
		modifiers: object.definition-modifiers,
		word: object.macro-call-word,
		fragment: object.macro-call-fragment);
end method print-object;
*/

define sealed method print-message
    (call :: <definition-macro-call-parse>, stream :: <stream>) => ();
  format(stream, "``define %s'' macro", call.macro-call-word.token-symbol);
end method print-message;

// <body-style-definition-macro-call-parse> -- exported.
//
// A call to a body style definition macro.
// 
define class <body-style-definition-macro-call-parse>
    (<definition-macro-call-parse>)
end class <body-style-definition-macro-call-parse>;

define sealed domain make
  (singleton(<body-style-definition-macro-call-parse>));

// <list-style-definition-macro-call-parse> -- exported.
//
// A call to a list style definition macro.
// 
define class <list-style-definition-macro-call-parse>
    (<definition-macro-call-parse>)
end class <list-style-definition-macro-call-parse>;

define sealed domain make
  (singleton(<list-style-definition-macro-call-parse>));


// <local-declaration-parse> -- exported.
//
// Abstract superclass for all the various local declarations.
//
define abstract class <local-declaration-parse> (<constituent-parse>)
end class <local-declaration-parse>;

define sealed domain make (singleton(<local-declaration-parse>));

// <let-parse> -- exported.
//
// A let is just a bunch of local bindings.  We don't explicitly represent the
// scoping of lets.  It is up to ICR to notice that the stuff after a let is
// inside the scope of the bindings established by the let.
// 
define class <let-parse> (<local-declaration-parse>)
  //
  constant slot let-variables :: <variable-list>,
    required-init-keyword: variables:;
  //
  constant slot let-expression :: <expression-parse>,
    required-init-keyword: expression:;
end class <let-parse>;

define sealed domain make (singleton(<let-parse>));

/*
define sealed method print-object
    (form :: <let-parse>, stream :: <stream>) => ();
  pprint-fields(form, stream,
		variables: form.let-variables,
		expression: form.let-expression);
end method print-object;
*/

// <handler-parse> -- exported.
//
define class <handler-parse> (<local-declaration-parse>)
  //
  // The condition type expression.
  constant slot handler-type :: <expression-parse>,
    required-init-keyword: type:;
  //
  // The option arguments, each an <expression-parse>.
  constant slot handler-options :: <simple-object-vector> = #[],
    init-keyword: options:;
  //
  // The expression to compute the handler.
  constant slot handler-expression :: <expression-parse>,
    required-init-keyword: handler:;
end class <handler-parse>;

define sealed domain make (singleton(<handler-parse>));

/*
define sealed method print-object
    (form :: <handler-parse>, stream :: <stream>) => ();
  pprint-fields(form, stream,
		type: form.handler-type,
		options: form.handler-options,
		expression: form.handler-expression);
end method print-object;
*/

// <local-parse> -- exported.
//
define class <local-parse> (<local-declaration-parse>)
  //
  // Vector of <method-parse>s.
  constant slot local-methods :: <simple-object-vector>,
    required-init-keyword: methods:;
end class <local-parse>;

define sealed domain make (singleton(<local-parse>));


/*
define sealed method print-object
    (loc :: <local-parse>, stream :: <stream>) => ();
  pprint-fields(loc, stream, methods: loc.local-methods);
end method print-object;
*/


// <expression-parse> -- exported.
//
// Abstract superclass for all the different kinds of expressions.
//
define abstract class <expression-parse> (<constituent-parse>)
end class <expression-parse>;

define sealed domain make (singleton(<expression-parse>));


// <literal-ref-parse> -- exported.
//
define class <literal-ref-parse> (<expression-parse>)
  constant slot litref-literal :: <literal>,
    required-init-keyword: literal:;
end class <literal-ref-parse>;

define sealed domain make (singleton(<literal-ref-parse>));

/*
define sealed method print-object
    (lit :: <literal-ref-parse>, stream :: <stream>) => ();
  pprint-fields(lit, stream, value: lit.litref-literal);
end method print-object;
*/


// <varref-parse> -- exported.
//
// A reference to some variable.
// 
define class <varref-parse> (<expression-parse>)
  //
  // The identifier being referenced.
  constant slot varref-id :: <identifier-token>,
    required-init-keyword: id:;
end class <varref-parse>;

define sealed domain make (singleton(<varref-parse>));

/*
define sealed method print-object
    (varref :: <varref-parse>, stream :: <stream>) => ();
  pprint-fields(varref, stream, name: varref.varref-id);
end method print-object;
*/

// <varset-parse> -- exported.
//
// An assignment, as in ``foo := bar''.  Not actually produced by the parser.
// Instead, the := macro expands into one of these.
//
define class <varset-parse> (<expression-parse>)
  //
  // The place being assigned.
  constant slot varset-id :: <identifier-token>,
    required-init-keyword: id:;
  //
  // The new value being put in that place.
  constant slot varset-value :: <expression-parse>,
    required-init-keyword: value:;
end class <varset-parse>;

define sealed domain make (singleton(<varset-parse>));

/*
define sealed method print-object
    (varset :: <varset-parse>, stream :: <stream>) => ();
  pprint-fields(varset, stream,
		id: varset.varset-id,
		value: varset.varset-value);
end method print-object;
*/


// <funcall-parse> -- exported.
//
// A function application.
//
define class <funcall-parse> (<expression-parse>)
  //
  // The function being called.
  constant slot funcall-function :: <expression-parse>,
    required-init-keyword: function:;
  //
  // The arguments (as <expression-parse>s) to pass it.
  constant slot funcall-arguments :: <simple-object-vector>,
    required-init-keyword: arguments:;
end class <funcall-parse>;

define sealed domain make (singleton(<funcall-parse>));

/*
define sealed method print-object
    (funcall :: <funcall-parse>, stream :: <stream>) => ();
  let args = funcall.funcall-arguments;
  let vec = make(<vector>, size: args.size * 2);
  for (arg in args, i from 0)
    vec[i * 2] := format-to-string("argument %d", i);
    vec[i * 2 + 1] := arg;
  end for;
  apply(pprint-fields, funcall, stream,
	function: funcall.funcall-function,
	vec);
end method print-object;
*/

// <dot-parse> -- exported.
//
// A function application, in . form.  We don't just use a <funcall-parse>
// because we need to preserve the order of evaluation of the arguments.
//
define class <dot-parse> (<expression-parse>)
  //
  // The argument in the . call.
  constant slot dot-operand :: <expression-parse>,
    required-init-keyword: operand:;
  //
  // The thing we are calling.
  constant slot dot-name :: <identifier-token>,
    required-init-keyword: name:;
end class <dot-parse>;

define sealed domain make (singleton(<dot-parse>));

/*
define sealed method print-object
    (dot :: <dot-parse>, stream :: <stream>) => ();
  pprint-fields(dot, stream, operand: dot.dot-operand, name: dot.dot-name);
end method print-object;
*/


// <statement-parse> -- exported.
//
// A generic statement -- i.e. before macro expansion.
// 
define class <statement-parse> (<macro-call-parse>, <expression-parse>)
end class <statement-parse>;

define sealed domain make (singleton(<statement-parse>));

define sealed method print-message
    (call :: <statement-parse>, stream :: <stream>) => ();
  format(stream, "``%s'' statement", call.macro-call-word.token-symbol);
end method print-message;


// <function-macro-call-parse> -- exported.
//
// An invocation of some function-call macro.
// 
define class <function-macro-call-parse>
    (<macro-call-parse>, <expression-parse>)
end class <function-macro-call-parse>;

define sealed domain make (singleton(<function-macro-call-parse>));

define sealed method print-message
    (call :: <function-macro-call-parse>, stream :: <stream>) => ();
  format(stream, "``%s'' function macro", call.macro-call-word.token-symbol);
end method print-message;


// <body-parse> -- exported.
//
// A sequenctial body of constituents.  Used wherever we need a body.
// 
define class <body-parse> (<expression-parse>)
  //
  // Vector of <constituent-parse>s that make up this body.
  constant slot body-parts :: <simple-object-vector>,
    required-init-keyword: parts:;
end class <body-parse>;

define sealed domain make (singleton(<body-parse>));

/*
define sealed method print-object
    (body :: <body-parse>, stream :: <stream>) => ();
  pprint-fields(body, stream, parts: body.body-parts);
end method print-object;
*/


// <bind-exit-parse> -- exported.
//
// Internal parse tree node used to represent the binding of an exit function.
// Not actually generated by the parser.  Instead, the block macro magically
// expands into these as necessary.
// 
define class <bind-exit-parse> (<expression-parse>)
  //
  // The identifier to bind the exit function to.
  constant slot exit-name :: <identifier-token>,
    required-init-keyword: name:;
  //
  // The body to execute in the scope of the exit-function.
  constant slot exit-body :: <expression-parse>,
    required-init-keyword: body:;
end class <bind-exit-parse>;

define sealed domain make (singleton(<bind-exit-parse>));

/*
define sealed method print-object
    (form :: <bind-exit-parse>, stream :: <stream>) => ();
  pprint-fields(form, stream, name: form.exit-name, body: form.exit-body);
end method print-object;
*/


// <if-parse> -- exported.
//
// Internal parse tree node used to represent a conditional.  Not actually
// generated by the parser.  Instead, the if macro magically expands into
// these as necessary.
// 
define class <if-parse> (<expression-parse>)
  //
  // The condition to test.
  constant slot if-condition :: <expression-parse>,
    required-init-keyword: condition:;
  //
  // What to do if it is true.
  constant slot if-consequent :: <expression-parse>,
    required-init-keyword: consequent:;
  //
  // And what to do if it is false.
  constant slot if-alternate :: <expression-parse>,
    required-init-keyword: alternate:;
end class <if-parse>;

define sealed domain make (singleton(<if-parse>));

/*
define sealed method print-object
    (statement :: <if-parse>, stream :: <stream>) => ();
  pprint-fields(statement, stream,
		condition: statement.if-condition,
		consequent: statement.if-consequent,
		alternate: statement.if-alternate);
end method print-object;
*/


// <method-ref-parse> -- exported.
//
define class <method-ref-parse> (<expression-parse>)
  //
  // The method referenced.
  constant slot method-ref-method :: <method-parse>,
    required-init-keyword: method:;

  constant slot method-ref-options :: <simple-object-vector> = #[],
    init-keyword: options:;
end class <method-ref-parse>;

define sealed domain make (singleton(<method-ref-parse>));

/*
define sealed method print-object
    (expr :: <method-ref-parse>, stream :: <stream>) => ();
  pprint-fields(expr, stream, method: expr.method-ref-method);
end method print-object;
*/


// <callback-method-ref-parse> -- exported.
//
define class <callback-method-ref-parse> (<method-ref-parse>)

end class <callback-method-ref-parse>;

define sealed domain make (singleton(<callback-method-ref-parse>));


// <primitive-parse> -- exported.
//
// Used to represent some magic internal operation.  Not actually generated
// by the parser.  Instead, the %%primitive macro magically expands into these.
// 
define class <primitive-parse> (<expression-parse>)
  //
  // The name of the primitive.
  constant slot primitive-name :: <identifier-token>,
    required-init-keyword: name:;
  //
  // The <expression-parse> arguments to that primitive.
  constant slot primitive-operands :: <simple-object-vector>,
    required-init-keyword: operands:;
end class <primitive-parse>;

define sealed domain make (singleton(<primitive-parse>));

/*
define sealed method print-object
    (primitive :: <primitive-parse>, stream :: <stream>) => ();
  pprint-fields(primitive, stream,
		name: primitive.primitive-name,
		operands: primitive.primitive-operands);
end method print-object;
*/


// <unwind-protect-parse> -- exported.
//
// Represents a block/cleanup.  Not actually generated by the parser.  Instead,
// the block macro expands into these where necessary.
// 
define class <unwind-protect-parse> (<expression-parse>)
  //
  // The protected body.
  constant slot uwp-body :: <expression-parse>,
    required-init-keyword: body:;
  //
  // The cleanup code.
  constant slot uwp-cleanup :: <expression-parse>,
    required-init-keyword: cleanup:;
end class <unwind-protect-parse>;

define sealed domain make (singleton(<unwind-protect-parse>));

/*
define sealed method print-object
    (uwp :: <unwind-protect-parse>, stream :: <stream>) => ();
  pprint-fields(uwp, stream, body: uwp.uwp-body, cleanup: uwp.uwp-cleanup);
end method print-object;
*/



//// Macros.

// <define-macro-parse> -- exported.
//
// The parse of a define macro.
//
define class <define-macro-parse> (<definition-parse>)
  //
  // The NAME token that is the name of this macro.
  constant slot defmacro-name :: <symbol-token>,
    required-init-keyword: name:;
  //
  // The main rule set.
  constant slot defmacro-main-rule-set :: <main-rule-set>,
    required-init-keyword: main-rule-set:;
  //
  // A vector of the auxiliary rule sets.
  constant slot defmacro-auxiliary-rule-sets :: <simple-object-vector>,
    required-init-keyword: auxiliary-rule-sets:;
end class <define-macro-parse>;

define sealed domain make (singleton(<define-macro-parse>));

// <rule-set> -- exported.
//
// A set of rewrite rules.
// 
define abstract class <rule-set> (<object>)
  //
  // Vector of rules.
  constant slot rule-set-rules :: <simple-object-vector>,
    required-init-keyword: rules:;
end class <rule-set>;

define sealed domain make (singleton(<rule-set>));
define sealed domain initialize (<rule-set>);

// <main-rule-set> -- exported.
//
// The main rule set for some macro.
//
define class <main-rule-set> (<rule-set>)
end class <main-rule-set>;

define sealed domain make (singleton(<main-rule-set>));

// <auxiliary-rule-set> -- exported.
//
// An auxiliary rule set for some macro.
// 
define class <auxiliary-rule-set> (<rule-set>)
  //
  // The keyword name for this ruleset.
  constant slot rule-set-name :: <symbol>, required-init-keyword: name:;
  //
  // #t if this some rule in this rule sets ends in a body-variable, #f if
  // not.  Settable because we want to compute it late in the game.
  slot rule-set-body-variable? :: <boolean>,
    init-keyword: body-variable?:, init-value: #f;
  //
  // #f until we've extracted any intermediate words at the start of this
  // rule set, #t after.
  slot rule-set-processed-intermediate-words? :: <boolean>, init-value: #f;
end class <auxiliary-rule-set>;

define sealed domain make (singleton(<auxiliary-rule-set>));

/*
define sealed method print-object
    (aux-rule-set :: <auxiliary-rule-set>, stream :: <stream>) => ();
  pprint-fields(aux-rule-set, stream,
		name: aux-rule-set.rule-set-name,
		rules: aux-rule-set.rule-set-rules,
		body-variable?: aux-rule-set.rule-set-body-variable?);
end method print-object;
*/

// <rule> -- exported.
//
// A single rewrite rule for some macro.
// 
define abstract class <rule> (<object>)
  //
  // The pattern this rule matches.  Settable because we want to be able to
  // change it for define rules once we've figured out how much of it is
  // really the modifier pattern.  And because we want to be able to remove
  // the ``;-opt END'' at the end of body-style define and statement rules.
  slot rule-pattern :: <pattern>, required-init-keyword: pattern:;
  //
  // The template that will be spliced in if this rule matches.
  constant slot rule-template :: <template>,
    required-init-keyword: template:;
end class <rule>;

define sealed domain make (singleton(<rule>));
define sealed domain initialize (<rule>);

/*
define sealed method print-object
    (rule :: <rule>, stream :: <stream>) => ();
  pprint-fields(rule, stream,
		pattern: rule.rule-pattern,
		template: rule.rule-template);
end method print-object;
*/

// <main-rule> -- exported.
//
// A main rule for some macro.
//
define abstract class <main-rule> (<rule>)
  //
  // The name of the macro this rule is part of.  Note: this is the name of
  // the macro, not whatever the macro is defining (for definition macros).
  // Can start out #f because for some rules we can't tell where it is in
  // the pattern just by looking at the rule.
  slot main-rule-name :: false-or(<symbol-token>) = #f,
    init-keyword: name:;
end class <main-rule>;

define sealed domain make (singleton(<main-rule>));

/*
define sealed method print-object
    (rule :: <main-rule>, stream :: <stream>) => ();
  pprint-fields(rule, stream,
		name: rule.main-rule-name,
		pattern: rule.rule-pattern,
		template: rule.rule-template);
end method print-object;
*/

// <define-rule> -- exported.
//
// A main rule for some definition macro.
// 
define abstract class <define-rule> (<main-rule>)
  //
  // The modifiers pattern.  Starts out #f and gets filled in with the real
  // pattern later.  We can't fill it in at the start, because we have to
  // do some tricky stuff to figure out where the modifiers-pattern ends and
  // the regular pattern starts.
  slot define-rule-modifiers-pattern :: false-or(<pattern>),
    init-keyword: modifiers-pattern:, init-value: #f;
end class <define-rule>;

define sealed domain make (singleton(<define-rule>));

/*
define sealed method print-object
    (rule :: <define-rule>, stream :: <stream>) => ();
  pprint-fields(rule, stream,
		modifiers-pattern: rule.define-rule-modifiers-pattern,
		name: rule.main-rule-name,
		pattern: rule.rule-pattern,
		template: rule.rule-template);
end method print-object;
*/

// <body-style-define-rule> -- exported.
//
// A define rule for a body style definition macro.
// 
define class <body-style-define-rule> (<define-rule>)
end class <body-style-define-rule>;

define sealed domain make (singleton(<body-style-define-rule>));

// <list-style-define-rule> -- exported.
//
// A define rule for a list style definition macro.
// 
define class <list-style-define-rule> (<define-rule>)
end class <list-style-define-rule>;

define sealed domain make (singleton(<list-style-define-rule>));

// <statement-rule> -- exported.
//
// A define rule for a statement macro.
// 
define class <statement-rule> (<main-rule>)
end class <statement-rule>;

define sealed domain make (singleton(<statement-rule>));

// <function-rule> -- exported.
//
// A define rule for a function macro.
// 
define class <function-rule> (<main-rule>)
end class <function-rule>;

define sealed domain make (singleton(<function-rule>));


// <auxiliary-rule> -- exported.
//
// A rule in an auxiliary-rule-set.
// 
define class <auxiliary-rule> (<rule>)
end class <auxiliary-rule>;

define sealed domain make (singleton(<auxiliary-rule>));


// <pattern> -- exported.
//
// Shared superclass of all the different kinds of patterns.
//
define abstract class <pattern> (<object>)
end class <pattern>;

define sealed domain make (singleton(<pattern>));
define sealed domain initialize (<pattern>);

// <empty-pattern>
//
// A pattern that matches the empty fragment.
// 
define class <empty-pattern> (<pattern>)
end class <empty-pattern>;

define sealed domain make (singleton(<empty-pattern>));

// <binary-pattern>
//
// Shared abstract superclass for the patterns that combine two sub-patterns.
// 
define abstract class <binary-pattern> (<pattern>)
  //
  // The sub-pattern on the left.
  constant slot pattern-left :: <pattern>,
    required-init-keyword: left:;
  //
  // The sub-pattern on the right.
  constant slot pattern-right :: <pattern>,
    required-init-keyword: right:;
  //
  // #t if this is the last of this kind of binary pattern in a chain
  // of 'em.
  constant slot pattern-last? :: <boolean>,
    required-init-keyword: last:;
end class <binary-pattern>;

define sealed domain make (singleton(<binary-pattern>));

/*
define sealed method print-object
    (pattern :: <binary-pattern>, stream :: <stream>) => ();
  pprint-fields(pattern, stream,
		left: pattern.pattern-left,
		right: pattern.pattern-right,
		pattern.pattern-last? & #"last", pattern.pattern-last?);
end method print-object;
*/

// <semicolon-pattern> -- exported.
//
// Used to represent two patterns seperated by a ;.
//
define class <semicolon-pattern> (<binary-pattern>)
end class <semicolon-pattern>;

define sealed domain make (singleton(<semicolon-pattern>));

// <comma-pattern> -- exported.
//
// Used to represent two patterns seperated by a ,.
//
define class <comma-pattern> (<binary-pattern>)
end class <comma-pattern>;

define sealed domain make (singleton(<comma-pattern>));

// <sequential-pattern> -- exported.
//
// Used to represent two sequential patterns next to each other.
//
define class <sequential-pattern> (<binary-pattern>)
end class <sequential-pattern>;

define sealed domain make (singleton(<sequential-pattern>));

// <simple-pattern> -- exported.
//
// Abstract superclass for simple patterns, i.e. things that are not built
// up of multiple patterns.
//
define abstract class <simple-pattern> (<pattern>)
end class <simple-pattern>;

define sealed domain make (singleton(<simple-pattern>));


define class <bracketed-pattern> (<simple-pattern>)
  //
  // The left bracket token.
  constant slot pattern-left-token :: <token>,
    required-init-keyword: left-token:;
  //
  // The pattern between the brackets.
  constant slot pattern-guts :: <pattern>,
    required-init-keyword: guts:;
  //
  // The right bracket token.
  constant slot pattern-right-token :: <token>,
    required-init-keyword: right-token:;
end class <bracketed-pattern>;

define sealed domain make (singleton(<bracketed-pattern>));


// <variable-pattern>
//
// Matches variables (i.e. either ``foo'' or ``foo :: type'').
// 
define class <variable-pattern> (<simple-pattern>)
  //
  // The pattern-variable that gets bound to the variable name.
  constant slot variable-name-pattern :: <pattern-variable>,
    required-init-keyword: name-pattern:;
  //
  // The pattern-variable that gets bound to the type expression or <object>
  // if there is no type expression.
  constant slot variable-type-pattern :: <pattern-variable>,
    required-init-keyword: type-pattern:;
end class <variable-pattern>;

define sealed domain make (singleton(<variable-pattern>));

/*
define sealed method print-object
    (pattern :: <variable-pattern>, stream :: <stream>) => ();
  pprint-fields(pattern, stream,
		name-pattern: pattern.variable-name-pattern,
		type-pattern: pattern.variable-type-pattern);
end method print-object;
*/

// <bindings-pattern>
//
// Matches variable = expression or (variable-list) = expression
// 
define class <bindings-pattern> (<simple-pattern>)
  //
  constant slot bindings-variables-pattern
    :: type-union(<pattern-variable>, <variable-pattern>),
    required-init-keyword: variables-pattern:;
  //
  // The pattern-variable that gets bound to the initial-value expression.
  constant slot bindings-value-pattern :: <pattern-variable>,
    required-init-keyword: value-pattern:;
end class <bindings-pattern>;

define sealed domain make (singleton(<bindings-pattern>));

/*
define sealed method print-object
    (pattern :: <bindings-pattern>, stream :: <stream>) => ();
  pprint-fields(pattern, stream,
		variable-pattern: pattern.bindings-variables-pattern,
		value-pattern: pattern.bindings-value-pattern);
end method print-object;
*/


// <name-pattern> -- exported.
//
// Pattern that matches a literal name.
// 
define class <name-pattern> (<simple-pattern>)
  constant slot pattern-name :: <symbol-token>,
    required-init-keyword: name:;
end class <name-pattern>;

define sealed domain make (singleton(<name-pattern>));

/*
define sealed method print-object
    (pattern :: <name-pattern>, stream :: <stream>) => ();
  pprint-fields(pattern, stream, name: pattern.pattern-name);
end method print-object;
*/

// <arrow-pattern> -- exported.
//
// Pattern that matches a literal arrow.
// 
define class <arrow-pattern> (<simple-pattern>)
end class <arrow-pattern>;

define sealed domain make (singleton(<arrow-pattern>));


// <pattern-variable> -- exported.
//
// Pattern that matches some variable fragment and establishes a binding
// for it.
//
define class <pattern-variable> (<simple-pattern>, <source-location-mixin>)
  //
  // The name of this pattern variable.
  constant slot patvar-name :: false-or(<symbol>),
    init-value: #f, init-keyword: name:;
  //
  // The constraint, if any.  Settable because we want to be able to default
  // the constraint on pattern variables with the same name as a rule set
  // to be #"*" (wildcard) long after the actual pattern-variable objects
  // have been created.
  slot patvar-constraint :: false-or(<symbol>),
    init-value: #f, init-keyword: constraint:;
  //
  // True if this pattern variable is at the end of the pattern.  Settable
  // because we want to figure it out late in the game.
  slot patvar-at-end? :: <boolean>,
    init-value: #f, init-keyword: at-end:;
end class <pattern-variable>;

define sealed domain make (singleton(<pattern-variable>));

/*
define sealed method print-object
    (pattern :: <pattern-variable>, stream :: <stream>) => ();
  pprint-fields(pattern, stream,
		name: pattern.patvar-name,
		if (pattern.patvar-constraint) constraint: end,
		pattern.patvar-constraint,
		if (pattern.patvar-at-end?) at-end?: end,
		pattern.patvar-at-end?);
end method print-object;
*/

// <property-list-pattern> -- exported.
//
// A pattern that matches a property-list like fragment.
// 
define class <property-list-pattern> (<pattern>)
  //
  // The pattern variable to bind everything to if there is one.  Settable
  // because that makes the parser a tad bit simpler.
  slot plistpat-rest :: false-or(<pattern-variable>),
    init-value: #f, init-keyword: rest:;
  //
  // Simple-object-vector of <pattern-keyword>s.
  constant slot plistpat-keys :: false-or(<simple-object-vector>),
    init-value: #f, init-keyword: keys:;
  //
  // Indicates that #all-keys was present in the pattern.
  constant slot plistpat-all-keys? :: <boolean>,
    init-value: #f, init-keyword: all-keys:;
end class <property-list-pattern>;

define sealed domain make (singleton(<property-list-pattern>));

// <pattern-keyword> -- exported.
//
// A pattern variable used for keyword matching of property lists.
// 
define class <pattern-keyword> (<pattern-variable>)
  //
  // Pattern keywords get matched against the entire property value.  So they
  // are all effectivly at-end.
  inherited slot patvar-at-end?, init-value: #t;
  //
  // The default, or #f if none.
  constant slot patkey-default :: false-or(<template>),
    required-init-keyword: default:;
  //
  // #t if the word should be bound to a sequence of all the occurences of the
  // keyword instead of just the first.
  constant slot patkey-all? :: <boolean>,
    init-value: #f, init-keyword: all:;
end class <pattern-keyword>;

define sealed domain make (singleton(<pattern-keyword>));


// <template> -- exported.
//
// Abstract superclass of the different kinds of templates.
// 
define abstract class <template> (<object>)
end class <template>;

define sealed domain make (singleton(<template>));
define sealed domain initialize (<template>);

// <procedural-template> -- exported.
//
define class <procedural-template> (<template>)
  //
  // The procedure to ``call'' to produce the replacement.
  constant slot template-name :: <identifier-token>,
    required-init-keyword: name:;
  //
  // Vector of templates to pass it as arguments.
  constant slot template-arguments :: <simple-object-vector>,
    required-init-keyword: arguments:;
end class <procedural-template>;

define sealed domain make (singleton(<procedural-template>));

/*
define sealed method print-object
    (template :: <procedural-template>, stream :: <stream>) => ();
  pprint-fields(template, stream, name: template.template-name,
		arguments: template.template-arguments);
end method print-object;
*/

// <literal-template> -- exported.
//
define class <literal-template> (<template>)
  //
  // Vector of either <token>s, <bracketed-element>, or <pattern-variable-
  // reference>s.
  slot template-elements :: <simple-object-vector>,
    required-init-keyword: elements:;
end class <literal-template>;

define sealed domain make (singleton(<literal-template>));

/*
define sealed method print-object
    (template :: <literal-template>, stream :: <stream>) => ();
  pprint-fields(template, stream, elements: template.template-elements);
end method print-object;
*/

// <bracketed-element> -- exported.
//
// A template-element that corresponds to a balanced pair of some kind of
// brackets and some more elements between them.
//
define class <bracketed-element> (<object>)
  //
  // The token that goes on the left.
  constant slot bracketed-element-left-token :: <token>,
    required-init-keyword: left-token:;
  //
  // The literal-template that goes in the middle.
  constant slot bracketed-element-guts :: <literal-template>,
    required-init-keyword: guts:;
  //
  // The token that goes on the right.
  constant slot bracketed-element-right-token :: <token>,
    required-init-keyword: right-token:;
end class <bracketed-element>;

define sealed domain make (singleton(<bracketed-element>));
define sealed domain initialize (<bracketed-element>);

/*
define sealed method print-object
    (element :: <bracketed-element>, stream :: <stream>) => ();
  pprint-fields(element, stream,
		left: element.bracketed-element-left-token,
		guts: element.bracketed-element-guts,
		right: element.bracketed-element-right-token);
end method print-object;
*/

// <pattern-variable-reference> -- exported.
// 
define abstract class <pattern-variable-reference> (<object>)
  //
  // The variable being referenced, as a NAME, STRING, SYMBOL, or ELLIPSIS.
  // Left a token so that we have source context.
  constant slot patvarref-name :: <token>,
    required-init-keyword: name:;
end class <pattern-variable-reference>;

define sealed domain make (singleton(<pattern-variable-reference>));
define sealed domain initialize (<pattern-variable-reference>);

define class <simple-pattern-variable-reference> (<pattern-variable-reference>)
end class <simple-pattern-variable-reference>;

define sealed domain make (singleton(<simple-pattern-variable-reference>));

define class <ellipsis-pattern-variable-reference>
    (<pattern-variable-reference>)
end class <ellipsis-pattern-variable-reference>;

define sealed domain make (singleton(<ellipsis-pattern-variable-reference>));

define class <concatenating-pattern-variable-reference>
    (<pattern-variable-reference>)
  //
  // The prefix, or #f if none.
  constant slot patvarref-prefix :: false-or(<string>), 
    required-init-keyword: prefix:;
  //
  // The suffix, or #f if none.
  constant slot patvarref-suffix :: false-or(<string>), 
    required-init-keyword: suffix:;
end class <concatenating-pattern-variable-reference>;

define sealed domain make
  (singleton(<concatenating-pattern-variable-reference>));

/*
define sealed method print-object
    (pvarref :: <concatenating-pattern-variable-reference>, stream :: <stream>)
  => ();
  pprint-fields(pvarref, stream,
		if (pvarref.patvarref-prefix) prefix: end,
		  pvarref.patvarref-prefix,
		name: pvarref.patvarref-name,
		if (pvarref.patvarref-suffix) separator: end,
		  pvarref.patvarref-suffix);
end method print-object;
*/

define class <sequence-pattern-variable-reference>
    (<pattern-variable-reference>)
  //
  // The separator to stick between elements if the pattern variable holds
  // a collection.
  constant slot patvarref-separator :: false-or(<token>),
    init-value: #f, init-keyword: separator:;
end class <sequence-pattern-variable-reference>;

define sealed domain make (singleton(<sequence-pattern-variable-reference>));

define class <unhygienic-pattern-variable-reference>
    (<pattern-variable-reference>)
end class <unhygienic-pattern-variable-reference>;

define sealed domain make (singleton(<unhygienic-pattern-variable-reference>));



// <property> -- exported.
//
// Property lists are represented by vectors of properties.
//
define class <property> (<object>)
  //
  // The comma token that preceeds this property, of #f if it is the first
  // one.
  constant slot prop-comma :: false-or(<token>) = #f,
    init-keyword: comma:;
  //
  // The source location for the comma.  We don't use the token's source
  // location because we want to track it though macro expansions.
  constant slot prop-comma-srcloc :: <source-location>
      = make(<unknown-source-location>),
    init-keyword: comma-srcloc:;
  //
  // The keyword.
  constant slot prop-keyword :: <literal-token>,
    required-init-keyword: keyword:;
  //
  // The source location for the keyword.  We don't use the token's source
  // location because we want to track it though macro expansions.
  constant slot prop-keyword-srcloc :: <source-location>
      = make(<unknown-source-location>),
    init-keyword: keyword-srcloc:;
  //
  // And the associated property.
  constant slot prop-value :: <fragment>,
    required-init-keyword: value:;
end;

define sealed domain make (singleton(<property>));
define sealed domain initialize (<property>);

/*
define sealed method print-object
    (prop :: <property>, stream :: <stream>) => ();
  pprint-fields(prop, stream,
		keyword: prop.prop-keyword,
		value: prop.prop-value);
end;
*/

