module: parse-tree
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/parser/parse-tree.dylan,v 1.14 1996/02/19 20:29:51 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


//// Annotation classes.

// <property> -- exported.
//
// Property lists are represented by vectors of properties.
//
define class <property> (<object>)
  //
  // The keyword.
  slot prop-keyword :: <keyword-token>, setter: #f,
    required-init-keyword: keyword:;
  //
  // And the associated property, either an expression or a <property-set>.
  slot prop-value :: type-union(<expression>, <property-set>), setter: #f,
    required-init-keyword: value:;
end;

define method print-object (prop :: <property>, stream :: <stream>) => ();
  pprint-fields(prop, stream,
		keyword: prop.prop-keyword,
		value: prop.prop-value);
end;

// <bindings> -- exported.
//
// Used whenever we need to represent some kind of parameter bindings.  For
// example, let and define constant.
// 
define class <bindings> (<token>)
  //
  // The parameters being bound.
  slot bindings-parameter-list :: <parameter-list>, setter: #f,
    required-init-keyword: parameter-list:;
  //
  // The expression they are being bound to.
  slot bindings-expression :: <expression>,
    required-init-keyword: expression:;
end;

define method print-object (bindings :: <bindings>, stream :: <stream>) => ();
  pprint-fields(bindings, stream,
		param-list: bindings.bindings-parameter-list,
		expression: bindings.bindings-expression);
end;

// <parameter-list> -- exported.
//
// Used to represent both simple parameter lists (e.g. lets) and complex
// parameter lists (e.g. method param lists).
//
define class <parameter-list> (<object>)
  //
  // Vector of <parameter>s.
  slot paramlist-required-vars :: <simple-object-vector>,
    init-value: #[], init-keyword: required:;
  //
  // Either #f or the name of the #rest parameter.
  slot paramlist-rest :: false-or(<parameter>),
    init-value: #f, init-keyword: rest:;
  //
  // Either #f or the name of the #next parameter.
  slot paramlist-next :: false-or(<name-token>),
    init-value: #f, init-keyword: next:;
  //
  // Either #f or a (possibly empty) vector of <keyword-parameter>s.
  slot paramlist-keys :: false-or(<simple-object-vector>), setter: #f,
    init-value: #f, init-keyword: keys:;
  //
  // #t if #all-keys was supplied, #f otherwise.
  slot paramlist-all-keys? :: <boolean>, setter: #f,
    init-value: #f, init-keyword: all-keys:;
end;

define method print-object (paramlist :: <parameter-list>, stream :: <stream>)
    => ();
  pprint-fields(paramlist, stream,
		required-vars: paramlist.paramlist-required-vars,
		if (paramlist.paramlist-rest) #"#rest" end,
		paramlist.paramlist-rest,
		if (paramlist.paramlist-next) #"#next" end,
		paramlist.paramlist-next,
		if (paramlist.paramlist-keys) #"#key" end,
		paramlist.paramlist-keys,
		if (paramlist.paramlist-all-keys?) #"#all-keys" end,
		paramlist.paramlist-all-keys?);
end;

// <parameter> -- exported.
//
// Used whenever we need to represent a (potentially) typed variable.
//
define class <parameter> (<object>)
  //
  // The name.
  slot param-name :: <name-token>, setter: #f,
    required-init-keyword: name:;
  //
  // The type expression if there is one, or #f if not.
  slot param-type :: false-or(<expression>), setter: #f,
    init-value: #f, init-keyword: type:;
end;

define method print-object (param :: <parameter>, stream :: <stream>) => ();
  pprint-fields(param, stream,
		name: param.param-name,
		if (param.param-type) type: end, param.param-type);
end;

// <keyword-parameter> -- exported.
//
define class <keyword-parameter> (<parameter>)
  slot param-keyword :: <symbol>, setter: #f,
    required-init-keyword: keyword:;
  slot param-default :: false-or(<expression>), setter: #f,
    required-init-keyword: default:;
end;

define method pprint-object (param :: <keyword-parameter>, stream :: <stream>)
    => ();
  pprint-fields(param, stream,
		keyword: param.param-keyword,
		name: param.param-name,
		if (param.param-type) type: end, param.param-type,
		if (param.param-default) default: end, param.param-default);
end;

// <method-parse> -- exported.
//
define class <method-parse> (<source-location-mixin>)
  //
  // The name of this method, if there is one.
  slot method-name :: false-or(<name-token>),
    init-value: #f, init-keyword: name:;
  //
  // The parameter list.
  slot method-param-list :: <parameter-list>, setter: #f,
    required-init-keyword: parameter-list:;
  //
  // Parameter list describing the return values.
  slot method-returns :: <parameter-list>, setter: #f,
    init-function: method ()
		     make(<parameter-list>,
			  required: #[],
			  rest: make(<parameter>,
				     name: make(<name-token>,
						symbol: #"results")));
		   end,
    init-keyword: returns:;
  //
  // The body, a vector of constituents.
  slot method-body :: <simple-object-vector>, setter: #f,
    required-init-keyword: body:;
end;

define method print-object (meth :: <method-parse>, stream :: <stream>) => ();
  pprint-fields(meth, stream,
		if (meth.method-name) name: end, meth.method-name,
		param-list: meth.method-param-list,
		returns: meth.method-returns,
		body: meth.method-body);
end;

// <case-clause> -- exported.
//
define class <case-clause> (<object>)
  //
  // The ``label'' for this case clause.  Either a vector of expressions, or
  // #t for otherwise labels.
  slot case-label :: type-union(<simple-object-vector>, <true>), setter: #f,
    required-init-keyword: label:;
  
  // Starts out a stretchy-vector, and is converted into a simple-object
  // vector once it has been filled in.
  slot case-body :: <vector>,
    init-function: method () make(<stretchy-vector>, size: 0) end,
    init-keyword: body:;
end;

define method print-object (clause :: <case-clause>, stream :: <stream>) => ();
  pprint-fields(clause, stream,
		label: clause.case-label,
		body: as(<simple-object-vector>, clause.case-body));
end;

// <property-set> -- exported.
// 
define class <property-set> (<token>)
  slot property-set-members :: <simple-object-vector>, setter: #f,
    required-init-keyword: members:;
end;

define method print-object (pset :: <property-set>, stream :: <stream>) => ();
  pprint-fields(pset, stream, members: pset.property-set-members);
end;

// <use-clause> -- exported.
//
define class <use-clause> (<object>)
  slot use-name :: <name-token>, setter: #f,
    required-init-keyword: name:;
  slot use-import :: type-union(<expression>, <property-set>), setter: #f,
    required-init-keyword: import:;
  slot use-exclude :: type-union(<expression>, <property-set>), setter: #f,
    required-init-keyword: exclude:;
  slot use-prefix :: type-union(<expression>, <property-set>), setter: #f,
    required-init-keyword: prefix:;
  slot use-rename :: type-union(<expression>, <property-set>), setter: #f,
    required-init-keyword: rename:;
  slot use-export :: type-union(<expression>, <property-set>), setter: #f,
    required-init-keyword: export:;
end;

define method print-object (clause :: <use-clause>, stream :: <stream>) => ();
  pprint-fields(clause, stream,
		name: clause.use-name,
		import: clause.use-import,
		exclude: clause.use-exclude,
		prefix: clause.use-prefix,
		rename: clause.use-rename,
		export: clause.use-export);
end;

// <export-clause> -- exported.
//
define class <export-clause> (<object>)
  slot export-names :: <simple-object-vector>, setter: #f,
    required-init-keyword: names:;
end;

define method print-object (clause :: <export-clause>, stream :: <stream>)
    => ();
  pprint-fields(clause, stream, names: clause.export-names);
end;

// <create-clause> -- exported.
//
define class <create-clause> (<object>)
  slot create-names :: <simple-object-vector>, setter: #f,
    required-init-keyword: names:;
end;

define method print-object (clause :: <create-clause>, stream :: <stream>)
    => ();
  pprint-fields(clause, stream, names: clause.create-names);
end;


define abstract class <for-clause> (<object>)
end;

define class <for-while-clause> (<for-clause>)
  slot for-clause-condition :: <expression>, setter: #f,
    required-init-keyword: condition:;
end;

define abstract class <for-var-clause> (<for-clause>)
  slot for-clause-variable :: <parameter>, setter: #f,
    required-init-keyword: variable:;
end;

define class <for-in-clause> (<for-var-clause>)
  slot for-clause-collection :: <expression>, setter: #f,
    required-init-keyword: collection:;
  slot for-clause-keyed-by :: false-or(<parameter>), setter: #f,
    required-init-keyword: keyed-by:;
  slot for-clause-using :: <expression>, setter: #f,
    required-init-keyword: using:;
end;

define class <for-step-clause> (<for-var-clause>)
  slot for-clause-init :: <expression>, setter: #f,
    required-init-keyword: init:;
  slot for-clause-step :: <expression>, setter: #f,
    required-init-keyword: step:;
end;

define class <for-from-clause> (<for-var-clause>)
  slot for-clause-from :: <expression>, setter: #f,
    required-init-keyword: from:;
  slot for-clause-by :: <expression>, setter: #f,
    required-init-keyword: by:;
  slot for-clause-kind :: one-of(#"above", #"below", #"to", #f), setter: #f,
    init-value: #f, init-keyword: kind:;
  slot for-clause-bound :: false-or(<expression>), setter: #f,
    init-value: #f, init-keyword: bound:;
end;

define class <classopt> (<object>)
  slot classopt-kind :: one-of(#"slot", #"inherited", #"keyword"), setter: #f,
    required-init-keyword: kind:;
  slot classopt-name :: type-union(<name-token>, <keyword-token>), setter: #f,
    required-init-keyword: name:;
  slot classopt-plist :: <simple-object-vector>, setter: #f,
    required-init-keyword: plist:;
end;


//// Parse-tree classes.

// <constituent> -- exported.
//
// The various different kinds of constituent inherit from this.
//
define abstract class <constituent> (<token>)
end;

// <defining-form> -- exported.
//
// Shared by the various defining forms.
// 
define abstract class <defining-form> (<constituent>)
end;

// <modified-defining-form> -- internal.
//
define class <modified-defining-form> (<defining-form>)
  //
  // Vector of <name-token> modifiers.
  slot define-modifiers :: <simple-object-vector>,
    init-keyword: modifiers:;
end;

// <define-class-parse> -- exported.
//
define class <define-class-parse> (<modified-defining-form>)
  //
  // The name being defined.
  slot defclass-name :: <name-token>, setter: #f,
    required-init-keyword: name:;
  //
  // Vector of superclass expressions.
  slot defclass-supers :: <simple-object-vector>, setter: #f,
    required-init-keyword: supers:;
  //
  // Vector of slots.
  slot defclass-options :: <simple-object-vector>,
    required-init-keyword: options:;
end;

define method print-object (defclass :: <define-class-parse>,
			    stream :: <stream>)
    => ();
  pprint-fields(defclass, stream,
		name: defclass.defclass-name,
		supers: defclass.defclass-supers,
		options: defclass.defclass-options);
end;

// <define-constant-parse> -- exported.
//
define class <define-constant-parse> (<defining-form>)
  //
  // The bindings.
  slot defconst-bindings :: <bindings>,
    required-init-keyword: bindings:;
end;

define method print-object (defconst :: <define-constant-parse>,
			    stream :: <stream>)
    => ();
  pprint-fields(defconst, stream, bindings: defconst.defconst-bindings);
end;

// <define-generic-parse> -- exported.
// 
define class <define-generic-parse> (<modified-defining-form>)
  //
  // The name of the generic function being defined.
  slot defgen-name :: <name-token>;
  //
  // The parameter list for said generic funciton.
  slot defgen-param-list :: <parameter-list>,
    required-init-keyword: parameter-list:;
  //
  // The return types.
  slot defgen-returns :: <parameter-list>, setter: #f,
    init-function: method ()
		     make(<parameter-list>,
			  required: #[],
			  rest: make(<parameter>,
				     name: make(<name-token>,
						symbol: #"results")));
		   end,
    init-keyword: returns:;
  //
  // And the property list.
  slot defgen-plist :: <simple-object-vector>,
    init-value: #[], init-keyword: plist:;
end;

define class <seal-generic-parse> (<defining-form>)
  //
  // The name of the generic function being defined.
  slot sealgen-name :: <name-token>, required-init-keyword: name:;
  //
  // The type expressions.
  slot sealgen-type-exprs :: <simple-object-vector>,
    required-init-keyword: type-exprs:;
end;

// <define-library-parse> -- exported.
//
define class <define-library-parse> (<defining-form>)
  //
  // The name.
  slot deflibrary-name :: <name-token>, required-init-keyword: name:;
  //
  // The clauses.
  slot deflibrary-clauses :: <simple-object-vector>,
    required-init-keyword: clauses:;
end;

define method print-object (deflib :: <define-library-parse>,
			    stream :: <stream>)
    => ();
  pprint-fields(deflib, stream,
		name: deflib.deflibrary-name,
		clauses: deflib.deflibrary-clauses);
end;

// <define-method-parse> -- exported.
// 
define class <define-method-parse> (<modified-defining-form>)
  slot defmethod-method :: <method-parse>, required-init-keyword: method:;
end;

define method print-object (defmethod :: <define-method-parse>,
			    stream :: <stream>)
    => ();
  pprint-fields(defmethod, stream, method: defmethod.defmethod-method);
end;

// <define-module-parse> -- exported.
//
define class <define-module-parse> (<defining-form>)
  //
  // The name.
  slot defmodule-name :: <name-token>, required-init-keyword: name:;
  //
  // The clauses.
  slot defmodule-clauses :: <simple-object-vector>,
    required-init-keyword: clauses:;
end;

define method print-object (defmod :: <define-module-parse>,
			    stream :: <stream>)
    => ();
  pprint-fields(defmod, stream,
		name: defmod.defmodule-name,
		clauses: defmod.defmodule-clauses);
end;

// <define-variable-parse> -- exported.
//
define class <define-variable-parse> (<defining-form>)
  //
  // The bindings.
  slot defvar-bindings :: <bindings>,
    required-init-keyword: bindings:;
end;

define method print-object (defvar :: <define-variable-parse>,
			    stream :: <stream>)
    => ();
  pprint-fields(defvar, stream, bindings: defvar.defvar-bindings);
end;

// <define-parse> -- exported.
//
// For things like define class, define module, and define library.
//
define class <define-parse> (<modified-defining-form>)
  //
  // The define-word used.
  slot define-word :: <define-word-token>, required-init-keyword: word:;
  //
  // The name being defined.  This is not to be confused with the word
  // which is the kind of thing being defined (or alternatively, the name
  // of the macro defining this name).
  slot define-name :: <name-token>, required-init-keyword: name:;
  //
  // The fragment.
  slot define-fragment :: <fragment>,
    required-init-keyword: fragment:;
end;

define method print-object (def :: <define-parse>, stream :: <stream>) => ();
  pprint-fields(def, stream,
		modifiers: def.define-modifiers,
		word: def.define-word,
		name: def.define-name,
		fragment: def.define-fragment);
end;

// <define-bindings-parse> -- exported.
//
// A use of a bindings like define-word macro, like define constant and
// define variable.
//
define class <define-bindings-parse> (<modified-defining-form>)
  //
  // The define-bindings-word used.
  slot define-word :: <define-bindings-word-token>,
    required-init-keyword: word:;
  //
  // The <bindings> object, which holds the names being defined and the
  // expression used to compute 'em.
  slot define-bindings :: <bindings>, required-init-keyword: bindings:;
end;

define method print-object (def :: <define-bindings-parse>, stream :: <stream>)
    => ();
  pprint-fields(def, stream,
		modifiers: def.define-modifiers,
		word: def.define-word,
		bindings: def.define-bindings);
end;

// <define-macro-parse> -- exported.
//
// The parse of a define macro.
//
define abstract class <define-macro-parse> (<defining-form>)
  //
  // The name of this macro.
  slot defmacro-name :: <word-token>, required-init-keyword: name:;
  //
  // A vector of the main rules.
  slot defmacro-main-rule-set :: <simple-object-vector>,
    required-init-keyword: main-rule-set:;
  //
  // A vector of the auxiliary rule sets.
  slot defmacro-auxiliary-rule-sets :: <simple-object-vector>,
    required-init-keyword: auxiliary-rule-sets:;
end;

define class <define-define-macro-parse> (<define-macro-parse>)
end;

define class <define-define-bindings-macro-parse> (<define-macro-parse>)
end;

define class <define-statement-macro-parse> (<define-macro-parse>)
end;

define class <define-function-macro-parse> (<define-macro-parse>)
end;

// <local-declaration> -- exported.
//
// Abstract superclass for all the various local declarations.
//
define abstract class <local-declaration> (<constituent>)
end;

// <let> -- exported.
//
// A let is just a bunch of local bindings.  We don't explicitly represent the
// scoping of lets.  It is up to ICR to notice that the stuff after a let is
// inside the scope of the bindings established by the let.
// 
define class <let> (<local-declaration>)
  //
  slot let-bindings :: <bindings>, required-init-keyword: bindings:;
end;

define method print-object (form :: <let>, stream :: <stream>) => ();
  pprint-fields(form, stream, bindings: form.let-bindings);
end;

// <let-handler> -- exported.
//
define class <let-handler> (<local-declaration>)
  //
  // The condition type expression.
  slot handler-type :: <expression>, required-init-keyword: type:;
  //
  // Other random stuff.
  slot handler-plist :: <simple-object-vector>, required-init-keyword: plist:;
  //
  // The expression to compute the handler.
  slot handler-expression :: <expression>, required-init-keyword: handler:;
end;

define method print-object (form :: <let-handler>, stream :: <stream>) => ();
  pprint-fields(form, stream,
		type: form.handler-type,
		plist: form.handler-plist,
		expression: form.handler-expression);
end;

// <local> -- exported.
//
define class <local> (<local-declaration>)
  //
  // Vector of <method-parse>s.
  slot local-methods :: <simple-object-vector>,
    required-init-keyword: methods:;
end;

define method print-object (loc :: <local>, stream :: <stream>) => ();
  pprint-fields(loc, stream, methods: loc.local-methods);
end;

// <expression> -- exported.
//
// Abstract superclass for all the different kinds of expressions.
//
define abstract class <expression> (<constituent>)
end;

// <literal-ref> -- exported.
//
define class <literal-ref> (<expression>)
  slot litref-literal, required-init-keyword: literal:;
end;

define method print-object (lit :: <literal-ref>, stream :: <stream>) => ();
  pprint-fields(lit, stream, value: lit.litref-literal);
end;

// <binop-series> -- exported.
//
define class <binop-series> (<expression>)
  slot binop-series-operands :: <simple-object-vector>,
    required-init-keyword: operands:;
  slot binop-series-operators :: <simple-object-vector>,
    required-init-keyword: operators:;
end;

define method print-object (series :: <binop-series>, stream :: <stream>)
    => ();
  pprint-fields(series, stream,
		operands: series.binop-series-operands,
		operators: series.binop-series-operators);
end;

// <funcall> -- exported.
//
define class <funcall> (<expression>)
  slot funcall-function :: <expression>,
    required-init-keyword: function:;
  slot funcall-arguments :: <simple-object-vector>,
    required-init-keyword: arguments:;
end;

define method print-object (funcall :: <funcall>, stream :: <stream>) => ();
  let args = funcall.funcall-arguments;
  let vec = make(<vector>, size: args.size * 2);
  for (arg in args, i from 0)
    vec[i * 2] := format-to-string("argument %d", i);
    vec[i * 2 + 1] := arg;
  end;
  apply(pprint-fields, funcall, stream,
	function: funcall.funcall-function,
	vec);
end;

// <dot> -- exported.
//
define class <dot> (<expression>)
  slot dot-operand :: <expression>, required-init-keyword: operand:;
  slot dot-name :: <name-token>, required-init-keyword: name:;
end;

define method print-object (dot :: <dot>, stream :: <stream>) => ();
  pprint-fields(dot, stream, operand: dot.dot-operand, name: dot.dot-name);
end;

// <varref> -- exported.
//
define class <varref> (<expression>)
  slot varref-id :: <identifier-token>, required-init-keyword: id:;
end;

define method print-object (varref :: <varref>, stream :: <stream>) => ();
  pprint-fields(varref, stream, name: varref.varref-id);
end;

// <macro-statement> -- exported.
//
// A generic statement -- i.e. before macro expansion.
// 
define class <macro-statement> (<expression>)
  slot statement-begin-word :: <begin-word-token>,
    required-init-keyword: begin-word:;
  slot statement-fragment :: <fragment>,
    required-init-keyword: fragment:;
end;

define method print-object (statement :: <macro-statement>, stream :: <stream>)
    => ();
  pprint-fields(statement, stream,
		begin-word: statement.statement-begin-word,
		fragment: statement.statement-fragment);
end;

// <assignment> -- exported.
// 
define class <assignment> (<expression>)
  slot assignment-place :: <expression>,
    required-init-keyword: place:;
  slot assignment-value :: <expression>,
    required-init-keyword: value:;
end;

define method print-object (assignment :: <assignment>, stream :: <stream>)
    => ();
  pprint-fields(assignment, stream,
		place: assignment.assignment-place,
		value: assignment.assignment-value);
end;

// <begin> -- exported.
//
define class <begin> (<expression>)
  slot begin-body :: <simple-object-vector>,
    required-init-keyword: body:;
end;

define method print-object (statement :: <begin>, stream :: <stream>)
    => ();
  pprint-fields(statement, stream, body: statement.begin-body);
end;

// <bind-exit> -- exported.
//
define class <bind-exit> (<expression>)
  slot exit-name :: <name-token>, required-init-keyword: name:;
  slot exit-body :: <simple-object-vector>, required-init-keyword: body:;
end;

define method print-object (form :: <bind-exit>, stream :: <stream>) => ();
  pprint-fields(form, stream, name: form.exit-name, body: form.exit-body);
end;

// <for> -- exported.
//
define class <for> (<expression>)
  slot for-header :: <simple-object-vector>, required-init-keyword: header:;
  slot for-body :: <simple-object-vector>, required-init-keyword: body:;
  slot for-finally :: <simple-object-vector>, required-init-keyword: finally:;
end;

// <if> -- exported.
// 
define class <if> (<expression>)
  slot if-condition :: <expression>,
    required-init-keyword: condition:;
  slot if-consequent :: <simple-object-vector>,
    required-init-keyword: consequent:;
  slot if-alternate :: <simple-object-vector>,
    required-init-keyword: alternate:;
end;

define method print-object (statement :: <if>, stream :: <stream>)
    => ();
  pprint-fields(statement, stream,
		condition: statement.if-condition,
		consequent: statement.if-consequent,
		alternate: statement.if-alternate);
end;

// <method-ref> -- exported.
//
define class <method-ref> (<expression>)
  slot method-ref-method :: <method-parse>, required-init-keyword: method:;
end;

define method print-object (ref :: <method-ref>, stream :: <stream>) => ();
  pprint-fields(ref, stream, method: ref.method-ref-method);
end;

// <mv-call> -- exported.
// 
define class <mv-call> (<expression>)
  slot mv-call-operands :: <simple-object-vector>,
    required-init-keyword: operands:;
end;

define method print-object (mv-call :: <mv-call>, stream :: <stream>) => ();
  pprint-fields(mv-call, stream, operands: mv-call.mv-call-operands);
end;

// <primitive> -- exported.
// 
define class <primitive> (<expression>)
  slot primitive-name :: <identifier-token>,
    required-init-keyword: name:;
  slot primitive-operands :: <simple-object-vector>,
    required-init-keyword: operands:;
end;

define method print-object (primitive :: <primitive>, stream :: <stream>)
    => ();
  pprint-fields(primitive, stream,
		name: primitive.primitive-name,
		operands: primitive.primitive-operands);
end;

// <uwp> -- exported.
//
define class <uwp> (<expression>)
  slot uwp-body :: <simple-object-vector>, required-init-keyword: body:;
  slot uwp-cleanup :: <simple-object-vector>, required-init-keyword: cleanup:;
end;

define method print-object (uwp :: <uwp>, stream :: <stream>) => ();
  pprint-fields(uwp, stream, body: uwp.uwp-body, cleanup: uwp.uwp-cleanup);
end;


//// Macros.

define abstract class <rule> (<object>)
  //
  // The pattern this rule matches.
  slot rule-pattern :: <pattern>, required-init-keyword: pattern:;
  //
  // The template that will be spliced in if this rule matches.
  slot rule-template :: <template>, required-init-keyword: template:;
end;  

define method print-object (rule :: <rule>, stream :: <stream>) => ();
  pprint-fields(rule, stream,
		pattern: rule.rule-pattern,
		template: rule.rule-template);
end;

define abstract class <abstract-define-rule> (<rule>)
  slot define-rule-modifiers-pattern :: false-or(<pattern-sequence>),
    init-value: #f;
end;

define method print-object (rule :: <abstract-define-rule>, stream :: <stream>)
    => ();
  pprint-fields(rule, stream,
		modifiers-pattern: rule.define-rule-modifiers-pattern,
		pattern: rule.rule-pattern,
		template: rule.rule-template);
end;  

define class <define-rule> (<abstract-define-rule>)
end;

define class <define-bindings-rule> (<abstract-define-rule>)
end;

define class <statement-rule> (<rule>)
end;

define class <function-rule> (<rule>)
end;


define class <pattern> (<object>)
  //
  // Vector of ; seperated <pattern-list>s.
  slot pattern-pieces :: <simple-object-vector>,
    required-init-keyword: pieces:;
end;

define method print-object (pattern :: <pattern>, stream :: <stream>) => ();
  pprint-fields(pattern, stream, pieces: pattern.pattern-pieces);
end;

define class <pattern-list> (<object>)
  //
  // Vector of , seperated <pattern-sequence>s.  Except that the last one might
  // be a <property-list-pattern>.
  slot pattern-list-pieces :: <simple-object-vector>,
    required-init-keyword: pieces:;
end;

define method print-object (pattern :: <pattern-list>, stream :: <stream>)
    => ();
  pprint-fields(pattern, stream, pieces: pattern.pattern-list-pieces);
end;

define class <pattern-sequence> (<object>)
  //
  // Vector of simple-patterns.
  slot pattern-sequence-pieces :: <simple-object-vector>,
    required-init-keyword: pieces:;
end;

define method print-object (pattern :: <pattern-sequence>, stream :: <stream>)
    => ();
  pprint-fields(pattern, stream, pieces: pattern.pattern-sequence-pieces);
end;

define abstract class <simple-pattern> (<object>)
end;

// <variable-pattern>
//
// Matches variables (i.e. either ``foo'' or ``foo :: type'').
// 
define class <variable-pattern> (<simple-pattern>)
  //
  // The pattern-variable that gets bound to the variable name.
  slot variable-name-pattern :: <pattern-variable>,
    required-init-keyword: name:;
  //
  // The pattern-variable that gets bound to the type expression or <object>
  // if there is no type expression.
  slot variable-type-pattern :: false-or(<pattern-variable>), setter: #f,
    init-value: #f, init-keyword: type:;
end;

define method print-object (pattern :: <variable-pattern>, stream :: <stream>)
    => ();
  pprint-fields(pattern, stream,
		name-pattern: pattern.variable-name-pattern,
		type-pattern: pattern.variable-type-pattern);
end;

// <bound-variable-pattern>
//
// Matches variable = expression at the beginning of detail-lists.
// 
define class <bound-variable-pattern> (<simple-pattern>)
  //
  slot bound-variable-variable :: type-union(<variable-pattern>,<pattern-variable>),
    required-init-keyword: variable:;
  //
  // The pattern-variable that gets bound to the initial-value expression.
  slot bound-variable-value :: <pattern-variable>,
    required-init-keyword: value:;
end;  

define method print-object (pattern :: <bound-variable-pattern>,
			    stream :: <stream>)
    => ();
  pprint-fields(pattern, stream,
		variable-pattern: pattern.bound-variable-variable,
		value-pattern: pattern.bound-variable-value);
end;

define class <identifier-pattern> (<simple-pattern>)
  slot pattern-identifier :: <identifier-token>,
    required-init-keyword: identifier:;
end;

define method print-object (pattern :: <identifier-pattern>,
			    stream :: <stream>)
    => ();
  pprint-fields(pattern, stream, identifier: pattern.pattern-identifier);
end;

define abstract class <literal-pattern> (<simple-pattern>)
  slot pattern-literal :: <token>, required-init-keyword: literal:;
end;

define class <otherwise-pattern> (<literal-pattern>)
end;

define class <arrow-pattern> (<literal-pattern>)
end;

define class <details-pattern> (<simple-pattern>)
  slot pattern-sub-pattern :: <pattern>, required-init-keyword: sub-pattern:;
end;

define method print-object (pattern :: <details-pattern>, stream :: <stream>)
    => ();
  pprint-fields(pattern, stream, sub-pattern: pattern.pattern-sub-pattern);
end;

define class <pattern-variable> (<simple-pattern>)
  //
  // The name of this pattern variable.
  slot patvar-name :: false-or(<symbol>),
    init-value: #f, init-keyword: name:;
  //
  // The constraint, if any.
  slot patvar-constraint :: one-of(#f, #"expr", #"var", #"name",
				   #"body", #"case-body"),
    init-value: #f, init-keyword: constraint:;
  //
  // True if this pattern variable is a wildcard.
  slot patvar-wildcard? :: <boolean>,
    init-value: #f, init-keyword: wildcard:;
  //
  // True if this pattern variable is at the end of the pattern.
  slot patvar-at-end? :: <boolean>,
    init-value: #f, init-keyword: at-end:;
end;

define method print-object (pattern :: <pattern-variable>, stream :: <stream>)
    => ();
  pprint-fields(pattern, stream,
		name: pattern.patvar-name,
		if (pattern.patvar-constraint) constraint: end,
		pattern.patvar-constraint,
		if (pattern.patvar-wildcard?) wildcard?: end,
		pattern.patvar-wildcard?,
		if (pattern.patvar-at-end?) at-end?: end,
		pattern.patvar-at-end?);
end;

define method initialize
    (pv :: <pattern-variable>, #next next-method, #key token) => ();
  next-method();
  if (token)
    select (token by instance?)
      <name-token> =>
	pv.patvar-name := token.token-symbol;
	if (member?(token.token-symbol,
		    #[#"expr", #"var", #"name", #"body", #"case-body"]))
	  pv.patvar-constraint := token.token-symbol;
	end;
      <constrained-name-token> =>
	pv.patvar-name := token.token-symbol;
	pv.patvar-constraint := token.token-constraint;
      <ellipsis-token> =>
	#f;
    end;
  end;
end;

define class <property-list-pattern> (<object>)
  slot plistpat-rest :: false-or(<symbol>),
    init-value: #f, init-keyword: rest:;
  slot plistpat-keys :: false-or(<simple-object-vector>), setter: #f,
    init-value: #f, init-keyword: keys:;
  slot plistpat-all-keys? :: <boolean>, setter: #f,
    init-value: #f, init-keyword: all-keys:;
end;

define class <pattern-keyword> (<object>)
  //
  // The word for this pattern-keyword.  We don't use a pattern-variable
  // because pattern keywords arn't supposed to be constrained.
  slot patkey-name :: <symbol>, required-init-keyword: name:;
  //
  // The default expression, property-set, or #f if none.
  slot patkey-default :: type-union(<false>, <expression>, <property-set>),
    required-init-keyword: default:;
  //
  // #t if the word should be bound to a sequence of all the occurences of the
  // keyword instead of just the first.
  slot patkey-all? :: <boolean>, setter: #f,
    init-value: #f, init-keyword: all:;
end;

define class <auxiliary-rule-set> (<object>)
  //
  // The keyword name for this ruleset.
  slot rule-set-name :: <symbol>, required-init-keyword: name:;
  //
  // Vector of auxiliary-rules.
  slot rule-set-rules :: <simple-object-vector>, required-init-keyword: rules:;
  //
  // #t if this some rule in this rule sets ends in a body-variable, #f if
  // not.
  slot rule-set-body-variable? :: <boolean>, init-value: #f;
  //
  // #f until we've extracted any intermediate words at the start of this
  // rule set, #t after.
  slot rule-set-processed-intermediate-words? :: <boolean>, init-value: #f;
end;

define method print-object (aux-rule-set :: <auxiliary-rule-set>,
			    stream :: <stream>)
    => ();
  pprint-fields(aux-rule-set, stream,
		name: aux-rule-set.rule-set-name,
		rules: aux-rule-set.rule-set-rules,
		body-variable?: aux-rule-set.rule-set-body-variable?);
end;

define class <auxiliary-rule> (<rule>)
end;

define class <template> (<object>)
  //
  // Vector of either tokens or pattern-variable-references.
  slot template-parts :: <simple-object-vector>,
    required-init-keyword: parts:;
end;

define method print-object (template :: <template>, stream :: <stream>) => ();
  pprint-fields(template, stream, parts: template.template-parts);
end;

define class <paren-template> (<template>)
  slot template-left-token :: <token>, required-init-keyword: left-token:;
  slot template-right-token :: <token>, required-init-keyword: right-token:;
end;

define method print-object (template :: <paren-template>, stream :: <stream>)
    => ();
  pprint-fields(template, stream,
		left: template.template-left-token,
		parts: template.template-parts,
		right: template.template-right-token);
end;

define class <pattern-variable-reference> (<object>)
  //
  // The name being referenced, or #f if ...
  slot patvarref-name :: false-or(<symbol>), setter: #f,
    init-value: #f, init-keyword: name:;
  //
  // The separator to stick between elements if the pattern variable holds
  // a collection.
  slot patvarref-separator :: false-or(<token>), setter: #f,
    init-value: #f, init-keyword: separator:;
end;

define method print-object (pvarref :: <pattern-variable-reference>,
			    stream :: <stream>)
  => ();
  pprint-fields(pvarref, stream,
		name: pvarref.patvarref-name,
		if (pvarref.patvarref-separator) separator: end,
		pvarref.patvarref-separator);
end;
