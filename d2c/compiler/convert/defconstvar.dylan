module: define-constants-and-variables
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

// Parse tree classes and macro expanders.

define abstract class <define-binding-parse> (<definition-parse>)
  constant slot defbinding-variables :: <variable-list>,
    required-init-keyword: variables:;
  slot defbinding-expression :: <expression-parse>,
    required-init-keyword: expression:;
end class <define-binding-parse>;

define sealed domain make (singleton(<define-binding-parse>));
define sealed domain initialize (<define-binding-parse>);

define class <define-constant-parse> (<define-binding-parse>)
end class <define-constant-parse>;

define sealed domain make (singleton(<define-constant-parse>));

define-procedural-expander
  (#"make-define-constant",
   method (generator :: <expansion-generator>, variables-frag :: <fragment>,
	   expression-frag :: <fragment>)
       => ();
     generate-fragment
       (generator,
	make-parsed-fragment
	  (make(<define-constant-parse>,
		source-location: generator.generator-call.source-location,
		variables: parse-variable-list(make(<fragment-tokenizer>,
						    fragment: variables-frag)),
		expression: expression-from-fragment(expression-frag)),
	   source-location: generate-token-source-location(generator)));
   end method);

define class <define-variable-parse> (<define-binding-parse>)
end class <define-variable-parse>;

define sealed domain make (singleton(<define-variable-parse>));

define-procedural-expander
  (#"make-define-variable",
   method (generator :: <expansion-generator>, variables-frag :: <fragment>,
	   expression-frag :: <fragment>)
       => ();
     generate-fragment
       (generator,
	make-parsed-fragment
	  (make(<define-variable-parse>,
		source-location: generator.generator-source.source-location,
		variables: parse-variable-list(make(<fragment-tokenizer>,
						    fragment: variables-frag)),
		expression: expression-from-fragment(expression-frag)),
	   source-location: generate-token-source-location(generator)));
   end method);


// More definitions.

define class <constant-definition>
    (<bindings-definition>, <abstract-constant-definition>)
  slot const-defn-tlf :: <define-constant-tlf>;
end;

// definition-kind{<constant-definition>} -- method on imported GF
// 
define method definition-kind
    (defn :: <constant-definition>) => kind :: <byte-string>;
  "constant";
end method definition-kind;


// <constant-method-definition>
//
// For use with ``define constant foo = method ... end''
// 
define class <constant-method-definition>
    (<abstract-method-definition>,
     // We explicitly list function-definition to make sure it shows up in
     // the CPL before constant-definition so that we pick up fn-defns
     // version of defn-type instead of the slot.
     <function-definition>,
     <constant-definition>)
end;



// Top level form class definitions.

define abstract class <real-define-bindings-tlf> (<define-bindings-tlf>)
  constant slot tlf-variables :: <variable-list>,
    required-init-keyword: variables:;
  constant slot tlf-expression :: <expression-parse>,
    required-init-keyword: expression:;
  slot tlf-finalized? :: <boolean>,
    init-value: #f;
  slot tlf-anything-non-constant? :: <boolean>,
    init-value: #f;
end;

define method print-message (tlf :: <define-bindings-tlf>, stream :: <stream>)
    => ();
  write(stream, "Define ");
  select (tlf by instance?)
    <define-constant-tlf> => write(stream, "Constant ");
    <define-variable-tlf> => write(stream, "Variable ");
  end;
  for (defn in tlf.tlf-required-defns, first? = #t then #f)
    unless (first?)
      write(stream, ", ");
    end;
    print-message(defn.defn-name, stream);
  finally
    if (tlf.tlf-rest-defn)
      unless (first?)
	write(stream, ", ");
      end;
      write(stream, "#rest ");
      print-message(tlf.tlf-rest-defn.defn-name, stream);
    end;
  end;
end;

define method print-object (tlf :: <define-bindings-tlf>, stream :: <stream>)
    => ();
  let req-defns = tlf.tlf-required-defns;
  let rest-defn = tlf.tlf-rest-defn;
  if (req-defns.size == 1 & ~rest-defn)
    pprint-fields(tlf, stream, defn: req-defns[0]);
  else
    pprint-fields(tlf, stream,
		  defns: req-defns,
		  rest-defn & (rest-defn:), rest-defn);
  end;
end;

define class <define-constant-tlf> (<real-define-bindings-tlf>)
end;


define method initialize
    (tlf :: <define-constant-tlf>, #next next-method, #key)
    => ();
  for (defn in tlf.tlf-required-defns)
    defn.const-defn-tlf := tlf;
  end for;
  if (tlf.tlf-rest-defn)
    tlf.tlf-rest-defn.const-defn-tlf := tlf;
  end if;
end method initialize;


define class <define-constant-method-tlf> (<define-constant-tlf>)
end;

define class <define-variable-tlf> (<real-define-bindings-tlf>)
end;



// Process-top-level-form methods

define method process-top-level-form (form :: <define-constant-parse>) => ();
  let variables = form.defbinding-variables;
  if (variables.varlist-fixed.size == 1
	& variables.varlist-fixed[0].param-type == #f
	& variables.varlist-rest == #f
	& begin
	    let method-ref
	      = expand-until-method-ref(form.defbinding-expression);
	    method-ref & (form.defbinding-expression := method-ref);
	  end)
    process-aux(form, <define-constant-method-tlf>, <constant-method-definition>);
  else
    process-aux(form, <define-constant-tlf>, <constant-definition>);
  end;
end;

define method expand-until-method-ref (expr :: <expression-parse>)
    => res :: false-or(<method-ref-parse>);
  #f;
end;

define method expand-until-method-ref (expr :: <callback-method-ref-parse>)
    => res :: false-or(<method-ref-parse>);
  #f;
end;

define method expand-until-method-ref (expr :: <method-ref-parse>)
    => res :: false-or(<method-ref-parse>);
  expr;
end;

define method expand-until-method-ref (expr :: <body-parse>)
    => res :: false-or(<method-ref-parse>);
  let body = expr.body-parts;
  body.size == 1 & expand-until-method-ref(body[0]);
end;

define method expand-until-method-ref (expr :: <macro-call-parse>)
    => res :: false-or(<method-ref-parse>);
  expand-until-method-ref(macro-expand(expr));
end;

define method process-top-level-form (form :: <define-variable-parse>) => ();
  process-aux(form, <define-variable-tlf>, <variable-definition>);
end;

define method process-aux(form :: <define-binding-parse>,
			  tlf-class :: <class>,
			  defn-class :: <class>)
	=> ();

  let (variables :: <variable-list>, expr :: <expression-parse>, source :: <source-location>)
	= values(form.defbinding-variables, form.defbinding-expression, form.source-location);

  local method make-and-note-defn (param :: <parameter>)
	  let name = param.param-name;
	  let defn = make(defn-class,
			  name: make(<basic-name>,
				     symbol: name.token-symbol,
				     module: *Current-Module*),
			  source-location: source,
			  library: *Current-Library*);
	  note-variable-definition(defn);
	  defn;
	end;

  let required-defns = map(make-and-note-defn, variables.varlist-fixed);
  let rest-param = variables.varlist-rest;
  let rest-defn = rest-param & make-and-note-defn(rest-param);
  let tlf = make(tlf-class,
		 variables: variables,
		 expression: expr,
		 required-defns: required-defns,
		 rest-defn: rest-defn,
		 source-location: source);
  add!(*Top-Level-Forms*, tlf);
end;

define method process-aux(form :: <define-binding-parse>,
			  tlf-class == <define-constant-method-tlf>,
			  defn-class :: <class>)
	=> ();

  let (variables :: <variable-list>, expr :: <method-ref-parse>, source :: <source-location>)
    = values(form.defbinding-variables, form.defbinding-expression, form.source-location);

  let (inline-type-frag, movable?-frag, flushable?-frag)
    = extract-properties(expr.method-ref-options,
                         #"inline-type", #"movable", #"flushable");
  let inline-type
    = inline-type-frag & extract-identifier(inline-type-frag).token-symbol;
  let movable? = movable?-frag & extract-boolean(movable?-frag);
  let flushable? = flushable?-frag & extract-boolean(flushable?-frag);

  local method make-and-note-defn (param :: <parameter>)
	  let name = param.param-name;
	  let defn = make(defn-class,
			  name: make(<basic-name>,
				     symbol: name.token-symbol,
				     module: *Current-Module*),
			  source-location: source,
			  library: *Current-Library*,
                          inline-type: inline-type | #"default-inline",
                          movable: movable?,
                          flushable: flushable? | movable?);
	  note-variable-definition(defn);
	  defn;
	end;

  let required-defns = map(make-and-note-defn, variables.varlist-fixed);
  let rest-param = variables.varlist-rest;
  let rest-defn = rest-param & make-and-note-defn(rest-param);
  let tlf = make(tlf-class,
		 variables: variables,
		 expression: expr,
		 required-defns: required-defns,
		 rest-defn: rest-defn,
		 source-location: source);
  add!(*Top-Level-Forms*, tlf);
end;


// Compile-time value stuff.

define method ct-value (defn :: <constant-definition>)
    => res :: false-or(<ct-value>);
  if (defn.%defn-init-value == #"not-computed-yet")
    let tlf = defn.const-defn-tlf;
    if (tlf)
      if (tlf.tlf-finalized?)
	compiler-warning("%= is circularly defined.", defn);
	defn.defn-init-value := #f;
      else
	finalize-top-level-form(tlf);
      end;
    else
      error("%= doesn't have a value and we don't know how to compute it?",
	    defn);
    end;
  end;
  defn.defn-init-value;
end;

define method ct-value (defn :: <constant-method-definition>, 
                        #next next-method)
    => res :: false-or(<ct-value>);
  if (defn.function-defn-ct-value == #"not-computed-yet")
    let tlf = defn.const-defn-tlf;
    if (tlf)
      if (~tlf.tlf-finalized?)
        finalize-top-level-form(tlf);
      end;
    else
      error("%= doesn't have a value and we don't know how to compute it?",
            defn);
    end;
  end if;
  next-method();
end;


// finalize-top-level-form

define method finalize-top-level-form (tlf :: <define-bindings-tlf>) => ();
  unless (tlf.tlf-finalized?)
    // We set it to #t before doing anything so that we can detect
    // circularities.
    tlf.tlf-finalized? := #t;
    // Eval the expression
    let (#rest res) = ct-mv-eval(tlf.tlf-expression, #f);
    let constant? = res.empty? | ~(res[0] == #f);
    // Fill in all the required definitions.
    for (defn in tlf.tlf-required-defns,
	 param in tlf.tlf-variables.varlist-fixed,
	 index from 0)
      let type = if (param.param-type)
		   let ctype = ct-eval(param.param-type, #f);
		   instance?(ctype, <ctype>) & ctype;
		 else
		   object-ctype();
		 end;
      defn.defn-type := type;
      if (type)
	if (constant?)
	  let value = if (index < res.size)
			res[index];
		      else
			make(<literal-false>);
		      end;
	  if (cinstance?(value, type))
	    defn.defn-init-value := value;
	  else
	    // ### Should also spew a warning about the type not being correct.
	    tlf.tlf-anything-non-constant? := #t;
	    defn.defn-init-value := #f;
	  end;
	else
	  tlf.tlf-anything-non-constant? := #t;
	  defn.defn-init-value := #f;
	end;
      else
	tlf.tlf-anything-non-constant? := #t;
	defn.defn-init-value := #f;
	if (instance?(defn, <variable-definition>))
	  defn.var-defn-type-defn
	    := make(<constant-definition>,
		    name: make(<derived-name>, how: #"type-cell",
		    	       base: defn.defn-name),
		    library: defn.defn-library,
		    type: dylan-value(#"<type>"),
		    value: #f);
	end;
      end;
    end;
    // Fill in the rest definition, if there is one.
    if (tlf.tlf-rest-defn)
      if (~constant?)
	tlf.tlf-anything-non-constant? := #t;
	tlf.tlf-rest-defn.defn-init-value := #f;
      elseif (tlf.tlf-required-defns.size < res.size)
	let tail = copy-sequence(res, start: tlf.tlf-required-defns.size);
	if (every?(rcurry(instance?, <literal>), tail))
	  tlf.tlf-rest-defn.defn-init-value
	    := make(<literal-simple-object-vector>, contents: tail);
	else
	  tlf.tlf-anything-non-constant? := #t;
	  tlf.tlf-rest-defn.defn-init-value := #f;
	end;
      else
	tlf.tlf-rest-defn.defn-init-value
	  := make(<literal-simple-object-vector>, contents: #[]);
      end;
    end;
  end;
end;

define method finalize-top-level-form
    (tlf :: <define-constant-method-tlf>) => ();
  tlf.tlf-finalized? := #t;
  let meth = tlf.tlf-expression.method-ref-method;
  let (signature, anything-non-constant?)
    = compute-signature(meth.method-parameters, meth.method-returns);
  let defn = tlf.tlf-required-defns[0];
  defn.function-defn-signature := signature;
  if (anything-non-constant?)
    defn.function-defn-hairy? := #t;
    if(defn.function-defn-ct-value == #"not-computed-yet")
      defn.function-defn-ct-value := #f;
    end if;
    tlf.tlf-anything-non-constant? := #t;
    if (defn.function-defn-ct-value)
      error("When defining %s, noticed that a function was hairy"
            " after creating a ct-value.",
	    tlf.tlf-required-defns[0].defn-name);
    end;
  elseif (defn.method-defn-inline-type ~== #"not-inline")
    defn.%method-defn-inline-function := rcurry(expand-inline-function, tlf.tlf-expression.method-ref-method);
  end;
end;


// Convert-top-level-form

define method convert-top-level-form
    (builder :: <fer-builder>, tlf :: <define-bindings-tlf>) => ();
  if (tlf.tlf-anything-non-constant?)
    let lexenv = lexenv-for-tlf(tlf);
    let policy = $Default-Policy;
    let source = tlf.source-location;
    let init-builder = make-builder(builder);
    let variables = tlf.tlf-variables;
    local
      method foo (defn, param)
	let temp = make-local-var(builder, param.param-name.token-symbol,
				  defn.defn-type | object-ctype());
	unless (defn.defn-init-value)
	  let checked
	    = if (defn.defn-type)
		temp;
	      else
		let type = make-local-var(builder, #"type",
					  dylan-value(#"<type>"));
		fer-convert(builder, param.param-type, lexenv,
			    #"assignment", type);
		if (instance?(defn, <variable-definition>))
		  build-defn-set(builder, policy, source,
				 defn.var-defn-type-defn, type);
		end;
		make-check-type-operation(init-builder, policy, source,
					  temp, type);
	      end;
	  build-defn-set(init-builder, policy, source, defn, checked);
	end;
	temp;
      end;
    let vars = map-as(<list>, foo, tlf.tlf-required-defns,
		      variables.varlist-fixed);
    let rest-defn = tlf.tlf-rest-defn;
    if (rest-defn & ~rest-defn.defn-init-value)
      let rest-temp
	= make-local-var(builder,
			 variables.varlist-rest.param-name.token-symbol,
			 rest-defn.defn-type | object-ctype());
      let cluster = make-values-cluster(builder, #"cluster", wild-ctype());
      fer-convert(builder, tlf.tlf-expression, lexenv,
		  #"assignment", cluster);
      build-assignment
	(builder, policy, source, concatenate(vars, list(rest-temp)),
	 make-operation
	   (builder, <primitive>,
	    list(cluster,
		 make-literal-constant(builder, vars.size)),
	    name: #"canonicalize-results"));
      build-defn-set(init-builder, policy, source, rest-defn, rest-temp);
    else
      fer-convert(builder, tlf.tlf-expression, lexenv,
		  #"assignment", vars);
    end;
    build-region(builder, builder-result(init-builder));
  end;
end;

define method convert-top-level-form
    (builder :: <fer-builder>, tlf :: <define-constant-method-tlf>,
     #next next-method)
    => ();
  let defn = tlf.tlf-required-defns[0];
  let lexenv = lexenv-for-tlf(tlf);
  let meth = tlf.tlf-expression.method-ref-method;
  let leaf = fer-convert-method(builder, meth, defn.defn-name, defn.ct-value, #"global",
				lexenv, lexenv);
  if (defn.function-defn-hairy?)
    build-defn-set(builder, lexenv.lexenv-policy, tlf.source-location, defn, leaf);
  end;
end;


// Dump/load stuff.

define method dump-od (tlf :: <define-bindings-tlf>, state :: <dump-state>)
    => ();
  for (defn in tlf.tlf-required-defns)
    dump-simple-object(#"define-binding-tlf", state, defn);
  end;
  if (tlf.tlf-rest-defn)
    dump-simple-object(#"define-binding-tlf", state, tlf.tlf-rest-defn);
  end;
end;

define constant $bindings-definition-slots
  = list(defn-type, type:, defn-type-setter,
	 %defn-init-value, value:, defn-init-value-setter);

add-make-dumper(#"constant-definition", *compiler-dispatcher*,
		<constant-definition>,
		concatenate($definition-slots,
			    $bindings-definition-slots),
		load-external: #t);

add-make-dumper(#"constant-method-definition", *compiler-dispatcher*,
		<constant-method-definition>,
		// abstract-method-definition-slots includes defn-slots
		concatenate($abstract-method-definition-slots,
			    $bindings-definition-slots),
		load-external: #t);

add-make-dumper(#"variable-definition", *compiler-dispatcher*,
		<variable-definition>,
		concatenate($definition-slots,
			    $bindings-definition-slots,
			    list(var-defn-type-defn, type-defn:,
				   var-defn-type-defn-setter)),
		load-external: #t);

// Seals for file defconstvar.dylan

// <constant-definition> -- subclass of <bindings-definition>, <abstract-constant-definition>
define sealed domain make(singleton(<constant-definition>));
define sealed domain initialize(<constant-definition>);
// <constant-method-definition> -- subclass of <abstract-method-definition>,

define sealed domain make(singleton(<constant-method-definition>));
define sealed domain initialize(<constant-method-definition>);
// <define-constant-tlf> -- subclass of <real-define-bindings-tlf>
define sealed domain make(singleton(<define-constant-tlf>));
// <define-constant-method-tlf> -- subclass of <define-constant-tlf>
define sealed domain make(singleton(<define-constant-method-tlf>));
// <define-variable-tlf> -- subclass of <real-define-bindings-tlf>
define sealed domain make(singleton(<define-variable-tlf>));
