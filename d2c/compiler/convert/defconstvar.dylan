module: define-constants-and-variables
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/convert/defconstvar.dylan,v 1.15 1995/05/05 08:49:51 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


// definition class definitions

define abstract class <bindings-definition> (<definition>)
  //
  // The <ctype> for this definition if it is a compile-time constant.  Filled
  // in by finalize-top-level-form.
  slot defn-type :: union(<false>, <ctype>), init-keyword: type:;
  //
  // The initial value (or only value for constants) if it is a compile-time
  // value, #f if it isn't compile-time computable, and #"not-computed-yet"
  // if we haven't figured it out yet.  Filled in either by ct-value on a
  // constant or by finalize-top-level-form.
  slot defn-init-value :: union(<ct-value>, one-of(#f, #"not-computed-yet")),
    init-value: #"not-computed-yet", init-keyword: value:;
end;

define class <constant-definition>
    (<bindings-definition>, <abstract-constant-definition>)
  //
  // The top level form that makes this definition, or #f if it wasn't defined
  // by a define constant in this library.  If this is #f, then the init-value
  // must be supplied.
  slot defconst-tlf :: union(<false>, <define-constant-tlf>),
    init-value: #f;
end;

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
  inherited slot defn-init-value, init-value: #f;
end;

define class <variable-definition> (<bindings-definition>)
  //
  // The <constant-definition> for the type if the type isn't a compile-time
  // constant.  Filled in by finalize-top-level-form.
  slot var-defn-type-defn :: union(<false>, <constant-definition>),
    init-value: #f;
end;


// Top level form class definitions.

define abstract class <define-bindings-tlf> (<define-tlf>)
  slot tlf-bindings :: <bindings>,
    required-init-keyword: bindings:;
  slot tlf-required-defns :: <simple-object-vector>,
    required-init-keyword: required-defns:;
  slot tlf-rest-defn :: union(<false>, <bindings-definition>),
    required-init-keyword: rest-defn:;
  slot tlf-finalized? :: <boolean>,
    init-value: #f;
  slot tlf-anything-non-constant? :: <boolean>,
    init-value: #f;
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

define class <define-constant-tlf> (<define-bindings-tlf>)
end;

define method initialize (tlf :: <define-constant-tlf>, #key)
  for (defn in tlf.tlf-required-defns)
    defn.defconst-tlf := tlf;
  end;
  if (tlf.tlf-rest-defn)
    tlf.tlf-rest-defn.defconst-tlf := tlf;
  end;
end;

define class <define-constant-method-tlf> (<define-constant-tlf>)
end;

define class <define-variable-tlf> (<define-bindings-tlf>)
end;



// Process-top-level-form methods

define method process-top-level-form (form :: <define-constant-parse>) => ();
  let bindings = form.defconst-bindings;
  let param-list = bindings.bindings-parameter-list;
  if (param-list.paramlist-required-vars.size == 1
	& param-list.paramlist-required-vars[0].param-type == #f
	& param-list.paramlist-rest == #f
	& begin
	    let method-ref
	      = expand-until-method-ref(bindings.bindings-expression);
	    method-ref & (bindings.bindings-expression := method-ref);
	  end)
    process-aux(bindings, <define-constant-method-tlf>,
		<constant-method-definition>)
  else
    process-aux(bindings, <define-constant-tlf>, <constant-definition>);
  end;
end;

define method expand-until-method-ref (expr :: <expression>)
    => res :: union(<false>, <method-ref>);
  #f;
end;

define method expand-until-method-ref (expr :: <method-ref>)
    => res :: union(<false>, <method-ref>);
  expr;
end;

define method expand-until-method-ref (expr :: <begin>)
    => res :: union(<false>, <method-ref>);
  let body = expr.begin-body;
  body.size == 1 & expand-until-method-ref(body[0]);
end;

define method expand-until-method-ref (expr :: <macro-statement>)
    => res :: union(<false>, <method-ref>);
  let expansion = expand(expr, #f);
  expansion & expansion.size == 1 & expand-until-method-ref(expansion[0]);
end;

define method process-top-level-form (form :: <define-variable-parse>) => ();
  process-aux(form.defvar-bindings, <define-variable-tlf>,
	      <variable-definition>);
end;

define method process-aux (bindings :: <bindings>, tlf-class :: <class>,
			   defn-class :: <class>)
    => ();
  local method make-and-note-defn (name :: <name-token>)
	  let defn = make(defn-class,
			  name: make(<basic-name>,
				     symbol: name.token-symbol,
				     module: *Current-Module*));
	  note-variable-definition(defn);
	  defn;
	end;
  let paramlist = bindings.bindings-parameter-list;
  let rest-param = paramlist.paramlist-rest;
  add!($Top-Level-Forms,
       make(tlf-class,
	    bindings: bindings,
	    required-defns: map(compose(make-and-note-defn, param-name),
				paramlist.paramlist-required-vars),
	    rest-defn: rest-param & make-and-note-defn(rest-param)));
end;


// Compile-time value stuff.

define method ct-value (defn :: <constant-definition>)
    => res :: union(<false>, <ct-value>);
  if (defn.defn-init-value == #"not-computed-yet")
    let tlf = defn.defconst-tlf;
    if (tlf)
      if (tlf.tlf-finalized?)
	compiler-warning("%= is circularly defined.", defn);
	defn.defn-init-value = #f;
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


// finalize-top-level-form

define method finalize-top-level-form (tlf :: <define-bindings-tlf>) => ();
  unless (tlf.tlf-finalized?)
    // We set it to #t before doing anything so that we can detect
    // circularities.
    tlf.tlf-finalized? := #t;
    // Eval the expression
    let (#rest res) = ct-mv-eval(tlf.tlf-bindings.bindings-expression, #f);
    let constant? = res.empty? | ~(res[0] == #f);
    // Fill in all the required definitions.
    for (defn in tlf.tlf-required-defns,
	 param in tlf.tlf-bindings.bindings-parameter-list
	   .paramlist-required-vars,
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
	  if (csubtype?(make-canonical-singleton(value), type))
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
		    name: make(<type-cell-name>, base: defn.defn-name),
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
  let meth = tlf.tlf-bindings.bindings-expression.method-ref-method;
  let (signature, anything-non-constant?)
    = compute-signature(meth.method-param-list, meth.method-returns);
  let defn = tlf.tlf-required-defns[0];
  defn.function-defn-signature := signature;
  if (anything-non-constant?)
    defn.function-defn-hairy? := #t;
    tlf.tlf-anything-non-constant? := #t;
  end;
end;


// Convert-top-level-form

define method convert-top-level-form
    (builder :: <fer-builder>, tlf :: <define-bindings-tlf>) => ();
  if (tlf.tlf-anything-non-constant?)
    let lexenv = make(<lexenv>);
    let policy = $Default-Policy;
    let source = make(<source-location>);
    let init-builder = make-builder(builder);
    let bindings = tlf.tlf-bindings;
    let paramlist = bindings.bindings-parameter-list;
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
		  let type-defn = defn.var-defn-type-defn;
		  build-assignment
		    (builder, policy, source, #(),
		     make-operation(builder, <set>, list(type),
				    var: type-defn));
		end;
		make-check-type-operation(init-builder, temp, type);
	      end;
	  build-assignment(init-builder, policy, source, #(),
			   make-operation(init-builder, <set>, list(checked),
					  var: defn));
	end;
	temp;
      end;
    let vars = map-as(<list>, foo, tlf.tlf-required-defns,
		      paramlist.paramlist-required-vars);
    let rest-defn = tlf.tlf-rest-defn;
    if (rest-defn & ~rest-defn.defn-init-value)
      let rest-temp = make-local-var(builder,
				     paramlist.paramlist-rest.token-symbol,
				     rest-defn.defn-type | object-ctype());
      let cluster = make-values-cluster(builder, #"cluster", wild-ctype());
      fer-convert(builder, bindings.bindings-expression, lexenv,
		  #"assignment", cluster);
      build-assignment
	(builder, policy, source, concatenate(vars, list(rest-temp)),
	 make-operation
	   (builder, <primitive>, list(cluster),
	    name: #"canonicalize-results"));
      build-assignment(init-builder, policy, source, #(),
		       make-operation(init-builder, <set>, list(rest-temp),
				      var: rest-defn));
    else
      fer-convert(builder, bindings.bindings-expression, lexenv,
		  #"assignment", vars);
    end;
    build-region(builder, builder-result(init-builder));
  end;
end;

define method convert-top-level-form
    (builder :: <fer-builder>, tlf :: <define-constant-method-tlf>,
     #next next-method)
    => ();
  let lexenv = make(<lexenv>);
  let meth = tlf.tlf-bindings.bindings-expression.method-ref-method;
  let leaf = build-general-method(builder, meth, lexenv, lexenv);
  let literal-method? = instance?(leaf, <method-literal>);
  let defn = tlf.tlf-required-defns[0];
  defn.method-defn-leaf := literal-method? & leaf;
  if (defn.function-defn-hairy? | ~literal-method?)
    let source = make(<source-location>);
    build-assignment(builder, lexenv.lexenv-policy, source, #(),
		     make-operation(builder, <set>, list(leaf), var: defn));
  end;
end;
