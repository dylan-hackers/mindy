module: define-constants-and-variables
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/convert/defconstvar.dylan,v 1.2 1994/12/12 21:24:06 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


// definition class definitions

define abstract class <bindings-definition> (<definition>)
  //
  // The particular top-level form this defn came from.
  slot defn-tlf :: union(<false>, <define-bindings-tlf>),
    init-value: #f, init-keyword: tlf:;
  //
  // The <ctype> for this definition if it is a compile-time constant.  Filled
  // in by finalize-top-level-form.
  slot defn-type :: union(<false>, <ctype>), init-keyword: type:;
  //
  // The initial value (or only value for constants) if it is a compile-time
  // value, or #f otherwise.  Filled in either when defn-value on a constant,
  // or by finalize-top-level-form.
  slot defn-init-value :: union(<false>, <ct-value>), init-keyword: value:;
end;

define class <constant-definition>
    (<bindings-definition>, <abstract-constant-definition>)
end;

define class <variable-definition> (<bindings-definition>)
  //
  // The <constant-definition> for the type if the type isn't a compile-time
  // constant.  Filled in by finalize-top-level-form.
  slot var-defn-type-defn :: union(<false>, <constant-definition>);
end;


// Top level form class definitions.

define abstract class <define-bindings-tlf> (<define-tlf>)
  slot tlf-bindings :: <bindings>,
    required-init-keyword: bindings:;
  slot tlf-required-defns :: <simple-object-vector>,
    init-keyword: required-defns:;
  slot tlf-rest-defn :: union(<false>, <bindings-definition>),
    init-keyword: rest-defn:;
  slot tlf-finalized? :: one-of(#t, #f, #"computing"),
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

define class <define-variable-tlf> (<define-bindings-tlf>)
end;



// Process-top-level-form methods

define method process-top-level-form (form :: <define-constant-parse>) => ();
  process-aux(form.defconst-bindings, <define-constant-tlf>,
	      <constant-definition>);
end;

define method process-top-level-form (form :: <define-variable-parse>) => ();
  process-aux(form.defvar-bindings, <define-variable-tlf>,
	      <variable-definition>);
end;

define method process-aux (bindings :: <bindings>, tlf-class :: <class>,
			   defn-class :: <class>)
  let tlf = make(tlf-class, bindings: bindings);
  local method make-and-note-defn (name :: <name-token>)
	  let defn = make(defn-class,
			  name: make(<basic-name>,
				     symbol: name.token-symbol,
				     module: *Current-Module*),
			  tlf: tlf);
	  note-variable-definition(defn);
	  defn;
	end;
  let params = bindings.bindings-parameter-list;
  tlf.tlf-required-defns := map(compose(make-and-note-defn, param-name),
				params.paramlist-required-vars);
  tlf.tlf-rest-defn
    := params.paramlist-rest & make-and-note-defn(params.paramlist-rest);
  add!($Top-Level-Forms, tlf);
end;


// Compile-time value stuff.

define method ct-value (defn :: <constant-definition>)
    => res :: union(<false>, <cclass>);
  let tlf = defn.defn-tlf;
  select (tlf.tlf-finalized?)
    #t =>
      defn.defn-init-value;
    #f =>
      tlf.tlf-finalized? := #"computing";
      finalize-top-level-form(tlf);
      defn.defn-init-value;
    #"computing" =>
      error("constant %s circularly defined.", defn.defn-name);
  end;
end;


// finalize-top-level-form

define method finalize-top-level-form (tlf :: <define-bindings-tlf>) => ();
  unless (tlf.tlf-finalized?)
    let lexenv = make(<lexenv>);
    let (#rest res) = ct-mv-eval(tlf.tlf-bindings.bindings-expression, lexenv);
    let constant? = res.empty? | ~(res[0] == #f);
    for (defn in tlf.tlf-required-defns,
	 param in tlf.tlf-bindings.bindings-parameter-list
	   .paramlist-required-vars,
	 index from 0)
      let type = if (param.param-type)
		   ct-eval(param.param-type, lexenv);
		 else
		   object-ctype();
		 end;
      defn.defn-type := type;
      if (type)
	if (constant?)
	  let value = if (index < res.size)
			res[index];
		      else
			make(<ct-literal>, value: #f);
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
		    tlf: tlf,
		    type: dylan-value(#"<type>"),
		    init-value: #f);
	end;
      end;
    end;

    if (tlf.tlf-rest-defn)
      if (~constant?)
	tlf.tlf-anything-non-constant? := #t;
	tlf.tlf-rest-defn.defn-init-value := #f;
      elseif (tlf.tlf-required-defns.size < res.size)
	let tail = copy-sequence(res, start: tlf.tlf-required-defns.size);
	if (every?(rcurry(instance?, <ct-literal>), res))
	  tlf.tlf-rest-defn.defn-init-value
	    := make(<ct-literal>, value: map(ct-literal-value, tail));
	else
	  tlf.tlf-anything-non-constant? := #t;
	  tlf.tlf-rest-defn.defn-init-value := #f;
	end;
      else
	tlf.tlf-rest-defn.defn-init-value := make(<ct-literal>, value: #[]);
      end;
    end;
    tlf.tlf-finalized? := #t;
  end;
end;


// Convert-top-level-form

define method convert-top-level-form
    (builder :: <fer-builder>, tlf :: <define-bindings-tlf>) => ();
  if (tlf.tlf-anything-non-constant?)
    error("Can't deal with non-constant init values or types.");
  end;
end;
