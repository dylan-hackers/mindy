module: define-functions
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/convert/deffunc.dylan,v 1.4 1994/12/13 18:40:26 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


define abstract class <function-definition> (<abstract-constant-definition>)
  //
  // The signature.  Filled in during definition finalization.
  slot function-defn-signature :: <signature>, init-keyword: signature:;
  //
  // #t if this definition requires special handling at loadtime.  Can be
  // because of something non-constant in the signature or in the case of
  // methods, can be because the generic is hairy.  Fill in during
  // finalization.
  slot function-defn-hairy? :: <boolean>,
    init-value: #f, init-keyword: hairy:;
end;

define method defn-type (defn :: <function-definition>) => res :: <cclass>;
  dylan-value(#"<function>");
end;

define class <generic-definition> (<function-definition>)
  //
  // #f iff the open adjective wasn't supplied.
  slot generic-defn-sealed? :: <boolean>, required-init-keyword: sealed:;
  //
  // All the <method-definition>s defined on this generic function.
  slot generic-defn-methods :: <list>,
    init-value: #();
end;

define method defn-type (defn :: <generic-definition>) => res :: <cclass>;
  dylan-value(#"<generic-function>");
end;

define class <implicit-generic-definition>
    (<generic-definition>, <implicit-definition>)
  //
  // Implicit generic definitions are not sealed.
  keyword sealed:, init-value: #f;
end;

define abstract class <abstract-method-definition> (<function-definition>)
end;

define method defn-type (defn :: <abstract-method-definition>)
    => res :: <cclass>;
  dylan-value(#"<method>");
end;

define class <method-definition> (<abstract-method-definition>)
  //
  // The generic function this method is part of, or #f if the base-name is
  // undefined or not a generic function.
  slot method-defn-of :: union(<generic-definition>, <false>),
    required-init-keyword: method-of:;
end;


define class <define-generic-tlf> (<simple-define-tlf>)
  //
  // Make the definition required.
  required keyword defn:;
  //
  // The param list for the generic function.
  slot generic-tlf-param-list :: <parameter-list>,
    required-init-keyword: param-list:;
  //
  // The returns list for the generic function.
  slot generic-tlf-returns :: <parameter-list>,
    required-init-keyword: returns:;
end;

define class <define-method-tlf> (<simple-define-tlf>)
  //
  // The name being defined.  Note: this isn't the name of the method, it is
  // the name of the generic function.
  slot method-tlf-base-name :: <name>, required-init-keyword: base-name:;
  //
  // True if the define method is sealed, false if open.
  slot method-tlf-sealed? :: <boolean>, required-init-keyword: sealed:;
  //
  // The guts of the method being defined.
  slot method-tlf-parse :: <method-parse>, required-init-keyword: parse:;
  //
  // The implicit generic function definition, if there is one.  Made if there
  // is any chance it will be needed at parse time, and then cleared out if
  // it turns out to be redundent at defn finalization time.
  slot method-tlf-implicit-defn
    :: union(<implicit-generic-definition>, <false>),
    required-init-keyword: implicit-defn:;
end;


// process-top-level-form

define method process-top-level-form (form :: <define-generic-parse>) => ();
  let name = form.defgen-name.token-symbol;
  let (open?, sealed?)
    = extract-modifiers("define generic", name, form.define-modifiers,
			#"open", #"sealed");
  if (open? & sealed?)
    error("define generic %s can't be both open and sealed", name);
  end;
  extract-properties("define generic", form.defgen-plist);
  let defn = make(<generic-definition>,
		  name: make(<basic-name>,
			     symbol: name,
			     module: *Current-Module*),
		  sealed: sealed?);
  note-variable-definition(defn);
  add!($Top-Level-Forms,
       make(<define-generic-tlf>,
	    defn: defn,
	    param-list: form.defgen-param-list,
	    returns: form.defgen-returns));
end;

define method process-top-level-form (form :: <define-method-parse>) => ();
  let name = form.defmethod-method.method-name.token-symbol;
  let (open?, sealed?)
    = extract-modifiers("define method", name, form.define-modifiers,
			#"open", #"sealed");
  if (open? & sealed?)
    error("define method %s can't be both open and sealed", name);
  end;
  let var = find-variable(*Current-Module*, name);
  let base-name = make(<basic-name>, symbol: name, module: *Current-Module*);
  let implicit-defn
    = unless (var & var.variable-definition)
	let defn = make(<implicit-generic-definition>, name: base-name);
	note-variable-definition(defn);
	defn;
      end;
  add!($Top-Level-Forms,
       make(<define-method-tlf>, base-name: base-name, sealed: ~open?,
	    parse: form.defmethod-method, implicit-defn: implicit-defn));
end;


// finalize-top-level-form

define method finalize-top-level-form (tlf :: <define-generic-tlf>) => ();
  let defn = tlf.tlf-defn;
  let (signature, anything-non-constant?)
    = compute-signature(tlf.generic-tlf-param-list, tlf.generic-tlf-returns);
  defn.function-defn-signature := signature;
  if (anything-non-constant?)
    defn.function-defn-hairy? := #t;
    for (meth in defn.generic-defn-methods)
      meth.function-defn-hairy? := #t;
    end;
  end;
end;

define method finalize-top-level-form (tlf :: <define-method-tlf>) => ();
  let name = tlf.method-tlf-base-name;
  let var = find-variable(name.name-module, name.name-symbol);
  let generic-defn
    = if (var & instance?(var.variable-definition, <generic-definition>))
	var.variable-definition;
      end;
  let (signature, anything-non-constant?)
    = compute-signature(tlf.method-tlf-parse.method-param-list,
			tlf.method-tlf-parse.method-returns);
  let defn = make(<method-definition>,
		  name: make(<method-name>,
			     generic-function: name,
			     signature: signature),
		  signature: signature,
		  hairy: (anything-non-constant? | generic-defn == #f
			    | generic-defn.function-defn-hairy?),
		  method-of: generic-defn);
  tlf.tlf-defn := defn;
  if (generic-defn)
    generic-defn.generic-defn-methods
      := pair(defn, generic-defn.generic-defn-methods);
    if (tlf.method-tlf-implicit-defn == generic-defn)
      generic-defn.function-defn-signature
	:= make(<signature>,
		specializers: map(method (spec) object-ctype() end,
				  signature.specializers),
		rest-type:
		  signature.rest-type & ~signature.key-infos & object-ctype(),
		keys: signature.key-infos & #(),
		all-keys: #f,
		returns: #(),
		returns-rest-type: object-ctype());
    else
      tlf.method-tlf-implicit-defn := #f;
    end;
  else
    tlf.method-tlf-implicit-defn := #f;
  end;
end;

define method compute-signature
    (param-list :: <parameter-list>, returns :: <parameter-list>)
    => (signature :: <signature>, anything-non-constant? :: <boolean>);
  let anything-non-constant? = #f;
  let lexenv = make(<lexenv>);
  local
    method maybe-eval-type (param)
      let type = param.param-type;
      if (type)
	let ctype = ct-eval(type, lexenv);
	select (ctype by instance?)
	  <false> =>
	    anything-non-constant? := #t;
	    make(<unknown-ctype>);
	  <ctype> =>
	    ctype;
	  otherwise =>
	    // ### Should just be a warning.
	    error("%= isn't a type.", ctype);
	end;
      else
	object-ctype();
      end;
    end,
    method make-key-info (param)
      make(<key-info>,
	   key-name: param.param-keyword,
	   type: maybe-eval-type(param));
    end;
  values(make(<signature>,
	      specializers: map-as(<list>, maybe-eval-type,
				   param-list.paramlist-required-vars),
	      rest-type: param-list.paramlist-rest & object-ctype(),
	      keys: (param-list.paramlist-keys
		       & map-as(<list>, make-key-info,
				param-list.paramlist-keys)),
	      all-keys: param-list.paramlist-all-keys?,
	      returns: map-as(<list>, maybe-eval-type,
			      returns.paramlist-required-vars),
	      returns-rest-type: returns.paramlist-rest & object-ctype()),
	 anything-non-constant?);
end;


// Compile-top-level-form

define method convert-top-level-form
    (builder :: <fer-builder>, tlf :: <define-generic-tlf>) => ();
  convert-generic-definition(builder, tlf.tlf-defn);
end;

define method convert-generic-definition
    (builder :: <fer-builder>, defn :: <generic-definition>) => ();
  if (defn.function-defn-hairy?)
    let policy = $Default-Policy;
    let source = make(<source-location>);
    let args = make(<stretchy-vector>);
    // ### compute the args.
    build-assignment
      (builder, policy, source, make-definition-leaf(builder, defn),
       make-operation(builder,
		      pair(dylan-defn-leaf(builder, #"%make-gf"),
			   as(<list>, args))));
  end;
end;  

define method convert-top-level-form
    (builder :: <fer-builder>, tlf :: <define-method-tlf>) => ();
  if (tlf.method-tlf-implicit-defn)
    convert-generic-definition(builder, tlf.method-tlf-implicit-defn);
  end;
  let lexenv = make(<lexenv>);
  let method-literal = build-general-method(builder, tlf.method-tlf-parse,
					    lexenv, lexenv);
  let defn = tlf.tlf-defn;
  if (defn.function-defn-hairy?)
    let gf-name = tlf.method-tlf-base-name;
    let gf-var = find-variable(gf-name.name-module, gf-name.name-symbol);
    let gf-defn = gf-var & variable-definition(gf-var);
    if (gf-defn)
      let policy = $Default-Policy;
      let source = make(<source-location>);
      build-assignment
	(builder, policy, source, #(),
	 make-operation
	   (builder,
	    list(dylan-defn-leaf(builder, #"add-method"),
		 make-definition-leaf(builder, gf-defn),
		 method-literal)));
    else
      error("No definition for generic function %=, and can't implicitly "
	      "define it.",
	    gf-name);
    end;
  end;
end;
