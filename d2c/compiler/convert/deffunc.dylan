module: define-functions
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/convert/deffunc.dylan,v 1.60 1996/02/21 17:03:15 ram Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


define method defn-type (defn :: <function-definition>) => res :: <cclass>;
  dylan-value(#"<function>");
end;

define class <generic-definition> (<function-definition>)
  //
  // #f iff the open adjective wasn't supplied.
  slot generic-defn-sealed? :: <boolean>, init-keyword: sealed:,
    init-function:
      curry(error, "sealed: unsupplied in make of <generic-definition>");
  //
  // All the <method-definition>s defined on this generic function.
  slot generic-defn-methods :: <list>,
    init-value: #(), init-keyword: methods:;
  //
  // List of all the seals on this generic function.  Each seal is a list of
  // <seal-info>s.
  slot generic-defn-seals :: <list>,
    init-value: #(), init-keyword: seals:;
  //
  // The discriminator ct-value, if there is one.
  slot %generic-defn-discriminator
    :: type-union(<ct-function>, one-of(#f, #"not-computed-yet")),
    init-value: #"not-computed-yet", init-keyword: discriminator:;
end;

define method defn-type (defn :: <generic-definition>) => res :: <cclass>;
  dylan-value(#"<generic-function>");
end;

define class <implicit-generic-definition>
    (<generic-definition>, <implicit-definition>)
  //
  // Implicit generic definitions are sealed.
  inherited slot generic-defn-sealed?, init-value: #t;
end;

define abstract class <abstract-method-definition> (<function-definition>)
  //
  // The <method-parse> if we are to inline this method, #f otherwise.
  slot method-defn-inline-expansion :: false-or(<method-parse>),
    init-value: #f, init-keyword: inline-expansion:;
  //
  // The <function-literal> to clone when inlining this method, #f if we can't
  // inline it, and #"not-computed-yet" if we haven't tried yet.
  slot %method-defn-inline-function
    :: type-union(<function-literal>, one-of(#f, #"not-computed-yet")),
    init-value: #"not-computed-yet", init-keyword: inline-function:;
end;

// The actual definition of this is in cheese.dylan because it wants to call
// optimize-component, but the module system won't let us do yet.  Well, okay,
// we could have played some games with creating optimize-component somewhere
// and forward referencing it, but hell, if we have to be gross, we might as
// well be way gross.
// 
define generic method-defn-inline-function
    (defn :: <abstract-method-definition>)
    => res :: false-or(<function-literal>);

define method defn-type (defn :: <abstract-method-definition>)
    => res :: <cclass>;
  dylan-value(#"<method>");
end;

define class <method-definition> (<abstract-method-definition>)
  //
  // The generic function this method is part of, or #f if the base-name is
  // undefined or not a generic function.
  slot method-defn-of :: false-or(<generic-definition>),
    init-value: #f, init-keyword: method-of:;
  //
  // True if this method is congruent with the corresponding GF.
  slot method-defn-congruent? :: <boolean>,
    init-value: #f, init-keyword: congruent:;
end;

define class <accessor-method-definition> (<method-definition>)
  slot accessor-method-defn-slot-info :: false-or(<slot-info>),
    required-init-keyword: slot:;
end;

define class <getter-method-definition> (<accessor-method-definition>)
end;

define class <setter-method-definition> (<accessor-method-definition>)
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

define method print-message
    (tlf :: <define-generic-tlf>, stream :: <stream>) => ();
  format(stream, "Define Generic %s", tlf.tlf-defn.defn-name);
end;

define class <define-implicit-generic-tlf> (<simple-define-tlf>)
  //
  // Make the definition required.
  required keyword defn:;
end;

define method print-message
    (tlf :: <define-implicit-generic-tlf>, stream :: <stream>) => ();
  format(stream, "{Implicit} Define Generic %s", tlf.tlf-defn.defn-name);
end;

define class <seal-generic-tlf> (<top-level-form>)
  //
  // The name of the generic function.
  slot seal-generic-name :: <name>,
    required-init-keyword: name:;
  //
  // The generic defn for this seal generic.  Filled in at finalization time.
  slot seal-generic-defn :: false-or(<generic-definition>),
    init-value: #f;
  //
  // The library doing the seal.
  slot seal-generic-library :: <library>,
    required-init-keyword: library:;
  //
  // The type expressions.
  slot seal-generic-type-exprs :: <simple-object-vector>,
    required-init-keyword: type-exprs:;
  //
  // The types, once we have evaluated them.  #f until then.
  slot seal-generic-types :: false-or(<simple-object-vector>),
    init-value: #f;
end;

define method print-message
    (tlf :: <seal-generic-tlf>, stream :: <stream>) => ();
  format(stream, "Seal Generic %s (", tlf.seal-generic-name);
  for (type in tlf.seal-generic-type-exprs, first? = #t then #f)
    unless (first?)
      write(", ", stream);
    end;
    print-message(ct-eval(type, #f) | "???", stream);
  end;
  write(')', stream);
end;

define class <define-method-tlf> (<simple-define-tlf>)
  //
  // The name being defined.  Note: this isn't the name of the method, it is
  // the name of the generic function.
  slot method-tlf-base-name :: <name>, required-init-keyword: base-name:;
  //
  // The library doing the defining.
  slot method-tlf-library :: <library>, required-init-keyword: library:;
  //
  // True if the define method is sealed, false if open.
  slot method-tlf-sealed? :: <boolean>, required-init-keyword: sealed:;
  //
  // True if the define method was declared inline, #f if not.
  slot method-tlf-inline? :: <boolean>, required-init-keyword: inline:;
  //
  // True if we can drop calls to this function when the results isn't used
  // because there are no side effects.
  slot method-tlf-flushable? :: <boolean>,
    init-value: #f, init-keyword: flushable:;
  //
  // True if we can move calls to this function around with impunity because
  // the result depends on nothing but the value of the arguments.
  slot method-tlf-movable? :: <boolean>,
    init-value: #f, init-keyword: movable:;
  //
  // The guts of the method being defined.
  slot method-tlf-parse :: <method-parse>, required-init-keyword: parse:;
end;

define method print-message
    (tlf :: <define-method-tlf>, stream :: <stream>) => ();
  format(stream, "Define Method %s", tlf.tlf-defn.defn-name);
end;


// process-top-level-form

define method process-top-level-form (form :: <define-generic-parse>) => ();
  let name = form.defgen-name.token-symbol;
  let (open?, sealed?, movable?, flushable?)
    = extract-modifiers("define generic", name, form.define-modifiers,
			#"open", #"sealed", #"movable", #"flushable");
  if (open? & sealed?)
    compiler-error-location(
      form,
      "define generic %s can't be both open and sealed",
      name);
  end;
  extract-properties("define generic", form.defgen-plist);
  let defn = make(<generic-definition>,
		  name: make(<basic-name>,
			     symbol: name,
			     module: *Current-Module*),
		  source-location: form.source-location,
		  library: *Current-Library*,
		  sealed: ~open?,
		  movable: movable?,
		  flushable: flushable? | movable?);
  note-variable-definition(defn);
  let tlf = make(<define-generic-tlf>,
	         defn: defn,
		 source-location: defn.source-location,
		 param-list: form.defgen-param-list,
		 returns: form.defgen-returns);
  defn.function-defn-signature := curry(compute-define-generic-signature, tlf);
  add!(*Top-Level-Forms*, tlf);
end;

define method compute-define-generic-signature
    (tlf :: <define-generic-tlf>) => res :: <signature>;
  let (signature, anything-non-constant?)
    = compute-signature(tlf.generic-tlf-param-list, tlf.generic-tlf-returns);
  let defn = tlf.tlf-defn;
  if (anything-non-constant?)
    defn.function-defn-hairy? := #t;
    if (defn.function-defn-ct-value)
      error("noticed that a function was hairy after creating a ct-value.");
    end;
  elseif (defn.generic-defn-sealed?)
    // Fill in the slot so that add-seal's call to function-defn-signature
    // doesn't cause us to recurse forever.
    defn.function-defn-signature := signature;
    add-seal(defn, defn.defn-library, signature.specializers, tlf);
  end;
  signature;
end;

define method process-top-level-form (form :: <seal-generic-parse>) => ();
  add!(*Top-Level-Forms*,
       make(<seal-generic-tlf>,
	    name: make(<basic-name>,
		       symbol: form.sealgen-name.token-symbol,
		       module: *Current-Module*),
	    source-location: form.source-location,
	    library: *Current-Library*,
	    type-exprs: form.sealgen-type-exprs));
end;

define method process-top-level-form (form :: <define-method-parse>) => ();
  let name = form.defmethod-method.method-name.token-symbol;
  let (sealed?, inline?, movable?, flushable?)
    = extract-modifiers("define method", name, form.define-modifiers,
			#"sealed", #"inline", #"movable", #"flushable");
  let base-name = make(<basic-name>, symbol: name, module: *Current-Module*);
  let parse = form.defmethod-method;
  let params = parse.method-param-list;
  implicitly-define-generic(*Current-Library*, base-name,
			    params.paramlist-required-vars.size,
			    params.paramlist-rest & ~params.paramlist-keys,
			    params.paramlist-keys & #t);
  let tlf = make(<define-method-tlf>,
		 base-name: base-name, library: *Current-Library*,
		 source-location: form.source-location,
		 sealed: sealed?, inline: inline?, movable: movable?,
		 flushable: flushable? | movable?,
		 parse: parse);
  add!(*Top-Level-Forms*, tlf);
end;

define method implicitly-define-generic
    (library :: <library>, name :: <basic-name>, num-required :: <integer>,
     variable-args? :: <boolean>, keyword-args? :: <boolean>)
    => ();
  let var = find-variable(name);
  unless (var & var.variable-definition)
    let defn = make(<implicit-generic-definition>,
		    name: name, library: library);
    defn.function-defn-signature
      := method ()
	   let specs = make(<list>, size: num-required,
			    fill: object-ctype());
	   let sig = make(<signature>,
			  specializers: specs,
			  rest-type: variable-args? & object-ctype(),
			  keys: keyword-args? & #(),
			  all-keys: #f,
			  returns: wild-ctype());
	   defn.function-defn-signature := sig;
	   add-seal(defn, library, specs, #f);
	   sig;
	 end;
    note-variable-definition(defn);
    add!(*Top-Level-Forms*, make(<define-implicit-generic-tlf>, defn: defn));
  end;
end;


// finalize-top-level-form

define method finalize-top-level-form (tlf :: <define-generic-tlf>) => ();
  // Force the processing of the signature and ct-value.
  tlf.tlf-defn.ct-value;
end;

define method finalize-top-level-form (tlf :: <seal-generic-tlf>) => ();
  let var = find-variable(tlf.seal-generic-name);
  let defn = var & var.variable-definition;
  unless (instance?(defn, <generic-definition>))
    compiler-error-location(
      tlf,
      "%s doesn't name a generic function, so can't be sealed.",
      tlf.seal-generic-name);
  end;
  tlf.seal-generic-defn := defn;
  local method eval-type (type-expr :: <expression>)
	    => type :: <ctype>;
	  let type = ct-eval(type-expr, #f) | make(<unknown-ctype>);
	  unless (instance?(type, <ctype>))
	    compiler-error-location(
	      tlf,
	      "Parameter in seal generic of %s isn't a type:\n  %s",
	      tlf.seal-generic-name,
	      type);
	  end;
	  type;
	end method eval-type;
  let types = map(eval-type, tlf.seal-generic-type-exprs);
  tlf.seal-generic-types := types;
  add-seal(defn, tlf.seal-generic-library, types, tlf);
end;

define method finalize-top-level-form (tlf :: <define-implicit-generic-tlf>)
    => ();
  let defn = tlf.tlf-defn;
  let name = defn.defn-name;
  let var = find-variable(name);
  if (var & var.variable-definition == defn)
    // The implicit defn is still around.  Force the processing of the
    // signature and ct-value.
    defn.ct-value;
  else
    remove!(*Top-Level-Forms*, tlf);
  end;
end;

define method finalize-top-level-form (tlf :: <define-method-tlf>)
    => ();
  let name = tlf.method-tlf-base-name;
  let (signature, anything-non-constant?)
    = compute-signature(tlf.method-tlf-parse.method-param-list,
			tlf.method-tlf-parse.method-returns);
  let defn = make(<method-definition>,
		  base-name: name,
		  source-location: tlf.source-location,
		  library: tlf.method-tlf-library,
		  signature: signature,
		  hairy: anything-non-constant?,
		  movable: tlf.method-tlf-movable?,
		  flushable: tlf.method-tlf-flushable?,
		  inline-expansion: tlf.method-tlf-inline?
		    & ~anything-non-constant?
		    & tlf.method-tlf-parse);
  tlf.tlf-defn := defn;
  let gf = defn.method-defn-of;
  if (gf)
    ct-add-method(tlf, gf, defn);
  end;
  if (tlf.method-tlf-sealed?)
    if (gf)
      add-seal(gf, tlf.method-tlf-library, signature.specializers, tlf);
    else
      compiler-error-location(tlf, "%s doesn't name a generic function", name);
    end;
  end;
end;

define method make
    (class :: one-of(<method-definition>, <accessor-method-definition>,
		     <getter-method-definition>, <setter-method-definition>),
     #next next-method, #rest keys,
     #key base-name, signature, hairy: hairy?,
          movable: movable?, flushable: flushable?)
    => res :: <method-definition>;
  if (base-name)
    let var = find-variable(base-name);
    let generic-defn
      = if (var & instance?(var.variable-definition, <generic-definition>))
	  var.variable-definition;
	end;
    apply(next-method, class,
	  name: make(<method-name>,
		     generic-function: base-name,
		     specializers: signature.specializers),
	  hairy: hairy?,
	  movable: movable?
	    | (generic-defn & generic-defn.function-defn-movable?),
	  flushable: flushable?
	    | (generic-defn & generic-defn.function-defn-flushable?),
	  method-of: generic-defn,
	  keys);
  else
    next-method();
  end;
end;

// This method exists just so make will recognize base-name as a valid
// init keyword.
// 
define method initialize
    (defn :: <method-definition>, #next next-method, #key base-name) => ();
  next-method();
end;

define method compute-signature
    (param-list :: <parameter-list>, returns :: <parameter-list>)
    => (signature :: <signature>, anything-non-constant? :: <boolean>);
  let anything-non-constant? = #f;
  local
    method maybe-eval-type (param)
      let type = param.param-type;
      if (type)
	let ctype = ct-eval(type, #f);
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
      let type = maybe-eval-type(param);
      let default = if (param.param-default)
		      ct-eval(param.param-default, #f);
		    else
		      as(<ct-value>, #f);
		    end;
      let required? = ~(instance?(type, <unknown-ctype>)
			  | default == #f
			  | cinstance?(default, type));
      make(<key-info>, key-name: param.param-keyword, type: type,
	   default: default, required: required?);
    end;
  values(make(<signature>,
	      specializers: map-as(<list>, maybe-eval-type,
				   param-list.paramlist-required-vars),
	      next: param-list.paramlist-next & #t,
	      rest-type: param-list.paramlist-rest & object-ctype(),
	      keys: (param-list.paramlist-keys
		       & map-as(<list>, make-key-info,
				param-list.paramlist-keys)),
	      all-keys: param-list.paramlist-all-keys?,

	      returns:
	        make-values-ctype(map-as(<list>, maybe-eval-type,
			                 returns.paramlist-required-vars),
				  returns.paramlist-rest & object-ctype())),
	 anything-non-constant?);
end;


// Congruence testing.

// Check congruence at one position, returning #t if definitely congruent.
// Meth and GF are for error context.
//
define method check-1-arg-congruent
    (mspec :: <values-ctype>, gspec :: <values-ctype>,
     wot :: <byte-string>,
     meth :: <method-definition>, gf :: <generic-definition>)
    => res :: <boolean>;
  let (val, val-p) = values-subtype?(mspec, gspec);
  case
    ~val-p =>
      compiler-warning-location
	(meth,
	 "Can't tell if %s %s is a subtype of %s,\n"
	 "so can't tell if method:\n"
	 "  %s\n"
	 "is congruent to GF\n"
	 "  %s",
	 wot, mspec, gspec, meth.defn-name, gf.defn-name);
      #f;

    ~val =>
      compiler-warning-location
	(meth,
	 "Method \n  %s \n"
	 "isn't congruent to GF\n   %s \n"
	 "because method %s type %s isn't a subtype of GF type %s.",
	 meth.defn-name, gf.defn-name, wot, mspec, gspec);
      #f;

    otherwise => #t;
  end case;
end method;


// Check that the methods on GF are congruent to it, and return the methods
// that are congruent.
//
define method check-congruence
    (meth :: <method-definition>, gf :: <generic-definition>)
    => res :: <boolean>;
  let gsig = gf.function-defn-signature;
  let gspecs = gsig.specializers;
  let msig = meth.function-defn-signature;
  let win = #t;

  let mspecs = msig.specializers;
  unless (size(mspecs) = size(gspecs))
    compiler-warning-location
      (meth,
       "Method %s has different number of required arguments than GF %s.",
       meth.defn-name, gf.defn-name);
    win := #f;
  end;
  for (mspec in mspecs, gspec in gspecs)
    win := check-1-arg-congruent(mspec, gspec, "argument", meth, gf) & win;
  end for;

  case
    gsig.key-infos =>
      if (~msig.key-infos)
	compiler-warning-location
	  (gf,
	   "GF %s accepts keywords but method %s doesn't.",
	   gf.defn-name, meth.defn-name);
	win := #f;
      elseif (~msig.all-keys?)
	for (gkey in gsig.key-infos)
	  let gkey-name = gkey.key-name;
	  let gspec = gkey.key-type;
	  block (found-it)
	    for (mkey in msig.key-infos)
	      if (mkey.key-name == gkey-name)
		win := check-1-arg-congruent(mkey.key-type, gspec,
					     "keyword arg", meth, gf)
		         & win;
		found-it();
	      end;
	    end for;
	    
	    compiler-warning-location
	      (gf,
	       "GF %s mandatory keyword arg %= is not accepted by method %s.",
	       gf.defn-name, gkey-name, meth.defn-name);
	    win := #f;
	  end block;
	end for;
      end if;

    msig.key-infos =>
      compiler-warning-location
	(meth,
	 "Method %s accepts keywords but GF %s doesn't.",
	 meth.defn-name, gf.defn-name);
      win := #f;

    gsig.rest-type & ~msig.rest-type =>
      compiler-warning-location
	(meth,
	 "GF %s accepts variable arguments, but method %s doesn't.",
	 gf.defn-name, meth.defn-name);
      win := #f;

    ~gsig.rest-type & msig.rest-type =>
      compiler-warning-location
	(meth,
	 "Method %s accepts variable arguments, but GF %s doesn't.",
	 meth.defn-name, gf.defn-name);
      win := #f;
  end;

  win & check-1-arg-congruent(msig.returns, gsig.returns, "result", meth, gf);
end method;


define method ct-add-method
    (tlf :: false-or(<top-level-form>), gf :: <generic-definition>,
     meth :: <method-definition>)
    => ();
  if (gf.generic-defn-sealed?
	& gf.defn-library ~== meth.defn-library)
    compiler-warning-location
      (meth,
       "In %s: library %s can't define methods on sealed generic %s.",
       tlf | "something", meth.defn-library.library-name, gf.defn-name);
    meth.function-defn-hairy? := #t;
  else
    let old-methods = gf.generic-defn-methods;
    let meth-specs = meth.function-defn-signature.specializers;
    block (return)
      for (old-meth in old-methods)
	if (meth-specs = old-meth.function-defn-signature.specializers)
	  compiler-warning-location
	    (meth,
	     "%s is multiply defined -- ignoring extra definition.",
	     meth.defn-name);
	  return();
	end;
      end;
      gf.generic-defn-methods := pair(meth, old-methods);
    end;
    if (check-congruence(meth, gf))
      meth.method-defn-congruent? := #t;
      unless (empty?(gf.function-defn-transformers))
	install-transformers(meth, gf.function-defn-transformers);
      end unless;
    else
      meth.function-defn-hairy? := #t;
    end;
  end if;
end method ct-add-method;

define method install-transformers
    (gf :: <generic-definition>, transformers :: <list>, #next next-method)
    => ();
  next-method();
  for (meth in gf.generic-defn-methods)
    install-transformers(meth, transformers);
  end for;
end method install-transformers;

define method install-transformers
    (meth :: <method-definition>, transformers :: <list>, #next next-method)
    => ();
  let meth-specs = meth.function-defn-signature.specializers;
  let new-transformers
    = choose(method (transformer)
	       let specs = transformer.transformer-specializers;
	       specs == #f | specs = meth-specs;
	     end,
	     transformers);
  unless (empty?(new-transformers))
    next-method(meth, new-transformers);
  end unless;
end method install-transformers;


// CT-value

define method ct-value (defn :: <generic-definition>)
    => res :: false-or(<ct-function>);
  let ctv = defn.function-defn-ct-value;
  if (ctv == #"not-computed-yet")
    // We extract the sig first, because doing so way change the -hairy? flag.
    let sig = defn.function-defn-signature;
    defn.function-defn-ct-value
      := unless (defn.function-defn-hairy?)
	   make(<ct-generic-function>,
		name: format-to-string("%s", defn.defn-name),
		signature: sig,
		definition: defn,
		sealed?: defn.generic-defn-sealed?);
	 end;
  else
    ctv;
  end;
end;

define method ct-value (defn :: <abstract-method-definition>)
    => res :: false-or(<ct-function>);
  let ctv = defn.function-defn-ct-value;
  if (ctv == #"not-computed-yet")
    defn.function-defn-ct-value
      := unless (defn.function-defn-hairy?)
	   make(<ct-method>,
		name: format-to-string("%s", defn.defn-name),
		signature: defn.function-defn-signature,
		definition: defn,
		hidden: instance?(defn, <method-definition>));
	 end;
  else
    ctv;
  end;
end;

define method ct-value (defn :: <accessor-method-definition>)
    => res :: false-or(<ct-function>);
  let ctv = defn.function-defn-ct-value;
  if (ctv == #"not-computed-yet")
    defn.function-defn-ct-value
      := unless (defn.function-defn-hairy?)
	   make(<ct-accessor-method>,
		name: format-to-string("%s", defn.defn-name),
		signature: defn.function-defn-signature,
		definition: defn,
		hidden: #t,
		slot-info: defn.accessor-method-defn-slot-info);
	 end;
  else
    ctv;
  end;
end;


// Compile-top-level-form

define method convert-top-level-form
    (builder :: <fer-builder>, tlf :: <define-generic-tlf>) => ();
  convert-generic-definition(builder, tlf.tlf-defn);
end;

define method convert-top-level-form
    (builder :: <fer-builder>, tlf :: <define-implicit-generic-tlf>) => ();
  let defn = tlf.tlf-defn;
  let name = defn.defn-name;
  let var = find-variable(name);
  if (var & var.variable-definition == defn)
    convert-generic-definition(builder, tlf.tlf-defn);
  end;
end;

define method convert-generic-definition
    (builder :: <fer-builder>, defn :: <generic-definition>) => ();
  if (defn.function-defn-hairy?)
    let policy = $Default-Policy;
    let source = make(<source-location>);
    let args = make(<stretchy-vector>);
    // ### compute the args.
    let temp = make-local-var(builder, #"gf", object-ctype());
    build-assignment
      (builder, policy, source, temp,
       make-unknown-call
	 (builder, ref-dylan-defn(builder, policy, source, #"%make-gf"), #f,
	  as(<list>, args)));
    fer-convert-defn-set(builder, policy, source, defn, temp);
  elseif (defn.generic-defn-discriminator)
    make-discriminator(builder, defn);
  end;
end;  

define method convert-top-level-form
    (builder :: <fer-builder>, tlf :: <seal-generic-tlf>) => ();
end;

define method convert-top-level-form
    (builder :: <fer-builder>, tlf :: <define-method-tlf>) => ();
  let defn = tlf.tlf-defn;
  let lexenv = make(<lexenv>);
  let leaf = fer-convert-method(builder, tlf.method-tlf-parse,
				format-to-string("%s", defn.defn-name),
				ct-value(defn), #"global", lexenv, lexenv);
  if (defn.function-defn-hairy? 
	| defn.method-defn-of == #f
	| defn.method-defn-of.function-defn-hairy?)
    // We don't use method-defn-of, because that is #f if there is a definition
    // but it isn't a define generic.
    let gf-name = tlf.method-tlf-base-name;
    let gf-var = find-variable(gf-name);
    let gf-defn = gf-var & gf-var.variable-definition;
    if (gf-defn)
      let policy = $Default-Policy;
      let source = make(<source-location>);
      let gf-leaf = fer-convert-defn-ref(builder, policy, source, gf-defn);
      build-assignment
	(builder, policy, source, #(),
	 make-unknown-call
	   (builder,
	    ref-dylan-defn(builder, policy, source, #"add-method"), #f,
	    list(gf-leaf, leaf)));
    else
      compiler-error-location
        (tlf,
	 "In %s:\n  no definition for %=, and can't implicitly define it.",
	 tlf, gf-name);
    end;
  end;
end;



// Generic function discriminator functions.

define method generic-defn-discriminator (gf :: <generic-definition>)
    => res :: false-or(<ct-function>);
  if (gf.%generic-defn-discriminator == #"not-computed-yet")
    gf.%generic-defn-discriminator
      := if (discriminator-possible?(gf))
	   let sig = gf.function-defn-signature;
	   make(<ct-function>,
		name: format-to-string("Discriminator for %s", gf.defn-name),
		signature:
		  if (sig.key-infos)
		    make(<signature>,
			 specializers: sig.specializers,
			 rest-type: sig.rest-type | object-ctype(),
			 keys: #(), all-keys: #t,
			 returns: sig.returns);
		  else
		    make(<signature>,
			 specializers: sig.specializers,
			 rest-type: sig.rest-type,
			 returns: sig.returns);
		  end);
	 else
	   #f;
	 end;
  else
    gf.%generic-defn-discriminator;
  end;
end;

define method discriminator-possible? (gf :: <generic-definition>)
    => res :: <boolean>;
  if (gf.generic-defn-sealed? & ~gf.function-defn-hairy?
	& gf.generic-defn-methods.size > 1)
    block (return)
      for (meth in gf.generic-defn-methods)
	if (meth.function-defn-hairy?)
	  return(#f);
	end;
	for (method-spec in meth.function-defn-signature.specializers,
	     gf-spec in gf.function-defn-signature.specializers)
	  unless (method-spec == gf-spec
		    | (instance?(method-spec, <cclass>)
			 & method-spec.sealed?
			 & every?(method (subclass :: <cclass>)
				    subclass.abstract? | subclass.unique-id;
				  end,
				  method-spec.subclasses)))
	    return(#f);
	  end;
	end for;
      end for;
      #t;
    end block;
  end if;
end method discriminator-possible?;

define method make-discriminator
    (builder :: <fer-builder>, gf :: <generic-definition>) => ();
  let policy = $Default-Policy;
  let source = make(<source-location>);
  let discriminator = gf.generic-defn-discriminator;
  let name = discriminator.ct-function-name;
  let sig = discriminator.ct-function-signature;

  let vars = make(<stretchy-vector>);
  for (specializer in sig.specializers,
       index from 0)
    let var = make-local-var(builder,
			     as(<symbol>, format-to-string("arg%d", index)),
			     specializer);
    add!(vars, var);
  end;
  let nspecs = vars.size;

  assert(~sig.next?);
  let rest-var
    = if (sig.rest-type)
	let var = make-local-var(builder, #"rest",
				 specifier-type(#"<simple-object-vector>"));
	add!(vars, var);
	var;
      else
	#f;
      end;
  assert(sig.key-infos == #f | sig.key-infos == #());

  let region = build-function-body(builder, policy, source, #f, name,
				   as(<list>, vars), sig.returns, #t);
  let results = make-values-cluster(builder, #"results", sig.returns);
  build-discriminator-tree
    (builder, policy, source, as(<list>, vars), rest-var, results,
     as(<list>, make(<range>, from: 0, below: nspecs)),
     sort-methods-set(gf.generic-defn-methods,
		      make(<vector>, size: nspecs, fill: #f),
		      empty-ctype()),
     gf);
  build-return(builder, policy, source, region, results);
  end-body(builder);
  
  make-function-literal(builder, discriminator, #f, #"global", sig, region);
end;

define method build-discriminator-tree
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     arg-vars :: <list>, rest-var :: false-or(<abstract-variable>),
     results :: <abstract-variable>, remaining-discriminations :: <list>,
     method-set :: <method-set>, gf :: <generic-definition>)
    => ();
  if (empty?(method-set.all-methods))
    build-assignment
      (builder, policy, source, results,
       make-error-operation
	 (builder, policy, source, #"no-applicable-methods-error"));
  elseif (empty?(remaining-discriminations))
    let ordered = method-set.ordered-methods;
    let ordered-ctvs = map(ct-value, ordered.tail);
    assert(every?(identity, ordered-ctvs));
    let ambig-ctvs = map(ct-value, method-set.ambiguous-methods);
    assert(every?(identity, ambig-ctvs));
    if (~empty?(ordered))
      let func-leaf = fer-convert-defn-ref(builder, policy, source,
					   ordered.head);
      let ambig-lit = unless (empty?(ambig-ctvs))
			make(<literal-pair>,
			     head: make(<literal-list>,
					contents: ambig-ctvs,
					sharable: #t),
			     tail: make(<literal-empty-list>),
			     sharable: #t);
		      end;
      let next-leaf
	= make-literal-constant(builder,
				make(<literal-list>,
				     contents: ordered-ctvs,
				     tail: ambig-lit,
				     sharable: #t));
      if (rest-var)
	let sig = gf.function-defn-signature;
	if (sig.key-infos)
	  let valid-keys
	    = as(<ct-value>,
		 if (sig.all-keys?)
		   #"all";
		 else
		   block (return)
		     map(curry(as, <ct-value>),
			 reduce(method (keys :: <simple-object-vector>,
					meth :: <method-definition>)
				    => res :: <simple-object-vector>;
				  let sig = meth.function-defn-signature;
				  if (sig.all-keys?)
				    return(#"all");
				  end if;
				  union(keys, map(key-name, sig.key-infos));
				end method,
				#[],
				method-set.all-methods));
		   end block;
		 end if);
	  build-assignment
	    (builder, policy, source, #(),
	     make-unknown-call
	       (builder,
		ref-dylan-defn(builder, policy, source, #"verify-keywords"),
		#f,
		list(rest-var, make-literal-constant(builder, valid-keys))));
	end if;

	let apply-leaf = ref-dylan-defn(builder, policy, source, #"apply");
	let values-leaf = ref-dylan-defn(builder, policy, source, #"values");
	let cluster = make-values-cluster(builder, #"args", wild-ctype());
	build-assignment
	  (builder, policy, source, cluster,
	   make-unknown-call(builder, apply-leaf, #f,
			     pair(values-leaf, arg-vars)));
	build-assignment
	  (builder, policy, source, results,
	   make-operation(builder, <mv-call>,
			  list(func-leaf, next-leaf, cluster),
			  use-generic-entry: #t));
      else
	build-assignment(builder, policy, source, results,
			 make-unknown-call(builder, func-leaf, next-leaf,
					   arg-vars));
      end;
    elseif (~empty?(ambig-ctvs))
      build-assignment
	(builder, policy, source, results,
	 make-error-operation
	   (builder, policy, source, #"ambiguous-method-error",
	    make-literal-constant(builder,
				  make(<literal-list>,
				       contents: ambig-ctvs,
				       sharable: #t))));
    else
      error("Where did all the methods go?");
    end;
  else
    //
    // Figure out which of the remaining positions would be the best one to
    // specialize on.
    let discriminate-on
      = if (remaining-discriminations.tail == #())
	  remaining-discriminations.head;
	else
	  let discriminate-on = #f;
	  let max-distinct-specializers = 0;
	  for (posn in remaining-discriminations)
	    let distinct-specializers
	      = count-distinct-specializers(method-set.all-methods, posn);
	    if (distinct-specializers > max-distinct-specializers)
	      max-distinct-specializers := distinct-specializers;
	      discriminate-on := posn;
	    end;
	  end;
	  discriminate-on;
	end;
    let remaining-discriminations
      = remove(remaining-discriminations, discriminate-on);
    //
    // Divide up the methods based on that one argument.
    let ranges = discriminate-on-one-arg(discriminate-on, method-set, gf);
    //
    // Extract the unique id for this argument.
    let class-temp = make-local-var(builder, #"class", object-ctype());
    let obj-class-leaf
      = ref-dylan-defn(builder, policy, source, #"%object-class");
    build-assignment(builder, policy, source, class-temp,
		     make-unknown-call(builder, obj-class-leaf, #f,
				       list(arg-vars[discriminate-on])));
    let id-temp = make-local-var(builder, #"id", object-ctype());
    let unique-id-leaf
      = ref-dylan-defn(builder, policy, source, #"unique-id");
    build-assignment(builder, policy, source, id-temp,
		     make-unknown-call(builder, unique-id-leaf, #f,
				       list(class-temp)));
    let less-then = ref-dylan-defn(builder, policy, source, #"<");
    //
    // Recursivly build an if tree based on that division of the methods.
    local
      method split-range (min, max)
	if (min == max)
	  let method-set = ranges[min].third;
	  let arg = arg-vars[discriminate-on];
	  let temp = copy-variable(builder, arg);
	  build-assignment
	    (builder, policy, source, temp,
	     make-operation(builder, <truly-the>, list(arg),
			    guaranteed-type: method-set.restriction-type));
	  arg-vars[discriminate-on] := temp;
	  build-discriminator-tree
	    (builder, policy, source, arg-vars, rest-var, results,
	     remaining-discriminations, method-set, gf);
	  arg-vars[discriminate-on] := arg;
	else
	  let half-way-point = ash(min + max, -1);
	  let cond-temp = make-local-var(builder, #"cond", object-ctype());
	  let ctv = as(<ct-value>, ranges[half-way-point].second + 1);
	  let bound = make-literal-constant(builder, ctv);
	  build-assignment(builder, policy, source, cond-temp,
			   make-unknown-call(builder, less-then, #f,
					     list(id-temp, bound)));
	  build-if-body(builder, policy, source, cond-temp);
	  split-range(min, half-way-point);
	  build-else(builder, policy, source);
	  split-range(half-way-point + 1, max);
	  end-body(builder);
	end;
      end;
    split-range(0, ranges.size - 1);
  end;
end;


define method count-distinct-specializers
    (methods :: <list>, arg-posn :: <integer>)
    => count :: <integer>;
  let distinct-specializers = #();
  for (meth in methods)
    let specializer = meth.function-defn-signature.specializers[arg-posn];
    unless (member?(specializer, distinct-specializers))
      distinct-specializers := pair(specializer, distinct-specializers);
    end;
  end;
  distinct-specializers.size;
end;


define class <method-set> (<object>)
  slot arg-classes :: <simple-object-vector>,
    required-init-keyword: arg-classes:;
  slot ordered-methods :: <list>,
    required-init-keyword: ordered:;
  slot ambiguous-methods :: <list>,
    required-init-keyword: ambiguous:;
  slot all-methods :: <list>,
    required-init-keyword: all:;
  slot restriction-type :: <ctype>,
    required-init-keyword: restriction-type:;
end;

// = on <method-set>s
// 
// Two method sets are ``the same'' if they have the same methods, the same
// ordered methods (in the same order), and the same ambigous methods.
// 
define method \= (set1 :: <method-set>, set2 :: <method-set>)
    => res :: <boolean>;
  set1.ordered-methods = set2.ordered-methods
    & same-unordered?(set1.all-methods, set2.all-methods)
    & same-unordered?(set1.ambiguous-methods, set2.ambiguous-methods);
end;

// same-unordered?
//
// Return #t if the two lists have the same elements in any order.
// We assume that there are no duplicates in either list.
// 
define method same-unordered? (list1 :: <list>, list2 :: <list>)
    => res :: <boolean>;
  list1.size == list2.size
    & block (return)
	for (elem in list1)
	  unless (member?(elem, list2))
	    return(#f);
	  end;
	end;
	#t;
      end;
end;

define method discriminate-on-one-arg
    (discriminate-on :: <integer>, method-set :: <method-set>,
     gf :: <generic-definition>)
    => res :: <simple-object-vector>;
  //
  // For each method, associate it with all the direct classes for which that
  // method will be applicable.  Applicable is an object table mapping class
  // objects to sets of methods.  Actually, it maps to pairs where the head
  // is the class again and the tail is the set because portable dylan doesn't
  // include keyed-by.
  let applicable = make(<object-table>);
  let always-applicable = #();
  let gf-spec = gf.function-defn-signature.specializers[discriminate-on];
  for (meth in method-set.all-methods)
    let specializer
      = meth.function-defn-signature.specializers[discriminate-on];
    let direct-classes = find-direct-classes(specializer);
    if (direct-classes)
      for (direct-class in direct-classes)
	let entry = element(applicable, direct-class, default: #f);
	if (entry)
	  entry.tail := pair(meth, entry.tail);
	else
	  applicable[direct-class] := list(direct-class, meth);
	end;
      end;
    elseif (specializer == gf-spec)
      always-applicable := pair(meth, always-applicable);
    end;
  end;
  //
  // Grovel over the direct-class -> applicable-methods mapping producing
  // an equivalent mapping that has direct classes with consecutive unique
  // ids and equivalent method sets merged.
  //
  // Each entry in ranges is a vector of [min, max, method-set].  If max is
  // #f then that means unbounded.  We maintain the invariant that there are
  // no holes.
  //
  let ranges
    = begin
	let arg-classes = copy-sequence(method-set.arg-classes);
	arg-classes[discriminate-on] := gf-spec;
	let method-set = sort-methods-set(always-applicable, arg-classes,
					  gf-spec);
	let possible-direct-classes = find-direct-classes(gf-spec);
	if (possible-direct-classes)
	  for (direct-class in possible-direct-classes.tail,
	       min-id = possible-direct-classes.head.unique-id
		 then min(min-id, direct-class.unique-id),
	       max-id = possible-direct-classes.head.unique-id
		 then max(max-id, direct-class.unique-id))
	  finally
	    list(vector(min-id, max-id, method-set));
	  end;
	else
	  list(vector(0, #f, method-set));
	end;
      end;
  for (entry in applicable)
    let direct-class = entry.head;
    let arg-classes = copy-sequence(method-set.arg-classes);
    arg-classes[discriminate-on] := direct-class;
    let method-set
      = sort-methods-set(concatenate(entry.tail, always-applicable),
			 arg-classes, direct-class.direct-type);
    let this-id = direct-class.unique-id;
    for (remaining = ranges then remaining.tail,
	 prev = #f then remaining,
	 while: begin
		  let range :: <simple-object-vector> = remaining.head;
		  let max = range.second;
		  max & max < this-id;
		end)
    finally
      let range :: <simple-object-vector> = remaining.head;
      let other-set = range.third;
      if (method-set = other-set)
	other-set.restriction-type
	  := ctype-union(other-set.restriction-type,
			 method-set.restriction-type);
      else
	let min = range.first;
	let max = range.second;
	let new = if (this-id == max)
		    if (remaining.tail == #())
		      list(vector(this-id, this-id, method-set));
		    else
		      let next-range :: <simple-object-vector>
			= remaining.tail.head;
		      let next-set = next-range.third;
		      if (method-set = next-set)
			method-set.restriction-type
			  := ctype-union(method-set.restriction-type,
					 next-set.restriction-type);
			pair(vector(this-id, next-range.second, method-set),
			     remaining.tail.tail);
		      else
			pair(vector(this-id, this-id, method-set),
			     remaining.tail);
		      end;
		    end;
		  else
		    pair(vector(this-id, this-id, method-set),
			 pair(vector(this-id + 1, max, other-set),
			      remaining.tail));
		  end;
	if (this-id == min)
	  if (prev)
	    let prev-range :: <simple-object-vector> = prev.head;
	    let prev-set = prev-range.third;
	    if (method-set = prev-set)
	      prev-set.restriction-type
		:= ctype-union(prev-set.restriction-type,
			       method-set.restriction-type);
	      prev-range.second := new.head.second;
	      prev.tail := new.tail;
	    else
	      prev.tail := new;
	    end;
	  else
	    ranges := new;
	  end;
	else
	  range.second := this-id - 1;
	  remaining.tail := new;
	end;
      end;
    end;
  end;
  //
  // Convert ranges into a vector and return it.
  as(<simple-object-vector>, ranges);
end;
    

define method sort-methods-set
    (methods :: <list>, arg-classes :: <simple-object-vector>,
     restriction-type :: <ctype>)
    => res :: <method-set>;
  let (ordered, ambiguous) = sort-methods(methods, arg-classes);
  make(<method-set>, arg-classes: arg-classes, ordered: ordered,
       ambiguous: ambiguous, all: methods, restriction-type: restriction-type);
end;


// Seal processing.

define class <seal-info> (<object>)
  slot seal-types :: <list>, required-init-keyword: types:;
  slot seal-methods :: false-or(<list>), init-value: #f;
end class <seal-info>;


define method add-seal
    (defn :: <generic-definition>, library :: <library>, types :: <list>,
     tlf :: false-or(<top-level-form>))
    => ();
  block (return)
    let specs = defn.function-defn-signature.specializers;
    if (specs.size ~== types.size)
      compiler-warning-location
        (tlf, "Wrong number of types in seal, wanted %d but got %d",
	 specs.size, types.size);
      return();
    end if;
    let bogus? = #f;
    for (spec in specs, type in types, index from 0)
      if (instance?(type, <unknown-ctype>))
	compiler-warning
	  ("In %s: type for arg %d in seal is unknown, hence ignoring seal.",
	   tlf | "???", index);
	return();
      end;
      unless (instance?(spec, <unknown-ctype>)
		| csubtype?(type, spec))
	compiler-warning
	  ("In %s: bad type in seal: %s is not a subtype of gf type %s",
	   tlf | "???", type, spec);
	bogus? := #t;
      end;
    end;
    if (bogus?)
      return();
    end;
    let new-seals = list(make(<seal-info>, types: types));
    for (old-seal in defn.generic-defn-seals)
      select (compare-methods(types, old-seal.seal-types, #f))
	#"more-specific", #"unordered" =>
	  // The new seal is more specific than or the same as the old seals,
	  // so we can ignore the new seal.
	  return();
	#"less-specific" =>
	  // The new seal is less specific than the old one, so we can blow off
	  // the old one.
	  begin end;
	otherwise =>
	  // We need them both.
	  new-seals := pair(old-seal, new-seals);
      end select;
    end for;
    defn.generic-defn-seals := new-seals;
  end block;
end;

define method add-seal
    (defn :: <generic-definition>, library :: <library>, types :: <sequence>,
     tlf :: false-or(<top-level-form>))
    => ();
  add-seal(defn, library, as(<list>, types), tlf);
end;



// ct-applicable-methods

define method ct-applicable-methods
    (gf :: <generic-definition>, call-types :: <list>)
    => (definitely :: false-or(<list>), maybe :: false-or(<list>));
  let seal-info = find-seal(gf, call-types);
  if (seal-info)
    let definitely-applicable = #();
    let maybe-applicable = #();
    for (meth in seal-info.seal-methods | compute-seal-methods(seal-info, gf))
      select (compare-methods(meth, call-types, #f))
	#"disjoint" =>
	  begin end;
	#"less-specific", #"unordered" =>
	  definitely-applicable := pair(meth, definitely-applicable);
	otherwise =>
	  maybe-applicable := pair(meth, maybe-applicable);
      end select;
    end for;
    values(definitely-applicable, maybe-applicable);
  else
    // The argument types arn't covered by a seal.
    values(#f, #f);
  end if;
end method;


define method find-seal (gf :: <generic-definition>, call-types :: <list>)
    => res :: false-or(<seal-info>);
  block (return)
    for (seal-info in gf.generic-defn-seals)
      select (compare-methods(call-types, seal-info.seal-types, #f))
	#"more-specific", #"unordered" => return(seal-info);
	otherwise => begin end;
      end select;
    end for;
    #f;
  end block;
end method find-seal;


define method compute-seal-methods
    (seal-info :: <seal-info>, gf :: <generic-definition>)
    => methods :: <list>;
  let types = seal-info.seal-types;
  seal-info.seal-methods
    := choose(method (meth :: <method-definition>)
		  => res :: <boolean>;
		if (meth.method-defn-congruent?)
		  compare-methods(meth, types, #f) ~== #"disjoint";
		end;
	      end method,
	      gf.generic-defn-methods)
end method compute-seal-methods;


// method sorting and comparision utilities.

// sort-methods
//
// This routine takes a set of methods and sorts them by some subset of the
// arguments.
// 
define method sort-methods
    (methods :: <list>, arg-classes :: false-or(<simple-object-vector>))
    => (ordered :: false-or(<list>), ambiguous :: false-or(<list>));

  block (return)

    // Ordered accumulates the methods we can tell the ordering of.  Each
    // element in this list is either a method or a list of equivalent methods.
    let ordered = #();

    // Ambiguous accumulates the set of methods of which it is unclear which
    // follows next after ordered.  These methods will all be mutually
    // ambiguous or equivalent.
    let ambiguous = #();

    for (meth in methods)
      block (done-with-method)
	for (remaining = ordered then remaining.tail,
	     prev = #f then remaining,
	     until: remaining == #())
	  //
	  // Grab the method to compare this method against.  If the next
	  // element in ordered is a list of equivalent methods, grab the first
	  // one as characteristic.
	  let other
	    = if (instance?(remaining.head, <pair>))
		remaining.head.head;
	      else
		remaining.head;
	      end;
	  select (compare-methods(meth, other, arg-classes))
	    //
	    // Our method is more specific, so insert it in the list of ordered
	    // methods and go on to the next method.
	    #"more-specific" =>
	      if (prev)
		prev.tail := pair(meth, remaining);
	      else
		ordered := pair(meth, remaining);
	      end;
	      done-with-method();
	    #"less-specific" =>
	      //
	      // Our method is less specific, so we can't do anything at this
	      // time.
	      #f;
	    #"unordered" =>
	      //
	      // Our method is equivalent.  Add it to the set of equivalent
	      // methods, making such a set if necessary.
	      if (instance?(remaining.head, <pair>))
		remaining.head := pair(meth, remaining.head);
	      else
		remaining.head := list(meth, remaining.head);
	      end;
	      done-with-method();
	    #"ambiguous" =>
	      //
	      // We know that the other method is more specific than anything
	      // in the current ambiguous set, so throw it away making a new
	      // ambiguous set.  Taking into account that we might have a set
	      // of equivalent methods on our hands.
	      remaining.tail := #();
	      if (instance?(remaining.head, <pair>))
		ambiguous := pair(meth, remaining.head);
	      else
		ambiguous := list(meth, remaining.head);
	      end;
	      done-with-method();
	    #"unknown" =>
	      compiler-warning-location
	        (meth,
		 "Can't statically determine the ordering of %s "
		 "and %s and both are applicable.",
		 meth.defn-name, other.defn-name);
	      return(#f, #f);
	  end;
	finally
	  //
	  // Our method was less specific than any method in the ordered list.
	  // This either means that our method needs to be tacked onto the end
	  // of the ordered list, added to the ambiguous list, or ignored.
	  // Compare the method against all the methods in the ambiguous list
	  // to figure out which.
	  let ambiguous-with = #();
	  for (remaining = ambiguous then remaining.tail,
	       until: remaining == #())
	    select (compare-methods(meth, remaining.head, arg-classes))
	      #"more-specific" =>
		#f;
	      #"less-specific" =>
		done-with-method();
	      #"unordered" =>
		ambiguous := pair(meth, ambiguous);
		done-with-method();
	      #"ambiguous" =>
		ambiguous-with := pair(remaining.head, ambiguous-with);
	      #"unknown" =>
		compiler-warning-location
		  (meth,
		   "Can't statically determine the ordering of "
		   "%s and %s and both are applicable.",
		   meth.defn-name, remaining.head.defn-name);
		return(#f, #f);
	    end;
	  end;
	  //
	  // Ambiguous-with is only #() if we are more specific than anything
	  // currently in the ambigous set.  So tack us onto the end of the
	  // ordered set.  Otherwise, set the ambigous set to us and everything
	  // we are ambiguous with.
	  if (ambiguous-with == #())
	    if (prev)
	      prev.tail := list(meth);
	    else
	      ordered := list(meth);
	    end;
	  else
	    ambiguous := pair(meth, ambiguous-with);
	  end;
	end;
      end;
    end;

    values(ordered, ambiguous);
  end;
end method sort-methods;


define method compare-methods
    (meth1 :: <method-definition>, meth2 :: <method-definition>,
     arg-classes :: false-or(<simple-object-vector>))
    => res :: one-of(#"more-specific", #"less-specific", #"unordered",
		     #"disjoint", #"ambiguous", #"unknown");
  compare-methods(meth1.function-defn-signature.specializers, 
		  meth2.function-defn-signature.specializers,
		  arg-classes);
end;

define method compare-methods
    (meth1 :: <method-definition>, specs2 :: <list>,
     arg-classes :: false-or(<simple-object-vector>))
    => res :: one-of(#"more-specific", #"less-specific", #"unordered",
		     #"disjoint", #"ambiguous", #"unknown");
  compare-methods(meth1.function-defn-signature.specializers, specs2,
		  arg-classes);
end;

define method compare-methods
    (specs1 :: <list>, specs2 :: <list>,
     arg-classes :: false-or(<simple-object-vector>))
    => res :: one-of(#"more-specific", #"less-specific", #"unordered",
		     #"disjoint", #"ambiguous", #"unknown");
  block (return)
    let result = #"unordered";
    for (index from 0,
	 spec1 in specs1,
	 spec2 in specs2)
      let arg-class = arg-classes & arg-classes[index];
      //
      // If this is an argument that we are actually sorting by,
      if ((arg-classes == #f | arg-class) & spec1 ~== spec2)
	//
	// If the two specializers are the same, then this argument offers no
	// ordering.
	let this-one
	  = if (csubtype?(spec1, spec2))
	      #"more-specific";
	    elseif (csubtype?(spec2, spec1))
	      #"less-specific";
	    elseif (~ctypes-intersect?(spec1, spec2))
	      return(#"disjoint");
	    elseif (instance?(spec1, <cclass>) & instance?(spec2, <cclass>))
	      // Neither argument is a subclass of the other.  So we have to
	      // base it on the precedence list of the actual argument class.
	      if (arg-classes)
		let cpl = arg-class.precedence-list;
		block (found)
		  for (super in cpl)
		    if (super == spec1)
		      found(#"more-specific");
		    elseif (super == spec2)
		      found(#"less-specific");
		    end;
		  finally
		    error("%= isn't applicable", arg-class);
		  end;
		end;
	      else
		return(#"unknown");
	      end if;
	    elseif (instance?(spec1, <unknown-ctype>)
		      | instance?(spec2, <unknown-ctype>))
	      return(#"unknown");
	    else
	      // Neither argument is a subtype of the other and we have a
	      // non-class specializers.  That's ambiguous, folks.
	      return(#"ambiguous");
	    end;
	unless (result == this-one)
	  if (result == #"unordered")
	    result := this-one;
	  else
	    return(#"ambiguous");
	  end;
	end;
      end;
    end;
    result;
  end;
end;



// Dumping stuff.

define method dump-od
    (tlf :: <define-method-tlf>, state :: <dump-state>) => ();
  let defn = tlf.tlf-defn;
  let gf = defn.method-defn-of;
  if (gf & name-inherited-or-exported?(defn.defn-name))
    dump-od(defn, state);
    if (tlf.method-tlf-sealed? & defn.defn-library ~== gf.defn-library)
      dump-simple-object(#"seal-generic", state,
			 gf,
			 defn.defn-library,
			 defn.function-defn-signature.specializers);
    end if;
  end if;
end method dump-od;

define method dump-od
    (tlf :: <seal-generic-tlf>, state :: <dump-state>) => ();
  if (tlf.seal-generic-defn.defn-library ~== tlf.seal-generic-library)
    assert(name-inherited-or-exported?(tlf.seal-generic-name));
    dump-simple-object(#"seal-generic", state,
		       tlf.seal-generic-defn,
		       tlf.seal-generic-library,
		       tlf.seal-generic-types);
  end if;
end method dump-od;

add-od-loader(*compiler-dispatcher*, #"seal-generic",
	      method (state :: <load-state>) => res :: <false>;
		let gf = load-object-dispatch(state);
		let library = load-object-dispatch(state);
		let types = load-object-dispatch(state);
		assert-end-object(state);
		add-seal(gf, library, types, #f);
	      end method);


define constant $function-definition-slots
  = concatenate($definition-slots,
		list(function-defn-signature, signature:,
		       function-defn-signature-setter,
		     function-defn-hairy?, hairy:, #f,
		     function-defn-ct-value, #f, function-defn-ct-value-setter,
		     function-defn-flushable?, flushable:, #f,
		     function-defn-movable?, movable:, #f));

define constant $generic-definition-slots
  = concatenate($function-definition-slots,
		list(generic-defn-sealed?, sealed:, #f,
		     generic-defn-seals, seals:, #f,
		     generic-defn-discriminator, discriminator:,
		       %generic-defn-discriminator-setter));

// List of method definitions that we saw in a GF and that we need to make sure
// are dumped.
//
define variable *queued-methods* = #();

// Make sure that all of the method definitions get dumped too.  If some of
// them came from elsewhere it doesn't matter, we'll just have a gratuitous
// external reference.
// 
define constant dump-them-methods = method
    (x :: <generic-definition>, buf :: <dump-buffer>)
 => ();
  ignore(buf);
  for (meth in x.generic-defn-methods)
    *queued-methods* := pair(meth, *queued-methods*);
  end;
// dformat("Queuing %=\n", x);
end method;

define /* exported */ method dump-queued-methods (buf :: <dump-buffer>)
  // We clear the queue before dumping the methods in case dumping some
  // method triggers the addition of other methods to the queue.  But we
  // flame out if that happened, because I'm not sure that should be supported.
  // Basically, I noticed that the way this code was written before it would
  // quietly ignore any additional methods, so I figured it would be an
  // improvement to at least puke if that happens.  If it doens't puke, well,
  // then there isn't any need for anything fancier.  If it does puke, then
  // we can easily change it again.  -William
  let methods = *queued-methods*;
  *queued-methods* := #();
  for (meth in methods)
    dump-od(meth, buf);
  end;
  assert(*queued-methods* == #());
end;


add-make-dumper(#"generic-definition", *compiler-dispatcher*,
		<generic-definition>, $generic-definition-slots,
		load-external: #t,
		dump-side-effect: dump-them-methods);

add-make-dumper(#"implicit-generic-definition", *compiler-dispatcher*,
		<implicit-generic-definition>, $generic-definition-slots,
		load-external: #t,
		dump-side-effect: dump-them-methods);

add-make-dumper(#"seal-info", *compiler-dispatcher*, <seal-info>,
		list(seal-types, types:, #f));

define constant $abstract-method-definition-slots
  = concatenate($function-definition-slots,
		list(method-defn-inline-function, inline-function:,
		       %method-defn-inline-function-setter));

define method set-method-defn-of
    (gf :: false-or(<generic-definition>), meth :: <method-definition>) => ();
  meth.method-defn-of := gf;
  if (gf)
    ct-add-method(#f, gf, meth);
  end;
end;
/*
define constant hackola = method (x, y)
  dformat("Dumping defn for method on %=\n",
  	  x.defn-name.method-name-generic-function.name-symbol);
end method;
*/
define constant $method-definition-slots
  = concatenate($abstract-method-definition-slots,
		list(method-defn-of, #f, set-method-defn-of,
		       method-defn-congruent?, congruent:, #f));

add-make-dumper(#"method-definition", *compiler-dispatcher*,
		<method-definition>, $method-definition-slots,
		load-external: #t);

define constant $accessor-method-definition-slots
  = concatenate($method-definition-slots,
		list(accessor-method-defn-slot-info, slot:,
		       accessor-method-defn-slot-info-setter));

add-make-dumper(#"getter-method-definition", *compiler-dispatcher*,
		<getter-method-definition>, $accessor-method-definition-slots,
		load-external: #t);

add-make-dumper(#"setter-method-definition", *compiler-dispatcher*,
		<setter-method-definition>, $accessor-method-definition-slots,
		load-external: #t);
