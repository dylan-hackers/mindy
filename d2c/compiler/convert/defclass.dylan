module: define-classes
rcs-header: $Header: /scm/cvs/src/d2c/compiler/convert/defclass.dylan,v 1.41 2002/09/04 21:26:49 housel Exp $
copyright: see below



//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000, 2001, 2002  Gwydion Dylan Maintainers
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

// Parse tree stuff.

define class <define-class-parse> (<definition-parse>)
  constant slot defclass-name :: <identifier-token>,
    required-init-keyword: name:;
  constant slot defclass-superclass-exprs :: <simple-object-vector>,
    required-init-keyword: superclass-exprs:;
  constant slot defclass-slots :: <simple-object-vector>,
    required-init-keyword: slots:;
  constant slot defclass-options :: <simple-object-vector>,
    required-init-keyword: options:;
end class <define-class-parse>;

define-procedural-expander
  (#"make-define-class",
   method (generator :: <expansion-generator>, name-frag :: <fragment>,
	   supers-frag :: <fragment>, slots-frag :: <fragment>,
	   options-frag :: <fragment>)
       => ();
     generate-fragment
       (generator,
	make-parsed-fragment
	  (make(<define-class-parse>,
		name: extract-name(name-frag),
		source-location: generator.generator-call.source-location,
		superclass-exprs: map(expression-from-fragment,
				      split-fragment-at-commas(supers-frag)),
		slots: map(extract-slot, split-fragment-at-commas(slots-frag)),
		options: parse-property-list(make(<fragment-tokenizer>,
						  fragment: options-frag))),
	   source-location: generate-token-source-location(generator)));
   end method);

define method extract-slot (frag :: <fragment>)
    => res :: <abstract-slot-parse>;
  if (instance?(frag, <token-fragment>)
	& frag.fragment-token.token-kind == $error-token
	& instance?(frag.fragment-token, <pre-parsed-token>)
	& instance?(frag.fragment-token.token-parse-tree,
		    <abstract-slot-parse>))
    frag.fragment-token.token-parse-tree;
  else
    error("bug in define class macro: %= isn't a slot parse", frag);
  end if;
end method extract-slot;


define abstract class <abstract-slot-parse> (<object>)
end class <abstract-slot-parse>;

define class <slot-parse> (<abstract-slot-parse>, <source-location-mixin>)
  constant slot slot-parse-name :: <identifier-token>,
    required-init-keyword: name:;
  constant slot slot-parse-options :: <simple-object-vector>,
    required-init-keyword: options:;
end class <slot-parse>;

define-procedural-expander
  (#"make-slot",
   method (generator :: <expansion-generator>, name-frag :: <fragment>,
	   options-frag :: <fragment>)
       => ();
     generate-fragment
       (generator,
	make-magic-fragment
	  (make(<slot-parse>,
		name: extract-name(name-frag),
		source-location: generator.generator-call.source-location,
		options: parse-property-list(make(<fragment-tokenizer>,
						  fragment: options-frag))),
	   source-location: generate-token-source-location(generator)))
   end method);

define class <inherited-slot-parse> (<abstract-slot-parse>)
  constant slot inherited-slot-parse-name :: <identifier-token>,
    required-init-keyword: name:;
  constant slot inherited-slot-parse-options :: <simple-object-vector>,
    required-init-keyword: options:;
end class <inherited-slot-parse>;

define-procedural-expander
  (#"make-inherited-slot",
   method (generator :: <expansion-generator>, name-frag :: <fragment>,
	   options-frag :: <fragment>)
       => ();
     generate-fragment
       (generator,
	make-magic-fragment
	  (make(<inherited-slot-parse>,
		name: extract-name(name-frag),
		options: parse-property-list(make(<fragment-tokenizer>,
						  fragment: options-frag))),
	   source-location: generate-token-source-location(generator)))
   end method);

define class <init-arg-parse> (<abstract-slot-parse>)
  constant slot init-arg-parse-keyword :: <symbol>,
    required-init-keyword: keyword:;
  constant slot init-arg-parse-options :: <simple-object-vector>,
    required-init-keyword: options:;
end class <init-arg-parse>;

define-procedural-expander
  (#"make-init-arg",
   method (generator :: <expansion-generator>, keyword-frag :: <fragment>,
	   options-frag :: <fragment>)
       => ();
     let keyword
       = if (instance?(keyword-frag, <token-fragment>)
               & keyword-frag.fragment-token.token-kind == $symbol-token)
           keyword-frag.fragment-token.token-literal.literal-value;
         else
           compiler-fatal-error-location
             (keyword-frag,
              "Argument to keyword (%=) must be a keyword", keyword-frag);
         end;
     generate-fragment
       (generator,
	make-magic-fragment
	  (make(<init-arg-parse>,
		keyword: keyword,
		options: parse-property-list(make(<fragment-tokenizer>,
						  fragment: options-frag))),
	   source-location: generate-token-source-location(generator)))
   end method);

define method extract-keyword (frag :: <fragment>)
    => keyword :: <symbol>;
  if (instance?(frag, <token-fragment>)
	& frag.fragment-token.token-kind == $symbol-token)
    frag.fragment-token.token-literal.literal-value;
  else
    error("Bug in define class macro: %= isn't a keyword.", frag);
  end if;
end method extract-keyword;


// 

define class <real-class-definition> (<class-definition>)
  //
  // The <cclass> for this class definition, #f if unknown (e.g. non-constant
  // superclasses), #"not-computed-yet" if we haven't computed it yet, or
  // #"computing" if we are actively working on it.
  slot class-defn-cclass
    :: type-union(<cclass>, one-of(#f, #"not-computed-yet", #"computing")),
    init-value: #"not-computed-yet", init-keyword: class:;
  //
  // Deferred evaluations function, of #f if there isn't one.
  slot %class-defn-deferred-evaluations-function
    :: type-union(<ct-function>, one-of(#f, #"not-computed-yet")),
    init-value: #"not-computed-yet";
  //
  // Keyword defaulter function, of #f if there isn't one.
  slot %class-defn-key-defaulter-function
    :: type-union(<ct-function>, one-of(#f, #"not-computed-yet")),
    init-value: #"not-computed-yet";
  //
  // The maker function, of #f if there isn't one.
  slot %class-defn-maker-function
    :: type-union(<ct-function>, one-of(#f, #"not-computed-yet")),
    init-value: #"not-computed-yet";
end;

define method defn-type (defn :: <real-class-definition>) => res :: <cclass>;
  class-ctype();
end;

define class <local-class-definition> (<real-class-definition>)
  // 
  // Vector of <expression-parse>s for the superclasses.
  constant slot class-defn-supers :: <simple-object-vector>,
    required-init-keyword: supers:;
  //
  // Several boolean flags, just what the names say.
  constant slot class-defn-functional? :: <boolean>,
    required-init-keyword: functional:;
  constant slot class-defn-sealed? :: <boolean>,
    required-init-keyword: sealed:;
  slot class-defn-abstract? :: <boolean>,
    required-init-keyword: abstract:;
  constant slot class-defn-primary? :: <boolean>,
    required-init-keyword: primary:;
  //
  // Vector of the slots.
  constant slot class-defn-slots :: <simple-object-vector>,
    required-init-keyword: slots:;
  //
  // Vector of slot init value overrides.
  constant slot class-defn-overrides :: <simple-object-vector>,
    required-init-keyword: overrides:;
  //
  // Vector of init keyword arguments
  constant slot class-defn-keywords :: <simple-object-vector>,
    required-init-keyword: keywords:;
end;  

define /* exported */ class <slot-defn> (<source-location-mixin>)
  //
  // The class that introduces this slot.
  slot slot-defn-class :: <real-class-definition>;
  //
  // #t if this slot is sealed, #f if not.  This really means that the getter
  // generic function is sealed on this class and the setter (if any) is sealed
  // on object and this class.
  constant slot slot-defn-sealed? :: <boolean>,
    required-init-keyword: sealed:;
  //
  // The allocation of this slot.
  constant slot slot-defn-allocation :: <slot-allocation>,
    required-init-keyword: allocation:;
  //
  // The expression to compute the type.
  constant slot slot-defn-type :: false-or(<expression-parse>),
    required-init-keyword: type:;
  //
  // The name of the getter generic function.
  constant slot slot-defn-getter-name :: <name>,
    required-init-keyword: getter-name:;
  //
  // The getter method.  Filled in when computed.
  slot slot-defn-getter :: <getter-method-definition>;
  //
  // The name of the setter generic function, or #f if there is no setter.
  constant slot slot-defn-setter-name :: false-or(<name>),
    required-init-keyword: setter-name:;
  //
  // The setter method.  Filled in when computed.
  slot slot-defn-setter :: false-or(<setter-method-definition>);
  //
  // The init-value expression, or #f if one wasn't supplied.
  constant slot slot-defn-init-value :: false-or(<expression-parse>),
    init-value: #f, init-keyword: init-value:;
  //
  // The init-function, or #f if there isn't one.
  constant slot slot-defn-init-function :: false-or(<expression-parse>),
    init-value: #f, init-keyword: init-function:;
  //
  // The init-keyword, or #f if there isn't one.
  constant slot slot-defn-init-keyword :: false-or(<symbol>),
    init-value: #f, init-keyword: init-keyword:;
  //
  // #t if the init-keyword is required, #f if not.
  constant slot slot-defn-init-keyword-required? :: <boolean>,
    init-value: #f, init-keyword: init-keyword-required:;
  //
  // The sizer slot defn.
  constant slot slot-defn-sizer-defn :: false-or(<slot-defn>),
    init-value: #f, init-keyword: sizer-defn:;

  // The slot-info for this slot, or #f if we haven't computed it or don't know
  // enough about the class to compute it at all.
  slot slot-defn-info :: false-or(<slot-info>),
    init-value: #f;
end;

define class <override-defn> (<object>)
  //
  // The class that introduces this override.
  slot override-defn-class :: <real-class-definition>;
  //
  // The name of the getter.
  constant slot override-defn-getter-name :: <name>,
    required-init-keyword: getter-name:;
  //
  // The init-value expression, or #f if none.
  constant slot override-defn-init-value :: false-or(<expression-parse>),
    init-value: #f, init-keyword: init-value:;
  //
  // The init-function expression, or #f if none.
  constant slot override-defn-init-function :: false-or(<expression-parse>),
    init-value: #f, init-keyword: init-function:;
  //
  // The <override-info> for this override, or #f if we haven't computed it
  // or don't know enough about the class to compute it at all.
  slot override-defn-info :: false-or(<override-info>),
    init-value: #f;
end;

define class <keyword-defn> (<object>)
  //
  // The class that introduces this keyword.
  slot keyword-defn-class :: <real-class-definition>;
  //
  // The keyword
  constant slot keyword-defn-symbol :: <symbol>,
    required-init-keyword: symbol:;
  //
  // The init-value expression, or #f if none.
  constant slot keyword-defn-init-value :: false-or(<expression-parse>),
    init-value: #f, init-keyword: init-value:;
  //
  // The init-function expression, or #f if none.
  constant slot keyword-defn-init-function :: false-or(<expression-parse>),
    init-value: #f, init-keyword: init-function:;
  //
  // Is this keyword required?
  constant slot keyword-defn-required? :: <boolean>, 
    init-value: #f, init-keyword: required?:;
  // 
  // The type restriction for this keyword, if any.
  constant slot keyword-defn-type :: false-or(<expression-parse>),
    init-value: #f, init-keyword: type:;
  //
  // The <keyword-info> for this keyword, or #f if we haven't computed it
  // or don't know enough about the class to compute it at all.
  slot keyword-defn-info :: false-or(<keyword-info>),
    init-value: #f;
end;


define class <maker-function-definition> (<abstract-method-definition>)
  slot maker-func-defn-class-defn :: <class-definition>,
    init-keyword: class-defn:;
end class <maker-function-definition>;


define class <init-function-definition> (<abstract-method-definition>)
  constant slot init-func-defn-method-parse :: false-or(<method-parse>),
    init-value: #f, init-keyword: method-parse:;
end class <init-function-definition>;


// Top level form processing.

// During top level form processing, we parse the define class form
// and build the necessary <local-class-definition>, <slot-defn>, and
// <override-defn> objects.  We only check for syntactic errors and
// local semantic errors.  By local semantic errors, I mean errors
// that can be detected by looking at nothing more than this class
// itself.
//
// We also note the class definition and any implicit definitions for slot
// accessors.

define method process-top-level-form (form :: <define-class-parse>) => ();
  let name = form.defclass-name.token-symbol;
  let (class-functional?-frag, class-sealed?-frag, class-primary?-frag,
       class-abstract?-frag)
    = extract-properties(form.defclass-options, #"functional", #"sealed",
			 #"primary", #"abstract");
  let class-functional?
    = class-functional?-frag & extract-boolean(class-functional?-frag);
  let class-sealed?
    = ~class-sealed?-frag | extract-boolean(class-sealed?-frag);
  let class-primary?
    = class-primary?-frag & extract-boolean(class-primary?-frag);
  let class-abstract?
    = class-abstract?-frag & extract-boolean(class-abstract?-frag);
  let slots = make(<stretchy-vector>);
  let overrides = make(<stretchy-vector>);
  let keywords = make(<stretchy-vector>);
  unless (class-abstract? | empty?(form.defclass-superclass-exprs))
    add!(overrides,
	 make(<override-defn>,
	      getter-name: make(<basic-name>, symbol: #"%object-class",
				module: $Dylan-Module),
	      init-value: make(<varref-parse>, id: form.defclass-name)));
  end;
  for (option in form.defclass-slots)
    block ()
      process-slot(name, class-functional?, slots, overrides, keywords, option);
    exception (<fatal-error-recovery-restart>)
      #f;
    end block;
  end for;
  let slots = as(<simple-object-vector>, slots);
  let overrides = as(<simple-object-vector>, overrides);
  let keywords = as(<simple-object-vector>, keywords);
  let defn = make(<local-class-definition>,
		  name: make(<basic-name>,
			     symbol: name,
			     module: *Current-Module*),
		  source-location: form.source-location,
		  library: *Current-Library*,
		  supers: form.defclass-superclass-exprs,
		  functional: class-functional?,
		  sealed: class-sealed?,
		  primary: class-primary?,
		  abstract: class-abstract?,
		  slots: slots,
		  overrides: overrides,
		  keywords: keywords);
  for (slot in slots)
    slot.slot-defn-class := defn;
    //
    // Implicity define the accessor generics.
    if (slot.slot-defn-sizer-defn)
      implicitly-define-generic
	(*Current-Library*, slot.slot-defn-getter-name, 2, #f, #f);
      if (slot.slot-defn-setter-name)
	implicitly-define-generic
	  (*Current-Library*, slot.slot-defn-setter-name, 3, #f, #f);
      end;
    else
      implicitly-define-generic
	(*Current-Library*, slot.slot-defn-getter-name, 1, #f, #f);
      if (slot.slot-defn-setter-name)
	implicitly-define-generic
	  (*Current-Library*, slot.slot-defn-setter-name, 2, #f, #f);
      end;
    end;
  end;
  for (override in overrides)
    override.override-defn-class := defn;
  end for;
  for (keyword in keywords)
    keyword.keyword-defn-class := defn;
  end for;
  note-variable-definition(defn);
  add!(*Top-Level-Forms*, make(<define-class-tlf>, defn: defn));
end method process-top-level-form;


define generic process-slot
    (class-name :: <symbol>, class-functional? :: <boolean>,
     slots :: <stretchy-vector>, overrides :: <stretchy-vector>, keywords :: <stretchy-vector>,
     slot :: <abstract-slot-parse>)
    => ();

define method process-slot
    (class-name :: <symbol>, class-functional? :: <boolean>,
     slots :: <stretchy-vector>, overrides :: <stretchy-vector>, keywords :: <stretchy-vector>,
     slot :: <slot-parse>)
    => ();
  let (sealed?-frag, allocation-frag, type-frag, setter-frag,
       init-keyword-frag, req-init-keyword-frag, init-value-frag,
       init-expr-frag, init-function-frag, sizer-frag,
       size-init-keyword-frag, req-size-init-keyword-frag,
       size-init-value-frag, size-init-function-frag)
    = extract-properties(slot.slot-parse-options,
			 sealed:, allocation:, type:, setter:,
			 init-keyword:, required-init-keyword:,
			 init-value:, init-expr:, init-function:,
			 sizer:, size-init-keyword:,
			 required-size-init-keyword:,
			 size-init-value:, size-init-function:);
  let getter = slot.slot-parse-name.token-symbol;
  let sealed? = sealed?-frag & extract-boolean(sealed?-frag);
  let allocation = if (allocation-frag)
		     extract-identifier(allocation-frag).token-symbol;
		   else
		     #"instance";
		   end;
  let type = type-frag & expression-from-fragment(type-frag);
  let setter = if (class-functional? & allocation == #"instance")
		 let id
		   = setter-frag & extract-identifier-or-false(setter-frag);
		 if (id)
		   compiler-warning-location
		     (id,
		      "Instance allocation slots in functional classes can't"
			" have a setter.");
		 end;
		 #f;
	       elseif (setter-frag)
		 let id = extract-identifier-or-false(setter-frag);
		 id & id.token-symbol;
	       else
		 symcat(getter, "-setter");
	       end;
  let init-keyword = init-keyword-frag & extract-keyword(init-keyword-frag);
  let req-init-keyword
    = req-init-keyword-frag & extract-keyword(req-init-keyword-frag);
  let init-value
    = init-value-frag & expression-from-fragment(init-value-frag);
  let init-expr = init-expr-frag & expression-from-fragment(init-expr-frag);
  let init-function
    = init-function-frag & expression-from-fragment(init-function-frag);
  let sizer = sizer-frag & extract-identifier(sizer-frag).token-symbol;
  let size-init-keyword
    = size-init-keyword-frag & extract-keyword(size-init-keyword-frag);
  let req-size-init-keyword
    = (req-size-init-keyword-frag
	 & extract-keyword(req-size-init-keyword-frag));
  let size-init-value
    = size-init-value-frag & expression-from-fragment(size-init-value-frag);
  let size-init-function
    = (size-init-function-frag
	 & expression-from-fragment(size-init-function-frag));

  if (init-value)
    if (init-expr)
      compiler-fatal-error-location
	(init-value,
	 "Can't supply both an init-value: and an init-expression.");
    end if;
    if (init-function)
      compiler-fatal-error-location
	(init-value,
	 "Can't supply both an init-value: and an init-function:.");
    end;
    if (req-init-keyword)
      compiler-fatal-error-location
	(init-value,
	 "Can't supply both an init-value: and a required-init-keyword:.");
    end;
  elseif (init-expr)
    if (init-function)
      compiler-fatal-error-location
	(init-expr,
	 "Can't supply both an init-function: and an init-expression.");
    end if;
    if (req-init-keyword)
      compiler-fatal-error-location
	(init-expr,
	 "Can't supply both an init-value: and a required-init-keyword:.");
    end;
    if (instance?(init-expr, <literal-ref-parse>))
      init-value := init-expr;
    else
      init-function
	:= make(<method-ref-parse>,
		method: make(<method-parse>,
			     parameters: make(<parameter-list>, fixed: #[]),
			     body: init-expr));
    end if;
  elseif (init-function)
    if (req-init-keyword)
      compiler-fatal-error-location
	(init-function,
	 "Can't supply both an init-function: and a "
	   "required-init-keyword:.");
    end;
  end;
  if (init-keyword & req-init-keyword)
    compiler-fatal-error-location
      (simplify-source-location(init-keyword-frag.source-location),
       "Can't supply both an init-keyword: and a required-init-keyword:.");
  end;

  let getter-name = make(<basic-name>, symbol: getter,
			 module: *Current-Module*);
  let setter-name = setter & make(<basic-name>, symbol: setter,
				  module: *Current-Module*);

  let size-defn
    = if (sizer)
	let sizer-name
	  = make(<basic-name>, symbol: sizer, module: *Current-Module*);
	
	unless (allocation == #"instance")
	  compiler-fatal-error-location
	    (simplify-source-location(sizer-frag.source-location),
	     "Only instance allocation slots can be variable length, but "
	       "%s has %s allocation",
	     getter, allocation);
	end;
	
	if (size-init-value)
	  if (size-init-function)
	    compiler-fatal-error-location
	      (size-init-value,
	       "Can't have both a size-init-value: and size-init-function:");
	  end;
	elseif (~(size-init-function | req-size-init-keyword))
	  compiler-fatal-error
	    ("The Initial size for vector slot %s must be supplied somehow.",
	     getter);
	end;
	
	if (size-init-keyword & req-size-init-keyword)
	  compiler-fatal-error-location
	    (simplify-source-location
	       (size-init-keyword-frag.source-location),
	     "Can't have both a size-init-keyword: and a "
	       "required-size-init-keyword:");
	end;
	
	let slot = make(<slot-defn>,
			sealed: sealed?,
			allocation: #"instance",
			type:
			  make(<varref-parse>,
			       id: make(<identifier-token>,
					kind: $raw-ordinary-word-token,
					symbol: #"<integer>",
					module: $Dylan-Module,
					uniquifier: make(<uniquifier>))),
			getter-name: sizer-name,
			setter-name: #f,
			source-location: slot.source-location,
			init-value: size-init-value,
			init-function: size-init-function,
			init-keyword:
			  size-init-keyword | req-size-init-keyword,
			init-keyword-required:
			  req-size-init-keyword & #t);
	add!(slots, slot);
	slot;
      else
	if (size-init-value)
	  compiler-fatal-error-location
	    (size-init-value,
	     "Can't supply a size-init-value: without a sizer: generic "
	       "function");
	end;
	if (size-init-function)
	  compiler-fatal-error-location
	    (size-init-function,
	     "Can't supply a size-init-function: without a "
	       "sizer: generic function");
	end;
	if (size-init-keyword)
	  compiler-fatal-error-location
	    (simplify-source-location
	       (size-init-keyword-frag.source-location),
	     "Can't supply a size-init-keyword: without a "
	       "sizer: generic function");
	end;
	if (req-size-init-keyword)
	  compiler-fatal-error-location
	    (simplify-source-location
	       (req-size-init-keyword-frag.source-location),
	     "Can't supply a required-size-init-keyword: "
	       "without a sizer: generic function");
	end;

	#f;
      end;
  
  let slot = make(<slot-defn>,
		  sealed: sealed? & #t,
		  allocation: allocation,
		  type: type,
		  getter-name: getter-name,
		  setter-name: setter-name,
		  source-location: slot.source-location,
		  init-value: init-value,
		  init-function: init-function,
		  init-keyword: init-keyword | req-init-keyword,
		  sizer-defn: size-defn,
		  init-keyword-required: req-init-keyword & #t);
  add!(slots, slot);
end method process-slot;

define method process-slot
    (class-name :: <symbol>, class-functional? :: <boolean>,
     slots :: <stretchy-vector>, overrides :: <stretchy-vector>,
     keywords :: <stretchy-vector>, slot :: <inherited-slot-parse>)
    => ();
  let (init-value-frag, init-expr-frag, init-function-frag)
    = extract-properties(slot.inherited-slot-parse-options,
			 init-value:, init-expr:, init-function:);

  let init-value = init-value-frag & expression-from-fragment(init-value-frag);
  let init-expr = init-expr-frag & expression-from-fragment(init-expr-frag);
  let init-function
    = init-function-frag & expression-from-fragment(init-function-frag);

  if (init-value)
    if (init-expr)
      compiler-fatal-error-location
	(init-expr,
	 "Can't supply both an init-value: and an init-expression.");
    end if;
    if (init-function)
      compiler-fatal-error-location
	(init-function,
	 "Can't supply both an init-value: and an init-function:.");
    end;
  elseif (init-expr)
    if (init-function)
      compiler-fatal-error-location
	(init-function,
	 "Can't supply both an init-function: and an init-expression.");
    end if;
    if (instance?(init-expr, <literal-ref-parse>))
      init-value := init-expr;
    else
      init-function
	:= make(<method-ref-parse>,
		method: make(<method-parse>,
			     parameters: make(<parameter-list>, fixed: #[]),
			     body: init-expr));
    end if;
  end;

  add!(overrides,
       make(<override-defn>,
	    getter-name:
	      make(<basic-name>,
		   symbol: slot.inherited-slot-parse-name.token-symbol,
		   module: *Current-Module*),
	    init-value: init-value,
	    init-function: init-function));
end method process-slot;

define method process-slot
    (class-name :: <symbol>, class-functional? :: <boolean>,
     slots :: <stretchy-vector>, overrides :: <stretchy-vector>,
     keywords :: <stretchy-vector>, slot :: <init-arg-parse>)
 => ();
  let (required?-frag, type-frag, init-value-frag, init-function-frag, init-expr-frag)
    = extract-properties(slot.init-arg-parse-options,
			 required:, type:, init-value:, init-function:, init-expr:);
  
  let required? = required?-frag & extract-boolean(required?-frag);
  let type = type-frag & expression-from-fragment(type-frag);
  let init-value = init-value-frag & expression-from-fragment(init-value-frag);
  let init-function
    = init-function-frag & expression-from-fragment(init-function-frag);
  let init-expr
    = init-expr-frag & expression-from-fragment(init-expr-frag);

  if (init-value)
    if (init-expr)
      compiler-fatal-error-location
	(init-value,
	 "Can't supply both an init-value: and an init-expression.");
    end if;
    if (init-function)
      compiler-fatal-error-location
	(init-value,
	 "Can't supply both an init-value: and an init-function:.");
    end;
    if (required?)
      compiler-fatal-error-location
	(init-value,
	 "Can't supply both an init-value: and a required-init-keyword:.");
    end;
  elseif (init-expr)
    if (init-function)
      compiler-fatal-error-location
	(init-expr,
	 "Can't supply both an init-function: and an init-expression.");
    end if;
    if (required?)
      compiler-fatal-error-location
	(init-expr,
	 "Can't supply both an init-value: and a required-init-keyword:.");
    end;
    if (instance?(init-expr, <literal-ref-parse>))
      init-value := init-expr;
    else
      init-function
	:= make(<method-ref-parse>,
		method: make(<method-parse>,
			     parameters: make(<parameter-list>, fixed: #[]),
			     body: init-expr));
    end if;
  elseif (init-function)
    if (required?)
      compiler-fatal-error-location
	(init-function,
	 "Can't supply both an init-function: and a "
	   "required-init-keyword:.");
    end;
  end;

  add!(keywords,
       make(<keyword-defn>,
	    symbol: slot.init-arg-parse-keyword,
	    required?: required?,
	    type: type,
	    init-value: init-value,
	    init-function: init-function));
end method process-slot;



// CT-Value

// Compute the compile-time value for a class definition.  This is the
// <cclass> object.  If we can't compute that for some reason, return #f
// to indicate that this class doesn't have a compile-time value.

define method ct-value (defn :: <real-class-definition>)
    => res :: false-or(<cclass>);
  select (defn.class-defn-cclass)
    #"not-computed-yet" =>
      defn.class-defn-cclass := #"computing";
      let (class, init-args) = compute-cclass(defn);
      if(class)
        defn.class-defn-cclass := apply(make, class, init-args);
      end if;
    #"computing" =>
      compiler-error-location
	(defn,
	 "class %s circularly defined.",
	 defn.defn-name.name-symbol);
      #f;
    otherwise =>
      defn.class-defn-cclass;
  end;
end;

define method compute-cclass (defn :: <real-class-definition>)
    => (cclass-class :: false-or(<class>), init-args :: <sequence>);
  //
  // Evaluate the superclasses, and check them for validity.
  let super-exprs = defn.class-defn-supers;
  let nsupers = super-exprs.size;
  let supers = make(<simple-object-vector>, size: nsupers);
  let closest-super = #f;
  let closest-primary = #f;
  let bogus? = #f;
  for (index from 0 below nsupers)
    let super-expr = super-exprs[index];
    let super = ct-eval(super-expr, #f);
    if (instance?(super, <cclass>))
      //
      // Store the superclass.
      supers[index] := super;
      //
      // Make sure we aren't trying to inherit from a sealed class.
      if (super.sealed? & super.loaded?)
	compiler-error-location
	  (super-expr.source-location,
	   "%s can't inherit from %s because %s is sealed.",
	   defn.defn-name, super, super);
	bogus? := #t;
      end if;
      //
      // Check that everything is okay with the abstract adjective.
      if (defn.class-defn-abstract? & ~super.abstract?)
	compiler-warning-location
	  (super-expr.source-location,
	   "abstract class %s can't inherit from %s because "
	     "%s is concrete -- ignoring abstract abjective.",
	   defn.defn-name, super, super);
	defn.class-defn-abstract? := #f;
      end if;
      //
      // Check that everything is okay with the functional adjective.
      if (defn.class-defn-functional?)
	//
	// Make sure we aren't trying to inherit from anything we can't.
	if (super.not-functional?)
	  compiler-error-location
	    (super-expr.source-location,
	     "functional class %s can't inherit from %s "
	       "because %s %s and is not functional.",
	     defn.defn-name, super, super,
	     if (super.abstract?)
	       "has instance slots";
	     else
	       "is concrete";
	     end if);
	  bogus? := #t;
	end if;
      else
	//
	// It isn't a functional class, so make sure we aren't trying to
	// inherit from a functional class.
	if (super.functional?)
	  compiler-error-location
	    (super-expr.source-location,
	     "class %s can't inherit from %s because %s is functional.",
	     defn.defn-name, super, super);
	  bogus? := #t;
	end if;
      end if;
      //
      // Check to see if this superclass's closest-primary-superclass
      // is closer than any of the others so far.
      let other-primary = super.closest-primary-superclass;
      if (~closest-primary | csubtype?(other-primary, closest-primary))
	closest-super := super;
	closest-primary := other-primary;
      elseif (~csubtype?(closest-primary, other-primary))
	local method describe (primary, super)
		if (primary == super)
		  as(<string>, primary.cclass-name.name-symbol);
		else
		  format-to-string("%s (inherited via %s)",
				   primary.cclass-name.name-symbol,
				   super.cclass-name.name-symbol);
		end;
	      end;
	compiler-error-location
	  (super-expr.source-location,
	   "%s can't inherit from %s and %s because they are both primary "
	     "and neither is a subclass of the other.",
	   defn.defn-name,
	   describe(closest-primary, closest-super),
	   describe(other-primary, super));
	bogus? := #t;
      end if;
    else
      //
      // The superclass isn't a <class>.  So complain.
      if (super)
	compiler-error-location
	  (super-expr.source-location,
	   "%s superclass of %s is not a class: %s.",
	   integer-to-english(index + 1, as: #"ordinal"),
	   defn.defn-name, super);
      else
	compiler-warning-location
	  (super-expr.source-location,
	   "%s superclass of %s is not obviously a constant.",
	   integer-to-english(index + 1, as: #"ordinal"),
	   defn.defn-name);
      end if;
      bogus? := #t;
    end if;
  end for;

  if (defn == dylan-defn(#"<object>"))
    unless (nsupers.zero?)
      error("<object> has superclasses?");
    end unless;
  else
    if (nsupers.zero?)
      compiler-error-location
	(defn, "%s has no superclasses.", defn.defn-name);
      bogus? := #t;
    elseif (closest-primary == #f & ~bogus?)
      error("<object> isn't being inherited or isn't primary?");
    end if;
  end if;

  if(bogus?)
    values(#f, #());
  else
    //
    // Compute the slots and overrides.
    let slot-infos = map(compute-slot, defn.class-defn-slots);
    let override-infos = map(compute-override, defn.class-defn-overrides);
    let keyword-infos = map(compute-keyword, defn.class-defn-keywords);
    // Filter out class allocated slots
    //
    let class-slot? = rcurry(instance?, <class-slot-info>);
    let class-slot-infos = choose(class-slot?, slot-infos);
    // Filter out each-subclass allocated slots
    //
    let each-subclass-slot? = rcurry(instance?, <each-subclass-slot-info>);
    let each-subclass-slot-infos = choose(each-subclass-slot?, slot-infos);
    //
    // Superclasses that have indirect slots
    let superclasses-with-meta = choose(class-metaclass, supers);
    local method each-subclass-slots? (super :: <defined-cclass>)
	   => each-subclass-slots? :: <boolean>;
	    any?(each-subclass-slot?, super.new-slot-infos)
	      | any?(each-subclass-slots?, super.direct-superclasses)
	  end;
    // Superclasses that have each-subclass slots
    let superclasses-with-esc-meta = choose(each-subclass-slots?,
					    superclasses-with-meta);
    let superclasses-with-esc-meta = superclasses-with-esc-meta.remove-duplicates!;
    local method extract-esc-metas(super-with-esc :: <defined-cclass>)
	      => esc-metas :: <list>;
	    let meta = super-with-esc.class-metaclass;
	    if (any?(class-slot?, super-with-esc.new-slot-infos))
	      // Peel off metaclass contributing the class slots only
	      meta.direct-superclasses
	    else
	      meta.list
	    end;
	  end;
    // Gather all metaclasses that contribute esc-slots
    let esc-metas = reduce(concatenate, #(), map(extract-esc-metas, superclasses-with-esc-meta));
    let esc-metas = esc-metas.remove-duplicates!;
// needed??    let esc-metas = remove!(esc-metas, class-ctype());
    
    let metaclass
      = unless (class-slot-infos.empty?
		  & each-subclass-slot-infos.empty?
		  & esc-metas.empty?)

	// now that we got here, we have to handle 7 cases.
	// these are detailed in the following table and illustrate
	// how the CPL for the metaclass should look like (i.e.
	// how the contributing metaclasses should nest)
	
	// class-slots	esc-slots  inherited-esc-slots => CPL
	// 1.	NO	NO		YES		esc(inh)	// where "esc" won't contribute
	// 1a.	NO	NO		YES (1 meta)	inh1		// we could reuse it! (not yet implemented)
	// 2.	NO	YES		NO		esc(<class>)
	// 3.	NO	YES		YES		esc(inh)
	// 4.	YES	NO		NO		cls(<class>)
	// 5.	YES	NO		YES		cls(inh)
	// 6.	YES	YES		NO		cls(esc(<class>))
	// 7.	YES	YES		YES		cls(esc(inh))

	  local method associate(info :: <indirect-slot-info>)
		 => associated :: <meta-slot-info>;
		  info.associated-meta-slot
		    := make(<meta-slot-info>,
			    referred: info,
			    getter: #f,
			    read-only: info.slot-read-only?,
			    init-keyword: info.slot-init-keyword,
			    init-keyword-required: info.slot-init-keyword-required?);
		end method;

	  let associated-infos = map-as(<simple-object-vector>, associate, class-slot-infos);
	  let associated-esc-infos = map-as(<simple-object-vector>, associate, each-subclass-slot-infos);

	  let super-metas = if (esc-metas.empty?) class-ctype().list else esc-metas end;

	  let meta-supers :: <list>
	    = if (~class-slot-infos.empty?
		    & each-subclass-slot-infos.empty?)
		// class-meta will be a wrapper below...
		// cases 4. and 5.
		super-metas
	      else
		// we have introduced each-subclass slots
		list(make(<meta-cclass>,
			  loading: #f,
			  name: make(<derived-name>,
				     how: #"each-subclass-meta",
				     base: defn.defn-name),
			  direct-superclasses: super-metas,
			  not-functional: #t,
			  functional: #f,
			  sealed: #t,
			  abstract: #t,
			  primary: #f,
			  slots: associated-esc-infos,
			  metaclass: #f))
	      end;

	  if (class-slot-infos.empty?)
	    assert(meta-supers.size == 1);
	    // cases 1., 2. and 3.
	    meta-supers.first
	  else
	    make(<meta-cclass>,
		 loading: #f,
		 name: make(<derived-name>,
			    how: #"class-meta",
			    base: defn.defn-name),
		 direct-superclasses: meta-supers,
		 not-functional: #t,
		 functional: #f,
		 sealed: #t,
		 abstract: #t,
		 primary: #t,
		 slots: associated-infos,
		 metaclass: #f)
	  end if
	end unless;
    //
    // Class and init arguments for constructing the class
    values(<defined-cclass>,
           list(loading: #f,
                name: defn.defn-name,
                defn: defn,
                direct-superclasses: as(<list>, supers),
                not-functional:
                  // Do we proclude functional subclasses?
                  if (defn.class-defn-functional?)
                    #f;
                  elseif (defn.class-defn-abstract?)
                    ~supers.empty?
                      & (any?(not-functional?, supers)
                           | any?(inhibits-functional-classes?, slot-infos));
                  else
                    #t;
                  end,
                functional: defn.class-defn-functional?,
                sealed: defn.class-defn-sealed?,
                primary: defn.class-defn-primary?,
                abstract: defn.class-defn-abstract?,
                slots: slot-infos,
                overrides: override-infos,
                keywords: keyword-infos,
                metaclass: metaclass));
  end if;
end method compute-cclass;

define method compute-slot (slot :: <slot-defn>) => info :: <slot-info>;
  let getter-name = slot.slot-defn-getter-name;
  //
  // Note: we don't pass in anything for the type, init-value, or
  // init-function, because we need to compile-time-eval those, which we
  // can't do until tlf-finalization time.
  let info
    = if (slot.slot-defn-sizer-defn)
	make(<vector-slot-info>,
	     getter: find-variable(getter-name, create: #t),
	     read-only: slot.slot-defn-setter-name == #f,
	     init-value: slot.slot-defn-init-value & #t,
	     init-function: slot.slot-defn-init-function & #t,
	     init-keyword: slot.slot-defn-init-keyword,
	     init-keyword-required:
	       slot.slot-defn-init-keyword-required?,
	     size-slot: slot.slot-defn-sizer-defn.slot-defn-info);
      else
	make(<slot-info>,
	     allocation: slot.slot-defn-allocation,
	     getter: find-variable(getter-name, create: #t),
	     read-only: slot.slot-defn-setter-name == #f,
	     init-value: slot.slot-defn-init-value & #t,
	     init-function: slot.slot-defn-init-function & #t,
	     init-keyword: slot.slot-defn-init-keyword,
	     init-keyword-required:
	       slot.slot-defn-init-keyword-required?);
      end;
  slot.slot-defn-info := info;
  info;
end;

define method compute-override
    (override :: <override-defn>) => info :: <override-info>;
  let getter-name = override.override-defn-getter-name;
  //
  // Note: we don't pass in anything for the init-value or init-function,
  // because we need to compile-time-eval those, which we can't do until
  // tlf-finalization time.
  let info = make(<override-info>,
		  getter: find-variable(getter-name, create: #t),
		  init-value: override.override-defn-init-value & #t,
		  init-function: override.override-defn-init-function & #t);
  override.override-defn-info := info;
  info;
end;

define method compute-keyword
    (keyword :: <keyword-defn>) => info :: <keyword-info>;
  //
  // Note: we don't pass in anything for the type, init-value, or
  // init-function, because we need to compile-time-eval those, which we
  // can't do until tlf-finalization time.
  let info = make(<keyword-info>,
		  symbol: keyword.keyword-defn-symbol,
		  required?: keyword.keyword-defn-required?,
		  init-value: keyword.keyword-defn-init-value & #t,
		  init-function: keyword.keyword-defn-init-function & #t);
  keyword.keyword-defn-info := info;
  info;
end;

define generic inhibits-functional-classes?
    (slot :: <slot-info>) => res :: <boolean>;

define method inhibits-functional-classes?
    (slot :: <slot-info>) => res :: <boolean>;
  #f;
end method inhibits-functional-classes?;

define method inhibits-functional-classes?
    (slot :: <instance-slot-info>) => res :: <boolean>;
  #t;
end method inhibits-functional-classes?;


// Top level form finalization.

define method finalize-top-level-form (tlf :: <define-class-tlf>) => ();
  let defn = tlf.tlf-defn;
  //
  // Compute the cclass if it hasn't been computed yet.
  let cclass :: false-or(<cclass>)
    = if (defn.class-defn-cclass == #"not-computed-yet")
        defn.class-defn-cclass := #"computing";
        let (class, init-args) = compute-cclass(defn);
        if(class)
          defn.class-defn-cclass := apply(make, class, init-args);
        end if;
      else
	defn.class-defn-cclass;
      end;
  let class-type = cclass | make(<unknown-ctype>);

  // Finalize the slots.
  for (slot in defn.class-defn-slots)
    finalize-slot(slot, cclass, class-type, tlf);
  end;

  // Finalize the overrides.
  for (override in defn.class-defn-overrides)
    // Fill in the <override-info> with the init value.
    let info = override.override-defn-info;
    if (info)
      if (override.override-defn-init-function)
	let (ctv, change-to-init-value?)
	  = maybe-define-init-function(override.override-defn-init-function, 
				       override.override-defn-getter-name,
				       tlf);
	if (ctv)
	  if (change-to-init-value?)
	    info.override-init-function := #f;
	    info.override-init-value := ctv;
	  else
	    info.override-init-function := ctv;
	  end if;
	end if;
      elseif (override.override-defn-init-value)
	let init-val = ct-eval(override.override-defn-init-value, #f);
	if (init-val)
	  info.override-init-value := init-val;
	end;
      end;
    end;
  end;

 // Finalize the initialization argument keywords too.
  for (keyword in defn.class-defn-keywords)
    // Fill in the <keyword-info> with the init value.
    let info = keyword.keyword-defn-info;
    if (info)
      let keyword-type
        = if (keyword.keyword-defn-type)
            let type = ct-eval(keyword.keyword-defn-type, #f);
            if (instance?(type, <ctype>))
              type;
            else
              make(<unknown-ctype>);
            end;
          else
            object-ctype();
          end;
      info.keyword-type := keyword-type;

      if (keyword.keyword-defn-init-function)
	let (ctv, change-to-init-value?)
	  = maybe-define-init-function(keyword.keyword-defn-init-function, 
                                       #f,
				       tlf);
	if (ctv)
	  if (change-to-init-value?)
	    info.keyword-init-function := #f;
	    info.keyword-init-value := ctv;
	  else
	    info.keyword-init-function := ctv;
	  end if;
	end if;
      elseif (keyword.keyword-defn-init-value)
	let init-val = ct-eval(keyword.keyword-defn-init-value, #f);
	if (init-val)
	  info.keyword-init-value := init-val;
	end;
      end;
    end;
  end;
end;

define method finalize-slot
    (slot :: <slot-defn>, cclass :: false-or(<cclass>), class-type :: <ctype>,
     tlf :: <define-class-tlf>)
    => ();
  //
  // Compute the type of the slot.
  let slot-type
    = if (slot.slot-defn-type)
	let type = ct-eval(slot.slot-defn-type, #f);
	if (instance?(type, <ctype>))
	  type;
	else
	  make(<unknown-ctype>);
	end;
      else
	object-ctype();
      end;

  let specializers
    = if (slot.slot-defn-sizer-defn)
	list(class-type, specifier-type(#"<integer>"));
      else
	list(class-type);
      end;

  // Fill in the <slot-info> with the type, init value, and init-function.
  let info = slot.slot-defn-info;
  if (info)
    info.slot-type := slot-type;

    if (slot.slot-defn-init-function)
      let (ctv, change-to-init-value?)
	= maybe-define-init-function(slot.slot-defn-init-function,
				     slot.slot-defn-getter-name,
				     tlf);
      if (ctv)
	if (change-to-init-value?)
	  info.slot-init-function := #f;
	  info.slot-init-value := ctv;
	else
	  info.slot-init-function := ctv;
	end if;
      end if;
    elseif (slot.slot-defn-init-value)
      let init-val = ct-eval(slot.slot-defn-init-value, #f);
      if (init-val)
	info.slot-init-value := init-val;
      end if;
    end if;
  end if;

  // Fill in the type for meta slot too.
  if (instance?(info, <indirect-slot-info>))
    let meta :: <meta-slot-info> = info.associated-meta-slot;
    meta.slot-type := slot-type;
/* not yet    
    // We can also reuse non-sideeffecting init-values.
    // All other cases should be handled via deferred-evaluations.
    if (info.slot-init-value ~== #t & info.slot-init-value)
      meta.slot-init-value := info.slot-init-value;
    end if;
*/
  end if;

  // Define the accessor methods.
  unless (slot.slot-defn-allocation == #"virtual")
    //
    // Extract the library from the class definition.
    let library = tlf.tlf-defn.defn-library;
    //
    // Are the accessor methods hairy?
    let hairy? = ~cclass | instance?(slot-type, <unknown-ctype>);
    //
    slot.slot-defn-getter
      := make(<getter-method-definition>,
	      base-name: slot.slot-defn-getter-name,
	      library: library,
	      signature: make(<signature>,
			      specializers: specializers,
			      returns: slot-type),
	      hairy: hairy?,
	      slot: info);
    let gf = slot.slot-defn-getter.method-defn-of;
    if (gf)
      ct-add-method(gf, slot.slot-defn-getter);
    end;
    if (slot.slot-defn-sealed?)
      if (gf)
	add-seal(gf, library, specializers, tlf);
      else
	compiler-error
	  ("%s doesn't name a generic function, so can't be sealed.",
	   slot.slot-defn-getter-name);
      end;
    end;
    slot.slot-defn-setter
      := if (slot.slot-defn-setter-name)
	   let defn = make(<setter-method-definition>,
			   base-name: slot.slot-defn-setter-name,
			   library: library,
			   signature: make(<signature>,
					   specializers:
					     pair(slot-type, specializers),
					   returns: slot-type),
			   hairy: hairy?,
			   slot: info);
	   let gf = defn.method-defn-of;
	   if (gf)
	     ct-add-method(gf, defn);
	   end;
	   if (slot.slot-defn-sealed?)
	     if (gf)
	       add-seal(gf, library, pair(slot-type, specializers), tlf);
	     else
	       compiler-error
		 ("%s doesn't name a generic function, so can't be sealed.",
		  slot.slot-defn-setter-name);
	     end;
	   end;
	   defn;
	 else
	   #f;
	 end;
  end unless;
end method finalize-slot;


define method maybe-define-init-function
    (expr :: <expression-parse>, getter-name :: false-or(<basic-name>),
     tlf :: <define-class-tlf>)
    => (ctv :: false-or(<ct-value>), change-to-init-value? :: <boolean>);
  let init-val = ct-eval(expr, #f);
  if (init-val)
    if (cinstance?(init-val, function-ctype()))
      values(init-val, #f);
    else
      compiler-error-location(expr, "Invalid init-function: %s.", init-val);
      values(#f, #f);
    end if;
  else
    let method-ref = expand-until-method-ref(expr);
    if (method-ref)
      let method-parse = method-ref.method-ref-method;
      let (signature, anything-non-constant?)
	= compute-signature(method-parse.method-parameters,
			    method-parse.method-returns);
      if (anything-non-constant?)
	values(#f, #f);
      else
	let result-type = first(signature.returns.positional-types,
				default: signature.returns.rest-value-type);
	let ctv = ct-eval(method-parse.method-body, #f);
	if (ctv & cinstance?(ctv, result-type))
	  // Change it to an init-value.
	  values(ctv, #t);
	else
	  // Make a constant init-function definition.
	  let result-param
	    = (first(method-parse.method-returns.varlist-fixed,
		     default: method-parse.method-returns.varlist-rest)
		 | make(<parameter>,
			name: make(<identifier-token>,
				   kind: $raw-ordinary-word-token,
				   symbol: #"result")));
	  let new-method-parse
	    = make(<method-parse>,
		   parameters: make(<parameter-list>),
		   returns: make(<variable-list>, fixed: vector(result-param)),
		   body: make(<funcall-parse>,
			      function: make(<method-ref-parse>,
					     method: method-parse),
			      arguments: #[]));
	  let (new-signature, anything-non-constant?)
	    = compute-signature(new-method-parse.method-parameters,
				new-method-parse.method-returns);
	  if (anything-non-constant?)
	    error("%= shouldn't be able to have anything non-constant in it",
		  new-signature);
	  end if;
	  let name
            = if(getter-name)
                make(<derived-name>, how: #"init-function", base: getter-name);
              else
                make(<anonymous-name>, location: source-location(tlf));
              end;
	  let init-func-defn
	    = make(<init-function-definition>, name: name,
		   library: tlf.tlf-defn.defn-library,
		   signature: new-signature,
		   method-parse: new-method-parse);
	  add!(tlf.tlf-init-function-defns, init-func-defn);
	  values(init-func-defn.ct-value, #f);
	end if;
      end if;
    else
      values(#f, #f);
    end if;
  end if;
end method maybe-define-init-function;


// class-defn-mumble-function accessors.

define method class-defn-deferred-evaluations-function
    (defn :: <real-class-definition>) => res :: false-or(<ct-function>);
  if (defn.%class-defn-deferred-evaluations-function == #"not-computed-yet")
    error("Internal error: Too late to compile deferred evaluation function.");
  else
    defn.%class-defn-deferred-evaluations-function;
  end;
end;

define method class-defn-deferred-evaluations-function
    (defn :: <local-class-definition>) => res :: false-or(<ct-function>);
  if (defn.%class-defn-deferred-evaluations-function == #"not-computed-yet")
    defn.%class-defn-deferred-evaluations-function
      := if (block (return)
	       let cclass = ct-value(defn);
	       unless (cclass)
		 return(#f);
	       end;
	       // If any of our superclasses have a deferred evaluations
	       // function, we need one.
	       for (super in cclass.direct-superclasses)
		 if (super.class-defn.class-defn-deferred-evaluations-function)
		   return(#t);
		 end;
	       end;
	       // If any of our slots require some deferred evaluations,
	       // then we need a deferred evaluations function.
	       for (slot-defn in defn.class-defn-slots)
		 let info = slot-defn.slot-defn-info;
		 if (instance?(info.slot-type, <unknown-ctype>)
		       | info.slot-init-value == #t
		       | info.slot-init-function == #t)
		   return(#t);
		 end;
	       end;
	       // Same for the overrides.
	       for (override-defn in defn.class-defn-overrides)
		 let info = override-defn.override-defn-info;
		 if (info.override-init-value == #t
		       | info.override-init-function == #t)
		   return(#t);
		 end;
	       end;
	       // And for initialization argument keywords
	       for (keyword-defn in defn.class-defn-keywords)
		 let info = keyword-defn.keyword-defn-info;
		 if (info.keyword-init-value == #t
		       | info.keyword-init-function == #t)
		   return(#t);
		 end;
	       end;
	       // Inherited each-subclass slots w/ non obvious init
	       // values impose the existence of the deferred-evaluations
	       // function. (covered also below) SO THIS IS REDUNDANT??? TODO
	       for (override-defn in defn.class-defn-overrides)
		 let info = override-defn.override-defn-info;
		 if (instance?(info.override-slot, <each-subclass-slot-info>))
		   if (info.override-init-value /* == #t е TODO (same as above?) */
			| info.override-init-function)
		     return(#t);
		   end;
		 end;
	       end;


	       // Any each-subclass slots w/ non obvious init
	       // values impose the existence of the deferred-evaluations
	       // function.
	       for (slot-info in cclass.all-slot-infos)
		 if (instance?(slot-info, <each-subclass-slot-info>))
		   if (slot-info.slot-init-value /* == #t е TODO (same as above?) */
			| slot-info.slot-init-function)
		     return(#t);
		   end;
		   // examine if overriddenееее
		 end;
	       end;


	       // Also indirect slots with non-ctv init-values/functions
	       // imply a deferred-evaluations function.
	       for (slot-defn in defn.class-defn-slots)
		 let info = slot-defn.slot-defn-info;
		 if (instance?(info, <indirect-slot-info>))
		   if (info.slot-init-value /* == #t е TODO */
			| info.slot-init-function)
		     return(#t);
		   end;
		 end;
	       end;
	       
	     end)
	   make(<ct-function>,
		name: make(<derived-name>, how: #"deferred-evaluation",
			   base: defn.defn-name),
		signature: make(<signature>, specializers: #(),
				returns: make-values-ctype(#(), #f)));
	 else
	   #f;
	 end;
  else
    defn.%class-defn-deferred-evaluations-function;
  end;
end;

define method class-defn-key-defaulter-function
    (defn :: <real-class-definition>) => res :: false-or(<ct-function>);
  if (defn.%class-defn-key-defaulter-function == #"not-computed-yet")
    error("Internal error: Too late to compile deferred evaluation function.");
  else
    defn.%class-defn-key-defaulter-function;
  end;
end;

define method class-defn-key-defaulter-function
    (defn :: <local-class-definition>) => res :: false-or(<ct-function>);
  if (defn.%class-defn-key-defaulter-function == #"not-computed-yet")
    let cclass = ct-value(defn);
    defn.%class-defn-key-defaulter-function
      := if (cclass
	       & ~cclass.abstract?
	       & (~empty?(cclass.keyword-infos)
		    | any?(method(super)
			     super.class-defn.class-defn-key-defaulter-function
			   end, cclass.direct-superclasses)))
	   make(<ct-function>,
		name:
		  make(<derived-name>,
		       how: #"key-defaulter",
		       base: defn.defn-name),
		signature:
		  make(<signature>,
		       specializers:
			 list(specifier-type(#"<simple-object-vector>")),
		       returns:
			 specifier-type(#"<simple-object-vector>")));
	 else
	   #f;
	 end;
  else
    defn.%class-defn-key-defaulter-function;
  end;
end;
	   

define function add-key-info!
    (key :: <symbol>,
     key-infos :: <stretchy-vector>,
     slot :: <slot-info>,
     type :: <ctype>,
     override :: false-or(<override-info>),
     default :: false-or(<ct-value>))
    => key-info :: <key-info>;
  let required? = ~override & slot.slot-init-keyword-required?;
  let default-bogus? = default & ~cinstance?(default, type);
  let key-info = make(<key-info>, key-name: key, type: type,
		      required: required? | default-bogus?,
		      default: default);
  add!(key-infos, key-info);
  key-info
end function;

define function find-override
    (slot :: <slot-info>, cclass :: <cclass>)
  => override :: false-or(<override-info>);
  block (found)
    for (override in slot.slot-overrides)
      if ((override.override-init-value
	   | override.override-init-function)
	  & csubtype?(cclass, override.override-introduced-by))
	found(override);
      end;
    finally
      #f;
    end;
  end;
end;

define method class-defn-maker-function
    (defn :: <real-class-definition>) => res :: false-or(<ct-function>);
  if (defn.%class-defn-maker-function == #"not-computed-yet")
    error("Internal error: too late to compute maker function.");
  else
    defn.%class-defn-maker-function;
  end if;
end method class-defn-maker-function;

define method class-defn-maker-function
    (defn :: <local-class-definition>) => res :: false-or(<ct-function>);
  if (defn.%class-defn-maker-function == #"not-computed-yet")
    defn.%class-defn-maker-function
      := block (return)
	   let cclass = ct-value(defn);
	   //
	   // If the class is hairy or abstract, no maker.
	   if (cclass == #f | cclass.abstract?)
	     return(#f);
	   end;
	   //
	   let key-infos = make(<stretchy-vector>);
	   for (slot in cclass.all-slot-infos)
	     unless (instance?(slot, <virtual-slot-info>))
	       if (instance?(slot.slot-type, <unknown-ctype>))
		 //
		 // Unknown slot type: no maker.
		 return(#f);
	       end;
	       //
	       // Find the active override.
	       let override = find-override(slot, cclass);
	       //
	       // If there is an init-function, and it isn't a constant,
	       // give up.
	       let init-function
		 = if (override)
		     override.override-init-function;
		   else
		     slot.slot-init-function;
		   end if;
	       if (init-function == #t)
		 return(#f);
	       end if;
	       //
	       // If there is an init-value, and it isn't a constant, give up.
	       let init-value
		 = if (override)
		     override.override-init-value;
		   else
		     slot.slot-init-value;
		   end;
	       if (init-value == #t)
		 return(#f);

		// add-key-info! could be extended to accept lazy values, i.e.
		// a thunk (not <ct-function>) that fetches the deferred-eval
		// value from the slot-info.
		// this would be useful when init-value is a module variable or
		// runtime computed constant, where the C location is known.

	       end;
	       //
	       // If the slot is keyword initializable, make a key-info for it.
	       let key = slot.slot-init-keyword;
	       if (key)
		 add-key-info!(key, key-infos, slot, slot.slot-type,
			       override, init-value);
	       end if;
	     end unless;
	   end for;
	   //
	   // Okay, we can make a ctv for the maker function.  First,
	   // compute some values we will need.
	   let name = make(<derived-name>, how: #"maker", base: defn.defn-name);
	   let sig = make(<signature>, specializers: #(),
			  keys: as(<list>, key-infos), all-keys: #t,
			  returns: make(<direct-instance-ctype>,
					base-class: cclass));
	   //
	   // If this is the maker for an immediate representation class,
	   // set up the maker so that it can be inlined.
	   let instance-rep = pick-representation(cclass, #"speed");
	   let maker-defn
	     = if (instance?(instance-rep, <immediate-representation>))
		 make(<maker-function-definition>,
		      name: name,
		      source-location: defn.source-location,
		      library: defn.defn-library,
		      signature: sig,
		      inline-function: maker-inline-expansion,
		      inline-type: #"inline",
		      class-defn: defn);
	       end if;
	   //
	   // And make the ctv.
	   make(<ct-function>, name: name, signature: sig,
		definition: maker-defn);
	 end block;
  else
    defn.%class-defn-maker-function;
  end if;
end method class-defn-maker-function;


define method maker-inline-expansion
    (maker-defn :: <maker-function-definition>)
    => res :: <function-literal>;
  let class-defn = maker-defn.maker-func-defn-class-defn;
  let component = make(<fer-component>);
  let builder = make-builder(component);
  let region = build-maker-function-body(builder, class-defn);
  let leaf = make-function-literal(builder, #f, #"function", #"local",
				   maker-defn.function-defn-signature, region);
  optimize-component(*current-optimizer*, component, simplify-only?: #t);
  leaf;
end method maker-inline-expansion;


// Some helpful functions for slot initialization.

define function call-init-function
  (init-function,
   slot :: <slot-info>,
   override :: false-or(<override-info>),
   slot-name :: <symbol>,
   builder :: <fer-builder>,
// ICE!!!   builder :: <internal-builder>,
   policy :: <policy>,
   source :: <source-location>,
   init-value-var :: <abstract-variable>,
   init-function-leaf :: false-or(<leaf>),
   type-var :: false-or(<abstract-variable>))
 => ();
  if (init-function == #t)
    let init-function-ref :: <leaf>
      = init-function-leaf
	| make-local-var(builder,
			 symcat(slot-name, "-init-function"),
			 function-ctype());
    unless (init-function-leaf)
      let (getter-name, instance)
	= if (override)
	    values(#"override-init-function", override);
	  else
	    values(#"slot-init-function", slot);
	  end;
      build-assignment
	(builder, policy, source,
	 init-function-ref,
	 make-unknown-call
	   (builder,
	    ref-dylan-defn(builder, policy, source, getter-name),
	    #f,
	    list(make-literal-constant(builder, instance))));
    end unless;

    build-assignment
      (builder, policy, source, init-value-var,
       make-unknown-call(builder, init-function-ref, #f, #()));
  elseif (init-function)
	    // is it a reducible function? -- then just assign the ctv result, dont call the func! TODO
    let init-func-leaf
      = make-literal-constant(builder, init-function);
    build-assignment
      (builder, policy, source, init-value-var,
       make-unknown-call(builder, init-func-leaf, #f, #()));
  else
    error("shouldn't have called call-init-function "
	  "when init-function is false");
  end;

  if (type-var)
    build-assignment
      (builder, policy, source, init-value-var,
       make-check-type-operation
	 (builder, policy, source,
	  init-value-var, type-var));
  end;
end function call-init-function;




// build-home-store store a value into the specified slot of its homing instance.
// currently only handles class slots, but could be extended to instance and each subclass
// slots too. Also it could handle the init? slots.
define generic build-home-store
  (builder :: <fer-builder>,
   new :: <leaf>,
   slot-info :: <slot-info>,
   override-info :: false-or(<override-info>), 
   policy :: <policy>,
   source :: <source-location>,
   class-instance :: <cclass>)
 => ();

define method build-home-store
  (builder :: <fer-builder>,
   new :: <leaf>,
   slot-info :: <class-slot-info>,
   override-info :: false-or(<override-info>), 
   policy :: <policy>,
   source :: <source-location>,
   class-instance :: <cclass>)
 => ();
  let slot-home
    = build-slot-home
        (slot-info.slot-getter.variable-name,
	 make-literal-constant(builder, class-instance),
	 builder, policy, source);
  let meta-slot = slot-info.associated-meta-slot;
  assert(~meta-slot.slot-initialized?-slot);
  let meta-instance = meta-slot.slot-introduced-by;
  let offset = find-slot-offset(meta-slot, meta-instance);
  build-assignment
    (builder, policy, source, #(),
     make-operation
       (builder, <heap-slot-set>,
	list(new, slot-home, make-literal-constant(builder, offset)),
	slot-info: meta-slot));
end method;

define method build-home-store
  (builder :: <fer-builder>,
   new :: <leaf>,
   slot-info :: <each-subclass-slot-info>,
   override-info :: false-or(<override-info>), 
   policy :: <policy>,
   source :: <source-location>,
   class-instance :: <cclass>)
 => ();
  let slot-home
    = build-slot-home
        (slot-info.slot-getter.variable-name,
	 make-literal-constant(builder, class-instance),
	 builder, policy, source);
  let meta-slot = slot-info.associated-meta-slot;
  assert(override-info | ~meta-slot.slot-initialized?-slot);
  let meta-instance
    = class-instance & class-instance.class-metaclass
      | if (override-info)
	  override-info.override-introduced-by.class-metaclass;
        else
	  meta-slot.slot-introduced-by;
        end;
  let offset = find-slot-offset(meta-slot, meta-instance); // there may be more than one!!еее
  build-assignment
    (builder, policy, source, #(),
     make-operation
       (builder, <heap-slot-set>,
	list(new, slot-home, make-literal-constant(builder, offset)),
	slot-info: meta-slot));
end method;


// Top level form conversion.


define method convert-top-level-form
    (tl-builder :: <fer-builder>, tlf :: <define-class-tlf>) => ();
  let defn = tlf.tlf-defn;
  let cclass = ct-value(defn);
  if (cclass == #f)
    // The class is sufficiently hairy that we can't do anything.
    // Build top-level init code to create the class at runtime.
    error("### Can't deal with hairy classes yet.");
  else
    // The construction of the class object and the initialization of the class
    // variable will be handled by the linker.  We just need to build the
    // deferred-evaluations, key-defaulter, and maker functions.

    let lexenv = make(<lexenv>, method-name: defn.defn-name);
    let policy = lexenv.lexenv-policy;
    let source = defn.source-location;
    
    for (init-func-defn in tlf.tlf-init-function-defns)
      let meth = init-func-defn.init-func-defn-method-parse;
      let name = init-func-defn.defn-name;
      fer-convert-method(tl-builder, meth, name, init-func-defn.ct-value,
			 #"global", lexenv, lexenv);
    end for;

    let evals-builder = make-builder(tl-builder);
    begin

      // Do the deferred evaluations for any of the superclasses that need it.
      for (super in cclass.direct-superclasses)
	if (super.class-defn.class-defn-deferred-evaluations-function)
	  build-assignment
	    (evals-builder, policy, source, #(),
	     make-unknown-call
	       (evals-builder,
		ref-dylan-defn(evals-builder, policy, source,
			       #"maybe-do-deferred-evaluations"),
		#f,
		list(make-literal-constant(evals-builder, super))));
	end;
      end;

      local method update-indirect-slot
	(slot-info :: <indirect-slot-info>,
	 override-info :: false-or(<override-info>),
	 slot-name :: <symbol>,
	 init-value :: type-union(<ct-value>, <boolean>),
	 init-function :: type-union(<ct-value>, <boolean>),
	 type :: <ctype>,
	 init-value-var :: false-or(<initial-variable>),
	 make-init-value-var :: <function>,
	 init-function-leaf :: false-or(<leaf>),
	 type-var :: false-or(<initial-variable>))
       => ();
	if (init-value) // later: init-value == #t TODO
	  //
	  // Copy over the value into the class instance.
	  let var = init-value-var | make-init-value-var();
	  unless (init-value-var)
	    build-assignment
	      (evals-builder, policy, source, var,
	       make-unknown-call
		 (evals-builder,
		  ref-dylan-defn(evals-builder, policy, source,
				 if (override-info)
				   #"override-init-value"
				 else
				   #"slot-init-value"
				 end),
		  #f,
		  list(make-literal-constant(evals-builder, override-info | slot-info))));
	  end unless;
	  //
	  // assign to the indirect slot.
	  // cannot simply use the slot-setter because slot might be constant.
	  build-home-store(evals-builder, var,
			   slot-info, override-info,
			   policy, source, cclass);
	elseif (init-function)
	  //
	  // Invoke the init function and store the result
	  // into the class instance.
	  let var = make-init-value-var();
	  call-init-function(init-function, slot-info, override-info,
			     slot-name, evals-builder, policy, source,
			     var, init-function-leaf, type-var);
	  // assign to the indirect slot.
	  // cannot simply use the slot-setter because slot might be constant.
	  build-home-store(evals-builder, var,
			   slot-info, override-info,
			   policy, source, cclass);
	end if;
      end method update-indirect-slot;

      for (slot-defn in defn.class-defn-slots,
	   index from 0)
	let slot-info = slot-defn.slot-defn-info;
	let getter = slot-info.slot-getter;
 	let slot-name = slot-info.slot-getter.variable-name;

	let slot-type = slot-info.slot-type;
	let (type, type-var)
	  = if (instance?(slot-type, <unknown-ctype>))
	      let type-expr = slot-defn.slot-defn-type;
	      let var
		= make-local-var(evals-builder,
				 symcat(slot-name, "-type"),
				 specifier-type(#"<type>"));
	      fer-convert(evals-builder, type-expr, lexenv,
			  #"assignment", var);
	      build-assignment
		(evals-builder, policy, source, #(),
		 make-unknown-call
		   (evals-builder,
		    ref-dylan-defn(evals-builder, policy, source,
				   #"slot-type-setter"),
		    #f,
		    list(var,
			 make-literal-constant(evals-builder, slot-info))));
	      values(object-ctype(), var);
	    else
	      values(slot-type, #f);
	    end;

	local method make-init-value-var() => var :: <initial-variable>;
		make-local-var(evals-builder, symcat(slot-name, "-init-value"),
			       type);
	      end;

	let init-value = slot-info.slot-init-value;
	let init-function = slot-info.slot-init-function;

	let (init-value-var, init-function-leaf) =
	  if (init-value == #t)
	    let var = make-init-value-var();
	    fer-convert(evals-builder, slot-defn.slot-defn-init-value,
			lexenv, #"assignment", var);
	    build-assignment
	      (evals-builder, policy, source, #(),
	       make-unknown-call
		 (evals-builder,
		  ref-dylan-defn(evals-builder, policy, source,
				 #"slot-init-value-setter"),
		  #f,
		  list(var, make-literal-constant(evals-builder, slot-info))));
	    var;
	  elseif (init-function == #t)
	    let leaf = convert-init-function(evals-builder, slot-info.slot-getter,
					     slot-defn.slot-defn-init-function,
					     type);
	    build-assignment
	      (evals-builder, policy, source, #(),
	       make-unknown-call
		 (evals-builder,
		  ref-dylan-defn(evals-builder, policy, source,
				 #"slot-init-function-setter"),
		  #f,
		  list(leaf, make-literal-constant(evals-builder, slot-info))));
	    values(#f, leaf);
	  end if;

	// Now that the <slot-descriptor> contains the proper values,
	// we can begin to initialize the storage allocated for
	// indirect slots (if they are not already).
	if (instance?(slot-info, <indirect-slot-info>))
	  update-indirect-slot(slot-info, #f, slot-name, init-value, init-function, type,
			       init-value-var, make-init-value-var, init-function-leaf, type-var);
	end if;

	unless (#"virtual" == slot-defn.slot-defn-allocation)
	  if (type-var)
	    local
	      method build-call (name, #rest args)
		let temp = make-local-var(evals-builder, name, object-ctype());
		build-assignment
		  (evals-builder, policy, source, temp,
		   make-unknown-call
		     (evals-builder,
		      ref-dylan-defn(evals-builder, policy, source, name),
		      #f, as(<list>, args)));
		temp;
	      end,
	      method build-add-method (gf-name, method-defn, method-leaf)
		// We don't use method-defn-of, because that is #f if there
		// is a definition but it isn't a define generic.
		let gf-var = find-variable(gf-name);
		let gf-defn = gf-var & gf-var.variable-definition;
		if (gf-defn)
		  let gf-leaf = build-defn-ref(evals-builder, policy,
					       source, gf-defn);
		  build-assignment
		    (evals-builder, policy, source, #(),
		     make-unknown-call
		       (evals-builder,
			ref-dylan-defn(evals-builder, policy, source,
				       #"add-method"),
			#f,
			list(gf-leaf, method-leaf)));
		  build-defn-set(evals-builder, policy, source, method-defn,
				 method-leaf);
		else
		  compiler-fatal-error-location
		    (tlf,
		     "No definition for %s, and can't implicitly define it.",
		     gf-name.name-symbol);
		end;
	      end;

	    let results = build-call(#"list", type-var);
	    let cclass-leaf = make-literal-constant(evals-builder, cclass);
	    let false-leaf
	      = make-literal-constant(evals-builder, #f);
	    begin
	      let getter
		= build-getter(evals-builder, #f, slot-defn, slot-info);
	      let getter-specializers = build-call(#"list", cclass-leaf);
	      let meth = build-call(#"%make-method", getter-specializers,
				    results, false-leaf, getter);
	      build-add-method(slot-defn.slot-defn-getter-name,
			       slot-defn.slot-defn-getter, meth);
	    end;
	    if (slot-defn.slot-defn-setter)
	      let setter
		= build-setter(evals-builder, #f, slot-defn, slot-info);
	      let setter-specializers = build-call(#"list", type-var,
						   cclass-leaf);
	      let meth = build-call(#"%make-method", setter-specializers,
				    results, false-leaf, setter);
	      build-add-method(slot-defn.slot-defn-setter-name,
			       slot-defn.slot-defn-setter, meth);
	    end;
	  else
	    begin
	      let getter = slot-defn.slot-defn-getter.ct-value;
	      let getter-standin = slot-accessor-standin(slot-info, #"getter");
	      if (getter-standin)
		getter.ct-accessor-standin := getter-standin;
	      else
		build-getter(tl-builder, getter, slot-defn, slot-info);
	      end if;
	    end;
	    if (slot-defn.slot-defn-setter)
	      let setter = slot-defn.slot-defn-setter.ct-value;
	      let setter-standin = slot-accessor-standin(slot-info, #"setter");
	      if (setter-standin)
		setter.ct-accessor-standin := setter-standin;
	      else
		build-setter(tl-builder, setter, slot-defn, slot-info);
	      end if;
	    end if;
	  end if;
	end unless;
      end for;

      local method effective-override(slot-info :: <slot-info>, #key start)
       => override :: false-or(<override-info>);
	block (return)
	  if (start)
	    if (start.override-init-value | start.override-init-function)
	      return(start);
	    end;
	  else
	    for (override in cclass.override-infos)
	      if (override.override-slot == slot-info)
	        return(#f);
	      end if;
	    end for;
	  end;
	  for (super in cclass.precedence-list.tail)
	    for (override in super.override-infos)
	      if (override.override-slot == slot-info
		  & (override.override-init-value | override.override-init-function))
	        return(override);
	      end;
	    end for;
	  end for;
	end block;
      end method;
      
      for (override-defn in defn.class-defn-overrides,
	   index from 0)
	let override-info = override-defn.override-defn-info;
	let getter = override-info.override-getter;
	let slot-name = getter.variable-name;
	let init-value = override-info.override-init-value;
	let init-function = override-info.override-init-function;
	let type = object-ctype(); /// еее as above!!!


	local method make-init-value-var() => var :: <initial-variable>;
		make-local-var(evals-builder, symcat(slot-name, "-init-value"),
			       type);
	      end; /// reuse ееее TODO


	let (init-value-var, init-function-leaf) =
	  if (init-value == #t)
	    let var = make-init-value-var();
	    fer-convert(evals-builder, override-defn.override-defn-init-value,
			lexenv, #"assignment", var);
	    build-assignment
	      (evals-builder, policy, source, #(),
	       make-unknown-call
		 (evals-builder,
		  ref-dylan-defn(evals-builder, policy, source,
				 #"override-init-value-setter"),
		  #f,
		  list(var, make-literal-constant(evals-builder, override-info))));
	    var;
	  elseif (init-function == #t)
	    let leaf = convert-init-function(evals-builder, getter,
					     override-defn.override-defn-init-function,
					     type);
	    build-assignment
	      (evals-builder, policy, source, #(),
	       make-unknown-call
		 (evals-builder,
		  ref-dylan-defn(evals-builder, policy, source,
				 #"override-init-function-setter"),
		  #f,
		  list(leaf, make-literal-constant(evals-builder, override-info))));
	    values(#f, leaf);
	  end if;

	local method effective-inits(slot-info :: <slot-info>, #key start)
	 => (override-info :: false-or(<override-info>),
	     init-value :: type-union(<ct-value>, <boolean>),
	     init-function :: type-union(<ct-value>, <boolean>));

	  let override-info = effective-override(slot-info, start: start);
	  values(override-info,
		 override-info & override-info.override-init-value
		  | slot-info.slot-init-value,
		 override-info & override-info.override-init-function
		  | slot-info.slot-init-function)
	end method effective-inits;

	// now update the each-subclass-slot
	// since all deferred evaluations are performed at this point,
	// we can assume that slot and override descriptors are properly set up.
	let slot-info = override-info.override-slot;
	if (instance?(slot-info, <each-subclass-slot-info>))
	  let (override-info, init-value, init-function)
	    = effective-inits(slot-info, start: override-info);
	  update-indirect-slot(slot-info, override-info, slot-name, init-value, init-function, type,
			       init-value-var, make-init-value-var, init-function-leaf,
			       /* еее type-var */ #f);
	end if;
      end for;
      
      // now it remains to initialize all the remaining each subclass slots
      // that are inherited but not mentioned by override-infos
      //
      for (slot-info in cclass.all-slot-infos)
	if (instance?(slot-info, <each-subclass-slot-info>)
	    & slot-info.slot-introduced-by ~== cclass
	    & ~any?(compose(curry(\==, slot-info), override-slot), cclass.override-infos))
	  
	  // use effective-inits above!!! еее
	  let override-info = slot-info.effective-override; // еее reuse find-override !!!
	  // since all deferred evaluations are performed at this point,
	  // we can assume that slot and override descriptors are properly set up.
	  let init-value = override-info & override-info.override-init-value
			   | slot-info.slot-init-value;
	  let init-function = override-info & override-info.override-init-function
			      | slot-info.slot-init-function;
	  let type = object-ctype(); /// еее as above!!!
	  let slot-name = slot-info.slot-getter.variable-name;
	  
	  local method make-init-value-var() => var :: <initial-variable>;
		make-local-var(evals-builder, symcat(slot-name, "-init-value"),
			       type);
	      end;

	  update-indirect-slot
	    (slot-info, override-info, slot-name,
	     init-value, init-function, type,
	     #f, make-init-value-var, #f, #f);
	end if;
      end for;
    end begin;

    unless (cclass.abstract?)
      if(~empty?(cclass.keyword-infos)
	   | any?(method(super)
		      super.class-defn.class-defn-key-defaulter-function
		  end, cclass.direct-superclasses))
	//
	// Build the key-defaulter
	let (defaulter-region, defaulter-signature)
	  = build-key-defaulter-function-body(tl-builder, defn);
	
	let ctv = defn.class-defn-key-defaulter-function;
	if(ctv)
	  make-function-literal(tl-builder, ctv, #"function", #"global",
				defaulter-signature, defaulter-region);
	else
	  let defaulter-leaf
	    = make-function-literal(tl-builder, #f, #"function", #"local",
				    defaulter-signature, defaulter-region);
	  build-assignment
	    (evals-builder, policy, source, #(),
	     make-unknown-call
	       (evals-builder,
		ref-dylan-defn(evals-builder, policy, source,
			       #"class-key-defaulter-setter"),
		#f,
		list(defaulter-leaf,
		     make-literal-constant(evals-builder, cclass))));
	end if;
      end if;
      //
      // Build the maker.
      let (maker-region, maker-signature)
	= build-maker-function-body(tl-builder, defn);
      
      // Fill in the maker function.
      let ctv = defn.class-defn-maker-function;
      if (ctv)
	make-function-literal(tl-builder, ctv, #"function", #"global",
			      maker-signature, maker-region);
      else
	// The maker function isn't a compile-time constant, so add code to
	// the deferred evaluations to install it.
	let maker-leaf
	  = make-function-literal(tl-builder, #f, #"function", #"local",
				  maker-signature, maker-region);
	build-assignment
	  (evals-builder, policy, source, #(),
	   make-unknown-call
	     (evals-builder,
	      ref-dylan-defn(evals-builder, policy, source,
			     #"class-maker-setter"),
	      #f,
	      list(maker-leaf,
		   make-literal-constant(evals-builder, cclass))));
      end if;
    end unless;

    //
    // Finish building the deferred evaluations function.
    let ctv = defn.class-defn-deferred-evaluations-function;
    if (ctv)
      let func-region = build-function-body(tl-builder, policy, source, #f,
					    ctv.ct-function-name,
					    #(), make-values-ctype(#(), #f),
					    #t);
      build-region(tl-builder, builder-result(evals-builder));
      
      // It returns nothing.
      build-return(tl-builder, policy, source, func-region, #());
      end-body(tl-builder);
      make-function-literal(tl-builder, ctv, #"function", #"global",
			    ctv.ct-function-signature, func-region);
    else
      assert(instance?(builder-result(evals-builder), <empty-region>));
    end if;
  end if;
end method convert-top-level-form;


define method make-descriptors-leaf
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     what :: <byte-string>, for-class :: <cclass>)
    => leaf :: <leaf>;
  let var = make-local-var(builder, symcat(what, "-descriptors"),
			   object-ctype());
  build-assignment
    (builder, policy, source, var,
     make-unknown-call
       (builder,
	ref-dylan-defn(builder, policy, source,
		       symcat("class-", what, "-descriptors")),
	#f,
	list(make-literal-constant(builder, for-class))));
  var;
end;

define class <initkey-info> (<object>)
  constant slot initkey-keyword-info :: <keyword-info>,
    required-init-keyword: keyword-info:;
  constant slot initkey-var :: <abstract-variable>,
    required-init-keyword: var:;
  constant slot initkey-supplied?-var :: false-or(<abstract-variable>),
    required-init-keyword: supplied?-var:;
end class;

define method build-key-defaulter-function-body
    (tl-builder :: <fer-builder>, defn :: <class-definition>)
    => (defaulter-region :: <fer-function-region>,
	signature :: <signature>);
  let builder = make-builder(tl-builder);
  let cclass :: <cclass> = defn.ct-value;
  let keyword-infos = cclass.all-keyword-infos;

  let policy = $Default-Policy;
  let source = defn.source-location;

  let sov-type = specifier-type(#"<simple-object-vector>");
  let arg = make-local-var(builder, #"keys", sov-type);

  let defaulter-region
    = build-function-body(builder, policy, source, #f,
			  make(<derived-name>, how: #"key-defaulter",
			       base: defn.defn-name),
			  list(arg),
			  sov-type, #t);

  //
  // Create value and supplied? local variables for each keyword
  local
    method make-initkey-info
        (info :: <keyword-info>)
     => (initkey-info :: <initkey-info>);
      let key = info.keyword-symbol;
      let type = info.keyword-type;
      let var = make-local-var(builder, key, type);
      let default = info.keyword-init-value;
      let default-bogus?
        = default & ~cinstance?(info.keyword-init-value, type);
      let needs-supplied?-var? = info.keyword-needs-supplied?-var;
      let supplied?-var
        = if (default-bogus? | needs-supplied?-var?)
            make-local-var(builder, as(<symbol>,
                                       concatenate(as(<string>, key),
                                                   "-supplied?")),
                           dylan-value(#"<boolean>"));
	    else
	      #f;
          end;
      build-assignment
        (builder, policy, source, var,
         if (default & ~default-bogus?)
           make-literal-constant(builder, default);
         else
           make(<uninitialized-value>, derived-type: type.ctype-extent);
         end);
      make(<initkey-info>,
           keyword-info: info,
           var: var,
           supplied?-var: supplied?-var);
    end method;
  let initkey-infos = map(make-initkey-info, keyword-infos);

  //
  // Loop over the keyword arguments starting from the leftmost
  // end so that the rightmost instance of a repeated keyword wins out
  let arg-size-var
    = make-local-var(builder, #"keys-size", dylan-value(#"<integer>"));
  build-assignment(builder, policy, source,
                   arg-size-var,
                   make-unknown-call
                     (builder,
                      ref-dylan-defn(builder, policy, source, #"size"),
                      #f, list(arg)));

  let index-var
    = make-local-var(builder, #"index", dylan-value(#"<integer>"));
  build-assignment
    (builder, policy, source, index-var,
     make-unknown-call
       (builder, ref-dylan-defn(builder, policy, source, #"-"), #f,
        list(arg-size-var, make-literal-constant (builder, 2))));

  let others-var
    = make-local-var(builder, #"others", dylan-value(#"<list>"));
  build-assignment
    (builder, policy, source, others-var, make-literal-constant(builder, #()));

  //
  // Loop over the keyword arguments vector
  let done-block = build-block-body(builder, policy, source);
  build-loop-body(builder, policy, source);

  let done-var = make-local-var(builder, #"done?", object-ctype());
  build-assignment
    (builder, policy, source, done-var,
     make-unknown-call(builder,
                       ref-dylan-defn(builder, policy, source, #"<"),
                       #f,
                       list(index-var, make-literal-constant(builder, 0))));
  build-if-body(builder, policy, source, done-var);
  build-exit(builder, policy, source, done-block);
  build-else(builder, policy, source);

  // Fetch keyword/value pair from the arguments vector
  let key-var = make-local-var(builder, #"key", dylan-value(#"<symbol>"));
  build-assignment
    (builder, policy, source, key-var,
     make-unknown-call(builder,
                       ref-dylan-defn(builder, policy, source, #"%element"),
                       #f, list(arg, index-var)));

  let temp = make-local-var(builder, #"temp", dylan-value(#"<integer>"));
  build-assignment
    (builder, policy, source, temp,
     make-unknown-call(builder,
                       ref-dylan-defn(builder, policy, source, #"+"),
                       #f, list(index-var,
                                make-literal-constant(builder, 1))));
  let val-var = make-local-var(builder, #"value", object-ctype());
  build-assignment
    (builder, policy, source, val-var,
     make-unknown-call(builder,
                       ref-dylan-defn(builder, policy, source, #"%element"),
                       #f, list(arg, temp)));

  //
  // Build if-checks for each keyword
  for(info in initkey-infos)
    let keyword-info = info.initkey-keyword-info;

    let cond = make-local-var(builder, #"condition", object-ctype());
    build-assignment
      (builder, policy, source, cond,
       make-unknown-call
         (builder,
          ref-dylan-defn(builder, policy, source, #"=="),
          #f,
          list(key-var,
               make-literal-constant(builder, keyword-info.keyword-symbol))));
    build-if-body(builder, policy, source, cond);

    build-assignment(builder, policy, source, info.initkey-var, val-var);
    if(info.initkey-supplied?-var)
      build-assignment(builder, policy, source,
                       info.initkey-supplied?-var,
                       make-literal-constant(builder, #t));
    end if;
    build-else(builder, policy, source);
  end for;

  //
  // Add unknown keywords to the others list
  let pair-leaf = ref-dylan-defn(builder, policy, source, #"pair");
  build-assignment
    (builder, policy, source, others-var,
     make-unknown-call(builder, pair-leaf, #f,
                       list(val-var, others-var)));
  build-assignment
    (builder, policy, source, others-var,
     make-unknown-call(builder, pair-leaf, #f,
                       list(key-var, others-var)));

  for(i from 0 below initkey-infos.size)
    end-body(builder);
  end for;

  //
  // Decrement the loop counter
  build-assignment
    (builder, policy, source, index-var,
     make-unknown-call(builder,
                       ref-dylan-defn(builder, policy, source, #"-"),
                       #f,
                       list(index-var, make-literal-constant(builder, 2))));
  end-body(builder); // if
  end-body(builder); // loop
  end-body(builder); // block

  //
  // Check for required but absent keywords and for init-functions that
  // might need to be called.
  for(info in initkey-infos)
    let keyword-info = info.initkey-keyword-info;

    if(keyword-info.keyword-required? | keyword-info.keyword-init-function)
      if(info.initkey-supplied?-var)
        build-if-body(builder, policy, source, info.initkey-supplied?-var);
      else
        let supplied?-var
          = make-local-var(builder, #"supplied?", object-ctype());

        build-assignment
          (builder, policy, source, supplied?-var,
           make-operation(builder, <primitive>,
                          list(info.initkey-var), name: #"initialized?"));
        build-if-body(builder, policy, source, supplied?-var);
      end if;
      build-else(builder, policy, source);

      if(keyword-info.keyword-init-function)
        let init-func-leaf
          = make-literal-constant(builder, keyword-info.keyword-init-function);
        build-assignment
          (builder, policy, source, info.initkey-var,
           make-unknown-call(builder, init-func-leaf, #f, #()));
      else
        build-assignment
          (builder, policy, source, #(),
           make-error-operation
             (builder, policy, source,
              #"missing-required-init-keyword-error",
              make-literal-constant(builder, keyword-info.keyword-symbol),
              make-literal-constant(builder, cclass)));
      end if;

      end-body(builder);
    end if;
  end for;

  //
  // Combine known keyword/value pairs with others into a vector
  let apply-args = list(others-var);
  for(info in initkey-infos)
    let keyword-info = info.initkey-keyword-info;
    
    apply-args
      := pair(make-literal-constant(builder, keyword-info.keyword-symbol),
              pair(info.initkey-var, apply-args));
  end for;

  //
  // The optimizer will take care of this
  let vector-leaf = ref-dylan-defn(builder, policy, source, #"vector");
  let vec-var = make-local-var(builder, #"vec", sov-type);
  build-assignment
    (builder, policy, source, vec-var,
     make-unknown-call(builder,
                       ref-dylan-defn(builder, policy, source, #"apply"),
                       #f, pair(vector-leaf, apply-args)));

  build-return(builder, policy, source, defaulter-region, vec-var);
  end-body(builder);
  values(defaulter-region,
	 make(<signature>, specializers: list(sov-type), returns: sov-type));
end method;

define method build-maker-function-body
    (tl-builder :: <fer-builder>, defn :: <class-definition>)
    => (maker-region :: <fer-function-region>,
	signature :: <signature>);
  let key-infos = make(<stretchy-vector>);
  let maker-args = make(<stretchy-vector>);
  let setup-builder = make-builder(tl-builder);
  let maker-builder = make-builder(tl-builder);
  let init-builder = make-builder(tl-builder);
  let cclass :: <cclass> = defn.ct-value;
  let direct = make(<direct-instance-ctype>, base-class: cclass);
  let instance-leaf = make-local-var(init-builder, #"instance", direct);
  let representation = pick-representation(direct, #"speed");
  let immediate-rep?
    = instance?(representation, <immediate-representation>);
  let make-immediate-args = make(<stretchy-vector>);
  let data-word-leaf = #f;
  let size-leaf = #f;
  let vector-slot = cclass.vector-slot;
  let size-slot = vector-slot & vector-slot.slot-size-slot;

  let policy = $Default-Policy;
  let source = defn.source-location;

  for (slot in cclass.all-slot-infos, index from 0)
    //
    // If there isn't a getter, this is an init? slot.  Init? slots
    // are initialized along with the regular slot.
    // Also, don't need to do anything for virtual slots.
    if (slot.slot-getter & ~instance?(slot, <virtual-slot-info>))
      let slot-name = slot.slot-getter.variable-name;

      // Get ahold of the type.
      let slot-type = slot.slot-type;
      let (type, type-var)
	= if (instance?(slot-type, <unknown-ctype>))
	    let var = make-local-var(maker-builder,
				     symcat(slot-name, "-type"),
				     specifier-type(#"<type>"));
	    build-assignment
	      (maker-builder, policy, source, var,
	       make-unknown-call
		 (maker-builder,
		  ref-dylan-defn(maker-builder, policy, source, #"slot-type"),
		  #f,
		  list(make-literal-constant(maker-builder, slot))));
	    values(object-ctype(), var);
	  else
	    values(slot-type, #f);
	  end;
      //
      // Find the active override if there is one. 
      let override = find-override(slot, cclass);
      //
      // Get the init-value or init-function, either from the
      // active override or from the slot itself if there is no
      // active override.
      let (init-value, init-function)
	= if (override)
	    values(override.override-init-value,
		   override.override-init-function);
	  else
	    values(slot.slot-init-value, slot.slot-init-function);
	  end;

      local method extract-init-value (init-value-var) => ();
	      if (init-value == #t)
		if (override)
		  build-assignment
		    (maker-builder, policy, source,
		     init-value-var,
		     make-unknown-call
		       (maker-builder,
			ref-dylan-defn(maker-builder, policy, source,
				       #"override-init-value"),
			#f,
			list(make-literal-constant(maker-builder, override))));
		else
		  build-assignment
		    (maker-builder, policy, source,
		     init-value-var,
		     make-unknown-call
		       (maker-builder,
			ref-dylan-defn(maker-builder, policy, source,
				       #"slot-init-value"),
			#f,
			list(make-literal-constant(maker-builder, slot))));
		end if;
	      elseif (init-value)
		build-assignment
		  (maker-builder, policy, source, init-value-var,
		   make-literal-constant(maker-builder, init-value));
	      else
		error("shouldn't have called extract-init-value "
			"when init-value is false");
	      end;

	      if (type-var)
		build-assignment
		  (maker-builder, policy, source, init-value-var,
		   make-check-type-operation
		     (maker-builder, policy, source,
		      init-value-var, type-var));
	      end;
	    end method extract-init-value,
	    method call-init-function-simpl (init-value-var) => ();
	      call-init-function(init-function, slot, override, slot-name,
				 maker-builder, policy, source,
				 init-value-var, #f, type-var);
	    end method call-init-function-simpl;

      select (slot by instance?)
	<instance-slot-info> =>
	  local
	    method build-slot-init
		(slot :: false-or(<slot-info>), leaf :: <leaf>) => ();
	      if (slot)
		if (immediate-rep?)
		  add!(make-immediate-args, leaf);
		else
		  let posn
		    = get-direct-position(slot.slot-positions, cclass);
		  unless (posn)
		    error("Couldn't find the position for %s",
			  slot-name);
		  end unless;
		  if (posn == #"data-word")
		    data-word-leaf := leaf;
		  else
		    let posn-leaf
		      = make-literal-constant(init-builder, posn);
		    if (instance?(slot, <vector-slot-info>))
		      // We need to build a loop to initialize every
		      // element.
		      let block-region
			= build-block-body(init-builder, policy, source);
		      let index
			= make-local-var(init-builder, #"index",
					 specifier-type(#"<integer>"));
		      build-assignment
			(init-builder, policy, source, index,
			 make-literal-constant(init-builder, 0));
		      build-loop-body(init-builder, policy, source);
		      let more?
			= make-local-var(init-builder, #"more?",
					 boolean-ctype());
		      build-assignment
			(init-builder, policy, source, more?,
			 make-unknown-call
			   (init-builder,
			    ref-dylan-defn(init-builder, policy, source,
					   #"<"),
			    #f,
			    list(index, size-leaf)));
		      build-if-body(init-builder, policy, source, more?);
		      build-assignment
			(init-builder, policy, source, #(),
			 make-operation
			   (init-builder, <heap-slot-set>,
			    list(leaf, instance-leaf, posn-leaf, index),
			    slot-info: slot));
		      build-assignment
			(init-builder, policy, source, index,
			 make-unknown-call
			   (init-builder,
			    ref-dylan-defn(init-builder, policy, source,
					   #"+"),
			    #f,
			    list(index,
				 make-literal-constant(init-builder, 1))));
		      build-else(init-builder, policy, source);
		      build-exit
			(init-builder, policy, source, block-region);
		      end-body(init-builder);
		      end-body(init-builder);
		      end-body(init-builder);
		    else
		      build-assignment
			(init-builder, policy, source, #(),
			 make-operation
			   (init-builder, <heap-slot-set>,
			    list(leaf, instance-leaf, posn-leaf),
			    slot-info: slot));
		      if (slot == size-slot)
			size-leaf := leaf;
		      end if;
		    end if;
		  end if;
		end if;
	      end if;
	    end method build-slot-init;

	  let key = slot.slot-init-keyword;
	  if (key)
	    let default = ~(init-value == #t) & init-value;
	    let key-info = add-key-info!(key, key-infos, slot, type, override, default);
	    let init-value-var
	      = make-local-var(maker-builder,
			       symcat(slot-name, "-init-value"),
			       type);
	    if (default)
	      add!(maker-args, init-value-var);
	      build-slot-init(slot, init-value-var);
	      build-slot-init(slot.slot-initialized?-slot,
			      make-literal-constant(init-builder, #t));
	    else
	      let arg = make-local-var(maker-builder, key, type);
	      add!(maker-args, arg);
	      let supplied?-arg
		= make-local-var(maker-builder,
				 symcat(key, "-supplied?"),
				 boolean-ctype());
	      if (key-info.key-needs-supplied?-var)
		add!(maker-args, supplied?-arg);
	      else
		build-assignment
		  (maker-builder, policy, source, supplied?-arg,
		   make-operation(maker-builder, <primitive>, list(arg),
				  name: #"initialized?"));
	      end;
	      build-if-body(maker-builder, policy, source, supplied?-arg);
	      build-assignment(maker-builder, policy, source,
			       init-value-var, arg);
	      build-else(maker-builder, policy, source);
	      if (init-value)
		extract-init-value(init-value-var);
	      elseif (init-function)
		call-init-function-simpl(init-value-var);
	      elseif (slot.slot-init-keyword-required?)
		build-assignment
		  (maker-builder, policy, source, #(),
		   make-error-operation
		     (maker-builder, policy, source,
		      #"missing-required-init-keyword-error",
		      make-literal-constant(maker-builder, key),
		      make-literal-constant(maker-builder, cclass)));
	      else
		build-assignment(maker-builder, policy, source,
				 init-value-var,
				 make(<uninitialized-value>,
				      derived-type: type.ctype-extent));
	      end;
	      end-body(maker-builder);
	      build-slot-init(slot, init-value-var);
	      build-slot-init(slot.slot-initialized?-slot,
			      if (init-value | init-function)
				make-literal-constant(init-builder, #t);
			      else
				supplied?-arg;
			      end);
	    end;
	  else
	    if (init-value | init-function)
	      let init-value-var
		= make-local-var(maker-builder,
				 symcat(slot-name, "-init-value"),
				 type);
	      if (init-value)
		extract-init-value(init-value-var);
	      else
		call-init-function-simpl(init-value-var);
	      end;
	      build-slot-init(slot, init-value-var);
	      build-slot-init(slot.slot-initialized?-slot,
			      make-literal-constant(init-builder, #t));
	    else
	      build-slot-init
		(slot, make(<uninitialized-value>,
			    derived-type: type.ctype-extent));
	      build-slot-init
		(slot.slot-initialized?-slot,
		 make-literal-constant(init-builder, #f));
	    end if;
	  end if;

	<each-subclass-slot-info>,
	<class-slot-info> =>
	  // If the slot is keyword-initializable, add stuff to the maker
	  // to check for that keyword and change the class slot.
	  
	  local
	    method build-slot-init
		(slot :: <indirect-slot-info>, leaf :: <leaf>, init?-leaf :: <leaf>) => ();

	      if (immediate-rep?) //еее???
		add!(make-immediate-args, leaf);
	      else
		let associated = slot.associated-meta-slot;
		let metaclass = associated.slot-introduced-by;
		
		let posn
		  = get-direct-position(associated.slot-positions, metaclass);
		unless (posn)
		  error("Couldn't find the position for %s", slot-name);
		end unless;
		if (posn == #"data-word")
		  error("Indirect slot allocated in data word for %s?", slot-name);
		else
		  let posn-leaf
		    = make-literal-constant(maker-builder, posn);
		  
		  let slot-initialized?-posn-leaf :: false-or(<leaf>)
		    = associated.slot-initialized?-slot
			& make-literal-constant(maker-builder, posn);

		  let slot-home
		    = build-slot-home(slot-name,
				      make-literal-constant(maker-builder, cclass),
				      maker-builder, policy, source);

		  build-assignment
		    (maker-builder, policy, source, #(),
		     make-operation
		       (maker-builder, <heap-slot-set>,
			list(leaf, slot-home, posn-leaf),
			slot-info: associated));
		  if (slot-initialized?-posn-leaf)
		    build-assignment
		      (maker-builder, policy, source, #(),
		       make-operation
			 (maker-builder, <heap-slot-set>,
			  list(init?-leaf, slot-home, slot-initialized?-posn-leaf),
			  slot-info: associated.slot-initialized?-slot));
		  end if;
		end if;
	      end if;
	    end method build-slot-init;

	  let key = slot.slot-init-keyword;
	  if (key)
	    let key-info = add-key-info!(key, key-infos, slot, type, override, #f);
	    let init-value-var
	      = make-local-var(maker-builder,
			       symcat(slot-name, "-init-value"),
			       type);
	    let arg = make-local-var(maker-builder, key, type);
	    add!(maker-args, arg);
	    let supplied?-arg
	      = make-local-var(maker-builder,
			       symcat(key, "-supplied?"),
			       boolean-ctype());
	    if (key-info.key-needs-supplied?-var)
	      add!(maker-args, supplied?-arg);
	    else
	      build-assignment
		(maker-builder, policy, source, supplied?-arg,
		 make-operation(maker-builder, <primitive>, list(arg),
				name: #"initialized?"));
	    end;
	    build-if-body(maker-builder, policy, source, supplied?-arg);
	    build-assignment(maker-builder, policy, source,
			     init-value-var, arg);
	    build-slot-init(slot, init-value-var, supplied?-arg);
	    if (slot.slot-init-keyword-required?)
	      build-else(maker-builder, policy, source);
	      build-assignment
		(maker-builder, policy, source, #(),
		 make-error-operation
		   (maker-builder, policy, source,
		    #"missing-required-init-keyword-error",
		    make-literal-constant(maker-builder, key),
		    make-literal-constant(maker-builder, cclass)));
	    end if;
	    end-body(maker-builder);
	  end if;
      end select;
    end if;
  end for;
  
  let name = make(<derived-name>, how: #"maker", base: defn.defn-name);
  let maker-region
    = build-function-body(tl-builder, policy, source, #f, name,
			  as(<list>, maker-args), cclass, #t);
  build-region(tl-builder, builder-result(setup-builder));
  build-region(tl-builder, builder-result(maker-builder));
  let bytes = cclass.instance-slots-layout.layout-length;
  let base-len
    = make-literal-constant(tl-builder, bytes);
  let len-leaf
    = if (vector-slot)
	let fi = specifier-type(#"<integer>");
	let elsize
	  = vector-slot.slot-representation.representation-size;
	let extra
	  = if (elsize == 1)
	      size-leaf;
	    else
	      let var = make-local-var(tl-builder, #"extra", fi);
	      let elsize-leaf
		= make-literal-constant(tl-builder, elsize);
	      build-assignment
		(tl-builder, policy, source, var,
		 make-unknown-call
		   (tl-builder,
		    ref-dylan-defn(tl-builder, policy, source, #"*"),
		    #f,
		    list(size-leaf, elsize-leaf)));
	      var;
	    end;
	let var = make-local-var(tl-builder, #"bytes", fi);
	build-assignment
	  (tl-builder, policy, source, var,
	   make-unknown-call
	     (tl-builder,
	      ref-dylan-defn(tl-builder, policy, source, #"+"),
	      #f,
	      list(base-len, extra)));
	var;
      else
	base-len;
      end;
  build-assignment
    (tl-builder, policy, source, instance-leaf,
     if (immediate-rep?)
       make-operation
	 (tl-builder, <primitive>, as(<list>, make-immediate-args),
	  name: #"make-immediate", derived-type: direct.ctype-extent);
     elseif (data-word-leaf)
       make-operation
	 (tl-builder, <primitive>,
	  list(make-literal-constant(tl-builder, cclass),
	       len-leaf, data-word-leaf),
	  name: #"allocate-with-data-word", derived-type: direct.ctype-extent);
     else
       make-operation
	 (tl-builder, <primitive>,
	  list(make-literal-constant(tl-builder, cclass), len-leaf),
	  name: #"allocate", derived-type: direct.ctype-extent);
     end if);
  build-region(tl-builder, builder-result(init-builder));
  build-return(tl-builder, policy, source, maker-region,
	       list(instance-leaf));
  end-body(tl-builder);
  values(maker-region,
	 make(<signature>, specializers: #(),
	      keys: as(<list>, key-infos),
	      all-keys: #t,
	      returns: direct));
end method build-maker-function-body;


define function convert-init-function
    (builder :: <fer-builder>,
     getter :: <variable>,
     init-function :: <expression-parse>,
     result-type :: <ctype>)
    => res :: <leaf>;
  let slot-name = getter.variable-name;
  let fun-name = make(<derived-name>,
  		      base: make(<basic-name>, symbol: slot-name,
		      		 module: getter.variable-home),
		      how: #"init-function");
  let lexenv = make(<lexenv>, method-name: fun-name);
  let policy = lexenv.lexenv-policy;
  let source = make(<source-location>);
  let var = make-lexical-var(builder, symcat(slot-name, "-init-function"),
			     source, function-ctype());
  fer-convert(builder, init-function, lexenv, #"let", var);

  let func-region
    = build-function-body(builder, policy, source, #t,
    			  fun-name, #(),
			  result-type, #f);
  let temp = make-local-var(builder, #"result", result-type);
  build-assignment(builder, policy, source, temp,
		   make-unknown-call(builder, var, #f, #()));
  build-return(builder, policy, source, func-region, temp);
  end-body(builder);
  make-function-literal(builder, #f, #"function", #"local",
			make(<signature>, specializers: #()),
			func-region);
end;


define method slot-accessor-standin
    (slot :: <instance-slot-info>, kind :: one-of(#"getter", #"setter"))
    => standin :: false-or(<ct-function>);
  if (instance?(slot, <vector-slot-info>))
    #f;
  elseif (find-slot-offset(slot, slot.slot-introduced-by))
    let rep = slot.slot-representation;
    let standin-name :: false-or(<symbol>)
      = if (rep == *general-rep*)
	  symcat("general-rep-", kind);
	elseif (rep == *heap-rep*)
	  symcat("heap-rep-", kind);
	else
	  #f;
	end if;
    if (standin-name)
      let defn = dylan-defn(standin-name);
      if (defn)
	defn.ct-value;
      else
	#f;
      end if;
    else
      #f;
    end if;
  end if;
end method slot-accessor-standin;



define method slot-accessor-standin // used for spew-object (cback)еее relevance???
    (slot :: <indirect-slot-info>, kind :: one-of(#"getter", #"setter"))
    => standin :: false-or(<ct-function>);


//  if (find-slot-offset(meta-slot /*slot*/, meta-class /*slot.slot-introduced-by*/))
/*    let rep = meta-slot.slot-representation;
    let standin-name :: false-or(<symbol>)
      = if (rep == *general-rep*)
	  symcat("general-rep-", kind);
	elseif (rep == *heap-rep*)
	  symcat("heap-rep-", kind);
	else
	  #f;
	end if;
    if (standin-name)
      let defn = dylan-defn(standin-name);
      if (defn)
	defn.ct-value;
      else
	#f;
      end if;
    else
      #f;
    end if;
  end if;*/
  
  #f
end method slot-accessor-standin;


define method might-be-in-data-word?
    (slot :: <slot-info>) => res :: <boolean>;
  //
  // For a slot to ever be in the data-word, it must be in the data-word of
  // the class that introduced it.
  slot.slot-introduced-by.data-word-slot == slot;
end method might-be-in-data-word?;


define method build-getter
    (builder :: <fer-builder>, ctv :: false-or(<ct-method>),
     defn :: <slot-defn>, slot :: <each-subclass-slot-info>)
    => res :: <method-literal>;
  let getter-name
      = make(<derived-name>, how: #"getter",
     	     base: defn.slot-defn-getter.defn-name);
  let lexenv = make(<lexenv>, method-name: getter-name);
  let policy = lexenv.lexenv-policy;
  let source = make(<source-location>);
  let cclass = slot.slot-introduced-by;
  let instance = make-lexical-var(builder, #"object", source, cclass);
  let index = #f;
  let type = slot.slot-type;
  let region = build-function-body
    (builder, policy, source, #f,
     getter-name,
     list(instance),
     type, #t);
  let result = make-local-var(builder, #"result", type);

/*  let slot-home
    = build-slot-home(slot.slot-getter.variable-name,
		      make-literal-constant(builder, cclass),
		      builder, policy, source);
*/



/* the below is for each-subclass slots!!!┤┤┤*/

  let slot-home
    = build-slot-home(slot.slot-getter.variable-name,
		      make-unknown-call
			(builder,
			 ref-dylan-defn(builder, policy, source, #"%object-class"),
			 #f,
			 list(instance)),
		      builder, policy, source);



  let offset = get-universal-position(slot.associated-meta-slot.slot-positions);

  build-assignment
    (builder, policy, source, result,
     make-operation
       (builder, <heap-slot-ref>,
	list(slot-home, make-literal-constant(builder, offset)),
	derived-type: type.ctype-extent,
	slot-info: slot.associated-meta-slot));


/*  local
    method get (offset :: <leaf>, init?-offset :: false-or(<leaf>)) => ();
      if (init?-offset)
	let init?-slot = slot.slot-initialized?-slot;
	let temp = make-local-var(builder, #"initialized?",
				  boolean-ctype());
	build-assignment
	  (builder, policy, source, temp,
	   make-operation
	     (builder, <heap-slot-ref>,
	      list(instance, init?-offset),
	      derived-type: init?-slot.slot-type.ctype-extent,
	      slot-info: init?-slot));
	build-if-body(builder, policy, source, temp);
	build-else(builder, policy, source);
	build-assignment
	  (builder, policy, source, #(),
	   make-error-operation
	     (builder, policy, source, #"uninitialized-slot-error",
	      make-literal-constant(builder, slot), instance));
	end-body(builder);
      end;
      let maybe-data-word? = slot.might-be-in-data-word?;
      if (maybe-data-word?)
	assert(~init?-offset);
	assert(~index);
	let temp = make-local-var(builder, #"data-word?",
				  boolean-ctype());
	build-assignment
	  (builder, policy, source, temp,
	   make-unknown-call
	     (builder, ref-dylan-defn(builder, policy, source, #"=="), #f,
	      list(offset,
		   make-literal-constant(builder, #"data-word"))));
	build-if-body(builder, policy, source, temp);
	build-assignment
	  (builder, policy, source, result,
	   make-operation
	     (builder, <data-word-ref>, list(instance),
	      derived-type: slot.slot-type.ctype-extent, slot-info: slot));
	build-else(builder, policy, source);
      end if;
      build-assignment
	(builder, policy, source, result,
	 make-operation
	   (builder, <heap-slot-ref>,
	    if (index)
	      list(instance, offset, index);
	    else
	      list(instance, offset);
	    end,
	    derived-type: slot.slot-type.ctype-extent,
	    slot-info: slot));
      if (maybe-data-word?)
	end-body(builder);
      end if;
      unless (init?-offset | slot-guaranteed-initialized?(slot, cclass))
	let temp = make-local-var(builder, #"initialized?", object-ctype());
	build-assignment(builder, policy, source, temp,
			 make-operation(builder, <primitive>, list(result),
					name: #"initialized?"));
	build-if-body(builder, policy, source, temp);
	build-else(builder, policy, source);
	build-assignment
	  (builder, policy, source, #(),
	   make-error-operation
	     (builder, policy, source, #"uninitialized-slot-error",
	      make-literal-constant(builder, slot), instance));
	end-body(builder);
      end;
    end;*/
//  build-slot-posn-dispatch(builder, slot, instance, get);

// build-slot-access(from metaclass)!!!

  build-return(builder, policy, source, region, result);
  end-body(builder);
  make-function-literal
    (builder, ctv, #"method", if (ctv) #"global" else #"local" end,
     make(<signature>,
	  specializers:
	    if (index)
	      list(cclass, specifier-type(#"<integer>"));
	    else
	      list(cclass);
	    end,
	  returns: type),
     region);
end;



define method build-getter
    (builder :: <fer-builder>, ctv :: false-or(<ct-method>),
     defn :: <slot-defn>, slot :: <class-slot-info>)
    => res :: <method-literal>;
  let getter-name
      = make(<derived-name>, how: #"getter",
     	     base: defn.slot-defn-getter.defn-name);
  let lexenv = make(<lexenv>, method-name: getter-name);
  let policy = lexenv.lexenv-policy;
  let source = make(<source-location>);
  let cclass = slot.slot-introduced-by;
  let instance = make-lexical-var(builder, #"object", source, cclass);
  let index = #f;
  let type = slot.slot-type;
  let region = build-function-body
    (builder, policy, source, #f,
     getter-name,
     list(instance),
     type, #t);
  let result = make-local-var(builder, #"result", type);

  let slot-home
    = build-slot-home(slot.slot-getter.variable-name,
		      make-literal-constant(builder, cclass),
		      builder, policy, source);

  let offset = get-universal-position(slot.associated-meta-slot.slot-positions);

  build-assignment
    (builder, policy, source, result,
     make-operation
       (builder, <heap-slot-ref>,
	list(slot-home, make-literal-constant(builder, offset)),
	derived-type: type.ctype-extent,
	slot-info: slot.associated-meta-slot));


/*  local
    method get (offset :: <leaf>, init?-offset :: false-or(<leaf>)) => ();
      if (init?-offset)
	let init?-slot = slot.slot-initialized?-slot;
	let temp = make-local-var(builder, #"initialized?",
				  boolean-ctype());
	build-assignment
	  (builder, policy, source, temp,
	   make-operation
	     (builder, <heap-slot-ref>,
	      list(instance, init?-offset),
	      derived-type: init?-slot.slot-type.ctype-extent,
	      slot-info: init?-slot));
	build-if-body(builder, policy, source, temp);
	build-else(builder, policy, source);
	build-assignment
	  (builder, policy, source, #(),
	   make-error-operation
	     (builder, policy, source, #"uninitialized-slot-error",
	      make-literal-constant(builder, slot), instance));
	end-body(builder);
      end;
      let maybe-data-word? = slot.might-be-in-data-word?;
      if (maybe-data-word?)
	assert(~init?-offset);
	assert(~index);
	let temp = make-local-var(builder, #"data-word?",
				  boolean-ctype());
	build-assignment
	  (builder, policy, source, temp,
	   make-unknown-call
	     (builder, ref-dylan-defn(builder, policy, source, #"=="), #f,
	      list(offset,
		   make-literal-constant(builder, #"data-word"))));
	build-if-body(builder, policy, source, temp);
	build-assignment
	  (builder, policy, source, result,
	   make-operation
	     (builder, <data-word-ref>, list(instance),
	      derived-type: slot.slot-type.ctype-extent, slot-info: slot));
	build-else(builder, policy, source);
      end if;
      build-assignment
	(builder, policy, source, result,
	 make-operation
	   (builder, <heap-slot-ref>,
	    if (index)
	      list(instance, offset, index);
	    else
	      list(instance, offset);
	    end,
	    derived-type: slot.slot-type.ctype-extent,
	    slot-info: slot));
      if (maybe-data-word?)
	end-body(builder);
      end if;
      unless (init?-offset | slot-guaranteed-initialized?(slot, cclass))
	let temp = make-local-var(builder, #"initialized?", object-ctype());
	build-assignment(builder, policy, source, temp,
			 make-operation(builder, <primitive>, list(result),
					name: #"initialized?"));
	build-if-body(builder, policy, source, temp);
	build-else(builder, policy, source);
	build-assignment
	  (builder, policy, source, #(),
	   make-error-operation
	     (builder, policy, source, #"uninitialized-slot-error",
	      make-literal-constant(builder, slot), instance));
	end-body(builder);
      end;
    end;*/
//  build-slot-posn-dispatch(builder, slot, instance, get);

// build-slot-access(from metaclass)!!!

  build-return(builder, policy, source, region, result);
  end-body(builder);
  make-function-literal
    (builder, ctv, #"method", if (ctv) #"global" else #"local" end,
     make(<signature>,
	  specializers:
	    if (index)
	      list(cclass, specifier-type(#"<integer>"));
	    else
	      list(cclass);
	    end,
	  returns: type),
     region);
end;


define method build-getter
    (builder :: <fer-builder>, ctv :: false-or(<ct-method>),
     defn :: <slot-defn>, slot :: <instance-slot-info>)
    => res :: <method-literal>;
  let getter-name
      = make(<derived-name>, how: #"getter",
     	     base: defn.slot-defn-getter.defn-name);
  let lexenv = make(<lexenv>, method-name: getter-name);
  let policy = lexenv.lexenv-policy;
  let source = make(<source-location>);
  let cclass = slot.slot-introduced-by;
  let instance = make-lexical-var(builder, #"object", source, cclass);
  let index = if (instance?(slot, <vector-slot-info>))
		make-lexical-var(builder, #"index", source,
				 specifier-type(#"<integer>"));
	      else
		#f;
	      end if;
  let type = slot.slot-type;
  let region = build-function-body
    (builder, policy, source, #f,
     getter-name,
     if (index)
       list(instance, index);
     else
       list(instance);
     end,
     type, #t);
  let result = make-local-var(builder, #"result", type);
  local
    method get (offset :: <leaf>, init?-offset :: false-or(<leaf>)) => ();
      if (init?-offset)
	let init?-slot = slot.slot-initialized?-slot;
	let temp = make-local-var(builder, #"initialized?", boolean-ctype());
	build-assignment
	  (builder, policy, source, temp,
	   make-operation
	     (builder, <heap-slot-ref>,
	      list(instance, init?-offset),
	      derived-type: init?-slot.slot-type.ctype-extent,
	      slot-info: init?-slot));
	build-if-body(builder, policy, source, temp);
	build-else(builder, policy, source);
	build-assignment
	  (builder, policy, source, #(),
	   make-error-operation
	     (builder, policy, source, #"uninitialized-slot-error",
	      make-literal-constant(builder, slot), instance));
	end-body(builder);
      end;
      let maybe-data-word? = slot.might-be-in-data-word?;
      if (maybe-data-word?)
	assert(~init?-offset);
	assert(~index);
	let temp = make-local-var(builder, #"data-word?", boolean-ctype());
	build-assignment
	  (builder, policy, source, temp,
	   make-unknown-call
	     (builder, ref-dylan-defn(builder, policy, source, #"=="), #f,
	      list(offset,
		   make-literal-constant(builder, #"data-word"))));
	build-if-body(builder, policy, source, temp);
	build-assignment
	  (builder, policy, source, result,
	   make-operation
	     (builder, <data-word-ref>, list(instance),
	      derived-type: type.ctype-extent, slot-info: slot));
	build-else(builder, policy, source);
      end if;
      build-assignment
	(builder, policy, source, result,
	 make-operation
	   (builder, <heap-slot-ref>,
	    if (index)
	      list(instance, offset, index);
	    else
	      list(instance, offset);
	    end,
	    derived-type: type.ctype-extent,
	    slot-info: slot));
      if (maybe-data-word?)
	end-body(builder);
      end if;
      unless (init?-offset | slot-guaranteed-initialized?(slot, cclass))
	let temp = make-local-var(builder, #"initialized?", object-ctype());
	build-assignment(builder, policy, source, temp,
			 make-operation(builder, <primitive>, list(result),
					name: #"initialized?"));
	build-if-body(builder, policy, source, temp);
	build-else(builder, policy, source);
	build-assignment
	  (builder, policy, source, #(),
	   make-error-operation
	     (builder, policy, source, #"uninitialized-slot-error",
	      make-literal-constant(builder, slot), instance));
	end-body(builder);
      end;
    end;
  build-slot-posn-dispatch(builder, slot, instance, get);
  build-return(builder, policy, source, region, result);
  end-body(builder);
  make-function-literal
    (builder, ctv, #"method", if (ctv) #"global" else #"local" end,
     make(<signature>,
	  specializers:
	    if (index)
	      list(cclass, specifier-type(#"<integer>"));
	    else
	      list(cclass);
	    end,
	  returns: type),
     region);
end;

define method build-getter
    (builder :: <fer-builder>, ctv :: false-or(<ct-method>),
     defn :: <slot-defn>, slot :: <slot-info>)
    => res :: <method-literal>;
  error("Unsupported slot type: %=", object-class(slot));
end;

define method build-setter
    (builder :: <fer-builder>, ctv :: false-or(<ct-method>),
     defn :: <slot-defn>, slot :: <slot-info>)
    => res :: <method-literal>;
  error("Unsupported slot type: %=", object-class(slot));
end;

define method build-setter
    (builder :: <fer-builder>, ctv :: false-or(<ct-method>),
     defn :: <slot-defn>, slot :: <each-subclass-slot-info>)
    => res :: <method-literal>;
  let setter-name
    = make(<derived-name>, how: #"setter",
     	   base: defn.slot-defn-setter.defn-name);
  let lexenv = make(<lexenv>, method-name: setter-name);
  let policy = lexenv.lexenv-policy;
  let source = make(<source-location>);
  let type = slot.slot-type;
  let new = make-lexical-var(builder, #"new-value", source, type);
  let cclass = slot.slot-introduced-by;
  let instance = make-lexical-var(builder, #"object", source, cclass);
  let index = #f;
  let region = build-function-body
    (builder, policy, source, #f,
     setter-name,
     list(new, instance),
     type, #t);
  let result = make-local-var(builder, #"result", type);

  let slot-home
    = build-slot-home(slot.slot-getter.variable-name,
		      make-unknown-call
			(builder,
			 ref-dylan-defn(builder, policy, source, #"%object-class"),
			 #f,
			 list(instance)),
		      builder, policy, source);

  let offset = get-universal-position(slot.associated-meta-slot.slot-positions);

  build-assignment(builder, policy, source, #(),
		   make-operation(builder, <heap-slot-set>,
				  list(new,
				       slot-home,
				       make-literal-constant(builder, offset)),
				  slot-info: slot.associated-meta-slot));

/*  local
    method set (offset :: <leaf>, init?-offset :: false-or(<leaf>)) => ();
      build-assignment(builder, policy, source, #(),
		       make-operation(builder, <heap-slot-set>,
				      if (index)
					list(new, instance, offset, index);
				      else
					list(new, instance, offset);
				      end if,
				      slot-info: slot));
      if (init?-offset)
	let init?-slot = slot.slot-initialized?-slot;
	let true-leaf = make-literal-constant(builder, #t);
	let init-op = make-operation
	  (builder, <heap-slot-set>, list(true-leaf, instance, init?-offset),
	   slot-info: init?-slot);
	build-assignment(builder, policy, source, #(), init-op);
      end;
    end;
*/

//  build-slot-posn-dispatch(builder, slot, instance, set);
  build-return(builder, policy, source, region, new);
  end-body(builder);
  make-function-literal
    (builder, ctv, #"method", if (ctv) #"global" else #"local" end,
     make(<signature>,
	  specializers:
	    if (index)
	      list(type, cclass, specifier-type(#"<integer>"));
	    else
	      list(type, cclass);
	    end,
	  returns: type),
     region);
end;



define method build-setter
    (builder :: <fer-builder>, ctv :: false-or(<ct-method>),
     defn :: <slot-defn>, slot :: <class-slot-info>)
    => res :: <method-literal>;
  let setter-name
    = make(<derived-name>, how: #"setter",
     	   base: defn.slot-defn-setter.defn-name);
  let lexenv = make(<lexenv>, method-name: setter-name);
  let policy = lexenv.lexenv-policy;
  let source = make(<source-location>);
  let type = slot.slot-type;
  let new = make-lexical-var(builder, #"new-value", source, type);
  let cclass = slot.slot-introduced-by;
  let instance = make-lexical-var(builder, #"object", source, cclass);
  let index = #f;
  let region = build-function-body
    (builder, policy, source, #f,
     setter-name,
     list(new, instance),
     type, #t);
  let result = make-local-var(builder, #"result", type);

  let slot-home
    = build-slot-home(slot.slot-getter.variable-name,
		      make-literal-constant(builder, cclass),
		      builder, policy, source);

  let offset = get-universal-position(slot.associated-meta-slot.slot-positions);

  build-assignment(builder, policy, source, #(),
		   make-operation(builder, <heap-slot-set>,
				  list(new,
				       slot-home,
				       make-literal-constant(builder, offset)),
				  slot-info: slot.associated-meta-slot));

/*  local
    method set (offset :: <leaf>, init?-offset :: false-or(<leaf>)) => ();
      build-assignment(builder, policy, source, #(),
		       make-operation(builder, <heap-slot-set>,
				      if (index)
					list(new, instance, offset, index);
				      else
					list(new, instance, offset);
				      end if,
				      slot-info: slot));
      if (init?-offset)
	let init?-slot = slot.slot-initialized?-slot;
	let true-leaf = make-literal-constant(builder, #t);
	let init-op = make-operation
	  (builder, <heap-slot-set>, list(true-leaf, instance, init?-offset),
	   slot-info: init?-slot);
	build-assignment(builder, policy, source, #(), init-op);
      end;
    end;
*/

//  build-slot-posn-dispatch(builder, slot, instance, set);
  build-return(builder, policy, source, region, new);
  end-body(builder);
  make-function-literal
    (builder, ctv, #"method", if (ctv) #"global" else #"local" end,
     make(<signature>,
	  specializers:
	    if (index)
	      list(type, cclass, specifier-type(#"<integer>"));
	    else
	      list(type, cclass);
	    end,
	  returns: type),
     region);
end;


define method build-setter
    (builder :: <fer-builder>, ctv :: false-or(<ct-method>),
     defn :: <slot-defn>, slot :: <instance-slot-info>)
    => res :: <method-literal>;
  let setter-name
    = make(<derived-name>, how: #"setter",
     	   base: defn.slot-defn-setter.defn-name);
  let init?-slot = slot.slot-initialized?-slot;
  let lexenv = make(<lexenv>, method-name: setter-name);
  let policy = lexenv.lexenv-policy;
  let source = make(<source-location>);
  let type = slot.slot-type;
  let new = make-lexical-var(builder, #"new-value", source, type);
  let cclass = slot.slot-introduced-by;
  let instance = make-lexical-var(builder, #"object", source, cclass);
  let index = if (instance?(slot, <vector-slot-info>))
		let fi = specifier-type(#"<integer>");
		let index = make-lexical-var(builder, #"index", source, fi);
		index;
	      else
		#f;
	      end if;
  let region = build-function-body
    (builder, policy, source, #f,
     setter-name,
     if (index)
       list(new, instance, index);
     else
       list(new, instance);
     end,
     type, #t);
  let result = make-local-var(builder, #"result", type);
  local
    method set (offset :: <leaf>, init?-offset :: false-or(<leaf>)) => ();
      build-assignment(builder, policy, source, #(),
		       make-operation(builder, <heap-slot-set>,
				      if (index)
					list(new, instance, offset, index);
				      else
					list(new, instance, offset);
				      end if,
				      slot-info: slot));
      if (init?-offset)
	let init?-slot = slot.slot-initialized?-slot;
	let true-leaf = make-literal-constant(builder, #t);
	let init-op = make-operation
	  (builder, <heap-slot-set>, list(true-leaf, instance, init?-offset),
	   slot-info: init?-slot);
	build-assignment(builder, policy, source, #(), init-op);
      end;
    end;
  build-slot-posn-dispatch(builder, slot, instance, set);
  build-return(builder, policy, source, region, new);
  end-body(builder);
  make-function-literal
    (builder, ctv, #"method", if (ctv) #"global" else #"local" end,
     make(<signature>,
	  specializers:
	    if (index)
	      list(type, cclass, specifier-type(#"<integer>"));
	    else
	      list(type, cclass);
	    end,
	  returns: type),
     region);
end;

define method build-slot-posn-dispatch
    (builder :: <fer-builder>, slot :: <instance-slot-info>,
     instance-leaf :: <leaf>, thunk :: <function>)
    => ();
  let cclass = slot.slot-introduced-by;
  if (cclass.all-subclasses-known? | cclass.primary?)
    // We don't have to do a runtime slot-position lookup, so make us a static
    // slot accessor method.
    let new-thunk
      = method (offset :: <slot-position>,
		init?-offset :: false-or(<slot-position>))
	    => ();
	  thunk(make-literal-constant(builder, offset),
		init?-offset
		  & make-literal-constant(builder, init?-offset));
	end method;
    let position = get-universal-position(slot.slot-positions);
    let init?-slot = slot.slot-initialized?-slot;
    let init?-position
      = (init?-slot & get-universal-position(init?-slot.slot-positions));
    if (position & (init?-slot == #f | init?-position))
      // The slot only ever shows up at one place.  So just use that one
      // place.
      new-thunk(position, init?-position);
    else
      // The slot shows up at multiple positions.  This had better only happen
      // when the class is sealed because we are only supposed to try making
      // a static posn-dispatch when the class is sealed or primary and if the
      // class were primary, then there should only be one possible position
      // for each slot.
      assert(cclass.sealed?);
      
      if (every?(disjoin(abstract?, unique-id), cclass.subclasses))
	// All the concrete subclasses have unique-id's, so we can compute a
	// direct mapping from instance.object-class.unique-id to offset.
	build-unique-id-slot-posn-dispatch
	  (builder, slot, instance-leaf, new-thunk);
      else
	// One or more concrete subclass doesn't have a unique-id so we have
	// to build an instance? tree.
	build-instance?-slot-posn-dispatch
	  (builder, slot, instance-leaf, new-thunk);
      end if;
    end if;
  else
    // Open non-primary class.
    build-runtime-slot-posn-dispatch(builder, slot, instance-leaf, thunk);
  end if;
end method build-slot-posn-dispatch;


define method build-unique-id-slot-posn-dispatch
    (builder :: <fer-builder>, slot :: <instance-slot-info>,
     instance-leaf :: <leaf>, thunk :: <function>)
    => ();
  let policy = $Default-Policy;
  let source = make(<source-location>);
  let cclass = slot.slot-introduced-by;
  let positions = slot.slot-positions;
  let init?-positions
    = (slot.slot-initialized?-slot
	 & slot.slot-initialized?-slot.slot-positions);
  let ranges = #();
  let prev = #f;
  for (entry in sort!(map(method (subclass)
			    let id = subclass.unique-id;
			    vector(id, id,
				   get-direct-position(positions, subclass),
				   init?-positions
				     & get-direct-position(init?-positions,
							   subclass));
			  end,
			  find-direct-classes(cclass)),
		      test: method (entry1, entry2)
			      entry1[0] < entry2[0];
			    end))
    if (prev == #f)
      ranges := list(entry);
      prev := ranges;
    elseif (prev.head[2] == entry[2] & prev.head[3] == entry[3])
      prev.head[1] := entry[1];
    else
      let new = list(entry);
      prev.tail := new;
      prev := new;
    end;
  finally
    let ranges = as(<simple-object-vector>, ranges);
    let less-then = ref-dylan-defn(builder, policy, source, #"<");
    //
    // Extract the unique id for this argument.
    let class-temp = make-local-var(builder, #"class", object-ctype());
    let obj-class-leaf
      = ref-dylan-defn(builder, policy, source, #"%object-class");
    build-assignment(builder, policy, source, class-temp,
		     make-unknown-call(builder, obj-class-leaf, #f,
				       list(instance-leaf)));
    let id-temp = make-local-var(builder, #"id", object-ctype());
    let unique-id-leaf
      = ref-dylan-defn(builder, policy, source, #"unique-id");
    build-assignment(builder, policy, source, id-temp,
		     make-unknown-call(builder, unique-id-leaf, #f,
				       list(class-temp)));
    local
      method split-range (min, max)
	if (min == max)
	  let entry :: <simple-object-vector> = ranges[min];
	  thunk(entry[2], entry[3]);
	else
	  let half-way-point = ash(min + max, -1);
	  let cond-temp = make-local-var(builder, #"cond", object-ctype());
	  let bound = make-literal-constant(builder,
					    ranges[half-way-point][1] + 1);
	  build-assignment
	    (builder, policy, source, cond-temp,
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
end method build-unique-id-slot-posn-dispatch;


define method build-instance?-slot-posn-dispatch
    (builder :: <fer-builder>, slot :: <instance-slot-info>,
     instance-leaf :: <leaf>, thunk :: <function>)
    => ();
  // XXX here used to be a break(), meaning that whoever debugged that thing didn't finish.
  // That is probably a bad sign.

  let policy = $Default-Policy;
  let source = make(<source-location>);
  let cclass = slot.slot-introduced-by;
  let positions = as(<list>, slot.slot-positions);
  let init?-positions
    = (slot.slot-initialized?-slot
	 & as(<list>, slot.slot-initialized?-slot.slot-positions));
  local
    method split (classes :: <list>, possible-splits :: <list>)
	=> ();
      let best-test = #f;
      let best-yes-classes = #f;
      let best-yes-count = #f;
      let best-no-classes = #f;
      let best-no-count = #f;
      let best-weight = 0;

      for (split :: <cclass> in possible-splits)
	let yes-classes = #();
	let no-classes = #();
	for (class in classes)
	  if (csubtype?(class, split))
	    yes-classes := pair(class, yes-classes);
	  else
	    no-classes := pair(class, no-classes);
	  end if;
	end for;
	let yes-count
	  = count-distinct-positions(yes-classes, positions, init?-positions);
	let no-count
	  = count-distinct-positions(no-classes, positions, init?-positions);
	let weight = yes-count * no-count;
	if (weight > best-weight)
	  best-test := split;
	  best-yes-classes := yes-classes;
	  best-yes-count := yes-count;
	  best-no-classes := no-classes;
	  best-no-count := no-count;
	  best-weight := weight;
	end if;
      end for;

      let cond-temp = make-local-var(builder, #"cond", object-ctype());
      let type-leaf = make-literal-constant(builder, best-test);
      let instance?-leaf
	= ref-dylan-defn(builder, policy, source, #"instance?");
      build-assignment
	(builder, policy, source, cond-temp,
	 make-unknown-call
	   (builder, instance?-leaf, #f,
	    list(instance-leaf, type-leaf)));
      build-if-body(builder, policy, source, cond-temp);

      if (best-yes-count == 1)
	let characteristic-class = best-yes-classes.first;
	thunk(lookup-position(characteristic-class, positions),
	      lookup-position(characteristic-class, init?-positions));
      else
	split(best-yes-classes,
	      restrict-splits(possible-splits, best-test, #t));
      end if;

      build-else(builder, policy, source);

      if (best-no-count == 1)
	let characteristic-class = best-no-classes.first;
	thunk(lookup-position(characteristic-class, positions),
	      lookup-position(characteristic-class, init?-positions));
      else
	split(best-no-classes,
	      restrict-splits(possible-splits, best-test, #f));
      end if;

      end-body(builder);
    end method split;

  let initial-splits = map(head, positions);
  if (init?-positions)
    for (entry in init?-positions)
      let split :: <cclass> = entry.head;
      unless (member?(split, initial-splits))
	initial-splits := pair(split, initial-splits);
      end unless;
    end for;
  end if;
  split(find-direct-classes(cclass),
	restrict-splits(initial-splits, cclass, #t));
end method build-instance?-slot-posn-dispatch;

define method lookup-position (class :: <cclass>, positions :: <list>)
    => res :: false-or(<integer>);
  block (return)
    for (entry in positions)
      if (csubtype?(class, entry.head))
	return(entry.tail);
      end if;
    end for;
    #f;
  end block;
end method lookup-position;

define method lookup-position (class :: <cclass>, positions :: <false>)
    => res :: false-or(<integer>);
  #f;
end method lookup-position;

define method restrict-splits
    (splits :: <list>, class :: <cclass>, if-yes? :: <boolean>)
    => res :: <list>;
  choose(method (split :: <cclass>) => res :: <boolean>;
	   split ~== class & csubtype?(split, class) == if-yes?;
	 end method,
	 splits);
end method restrict-splits;

define method count-distinct-positions
    (classes :: <list>, positions :: <list>,
     init?-positions :: false-or(<list>))
    => res :: <integer>;
  let entries = #();
  for (class in classes)
    let offset = lookup-position(class, positions);
    let init?-offset = lookup-position(class, init?-positions);
    block (next)
      for (entry :: <pair> in entries)
	if (entry.head == offset & entry.tail == init?-offset)
	  next();
	end if;
      end for;
      entries := pair(pair(offset, init?-offset), entries);
    end block;
  end for;
  entries.size;
end method count-distinct-positions;


define method build-runtime-slot-posn-dispatch
    (builder :: <fer-builder>, slot :: <instance-slot-info>,
     instance-leaf :: <leaf>, thunk :: <function>)
    => ();
  let policy = $Default-Policy;
  let source = make(<source-location>);

  let class-temp = make-local-var(builder, #"class", object-ctype());
  let obj-class-leaf
    = ref-dylan-defn(builder, policy, source, #"%object-class");
  build-assignment(builder, policy, source, class-temp,
		   make-unknown-call(builder, obj-class-leaf, #f,
				     list(instance-leaf)));

  local
    method make-offset-var
	(name :: <symbol>, slot :: false-or(<instance-slot-info>))
	=> var :: false-or(<abstract-variable>);
      if (slot)
	let var = make-local-var(builder, name,
				 if (slot.might-be-in-data-word?)
				   specifier-type
				     (#(union:, #"<integer>",
					#(singleton:, #"data-word")));
				 else
				   specifier-type(#"<integer>");
				 end if);
	build-assignment
	  (builder, policy, source, var,
	   make-unknown-call
	     (builder,
	      ref-dylan-defn(builder, policy, source, #"find-slot-offset"),
	      #f,
	      list(class-temp, make-literal-constant(builder, slot))));
	var;
      else
	#f;
      end if;
    end method make-offset-var;
  thunk(make-offset-var(#"offset", slot),
	make-offset-var(#"init?-offset", slot.slot-initialized?-slot));
end method build-runtime-slot-posn-dispatch;


// Dumping stuff.

// dump-od{<define-class-tlf>}
//
// We dump the a define-binding-tlf to establish the name of the
// <real-class-definition>.  Then we dump all the accessor method definitions
// to make sure they get re-instantiated.
//
define method dump-od (tlf :: <define-class-tlf>, state :: <dump-state>) => ();
  let defn = tlf.tlf-defn;
  dump-simple-object(#"define-binding-tlf", state, defn);
  for (slot in defn.class-defn-slots)
    unless (slot.slot-defn-allocation == #"virtual")
      let sealed? = slot.slot-defn-sealed?;
      let getter = slot.slot-defn-getter;
      if (getter.method-defn-of
	    & name-inherited-or-exported?(getter.defn-name))
	dump-od(slot.slot-defn-getter, state);
	if (sealed? & getter.method-defn-of.defn-library ~== defn.defn-library)
	  dump-simple-object(#"sealed-domain", state,
			     getter.method-defn-of,
			     defn.defn-library,
			     getter.function-defn-signature.specializers);
	end if;
      end;
      let setter = slot.slot-defn-setter;
      if (setter & setter.method-defn-of
	    & name-inherited-or-exported?(setter.defn-name))
	dump-od(setter, state);
	if (sealed? & setter.method-defn-of.defn-library ~== defn.defn-library)
	  dump-simple-object
	    (#"sealed-domain", state, setter.method-defn-of, defn.defn-library,
             setter.function-defn-signature.specializers);
	end if;
      end if;
    end unless;
  end for;
end method dump-od;

// These methods act like getters/setters on the <real-class-definition>, but
// really get/set slots in the cclass.  They are used so that we can dump
// cclass objects without having to reference non-type things.

define method class-defn-new-slot-infos
    (defn :: <real-class-definition>) => res :: <simple-object-vector>;
  let class = defn.class-defn-cclass;
  class & class.new-slot-infos;
end;

define method class-defn-new-slot-infos-setter
    (vec :: false-or(<simple-object-vector>), defn :: <real-class-definition>)
    => ();
  if (vec)
    defn.class-defn-cclass.new-slot-infos := vec;
  end;
end;

define method class-defn-all-slot-infos
    (defn :: <real-class-definition>) => res :: <simple-object-vector>;
  let class = defn.class-defn-cclass;
  class & class.all-slot-infos;
end;

define method class-defn-all-slot-infos-setter
    (vec :: false-or(<simple-object-vector>), defn :: <real-class-definition>)
    => ();
  if (vec)
    defn.class-defn-cclass.all-slot-infos := vec;
  end;
end;

define method class-defn-override-infos
    (defn :: <real-class-definition>) => res :: <simple-object-vector>;
  let class = defn.class-defn-cclass;
  class & class.override-infos;
end;

define method class-defn-override-infos-setter
    (vec :: false-or(<simple-object-vector>), defn :: <real-class-definition>)
    => ();
  if (vec)
    defn.class-defn-cclass.override-infos := vec;
  end;
end;

define method class-defn-keyword-infos
    (defn :: <real-class-definition>) => res :: <simple-object-vector>;
  let class = defn.class-defn-cclass;
  class & class.keyword-infos;
end;

define method class-defn-keyword-infos-setter
    (vec :: false-or(<simple-object-vector>), defn :: <real-class-definition>)
    => ();
  if (vec)
    defn.class-defn-cclass.keyword-infos := vec;
  end;
end;

define method class-defn-vector-slot
    (defn :: <real-class-definition>) => res :: false-or(<vector-slot-info>);
  let class = defn.class-defn-cclass;
  class & class.vector-slot;
end;

define method class-defn-vector-slot-setter
    (info :: false-or(<vector-slot-info>), defn :: <real-class-definition>)
    => ();
  let class = defn.class-defn-cclass;
  if (class)
    class.vector-slot := info;
  end;
end;

define constant $class-definition-slots
  = concatenate($definition-slots,
		list(class-defn-cclass, class:, #f,
		     class-defn-deferred-evaluations-function, #f,
		       %class-defn-deferred-evaluations-function-setter,
		     class-defn-key-defaulter-function, #f,
		       %class-defn-key-defaulter-function-setter,
		     class-defn-maker-function, #f,
		       %class-defn-maker-function-setter,
		     class-defn-new-slot-infos, #f,
		       class-defn-new-slot-infos-setter,
    /* ### -- currently recomputed, so we don't really need to dump them.
		     class-defn-all-slot-infos, #f,
		       class-defn-all-slot-infos-setter, */
		     class-defn-override-infos, #f,
		       class-defn-override-infos-setter,
		     class-defn-keyword-infos, #f,
		       class-defn-keyword-infos-setter
    /* ### -- currently recomputed, so we don't really need to dump them.
		     , class-defn-vector-slot, #f,
		       class-defn-vector-slot-setter */));

add-make-dumper(#"class-definition", *compiler-dispatcher*,
		<real-class-definition>, $class-definition-slots,
		load-external: #t,
		load-side-effect:
		  method (defn :: <real-class-definition>) => ();
		    let class = defn.class-defn-cclass;
		    if (class)
		      class.class-defn := defn;
		    end;
		  end);

add-make-dumper(#"class-definition", *compiler-dispatcher*,
		<local-class-definition>, $class-definition-slots,
		dumper-only: #t);

add-make-dumper(#"init-function-definition", *compiler-dispatcher*,
		<init-function-definition>,
		$abstract-method-definition-slots,
		load-external: #t);

add-make-dumper
  (#"maker-function-definition", *compiler-dispatcher*,
   <maker-function-definition>,
   concatenate
     ($abstract-method-definition-slots,
      list(maker-func-defn-class-defn, class-defn:,
	     maker-func-defn-class-defn-setter)),
   load-external: #t);

// Seals for file compiler/convert/defclass.dylan

// <define-class-parse> -- subclass of <definition-parse>
define sealed domain make(singleton(<define-class-parse>));
define sealed domain initialize(<define-class-parse>);
// <slot-parse> -- subclass of <abstract-slot-parse>
define sealed domain make(singleton(<slot-parse>));
// <inherited-slot-parse> -- subclass of <abstract-slot-parse>
define sealed domain make(singleton(<inherited-slot-parse>));
// <init-arg-parse> -- subclass of <abstract-slot-parse>
define sealed domain make(singleton(<init-arg-parse>));
// <real-class-definition> -- subclass of <class-definition>
define sealed domain make(singleton(<real-class-definition>));
define sealed domain initialize(<real-class-definition>);
// <local-class-definition> -- subclass of <real-class-definition>
define sealed domain make(singleton(<local-class-definition>));
// <slot-defn> -- subclass of <object>
define sealed domain make(singleton(<slot-defn>));
define sealed domain initialize(<slot-defn>);
// <override-defn> -- subclass of <object>
define sealed domain make(singleton(<override-defn>));
define sealed domain initialize(<override-defn>);
// <init-function-definition> -- subclass of <abstract-method-definition>
define sealed domain make(singleton(<init-function-definition>));
define sealed domain initialize(<init-function-definition>);
// <maker-function-definition> -- subclass of <abstract-method-definition>
define sealed domain make (singleton(<maker-function-definition>));
define sealed domain initialize (<maker-function-definition>);
