module: define-classes
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/convert/defclass.dylan,v 1.10 1995/03/04 22:00:28 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

define class <class-definition> (<abstract-constant-definition>)
  //
  // The <cclass> for this class definition, #f if unknown (e.g. non-constant
  // superclasses), #"not-computed-yet" if we haven't computed it yet, or
  // #"computing" if we are actively working on it.
  slot class-defn-cclass
    :: union(<cclass>, one-of(#f, #"not-computed-yet", #"computing")),
    init-value: #"not-computed-yet";
  // 
  // Vector of <expression>s for the superclasses.
  slot class-defn-supers :: <simple-object-vector>,
    required-init-keyword: supers:;
  //
  // Several boolean flags, just what the names say.
  slot class-defn-functional? :: <boolean>,
    required-init-keyword: functional:;
  slot class-defn-sealed? :: <boolean>,
    required-init-keyword: sealed:;
  slot class-defn-abstract? :: <boolean>,
    required-init-keyword: abstract:;
  slot class-defn-primary? :: <boolean>,
    required-init-keyword: primary:;
  //
  // Vector of the slots.
  slot class-defn-slots :: <simple-object-vector>,
    required-init-keyword: slots:;
  //
  // Vector of slot init value overrides.
  slot class-defn-overrides :: <simple-object-vector>,
    required-init-keyword: overrides:;
end;

define method defn-type (defn :: <class-definition>) => res :: <cclass>;
  dylan-value(#"<class>");
end;

define class <slot-defn> (<object>)
  //
  // The class that introduces this slot.
  slot slot-class :: <class-definition>;
  //
  // #t if this class is sealed, #f if not.  This really means that the added
  // methods are sealed.
  slot slot-sealed? :: <boolean>,
    required-init-keyword: sealed:;
  //
  // The allocation of this slot.
  slot slot-allocation :: one-of(#"instance", #"class", #"each-subclass",
				 #"constant", #"virtual"),
    required-init-keyword: allocation:;
  //
  // The expression to compute the type.
  slot slot-type-expr :: union(<expression>, <false>),
    required-init-keyword: type:;
  //
  // The type if it is compile-time evaluatable, and #f if not.
  slot slot-type :: union(<ctype>, <false>),
    init-value: #f;
  //
  // The name of the getter generic function.
  slot slot-getter-name :: <name>,
    required-init-keyword: getter-name:;
  //
  // The defn for the getter method added to that generic function, or #f
  // if we haven't computed the method yet.  Or if the slot is virtual, in
  // which case we don't give it a method.
  slot slot-getter-method :: union(<getter-method-definition>, <false>),
    init-value: #f;
  //
  // The name of the setter generic function, or #f if there is no setter.
  slot slot-setter-name :: union(<name>, <false>),
    required-init-keyword: setter-name:;
  //
  // The defn for the setter method added to that generic function, or #f
  // if there isn't a setter, we haven't computed the method yet, or the slot
  // is virtual.
  slot slot-setter-method :: union(<setter-method-definition>, <false>),
    init-value: #f;
  //
  // The init-value expression, or #f if one wasn't supplied.
  slot slot-init-value-expr :: union(<false>, <expression>),
    init-value: #f, init-keyword: init-value-expr:;
  //
  // The init-value if it can be represented as a compile-time value.  If it
  // can't we convert it into an init-function.
  slot slot-init-value :: union(<false>, <ct-value>),
    init-value: #f;
  //
  // The init-function, or #f if there isn't one.
  slot slot-init-function :: union(<expression>, <false>),
    init-value: #f, init-keyword: init-function:;
  //
  // The init-keyword, or #f if there isn't one.
  slot slot-init-keyword :: union(<literal-symbol>, <false>),
    init-value: #f, init-keyword: init-keyword:;
  //
  // #t if the init-keyword is required, #f if not.
  slot slot-init-keyword-required? :: <boolean>,
    init-value: #f, init-keyword: init-keyword-required:;
end;

define class <override-defn> (<object>)
  //
  // The class that introduces this override.
  slot override-class :: <class-definition>;
  //
  // The name of the getter.
  slot override-getter-name :: <name>,
    required-init-keyword: getter-name:;
  //
  // The init-value expression, or #f if none.
  slot override-init-value-expr :: union(<false>, <expression>),
    init-value: #f, init-keyword: init-value-expr:;
  //
  // The init value, if a compile-time constant.  If it wasn't a compile
  // time constant it is converted into an init function.
  slot override-init-value :: union(<false>, <ct-value>),
    init-value: #f;
  //
  // The init-function expression, or #f if none.
  slot override-init-function :: union(<false>, <expression>),
    init-value: #f, init-keyword: init-function:;
end;


define abstract class <accessor-method-definition> (<method-definition>)
  slot method-defn-slot :: <slot-defn>, required-init-keyword: slot:;
end;

define class <getter-method-definition> (<accessor-method-definition>)
end;

define class <setter-method-definition> (<accessor-method-definition>)
end;


define class <define-class-tlf> (<simple-define-tlf>)
  //
  // Make the definition required.
  required keyword defn:;
end;


// Top level form processing.

// During top level form processing, we parse the define class form and
// build the necessary <class-definition>, <slot-defn>, and <override-defn>
// objects.  We only check for syntactic errors and local semantic errors.
// By local semantic errors, I mean errors that can be detected by looking
// at nothing more than this class itself.
//
// We also note the class definition and any implicit definitions for slot
// accessors.

define method process-top-level-form (form :: <define-class-parse>) => ();
  let name = form.defclass-name.token-symbol;
  let (class-functional?, class-open?, class-sealed?, class-primary?,
       class-free?, class-abstract?, class-concrete?)
    = extract-modifiers("define class", name, form.define-modifiers,
			#"functional", #"open", #"sealed", #"primary", #"free",
			#"abstract", #"concrete");
  if (class-open? & class-sealed?)
    compiler-error("define class %s can't be both open and sealed.", name);
  end;
  if (class-primary? & class-free?)
    compiler-error("define class %s can't be both primary and free.", name);
  end;
  if (class-abstract? & class-concrete?)
    compiler-error("define class %s can't be both abstract and concrete.",
		   name);
  end;
  let slots = make(<stretchy-vector>);
  let overrides = make(<stretchy-vector>);
  unless (class-abstract?)
    add!(overrides,
	 make(<override-defn>,
	      getter-name: make(<basic-name>, symbol: #"%object-class",
				module: $Dylan-Module),
	      init-value-expr: make(<varref>, name: form.defclass-name)));
  end;
  for (option in form.defclass-options)
    select (option.classopt-kind)
      #"slot" =>
	let (open?, sealed?, allocation, type, setter, init-keyword,
	     req-init-keyword, init-value, init-function)
	  = extract-properties("slot spec", option.classopt-plist,
			       open:, sealed:, allocation:, type:, setter:,
			       init-keyword:, required-init-keyword:,
			       init-value:, init-function:);
	if (open? & sealed?)
	  compiler-error("Can't be both open and sealed.");
	end;
	let allocation = if (allocation)
			   allocation.varref-name.token-symbol;
			 else
			   #"instance";
			 end;
	let getter = option.classopt-name.token-symbol;
	unless (type == #f | instance?(type, <expression>))
	  compiler-error("Bogus type expression: %=", type);
	end;
	let setter = if (allocation == #"constant")
		       if (setter)
			 compiler-warning("Constant slots can't "
					    "have a setter.");
		       end;
		       #f;
		     elseif (class-functional? & allocation == #"instance")
		       if (setter)
			 compiler-warning("Instance allocation slots in "
					    "functional classes can't "
					    "have a setter");
		       end;
		       #f;
		     elseif (instance?(setter, <varref>))
		       setter.varref-name.token-symbol;
		     elseif (instance?(setter, <literal-ref>)
			       & setter.litref-literal = #f)
		       #f;
		     elseif (setter)
		       compiler-error("Bogus setter name: %=", setter);
		     else
		       as(<symbol>,
			  concatenate(as(<string>, getter), "-setter"));
		     end;
	if (init-value)
	  if (init-function)
	    compiler-error("Can't supply both an init-value: and an "
			     "init-function:.");
	  end;
	  if (req-init-keyword)
	    compiler-error("Can't supply both an init-value: and a "
			     "required-init-keyword:.");
	  end;
	  unless (instance?(init-value, <expression>))
	    compiler-error("Bogus init-value: %=", init-value);
	  end;
	elseif (init-function)
	  if (req-init-keyword)
	    compiler-error("Can't supply both an init-function: and a "
			     "required-init-keyword:.");
	  end;
	  unless (instance?(init-function, <expression>))
	    compiler-error("Bogus init-function: %=", init-function);
	  end;
	end;
	if (init-keyword)
	  if (req-init-keyword)
	    compiler-error("Can't supply both an init-keyword: and a "
			     "required-init-keyword:.");
	    unless (instance?(init-keyword, <literal-ref>)
		      & instance?(init-keyword.litref-literal, <literal-symbol>))
	      compiler-error("Bogus init-keyword: %=", init-keyword);
	    end;
	  end;
	elseif (req-init-keyword)
	  unless (instance?(req-init-keyword, <literal-ref>)
		    & instance?(req-init-keyword.litref-literal, <literal-symbol>))
	    compiler-error("Bogus required-init-keyword: %=",
			   req-init-keyword);
	  end;
	end;
	let getter-name = make(<basic-name>, symbol: getter,
			       module: *Current-Module*);
	let setter-name = setter & make(<basic-name>, symbol: setter,
					module: *Current-Module*);
	let slot = make(<slot-defn>,
			     sealed: ~open?,
			     allocation: allocation,
			     type: type,
			     getter-name: getter-name,
			     setter-name: setter-name,
			     init-value: init-value,
			     init-function: init-function,
			     init-keyword:
			       if (init-keyword)
				 init-keyword.litref-literal;
			       elseif (req-init-keyword)
				 req-init-keyword.litref-literal;
			       end,
			     init-keyword-required: req-init-keyword & #t);
	implicitly-define-generic(getter-name, 1, #f, #f);
	if (setter)
	  implicitly-define-generic(setter-name, 2, #f, #f);
	end;
	add!(slots, slot);

      #"inherited" =>
	let (init-value, init-function)
	  = extract-properties("inherited slot spec", option.classopt-plist,
			       init-value:, init-function:);
	if (init-value)
	  if (init-function)
	    compiler-error("Can't supply both an init-value: and an "
			     "init-function:");
	  end;
	  unless (instance?(init-value, <expression>))
	    compiler-error("Bogus init-value: %=", init-value);
	  end;
	elseif (init-function)
	  unless (instance?(init-function, <expression>))
	    compiler-error("Bogus init-function: %=", init-function);
	  end;
	end;
	add!(overrides,
	     make(<override-defn>,
		  getter-name:
		    make(<basic-name>,
			 symbol: option.classopt-name.token-symbol,
			 module: *Current-Module*),
		  init-value-expr: init-value,
		  init-function: init-function));

      #"keyword" =>
	unless (instance?(option.classopt-name, <literal-ref>)
		  & instance?(option.classopt-name.litref-literal, <literal-symbol>))
	  compiler-error("Bogus keyword: %=", option.classopt-name);
	end;
	let (required?, type, init-value, init-function)
	  = extract-properties("init arg spec", option.classopt-plist,
			       required:, type:, init-value:, init-function:);
	if (required?)
	  if (init-value)
	    compiler-error("Can't supply an init-value: for required keyword "
			     "init arg specs");
	  end;
	  if (init-function)
	    compiler-error("Can't supply an init-function: for required "
			     "keyword init arg specs");
	  end;
	elseif (init-value)
	  if (init-function)
	    compiler-error("Can't supply both an init-value: and an "
			     "init-function: for keyword init arg specs");
	  end;
	  unless (instance?(init-value, <expression>))
	    compiler-error("Bogus init-value: %=", init-value);
	  end;
	elseif (init-function)
	  unless (instance?(init-function, <expression>))
	    compiler-error("Bogus init-function: %=", init-function);
	  end;
	end;
	if (type)
	  unless (instance?(type, <expression>))
	    compiler-error("Bogus type: %=", type);
	  end;
	end;
	// ### Need to do something with it.
    end;
  end;
  let slots = as(<simple-object-vector>, slots);
  let overrides = as(<simple-object-vector>, overrides);
  let defn = make(<class-definition>,
		  name: make(<basic-name>,
			     symbol: name,
			     module: *Current-Module*),
		  supers: form.defclass-supers,
		  functional: class-functional?,
		  sealed: ~class-open?,
		  primary: class-primary?,
		  abstract: class-abstract?,
		  slots: slots,
		  overrides: overrides);
  for (slot in slots)
    slot.slot-class := defn;
  end;
  for (override in overrides)
    override.override-class := defn;
  end;
  note-variable-definition(defn);
  add!($Top-Level-Forms, make(<define-class-tlf>, defn: defn));
end;


// CT-Value

// Compute the compile-time value for a class definition.  This is the
// <cclass> object.  If we can't compute that for some reason, return #f
// to indicate that this class doesn't have a compile-time value.

define method ct-value (defn :: <class-definition>)
    => res :: union(<false>, <cclass>);
  select (defn.class-defn-cclass)
    #"not-computed-yet" =>
      defn.class-defn-cclass := compute-cclass(defn);
    #"computing" =>
      compiler-warning("class %s circularly defined.",
		       defn.defn-name.name-symbol);
      #f;
    otherwise =>
      defn.class-defn-cclass;
  end;
end;

define method compute-cclass (defn :: <class-definition>)
    => res :: union(<false>, <cclass>);
  block (return)
    //
    // Mark that we are trying to compute this class.
    defn.class-defn-cclass := #"computing";
    //
    // Evaluate the superclasses, giving up if any are unknown.
    let supers = map(method (super)
		       ct-eval(super, #f)
			 | begin
			     compiler-warning("Non-constant superclass");
			     return(#f);
			   end;
		     end,
		     defn.class-defn-supers);
    //
    // Check that everything is okay with the abstract adjective.
    if (defn.class-defn-abstract?)
      unless (every?(abstract?, supers))
	compiler-warning("Abstract classes can only inherit from other "
			   "abstract classes -- ignoring abstract abjective.");
	defn.class-defn-abstract? := #f;
      end;
    end;
    //
    // Check that everything is okay with the functional adjective.
    if (defn.class-defn-functional?)
      //
      // Make sure we arn't trying to inherit from anything we can't.
      if (any?(not-functional?, supers))
	compiler-warning("Functional classes can only inherit from other "
			   "functional classes and abstract classes without any "
			   "slots");
	return(#f);
      end;
      //
      // Add <functional-object> to our direct superclasses unless it is already
      // there.
      let functional-object = dylan-value(#"<functional-object>");
      unless (find-key(functional-object, supers))
	let object = dylan-value(#"<object>");
	let object-pos = find-key(object, supers);
	if (object-pos)
	  supers := concatenate(add(copy-sequence(supers, start: 0, end: object-pos),
				    functional-object),
				copy-sequence(supers, start: object-pos));
	else
	  supers := add(supers, functional-object);
	end;
      end;
    else
      //
      // It isn't a functional class, so make sure we arn't trying to inherit
      // from a functional class.
      if (any?(functional?, supers))
	compiler-warning("Functional classes can only be inherited from by "
			   "other functional classes.");
	return(#f);
      end;
    end;
    //
    // Check to make sure we don't try mixing two incompatible primary classes.
    let closest-super = #f;
    let closest-primary = #f;
    for (super in supers)
      let other-primary = super.closest-primary-superclass;
      if (~closest-primary | csubtype?(other-primary, closest-primary))
	closest-super := super;
	closest-primary := other-primary;
      elseif (~csubtype?(closest-primary, other-primary))
	local method describe (primary, super)
		if (primary == super)
		  as(<string>, primary.cclass-name.name-symbol);
		else
		  format-to-string("~= (inherited via ~s)",
				   primary.cclass-name.name-symbol,
				   super.cclass-name.name-symbol);
		end;
	      end;
	compiler-warning("Can't mix ~s and ~s because they are both primary",
			 describe(closest-primary, closest-super),
			 describe(other-primary, super));
	return(#f);
      end;
    end;
    if (closest-primary == #f)
      unless (defn == dylan-defn(#"<object>"))
	error("<object> isn't being inherited or isn't primary?");
      end;
    end;
    //
    // Make and return the <cclass>.
    make(<defined-cclass>,
	 name: defn.defn-name,
	 direct-superclasses: as(<list>, supers),
	 not-functional:
	   // Do we proclude functional subclasses?
	   if (defn.class-defn-functional?)
	     #f;
	   elseif (defn.class-defn-abstract?)
	     if (empty?(supers))
	       #f;
	     elseif (~empty?(defn.class-defn-slots))
	       #t;
	     else
	       any?(not-functional?, supers);
	     end;
	   else
	     #t;
	   end,
	 functional: defn.class-defn-functional?,
	 sealed: defn.class-defn-sealed?,
	 primary: defn.class-defn-primary?,
	 abstract: defn.class-defn-abstract?);
  end;
end;


// Top level form finalization.

define method finalize-top-level-form (tlf :: <define-class-tlf>) => ();
  let defn = tlf.tlf-defn;
  //
  // Compute the cclass if it hasn't been computed yet.
  let cclass
    = if (defn.class-defn-cclass == #"not-computed-yet")
	defn.class-defn-cclass := compute-cclass(defn);
      else
	defn.class-defn-cclass;
      end;
  //
  // Finalize the slots.
  for (slot in defn.class-defn-slots)
    //
    // Compute the type of the slot.
    let slot-type-expr = slot.slot-type-expr;
    let slot-type
      = if (slot-type-expr)
	  let type = ct-eval(slot-type-expr, #f);
	  instance?(type, <ctype>) & type;
	else
	  object-ctype();
	end;
    let hairy? = ~(cclass & slot-type);
    slot.slot-type := slot-type;
    //
    // Add the accessor methods.
    unless (slot.slot-allocation == #"virtual")
      slot.slot-getter-method
	:= make(<getter-method-definition>,
		base-name: slot.slot-getter-name,
		signature: make(<signature>,
				specializers:
				  list(cclass | make(<unknown-ctype>)),
				rest-type: #f,
				keys: #(),
				all-keys?: #f,
				returns: slot-type | make(<unknown-ctype>)),
		hairy: hairy?,
		sealed: slot.slot-sealed?,
		slot: slot);
      if (slot.slot-setter-name)
	slot.slot-setter-method
	  := make(<setter-method-definition>,
		  base-name: slot.slot-getter-name,
		  signature: make(<signature>,
				  specializers:
				    list(slot-type | make(<unknown-ctype>),
					 cclass | make(<unknown-ctype>)),
				  rest-type: #f,
				  keys: #(),
				  all-keys?: #f,
				  returns: slot-type | make(<unknown-ctype>)),
		  hairy: hairy?,
		  sealed: slot.slot-sealed?,
		  slot: slot);
      end;
    end;
    //
    // Compute the initial-value, converting it into an init-function if
    // necessary.
    let init-value-expr = slot.slot-init-value-expr;
    if (init-value-expr)
      let init-value = ct-eval(init-value-expr, #f);
      if (init-value)
	slot.slot-init-value := init-value;
      else
	slot.slot-init-function
	  := make(<funcall>,
		  function: make(<varref>, name: make-dylan-name(#"always")),
		  arguments: vector(init-value-expr));
      end;
    end;
  end;
end;


define method convert-top-level-form
    (builder :: <fer-builder>, tlf :: <define-class-tlf>) => ();
/*
  local method make-symbol-literal (sym)
	  make-literal-constant(builder, make(<ct-literal>, value: sym));
	end;
  let defn = tlf.tlf-defn;
  let lexenv = make(<lexenv>);
  let policy = lexenv.lexenv-policy;
  let source = make(<source-location>);
  let vector-leaf = dylan-defn-leaf(builder, #"vector");
  if (~ct-value(defn))
    let cclass-ctype = dylan-defn(#"<class>");
    let args = make(<stretchy-vector>);
    add!(args, dylan-defn-leaf(builder, #"%make-class"));
    add!(args, make-symbol-literal(defn.defn-name.name-symbol));
    begin
      let supers-args = make(<stretchy-vector>);
      add!(supers-args, vector-leaf);
      for (super in defn.class-defn-supers)
	let temp = make-local-var(builder, #"temp", cclass-ctype);
	fer-convert(builder, super, lexenv, #"assignment", temp);
	add!(supers-args, temp);
      end;
      let temp = make-local-var(builder, #"supers", object-ctype());
      build-assignment(builder, policy, source, temp,
		       make-operation(builder, as(<list>, supers-args)));
      add!(args, temp);
    end;
    build-assignment(builder, policy, source,
		     make-definition-leaf(builder, defn),
		     make-operation(builder, as(<list>, args)));
  end;

    begin
      add!(args, make-keyword-literal(slots:));
      let slots-args = make(<stretchy-vector>);
      add!(slots-args, dylan-defn-leaf(builder, #"vector"));
      for (slot in defn.class-defn-slots)
	let slot-args = make(<stretchy-vector>);
	add!(slot-args, dylan-defn-leaf(builder, #"vector"));
	add!(slot-args, make-keyword-literal(allocation:));
	add!(slot-args, make-keyword-literal(slot.slot-allocation));
	add!(slot-args, make-keyword-literal(getter:));
	begin
	  let name = slot.slot-getter-name;
	  let var = find-variable(name.name-module, name.name-symbol);
	  let defn = var & var.variable-definition;
	  if (defn)
	    add!(slot-args, make-definition-leaf(builder, defn));
	  else
	    error("No definition for %=, and can't implicitly define it.",
		  name);
	  end;
	end;
	if (slot.slot-setter-name)
	  let name = slot.slot-setter-name;
	  let var = find-variable(name.name-module, name.name-symbol);
	  let defn = var & var.variable-definition;
	  if (defn)
	    add!(slot-args, make-keyword-literal(setter:));
	    add!(slot-args, make-definition-leaf(builder, defn));
	  else
	    error("No definition for %=, and can't implicitly define it.",
		  name);
	  end;
	end;
	if (slot.slot-type-expr)
	  add!(slot-args, make-keyword-literal(type:));
	  if (slot.slot-type)
	    add!(slot-args,
		 make-literal-constant(builder, slot.slot-type));
	  else
	    let temp = make-local-var(builder, #"type",
				      dylan-value(#"<type>"));
	    fer-convert(builder, slot.slot-type-expr, lexenv,
			#"assignment", temp);
	    add!(slot-args, temp);
	  end;
	end;
	if (slot.slot-init-value)
	  add!(slot-args, make-keyword-literal(init-value:));
	  add!(slot-args,
	       fer-convert(builder, slot.slot-init-value, lexenv,
			   #"leaf", #"init-value"));
	end;
	if (slot.slot-init-function)
	  add!(slot-args, make-keyword-literal(init-function:));
	  let temp = make-local-var(builder, #"init-function",
				    function-ctype());
	  fer-convert(builder, slot.slot-init-function, lexenv,
		      #"assignment", temp);
	  add!(slot-args, temp);
	end;
	if (slot.slot-init-keyword)
	  add!(slot-args,
	       if (slot.slot-init-keyword-required?)
		 make-keyword-literal(required-init-keyword:);
	       else
		 make-keyword-literal(init-keyword:);
	       end);
	  add!(slot-args, make-keyword-literal(slot.slot-init-keyword));
	end;
	let temp = make-local-var(builder,
				  slot.slot-getter.defn-name.name-symbol,
				  object-ctype());
	build-assignment(builder, policy, source, temp,
			 make-operation(builder, as(<list>, slot-args)));
	add!(slots-args, temp);
      end;
      let temp = make-local-var(builder, #"slots", object-ctype());
      build-assignment(builder, policy, source, temp,
		       make-operation(builder, as(<list>, slots-args)));
      add!(args, temp);
    end;
  end;
*/
end;

