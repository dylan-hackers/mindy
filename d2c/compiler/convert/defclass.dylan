module: define-classes
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/convert/defclass.dylan,v 1.24 1995/05/29 20:59:09 wlott Exp $
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
  //
  // Defered evaluations function, of #f if there isn't one.
  slot class-defn-defered-evaluations-function :: false-or(<function-literal>),
    init-value: #f;
  //
  // The maker function, of #f if there isn't one.
  slot class-defn-maker-function :: false-or(<function-literal>),
    init-value: #f;
end;

define method defn-type (defn :: <class-definition>) => res :: <cclass>;
  dylan-value(#"<class>");
end;

define class <slot-defn> (<object>)
  //
  // The class that introduces this slot.
  slot slot-defn-class :: <class-definition>;
  //
  // #t if this slot is sealed, #f if not.  This really means that the getter
  // generic function is sealed on this class and the setter (if any) is sealed
  // on object and this class.
  slot slot-defn-sealed? :: <boolean>,
    required-init-keyword: sealed:;
  //
  // The allocation of this slot.
  slot slot-defn-allocation :: <slot-allocation>,
    required-init-keyword: allocation:;
  //
  // The expression to compute the type.
  slot slot-defn-type :: union(<expression>, <false>),
    required-init-keyword: type:;
  //
  // The name of the getter generic function.
  slot slot-defn-getter-name :: <name>,
    required-init-keyword: getter-name:;
  //
  // The getter method.  Filled in when computed.
  slot slot-defn-getter :: <getter-method-definition>;
  //
  // The name of the setter generic function, or #f if there is no setter.
  slot slot-defn-setter-name :: union(<name>, <false>),
    required-init-keyword: setter-name:;
  //
  // The setter method.  Filled in when computed.
  slot slot-defn-setter :: false-or(<setter-method-definition>);
  //
  // The init-value expression, or #f if one wasn't supplied.
  slot slot-defn-init-value :: union(<false>, <expression>),
    init-value: #f, init-keyword: init-value:;
  //
  // The init-function, or #f if there isn't one.
  slot slot-defn-init-function :: union(<expression>, <false>),
    init-value: #f, init-keyword: init-function:;
  //
  // The init-keyword, or #f if there isn't one.
  slot slot-defn-init-keyword :: union(<literal-symbol>, <false>),
    init-value: #f, init-keyword: init-keyword:;
  //
  // #t if the init-keyword is required, #f if not.
  slot slot-defn-init-keyword-required? :: <boolean>,
    init-value: #f, init-keyword: init-keyword-required:;
  //
  // The sizer slot defn.
  slot slot-defn-sizer-defn :: false-or(<slot-defn>),
    init-value: #f, init-keyword: sizer-defn:;

  // The slot-info for this slot, or #f if we haven't computed it or don't know
  // enough about the class to compute it at all.
  slot slot-defn-info :: union(<false>, <slot-info>),
    init-value: #f;
end;

define class <override-defn> (<object>)
  //
  // The class that introduces this override.
  slot override-defn-class :: <class-definition>;
  //
  // The name of the getter.
  slot override-defn-getter-name :: <name>,
    required-init-keyword: getter-name:;
  //
  // The init-value expression, or #f if none.
  slot override-defn-init-value :: union(<false>, <expression>),
    init-value: #f, init-keyword: init-value:;
  //
  // The init-function expression, or #f if none.
  slot override-defn-init-function :: union(<false>, <expression>),
    init-value: #f, init-keyword: init-function:;
  //
  // The <override-info> for this override, or #f if we haven't computed it
  // or don't know enough about the class to compute it at all.
  slot override-defn-info :: union(<false>, <override-info>),
    init-value: #f;
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
  unless (empty?(form.defclass-supers))
    add!(overrides,
	 make(<override-defn>,
	      getter-name: make(<basic-name>, symbol: #"%object-class",
				module: $Dylan-Module),
	      init-value: make(<varref>, id: form.defclass-name)));
  end;
  for (option in form.defclass-options)
    select (option.classopt-kind)
      #"slot" =>
	let (sealed?, allocation, type, setter, init-keyword,
	     req-init-keyword, init-value, init-function, sizer,
	     size-init-keyword, req-size-init-keyword,
	     size-init-value, size-init-function)
	  = extract-properties("slot spec", option.classopt-plist,
			       sealed:, allocation:, type:, setter:,
			       init-keyword:, required-init-keyword:,
			       init-value:, init-function:,
			       sizer:, size-init-keyword:,
			       required-size-init-keyword:,
			       size-init-value:, size-init-function:);
	let allocation = if (allocation)
			   allocation.varref-id.token-symbol;
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
		       setter.varref-id.token-symbol;
		     elseif (instance?(setter, <literal-ref>)
			       & setter.litref-literal = #f)
		       #f;
		     elseif (setter)
		       compiler-error("Bogus setter name: %=", setter);
		     else
		       symcat(getter, "-setter");
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
	  end;
	  unless (instance?(init-keyword, <literal-ref>)
		    & instance?(init-keyword.litref-literal,
				<literal-symbol>))
	    compiler-error("Bogus init-keyword: %=", init-keyword);
	  end;
	elseif (req-init-keyword)
	  unless (instance?(req-init-keyword, <literal-ref>)
		    & instance?(req-init-keyword.litref-literal,
				<literal-symbol>))
	    compiler-error("Bogus required-init-keyword: %=",
			   req-init-keyword);
	  end;
	end;

	let getter-name = make(<basic-name>, symbol: getter,
			       module: *Current-Module*);
	let setter-name = setter & make(<basic-name>, symbol: setter,
					module: *Current-Module*);

	let size-defn
	  = if (instance?(sizer, <varref>))
	      let sizer-name
		= make(<basic-name>, symbol: sizer.varref-id.token-symbol,
		       module: *Current-Module*);
	      
	      unless (allocation == #"instance")
		compiler-error("Only instance allocation slots can be "
				 "variable length.");
	      end;
	      
	      if (size-init-value)
		unless (instance?(size-init-value, <expression>))
		  compiler-error("Bogus size-init-value: %=", size-init-value);
		end;
		if (size-init-function)
		  compiler-error("Can't have both a size-init-value: and "
				   "size-init-function:");
		end;
	      elseif (size-init-function)
		unless (instance?(size-init-function, <expression>))
		  compiler-error("Bogus size-init-function: %=",
				 size-init-value);
		end;
	      elseif (~req-size-init-keyword)
		compiler-error("The Initial size must be supplied somehow.");
	      end;
	      
	      if (size-init-keyword)
		if (req-size-init-keyword)
		  compiler-error("Can't have both a size-init-keyword: and a "
				   "required-size-init-keyword:");
		end;
		unless (instance?(size-init-keyword, <literal-ref>)
			  & instance?(size-init-keyword.litref-literal,
				      <literal-symbol>))
		  compiler-error("Bogus size-init-keyword: %=",
				 size-init-keyword);
		end;
	      elseif (req-size-init-keyword)
		unless (instance?(req-size-init-keyword, <literal-ref>)
			  & instance?(req-size-init-keyword.litref-literal,
				      <literal-symbol>))
		  compiler-error("Bogus required-size-init-keyword: %=",
				 req-size-init-keyword);
		end;
	      end;
	      
	      let slot = make(<slot-defn>,
			      sealed: sealed? & #t,
			      allocation: allocation,
			      type:
				make(<varref>,
				     id: make(<name-token>,
					      symbol: #"<fixed-integer>",
					      module: $Dylan-Module,
					      uniquifier: make(<uniquifier>))),
			      getter-name: sizer-name,
			      setter-name: #f,
			      init-value: size-init-value,
			      init-function: size-init-function,
			      init-keyword:
				if (size-init-keyword)
				  size-init-keyword.litref-literal;
				elseif (req-size-init-keyword)
				  req-size-init-keyword.litref-literal;
				end,
			      init-keyword-required:
				req-size-init-keyword & #t);
	      add!(slots, slot);
	      slot;
	    else
	      unless (sizer == #f
			| (instance?(sizer, <literal-ref>)
			     & sizer.litref-literal = #f))
		compiler-error("Bogus sizer name: %=", sizer);
	      end;
	      if (size-init-value)
		compiler-error("Can't supply a size-init-value: without a "
				 "sizer: generic function");
	      end;
	      if (size-init-function)
		compiler-error("Can't supply a size-init-function: without a "
				 "sizer: generic function");
	      end;
	      if (size-init-keyword)
		compiler-error("Can't supply a size-init-keyword: without a "
				 "sizer: generic function");
	      end;
	      if (req-size-init-keyword)
		compiler-error("Can't supply a required-size-init-keyword: "
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
			init-value: init-value,
			init-function: init-function,
			init-keyword:
			  if (init-keyword)
			    init-keyword.litref-literal;
			  elseif (req-init-keyword)
			    req-init-keyword.litref-literal;
			  end,
			sizer-defn: size-defn,
			init-keyword-required: req-init-keyword & #t);
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
		  init-value: init-value,
		  init-function: init-function));

      #"keyword" =>
	unless (instance?(option.classopt-name, <literal-ref>)
		  & instance?(option.classopt-name.litref-literal,
			      <literal-symbol>))
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
    slot.slot-defn-class := defn;
  end;
  for (override in overrides)
    override.override-defn-class := defn;
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
			   "functional classes and abstract classes without "
			   "any slots");
	return(#f);
      end;
      //
      // Add <functional-object> to our direct superclasses unless it is
      // already there.
      let functional-object = dylan-value(#"<functional-object>");
      unless (member?(functional-object, supers))
	let object = dylan-value(#"<object>");
	let object-pos = find-key(supers, curry(\==, object));
	if (object-pos)
	  supers
	    := concatenate(add(copy-sequence(supers, start: 0,
					     end: object-pos),
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
	 defn: defn,
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
	 abstract: defn.class-defn-abstract?,
	 slots: map(compute-slot, defn.class-defn-slots),
	 overrides: map(compute-override, defn.class-defn-overrides));
  end;
end;

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
	     init-keyword: if (slot.slot-defn-init-keyword)
			     slot.slot-defn-init-keyword.literal-value;
			   else
			     #f;
			   end,
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
	     init-keyword: if (slot.slot-defn-init-keyword)
			     slot.slot-defn-init-keyword.literal-value;
			   else
			     #f;
			   end,
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



// Top level form finalization.

define method finalize-top-level-form (tlf :: <define-class-tlf>) => ();
  let defn = tlf.tlf-defn;
  //
  // Compute the cclass if it hasn't been computed yet.
  let cclass :: union(<false>, <cclass>)
    = if (defn.class-defn-cclass == #"not-computed-yet")
	defn.class-defn-cclass := compute-cclass(defn);
      else
	defn.class-defn-cclass;
      end;
  let class-type = cclass | make(<unknown-ctype>);
  //
  // Finalize the slots.
  for (slot in defn.class-defn-slots)
    finalize-slot(slot, cclass, class-type);
  end;

  // Finalize the overrides.
  for (override in defn.class-defn-overrides)
    // Fill in the <override-info> with the init value.
    let info = override.override-defn-info;
    if (info)
      if (override.override-defn-init-value)
	let init-val = ct-eval(override.override-defn-init-value, #f);
	if (init-val)
	  info.override-init-value := init-val;
	end;
      end;
      if (override.override-defn-init-function)
	let init-val = ct-eval(override.override-defn-init-function, #f);
	if (init-val)
	  info.override-init-function := init-val;
	end;
      end;
    end;
  end;
end;

define method finalize-slot
    (slot :: <slot-defn>, cclass :: <cclass>, class-type :: <ctype>) => ();
  //
  // Implicity define the accessor generics.
  if (slot.slot-defn-sizer-defn)
    implicitly-define-generic(slot.slot-defn-getter-name, 2, #f, #f);
    if (slot.slot-defn-setter-name)
      implicitly-define-generic(slot.slot-defn-setter-name, 3, #f, #f);
    end;
  else
    implicitly-define-generic(slot.slot-defn-getter-name, 1, #f, #f);
    if (slot.slot-defn-setter-name)
      implicitly-define-generic(slot.slot-defn-setter-name, 2, #f, #f);
    end;
  end;
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
	list(class-type, specifier-type(#"<fixed-integer>"));
      else
	list(class-type);
      end;

  // Fill in the <slot-info> with the type, init value, and init-function
  let info = slot.slot-defn-info;
  if (info)
    info.slot-type := slot-type;

    if (slot.slot-defn-init-value)
      let init-val = ct-eval(slot.slot-defn-init-value, #f);
      if (init-val)
	info.slot-init-value := init-val;
      end;
    elseif (slot.slot-defn-init-function)
      let init-val = ct-eval(slot.slot-defn-init-function, #f);
      if (init-val)
	info.slot-init-function := init-val;
      end;
    end;
  end;

  // Define the accessor methods.
  unless (slot.slot-defn-allocation == #"virtual")
    //
    // Are the accessor methods hairy?
    let hairy? = ~cclass | instance?(slot-type, <unknown-ctype>);
    //
    // Note: the act of making these method definitions associates them with
    // the appropriate generic function.
    slot.slot-defn-getter
      := make(<getter-method-definition>,
	      base-name: slot.slot-defn-getter-name,
	      signature: make(<signature>,
			      specializers: specializers,
			      returns: slot-type),
	      hairy: hairy?,
	      slot: info);
    if (slot.slot-defn-sealed?)
      let gf = slot.slot-defn-getter.method-defn-of;
      if (gf)
	add-seal(gf, specializers);
      end;
    end;
    slot.slot-defn-setter
      := if (slot.slot-defn-setter-name)
	   let defn = make(<setter-method-definition>,
			   base-name: slot.slot-defn-setter-name,
			   signature: make(<signature>,
					   specializers:
					     pair(slot-type, specializers),
					   returns: slot-type),
			   hairy: hairy?,
			   slot: info);
	   if (slot.slot-defn-sealed?)
	     let gf = defn.method-defn-of;
	     if (gf)
	       add-seal(gf, pair(object-ctype(), specializers));
	     end;
	   end;
	   defn;
	 else
	   #f;
	 end;
  end;
end;


// Top level form conversion.


define method convert-top-level-form
    (builder :: <fer-builder>, tlf :: <define-class-tlf>) => ();
  let defn = tlf.tlf-defn;
  let lexenv = make(<lexenv>);
  let policy = lexenv.lexenv-policy;
  let source = make(<source-location>);
  let cclass = ct-value(defn);
  if (cclass == #f)
    // The class is sufficiently hairy that we can't do anything.
    // Build top-level init code to create the class at runtime.
    error("### Can't deal with hairy classes yet.");
  else
    // The construction of the class object and the initialization of the class
    // variable will be handled by the linker.  We just need to build the
    // defered-evaluations, key-defaulter, and maker functions.

    let anything-non-constant? = #f;

    let %evals-slot-descriptors-leaf = #f;
    let %evals-override-descriptors-leaf = #f;

    local
      method make-descriptors-leaf (builder, what, for-class)
	let var = make-local-var(builder, symcat(what, "-descriptors"),
				 object-ctype());
	build-assignment
	  (builder, policy, source, var,
	   make-unknown-call
	     (builder,
	      dylan-defn-leaf(builder, symcat("class-", what, "-descriptors")),
	      #f,
	      list(make-literal-constant(builder, for-class))));
	var;
      end,
      method evals-slot-descriptors-leaf ()
	%evals-slot-descriptors-leaf
	  | (%evals-slot-descriptors-leaf
	       := make-descriptors-leaf(builder, "slot", cclass));
      end,
      method evals-override-descriptors-leaf ()
	%evals-override-descriptors-leaf
	  | (%evals-override-descriptors-leaf
	       := make-descriptors-leaf(builder, "override", cclass));
      end;

    for (slot-defn in defn.class-defn-slots)
      let slot-info = slot-defn.slot-defn-info;
      let slot-name = slot-info.slot-getter.variable-name;

      let %evals-slot-descriptor-leaf = #f;
      local
	method evals-slot-descriptor-leaf ()
	  if (%evals-slot-descriptor-leaf)
	    %evals-slot-descriptor-leaf;
	  else
	    let var = make-local-var(builder,
				     symcat(slot-name, "-descriptor"),
				     object-ctype());
	    let index = find-key(cclass.all-slot-infos, curry(\==, slot-info));
	    build-assignment
	      (builder, policy, source, var,
	       make-unknown-call
		 (builder,
		  dylan-defn-leaf(builder, #"element"),
		  #f,
		  list(evals-slot-descriptors-leaf(),
		       make-literal-constant
			 (builder, as(<ct-value>, index)))));
	    %evals-slot-descriptor-leaf := var;
	  end;
	end;

      let slot-type = slot-info.slot-type;
      let (type, type-var)
	= if (instance?(slot-type, <unknown-ctype>))
	    anything-non-constant? := #t;
	    let type-expr = slot-defn.slot-defn-type;
	    let var = make-local-var(builder, symcat(slot-name, "-type"),
				     specifier-type(#"<type>"));
	    fer-convert(builder, type-expr, lexenv, #"assignment", var);
	    build-assignment
	      (builder, policy, source, #(),
	       make-unknown-call
		 (builder, dylan-defn-leaf(builder, #"slot-type-setter"), #f,
		  list(var, evals-slot-descriptor-leaf())));
	    values(object-ctype(), var);
	  else
	    values(slot-type, #f);
	  end;

      let allocation = slot-defn.slot-defn-allocation;

      let init-value = slot-info.slot-init-value;
      let init-function = slot-info.slot-init-function;
      if (init-value == #t)
	anything-non-constant? := #t;
	let var = make-local-var(builder, symcat(slot-name, "-init-value"),
				 object-ctype());
	fer-convert(builder, slot-defn.slot-defn-init-value,
		    lexenv, #"assignment", var);
	build-assignment
	  (builder, policy, source, #(),
	   make-unknown-call
	     (builder,
	      dylan-defn-leaf(builder, #"init-value-or-function-setter"),
	      list(var, evals-slot-descriptor-leaf())));
      elseif (init-function == #t)
	anything-non-constant? := #t;
	let var = make-local-var(builder, symcat(slot-name, "-init-function"),
				 function-ctype());
	fer-convert(builder, slot-defn.slot-defn-init-function,
		    lexenv, #"assignment", var);
	build-assignment
	  (builder, policy, source, #(),
	   make-unknown-call
	     (builder,
	      dylan-defn-leaf(builder, #"init-value-or-function-setter"),
	      list(var, evals-slot-descriptor-leaf())));
      end;

      unless (allocation == #"virtual")
	if (defn.class-defn-sealed? | defn.class-defn-primary?)
	  if (type-var)
	    local
	      method build-call (name, #rest args)
		let temp = make-local-var(builder, name, object-ctype());
		build-assignment
		  (builder, lexenv.lexenv-policy, source, temp,
		   make-unknown-call(builder, dylan-defn-leaf(builder, name),
				     #f, as(<list>, args)));
		temp;
	      end,
	      method build-add-method (gf-name, method-leaf)
		// We don't use method-defn-of, because that is #f if there is
		// a definition but it isn't a define generic.
		let gf-var = find-variable(gf-name);
		let gf-defn = gf-var & gf-var.variable-definition;
		if (gf-defn)
		  let policy = $Default-Policy;
		  let source = make(<source-location>);
		  build-assignment
		    (builder, policy, source, #(),
		     make-unknown-call
		       (builder, dylan-defn-leaf(builder, #"add-method"), #f,
			list(make-definition-leaf(builder, gf-defn),
			     method-leaf)));
		else
		  error("No definition for %=, and can't "
			  "implicitly define it.",
			gf-name);
		end;
	      end;

	    let results = build-call(#"list", type-var);
	    let cclass-leaf = make-literal-constant(builder, cclass);
	    let false-leaf
	      = make-literal-constant(builder, as(<ct-value>, #f));
	    begin
	      let getter = build-getter(builder, slot-defn, slot-info);
	      let getter-specializers = build-call(#"list", cclass-leaf);
	      let meth = build-call(#"%make-method", getter-specializers,
				    results, false-leaf, getter);
	      build-add-method(slot-defn.slot-defn-getter-name, meth);
	    end;
	    if (slot-defn.slot-defn-setter)
	      let setter = build-setter(builder, slot-defn, slot-info);
	      let setter-specializers = build-call(#"list", type-var,
						   cclass-leaf);
	      let meth = build-call(#"%make-method", setter-specializers,
				    results, false-leaf, setter);
	      build-add-method(slot-defn.slot-defn-setter-name, meth);
	    end;
	  else
	    slot-defn.slot-defn-getter.method-defn-leaf
	      := build-getter(builder, slot-defn, slot-info);
	    if (slot-defn.slot-defn-setter)
	      slot-defn.slot-defn-setter.method-defn-leaf
		:= build-setter(builder, slot-defn, slot-info);
	    end;
	  end;
	else
	  error("### Can't deal with open free classes yet.");
	end;
      end;
    end;

    for (override-defn in defn.class-defn-overrides,
	 index from 0)
      let override-info = override-defn.override-defn-info;
      let slot-name = override-info.override-getter.variable-name;
      let init-value = override-info.override-init-value;
      let init-function = override-info.override-init-function;

      if (init-value == #t | init-function == #t)
	anything-non-constant? := #t;

	let descriptor-var
	  = make-local-var(builder,
			   symcat(slot-name, "-override-descriptor"),
			   object-ctype());
	build-assignment
	  (builder, policy, source, descriptor-var,
	   make-unknown-call
	     (builder,
	      dylan-defn-leaf(builder, #"element"),
	      #f,
	      list(evals-override-descriptors-leaf(),
		   make-literal-constant
		     (builder, as(<ct-value>, index)))));

	if (init-value)
	  let var = make-local-var(builder,
				   symcat(slot-name, "-override-init-value"),
				   object-ctype());
	  fer-convert(builder, override-defn.override-defn-init-value, lexenv,
		      #"assignment", var);
	  build-assignment
	    (builder, policy, source, #(),
	     make-unknown-call
	       (builder,
		dylan-defn-leaf(builder, #"init-value-or-function-setter"),
		list(var, descriptor-var)));
	else
	  let var = make-local-var(builder,
				   symcat(slot-name,
					  "-override-init-function"),
				   function-ctype());
	  fer-convert(builder, override-defn.override-defn-init-function,
		      lexenv, #"assignment", var);
	  build-assignment
	    (builder, policy, source, #(),
	     make-unknown-call
	       (builder,
		dylan-defn-leaf(builder, #"init-value-or-function-setter"),
		list(var, descriptor-var)));
	end;
      end;
    end;

    // ### Build the key-defaulter (if concrete)

    // Build the maker (if concrete) and do any slot processing that
    // has to happen for every slot in the class.

    let representation = pick-representation(cclass, #"speed");
    let data-word? = instance?(representation, <data-word-representation>);
    let key-infos = make(<stretchy-vector>);
    let maker-args = make(<stretchy-vector>);
    let maker-builder = make-builder(builder);
    let init-builder = make-builder(builder);
    let instance-leaf
      = unless (cclass.abstract?
		  | (instance?(representation, <immediate-representation>)
		       & ~data-word?))
	  make-local-var(init-builder, #"instance", cclass);
	end;
    let size-leaf = #f;
    let vector-slot = cclass.vector-slot;
    let size-slot = vector-slot & vector-slot.slot-size-slot;

    let %maker-slot-descriptors-leaf = #f;
    let %maker-override-descriptors-leaves = #();

    local
      method maker-slot-descriptors-leaf ()
	%maker-slot-descriptors-leaf
	  | (%maker-slot-descriptors-leaf
	       := make-descriptors-leaf(maker-builder, "slot", cclass));
      end,
      method maker-override-descriptors-leaf (introducer :: <cclass>)
	block (return)
	  for (entry in %maker-override-descriptors-leaves)
	    if (entry.head == introducer)
	      return(entry.tail);
	    end;
	  end;
	  let new
	    = make-descriptors-leaf(maker-builder, "override", introducer);
	  %maker-override-descriptors-leaves
	    := pair(pair(introducer, new), %maker-override-descriptors-leaves);
	  new;
	end;
      end;

    for (slot in cclass.all-slot-infos, index from 0)
      let slot-name = slot.slot-getter.variable-name;
      let %maker-slot-descriptor-leaf = #f;
      local
	method maker-slot-descriptor-leaf ()
	  if (%maker-slot-descriptor-leaf)
	    %maker-slot-descriptor-leaf;
	  else
	    let var = make-local-var(maker-builder,
				     symcat(slot-name, "-descriptor"),
				     object-ctype());
	    build-assignment
	      (maker-builder, policy, source, var,
	       make-unknown-call
		 (maker-builder,
		  dylan-defn-leaf(maker-builder, #"element"),
		  #f,
		  list(maker-slot-descriptors-leaf(),
		       make-literal-constant
			 (maker-builder, as(<ct-value>, index)))));
	    %maker-slot-descriptor-leaf := var;
	  end;
	end;
	    
      select (slot by instance?)
	<instance-slot-info> =>
	  //
	  // If instance-leaf if #f, then we are an abstract class.
	  // 
	  // If there isn't a getter, this is a bound? slot.  Bound? slots
	  // are initialized along with the regular slot.
	  //
	  // If this class is represented as a data-word, we want to ignore
	  // the %object-class slot.
	  // 
	  if (instance-leaf & slot.slot-getter
		& ~(data-word? & zero?(index)))
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
			dylan-defn-leaf(maker-builder, #"slot-type"),
			#f,
			list(maker-slot-descriptor-leaf())));
		  values(object-ctype(), var);
		else
		  values(slot-type, #f);
		end;

	    let override
	      = block (return)
		  for (override in slot.slot-overrides)
		    if (csubtype?(cclass, override.override-introduced-by))
		      return(override);
		    end;
		  finally
		    #f;
		  end;
		end;

	    let (init-value, init-function)
	      = if (override)
		  values(override.override-init-value,
			 override.override-init-function);
		else
		  values(slot.slot-init-value, slot.slot-init-function);
		end;

	    local
	      method build-slot-init
		  (slot :: false-or(<slot-info>), leaf :: <leaf>) => ();
		if (slot)
		  if (data-word?)
		    assert(index == 1);
		    build-assignment
		      (init-builder, policy, source, instance-leaf,
		       make-operation
			 (init-builder, <primitive>, list(leaf),
			  name: #"make-data-word-instance",
			  derived-type: cclass));
		  else
		    let posn
		      = block (return)
			  for (entry in slot.slot-positions)
			    if (csubtype?(cclass, entry.head))
			      return(entry.tail);
			    end;
			  end;
			  error("Couldn't find the position for %s",
				slot.slot-getter.variable-name);
			end;
		    if (instance?(slot, <vector-slot-info>))
		      // We need to build a loop to initialize every element.
		      let block-region = build-block-body(init-builder, policy,
							  source);
		      let index
			= make-local-var(init-builder, #"index",
					 specifier-type(#"<fixed-integer>"));
		      build-assignment
			(init-builder, policy, source, index,
			 make-literal-constant
			   (init-builder, as(<ct-value>, 0)));
		      build-loop-body(init-builder, policy, source);
		      let more? = make-local-var(init-builder, #"more?",
						specifier-type(#"<boolean>"));
		      build-assignment
			(init-builder, policy, source, more?,
			 make-unknown-call
			   (init-builder, dylan-defn-leaf(init-builder, #"<"),
			    #f, list(index, size-leaf)));
		      build-if-body(init-builder, policy, source, more?);
		      build-assignment
			(init-builder, policy, source, #(),
			 make-operation(init-builder, <slot-set>,
					list(leaf, instance-leaf, index),
					slot-info: slot,
					slot-offset: posn));
		      build-assignment
			(init-builder, policy, source, index,
			 make-unknown-call
			   (init-builder, dylan-defn-leaf(init-builder, #"+"),
			    #f, list(index,
				     make-literal-constant
				       (init-builder, as(<ct-value>, 1)))));
		      build-else(init-builder, policy, source);
		      build-exit(init-builder, policy, source, block-region);
		      end-body(init-builder);
		      end-body(init-builder);
		      end-body(init-builder);
		    else
		      build-assignment
			(init-builder, policy, source, #(),
			 make-operation(init-builder, <slot-set>,
					list(leaf, instance-leaf),
					slot-info: slot,
					slot-offset: posn));
		      if (slot == size-slot)
			size-leaf := leaf;
		      end;
		    end;
		  end;
		end;
	      end,
	      method extract-init-value-or-function (init-value-var) => ();
		if (init-value == #t | init-function == #t)
		  anything-non-constant? := #t;
		  let descriptor-leaf
		    = if (override)
			let debug-name = symcat(slot-name, "-descriptor");
			let var = make-local-var(maker-builder, debug-name,
						 object-ctype());
			let introducer = override.override-introduced-by;
			build-assignment
			  (maker-builder, policy, source, var,
			   make-unknown-call
			     (maker-builder,
			      dylan-defn-leaf(maker-builder, #"element"),
			      #f,
			      list(maker-override-descriptors-leaf(introducer),
				   make-literal-constant
				     (maker-builder,
				      as(<ct-value>,
					 find-key(introducer.override-infos,
						  curry(\==, override)))))));
			var;
		      else
			maker-slot-descriptor-leaf();
		      end;
		  let init-function-var
		    = (init-function
			 & make-local-var(maker-builder,
					  symcat(slot-name, "-init-function"),
					  function-ctype()));
		  build-assignment
		    (maker-builder, policy, source,
		     init-function-var | init-value-var,
		     make-unknown-call
		       (maker-builder,
			dylan-defn-leaf(maker-builder,
					#"init-value-or-function"),
			#f,
			list(descriptor-leaf)));
		  if (init-function)
		    build-assignment
		      (maker-builder, policy, source, init-value-var,
		       make-unknown-call(maker-builder, init-function-var,
					 #f, #()));
		  end;
		elseif (init-value)
		  build-assignment
		    (maker-builder, policy, source, init-value-var,
		     make-literal-constant(maker-builder, init-value));
		elseif (init-function)
		  let init-func-leaf
		    = make-literal-constant(maker-builder, init-function);
		  build-assignment
		    (maker-builder, policy, source, init-value-var,
		     make-unknown-call(maker-builder, init-func-leaf, #f,
				       #()));
		else
		  error("shouldn't have called extract-init-value-or-function "
			  "when neither init-value nor init-function is true");
		end;

		if (type-var)
		  build-assignment
		    (maker-builder, policy, source, init-value-var,
		     make-check-type-operation
		       (maker-builder, init-value-var, type-var));
		end;
	      end;
	    
	    if (slot.slot-init-keyword)
	      let key = slot.slot-init-keyword;
	      let default = ~(init-value == #t) & init-value;
	      let init-value-var
		= make-local-var(maker-builder,
				 symcat(slot-name, "-init-value"),
				 type);
	      let key-info = make(<key-info>, key-name: key, type: type,
				  required: slot.slot-init-keyword-required?,
				  default: default, supplied?-var: ~default);
	      add!(key-infos, key-info);
	      if (default)
		add!(maker-args, init-value-var);
		build-slot-init(slot, init-value-var);
		build-slot-init(slot.slot-initialized?-slot,
				make-literal-constant(init-builder,
						      as(<ct-value>, #t)));
	      else
		let arg = make-local-var(maker-builder, key, type);
		add!(maker-args, arg);
		let supplied?-arg
		  = make-local-var(maker-builder,
				   symcat(key, "-supplied?"),
				   specifier-type(#"<boolean>"));
		add!(maker-args, supplied?-arg);
		build-if-body(maker-builder, policy, source, supplied?-arg);
		build-assignment(maker-builder, policy, source,
				 init-value-var, arg);
		build-else(maker-builder, policy, source);
		if (init-value | init-function)
		  extract-init-value-or-function(init-value-var);
		elseif (slot.slot-init-keyword-required?)
		  build-assignment
		    (maker-builder, policy, source, #(),
		     make-error-operation
		       (maker-builder, "Missing required-init-keyword %=",
			make-literal-constant
			  (maker-builder, as(<ct-value>, key))));
		else
		  build-assignment(maker-builder, policy, source,
				   init-value-var,
				   make(<uninitialized-value>,
					derived-type: type));
		end;
		end-body(maker-builder);
		build-slot-init(slot, init-value-var);
		build-slot-init(slot.slot-initialized?-slot,
				if (init-value | init-function)
				  make-literal-constant(init-builder,
							as(<ct-value>, #t));
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
		extract-init-value-or-function(init-value-var);
		build-slot-init(slot, init-value-var);
		build-slot-init(slot.slot-initialized?-slot,
				make-literal-constant(init-builder,
						      as(<ct-value>, #t)));
	      else
		build-slot-init
		  (slot, make(<uninitialized-value>, derived-type: type));
		build-slot-init
		  (slot.slot-initialized?-slot,
		   make-literal-constant(init-builder, as(<ct-value>, #f)));
	      end;
	    end;
	  end;
	<each-subclass-slot-info> =>
	  // ### Add stuff to the derived-evaluations function to init the
	  // slot.  If the slot is keyword-initializable, add stuff to the
	  // maker to check for that keyword and change the each-subclass
	  // slot.
	  error("Can't deal with each-subclass slots yet.");
	<class-slot-info> =>
	  // ### If the slot is keyword-initializable, add stuff to the maker
	  // to check for that keyword and change the class slot.
	  error("Can't deal with class slots yet.");
	<constant-slot-info>, <virtual-slot-info> =>
	  // Don't need to do anything for constant or virtual slots.
	  #f;
      end;
    end;
    
    let maker-leaf
      = if (instance-leaf)
	  let name = format-to-string("Maker for %s", defn.defn-name);
	  let maker-region
	    = build-function-body(builder, policy, source, name,
				  as(<list>, maker-args), #"best");
	  build-region(builder, builder-result(maker-builder));
	  let bytes = cclass.instance-slots-layout.layout-length;
	  let base-len = make-literal-constant(builder, as(<ct-value>, bytes));
	  let len-leaf
	    = if (vector-slot)
		let fi = specifier-type(#"<fixed-integer>");
		let elsize
		  = vector-slot.slot-representation.representation-size;
		let extra
		  = if (elsize == 1)
		      size-leaf;
		    else
		      let var = make-local-var(builder, #"extra", fi);
		      let elsize-leaf
			= make-literal-constant(builder,
						as(<ct-value>, elsize));
		      build-assignment
			(builder, policy, source, var,
			 make-unknown-call
			   (builder, dylan-defn-leaf(builder, #"*"), #f,
			    list(size-leaf, elsize-leaf)));
		      var;
		    end;
		let var = make-local-var(builder, #"bytes", fi);
		build-assignment
		  (builder, policy, source, var,
		   make-unknown-call
		     (builder, dylan-defn-leaf(builder, #"+"), #f,
		      list(base-len, extra)));
		var;
	      else
		base-len;
	      end;
	  unless (data-word?)
	    build-assignment
	      (builder, policy, source, instance-leaf,
	       make-operation
		 (builder, <primitive>, list(len-leaf),
		  name: #"allocate", derived-type: cclass));
	  end;
	  build-region(builder, builder-result(init-builder));
	  build-return(builder, policy, source, maker-region,
		       list(instance-leaf));
	  end-body(builder);
	  make-function-literal(builder, #"global",
				make(<signature>, specializers: #(),
				     keys: as(<list>, key-infos),
				     all-keys: #t,
				     returns: cclass),
				maker-region);
	end;
    
    if (anything-non-constant?)
      let guts = builder-result(builder);
      let name = format-to-string("Defered evaluations for %s",
				  defn.defn-name);
      let func-region = build-function-body(builder, policy, source, name, #(),
					    #"best");
      // Replace the defered-evaluations function with one that signals
      // an error.
      let flame-region = build-function-body(builder, policy, source,
					     "Anonymous Method", #(), #"best");
      build-assignment
	(builder, policy, source, #(),
	 make-error-operation
	   (builder,
	    "Circularity detected while processing the defered evaluations "
	      "for %=",
	    make-literal-constant(builder, cclass)));
      end-body(builder);
      let defered-evaluations-setter-leaf
	= dylan-defn-leaf(builder, #"class-defered-evaluations-setter");
      build-assignment
	(builder, policy, source, #(),
	 make-unknown-call
	   (builder, defered-evaluations-setter-leaf, #f,
	    list(make-literal-constant(builder, cclass),
		 make-function-literal(builder, #"local",
				       make(<signature>, specializers: #()),
				       flame-region))));
      // Splice in the guts we saved earlier.
      build-region(builder, guts);
      // ### install the key-defaulter function.
      // Install the maker function.
      build-assignment
	(builder, policy, source, #(),
	 make-unknown-call
	   (builder, dylan-defn-leaf(builder, #"class-maker-setter"), #f,
	    list(make-literal-constant(builder, cclass),
		 make-literal-constant(builder, as(<ct-value>, #f)))));
      // Clear the defered-evaluations function.
      build-assignment
	(builder, policy, source, #(),
	 make-unknown-call
	   (builder, defered-evaluations-setter-leaf, #f,
	    list(make-literal-constant(builder, cclass),
		 make-literal-constant(builder, as(<ct-value>, #f)))));
      // Return nothing.
      build-return(builder, policy, source, func-region, #());
      end-body(builder);
      defn.class-defn-defered-evaluations-function
	:= make-function-literal(builder, #"global",
				 make(<signature>, specializers: #()),
				 func-region);
    else
      // ### fill in the key-defaulter function.
      defn.class-defn-maker-function := maker-leaf;
    end;
  end;
end;

/*
define method build-hairy-class (???)
  local method make-symbol-literal (sym)
	  make-literal-constant(builder, make(<ct-literal>, value: sym));
	end;
  let vector-leaf = dylan-defn-leaf(builder, #"vector");
  if (~ct-value(defn))
    let cclass-ctype = dylan-defn(#"<class>");
    let args = make(<stretchy-vector>);
    add!(args, make-symbol-literal(defn.defn-name.name-symbol));
    begin
      let supers-args = make(<stretchy-vector>);
      for (super in defn.class-defn-supers)
	let temp = make-local-var(builder, #"temp", cclass-ctype);
	fer-convert(builder, super, lexenv, #"assignment", temp);
	add!(supers-args, temp);
      end;
      let temp = make-local-var(builder, #"supers", object-ctype());
      build-assignment(builder, policy, source, temp,
		       make-unknown-call(builder, vector-leaf, #f,
					 as(<list>, supers-args)));
      add!(args, temp);
    end;
    build-assignment
      (builder, policy, source, make-definition-leaf(builder, defn),
       make-unknown-call(builder, dylan-defn-leaf(builder, #"%make-class"), #f,
			 as(<list>, args)));
  end;

    begin
      add!(args, make-keyword-literal(slots:));
      let slots-args = make(<stretchy-vector>);
      add!(slots-args, dylan-defn-leaf(builder, #"vector"));
      for (slot in defn.class-defn-slots)
	let slot-args = make(<stretchy-vector>);
	add!(slot-args, dylan-defn-leaf(builder, #"vector"));
	add!(slot-args, make-keyword-literal(allocation:));
	add!(slot-args, make-keyword-literal(slot.slot-defn-allocation));
	add!(slot-args, make-keyword-literal(getter:));
	begin
	  let name = slot.slot-defn-getter-name;
	  let var = find-variable(name);
	  let defn = var & var.variable-definition;
	  if (defn)
	    add!(slot-args, make-definition-leaf(builder, defn));
	  else
	    error("No definition for %=, and can't implicitly define it.",
		  name);
	  end;
	end;
	if (slot.slot-defn-setter-name)
	  let name = slot.slot-defn-setter-name;
	  let var = find-variable(name);
	  let defn = var & var.variable-definition;
	  if (defn)
	    add!(slot-args, make-keyword-literal(setter:));
	    add!(slot-args, make-definition-leaf(builder, defn));
	  else
	    error("No definition for %=, and can't implicitly define it.",
		  name);
	  end;
	end;
	if (slot.slot-defn-type)
	  add!(slot-args, make-keyword-literal(type:));
	  if (slot.slot-type)
	    add!(slot-args,
		 make-literal-constant(builder, slot.slot-type));
	  else
	    let temp = make-local-var(builder, #"type",
				      dylan-value(#"<type>"));
	    fer-convert(builder, slot.slot-defn-type, lexenv,
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
	if (slot.slot-defn-init-function)
	  add!(slot-args, make-keyword-literal(init-function:));
	  let temp = make-local-var(builder, #"init-function",
				    function-ctype());
	  fer-convert(builder, slot.slot-defn-init-function, lexenv,
		      #"assignment", temp);
	  add!(slot-args, temp);
	end;
	if (slot.slot-defn-init-keyword)
	  add!(slot-args,
	       if (slot.slot-defn-init-keyword-required?)
		 make-keyword-literal(required-init-keyword:);
	       else
		 make-keyword-literal(init-keyword:);
	       end);
	  add!(slot-args, make-keyword-literal(slot.slot-defn-init-keyword));
	end;
	let temp = make-local-var(builder,
				  slot.slot-getter.defn-name.name-symbol,
				  object-ctype());
	build-assignment(builder, policy, source, temp,
			 make-unknown-call(builder, as(<list>, slot-args)));
	add!(slots-args, temp);
      end;
      let temp = make-local-var(builder, #"slots", object-ctype());
      build-assignment(builder, policy, source, temp,
		       make-unknown-call(builder, as(<list>, slots-args)));
      add!(args, temp);
    end;
  end;
end;
*/

define method build-getter
    (builder :: <fer-builder>, defn :: <slot-defn>,
     slot :: <instance-slot-info>)
    => res :: <method-literal>;
  let lexenv = make(<lexenv>);
  let policy = lexenv.lexenv-policy;
  let source = make(<source-location>);
  let cclass = slot.slot-introduced-by;
  let instance = make-lexical-var(builder, #"object", source, cclass);
  let (index, args, specializers)
    = if (instance?(slot, <vector-slot-info>))
	let fi = specifier-type(#"<fixed-integer>");
	let index = make-lexical-var(builder, #"index", source, fi);
	values(index,
	       list(instance, index),
	       list(cclass, fi));
      else
	values(#f,
	       list(instance),
	       list(cclass));
      end;
  let region = build-function-body
    (builder, policy, source,
     format-to-string("Slot Getter %s", defn.slot-defn-getter.defn-name),
     args, #"best");
  let type = slot.slot-type;
  let meth = make-method-literal
    (builder, #"global",
     make(<signature>, specializers: specializers, returns: type),
     region);
  let result = make-local-var(builder, #"result", type);
  local
    method get (offset :: <fixed-integer>,
		init?-offset :: false-or(<fixed-integer>))
      if (init?-offset)
	let init?-slot = slot.slot-initialized?-slot;
	let temp = make-local-var(builder, #"initialized?", object-ctype());
	build-assignment(builder, policy, source, temp,
			 make-operation(builder, <slot-ref>, list(instance),
					derived-type: init?-slot.slot-type,
					slot-info: init?-slot,
					slot-offset: init?-offset));
	build-if-body(builder, policy, source, temp);
	build-else(builder, policy, source);
	build-assignment
	  (builder, policy, source, #(),
	   make-error-operation(builder, "Slot is not initialized"));
	end-body(builder);
      end;
      build-assignment(builder, policy, source, result,
		       make-operation(builder, <slot-ref>, args,
				      derived-type: slot.slot-type,
				      slot-info: slot, slot-offset: offset));
      unless (init?-offset | slot-guaranteed-initialized?(slot, cclass))
	let temp = make-local-var(builder, #"initialized?", object-ctype());
	build-assignment(builder, policy, source, temp,
			 make-operation(builder, <primitive>, list(result),
					name: #"initialized?"));
	build-if-body(builder, policy, source, temp);
	build-else(builder, policy, source);
	build-assignment
	  (builder, policy, source, #(),
	   make-error-operation(builder, "Slot is not initialized"));
	end-body(builder);
      end;
    end;
  build-slot-posn-dispatch(builder, slot, instance, get);
  build-return(builder, policy, source, region, result);
  end-body(builder);
  meth;
end;

define method build-setter
    (builder :: <fer-builder>, defn :: <slot-defn>,
     slot :: <instance-slot-info>)
    => res :: <method-literal>;
  let init?-slot = slot.slot-initialized?-slot;
  let lexenv = make(<lexenv>);
  let policy = lexenv.lexenv-policy;
  let source = make(<source-location>);
  let type = slot.slot-type;
  let new = make-lexical-var(builder, #"new-value", source, type);
  let cclass = slot.slot-introduced-by;
  let instance = make-lexical-var(builder, #"object", source, cclass);
  let (index, args, specializers)
    = if (instance?(slot, <vector-slot-info>))
	let fi = specifier-type(#"<fixed-integer>");
	let index = make-lexical-var(builder, #"index", source, fi);
	values(index,
	       list(new, instance, index),
	       list(type, cclass, fi));
      else
	values(#f,
	       list(new, instance),
	       list(type, cclass));
      end;
  let region = build-function-body
    (builder, policy, source,
     format-to-string("Slot Setter %s", defn.slot-defn-setter.defn-name),
     args, #"best");
  let meth = make-method-literal
    (builder, #"global",
     make(<signature>, specializers: specializers, returns: type),
     region);
  let result = make-local-var(builder, #"result", type);
  local
    method set (offset :: <fixed-integer>,
		init?-offset :: false-or(<fixed-integer>))
      build-assignment(builder, policy, source, #(),
		       make-operation(builder, <slot-set>, args,
				      slot-info: slot, slot-offset: offset));
      if (init?-offset)
	let init?-slot = slot.slot-initialized?-slot;
	let true-leaf = make-literal-constant(builder, make(<literal-true>));
	let init-op = make-operation
	  (builder, <slot-set>, list(true-leaf, instance),
	   slot-info: init?-slot, slot-offset: init?-offset);
	build-assignment(builder, policy, source, #(), init-op);
      end;
    end;
  build-slot-posn-dispatch(builder, slot, instance, set);
  build-return(builder, policy, source, region, new);
  end-body(builder);
  meth;
end;

define method build-slot-posn-dispatch
    (builder :: <fer-builder>, slot :: <instance-slot-info>,
     instance-leaf :: <leaf>, thunk :: <function>)
    => ();
  let policy = $Default-Policy;
  let source = make(<source-location>);
  let cclass = slot.slot-introduced-by;
  let positions = slot.slot-positions;
  let init?-positions
    = slot.slot-initialized?-slot & slot.slot-initialized?-slot.slot-positions;
  if (positions.tail == #()
	& (init?-positions == #f | init?-positions.tail == #()))
    // The slot only ever shows up at one place.  So just use that one
    // place.
    thunk(positions.head.tail,
	  init?-positions & init?-positions.head.tail);
  elseif (cclass.sealed?)
    // The slot shows up in more than one place, but the class is sealed so
    // we can compute a direct mapping from instance.object-class.unique-id
    // to offset.
    local
      method find-position-for (subclass, posns)
	block (return)
	  for (posn in posns)
	    if (csubtype?(subclass, posn.head))
	      return(posn.tail);
	    end;
	  end;
	  error("Subclass %= isn't in the position table?", subclass);
	end;
      end;
    let ranges = #();
    let prev = #f;
    for (entry in sort!(map(method (subclass)
			      let id = subclass.unique-id;
			      vector(id, id,
				     find-position-for(subclass, positions),
				     init?-positions
				       & find-position-for(subclass,
							   init?-positions));
			    end,
			    find-direct-classes(cclass)),
			key: method (entry1, entry2)
			       entry1[0] < entry2[0];
			     end))
      if (prev == #f)
	ranges := list(entry);
	prev := ranges;
      elseif (prev.head[2] == entry[2] & prev.head[3] == entry[3])
	prev[1] := entry[1];
      else
	let new = list(entry);
	prev.tail := new;
	prev := new;
      end;
    finally
      let ranges = as(<simple-object-vector>, ranges);
      let less-then = dylan-defn-leaf(builder, #"<");
      //
      // Extract the unique id for this argument.
      let class-temp = make-local-var(builder, #"class", object-ctype());
      let obj-class-leaf = dylan-defn-leaf(builder, #"%object-class");
      build-assignment(builder, policy, source, class-temp,
		       make-unknown-call(builder, obj-class-leaf, #f,
					 list(instance-leaf)));
      let id-temp = make-local-var(builder, #"id", object-ctype());
      let unique-id-leaf = dylan-defn-leaf(builder, #"unique-id");
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
	    let ctv = as(<ct-value>, ranges[half-way-point][1] + 1);
	    let bound = make-literal-constant(builder, ctv);
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
  else
    let instance?-leaf = dylan-defn-leaf(builder, #"instance?");
    local
      method split (positions :: <list>, init?-positions :: false-or(<list>))
	if (empty?(positions) | (init?-positions & empty?(init?-positions)))
	  error("Ran out of positions?");
	end;
	let offset = positions.head.tail;
	let init?-offset = init?-positions & init?-positions.head.tail;
	if (every?(method (entry)
		     entry.tail == offset;
		   end,
		   positions.tail)
	      & (init?-offset == #f
		   | every?(method (entry)
			      entry.tail == init?-offset;
			    end,
			    init?-positions.tail)))
	  thunk(offset, init?-offset);
	else
	  let best-test = #f;
	  let best-weight = 0;
	  let best-yes = #f;
	  let best-yes-init? = #f;
	  let best-no = #f;
	  let best-no-init? = #f;
	  for (entry in positions)
	    let (weight, yes, yes-init?, no, no-init?)
	      = try-split(entry.head, positions, init?-positions);
	    if (weight > best-weight)
	      best-test := entry.head;
	      best-weight := weight;
	      best-yes := yes;
	      best-yes-init? := yes-init?;
	      best-no := no;
	      best-no-init? := no-init?;
	    end;
	  end;
	  let cond-temp = make-local-var(builder, #"cond", object-ctype());
	  let type-leaf = make-literal-constant(builder, best-test);
	  build-assignment
	    (builder, policy, source, cond-temp,
	     make-unknown-call
	       (builder, instance?-leaf, #f, list(instance-leaf, type-leaf)));
	  build-if-body(builder, policy, source, cond-temp);
	  split(best-yes, best-yes-init?);
	  build-else(builder, policy, source);
	  split(best-no, best-no-init?);
	  end-body(builder);
	end;
      end;
    split(positions, init?-positions);
  end;
end;


define method try-split
    (test :: <cclass>, positions :: <list>,
     init?-positions :: false-or(<list>))
    => (weight :: <fixed-integer>,
	yes :: <list>, yes-init? :: false-or(<list>),
	no :: <list>, no-init? :: false-or(<list>));
  let yes = #();
  let yes-count = 0;
  let no = #();
  let no-count = 0;

  local
    method add-to-what (entry)
      if (csubtype?(entry.head, test))
	values(#t, #f);
      elseif (ctypes-intersect?(entry.head, test))
	values(#t, #t);
      else
	values(#f, #t);
      end;
    end,
    method unique-position? (entry, list)
      block (return)
	for (already in yes)
	  if (already.tail == entry.tail)
	    return(#f);
	  end;
	end;
	#t;
      end;
    end;

  for (entry in positions)
    let (add-to-yes?, add-to-no?) = add-to-what(entry);
    if (add-to-yes?)
      if (unique-position?(entry, yes))
	yes-count := yes-count + 1;
      end;
      yes := pair(entry, yes);
    end;
    if (add-to-no?)
      if (unique-position?(entry, no))
	no-count := no-count + 1;
      end;
      no := pair(entry, no);
    end;
  end;

  if (init?-positions)
    let yes-init? = #();
    let no-init? = #();

    for (entry in init?-positions)
      let (add-to-yes?, add-to-no?) = add-to-what(entry);
      if (add-to-yes?)
	if (unique-position?(entry, yes-init?))
	  yes-count := yes-count + 1;
	end;
	yes-init? := pair(entry, yes-init?);
      end;
      if (add-to-no?)
	if (unique-position?(entry, no-init?))
	  no-count := no-count + 1;
	end;
	no-init? := pair(entry, no-init?);
      end;
    end;
    values(yes-count * no-count, yes, yes-init?, no, no-init?);

  else
    values(yes-count * no-count, yes, #f, no, #f);
  end;
end;
