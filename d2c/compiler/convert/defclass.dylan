module: define-classes
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/convert/defclass.dylan,v 1.46 1996/01/03 21:37:02 ram Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

define class <real-class-definition> (<class-definition>)
  //
  // The <cclass> for this class definition, #f if unknown (e.g. non-constant
  // superclasses), #"not-computed-yet" if we haven't computed it yet, or
  // #"computing" if we are actively working on it.
  slot class-defn-cclass
    :: type-union(<cclass>, one-of(#f, #"not-computed-yet", #"computing")),
    init-value: #"not-computed-yet", init-keyword: class:;
  //
  // Defered evaluations function, of #f if there isn't one.
  slot %class-defn-defered-evaluations-function
    :: type-union(<ct-function>, one-of(#f, #"not-computed-yet")),
    init-value: #"not-computed-yet";
  //
  // The maker function, of #f if there isn't one.
  slot %class-defn-maker-function
    :: type-union(<ct-function>, one-of(#f, #"not-computed-yet")),
    init-value: #"not-computed-yet";
end;

define method defn-type (defn :: <real-class-definition>) => res :: <cclass>;
  dylan-value(#"<class>");
end;

define class <local-class-definition> (<real-class-definition>)
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

define class <slot-defn> (<object>)
  //
  // The class that introduces this slot.
  slot slot-defn-class :: <real-class-definition>;
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
  slot slot-defn-type :: false-or(<expression>),
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
  slot slot-defn-setter-name :: false-or(<name>),
    required-init-keyword: setter-name:;
  //
  // The setter method.  Filled in when computed.
  slot slot-defn-setter :: false-or(<setter-method-definition>);
  //
  // The init-value expression, or #f if one wasn't supplied.
  slot slot-defn-init-value :: false-or(<expression>),
    init-value: #f, init-keyword: init-value:;
  //
  // The init-function, or #f if there isn't one.
  slot slot-defn-init-function :: false-or(<expression>),
    init-value: #f, init-keyword: init-function:;
  //
  // The init-keyword, or #f if there isn't one.
  slot slot-defn-init-keyword :: false-or(<literal-symbol>),
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
  slot slot-defn-info :: false-or(<slot-info>),
    init-value: #f;
end;

define class <override-defn> (<object>)
  //
  // The class that introduces this override.
  slot override-defn-class :: <real-class-definition>;
  //
  // The name of the getter.
  slot override-defn-getter-name :: <name>,
    required-init-keyword: getter-name:;
  //
  // The init-value expression, or #f if none.
  slot override-defn-init-value :: false-or(<expression>),
    init-value: #f, init-keyword: init-value:;
  //
  // The init-function expression, or #f if none.
  slot override-defn-init-function :: false-or(<expression>),
    init-value: #f, init-keyword: init-function:;
  //
  // The <override-info> for this override, or #f if we haven't computed it
  // or don't know enough about the class to compute it at all.
  slot override-defn-info :: false-or(<override-info>),
    init-value: #f;
end;


define class <define-class-tlf> (<simple-define-tlf>)
  //
  // Make the definition required.
  required keyword defn:;
end;

define method print-message
    (tlf :: <define-class-tlf>, stream :: <stream>) => ();
  format(stream, "Define Class %s", tlf.tlf-defn.defn-name);
end;


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
	let setter = if (class-functional? & allocation == #"instance")
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
	unless (instance?(option.classopt-name, <keyword-token>))
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
  let defn = make(<local-class-definition>,
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
  end;
  for (override in overrides)
    override.override-defn-class := defn;
  end;
  note-variable-definition(defn);
  add!(*Top-Level-Forms*, make(<define-class-tlf>, defn: defn));
end;


// CT-Value

// Compute the compile-time value for a class definition.  This is the
// <cclass> object.  If we can't compute that for some reason, return #f
// to indicate that this class doesn't have a compile-time value.

define method ct-value (defn :: <real-class-definition>)
    => res :: false-or(<cclass>);
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

define method compute-cclass (defn :: <real-class-definition>)
    => res :: false-or(<cclass>);
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
    // Check that we arn't trying to inherit from a sealed class from some
    // other library.
    for (super :: <cclass> in supers)
      if (super.sealed? & super.loaded?)
	compiler-error("Can't inherit from sealed class %s", super);
      end if;
    end for;
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
	 loading: #f,
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
  let cclass :: false-or(<cclass>)
    = if (defn.class-defn-cclass == #"not-computed-yet")
	defn.class-defn-cclass := compute-cclass(defn);
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
    (slot :: <slot-defn>, cclass :: <cclass>, class-type :: <ctype>,
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
	if (cinstance?(init-val, function-ctype()))
	  info.slot-init-function := init-val;
	else
	  compiler-warning("Invalid init-function: %s", init-val);
	end;
      end;
    end;
  end;

  // Define the accessor methods.
  unless (slot.slot-defn-allocation == #"virtual")
    //
    // Are the accessor methods hairy?
    let hairy? = ~cclass | instance?(slot-type, <unknown-ctype>);
    //
    slot.slot-defn-getter
      := make(<getter-method-definition>,
	      base-name: slot.slot-defn-getter-name,
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
	add-seal(gf, specializers, tlf);
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
	       add-seal(gf, pair(object-ctype(), specializers), tlf);
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
  end;
end;


// class-defn-mumble-function accessors.


define method class-defn-defered-evaluations-function
    (defn :: <real-class-definition>) => res :: false-or(<ct-function>);
  if (defn.%class-defn-defered-evaluations-function == #"not-computed-yet")
    defn.%class-defn-defered-evaluations-function
      := if (block (return)
	       let cclass = ct-value(defn);
	       unless (cclass)
		 return(#f);
	       end;
	       // If any of our superclasses have a defered evaluations
	       // function, we need one.
	       for (super in cclass.direct-superclasses)
		 if (super.class-defn.class-defn-defered-evaluations-function)
		   return(#t);
		 end;
	       end;
	       // If any of our slots require some defered evaluations,
	       // then we need a defered evaluations function.
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
	       // ### inherited each-subclass slots w/ non obvious init
	       // values impose the existance of the defered-evaluations
	       // function.
	     end)
	   make(<ct-function>,
		name: format-to-string("Defered evaluations for %s",
				       defn.defn-name),
		signature: make(<signature>, specializers: #(),
				returns: make-values-ctype(#(), #f)));
	 else
	   #f;
	 end;
  else
    defn.%class-defn-defered-evaluations-function;
  end;
end;

define method class-defn-maker-function
    (defn :: <real-class-definition>) => res :: false-or(<ct-function>);
  if (defn.%class-defn-maker-function == #"not-computed-yet")
    defn.%class-defn-maker-function
      := block (return)
	   let cclass = ct-value(defn);
	   if (cclass == #f | cclass.abstract?)
	     return(#f);
	   end;
	   let instance-rep = pick-representation(cclass, #"speed");
	   if (instance?(instance-rep, <immediate-representation>)
		 & ~instance?(instance-rep, <data-word-representation>))
	     return(#f);
	   end;
	   let key-infos = make(<stretchy-vector>);
	   for (slot in cclass.all-slot-infos)
	     if (instance?(slot, <instance-slot-info>))
	       if (instance?(slot.slot-type, <unknown-ctype>))
		 return(#f);
	       end;
	       let override
		 = block (found)
		     for (override in slot.slot-overrides)
		       if (csubtype?(cclass,
				     override.override-introduced-by))
			 found(override);
		       end;
		     finally
		       #f;
		     end;
		   end;
	       let init-value
		 = if (override)
		     if (override.override-init-function == #t)
		       return(#f);
		     end;
		     override.override-init-value;
		   else
		     if (slot.slot-init-function == #t)
		       return(#f);
		     end;
		     slot.slot-init-value;
		   end;
	       if (init-value == #t)
		 return(#f);
	       end;
	       let key = slot.slot-init-keyword;
	       if (key)
		 let type = slot.slot-type;
		 let required? = ~override & slot.slot-init-keyword-required?;
		 let default-bogus?
		   = init-value & ~cinstance?(init-value, type);
		 let key-info
		   = make(<key-info>, key-name: key, type: type,
			  required: required? | default-bogus?,
			  default: init-value);
		 add!(key-infos, key-info);
	       end;
	     end;
	   end;
	   make(<ct-function>,
		name: format-to-string("Maker for %s", defn.defn-name),
		signature: make(<signature>, specializers: #(),
				keys: as(<list>, key-infos),
				all-keys: #t,
				returns: cclass));
	 end;
  else
    defn.%class-defn-maker-function;
  end;
end;


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
    // defered-evaluations, key-defaulter, and maker functions.

    let lexenv = make(<lexenv>);
    let policy = lexenv.lexenv-policy;
    let source = make(<source-location>);
    
    local
      method make-descriptors-leaf (builder, what, for-class)
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

    let evals-builder = make-builder(tl-builder);
    begin
      let %evals-slot-descriptors-leaf = #f;
      let %evals-override-descriptors-leaf = #f;

      // Do the defered evaluations for any of the superclasses that need it.
      for (super in cclass.direct-superclasses)
	if (super.class-defn.class-defn-defered-evaluations-function)
	  build-assignment
	    (evals-builder, policy, source, #(),
	     make-unknown-call
	       (evals-builder,
		ref-dylan-defn(evals-builder, policy, source,
			       #"maybe-do-defered-evaluations"),
		#f,
		list(make-literal-constant(evals-builder, super))));
	end;
      end;

      local
	method evals-slot-descriptors-leaf ()
	  %evals-slot-descriptors-leaf
	    | (%evals-slot-descriptors-leaf
		 := make-descriptors-leaf(evals-builder, "new-slot", cclass));
	end,
	method evals-override-descriptors-leaf ()
	  %evals-override-descriptors-leaf
	    | (%evals-override-descriptors-leaf
		 := make-descriptors-leaf(evals-builder, "override", cclass));
	end;

      for (slot-defn in defn.class-defn-slots,
	   index from 0)
	let slot-info = slot-defn.slot-defn-info;
	let slot-name = slot-info.slot-getter.variable-name;

	let %evals-slot-descriptor-leaf = #f;
	local
	  method evals-slot-descriptor-leaf ()
	    if (%evals-slot-descriptor-leaf)
	      %evals-slot-descriptor-leaf;
	    else
	      let var = make-local-var(evals-builder,
				       symcat(slot-name, "-descriptor"),
				       object-ctype());
	      build-assignment
		(evals-builder, policy, source, var,
		 make-unknown-call
		   (evals-builder,
		    ref-dylan-defn(evals-builder, policy, source, #"element"),
		    #f,
		    list(evals-slot-descriptors-leaf(),
			 make-literal-constant
			   (evals-builder, as(<ct-value>, index)))));
	      %evals-slot-descriptor-leaf := var;
	    end;
	  end;

	let slot-type = slot-info.slot-type;
	let (type, type-var)
	  = if (instance?(slot-type, <unknown-ctype>))
	      let type-expr = slot-defn.slot-defn-type;
	      let var
		= make-local-var(evals-builder, symcat(slot-name, "-type"),
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
		    list(var, evals-slot-descriptor-leaf())));
	      values(object-ctype(), var);
	    else
	      values(slot-type, #f);
	    end;

	let allocation = slot-defn.slot-defn-allocation;

	let init-value = slot-info.slot-init-value;
	let init-function = slot-info.slot-init-function;
	if (init-value == #t)
	  let var = make-local-var(evals-builder,
				   symcat(slot-name, "-init-value"),
				   object-ctype());
	  fer-convert(evals-builder, slot-defn.slot-defn-init-value,
		      lexenv, #"assignment", var);
	  build-assignment
	    (evals-builder, policy, source, #(),
	     make-unknown-call
	       (evals-builder,
		ref-dylan-defn(evals-builder, policy, source,
			       #"slot-init-value-setter"),
		#f,
		list(var, evals-slot-descriptor-leaf())));
	elseif (init-function == #t)
	  let leaf = convert-init-function(evals-builder, slot-name,
					   slot-defn.slot-defn-init-function);
	  build-assignment
	    (evals-builder, policy, source, #(),
	     make-unknown-call
	       (evals-builder,
		ref-dylan-defn(evals-builder, policy, source,
			       #"slot-init-function-setter"),
		#f,
		list(leaf, evals-slot-descriptor-leaf())));
	end;
	
	unless (allocation == #"virtual")
	  if (defn.class-defn-sealed? | defn.class-defn-primary?)
	    if (type-var)
	      local
		method build-call (name, #rest args)
		  let temp = make-local-var(evals-builder, name,
					    object-ctype());
		  build-assignment
		    (evals-builder, policy, source, temp,
		     make-unknown-call
		       (evals-builder,
			ref-dylan-defn(evals-builder, policy, source, name),
			#f, as(<list>, args)));
		  temp;
		end,
		method build-add-method (gf-name, method-leaf)
		  // We don't use method-defn-of, because that is #f if there
		  // is a definition but it isn't a define generic.
		  let gf-var = find-variable(gf-name);
		  let gf-defn = gf-var & gf-var.variable-definition;
		  if (gf-defn)
		    let gf-leaf = fer-convert-defn-ref(evals-builder, policy,
						       source, gf-defn);
		    build-assignment
		      (evals-builder, policy, source, #(),
		       make-unknown-call
			 (evals-builder,
			  ref-dylan-defn(evals-builder, policy, source,
					 #"add-method"),
			  #f,
			  list(gf-leaf, method-leaf)));
		  else
		    error("No definition for %=, and can't "
			    "implicitly define it.",
			  gf-name);
		  end;
		end;

	      let results = build-call(#"list", type-var);
	      let cclass-leaf = make-literal-constant(evals-builder, cclass);
	      let false-leaf
		= make-literal-constant(evals-builder, as(<ct-value>, #f));
	      begin
		let getter
		  = build-getter(evals-builder, #f, slot-defn, slot-info);
		let getter-specializers = build-call(#"list", cclass-leaf);
		let meth = build-call(#"%make-method", getter-specializers,
				      results, false-leaf, getter);
		build-add-method(slot-defn.slot-defn-getter-name, meth);
	      end;
	      if (slot-defn.slot-defn-setter)
		let setter
		  = build-setter(evals-builder, #f, slot-defn, slot-info);
		let setter-specializers = build-call(#"list", type-var,
						     cclass-leaf);
		let meth = build-call(#"%make-method", setter-specializers,
				      results, false-leaf, setter);
		build-add-method(slot-defn.slot-defn-setter-name, meth);
	      end;
	    else
	      build-getter(tl-builder, slot-defn.slot-defn-getter.ct-value,
			   slot-defn, slot-info);
	      if (slot-defn.slot-defn-setter)
		build-setter(tl-builder, slot-defn.slot-defn-setter.ct-value,
			     slot-defn, slot-info);
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
	  let descriptor-var
	    = make-local-var(evals-builder,
			     symcat(slot-name, "-override-descriptor"),
			     object-ctype());
	  build-assignment
	    (evals-builder, policy, source, descriptor-var,
	     make-unknown-call
	       (evals-builder,
		ref-dylan-defn(evals-builder, policy, source, #"element"),
		#f,
		list(evals-override-descriptors-leaf(),
		     make-literal-constant
		       (evals-builder, as(<ct-value>, index)))));

	  if (init-value)
	    let var = make-local-var(evals-builder,
				     symcat(slot-name, "-override-init-value"),
				     object-ctype());
	    fer-convert(evals-builder, override-defn.override-defn-init-value,
			lexenv, #"assignment", var);
	    build-assignment
	      (evals-builder, policy, source, #(),
	       make-unknown-call
		 (evals-builder,
		  ref-dylan-defn(evals-builder, policy, source,
				 #"override-init-value-setter"),
		  list(var, descriptor-var)));
	  else
	    let leaf
	      = convert-init-function(evals-builder, slot-name,
				      override-defn
					.override-defn-init-function);
	    build-assignment
	      (evals-builder, policy, source, #(),
	       make-unknown-call
		 (evals-builder,
		  ref-dylan-defn(evals-builder, policy, source,
				 #"override-init-function-setter"),
		  list(leaf, descriptor-var)));
	  end;
	end;
      end;
    end;

    // ### Build the key-defaulter (if concrete)

    // Build the maker (if concrete) and do any slot processing that
    // has to happen for every slot in the class.

    begin
      let representation = pick-representation(cclass, #"speed");
      let data-word? = instance?(representation, <data-word-representation>);
      let key-infos = make(<stretchy-vector>);
      let maker-args = make(<stretchy-vector>);
      let maker-builder = make-builder(tl-builder);
      let init-builder = make-builder(tl-builder);
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
		 := make-descriptors-leaf(maker-builder, "all-slot", cclass));
	end,
	method maker-override-descriptors-leaf (introducer :: <cclass>)
	  block (return)
	    for (entry in %maker-override-descriptors-leaves)
	      if (entry.head == introducer)
		return(entry.tail);
	      end;
	    end;
	    let new = make-descriptors-leaf(maker-builder, "override",
					    introducer);
	    %maker-override-descriptors-leaves
	      := pair(pair(introducer, new),
		      %maker-override-descriptors-leaves);
	    new;
	  end;
	end;

      for (slot in cclass.all-slot-infos, index from 0)
	let slot-name = slot.slot-getter & slot.slot-getter.variable-name;
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
		    ref-dylan-defn(maker-builder, policy, source, #"element"),
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
			  ref-dylan-defn(maker-builder, policy, source,
					 #"slot-type"),
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
			let block-region
			  = build-block-body(init-builder, policy, source);
			let index
			  = make-local-var(init-builder, #"index",
					   specifier-type(#"<fixed-integer>"));
			build-assignment
			  (init-builder, policy, source, index,
			   make-literal-constant
			     (init-builder, as(<ct-value>, 0)));
			build-loop-body(init-builder, policy, source);
			let more?
			  = make-local-var(init-builder, #"more?",
					   specifier-type(#"<boolean>"));
			build-assignment
			  (init-builder, policy, source, more?,
			   make-unknown-call
			     (init-builder,
			      ref-dylan-defn(init-builder, policy, source,
					     #"<"),
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
			     (init-builder,
			      ref-dylan-defn(init-builder, policy, source,
					     #"+"),
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
		method maker-override-descriptor-leaf () => res :: <leaf>;
		  let debug-name = symcat(slot-name, "-descriptor");
		  let var = make-local-var(maker-builder, debug-name,
					   object-ctype());
		  let introducer = override.override-introduced-by;
		  build-assignment
		    (maker-builder, policy, source, var,
		     make-unknown-call
		       (maker-builder,
			ref-dylan-defn(maker-builder, policy, source,
				       #"element"),
			#f,
			list(maker-override-descriptors-leaf(introducer),
			     make-literal-constant
			       (maker-builder,
				as(<ct-value>,
				   find-key(introducer.override-infos,
					    curry(\==, override)))))));
		  var;
		end,
		method extract-init-value (init-value-var) => ();
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
			    list(maker-override-descriptor-leaf())));
		    else
		      build-assignment
			(maker-builder, policy, source,
			 init-value-var,
			 make-unknown-call
			   (maker-builder,
			    ref-dylan-defn(maker-builder, policy, source,
					   #"slot-init-value"),
			    #f,
			    list(maker-slot-descriptor-leaf())));
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
		end,
		method call-init-function (init-value-var) => ();
		  if (init-function == #t)
		    let init-function-var
		      = make-local-var(maker-builder,
				       symcat(slot-name, "-init-function"),
				       function-ctype());
		    if (override)
		      build-assignment
			(maker-builder, policy, source,
			 init-function-var,
			 make-unknown-call
			   (maker-builder,
			    ref-dylan-defn(maker-builder, policy, source,
					   #"override-init-function"),
			    #f,
			    list(maker-override-descriptor-leaf())));
		    else
		      build-assignment
			(maker-builder, policy, source,
			 init-function-var,
			 make-unknown-call
			   (maker-builder,
			    ref-dylan-defn(maker-builder, policy, source,
					   #"slot-init-function"),
			    #f,
			    list(maker-slot-descriptor-leaf())));
		    end;
		    build-assignment
		      (maker-builder, policy, source, init-value-var,
		       make-unknown-call(maker-builder, init-function-var,
					 #f, #()));
		  elseif (init-function)
		    let init-func-leaf
		      = make-literal-constant(maker-builder, init-function);
		    build-assignment
		      (maker-builder, policy, source, init-value-var,
		       make-unknown-call(maker-builder, init-func-leaf, #f,
					 #()));
		  else
		    error("shouldn't have called call-init-function "
			    "when init-function is false");
		  end;

		  if (type-var)
		    build-assignment
		      (maker-builder, policy, source, init-value-var,
		       make-check-type-operation
			 (maker-builder, policy, source,
			  init-value-var, type-var));
		  end;
		end;
	      
	      let key = slot.slot-init-keyword;
	      if (key)
		let required? = ~override & slot.slot-init-keyword-required?;
		let default = ~(init-value == #t) & init-value;
		let default-bogus? = default & ~cinstance?(default, type);
		let key-info = make(<key-info>, key-name: key, type: type,
				    required: required? | default-bogus?,
				    default: default);
		let init-value-var
		  = make-local-var(maker-builder,
				   symcat(slot-name, "-init-value"),
				   type);
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
		    call-init-function(init-value-var);
		  elseif (slot.slot-init-keyword-required?)
		    build-assignment
		      (maker-builder, policy, source, #(),
		       make-error-operation
			 (maker-builder, policy, source,
			  #"missing-required-init-keyword-error",
			  make-literal-constant
			    (maker-builder, as(<ct-value>, key)),
			  make-literal-constant(maker-builder, cclass)));
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
		  if (init-value)
		    extract-init-value(init-value-var);
		  else
		    call-init-function(init-value-var);
		  end;
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
	  <virtual-slot-info> =>
	    // Don't need to do anything for virtual slots.
	    #f;
	end;
      end;
      
      if (instance-leaf)
	let name = format-to-string("Maker for %s", defn.defn-name);
	let maker-region
	  = build-function-body(tl-builder, policy, source, #f, name,
				as(<list>, maker-args), cclass, #t);
	build-region(tl-builder, builder-result(maker-builder));
	let bytes = cclass.instance-slots-layout.layout-length;
	let base-len
	  = make-literal-constant(tl-builder, as(<ct-value>, bytes));
	let len-leaf
	  = if (vector-slot)
	      let fi = specifier-type(#"<fixed-integer>");
	      let elsize
		= vector-slot.slot-representation.representation-size;
	      let extra
		= if (elsize == 1)
		    size-leaf;
		  else
		    let var = make-local-var(tl-builder, #"extra", fi);
		    let elsize-leaf
		      = make-literal-constant(tl-builder,
					      as(<ct-value>, elsize));
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
		    #f, list(base-len, extra)));
	      var;
	    else
	      base-len;
	    end;
	unless (data-word?)
	  build-assignment
	    (tl-builder, policy, source, instance-leaf,
	     make-operation
	       (tl-builder, <primitive>, list(len-leaf),
		name: #"allocate", derived-type: cclass));
	end;
	build-region(tl-builder, builder-result(init-builder));
	build-return(tl-builder, policy, source, maker-region,
		     list(instance-leaf));
	end-body(tl-builder);
	
	// Fill in the maker function.
	let ctv = defn.class-defn-maker-function;
	if (ctv)
	  make-function-literal(tl-builder, ctv, #f, #"global",
				ctv.ct-function-signature, maker-region);
	else
	  // The maker function isn't a compile-time constant, so add code to
	  // the defered evaluations to install it.
	  let maker-leaf
	    = make-function-literal(tl-builder, #f, #f, #"local",
				    make(<signature>, specializers: #(),
					 keys: as(<list>, key-infos),
					 all-keys: #t,
					 returns: cclass),
				    maker-region);
	  build-assignment
	    (evals-builder, policy, source, #(),
	     make-unknown-call
	       (evals-builder,
		ref-dylan-defn(evals-builder, policy, source,
			       #"class-maker-setter"),
		#f,
		list(maker-leaf,
		     make-literal-constant(evals-builder, cclass))));
	end;
      end;
    end;

    let ctv = defn.class-defn-defered-evaluations-function;
    if (ctv)
      let func-region = build-function-body(tl-builder, policy, source, #f,
					    ctv.ct-function-name,
					    #(), make-values-ctype(#(), #f),
					    #t);
      build-region(tl-builder, builder-result(evals-builder));
      
      // ### install the key-defaulter function here?

      // Return nothing.
      build-return(tl-builder, policy, source, func-region, #());
      end-body(tl-builder);
      make-function-literal(tl-builder, ctv, #f, #"global",
			    ctv.ct-function-signature, func-region);
    end;
  end;
end;


define method convert-init-function
    (builder :: <fer-builder>, slot-name :: <symbol>,
     init-function :: <expression>)
    => res :: <leaf>;
  let lexenv = make(<lexenv>);
  let policy = lexenv.lexenv-policy;
  let source = make(<source-location>);
  let var = make-lexical-var(builder, symcat(slot-name, "-init-function"),
			     source, function-ctype());
  fer-convert(builder, init-function, lexenv, #"let", var);
  let func-region
    = build-function-body(builder, policy, source, #t,
			  concatenate("Init Function for ",
				      as(<string>, slot-name)), #(),
			  object-ctype(), #f);
  let temp = make-local-var(builder, #"result", object-ctype());
  build-assignment(builder, policy, source, temp,
		   make-unknown-call(builder, var, #f, #()));
  build-return(builder, policy, source, func-region, temp);
  end-body(builder);
  make-function-literal(builder, #f, #f, #"local",
			make(<signature>, specializers: #()),
			func-region);
end;


define method build-getter
    (builder :: <fer-builder>, ctv :: false-or(<ct-method>),
     defn :: <slot-defn>, slot :: <instance-slot-info>)
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
  let type = slot.slot-type;
  let region = build-function-body
    (builder, policy, source, #f,
     format-to-string("Slot Getter %s", defn.slot-defn-getter.defn-name),
     args, type, #t);
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
	   make-error-operation
	     (builder, policy, source, #"uninitialized-slot-error"));
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
	   make-error-operation
	     (builder, policy, source, #"uninitialized-slot-error"));
	end-body(builder);
      end;
    end;
  build-slot-posn-dispatch(builder, slot, instance, get);
  build-return(builder, policy, source, region, result);
  end-body(builder);
  make-function-literal
    (builder, ctv, #t, if (ctv) #"global" else #"local" end,
     make(<signature>, specializers: specializers, returns: type),
     region);
end;

define method build-setter
    (builder :: <fer-builder>, ctv :: false-or(<ct-method>),
     defn :: <slot-defn>, slot :: <instance-slot-info>)
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
    (builder, policy, source, #f,
     format-to-string("Slot Setter %s", defn.slot-defn-setter.defn-name),
     args, type, #t);
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
  make-function-literal
    (builder, ctv, #t, if (ctv) #"global" else #"local" end,
     make(<signature>, specializers: specializers, returns: type),
     region);
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
  elseif (cclass.sealed?
	    & every?(method (subclass :: <cclass>)
		       subclass.abstract? | subclass.unique-id;
		     end,
		     cclass.subclasses))
    // The slot shows up in more than one place, but the class is sealed and
    // all the concrete subclasses have unique-id's, so we can compute a
    // direct mapping from instance.object-class.unique-id to offset.
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
	  let instance?-leaf
	    = ref-dylan-defn(builder, policy, source, #"instance?");
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
    let getter = slot.slot-defn-getter;
    if (getter.method-defn-of)
      dump-od(slot.slot-defn-getter, state);
    end;
    let setter = slot.slot-defn-setter;
    if (setter & setter.method-defn-of)
      dump-od(setter, state);
    end;
  end;
end;

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
		     %class-defn-defered-evaluations-function, #f,
		       %class-defn-defered-evaluations-function-setter,
		     %class-defn-maker-function, #f,
		       %class-defn-maker-function-setter,
		     class-defn-new-slot-infos, #f,
		       class-defn-new-slot-infos-setter,
		     class-defn-all-slot-infos, #f,
		       class-defn-all-slot-infos-setter,
		     class-defn-override-infos, #f,
		       class-defn-override-infos-setter,
		     class-defn-vector-slot, #f,
		       class-defn-vector-slot-setter));

add-make-dumper(#"class-definition", *compiler-dispatcher*, <real-class-definition>,
		$class-definition-slots, load-external: #t,
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

