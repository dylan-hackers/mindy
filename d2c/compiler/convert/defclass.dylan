module: define-classes
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/convert/defclass.dylan,v 1.6 1994/12/16 11:50:29 wlott Exp $
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
end;

define method defn-type (defn :: <class-definition>) => res :: <cclass>;
  dylan-value(#"<class>");
end;

define class <slot-defn> (<object>)
  slot slot-class :: <class-definition>;
  slot slot-sealed? :: <boolean>,
    required-init-keyword: sealed:;
  slot slot-allocation :: one-of(#"instance", #"class", #"each-subclass",
				 #"constant", #"virtual"),
    required-init-keyword: allocation:;
  slot slot-type-expr :: union(<expression>, <false>),
    required-init-keyword: type:;
  slot slot-type :: union(<ctype>, <false>),
    init-value: #f;
  slot slot-getter-name :: <name>,
    required-init-keyword: getter-name:;
  slot slot-getter-method :: union(<getter-method-definition>, <false>),
    init-value: #f;
  slot slot-setter-name :: union(<name>, <false>),
    required-init-keyword: setter-name:;
  slot slot-setter-method :: union(<setter-method-definition>, <false>),
    init-value: #f;
  slot slot-init-value :: union(<expression>, <false>),
    init-value: #f, init-keyword: init-value:;
  slot slot-init-function :: union(<expression>, <false>),
    init-value: #f, init-keyword: init-function:;
  slot slot-init-keyword :: union(<symbol>, <false>),
    init-value: #f, init-keyword: init-keyword:;
  slot slot-init-keyword-required? :: <boolean>,
    init-value: #f, init-keyword: init-keyword-required:;
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



define method process-top-level-form (form :: <define-class-parse>) => ();
  let slots = make(<stretchy-vector>);
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
	  error("Can't be both open and sealed.");
	end;
	let allocation = if (allocation)
			   allocation.varref-name.token-symbol;
			 else
			   #"instance";
			 end;
	let getter = option.classopt-name.token-symbol;
	unless (instance?(type, <expression>))
	  error("Bogus type expression: %=", type);
	end;
	let setter = if (allocation == #"constant")
		       if (setter)
			 error("Constant slots can't have a setter.");
		       else
			 #f;
		       end;
		     else
		       if (instance?(setter, <varref>))
			 setter.varref-name.token-symbol;
		       elseif (instance?(setter, <literal>)
				 & setter.lit-value == #f)
			 #f;
		       elseif (setter)
			 error("Bogus setter name: %=", setter);
		       else
			 as(<symbol>,
			    concatenate(as(<string>, getter), "-setter"));
		       end;
		     end;
	if (init-value)
	  if (init-function)
	    error("Can't supply both an init-value: and an init-function:.");
	  end;
	  if (req-init-keyword)
	    error("Can't supply both an init-value: and a "
		    "required-init-keyword:.");
	  end;
	  unless (instance?(init-value, <expression>))
	    error("Bogus init-value: %=", init-value);
	  end;
	elseif (init-function)
	  if (req-init-keyword)
	    error("Can't supply both an init-function: and a "
		    "required-init-keyword:.");
	  end;
	  unless (instance?(init-function, <expression>))
	    error("Bogus init-function: %=", init-function);
	  end;
	end;
	if (init-keyword)
	  if (req-init-keyword)
	    error("Can't supply both an init-keyword: and a "
		    "required-init-keyword:.");
	    unless (instance?(init-keyword, <literal>)
		      & instance?(init-keyword.lit-value, <symbol>))
	      error("Bogus init-keyword: %=", init-keyword);
	    end;
	  end;
	elseif (req-init-keyword)
	  unless (instance?(req-init-keyword, <literal>)
		    & instance?(req-init-keyword.lit-value, <symbol>))
	    error("Bogus required-init-keyword: %=", req-init-keyword);
	  end;
	end;
	let getter-name = make(<basic-name>, symbol: getter,
			       module: *Current-Module*);
	let setter-name = setter & make(<basic-name>, symbol: setter,
					module: *Current-Module*);
	let slot-defn = make(<slot-defn>,
			     sealed: ~open?,
			     allocation: allocation,
			     type: type,
			     getter-name: getter-name,
			     setter-name: setter-name,
			     init-value: init-value,
			     init-function: init-function,
			     init-keyword:
			       if (init-keyword)
				 init-keyword.lit-value;
			       elseif (req-init-keyword)
				 req-init-keyword.lit-value;
			       end,
			     init-keyword-required: req-init-keyword & #t);
	implicitly-define-generic(getter-name, 1, #f, #f);
	if (setter)
	  implicitly-define-generic(setter-name, 2, #f, #f);
	end;
	add!(slots, slot-defn);

      #"inherited" =>
	let (init-value, init-function)
	  = extract-properties("inherited slot spec", option.classopt-plist,
			       init-value:, init-function:);
	if (init-value)
	  if (init-function)
	    error("Can't supply both an init-value: and an init-function:");
	  end;
	  unless (instance?(init-value, <expression>))
	    error("Bogus init-value: %=", init-value);
	  end;
	elseif (init-function)
	  unless (instance?(init-function, <expression>))
	    error("Bogus init-function: %=", init-function);
	  end;
	end;
	// ### Need to do something with it.
      #"keyword" =>
	unless (instance?(option.classopt-name, <literal>)
		  & instance?(option.classopt-name.lit-value, <symbol>))
	  error("Bogus keyword: %=", option.classopt-name);
	end;
	let (required?, type, init-value, init-function)
	  = extract-properties("init arg spec", option.classopt-plist,
			       required:, type:, init-value:, init-function:);
	if (required?)
	  if (init-value)
	    error("Can't supply an init-value: for required keyword init "
		    "arg specs");
	  end;
	  if (init-function)
	    error("Can't supply an init-function: for required keyword init "
		    "arg specs");
	  end;
	elseif (init-value)
	  if (init-function)
	    error("Can't supply both an init-value: and an init-function: for "
		    "keyword init arg specs");
	  end;
	  unless (instance?(init-value, <expression>))
	    error("Bogus init-value: %=", init-value);
	  end;
	elseif (init-function)
	  unless (instance?(init-function, <expression>))
	    error("Bogus init-function: %=", init-function);
	  end;
	end;
	if (type)
	  unless (instance?(type, <expression>))
	    error("Bogus type: %=", type);
	  end;
	end;
	// ### Need to do something with it.
    end;
  end;
  let name = form.defclass-name.token-symbol;
  let (open?, sealed?, primary?, free?, abstract?, concrete?)
    = extract-modifiers("define class", name, form.define-modifiers,
			#"open", #"sealed", #"primary", #"free",
			#"abstract", #"concrete");
  if (open? & sealed?)
    error("define class %s can't be both open and sealed.", name);
  end;
  if (primary? & free?)
    error("define class %s can't be both primary and free.", name);
  end;
  if (abstract? & concrete?)
    error("define class %s can't be both abstract and concrete.", name);
  end;
  let slots = as(<simple-object-vector>, slots);
  let defn = make(<class-definition>,
		  name: make(<basic-name>,
			     symbol: name,
			     module: *Current-Module*),
		  supers: form.defclass-supers,
		  sealed: ~open?, primary: primary?,
		  abstract: abstract?,
		  slots: slots);
  for (slot-defn in slots)
    slot-defn.slot-class := defn;
  end;
  note-variable-definition(defn);
  add!($Top-Level-Forms, make(<define-class-tlf>, defn: defn));
end;


define method ct-value (defn :: <class-definition>)
    => res :: union(<false>, <cclass>);
  select (defn.class-defn-cclass)
    #"not-computed-yet" =>
      defn.class-defn-cclass := #"computing";
      let res = block (return)
		  let supers = map(method (super)
				     ct-eval(super, #f) | return(#f);
				   end,
				   defn.class-defn-supers);
		  make(<defined-cclass>,
		       name: defn.defn-name,
		       direct-superclasses: as(<list>, supers),
		       sealed: defn.class-defn-sealed?,
		       primary: defn.class-defn-primary?,
		       abstract: defn.class-defn-abstract?);
		end;
      defn.class-defn-cclass := res;
    #"computing" =>
      error("class %s circularly defined.", defn.defn-name);
    otherwise =>
      defn.class-defn-cclass;
  end;
end;


define method finalize-top-level-form (tlf :: <define-class-tlf>) => ();
  let defn = tlf.tlf-defn;
  //
  // Call ct-value in order to make the <cclass> object for this class if
  // it hasn't already been made.
  let cclass = ct-value(defn);
  //
  // Finalize the slots.
  for (slot-defn in defn.class-defn-slots)
    let slot-type-expr = slot-defn.slot-type-expr;
    let slot-type
      = if (slot-type-expr)
	  let type = ct-eval(slot-type-expr, #f);
	  instance?(type, <ctype>) & type;
	else
	  object-ctype();
	end;
    let hairy? = ~(cclass & slot-type);
    slot-defn.slot-type := slot-type;
    unless (slot-defn.slot-allocation == #"virtual")
      slot-defn.slot-getter-method
	:= make(<getter-method-definition>,
		base-name: slot-defn.slot-getter-name,
		signature: make(<signature>,
				specializers:
				  list(cclass | make(<unknown-ctype>)),
				rest-type: #f,
				keys: #(),
				all-keys?: #f,
				returns:
				  list(slot-type | make(<unknown-ctype>)),
				returns-rest-type: #f),
		hairy: hairy?,
		sealed: slot-defn.slot-sealed?);
      if (slot-defn.slot-setter-name)
	slot-defn.slot-setter-method
	  := make(<getter-method-definition>,
		  base-name: slot-defn.slot-getter-name,
		  signature: make(<signature>,
				  specializers:
				    list(slot-type | make(<unknown-ctype>),
					 cclass | make(<unknown-ctype>)),
				  rest-type: #f,
				  keys: #(),
				  all-keys?: #f,
				  returns:
				    list(slot-type | make(<unknown-ctype>)),
				  returns-rest-type: #f),
		  hairy: hairy?,
		  sealed: slot-defn.slot-sealed?);
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
	fer-convert(builder, super, lexenv, temp);
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
      for (slot-defn in defn.class-defn-slots)
	let slot-args = make(<stretchy-vector>);
	add!(slot-args, dylan-defn-leaf(builder, #"vector"));
	add!(slot-args, make-keyword-literal(allocation:));
	add!(slot-args, make-keyword-literal(slot-defn.slot-allocation));
	add!(slot-args, make-keyword-literal(getter:));
	begin
	  let name = slot-defn.slot-getter-name;
	  let var = find-variable(name.name-module, name.name-symbol);
	  let defn = var & var.variable-definition;
	  if (defn)
	    add!(slot-args, make-definition-leaf(builder, defn));
	  else
	    error("No definition for %=, and can't implicitly define it.",
		  name);
	  end;
	end;
	if (slot-defn.slot-setter-name)
	  let name = slot-defn.slot-setter-name;
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
	if (slot-defn.slot-type-expr)
	  add!(slot-args, make-keyword-literal(type:));
	  if (slot-defn.slot-type)
	    add!(slot-args,
		 make-literal-constant(builder, slot-defn.slot-type));
	  else
	    let temp = make-local-var(builder, #"type",
				      dylan-value(#"<type>"));
	    fer-convert(builder, slot-defn.slot-type-expr, lexenv, temp);
	    add!(slot-args, temp);
	  end;
	end;
	if (slot-defn.slot-init-value)
	  add!(slot-args, make-keyword-literal(init-value:));
	  let temp = make-local-var(builder, #"init-value", object-ctype());
	  fer-convert(builder, slot-defn.slot-init-value, lexenv, temp);
	  add!(slot-args, temp);
	end;
	if (slot-defn.slot-init-function)
	  add!(slot-args, make-keyword-literal(init-function:));
	  let temp = make-local-var(builder, #"init-function", object-ctype());
	  fer-convert(builder, slot-defn.slot-init-function, lexenv, temp);
	  add!(slot-args, temp);
	end;
	if (slot-defn.slot-init-keyword)
	  add!(slot-args,
	       if (slot-defn.slot-init-keyword-required?)
		 make-keyword-literal(required-init-keyword:);
	       else
		 make-keyword-literal(init-keyword:);
	       end);
	  add!(slot-args, make-keyword-literal(slot-defn.slot-init-keyword));
	end;
	let temp = make-local-var(builder,
				  slot-defn.slot-getter.defn-name.name-symbol,
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

