module: define-classes
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/convert/defclass.dylan,v 1.1 1994/12/12 13:01:16 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

define class <class-definition> (<definition>)
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

define class <slot> (<object>)
end;

define class <define-class-tlf> (<simple-define-tlf>)
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
	note-variable-definition(make(<method-definition>,
				      name: make(<basic-name>,
						 symbol: getter,
						 module: *Current-Module*)));
	if (setter)
	  note-variable-definition(make(<method-definition>,
					name: make(<basic-name>,
						   symbol: setter,
						   module: *Current-Module*)));
	end;
	add!(slots,
	     make(<slot>,
		  open: open? & #t,
		  allocation: allocation,
		  getter: getter,
		  setter: setter,
		  type: type,
		  init-value: init-value,
		  init-function: init-function,
		  init-keyword: init-keyword & init-keyword.lit-value,
		  req-init-keyword:
		    req-init-keyword & req-init-keyword.lit-value));
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
  let defn = make(<class-definition>,
		  name: make(<basic-name>,
			     symbol: name,
			     module: *Current-Module*),
		  supers: form.defclass-supers,
		  sealed: ~open?, primary: primary?,
		  abstract: abstract?,
		  slots: as(<simple-object-vector>, slots));
  note-variable-definition(defn);
  add!($Top-Level-Forms, make(<define-class-tlf>, defn: defn));
end;


define method ct-value (defn :: <class-definition>)
    => res :: union(<false>, <cclass>);
  select (defn.class-defn-cclass)
    #"not-computed-yet" =>
      defn.class-defn-cclass := #"computing";
      let res = block (return)
		  let lexenv = make(<lexenv>);
		  let supers = map(method (super)
				     ct-eval(super, lexenv) | return(#f);
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
  // Call ct-value in order to make the <cclass> object for this class if
  // it hasn't already been made.
  ct-value(tlf.tlf-defn);
end;

define method convert-top-level-form
    (builder :: <fer-builder>, tlf :: <define-class-tlf>) => ();
  unless (ct-value(tlf.tlf-defn))
    error("Can't deal with non-constant superclasses.");
  end;
end;
