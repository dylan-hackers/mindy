module: cheese
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/optimize/xep.dylan,v 1.1 1996/02/02 23:19:47 wlott Exp $
copyright: Copyright (c) 1996  Carnegie Mellon University
	   All rights reserved.


// External entry construction.

define method build-external-entries (component :: <component>) => ();
  for (function in component.all-function-literals)
    if (function.general-entry == #f)
      maybe-build-external-entries-for(component, function);
    end;
  end;
end;

define method maybe-build-external-entries-for
    (component :: <component>, func :: <function-literal>) => ();
  select(func.visibility)
    #"global" =>
      build-external-entries-for(component, func);
    #"local" =>
      block (return)
	for (dep = func.dependents then dep.source-next,
	     while: dep)
	  let dependent = dep.dependent;
	  unless (instance?(dependent, <known-call>)
		    & dependent.depends-on == dep)
	    build-external-entries-for(component, func);
	    return();
	  end;
	end;
      end;
  end;
end;

define method build-external-entries-for
    (component :: <component>, function :: <function-literal>) => ();
  function.general-entry
    := build-xep(component, #f, function, function.signature);
end;

define method build-external-entries-for
    (component :: <component>, function :: <method-literal>) => ();
  let ctv = function.ct-function;
  unless (ctv & ctv.ct-method-hidden?)
    function.general-entry
      := build-xep(component, #f, function, function.signature);
  end;
  unless (function.generic-entry)
    function.generic-entry
      := build-xep(component, #t, function, function.signature);
  end;
end;


define method build-xep
    (component :: <component>, generic-entry? :: <boolean>,
     function :: <function-literal>, signature :: <signature>)
    => xep :: <fer-function-region>;
  let main-entry = function.main-entry;
  let builder = make-builder(component);
  let policy = $Default-Policy;
  let source = make(<source-location>);
  let closure?
    = instance?(main-entry, <lambda>) & main-entry.environment.closure-vars;
  let self-leaf
    = make-local-var(builder, #"self",
		     specifier-type(if (instance?(function, <method-literal>))
				      if (closure?)
					#"<method-closure>";
				      else
					#"<method>";
				      end;
				    else
				      if (closure?)
					#"<raw-closure>";
				      else
					#"<raw-function>";
				      end;
				    end));
  let nargs-leaf = make-local-var(builder, #"nargs", 
				  dylan-value(#"<integer>"));
  let next-info-leaf
    = generic-entry? & make-local-var(builder, #"next-method-info",
				      dylan-value(#"<list>"));
  let name = format-to-string("%s entry for %s",
			      if (generic-entry?) "Generic" else "General" end,
			      main-entry.name);
  let xep = build-function-body(builder, policy, source, #f, name,
				if (generic-entry?)
				  list(self-leaf, nargs-leaf, next-info-leaf);
				else
				  list(self-leaf, nargs-leaf);
				end,
				wild-ctype(), #t);
  let new-args = make(<stretchy-vector>);
  if (instance?(main-entry, <lambda>) & main-entry.environment.closure-vars)
    let closure-ref-leaf = ref-dylan-defn(builder, policy, source,
					  #"closure-var");
    for (closure-var = main-entry.environment.closure-vars
	   then closure-var.closure-next,
	 index from 0,
	 while: closure-var)
      let copy = closure-var.copy-var;
      let pre-type = make-local-var(builder, copy.var-info.debug-name,
				    object-ctype());
      let index-leaf = make-literal-constant(builder, as(<ct-value>, index));
      build-assignment(builder, policy, source, pre-type,
		       make-unknown-call(builder, closure-ref-leaf, #f,
					 list(self-leaf, index-leaf)));
      let post-type = make-local-var(builder, copy.var-info.debug-name,
				     copy.derived-type);
      build-assignment(builder, policy, source, post-type,
		       make-operation(builder, <truly-the>, list(pre-type),
				      guaranteed-type: copy.derived-type));
      add!(new-args, post-type);
    end;
  end;

  let arg-types = signature.specializers;
  let raw-ptr-type = dylan-value(#"<raw-pointer>");
  let args-leaf = make-local-var(builder, #"args", raw-ptr-type);
  let wanted-leaf
    = make-literal-constant(builder, as(<ct-value>, arg-types.size));
  if (generic-entry?)
    // We don't have to check the number of arguments, we just have to
    // find the arg pointer.
    build-assignment
      (builder, policy, source, args-leaf,
       make-operation(builder, <primitive>,
		      list(if (signature.rest-type | signature.key-infos)
			     nargs-leaf;
			   else
			     wanted-leaf;
			   end),
		      name: #"extract-args"));
  else
    if (signature.rest-type == #f & signature.key-infos == #f)
      let op = make-unknown-call
	(builder, ref-dylan-defn(builder, policy, source, #"=="), #f,
	 list(nargs-leaf, wanted-leaf));
      let temp = make-local-var(builder, #"nargs-okay?", object-ctype());
      build-assignment(builder, policy, source, temp, op);
      build-if-body(builder, policy, source, temp);
      build-else(builder, policy, source);
      build-assignment
	(builder, policy, source, #(),
	 make-error-operation
	   (builder, policy, source, #"wrong-number-of-arguments-error",
	    make-literal-constant(builder, as(<ct-value>, #t)),
	    wanted-leaf, nargs-leaf));
      end-body(builder);
      build-assignment(builder, policy, source, args-leaf,
		       make-operation(builder, <primitive>, list(wanted-leaf),
				      name: #"extract-args"));
    else
      unless (empty?(arg-types))
	let op = make-unknown-call
	  (builder, ref-dylan-defn(builder, policy, source, #"<"), #f,
	   list(nargs-leaf, wanted-leaf));
	let temp = make-local-var(builder, #"nargs-okay?", object-ctype());
	build-assignment(builder, policy, source, temp, op);
	build-if-body(builder, policy, source, temp);
	build-assignment
	  (builder, policy, source, #(),
	   make-error-operation
	   (builder, policy, source, #"wrong-number-of-arguments-error",
	    make-literal-constant(builder, as(<ct-value>, #f)),
	    wanted-leaf, nargs-leaf));
	build-else(builder, policy, source);
	end-body(builder);
      end;
      if (signature.key-infos)
	let func = ref-dylan-defn(builder, policy, source,
				  if (odd?(arg-types.size))
				    #"even?";
				  else
				    #"odd?";
				  end);
	let op = make-unknown-call(builder, func, #f, list(nargs-leaf));
	let temp = make-local-var(builder, #"nkeys-okay?", object-ctype());
	build-assignment(builder, policy, source, temp, op);
	build-if-body(builder, policy, source, temp);
	build-assignment
	  (builder, policy, source, #(),
	   make-error-operation
	     (builder, policy, source,
	      #"odd-number-of-keyword/value-arguments-error"));
	build-else(builder, policy, source);
	end-body(builder);
      end;
      build-assignment(builder, policy, source, args-leaf,
		       make-operation(builder, <primitive>, list(nargs-leaf),
				      name: #"extract-args"));
    end;
  end;

  for (type in arg-types,
       index from 0)
    let temp = make-local-var(builder, #"arg",
			      if (generic-entry?)
				object-ctype();
			      else
				type;
			      end);
    let index-leaf = make-literal-constant(builder, as(<ct-value>, index));
    build-assignment(builder, policy, source, temp,
		     make-operation(builder, <primitive>,
				    list(args-leaf, index-leaf),
				    name: #"extract-arg"));
    if (generic-entry?)
      let post-type = make-local-var(builder, #"arg", type);
      build-assignment(builder, policy, source, post-type,
		       make-operation(builder, <truly-the>, list(temp),
				      guaranteed-type: type));
      add!(new-args, post-type);
    else
      add!(new-args, temp);
    end;
  end;

  if (signature.next?)
    add!(new-args,
	 if (generic-entry?)
	   next-info-leaf;
	 else
	   make-literal-constant(builder, as(<ct-value>, #()));
	 end);
  end;

  if (signature.rest-type | (signature.next? & signature.key-infos))
    let op = make-operation(builder, <primitive>,
			    list(args-leaf, wanted-leaf, nargs-leaf),
			    name: #"make-rest-arg");
    let rest-var = make-local-var(builder, #"rest", object-ctype());
    build-assignment(builder, policy, source, rest-var, op);
    add!(new-args, rest-var);
  end;

  if (signature.key-infos)
    let key-var = make-local-var(builder, #"key", dylan-value(#"<symbol>"));
    let val-var = make-local-var(builder, #"value", object-ctype());
    let key-dispatch-builder = make-builder(builder);
    let unsupplied-flame-builder = make-builder(builder);
    local
      method build-next-key (remaining)
	if (empty?(remaining))
	  unless (generic-entry? | signature.all-keys?)
	    build-assignment
	      (key-dispatch-builder, policy, source, #(),
	       make-error-operation
		 (key-dispatch-builder, policy, source,
		  #"unrecognized-keyword-error", key-var));
	  end;
	else
	  let key-info = remaining.head;
	  let key = key-info.key-name;
	  let var = make-local-var(builder, key, key-info.key-type);
	  let type = key-info.key-type;
	  let default = key-info.key-default;
	  let default-bogus?
	    = default & ~cinstance?(key-info.key-default, type);
	  let needs-supplied?-var? = key-info.key-needs-supplied?-var;
	  let supplied?-var
	    = if (default-bogus? | needs-supplied?-var?)
		make-local-var(builder,
			       as(<symbol>,
				  concatenate(as(<string>, key),
					      "-supplied?")),
			       dylan-value(#"<boolean>"));
	      else
		#f;
	      end;
	  add!(new-args, var);
	  build-assignment
	    (builder, policy, source, var,
	     if (default & ~default-bogus?)
	       make-literal-constant(builder, default);
	     else
	       make(<uninitialized-value>, derived-type: type);
	     end);
	  if (supplied?-var)
	    if (needs-supplied?-var?)
	      add!(new-args, supplied?-var);
	    end;
	    build-assignment
	      (builder, policy, source, supplied?-var,
	       make-literal-constant(builder, as(<ct-value>, #f)));
	  end;
	  let temp = make-local-var(key-dispatch-builder, #"condition",
				    object-ctype());
	  build-assignment
	    (key-dispatch-builder, policy, source, temp,
	     make-unknown-call
	       (key-dispatch-builder,
		ref-dylan-defn(key-dispatch-builder, policy, source, #"=="),
		#f,
		list(key-var,
		     make-literal-constant(key-dispatch-builder,
					   as(<ct-value>, key)))));
	  build-if-body(key-dispatch-builder, policy, source, temp);
	  build-assignment(key-dispatch-builder, policy, source, var, val-var);
	  if (supplied?-var)
	    build-assignment
	      (key-dispatch-builder, policy, source, supplied?-var, 
	       make-literal-constant(key-dispatch-builder,
				     as(<ct-value>, #t)));
	  end;
	  build-else(key-dispatch-builder, policy, source);
	  build-next-key(remaining.tail);
	  end-body(key-dispatch-builder);

	  if (default-bogus?)
	    build-if-body(unsupplied-flame-builder, policy, source,
			  supplied?-var);
	    build-else(unsupplied-flame-builder, policy, source);
	    build-assignment
	      (unsupplied-flame-builder, policy, source, #(),
	       make-error-operation
		 (unsupplied-flame-builder, policy, source,
		  #"type-error",
		  make-literal-constant(unsupplied-flame-builder, default),
		  make-literal-constant(unsupplied-flame-builder, type)));
	    end-body(unsupplied-flame-builder);
	  end;
	end;
      end;

    let index-var = make-local-var(key-dispatch-builder, #"index",
				   dylan-value(#"<integer>"));
    build-assignment
      (key-dispatch-builder, policy, source, index-var,
       make-unknown-call
	 (key-dispatch-builder,
	  ref-dylan-defn(key-dispatch-builder, policy, source, #"-"),
	  #f,
	  list(nargs-leaf,
	       make-literal-constant
		 (key-dispatch-builder, as(<ct-value>, 2)))));

    let done-block = build-block-body(key-dispatch-builder, policy, source);
    build-loop-body(key-dispatch-builder, policy, source);

    let done-var
      = make-local-var(key-dispatch-builder, #"done?", object-ctype());
    build-assignment
      (key-dispatch-builder, policy, source, done-var,
       make-unknown-call(key-dispatch-builder,
			 ref-dylan-defn(key-dispatch-builder, policy, source,
					#"<"),
			 #f, list(index-var, wanted-leaf)));
    build-if-body(key-dispatch-builder, policy, source, done-var);
    build-exit(key-dispatch-builder, policy, source, done-block);
    build-else(key-dispatch-builder, policy, source);
    begin
      let op = make-operation(key-dispatch-builder, <primitive>,
			      list(args-leaf, index-var),
			      name: #"extract-arg");
      if (generic-entry?)
	let temp = make-local-var(key-dispatch-builder, #"key",
				  object-ctype());
	build-assignment(key-dispatch-builder, policy, source, temp, op);
	op := make-operation(key-dispatch-builder, <truly-the>, list(temp),
			     guaranteed-type: dylan-value(#"<symbol>"));
      end;
      build-assignment(key-dispatch-builder, policy, source, key-var, op);
    end;
    let temp = make-local-var(key-dispatch-builder, #"temp",
			      dylan-value(#"<integer>"));
    build-assignment
      (key-dispatch-builder, policy, source, temp,
       make-unknown-call(key-dispatch-builder,
			 ref-dylan-defn(key-dispatch-builder, policy, source,
					#"+"),
			 #f,
			 list(index-var,
			      make-literal-constant(key-dispatch-builder,
						    as(<ct-value>, 1)))));
    build-assignment(key-dispatch-builder, policy, source, val-var,
		     make-operation(key-dispatch-builder, <primitive>,
				    list(args-leaf, temp),
				    name: #"extract-arg"));
    build-next-key(signature.key-infos);
    build-assignment
      (key-dispatch-builder, policy, source, index-var,
       make-unknown-call(key-dispatch-builder,
			 ref-dylan-defn(key-dispatch-builder, policy, source,
					#"-"),
			 #f,
			 list(index-var,
			      make-literal-constant(key-dispatch-builder,
						    as(<ct-value>, 2)))));
    end-body(key-dispatch-builder); // if
    end-body(key-dispatch-builder); // loop
    end-body(key-dispatch-builder); // block
    build-region(builder, builder-result(key-dispatch-builder));
    build-region(builder, builder-result(unsupplied-flame-builder));
  end;

  build-assignment(builder, policy, source, #(),
		   make-operation(builder, <primitive>, list(args-leaf),
				  name: #"pop-args"));
  let cluster = make-values-cluster(builder, #"results", wild-ctype());
  build-assignment(builder, policy, source, cluster,
		   make-operation(builder, <known-call>,
				  pair(function, as(<list>, new-args))));
  build-return(builder, policy, source, xep, cluster);
  end-body(builder);

  xep;
end;
