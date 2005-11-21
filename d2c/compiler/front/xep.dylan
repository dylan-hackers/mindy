module: xep-tools
copyright: see below


//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998 - 2003  Gwydion Dylan Maintainers
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

// External entry construction.

define function build-local-xeps (component :: <component>) => ();
  for (func in component.all-function-literals)
    if (func.general-entry == #f & func.visibility == #"local")
      block (return)
	for (dep = func.dependents then dep.source-next,
	     while: dep)
	  let dependent = dep.dependent;
	  if (~instance?(dependent, <known-call>)
		    | dependent.depends-on ~== dep)
	    build-external-entries-for(component, func);
	    return();
	  end if;
	end for;
      end block;
    end if;
  end for;
end;

// XXX - Yuck. This depends on the optimizer interface.
define function build-xep-component
    (optimizer :: <abstract-optimizer>,
     function :: <ct-function>, generic-entry? :: <boolean>)
 => (entry :: <fer-function-region>, component :: <component>);
  let component = make(<fer-component>);
  let entry = build-xep(function, generic-entry?, component);
  optimize-component(optimizer, component);
  values(entry, component);
end function build-xep-component;

define function build-xep-get-ctv (func :: <function-literal>)
 => ctv :: <ct-function>;
  func.ct-function
    | (func.ct-function
	 := make(if (instance?(func, <method-literal>))
		   <ct-method>;
		 elseif (instance?(func, <callback-literal>))
		   <ct-callback-function>;
		 else
		   <ct-function>;
		 end,
		 name: func.main-entry.name,
		 signature: func.signature));
end function build-xep-get-ctv;

define generic build-external-entries-for
    (component :: <component>, function :: <function-literal>) => ();

define method build-external-entries-for
    (component :: <component>, function :: <function-literal>) => ();
  function.general-entry := build-xep(function, #f, component);
  build-xep-get-ctv(function).has-general-entry? := #t;
end;

define method build-external-entries-for
    (component :: <component>, function :: <callback-literal>) => ();
  function.general-entry := build-xep(function, #f, component);
  build-xep-get-ctv(function).has-general-entry? := #t;
  function.callback-entry := build-callback-xep(function, component);
  build-xep-get-ctv(function).has-callback-entry? := #t;
end;

define method build-external-entries-for
    (component :: <component>, function :: <method-literal>) => ();
  function.general-entry := build-xep(function, #f, component);
  build-xep-get-ctv(function).has-general-entry? := #t;
  unless (function.generic-entry)
    build-xep-get-ctv(function).has-generic-entry? := #t;
    function.generic-entry := build-xep(function, #t, component);
  end;
end;


define class <keyarg-info> (<object>)

  constant slot keyarg-key-info :: <key-info>,
    required-init-keyword: key-info:;

  constant slot keyarg-var :: <abstract-variable>,
    required-init-keyword: var:;

  constant slot keyarg-default-bogus? :: <boolean>,
    required-init-keyword: default-bogus?:;

  constant slot keyarg-supplied?-var :: false-or(<abstract-variable>),
    required-init-keyword: supplied?-var:;
end class <keyarg-info>;


// build-xep -- exported.
//
// Build an external entry point for a given function.
//
define generic build-xep
    (function :: type-union(<ct-function>, <function-literal>),
     generic-entry? :: <boolean>,
     component :: <component>)
 => (xep :: <fer-function-region>);

define method build-xep
    (function :: <ct-function>, generic-entry? :: <boolean>,
     component :: <component>)
 => (xep :: <fer-function-region>);

  let defn = function.ct-function-definition;
  let loc = if (defn) defn.source-location
	    else
// GGR: for later debugging	      compiler-warning("has no defn: %=", function); //### FIXME
	      make(<source-location>)
	    end;
  
  aux-build-xep(make(<literal-constant>, value: function),
		function.ct-function-signature, component,
		function.ct-function-name,
		select(function by instance?)
		  <ct-method> => #"method";
		  <ct-callback-function> => #"callback";
		  <ct-function> => #"function";
		end,
		generic-entry?, #f,
		loc);
end method build-xep;

define method build-xep
    (function :: <function-literal>, generic-entry? :: <boolean>,
     component :: <component>)
 => xep :: <fer-function-region>;
  let main-entry = function.main-entry;


  let ct-func = function.ct-function;
// GGR: for later debugging  ct-func | compiler-warning("has no ct-func: %=", function); //### FIXME
  let defn = ct-func & ct-func.ct-function-definition;
// GGR: for later debugging  (ct-func & ~defn) & compiler-warning("has no defn: %=, %=", function, ct-func); //### FIXME
  let loc = if (defn) defn.source-location else make(<source-location>) end;

  aux-build-xep(function, function.signature, component, main-entry.name,
		select(function by instance?)
		  <method-literal> => #"method";
		  <callback-literal> => #"callback";
		  <function-literal> => #"function";
		end,
		generic-entry?,
		main-entry,
		loc);
end;

define function aux-build-xep
    (function :: <expression>, signature :: <signature>,
     component :: <component>, entry-name,
     kind :: one-of(#"function", #"method", #"callback"),
     generic-entry? :: <boolean>, main-entry :: false-or(<object>),
     source :: <source-location>)
 => (xep :: <fer-function-region>);
  let builder = make-builder(component);
  let policy = $Default-Policy;
//  let source = make(<source-location>); // ##
  let closure?
    = instance?(main-entry, <lambda>) & main-entry.environment.closure-vars;
  let self-leaf
    = make-local-var(builder, #"self",
		     specifier-type(select(kind)
				      #"method" =>
					if (closure?)
					  #"<method-closure>";
					else
					  #"<method>";
					end;
				      #"function" =>
					if (closure?)
					  #"<raw-closure>";
					else
					  #"<raw-function>";
					end;
				      #"callback" =>
					if (closure?)
					  #"<callback-closure>";
					else
					  #"<callback-function>";
					end;
				    end));
  let nargs-leaf = make-local-var(builder, #"nargs", 
				  dylan-value(#"<integer>"));
  let next-info-leaf
    = generic-entry? & make-local-var(builder, #"next-method-info",
				      dylan-value(#"<list>"));
  let name = make(<derived-name>,
  		  how: if (generic-entry?)
		         #"generic-entry"
		       else
		         #"general-entry"
		       end,
		  base: entry-name);
  let xep = build-function-body(builder, policy, source, #f, name,
				if (generic-entry?)
				  list(self-leaf, nargs-leaf, next-info-leaf);
				else
				  list(self-leaf, nargs-leaf);
				end,
				wild-ctype(), #t);
  let new-args = make(<stretchy-vector>);
  if (closure?)
    let closure-ref-leaf = ref-dylan-defn(builder, policy, source,
					  #"closure-var");
    for (closure-var = main-entry.environment.closure-vars
	   then closure-var.closure-next,
	 index from 0,
	 while: closure-var)
      let copy = closure-var.copy-var;
      let pre-type = make-local-var(builder, copy.var-info.debug-name,
				    object-ctype());
      let index-leaf = make-literal-constant(builder, index);
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
    = make-literal-constant(builder, arg-types.size);
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
	    make-literal-constant(builder, #t),
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
	    make-literal-constant(builder, #f),
	    wanted-leaf, nargs-leaf));
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
    let index-leaf = make-literal-constant(builder, index);
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
	   make-literal-constant(builder, #());
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

    // The first thing we need to do is make and initialize variables for all
    // the keyword arguments.
    local
      method make-keyarg-info (key-info :: <key-info>)
	  => res :: <keyarg-info>;
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
			     boolean-ctype());
	    else
	      #f;
	    end;
	add!(new-args, var);
	build-assignment
	  (builder, policy, source, var,
	   if (default & ~default-bogus?)
	     make-literal-constant(builder, default);
	   else
	     make(<uninitialized-value>, derived-type: type.ctype-extent);
	   end);
	if (supplied?-var)
	  if (needs-supplied?-var?)
	    add!(new-args, supplied?-var);
	  end;
	  build-assignment
	    (builder, policy, source, supplied?-var,
	     make-literal-constant(builder, #f));
	end;
	make(<keyarg-info>, key-info: key-info, var: var,
	     default-bogus?: default-bogus?, supplied?-var: supplied?-var);
      end method make-keyarg-info;
    let keyarg-infos = map(make-keyarg-info, signature.key-infos);

    let index-var
      = make-local-var(builder, #"index", dylan-value(#"<integer>"));
    build-assignment
      (builder, policy, source, index-var,
       make-unknown-call
	 (builder,
	  ref-dylan-defn(builder, policy, source, #"-"),
	  #f,
	  list(nargs-leaf,
	       make-literal-constant
		 (builder, 2))));

    let done-block = build-block-body(builder, policy, source);
    build-loop-body(builder, policy, source);

    let done-var = make-local-var(builder, #"done?", object-ctype());
    build-assignment
      (builder, policy, source, done-var,
       make-unknown-call(builder,
			 ref-dylan-defn(builder, policy, source, #"<"),
			 #f, list(index-var, wanted-leaf)));
    build-if-body(builder, policy, source, done-var);
    build-exit(builder, policy, source, done-block);
    build-else(builder, policy, source);
    let key-var = make-local-var(builder, #"key", dylan-value(#"<symbol>"));
    begin
      let op = make-operation(builder, <primitive>, list(args-leaf, index-var),
			      name: #"extract-arg");
      if (generic-entry?)
	let temp = make-local-var(builder, #"key", object-ctype());
	build-assignment(builder, policy, source, temp, op);
	op := make-operation(builder, <truly-the>, list(temp),
			     guaranteed-type: dylan-value(#"<symbol>"));
      end;
      build-assignment(builder, policy, source, key-var, op);
    end;
    let temp = make-local-var(builder, #"temp", dylan-value(#"<integer>"));
    build-assignment
      (builder, policy, source, temp,
       make-unknown-call(builder,
			 ref-dylan-defn(builder, policy, source, #"+"),
			 #f,
			 list(index-var,
			      make-literal-constant(builder, 1))));
    let val-var = make-local-var(builder, #"value", object-ctype());
    build-assignment
      (builder, policy, source, val-var,
       make-operation
	 (builder, <primitive>, list(args-leaf, temp), name: #"extract-arg"));

    local
      method build-next-key (remaining :: <list>, done :: <list>)
	if (empty?(remaining))
	  unless (generic-entry? | signature.all-keys?)
	    build-assignment
	      (builder, policy, source, #(),
	       make-error-operation
		 (builder, policy, source,
		  #"unrecognized-keyword-error", key-var));
	  end;
	else
	  let keyarg-info :: <keyarg-info> = remaining.head;
	  let key-info = keyarg-info.keyarg-key-info;
	  let key = key-info.key-name;
	  if (member?(key, done))
	    build-next-key(remaining.tail, done);
	  else
	    let temp = make-local-var(builder, #"condition", object-ctype());
	    build-assignment
	      (builder, policy, source, temp,
	       make-unknown-call
		 (builder,
		  ref-dylan-defn(builder, policy, source, #"=="),
		  #f,
		  list(key-var,
		       make-literal-constant(builder, key))));
	    build-if-body(builder, policy, source, temp);

	    for (keyarg-info :: <keyarg-info> in remaining)
	      if (keyarg-info.keyarg-key-info.key-name == key)
		let var = keyarg-info.keyarg-var;
		build-assignment(builder, policy, source, var, val-var);
		let supplied?-var = keyarg-info.keyarg-supplied?-var;
		if (supplied?-var)
		  build-assignment
		    (builder, policy, source, supplied?-var, 
		     make-literal-constant(builder, #t));
		end;
	      end if;
	    end for;
	    build-else(builder, policy, source);
	    build-next-key(remaining.tail, pair(key, done));
	    end-body(builder);
	  end if;
	end if;
      end method build-next-key;
    build-next-key(keyarg-infos, #());

    build-assignment
      (builder, policy, source, index-var,
       make-unknown-call(builder,
			 ref-dylan-defn(builder, policy, source, #"-"),
			 #f,
			 list(index-var,
			      make-literal-constant(builder, 2))));
    end-body(builder); // if
    end-body(builder); // loop
    end-body(builder); // block

    for (info in keyarg-infos)
      if (info.keyarg-default-bogus?)
	build-if-body(builder, policy, source, info.keyarg-supplied?-var);
	build-else(builder, policy, source);
	build-assignment
	  (builder, policy, source, #(),
	   make-error-operation
	     (builder, policy, source,
	      #"type-error",
	      make-literal-constant(builder, info.keyarg-key-info.key-default),
	      make-literal-constant(builder, info.keyarg-key-info.key-type)));
	end-body(builder);
      end if;
    end for;
  end;

  build-assignment(builder, policy, source, #(),
		   make-operation(builder, <primitive>, list(args-leaf),
				  name: #"pop-args"));
  let cluster = make-values-cluster(builder, #"results", wild-ctype());
  let ops = pair(function, as(<list>, new-args));
  build-assignment(builder, policy, source, cluster,
		   make-operation(builder, <known-call>, ops));
  build-return(builder, policy, source, xep, cluster);
  end-body(builder);

  xep;
end;

define function build-callback-xep
    (function :: <callback-literal>, component :: <component>)
 => (xep :: <fer-function-region>);
  let main-entry = function.main-entry;
  let entry-name = main-entry.name;
  let signature = function.signature;
  let builder = make-builder(component);
  let policy = $Default-Policy;
  let source = make(<source-location>); //### FIXME see build-xep{<function-literal>}
  let closure?
    = instance?(main-entry, <lambda>) & main-entry.environment.closure-vars;
  let self-leaf
    = make-local-var(builder, #"self",
		     specifier-type(if (closure?)
				      #"<callback-closure>";
				    else
				      #"<callback-function>";
				    end));
  let name = make(<derived-name>, how: #"callback-entry", base: entry-name);
  let vars = map-as(<list>,
		    method(type) make-local-var(builder, #"arg", type) end,
		    signature.specializers);

  let xep = build-function-body(builder, policy, source, #f, name,
				if(closure?)
				  pair(self-leaf, vars);
				else
				  vars;
				end,
				signature.returns, #t,
				calling-convention: #"callback");

  let new-args = make(<stretchy-vector>);
  if (closure?)
    let closure-ref-leaf = ref-dylan-defn(builder, policy, source,
					  #"closure-var");
    for (closure-var = main-entry.environment.closure-vars
	   then closure-var.closure-next,
	 index from 0,
	 while: closure-var)
      let copy = closure-var.copy-var;
      let pre-type = make-local-var(builder, copy.var-info.debug-name,
				    object-ctype());
      let index-leaf = make-literal-constant(builder, index);
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

  for(var in vars)
    add!(new-args, var);
  end;

  let cluster = make-values-cluster(builder, #"result", signature.returns);

  let ops = pair(function, as(<list>, new-args));
  build-assignment(builder, policy, source, cluster,
		   make-operation(builder, <known-call>, ops));
  build-return(builder, policy, source, xep, cluster);
  end-body(builder);
  xep;
end function build-callback-xep;



// Seals for file xep.dylan

// <keyarg-info> -- subclass of <object>
define sealed domain make(singleton(<keyarg-info>));
define sealed domain initialize(<keyarg-info>);
