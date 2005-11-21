module: fer-convert
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

define constant <var-or-vars>
  = type-union(<abstract-variable>, <list>);


// Result stuff.

define constant <result-designator>
  = one-of(#"nothing", #"assignment", #"let", #"expr", #"leaf");

define constant <result-datum>
  = type-union(<var-or-vars>, <symbol>, <false>);

define constant <result> = false-or(<expression>);


define generic deliver-result
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     want :: <result-designator>, datum :: <result-datum>, result :: <result>)
    => res :: <result>;

define method deliver-result
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     want == #"nothing", datum :: <result-datum>, result :: <result>)
    => res :: <result>;
  #f;
end;

define method deliver-result
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     want == #"nothing", datum :: <result-datum>, result :: <operation>)
    => res :: <result>;
  build-assignment(builder, policy, source, #(), result);
  #f;
end;

define method deliver-result
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     want == #"assignment", datum :: <result-datum>, result :: <result>)
    => res :: <result>;
  build-assignment
    (builder, policy, source, datum,
     result | make-literal-constant(builder, #f));

  #f;
end;

define method deliver-result
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     want == #"let", datum :: <result-datum>, result :: <result>)
    => res :: <result>;
  build-let
    (builder, policy, source, datum,
     result | make-literal-constant(builder, #f));
  #f;
end;

define method deliver-result
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     want == #"expr", datum :: <result-datum>, result :: <result>)
    => res :: <result>;
  result | make-literal-constant(builder, #f);
end;

define method deliver-result
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     want == #"leaf", datum :: <result-datum>, result :: <result>)
    => res :: <result>;
  result | make-literal-constant(builder, #f);
end;

define method deliver-result
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     want == #"leaf", datum :: <result-datum>, result :: <operation>)
    => res :: <result>;
  let temp = make-local-var(builder, datum, object-ctype());
  build-assignment(builder, policy, source, temp, result);
  temp;
end;



// fer-convert

define generic fer-convert
    (builder :: <fer-builder>, form :: <constituent-parse>, lexenv :: <lexenv>,
     want :: <result-designator>, datum :: <result-datum>)
    => res :: <result>;


define method fer-convert
    (builder :: <fer-builder>, form :: <constituent-parse>, lexenv :: <lexenv>,
     want :: <result-designator>, datum :: <result-datum>)
    => res :: <result>;
  error("Can't fer-convert %=", form);
end;

define method fer-convert
    (builder :: <fer-builder>, form :: <definition-parse>, lexenv :: <lexenv>,
     want :: <result-designator>, datum :: <result-datum>)
    => res :: <result>;
  compiler-error-location(form, "definitions are only allowed at top-level");
end;

define method fer-convert
    (builder :: <fer-builder>, form :: <macro-call-parse>, lexenv :: <lexenv>,
     want :: <result-designator>, datum :: <result-datum>)
    => res :: <result>;
  fer-convert(builder, macro-expand(form), lexenv, want, datum);
end;

define method fer-convert
    (builder :: <fer-builder>, form :: <let-parse>, lexenv :: <lexenv>,
     want :: <result-designator>, datum :: <result-datum>)
    => res :: <result>;
  let source = form.source-location;
  let varlist = form.let-variables;
  let params = varlist.varlist-fixed;
  let rest = varlist.varlist-rest;
  let rest-temp
    = if (rest)
	if (rest.param-type)
	  compiler-warning-location
	    (rest.param-type,
	     "let #rest variables can't have types -- ignoring");
	end;
	make-local-var(builder, rest.param-name.token-symbol, object-ctype());
      end;
  let nfixed = params.size;
  let types = make(<vector>, size: nfixed);
  let type-temps = make(<vector>, size: nfixed);
  let temps = make(<list>, size: nfixed);

  // Make temps for all the values and evaluate any types that arn't constant.
  for (param in params, index from 0, temp-ptr = temps then temp-ptr.tail)
    let type
      = if (param.param-type)
	  let ct-type = ct-eval(param.param-type, lexenv);
	  if (ct-type)
	    ct-type;
	  else
	    let type-local
	      = make-local-var(builder, #"type", specifier-type(#"<type>"));
	    fer-convert(builder, param.param-type,
			make(<lexenv>, inside: lexenv),
			#"assignment", type-local);
	    let type-temp
	      = make-lexical-var(builder, #"type", source,
				 specifier-type(#"<type>"));
	    build-let(builder, lexenv.lexenv-policy, source,
		      type-temp, type-local);
	    type-temps[index] := type-temp;
	    object-ctype();
	  end;
	else
	  object-ctype();
	end;
    types[index] := type;
    temp-ptr.head
      := make-local-var(builder, param.param-name.token-symbol, type);
  end;

  // Evaluate the expression, getting the results in the temps
  if (rest)
    let cluster = make-values-cluster(builder, #"temps", wild-ctype());
    fer-convert(builder, form.let-expression,
		make(<lexenv>, inside: lexenv), #"assignment", cluster);
    build-assignment
      (builder, lexenv.lexenv-policy, source,
       concatenate(temps, list(rest-temp)),
       make-operation(builder, <primitive>,
		      list(cluster,
			   make-literal-constant(builder, temps.size)),
		      name: #"canonicalize-results"));
  else
    fer-convert(builder, form.let-expression,
		make(<lexenv>, inside: lexenv), #"assignment", temps);
  end;

  // Copy the temps into real lexical vars and update the lexenv with em.
  for (param in params, type in types, type-temp in type-temps, temp in temps)
    let name = param.param-name;
    let var = make-lexical-var(builder, name.token-symbol, source, type);
    add-binding(lexenv, name, var, type-var: type-temp);
    build-let(builder, lexenv.lexenv-policy, source, var,
	      if (type-temp)
		make-check-type-operation
		  (builder, lexenv.lexenv-policy, source,
		   temp, type-temp);
	      else
		temp;
	      end);
  end;
  if (rest)
    let name = rest.param-name;
    let var = make-lexical-var(builder, name.token-symbol, source,
			       object-ctype());
    add-binding(lexenv, name, var);
    build-let(builder, lexenv.lexenv-policy, source, var, rest-temp);
  end;

  // Supply #f as the result.
  deliver-result(builder, lexenv.lexenv-policy, source, want, datum,
		 make-literal-constant(builder, #f));
end;

define method fer-convert
    (builder :: <fer-builder>, form :: <handler-parse>, lexenv :: <lexenv>,
     want :: <result-designator>, datum :: <result-datum>)
    => res :: <result>;
  let source = form.source-location;
  // First, build the call to push-handler.
  let policy = lexenv.lexenv-policy;
  let func = ref-dylan-defn(builder, policy, source, #"push-handler");
  let args = make(<stretchy-vector>);
  add!(args,
       fer-convert(builder, form.handler-type,
		   make(<lexenv>, inside: lexenv), #"leaf", #"type"));
  add!(args,
       fer-convert(builder, form.handler-expression,
		   make(<lexenv>, inside: lexenv), #"leaf", #"handler"));
  for (option in form.handler-options)
    add!(args,
	 fer-convert(builder, option,
		     make(<lexenv>, inside: lexenv), #"leaf",
		     #"temp"));
  end;
  build-assignment(builder, policy, source, #(),
		   make-unknown-call(builder, func, #f, as(<list>, args)));

  if (instance?(lexenv, <body-lexenv>))
    // Record the fact that we've added another handler.
    lexenv.lexenv-handlers := lexenv.lexenv-handlers + 1;
  else
    // Pop the handler now, because we are the only thing inside this
    // body.
    let pop-handler = ref-dylan-defn(builder, policy, source, #"pop-handler");
    build-assignment(builder, policy, source, #(),
		     make-unknown-call(builder, pop-handler, #f, #()));
  end if;

  // Supply #f as the result.
  deliver-result(builder, lexenv.lexenv-policy, source, want, datum,
		 make-literal-constant(builder, #f));
end;

// Coerce a token to a <basic-name>
define function token-basic-name (tok :: <token>) => res :: <basic-name>;
  make(<basic-name>, symbol: tok.token-symbol,
       module: tok.token-module);
end function;


define method fer-convert
    (builder :: <fer-builder>, form :: <local-parse>, lexenv :: <lexenv>,
     want :: <result-designator>, datum :: <result-datum>)
    => res :: <result>;
  let source = form.source-location;
  let specializer-lexenv = make(<lexenv>, inside: lexenv);
  let vars
    = map(method (meth)
	    let name = meth.method-name;
	    let var = make-lexical-var(builder, name.token-symbol,
				       name.source-location,
				       function-ctype());
	    add-binding(lexenv, name, var);
	    var;
	  end,
	  form.local-methods);
  for (var in vars, meth in form.local-methods)
    build-let(builder, lexenv.lexenv-policy, source, var,
	      fer-convert-method
	        (builder, meth,
		 make(<internal-name>, symbol: meth.method-name.token-symbol,
		      base: lexenv.lexenv-method-name),
		 #f, #"local", specializer-lexenv, lexenv));
  end;

  // Supply #f as the result.
  deliver-result(builder, lexenv.lexenv-policy, source, want, datum,
		 make-literal-constant(builder, #f));
end;

define method fer-convert
    (builder :: <fer-builder>, form :: <literal-ref-parse>, lexenv :: <lexenv>,
     want :: <result-designator>, datum :: <result-datum>)
    => res :: <result>;
  deliver-result
    (builder, lexenv.lexenv-policy, form.source-location, want, datum,
     make-literal-constant(builder, form.litref-literal));
end;

define method fer-convert
    (builder :: <fer-builder>, form :: <varref-parse>, lexenv :: <lexenv>,
     want :: <result-designator>, datum :: <result-datum>)
    => res :: <result>;
  let id = form.varref-id;
  let source = id.source-location;
  let binding = find-binding(lexenv, id);
  if (instance?(binding, <top-level-binding>))
  	binding := #f;	// GGR: quick hack!!!
  end;
  deliver-result(builder, lexenv.lexenv-policy, source, want, datum,
		 if (binding)
		   binding.binding-var;
		 else
		   let name = id-name(id);
		   let var = find-variable(name);
		   let defn = var & var.variable-definition;
		   if (defn)
		     build-defn-ref(builder, lexenv.lexenv-policy,
				    source, defn);
		   else
		     compiler-error-location
		       (source, "Undefined variable: %s", name);
		     make-error-operation
		       (builder, lexenv.lexenv-policy, source,
			"Undefined variable: %s",
			make-literal-constant
			  (builder, format-to-string("%s", name)));
		   end;
		 end);
end;

define method fer-convert
    (builder :: <fer-builder>, form :: <varset-parse>, lexenv :: <lexenv>,
     want :: <result-designator>, datum :: <result-datum>)
    => res :: <result>;
  let id = form.varset-id;
  let source = id.source-location;
  let policy = lexenv.lexenv-policy;
  let binding = find-binding(lexenv, id);
  let name = id-name(id);
  if (binding)
    local method convert-and-check(hint, acquire-type :: <function>)
      let temp = fer-convert(builder, form.varset-value,
			     make(<lexenv>, inside: lexenv),
			     #"leaf", #"new-value");
      if (hint)
	let checked
	  = make-local-var(builder, #"checked-new-value", object-ctype());
	build-assignment
	  (builder, policy, source, checked,
	   make-check-type-operation
	     (builder, policy, source, temp,
	      acquire-type(hint)));
	checked;
      else
	temp;
      end;
    end method;
    if (instance?(binding, <top-level-binding>))
      let defn = binding.binding-var.variable-definition;
      if (instance?(defn, <variable-definition>))
	let checked
	  = convert-and-check(defn.var-defn-type-defn,
				/* suspicious! */curry(build-defn-ref, builder, policy, source));
	build-defn-set(builder, policy, source, defn, checked);
	deliver-result(builder, policy, source, want, datum,
		       checked);
      else
	compiler-warning-location
	  (form, "Attempt to assign constant module variable: %s", name);
	deliver-result
	  (builder, policy, source, want, datum,
	   make-error-operation
	     (builder, policy, source,
	      "Can't assign constant module variable: %s",
	      make-literal-constant
		(builder, format-to-string("%s", name))));
      end if;
    else
      let checked = convert-and-check(binding.binding-type-var, identity);
      build-assignment(builder, policy, source,
		       binding.binding-var, checked);
      deliver-result(builder, policy, source,
		     want, datum, checked);
    end if;
  else
    compiler-error-location(id, "Undefined variable: %s", name);
    deliver-result
      (builder, policy, source, want, datum,
       make-error-operation
	 (builder, policy, source,
	  "Undefined variable: %s",
	  make-literal-constant
	    (builder, format-to-string("%s", name))));
  end if;
end method fer-convert;

define constant $arg-names
  = #[#"arg0", #"arg1", #"arg2", #"arg3", #"arg4", #"arg5", #"arg6", #"arg7",
	#"arg8", #"arg9"];

define method fer-convert
    (builder :: <fer-builder>, form :: <funcall-parse>, lexenv :: <lexenv>,
     want :: <result-designator>, datum :: <result-datum>)
    => res :: <result>;
  let func = fer-convert(builder, form.funcall-function,
			 make(<lexenv>, inside: lexenv),
			 #"leaf", #"function");
  let ops = make(<list>, size: form.funcall-arguments.size);
  for (arg in form.funcall-arguments,
       op-ptr = ops then op-ptr.tail,
       index from 0)
    let name = if (index < $arg-names.size)
		 $arg-names[index];
	       else
		 as(<symbol>, format-to-string("arg%d", index));
	       end;
    op-ptr.head := fer-convert(builder, arg, make(<lexenv>, inside: lexenv),
			       #"leaf", name);
  end;
  let ct-source-location 
    = make-literal-constant(builder, format-to-string("%=", form.source-location));
  deliver-result
    (builder, lexenv.lexenv-policy, form.source-location, want, datum,
     make-unknown-call(builder, func, #f, ops, 
                       ct-source-location: ct-source-location));
end;

define method fer-convert
    (builder :: <fer-builder>, form :: <dot-parse>, lexenv :: <lexenv>,
     want :: <result-designator>, datum :: <result-datum>)
    => res :: <result>;
  let arg-leaf = fer-convert(builder, form.dot-operand,
			     make(<lexenv>, inside: lexenv),
			     #"leaf", #"argument");
  let fun-leaf = fer-convert(builder, make(<varref-parse>, id: form.dot-name),
			     make(<lexenv>, inside: lexenv),
			     #"leaf", #"function");
  let ct-source-location 
    = make-literal-constant(builder, format-to-string("%=", form.source-location));
  deliver-result
    (builder, lexenv.lexenv-policy, form.source-location, want, datum,
     make-unknown-call(builder, fun-leaf, #f, list(arg-leaf),
                       ct-source-location: ct-source-location));
end;

define method fer-convert
    (builder :: <fer-builder>, form :: <body-parse>, lexenv :: <lexenv>,
     want :: <result-designator>, datum :: <result-datum>)
    => res :: <result>;
  let lexenv = make(<body-lexenv>, inside: lexenv);
  let source = form.source-location;
  let body = form.body-parts;
  let result
    = if (empty?(body))
	deliver-result(builder, lexenv.lexenv-policy, source, want, datum, #f);
      else
	for (i from 0 below body.size - 1)
	  fer-convert(builder, body[i], lexenv, #"nothing", #f);
	end;
	fer-convert(builder, body[body.size - 1], lexenv, want, datum);
      end;
  
  unless (zero?(lexenv.lexenv-handlers))
    let policy = lexenv.lexenv-policy;
    let pop-handler = ref-dylan-defn(builder, policy, source, #"pop-handler");
    for (i from 0 below lexenv.lexenv-handlers)
      build-assignment(builder, policy, source, #(),
		       make-unknown-call(builder, pop-handler, #f, #()));
    end;
  end;

  result;
end;

define method fer-convert
    (builder :: <fer-builder>, form :: <bind-exit-parse>, lexenv :: <lexenv>,
     want :: <result-designator>, datum :: <result-datum>)
    => res :: <result>;
  let source = form.source-location;
  let nlx-info = make(<nlx-info>);
  let name = form.exit-name;
  let state-type = specifier-type(#"<raw-pointer>");
  let saved-state-var = make-local-var(builder, #"saved-state", state-type);
  let policy = lexenv.lexenv-policy;
  let body-region
    = build-function-body
        (builder, policy, source, #t,
	 make(<internal-name>, symbol: name.token-symbol,
	      base: lexenv.lexenv-method-name),
	 list(saved-state-var), wild-ctype(), #f);
  let body-sig = make(<signature>, specializers: list(state-type));
  let body-literal
    = make-function-literal(builder, #f, #"function", #"local", body-sig,
			    body-region);
  let catcher-var
    = make-lexical-var(builder, symcat(name.token-symbol, "-catcher"),
		       source, object-ctype());
  build-let(builder, policy, source, catcher-var,
	    make-operation(builder, <make-catcher>, list(saved-state-var),
			   nlx-info: nlx-info));
  let lexenv = make(<lexenv>, inside: lexenv);
  let exit = make-lexical-var(builder, name.token-symbol, source,
			      function-ctype());
  add-binding(lexenv, name, exit);
  build-let(builder, policy, source, exit,
	    make-exit-function(builder, nlx-info, catcher-var));
  let cluster = make-values-cluster(builder, #"results", wild-ctype());
  fer-convert(builder, form.exit-body, lexenv, #"assignment", cluster);
  build-assignment
    (builder, policy, source, #(),
     make-operation(builder, <disable-catcher>, list(catcher-var),
		    nlx-info: nlx-info));
  build-return(builder, policy, source, body-region, cluster);
  end-body(builder);
  deliver-result(builder, lexenv.lexenv-policy, source, want, datum,
		 make-operation(builder, <catch>, list(body-literal),
				nlx-info: nlx-info));
end;

define method fer-convert
    (builder :: <fer-builder>, form :: <if-parse>, lexenv :: <lexenv>,
     want :: one-of(#"leaf", #"expr"), datum :: <result-datum>)
    => res :: <result>;
  let leaf = make-local-var(builder, datum, object-ctype());
  fer-convert(builder, form, lexenv, #"assignment", leaf);
  leaf;
end;

define method fer-convert
    (builder :: <fer-builder>, form :: <if-parse>, lexenv :: <lexenv>,
     want :: one-of(#"nothing", #"assignment"), datum :: <result-datum>)
    => res :: <result>;
  let source = form.source-location;
  build-if-body(builder, lexenv.lexenv-policy, source,
		fer-convert(builder, form.if-condition,
			    make(<lexenv>, inside: lexenv),
			    #"leaf", #"condition"));
  fer-convert(builder, form.if-consequent, make(<lexenv>, inside: lexenv),
	      want, datum);
  build-else(builder, lexenv.lexenv-policy, source);
  fer-convert(builder, form.if-alternate, make(<lexenv>, inside: lexenv),
	      want, datum);
  end-body(builder);
  #f;
end;

define method fer-convert
    (builder :: <fer-builder>, form :: <method-ref-parse>, lexenv :: <lexenv>,
     want :: <result-designator>, datum :: <result-datum>)
    => res :: <result>;
  let source = form.source-location;
  let temp = make-local-var(builder, #"method", function-ctype());
  build-assignment(builder, lexenv.lexenv-policy, source, temp,
		   fer-convert-method(builder, form.method-ref-method,
		   		      make(<internal-name>, symbol: #"method",
				      	   base: lexenv.lexenv-method-name),
				      #f, #"local", lexenv, lexenv));
  deliver-result(builder, lexenv.lexenv-policy, source, want, datum, temp);
end;

define method fer-convert
    (builder :: <fer-builder>, form :: <callback-method-ref-parse>,
     lexenv :: <lexenv>, want :: <result-designator>, datum :: <result-datum>)
    => res :: <result>;
  let source = form.source-location;
  let temp = make-local-var(builder, #"method", function-ctype());
  build-assignment(builder, lexenv.lexenv-policy, source, temp,
		   fer-convert-callback-method(builder,
						 form.method-ref-method,
		   		      make(<internal-name>, symbol: #"method",
				      	   base: lexenv.lexenv-method-name),
				      #f, #"local", lexenv, lexenv));
  deliver-result(builder, lexenv.lexenv-policy, source, want, datum, temp);
end;

define method fer-convert
    (builder :: <fer-builder>, form :: <primitive-parse>, lexenv :: <lexenv>,
     want :: <result-designator>, datum :: <result-datum>)
    => res :: <result>;
  let name = form.primitive-name.token-symbol;
  let info = primitive-info-or-lose(name);
  let operands = form.primitive-operands;
  let ops = make(<list>, size: operands.size);
  local
    method repeat (op-ptr :: <list>, index :: <integer>, types :: <list>)
      if (op-ptr == #())
	unless (types == #() | types.head == #"rest")
	  compiler-fatal-error-location
	    (form, "Too few arguments to %%%%primitive %s", name);
	end;
      elseif (types == #())
	compiler-fatal-error-location
	  (form, "Too many arguments to %%%%primitive %s", name);
      else
	let (type, remaining-types)
	  = if (types.head == #"rest")
	      values(types.tail.head, types);
	    else
	      values(types.head, types.tail);
	    end;
	let name = if (index < $arg-names.size)
		     $arg-names[index];
		   else
		     as(<symbol>, format-to-string("arg%d", index));
		   end;
	let arg = operands[index];
	let lexenv = make(<lexenv>, inside: lexenv);
	let var = if (type == #"cluster")
		    make-values-cluster(builder, name, wild-ctype());
		  else
		    make-local-var(builder, name, type);
		  end;
	fer-convert(builder, arg, lexenv, #"assignment", var);
	op-ptr.head := var;
	repeat(op-ptr.tail, index + 1, remaining-types);
      end;
    end;
  repeat(ops, 0, info.priminfo-arg-types);
  deliver-result
    (builder, lexenv.lexenv-policy, form.source-location, want, datum,
     make-operation(builder, <primitive>, ops, name: name));
end;

define method fer-convert
    (builder :: <fer-builder>, form :: <unwind-protect-parse>,
     lexenv :: <lexenv>, want :: <result-designator>, datum :: <result-datum>)
    => res :: <result>;
  let source = form.source-location;
  let policy = lexenv.lexenv-policy;
  let cleanup-builder = make-builder(builder);
  let cleanup-region
    = build-function-body
        (cleanup-builder, policy, source, #t,
	  make(<internal-name>, symbol: #"Block-Cleanup",
	       base: lexenv.lexenv-method-name),
	  #(), make-values-ctype(#(), #f), #f);
  let cleanup-literal
    = make-function-literal(cleanup-builder, #f, #"function", #"local",
			    make(<signature>, specializers: #(),
				 returns: make-values-ctype(#(), #f)),
			    cleanup-region);

  build-unwind-protect-body(builder, policy, source, cleanup-literal);
  build-assignment
    (builder, policy, source, #(),
     make-unknown-call
       (builder,
	ref-dylan-defn(builder, policy, source, #"push-unwind-protect"),
	#f,
	list(cleanup-literal)));
  let res = fer-convert(builder, form.uwp-body, make(<lexenv>, inside: lexenv),
			want, datum);
  build-assignment
    (builder, policy, source, #(),
     make-unknown-call
       (builder,
	ref-dylan-defn(builder, policy, source, #"pop-unwind-protect"),
	#f, #()));
  build-assignment(builder, policy, source, #(),
		   make-unknown-call(builder, cleanup-literal, #f, #()));
  end-body(builder);

  fer-convert(cleanup-builder, form.uwp-cleanup,
	      make(<lexenv>, inside: lexenv), #"nothing", #f);
  build-return(cleanup-builder, policy, source, cleanup-region, #());
  end-body(cleanup-builder);

  res;
end;


// Method conversion.

// fer-convert-method  --  Exported
//
define function fer-convert-method
    (builder :: <fer-builder>, meth :: <method-parse>,
     name :: <name>, ctv :: false-or(<ct-function>),
     visibility :: <function-visibility>, specializer-lexenv :: <lexenv>,
     lexenv :: <lexenv>,
     #key next-method-info :: false-or(<list>))
    => res :: <leaf>;
  let lexenv = make(<lexenv>, inside: lexenv);
  let source = meth.method-body.source-location; // FIXME: better change lines make(<method-parse> in parser.input
  // FIXME: and "method-definition (method-and-name method-body END method-and-name-opt)" to update source-loc
  let policy = lexenv.lexenv-policy;
  let specializer-policy = specializer-lexenv.lexenv-policy;

  local
    method param-type-and-var (param)
      if (param.param-type)
	let type = ct-eval(param.param-type, specializer-lexenv);
	if (type)
	  values(type, #f);
	else
	  let temp = make-local-var(builder, #"type",
				    specifier-type(#"<type>"));
	  fer-convert(builder, param.param-type,
		      make(<lexenv>, inside: specializer-lexenv),
		      #"assignment", temp);
	  let var = make-lexical-var(builder, #"type", source,
				     specifier-type(#"<type>"));
	  build-let(builder, specializer-policy, source,
		    var, temp);
	  values(object-ctype(), var);
	end;
      else
	values(object-ctype(), #f);
      end;
    end;

  let paramlist = meth.method-parameters;
  let vars = make(<stretchy-vector>);
  let body-builder = make-builder(builder);

  let specializers = make(<stretchy-vector>);
  let specializer-leaves = make(<stretchy-vector>);
  let non-const-arg-types? = #f;
  for (param in paramlist.varlist-fixed)
    let (type, type-var) = param-type-and-var(param);
    add!(specializers, type);
    let name = param.param-name;
    let var = make-lexical-var(body-builder, name.token-symbol, source, type);
    add-binding(lexenv, name, var, type-var: type-var);
    add!(vars, var);
    if (type-var)
      non-const-arg-types? := #t;
      add!(specializer-leaves, type-var);
    else
      add!(specializer-leaves, make-literal-constant(body-builder, type));
    end;
  end;
  let next = paramlist.paramlist-next;
  let rest = paramlist.varlist-rest;
  let rest-var
    = if (rest)
	if (rest.param-type)
	  compiler-warning-location
	    (rest.param-type,
	     "#rest parameters can't have a type -- ignoring");
	end;
	let name = rest.param-name;
	let var = make-lexical-var(body-builder, name.token-symbol, source,
				   // ### should this be <object>?
				   specifier-type(#"<simple-object-vector>"));
	add-binding(lexenv, name, var);
	var;
      elseif (next & paramlist.paramlist-keys)
	make-lexical-var(body-builder, #"rest", source,
			 specifier-type(#"<simple-object-vector>"));
      end;
  if (next)
    //
    // Make the next-info var.
    let next-info-var
      = make-lexical-var(body-builder, #"next-method-info", source,
			 specifier-type(#"<list>"));
    //
    // Make the actual #next var.
    let var = make-lexical-var(body-builder, next.token-symbol, source,
			       object-ctype());
    add-binding(lexenv, next, var);
    //
    // And bind it up.
    if (next-method-info == #())
      // We can statically tell that there is no next method.  So just bind
      // the #next var to #f.
      build-let(body-builder, policy, source, var,
		make-literal-constant(body-builder, #f));
    else
      let orig-args-var
	= make-local-var(body-builder, #"orig-args",
			 specifier-type(#"<simple-object-vector>"));
      if (rest-var)
	let fixed = make-values-cluster(builder, #"fixed", wild-ctype());
	build-assignment
	  (body-builder, policy, source, fixed,
	   make-operation
	     (body-builder, <primitive>, as(<list>, vars), name: #"values"));
	let rest = make-values-cluster(builder, #"rest", wild-ctype());
	build-assignment
	  (body-builder, policy, source, rest,
	   make-operation
	     (body-builder, <primitive>, list(rest-var),
	      name: #"values-sequence"));
	let cluster = make-values-cluster(builder, #"cluster", wild-ctype());
	build-assignment
	  (body-builder, policy, source, cluster,
	   make-operation
	     (body-builder, <primitive>, list(fixed, rest),
	      name: #"merge-clusters"));
	build-assignment
	  (body-builder, policy, source, orig-args-var,
	   make-operation
	     (body-builder, <primitive>,
	      list(cluster,
		   make-literal-constant(body-builder, 0)),
	      name: #"canonicalize-results"));
      else
	build-assignment
	  (body-builder, policy, source, orig-args-var,
	   make-operation
	     (body-builder, <primitive>, as(<list>, vars), name: #"vector"));
      end if;

      build-let
	(body-builder, policy, source, var,
	 make-operation
	   (body-builder, <primitive>,
	    list(if (next-method-info)
		   make-literal-constant(body-builder, next-method-info);
		 else
		   next-info-var;
		 end if,
		 orig-args-var),
	    name: #"make-next-method"));
    end if;
    //
    // Add the next-info-var to the vars.  We have to add it here because we
    // don't want it being included in the original-args vector.
    add!(vars, next-info-var);
  end if;

  if (rest-var)
    // This has to be added after the next-info-var so that the vars show
    // up in the correct order.
    add!(vars, rest-var);
  end;

  let keyword-infos
    = if (paramlist.paramlist-keys)
	let infos = make(<stretchy-vector>);
	for (param in paramlist.paramlist-keys)
	  let name = param.param-name;
	  let (type, type-var) = param-type-and-var(param);
	  let var = make-lexical-var(body-builder, name.token-symbol,
				     source, type);
	  let default = if (param.param-default)
			  ct-eval(param.param-default, lexenv);
			else
			  make(<literal-false>);
			end;
	  if (default)
	    add!(infos,
		 make(<key-info>, key-name: param.param-keyword,
		      default: default, type: type,
		      required: ~cinstance?(default, type)));
	    add!(vars, var);
	  else
	    let temp = make-local-var(body-builder, name.token-symbol, type);
	    let pre-default
	      = make-lexical-var(body-builder, name.token-symbol,
				 source, type);
	    let info = make(<key-info>, key-name: param.param-keyword,
			    type: type, default: #f);
	    add!(infos, info);
	    add!(vars, pre-default);
	    let supplied?-var
	      = make-lexical-var(body-builder,
				 as(<symbol>,
				    format-to-string("%s-supplied?",
						     name.token-symbol)),
				 source,
				 specifier-type(#"<boolean>"));
	    let rep = pick-representation(type, #"speed");
	    if (rep.representation-has-bottom-value?)
	      build-let
		(body-builder, policy, source, supplied?-var,
		 make-operation
		   (body-builder, <primitive>, list(pre-default),
		    name: #"initialized?"));
	    else
	      add!(vars, supplied?-var);
	    end;
	    build-if-body(body-builder, policy, source, supplied?-var);
	    build-assignment(body-builder, policy, source, temp, pre-default);
	    build-else(body-builder, policy, source);
	    fer-convert(body-builder, param.param-default,
			make(<lexenv>, inside: lexenv),
			#"assignment", temp);
	    end-body(body-builder);
	    build-let(body-builder, policy, source, var, temp);
	  end;
	  if (type-var)
	    non-const-arg-types? := #t;
	    let checked = make-lexical-var(body-builder, name.token-symbol,
					   source, object-ctype());
	    build-assignment
	      (body-builder, policy, source, checked,
	       make-check-type-operation
		 (body-builder, policy, source, var, type-var));
	    add-binding(lexenv, name, checked, type-var: type-var);
	  else
	    add-binding(lexenv, name, var);
	  end;
	end;
	as(<list>, infos);
      end;
  
  let returns = meth.method-returns;
  let fixed-results = make(<stretchy-vector>);
  let checked-fixed-results = make(<stretchy-vector>);
  let result-types = make(<stretchy-vector>);
  let result-type-leaves = make(<stretchy-vector>);
  let non-const-result-types? = #f;
  let result-check-builder = make-builder(builder);
  for (param in returns.varlist-fixed)
    let (type, type-var) = param-type-and-var(param);
    add!(result-types, type);
    let var
      = make-local-var(result-check-builder, param.param-name.token-symbol,
		       type);
    if (type-var)
      non-const-result-types? := #t;
      add!(result-type-leaves, type-var);
      let temp = make-local-var(result-check-builder,
				param.param-name.token-symbol,
				type);
      add!(fixed-results, temp);
      build-assignment
	(result-check-builder, specializer-policy, source, var,
	 make-check-type-operation
	   (result-check-builder, specializer-policy, source,
	    temp, type-var));
    else
      add!(result-type-leaves,
	   make-literal-constant(result-check-builder, type));
      add!(fixed-results, var);
    end;
    add!(checked-fixed-results, var);
  end;

  let (rest-type, rest-type-leaf, need-to-check-rest?)
    = if (returns.varlist-rest)
	let (type, type-var) = param-type-and-var(returns.varlist-rest);

	if (type-var)
	  non-const-result-types? := #t;
	  values(type, type-var, #t);
	else
	  values(type,
		 make-literal-constant(builder, type),
		 ~(type == object-ctype()));
	end;
      end;

  let vars = as(<list>, vars);
  let result-type = make-values-ctype(as(<list>, result-types), rest-type);
  let lambda?
    = visibility == #"local" | non-const-arg-types? | non-const-result-types?;
  let function-region
    = build-function-body(builder, policy, source, lambda?,
			  name, vars, result-type, ~lambda?);

  build-region(builder, builder-result(body-builder));

  if (rest-type)
    let rest-symbol = returns.varlist-rest.param-name.token-symbol;
    if (empty?(fixed-results) & ~need-to-check-rest?)
      let cluster
	= make-values-cluster(builder, rest-symbol, wild-ctype());
      fer-convert(builder, meth.method-body, lexenv, #"assignment", cluster);
      build-return(builder, policy, source, function-region, cluster);
    else
      let cluster = make-values-cluster(builder, #"results", wild-ctype());
      fer-convert(builder, meth.method-body, lexenv, #"assignment", cluster);

      let rest-result
	= make-local-var(builder, rest-symbol, object-ctype());
      build-assignment
	(builder, policy, source,
	 concatenate(as(<list>, fixed-results), list(rest-result)),
	 make-operation(builder, <primitive>,
			list(cluster,
			     make-literal-constant
			       (builder, fixed-results.size)),
			name: #"canonicalize-results"));

      build-region(builder, builder-result(result-check-builder));

      let checked-rest-result
	= if (need-to-check-rest?)
	    let temp = make-local-var(builder, rest-symbol, object-ctype());
	    build-assignment
	      (builder, policy, source, temp,
	       make-unknown-call
		 (builder,
		  ref-dylan-defn(builder, policy, source, #"check-types"),
		  #f, list(rest-result, rest-type-leaf)));
	    temp;
	  else
	    rest-result;
	  end if;

      let checked-cluster
	= make-values-cluster(builder, #"results", wild-ctype());

      let args = make(<stretchy-vector>);
      add!(args, ref-dylan-defn(builder, policy, source, #"values"));
      for (fixed in checked-fixed-results)
	add!(args, fixed);
      end;
      add!(args, checked-rest-result);
      build-assignment
	(builder, policy, source, checked-cluster,
	 make-unknown-call
	   (builder, ref-dylan-defn(builder, policy, source, #"apply"), #f,
	    as(<list>, args)));
      build-return(builder, policy, source, function-region, checked-cluster);
    end;
  else
    fer-convert(builder, meth.method-body, lexenv, #"assignment",
		as(<list>, fixed-results));
    build-region(builder, builder-result(result-check-builder));
    build-return(builder, policy, source,
		 function-region, as(<list>, checked-fixed-results));
  end;

  end-body(builder);

  let signature
    = make(<signature>,
	   specializers: as(<list>, specializers),
	   next: next & #t,
	   rest-type: rest & object-ctype(),
	   keys: keyword-infos & as(<list>, keyword-infos),
	   all-keys: paramlist.paramlist-all-keys?,
	   returns: make-values-ctype(as(<list>, result-types),
	   			      rest-type));

  if (non-const-arg-types? | non-const-result-types?)
    assert(ctv == #f);
    local
      method build-call (name, args)
	let temp = make-local-var(builder, name, object-ctype());
	build-assignment
	  (builder, policy, source, temp,
	   make-unknown-call
	     (builder, ref-dylan-defn(builder, policy, source, name), #f,
	      as(<list>, args)));
	temp;
      end;
    build-call(#"%make-method",
	       list(build-call(#"vector", specializer-leaves),
		    build-call(#"vector", result-type-leaves),
		    rest-type-leaf
		      | make-literal-constant(builder, empty-ctype()),
		    make-function-literal(builder, #f, #"method", #"local",
					  signature, function-region)));
  else
    make-function-literal(builder, ctv, #"method", visibility, signature,
			  function-region);
  end;
end;

// fer-convert-callback-method
//
define function fer-convert-callback-method
    (builder :: <fer-builder>, meth :: <callback-method-parse>,
     name :: <name>, ctv :: false-or(<ct-function>),
     visibility :: <function-visibility>, specializer-lexenv :: <lexenv>,
     lexenv :: <lexenv>,
     #key next-method-info :: false-or(<list>))
    => res :: <leaf>;
  let lexenv = make(<lexenv>, inside: lexenv);
  let source = meth.source-location;
  let policy = lexenv.lexenv-policy;
  let specializer-policy = specializer-lexenv.lexenv-policy;

  let paramlist = meth.method-parameters;
  let vars = make(<stretchy-vector>);
  let body-builder = make-builder(builder);

  let specializers = make(<stretchy-vector>);
  let non-const-arg-types? = #f;
  for (param in paramlist.varlist-fixed)
    let type
      = if (param.param-type)
	  let ct-type = ct-eval(param.param-type, specializer-lexenv);
	  if (ct-type)
	    ct-type;
	  else
	    compiler-error-location
	      (param.param-type,
	       "callback-method types must be compile-time constants");
	  end;
	else
	  object-ctype();
	end;

    add!(specializers, type);
    let name = param.param-name;
    let var = make-lexical-var(body-builder, name.token-symbol, source, type);
    add-binding(lexenv, name, var);
    add!(vars, var);
  end;

  if (paramlist.varlist-rest)
    compiler-error-location
      (meth, "callback-methods can't have #rest arguments");
  end;
  if (paramlist.paramlist-next)
    compiler-error-location
      (meth, "callback-methods can't have #next arguments");
  end if;
  if (paramlist.paramlist-keys)
    compiler-error-location
      (meth, "callback-method can't have keyword arguments");
  end if;

  let returns = meth.method-returns;
  let fixed-results = make(<stretchy-vector>);
  let checked-fixed-results = make(<stretchy-vector>);
  let result-types = make(<stretchy-vector>);
  let non-const-result-types? = #f;
  let result-check-builder = make-builder(builder);
  for (param in returns.varlist-fixed)
    let type
      = if (param.param-type)
	  let ct-type = ct-eval(param.param-type, specializer-lexenv);
	  if (ct-type)
	    ct-type;
	  else
	    compiler-error-location
	      (param.param-type,
	       "callback-method types must be compile-time constants");
	  end;
	else
	  object-ctype();
	end;
    add!(result-types, type);
    let var
      = make-local-var(result-check-builder, param.param-name.token-symbol,
		       type);
    add!(fixed-results, var);
    add!(checked-fixed-results, var);
  end;

  if(checked-fixed-results.size > 1 | returns.varlist-rest)
    compiler-error-location
      (meth, "callback-method can only return one value");
  end if;

  let vars = as(<list>, vars);
  let result-type = make-values-ctype(as(<list>, result-types), #f);
  let lambda? = visibility == #"local";
  let function-region
    = build-function-body(builder, policy, source, lambda?,
			  name, vars, result-type, ~lambda?);

  build-region(builder, builder-result(body-builder));

  fer-convert(builder, meth.method-body, lexenv, #"assignment",
	      as(<list>, fixed-results));
  build-region(builder, builder-result(result-check-builder));
  build-return(builder, policy, source,
		 function-region, as(<list>, checked-fixed-results));

  end-body(builder);

  let signature
    = make(<signature>,
	   specializers: as(<list>, specializers),
	   next: #f,
	   rest-type: #f,
	   keys: #f,
	   all-keys: #f,
	   returns: make-values-ctype(as(<list>, result-types), #f));

  make-function-literal(builder, ctv, #"callback", visibility, signature,
			function-region);
end;
