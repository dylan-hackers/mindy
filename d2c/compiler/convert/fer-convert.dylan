module: fer-convert
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/convert/fer-convert.dylan,v 1.6 1994/12/16 11:54:35 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

define constant <var-or-vars> = union(<abstract-variable>, <list>);

define constant source = make(<source-location>);

define generic fer-convert (builder :: <fer-builder>,
			    form :: <constituent>,
			    lexenv :: <lexenv>,
			    target-vars :: <var-or-vars>)
    => ();


define method fer-convert-body (builder :: <fer-builder>,
				body :: <simple-object-vector>,
				lexenv :: <lexenv>,
				target-vars :: <var-or-vars>)
    => ();
  if (empty?(body))
    build-assignment(builder, lexenv.lexenv-policy, source, target-vars,
		     make-literal-constant(builder,
					   make(<ct-literal>, value: #f)));
  else
    for (i from 0 below body.size - 1)
      fer-convert(builder, body[i], lexenv, #());
    end;
    fer-convert(builder, body[body.size - 1], lexenv, target-vars);
  end;
end;

define method fer-convert (builder :: <fer-builder>, form :: <constituent>,
			   lexenv :: <lexenv>, target-vars :: <var-or-vars>)
    => ();
  let expansion = expand(form, lexenv);
  if (expansion)
    fer-convert-body(builder, expansion, lexenv, target-vars);
  else
    error("Can't fer-convert %=", form);
  end;
end;


define method fer-convert (builder :: <fer-builder>, form :: <let>,
			   lexenv :: <lexenv>, target-vars :: <var-or-vars>)
    => ();
  let bindings = form.let-bindings;
  let paramlist = bindings.bindings-parameter-list;
  let params = paramlist.paramlist-required-vars;
  let rest = paramlist.paramlist-rest;
  let rest-temp
    = rest & make-local-var(builder, rest.token-symbol, object-ctype());
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
	      = make-local-var(builder, #"type", dylan-value(#"<type>"));
	    fer-convert(builder, param.param-type,
			make(<lexenv>, inside: lexenv),
			type-local);
	    let type-temp
	      = make-lexical-var(builder, #"type", source,
				 dylan-value(#"<type>"));
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
  if (paramlist.paramlist-rest)
    let cluster = make-values-cluster(builder, #"temps", wild-ctype());
    fer-convert(builder, bindings.bindings-expression,
		make(<lexenv>, inside: lexenv), cluster);
    canonicalize-results(builder, lexenv.lexenv-policy, source,
			 cluster, temps, rest-temp);
  else
    fer-convert(builder, bindings.bindings-expression,
		make(<lexenv>, inside: lexenv), temps);
  end;

  // Copy the temps into real lexical vars and update the lexenv with em.
  for (param in params, type in types, type-temp in type-temps, temp in temps)
    let name = param.param-name;
    let var = make-lexical-var(builder, name.token-symbol, source, type);
    add-binding(lexenv, name, var, type-var: type-temp);
    build-let(builder, lexenv.lexenv-policy, source, var,
	      if (type-temp)
		make-check-type-operation(builder, temp, type-temp);
	      else
		temp;
	      end);
  end;
  if (rest)
    let var = make-lexical-var(builder, rest.token-symbol, source,
			       object-ctype());
    add-binding(lexenv, rest, var);
    build-let(builder, lexenv.lexenv-policy, source, var, rest-temp);
  end;

  // Supply #f as the result.
  unless (target-vars == #())
    build-assignment(builder, lexenv.lexenv-policy, source, target-vars,
		     make-literal-constant(builder,
					   make(<ct-literal>, value: #f)));
  end;
end;

define method fer-convert (builder :: <fer-builder>, form :: <local>,
			   lexenv :: <lexenv>, target-vars :: <var-or-vars>)
    => ();
  let specializer-lexenv = make(<lexenv>, inside: lexenv);
  let vars
    = map(method (meth)
	    let name = meth.method-name;
	    let var = make-lexical-var(builder, name.token-symbol, source,
				       function-ctype());
	    add-binding(lexenv, name, var);
	    var;
	  end,
	  form.local-methods);
  for (var in vars, meth in form.local-methods)
    build-let(builder, lexenv.lexenv-policy, source, var,
	      build-general-method(builder, meth, specializer-lexenv, lexenv));
  end;
  unless (target-vars == #())
    build-assignment(builder, lexenv.lexenv-policy, source, target-vars,
		     make-literal-constant(builder,
					   make(<ct-literal>, value: #f)));
  end;
end;

define method fer-convert (builder :: <fer-builder>, form :: <literal>,
			   lexenv :: <lexenv>, target-vars :: <var-or-vars>)
    => ();
  build-assignment
    (builder, lexenv.lexenv-policy, source, target-vars,
     make-literal-constant(builder, make(<ct-literal>,value: form.lit-value)));
end;

define constant $arg-names
  = #[#"arg0", #"arg1", #"arg2", #"arg3", #"arg4", #"arg5", #"arg6", #"arg7",
	#"arg8", #"arg9"];

define method fer-convert (builder :: <fer-builder>, form :: <funcall>,
			   lexenv :: <lexenv>, target-vars :: <var-or-vars>)
    => ();
  let expansion = expand(form, lexenv);
  if (expansion)
    fer-convert-body(builder, expansion, lexenv, target-vars);
  else
    let fun-temp = make-local-var(builder, #"function", function-ctype());
    fer-convert(builder, form.funcall-function, make(<lexenv>, inside: lexenv),
		fun-temp);
    let ops = make(<list>, size: form.funcall-arguments.size + 1);
    ops.head := fun-temp;
    for (arg in form.funcall-arguments,
	 op-ptr = ops.tail then op-ptr.tail,
	 index from 0)
      let temp = make-local-var(builder,
				if (index < $arg-names.size)
				  $arg-names[index];
				else
				  as(<symbol>,
				     format-to-string("arg%d", index));
				end,
				object-ctype());
      fer-convert(builder, arg, make(<lexenv>, inside: lexenv), temp);
      op-ptr.head := temp;
    end;
    build-assignment(builder, lexenv.lexenv-policy, source,
		     target-vars, make-operation(builder, ops));
  end;
end;

define method fer-convert (builder :: <fer-builder>, form :: <dot>,
			   lexenv :: <lexenv>, target-vars :: <var-or-vars>)
    => ();
  let expansion = expand(form, lexenv);
  if (expansion)
    fer-convert-body(builder, expansion, lexenv, target-vars);
  else
    let arg-temp = make-local-var(builder, #"argument", object-ctype());
    fer-convert(builder, form.dot-operand, make(<lexenv>, inside: lexenv),
		arg-temp);
    let fun-temp = make-local-var(builder, #"function", function-ctype());
    fer-convert(builder, make(<varref>, name: form.dot-name),
		lexenv, fun-temp);
    build-assignment(builder, lexenv.lexenv-policy, source, target-vars,
		     make-operation(builder, list(fun-temp, arg-temp)));
  end;
end;

define method fer-convert (builder :: <fer-builder>, form :: <varref>,
			   lexenv :: <lexenv>, target-vars :: <var-or-vars>)
    => ();
  let name = form.varref-name;
  let binding = find-binding(lexenv, name);
  build-assignment
    (builder, lexenv.lexenv-policy, source, target-vars,
     if (binding)
       binding.binding-var;
     else
       let var = find-variable(name.token-module, name.token-symbol);
       let defn = var & var.variable-definition;
       if (defn)
	 make-definition-leaf(builder, defn);
       else
	 make-error-operation(builder, "Undefined variable");
       end;
     end);
end;

define method fer-convert (builder :: <fer-builder>, form :: <assignment>,
			   lexenv :: <lexenv>, target-vars :: <var-or-vars>)
    => ();
  let expansion = expand(form, lexenv);
  if (expansion)
    fer-convert-body(builder, expansion, lexenv, target-vars);
  else
    let place = form.assignment-place;
    unless (instance?(place, <varref>))
      error("Assignment to complex place didn't get expanded away?");
    end;
    let name = place.varref-name;
    let temp = make-local-var(builder, #"temp", object-ctype());
    fer-convert(builder, form.assignment-value, make(<lexenv>, inside: lexenv),
		temp);
    let binding = find-binding(lexenv, name);
    let (leaf, type-leaf)
      = if (binding)
	  values(binding.binding-var, binding.binding-type-var);
	else
	  let var = find-variable(name.token-module, name.token-symbol);
	  let defn = var & var.variable-definition;
	  if (~defn)
	    build-assignment
	      (builder, lexenv.lexenv-policy, source, target-vars,
	       make-error-operation(builder, "Undefined variable"));
	    values(#f, #f);
	  elseif (instance?(defn, <variable-definition>))
	    let type-defn = defn.var-defn-type-defn;
	    values(make-definition-leaf(builder, defn),
		   type-defn & make-definition-leaf(builder, type-defn));
	  else
	    build-assignment
	      (builder, lexenv.lexenv-policy, source, target-vars,
	       make-error-operation(builder,
				    "Can't assign constant module varaibles"));
	    values(#f, #f);
	  end;
	end;
    if (leaf)
      if (type-leaf)
	let checked = make-local-var(builder, #"checked", object-ctype());
	build-assignment(builder, lexenv.lexenv-policy, source, checked,
			 make-check-type-operation(builder, temp, type-leaf));
	build-assignment(builder, lexenv.lexenv-policy, source, leaf, checked);
	build-assignment(builder, lexenv.lexenv-policy, source, target-vars,
			 checked);
      else
	build-assignment(builder, lexenv.lexenv-policy, source, leaf, temp);
	build-assignment(builder, lexenv.lexenv-policy, source, target-vars,
			 temp);
      end;
    end;
  end;
end;

define method fer-convert (builder :: <fer-builder>, form :: <begin>,
			   lexenv :: <lexenv>, target-vars :: <var-or-vars>)
    => ();
  fer-convert-body(builder, form.begin-body, make(<lexenv>, inside: lexenv),
		   target-vars);
end;

define method fer-convert (builder :: <fer-builder>, form :: <bind-exit>,
			   lexenv :: <lexenv>, target-vars :: <var-or-vars>)
    => ();
  let blk = build-block-body(builder, lexenv.lexenv-policy, source);
  let lexenv = make(<lexenv>, inside: lexenv);
  let name = form.exit-name;
  let exit = make-lexical-var(builder, name.token-symbol, source,
			      function-ctype());
  add-binding(lexenv, name, exit);
  build-let(builder, lexenv.lexenv-policy, source, exit,
	    make-exit-function(builder, blk));
  fer-convert-body(builder, form.exit-body, lexenv, target-vars);
  end-body(builder);
end;

define method fer-convert (builder :: <fer-builder>, form :: <if>,
			   lexenv :: <lexenv>, target-vars :: <var-or-vars>)
    => ();
  let temp = make-local-var(builder, #"condition", object-ctype());
  fer-convert(builder, form.if-condition, make(<lexenv>, inside: lexenv),
	      temp);
  build-if-body(builder, lexenv.lexenv-policy, source, temp);
  fer-convert-body(builder, form.if-consequent, make(<lexenv>, inside: lexenv),
		   target-vars);
  build-else(builder, lexenv.lexenv-policy, source);
  fer-convert-body(builder, form.if-alternate, make(<lexenv>, inside: lexenv),
		   target-vars);
  end-body(builder);
end;

define method fer-convert (builder :: <fer-builder>, form :: <method-ref>,
			   lexenv :: <lexenv>, target-vars :: <var-or-vars>)
    => ();
  build-assignment(builder, lexenv.lexenv-policy, source, target-vars,
		   build-general-method(builder, form.method-ref-method,
					lexenv, lexenv));
end;

define method fer-convert (builder :: <fer-builder>, form :: <mv-call>,
			   lexenv :: <lexenv>, target-vars :: <var-or-vars>)
    => ();
  let ops = make(<list>, size: form.mv-call-operands.size);
  for (op in form.mv-call-operands,
       op-ptr = ops then op-ptr.tail,
       type = function-ctype() then object-ctype(),
       name = #"function" then #"mv-operand",
       index from 0)
    let temp = make-local-var(builder, name, type);
    fer-convert(builder, op, make(<lexenv>, inside: lexenv), temp);
    op-ptr.head := temp;
  end;
  build-assignment(builder, lexenv.lexenv-policy, source, target-vars,
		   make-mv-operation(builder, ops));
end;

// ### <uwp>?


// Method conversion.

define method build-general-method
    (builder :: <fer-builder>, meth :: <method-parse>,
     specializer-lexenv :: <lexenv>, lexenv :: <lexenv>)
    => res :: <leaf>;
  let lexenv = make(<lexenv>, inside: lexenv);

  local
    method param-type-and-var (param)
      if (param.param-type)
	let type = ct-eval(param.param-type, specializer-lexenv);
	if (type)
	  values(type, #f);
	else
	  let temp = make-local-var(builder, #"type", dylan-value(#"<type>"));
	  fer-convert(builder, param.param-type,
		      make(<lexenv>, inside: specializer-lexenv),
		      temp);
	  let var = make-lexical-var(builder, #"type", source,
				     dylan-value(#"<type>"));
	  build-let(builder, specializer-lexenv.lexenv-policy, source,
		    var, temp);
	  values(object-ctype(), var);
	end;
      else
	values(object-ctype(), #f);
      end;
    end;

  let paramlist = meth.method-param-list;
  let fixed-vars = make(<stretchy-vector>);
  let specializers = make(<stretchy-vector>);
  let non-const-arg-types? = #f;
  let arg-check-builder = make-builder(builder);
  for (param in paramlist.paramlist-required-vars)
    let (type, type-var) = param-type-and-var(param);
    let name = param.param-name;
    let var = make-lexical-var(builder, name.token-symbol, source, type);
    add-binding(lexenv, name, var, type-var: type-var);
    if (type-var)
      non-const-arg-types? := #t;
      add!(specializers, type-var);
      //let temp = make-lexical-var(builder, name.token-symbol, source, type);
      let temp = var;
      add!(fixed-vars, temp);
      let op = make-check-type-operation(arg-check-builder, temp, type-var);
      build-assignment(arg-check-builder, specializer-lexenv.lexenv-policy,
		       source, var, op);
    else
      add!(specializers, make-literal-constant(builder, type));
      add!(fixed-vars, var);
    end;
  end;
  let next-var
    = begin
	let next = paramlist.paramlist-next;
	if (next)
	  let var = make-lexical-var(builder, next.token-symbol, source,
				     object-ctype());
	  add-binding(lexenv, next, var);
	  var;
	end;
      end;
  let rest-var
    = begin
	let rest = paramlist.paramlist-rest;
	if (rest)
	  let var = make-lexical-var(builder, rest.token-symbol, source,
				     object-ctype());
	  add-binding(lexenv, rest, var);
	  var;
	end;
      end;
  let keywords
    = if (paramlist.paramlist-keys)
	let infos = make(<stretchy-vector>);
	for (param in paramlist.paramlist-keys)
	  let name = param.param-name;
	  let (type, type-var) = param-type-and-var(param);
	  let var = make-lexical-var(builder, name.token-symbol, source, type);
	  let default = ct-eval(param.param-default, lexenv);
	  if (default)
	    add!(infos, make(<keyword-info>, symbol: param.param-keyword,
			     var: var, default: default, type: type));
	  else
	    let pre-default
	      = make-lexical-var(builder, name.token-symbol, source,
				 object-ctype());
	    add!(infos, make(<keyword-info>, symbol: param.param-keyword,
			     var: pre-default, type: object-ctype(),
			     default: $Unbound-Marker-CT-Value));
	    let temp = make-local-var(builder, #"temp", object-ctype());
	    build-assignment
	      (builder, lexenv.lexenv-policy, source, temp,
	       make-operation
		 (builder,
		  list(dylan-defn-leaf(builder, #"=="),
		       pre-default,
		       make-literal-constant(builder,
					     $Unbound-Marker-CT-Value))));
	    build-if-body(builder, lexenv.lexenv-policy, source, temp);
	    fer-convert(builder, param.param-default,
			make(<lexenv>, inside: lexenv),
			var);
	    build-else(builder, lexenv.lexenv-policy, source);
	    build-assignment(builder, lexenv.lexenv-policy, source, var,
			     pre-default);
	    end-body(builder);
	  end;
	  if (type-var)
	    build-assignment
	      (builder, lexenv.lexenv-policy, source, var,
	       make-check-type-operation(builder, var, type-var));
	  end;
	  add-binding(lexenv, name, var, type-var: type-var);
	end;
	as(<list>, infos);
      end;
  
  let returns = meth.method-returns;
  let fixed-results = make(<stretchy-vector>);
  let non-const-result-types? = #f;
  let result-types = make(<stretchy-vector>);
  let result-check-builder = make-builder(builder);
  for (param in returns.paramlist-required-vars)
    let (type, type-var) = param-type-and-var(param);
    let var = make-local-var(builder, param.param-name.token-symbol, type);
    if (type-var)
      non-const-result-types? := #t;
      add!(result-types, type-var);
      let temp = make-local-var(builder, param.param-name.token-symbol, type);
      let op = make-check-type-operation(result-check-builder, var, type-var);
      build-assignment(result-check-builder, specializer-lexenv.lexenv-policy,
		       source, temp, op);
      add!(fixed-results, temp);
    else
      add!(result-types, make-literal-constant(builder, type));
      add!(fixed-results, var);
    end;
  end;

  let rest-result
    = if (returns.paramlist-rest)
	make-local-var(builder, returns.paramlist-rest.token-symbol,
		       object-ctype());
      end;

  let (results, results-temp)
    = if (non-const-result-types?)
	values(make-values-cluster(builder, #"results", wild-ctype()),
	       make-values-cluster(builder, #"temps", wild-ctype()));
      elseif (~rest-result)
	values(as(<list>, fixed-results), #f);
      elseif (empty?(fixed-results))
	values(make-values-cluster(builder, #"results", wild-ctype()), #f);
      else
	values(make-values-cluster(builder, #"results", wild-ctype()),
	       make-values-cluster(builder, #"temps", wild-ctype()));
      end;
  let method-literal
    = build-hairy-method-body(builder, lexenv.lexenv-policy, source,
			      fixed-vars, next-var, rest-var,
			      keywords, results);
  if (non-const-arg-types?)
    build-region(builder, builder-result(arg-check-builder));
  end;
  fer-convert-body(builder, meth.method-body, lexenv, results-temp | results);

  if (results-temp)
    canonicalize-results(builder, lexenv.lexenv-policy, source, results-temp,
			 as(<list>, fixed-results), rest-result);
    build-region(builder, builder-result(result-check-builder));
    let args = make(<stretchy-vector>);
    if (rest-result)
      add!(args, dylan-defn-leaf(builder, #"apply"));
    end;
    add!(args, dylan-defn-leaf(builder, #"values"));
    for (fixed in fixed-results)
      add!(args, fixed);
    end;
    if (rest-result)
      add!(args, rest-result);
    end;
    build-assignment(builder, lexenv.lexenv-policy, source, results,
		     make-operation(builder, as(<list>, args)));
  end;
  end-body(builder);

  if (non-const-arg-types? | non-const-result-types?)
    local
      method build-call (name, args)
	let ops = pair(dylan-defn-leaf(builder, name), as(<list>, args));
	let temp = make-local-var(builder, name, object-ctype());
	build-assignment(builder, lexenv.lexenv-policy, source, temp,
			 make-operation(builder, ops));
	temp;
      end;
    build-call(#"%make-method",
	       list(build-call(#"list", specializers),
		    build-call(#"list", result-types),
		    make-literal-constant
		      (builder,
		       if (rest-result)
			 object-ctype();
		       else
			 make(<ct-literal>, value: #f);
		       end),
		    method-literal));
  else
    method-literal;
  end;
end;



// build-hairy-method-body.

/*

define class <hairy-method-literal> (<object>)
  slot hairy-method-required-vars :: <list>,
    required-init-keyword: required-vars:;
  slot hairy-method-next-var :: union(<lexical-var>, <false>),
    required-init-keyword: next-var:;
  slot hairy-method-rest-var :: union(<lexical-var>, <false>),
    required-init-keyword: rest-var:;
  slot hairy-method-keywords :: union(<list>, <false>),
    required-init-keyword: keywords:;
  slot hairy-method-main-entry :: <bare-method-literal>,
    required-init-keyword: main-entry:;
  slot hairy-method-more-arg-entry :: union(<bare-method-literal>, <false>),
    init-value: #f;
  slot hairy-method-general-entry :: union(<bare-method-literal>, <false>),
    init-value: #f;
end;

*/

define class <keyword-info> (<object>)
  slot keyinfo-symbol :: <symbol>,
    required-init-keyword: symbol:;
  slot keyinfo-var :: <abstract-variable>,
    required-init-keyword: var:;
  slot keyinfo-default :: <ct-value>,
    required-init-keyword: default:;
  slot keyinfo-type :: <ctype>,
    required-init-keyword: type:;
end;

define generic build-hairy-method-body
    (builder :: <fer-builder>,
     policy :: <policy>,
     source :: <source-location>,
     fixed-vars :: <sequence>,
     next-var :: union(<abstract-variable>, <false>),
     rest-var :: union(<abstract-variable>, <false>),
     keywords :: union(<sequence>, <false>),
     result-vars :: <var-or-vars>)
    => res :: <leaf>;

define method build-hairy-method-body
    (builder :: <fer-builder>,
     policy :: <policy>,
     source :: <source-location>,
     fixed-vars :: <sequence>,
     next-var :: <false>,
     rest-var :: <false>,
     keywords :: <false>,
     result-vars :: <var-or-vars>)
    => res :: <leaf>;
  build-method-body(builder, policy, source, as(<list>, fixed-vars),
		    result-vars);
end;

/*

define method build-hairy-method-body
    (builder :: <fer-builder>,
     policy :: <policy>,
     source :: <source-location>,
     fixed-vars :: <sequence>,
     next-var :: union(<abstract-variable>, <false>),
     rest-var :: union(<abstract-variable>, <false>),
     keywords :: union(<sequence>, <false>),
     result-vars :: <var-or-vars>)
    => res :: <leaf>;
  let vars = map-as(<stretchy-vector>, identity, fixed-vars);
  let body-builder = make-builder(builder);
  if (next-var)
    let var = make-lexical-var(body-builder, #"next-method-info", source,
			       object-ctype());
    add!(vars, var);
    if (keywords & ~rest-var)
      rest-var := make-lexical-var(body-builder, #"rest", source,
				   object-ctype());
    end;
  end;
  if (rest-var)
    let context = make-lexical-var(body-builder, #"context", source,
				   object-ctype());
    let count = make-lexical-var(body-builder, #"count", source,
				 object-ctype());
    add!(vars, context);
    add!(vars, count);
    let fn-leaf = dylan-defn-leaf(body-builder, #"%make-rest-arg");
    build-let(body-builder, policy, source, rest-var,
	      make-operation(body-builder,
			     list(fn-leaf, context, count)));
  end;
  if (next-var)
    let ops = make(<stretchy-vector>);
    if (rest-var)
      add!(ops, dylan-defn-leaf(body-builder, #"apply"));
    end;
    add!(ops, dylan-defn-leaf(body-builder, #"%make-next-method"));
    for (var in fixed-vars) add!(ops, var) end;
    if (rest-var)
      add!(ops, rest-var);
    end;
    build-let(body-builder, policy, source, next-var,
	      make-operation(body-builder, as(<list>, ops)));
  end;
  if (keywords)
    for (info in keywords)
      add!(vars, info.keyinfo-var);
    end;
  end;
  let method-leaf = build-method-body(builder, policy, source,
				      as(<list>, vars), result-vars);
  build-region(builder, builder-result(body-builder));

  make(<hairy-method-leaf>,
       required-vars: as(<list>, fixed-vars),
       next-var: next-var,
       rest-var: rest-var,
       keywords: keywords,
       main-entry: method-leaf);
end;

define method build-hairy-method-more-arg-entry (leaf)
  let vars = make(<stretchy-vector>);
  let args = make(<stretchy-vector>);
  add!(args, leaf.hairy-method-main-entry);
  let body-builder = make-builder(builder);
  for (main-var in leaf.hairy-method-required-vars)
    let var = copy-variable(main-var);
    add!(vars, var);
    add!(args, var);
  end;
  let next-var = if (leaf.hairy-method-next-var)
		   let var = copy-variable(leaf.hairy-method-next-var);
		   add!(args, var);
		   var;
		 else
		   make-lexical-var(builder, #"next-method-info", source,
				    object-ctype());
		 end;
  add!(vars, next-var);
  if (leaf.hairy-method-rest-var | leaf.hairy-method-keywords)
    let context = make-lexical-var(body-builder, #"context", source,
				   object-ctype());
    let count = make-lexical-var(body-builder, #"count", source,
				 object-ctype());
    add!(vars, context);
    add!(vars, count);
    if (leaf.hairy-method-rest-var)
      add!(args, context);
      add!(args, count);
    end;
    if (leaf.hairy-method-keywords)
      build-keyword-dispatch(body-builder, policy, source, args,
			     leaf.hairy-method-keywords,
			     next-var, context, count);
    end;
  end;
  let cluster = make-values-cluster(builder, #"results", wild-ctype());
  let method-leaf = build-method-body(builder, policy, source,
				      as(<list>, vars), cluster);
  build-region(builder, builder-result(body-builder));
  build-assignment(builder, policy, source, cluster,
		   make-operation(builder, as(<list>, args)));
  end-body(builder);
  method-leaf;
end;


define method build-keyword-dispatch(builder, policy, source, args, keywords,
				     next-var, context-var, count-var)
  let vars = make(<stretchy-vector>);
  let ops = make(<stretchy-vector>);
  add!(ops, dylan-defn-leaf(builder, #"%extract-keywords"));
  add!(ops, context-var);
  add!(ops, count-var);
  add!(ops, next-var);
  for (info in keywords)
    let var = make-local-var(builder, info.keyinfo-symbol, object-ctype());
    add!(vars, var);
    add!(args, var);
    add!(ops, make-literal-constant(builder,
				    make(<ct-literal>, info.keyinfo-symbol)));
    add!(ops, make-literal-constant(builder, info.keyinfo-default));
  end;
  build-assignment(builder, policy, source, as(<list>, vars),
		   make-operation(builder, as(<list>, ops)));
end;

define method build-hairy-method-general-entry (leaf)
  let context-var = make-lexical-var(builder, #"context", source,
				     object-ctype());
  let count-var = make-lexical-var(builder, #"count", source, object-ctype());
  let args = make(<stretchy-vector>);
  add!(args, leaf.hairy-method-more-arg-entry);
  let body-builder = make-builder(builder);
  let fixed-vars = leaf.hairy-method-required-vars;
  let nfixed = fixed-vars.size;
  let more? = (leaf.hairy-method-rest-var | leaf.hairy-method-keywords) & #t;
  begin
    let ops = list(dylan-defn-leaf(body-builder, #"%check-arg-count"),
		   count-var,
		   make-literal-constant
		     (body-builder, make(<ct-literal>, value: nfixed)),
		   make-literal-constant
		     (body-builder, make(<ct-literal>, value: more?)));
    build-assignment(body-builder, policy, source, #(),
		     make-operation(body-builder, ops));
  end;
  for (var in fixed-vars, index from 0)
    let temp = make-lexical-var(builder, #"temp", source, object-ctype());
    add!(args, temp);
    let ops = list(dylan-defn-leaf(body-builder, #"%arg"),
		   context-var,
		   make-literal-constant
		     (body-builder, make(<ct-literal>, value: index)));
    build-let(body-builder, policy, source, temp,
	      make-operation(body-builder, ops));
  end;
  add!(args,make-literal-constant(body-builder,make(<ct-literal>, value: #f)));
  if (more?)
    let context = make-local-var(body-builder, #"context", object-ctype());
    let count = make-local-var(body-builder, #"count", object-ctype());
    let ops = list(dylan-defn-leaf(body-builder, #"%more-arg-context"),
		   context-var, count-var,
		   make-literal-constant
		     (body-builder, make(<ct-literal>, value: nfixed)));
    build-assignment(body-builder, policy, source, list(context, count),
		     make-operation(body-builder, ops));
    add!(args, context);
    add!(args, count);
  end;
  let cluster = make-values-cluster(builder, #"results", wild-ctype());
  let method-leaf = build-method-body(builder, policy, source,
				      list(context-var, count-var),
				      cluster);
  build-region(builder, builder-result(body-builder));
  build-assignment(builder, policy, source, cluster,
		   make-operation(builder, as(<list>, args)));
  end-body(builder);
end;

*/


// canonicalize-results
//
// Spread the values in results out into fixed-results and rest-result.
//
define generic canonicalize-results (builder :: <fer-builder>,
				     policy :: <policy>,
				     source :: <source-location>,
				     results :: <abstract-variable>,
				     fixed-results :: <list>,
				     rest-result :: union(<abstract-variable>, <false>))
    => ();


define method canonicalize-results (builder :: <fer-builder>,
				    policy :: <policy>,
				    source :: <source-location>,
				    results :: <abstract-variable>,
				    fixed-results :: <list>,
				    rest-result :: <false>,
				    #next next-method)
    => ();
  build-assignment(builder, policy, source, fixed-results, results);
end;

define method canonicalize-results (builder :: <fer-builder>,
				    policy :: <policy>,
				    source :: <source-location>,
				    results :: <abstract-variable>,
				    fixed-results :: <list>,
				    rest-result :: <abstract-variable>)
    => ();
  // We either want a rest var.  So we have to spread the values-cluster out
  // by mv-calling a method that looks like:
  //   method (x, y, z, #rest r)
  //     values(x, y, z, r);
  //   end
  let ops = make(<stretchy-vector>);
  add!(ops, dylan-defn-leaf(builder, #"values"));
  let fixed-vars
    = map(method (result)
	    let var = make-lexical-var(builder, #"temp", source,
				       object-ctype());
	    add!(ops, var);
	    var;
	  end,
	  fixed-results);
  let rest-var = make-lexical-var(builder, #"rest-temp", source,
				  object-ctype());
  add!(ops, rest-var);
  let cluster = make-values-cluster(builder, #"temps", wild-ctype());
  let method-leaf
    = build-hairy-method-body(builder, policy, source, fixed-vars, #f,
			      rest-var, #f, cluster);
  build-assignment(builder, policy, source, cluster,
		   make-operation(builder, as(<list>, ops)));
  end-body(builder);
  build-assignment(builder, policy, source,
		   concatenate(fixed-results, list(rest-result)),
		   make-mv-operation(builder, list(method-leaf, results)));
end;


// Random utilities.

define method dylan-defn-leaf (builder :: <fer-builder>, name :: <symbol>)
    => res :: <leaf>;
  make-definition-leaf(builder, dylan-defn(name))
    | error("%s undefined?", name);
end;

define method make-check-type-operation (builder :: <fer-builder>,
					 value-leaf :: <leaf>,
					 type-leaf :: <leaf>)
    => res :: <operation>;
  make-operation(builder,
		 list(dylan-defn-leaf(builder, #"check-type"),
		      value-leaf,
		      type-leaf));
end method;

define method make-error-operation (builder :: <fer-builder>,
				    msg :: <byte-string>)
    => res :: <operation>;
  make-operation(builder,
		 list(dylan-defn-leaf(builder, #"error"),
		      make-literal-constant(builder,
					    make(<ct-literal>, value: msg))));
end method;

