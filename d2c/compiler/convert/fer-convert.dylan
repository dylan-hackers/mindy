module: fer-convert
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/convert/fer-convert.dylan,v 1.24 1995/05/05 14:48:17 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

define constant <var-or-vars>
  = union(<abstract-variable>, <list>);


// Result stuff.

define constant <result-designator>
  = one-of(#"nothing", #"assignment", #"let", #"expr", #"leaf");

define constant <result-datum>
  = type-or(<var-or-vars>, <symbol>, <false>);

define constant <result> = union(<false>, <fer-expression>);


define generic deliver-result (builder :: <fer-builder>, policy :: <policy>,
			       source :: <source-location>, 
			       want :: <result-designator>,
			       datum :: <result-datum>,
			       result :: <result>)
    => res :: <result>;

define method deliver-result (builder :: <fer-builder>, policy :: <policy>,
			      source :: <source-location>, 
			      want == #"nothing",
			      datum :: <result-datum>,
			      result :: <result>)
    => res :: <result>;
  #f;
end;

define method deliver-result (builder :: <fer-builder>, policy :: <policy>,
			      source :: <source-location>, 
			      want == #"nothing",
			      datum :: <result-datum>,
			      result :: <operation>)
    => res :: <result>;
  build-assignment(builder, policy, source, #(), result);
  #f;
end;

define method deliver-result (builder :: <fer-builder>, policy :: <policy>,
			      source :: <source-location>, 
			      want == #"assignment",
			      datum :: <result-datum>,
			      result :: <result>)
    => res :: <result>;
  build-assignment
    (builder, policy, source, datum,
     result | make-literal-constant(builder, make(<literal-false>)));

  #f;
end;

define method deliver-result (builder :: <fer-builder>, policy :: <policy>,
			      source :: <source-location>, 
			      want == #"let",
			      datum :: <result-datum>,
			      result :: <result>)
    => res :: <result>;
  build-let
    (builder, policy, source, datum,
     result | make-literal-constant(builder, make(<literal-false>)));

  #f;
end;

define method deliver-result (builder :: <fer-builder>, policy :: <policy>,
			      source :: <source-location>, 
			      want == #"expr",
			      datum :: <result-datum>,
			      result :: <result>)
    => res :: <result>;
  result | make-literal-constant(builder, make(<literal-false>));
end;

define method deliver-result (builder :: <fer-builder>, policy :: <policy>,
			      source :: <source-location>, 
			      want == #"leaf",
			      datum :: <result-datum>,
			      result :: <result>)
    => res :: <result>;
  result | make-literal-constant(builder, make(<literal-false>));
end;

define method deliver-result (builder :: <fer-builder>, policy :: <policy>,
			      source :: <source-location>, 
			      want == #"leaf",
			      datum :: <result-datum>,
			      result :: <operation>)
    => res :: <result>;
  let temp = make-local-var(builder, datum, object-ctype());
  build-assignment(builder, policy, source, temp, result);
  temp;
end;



// fer-convert

define constant source = make(<source-location>);

define generic fer-convert (builder :: <fer-builder>,
			    form :: <constituent>,
			    lexenv :: <lexenv>,
			    want :: <result-designator>,
			    datum :: <result-datum>)
    => res :: <result>;


define method fer-convert-body (builder :: <fer-builder>,
				body :: <simple-object-vector>,
				lexenv :: <lexenv>,
				want :: <result-designator>,
				datum :: <result-datum>)
    => res :: <result>;
  if (empty?(body))
    deliver-result(builder, lexenv.lexenv-policy, source, want, datum, #f);
  else
    for (i from 0 below body.size - 1)
      fer-convert(builder, body[i], lexenv, #"nothing", #f);
    end;
    fer-convert(builder, body[body.size - 1], lexenv, want, datum);
  end;
end;

define method fer-convert (builder :: <fer-builder>, form :: <constituent>,
			   lexenv :: <lexenv>, want :: <result-designator>,
			   datum :: <result-datum>)
    => res :: <result>;
  let expansion = expand(form, lexenv);
  if (expansion)
    fer-convert-body(builder, expansion, lexenv, want, datum);
  else
    error("Can't fer-convert %=", form);
  end;
end;

define method fer-convert (builder :: <fer-builder>, form :: <let>,
			   lexenv :: <lexenv>, want :: <result-designator>,
			   datum :: <result-datum>)
    => res :: <result>;
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
			#"assignment", type-local);
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
		make(<lexenv>, inside: lexenv), #"assignment", cluster);
    build-assignment
      (builder, lexenv.lexenv-policy, source,
       concatenate(temps, list(rest-temp)),
       make-operation(builder, <fer-primitive>, list(cluster),
		      name: #"canonicalize-results"));
  else
    fer-convert(builder, bindings.bindings-expression,
		make(<lexenv>, inside: lexenv), #"assignment", temps);
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
  deliver-result(builder, lexenv.lexenv-policy, source, want, datum,
		 make-literal-constant(builder, make(<literal-false>)));
end;

define method fer-convert (builder :: <fer-builder>, form :: <local>,
			   lexenv :: <lexenv>, want :: <result-designator>,
			   datum :: <result-datum>)
    => res :: <result>;
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
	      build-general-method(builder, meth, #f,
				   specializer-lexenv, lexenv));
  end;

  // Supply #f as the result.
  deliver-result(builder, lexenv.lexenv-policy, source, want, datum,
		 make-literal-constant(builder, make(<literal-false>)));
end;

define method fer-convert (builder :: <fer-builder>, form :: <literal-ref>,
			   lexenv :: <lexenv>, want :: <result-designator>,
			   datum :: <result-datum>)
    => res :: <result>;
  deliver-result(builder, lexenv.lexenv-policy, source, want, datum,
		 make-literal-constant(builder, form.litref-literal));
end;

define constant $arg-names
  = #[#"arg0", #"arg1", #"arg2", #"arg3", #"arg4", #"arg5", #"arg6", #"arg7",
	#"arg8", #"arg9"];

define method fer-convert (builder :: <fer-builder>, form :: <funcall>,
			   lexenv :: <lexenv>, want :: <result-designator>,
			   datum :: <result-datum>)
    => res :: <result>;
  let expansion = expand(form, lexenv);
  if (expansion)
    fer-convert-body(builder, expansion, lexenv, want, datum);
  else
    let ops = make(<list>, size: form.funcall-arguments.size + 1);
    ops.head := fer-convert(builder, form.funcall-function,
			    make(<lexenv>, inside: lexenv),
			    #"leaf", #"function");
    for (arg in form.funcall-arguments,
	 op-ptr = ops.tail then op-ptr.tail,
	 index from 0)
      let name = if (index < $arg-names.size)
		   $arg-names[index];
		 else
		   as(<symbol>, format-to-string("arg%d", index));
		 end;
      op-ptr.head := fer-convert(builder, arg, make(<lexenv>, inside: lexenv),
				 #"leaf", name);
    end;
    deliver-result(builder, lexenv.lexenv-policy, source, want, datum,
		   make-unknown-call(builder, ops));
  end;
end;

define method fer-convert (builder :: <fer-builder>, form :: <dot>,
			   lexenv :: <lexenv>, want :: <result-designator>,
			   datum :: <result-datum>)
    => res :: <result>;
  let expansion = expand(form, lexenv);
  if (expansion)
    fer-convert-body(builder, expansion, lexenv, want, datum);
  else
    let arg-leaf = fer-convert(builder, form.dot-operand,
			       make(<lexenv>, inside: lexenv),
			       #"leaf", #"argument");
    let fun-leaf = fer-convert(builder, make(<varref>, id: form.dot-name),
			       make(<lexenv>, inside: lexenv),
			       #"leaf", #"function");
    deliver-result(builder, lexenv.lexenv-policy, source, want, datum,
		   make-unknown-call(builder, list(fun-leaf, arg-leaf)));
  end;
end;

define method fer-convert (builder :: <fer-builder>, form :: <varref>,
			   lexenv :: <lexenv>, want :: <result-designator>,
			   datum :: <result-datum>)
    => res :: <result>;
  let id = form.varref-id;
  let binding = find-binding(lexenv, id);
  deliver-result(builder, lexenv.lexenv-policy, source, want, datum,
		 if (binding)
		   binding.binding-var;
		 else
		   let var = find-variable(id-name(id));
		   let defn = var & var.variable-definition;
		   if (defn)
		     make-definition-leaf(builder, defn);
		   else
		     make-error-operation(builder, "Undefined variable");
		   end;
		 end);
end;

define method fer-convert (builder :: <fer-builder>, form :: <assignment>,
			   lexenv :: <lexenv>, want :: <result-designator>,
			   datum :: <result-datum>)
    => res :: <result>;
  let expansion = expand(form, lexenv);
  if (expansion)
    fer-convert-body(builder, expansion, lexenv, want, datum);
  else
    let place = form.assignment-place;
    unless (instance?(place, <varref>))
      error("Assignment to complex place didn't get expanded away?");
    end;
    let id = place.varref-id;
    let binding = find-binding(lexenv, id);
    block (return)
      let (leaf, type-leaf, defn)
	= if (binding)
	    values(binding.binding-var, binding.binding-type-var, #f);
	  else
	    let var = find-variable(id-name(id));
	    let defn = var & var.variable-definition;
	    if (~defn)
	      return(deliver-result
		       (builder, lexenv.lexenv-policy, source, want, datum,
			make-error-operation(builder, "Undefined variable")));
	    elseif (instance?(defn, <variable-definition>))
	      let type-defn = defn.var-defn-type-defn;
	      values(make-definition-leaf(builder, defn),
		     type-defn & make-definition-leaf(builder, type-defn),
		     defn);
	    else
	      return(deliver-result
		       (builder, lexenv.lexenv-policy, source, want, datum,
			make-error-operation
			  (builder,
			   "Can't assign constant module varaibles")));
	    end;
	  end;
      let temp = fer-convert(builder, form.assignment-value,
			     make(<lexenv>, inside: lexenv),
			     #"leaf", #"temp");
      if (type-leaf)
	let checked = make-local-var(builder, #"checked", object-ctype());
	build-assignment(builder, lexenv.lexenv-policy, source, checked,
			 make-check-type-operation(builder, temp, type-leaf));
	temp := checked;
      end;
      if (binding)
	build-assignment(builder, lexenv.lexenv-policy, source, leaf, temp);
      else
	build-assignment
	  (builder, lexenv.lexenv-policy, source, #(),
	   make-operation(builder, <set>, list(temp), var: defn));
      end;
      deliver-result(builder, lexenv.lexenv-policy, source, want, datum, temp);
    end;
  end;
end;

define method fer-convert (builder :: <fer-builder>, form :: <begin>,
			   lexenv :: <lexenv>, want :: <result-designator>,
			   datum :: <result-datum>)
    => res :: <result>;
  fer-convert-body(builder, form.begin-body, make(<lexenv>, inside: lexenv),
		   want, datum);
end;

define method fer-convert (builder :: <fer-builder>, form :: <bind-exit>,
			   lexenv :: <lexenv>, want :: <result-designator>,
			   datum :: <result-datum>)
    => res :: <result>;
  let blk = build-block-body(builder, lexenv.lexenv-policy, source);
  let lexenv = make(<lexenv>, inside: lexenv);
  let name = form.exit-name;
  let exit = make-lexical-var(builder, name.token-symbol, source,
			      function-ctype());
  add-binding(lexenv, name, exit);
  build-let(builder, lexenv.lexenv-policy, source, exit,
	    make-exit-function(builder, blk));
  let cluster = make-values-cluster(builder, #"results", wild-ctype());
  fer-convert-body(builder, form.exit-body, lexenv, #"assignment", cluster);
  build-assignment(builder, lexenv.lexenv-policy, source, #(),
		   make-operation(builder, <mv-call>, list(exit, cluster)));
  end-body(builder);
  deliver-result(builder, lexenv.lexenv-policy, source, want, datum,
		 blk.catcher);
end;

define method fer-convert (builder :: <fer-builder>, form :: <if>,
			   lexenv :: <lexenv>,
			   want :: one-of(#"leaf", #"expr"),
			   datum :: <result-datum>)
    => res :: <result>;
  let leaf = make-local-var(builder, datum, object-ctype());
  fer-convert(builder, form, lexenv, #"assignment", leaf);
  leaf;
end;

define method fer-convert (builder :: <fer-builder>, form :: <if>,
			   lexenv :: <lexenv>,
			   want :: one-of(#"nothing", #"assignment"),
			   datum :: <result-datum>)
    => res :: <result>;
  build-if-body(builder, lexenv.lexenv-policy, source,
		fer-convert(builder, form.if-condition,
			    make(<lexenv>, inside: lexenv),
			    #"leaf", #"condition"));
  fer-convert-body(builder, form.if-consequent, make(<lexenv>, inside: lexenv),
		   want, datum);
  build-else(builder, lexenv.lexenv-policy, source);
  fer-convert-body(builder, form.if-alternate, make(<lexenv>, inside: lexenv),
		   want, datum);
  end-body(builder);
  #f;
end;

define method fer-convert (builder :: <fer-builder>, form :: <method-ref>,
			   lexenv :: <lexenv>, want :: <result-designator>,
			   datum :: <result-datum>)
    => res :: <result>;
  deliver-result(builder, lexenv.lexenv-policy, source, want, datum,
		 build-general-method(builder, form.method-ref-method, #f,
				      lexenv, lexenv));
end;

define method fer-convert (builder :: <fer-builder>, form :: <mv-call>,
			   lexenv :: <lexenv>, want :: <result-designator>,
			   datum :: <result-datum>)
    => res :: <result>;
  let operands = form.mv-call-operands;
  unless (operands.size == 2)
    error("%%mv-call with other than two operands?");
  end;
  let function
    = fer-convert(builder, operands[0], make(<lexenv>, inside: lexenv),
		  #"leaf", #"function");
  let cluster = make-values-cluster(builder, #"results", wild-ctype());
  fer-convert(builder, operands[1], make(<lexenv>, inside: lexenv),
	      #"assignment", cluster);
  deliver-result(builder, lexenv.lexenv-policy, source, want, datum,
		 make-operation(builder, <mv-call>, list(function, cluster)));
end;

define method fer-convert (builder :: <fer-builder>, form :: <primitive>,
			   lexenv :: <lexenv>, want :: <result-designator>,
			   datum :: <result-datum>)
    => res :: <result>;
  let operands = form.primitive-operands;
  let ops = make(<list>, size: operands.size);
  for (arg in operands, op-ptr = ops then op-ptr.tail, index from 0)
    let name = if (index < $arg-names.size)
		 $arg-names[index];
	       else
		 as(<symbol>, format-to-string("arg%d", index));
	       end;
    op-ptr.head := fer-convert(builder, arg, make(<lexenv>, inside: lexenv),
			       #"leaf", name);
  end;
  deliver-result
    (builder, lexenv.lexenv-policy, source, want, datum,
     make-operation(builder, <fer-primitive>, ops,
		    name: form.primitive-name.token-symbol));
end;

define method fer-convert (builder :: <fer-builder>, form :: <uwp>,
			   lexenv :: <lexenv>, want :: <result-designator>,
			   datum :: <result-datum>)
    => res :: <result>;
  let res = fer-convert-body(builder, form.uwp-body,
			     make(<lexenv>, inside: lexenv),
			     want, datum);
  fer-convert-body(builder, form.uwp-cleanup,
		   make(<lexenv>, inside: lexenv),
		   #"nothing", #f);
  res;
end;


// Method conversion.

define method build-general-method
    (builder :: <fer-builder>, meth :: <method-parse>,
     name :: false-or(<string>),
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
		      #"assignment", temp);
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
  let specializer-leaves = make(<stretchy-vector>);
  let non-const-arg-types? = #f;
  let arg-check-builder = make-builder(builder);
  for (param in paramlist.paramlist-required-vars)
    let (type, type-var) = param-type-and-var(param);
    add!(specializers, type);
    let name = param.param-name;
    let var = make-lexical-var(builder, name.token-symbol, source, type);
    add-binding(lexenv, name, var, type-var: type-var);
    if (type-var)
      non-const-arg-types? := #t;
      add!(specializer-leaves, type-var);
      let temp = make-lexical-var(builder, name.token-symbol, source, type);
      add!(fixed-vars, temp);
      let op = make-check-type-operation(arg-check-builder, temp, type-var);
      build-assignment(arg-check-builder, specializer-lexenv.lexenv-policy,
		       source, var, op);
    else
      add!(specializer-leaves, make-literal-constant(builder, type));
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
  let (keyword-infos, keyword-vars)
    = if (paramlist.paramlist-keys)
	let infos = make(<stretchy-vector>);
	let vars = make(<stretchy-vector>);
	for (param in paramlist.paramlist-keys)
	  let name = param.param-name;
	  let (type, type-var) = param-type-and-var(param);
	  let var = make-lexical-var(builder, name.token-symbol, source, type);
	  let default = if (param.param-default)
			  ct-eval(param.param-default, lexenv);
			else
			  make(<literal-false>);
			end;
	  if (default)
	    add!(infos, make(<key-info>, key-name: param.param-keyword,
			     default: default, type: type));
	    add!(vars, var);
	  else
	    let pre-default
	      = make-lexical-var(builder, name.token-symbol, source,
				 object-ctype());
	    add!(infos, make(<key-info>, key-name: param.param-keyword,
			     type: object-ctype(),
			     default: #f));
	    add!(vars, pre-default);
	    let temp = make-local-var(builder, #"temp", object-ctype());
	    build-assignment
	      (builder, lexenv.lexenv-policy, source, temp,
	       make-unknown-call
		 (builder,
		  list(dylan-defn-leaf(builder, #"unbound?"),
		       pre-default)));
	    build-if-body(builder, lexenv.lexenv-policy, source, temp);
	    fer-convert(builder, param.param-default,
			make(<lexenv>, inside: lexenv),
			#"assignment", var);
	    build-else(builder, lexenv.lexenv-policy, source);
	    build-assignment(builder, lexenv.lexenv-policy, source, var,
			     pre-default);
	    end-body(builder);
	  end;
	  if (type-var)
	    let checked = make-lexical-var(builder, name.token-symbol, source,
					   object-ctype());
	    build-assignment
	      (builder, lexenv.lexenv-policy, source, checked,
	       make-check-type-operation(builder, var, type-var));
	    add-binding(lexenv, name, checked, type-var: type-var);
	  else
	    add-binding(lexenv, name, var);
	  end;
	end;
	values(as(<list>, infos),
	       as(<list>, vars));
      end;
  
  let returns = meth.method-returns;
  let fixed-results = make(<stretchy-vector>);
  let non-const-result-types? = #f;
  let result-types = make(<stretchy-vector>);
  let result-type-leaves = make(<stretchy-vector>);
  let result-check-builder = make-builder(builder);
  for (param in returns.paramlist-required-vars)
    let (type, type-var) = param-type-and-var(param);
    add!(result-types, type);
    let var = make-local-var(builder, param.param-name.token-symbol, type);
    if (type-var)
      non-const-result-types? := #t;
      add!(result-type-leaves, type-var);
      let temp = make-local-var(builder, param.param-name.token-symbol, type);
      let op = make-check-type-operation(result-check-builder, var, type-var);
      build-assignment(result-check-builder, specializer-lexenv.lexenv-policy,
		       source, temp, op);
      add!(fixed-results, temp);
    else
      add!(result-type-leaves, make-literal-constant(builder, type));
      add!(fixed-results, var);
    end;
  end;

  let rest-result
    = if (returns.paramlist-rest)
	make-local-var(builder, returns.paramlist-rest.token-symbol,
		       object-ctype());
      end;

  let signature
    = make(<signature>,
	   specializers: as(<list>, specializers),
	   next: next-var & #t,
	   rest-type: rest-var & object-ctype(),
	   keys: keyword-infos & as(<list>, keyword-infos),
	   all-keys?: paramlist.paramlist-all-keys?,
	   returns: make-values-ctype(as(<list>, result-types),
	   			      rest-result & object-ctype()));

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
  let (method-literal, method-region)
    = build-hairy-method-body(builder, lexenv.lexenv-policy, source,
			      if (name)
				name;
			      elseif (meth.method-name)
				as(<string>, meth.method-name.token-symbol);
			      else
				"Anonymous Method";
			      end,
			      signature, fixed-vars, next-var, rest-var,
			      keyword-vars);
  if (non-const-arg-types?)
    build-region(builder, builder-result(arg-check-builder));
  end;
  fer-convert-body(builder, meth.method-body, lexenv,
		   #"assignment", results-temp | results);

  if (results-temp)
    build-assignment
      (builder, lexenv.lexenv-policy, source,
       concatenate(as(<list>, fixed-results), list(rest-result)),
       make-operation
	 (builder, <fer-primitive>, list(results-temp),
	  name: #"canonicalize-results"));
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
		     make-unknown-call(builder, as(<list>, args)));
  end;
  build-return(builder, lexenv.lexenv-policy, source, method-region, results);
  end-body(builder);

  if (non-const-arg-types? | non-const-result-types?)
    local
      method build-call (name, args)
	let ops = pair(dylan-defn-leaf(builder, name), as(<list>, args));
	let temp = make-local-var(builder, name, object-ctype());
	build-assignment(builder, lexenv.lexenv-policy, source, temp,
			 make-unknown-call(builder, ops));
	temp;
      end;
    build-call(#"%make-method",
	       list(build-call(#"list", specializer-leaves),
		    build-call(#"list", result-type-leaves),
		    make-literal-constant
		      (builder,
		       if (rest-result)
			 object-ctype();
		       else
			 make(<literal-false>);
		       end),
		    method-literal));
  else
    method-literal;
  end;
end;



// build-hairy-method-body.

define generic build-hairy-method-body
    (builder :: <fer-builder>,
     policy :: <policy>,
     source :: <source-location>,
     name :: <byte-string>,
     signature :: <signature>,
     fixed-vars :: <sequence>,
     next-var :: union(<abstract-variable>, <false>),
     rest-var :: union(<abstract-variable>, <false>),
     keyword-vars :: union(<sequence>, <false>))
    => (literal :: <method-literal>, region :: <method-region>);

define method build-hairy-method-body
    (builder :: <fer-builder>,
     policy :: <policy>,
     source :: <source-location>,
     name :: <byte-string>,
     signature :: <signature>,
     fixed-vars :: <sequence>,
     next-var :: <false>,
     rest-var :: <false>,
     keyword-vars :: <false>)
    => (literal :: <method-literal>, region :: <method-region>);
  let lambda = build-method-body(builder, policy, source, name,
				 as(<list>, fixed-vars));
  values(lambda, lambda);
end;

define method build-hairy-method-body
    (builder :: <fer-builder>,
     policy :: <policy>,
     source :: <source-location>,
     name :: <byte-string>,
     signature :: <signature>,
     fixed-vars :: <sequence>,
     next-var :: union(<abstract-variable>, <false>),
     rest-var :: union(<abstract-variable>, <false>),
     keyword-vars :: union(<sequence>, <false>))
    => (literal :: <method-literal>, region :: <method-region>);
  let vars = map-as(<stretchy-vector>, identity, fixed-vars);
  let body-builder = make-builder(builder);
  if (next-var)
    let var = make-lexical-var(body-builder, #"next-method-info", source,
			       object-ctype());
    add!(vars, var);
    if (keyword-vars & ~rest-var)
      rest-var := make-lexical-var(body-builder, #"rest", source,
				   object-ctype());
      signature
	:= make(<signature>,
		specializers: signature.specializers,
		next: #t,
		rest-type: object-ctype(),
		keys: signature.key-infos,
		all-keys: signature.all-keys?,
		returns: signature.returns);
    end;
  end;
  if (rest-var)
    add!(vars, rest-var);
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
	      make-unknown-call(body-builder, as(<list>, ops)));
  end;
  if (keyword-vars)
    for (var in keyword-vars)
      add!(vars, var);
    end;
  end;
  let method-leaf = build-method-body(builder, policy, source, name,
				      as(<list>, vars));
  build-region(builder, builder-result(body-builder));

  values(make-hairy-method-literal(builder, policy, source,
				   signature, method-leaf),
	 method-leaf);
end;

/*

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
		   make-unknown-call(builder, as(<list>, args)));
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
    add!(ops,
	 make-literal-constant
	   (builder, make(<literal-symbol>, value: info.keyinfo-symbol)));
    add!(ops, make-literal-constant(builder, info.keyinfo-default));
  end;
  build-assignment(builder, policy, source, as(<list>, vars),
		   make-unknown-call(builder, as(<list>, ops)));
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
		     (body-builder, make(<literal-fixed-integer>, value: nfixed)),
		   make-literal-constant
		     (body-builder,
		      if (more?)
			make(<literal-true>);
		      else
			make(<literal-false>);
		      end));
    build-assignment(body-builder, policy, source, #(),
		     make-unknown-call(body-builder, ops));
  end;
  for (var in fixed-vars, index from 0)
    let temp = make-lexical-var(builder, #"temp", source, object-ctype());
    add!(args, temp);
    let ops = list(dylan-defn-leaf(body-builder, #"%arg"),
		   context-var,
		   make-literal-constant
		     (body-builder, make(<literal-fixed-integer>, value: index)));
    build-let(body-builder, policy, source, temp,
	      make-unknown-call(body-builder, ops));
  end;
  add!(args, make-literal-constant(body-builder, make(<literal-false>)));
  if (more?)
    let context = make-local-var(body-builder, #"context", object-ctype());
    let count = make-local-var(body-builder, #"count", object-ctype());
    let ops = list(dylan-defn-leaf(body-builder, #"%more-arg-context"),
		   context-var, count-var,
		   make-literal-constant
		     (body-builder, make(<literal-fixed-integer>, value: nfixed)));
    build-assignment(body-builder, policy, source, list(context, count),
		     make-unknown-call(body-builder, ops));
    add!(args, context);
    add!(args, count);
  end;
  let cluster = make-values-cluster(builder, #"results", wild-ctype());
  let method-leaf = build-method-body(builder, policy, source,
				      list(context-var, count-var),
				      cluster);
  build-region(builder, builder-result(body-builder));
  build-assignment(builder, policy, source, cluster,
		   make-unknown-call(builder, as(<list>, args)));
  end-body(builder);
end;

*/


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
  make-unknown-call(builder,
		    list(dylan-defn-leaf(builder, #"check-type"),
			 value-leaf,
			 type-leaf));
end method;

define method make-error-operation
    (builder :: <fer-builder>, msg :: <byte-string>, #rest args)
    => res :: <operation>;
  let error = dylan-defn-leaf(builder, #"error");
  let msg = make-literal-constant(builder, as(<ct-value>, msg));
  make-unknown-call(builder, concatenate(list(error, msg), args));
end method;

