module: expand
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/Attic/expand.dylan,v 1.11 1996/01/27 00:17:40 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


define generic expand (form :: <constituent>,
		       lexenv :: false-or(<lexenv>))
    => results :: false-or(<simple-object-vector>);

// expand-macro -- gf method.
//
// By default, just return #f indicating the form doesn't expand.
//
define method expand (form :: <constituent>,
		      lexenv :: false-or(<lexenv>))
    => results :: false-or(<simple-object-vector>);
  #f;
end;



// Random utilities.

define method make-temp (name :: <symbol>) => res :: <name-token>;
  make(<name-token>, symbol: name, uniquifier: make(<uniquifier>));
end;

define method make-dylan-id (name :: <symbol>) => res :: <name-token>;
  make(<name-token>,
       symbol: name,
       module: $Dylan-Module,
       uniquifier: make(<uniquifier>));
end;
  

define method bind-temp (name :: <symbol>, expr :: <expression>)
    => (temp :: <name-token>, bind-form :: <let>);
  let temp = make-temp(name);
  let param = make(<parameter>, name: temp);
  let paramlist = make(<parameter-list>, required: vector(param));
  values(temp,
	 make(<let>,
	      bindings: make(<bindings>,
			     parameter-list: paramlist,
			     expression: expr)));
end;



// binop series expander.

define method expand (series :: <binop-series>,
		      lexenv :: false-or(<lexenv>))
    => res :: <simple-object-vector>;
  local
    method repeat (operator-stack :: <list>, operand-stack :: <list>,
		   op :: <binary-operator-token>,
		   operands :: <list>, operators :: <list>)
	=> res :: <expression>;
      if (empty?(operators))
	for (op in operator-stack,
	     operand in tail(operand-stack),
	     result = make-binary-fn-call(op,
					  head(operand-stack),
					  head(operands))
	       then make-binary-fn-call(op, operand, result))
	finally
	  result;
	end;
      else
	let next = head(operators);
	if (if (op.operator-left-associative?) \>= else \> end
	      (op.operator-precedence, next.operator-precedence))
	  let new = make-binary-fn-call(op,
					head(operand-stack),
					head(operands));
	  if (empty?(operator-stack))
	    repeat(operator-stack,
		   pair(new, operand-stack),
		   next,
		   tail(operands),
		   tail(operators));
	  else
	    repeat(tail(operator-stack),
		   tail(operand-stack),
		   head(operator-stack),
		   pair(new, tail(operands)),
		   operators);
	  end;
	else
	  repeat(pair(op, operator-stack),
		 pair(head(operands), operand-stack),
		 next,
		 tail(operands),
		 tail(operators));
	end;
      end;
    end;
  let operands = as(<list>, series.binop-series-operands);
  let operators = as(<list>, series.binop-series-operators);
  vector(repeat(#(),
		list(head(operands)),
		head(operators),
		tail(operands),
		tail(operators)));
end;

define method make-binary-fn-call(operator :: <binary-operator-token>,
				  left :: <expression>,
				  right :: <expression>)
    => res :: <expression>;
  make(<funcall>,
       function: make(<varref>, id: operator),
       arguments: vector(left, right));
end;



// assignment expander.

define method expand (form :: <assignment>, lexenv :: false-or(<lexenv>))
    => res :: false-or(<simple-object-vector>);
  expand-assignment(form.assignment-place, form.assignment-value);
end;

define generic expand-assignment (place :: <expression>, value :: <expression>)
    => res :: false-or(<simple-object-vector>);

define method make-setter (place :: <identifier-token>)
    => setter :: <name-token>;
  make(<name-token>,
       symbol: symcat(place.token-symbol, "-setter"),
       module: place.token-module,
       uniquifier: place.token-uniquifier);
end;

define method expand-assignment (place :: <varref>, value :: <expression>)
    => res :: false-or(<simple-object-vector>);
  #f;
end;

define method expand-assignment (place :: <funcall>, value :: <expression>)
    => res :: false-or(<simple-object-vector>);
  unless (instance?(place.funcall-function, <varref>))
    error("Bogus place for assignment: %=", place);
  end;
  let setter = make-setter(place.funcall-function.varref-id);
  let forms = make(<stretchy-vector>);
  let args = make(<stretchy-vector>);
  let (value-temp, value-bind-form) = bind-temp(#"value", value);
  add!(args, make(<varref>, id: value-temp));
  for (arg in place.funcall-arguments)
    let (temp, bind-form) = bind-temp(#"arg", arg);
    add!(args, make(<varref>, id: temp));
    add!(forms, bind-form);
  end;
  add!(forms, value-bind-form);
  add!(forms,
       make(<funcall>,
	    function: make(<varref>, id: setter),
	    arguments: as(<simple-object-vector>, args)));
  add!(forms, make(<varref>, id: value-temp));
  vector(make(<begin>, body: as(<simple-object-vector>, forms)));
end;

define method expand-assignment (place :: <dot>, value :: <expression>)
    => res :: false-or(<simple-object-vector>);
  let (value-temp, value-bind-form) = bind-temp(#"value", value);
  let (arg-temp, arg-bind-form) = bind-temp(#"arg", place.dot-operand);
  let function = make(<varref>, id: make-setter(place.dot-name));
  let args = vector(make(<varref>, id: value-temp),
		    make(<varref>, id: arg-temp));
  let funcall = make(<funcall>, function: function, arguments: args);
  vector(make(<begin>,
	      body: vector(arg-bind-form, value-bind-form, funcall,
			   make(<varref>, id: value-temp))));
end;

define method expand-assignment (place :: <expression>, value :: <expression>)
    => res :: false-or(<simple-object-vector>);
  error("Bogus place for assignment: %=", place);
end;


// for expander

// For loops expand into a body of code structured as follows:
//
// let temps;				<- outer-body
// local method repeat (step-vars)
//	   block (return)
//	     unless (implied-end-tests)
//	       let in-vars;		<- inner-body
//	       if (explicit-end-test)
//	         body;
//	         mv-call(return, repeat(step-forms));
//	       end;
//	     end;
//	     finally;
//	   end;
//       end;
// repeat(init-forms);

define method expand (form :: <for>, lexenv :: false-or(<lexenv>))
    => res :: false-or(<simple-object-vector>);
  if (lexenv)
    let outer-body = make(<stretchy-vector>);
    let inner-body = make(<stretchy-vector>);
    let step-vars = make(<stretchy-vector>);
    let init-forms = make(<stretchy-vector>);
    let step-forms = make(<stretchy-vector>);
    let implied-end-tests = make(<stretchy-vector>);
    let explicit-end-test = #f;
    for (clause in form.for-header)
      explicit-end-test
	:= process-for-clause(clause, outer-body, inner-body,
			      step-vars, init-forms, step-forms,
			      implied-end-tests, lexenv);
    end;
    let repeat = make-temp(#"repeat");
    let repeat-call = make(<funcall>,
			   function: make(<varref>, id: repeat),
			   arguments: as(<simple-object-vector>, step-forms));
    let return = make-temp(#"return");
    unless (empty?(form.for-finally))
      repeat-call
	:= make(<mv-call>,
		operands: vector(make(<varref>, id: return), repeat-call));
    end;
    if (explicit-end-test)
      add!(inner-body,
	   make(<if>,
		condition: explicit-end-test,
		consequent: add(form.for-body, repeat-call),
		alternate: #[]));
    else
      inner-body := concatenate(inner-body, form.for-body);
      add!(inner-body, repeat-call);
    end;
    for (index from implied-end-tests.size - 1 to 0 by -1)
      inner-body := vector(make(<if>,
				condition: implied-end-tests[index],
				consequent: #[],
				alternate: as(<simple-object-vector>,
					      inner-body)));
    end;
    let method-body = as(<simple-object-vector>, inner-body);
    unless (empty?(form.for-finally))
      method-body
	:= vector(make(<bind-exit>,
		       name: return,
		       body: concatenate(method-body, form.for-finally)));
    end;
    let param-list = make(<parameter-list>,
			  required: as(<simple-object-vector>, step-vars));
    let method-parse = make(<method-parse>,
			    name: repeat,
			    parameter-list: param-list,
			    body: method-body);
    add!(outer-body, make(<local>, methods: vector(method-parse)));
    add!(outer-body,
	 make(<funcall>,
	      function: make(<varref>, id: repeat),
	      arguments: as(<simple-object-vector>, init-forms)));
    vector(make(<begin>, body: as(<simple-object-vector>, outer-body)));
  end;
end;
  
define method process-for-clause (clause :: <for-while-clause>,
				  outer-body :: <stretchy-vector>,
				  inner-body :: <stretchy-vector>,
				  step-vars :: <stretchy-vector>,
				  init-forms :: <stretchy-vector>,
				  step-forms :: <stretchy-vector>,
				  implied-end-tests :: <stretchy-vector>,
				  lexenv :: false-or(<lexenv>))
  clause.for-clause-condition;
end;

define method process-for-clause (clause :: <for-in-clause>,
				  outer-body :: <stretchy-vector>,
				  inner-body :: <stretchy-vector>,
				  step-vars :: <stretchy-vector>,
				  init-forms :: <stretchy-vector>,
				  step-forms :: <stretchy-vector>,
				  implied-end-tests :: <stretchy-vector>,
				  lexenv :: false-or(<lexenv>))
  let var = bind-type(clause.for-clause-variable, outer-body, lexenv);
  let name = var.param-name.token-symbol;
  let (coll-temp, coll-bind)
    = bind-temp(symcat(name, "-coll"),
		clause.for-clause-collection);
  add!(outer-body, coll-bind);
  let state-temp = make-temp(symcat(name, "-state"));
  let limit-temp = make-temp(symcat(name, "-limit"));
  let next-temp = make-temp(symcat(name, "-next"));
  let done-temp = make-temp(symcat(name, "-done"));
  let curkey-temp = make-temp(symcat(name, "-curkey"));
  let curel-temp = make-temp(symcat(name, "-curel"));
  let params = make(<parameter-list>,
		    required: map(curry(make, <parameter>, name:),
				  vector(state-temp, limit-temp, next-temp,
					 done-temp, curkey-temp, curel-temp)));
  let fip = make-dylan-id(#"forward-iteration-protocol");
  let fip-call = make(<funcall>,
		      function: make(<varref>, id: fip),
		      arguments: vector(make(<varref>, id: coll-temp)));
  add!(outer-body,
       make(<let>,
	    bindings: make(<bindings>,
			   parameter-list: params,
			   expression: fip-call)));
  add!(step-vars, make(<parameter>, name: state-temp));
  add!(init-forms, make(<varref>, id: state-temp));
  add!(implied-end-tests,
       make(<funcall>,
	    function: make(<varref>, id: done-temp),
	    arguments: vector(make(<varref>, id: coll-temp),
			      make(<varref>, id: state-temp),
			      make(<varref>, id: limit-temp))));
  let curel = make(<funcall>,
		   function: make(<varref>, id: curel-temp),
		   arguments: vector(make(<varref>, id: coll-temp),
				     make(<varref>, id: state-temp)));
  add!(inner-body,
       make(<let>,
	    bindings: make(<bindings>,
			   parameter-list: make(<parameter-list>,
						required: vector(var)),
			   expression: curel)));
  add!(step-forms,
       make(<funcall>,
	    function: make(<varref>, id: next-temp),
	    arguments: vector(make(<varref>, id: coll-temp),
			      make(<varref>, id: state-temp))));
  #f;
end;

define method process-for-clause (clause :: <for-step-clause>,
				  outer-body :: <stretchy-vector>,
				  inner-body :: <stretchy-vector>,
				  step-vars :: <stretchy-vector>,
				  init-forms :: <stretchy-vector>,
				  step-forms :: <stretchy-vector>,
				  implied-end-tests :: <stretchy-vector>,
				  lexenv :: false-or(<lexenv>))
  add!(step-vars, bind-type(clause.for-clause-variable, outer-body, lexenv));
  let (temp, bind-form)
    = bind-temp(symcat(clause.for-clause-variable.param-name.token-symbol,
		       "-init"),
		clause.for-clause-init);
  add!(outer-body, bind-form);
  add!(init-forms, make(<varref>, id: temp));
  add!(step-forms, clause.for-clause-step);
  #f;
end;

define method process-for-clause (clause :: <for-from-clause>,
				  outer-body :: <stretchy-vector>,
				  inner-body :: <stretchy-vector>,
				  step-vars :: <stretchy-vector>,
				  init-forms :: <stretchy-vector>,
				  step-forms :: <stretchy-vector>,
				  implied-end-tests :: <stretchy-vector>,
				  lexenv :: false-or(<lexenv>))
  let var = bind-type(clause.for-clause-variable, outer-body, lexenv);
  add!(step-vars, var);
  let name = var.param-name.token-symbol;
  let (start-temp, start-bind)
    = bind-temp(symcat(name, "-start"), clause.for-clause-from);
  add!(outer-body, start-bind);
  add!(init-forms, make(<varref>, id: start-temp));
  let bound
    = if (clause.for-clause-bound)
	let (bound-temp, bound-bind)
	  = bind-temp(symcat(name, "-bound"), clause.for-clause-bound);
	add!(outer-body, bound-bind);
	make(<varref>, id: bound-temp);
      end;
  let by-expr
    = if (~clause.for-clause-by)
	make(<literal-ref>, literal: as(<ct-value>, 1));
      elseif (instance?(clause.for-clause-by, <literal-ref>))
	clause.for-clause-by;
      else
	let (by-temp, by-bind)
	  = bind-temp(symcat(name, "-by"), clause.for-clause-by);
	add!(outer-body, by-bind);
	make(<varref>, id: by-temp);
      end;
  add!(step-forms,
       make(<funcall>,
	    function: make(<varref>, id: make-dylan-id(#"+")),
	    arguments: vector(make(<varref>, id: var.param-name), by-expr)));
  if (bound)
    let fn
      = select (clause.for-clause-kind)
	  #"above" => make-dylan-id(#"<=");
	  #"below" => make-dylan-id(#">=");
	  #"to" =>
	    if (instance?(by-expr, <literal-ref>)
		  & instance?(by-expr.litref-literal, <literal-number>))
	      make-dylan-id(if (by-expr.litref-literal < 0)
			      #"<";
			    else
			      #">";
			    end);
	    else
	      let cmp
		= make(<funcall>,
		       function: make(<varref>, id: make-dylan-id(#"<")),
		       arguments: vector(by-expr,
					 make(<literal-ref>,
					      literal: as(<ct-value>, 0))));
	      let test
		= make(<if>,
		       condition: cmp,
		       consequent:
			 vector(make(<varref>, id: make-dylan-id(#"<"))),
		       alternate:
			 vector(make(<varref>, id: make-dylan-id(#">"))));
	      let (fn-temp, fn-bind) = bind-temp(symcat(name, "-test"), test);
	      add!(outer-body, fn-bind);
	      fn-temp;
	    end;
	end;
    add!(implied-end-tests,
	 make(<funcall>,
	      function: make(<varref>, id: fn),
	      arguments: vector(make(<varref>, id: var.param-name), bound)));
  end;
  #f;
end;

define method bind-type (var :: <parameter>, outer-body :: <stretchy-vector>,
			 lexenv :: <lexenv>)
    => new-var :: <parameter>;
  if (var.param-type & ~instance?(ct-eval(var.param-type, lexenv), <ctype>))
    let (temp, bind-form)
      = bind-temp(symcat(var.param-name.token-symbol, "-type"),
		  var.param-type);
    add!(outer-body, bind-form);
    make(<parameter>,
	 name: var.param-name,
	 type: make(<varref>, id: temp));
  else
    var;
  end;
end;
