module: compile-time-eval
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/convert/cteval.dylan,v 1.8 1995/06/01 14:29:29 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


// ct-eval and ct-mv-eval

// ct-eval -- exported.
//
// Return the <ct-value> for the result of evaluating expr in lexenv if it
// is a compile-time constant.  Otherwise, return #f.  We only use lexenv
// to tell when a constant module variable has been locally shadowed.
//
define generic ct-eval (expr :: <constituent>,
			lexenv :: union(<false>, <lexenv>))
    => res :: union(<ct-value>, <false>);

// ct-mv-eval -- exported.
//
// Like ct-eval, but return all the values.
// 
define generic ct-mv-eval (expr :: <constituent>,
			   lexenv :: union(<false>, <lexenv>));


// ct-eval(<constituent>,...) -- exported method.
//
// Just call ct-mv-eval and return the first result, if there is one.
// Otherwise return the compile-time literal for #f, because the expression
// constantly returns no values.
// 
define method ct-eval (expr :: <constituent>,
		       lexenv :: union(<false>, <lexenv>))
    => res :: union(<ct-value>, <false>);
  let (#rest results) = ct-mv-eval(expr, lexenv);
  if (empty?(results))
    make(<literal-false>);
  else
    results[0];
  end;
end;


// ct-mv-eval(<constituent>, <lexenv>) -- exported method.
//
// The default method just retries with the expansion if there is one, and
// bails if not.
// 
define method ct-mv-eval (expr :: <constituent>,
			  lexenv :: union(<false>, <lexenv>))
  let (expansion) = expand(expr, lexenv);
  if (expansion)
    ct-mv-eval-body(expansion, lexenv);
  else
    #f;
  end;
end;

// ct-mv-eval(<literal-ref>, <lexenv>) -- exported method.
//
// Just return a compile-time literal.
//
define method ct-mv-eval (expr :: <literal-ref>,
			  lexenv :: union(<false>, <lexenv>))
    => res :: <ct-value>;
  expr.litref-literal;
end;

// ct-mv-eval(<varref>, <lexenv>) -- exported method.
//
// As long as the variable isn't bound in the lexenv, call ct-value on the
// definition (assuming there is one).
//
define method ct-mv-eval (expr :: <varref>,
			  lexenv :: union(<false>, <lexenv>))
    => res :: union(<ct-value>, <false>);
  let id = expr.varref-id;
  unless (lexenv & find-binding(lexenv, id))
    let var = find-variable(id-name(id));
    var & var.variable-definition & ct-value(var.variable-definition);
  end;
end;

// ct-mv-eval(<funcall>, <lexenv>) -- exported method.
//
// If there is an expansion, eval it.  Otherwise, check it see if it is one
// of the functions we know something about.
// 
define method ct-mv-eval (expr :: <funcall>,
			  lexenv :: union(<false>, <lexenv>))
  let (expansion) = expand(expr, lexenv);
  if (expansion)
    ct-mv-eval-body(expansion, lexenv);
  else
    ct-mv-eval-funcall(expr.funcall-function, expr.funcall-arguments, lexenv);
  end;
end;

// ct-mv-eval(<dot>, <lexenv>) -- exported method.
//
// If there is an expansion, eval it.  Otherwise, check it see if it is one
// of the functions we know something about.
// 
define method ct-mv-eval (expr :: <dot>,
			  lexenv :: union(<false>, <lexenv>))
  let (expansion) = expand(expr, lexenv);
  if (expansion)
    ct-mv-eval-body(expansion, lexenv);
  else
    ct-mv-eval-funcall(expr.dot-name, vector(expr.dot-operand), lexenv);
  end;
end;

// ct-mv-eval(<begin>, <lexenv>) -- exported method.
//
// Just eval the body.
//
define method ct-mv-eval (expr :: <begin>,
			  lexenv :: union(<false>, <lexenv>))
  ct-mv-eval-body(expr.begin-body, lexenv);
end;


// ct-mv-eval-body -- internal.
//
// Eval the body, and return the results of the last form.  We make sure all
// the intervening forms return a compile-time constant value, because we
// that is the best guess we can make about them not having side effects.
//
define method ct-mv-eval-body (body :: <simple-object-vector>,
			       lexenv :: union(<false>, <lexenv>))
  if (empty?(body))
    make(<literal-false>);
  else
    block (return)
      let last = body.size - 1;
      for (i from 0 below last)
	unless (ct-eval(body[i], lexenv))
	  return(#f);
	end;
      end;
      ct-mv-eval(body[last], lexenv);
    end;
  end;
end;

// ct-mv-eval-function -- internal.
//
// If the function is one of the ones we know about, deal with it.  If not,
// then return #f.
//
define generic ct-mv-eval-funcall
    (function :: union(<expression>, <identifier-token>),
     args :: <simple-object-vector>,
     lexenv :: union(<false>, <lexenv>));

// ct-mv-eval-function -- internal method
//
// Give up because the function expression is something we can't deal with.
//
define method ct-mv-eval-funcall (function :: <expression>,
				  args :: <simple-object-vector>,
				  lexenv :: union(<false>, <lexenv>))
  #f;
end;

// ct-mv-eval-function -- internal method
//
// The function is a varref, so just try again directly on the name.
//
define method ct-mv-eval-funcall (function :: <varref>,
				  args :: <simple-object-vector>,
				  lexenv :: union(<false>, <lexenv>))
  ct-mv-eval-funcall(function.varref-id, args, lexenv);
end;

// The <variable> objects for the various functions we know how to deal with.
// 
define constant $singleton-var = dylan-var(#"singleton", create: #t);
define constant $union-var = dylan-var(#"union", create: #t);
define constant $type-or-var = dylan-var(#"type-or", create: #t);
define constant $false-or-var = dylan-var(#"false-or", create: #t);
define constant $one-of-var = dylan-var(#"one-of", create: #t);
define constant $limited-var = dylan-var(#"limited", create: #t);
define constant $make-var = dylan-var(#"make", create: #t);
define constant $negative-var = dylan-var(#"negative", create: #t);
define constant $plus-var = dylan-var(#"+", create: #t);
define constant $minus-var = dylan-var(#"-", create: #t);
define constant $times-var = dylan-var(#"*", create: #t);
define constant $ash-var = dylan-var(#"ash", create: #t);
define constant $expt-var = dylan-var(#"^", create: #t);

// ct-mv-eval-funcall -- internal
//
// If the name has been shadowed, give up.  Otherwise, look it up and see
// if it is any of the functions we magically know about.  If so, compute
// the result and return it.
// 
define method ct-mv-eval-funcall (function :: <identifier-token>,
				  args :: <simple-object-vector>,
				  lexenv :: union(<false>, <lexenv>))
  block (return)
    if (lexenv & find-binding(lexenv, function))
      return(#f);
    end;
    let args = map(method (arg) ct-eval(arg, lexenv) | return(#f) end, args);
    let var = find-variable(id-name(function));
    select (var)
      #f =>
	#f;
      $singleton-var =>
	if (args.size == 1)
	  let thing = args[0];
	  instance?(thing, <eql-ct-value>) & make-canonical-singleton(args[0]);
	end;
      $union-var =>
	if (args.size == 2
	      & instance?(args[0], <ctype>)
	      & instance?(args[1], <ctype>))
	  ctype-union(args[0], args[1]);
	else
	  #f;
	end;
      $false-or-var =>
	if (args.size == 1 & instance?(args[0], <ctype>))
	  ctype-union(args[0], specifier-type(#"<false>"));
	else
	  #f;
	end;
      $type-or-var =>
	if (every?(rcurry(instance?, <ctype>), args))
	  reduce(ctype-union, empty-ctype(), args);
	end;
      $one-of-var =>
	if (every?(rcurry(instance?, <eql-ct-value>), args))
	  reduce(ctype-union, empty-ctype(),
		 map(make-canonical-singleton, args));
	end;
      $limited-var =>
	if (~empty?(args) & instance?(args[0], <cclass>))
	  select (args[0] by csubtype?)
	    dylan-value(#"<integer>") =>
	      let (okay, min, max) = ct-keywords(args, 1, #"min", #"max");
	      if (okay)
		if ((min == #f | instance?(min, <literal-integer>))
		      & (max == #f | instance?(max, <literal-integer>)))
		  let min = min & min.literal-value;
		  let max = max & max.literal-value;
		  make(<limited-integer-ctype>, base-class: args[0],
		       low-bound: min, high-bound: max);
		end;
	      end;
	    otherwise =>
	      #f;
	  end;
	end;
      $make-var =>
	if (~empty?(args))
	  select (args[0])
	    dylan-value(#"<singleton>") =>
	      let (okay, object) = ct-keywords(args, 1, #"object");
	      if (okay & instance?(object, <eql-ct-value>))
		make-canonical-singleton(object);
	      end;
	    dylan-value(#"<byte-character-type>") =>
	      if (ct-keywords(args, 1))
		make(<byte-character-ctype>);
	      end;
	    otherwise =>
	      #f;
	  end;
	end;
/*
      $negative-var =>
	ct-eval-integer-func(negative, 1, args);
      $plus-var =>
	ct-eval-integer-func(\+, 2, args);
      $minus-var =>
	ct-eval-integer-func(\-, 2, args);
      $times-var =>
	ct-eval-integer-func(\*, 2, args);
      $ash-var =>
	ct-eval-integer-func(ash, 2, args);
      $expt-var =>
	ct-eval-integer-func(\^, 2, args);
*/
      otherwise =>
	#f;
    end;
  end;
end;


define method ct-keywords (args :: <simple-object-vector>, start :: <integer>,
			   #rest keys)
    => (okay :: <boolean>, #rest values);
  block (return)
    unless (even?(args.size - start))
      return(#f);
    end;
    for (index from start below args.size by 2)
      let ct-key = args[index];
      unless (instance?(ct-key, <literal-symbol>)
		& member?(ct-key.literal-value, keys))
	return(#f);
      end;
    end;
    apply(values,
	  #t,
	  map(method (key)
		block (found)
		  for (index from start below args.size by 2)
		    if (args[index] = key)
		      found(args[index + 1]);
		    end;
		  end;
		  #f;
		end;
	      end,
	      keys));
  end;
end;


/*

define method ct-eval-integer-func (function :: <function>, nargs :: <integer>,
				    args :: <simple-object-vector>)
  if (nargs == args.size & every?(rcurry(instance?, <literal-integer>), args))
    make(<ct-literal>, value: apply(function, map(literal-value, args)));
  end;
end;

*/
