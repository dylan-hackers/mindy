module: compile-time-eval
copyright: see below


//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000, 2001, 2002  Gwydion Dylan Maintainers
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

// ct-eval and ct-mv-eval

// ct-eval -- exported.
//
// Return the <ct-value> for the result of evaluating expr in lexenv if it
// is a compile-time constant.  Otherwise, return #f.  We only use lexenv
// to tell when a constant module variable has been locally shadowed.
//
define method ct-eval
    (expr :: <constituent-parse>, lexenv :: false-or(<lexenv>))
    => res :: false-or(<ct-value>);
  let (#rest results) = ct-mv-eval(expr, lexenv);
  if (empty?(results))
    make(<literal-false>);
  else
    results[0];
  end;
end;


// ct-mv-eval -- exported.
//
// Like ct-eval, but return all the values.
// 
define generic ct-mv-eval
    (expr :: <constituent-parse>, lexenv :: false-or(<lexenv>))
    => (#rest results :: false-or(<ct-value>));


// ct-mv-eval(<constituent-parse>, <lexenv>) -- exported method
//
// Default method that just gives up.
// 
define method ct-mv-eval
    (expr :: <constituent-parse>, lexenv :: false-or(<lexenv>))
    => (#rest results :: false-or(<ct-value>));
  #f;
end method ct-mv-eval;

// ct-mv-eval(<macro-call-parse>, <lexenv>) -- exported method.
//
// Just ct-mv-eval the macro expansion of the macro call.
// 
define method ct-mv-eval
    (expr :: <macro-call-parse>, lexenv :: false-or(<lexenv>))
    => (#rest results :: false-or(<ct-value>));
  ct-mv-eval(macro-expand(expr), lexenv);
end;

// ct-mv-eval(<literal-ref-parse>, <lexenv>) -- exported method.
//
// Just return a compile-time literal.
//
define method ct-mv-eval
    (expr :: <literal-ref-parse>, lexenv :: false-or(<lexenv>))
    => res :: <ct-value>;
  expr.litref-literal;
end;

// ct-mv-eval(<varref-parse>, <lexenv>) -- exported method.
//
// As long as the variable isn't bound in the lexenv, call ct-value on the
// definition (assuming there is one).  This is inhibited if the definition is
// dynamic.
//
define method ct-mv-eval
    (expr :: <varref-parse>, lexenv :: false-or(<lexenv>))
    => res :: false-or(<ct-value>);
  let id = expr.varref-id;
  unless (lexenv & local-binding?(lexenv, id))
    let var = find-variable(id-name(id));
    if (var)
      let def = var.variable-definition;
      def & ~def.defn-dynamic? & ct-value(def);
    end if;
  end;
end;

// ct-mv-eval(<funcall-parse>, <lexenv>) -- exported method.
//
// Check it see if it is one of the functions we know something about.
// 
define method ct-mv-eval
    (expr :: <funcall-parse>, lexenv :: false-or(<lexenv>))
    => (#rest results :: false-or(<ct-value>));
  ct-mv-eval-funcall(expr.funcall-function, expr.funcall-arguments, lexenv);
end;

// ct-mv-eval(<dot-parse>, <lexenv>) -- exported method.
//
// Check it see if it is one of the functions we know something about.
// 
define method ct-mv-eval
    (expr :: <dot-parse>, lexenv :: false-or(<lexenv>))
    => (#rest results :: false-or(<ct-value>));
  ct-mv-eval-funcall(expr.dot-name, vector(expr.dot-operand), lexenv);
end;

// ct-mv-eval(<body-parse>, <lexenv>) -- exported method.
//
// Just eval the body.
//
define method ct-mv-eval
    (expr :: <body-parse>, lexenv :: false-or(<lexenv>))
    => (#rest results :: false-or(<ct-value>));
  ct-mv-eval-body(expr.body-parts, lexenv);
end;

// ct-mv-eval-body -- internal.
//
// Eval the body, and return the results of the last form.  We make sure all
// the intervening forms return a compile-time constant value, because
// that is the best guess we can make about them not having side effects.
//
define method ct-mv-eval-body
    (body :: <simple-object-vector>, lexenv :: false-or(<lexenv>))
    => (#rest results :: false-or(<ct-value>));
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
    (function :: type-union(<expression-parse>, <identifier-token>),
     args :: <simple-object-vector>,
     lexenv :: false-or(<lexenv>))
    => (#rest results :: false-or(<ct-value>));

// ct-mv-eval-function -- internal method
//
// Give up because the function expression is something we can't deal with.
//
define method ct-mv-eval-funcall
    (function :: <expression-parse>, args :: <simple-object-vector>,
     lexenv :: false-or(<lexenv>))
    => (#rest results :: false-or(<ct-value>));
  #f;
end;

// ct-mv-eval-function -- internal method
//
// The function is a varref, so just try again directly on the name.
//
define method ct-mv-eval-funcall
    (function :: <varref-parse>, args :: <simple-object-vector>,
     lexenv :: false-or(<lexenv>))
    => (#rest results :: false-or(<ct-value>));
  ct-mv-eval-funcall(function.varref-id, args, lexenv);
end;

// ct-mv-eval-funcall -- internal
//
// If the name has been shadowed, give up.  Otherwise, look it up and see
// if it is any of the functions we magically know about.  If so, compute
// the result and return it.
// 
define method ct-mv-eval-funcall
    (function :: <identifier-token>, args :: <simple-object-vector>,
     lexenv :: false-or(<lexenv>))
    => (#rest results :: false-or(<ct-value>));
  unless (lexenv & local-binding?(lexenv, function))
    let var = find-variable(id-name(function));
    if (var)
      let evaluator = var.variable-ct-evaluator;
      if (evaluator)
	block (return)
	  let args = map(method (arg :: <expression-parse>)
			     => res :: <ct-value>;
			   ct-eval(arg, lexenv) | return(#f)
			 end,
			 args);
	  apply(evaluator, args);
	end block;
      end if;
    end if;
  end unless;
end method ct-mv-eval-funcall;


// compile-time evaluation methods for various functions.


define method define-ct-evaluator
    (name :: <symbol>, specializers :: <list>, body :: <function>) => ();
  let var = dylan-var(name, create: #t);
  var.variable-ct-evaluator
    := method (#rest args)
	 let specializers
	   = map(method (specifier :: type-union(<type-specifier>,
						 singleton(#"rest")))
		     => res :: type-union(<ctype>, singleton(#"rest"));
		   if (specifier == #"rest")
		     #"rest";
		   else
		     specifier-type(specifier);
		   end;
		 end,
		 specializers);
	 let new-evaluator
	   = method (#rest args)
	       block (return)
		 let remaining = specializers;
		 for (arg :: <ct-value> in args)
		   if (remaining == #())
		     return(#f);
		   else
		     let specializer = remaining.head;
		     if (specializer == #"rest")
		       unless (cinstance?(arg, remaining.tail.head))
			 return(#f);
		       end;
		     else
		       unless (cinstance?(arg, specializer))
			 return(#f);
		       end;
		       remaining := remaining.tail;
		     end if;
		   end if;
		 end for;
		 if (remaining == #() | remaining.head == #"rest")
		   apply(body, args);
		 else
		   #f;
		 end;
	       end block;
	     end method;
	 var.variable-ct-evaluator := new-evaluator;
	 apply(new-evaluator, args);
       end;
end;

define method ct-keywords (args :: <simple-object-vector>, #rest keys)
    => (okay :: <boolean>, #rest values);
  block (return)
    unless (even?(args.size))
      return(#f);
    end;
    for (index from 0 below args.size by 2)
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
		  for (index from 0 below args.size by 2)
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

define method ct-eval-rational-function (function :: <function>, #rest args)
    => res :: false-or(<literal-rational>);
  let any-ratios? = #f;
  let any-bignums? = #f;
  for (arg in args)
    select (arg by instance?)
      <literal-ratio> => any-ratios? := #t;
      <literal-extended-integer> => any-bignums? := #t;
      <literal-integer> => #f;
      otherwise =>
	error("ct-eval-ration-function call on non-rational?");
    end select;
  end for;
  make(if (any-ratios?)
	 <literal-ratio>;
       elseif (any-bignums?)
	 <literal-extended-integer>;
       else
	 <literal-integer>;
       end,
       value: apply(function, map(literal-value, args)));
end;



define-ct-evaluator(#"singleton", #(#"<object>"),
		    method (object :: <ct-value>)
			=> res :: false-or(<ct-value>);
		      if (instance?(object, <eql-ct-value>))
			make(<singleton-ctype>, value: object);
		      else
			#f;
		      end;
		    end);

define-ct-evaluator(#"subclass", #(#"<class>"),
		    method (class :: <cclass>) => res :: <subclass-ctype>;
		      make(<subclass-ctype>, of: class);
		    end method);

define-ct-evaluator(#"direct-instance", #(#"<class>"),
		    method (class :: <cclass>)
		     => res :: false-or(<direct-instance-ctype>);
		      unless (class.abstract?)
			make(<direct-instance-ctype>, base-class: class);
		      end unless;
		    end method);

define-ct-evaluator(#"type-union", #(rest:, #"<type>"),
		    method (#rest types) => res :: <ctype>;
		      reduce(ctype-union, empty-ctype(), types);
		    end);
		      
define-ct-evaluator(#"false-or", #(#"<type>"),
		    method (type :: <ctype>) => res :: <ctype>;
		      ctype-union(type, specifier-type(#"<false>"));
		    end);

define-ct-evaluator(#"one-of", #(rest:, #"<object>"),
		    method (#rest objects) => res :: false-or(<ctype>);
		      block (return)
			for (result = empty-ctype()
			       then ctype-union(result,
						make(<singleton-ctype>,
						     value: value)),
			     value in objects)
			  unless (instance?(value, <eql-ct-value>))
			    return(#f);
			  end unless;
			finally
			  result;
			end for;
		      end block;
		    end method);

define-ct-evaluator
  (#"limited", #(#"<class>", rest:, #"<object>"),
   method (class :: <cclass>, #rest keys)
       => res :: false-or(<ctype>);
     select (class by csubtype?)
       dylan-value(#"<general-integer>") =>
	 let (okay, min, max) = ct-keywords(keys, #"min", #"max");
	 if (okay)
	   if ((min == #f | instance?(min, <literal-general-integer>))
		 & (max == #f | instance?(max, <literal-general-integer>)))
	     let min = min & min.literal-value;
	     let max = max & max.literal-value;
	     make-canonical-limited-integer(class, min, max);
	   end;
	 end;
       dylan-value(#"<collection>") =>
	 let (okay, element-type, size) = ct-keywords(keys, #"of", #"size");
	 if (okay)
	   if ((size == #f | instance?(size, <literal-general-integer>))
		 & (instance?(element-type, <ctype>)))
	     make(<limited-collection-ctype>, base-class: class,
		  element-type: element-type,
		  size: size & as(<integer>, size.literal-value));
	   end if;
	 end if;
       otherwise =>
	 #f;
     end;
   end method);

define-ct-evaluator
  (#"make", #(#"<class>", rest:, #"<object>"),
   method (class :: <cclass>, #rest keys)
       => res :: false-or(<ct-value>);
     select (class)
       dylan-value(#"<singleton>") =>
	 let (okay, object) = ct-keywords(keys, #"object");
	 if (okay & instance?(object, <eql-ct-value>))
	   make(<singleton-ctype>, value: object);
	 end;
       dylan-value(#"<byte-character-type>") =>
	 if (ct-keywords(keys))
	   make(<byte-character-ctype>);
	 end;
       dylan-value(#"<not-supplied-marker>") =>
	 if (ct-keywords(keys))
	   make(<ct-not-supplied-marker>);
	 end;
       otherwise =>
	 #f;
     end;
   end method);

define-ct-evaluator(#"negative", #(#"<rational>"),
		    curry(ct-eval-rational-function, negative));

define-ct-evaluator(#"abs", #(#"<rational>"),
		    curry(ct-eval-rational-function, abs));

define-ct-evaluator(#"+", #(#"<rational>", #"<rational>"),
		    curry(ct-eval-rational-function, \+));

define-ct-evaluator(#"-", #(#"<rational>", #"<rational>"),
		    curry(ct-eval-rational-function, \-));

define-ct-evaluator(#"*", #(#"<rational>", #"<rational>"),
		    curry(ct-eval-rational-function, \*));

define-ct-evaluator
  (#"ash", #(#"<general-integer>", #"<integer>"),
   curry(ct-eval-rational-function,
	 method (x :: <extended-integer>, y :: <extended-integer>)
	     => res :: <extended-integer>;
	   ash(x, as(<integer>, y));
	 end));

define-ct-evaluator(#"^", #(#"<general-integer>", #"<general-integer>"),
		    curry(ct-eval-rational-function, \^));

define-ct-evaluator(#"logior", #(rest:, #"<general-integer>"),
		    curry(ct-eval-rational-function, logior));

define-ct-evaluator(#"logxor", #(rest:, #"<general-integer>"),
		    curry(ct-eval-rational-function, logxor));

define-ct-evaluator(#"logand", #(rest:, #"<general-integer>"),
		    curry(ct-eval-rational-function, logand));

define-ct-evaluator(#"lognot", #(#"<general-integer>"),
		    curry(ct-eval-rational-function, lognot));
