module: Dylan

//////////////////////////////////////////////////////////////////////
//
//  Copyright (C) 1994, Carnegie Mellon University
//  All rights reserved.
//
//  This code was produced by the Gwydion Project at Carnegie Mellon
//  University.  If you are interested in using this code, contact
//  "Scott.Fahlman@cs.cmu.edu" (Internet).
//
//////////////////////////////////////////////////////////////////////
//
//  $Header: /home/housel/work/rcs/gd/src/mindy/libraries/dylan/func.dylan,v 1.3 1994/04/13 03:00:54 rgs Exp $
//
//  This file does whatever.
//

define method compose (function, #rest more-functions)
  if (empty?(more-functions))
    function;
  else
    let next = apply(compose, more-functions);
    method (#rest args)
      function(apply(next, args));
    end;
  end;
end;

define method complement (predicate)
  method (#rest args)
    ~apply(predicate, args);
  end;
end;

define method disjoin (predicate, #rest more-predicates)
  if (empty?(more-predicates))
    predicate;
  else
    let next = apply(disjoin, more-predicates);
    method (#rest args)
      apply(predicate, args) | apply(next, args);
    end;
  end;
end;

define method conjoin (predicate, #rest more-predicates)
  if (empty?(more-predicates))
    predicate;
  else
    let next = apply(conjoin, more-predicates);
    method (#rest args)
      apply(predicate, args) & apply(next, args);
    end;
  end;
end;

define method curry (function, #rest curried-args)
  method (#rest args)
    apply-curry(function, curried-args, args);
  end;
end;

define method rcurry (function, #rest curried-args)
  method (#rest args)
    apply-curry(function, args, curried-args);
  end;
end;

define method always (object)
  method (#rest args)
    object;
  end;
end;

define method applicable-method? (gf :: <generic-function>, #rest args)
  any?(method (meth)
	 apply(applicable-method?, meth, args);
       end,
       generic-function-methods(gf));
end;

define method make-next-method-function (methods, #rest orig-args)
  if (empty?(methods))
    #f;
  else
    method (#rest new-args)
      do-next-method(methods,
		     if (empty?(new-args))
		       orig-args;
		     else
		       new-args;
		     end);
    end;
  end;
end;

define method generic-apply (function :: <function>, #rest arguments)
  let num-regular-args = size(arguments) - 1;
  let more-args = element(arguments, num-regular-args);
  let new-args = make(<vector>, size: num-regular-args + size(more-args));
  for (i from 0 below num-regular-args)
    new-args[i] := arguments[i];
  end;
  for (arg in more-args, i from num-regular-args)
    new-args[i] := arg;
  end;
  apply(function, new-args);
end;
