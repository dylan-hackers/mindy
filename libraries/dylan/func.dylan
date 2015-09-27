module: Dylan

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
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
//
//  This file contains the stuff from the functional operations chapter.
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


define method %define-sealed-domain
    (gf :: <generic-function>, #rest types) => ();
  unless (gf.function-arguments == types.size)
    error("Wrong number of types in define sealed domain for %=.\n"
	    "Wanted %d, but got %d",
	  gf, gf.function-arguments, types.size);
  end unless;
  for (gf-type in gf.function-specializers,
       seal-type :: <type> in types)
    unless (subtype?(seal-type, gf-type))
      error("Seal type %= isn't a subtype of gf type %=.",
	    seal-type, gf-type);
    end unless;
  end for;
end method %define-sealed-domain;
