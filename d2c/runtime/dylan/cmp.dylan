rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/cmp.dylan,v 1.1 1998/05/03 19:55:37 andreas Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
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
//    University.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports, questions, comments, and suggestions should be sent by
// E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
//
//======================================================================

//
// Comparisons.
//
// This file defines the various standard comparisons.  Other files may
// add additional methods to these generic functions, but this file
// supplied the general framework.
//


// identity comparisons.

// == -- exported from dylan.
//
// The == function.
//
// Optimization of == is highly magical.  The compiler has hard-wired into
// it knowledge about what == methods exist.  If you add any methods to ==,
// you *must* update the compiler accordingly, or it will incorrectly
// decide to use the <object>,<object> method when it should be using
// your new method.
//
define generic \== (x :: <object>, y :: <object>) => answer :: <boolean>;

// =={<object>,<object>} -- exported GF method.
//
// If the two objects do not have the same class, then they can't be ==.
// If they have the same class, then check to see if that class is functional.
// If so, defer to functional-== to see if they are ==.  Otherwise,
// just do a pointer-comparison.
//
define method \== (x :: <object>, y :: <object>)
    => answer :: <boolean>;
  let x-class :: <class> = x.object-class;
  if (x-class.class-functional?)
    x-class == y.object-class & functional-==(x-class, x, y);
  else
    %%primitive(\==, x, y);
  end;
end;

// ~== -- exported from Dylan.
//
// The complement of ==.  And implemented just as such.
// 
define inline method \~== (x :: <object>, y :: <object>) => res :: <boolean>;
  ~(x == y);
end;

// slow-functional-== -- internal.
//
// The compiler transforms calls to == when it can prove that at least
// one argument is a functional class to a call to this routine instead.
// In the worst case, this allows us to skip the class-functional? check.
// But more likely, the compiler will further optimize the call by selecting
// the specific functional-== method.  The compiler doesn't do it all in one
// step because it is a bit easier to do it in two stages like this.
//
define method slow-functional-== (x :: <object>, y :: <object>)
    => answer :: <boolean>;
  let x-class :: <class> = x.object-class;
  if (~(x-class == y.object-class))
    #f;
  else
    functional-==(x-class, x, y);
  end if;
end method slow-functional-==;

// functional-== -- exported from extensions.
//
// Handles == of functional classes.  Both x and y are guaranteed to be
// (possibly indirect) instances of the class.  The class is passed in
// to allow the sealing of functional-== on an instantiable open class.
// 
// There are no default methods because it is up to each functional class to
// define how functional equality works on them.
//
define open generic functional-==
    (class :: <class>, x :: <object>, y :: <object>)
    => answer :: <boolean>;


// equality/inequality comparisons.

// = -- exported from Dylan.
//
// The = function.
//
define open generic \= (x :: <object>, y :: <object>) => answer :: <boolean>;

// ={<object>,<object>} -- exported GF method.
//
// The default for = is to just defer to ==.
// 
define inline method \= (x :: <object>, y :: <object>) => answer :: <boolean>;
  x == y;
end;

// < -- exported from Dylan.
//
// The < function.  Note: there is no default for <.
// 
define open generic \< (x :: <object>, y :: <object>) => answer :: <boolean>;

// <= -- exported from Dylan.
//
// The <= function.
// 
define generic \<= (x :: <object>, y :: <object>) => answer :: <boolean>;

// <={<object>,<object>} -- exported GF method.
//
// The default for <= is to just implement it in terms of ~ and <.  This
// method will be shadowed by floats in order to get NaN comparison to work
// correctly.
//
define inline method \<= (x :: <object>, y :: <object>) => answer :: <boolean>;
  ~(y < x);
end method;

// ~= -- exported from Dylan.
//
// The ~= function.
//
define generic \~= (x :: <object>, y :: <object>) => answer :: <boolean>;

// ~={<object>,<object>} -- exported GF method.
//
// The default for ~= is to just implement it in terms of ~ and =.  This
// method will be shadowed by floats in order to get NaN comparison to work
// correctly.
//
define inline method \~= (x :: <object>, y :: <object>) => answer :: <boolean>;
  ~(x = y);
end method;

// >= -- exported from Dylan.
//
// Just defined in terms of <=.
// 
define inline method \>= (x :: <object>, y :: <object>)
    => answer :: <boolean>;
  y <= x;
end method;

// > -- exported from Dylan.
//
// Just defined in terms of <.
// 
define inline method \> (x :: <object>, y :: <object>)
    => answer :: <boolean>;
  y < x;
end method;



// min and max.

// min -- exported from Dylan
//
// Return the minimum of a bunch of objects.
// 
define inline method min (object :: <object>, #rest more-objects)
    => res :: <object>;
  reduce(binary-min, object, more-objects);
end;
//
// binary-min is a seperate function instead of being an anonymous method
// inside of min so that when min gets inlined each reference to binary-min
// will get independently inlined.
// 
define inline method binary-min (x :: <object>, y :: <object>)
    => res :: <object>;
  if (x < y) x else y end;
end;	  

// max -- exported from Dylan
//
// Return the maximum of a bunch of objects.
// 
define inline method max (object :: <object>, #rest more-objects)
    => res :: <object>;
  reduce(binary-max, object, more-objects);
end;
//
// binary-max is a seperate function instead of being an anonymous method
// inside of max so that when max gets inlined each reference to binary-min
// will get independently inlined.
// 
define inline method binary-max (x :: <object>, y :: <object>)
    => max :: <object>;
  if (x < y) y else x end;
end;	  
