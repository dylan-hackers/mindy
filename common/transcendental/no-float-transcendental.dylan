module: Transcendental
author: Ben Folk-Williams
synopsis: Transcendentals.
RCS-header: $Header: /scm/cvs/src/common/transcendental/Attic/no-float-transcendental.dylan,v 1.3 2002/08/23 12:21:48 bruce Exp $
copyright: see below

//======================================================================
//
// Copyright (c) 1996  Carnegie Mellon University
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

// A quick and dirty Transcendental library, implemented by calling
// out to C.

/// ### I want to inline these functions, but then we end up with calls to
/// sin & co. in C files that don't have #include <math.h>
/// Solution?

/// Not quite complete yet:
/// ### Possibly don't catch all errors at the dylan level.
/// ### Need to deal with extended integers (?)
/// ### Need to implement asinh etc. for other than hp.
/// ### Not sure that the \^ we already have from the dylan module is
///     conformant with the spec that the rest of this file is implemented
///     from.

/// andreas:
/// A lot of this looks broken, such as all foof single precison calls.
/// We probably have to do a lot of testing in configure to find out
/// what functions are there and what not.

c-include("math.h");

// We write out pi rather than use C's M_PI because not all C
// compilers have M_PI.  Similarly for e.
define constant $double-pi :: <double-float> 
  = as(<double-float>, 3.14159265358979323846);
define constant $single-pi :: <single-float> = as(<single-float>, $double-pi);

define constant $double-e :: <double-float>
  = as(<double-float>, 2.7182818284590452354);
define constant $single-e :: <single-float> = as(<single-float>, $double-e);

define generic sin (x :: <real>) => y :: <float>;
define generic cos (x :: <real>) => y :: <float>;
define generic tan (x :: <real>) => y :: <float>;
define generic asin (x :: <real>) => y :: <float>;
define generic acos (x :: <real>) => y :: <float>;
define generic atan (x :: <real>) => y :: <float>;
define generic atan2 (y :: <real>, x :: <real>) => z :: <float>;
define generic sinh (x :: <real>) => y :: <float>;
define generic cosh (x :: <real>) => y :: <float>;
define generic tanh (x :: <real>) => y :: <float>;
define generic asinh (x :: <real>) => y :: <float>;
define generic acosh (x :: <real>) => y :: <float>;
define generic atanh (x :: <real>) => y :: <float>;
define generic log (x :: <real>, #key base :: <real>) => y :: <float>;
define generic logn (x :: <real>, b :: <real>) => y :: <float>;
define generic exp (x :: <real>) => y :: <float>;
define generic sqrt (x :: <real>) => y :: <float>;
define generic isqrt (x :: <integer>) => y :: <integer>;
// The generic \^ already exists. (see runtime/dylan/num.dylan)

// Already have methods on \^ for <real>, <integer>
// (see runtime/dylan/num.dylan)

/*
define sealed method \^ (b :: <integer>, x :: <real>)
 => y :: <single-float>;
  if (b.zero? & ~x.positive?)
    error("Exponent must be positive if base is zero"); 
  end;
  if (b.negative? & ~x.integral?)
    error("Exponent must be an integer if base is negative.");
  end;
  call-out("powf", float:, float: as(<single-float>, b), float: x);
end method;

define sealed method \^ (b :: <single-float>, x :: <real>)
 => y :: <single-float>;
  if (b.zero? & ~x.positive?)
    error("Exponent must be positive if base is zero"); 
  end;
  if (b.negative? & ~x.integral?)
    error("Exponent must be an integer if base is negative.");
  end;
  call-out("powf", float:, float: b, float: x);
end method;

define sealed method \^ (b :: <double-float>, x :: <real>)
 => y :: <double-float>;
  if (b.zero? & ~x.positive?)
    error("Exponent must be positive if base is zero"); 
  end;
  if (b.negative? & ~x.integral?)
    error("Exponent must be an integer if base is negative.");
  end;
  call-out("pow", double:, double: b, double: x);
end method;
*/

define sealed method log (x :: <double-float>,
			  #key base :: <real> = $double-e)
 => y :: <double-float>;
  if (x.negative?) error("%= is negative", x) end;
  if (base <= 1) error("Base %= is not greater than 1", base) end;
  select (base)
    $double-e, $single-e => call-out("log", double:, double: x);

#if (compiled-for-hpux) // #if (have-log2)
    2, 2.0d0, 2.0s0 => call-out("log2", double:, double: x);
#endif

    10, 10.0d0, 10.0s0 => call-out("log10", double:, double: x);
    otherwise => call-out("log", double:, double: x) / call-out("log", double:, double: base);
  end select;
end method log;

define sealed method log (x :: <single-float>,
			  #key base :: <real> = $double-e)
 => y :: <single-float>;
  if (x.negative?) error("%= is negative", x) end;
  if (base <= 1) error("Base %= is not greater than 1", base) end;
  select (base)
    $double-e, $single-e => call-out("log", float:, float: x);

#if (compiled-for-hpux) // #if (have-log2)
    2, 2.0d0, 2.0s0 => call-out("log2", float:, float: x);
#endif

    10, 10.0d0, 10.0s0 => call-out("log10", float:, float: x);
    otherwise => call-out("log", float:, float: x) / call-out("log", double:, double: base);
  end select;
end method log;

define sealed method log (x :: <integer>,
			  #key base :: <real> = $double-e)
 => y :: <single-float>;
  if (x.negative?) error("%= is negative", x) end;
  if (base <= 1) error("Base %= is not greater than 1", base) end;
  select (base)
    $double-e, $single-e => call-out("log", float:,
				       float: as(<single-float>, x));
#if (compiled-for-hpux) // #if (have-log2)
    2, 2.0d0, 2.0s0 => call-out("log2", float:,
				float: as(<single-float>, x));
#endif

    10, 10.0d0, 10.0s0 => call-out("log10", float:,
				   float: as(<single-float>, x));
    otherwise => call-out("log", float:, float: as(<single-float>, x)) / call-out("log", double:, double: base);
  end select;
end method log;

// Natural log function, added to support Functional Object's API.
// 8-3-2002 <bfulgham@debian.org>
define sealed method logn (x :: <double-float>, b :: <real>)
 => y :: <double-float>;
  log(x, base: b);
end method logn;

define sealed method logn (x :: <single-float>, b :: <real>)
 => y :: <single-float>;
  log(x, base: b);
end method logn;

define sealed method logn (x :: <integer>,  b :: <real>)
 => y :: <single-float>;
  log(x, base: b);
end method logn;

define sealed method isqrt (x :: <integer>) => y :: <integer>;
  if (x.negative?) error("%= is negative", x) end;
  floor(call-out("sqrt", float:, float: as(<single-float>, x)));
end method isqrt;

define sealed method sqrt (x :: <double-float>) => y :: <double-float>;
  if (x.negative?) error("%= is negative", x) end;
  call-out("sqrt", double:, double: x);
end method sqrt;

define sealed method exp (x :: <double-float>) => y :: <double-float>;
  call-out("exp", double:, double: x);
end method exp;

define sealed method sin (x :: <double-float>) => y :: <double-float>;
  call-out("sin", double:, double: x);
end method sin;

define sealed method cos (x :: <double-float>) => y :: <double-float>;
  call-out("cos", double:, double: x);
end method cos;

define sealed method tan (x :: <double-float>) => y :: <double-float>;
  call-out("tan", double:, double: x);
end method tan;

define sealed method asin (x :: <double-float>) => y :: <double-float>;
  if (x < -1 | x > 1) error("%= is not in the range [-1, 1]", x) end;
  call-out("asin", double:, double: x);
end method asin;

define sealed method acos (x :: <double-float>) => y :: <double-float>;
  if (x < -1 | x > 1) error("%= is not in the range [-1, 1]", x) end;
  call-out("acos", double:, double: x);
end method acos;

define sealed method atan (x :: <double-float>) => y :: <double-float>;
  call-out("atan", double:, double: x);
end method atan;

define sealed method atan2 (y :: <double-float>, x :: <double-float>)
 => z :: <double-float>;
  if (y.zero? & x.zero?) error("Both args are zero") end;
  call-out("atan2", double:, double: y, double: x);
end method atan2;

define sealed method atan2 (y :: <double-float>, x :: <integer>)
 => z :: <double-float>;
  if (y.zero? & x.zero?) error("Both args are zero") end;
  call-out("atan2", double:, double: y, double: as(<double-float>, x));
end method atan2;

define sealed method atan2 (y :: <single-float>, x :: <integer>)
 => z :: <single-float>;
  if (y.zero? & x.zero?) error("Both args are zero") end;
  as(<single-float>, call-out("atan2", double:, double: y, double: as(<double-float>, x)) );
end method atan2;

define sealed method atan2 (y :: <integer>, x :: <single-float>)
 => z :: <single-float>;
  if (y.zero? & x.zero?) error("Both args are zero") end;
  as(<single-float>, call-out("atan2", double:, double: y, double: as(<double-float>, x)) );
end method atan2;

define sealed method atan2 (y :: <integer>, x :: <double-float>)
 => z :: <double-float>;
  if (y.zero? & x.zero?) error("Both args are zero") end;
  call-out("atan2", double:, double: as(<double-float>, y),
	   double: x);
end method atan2;

define sealed method atan2 (y :: <double-float>, x :: <single-float>)
 => z :: <double-float>;
  if (y.zero? & x.zero?) error("Both args are zero") end;
  call-out("atan2", double:, double: y, double: as(<double-float>, x));
end method atan2;

define sealed method atan2 (y :: <single-float>, x :: <double-float>)
 => z :: <double-float>;
  if (y.zero? & x.zero?) error("Both args are zero") end;
  call-out("atan2", double:, double: as(<double-float>, y), double: x);
end method atan2;

define sealed method sinh (x :: <integer>) => y :: <single-float>;
  as( <single-float>, call-out("sin", double:, double: as(<double-float>, x)) );
end method sinh;

define sealed method sinh (x :: <single-float>) => y :: <single-float>;
  as( <single-float>, call-out("sin", double:, double: as(<double-float>, x)) );
end method sinh;

define sealed method sinh (x :: <double-float>) => y :: <double-float>;
  call-out("sinh", double:, double: x);
end method sinh;

define sealed method cosh (x :: <integer>) => y :: <single-float>;
  as( <single-float>, call-out("cos", double:, double: as(<double-float>, x)) );
end method cosh;

define sealed method cosh (x :: <single-float>) => y :: <single-float>;
  as( <single-float>, call-out("cos", double:, double: as(<double-float>, x)) );
end method cosh;

define sealed method cosh (x :: <double-float>) => y :: <double-float>;
  call-out("cosh", double:, double: x);
end method cosh;

define sealed method tanh (x :: <integer>) => y :: <single-float>;
  as( <single-float>, call-out("tan", double:, double: as(<double-float>, x)) );
end method tanh;

define sealed method tanh (x :: <single-float>) => y :: <single-float>;
  as( <single-float>, call-out("tan", double:, double: as(<double-float>, x)) );
end method tanh;

define sealed method tanh (x :: <double-float>) => y :: <double-float>;
  call-out("tanh", double:, double: x);
end method tanh;


// Darwin (and others) don't have the ...f() calls, 
// so we substitute the ordinary calls, taking the speed hit

define sealed method acos (x :: <integer>) => y :: <single-float>;
  if (x < -1 | x > 1) error("%= is not in the range [-1, 1]", x) end;
  as( <single-float>, call-out("acos", double:, double: as(<double-float>, x)) );
end method acos;

define sealed method acos (x :: <single-float>) => y :: <single-float>;
  if (x < -1 | x > 1) error("%= is not in the range [-1, 1]", x) end;
  as( <single-float>, call-out("acos", double:, double: as(<double-float>, x)) );
end method acos;

define sealed method asin (x :: <integer>) => y :: <single-float>;
  if (x < -1 | x > 1) error("%= is not in the range [-1, 1]", x) end;
  as( <single-float>, call-out("asin", double:, double: as(<double-float>, x)) );
end method asin;

define sealed method asin (x :: <single-float>) => y :: <single-float>;
  if (x < -1 | x > 1) error("%= is not in the range [-1, 1]", x) end;
  as( <single-float>, call-out("asin", double:, double: as(<double-float>, x)) );
end method asin;

define sealed method atan2 (y :: <integer>, x :: <integer>)
 => z :: <single-float>;
  if (y.zero? & x.zero?) error("Both args are zero") end;
  as( <single-float>, call-out("atan2", double:, double: as(<double-float>, y),
	   double: as(<double-float>, x)) );
end method atan2;

define sealed method atan2 (y :: <single-float>, x :: <single-float>)
 => z :: <single-float>;
  if (y.zero? & x.zero?) error("Both args are zero") end;
  as( <single-float>, call-out("atan2", double:, double: as(<double-float>, y),
	   double: as(<double-float>, x)) );
end method atan2;

define sealed method atan (x :: <integer>) => y :: <single-float>;
  as( <single-float>, call-out("atan", double:, double: as(<double-float>, x)) );
end method atan;

define sealed method atan (x :: <single-float>) => y :: <single-float>;
  as( <single-float>, call-out("atan", double:, double: as(<double-float>, x)) );
end method atan;

define sealed method cos (x :: <integer>) => y :: <single-float>;
  as( <single-float>, call-out("cos", double:, double: as(<double-float>, x)) );
end method cos;

define sealed method cos (x :: <single-float>) => y :: <single-float>;
  as( <single-float>, call-out("cos", double:, double: as(<double-float>, x)) );
end method cos;

define sealed method exp (x :: <integer>) => y :: <single-float>;
  as( <single-float>, call-out("exp", double:, double: as(<double-float>, x)) );
end method exp;

define sealed method exp (x :: <single-float>) => y :: <single-float>;
  as( <single-float>, call-out("exp", double:, double: as(<double-float>, x)) );
end method exp;

define sealed method sin (x :: <integer>) => y :: <single-float>;
  as( <single-float>, call-out("sin", double:, double: as(<double-float>, x)) );
end method sin;

define sealed method sin (x :: <single-float>) => y :: <single-float>;
  as( <single-float>, call-out("sin", double:, double: as(<double-float>, x)) );
end method sin;

define sealed method sqrt (x :: <integer>) => y :: <single-float>;
  if (x.negative?) error("%= is negative", x) end;
  as( <single-float>, call-out("sqrt", double:, double: as(<double-float>, x)) );
end method sqrt;

define sealed method sqrt (x :: <single-float>) => y :: <single-float>;
  if (x.negative?) error("%= is negative", x) end;
  as( <single-float>, call-out("sqrt", double:, double: as(<double-float>, x)) );
end method sqrt;

define sealed method tan (x :: <integer>) => y :: <single-float>;
  as( <single-float>, call-out("tan", double:, double: as(<double-float>, x)) );
end method tan;

define sealed method tan (x :: <single-float>) => y :: <single-float>;
  as( <single-float>, call-out("tan", double:, double: as(<double-float>, x)) );
end method tan;

// Inverse hyperbolic trig functions are not implemented for x86 because C
// doesn't have them, and we haven't yet felt up to writing our own.
// 
// Linux math lib has them, so include them.

#if (compiled-for-hpux | compiled-for-linux | compiled-for-beos)

define sealed method asinh (x :: <integer>) => y :: <single-float>;
  as(<single-float>,
     call-out("asinh", double:, double: as(<double-float>, x)));
end method asinh;

define sealed method asinh (x :: <single-float>) => y :: <single-float>;
  as(<single-float>,  
     call-out("asinh", double:, double: as(<double-float>, x)));
end method asinh;

define sealed method asinh (x :: <double-float>) => y :: <double-float>;
  call-out("asinh", double:, double: x);
end method asinh;

define sealed method acosh (x :: <integer>) => y :: <single-float>;
  as(<single-float>,
     call-out("acosh", double:, double: as(<double-float>, x)));
end method acosh;

define sealed method acosh (x :: <single-float>) => y :: <single-float>;
  as(<single-float>,
     call-out("acosh", double:, double: as(<double-float>, x)));
end method acosh;

define sealed method acosh (x :: <double-float>) => y :: <double-float>;
  call-out("acosh", double:, double: x);
end method acosh;

define sealed method atanh (x :: <integer>) => y :: <single-float>;
  as(<single-float>,
     call-out("atanh", double:, double: as(<double-float>, x)));
end method atanh;

define sealed method atanh (x :: <single-float>) => y :: <single-float>;
  as(<single-float>,
     call-out("atanh", double:, double: as(<double-float>, x)));
end method atanh;

define sealed method atanh (x :: <double-float>) => y :: <double-float>;
  call-out("atanh", double:, double: x);
end method atanh;



#endif
