module: Transcendental
author: Ben Folk-Williams
synopsis: Transcendentals.
RCS-header: $Header: /scm/cvs/src/common/transcendental/transcendental.dylan,v 1.11 2003/10/07 22:28:46 housel Exp $
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

/// Not quite complete yet:
/// ### Possibly don't catch all errors at the dylan level.
/// ### Need to deal with extended integers (?)
/// ### Need to implement asinh etc. for other than hp.
/// ### Not sure that the \^ we already have from the dylan module is
///     conformant with the spec that the rest of this file is implemented
///     from.

// We write out pi rather than use C's M_PI because not all C
// compilers have M_PI.  Similarly for e.
define constant $double-pi :: <double-float> = 3.14159265358979323846d0;
define constant $single-pi :: <single-float> = 3.14159265358979323846s0;

define constant $extended-e :: <extended-float>
  = 2.7182818284590452353602874713526624977572x0;
define constant $double-e :: <double-float> = 2.71828182845904523536d0;
define constant $single-e :: <single-float> = 2.71828182845904523536s0;

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

define sealed inline method \^ (b :: <integer>, x :: <float>)
 => y :: <single-float>;
  if (b.zero? & ~x.positive?)
    error("Exponent must be positive if base is zero"); 
  end;
  if (b.negative? & ~x.integral?)
    error("Exponent must be an integer if base is negative.");
  end;
  c-include("math.h");
  call-out("powf", float:,
           float: as(<single-float>, b),
           float: as(<single-float>, x));
end method;

define sealed inline method \^ (b :: <single-float>, x :: <float>)
 => y :: <single-float>;
  if (b.zero? & ~x.positive?)
    error("Exponent must be positive if base is zero"); 
  end;
  if (b.negative? & ~x.integral?)
    error("Exponent must be an integer if base is negative.");
  end;
  c-include("math.h");
  call-out("powf", float:,
           float: b,
           float: as(<single-float>, x));
end method;

define sealed inline method \^ (b :: <double-float>, x :: <float>)
 => y :: <double-float>;
  if (b.zero? & ~x.positive?)
    error("Exponent must be positive if base is zero"); 
  end;
  if (b.negative? & ~x.integral?)
    error("Exponent must be an integer if base is negative.");
  end;
  c-include("math.h");
  call-out("pow", double:, double: b, double: as(<double-float>, x));
end method;

define sealed inline method log
    (x :: <extended-float>, #key base :: <real> = $extended-e)
 => (y :: <extended-float>);
  if (x.negative?) error("%= is negative", x) end;
  if (base <= 1) error("Base %= is not greater than 1", base) end;
  c-include("math.h");
  select (base)
    $extended-e, $double-e, $single-e =>
      call-out("logl", long-double:, long-double: x);
    2.0x0, 2.0d0, 2.0s0, 2 =>
      call-out("log2l", long-double:, long-double: x);
    10.0x0, 10.0d0, 10.0s0, 10 =>
      call-out("log10l", long-double:, long-double: x);
    otherwise =>
      call-out("logl", long-double:, long-double: x)
        / call-out("logl", long-double:,
                   long-double: as(<extended-float>, base));
  end select;
end method log;

define sealed inline method log
    (x :: <double-float>, #key base :: <real> = $double-e)
 => (y :: <double-float>);
  if (x.negative?) error("%= is negative", x) end;
  if (base <= 1) error("Base %= is not greater than 1", base) end;
  c-include("math.h");
  select (base)
    $extended-e, $double-e, $single-e =>
      call-out("log", double:, double: x);
    2.0x0, 2.0d0, 2.0s0, 2 =>
      call-out("log2", double:, double: x);
    10.0x0, 10.0d0, 10.0s0, 10 =>
      call-out("log10", double:, double: x);
    otherwise =>
      call-out("log", double:, double: x)
        / call-out("log", double:, double: as(<double-float>, base));
  end select;
end method log;

define sealed inline method log
    (x :: <single-float>, #key base :: <real> = $double-e)
 => y :: <single-float>;
  if (x.negative?) error("%= is negative", x) end;
  if (base <= 1) error("Base %= is not greater than 1", base) end;
  c-include("math.h");
  select (base)
    $extended-e, $double-e, $single-e =>
      call-out("logf", float:, float: x);
    2.0x0, 2.0d0, 2.0s0, 2 =>
      call-out("log2f", float:, float: x);
    10.0x0, 10.0d0, 10.0s0, 10 =>
      call-out("log10f", float:, float: x);
    otherwise =>
      call-out("logf", float:, float: x)
        / call-out("logf", float:, float: as(<single-float>, base));
  end select;
end method log;

define sealed inline method log
    (x :: <integer>, #key base :: <real> = $double-e)
 => (y :: <single-float>);
  if (x.negative?) error("%= is negative", x) end;
  if (base <= 1) error("Base %= is not greater than 1", base) end;
  select (base)
    $extended-e, $double-e, $single-e =>
      call-out("logf", float:, float: as(<single-float>, x));
    2.0x0, 2.0d0, 2.0s0, 2 =>
      call-out("log2f", float:, float: as(<single-float>, x));
    10.0x0, 10.0d0, 10.0s0, 10 =>
      call-out("log10f", float:, float: as(<single-float>, x));
    otherwise =>
      call-out("logf", float:, float: as(<single-float>, x))
        / call-out("logf", float:, float: as(<single-float>, base));
  end select;
end method log;

define sealed inline method logn
    (x :: <extended-float>, b :: <real>)
 => (y :: <extended-float>);
  log(x, base: b);
end method logn;

define sealed inline method logn
    (x :: <double-float>, b :: <real>)
 => (y :: <double-float>);
  log(x, base: b);
end method logn;

define sealed inline method logn
    (x :: <single-float>, b :: <real>)
 => (y :: <single-float>);
  log(x, base: b);
end method logn;

define sealed inline method logn
    (x :: <integer>,  b :: <real>)
 => (y :: <single-float>);
  log(x, base: b);
end method logn;

define sealed inline method isqrt (x :: <integer>) => y :: <integer>;
  if (x.negative?) error("%= is negative", x) end;
  c-include("math.h");
  floor(call-out("sqrt", float:, float: as(<single-float>, x)));
end method isqrt;

define sealed inline method sqrt (x :: <integer>) => y :: <single-float>;
  if (x.negative?) error("%= is negative", x) end;
  c-include("math.h");
  call-out("sqrtf", float:, float: as(<single-float>, x));
end method sqrt;

define sealed inline method sqrt (x :: <single-float>) => y :: <single-float>;
  if (x.negative?) error("%= is negative", x) end;
  c-include("math.h");
  call-out("sqrtf", float:, float: x);
end method sqrt;

define sealed inline method sqrt (x :: <double-float>) => y :: <double-float>;
  if (x.negative?) error("%= is negative", x) end;
  c-include("math.h");
  call-out("sqrt", double:, double: x);
end method sqrt;

define sealed inline method exp (x :: <integer>) => y :: <single-float>;
  c-include("math.h");
  call-out("expf", float:, float: as(<single-float>, x));
end method exp;

define sealed inline method exp (x :: <single-float>) => y :: <single-float>;
  c-include("math.h");
  call-out("expf", float:, float: x);
end method exp;

define sealed inline method exp (x :: <double-float>) => y :: <double-float>;
  c-include("math.h");
  call-out("exp", double:, double: x);
end method exp;

define sealed inline method sin (x :: <integer>) => y :: <single-float>;
  c-include("math.h");
  call-out("sinf", float:, float: as(<single-float>, x));
end method sin;

define sealed inline method sin (x :: <single-float>) => y :: <single-float>;
  c-include("math.h");
  call-out("sinf", float:, float: x);
end method sin;

define sealed inline method sin (x :: <double-float>) => y :: <double-float>;
  c-include("math.h");
  call-out("sin", double:, double: x);
end method sin;

define sealed inline method cos (x :: <integer>) => y :: <single-float>;
  c-include("math.h");
  call-out("cosf", float:, float: as(<single-float>, x));
end method cos;

define sealed inline method cos (x :: <single-float>) => y :: <single-float>;
  c-include("math.h");
  call-out("cosf", float:, float: x);
end method cos;

define sealed inline method cos (x :: <double-float>) => y :: <double-float>;
  c-include("math.h");
  call-out("cos", double:, double: x);
end method cos;

define sealed inline method tan (x :: <integer>) => y :: <single-float>;
  c-include("math.h");
  call-out("tanf", float:, float: as(<single-float>, x));
end method tan;

define sealed inline method tan (x :: <single-float>) => y :: <single-float>;
  c-include("math.h");
  call-out("tanf", float:, float: x);
end method tan;

define sealed inline method tan (x :: <double-float>) => y :: <double-float>;
  c-include("math.h");
  call-out("tan", double:, double: x);
end method tan;

define sealed inline method asin (x :: <integer>) => y :: <single-float>;
  if (x < -1 | x > 1) error("%= is not in the range [-1, 1]", x) end;
  c-include("math.h");
  call-out("asinf", float:, float: as(<single-float>, x));
end method asin;

define sealed inline method asin (x :: <single-float>) => y :: <single-float>;
  if (x < -1 | x > 1) error("%= is not in the range [-1, 1]", x) end;
  c-include("math.h");
  call-out("asinf", float:, float: x);
end method asin;

define sealed inline method asin (x :: <double-float>) => y :: <double-float>;
  if (x < -1 | x > 1) error("%= is not in the range [-1, 1]", x) end;
  c-include("math.h");
  call-out("asin", double:, double: x);
end method asin;

define sealed inline method acos (x :: <integer>) => y :: <single-float>;
  if (x < -1 | x > 1) error("%= is not in the range [-1, 1]", x) end;
  c-include("math.h");
  call-out("acosf", float:, float: as(<single-float>, x));
end method acos;

define sealed inline method acos (x :: <single-float>) => y :: <single-float>;
  if (x < -1 | x > 1) error("%= is not in the range [-1, 1]", x) end;
  c-include("math.h");
  call-out("acosf", float:, float: x);
end method acos;

define sealed inline method acos (x :: <double-float>) => y :: <double-float>;
  if (x < -1 | x > 1) error("%= is not in the range [-1, 1]", x) end;
  c-include("math.h");
  call-out("acos", double:, double: x);
end method acos;

define sealed inline method atan (x :: <integer>) => y :: <single-float>;
  c-include("math.h");
  call-out("atanf", float:, float: as(<single-float>, x));
end method atan;

define sealed inline method atan (x :: <single-float>) => y :: <single-float>;
  c-include("math.h");
  call-out("atanf", float:, float: x);
end method atan;

define sealed inline method atan (x :: <double-float>) => y :: <double-float>;
  c-include("math.h");
  call-out("atan", double:, double: x);
end method atan;

define sealed inline method atan2 (y :: <integer>, x :: <integer>)
 => z :: <single-float>;
  if (y.zero? & x.zero?) error("Both args are zero") end;
  c-include("math.h");
  call-out("atan2f", float:, float: as(<single-float>, y),
	   float: as(<single-float>, x));
end method atan2;

define sealed inline method atan2 (y :: <single-float>, x :: <single-float>)
 => z :: <single-float>;
  if (y.zero? & x.zero?) error("Both args are zero") end;
  c-include("math.h");
  call-out("atan2f", float:, float: y, float: x);
end method atan2;

define sealed inline method atan2 (y :: <double-float>, x :: <double-float>)
 => z :: <double-float>;
  if (y.zero? & x.zero?) error("Both args are zero") end;
  c-include("math.h");
  call-out("atan2", double:, double: y, double: x);
end method atan2;

define sealed inline method atan2 (y :: <double-float>, x :: <integer>)
 => z :: <double-float>;
  if (y.zero? & x.zero?) error("Both args are zero") end;
  c-include("math.h");
  call-out("atan2", double:, double: y, double: as(<double-float>, x));
end method atan2;

define sealed inline method atan2 (y :: <single-float>, x :: <integer>)
 => z :: <single-float>;
  if (y.zero? & x.zero?) error("Both args are zero") end;
  c-include("math.h");
  call-out("atan2f", float:, float: y, float: as(<single-float>, x));
end method atan2;

define sealed inline method atan2 (y :: <integer>, x :: <single-float>)
 => z :: <single-float>;
  if (y.zero? & x.zero?) error("Both args are zero") end;
  c-include("math.h");
  call-out("atan2f", float:, float: as(<single-float>, y),
	   float: x);
end method atan2;

define sealed inline method atan2 (y :: <integer>, x :: <double-float>)
 => z :: <double-float>;
  if (y.zero? & x.zero?) error("Both args are zero") end;
  c-include("math.h");
  call-out("atan2", double:, double: as(<double-float>, y),
	   double: x);
end method atan2;

define sealed inline method atan2 (y :: <double-float>, x :: <single-float>)
 => z :: <double-float>;
  if (y.zero? & x.zero?) error("Both args are zero") end;
  c-include("math.h");
  call-out("atan2", double:, double: y, double: as(<double-float>, x));
end method atan2;

define sealed inline method atan2 (y :: <single-float>, x :: <double-float>)
 => z :: <double-float>;
  if (y.zero? & x.zero?) error("Both args are zero") end;
  c-include("math.h");
  call-out("atan2", double:, double: as(<double-float>, y), double: x);
end method atan2;

define sealed inline method sinh (x :: <integer>) => y :: <single-float>;
  c-include("math.h");
  call-out("sinhf", float:, float: as(<single-float>, x));
end method sinh;

define sealed inline method sinh (x :: <single-float>) => y :: <single-float>;
  c-include("math.h");
  call-out("sinhf", float:, float: x);
end method sinh;

define sealed inline method sinh (x :: <double-float>) => y :: <double-float>;
  c-include("math.h");
  call-out("sinh", double:, double: x);
end method sinh;

define sealed inline method cosh (x :: <integer>) => y :: <single-float>;
  c-include("math.h");
  call-out("coshf", float:, float: as(<single-float>, x));
end method cosh;

define sealed inline method cosh (x :: <single-float>) => y :: <single-float>;
  c-include("math.h");
  call-out("coshf", float:, float: x);
end method cosh;

define sealed inline method cosh (x :: <double-float>) => y :: <double-float>;
  c-include("math.h");
  call-out("cosh", double:, double: x);
end method cosh;

define sealed inline method tanh (x :: <integer>) => y :: <single-float>;
  c-include("math.h");
  call-out("tanf", float:, float: as(<single-float>, x));
end method tanh;

define sealed inline method tanh (x :: <single-float>) => y :: <single-float>;
  c-include("math.h");
  call-out("tanhf", float:, float: x);
end method tanh;

define sealed inline method tanh (x :: <double-float>) => y :: <double-float>;
  c-include("math.h");
  call-out("tanh", double:, double: x);
end method tanh;


// Inverse hyperbolic trig functions are not implemented for some
// platforms, and we haven't yet felt up to writing our own.
// 

#if (compiled-for-hpux | compiled-for-linux | compiled-for-freebsd | compiled-for-beos | compiled-for-cygnus)

define sealed method asinh (x :: <integer>) => y :: <single-float>;
  c-include("math.h");
  as(<single-float>,
     call-out("asinh", double:, double: as(<double-float>, x)));
end method asinh;

define sealed method asinh (x :: <single-float>) => y :: <single-float>;
  c-include("math.h");
  as(<single-float>,  
     call-out("asinh", double:, double: as(<double-float>, x)));
end method asinh;

define sealed method asinh (x :: <double-float>) => y :: <double-float>;
  c-include("math.h");
  call-out("asinh", double:, double: x);
end method asinh;

define sealed method acosh (x :: <integer>) => y :: <single-float>;
  c-include("math.h");
  as(<single-float>,
     call-out("acosh", double:, double: as(<double-float>, x)));
end method acosh;

define sealed method acosh (x :: <single-float>) => y :: <single-float>;
  c-include("math.h");
  as(<single-float>,
     call-out("acosh", double:, double: as(<double-float>, x)));
end method acosh;

define sealed method acosh (x :: <double-float>) => y :: <double-float>;
  c-include("math.h");
  call-out("acosh", double:, double: x);
end method acosh;

define sealed method atanh (x :: <integer>) => y :: <single-float>;
  c-include("math.h");
  as(<single-float>,
     call-out("atanh", double:, double: as(<double-float>, x)));
end method atanh;

define sealed method atanh (x :: <single-float>) => y :: <single-float>;
  c-include("math.h");
  as(<single-float>,
     call-out("atanh", double:, double: as(<double-float>, x)));
end method atanh;

define sealed method atanh (x :: <double-float>) => y :: <double-float>;
  c-include("math.h");
  call-out("atanh", double:, double: x);
end method atanh;

#endif
