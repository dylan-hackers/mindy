module: dylan-viscera
author: Ben Folk-Williams
synopsis: Transcendentals.
copyright: Copyright (c) 1996  Carnegie Mellon University
	   All rights reserved.

/// Not quite complete yet:
/// Need to add some error checks. Right now it's up to C to catch everything.
/// Need to implement log for arbitrary bases.
/// Need to deal with extended integers (?)

c-include("math.h");

define constant $double-pi :: <double-float> = 3.14159265358979323846d0;
define constant $single-pi :: <single-float> = as(<single-float>, $double-pi);

define constant $double-e :: <double-float> = 2.7182818284590452354d0;
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
define generic exp (x :: <real>) => y :: <float>;
define generic sqrt (x :: <real>) => y :: <float>;
define generic isqrt (x :: <integer>) => y :: <integer>;
// The generic \^ already exists. (see num.dylan)

// Already have method on \^ for <real>, <integer>
/*
define inline method \^ (b :: <integer>, x :: <real>)
 => y :: <single-float>;
  call-out("powf", float:, float: as(<single-float>, b), float: x);
end method;

define inline method \^ (b :: <single-float>, x :: <real>)
 => y :: <single-float>;
  call-out("powf", float:, float: b, float: x);
end method;

define inline method \^ (b :: <double-float>, x :: <real>)
 => y :: <double-float>;
  call-out("pow", double:, double: b, double: x);
end method;
*/

define inline method log (x :: <double-float>,
			  #key base :: <real> = $double-e)
 => y :: <double-float>;
  select (base)
    $double-e, $single-e => call-out("log", double:, double: x);
    2, 2.0d0, 2.0s0 => call-out("log2", double:, double: x);
    10, 10.0d0, 10.0s0 => call-out("log10", double:, double: x);
    otherwise => error("Haven't bothered to implement a real log yet. Base must be e, 2, or 10.");
  end select;
end method log;

define inline method log (x :: <single-float>,
			  #key base :: <real> = $double-e)
 => y :: <single-float>;
  select (base)
    $double-e, $single-e => call-out("log", float:, float: x);
    2, 2.0d0, 2.0s0 => call-out("log2", float:, float: x);
    10, 10.0d0, 10.0s0 => call-out("log10", float:, float: x);
    otherwise => error("Haven't bothered to implement a real log yet. Base must be e, 2, or 10.");
  end select;
end method log;

define inline method log (x :: <integer>,
			  #key base :: <real> = $double-e)
 => y :: <single-float>;
  select (base)
    $double-e, $single-e => call-out("log", float:,
				       float: as(<single-float>, x));
    2, 2.0d0, 2.0s0 => call-out("log2", float:,
				float: as(<single-float>, x));
    10, 10.0d0, 10.0s0 => call-out("log10", float:,
				   float: as(<single-float>, x));
    otherwise => error("Haven't bothered to implement a real log yet. Base must be e, 2, or 10.");
  end select;
end method log;

define inline method isqrt (x :: <integer>) => y :: <integer>;
  floor(call-out("sqrt", float:, float: as(<single-float>, x)));
end method isqrt;

define inline method sqrt (x :: <integer>) => y :: <single-float>;
  call-out("sqrtf", float:, float: as(<single-float>, x));
end method sqrt;

define inline method sqrt (x :: <single-float>) => y :: <single-float>;
  call-out("sqrtf", float:, float: x);
end method sqrt;

define inline method sqrt (x :: <double-float>) => y :: <double-float>;
  call-out("sqrt", double:, double: x);
end method sqrt;

define inline method exp (x :: <integer>) => y :: <single-float>;
  call-out("expf", float:, float: as(<single-float>, x));
end method exp;

define inline method exp (x :: <single-float>) => y :: <single-float>;
  call-out("expf", float:, float: x);
end method exp;

define inline method exp (x :: <double-float>) => y :: <double-float>;
  call-out("exp", double:, double: x);
end method exp;

define inline method sin (x :: <integer>) => y :: <single-float>;
  call-out("sinf", float:, float: as(<single-float>, x));
end method sin;

define inline method sin (x :: <single-float>) => y :: <single-float>;
  call-out("sinf", float:, float: x);
end method sin;

define inline method sin (x :: <double-float>) => y :: <double-float>;
  call-out("sin", double:, double: x);
end method sin;

define inline method cos (x :: <integer>) => y :: <single-float>;
  call-out("cosf", float:, float: as(<single-float>, x));
end method cos;

define inline method cos (x :: <single-float>) => y :: <single-float>;
  call-out("cosf", float:, float: x);
end method cos;

define inline method cos (x :: <double-float>) => y :: <double-float>;
  call-out("cos", double:, double: x);
end method cos;

define inline method tan (x :: <integer>) => y :: <single-float>;
  call-out("tanf", float:, float: as(<single-float>, x));
end method tan;

define inline method tan (x :: <single-float>) => y :: <single-float>;
  call-out("tanf", float:, float: x);
end method tan;

define inline method tan (x :: <double-float>) => y :: <double-float>;
  call-out("tan", double:, double: x);
end method tan;

define inline method asin (x :: <integer>) => y :: <single-float>;
  call-out("asinf", float:, float: as(<single-float>, x));
end method asin;

define inline method asin (x :: <single-float>) => y :: <single-float>;
  call-out("asinf", float:, float: x);
end method asin;

define inline method asin (x :: <double-float>) => y :: <double-float>;
  call-out("asin", double:, double: x);
end method asin;

define inline method acos (x :: <integer>) => y :: <single-float>;
  call-out("acosf", float:, float: as(<single-float>, x));
end method acos;

define inline method acos (x :: <single-float>) => y :: <single-float>;
  call-out("acosf", float:, float: x);
end method acos;

define inline method acos (x :: <double-float>) => y :: <double-float>;
  call-out("acos", double:, double: x);
end method acos;

define inline method atan (x :: <integer>) => y :: <single-float>;
  call-out("atanf", float:, float: as(<single-float>, x));
end method atan;

define inline method atan (x :: <single-float>) => y :: <single-float>;
  call-out("atanf", float:, float: x);
end method atan;

define inline method atan (x :: <double-float>) => y :: <double-float>;
  call-out("atan", double:, double: x);
end method atan;

define inline method atan2 (y :: <integer>, x :: <integer>)
 => z :: <single-float>;
  call-out("atan2f", float:, float: as(<single-float>, y),
	   float: as(<single-float>, x));
end method atan2;

define inline method atan2 (y :: <single-float>, x :: <single-float>)
 => z :: <single-float>;
  call-out("atan2f", float:, float: y, float: x);
end method atan2;

define inline method atan2 (y :: <double-float>, x :: <double-float>)
 => z :: <double-float>;
  call-out("atan2", double:, double: y, double: x);
end method atan2;

define inline method atan2 (y :: <double-float>, x :: <integer>)
 => z :: <double-float>;
  call-out("atan2", double:, double: y, double: as(<double-float>, x));
end method atan2;

define inline method atan2 (y :: <single-float>, x :: <integer>)
 => z :: <single-float>;
  call-out("atan2f", float:, float: y, float: as(<single-float>, x));
end method atan2;

define inline method atan2 (y :: <integer>, x :: <single-float>)
 => z :: <single-float>;
  call-out("atan2f", float:, float: as(<single-float>, y),
	   float: x);
end method atan2;

define inline method atan2 (y :: <integer>, x :: <double-float>)
 => z :: <double-float>;
  call-out("atan2", double:, double: as(<double-float>, y),
	   double: x);
end method atan2;

define inline method atan2 (y :: <double-float>, x :: <single-float>)
 => z :: <double-float>;
  call-out("atan2", double:, double: y, double: as(<double-float>, x));
end method atan2;

define inline method atan2 (y :: <single-float>, x :: <double-float>)
 => z :: <double-float>;
  call-out("atan2", double:, double: as(<double-float>, y), double: x);
end method atan2;

define inline method sinh (x :: <integer>) => y :: <single-float>;
  call-out("sinf", float:, float: as(<single-float>, x));
end method sinh;

define inline method sinh (x :: <single-float>) => y :: <single-float>;
  call-out("sinf", float:, float: x);
end method sinh;

define inline method sinh (x :: <double-float>) => y :: <double-float>;
  call-out("sin", double:, double: x);
end method sinh;

define inline method cosh (x :: <integer>) => y :: <single-float>;
  call-out("cosf", float:, float: as(<single-float>, x));
end method cosh;

define inline method cosh (x :: <single-float>) => y :: <single-float>;
  call-out("cosf", float:, float: x);
end method cosh;

define inline method cosh (x :: <double-float>) => y :: <double-float>;
  call-out("cos", double:, double: x);
end method cosh;

define inline method tanh (x :: <integer>) => y :: <single-float>;
  call-out("tanf", float:, float: as(<single-float>, x));
end method tanh;

define inline method tanh (x :: <single-float>) => y :: <single-float>;
  call-out("tanf", float:, float: x);
end method tanh;

define inline method tanh (x :: <double-float>) => y :: <double-float>;
  call-out("tan", double:, double: x);
end method tanh;

define inline method asinh (x :: <integer>) => y :: <single-float>;
  call-out("asinf", float:, float: as(<single-float>, x));
end method asinh;

define inline method asinh (x :: <single-float>) => y :: <single-float>;
  call-out("asinf", float:, float: x);
end method asinh;

define inline method asinh (x :: <double-float>) => y :: <double-float>;
  call-out("asin", double:, double: x);
end method asinh;

define inline method acosh (x :: <integer>) => y :: <single-float>;
  call-out("acosf", float:, float: as(<single-float>, x));
end method acosh;

define inline method acosh (x :: <single-float>) => y :: <single-float>;
  call-out("acosf", float:, float: x);
end method acosh;

define inline method acosh (x :: <double-float>) => y :: <double-float>;
  call-out("acos", double:, double: x);
end method acosh;

define inline method atanh (x :: <integer>) => y :: <single-float>;
  call-out("atanf", float:, float: as(<single-float>, x));
end method atanh;

define inline method atanh (x :: <single-float>) => y :: <single-float>;
  call-out("atanf", float:, float: x);
end method atanh;

define inline method atanh (x :: <double-float>) => y :: <double-float>;
  call-out("atan", double:, double: x);
end method atanh;

