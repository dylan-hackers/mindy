module: dylan
rcs-header: $Header: /scm/cvs/src/mindy/libraries/dylan/ratio.dylan,v 1.1 1998/05/03 19:55:21 andreas Exp $

//======================================================================
//
// Copyright (c) 1995  Carnegie Mellon University
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
//  This file contains the support code for ratios.
//

// Note: the <ratio> class itself is defined by C code so that == can examine its
// slots.

define method make (class == <ratio>, #key numerator, denominator)
    => res :: <ratio>;
  //
  // Convert them to extended integers.
  let numerator = as(<extended-integer>, numerator);
  let denominator = as(<extended-integer>, denominator);
  //
  // Make sure the denominator is positive.
  let (numerator, denominator)
    = if (negative?(denominator))
	values(- numerator, - denominator);
      elseif (positive?(denominator))
	values(numerator, denominator);
      else
	error("Can't make a ratio with a zero denominator.");
      end;
  //
  // Now divide out the gcd and make the ratio object.
  let gcd = gcd(numerator, denominator);
  if (gcd = 1)
    make-ratio(numerator, denominator);
  else
    make-ratio(truncate/(numerator, gcd), truncate/(denominator, gcd));
  end;
end;

define method ratio (num :: <general-integer>, denom :: <general-integer>)
    => res :: <ratio>;
  make(<ratio>, numerator: num, denominator: denom);
end;

define method as (class == <ratio>, num :: <number>)
 => number :: <ratio>;
  rationalize(num);
end;


define method as (class :: limited(<class>, subclass-of: <float>),
		  num :: <ratio>)
    => res :: <float>;
  as(class, num.numerator) / as(class, num.denominator);
end;

define method \= (a :: <ratio>, b :: <ratio>)
    => res :: <boolean>;
  a == b;
end;

define method \< (a :: <ratio>, b :: <ratio>)
    => res :: <boolean>;
  // Start with:
  //   an/ad < bn/bd
  // Multiply by ad and bd:
  //   an*bd < bn*ad
  a.numerator * b.denominator < b.numerator * a.denominator;
end;

define method zero? (r :: <ratio>)
    => res :: <boolean>;
  r.numerator.zero?;
end;

define method positive? (r :: <ratio>)
    => res :: <boolean>;
  r.numerator.positive?;
end;

define method negative? (r :: <ratio>)
    => res :: <boolean>;
  r.numerator.negative?;
end;

define method integral? (r :: <ratio>)
    => res :: <boolean>;
  r.denominator = 1;
end;

define method \+ (a :: <ratio>, b :: <ratio>)
    => res :: <ratio>;
  // Start with:
  //   an/ad + bn/bd
  // Multiply the left by bd/bd and the right by ad/ad:
  //   (an*bd)/(ad*bd) + (bn*ad)/(ad*bd)
  // Factor out the new denominator:
  //   (an*bd + bn*ad) / (ad*bd)
  ratio(a.numerator * b.denominator + b.numerator * a.denominator,
	a.denominator * b.denominator);
end;

define method \* (a :: <ratio>, b :: <ratio>)
    => res :: <ratio>;
  // Start with:
  //   an/ad * bn/bd
  // Collect terms:
  //   (an * bn) / (ad * bd)
  ratio(a.numerator * b.numerator, a.denominator * b.denominator);
end;

define method \- (a :: <ratio>, b :: <ratio>)
    => res :: <ratio>;
  // Start with:
  //   an/ad - bn/bd
  // Multiply the left by bd/bd and the right by ad/ad:
  //   (an*bd)/(ad*bd) - (bn*ad)/(ad*bd)
  // Factor out the new denominator:
  //   (an*bd - bn*ad) / (ad*bd)
  ratio(a.numerator * b.denominator - b.numerator * a.denominator,
	a.denominator * b.denominator);
end;

define method \/ (a :: <ratio>, b :: <ratio>)
    => res :: <ratio>;
  // Start with:
  //   (an/ad) / (bn/bd)
  // Invert the right hand side:
  //   an/ad * bd/bn
  // Collect terms:
  //   (an * bd) / (ad * bn)
  ratio(a.numerator * b.denominator, a.denominator * b.numerator);
end;

define method negative (r :: <ratio>)
    => res :: <ratio>;
  ratio(-r.numerator, r.denominator);
end;

define method floor (r :: <ratio>)
    => (quo :: <extended-integer>, rem :: <ratio>);
  let quo = floor/(r.numerator, r.denominator);
  values(quo, r - quo);
end;

define method ceiling (r :: <ratio>)
    => (quo :: <extended-integer>, rem :: <ratio>);
  let quo = ceiling/(r.numerator, r.denominator);
  values(quo, r - quo);
end;

define method round (r :: <ratio>)
    => (quo :: <extended-integer>, rem :: <ratio>);
  let quo = round/(r.numerator, r.denominator);
  values(quo, r - quo);
end;

define method truncate (r :: <ratio>)
    => (quo :: <extended-integer>, rem :: <ratio>);
  let quo = truncate/(r.numerator, r.denominator);
  values(quo, r - quo);
end;

define method abs (r :: <ratio>)
    => res :: <ratio>;
  ratio(r.numerator.abs, r.denominator);
end;

define method rationalize (num :: <general-integer>) => res :: <ratio>;
  ratio(num, 1);
end;

define method rationalize (num :: <ratio>) => res :: <ratio>;
  num;
end;

define method rationalize (num :: <float>) => res :: <ratio>;
  let sign = if (negative?(num))
	       num := -num;
	       -1;
	     else
	       1;
	     end;
  let scale = 0;
  while (num >= 1)
    scale := scale + 8;
    num := num / 256;
  end;
  let numerator = as(<extended-integer>, 0);
  until (zero?(num))
    let (chunk, remainder) = floor(num * 256);
    numerator := ash(numerator, 8) + chunk;
    num := remainder;
    scale := scale - 8;
  end;
  if (zero?(scale))
    ratio(sign * numerator, 1);
  else
    sign * numerator * ratio(2,1) ^ scale;
  end;
end;


// Ratio/integer contagion methods

// These methods could just convert the integer into a ratio and then call
// the ratio/ratio methods, but we do the op directly because the ops are
// rather simple and this way we don't have to make a ratio that is just going
// to be pulled apart and thrown away.  And we also avoid a bunch of spurious
// multiplies by 1.

define method \= (a :: <ratio>, b :: <general-integer>)
    => res :: <boolean>;
  a.numerator = b & a.denominator = 1;
end;

define method \= (a :: <general-integer>, b :: <ratio>)
    => res :: <boolean>;
  a = b.numerator & 1 = b.denominator;
end;

define method \< (a :: <ratio>, b :: <general-integer>)
    => res :: <boolean>;
  // Start with:
  //   a_n/a_d < b
  // multiply though by a_d:
  //   a_n < b * a_d
  a.numerator < b * a.denominator;
end;

define method \< (a :: <general-integer>, b :: <ratio>)
    => res :: <boolean>;
  // Start with:
  //   a < b_n/b_d
  // multiply though by b_d:
  //   a * b_d < b_n
  a * b.denominator < b.numerator;
end;

define method \+ (a :: <general-integer>, b :: <ratio>)
    => res :: <ratio>;
  // Start with:
  //   a + b_n/b_d
  // Factor out 1/b_d:
  //   (a*b_d + b_n) / b_d
  ratio(a * b.denominator + b.numerator, b.denominator);
end;

define method \+ (a :: <ratio>, b :: <general-integer>)
    => res :: <ratio>;
  // Start with:
  //   a_n/a_d + b
  // Factor out 1/a_d:
  //   (a_n + b*a_d) / a_d;
  ratio(a.numerator + b * a.denominator, a.denominator);
end;

define method \* (a :: <general-integer>, b :: <ratio>)
    => res :: <ratio>;
  ratio(a * b.numerator, b.denominator);
end;

define method \* (a :: <ratio>, b :: <general-integer>)
    => res :: <ratio>;
  ratio(a.numerator * b, a.denominator);
end;

define method \- (a :: <general-integer>, b :: <ratio>)
    => res :: <ratio>;
  // Start with:
  //   a - b_n/b_d
  // Factor out 1/b_d:
  //   (a*b_d - b_n) / b_d
  ratio(a * b.denominator - b.numerator, b.denominator);
end;

define method \- (a :: <ratio>, b :: <general-integer>)
    => res :: <ratio>;
  // Start with:
  //   a_n/a_d - b
  // Factor out 1/a_d:
  //   (a_n - b*a_d) / a_d;
  ratio(a.numerator - b * a.denominator, a.denominator);
end;

define method \/ (a :: <general-integer>, b :: <ratio>)
    => res :: <ratio>;
  // Start with:
  //   a / b_n/b_d
  // Invert the right hand side:
  //   a * b_d/b_n
  // Collect terms:
  //   a*b_d / b_n
  ratio(a * b.denominator, b.numerator);
end;

define method \/ (a :: <ratio>, b :: <general-integer>)
    => res :: <ratio>;
  // Start with:
  //   a_n/a_d / b
  // Invert the right hand side:
  //   a_n/a_d * 1/b
  // Collect terms:
  //   a_n / a_d*b
  ratio(a.numerator, a.denominator * b);
end;


// Ratio/float contagion methods.

// ### These comparisons really should be converting the float into a ratio
// and then comparing them.  Oh well.

define method \= (a :: <ratio>, b :: <single-float>)
    => res :: <boolean>;
  as(<single-float>, a) = b;
end;

define method \= (a :: <single-float>, b :: <ratio>)
    => res :: <boolean>;
  a = as(<single-float>, b);
end;

define method \= (a :: <ratio>, b :: <double-float>)
    => res :: <boolean>;
  as(<double-float>, a) = b;
end;

define method \= (a :: <double-float>, b :: <ratio>)
    => res :: <boolean>;
  a = as(<double-float>, b);
end;

define method \= (a :: <ratio>, b :: <extended-float>)
    => res :: <boolean>;
  as(<extended-float>, a) = b;
end;

define method \= (a :: <extended-float>, b :: <ratio>)
    => res :: <boolean>;
  a = as(<extended-float>, b);
end;

define method \< (a :: <ratio>, b :: <single-float>)
    => res :: <boolean>;
  as(<single-float>, a) < b;
end;

define method \< (a :: <single-float>, b :: <ratio>)
    => res :: <boolean>;
  a < as(<single-float>, b);
end;

define method \< (a :: <ratio>, b :: <double-float>)
    => res :: <boolean>;
  as(<double-float>, a) < b;
end;

define method \< (a :: <double-float>, b :: <ratio>)
    => res :: <boolean>;
  a < as(<double-float>, b);
end;

define method \< (a :: <ratio>, b :: <extended-float>)
    => res :: <boolean>;
  as(<extended-float>, a) < b;
end;

define method \< (a :: <extended-float>, b :: <ratio>)
    => res :: <boolean>;
  a < as(<extended-float>, b);
end;

define method \+ (a :: <ratio>, b :: <single-float>)
    => res :: <single-float>;
  as(<single-float>, a) + b;
end;

define method \+ (a :: <single-float>, b :: <ratio>)
    => res :: <single-float>;
  a + as(<single-float>, b);
end;

define method \+ (a :: <ratio>, b :: <double-float>)
    => res :: <double-float>;
  as(<double-float>, a) + b;
end;

define method \+ (a :: <double-float>, b :: <ratio>)
    => res :: <double-float>;
  a + as(<double-float>, b);
end;

define method \+ (a :: <ratio>, b :: <extended-float>)
    => res :: <extended-float>;
  as(<extended-float>, a) + b;
end;

define method \+ (a :: <extended-float>, b :: <ratio>)
    => res :: <extended-float>;
  a + as(<extended-float>, b);
end;

define method \* (a :: <ratio>, b :: <single-float>)
    => res :: <single-float>;
  as(<single-float>, a) * b;
end;

define method \* (a :: <single-float>, b :: <ratio>)
    => res :: <single-float>;
  a * as(<single-float>, b);
end;

define method \* (a :: <ratio>, b :: <double-float>)
    => res :: <double-float>;
  as(<double-float>, a) * b;
end;

define method \* (a :: <double-float>, b :: <ratio>)
    => res :: <double-float>;
  a * as(<double-float>, b);
end;

define method \* (a :: <ratio>, b :: <extended-float>)
    => res :: <extended-float>;
  as(<extended-float>, a) * b;
end;

define method \* (a :: <extended-float>, b :: <ratio>)
    => res :: <extended-float>;
  a * as(<extended-float>, b);
end;

define method \- (a :: <ratio>, b :: <single-float>)
    => res :: <single-float>;
  as(<single-float>, a) - b;
end;

define method \- (a :: <single-float>, b :: <ratio>)
    => res :: <single-float>;
  a - as(<single-float>, b);
end;

define method \- (a :: <ratio>, b :: <double-float>)
    => res :: <double-float>;
  as(<double-float>, a) - b;
end;

define method \- (a :: <double-float>, b :: <ratio>)
    => res :: <double-float>;
  a - as(<double-float>, b);
end;

define method \- (a :: <ratio>, b :: <extended-float>)
    => res :: <extended-float>;
  as(<extended-float>, a) - b;
end;

define method \- (a :: <extended-float>, b :: <ratio>)
    => res :: <extended-float>;
  a - as(<extended-float>, b);
end;

define method \/ (a :: <ratio>, b :: <single-float>)
    => res :: <single-float>;
  as(<single-float>, a) / b;
end;

define method \/ (a :: <single-float>, b :: <ratio>)
    => res :: <single-float>;
  a / as(<single-float>, b);
end;

define method \/ (a :: <ratio>, b :: <double-float>)
    => res :: <double-float>;
  as(<double-float>, a) / b;
end;

define method \/ (a :: <double-float>, b :: <ratio>)
    => res :: <double-float>;
  a / as(<double-float>, b);
end;

define method \/ (a :: <ratio>, b :: <extended-float>)
    => res :: <extended-float>;
  as(<extended-float>, a) / b;
end;

define method \/ (a :: <extended-float>, b :: <ratio>)
    => res :: <extended-float>;
  a / as(<extended-float>, b);
end;

