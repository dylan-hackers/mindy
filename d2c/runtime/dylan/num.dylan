rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/num.dylan,v 1.1 1998/05/03 19:55:38 andreas Exp $
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

// Abstract classes. 

define open abstract class <number> (<object>)
end;

define abstract class <complex> (<number>)
end;

define abstract class <real> (<complex>)
end;

define abstract class <rational> (<real>)
end;

define abstract class <general-integer> (<rational>)
end;

define abstract class <float> (<real>)
end;


// Psuedo-number methods

// These are defined on object because they are useful abstract interfaces.

define open generic zero? (object :: <object>) => res :: <boolean>;

define open generic positive? (num :: <object>) => res :: <boolean>;

define open generic negative? (num :: <object>) => res :: <boolean>;

define open generic integral? (num :: <object>) => res :: <boolean>;

define open generic \+ (num1 :: <object>, num2 :: <object>);

define open generic \* (num1 :: <object>, num2 :: <object>);

define open generic \- (num1 :: <object>, num2 :: <object>);

define open generic \/ (num1 :: <object>, num2 :: <object>);

define open generic negative (num :: <object>) => res :: <object>;

define open generic \^ (num :: <object>, power :: <object>) => res :: <object>;

define open generic abs (num :: <object>) => res :: <object>;


// Complex methods.

define sealed domain \= (<complex>, <complex>);

define sealed inline method zero? (num :: <complex>) => res :: <boolean>;
  num = 0;
end;

define sealed domain \+ (<complex>, <complex>);

define sealed domain \* (<complex>, <complex>);

define sealed domain \- (<complex>, <complex>);

define sealed domain \/ (<complex>, <complex>);

define sealed domain \^ (<complex>, <complex>);

define sealed domain abs (<complex>);



// Real methods.

define sealed domain \< (<real>, <real>);

define sealed inline method positive? (num :: <real>) => res :: <boolean>;
  num > 0;
end;

define sealed inline method negative? (num :: <real>) => res :: <boolean>;
  num < 0;
end;

define sealed inline method integral? (num :: <real>) => res :: <boolean>;
  let (quo, rem) = floor(num);
  zero?(rem);
end;

define sealed inline method negative (num :: <real>) => res :: <real>;
  0 - num;
end;

define sealed generic floor (num :: <real>)
    => (quo :: <general-integer>, rem :: <real>);

define sealed generic ceiling (num :: <real>)
    => (quo :: <general-integer>, rem :: <real>);

define sealed generic round (num :: <real>)
    => (quo :: <general-integer>, rem :: <real>);

define sealed generic truncate (num :: <real>)
    => (quo :: <general-integer>, rem :: <real>);

define sealed generic floor/ (a :: <real>, b :: <real>)
    => (quo :: <general-integer>, rem :: <real>);

define inline method floor/ (a :: <real>, b :: <real>)
    => (quo :: <integer>, rem :: <real>);
  let quo = floor(a / b);
  values(quo, a - quo * b);
end;

define sealed generic ceiling/ (a :: <real>, b :: <real>)
    => (quo :: <general-integer>, rem :: <real>);

define inline method ceiling/ (a :: <real>, b :: <real>)
    => (quo :: <integer>, rem :: <real>);
  let quo = ceiling(a / b);
  values(quo, a - quo * b);
end;

define sealed generic round/ (a :: <real>, b :: <real>)
    => (quo :: <general-integer>, rem :: <real>);

define inline method round/ (a :: <real>, b :: <real>)
    => (quo :: <integer>, rem :: <real>);
  let quo = round(a / b);
  values(quo, a - quo * b);
end;

define sealed generic truncate/ (a :: <real>, b :: <real>)
    => (quo :: <general-integer>, rem :: <real>);

define inline method truncate/ (a :: <real>, b :: <real>)
    => (quo :: <integer>, rem :: <real>);
  let quo = truncate(a / b);
  values(quo, a - quo * b);
end;

define sealed generic modulo (real1 :: <real>, real2 :: <real>)
    => res :: <real>;

define inline method modulo (real1 :: <real>, real2 :: <real>)
    => res :: <real>;
  let (quo, rem) = floor/(real1, real2);
  rem;
end;

define sealed generic remainder (real1 :: <real>, real2 :: <real>)
    => res :: <real>;

define inline method remainder (real1 :: <real>, real2 :: <real>)
    => res :: <real>;
  let (quo, rem) = truncate/(real1, real2);
  rem;
end;

define inline method abs (num :: <real>)
    => res :: <real>;
  if (negative?(num))
    -num;
  else
    num;
  end;
end;


// Integer methods.

define sealed generic odd? (a :: <general-integer>) => res :: <boolean>;

define inline method odd? (a :: <general-integer>) => res :: <boolean>;
  ~even?(a);
end;

define sealed generic even? (a :: <general-integer>) => res :: <boolean>;

// No default method for even?.

define inline method integral? (a :: <general-integer>) => res :: <boolean>;
  #t;
end;

define inline method floor (a :: <general-integer>)
    => (quo :: <general-integer>, rem :: <general-integer>);
  values(a, 0);
end;

define inline method ceiling (a :: <general-integer>)
    => (quo :: <general-integer>, rem :: <general-integer>);
  values(a, 0);
end;

define inline method round (a :: <general-integer>)
    => (quo :: <general-integer>, rem :: <general-integer>);
  values(a, 0);
end;

define inline method truncate (a :: <general-integer>)
    => (quo :: <general-integer>, rem :: <general-integer>);
  values(a, 0);
end;

define inline method logior (#rest integers)
    => res :: <general-integer>;
  reduce(binary-logior, 0, integers);
end;

define sealed generic binary-logior
    (x :: <general-integer>, y :: <general-integer>)
    => res :: <general-integer>;

define inline method logxor (#rest integers)
    => res :: <general-integer>;
  reduce(binary-logxor, 0, integers);
end;

define sealed generic binary-logxor
    (x :: <general-integer>, y :: <general-integer>)
    => res :: <general-integer>;

define inline method logand (#rest integers)
    => res :: <general-integer>;
  reduce(binary-logand, -1, integers);
end;

define sealed generic binary-logand
    (x :: <general-integer>, y :: <general-integer>)
    => res :: <general-integer>;

define sealed generic lognot (x :: <general-integer>)
    => res :: <general-integer>;

define sealed generic logbit? (index :: <integer>, int :: <general-integer>)
    => res :: <boolean>;

define sealed generic ash (int :: <general-integer>, count :: <integer>)
    => res :: <general-integer>;

define sealed generic lcm (x :: <general-integer>, y :: <general-integer>)
    => res :: <general-integer>;

define method lcm (x :: <general-integer>, y :: <general-integer>)
    => res :: <general-integer>;
  truncate/(max(x, y), gcd(x, y)) * min(x, y);
end;

define sealed generic gcd (x :: <general-integer>, y :: <general-integer>)
    => res :: <general-integer>;

define sealed generic integer-length (x :: <general-integer>)
    => res :: <integer>;


// Fixed Integers.

define functional class <integer> (<general-integer>)
  slot value :: <integer>, init-value: 0;
end;

define sealed method make (class == <integer>, #key)
    => res :: <never-returns>;
  error("Can't make instances of <integer>, they just are.");
end;

// $fixed-integer-bits, $minimum-integer and $maximum-integer.
//
// Note the clever way we compute the second two of these that doesn't
// overflow.  Tricky, huh?
// 
define constant $fixed-integer-bits = 32;
define constant $minimum-integer :: <integer>
  = ash(-1, $fixed-integer-bits - 1);
define constant $maximum-integer :: <integer>
  = lognot($minimum-integer);

define sealed domain as (singleton(<integer>), <complex>);

define inline method \== (a :: <integer>, b :: <integer>)
    => res :: <boolean>;
  %%primitive(fixnum-=, a, b);
end;

define inline method \== (a :: <integer>, b :: <object>)
    => res :: <boolean>;
  #f;
end;

define inline method \< (a :: <integer>, b :: <integer>)
    => res :: <boolean>;
  %%primitive(fixnum-<, a, b);
end;

define inline method even? (a :: <integer>) => res :: <boolean>;
  zero?(logand(a, 1));
end;

define inline method \+ (a :: <integer>, b :: <integer>)
    => res :: <integer>;
  %%primitive(fixnum-+, a, b);
end;

define inline method \* (a :: <integer>, b :: <integer>)
    => res :: <integer>;
  %%primitive(fixnum-*, a, b);
end;

define inline method \- (a :: <integer>, b :: <integer>)
    => res :: <integer>;
  %%primitive(fixnum--, a, b);
end;

define inline method negative (a :: <integer>)
    => res :: <integer>;
  %%primitive(fixnum-negative, a);
end;

// floor/{<integer>,<integer>}
//
// Divide a by b, rounding towards negative infinity.
//
define method floor/ (a :: <integer>, b :: <integer>)
    => (quo :: <integer>, rem :: <integer>);
  //
  // Start with whatever truncate gives us.
  let (q, r) = truncate/(a, b);
  //
  if (zero?(r)) 
    // If we didn't have to round, then we are sitting pretty.
    values(q, r);
    
  elseif (negative?(logxor(r, b)))
    // If the sign of the remainder and the sign of b are not the same,
    // then we rounded up instead of down.  We compare the signs by
    // looking at the sign bit of their exclusive-or.  If it is set (i.e.
    // negative) then the signs (i.e. sign bits) of r and b were different.
    values(q - 1, r + b);

  else
    // Otherwise we rounded in the correct direction.
    values(q, r);
  end if;
end method floor/;

// ceiling/{<integer>,<integer>}
// 
define method ceiling/ (a :: <integer>, b :: <integer>)
    => (quo :: <integer>, rem :: <integer>);
  //
  // Start with whatever truncate gives us.
  let (q, r) = truncate/(a, b);

  if (zero?(r))
    // If we didn't have to round, then we are sitting pretty.
    values(q, r);

  elseif (negative?(logxor(r, b)))
    // If the signs of the remainder and b are the same, then we rounded
    // correctly.
    values(q, r);

  else
    // Otherwise, we rounded down instead of up.
    values(q + 1, r - b);
  end if;
end method ceiling/;

// round/{<integer>,<integer>}
// 
define method round/ (a :: <integer>, b :: <integer>)
    => (quo :: <integer>, rem :: <integer>);
  let (q, r) = truncate/(a, b);

  if (zero?(r))
    values(q, r);
  elseif (positive?(b))
    let limit = ash(b, -1);
    if (r > limit | (r == limit & odd?(q)))
      values(q + 1, r - b);
    elseif (r < -limit | (r == -limit & odd?(q)))
      values(q - 1, r + b);
    else
      values(q, r);
    end;
  else
    let limit = ash(-b, -1);
    if (r > limit | (r == limit & odd?(q)))
      values(q - 1, r + b);
    elseif (r < -limit | (r == -limit & odd?(q)))
      values(q + 1, r - b);
    else
      values(q, r);
    end;
  end;
end method round/;

// truncate/{<integer>,<integer>}
//
// Divide a by b, rounding towards zero.
//
define inline method truncate/
    (a :: <integer>, b :: <integer>)
    => (quo :: <integer>, rem :: <integer>);
  if (zero?(b))
    error("Division by zero.");
  else
    %%primitive(fixnum-divide, a, b);
  end;
end;

define method \^ (base :: <complex>, power :: <integer>)
    => res :: <number>;
  case
    negative?(power) =>
      if (power == $minimum-integer)
	1 / base ^ -(as(<extended-integer>, power));
      else
	1 / base ^ -power;
      end;
    base == 2 =>
      ash(1, power);
    base == #e2 =>
      ash(#e1, power);
    otherwise =>
      for (power :: <integer> = power then ash(power, -1),
	   total = as(object-class(base), 1)
	     then if (odd?(power)) base * total else total end,
	   base = base then base * base,
	   until: zero?(power))
      finally
	total;
      end;
  end;
end;

define inline method binary-logior (a :: <integer>, b :: <integer>)
    => res :: <integer>;
  %%primitive(fixnum-logior, a, b);
end;

define inline method binary-logxor (a :: <integer>, b :: <integer>)
    => res :: <integer>;
  %%primitive(fixnum-logxor, a, b);
end;

define inline method binary-logand (a :: <integer>, b :: <integer>)
    => res :: <integer>;
  %%primitive(fixnum-logand, a, b);
end;

define inline method lognot (a :: <integer>)
    => res :: <integer>;
  %%primitive(fixnum-lognot, a);
end;

define inline method logbit?
    (index :: <integer>, integer :: <integer>)
    => res :: <boolean>;
  odd?(ash(integer, -index));
end;

define inline method ash (integer :: <integer>, count :: <integer>)
    => res :: <integer>;
  if (negative?(count))
    %%primitive(fixnum-shift-right, integer, -count);
  else
    %%primitive(fixnum-shift-left, integer, count);
  end;
end;

// gcd -- exported generic function method
//
// I have no idea why this results in the gcd, but it apparently does.
// But I'll explain my understanding of what it does so that if anyone
// ever wants to try to figure out why it ends up with the gcd, they
// won't have to reconstruct what I just spent a bunch of effort
// trying to figure out.  It is rather twisted.
//
// Rob says that this came from Knuth, so if you want to actually
// understand it, check there.
//
define method gcd (u :: <integer>, v :: <integer>)
    => res :: <integer>;
  if (u == 0)
    v;
  elseif (v == 0)
    u;
  else
    // 
    // The first thing we do is compute the largest power of two both
    // integers are a multiple of.  Basically, this means counting the
    // number of zero bits at the low end of the numbers.  In the
    // process, we divide out this power of two (by shifting the
    // numbers down).
    //
    // The use of odd?(logior(u, v)) is equivalent to odd?(u) |
    // odd?(v) except that it is a bit faster because it only involves
    // one test.
    //
    for (factors-of-two :: <integer> from 0,
	 u :: <integer> = u then ash(u, -1),
	 v :: <integer> = v then ash(v, -1),
	 until: odd?(logior(u, v)))
    finally
      // 
      // Now we make both u and v positive.  We don't just call abs
      // directly, because abs doesn't work on
      // $most-negative-fixed-integer.  So we rely on the fact that
      // $most-neg-fi is even, hence we can shift it down one then
      // negate it.
      //
      let u :: <integer> = abs(if (odd?(u)) u else ash(u, -1) end);
      let v :: <integer> = abs(if (odd?(v)) v else ash(v, -1) end);
      //
      block (return)
	//
	// Basically, we shift u and v down until both are odd.  Then
	// we subtract the smaller from the larger, replacing the
	// larger with the difference.  We stop when u and v become
	// the same.
	//
	// In practice, temp holds either u or -v, whichever one we
	// are working on shifting at the moment.  When we start the
	// loop, one of u or v is odd, so temp gets initialized with
	// the other.  We keep shifting until temp becomes odd.  Now
	// they both are odd.  We can figure out which of u or v we
	// were shifting based on the sign of temp.
	//
	// Okay, so now we have u and v both odd, and we want to
	// subtract the smaller from the larger.  Instead, we just
	// subtract v from u.  If v is the larger, then the result is
	// negative, but when we are working on v, we want temp to
	// hold -v, so that's okay.
	//
	// Once we've done the subtract and (conceptual) replace, the
	// replacement is even (because odd - odd = even) and the
	// non-replaced one of u or v is still odd.  So our loop
	// invarent of either u or v being odd is still true.
	//
	for (temp :: <integer> = if (odd?(u)) -v else ash(u, -1) end
	       then ash(temp, -1))
	  if (odd?(temp))
	    if (positive?(temp))
	      u := temp;
	    else
	      v := -temp;
	    end;
	    temp := u - v;
	    if (zero?(temp))
	      //
	      // Now that we are done, we shift u up by the original
	      // factors-of-two we shifted out.
	      //
	      return(ash(u, factors-of-two));
	    end;
	  end;
	end;
      end;
    end;
  end;
end;

// integer-length(<integer>) -- exported from Extensions.
//
// Return the number of ``interesting'' bits in x.  The interesting bits
// are all but the sign bits.
//
define method integer-length (x :: <integer>) => res :: <integer>;
  local
    method repeat (x :: <integer>, length :: <integer>, bits :: <integer>)
	=> res :: <integer>;
      if (bits.zero?)
	length;
      else
	let shift = ash(bits, -1);
	if (x < ash(1, shift))
	  repeat(x, length, shift);
	else
	  repeat(ash(x, -bits), length + bits, shift);
	end if;
      end if;
    end method repeat;
  repeat(if (x.negative?) lognot(x) else x end, 0, $fixed-integer-bits);
end method integer-length;



// Float methods.

define sealed domain as (singleton(<float>), <complex>);

define inline method as (class == <float>, num :: <float>)
    => res :: <float>;
  num;
end;

define inline method as (class == <float>, num :: <rational>)
    => res :: <float>;
  as(<single-float>, num);
end;


// Single floats.

define functional class <single-float> (<float>)
  slot value :: <single-float>, init-value: 0.0s0;
end;

define sealed method make (class == <single-float>, #key)
    => res :: <never-returns>;
  error("Can't make instances of <single-float>, they just are.");
end;

define sealed domain as (singleton(<single-float>), <complex>);

define inline method as (class == <single-float>, num :: <integer>)
    => res :: <single-float>;
  %%primitive(fixed-as-single, num);
end;

define inline method as (class == <single-float>, num :: <single-float>)
    => res :: <single-float>;
  num;
end;

define inline method as (class == <single-float>, num :: <double-float>)
    => res :: <single-float>;
  %%primitive(double-as-single, num);
end;

define inline method as (class == <single-float>, num :: <extended-float>)
    => res :: <single-float>;
  %%primitive(extended-as-single, num);
end;

define inline method \== (a :: <single-float>, b :: <single-float>)
    => res :: <boolean>;
  %%primitive(single-==, a, b);
end;

define inline method \== (a :: <single-float>, b :: <object>)
    => res :: <boolean>;
  #f;
end;

define inline method \= (a :: <single-float>, b :: <single-float>)
    => res :: <boolean>;
  %%primitive(single-=, a, b);
end;

define inline method \= (a :: <single-float>, b :: <integer>)
    => res :: <boolean>;
  a = as(<single-float>, b);
end;

define inline method \= (a :: <integer>, b :: <single-float>)
    => res :: <boolean>;
  as(<single-float>, a) = b;
end;

define inline method \< (a :: <single-float>, b :: <single-float>)
    => res :: <boolean>;
  %%primitive(single-<, a, b);
end;

define inline method \< (a :: <single-float>, b :: <integer>)
    => res :: <boolean>;
  a < as(<single-float>, b);
end;

define inline method \< (a :: <integer>, b :: <single-float>)
    => res :: <boolean>;
  as(<single-float>, a) < b;
end;

define inline method \<= (a :: <single-float>, b :: <single-float>)
    => res :: <boolean>;
  %%primitive(single-<=, a, b);
end;

define inline method \<= (a :: <single-float>, b :: <integer>)
    => res :: <boolean>;
  a <= as(<single-float>, b);
end;

define inline method \<= (a :: <integer>, b :: <single-float>)
    => res :: <boolean>;
  as(<single-float>, a) <= b;
end;

define inline method \~= (a :: <single-float>, b :: <single-float>)
    => res :: <boolean>;
  %%primitive(single-~=, a, b);
end;

define inline method \~= (a :: <single-float>, b :: <integer>)
    => res :: <boolean>;
  a ~= as(<single-float>, b);
end;

define inline method \~= (a :: <integer>, b :: <single-float>)
    => res :: <boolean>;
  as(<single-float>, a) ~= b;
end;

define inline method \+ (a :: <single-float>, b :: <single-float>)
    => res :: <single-float>;
  %%primitive(single-+, a, b);
end;

define inline method \+ (a :: <single-float>, b :: <integer>)
    => res :: <single-float>;
  a + as(<single-float>, b);
end;

define inline method \+ (a :: <integer>, b :: <single-float>)
    => res :: <single-float>;
  as(<single-float>, a) + b;
end;

define inline method \* (a :: <single-float>, b :: <single-float>)
    => res :: <single-float>;
  %%primitive(single-*, a, b);
end;

define inline method \* (a :: <single-float>, b :: <integer>)
    => res :: <single-float>;
  a * as(<single-float>, b);
end;

define inline method \* (a :: <integer>, b :: <single-float>)
    => res :: <single-float>;
  as(<single-float>, a) * b;
end;

define inline method \- (a :: <single-float>, b :: <single-float>)
    => res :: <single-float>;
  %%primitive(single--, a, b);
end;

define inline method \- (a :: <single-float>, b :: <integer>)
    => res :: <single-float>;
  a - as(<single-float>, b);
end;

define inline method \- (a :: <integer>, b :: <single-float>)
    => res :: <single-float>;
  as(<single-float>, a) - b;
end;

define inline method \/ (a :: <single-float>, b :: <single-float>)
    => res :: <single-float>;
  %%primitive(single-/, a, b);
end;

define inline method \/ (a :: <single-float>, b :: <integer>)
    => res :: <single-float>;
  a / as(<single-float>, b);
end;

define inline method \/ (a :: <integer>, b :: <single-float>)
    => res :: <single-float>;
  as(<single-float>, a) / b;
end;

define inline method negative (a :: <single-float>)
    => res :: <single-float>;
  let quo = %%primitive(single-negative, a);
  values(quo, a - quo);
end;

define inline method floor (a :: <single-float>)
    => (quo :: <integer>, rem :: <single-float>);
  let quo = %%primitive(single-floor, a);
  values(quo, a - quo);
end;

define inline method ceiling (a :: <single-float>)
    => (quo :: <integer>, rem :: <single-float>);
  let quo = %%primitive(single-ceiling, a);
  values(quo, a - quo);
end;

define inline method round (a :: <single-float>)
    => (quo :: <integer>, rem :: <single-float>);
  let quo = %%primitive(single-round, a);
  values(quo, a - quo);
end;

define inline method truncate (a :: <single-float>)
    => (quo :: <integer>, rem :: <single-float>);
  let quo = if (negative?(a))
	      %%primitive(single-ceiling, a);
	    else
	      %%primitive(single-floor, a);
	    end;
  values(quo, a - quo);
end;

define inline method abs (a :: <single-float>)
    => abs :: <single-float>;
  %%primitive(single-abs, a);
end;


// Double floats.

define functional class <double-float> (<float>)
  slot value :: <double-float>, init-value: 0.0d0;
end;

define sealed method make (class == <double-float>, #key)
    => res :: <never-returns>;
  error("Can't make instances of <double-float>, they just are.");
end;

define sealed domain as (singleton(<double-float>), <complex>);

define inline method as (class == <double-float>, num :: <integer>)
    => res :: <double-float>;
  %%primitive(fixed-as-double, num);
end;

define inline method as (class == <double-float>, num :: <single-float>)
    => res :: <double-float>;
  %%primitive(single-as-double, num);
end;

define inline method as (class == <double-float>, num :: <double-float>)
    => res :: <double-float>;
  num;
end;

define inline method as (class == <double-float>, num :: <extended-float>)
    => res :: <double-float>;
  %%primitive(extended-as-double, num);
end;

define inline method \== (a :: <double-float>, b :: <double-float>)
    => res :: <boolean>;
  %%primitive(double-==, a, b);
end;

define inline method \== (a :: <double-float>, b :: <object>)
    => res :: <boolean>;
  #f;
end;

define inline method \= (a :: <double-float>, b :: <double-float>)
    => res :: <boolean>;
  %%primitive(double-=, a, b);
end;

define inline method \= (a :: <double-float>, b :: <integer>)
    => res :: <boolean>;
  a = as(<double-float>, b);
end;

define inline method \= (a :: <integer>, b :: <double-float>)
    => res :: <boolean>;
  as(<double-float>, a) = b;
end;

define inline method \= (a :: <double-float>, b :: <single-float>)
    => res :: <boolean>;
  a = as(<double-float>, b);
end;

define inline method \= (a :: <single-float>, b :: <double-float>)
    => res :: <boolean>;
  as(<double-float>, a) = b;
end;

define inline method \< (a :: <double-float>, b :: <double-float>)
    => res :: <boolean>;
  %%primitive(double-<, a, b);
end;

define inline method \< (a :: <double-float>, b :: <integer>)
    => res :: <boolean>;
  a < as(<double-float>, b);
end;

define inline method \< (a :: <integer>, b :: <double-float>)
    => res :: <boolean>;
  as(<double-float>, a) < b;
end;

define inline method \< (a :: <double-float>, b :: <single-float>)
    => res :: <boolean>;
  a < as(<double-float>, b);
end;

define inline method \< (a :: <single-float>, b :: <double-float>)
    => res :: <boolean>;
  as(<double-float>, a) < b;
end;

define inline method \<= (a :: <double-float>, b :: <double-float>)
    => res :: <boolean>;
  %%primitive(double-<=, a, b);
end;

define inline method \<= (a :: <double-float>, b :: <integer>)
    => res :: <boolean>;
  a <= as(<double-float>, b);
end;

define inline method \<= (a :: <integer>, b :: <double-float>)
    => res :: <boolean>;
  as(<double-float>, a) <= b;
end;

define inline method \<= (a :: <double-float>, b :: <single-float>)
    => res :: <boolean>;
  a <= as(<double-float>, b);
end;

define inline method \<= (a :: <single-float>, b :: <double-float>)
    => res :: <boolean>;
  as(<double-float>, a) <= b;
end;

define inline method \~= (a :: <double-float>, b :: <double-float>)
    => res :: <boolean>;
  %%primitive(double-~=, a, b);
end;

define inline method \~= (a :: <double-float>, b :: <integer>)
    => res :: <boolean>;
  a ~= as(<double-float>, b);
end;

define inline method \~= (a :: <integer>, b :: <double-float>)
    => res :: <boolean>;
  as(<double-float>, a) ~= b;
end;

define inline method \~= (a :: <double-float>, b :: <single-float>)
    => res :: <boolean>;
  a ~= as(<double-float>, b);
end;

define inline method \~= (a :: <single-float>, b :: <double-float>)
    => res :: <boolean>;
  as(<double-float>, a) ~= b;
end;

define inline method \+ (a :: <double-float>, b :: <double-float>)
    => res :: <double-float>;
  %%primitive(double-+, a, b);
end;

define inline method \+ (a :: <double-float>, b :: <integer>)
    => res :: <double-float>;
  a + as(<double-float>, b);
end;

define inline method \+ (a :: <integer>, b :: <double-float>)
    => res :: <double-float>;
  as(<double-float>, a) + b;
end;

define inline method \+ (a :: <double-float>, b :: <single-float>)
    => res :: <double-float>;
  a + as(<double-float>, b);
end;

define inline method \+ (a :: <single-float>, b :: <double-float>)
    => res :: <double-float>;
  as(<double-float>, a) + b;
end;

define inline method \* (a :: <double-float>, b :: <double-float>)
    => res :: <double-float>;
  %%primitive(double-*, a, b);
end;

define inline method \* (a :: <double-float>, b :: <integer>)
    => res :: <double-float>;
  a * as(<double-float>, b);
end;

define inline method \* (a :: <integer>, b :: <double-float>)
    => res :: <double-float>;
  as(<double-float>, a) * b;
end;

define inline method \* (a :: <double-float>, b :: <single-float>)
    => res :: <double-float>;
  a * as(<double-float>, b);
end;

define inline method \* (a :: <single-float>, b :: <double-float>)
    => res :: <double-float>;
  as(<double-float>, a) * b;
end;

define inline method \- (a :: <double-float>, b :: <double-float>)
    => res :: <double-float>;
  %%primitive(double--, a, b);
end;

define inline method \- (a :: <double-float>, b :: <integer>)
    => res :: <double-float>;
  a - as(<double-float>, b);
end;

define inline method \- (a :: <integer>, b :: <double-float>)
    => res :: <double-float>;
  as(<double-float>, a) - b;
end;

define inline method \- (a :: <double-float>, b :: <single-float>)
    => res :: <double-float>;
  a - as(<double-float>, b);
end;

define inline method \- (a :: <single-float>, b :: <double-float>)
    => res :: <double-float>;
  as(<double-float>, a) - b;
end;

define inline method \/ (a :: <double-float>, b :: <double-float>)
    => res :: <double-float>;
  %%primitive(double-/, a, b);
end;

define inline method \/ (a :: <double-float>, b :: <integer>)
    => res :: <double-float>;
  a / as(<double-float>, b);
end;

define inline method \/ (a :: <integer>, b :: <double-float>)
    => res :: <double-float>;
  as(<double-float>, a) / b;
end;

define inline method \/ (a :: <double-float>, b :: <single-float>)
    => res :: <double-float>;
  a / as(<double-float>, b);
end;

define inline method \/ (a :: <single-float>, b :: <double-float>)
    => res :: <double-float>;
  as(<double-float>, a) / b;
end;

define inline method negative (a :: <double-float>)
    => res :: <double-float>;
  %%primitive(double-negative, a);
end;

define inline method floor (a :: <double-float>)
    => (quo :: <integer>, rem :: <double-float>);
  let quo = %%primitive(double-floor, a);
  values(quo, a - quo);
end;

define inline method ceiling (a :: <double-float>)
    => (quo :: <integer>, rem :: <double-float>);
  let quo = %%primitive(double-ceiling, a);
  values(quo, a - quo);
end;

define inline method round (a :: <double-float>)
    => (quo :: <integer>, rem :: <double-float>);
  let quo = %%primitive(double-round, a);
  values(quo, a - quo);
end;

define inline method truncate (a :: <double-float>)
    => (quo :: <integer>, rem :: <double-float>);
  let quo = if (negative?(a))
	      %%primitive(double-ceiling, a);
	    else
	      %%primitive(double-floor, a);
	    end;
  values(quo, a - quo);
end;

define inline method abs (a :: <double-float>)
    => abs :: <double-float>;
  %%primitive(double-abs, a);
end;


// Extended floats.

define functional class <extended-float> (<float>)
  slot value :: <extended-float>, init-value: 0.0x0;
end;

define sealed method make (class == <extended-float>, #key)
    => res :: <never-returns>;
  error("Can't make instances of <extended-float>, they just are.");
end;

define sealed domain as (singleton(<extended-float>), <complex>);

define inline method as (class == <extended-float>, num :: <integer>)
    => res :: <extended-float>;
  %%primitive(fixed-as-extended, num);
end;

define inline method as (class == <extended-float>, num :: <single-float>)
    => res :: <extended-float>;
  %%primitive(single-as-extended, num);
end;

define inline method as (class == <extended-float>, num :: <double-float>)
    => res :: <extended-float>;
  %%primitive(double-as-extended, num);
end;

define inline method as (class == <extended-float>, num :: <extended-float>)
    => res :: <extended-float>;
  num;
end;

define inline method \== (a :: <extended-float>, b :: <extended-float>)
    => res :: <boolean>;
  %%primitive(extended-==, a, b);
end;

define inline method \== (a :: <extended-float>, b :: <object>)
    => res :: <boolean>;
  #f;
end;

define inline method \= (a :: <extended-float>, b :: <extended-float>)
    => res :: <boolean>;
  %%primitive(extended-=, a, b);
end;

define inline method \= (a :: <extended-float>, b :: <integer>)
    => res :: <boolean>;
  a = as(<extended-float>, b);
end;

define inline method \= (a :: <integer>, b :: <extended-float>)
    => res :: <boolean>;
  as(<extended-float>, a) = b;
end;

define inline method \= (a :: <extended-float>, b :: <single-float>)
    => res :: <boolean>;
  a = as(<extended-float>, b);
end;

define inline method \= (a :: <single-float>, b :: <extended-float>)
    => res :: <boolean>;
  as(<extended-float>, a) = b;
end;

define inline method \= (a :: <extended-float>, b :: <double-float>)
    => res :: <boolean>;
  a = as(<extended-float>, b);
end;

define inline method \= (a :: <double-float>, b :: <extended-float>)
    => res :: <boolean>;
  as(<extended-float>, a) = b;
end;

define inline method \< (a :: <extended-float>, b :: <extended-float>)
    => res :: <boolean>;
  %%primitive(extended-<, a, b);
end;

define inline method \< (a :: <extended-float>, b :: <integer>)
    => res :: <boolean>;
  a < as(<extended-float>, b);
end;

define inline method \< (a :: <integer>, b :: <extended-float>)
    => res :: <boolean>;
  as(<extended-float>, a) < b;
end;

define inline method \< (a :: <extended-float>, b :: <single-float>)
    => res :: <boolean>;
  a < as(<extended-float>, b);
end;

define inline method \< (a :: <single-float>, b :: <extended-float>)
    => res :: <boolean>;
  as(<extended-float>, a) < b;
end;

define inline method \< (a :: <extended-float>, b :: <double-float>)
    => res :: <boolean>;
  a < as(<extended-float>, b);
end;

define inline method \< (a :: <double-float>, b :: <extended-float>)
    => res :: <boolean>;
  as(<extended-float>, a) < b;
end;

define inline method \<= (a :: <extended-float>, b :: <extended-float>)
    => res :: <boolean>;
  %%primitive(extended-<=, a, b);
end;

define inline method \<= (a :: <extended-float>, b :: <integer>)
    => res :: <boolean>;
  a <= as(<extended-float>, b);
end;

define inline method \<= (a :: <integer>, b :: <extended-float>)
    => res :: <boolean>;
  as(<extended-float>, a) <= b;
end;

define inline method \<= (a :: <extended-float>, b :: <single-float>)
    => res :: <boolean>;
  a <= as(<extended-float>, b);
end;

define inline method \<= (a :: <single-float>, b :: <extended-float>)
    => res :: <boolean>;
  as(<extended-float>, a) <= b;
end;

define inline method \<= (a :: <extended-float>, b :: <double-float>)
    => res :: <boolean>;
  a <= as(<extended-float>, b);
end;

define inline method \<= (a :: <double-float>, b :: <extended-float>)
    => res :: <boolean>;
  as(<extended-float>, a) <= b;
end;

define inline method \~= (a :: <extended-float>, b :: <extended-float>)
    => res :: <boolean>;
  %%primitive(extended-~=, a, b);
end;

define inline method \~= (a :: <extended-float>, b :: <integer>)
    => res :: <boolean>;
  a ~= as(<extended-float>, b);
end;

define inline method \~= (a :: <integer>, b :: <extended-float>)
    => res :: <boolean>;
  as(<extended-float>, a) ~= b;
end;

define inline method \~= (a :: <extended-float>, b :: <single-float>)
    => res :: <boolean>;
  a ~= as(<extended-float>, b);
end;

define inline method \~= (a :: <single-float>, b :: <extended-float>)
    => res :: <boolean>;
  as(<extended-float>, a) ~= b;
end;

define inline method \~= (a :: <extended-float>, b :: <double-float>)
    => res :: <boolean>;
  a ~= as(<extended-float>, b);
end;

define inline method \~= (a :: <double-float>, b :: <extended-float>)
    => res :: <boolean>;
  as(<extended-float>, a) ~= b;
end;

define inline method \+ (a :: <extended-float>, b :: <extended-float>)
    => res :: <extended-float>;
  %%primitive(extended-+, a, b);
end;

define inline method \+ (a :: <extended-float>, b :: <integer>)
    => res :: <extended-float>;
  a + as(<extended-float>, b);
end;

define inline method \+ (a :: <integer>, b :: <extended-float>)
    => res :: <extended-float>;
  as(<extended-float>, a) + b;
end;

define inline method \+ (a :: <extended-float>, b :: <single-float>)
    => res :: <extended-float>;
  a + as(<extended-float>, b);
end;

define inline method \+ (a :: <single-float>, b :: <extended-float>)
    => res :: <extended-float>;
  as(<extended-float>, a) + b;
end;

define inline method \+ (a :: <extended-float>, b :: <double-float>)
    => res :: <extended-float>;
  a + as(<extended-float>, b);
end;

define inline method \+ (a :: <double-float>, b :: <extended-float>)
    => res :: <extended-float>;
  as(<extended-float>, a) + b;
end;

define inline method \* (a :: <extended-float>, b :: <extended-float>)
    => res :: <extended-float>;
  %%primitive(extended-*, a, b);
end;

define inline method \* (a :: <extended-float>, b :: <integer>)
    => res :: <extended-float>;
  a * as(<extended-float>, b);
end;

define inline method \* (a :: <integer>, b :: <extended-float>)
    => res :: <extended-float>;
  as(<extended-float>, a) * b;
end;

define inline method \* (a :: <extended-float>, b :: <single-float>)
    => res :: <extended-float>;
  a * as(<extended-float>, b);
end;

define inline method \* (a :: <single-float>, b :: <extended-float>)
    => res :: <extended-float>;
  as(<extended-float>, a) * b;
end;

define inline method \* (a :: <extended-float>, b :: <double-float>)
    => res :: <extended-float>;
  a * as(<extended-float>, b);
end;

define inline method \* (a :: <double-float>, b :: <extended-float>)
    => res :: <extended-float>;
  as(<extended-float>, a) * b;
end;

define inline method \- (a :: <extended-float>, b :: <extended-float>)
    => res :: <extended-float>;
  %%primitive(extended--, a, b);
end;

define inline method \- (a :: <extended-float>, b :: <integer>)
    => res :: <extended-float>;
  a - as(<extended-float>, b);
end;

define inline method \- (a :: <integer>, b :: <extended-float>)
    => res :: <extended-float>;
  as(<extended-float>, a) - b;
end;

define inline method \- (a :: <extended-float>, b :: <single-float>)
    => res :: <extended-float>;
  a - as(<extended-float>, b);
end;

define inline method \- (a :: <single-float>, b :: <extended-float>)
    => res :: <extended-float>;
  as(<extended-float>, a) - b;
end;

define inline method \- (a :: <extended-float>, b :: <double-float>)
    => res :: <extended-float>;
  a - as(<extended-float>, b);
end;

define inline method \- (a :: <double-float>, b :: <extended-float>)
    => res :: <extended-float>;
  as(<extended-float>, a) - b;
end;

define inline method \/ (a :: <extended-float>, b :: <extended-float>)
    => res :: <extended-float>;
  %%primitive(extended-/, a, b);
end;

define inline method \/ (a :: <extended-float>, b :: <integer>)
    => res :: <extended-float>;
  a / as(<extended-float>, b);
end;

define inline method \/ (a :: <integer>, b :: <extended-float>)
    => res :: <extended-float>;
  as(<extended-float>, a) / b;
end;

define inline method \/ (a :: <extended-float>, b :: <single-float>)
    => res :: <extended-float>;
  a / as(<extended-float>, b);
end;

define inline method \/ (a :: <single-float>, b :: <extended-float>)
    => res :: <extended-float>;
  as(<extended-float>, a) / b;
end;

define inline method \/ (a :: <extended-float>, b :: <double-float>)
    => res :: <extended-float>;
  a / as(<extended-float>, b);
end;

define inline method \/ (a :: <double-float>, b :: <extended-float>)
    => res :: <extended-float>;
  as(<extended-float>, a) / b;
end;

define inline method negative (a :: <extended-float>)
    => res :: <extended-float>;
  %%primitive(extended-negative, a);
end;

define inline method floor (a :: <extended-float>)
    => (quo :: <integer>, rem :: <extended-float>);
  let quo = %%primitive(extended-floor, a);
  values(quo, a - quo);
end;

define inline method ceiling (a :: <extended-float>)
    => (quo :: <integer>, rem :: <extended-float>);
  let quo = %%primitive(extended-ceiling, a);
  values(quo, a - quo);
end;

define inline method round (a :: <extended-float>)
    => (quo :: <integer>, rem :: <extended-float>);
  let quo = %%primitive(extended-round, a);
  values(quo, a - quo);
end;

define inline method truncate (a :: <extended-float>)
    => (quo :: <integer>, rem :: <extended-float>);
  let quo = if (negative?(a))
	      %%primitive(extended-ceiling, a);
	    else
	      %%primitive(extended-floor, a);
	    end;
  values(quo, a - quo);
end;

define inline method abs (a :: <extended-float>)
    => abs :: <extended-float>;
  %%primitive(extended-abs, a);
end;
