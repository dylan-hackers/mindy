module: Dylan
rcs-header: $Header: /scm/cvs/src/mindy/libraries/dylan/num.dylan,v 1.1 1998/05/03 19:55:21 andreas Exp $

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
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
//  This file contains the support for numbers that isn't built in.
//


// Predicates.

define open generic zero? (object :: <object>) => res :: <boolean>;
define open generic positive? (num :: <object>) => res :: <boolean>;
define open generic negative? (num :: <object>) => res :: <boolean>;
define open generic integral? (num :: <object>) => res :: <boolean>;

define method odd? (x :: <general-integer>) => answer :: <boolean>;
  logbit?(0, x);
end;

define method even? (x :: <general-integer>) => answer :: <boolean>;
  ~logbit?(0, x);
end;

define method zero? (x :: <number>) => answer :: <boolean>;
  x = 0;
end;

define method positive? (x :: <real>) => answer :: <boolean>;
  x > 0;
end;

define method negative? (x :: <real>) => answer :: <boolean>;
  x < 0;
end;

define method integral? (x :: <general-integer>) => answer :: <boolean>;
  #t
end;

define method integral? (x :: <number>) => answer :: <boolean>;
  #f
end;


// Contagion.

define method combine-contagion (x :: <integer>, y :: <extended-integer>)
  values (as (<extended-integer>, x), y);
end method;

define method combine-contagion (x :: <extended-integer>, y :: <integer>)
  values (x, as (<extended-integer>, y));
end method;

define method combine-contagion (x :: <rational>, y :: <single-float>)
  values (as (<single-float>, x), y);
end;

define method combine-contagion (x :: <single-float>, y :: <rational>)
  values (x, as (<single-float>, y));
end;

define method combine-contagion (x :: <rational>, y :: <double-float>)
  values (as (<double-float>, x), y);
end;

define method combine-contagion (x :: <double-float>, y :: <rational>)
  values (x, as (<double-float>, y));
end;

define method combine-contagion (x :: <rational>, y :: <extended-float>)
  values (as (<extended-float>, x), y);
end;

define method combine-contagion (x :: <extended-float>, y :: <rational>)
  values (x, as (<extended-float>, y));
end;

define method combine-contagion (x :: <single-float>, y :: <double-float>)
  values (as (<double-float>, x), y);
end;

define method combine-contagion (x :: <double-float>, y :: <single-float>)
  values (x, as (<double-float>, y));
end;

define method combine-contagion (x :: <single-float>, y :: <extended-float>)
  values (as (<extended-float>, x), y);
end;

define method combine-contagion (x :: <extended-float>, y :: <single-float>)
  values (x, as (<extended-float>, y));
end;

define method combine-contagion (x :: <double-float>, y :: <extended-float>)
  values (as (<extended-float>, x), y);
end;

define method combine-contagion (x :: <extended-float>, y :: <double-float>)
  values (x, as (<extended-float>, y));
end;

define method compare-contagion (x :: <real>, y :: <real>)
   combine-contagion (x, y);
end method;



// Additional methods for +, etc.

define method \+ (x :: <number>, y :: <number>)
  let (x, y) = combine-contagion(x, y);
  x + y;
end;

define method \- (x :: <number>, y :: <number>)
  let (x, y) = combine-contagion(x, y);
  x - y;
end;

define method \* (x :: <number>, y :: <number>)
  let (x, y) = combine-contagion(x, y);
  x * y;
end;

define method \/ (x :: <real>, y :: <float>)
  let (x, y) = combine-contagion(x, y);
  x / y;
end;

define method \/ (x :: <float>, y :: <rational>)
  let (x, y) = combine-contagion(x, y);
  x / y;
end;

define method truncate (x :: <general-integer>)
      => (q :: <general-integer>, r :: <integer>);
  truncate/ (x, 1);
end;

define method floor (x :: <general-integer>)
      => (q :: <general-integer>, r :: <integer>);
  floor/ (x, 1);
end;

define method ceiling (x :: <general-integer>)
      => (q :: <general-integer>, r :: <integer>);
  ceiling/ (x, 1);
end;

define method round (x :: <general-integer>)
      => (q :: <general-integer>, r :: <integer>);
  round/ (x, 1);
end;

define method floor/ (x :: <extended-integer>, y :: <integer>)
    => (q :: <extended-integer>, r :: <integer>);
  let (q, r) = floor/ (x, as(<extended-integer>, y));
  values(q, as(<integer>, r));
end;

define method floor/ (x :: <integer>, y :: <extended-integer>)
    => (q :: <extended-integer>, r :: <extended-integer>);
  floor/ (as(<extended-integer>, x), y);
end;

define method floor/ (x :: <real>, y :: <real>)
    => (q :: <general-integer>, r :: <real>);
  let res = floor (x / y);
  values (res, x - res * y);
end;

define method ceiling/ (x :: <extended-integer>, y :: <integer>)
    => (q :: <extended-integer>, r :: <integer>);
  let (q, r) = ceiling/ (x, as(<extended-integer>, y));
  values(q, as(<integer>, r));
end;

define method ceiling/ (x :: <integer>, y :: <extended-integer>)
    => (q :: <extended-integer>, r :: <extended-integer>);
  ceiling/ (as(<extended-integer>, x), y);
end;

define method ceiling/ (x :: <real>, y :: <real>)
    => (q :: <general-integer>, r :: <real>);
  let res = ceiling (x / y);
  values (res, x - res * y);
end;

define method round/ (x :: <extended-integer>, y :: <integer>)
    => (q :: <extended-integer>, r :: <integer>);
  let (q, r) = round/ (x, as(<extended-integer>, y));
  values(q, as(<integer>, r));
end;

define method round/ (x :: <integer>, y :: <extended-integer>)
    => (q :: <extended-integer>, r :: <extended-integer>);
  round/ (as(<extended-integer>, x), y);
end;

define method round/ (x :: <real>, y :: <real>)
    => (q :: <general-integer>, r :: <real>);
  let res = round (x / y);
  values (res, x - res * y);
end;

define method truncate/ (x :: <extended-integer>, y :: <integer>)
    => (q :: <extended-integer>, r :: <integer>);
  let (q, r) = truncate/ (x, as(<extended-integer>, y));
  values(q, as(<integer>, r));
end;

define method truncate/ (x :: <integer>, y :: <extended-integer>)
    => (q :: <extended-integer>, r :: <extended-integer>);
  truncate/ (as(<extended-integer>, x), y);
end;

define method truncate/ (x :: <real>, y :: <real>)
    => (q :: <general-integer>, r :: <real>);
  let res = truncate (x / y);
  values(res, x - res * y);
end;

define method modulo (x :: <real>, y :: <real>)
  let (quo, rem) = floor/(x, y);
  rem;
end;

define method remainder (x :: <real>, y :: <real>)
  let (quo, rem) = truncate/(x, y);
  rem;
end;

define method binary-logand (x :: <general-integer>, y :: <general-integer>)
  let (x, y) = combine-contagion(x, y);
  binary-logand(x, y);
end;

define method binary-logior (x :: <general-integer>, y :: <general-integer>)
  let (x, y) = combine-contagion(x, y);
  binary-logior(x, y);
end;

define method binary-logxor (x :: <general-integer>, y :: <general-integer>)
  let (x, y) = combine-contagion(x, y);
  binary-logxor(x, y);
end;

define method \= (x :: <real>, y :: <real>) => answer :: <boolean>;
  let (x, y) = compare-contagion(x, y);
  x = y;
end;

define method \< (x :: <real>, y :: <real>) => answer :: <boolean>;
  let (x, y) = compare-contagion(x, y);
  x < y;
end;

define method \<= (x :: <real>, y :: <float>) => answer :: <boolean>;
  let (x, y) = compare-contagion(x, y);
  x <= y;
end;

define method \<= (x :: <float>, y :: <real>) => answer :: <boolean>;
  let (x, y) = compare-contagion(x, y);
  x <= y;
end;  



// Other routines.

define method \^ (base :: <number>, power :: <general-integer>)
 => result :: <number>;
  case
    negative? (power) =>
      1 / (base ^ (- power));
    base == 2 =>
      ash (1, power);
    otherwise =>
      for (power = power then ash (power, -1),
	   total = 1 then if (odd? (power)) base * total else total end,
	   base = base then base * base,
	   until: zero? (power))
      finally
	total;
      end;
  end;
end;

define method abs (real :: <real>)
  if (negative?(real))
    -real;
  else
    real;
  end;
end;

define method logior (#rest integers)
  if (integers.size == 2)
    // The binary case is really the only one that needs to be optimized,
    // since it is one of the easiest, and by far the most common.
    apply(binary-logior, integers);
  else
    reduce(binary-logior, 0, integers);
  end if;
end;

define method logxor (#rest integers)
  if (integers.size == 2)
    // The binary case is really the only one that needs to be optimized,
    // since it is one of the easiest, and by far the most common.
    apply(binary-logxor, integers);
  else
    reduce(binary-logxor, 0, integers);
  end if;
end;

define method logand (#rest integers)
  if (integers.size == 2)
    // The binary case is really the only one that needs to be optimized,
    // since it is one of the easiest, and by far the most common.
    apply(binary-logand, integers);
  else
    reduce(binary-logand, -1, integers);
  end if;
end;

define method lcm (n :: <general-integer>, m :: <general-integer>)
  truncate/(max(n, m), gcd(n, m)) * min(n, m);
end;

define method gcd (u :: <general-integer>, v :: <general-integer>)
  case
    zero?(u) => v;
    zero?(v) => u;
    otherwise
      for (k from 0,
	   u = abs(u) then ash(u, -1),
	   v = abs(v) then ash(v, -1),
	   until: odd?(logior(u, v)))
      finally
	block (return)
	  for (temp = if (odd?(u)) -v else ash(u, -1) end
		 then ash(temp, -1))
	    if (odd?(temp))
	      if (positive?(temp))
		u := temp;
	      else
		v := -temp;
	      end;
	      temp := u - v;
	      if (zero?(temp))
		return(ash(u, k));
	      end;
	    end if;
	  end for;
	end block;
      end for;
  end case;
end gcd;

define method min (x :: <real>, #rest more)
  select (size(more))
    0 => x;
    1 =>
      let y = first(more);
      if (y < x) y else x end if;
    otherwise =>
      for (y in more,
	   result = x then if (y < result) y else result end)
      finally
	result;
      end;
  end select;
end;

define method max (x :: <real>, #rest more)
  select (size(more))
    0 => x;
    1 =>
      let y = first(more);
      if (y > x) y else x end if;
    otherwise =>
      for (y in more,
	   result = x then if (y > result) y else result end)
      finally result;
      end;
  end select;
end;


// integer-length -- exported from Extensions.
//
// Return the number of ``interesting'' bits in x.  The interesting bits
// are all but the sign bits.
//
define generic integer-length (x :: <general-integer>) => res :: <integer>;
//
define method integer-length (x :: <integer>) => res :: <integer>;
  for (x = if (x < 0) lognot(x) else x end
	 then ash(x, -1),
       length from 0,
       until: x == 0)
  finally
    length;
  end for;
end method integer-length;
//
define method integer-length (x :: <extended-integer>) => res :: <integer>;
  for (x = if (x < 0) lognot(x) else x end then ash(x, -16),
       length from 0 by 16,
       until: x < 65536)
  finally
    for (x = as(<integer>, x) then ash(x, -1),
	 length from length,
	 until: x == 0)
    finally
      length;
    end for;
  end for;
end method integer-length;
