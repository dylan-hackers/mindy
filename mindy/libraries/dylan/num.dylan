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
//  $Header: /home/housel/work/rcs/gd/src/mindy/libraries/dylan/num.dylan,v 1.3 1994/04/20 02:38:43 rgs Exp $
//
//  This file does whatever.
//


// Predicates.

define method odd? (x :: <integer>)
  logbit?(x, 0);
end;

define method even? (x :: <integer>)
  ~logbit?(x, 0);
end;

define method zero? (x :: <number>)
  x = 0;
end;

define method positive? (x :: <real>)
  x > 0;
end;

define method negative? (x :: <real>)
  x < 0;
end;

define method integral? (x :: <integer>)
  #t
end;

define method integral? (x :: <number>)
  #f
end;


// Contagion.

define method contagion (x :: <integer>, y :: <single-float>)
  values(as(x, <single-float>), y);
end;

define method contagion (x :: <single-float>, y :: <integer>)
  values(x, as(<single-float>, y));
end;

define method contagion (x :: <integer>, y :: <double-float>)
  values(as(<double-float>, x), y);
end;

define method contagion (x :: <double-float>, y :: <integer>)
  values(x, as(<double-float>, y));
end;

define method contagion (x :: <single-float>, y :: <double-float>)
  values(as(<double-float>, x), y);
end;

define method contagion (x :: <double-float>, y :: <single-float>)
  values(x, as(<double-float>, y));
end;



// Additional methods for +, etc.

define method \+ (x :: <number>, y :: <number>)
  let (x, y) = contagion(x, y);
  x + y;
end;

define method \- (x :: <number>, y :: <number>)
  let (x, y) = contagion(x, y);
  x - y;
end;

define method \* (x :: <number>, y :: <number>)
  let (x, y) = contagion(x, y);
  x * y;
end;

define method \/ (x :: <float>, y :: <float>)
  let (x, y) = contagion(x, y);
  x / y;
end;

define method floor (x :: <real>)
  floor/(x, 1);
end;

define method ceiling (x :: <real>)
  ceiling/(x, 1);
end;

define method round (x :: <real>)
  round/(x, 1);
end;

define method truncate (x :: <real>)
  truncate/(x, 1);
end;

define method modulo (x :: <real>, y :: <real>)
  let (quo, rem) = floor/(x, y);
  rem;
end;

define method remainder (x :: <real>, y :: <real>)
  let (quo, rem) = truncate/(x, y);
  rem;
end;


// Other routines.

define method abs (real :: <real>)
  if (negative?(real))
    -real;
  else
    real;
  end;
end;

define method expt (base :: <integer>, power :: <integer>)
  case
    negative?(power)
      => 1 / expt(base, -power);
    base == 2
      => ash(1, power);
    otherwise
      for (power = power then ash(power, -1),
	   total = 1 then if (odd?(power)) base * total else total end,
	   base = base then base * base,
	   until zero?(power))
      finally
	total;
      end;
  end;
end;

define method min (x :: <real>, #rest more)
  select (size(more))
    0 => x;
    1 =>
      let y = first(more);
      if (y < x) y else x end if;
    otherwise =>
      for (y in more,
	   result = x then if (y < result) y else result end)
      finally result;
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

define method gcd (u :: <integer>, v :: <integer>)
  case
    zero?(u) => v;
    zero?(v) => u;
    otherwise
      for (k from 0,
	   u = abs(u) then ash(u, -1),
	   v = abs(v) then ash(v, -1),
	   until odd?(logior(u, v)))
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

define method lcm (n :: <integer>, m :: <integer>)
  truncate/(max(n, m), gcd(n, m)) * min(n, m);
end;

