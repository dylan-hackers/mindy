module: dylan


// Abstract classes. 

define open abstract class <number> (<object>)
end;

define open abstract class <complex> (<number>)
end;

define abstract class <real> (<complex>)
end;

define abstract class <rational> (<real>)
end;

define abstract class <integer> (<rational>)
end;

define abstract class <float> (<real>)
end;


// Psuedo-number methods

// These are defined on object because they are useful abstract interfaces.

define open generic \+ (num1 :: <object>, num2 :: <object>);

define open generic \* (num1 :: <object>, num2 :: <object>);

define open generic \- (num1 :: <object>, num2 :: <object>);

define open generic \/ (num1 :: <object>, num2 :: <object>);


// Number methods.

define open generic zero? (num :: <number>) => res :: <boolean>;

define inline method zero? (num :: <number>) => res :: <boolean>;
  num = 0;
end;

define open generic negative (num :: <number>) => res :: <number>;

define inline method negative (num :: <number>) => res :: <number>;
  0 - num;
end;

/* ### not absolutly needed
define open generic \^ (num :: <number>, power :: <number>) => res :: <number>;
*/

define open generic abs (num :: <number>) => res :: <number>;

/* ### not absolutly needed
define open generic rationalize (num :: <number>) => res :: <number>;

define open generic numerator (num :: <number>) => res :: <number>;

define open generic denominator (num :: <number>) => res :: <number>;
*/


// Real methods.

seal generic \= (<real>, <real>);

seal generic \~= (<real>, <real>);

seal generic \< (<real>, <real>);

seal generic \<= (<real>, <real>);

seal generic zero? (<real>);

define sealed generic positive? (num :: <real>) => res :: <boolean>;

define inline method positive? (num :: <real>) => res :: <boolean>;
  num > 0;
end;

define sealed generic negative? (num :: <real>) => res :: <boolean>;

define inline method negative? (num :: <real>) => res :: <boolean>;
  num < 0;
end;

/* ### not absolutly needed
define sealed generic integral? (num :: <real>) => res :: <boolean>;

define inline method integral? (num :: <real>) => res :: <boolean>;
  let (quo, rem) = floor(num);
  zero?(rem);
end;
*/

seal generic \+ (<real>, <real>);

seal generic \* (<real>, <real>);

seal generic \- (<real>, <real>);

seal generic \/ (<real>, <real>);

seal generic negative (<real>);

/* ### not absolutly needed
define sealed generic floor (num :: <real>)
    => (quo :: <integer>, rem :: <real>);

define sealed generic ceiling (num :: <real>)
    => (quo :: <integer>, rem :: <real>);

define sealed generic round (num :: <real>)
    => (quo :: <integer>, rem :: <real>);

define sealed generic truncate (num :: <real>)
    => (quo :: <integer>, rem :: <real>);
*/

define sealed generic floor/ (a :: <real>, b :: <real>)
    => (quo :: <integer>, rem :: <real>);

/* ### not absolutly needed
define inline method floor/ (a :: <real>, b :: <real>)
    => (quo :: <fixed-integer>, rem :: <real>);
  let quo = floor(a / b);
  values(quo, a - quo * b);
end;
*/

define sealed generic ceiling/ (a :: <real>, b :: <real>)
    => (quo :: <integer>, rem :: <real>);

/* ### not absolutly needed
define inline method ceiling/ (a :: <real>, b :: <real>)
    => (quo :: <fixed-integer>, rem :: <real>);
  let quo = ceiling(a / b);
  values(quo, a - quo * b);
end;
*/

define sealed generic round/ (a :: <real>, b :: <real>)
    => (quo :: <integer>, rem :: <real>);

/* ### not absolutly needed
define inline method round/ (a :: <real>, b :: <real>)
    => (quo :: <fixed-integer>, rem :: <real>);
  let quo = round(a / b);
  values(quo, a - quo * b);
end;
*/

define sealed generic truncate/ (a :: <real>, b :: <real>)
    => (quo :: <integer>, rem :: <real>);

/* ### not absolutly needed
define inline method truncate/ (a :: <real>, b :: <real>)
    => (quo :: <fixed-integer>, rem :: <real>);
  let quo = truncate(a / b);
  values(quo, a - quo * b);
end;
*/

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

/* ### not absolutly needed
seal generic \^ (<real>, <integer>);
*/

define sealed inline method abs (num :: <real>)
    => res :: <real>;
  if (negative?(num))
    -num;
  else
    num;
  end;
end;

/* ### not absolutly needed
seal generic rationalize (<real>);

seal generic numerator (<real>);

seal generic denominator (<real>);
*/

define sealed generic min (real :: <real>, #rest more-reals)
    => res :: <real>;

define inline method min (real :: <real>, #rest more-reals)
    => res :: <real>;
  reduce(binary-min, real, more-reals);
end;

define inline method binary-min (x :: <real>, y :: <real>) => res :: <real>;
  if (x < y) x else y end;
end;	  

define sealed generic max (real :: <real>, #rest more-reals)
    => res :: <real>;

define inline method max (real :: <real>, #rest more-reals)
    => res :: <real>;
  reduce(binary-max, real, more-reals);
end;

define inline method binary-max (x :: <real>, y :: <real>) => res :: <real>;
  if (x < y) y else x end;
end;	  



// Integer methods.

define sealed generic odd? (a :: <integer>) => res :: <boolean>;

define inline method odd? (a :: <integer>) => res :: <boolean>;
  ~even?(a);
end;

define sealed generic even? (a :: <integer>) => res :: <boolean>;

// No default method for even?.

/* ### not absolutly needed
define inline method integral? (a :: <integer>) => res :: <boolean>;
  #t;
end;

define inline method floor (a :: <integer>)
    => (quo :: <integer>, rem :: <integer>);
  values(a, 0);
end;

define inline method ceiling (a :: <integer>)
    => (quo :: <integer>, rem :: <integer>);
  values(a, 0);
end;

define inline method round (a :: <integer>)
    => (quo :: <integer>, rem :: <integer>);
  values(a, 0);
end;

define inline method truncate (a :: <integer>)
    => (quo :: <integer>, rem :: <integer>);
  values(a, 0);
end;
*/

define sealed generic logior (#rest integers)
    => res :: <integer>;

define inline method logior (#rest integers)
    => res :: <integer>;
  reduce(binary-logior, 0, integers);
end;

define sealed generic binary-logior (x :: <integer>, y :: <integer>)
    => res :: <integer>;

define sealed generic logxor (#rest integers)
    => res :: <integer>;

define inline method logxor (#rest integers)
    => res :: <integer>;
  reduce(binary-logxor, 0, integers);
end;

define sealed generic binary-logxor (x :: <integer>, y :: <integer>)
    => res :: <integer>;

define sealed generic logand (#rest integers)
    => res :: <integer>;

define inline method logand (#rest integers)
    => res :: <integer>;
  reduce(binary-logand, -1, integers);
end;

define sealed generic binary-logand (x :: <integer>, y :: <integer>)
    => res :: <integer>;

define sealed generic lognot (x :: <integer>) => res :: <integer>;

define sealed generic logbit? (index :: <integer>, int :: <integer>)
    => res :: <boolean>;

define sealed generic ash (int :: <integer>, count :: <integer>)
    => res :: <integer>;

/* ### not absolutly needed
define sealed generic lcm (x :: <integer>, y :: <integer>)
    => res :: <integer>;

define method lcm (x :: <integer>, y :: <integer>)
    => res :: <integer>;
  truncate/(max(x, y), gcd(x, y)) * min(x, y);
end;

define sealed generic gcd (x :: <integer>, y :: <integer>)
    => res :: <integer>;
*/


// Fixed Integers.

define functional class <fixed-integer> (<integer>)
  slot value :: <fixed-integer>, init-value: 0;
end;

define sealed method make (class == <fixed-integer>, #key) => res :: type-or();
  error("Can't make instances of <fixed-integer>, they just are.");
end;

// $fixed-integer-bits, $minimum-fixed-integer and $maximum-fixed-integer.
//
// Note the clever way we compute the second two of these that doesn't
// overflow.  Tricky, huh?
// 
define constant $fixed-integer-bits = 32;
define constant $minimum-fixed-integer :: <fixed-integer>
  = ash(-1, $fixed-integer-bits - 1);
define constant $maximum-fixed-integer :: <fixed-integer>
  = lognot($minimum-fixed-integer);

seal generic as (singleton(<fixed-integer>), <integer>);

define inline method as (class == <fixed-integer>, num :: <fixed-integer>)
    => res :: <fixed-integer>;
  num;
end;

define inline method \== (a :: <fixed-integer>, b :: <fixed-integer>)
    => res :: <boolean>;
  %%primitive fixnum-= (a, b);
end;

define inline method \== (a :: <fixed-integer>, b :: <object>)
    => res :: <boolean>;
  #f;
end;

/* Damn ambiguity rules.
define inline method \== (a :: <object>, b :: <fixed-integer>)
    => res :: <boolean>;
  #f;
end;
*/

// The fact that we are a functional class should automatically define this,
// but it doesn't yet.
//
define sealed inline method functional-==
    (a :: <fixed-integer>, b :: <fixed-integer>)
    => res :: <boolean>;
  a.value == b.value;
end;

seal generic functional-== (<fixed-integer>, <object>);
seal generic functional-== (<object>, <fixed-integer>);

define inline method \< (a :: <fixed-integer>, b :: <fixed-integer>)
    => res :: <boolean>;
  %%primitive fixnum-< (a, b);
end;

define inline method even? (a :: <fixed-integer>) => res :: <boolean>;
  zero?(logand(a, 1));
end;

define inline method \+ (a :: <fixed-integer>, b :: <fixed-integer>)
    => res :: <fixed-integer>;
  %%primitive fixnum-+ (a, b);
end;

define inline method \* (a :: <fixed-integer>, b :: <fixed-integer>)
    => res :: <fixed-integer>;
  %%primitive fixnum-* (a, b);
end;

define inline method \- (a :: <fixed-integer>, b :: <fixed-integer>)
    => res :: <fixed-integer>;
  %%primitive fixnum-- (a, b);
end;

define inline method negative (a :: <fixed-integer>)
    => res :: <fixed-integer>;
  %%primitive fixnum-negative (a);
end;

define inline method floor/ (a :: <fixed-integer>, b :: <fixed-integer>)
    => (quo :: <fixed-integer>, rem :: <fixed-integer>);
  %%primitive fixnum-floor/ (a, b);
end;

define inline method ceiling/ (a :: <fixed-integer>, b :: <fixed-integer>)
    => (quo :: <fixed-integer>, rem :: <fixed-integer>);
  %%primitive fixnum-ceiling/ (a, b);
end;

define inline method round/ (a :: <fixed-integer>, b :: <fixed-integer>)
    => (quo :: <fixed-integer>, rem :: <fixed-integer>);
  %%primitive fixnum-round/ (a, b);
end;

define inline method truncate/
    (a :: <fixed-integer>, b :: <fixed-integer>)
    => (quo :: <fixed-integer>, rem :: <fixed-integer>);
  %%primitive fixnum-truncate/ (a, b);
end;

/* ### not absolutly needed
define method \^ (base :: <number>, power :: <fixed-integer>)
    => res :: <number>;
  case
    negative?(power) =>
      if (power == $minimum-fixed-integer)
	1 / base ^ -(as(<extended-integer>, power));
      else
	1 / base ^ -power;
      end;
    base == 2 =>
      ash(1, power);
    base == #e2 =>
      ash(#e1, power);
    otherwise =>
      for (power :: <fixed-integer> = power then ash(power, -1),
	   total = as(object-class(base), 1)
	     then if (odd?(power)) base * total else total end,
	   base = base then base * base,
	   until: zero?(power))
      finally
	total;
      end;
  end;
end;
*/

define inline method binary-logior (a :: <fixed-integer>, b :: <fixed-integer>)
    => res :: <fixed-integer>;
  %%primitive fixnum-logior (a, b);
end;

define inline method binary-logxor (a :: <fixed-integer>, b :: <fixed-integer>)
    => res :: <fixed-integer>;
  %%primitive fixnum-logxor (a, b);
end;

define inline method binary-logand (a :: <fixed-integer>, b :: <fixed-integer>)
    => res :: <fixed-integer>;
  %%primitive fixnum-logand (a, b);
end;

define inline method lognot (a :: <fixed-integer>)
    => res :: <fixed-integer>;
  %%primitive fixnum-lognot (a);
end;

define inline method logbit?
    (index :: <fixed-integer>, integer :: <fixed-integer>)
    => res :: <boolean>;
  odd?(ash(integer, -index));
end;

define inline method ash (integer :: <fixed-integer>, count :: <fixed-integer>)
    => res :: <fixed-integer>;
  if (negative?(count))
    %%primitive fixnum-shift-right (integer, -count);
  else
    %%primitive fixnum-shift-left (integer, count);
  end;
end;

/* ### not absolutly needed
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
define method gcd (u :: <fixed-integer>, v :: <fixed-integer>)
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
    for (factors-of-two :: <fixed-integer> from 0,
	 u :: <fixed-integer> = u then ash(u, -1),
	 v :: <fixed-integer> = v then ash(v, -1),
	 until: odd?(logior(u, v)))
    finally
      // 
      // Now we make both u and v positive.  We don't just call abs
      // directly, because abs doesn't work on
      // $most-negative-fixed-integer.  So we rely on the fact that
      // $most-neg-fi is even, hence we can shift it down one then
      // negate it.
      //
      let u :: <fixed-integer> = abs(if (odd?(u)) u else ash(u, -1) end);
      let v :: <fixed-integer> = abs(if (odd?(v)) v else ash(v, -1) end);
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
	for (temp :: <fixed-integer> = if (odd?(u)) -v else ash(u, -1) end
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
*/


// Float methods.

/* ### not absolutly needed

seal generic as (singleton(<float>), <real>);

define inline method as (class == <float>, num :: <float>)
    => res :: <float>;
  num;
end;

define inline method as (class == <float>, num :: <rational>)
    => res :: <float>;
  as(<single-float>, num);
end;

*/


// Single floats.

define functional class <single-float> (<float>)
  slot value :: <single-float>, init-value: 0.0s0;
end;

define sealed method make (class == <single-float>, #key) => res :: type-or();
  error("Can't make instances of <single-float>, they just are.");
end;

/* ### not absolutly needed

seal generic as (singleton(<single-float>), <real>);

define inline method as (class == <single-float>, num :: <fixed-integer>)
    => res :: <single-float>;
  %%primitive fixed-as-single (num);
end;

define inline method as (class == <single-float>, num :: <single-float>)
    => res :: <single-float>;
  num;
end;

define inline method as (class == <single-float>, num :: <double-float>)
    => res :: <single-float>;
  %%primitive double-as-single (num);
end;

define inline method as (class == <single-float>, num :: <extended-float>)
    => res :: <single-float>;
  %%primitive extended-as-single (num);
end;

define inline method \== (a :: <single-float>, b :: <single-float>)
    => res :: <boolean>;
  %%primitive single-== (a, b);
end;

define inline method \== (a :: <single-float>, b :: <object>)
    => res :: <boolean>;
  #f;
end;

/* Damn ambiguity rules.
define inline method \== (a :: <object>, b :: <single-float>)
    => res :: <boolean>;
  #f;
end;
*/

// The fact that we are a functional class should automatically define this,
// but it doesn't yet.
//
define sealed inline method functional-==
    (a :: <single-float>, b :: <single-float>)
    => res :: <boolean>;
  a.value == b.value;
end;

seal generic functional-== (<single-float>, <object>);
seal generic functional-== (<object>, <single-float>);

define inline method \= (a :: <single-float>, b :: <single-float>)
    => res :: <boolean>;
  %%primitive single-= (a, b);
end;

define inline method \= (a :: <single-float>, b :: <fixed-integer>)
    => res :: <boolean>;
  a = as(<single-float>, b);
end;

define inline method \= (a :: <fixed-integer>, b :: <single-float>)
    => res :: <boolean>;
  as(<single-float>, a) = b;
end;

define inline method \< (a :: <single-float>, b :: <single-float>)
    => res :: <boolean>;
  %%primitive single-< (a, b);
end;

define inline method \< (a :: <single-float>, b :: <fixed-integer>)
    => res :: <boolean>;
  a < as(<single-float>, b);
end;

define inline method \< (a :: <fixed-integer>, b :: <single-float>)
    => res :: <boolean>;
  as(<single-float>, a) < b;
end;

define inline method \<= (a :: <single-float>, b :: <single-float>)
    => res :: <boolean>;
  %%primitive single-<= (a, b);
end;

define inline method \<= (a :: <single-float>, b :: <fixed-integer>)
    => res :: <boolean>;
  a <= as(<single-float>, b);
end;

define inline method \<= (a :: <fixed-integer>, b :: <single-float>)
    => res :: <boolean>;
  as(<single-float>, a) <= b;
end;

define inline method \~= (a :: <single-float>, b :: <single-float>)
    => res :: <boolean>;
  %%primitive single-~= (a, b);
end;

define inline method \~= (a :: <single-float>, b :: <fixed-integer>)
    => res :: <boolean>;
  a ~= as(<single-float>, b);
end;

define inline method \~= (a :: <fixed-integer>, b :: <single-float>)
    => res :: <boolean>;
  as(<single-float>, a) ~= b;
end;

define inline method \+ (a :: <single-float>, b :: <single-float>)
    => res :: <single-float>;
  %%primitive single-+ (a, b);
end;

define inline method \+ (a :: <single-float>, b :: <fixed-integer>)
    => res :: <single-float>;
  a + as(<single-float>, b);
end;

define inline method \+ (a :: <fixed-integer>, b :: <single-float>)
    => res :: <single-float>;
  as(<single-float>, a) + b;
end;

define inline method \* (a :: <single-float>, b :: <single-float>)
    => res :: <single-float>;
  %%primitive single-* (a, b);
end;

define inline method \* (a :: <single-float>, b :: <fixed-integer>)
    => res :: <single-float>;
  a * as(<single-float>, b);
end;

define inline method \* (a :: <fixed-integer>, b :: <single-float>)
    => res :: <single-float>;
  as(<single-float>, a) * b;
end;

define inline method \- (a :: <single-float>, b :: <single-float>)
    => res :: <single-float>;
  %%primitive single-- (a, b);
end;

define inline method \- (a :: <single-float>, b :: <fixed-integer>)
    => res :: <single-float>;
  a - as(<single-float>, b);
end;

define inline method \- (a :: <fixed-integer>, b :: <single-float>)
    => res :: <single-float>;
  as(<single-float>, a) - b;
end;

define inline method \/ (a :: <single-float>, b :: <single-float>)
    => res :: <single-float>;
  %%primitive single-/ (a, b);
end;

define inline method \/ (a :: <single-float>, b :: <fixed-integer>)
    => res :: <single-float>;
  a / as(<single-float>, b);
end;

define inline method \/ (a :: <fixed-integer>, b :: <single-float>)
    => res :: <single-float>;
  as(<single-float>, a) / b;
end;

define inline method negative (a :: <single-float>)
    => res :: <single-float>;
  let quo = %%primitive single-negative (a);
  values(quo, a - quo);
end;

define inline method floor (a :: <single-float>)
    => (quo :: <fixed-integer>, rem :: <single-float>);
  let quo = %%primitive single-floor (a);
  values(quo, a - quo);
end;

define inline method ceiling (a :: <single-float>)
    => (quo :: <fixed-integer>, rem :: <single-float>);
  let quo = %%primitive single-ceiling (a);
  values(quo, a - quo);
end;

define inline method round (a :: <single-float>)
    => (quo :: <fixed-integer>, rem :: <single-float>);
  let quo = %%primitive single-round (a);
  values(quo, a - quo);
end;

define inline method truncate (a :: <single-float>)
    => (quo :: <fixed-integer>, rem :: <single-float>);
  let quo = if (negative?(a))
	      %%primitive single-ceiling (a);
	    else
	      %%primitive single-floor (a);
	    end;
  values(quo, a - quo);
end;

define inline method abs (a :: <single-float>)
    => abs :: <single-float>;
  %%primitive single-abs (a);
end;

*/


// Double floats.

define functional class <double-float> (<float>)
  slot value :: <double-float>, init-value: 0.0d0;
end;

define sealed method make (class == <double-float>, #key) => res :: type-or();
  error("Can't make instances of <double-float>, they just are.");
end;

/* ### not absolutly needed

seal generic as (singleton(<double-float>), <real>);

define inline method as (class == <double-float>, num :: <fixed-integer>)
    => res :: <double-float>;
  %%primitive fixed-as-double (num);
end;

define inline method as (class == <double-float>, num :: <single-float>)
    => res :: <double-float>;
  %%primitive single-as-double (num);
end;

define inline method as (class == <double-float>, num :: <double-float>)
    => res :: <double-float>;
  num;
end;

define inline method as (class == <double-float>, num :: <extended-float>)
    => res :: <double-float>;
  %%primitive extended-as-double (num);
end;

define inline method \== (a :: <double-float>, b :: <double-float>)
    => res :: <boolean>;
  %%primitive double-== (a, b);
end;

define inline method \== (a :: <double-float>, b :: <object>)
    => res :: <boolean>;
  #f;
end;

/* Damn ambiguity rules.
define inline method \== (a :: <object>, b :: <double-float>)
    => res :: <boolean>;
  #f;
end;
*/

// The fact that we are a functional class should automatically define this,
// but it doesn't yet.
//
define sealed inline method functional-==
    (a :: <double-float>, b :: <double-float>)
    => res :: <boolean>;
  a.value == b.value;
end;

seal generic functional-== (<double-float>, <object>);
seal generic functional-== (<object>, <double-float>);

define inline method \= (a :: <double-float>, b :: <double-float>)
    => res :: <boolean>;
  %%primitive double-= (a, b);
end;

define inline method \= (a :: <double-float>, b :: <fixed-integer>)
    => res :: <boolean>;
  a = as(<double-float>, b);
end;

define inline method \= (a :: <fixed-integer>, b :: <double-float>)
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
  %%primitive double-< (a, b);
end;

define inline method \< (a :: <double-float>, b :: <fixed-integer>)
    => res :: <boolean>;
  a < as(<double-float>, b);
end;

define inline method \< (a :: <fixed-integer>, b :: <double-float>)
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
  %%primitive double-<= (a, b);
end;

define inline method \<= (a :: <double-float>, b :: <fixed-integer>)
    => res :: <boolean>;
  a <= as(<double-float>, b);
end;

define inline method \<= (a :: <fixed-integer>, b :: <double-float>)
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
  %%primitive double-~= (a, b);
end;

define inline method \~= (a :: <double-float>, b :: <fixed-integer>)
    => res :: <boolean>;
  a ~= as(<double-float>, b);
end;

define inline method \~= (a :: <fixed-integer>, b :: <double-float>)
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
  %%primitive double-+ (a, b);
end;

define inline method \+ (a :: <double-float>, b :: <fixed-integer>)
    => res :: <double-float>;
  a + as(<double-float>, b);
end;

define inline method \+ (a :: <fixed-integer>, b :: <double-float>)
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
  %%primitive double-* (a, b);
end;

define inline method \* (a :: <double-float>, b :: <fixed-integer>)
    => res :: <double-float>;
  a * as(<double-float>, b);
end;

define inline method \* (a :: <fixed-integer>, b :: <double-float>)
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
  %%primitive double-- (a, b);
end;

define inline method \- (a :: <double-float>, b :: <fixed-integer>)
    => res :: <double-float>;
  a - as(<double-float>, b);
end;

define inline method \- (a :: <fixed-integer>, b :: <double-float>)
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
  %%primitive double-/ (a, b);
end;

define inline method \/ (a :: <double-float>, b :: <fixed-integer>)
    => res :: <double-float>;
  a / as(<double-float>, b);
end;

define inline method \/ (a :: <fixed-integer>, b :: <double-float>)
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
  %%primitive double-negative (a);
end;

define inline method floor (a :: <double-float>)
    => (quo :: <fixed-integer>, rem :: <double-float>);
  let quo = %%primitive double-floor (a);
  values(quo, a - quo);
end;

define inline method ceiling (a :: <double-float>)
    => (quo :: <fixed-integer>, rem :: <double-float>);
  let quo = %%primitive double-ceiling (a);
  values(quo, a - quo);
end;

define inline method round (a :: <double-float>)
    => (quo :: <fixed-integer>, rem :: <double-float>);
  let quo = %%primitive double-round (a);
  values(quo, a - quo);
end;

define inline method truncate (a :: <double-float>)
    => (quo :: <fixed-integer>, rem :: <double-float>);
  let quo = if (negative?(a))
	      %%primitive double-ceiling (a);
	    else
	      %%primitive double-floor (a);
	    end;
  values(quo, a - quo);
end;

define inline method abs (a :: <double-float>)
    => abs :: <double-float>;
  %%primitive double-abs (a);
end;

*/


// Extended floats.

define functional class <extended-float> (<float>)
  slot value :: <extended-float>, init-value: 0.0x0;
end;

define sealed method make (class == <extended-float>, #key)
    => res :: type-or();
  error("Can't make instances of <extended-float>, they just are.");
end;

/* ### not absolutly needed

seal generic as (singleton(<extended-float>), <real>);

define inline method as (class == <extended-float>, num :: <fixed-integer>)
    => res :: <extended-float>;
  %%primitive fixed-as-extended (num);
end;

define inline method as (class == <extended-float>, num :: <single-float>)
    => res :: <extended-float>;
  %%primitive single-as-extended (num);
end;

define inline method as (class == <extended-float>, num :: <double-float>)
    => res :: <extended-float>;
  %%primitive double-as-extended (num);
end;

define inline method as (class == <extended-float>, num :: <extended-float>)
    => res :: <extended-float>;
  num;
end;

define inline method \== (a :: <extended-float>, b :: <extended-float>)
    => res :: <boolean>;
  %%primitive extended-== (a, b);
end;

define inline method \== (a :: <extended-float>, b :: <object>)
    => res :: <boolean>;
  #f;
end;

/* Damn ambiguity rules.
define inline method \== (a :: <object>, b :: <extended-float>)
    => res :: <boolean>;
  #f;
end;
*/

// The fact that we are a functional class should automatically define this,
// but it doesn't yet.
//
define sealed inline method functional-==
    (a :: <extended-float>, b :: <extended-float>)
    => res :: <boolean>;
  a.value == b.value;
end;

seal generic functional-== (<extended-float>, <object>);
seal generic functional-== (<object>, <extended-float>);

define inline method \= (a :: <extended-float>, b :: <extended-float>)
    => res :: <boolean>;
  %%primitive extended-= (a, b);
end;

define inline method \= (a :: <extended-float>, b :: <fixed-integer>)
    => res :: <boolean>;
  a = as(<extended-float>, b);
end;

define inline method \= (a :: <fixed-integer>, b :: <extended-float>)
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
  %%primitive extended-< (a, b);
end;

define inline method \< (a :: <extended-float>, b :: <fixed-integer>)
    => res :: <boolean>;
  a < as(<extended-float>, b);
end;

define inline method \< (a :: <fixed-integer>, b :: <extended-float>)
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
  %%primitive extended-<= (a, b);
end;

define inline method \<= (a :: <extended-float>, b :: <fixed-integer>)
    => res :: <boolean>;
  a <= as(<extended-float>, b);
end;

define inline method \<= (a :: <fixed-integer>, b :: <extended-float>)
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
  %%primitive extended-~= (a, b);
end;

define inline method \~= (a :: <extended-float>, b :: <fixed-integer>)
    => res :: <boolean>;
  a ~= as(<extended-float>, b);
end;

define inline method \~= (a :: <fixed-integer>, b :: <extended-float>)
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
  %%primitive extended-+ (a, b);
end;

define inline method \+ (a :: <extended-float>, b :: <fixed-integer>)
    => res :: <extended-float>;
  a + as(<extended-float>, b);
end;

define inline method \+ (a :: <fixed-integer>, b :: <extended-float>)
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
  %%primitive extended-* (a, b);
end;

define inline method \* (a :: <extended-float>, b :: <fixed-integer>)
    => res :: <extended-float>;
  a * as(<extended-float>, b);
end;

define inline method \* (a :: <fixed-integer>, b :: <extended-float>)
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
  %%primitive extended-- (a, b);
end;

define inline method \- (a :: <extended-float>, b :: <fixed-integer>)
    => res :: <extended-float>;
  a - as(<extended-float>, b);
end;

define inline method \- (a :: <fixed-integer>, b :: <extended-float>)
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
  %%primitive extended-/ (a, b);
end;

define inline method \/ (a :: <extended-float>, b :: <fixed-integer>)
    => res :: <extended-float>;
  a / as(<extended-float>, b);
end;

define inline method \/ (a :: <fixed-integer>, b :: <extended-float>)
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
  %%primitive extended-negative (a);
end;

define inline method floor (a :: <extended-float>)
    => (quo :: <fixed-integer>, rem :: <extended-float>);
  let quo = %%primitive extended-floor (a);
  values(quo, a - quo);
end;

define inline method ceiling (a :: <extended-float>)
    => (quo :: <fixed-integer>, rem :: <extended-float>);
  let quo = %%primitive extended-ceiling (a);
  values(quo, a - quo);
end;

define inline method round (a :: <extended-float>)
    => (quo :: <fixed-integer>, rem :: <extended-float>);
  let quo = %%primitive extended-round (a);
  values(quo, a - quo);
end;

define inline method truncate (a :: <extended-float>)
    => (quo :: <fixed-integer>, rem :: <extended-float>);
  let quo = if (negative?(a))
	      %%primitive extended-ceiling (a);
	    else
	      %%primitive extended-floor (a);
	    end;
  values(quo, a - quo);
end;

define inline method abs (a :: <extended-float>)
    => abs :: <extended-float>;
  %%primitive extended-abs (a);
end;

*/
