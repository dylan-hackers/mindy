rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/bignum.dylan,v 1.7 2002/08/24 14:38:07 bruce Exp $
copyright: see below
module: dylan-viscera


//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
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

// Extended integer digits.

define constant $digit-bits = 16;
define constant $digit-mask = lognot(ash(-1, $digit-bits));

// <digit> -- internal.
// 
define functional class <digit> (<object>)
  //
  // The value of this digit.
  slot value :: limited(<integer>, min: 0, max: $digit-mask),
    required-init-keyword: value:;
end;

define sealed domain make (singleton(<digit>));
define sealed domain initialize (<digit>);

// The fact that we are a functional class should automatically define this,
// but it doesn't yet.
//
define inline method functional-==
    (class == <digit>, a :: <digit>, b :: <digit>)
    => res :: <boolean>;
  a.value == b.value;
end;

define sealed domain functional-== (singleton(<digit>), <object>, <object>);

// Seal = on digits
// 
define sealed domain \= (<digit>, <object>);
define sealed domain \= (<object>, <digit>);

// < -- exported GF method.
//
// Return #t if the first digit is less than the second digit.
//
define inline method \< (digit1 :: <digit>, digit2 :: <digit>)
    => res :: <boolean>;
  digit1.value < digit2.value;
end;

// Seal < on digits.
// 
define sealed domain \< (<digit>, <object>);
define sealed domain \< (<object>, <digit>);

// make-digit -- internal.
//
// Make a digit from the low bits of a fixed integer.
// 
define inline method make-digit (num :: <integer>)
    => res :: <digit>;
  make(<digit>, value: logand(num, $digit-mask));
end;

// as-signed -- internal.
//
// Make a fixed integer from a sign extended digit.
//
define inline method as-signed (digit :: <digit>)
    => res :: <integer>;
  ash(ash(digit.value,
          $fixed-integer-bits - $digit-bits),
      $digit-bits - $fixed-integer-bits);
end;

// as-signed-2 -- internal.
//
// Make a fixed integer by sign extendeding two digits.
//
define inline method as-signed-2 (digit-a :: <digit>, digit-b :: <digit>)
    => res :: <integer>;
  let raw = logior(ash(digit-a.value, $digit-bits), digit-b.value);
  // Note: this is a no-op when $digit-bits is half of $fixed-integer-bits
  ash(ash(raw,
          $fixed-integer-bits - $digit-bits - $digit-bits),
      $digit-bits + $digit-bits - $fixed-integer-bits);
end;

// as-unsigned -- internal.
//
// Make a fixed integer from a digit treating it as an unsigned quantity.
//
define inline method as-unsigned (digit :: <digit>)
    => res :: <integer>;
  digit.value;
end;

// sign-extend-digit -- internal.
//
// Return a digit of all zeros or all ones based on the sign bit of the
// argument.
//
define inline method sign-extend-digit (digit :: <digit>)
    => res :: <digit>;
  if (digit-sign-bit-set?(digit))
    make-digit($digit-mask);
  else
    make-digit(0);
  end;
end;

// digit-sign-bit-set? -- internal.
//
// Return #t if the sign bit is set, and #f if not.
//
define inline method digit-sign-bit-set? (digit :: <digit>)
    => res :: <boolean>;
  logbit?($digit-bits - 1, digit.value);
end;

// $no-carry -- internal.
//
// The value to pass to digit-add to indicate no initial carry.
// 
define constant $no-carry = 0;

// digit-add -- internal
//
// Add the two digits and the carry in to produce a single digit and a carry
// out.
//
define inline method digit-add
    (digit1 :: <digit>, digit2 :: <digit>, carry :: <integer>)
    => (res :: <digit>, carry :: <integer>);
  let sum = digit1.value + digit2.value + carry;
  values(make-digit(sum),
	 ash(sum, - $digit-bits));
end;

// $no-borrow -- internal.
//
// The value to pass to digit-subtract to indicate no initial borrow.
//
define constant $no-borrow = 0;

// digit-subtract -- internal.
//
// Subtract digit2 and the borrow in from digit1 to produce a single digit
// and a borrow out.
//
define inline method digit-subtract
    (digit1 :: <digit>, digit2 :: <digit>, borrow :: <integer>)
    => (res :: <digit>, borrow :: <integer>);
  let sum = digit1.value - digit2.value - borrow;
  values(make-digit(sum),
	 logand(ash(sum, - $digit-bits), 1));
end;

// digit-multiply -- internal.
//
// Multiply the two digits producing a two-digit result.
//
define inline method digit-multiply (digit1 :: <digit>, digit2 :: <digit>)
    => (low :: <digit>, high :: <digit>);
  let prod = digit1.value * digit2.value;
  values(make-digit(prod),
	 make-digit(ash(prod, - $digit-bits)));
end;

// digit-shift -- internal.
//
// Shift high up by shift digits, taking the new bits from the top of low.
// 
define inline method digit-shift
    (high :: <digit>, low :: <digit>, shift :: <integer>)
    => res :: <digit>;
  make-digit(logior(ash(low.value, shift - $digit-bits),
		    ash(high.value, shift)));
end;

// digit-divide -- internal.
//
// Divide high:low by div and return the quotient and remainder.
// 
define inline method digit-divide
    (high :: <digit>, low :: <digit>, div :: <digit>)
    => (quo :: <digit>, rem :: <digit>);
  let (quo, rem) = floor/(logior(ash(high.value, $digit-bits), low.value),
			  div.value);
  values(make-digit(quo), make-digit(rem));
end;

define inline method digit-logior (x :: <digit>, y :: <digit>)
    => res :: <digit>;
  make-digit(logior(x.value, y.value));
end;

define inline method digit-logxor (x :: <digit>, y :: <digit>)
    => res :: <digit>;
  make-digit(logxor(x.value, y.value));
end;

define inline method digit-logand (x :: <digit>, y :: <digit>)
    => res :: <digit>;
  make-digit(logand(x.value, y.value));
end;

define inline method digit-lognot (x :: <digit>) => res :: <digit>;
  make-digit(lognot(x.value));
end;


// Extended integer class and utilities

// <extended-integer> -- exported via Extensions.
//
// A bignum.
// 
define class <extended-integer> (<general-integer>)
  //
  // A bignum is just a vector of digits.  We require a fill instead of
  // supplying an init-value because we can't supply an obviously constant
  // init-value.
  slot bignum-digit :: <digit>,
    sizer: bignum-size, required-size-init-keyword: size:,
    required-init-keyword: fill:;
end;

define sealed domain make (singleton(<extended-integer>));
define sealed domain initialize (<extended-integer>);

// make-bignum -- internal.
//
// Shorthand constructor function.
// 
define inline method make-bignum (size :: <integer>)
    => res :: <extended-integer>;
  make(<extended-integer>, size: size, fill: make-digit(0));
end;

// shrink-bignum -- internal.
//
// Change num's size to be new-size, throwing away any extra digits.  Note: it
// is guarenteed that new-size will be less than or equal to the current size.
//
define method shrink-bignum
    (num :: <extended-integer>, new-size :: <integer>)
    => new :: <extended-integer>;
  // %%primitive(shrink-bignum, num, new-size);
  if (new-size == num.bignum-size)
    num;
  else
    let new = make-bignum(new-size);
    for (index :: <integer> from 0 below new-size)
      bignum-digit(new, index) := bignum-digit(num, index);
    end;
    new;
  end;
end;

// normalized-length -- internal.
//
// Compute the minimum number of digits needed to store num by stripping off
// superfluous digits.  A digit is superfluous if is at the end and is the same
// as the sign extension of the previous digit.  Note: there has to be at least
// one digit.
//
define method normalized-length (num :: <extended-integer>,
				 len :: <integer>)
    => res-len :: <integer>;
  if (len > 1)
    for (index :: <integer> from len - 2 to 0 by -1,
	 prev-digit :: <digit> = bignum-digit(num, len - 1)
	   then bignum-digit(num, index),
	 while: sign-extend-digit(bignum-digit(num, index)) == prev-digit)
    finally
      index + 2;
    end;
  else
    len;
  end;
end;
    
// normalize-bignum -- internal.
//
// We always call shrink-bignum even if we don't think we are changing the
// length because the bignum might have more digits than the length we were
// originally passed.
//
define inline method normalize-bignum (num :: <extended-integer>,
				       len :: <integer>)
    => res :: <extended-integer>;
  if (num.bignum-size > 1)
    shrink-bignum(num, normalized-length(num, len));
  else
    num;
  end;
end;


// as methods.

define sealed domain as (singleton(<extended-integer>), <complex>);

define inline method as
    (class == <extended-integer>, num :: <extended-integer>)
    => res :: <extended-integer>;
  //
  // Converting an extended integer into an extended integer is rather easy.
  num;
end;

define method as (class == <extended-integer>, num :: <integer>)
    => res :: <extended-integer>;
  //
  // To convert a fixed integer into an extended integer, we recurse to
  // find how many digits we need, create a bignum, and then fill it in
  // on the way back out of the recursion.
  let sign = ash(num, 1 - $fixed-integer-bits);
  local method loop(n :: <integer>, len :: <integer>)
          let res = if (ash(n, 1 - $digit-bits) = sign)
                      make-bignum(len + 1);
                    else
                      loop(ash(n, -$digit-bits), len + 1);
                    end;
          bignum-digit(res, len) := make-digit(n);
          res;
        end;
  loop(num, 0);
end;

define method as (class == <integer>, num :: <extended-integer>)
    => res :: <integer>;
  //
  // To convert an extended integer into a fixnum, we just combine the digits
  // by shifting and logior-ing.  We use as-signed on the most significant
  // digit because that is the digit that has the sign bit in it.
  //
  let len = bignum-size(num);
  if (len = 1)
    bignum-digit(num, 0).as-signed;
  elseif (len = 2)
    as-signed-2(bignum-digit(num, 1), bignum-digit(num, 0));
  elseif (num < $minimum-integer | num > $maximum-integer)
    // TODO: make extended constants for min/max
    error("%= can't be represented as a <integer>", num);
  else
    local
      method repeat (index :: <integer>, result :: <integer>)
        if (negative?(index))
          result;
        else
          repeat(index - 1,
                 logior(ash(result, $digit-bits),
                        as-unsigned(bignum-digit(num, index))));
        end;
      end;
    repeat(len - 2, as-signed(bignum-digit(num, len - 1)));
  end;
end;

define method as (class == <single-float>, num :: <extended-integer>)
    => res :: <single-float>;
/*
  if (if (negative?(num))
	num < floor/($most-negative-single-float, #e1);
      else
	num > floor/($most-positive-single-float, #e1);
      end)
    error("%= can't be represented as a <single-float>", num);
  end;
*/
  bignum-as-float(class, num);
end;

define method as (class == <double-float>, num :: <extended-integer>)
    => res :: <double-float>;
/*
  if (if (negative?(num))
	num < floor/($most-negative-double-float, #e1);
      else
	num > floor/($most-positive-double-float, #e1);
      end)
    error("%= can't be represented as a <double-float>", num);
  end;
*/
  bignum-as-float(class, num);
end;

define method as (class == <extended-float>, num :: <extended-integer>)
    => res :: <extended-float>;
/*
  if (if (negative?(num))
	num < floor/($most-negative-extended-float, #e1);
      else
	num > floor/($most-positive-extended-float, #e1);
      end)
    error("%= can't be represented as a <extended-float>", num);
  end;
*/
  bignum-as-float(class, num);
end;

define inline method bignum-as-float
    (class :: <class>, num :: <extended-integer>)
    => res :: <float>;
  let len = bignum-size(num);
  if (len = 1)
    as(class, bignum-digit(num, 0).as-signed);
  elseif (len = 2)
    as(class, as-signed-2(bignum-digit(num, 1), bignum-digit(num, 0)));
  else
    local
      method repeat (index :: <integer>, result :: <float>)
        if (negative?(index))
          result;
        else
          repeat(index - 1,
                 result * ash(1, $digit-bits)
                   + as(class, as-unsigned(bignum-digit(num, index))));
        end;
      end;
    repeat(len - 2, as(class, as-signed(bignum-digit(num, len - 1))));
  end;
end;


// Comparison methods.

define method \== (num1 :: <extended-integer>, num2 :: <extended-integer>)
    => res :: <boolean>;
  let len1 = bignum-size(num1);
  let len2 = bignum-size(num2);
  if (len1 == len2)
    block (return)
      for (posn :: <integer> from len1 - 1 to 0 by -1)
	let digit1 = bignum-digit(num1, posn);
	let digit2 = bignum-digit(num2, posn);
	unless (digit1 == digit2)
	  return(#f);
	end;
      end;
      #t;
    end;
  end;
end;

define inline method \== (num :: <extended-integer>, thing :: <object>)
    => res :: <boolean>;
  #f;
end;

define method \< (num1 :: <extended-integer>, num2 :: <extended-integer>)
    => res :: <boolean>;
  let len1 = bignum-size(num1);
  let len2 = bignum-size(num2);
  if (len1 + len2 = 2)
    bignum-digit(num1, 0).as-signed < bignum-digit(num2, 0).as-signed;
  else
    let num1-neg = digit-sign-bit-set?(bignum-digit(num1, len1 - 1));
    let num2-neg = digit-sign-bit-set?(bignum-digit(num2, len2 - 1));
    if (num1-neg == num2-neg)
      if (len1 == len2)
        block (return)
          for (posn :: <integer> from len1 - 1 to 0 by -1)
            let digit1 = bignum-digit(num1, posn);
            let digit2 = bignum-digit(num2, posn);
            if (digit1 ~= digit2)
              return(digit1 < digit2);
            end;
          end;
          #f;
        end;
      elseif (len1 < len2)
        ~num1-neg;
      else
        num1-neg;
      end;
    else
      num1-neg;
    end;
  end;
end;

// even? -- exported generic function method.
//
// For a bignum to be even, the first digit must be even.
//
define inline sealed method even? (a :: <extended-integer>)
    => res :: <boolean>;
  bignum-digit(a, 0).value.even?;
end;

// zero? -- exported generic function method.
//
// For a bignum to be zero, the size has to be 1 and the only digit 0.
//
define inline method zero? (a :: <extended-integer>)
    => res :: <boolean>;
  a.bignum-size == 1 & bignum-digit(a, 0).value.zero?;
end;

// positive? -- exported generic function method.
//
// For a number to be positive, it has to be greater than zero.  For bignums,
// this means that if there is one digit, it is greater than zero and if there
// are multiple digits, the sign bit is clear.
//
define inline method positive? (a :: <extended-integer>)
    => res :: <boolean>;
  let len = a.bignum-size;
  if (len == 1)
    as-signed(bignum-digit(a, 0)) > 0;
  else
    ~digit-sign-bit-set?(bignum-digit(a, len - 1));
  end;
end;

// negative? -- exported generic function method.
//
// For a bignum to be negative, this sign bit has to be set.
//
define inline method negative? (a :: <extended-integer>)
    => res :: <boolean>;
  digit-sign-bit-set?(bignum-digit(a, a.bignum-size - 1));
end;


// Addition.

define method \+ (a :: <extended-integer>, b :: <extended-integer>)
    => res :: <extended-integer>;
  let a-len = bignum-size(a);
  let b-len = bignum-size(b);
  if (a-len + b-len = 2)
    as(<extended-integer>, bignum-digit(a, 0).as-signed + bignum-digit(b, 0).as-signed);
  else
    let (shorter, shorter-len, longer, longer-len)
      = if (a-len < b-len)
          values(a, a-len, b, b-len);
        else
          values(b, b-len, a, a-len);
        end;
    let res = make-bignum(longer-len + 1);
    let carry-in = $no-carry;
    let shorter-digit = make-digit(0);
    let longer-digit = make-digit(0);
    for (index :: <integer> from 0 below shorter-len)
      shorter-digit := bignum-digit(shorter, index);
      longer-digit := bignum-digit(longer, index);
      let (digit, carry-out) = digit-add(shorter-digit, longer-digit, carry-in);
      bignum-digit(res, index) := digit;
      carry-in := carry-out;
    end;
    let shorter-sign = sign-extend-digit(shorter-digit);
    for (index :: <integer> from shorter-len below longer-len)
      longer-digit := bignum-digit(longer, index);
      let (digit, carry-out) = digit-add(shorter-sign, longer-digit, carry-in);
      bignum-digit(res, index) := digit;
      carry-in := carry-out;
    end;
    bignum-digit(res, longer-len)
      := digit-add(shorter-sign, sign-extend-digit(longer-digit), carry-in);
    normalize-bignum(res, longer-len + 1);
  end;
end;


// Multiplication.

define method \* (a :: <extended-integer>, b :: <extended-integer>)
    => res :: <extended-integer>;
  let a-len = bignum-size(a);
  let b-len = bignum-size(b);
  if (a-len + b-len = 2)
    as(<extended-integer>, bignum-digit(a, 0).as-signed * bignum-digit(b, 0).as-signed);
  else
    let res-len = a-len + b-len;
    let res = make-bignum(res-len);
    for (index :: <integer> from 0 below res-len)
      bignum-digit(res, index) := make-digit(0);
    end;
    local
      method mult-and-add
          (a-digit :: <digit>, b-digit :: <digit>, res-index :: <integer>,
           carry :: <digit>)
       => new-carry :: <digit>;
        let (low, high) = digit-multiply(a-digit, b-digit);
        let (low, carry) = digit-add(low, carry, $no-carry);
        let high = digit-add(high, make-digit(0), carry);
        let res-digit = bignum-digit(res, res-index);
        let (low, carry) = digit-add(low, res-digit, $no-carry);
        let high = digit-add(high, make-digit(0), carry);
        bignum-digit(res, res-index) := low;
        high;
      end;
    let a-digit = make-digit(0);
    for (a-index :: <integer> from 0 below a-len)
      a-digit := bignum-digit(a, a-index);
      let b-digit = make-digit(0);
      let carry = make-digit(0);
      for (b-index :: <integer> from 0 below b-len)
        b-digit := bignum-digit(b, b-index);
        carry := mult-and-add(a-digit, b-digit, a-index + b-index, carry);
      end;
      let b-sign = sign-extend-digit(b-digit);
      for (b-index :: <integer> from b-len below res-len - a-index)
        carry := mult-and-add(a-digit, b-sign, a-index + b-index, carry);
      end;
    end;
    let a-sign = sign-extend-digit(a-digit);
    for (a-index :: <integer> from a-len below res-len)
      let carry = make-digit(0);
      for (b-index :: <integer> from 0 below res-len - a-index)
        let b-digit = bignum-digit(b, b-index);
        carry := mult-and-add(a-sign, b-digit, a-index + b-index, carry);
      end;
    end;
    normalize-bignum(res, res-len);
  end;
end;


// Subtraction.

define method \- (a :: <extended-integer>, b :: <extended-integer>)
    => res :: <extended-integer>;
  let a-len = bignum-size(a);
  let b-len = bignum-size(b);
  if (a-len + b-len = 2)
    as(<extended-integer>, bignum-digit(a, 0).as-signed - bignum-digit(b, 0).as-signed);
  else
    if (a-len < b-len)
      let res = make-bignum(b-len + 1);
      let borrow-in = $no-borrow;
      let a-digit = make-digit(0);
      let b-digit = make-digit(0);
      for (index :: <integer> from 0 below a-len)
        a-digit := bignum-digit(a, index);
        b-digit := bignum-digit(b, index);
        let (digit, borrow-out) = digit-subtract(a-digit, b-digit, borrow-in);
        bignum-digit(res, index) := digit;
        borrow-in := borrow-out;
      end;
      let a-sign = sign-extend-digit(a-digit);
      for (index :: <integer> from a-len below b-len)
        b-digit := bignum-digit(b, index);
        let (digit, borrow-out) = digit-subtract(a-sign, b-digit, borrow-in);
        bignum-digit(res, index) := digit;
        borrow-in := borrow-out;
      end;
      bignum-digit(res, b-len)
        := digit-subtract(a-sign, sign-extend-digit(b-digit), borrow-in);
      normalize-bignum(res, b-len + 1);
    else
      let res = make-bignum(a-len + 1);
      let borrow-in = $no-borrow;
      let a-digit = make-digit(0);
      let b-digit = make-digit(0);
      for (index :: <integer> from 0 below b-len)
        a-digit := bignum-digit(a, index);
        b-digit := bignum-digit(b, index);
        let (digit, borrow-out) = digit-subtract(a-digit, b-digit, borrow-in);
        bignum-digit(res, index) := digit;
        borrow-in := borrow-out;
      end;
      let b-sign = sign-extend-digit(b-digit);
      for (index :: <integer> from b-len below a-len)
        a-digit := bignum-digit(a, index);
        let (digit, borrow-out) = digit-subtract(a-digit, b-sign, borrow-in);
        bignum-digit(res, index) := digit;
        borrow-in := borrow-out;
      end;
      bignum-digit(res, a-len)
        := digit-subtract(sign-extend-digit(a-digit), b-sign, borrow-in);
      normalize-bignum(res, a-len + 1);
    end;
  end;
end;


// Negation.

define method negative (num :: <extended-integer>)
    => res :: <extended-integer>;
  let len = num.bignum-size;
  let res = make-bignum(len + 1);
  let borrow-in = $no-borrow;
  let digit = make-digit(0);
  for (index :: <integer> from 0 below len)
    digit := bignum-digit(num, index);
    let (res-digit, borrow-out)
      = digit-subtract(make-digit(0), digit, borrow-in);
    bignum-digit(res, index) := res-digit;
    borrow-in := borrow-out;
  end;
  bignum-digit(res, len)
    := digit-subtract(make-digit(0), sign-extend-digit(digit), borrow-in);
  normalize-bignum(res, len + 1);
end;


// Division.

define method divide-by-digit (num :: <extended-integer>, digit :: <digit>)
    => (quo :: <extended-integer>, rem :: <extended-integer>);
  let len = num.bignum-size;
  let quo = make-bignum(len);
  let rem = make-digit(0);
  for (index :: <integer> from len - 1 to 0 by -1)
    let (quo-digit, new-rem)
      = digit-divide(rem, bignum-digit(num, index), digit);
    bignum-digit(quo, index) := quo-digit;
    rem := new-rem;
  end;
  let big-rem = make-bignum(1);
  bignum-digit(big-rem, 0) := rem;
  values(normalize-bignum(quo, len), big-rem);
end;

define method divisor-shift (num :: <extended-integer>)
    => res :: <integer>;
  for (top-digit :: <integer>
	 = as-signed(bignum-digit(num, num.bignum-size - 1))
	 then ash(top-digit, -1),
       count :: <integer> from 1,
       until: top-digit == 0)
  finally
    $digit-bits - count;
  end;
end;

define method division-guess (x1 :: <digit>, x2 :: <digit>, x3 :: <digit>,
			      y1 :: <digit>, y2 :: <digit>)
    => res :: <digit>;
  block (return)
    for (guess :: <digit>
	   = if (x1 == y1)
	       make-digit(-1);
	     else
	       digit-divide(x1, x2, y1);
	     end
	   then digit-subtract(guess, make-digit(1), $no-borrow))
      let (guess*y1-low, guess*y1-high) = digit-multiply(guess, y1);
      let (guess*y2-low, guess*y2-high) = digit-multiply(guess, y2);
      let (sum-mid, carry) = digit-add(guess*y1-low, guess*y2-high, $no-carry);
      let sum-high = digit-add(guess*y1-high, make-digit(0), carry);
      if (sum-high < x1)
	return(guess);
      end;
      if (x1 == sum-high)
	if (sum-mid < x2)
	  return(guess);
	end;
	if (x2 == sum-mid)
	  if (x3 >= guess*y2-low)
	    return(guess);
	  end;
	end;
      end;
    end;
  end;
end;

define method shift-for-division
    (x :: <extended-integer>, shift :: <integer>)
    => res :: <extended-integer>;
  let len = x.bignum-size;
  if (zero?(shift))
    let res = make-bignum(len + 1);
    for (index :: <integer> from 0 below len)
      bignum-digit(res, index) := bignum-digit(x, index);
    end;
    bignum-digit(res, len) := make-digit(0);
    res;
  else
    let res = make-bignum(len + 2);
    let prev-digit = make-digit(0);
    for (index :: <integer> from 0 below len)
      let next-digit = bignum-digit(x, index);
      bignum-digit(res, index) := digit-shift(next-digit, prev-digit, shift);
      prev-digit := next-digit;
    end;
    bignum-digit(res, len) := digit-shift(make-digit(0), prev-digit, shift);
    bignum-digit(res, len + 1) := make-digit(0);
    res;
  end;
end;

// This is only called with +ve arguments
define method bignum-divide (x :: <extended-integer>, y :: <extended-integer>)
    => (quo :: <extended-integer>, rem :: <extended-integer>);
  let x-len = x.bignum-size;
  let y-len = y.bignum-size;
  if (zero?(y))
    error("Division by zero.");
  elseif (x-len < y-len
	    | (x-len == y-len
		 & (bignum-digit(x, x-len - 1)
		      < bignum-digit(y, y-len - 1))))
    values(#e0, x);
  elseif (y-len == 1)
    divide-by-digit(x, bignum-digit(y, 0));
  else
    let shift = divisor-shift(y);
    let x = shift-for-division(x, shift);
    let y = ash(y, shift);
    let x-len = x.bignum-size;
    let y-len = y.bignum-size;
    let length = x-len - y-len;
    let quo = make-bignum(length);

    for (i :: <integer> from length - 1 to 0 by -1)
      let x1 = bignum-digit(x, i + y-len);
      let x2 = bignum-digit(x, i + y-len - 1);
      let x3 = bignum-digit(x, i + y-len - 2);
      let y1 = bignum-digit(y, y-len - 1);
      let y2 = bignum-digit(y, y-len - 2);
      let guess = division-guess(x1, x2, x3, y1, y2);

      let carry = make-digit(0);
      let borrow = $no-borrow;
      for (j :: <integer> from 0 below y-len)
	let (low, high) = digit-multiply(bignum-digit(y, j), guess);
	let (digit, sum-carry) = digit-add(low, carry, $no-carry);
	carry := digit-add(high, make-digit(0), sum-carry);
	let (digit, borrow-out)
	  = digit-subtract(bignum-digit(x, i + j), digit, borrow);
	bignum-digit(x, i + j) := digit;
	borrow := borrow-out;
      end;
      let (digit, borrow-out)
	= digit-subtract(bignum-digit(x, i + y-len), carry, borrow);
      bignum-digit(x, i + y-len) := digit;
      if (digit.digit-sign-bit-set?)
	guess := digit-subtract(guess, make-digit(1), $no-borrow);
	let carry-in = $no-carry;
	for (j :: <integer> from 0 below y-len)
	  let (digit, carry-out)
	    = digit-add(bignum-digit(x, i + j),
			bignum-digit(y, j),
			carry-in);
	  bignum-digit(x, i + j) := digit;
	  carry-in := carry-out;
	end;
	bignum-digit(x, i + y-len)
	  := digit-add(bignum-digit(x, i + y-len), make-digit(0), carry-in);
      end;
      bignum-digit(quo, i) := guess;
    end;
    values(normalize-bignum(quo, length),
	   ash(normalize-bignum(x, x-len), -shift));
  end;
end;

define method floor/ (x :: <extended-integer>, y :: <extended-integer>)
    => (quo :: <extended-integer>, rem :: <extended-integer>);
  let x-len = x.bignum-size;
  let y-len = y.bignum-size;
  if (x-len + y-len = 2)
    let (quo, rem) = floor/(bignum-digit(x, 0).as-signed, bignum-digit(y, 0).as-signed);
    values(as(<extended-integer>, quo), as(<extended-integer>, rem));
  elseif (x-len <= 2 & y-len <= 2)
    let (quo, rem) = floor/(as-signed-2(bignum-digit(x, 1), bignum-digit(x, 0)),
                            as-signed-2(bignum-digit(y, 1), bignum-digit(y, 0)));
    values(as(<extended-integer>, quo), as(<extended-integer>, rem));
  else
    let (x-abs, x-neg) = if (negative?(x)) values(-x, #t) else values(x, #f) end;
    let (y-abs, y-neg) = if (negative?(y)) values(-y, #t) else values(y, #f) end;
    let (quo, rem) = bignum-divide(x-abs, y-abs);
    if (x-neg == y-neg)
      values(quo, if (y-neg) -rem else rem end);
    elseif (zero?(rem))
      values(-quo, rem);
    else
      values(-1 - quo, if (y-neg) y + rem else y - rem end);
    end;
  end;
end;

define method ceiling/ (x :: <extended-integer>, y :: <extended-integer>)
    => (quo :: <extended-integer>, rem :: <extended-integer>);
  let x-len = x.bignum-size;
  let y-len = y.bignum-size;
  if (x-len + y-len = 2)
    let (quo, rem) = ceiling/(bignum-digit(x, 0).as-signed, bignum-digit(y, 0).as-signed);
    values(as(<extended-integer>, quo), as(<extended-integer>, rem));
  elseif (x-len <= 2 & y-len <= 2)
    let (quo, rem) = ceiling/(as-signed-2(bignum-digit(x, 1), bignum-digit(x, 0)),
                              as-signed-2(bignum-digit(y, 1), bignum-digit(y, 0)));
    values(as(<extended-integer>, quo), as(<extended-integer>, rem));
  else
    let (x-abs, x-neg) = if (negative?(x)) values(-x, #t) else values(x, #f) end;
    let (y-abs, y-neg) = if (negative?(y)) values(-y, #t) else values(y, #f) end;
    let (quo, rem) = bignum-divide(x-abs, y-abs);
    if (x-neg ~== y-neg)
      values(-quo, if (x-neg) -rem else rem end);
    elseif (zero?(rem))
      values(quo, rem);
    else
      values(1 + quo, (if (x-neg) -rem else rem end) - y);
    end;
  end;
end;

define method round/ (x :: <extended-integer>, y :: <extended-integer>)
    => (quo :: <extended-integer>, rem :: <extended-integer>);
  let x-len = x.bignum-size;
  let y-len = y.bignum-size;
  if (x-len + y-len = 2)
    let (quo, rem) = round/(bignum-digit(x, 0).as-signed, bignum-digit(y, 0).as-signed);
    values(as(<extended-integer>, quo), as(<extended-integer>, rem));
  elseif (x-len <= 2 & y-len <= 2)
    let (quo, rem) = round/(as-signed-2(bignum-digit(x, 1), bignum-digit(x, 0)),
                            as-signed-2(bignum-digit(y, 1), bignum-digit(y, 0)));
    values(as(<extended-integer>, quo), as(<extended-integer>, rem));
  else
    let (x-abs, x-neg) = if (negative?(x)) values(-x, #t) else values(x, #f) end;
    let (y-abs, y-neg) = if (negative?(y)) values(-y, #t) else values(y, #f) end;
    let (quo :: <extended-integer>, rem :: <extended-integer>)
      = bignum-divide(x-abs, y-abs);
    let twice-rem = rem + rem;
    if (twice-rem > y-abs | (twice-rem == y-abs & odd?(quo)))
      quo := quo + 1;
      rem := rem - y-abs;
    end;
    values(if (x-neg == y-neg) quo else -quo end,
           if (x-neg) -rem else rem end);
  end;
end;

define method truncate/ (x :: <extended-integer>, y :: <extended-integer>)
    => (quo :: <extended-integer>, rem :: <extended-integer>);
  let x-len = x.bignum-size;
  let y-len = y.bignum-size;
  if (x-len + y-len = 2)
    let (quo, rem) = truncate/(bignum-digit(x, 0).as-signed, bignum-digit(y, 0).as-signed);
    values(as(<extended-integer>, quo), as(<extended-integer>, rem));
  elseif (x-len <= 2 & y-len <= 2)
    let (quo, rem) = truncate/(as-signed-2(bignum-digit(x, 1), bignum-digit(x, 0)),
                               as-signed-2(bignum-digit(y, 1), bignum-digit(y, 0)));
    values(as(<extended-integer>, quo), as(<extended-integer>, rem));
  else
    let (x-abs, x-neg) = if (negative?(x)) values(-x, #t) else values(x, #f) end;
    let (y-abs, y-neg) = if (negative?(y)) values(-y, #t) else values(y, #f) end;
    let (quo, rem) = bignum-divide(x-abs, y-abs);
    values(if (x-neg == y-neg) quo else -quo end,
           if (x-neg) -rem else rem end);
  end;
end;


// ^

define method \^ (base :: <complex>, power :: <extended-integer>)
    => res :: <number>;
  if (negative?(power))
    1 / base ^ -power;
  else
    let len = power.bignum-size;
    let total = as(object-class(base), 1);
    for (digit-index :: <integer> from 0 below len)
      let digit = as-signed(bignum-digit(power, digit-index));
      for (bit-index :: <integer> from 0 below $digit-bits)
	if (logbit?(bit-index, digit))
	  total := base * total;
	end;
	base := base * base;
      end;
    end;
    total;
  end;
end;


// GCD

// gcd -- exported generic function method
//
// Compute the gcd of x and y.  This algorithm has its origins in Knuth, so if
// you want to know why this results in the gcd, then check there, because I
// sure as hell don't know.
// 
define method gcd (x :: <extended-integer>, y :: <extended-integer>)
    => res :: <extended-integer>;
  let x-len = x.bignum-size;
  let y-len = y.bignum-size;
  let x-dig = bignum-digit(x, 0);
  let y-dig = bignum-digit(y, 0);
  if (x-len = 1 & x-dig.value.zero?)
    y;
  elseif (y-len = 1 & y-dig.value.zero?)
    x;
  elseif (x-len + y-len = 2)
    as(<extended-integer>,
       gcd(x-dig.as-signed, y-dig.as-signed));
  elseif (x-len <= 2 & y-len <= 2)
    let xv = if (x-len = 1)
               x-dig.as-signed;
             else
               as-signed-2(bignum-digit(x, 1), x-dig);
             end;
    let yv = if (y-len = 1)
               y-dig.as-signed;
             else
               as-signed-2(bignum-digit(y, 1), y-dig);
             end;
    as(<extended-integer>, gcd(xv, yv));
  else
    let x = if (negative?(x)) -x else copy-bignum(x) end;
    let y = if (negative?(y)) -y else copy-bignum(y) end;
    let (x-len, x-shift) = shift-until-odd(x, x.bignum-size);
    let (y-len, y-shift) = shift-until-odd(y, y.bignum-size);
    let factors-of-two = min(x-shift, y-shift);
    block (return)
      for (while: #t)
        select (three-way-compare(x, x-len, y, y-len))
          #"equal" =>
            return(ash(normalize-bignum(x, x-len), factors-of-two));
          #"less" =>
            y-len := subtract-in-place(y, y-len, x, x-len);
            y-len := shift-until-odd(y, y-len);
          #"greater" =>
            x-len := subtract-in-place(x, x-len, y, y-len);
            x-len := shift-until-odd(x, x-len);
        end;
      end;
    end;
  end;
end;

// copy-bignum -- internal
//
// Make a copy of x so that we can destructively modify it.
//
define method copy-bignum (x :: <extended-integer>)
    => res :: <extended-integer>;
  let len = x.bignum-size;
  let res = make-bignum(len);
  for (index :: <integer> from 0 below len)
    bignum-digit(res, index) := bignum-digit(x, index);
  end;
  res;
end;

// shift-until-odd -- internal.
//
// Shift x right until it is odd, returning the new length and the number of
// bits shifted.  X is guaranteed to be positive, so we don't have to worry
// about sign extending it or trying to shift 0 until it becomes odd.
// 
define method shift-until-odd (x :: <extended-integer>, len :: <integer>)
    => (new-len :: <integer>, shift :: <integer>);
  let digits = for (index :: <integer> from 0 below len,
		    while: bignum-digit(x, index) == make-digit(0))
	       finally
		 index;
	       end;
  let bits = for (index :: <integer> from 0,
		  digit = bignum-digit(x, digits)
		    then digit-shift(make-digit(0), digit, $digit-bits - 1),
		  while: even?(as-signed(digit)))
	     finally
	       index;
	     end;
  if (zero?(bits))
    if (zero?(digits))
      values(len, 0);
    else
      let new-len = len - digits;
      for (index :: <integer> from 0 below new-len)
	bignum-digit(x, index) := bignum-digit(x, index + digits);
      end;
      values(new-len, digits * $digit-bits);
    end;
  else
    let new-len = len - digits;
    let prev-digit = bignum-digit(x, digits);
    for (index :: <integer> from 1 below new-len)
      let next-digit = bignum-digit(x, index + digits);
      bignum-digit(x, index - 1)
	:= digit-shift(next-digit, prev-digit, $digit-bits - bits);
      prev-digit := next-digit;
    end;
    bignum-digit(x, new-len - 1)
      := digit-shift(make-digit(0), prev-digit, $digit-bits - bits);
    values(normalized-length(x, new-len), digits * $digit-bits + bits);
  end;
end;

// three-way-compare -- internal.
//
// Do a three-way compare of x and y (which are guaranteed to be positive).
// 
define method three-way-compare
    (x :: <extended-integer>, x-len :: <integer>,
     y :: <extended-integer>, y-len :: <integer>)
    => res :: one-of(#"less", #"equal", #"greater");
  if (x-len == y-len)
    block (return)
      for (index :: <integer> from x-len - 1 to 0 by -1)
	let x-digit = bignum-digit(x, index);
	let y-digit = bignum-digit(y, index);
	if (x-digit < y-digit)
	  return(#"less");
	elseif (y-digit < x-digit)
	  return(#"greater");
	end;
      finally
	return(#"equal");
      end;
    end;
  elseif (x-len < y-len)
    #"less";
  else
    #"greater";
  end;
end;

// subtract-in-place -- internal
//
// Subtract smaller from larger, modifying larger.  Both numbers are guaranteed
// to be positive, so we don't have to worry about the result becoming
// negative.
//
define method subtract-in-place (larger :: <extended-integer>,
				 larger-len :: <integer>,
				 smaller :: <extended-integer>,
				 smaller-len :: <integer>)
    => res-len :: <integer>;
  let borrow-in = $no-borrow;
  for (index :: <integer> from 0 below smaller-len)
    let larger-digit = bignum-digit(larger, index);
    let smaller-digit = bignum-digit(smaller, index);
    let (digit, borrow-out)
      = digit-subtract(larger-digit, smaller-digit, borrow-in);
    bignum-digit(larger, index) := digit;
    borrow-in := borrow-out;
  end;
  for (index :: <integer> from smaller-len below larger-len)
    let larger-digit = bignum-digit(larger, index);
    let (digit, borrow-out)
      = digit-subtract(larger-digit, make-digit(0), borrow-in);
    bignum-digit(larger, index) := digit;
    borrow-in := borrow-out;
  end;
  normalized-length(larger, larger-len);
end;



// Bitwise logical.

define method binary-logior (a :: <extended-integer>, b :: <extended-integer>)
    => res :: <extended-integer>;
  let a-len = a.bignum-size;
  let b-len = b.bignum-size;
  let (shorter, shorter-len, longer, longer-len)
    = if (a-len < b-len)
	values(a, a-len, b, b-len);
      else
	values(b, b-len, a, a-len);
      end;
  if (negative?(shorter))
    let res = make-bignum(shorter-len);
    for (index :: <integer> from 0 below shorter-len)
      bignum-digit(res, index)
	:= digit-logior(bignum-digit(longer, index),
			bignum-digit(shorter, index));
    end;
    normalize-bignum(res, shorter-len);
  else
    let res = make-bignum(longer-len);
    for (index :: <integer> from 0 below shorter-len)
      bignum-digit(res, index)
	:= digit-logior(bignum-digit(longer, index),
			bignum-digit(shorter, index));
    end;
    for (index :: <integer> from shorter-len below longer-len)
      bignum-digit(res, index) := bignum-digit(longer, index);
    end;
    normalize-bignum(res, longer-len);
  end;
end;

define method binary-logxor (a :: <extended-integer>, b :: <extended-integer>)
    => res :: <extended-integer>;
  let a-len = a.bignum-size;
  let b-len = b.bignum-size;
  let (shorter, shorter-len, longer, longer-len)
    = if (a-len < b-len)
	values(a, a-len, b, b-len);
      else
	values(b, b-len, a, a-len);
      end;
  let res = make-bignum(longer-len);
  for (index :: <integer> from 0 below shorter-len)
    bignum-digit(res, index)
      := digit-logxor(bignum-digit(longer, index),
		      bignum-digit(shorter, index));
  end;
  let shorter-sign
    = sign-extend-digit(bignum-digit(shorter, shorter-len - 1));
  for (index :: <integer> from shorter-len below longer-len)
    bignum-digit(res, index)
      := digit-logxor(bignum-digit(longer, index), shorter-sign);
  end;
  normalize-bignum(res, longer-len);
end;

define method binary-logand (a :: <extended-integer>, b :: <extended-integer>)
    => res :: <extended-integer>;
  let a-len = a.bignum-size;
  let b-len = b.bignum-size;
  let (shorter, shorter-len, longer, longer-len)
    = if (a-len < b-len)
	values(a, a-len, b, b-len);
      else
	values(b, b-len, a, a-len);
      end;
  if (negative?(shorter))
    let res = make-bignum(longer-len);
    for (index :: <integer> from 0 below shorter-len)
      bignum-digit(res, index)
	:= digit-logand(bignum-digit(longer, index),
			bignum-digit(shorter, index));
    end;
    for (index :: <integer> from shorter-len below longer-len)
      bignum-digit(res, index) := bignum-digit(longer, index);
    end;
    normalize-bignum(res, longer-len);
  else
    let res = make-bignum(shorter-len);
    for (index :: <integer> from 0 below shorter-len)
      bignum-digit(res, index)
	:= digit-logand(bignum-digit(longer, index),
			bignum-digit(shorter, index));
    end;
    normalize-bignum(res, shorter-len);
  end;
end;

define method lognot (num :: <extended-integer>)
    => res :: <extended-integer>;
  let len = num.bignum-size;
  let res = make-bignum(len);
  for (index :: <integer> from 0 below len)
    bignum-digit(res, index) := digit-lognot(bignum-digit(num, index));
  end;
  // We don't have to normalize it, because we can assume that num started
  // normalized.
  res;
end;

define method logbit? (index :: <integer>, num :: <extended-integer>)
    => res :: <boolean>;
  let (digit-index, bit-index) = floor/(index, $digit-bits);
  if (digit-index >= 0 & digit-index < num.bignum-size)
    let digit = bignum-digit(num, digit-index);
    logbit?(bit-index, as-signed(digit));
  else
    #f;
  end;
end;


// Shifting.

define method ash (num :: <extended-integer>, shift :: <integer>)
    => res :: <extended-integer>;
  if (zero?(shift))
    num;
  else
    let len = num.bignum-size;
    let (digits, bits) = floor/(shift, $digit-bits);
    if (zero?(bits))
      let res-len = len + digits;
      if (positive?(res-len))
	let res = make-bignum(len + digits);
	if (negative?(digits))
	  for (index :: <integer> from 0 below len + digits)
	    bignum-digit(res, index) := bignum-digit(num, index - digits);
	  end;
	else
	  for (index :: <integer> from 0 below digits)
	    bignum-digit(res, index) := make-digit(0);
	  end;
	  for (index :: <integer> from 0 below len)
	    bignum-digit(res, index + digits) := bignum-digit(num, index);
	  end;
	end;
	// We don't have to normalize it, because we can assume the original
	// number was normalized.
	res;
      else
	let res = make-bignum(1);
	bignum-digit(res, 0) := sign-extend-digit(bignum-digit(num, len - 1));
	res;
      end;
    else
      let res-len = len + digits + 1;
      if (positive?(res-len))
	let res = make-bignum(res-len);
	if (negative?(digits))
	  let prev-digit = bignum-digit(num, -1 - digits);
	  for (index :: <integer> from 0 below len + digits)
	    let next-digit = bignum-digit(num, index - digits);
	    bignum-digit(res, index)
	      := digit-shift(next-digit, prev-digit, bits);
	    prev-digit := next-digit;
	  end;
	  bignum-digit(res, len + digits)
	    := digit-shift(sign-extend-digit(prev-digit), prev-digit, bits);
	else
	  for (index :: <integer> from 0 below digits)
	    bignum-digit(res, index) := make-digit(0);
	  end;
	  let prev-digit = make-digit(0);
	  for (index :: <integer> from 0 below len)
	    let next-digit = bignum-digit(num, index);
	    bignum-digit(res, index + digits)
	      := digit-shift(next-digit, prev-digit, bits);
	    prev-digit := next-digit;
	  end;
	  bignum-digit(res, len + digits)
	    := digit-shift(sign-extend-digit(prev-digit), prev-digit, bits);
	end;
	normalize-bignum(res, res-len);
      else
	let res = make-bignum(1);
	bignum-digit(res, 0) := sign-extend-digit(bignum-digit(num, len - 1));
	res;
      end;
    end;
  end;
end;


// Integer-length

// integer-length{<extended-integer>}
//
// Return the number of ``interesting'' bits in x.  The interesting bits
// are all but the sign bits.
//
define method integer-length (num :: <extended-integer>)
    => res :: <integer>;
  let len-1 = num.bignum-size - 1;
  integer-length(as-signed(bignum-digit(num, len-1))) + len-1 * $digit-bits;
end method integer-length;


// Contagion methods.

define inline method \= (num1 :: <extended-integer>, num2 :: <integer>)
    => res :: <boolean>;
  num1 == as(<extended-integer>, num2);
end;

define inline method \= (num1 :: <integer>, num2 :: <extended-integer>)
    => res :: <boolean>;
  as(<extended-integer>, num1) == num2;
end;

define inline method \= (num1 :: <extended-integer>, num2 :: <single-float>)
    => res :: <boolean>;
  as(<single-float>, num1) = num2;
end;

define inline method \= (num1 :: <single-float>, num2 :: <extended-integer>)
    => res :: <boolean>;
  num1 = as(<single-float>, num2);
end;

define inline method \= (num1 :: <extended-integer>, num2 :: <double-float>)
    => res :: <boolean>;
  as(<double-float>, num1) = num2;
end;

define inline method \= (num1 :: <double-float>, num2 :: <extended-integer>)
    => res :: <boolean>;
  num1 = as(<double-float>, num2);
end;

define inline method \= (num1 :: <extended-integer>, num2 :: <extended-float>)
    => res :: <boolean>;
  as(<extended-float>, num1) = num2;
end;

define inline method \= (num1 :: <extended-float>, num2 :: <extended-integer>)
    => res :: <boolean>;
  num1 = as(<extended-float>, num2);
end;

define inline method \< (num1 :: <extended-integer>, num2 :: <integer>)
    => res :: <boolean>;
  num1 < as(<extended-integer>, num2);
end;

define inline method \< (num1 :: <integer>, num2 :: <extended-integer>)
    => res :: <boolean>;
  as(<extended-integer>, num1) < num2;
end;

define inline method \< (num1 :: <extended-integer>, num2 :: <single-float>)
    => res :: <boolean>;
  as(<single-float>, num1) < num2;
end;

define inline method \< (num1 :: <single-float>, num2 :: <extended-integer>)
    => res :: <boolean>;
  num1 < as(<single-float>, num2);
end;

define inline method \< (num1 :: <extended-integer>, num2 :: <double-float>)
    => res :: <boolean>;
  as(<double-float>, num1) < num2;
end;

define inline method \< (num1 :: <double-float>, num2 :: <extended-integer>)
    => res :: <boolean>;
  num1 < as(<double-float>, num2);
end;

define inline method \< (num1 :: <extended-integer>, num2 :: <extended-float>)
    => res :: <boolean>;
  as(<extended-float>, num1) < num2;
end;

define inline method \< (num1 :: <extended-float>, num2 :: <extended-integer>)
    => res :: <boolean>;
  num1 < as(<extended-float>, num2);
end;

define inline method \<= (a :: <extended-integer>, b :: <single-float>)
    => res :: <boolean>;
  as(<single-float>, a) <= b;
end;

define inline method \<= (a :: <single-float>, b :: <extended-integer>)
    => res :: <boolean>;
  a <= as(<single-float>, b);
end;

define inline method \<= (a :: <extended-integer>, b :: <double-float>)
    => res :: <boolean>;
  as(<double-float>, a) <= b;
end;

define inline method \<= (a :: <double-float>, b :: <extended-integer>)
    => res :: <boolean>;
  a <= as(<double-float>, b);
end;

define inline method \<= (a :: <extended-integer>, b :: <extended-float>)
    => res :: <boolean>;
  as(<extended-float>, a) <= b;
end;

define inline method \<= (a :: <extended-float>, b :: <extended-integer>)
    => res :: <boolean>;
  a <= as(<extended-float>, b);
end;

define inline method \~= (a :: <extended-integer>, b :: <single-float>)
    => res :: <boolean>;
  as(<single-float>, a) ~= b;
end;

define inline method \~= (a :: <single-float>, b :: <extended-integer>)
    => res :: <boolean>;
  a ~= as(<single-float>, b);
end;

define inline method \~= (a :: <extended-integer>, b :: <double-float>)
    => res :: <boolean>;
  as(<double-float>, a) ~= b;
end;

define inline method \~= (a :: <double-float>, b :: <extended-integer>)
    => res :: <boolean>;
  a ~= as(<double-float>, b);
end;

define inline method \~= (a :: <extended-integer>, b :: <extended-float>)
    => res :: <boolean>;
  as(<extended-float>, a) ~= b;
end;

define inline method \~= (a :: <extended-float>, b :: <extended-integer>)
    => res :: <boolean>;
  a ~= as(<extended-float>, b);
end;

define inline method \+ (a :: <extended-integer>, b :: <integer>)
    => res :: <extended-integer>;
  a + as(<extended-integer>, b);
end;

define inline method \+ (a :: <integer>, b :: <extended-integer>)
    => res :: <extended-integer>;
  as(<extended-integer>, a) + b;
end;

define inline method \+ (a :: <extended-integer>, b :: <single-float>)
    => res :: <single-float>;
  as(<single-float>, a) + b;
end;

define inline method \+ (a :: <single-float>, b :: <extended-integer>)
    => res :: <single-float>;
  a + as(<single-float>, b);
end;

define inline method \+ (a :: <extended-integer>, b :: <double-float>)
    => res :: <double-float>;
  as(<double-float>, a) + b;
end;

define inline method \+ (a :: <double-float>, b :: <extended-integer>)
    => res :: <double-float>;
  a + as(<double-float>, b);
end;

define inline method \+ (a :: <extended-integer>, b :: <extended-float>)
    => res :: <extended-float>;
  as(<extended-float>, a) + b;
end;

define inline method \+ (a :: <extended-float>, b :: <extended-integer>)
    => res :: <extended-float>;
  a + as(<extended-float>, b);
end;

define inline method \* (a :: <extended-integer>, b :: <integer>)
    => res :: <extended-integer>;
  a * as(<extended-integer>, b);
end;

define inline method \* (a :: <integer>, b :: <extended-integer>)
    => res :: <extended-integer>;
  as(<extended-integer>, a) * b;
end;

define inline method \* (a :: <extended-integer>, b :: <single-float>)
    => res :: <single-float>;
  as(<single-float>, a) * b;
end;

define inline method \* (a :: <single-float>, b :: <extended-integer>)
    => res :: <single-float>;
  a * as(<single-float>, b);
end;

define inline method \* (a :: <extended-integer>, b :: <double-float>)
    => res :: <double-float>;
  as(<double-float>, a) * b;
end;

define inline method \* (a :: <double-float>, b :: <extended-integer>)
    => res :: <double-float>;
  a * as(<double-float>, b);
end;

define inline method \* (a :: <extended-integer>, b :: <extended-float>)
    => res :: <extended-float>;
  as(<extended-float>, a) * b;
end;

define inline method \* (a :: <extended-float>, b :: <extended-integer>)
    => res :: <extended-float>;
  a * as(<extended-float>, b);
end;

define inline method \- (a :: <extended-integer>, b :: <integer>)
    => res :: <extended-integer>;
  a - as(<extended-integer>, b);
end;

define inline method \- (a :: <integer>, b :: <extended-integer>)
    => res :: <extended-integer>;
  as(<extended-integer>, a) - b;
end;

define inline method \- (a :: <extended-integer>, b :: <single-float>)
    => res :: <single-float>;
  as(<single-float>, a) - b;
end;

define inline method \- (a :: <single-float>, b :: <extended-integer>)
    => res :: <single-float>;
  a - as(<single-float>, b);
end;

define inline method \- (a :: <extended-integer>, b :: <double-float>)
    => res :: <double-float>;
  as(<double-float>, a) - b;
end;

define inline method \- (a :: <double-float>, b :: <extended-integer>)
    => res :: <double-float>;
  a - as(<double-float>, b);
end;

define inline method \- (a :: <extended-integer>, b :: <extended-float>)
    => res :: <extended-float>;
  as(<extended-float>, a) - b;
end;

define inline method \- (a :: <extended-float>, b :: <extended-integer>)
    => res :: <extended-float>;
  a - as(<extended-float>, b);
end;

define inline method \/ (a :: <extended-integer>, b :: <single-float>)
    => res :: <single-float>;
  as(<single-float>, a) / b;
end;

define inline method \/ (a :: <single-float>, b :: <extended-integer>)
    => res :: <single-float>;
  a / as(<single-float>, b);
end;

define inline method \/ (a :: <extended-integer>, b :: <double-float>)
    => res :: <double-float>;
  as(<double-float>, a) / b;
end;

define inline method \/ (a :: <double-float>, b :: <extended-integer>)
    => res :: <double-float>;
  a / as(<double-float>, b);
end;

define inline method \/ (a :: <extended-integer>, b :: <extended-float>)
    => res :: <extended-float>;
  as(<extended-float>, a) / b;
end;

define inline method \/ (a :: <extended-float>, b :: <extended-integer>)
    => res :: <extended-float>;
  a / as(<extended-float>, b);
end;

define inline method floor/ (a :: <extended-integer>, b :: <integer>)
    => (quo :: <extended-integer>, rem :: <extended-integer>);
  floor/(a, as(<extended-integer>, b));
end;

define inline method floor/ (a :: <integer>, b :: <extended-integer>)
    => (quo :: <extended-integer>, rem :: <extended-integer>);
  floor/(as(<extended-integer>, a), b);
end;

define inline method ceiling/ (a :: <extended-integer>, b :: <integer>)
    => (quo :: <extended-integer>, rem :: <extended-integer>);
  ceiling/(a, as(<extended-integer>, b));
end;

define inline method ceiling/ (a :: <integer>, b :: <extended-integer>)
    => (quo :: <extended-integer>, rem :: <extended-integer>);
  ceiling/(as(<extended-integer>, a), b);
end;

define inline method round/ (a :: <extended-integer>, b :: <integer>)
    => (quo :: <extended-integer>, rem :: <extended-integer>);
  round/(a, as(<extended-integer>, b));
end;

define inline method round/ (a :: <integer>, b :: <extended-integer>)
    => (quo :: <extended-integer>, rem :: <extended-integer>);
  round/(as(<extended-integer>, a), b);
end;

define inline method truncate/ (a :: <extended-integer>, b :: <integer>)
    => (quo :: <extended-integer>, rem :: <extended-integer>);
  truncate/(a, as(<extended-integer>, b));
end;

define inline method truncate/ (a :: <integer>, b :: <extended-integer>)
    => (quo :: <extended-integer>, rem :: <extended-integer>);
  truncate/(as(<extended-integer>, a), b);
end;

define inline method binary-logior
    (a :: <extended-integer>, b :: <integer>)
    => res :: <extended-integer>;
  binary-logior(a, as(<extended-integer>, b));
end;

define inline method binary-logior
    (a :: <integer>, b :: <extended-integer>)
    => res :: <extended-integer>;
  binary-logior(as(<extended-integer>, a), b);
end;

define inline method binary-logxor
    (a :: <extended-integer>, b :: <integer>)
    => res :: <extended-integer>;
  binary-logxor(a, as(<extended-integer>, b));
end;

define inline method binary-logxor
    (a :: <integer>, b :: <extended-integer>)
    => res :: <extended-integer>;
  binary-logxor(as(<extended-integer>, a), b);
end;

define inline method binary-logand
    (a :: <extended-integer>, b :: <integer>)
    => res :: <extended-integer>;
  binary-logand(a, as(<extended-integer>, b));
end;

define inline method binary-logand
    (a :: <integer>, b :: <extended-integer>)
    => res :: <extended-integer>;
  binary-logand(as(<extended-integer>, a), b);
end;
