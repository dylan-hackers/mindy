module:     Random
author:     dpierce@cs.cmu.edu
synopsis:   This file implements random numbers for the Gwydion
            implementation of Dylan.
copyright:  Copyright (C) 1994, Carnegie Mellon University.
            All rights reserved.
rcs-header: $Header: /home/housel/work/rcs/gd/src/mindy/libraries/random/Attic/distributions.dylan,v 1.2 1994/06/28 23:57:55 wlott Exp $

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


/* Random Number Distributions

   This file contains class definitions and functions for using random
   number distributions.  The basis of these methods is the generation
   of a uniform distribution on the interval [0, 1) using the Linear
   Congruential Method.

   Other probability distributions are generated using transformations
   of the basic unit uniform distribution.  Some that are defined here
   are other uniform distribution, normal, and exponential
   distributions.

   This implementation defines an abstract class
   <random-distribution>.  All subclasses of <random-distribution>
   must define a method for the generic function RANDOM.  This
   function returns a random number in the distribution.

*/

// <random-distribution> -- public
//
// Abstract superclass of random distributions.  Each subclass must
// define a method for RANDOM, which returns a random number from the
// distribution.
//
define abstract class <random-distribution> (<object>) end class;


// random -- public
//
// Methods on this function return a random number generated from
// whichever distribution is being used.
//
define generic random (random-distribution :: <random-distribution>)
   => random :: <real>;



/* Unit Uniform Distribution

		      Linear Congruential Method

   The basic algorithm used to generate random numbers is called the
   Linear Congruential Method (see, for example, Sedgewick,
   Algorithms, chapter 35).  The Linear Congruential method uses a
   recurrence

		       z  = a z    + 1 (mod m)
                        k      k-1

   to generate pseudo-random sequences of integers uniformly
   distributed between 0 and m - 1.  The first z is called the seed of
   the sequence.

   There are some basic rules for the values of the constants (see
   Sedgewick).  The value of m should be large.  Since Mindy has 30
   bit integers, we take m to be

                                     28
				m = 2

                                  = 268 435 456

   The value of a should not be as large, perhaps one digit shorter
   than m, and it should end with the three digits x21 where x is
   even.  So we take a (arbitrarily) to be

			        a = 29 413 621

   Now, we cannot calculate each z directly because the calculation
   would overflow the integer size.  Instead we multiply (a z) in
   parts.  We take k to be the square root of m.  Then if

		   p = p1 k + p0 and q = q1 k + q0

   then

	     p q = (p1 q1) m + (p1 q0 + p0 q1) k + p0 q0

   and

      p q (mod m) = ((p1 q0 + p0 q1 (mod k)) k + p0 q0) (mod m)

   Using this method we can calculate (a z) (mod m) without overflow.

   Now a uniform distribution on [0, 1) can be found by dividing the
   random variable z by m.

*/

// <uniform-distribution> -- public
// 
// Abstract superclass of uniform distributions.  Uniform
// distributions are distributions such that every element of the
// range should appear with equal frequency.
//
define abstract class <uniform-distribution> (<random-distribution>) end class;


// <unit-uniform-distribution> -- public
//
// This concrete class provides the implementation of the basic
// uniform random number distribution on [0, 1).  It uses the linear
// congruential method.
//
define class <unit-uniform-distribution> (<uniform-distribution>)
   slot random-seed :: <integer>,
      init-function: method () *dylan-random-seed* end method,
      init-keyword: seed:;
end class;


// random -- public
// 
// Returns a random number from the unit uniform distribution.  Uses
// the linear congruential method described above.  An integer between
// 0 and m - 1 is generated.  This is put into the RANDOM-SEED slot of
// the distribution and then divided by m to produce a real between 0
// and 1.
//

define constant $a$ = 29413621;
define constant $m$ = expt (2, 28);
define constant $k$ = expt (2, 14);
define constant a1 = floor/ ($a$, $k$);
define constant a0 = modulo ($a$, $k$);

define method random (dist :: <unit-uniform-distribution>)
      => random :: <double-float>;
   let z = dist.random-seed;
   let z1 = floor/ (z, $k$);
   let z0 = modulo (z, $k$);
   let r = modulo (modulo (a1 * z0 + a0 * z1, $k$) * $k$ + a0 * z0 , $m$);
   dist.random-seed := r;
   as (<double-float>, r) / as (<double-float>, $m$)
end method;



/* Other Uniform Distributions

   Most other random distribution generators rely on the unit uniform
   generator.

   A uniform distribution of reals over an arbitrary interval [a, b)
   can be obtained from the unit uniform random variable U by

			  R = (b - a) U + a

   Similarly, a uniform distribution of integers over an arbitrary
   interval [a, b] can be obtained from the unit uniform random
   variable U by

			  I = round ((b - a) U + a)

*/

// <real-uniform-distribution> -- public
//
// The concrete class for real uniform distributions.  Slots for the
// beginning and ending point of the distribution interval.
//
define class <real-uniform-distribution> (<uniform-distribution>)
   slot unit-uniform :: <unit-uniform-distribution>;
   slot random-from :: <real>,
      required-init-keyword: from:;
   slot random-to :: <real>,
      required-init-keyword: to:;
end class;


// initialize -- interface
//
// The unit uniform distribution used in the uniform has to be set up.
//
define method initialize (dist :: <real-uniform-distribution>,
			  #key seed = *dylan-random-seed*)
   dist.unit-uniform := make (<unit-uniform-distribution>, seed: seed);
end method;


// random -- public
//
// Generates real numbers which are distributed uniformly in some
// interval.  Uses the unit uniform generator, and applies the linear
// transformation described above.
//
define method random (dist :: <real-uniform-distribution>)
      => random :: <double-float>;
   (dist.random-to - dist.random-from) * random (dist.unit-uniform)
      + dist.random-from
end method;


// <integer-uniform-distribution> -- public
//
// The concrete class for integer uniform distributions.  Slots for
// the beginning and ending points of the distribution interval.
//
define class <integer-uniform-distribution> (<uniform-distribution>)
   slot unit-uniform :: <unit-uniform-distribution>;
   slot random-from :: <integer>,
      required-init-keyword: from:;
   slot random-to :: <integer>,
      required-init-keyword: to:;
end class;


// initialize -- interface
//
// The unit uniform distribution used in the uniform has to be set up.
//
define method initialize (dist :: <integer-uniform-distribution>,
			  #key seed = *dylan-random-seed*)
   dist.unit-uniform := make (<unit-uniform-distribution>, seed: seed);
end method;


// random -- public
//
// Generates integers which are distributed uniformly in some
// interval.  Uses the unit uniform generator, and applies the linear
// transformation described above.
//
// Note: The endpoints of the interval are both inclusive because of
// the rounding.  That is, the numbers generate are in [a, b] instead
// of [a, b).
//
define method random (dist :: <integer-uniform-distribution>)
      => random :: <integer>;
   round ((dist.random-to - dist.random-from) * random (dist.unit-uniform)
	     + dist.random-from)
end method;



/* Exponential Distribution

   The exponential distribution has a cumulative distribution function
   described by

                                   -lx
		       F(x) = 1 - e      x > 0

   An exponential distribution X can be generated from a unit uniform
   distribution U using a transformation.  That is, a number u from
   the uniform is chosen, then the inverse of the CDF is applied

                                    1
			   X(u) = - - ln(u)
                                    l

*/

// <exponential-distribution> -- public
//
// The concrete class for exponential distributions.  Slot for the
// lambda parameter of the distribution.
//
define class <exponential-distribution> (<random-distribution>)
   slot unit-uniform :: <unit-uniform-distribution>;
   slot lambda :: <real>,
      init-value: 1,
      init-keyword: lambda:;
end class;


// initialize -- interface
//
// The unit uniform distribution used in the exponential has to be set
// up.
//
define method initialize (dist :: <exponential-distribution>,
			  #key seed = *dylan-random-seed*)
   dist.unit-uniform := make (<unit-uniform-distribution>, seed: seed);
end method;


// random -- public
//
// Generates numbers distributed in an exponential distribution with
// parameter lambda.  Applies the inverse CDF of the exponential
// distribution to a number generated from a unit uniform
// distribution.
//
define method random (dist :: <exponential-distribution>)
      => random :: <double-float>;
   - (log (random (dist.unit-uniform)) / dist.lambda)
end method;



/* Normal Distribution

   The normal (or Gaussian) probability distribution has a very
   complicated probability density function.  So the methods used to
   generate it are somewhat obscure.  The normal distribution can be
   generated using two uniform distributions, A and B.  Numbers from a
   normal distribution with mean 0 and standard deviation (or sigma) 1
   can be found using this formula:

                                  1/2
		   X = (- 2 ln(A))    cos(2 pi B).

   This distribution can be transformed to a distribution with mean m
   and sigma o by a linear function.

			     Y = o X + m

*/

// <normal-distribution> -- public
//
// The concrete class for normal distributions.  Slots for the mean
// and standard deviation (sigma) parameters of the distribution.
//
define class <normal-distribution> (<random-distribution>)
   slot unit-uniform-A :: <unit-uniform-distribution>;
   slot unit-uniform-B :: <unit-uniform-distribution>;
   slot mean :: <real>,
      init-value: 0,
      init-keyword: mean:;
   slot sigma :: <real>,
      init-value: 1,
      init-keyword: sigma:;
end class;


// initialize -- interface
//
// Both unit uniform distributions used in the normal have to be set
// up.  The first is seeded from the seed given, and the second is
// seeded from a random number generated by the first.
//
define method initialize (dist :: <normal-distribution>,
			  #key seed = *dylan-random-seed*)
   dist.unit-uniform-A := make (<unit-uniform-distribution>, seed: seed);
   dist.unit-uniform-B := make (<unit-uniform-distribution>,
				seed: random (dist.unit-uniform-A));
end method;


// random -- public
// 
// Generates a normal distribution with mean 0 and sigma 1 from two
// numbers chosen from a unit uniform distribution, then applies the
// linear transformation to adjust to the real mean and sigma of the
// desired distribution.
// 
// (The constant $pi$ should be predefined or be defined as 4 *
// atan(1) but we don't have atan.)
//
define constant $pi$ = 3.14159265358975;
//
define method random (dist :: <normal-distribution>)
      => random :: <double-float>;
   let unit-normal-random = sqrt (-2 * log (random (dist.unit-uniform-A)))
      * cos (2 * $pi$ * random (dist.unit-uniform-B));
   dist.sigma * unit-normal-random + dist.mean
end method;



/* Dylan Global Random Distribution

   In addition to the variety of random distributions this library
   provides, we also want to have simpler functions for people to use
   without having to create a distribution.

   Global variables hold a default Dylan random seed and distribution.
   The function RANDOM-UNIFORM returns a number in some uniform
   distribution.  The function SEED-RANDOM! allows the user to set the
   global random seed.

*/

// *dylan-random-seed* -- public
// 
// This global variable is used as the default seed for
// <unit-uniform-distribution>s (and thus serves as the default seed
// for most other random distributions.
//
define variable *dylan-random-seed* = 42424242;


// *dylan-random-distribution* -- public
// 
// This global variable stores the default random distribution that is
// used for the following functions.  This gives the user the
// alternative of not having to create their own distributions.
//
define variable *dylan-random-distribution* =
   make (<unit-uniform-distribution>, seed: *dylan-random-seed*);


// random-uniform -- public
// 
// This function returns a random number from a uniform distribution.
// The bounds of the uniform distribution are given by the keywords
// from: and to:.  The type of the number returned is determined by
// the type of the bounds.  (If the bounds do not have the same type
// an error is signalled.)
// 
// If the bounds are <integer>, the random number is rounded.  If the
// bounds are some other type (such as <single-float>, etc.), the
// random number is coerced to that type.
// 
// This function uses *DYLAN-RANDOM-DISTRIBUTION* to generate the
// random number.
//
define constant random-uniform =
   method (#key from: from-bound = 0.0d0, to: to-bound = 1.0d0)
      if (~ object-class (from-bound) == object-class (to-bound))
	 error ("Arguments to random-uniform must have same type.");
      end if;
      let random = (to-bound - from-bound)
	 * random (*dylan-random-distribution*)
	 + from-bound;
      select (object-class (to-bound))
	 <integer> =>
	    round (random);
	 otherwise =>
	    as (object-class (to-bound), random);
      end select;
   end method;


// seed-random! -- public
//
// This functions seeds the default Dylan distribution.
//
define constant seed-random! =
   method (seed :: <integer>)
      if (seed > 0)
	 *dylan-random-seed* := seed;
	 *dylan-random-distribution* :=
	    make (<unit-uniform-distribution>, seed: *dylan-random-seed*);
      else
	 error ("Random seed must be > 0: %d", seed);
      end if;
      *dylan-random-seed*
   end method;



/* Chi-Square Test for the Random Number Generator

   The chi-square function can be used to test the integrity of the
   underlying linear congruential generator.  It takes a
   <integer-uniform-distribution> (which should be a distribution on
   an interval [0, r]) and calculates its chi square value.

*/

// chi-square -- public
// 
// The chi square value of a integer uniform distribution on [0, r] is
// found by taking the sum of the squares of the difference between
// the frequencies of the elements of the distribution and the mean
// frequency.  The mean frequency is the number of samples divided by
// the number of elements in the interval (N / r).  For this to work,
// N should be at least 10 r.
// 
// This function sets up a frequency array and generates N random
// numbers and fills in their frequencies.  It then calculates the chi
// square value of the distribution.
// 
// The chi square value should be no farther from r than twice the
// square root of r.
//
define method chi-square (dist :: <integer-uniform-distribution>)
   let r = dist.random-to;
   let N = 10 * r;
   let f = as (<double-float>, N) / as (<double-float>, r);
   let freq = make (<vector>, size: r + 1, fill: 0);
   for (i from 0 below N)
      let d = random (dist);
      freq[d] := freq[d] + 1;
   end for;
   for (i from 0 below r,
	sample-sum = 0.0 then sample-sum + expt (freq[i] - f, 2))
   finally
      sample-sum / f;
   end for;
end method;
