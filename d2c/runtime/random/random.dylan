rcs-header: $Header: /scm/cvs/src/d2c/runtime/random/random.dylan,v 1.5 2003/10/18 04:24:08 housel Exp $
module: Random
author: Nick Kramer (nkramer@cs.cmu.edu)

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

// A replacement for David Pierce's Random library.  This one is
// strongly based on Common Lisp.  Much of the implementation has been
// translated from CMUCL's rand.lisp, which was written by David Adam.
// Code that turns a uniform random distribution into Gaussian and
// exponential distributions has been adapted from David Pierce's
// library.

// Random state hackery:
//
define constant $random-const-a = 8373;
define constant $random-const-c = 101010101;
define constant $random-max = 54;

// Inclusive upper bound on the size of fixnum kept in the state (and returned
// by random-chunk.)  Must be even.
//
// ### We don't actually use $maximum-integer because it is different in
// Mindy and the new compiler and we want to generate the same sequences
// of random numbers.  For now.
// 
// define constant $random-upper-bound = $maximum-integer - 3;
define constant $random-upper-bound = ash(1, 30) - 4;

define sealed class <random-state> (<object>)
  slot state-j :: <integer> = 24;
  slot state-k :: <integer> = 0;
  slot state-seed :: <simple-object-vector>
    = make(<simple-object-vector>, size: $random-max + 1);
end class <random-state>;

define sealed domain make (singleton(<random-state>));

#if (threads)

define class <threadsafe-random-state> (<random-state>)
  slot mutex :: <spinlock>, init-function: method () make(<spinlock>) end;
end class <threadsafe-random-state>;

define sealed domain make (singleton(<threadsafe-random-state>));

#endif

define method initialize
    (state :: <random-state>, #next next-method, 
     #key seed :: <integer> = get-time-of-day())
  next-method();
  local method rand1 () => random-number :: <integer>;
	  seed := as(<integer>,
		     modulo(as(<extended-integer>, seed) * $random-const-a
			      + $random-const-c,
			    $random-upper-bound + 1));
	end method rand1;
  let seed-vec = state.state-seed;
  for (i :: <integer> from 0 to $random-max)
    seed-vec[i] := rand1();
  end;
end method initialize;
		     
define sealed domain initialize (<random-state>);

define variable *random-state* :: <random-state> = make(<random-state>);

define method shallow-copy (state :: <random-state>)
    => new-state :: <random-state>;
  let new-state = make(<random-state>);
  new-state.state-j := state.state-j;
  new-state.state-k := state.state-k;
  new-state.state-seed := shallow-copy(state.state-seed);
  new-state;
end method shallow-copy;


// $random-chunk-length -- internal.
//
// Number of bits in a random chunk.
// 
define constant $random-chunk-length :: <integer> = $random-upper-bound.integer-length;

// random-chunk  --  Internal
//
// This function generates fixnums between 0 and $random-upper-bound, 
// inclusive.  For the algorithm to work $random-upper-bound must be an 
// even positive fixnum.  State is the random state to use.
//
define method random-chunk (state :: <random-state>)
    => number :: <integer>;
  let seed = state.state-seed;
  let j = state.state-j;
  let k = state.state-k;
  state.state-j := if (j = 0) $random-max else j - 1 end;
  state.state-k := if (k = 0) $random-max else k - 1 end;
  let seed_j :: <integer> = seed[state.state-j];
  let seed_k :: <integer> = seed[state.state-k];
  let a = ($random-upper-bound - seed_j) - seed_k;
  seed[k] := if (a < 0)
	       -a;
	     else
	       $random-upper-bound - a;
	     end;
end method random-chunk;

#if (threads)
define method random-chunk
    (state :: <threadsafe-random-state>, #next next-method)
    => number :: <integer>;
  grab-lock(state.mutex);
  let res = next-method();
  release-lock(state.mutex);
  res;
end;
#endif


// Random integers:

// Amount we overlap chunks by when building a large integer to make up for
// the loss of randomness in the low bits.
//
define constant $random-integer-overlap = 3;

// Extra bits of randomness that we generate before taking the value MOD the
// limit, to avoid loss of randomness near the limit.
//
define constant $random-integer-extra-bits = 10;

// Number of bits returned by random-bits.
//
define constant $random-bits-count :: <integer>
  = $random-chunk-length - $random-integer-extra-bits;

// random-bits -- exported
//
// Quickly return a random integer with $random-bits-count random bits.
//
define method random-bits (#key state :: <random-state> = *random-state*) 
    => bits :: <integer>;
  ash(random-chunk(*random-state*), - $random-integer-extra-bits);
end method random-bits;


// Largest fixnum we can compute from one chunk of bits.
//
define constant $random-fixnum-max :: <integer>
  = ash(1, $random-bits-count) - 1;

// random -- exported
//
// Return a random integer between 0 (inclusive) and arg (exclusive).  We
// return the same kind of integer that arg is.
// 
define method random
    (arg :: <integer>, #key state, random) 
    => random-number :: <integer>;
  let state :: <random-state> = state | random | *random-state*;
  if (arg <= $random-fixnum-max)
    remainder(random-chunk(state), arg);
  else
    let shift = $random-chunk-length - $random-integer-overlap;
    for (bits :: <extended-integer>
	   = as(<extended-integer>, random-chunk(state))
	   then logxor(ash(bits, shift), random-chunk(state)),
	 count :: <integer>
	   = arg.integer-length + $random-integer-extra-bits - shift
	   then count - shift,
	 until: count < 0)
    finally
      as(<integer>, remainder(bits, arg));
    end for;
  end if;
end method random;


// Random floats.

// Not very efficient, but the CMUCL version couldn't be used
// because of all the weird insights into the float representation.
//
define method random-float
    (arg :: <number>, #key state = *random-state*) => number :: <float>;
  let max-value = as(<float>, arg);
  let random-num = as(<float>, random-bits(state: state));
  let random-bits-max-value
    = as(<float>, ash(1, $random-bits-count - 1));
  (random-num / random-bits-max-value) * max-value;
end method random-float;

// Gaussian Distribution
//
// The Gaussian (or Normal) probability distribution has a very
// complicated probability density function.  So the methods used to
// generate it are somewhat obscure.  The normal distribution can be
// generated using two uniform distributions, A and B.  Numbers from a
// normal distribution with mean 0 and standard deviation (or sigma) 1
// can be found using this formula:
// 
//                                1/2
// 		   X = (- 2 ln(A))    cos(2 pi B).
// 
// This distribution can be transformed to a distribution with mean m
// and sigma o by a linear function.
//
//			     Y = o X + m
//
// Generates a normal distribution with mean 0 and sigma 1 from two
// numbers chosen from a unit uniform distribution, then applies the
// linear transformation to adjust to the real mean and sigma of the
// desired distribution.
// 
define method random-gaussian (#key mean = 0, standard-deviation = 1, 
			       state = *random-state*)
 => random :: <float>;
  let unit-gaussian
    = sqrt(-2 * log(random-float(1.0, state: state))) 
              * cos(2 * $double-pi * random-float(1.0, state: state));
   standard-deviation * unit-gaussian + mean;
end method random-gaussian;

// Exponential Distribution
//
// The exponential distribution has a cumulative distribution function
// described by
//
//                                 -lx
//		       F(x) = 1 - e      x > 0
//
// An exponential distribution X can be generated from a unit uniform
// distribution U using a transformation.  That is, a number u from
// the uniform is chosen, then the inverse of the CDF is applied
//
//                                 1
//                        X(u) = - - ln(u)
//                                 l
//
// Generates numbers distributed in an exponential distribution with
// parameter lambda.  Applies the inverse CDF of the exponential
// distribution to a number generated from a unit uniform
// distribution.
//
define method random-exponential (#key lambda = 1, state = *random-state*)
      => random :: <double-float>;
  - log(random-float(1.0, state: state) / lambda);
end method random-exponential;


/////////////////////////////////////////////////////////////////////////
// test code
/////////////////////////////////////////////////////////////////////////

// Chi-Square Test for the Random Number Generator
//
// The chi-square function can be used to test the integrity of a
// uniform random number generator.  It takes a function which takes
// one parameter, like random(), and the upper bound of the range to
// calculate over, and returns the chi square value for the generator.
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
define method chi-square 
    (generator :: <function>, r :: <general-integer>)
    => chi-square :: <number>;
  let N = 10 * r;
  let f = as(<double-float>, N) / as(<double-float>, r);
  let freq = make(<vector>, size: r, fill: 0);
  for (i from 0 below N)
    let d = generator(r);
    freq[d] := freq[d] + 1;
  end for;
  let total = 0;
  for (i from 0 below r)
    total := total + freq[i] * freq[i];
  end for;
  ((as(<float>, total) * r) / N) - N;
end method chi-square;
