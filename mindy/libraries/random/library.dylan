module:     Dylan-User
author:     dpierce@cs.cmu.edu
synopsis:   This file defines the Gwydion random numbers library.
copyright:  Copyright (C) 1994, Carnegie Mellon University.
            All rights reserved.
  	    This code was produced by the Gwydion Project at Carnegie
            Mellon University.  If you are interested in using this
            code, contact "Scott.Fahlman@cs.cmu.edu" (Internet).
rcs-header: $Header: /home/housel/work/rcs/gd/src/mindy/libraries/random/Attic/library.dylan,v 1.1 1994/06/15 19:31:17 dpierce Exp $


/* Dylan Random Number Library

   This file contains a library definition for the Random library.

*/

// Random (Library) -- public
// 
define library Random
   use Dylan;
   export Random;
end library Random;


// Random (Module) -- public
//
define module Random
   use Dylan;
   export
      <random-distribution>, random,

      <uniform-distribution>,
      <unit-uniform-distribution>,
      <real-uniform-distribution>,
      <integer-uniform-distribution>,
      <exponential-distribution>,
      <normal-distribution>,

      *dylan-random-seed*, *dylan-random-distribution*,
      random-uniform, seed-random!,

      chi-square;
end module Random;
