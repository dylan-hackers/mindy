module:     Dylan-User
author:     dpierce@cs.cmu.edu
synopsis:   This file defines the Gwydion random numbers library.
copyright:  Copyright (C) 1994, Carnegie Mellon University.
            All rights reserved.
rcs-header: $Header: /home/housel/work/rcs/gd/src/mindy/libraries/random/Attic/library.dylan,v 1.2 1994/06/28 23:58:05 wlott Exp $

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
