module: %Transcendental
author: Nick Kramer (nkramer@cs.cmu.edu)
rcs-header: $Header: /scm/cvs/src/mindy/libraries/dylan/transcendental.dylan,v 1.1 1998/05/03 19:55:21 andreas Exp $

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
//  This file contains the support for transcendentals that isn't built in.
//

define method atan2 (num1 :: <single-float>, num2 :: <double-float>)
 => answer :: <double-float>;
  atan2(as(<double-float>, num1), num2);
end method atan2;

define method atan2 (num1 :: <double-float>, num2 :: <single-float>)
 => answer :: <double-float>;
  atan2(num1, as(<double-float>, num2));
end method atan2;

define method \^ (num1 :: <single-float>, num2 :: <double-float>) 
 => answer :: <double-float>;
  as(<double-float>, num1) ^ num2;
end method;

define method \^ (num1 :: <double-float>, num2 :: <single-float>) 
 => answer :: <double-float>;
  num1 ^ as(<double-float>, num2);
end method;



