module: Dylan-User
author: Nick Kramer (nkramer@cs.cmu.edu)
synopsis: Definition of the Transcendental library.
rcs-header: $Header: /home/housel/work/rcs/gd/src/common/transcendental/library.dylan,v 1.2 1996/10/13 20:52:23 bfw Exp $

//======================================================================
//
// Copyright (c) 1996  Carnegie Mellon University
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

define library Transcendental
  use Dylan;
  export Transcendental;
end library Transcendental;

define module Transcendental
  use Dylan,
    export: {\^};
  use Extensions;
  use System;

  export
    $double-pi, $single-pi, $double-e, $single-e,
    sin, cos, tan, asin, acos, atan, atan2, sinh, cosh, tanh,
    log, exp, sqrt, isqrt
  // ### Need to implement these for x86
  #if (compiled-for-hppa-hpux)
    , asinh, acosh, atanh
  #endif
    ;
end module Transcendental;
