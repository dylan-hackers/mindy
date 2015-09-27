module:     Dylan-User
author:     Russell Schaaf (rsbe@andrew.cmu.edu)
synopsis:   This file defines the Gwydion matrix operations library.
copyright:  See below.

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
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

// Dylan Matrix Library
//   this library contains a set of matrix operations for Dylan
//

// Matrix (Library) -- public
// 
define library Matrix
  use Dylan;
  export Matrix;
end library Matrix;


// Matrix (Module) -- public
//
define module Matrix
  use Dylan;
  // needed for <ratio>s and operations on them
  use Extensions;
  export
    <matrix>,
    matrix,
    identity-matrix,
    augment-matrix,
    gauss-jordan,
    inverse,
    det,
    transpose;
end module Matrix;
