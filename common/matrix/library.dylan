module:     Dylan-User
author:     Russell Schaaf (rsbe@andrew.cmu.edu)
synopsis:   This file defines the Gwydion matrix operations library.
copyright:  See below.
rcs-header: $Header: /scm/cvs/src/common/matrix/library.dylan,v 1.1 1998/05/03 19:55:01 andreas Exp $

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
