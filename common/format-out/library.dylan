module: Dylan-User
author: David Watson (dwatson@cmu.edu)
synopsis: Definition of the Format-Out library.
rcs-header: $Header: /scm/cvs/src/common/format-out/library.dylan,v 1.1 1998/05/03 19:55:01 andreas Exp $

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

define library Format-out
  use Dylan;
  use Format,
    export: {Format};
  use Standard-IO,
    export: {Standard-io};

  export Format-out;
end library Format-out;

define module Format-out
  use Dylan;
  use Format;
  use Standard-IO;

  export format-out;
end module Format-out;
