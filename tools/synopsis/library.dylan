module: dylan-user
author: Nick Kramer
rcs-header: $Header: /scm/cvs/src/tools/synopsis/library.dylan,v 1.1 1998/05/03 19:55:57 andreas Exp $

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
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

// Copyright JPRC 1996, all rights reserved.

define library synopsis
  use dylan;
  use streams;
  use standard-io;
  use format;
  use regular-expressions;
  use table-extensions;
end library synopsis;

define module synopsis
  use dylan;
  use extensions;
#if (~mindy)
  use system; // for %main
#endif
  use streams;
  use standard-io;
  use format;
  use regular-expressions;
  use table-extensions, import: { case-insensitive-equal };
end module synopsis;

