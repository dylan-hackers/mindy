module: dylan-user
author: Nick Kramer
copyright: Copyright (C) 1997, Carnegie Mellon University
	   All rights reserved.
	   This code was produced by the Gwydion Project at Carnegie Mellon
	   University.  If you are interested in using this code, contact
	   "Scott.Fahlman@cs.cmu.edu" (Internet).
rcs-header: $Header: /scm/cvs/src/tools/make-exports/library.dylan,v 1.1 1998/05/03 19:55:57 andreas Exp $

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

//======================================================================
//
// Copyright (c) 1997  Carnegie Mellon University
// All rights reserved.
//
//======================================================================

define library make-exports
  use dylan;
  use streams;
  use standard-io;
  use print;
  use format;
  use table-extensions;
  use string-extensions;
  use regular-expressions;
end library make-exports;

define module make-exports
  use dylan;
  use extensions;
#if (~mindy)
  use system; // for %main
#endif
  use streams;
  use standard-io;
  use print;
  use pprint;
  use format;
  use table-extensions;
  use substring-search;
  use regular-expressions;
end module make-exports;
