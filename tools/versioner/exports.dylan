module: dylan-user
library: versioner
rcs-header: $header$

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

define library versioner
  use dylan;
  use streams;
  use standard-io;
  use print;
  use format;
  use string-extensions;
  use regular-expressions;
  use table-extensions;
  use stream-extensions;
  use time;
end library versioner;

define module versioner
  use dylan;
  use extensions;
  use streams;
  use standard-io;
  use print;
  use pprint;
  use format;
  use regular-expressions;
  use substring-search;
  use string-conversions;
  use table-extensions;
  use piped-exec;
  use concatenated-streams;
  use time;
  use time-io;
  use Extensions, import: {exit};
#if (~mindy)
  use System,
     import: {pointer-deref, c-expr, <raw-pointer>, import-string};
#endif
end module versioner;

