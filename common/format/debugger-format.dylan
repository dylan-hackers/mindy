module: debugger-format
rcs-header: $Header: /scm/cvs/src/common/format/debugger-format.dylan,v 1.1 1998/05/03 19:55:00 andreas Exp $

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

define library debugger-format
  use Dylan;
  use streams;
  use Standard-IO;
  //
  // We use the format library even though we don't reference anything in
  // it to make sure it is loaded.  Otherwise, there won't be a <stream>
  // method on condition-format and condition-force-output.
  use Format;
end;

define module debugger-format
  use Dylan;
  use Extensions;
  use Standard-IO;
end;


*debug-output* := *standard-output*;
*warning-output* := *standard-output*;

