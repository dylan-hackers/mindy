module: debugger-format
rcs-header: $Header: /home/housel/work/rcs/gd/src/common/format/debugger-format.dylan,v 1.1 1996/03/20 00:05:55 nkramer Exp $

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
  use Streams;
  use Format;
end;

define module debugger-format
  use Dylan;
  use Extensions;
  use Streams;
  use Standard-IO;
  use Format;
end;


*debug-output* := *standard-output*;
*format-function* := format;
*force-output-function* := force-output;
