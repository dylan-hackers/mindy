module: Dylan-User
author: Russ Schaaf (rsbe@andrew.cmu.edu)
synopsis:   Extensions to the streams library
copyright:  See below.
rcs-header: $Header: /home/housel/work/rcs/gd/src/common/stream-ext/library.dylan,v 1.1 1996/03/15 06:43:03 rsbe Exp $

//======================================================================
//
// Copyright (c) 1994, 1995  Carnegie Mellon University
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


define library stream-extensions
  use dylan;
  use streams;
  export stream-extensions;
end library stream-extensions;

define module stream-extensions
  use dylan;
  use extensions;
  use streams;
  export <end-of-stream-error>,
	 <incomplete-read-error>,
	 read-to,
	 read-through,
	 read-to-end,
	 skip-through,
	 new-line,
	 read-as-list,
	 read-line-into!;
end module stream-extensions;
