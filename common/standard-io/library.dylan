module: Dylan-User
author: chiles@cs.cmu.edu
synopsis: Library and module definitions.
copyright: See below.
rcs-header: $Header: /home/housel/work/rcs/gd/src/common/standard-io/library.dylan,v 1.1 1996/07/12 02:23:21 bfw Exp $

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

define library Standard-IO
  use Dylan;
  use New-Streams;
  export Standard-IO;
end library;

define module Standard-IO
  use Dylan;
  use New-Streams,
    import: {<fd-stream>};
  export
    *standard-input*, *standard-output*, *standard-error*;
end module;
