module: Dylan-User
author: chiles@cs.cmu.edu
synopsis: This file defines the Print library and modules.
copyright: See below.
rcs-header: $Header: /scm/cvs/src/common/format/library.dylan,v 1.1 1998/05/03 19:55:00 andreas Exp $

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


///
/// These definitions go into the Dylan-User module because this is how we
/// jumpstart a library.
///

define library format
  use dylan;
  use streams;
  use print,
    export: {print};
  export format;
end library;

define module format
  use dylan;
  use extensions,
    import: {false-or, <general-integer>, <extended-integer>,
	     $minimum-integer, <byte-character>,
	     condition-format, condition-force-output,
	     report-condition};
  use streams;
  use print,
    import: {print};
  export
    format,
    print-message,
    format-to-string;
end module;
