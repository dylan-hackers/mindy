module:     Dylan-user
author:     Russell M. Schaaf (rsbe@cs.cmu.edu)
synopsis:   Interactive object inspector/class browser
copyright:  See below.
rcs-header: $Header: /home/housel/work/rcs/gd/src/mindy/libraries/inspector/Attic/library.dylan,v 1.1 1995/11/06 23:49:15 rsbe Exp $

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

define library inspector
  use dylan;
  use streams;
  use print;
  use string-extensions;
  export
    inspector;
end library inspector;

define module inspector
  use dylan;
  use extensions;
  use streams;
  use standard-io;
  use print;
  use string-conversions;
  use character-type;
  use introspection,
    import: all;
  export
    inspect,
    display-object-info;
end module inspector;
