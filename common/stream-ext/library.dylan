module: Dylan-User
author: Russ Schaaf (rsbe@andrew.cmu.edu), Nick Kramer, William Lott
synopsis:   Extensions to the streams library
copyright:  See below.

//======================================================================
//
// Copyright (c) 1994, 1995  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
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
//    University, and the Gwydion Dylan Maintainers.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
// Also, see http://www.gwydiondylan.org/ for updates and documentation. 
//
//======================================================================


define library stream-extensions
  use dylan;
  use streams;
  export
    stream-extensions, indenting-streams, concatenated-streams;
end library stream-extensions;

define module stream-extensions
  use dylan;
  use extensions;
  use streams;
  export read-as-list;
end module stream-extensions;

define module indenting-streams
  use dylan;
  use extensions;
  use streams;
  use system, import: {copy-bytes};
  export
    <indenting-stream>, indent;
end module indenting-streams;

define module concatenated-streams
  use dylan;
  use extensions;
  use streams;
  export
    <concatenated-stream>;
end module concatenated-streams;
