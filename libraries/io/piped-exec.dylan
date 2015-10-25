module: piped-exec
author: Nick Kramer
copyright: see below

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
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

// Implements piped-exec, aka fd-exec

define function piped-exec (command-line :: <string>)
 => writeable-pipe :: <stream>, readable-pipe :: <stream>;
  let (writeable-fd, readable-fd) = fd-exec(command-line);
  let writeable-stream
    = make(<fd-stream>, direction: #"output", fd: writeable-fd);
  let readable-stream
    = make(<fd-stream>, direction: #"input", fd: readable-fd);
  values(writeable-stream, readable-stream);
end function piped-exec;
