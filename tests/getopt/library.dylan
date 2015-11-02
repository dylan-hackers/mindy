library: getopttest
module: dylan-user
author: Jeff Dubrule, Eric Kidd
copyright: see below

//======================================================================
//
//  Copyright (c) 1998 Jeff Dubrule, Eric Kidd
//  All rights reserved.
//
//  Use and copying of this software and preparation of derivative
//  works based on this software are permitted, including commercial
//  use, provided that the following conditions are observed:
//
//  1. This copyright notice must be retained in full on any copies
//     and on appropriate parts of any derivative works. (Other names
//     and years may be added, so long as no existing ones are removed.)
//
//  This software is made available "as is".  Neither the authors nor
//  Carnegie Mellon University make any warranty about the software,
//  its performance, or its conformity to any specification.
//
//  Bug reports, questions, comments, and suggestions should be sent by
//  E-mail to the Internet address "gd-bugs@gwydiondylan.org".
//
//======================================================================

define library getopttest
  use dylan;
  use io;
  use parse-arguments;
end library;

define module getopttest
  use dylan;
  use extensions;
  use format-out;
  use parse-arguments;
end module;
