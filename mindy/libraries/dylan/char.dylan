module: Dylan
rcs-header: $Header: /scm/cvs/src/mindy/libraries/dylan/char.dylan,v 1.1 1998/05/03 19:55:20 andreas Exp $

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
// This file contains the support for characters that isn't built in.
//

define method as-uppercase (c :: <character>) => char :: <character>;
  if ('a' <= c & c <= 'z')
    as(<character>, as(<integer>, c) - 32);
  else
    c;
  end;
end;

define method as-lowercase (c :: <character>) => char :: <character>;
  if ('A' <= c & c <= 'Z')
    as(<character>, as(<integer>, c) + 32);
  else
    c;
  end;
end;
