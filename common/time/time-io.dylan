module: time-internal
author: dwatson@cmu.edu
synopsis: The Time-IO module.
copyright: See below.
rcs-header: $Header: /home/housel/work/rcs/gd/src/common/time/time-io.dylan,v 1.1 1996/07/29 18:31:10 dwatson Exp $
 
//======================================================================
//
// Copyright (c) 1996  Carnegie Mellon University
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

define method string-format-time
    (format :: <byte-string>, time :: <decoded-time>)
 => (string :: <object>);
  let buffer = make(<c-string>, size: 500);
  strftime(buffer, 500, format, time);
  let string = as(<byte-string>, buffer);
  string;
end method string-format-time;
