module: Internal-Time-Interface
author: Ben Folk-Williams
synopsis: A simple interface for calling some c time routines.
copyright: See below.

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

define constant <internal-time> = <integer>;

define constant anonymous-1
  = load-object-file(#("/lib/libc.sl"), include: #("times"));

define constant <clock-t> = <integer>;

define class <tms> (<statically-typed-pointer>) end;

define sealed method tms-utime
    (ptr :: <tms>) => (result :: <internal-time>);
  import-value(<internal-time>, unsigned-long-at(ptr, offset: 0));
end method tms-utime;

define sealed method tms-utime-setter
    (value :: <internal-time>, ptr :: <tms>) => (result :: <internal-time>);
  unsigned-long-at(ptr, offset: 0) := export-value(<clock-t>, value);
  value;
end method tms-utime-setter;

define sealed method tms-stime
    (ptr :: <tms>) => (result :: <internal-time>);
  import-value(<internal-time>, unsigned-long-at(ptr, offset: 4));
end method tms-stime;

define sealed method tms-stime-setter
    (value :: <internal-time>, ptr :: <tms>) => (result :: <internal-time>);
  unsigned-long-at(ptr, offset: 4) := export-value(<clock-t>, value);
  value;
end method tms-stime-setter;

define sealed method tms-cutime
    (ptr :: <tms>) => (result :: <internal-time>);
  import-value(<internal-time>, unsigned-long-at(ptr, offset: 8));
end method tms-cutime;

define sealed method tms-cutime-setter
    (value :: <internal-time>, ptr :: <tms>) => (result :: <internal-time>);
  unsigned-long-at(ptr, offset: 8) := export-value(<clock-t>, value);
  value;
end method tms-cutime-setter;

define sealed method tms-cstime
    (ptr :: <tms>) => (result :: <internal-time>);
  import-value(<internal-time>, unsigned-long-at(ptr, offset: 12));
end method tms-cstime;

define sealed method tms-cstime-setter
    (value :: <internal-time>, ptr :: <tms>) => (result :: <internal-time>);
  unsigned-long-at(ptr, offset: 12) := export-value(<clock-t>, value);
  value;
end method tms-cstime-setter;

define method pointer-value (value :: <tms>, #key index = 0) => (result :: <tms>);
  value + index * 16;
end method pointer-value;

define method content-size (value :: limited(<class>, subclass-of: <tms>)) => (result :: <integer>);
  16;
end method content-size;

define constant anonymous-2
  = constrain-c-function(find-c-function("times", file: anonymous-1), #(), #t, list(<clock-t>));
define method times
    ()
 => (result :: <internal-time>, arg1 :: <tms>);
  let arg1-ptr = make(<tms>);
  let result-value
    = anonymous-2(arg1-ptr);
  let arg1-value = pointer-value(arg1-ptr);
  values(import-value(<internal-time>, result-value), arg1-value);
end method times;

define constant anonymous-119
  = load-object-file(#("/lib/libc.sl"), include: #("sysconf"));

define constant $SC-CLK-TCK = 2;

define constant anonymous-120
  = constrain-c-function(find-c-function("sysconf", file: anonymous-119), #(), #t, list(<integer>));
define method sysconf
    (arg1 :: <integer>)
 => (result :: <integer>);
  let result-value
    = anonymous-120(arg1);
  values(result-value);
end method sysconf;

