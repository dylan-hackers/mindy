rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/machineword.dylan,v 1.1 2001/12/13 22:43:58 housel Exp $
copyright: see below
module: dylan-viscera

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

define functional class <machine-word> (<object>)
  slot value :: <integer>, required-init-keyword: %value:;
end;

define sealed inline method as
    (class == <machine-word>, integer :: <integer>)
 => (machine-word :: <machine-word>);
  make(<machine-word>, %value: integer);
end method;

define sealed inline method as
    (class == <integer>, machine-word :: <machine-word>)
 => (integer :: <integer>);
  machine-word.value;
end method;

define sealed inline method functional-==
    (class == <machine-word>, a :: <machine-word>, b :: <machine-word>)
    => res :: <boolean>;
  a.value == b.value;
end;

define sealed inline method \=
    (a :: <general-integer>, b :: <machine-word>)
 => (res :: <boolean>);
  a = b.value;
end method;

define sealed inline method \=
    (a :: <machine-word>, b :: <general-integer>)
 => (res :: <boolean>);
  a.value = b;
end method;

define sealed inline method \<
    (a :: <machine-word>, b :: <machine-word>)
 => (res :: <boolean>);
  a.value < b.value;
end method;

define sealed inline method \<
    (a :: <general-integer>, b :: <machine-word>)
 => (res :: <boolean>);
  a < b.value;
end method;

define sealed inline method \<
    (a :: <machine-word>, b :: <general-integer>)
 => (res :: <boolean>);
  a.value < b;
end method;

define constant $machine-word-size :: <integer> = $fixed-integer-bits;
define constant $maximum-signed-machine-word :: <machine-word>
  = as(<machine-word>, $maximum-integer);
define constant $minimum-signed-machine-word :: <machine-word>
  = as(<machine-word>, $minimum-integer);
define constant $maximum-unsigned-machine-word :: <machine-word>
  = as(<machine-word>, -1);
define constant $minimum-unsigned-machine-word :: <machine-word>
  = as(<machine-word>, 0);
