rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/machineword.dylan,v 1.7 2003/06/30 15:18:04 housel Exp $
copyright: see below
module: dylan-viscera

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000, 2001, 2002  Gwydion Dylan Maintainers
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
  constant slot value :: <integer>,
    init-value: 0, init-keyword: value:;
end;

define sealed abstract class <invalid-bit-number> (<error>)
  constant slot bit-number :: <integer>,
    required-init-keyword: bit-number:;
end class;

ignore(bit-number);

define sealed class <invalid-bit-index> (<invalid-bit-number>)
end class;

define sealed domain make (singleton(<invalid-bit-index>));
define sealed domain initialize (<invalid-bit-index>);

define function invalid-bit-index
    (index :: <integer>)
 => (res :: <never-returns>);
  error(make(<invalid-bit-index>, bit-number: index));
end;

define sealed inline method as
    (class == <machine-word>, integer :: <integer>)
 => (machine-word :: <machine-word>);
  make(<machine-word>, value: integer);
end method;

define sealed inline method as
    (class == <machine-word>, raw-pointer :: <raw-pointer>)
 => (machine-word :: <machine-word>);
  make(<machine-word>, value: %%primitive(raw-pointer-address, raw-pointer));
end method;

define sealed inline method as
    (class == <integer>, machine-word :: <machine-word>)
 => (integer :: <integer>);
  machine-word.value;
end method;

define sealed inline method as
    (class == <raw-pointer>, machine-word :: <machine-word>)
 => (raw-pointer :: <raw-pointer>);
  %%primitive(make-raw-pointer, machine-word.value);
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

define inline function valid-bit-number? (bit :: <integer>)
  (0 <= bit) & (bit < $machine-word-size);
end;

define inline function check-bit-index(idx :: <integer>)
  unless (valid-bit-number?(idx))
    invalid-bit-index(idx);
  end unless;
end;

define sealed inline method %logbit?
    (idx :: <integer>, a :: <machine-word>)
 => (res :: <boolean>);
  check-bit-index(idx);
  logbit?(idx, as(<integer>, a));
end method %logbit?;

define sealed inline method odd? (a :: <machine-word>)
 => (res :: <boolean>)
  %logbit?(0, a)
end method odd?;

define sealed inline method even? (a :: <machine-word>)
 => (res :: <boolean>)
  ~%logbit?(0, a)
end method even?;

define sealed inline method zero? (a :: <machine-word>)
 => (res :: <boolean>)
  a = $machine-word-zero
end method zero?;

define sealed inline method positive? (a :: <machine-word>)
 => (res :: <boolean>)
  a > $machine-word-zero
end method positive?;

define sealed inline method negative? (a :: <machine-word>)
 => (res :: <boolean>)
  a < $machine-word-zero
end method negative?;

define inline method %lognot
    (a :: <machine-word>)
 => (res :: <machine-word>);
  as(<machine-word>, lognot(as(<integer>, a)));
end method;

define inline method %logand
    (a :: <machine-word>, b :: <machine-word>)
 => (res :: <machine-word>);
  as(<machine-word>, logand(as(<integer>, a), as(<integer>, b)));
end method;

define inline method %logior
    (a :: <machine-word>, b :: <machine-word>)
 => (res :: <machine-word>);
  as(<machine-word>, logior(as(<integer>, a), as(<integer>, b)));
end method;

define inline method %logxor
    (a :: <machine-word>, b :: <machine-word>)
 => (res :: <machine-word>);
  as(<machine-word>, logxor(as(<integer>, a), as(<integer>, b)));
end method;

define inline method %shift-left
    (a :: <machine-word>, count :: <integer>)
 => (low :: <machine-word>, high :: <machine-word>, overflow? :: <boolean>);
  check-bit-index(count);
  let a = as(<integer>, a);
  let low = %%primitive(fixnum-shift-left, a, count);
  let high
    = %%primitive(fixnum-shift-right, a, $fixed-integer-bits - count); // ###
  values(as(<machine-word>, low), as(<machine-word>, high),
         high ~= 0 & high ~= -1);
end method;

define inline method %shift-right
    (a :: <machine-word>, count :: <integer>)
 => (res :: <machine-word>)
  check-bit-index(count);
  as(<machine-word>, %%primitive(fixnum-shift-right, as(<integer>, a), count));
end method;

define inline method u%rotate-left
    (a :: <machine-word>, count :: <integer>)
 => (res :: <machine-word>);
  check-bit-index(count);
  let a = as(<integer>, a);
  as(<machine-word>,
     logior(%%primitive(fixnum-shift-left, a, count),
            %%primitive(fixnum-logical-shift-right, a,
                        $fixed-integer-bits - count)));
end method;

define inline method u%rotate-right
    (a :: <machine-word>, count :: <integer>)
 => (res :: <machine-word>)
  check-bit-index(count);
  let a = as(<integer>, a);
  as(<machine-word>,
     logior(%%primitive(fixnum-logical-shift-right, a, count),
            %%primitive(fixnum-shift-left, a, $fixed-integer-bits - count)));
end method;

define inline method u%shift-left
    (a :: <machine-word>, count :: <integer>)
 => (res :: <machine-word>);
  check-bit-index(count);
  as(<machine-word>, %%primitive(fixnum-shift-left, as(<integer>, a), count));
end method;

define inline method u%shift-right
    (a :: <machine-word>, count :: <integer>)
 => (res :: <machine-word>)
  check-bit-index(count);
  as(<machine-word>,
     %%primitive(fixnum-logical-shift-right, as(<integer>, a), count));
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
define constant $machine-word-zero :: <machine-word>
  = as(<machine-word>, 0);


define sealed domain make (singleton(<machine-word>));
define sealed domain initialize (<machine-word>);
