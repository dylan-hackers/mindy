module: dylan-viscera
author: ram+@cs.cmu.edu
synopsis: <buffer> and <byte-vector>
copyright: See below.
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/buffer.dylan,v 1.1 1995/11/22 14:28:05 ram Exp $

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

%%primitive c-include ("string.h");

define /* exported */ constant <byte> =
  limited(<fixed-integer>, min: 0, max: 255);

define /* exported */ class <byte-vector> (<builtin-vector>)
  sealed slot %element :: <byte>,
    init-value: ' ', init-keyword: fill:,
    sizer: size, required-size-init-keyword: size:;
end;

seal generic make (singleton(<byte-vector>));
seal generic initialize (<byte-vector>);

define /* exported */ constant <buffer> = <byte-vector>;
define /* exported */ constant <buffer-index> = <fixed-integer>;
define /* exported */ constant $maximum-buffer-size = $maximum-fixed-integer;


define sealed inline method element
    (vec :: <byte-vector>, index :: <fixed-integer>,
     #key default = $not-supplied)
    => element :: <object>; // because of default:
  if (index >= 0 & index < vec.size)
    %element(vec, index);
  elseif (default == $not-supplied)
    element-error(vec, index);
  else
    default;
  end;
end;

define sealed inline method element-setter
    (new-value :: <byte>, vec :: <byte-vector>, index :: <fixed-integer>)
    => new-value :: <object>;
  if (index >= 0 & index < vec.size)
    %element(vec, index) := new-value;
  else
    element-error(vec, index);
  end;
end;

define constant <byte-like> = type-union(<byte-string>, <byte-vector>);

// Copy bytes from src to dest (which may overlap.)  
define /* exported */ generic copy-bytes 
  (dest :: <byte-like>, dest-start :: <fixed-integer>, 
   src :: <byte-like>, src-start :: <fixed-integer>, src-end :: <fixed-integer>)
 => ();

define method copy-bytes 
    (dest :: <byte-vector>, dstart :: <fixed-integer>,
     src :: <byte-vector>, sstart :: <fixed-integer>, send :: <fixed-integer>)
 => ();
  %%primitive call-out
    ("mmove", void:,
     ptr: slot-address(dest, %element) + dstart,
     ptr: slot-address(src, %element) + sstart,
     int: send - sstart);
end method;
