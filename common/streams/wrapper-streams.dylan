module: New-Streams
author: Ben Folk-Williams
synopsis: Wrapper Streams.
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

define open class <wrapper-stream> (<stream>)
  slot inner-stream :: <stream>, required-init-keyword: init-stream:;
end class;

define open generic inner-stream (stream :: <wrapper-stream>) 
 => res :: <stream>;

define open generic inner-stream-setter (inner-stream :: <stream>,
					 stream :: <wrapper-stream>)
 => res :: <stream>;

define open generic outer-stream (stream :: <stream>)
 => res :: <stream>;

define open generic outer-stream-setter (outer-stream :: <stream>,
					 stream :: <stream>)
 => res :: <stream>;
