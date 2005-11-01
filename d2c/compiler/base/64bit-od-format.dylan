Module: od-format
copyright: see below

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998 - 2004  Gwydion Dylan Maintainers
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



// Buffer interface for 64bit-platforms:

// ### not portable, should be generalized.  Should be provided somewhere else.
//

define /* exported */ constant $word-bytes = 8;
define /* exported */ constant $word-bits = 64;

define constant <word> = <integer>;

// Read a word from a buffer at a word-aligned byte offset.
// 
define method buffer-word(bbuf :: <buffer>, i :: <buffer-index>)
 => word :: <word>;
  // ### big-endian 64 assumption.  Should be a primitive.
  bbuf[i + 7] 
    + ash(bbuf[i + 6], 8)  
    + ash(bbuf[i + 5], 16)
    + ash(bbuf[i + 4], 24) 
    + ash(bbuf[i + 3], 32) 
    + ash(bbuf[i + 2], 40) 
    + ash(bbuf[i + 1], 48) 
    + ash(buf[i], 56);
end method;


// Write a word to a buffer at a word-aligned byte offset.
// 
define method buffer-word-setter
    (new-val :: <word>, bbuf :: <buffer>, i :: <buffer-index>)
 => res :: <word>;
  // ### big-endian 64 assumption.  Should be a primitive.
  bbuf[i + 0] := logand(ash(new-val, -56), 255);
  bbuf[i + 1] := logand(ash(new-val, -48), 255);
  bbuf[i + 2] := logand(ash(new-val, -40), 255);
  bbuf[i + 3] := logand(ash(new-val, -32), 255);
  bbuf[i + 4] := logand(ash(new-val, -24), 255);
  bbuf[i + 5] := logand(ash(new-val, -16), 255);
  bbuf[i + 6] := logand(ash(new-val, -8),  255); 
  bbuf[i + 7] := logand(    new-val,       255);          
end method;

define method dump-header-word
    (hi :: <integer>, obj :: <integer>, buf :: <dump-buffer>) => ();

  if (buf.buffer-pos == buf.dump-end) grow-dump-buffer(buf, $word-bytes) end;
  let i = buf.buffer-pos;

  let bbuf = buf.dump-buffer;
  // ### big-endian 64 assumption.  Should be a primitive.

  bbuf[i + 0] := hi;
  bbuf[i + 1] := logand(ash(new-val, -48), 255);
  bbuf[i + 2] := logand(ash(new-val, -40), 255);
  bbuf[i + 3] := logand(ash(new-val, -32), 255);
  bbuf[i + 4] := logand(ash(new-val, -24), 255);
  bbuf[i + 5] := logand(ash(new-val, -16), 255);
  bbuf[i + 6] := logand(ash(new-val, -8),  255); 
  bbuf[i + 7] := logand(    new-val,       255);          

  buf.buffer-pos := i + $word-bytes;
end method;


// Approximate inverse of dump-header-word, reads a header word from a buffer
// and returns the high byte and rest as two values.
//
define method buffer-header-word(bbuf :: <buffer>, i :: <buffer-index>) 
 => (hi-byte :: <integer>, low :: <integer>);
  // ### big-endian 32 assumption.  Should be a primitive.
  values(bbuf[i + 0],
         bbuf[i + 7] 
           + ash(bbuf[i + 6], 8)  
           + ash(bbuf[i + 5], 16)
           + ash(bbuf[i + 4], 24) 
           + ash(bbuf[i + 3], 32) 
           + ash(bbuf[i + 2], 40) 
           + ash(bbuf[i + 1], 48))
end method;


// Rounds a byte offset up to the next word boundary.
// ### assumes 4byte words.
//
define method round-to-word (x :: <integer>) => res :: <integer>;
  logand(x + 7, -8);
end method;

// Dump the header for the overall data-unit.
//
define method dump-unit-header
    (state :: <dump-state>, buf :: <dump-buffer>, oa-len :: <integer>)
 => ();
  dump-definition-header(#"64bit-data-unit", buf, subobjects: #t,
  		         raw-data: $odf-word-raw-data-format);

  // subtract header words to get raw data words.			 
  dump-word($data-unit-header-size - 2, buf);

  dump-word($od-format-major-version, buf);
  dump-word($od-format-minor-version, buf);
  dump-word(oa-len, buf);
  dump-word($like-an-hp-platform-characteristics, buf);
  dump-word(state.dump-type, buf);
  dump-word(compute-unit-hash(state), buf);
end method;

define method check-odf-id(id) => (ok? :: <boolean>)
  id = $64bit-data-unit-odf-id
end method check-odf-id;
