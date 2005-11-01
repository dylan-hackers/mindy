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



// Buffer interface for 32bit-platforms:

// ### not portable, should be generalized.  Should be provided somewhere else.
//

define /* exported */ constant $word-bytes = 4;
define /* exported */ constant $word-bits = 32;

#if (mindy)
define constant <word> = <general-integer>;
#else
define constant <word> = <integer>;
#endif

// Read a word from a buffer at a word-aligned byte offset.
// 
define method buffer-word(bbuf :: <buffer>, i :: <buffer-index>)
 => word :: <word>;
  
  let high-end = bbuf[i];

  // ### big-endian 32 assumption.  Should be a primitive.
  bbuf[i + 3] + ash(bbuf[i + 2], 8) + ash(bbuf[i + 1], 16)
    + 
#if (mindy)
    // for mindy, return extended if too big to be fixed...
      if (high-end.zero?)
	0;
      elseif (high-end < 64)
#endif
	ash(high-end, 24);
#if (mindy)
      else
	ash(as(<extended-integer>, high-end), 24);
      end if;
#endif
    
end method;


// Write a word to a buffer at a word-aligned byte offset.
// 
define method buffer-word-setter
    (new-val :: <word>, bbuf :: <buffer>, i :: <buffer-index>)
 => res :: <word>;
  // ### big-endian 32 assumption.  Should be a primitive.
#if (mindy)
  let (rest, byte4) = floor/(new-val, 256);
  let (rest, byte3) = floor/(as(<integer>, rest), 256);
  // This assumes that the word is unsigned (i.e. new-val is a positive int)
  let (byte1, byte2) = floor/(rest, 256);
#else
  let byte1 = logand(ash(new-val, -24), 255);
  let byte2 = logand(ash(new-val, -16), 255);
  let byte3 = logand(ash(new-val, -8), 255);
  let byte4 = logand(new-val, 255);
#endif

  bbuf[i + 0] := byte1;
  bbuf[i + 1] := byte2;
  bbuf[i + 2] := byte3;
  bbuf[i + 3] := as(<integer>, byte4);
end method;

#if (mindy)
define method buffer-word-setter
    (new-val :: <integer>, bbuf :: <buffer>, i :: <buffer-index>)
 => res :: <word>;
  // ### big-endian 32 assumption.  Should be a primitive.
  let (rest, byte4) = floor/(new-val, 256);
  let (rest, byte3) = floor/(rest, 256);
  // This assumes that the word is unsigned (i.e. new-val is a positive int)
  let (byte1, byte2) = floor/(rest, 256);
  bbuf[i + 0] := byte1;
  bbuf[i + 1] := byte2;
  bbuf[i + 2] := byte3;
  bbuf[i + 3] := byte4;
end method;
#endif

// #### HACK to allow us to dump headers w/o creating bignums in mindy.  This
// is particularly incorrect for e.g. end entries and references, since they
// might conceivably need more than 24 bits for their data part.
//
define method dump-header-word
    (hi :: <integer>, obj :: <integer>, buf :: <dump-buffer>) => ();

  if (buf.buffer-pos == buf.dump-end) grow-dump-buffer(buf, $word-bytes) end;
  let i = buf.buffer-pos;

  let bbuf = buf.dump-buffer;
  // ### big-endian 32 assumption.  Should be a primitive.
#if (mindy)
  let (rest, byte4) = floor/(obj, 256);
  let (rest, byte3) = floor/(rest, 256);
  // This assumes that the word is unsigned (i.e. new-val is a positive int)
  let (byte1, byte2) = floor/(rest, 256);
#else
  let byte2 = logand(ash(obj, -16), 255);
  let byte3 = logand(ash(obj, -8), 255);
  let byte4 = logand(obj, 255);
#endif

  bbuf[i + 0] := hi;
  bbuf[i + 1] := byte2;
  bbuf[i + 2] := byte3;
  bbuf[i + 3] := byte4;

  buf.buffer-pos := i + $word-bytes;
end method;


// Approximate inverse of dump-header-word, reads a header word from a buffer
// and returns the high byte and rest as two values.
//
define method buffer-header-word(bbuf :: <buffer>, i :: <buffer-index>) 
 => (hi-byte :: <integer>, low :: <integer>);
  // ### big-endian 32 assumption.  Should be a primitive.
  values(bbuf[i + 0],
	 bbuf[i + 3] + ash(bbuf[i + 2], 8) + ash(bbuf[i + 1], 16));
end method;


// Rounds a byte offset up to the next word boundary.
// ### assumes 4byte words.
//
define method round-to-word (x :: <integer>) => res :: <integer>;
  logand(x + 3, -4);
end method;


// Dump the header for the overall data-unit.
//
define method dump-unit-header
    (state :: <dump-state>, buf :: <dump-buffer>, oa-len :: <integer>)
 => ();
  dump-definition-header(#"32bit-data-unit", buf, subobjects: #t,
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
  id = $32bit-data-unit-odf-id
end method check-odf-id;
