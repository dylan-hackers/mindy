module: Concatenate
rcs-header: $Header: /home/housel/work/rcs/gd/src/demos/cat/cat.dylan,v 1.5 1996/07/30 19:37:50 bfw Exp $

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
// This demo demonstrates the streams library by duplicating the unix
// ``cat'' utility.
//
// We need to define our own library because we need to use the Streams
// library in addition to the standard Dylan library.
//

define library Concatenate
  use Dylan;
  use streams;
  use Standard-IO;
end;

define module Concatenate
  use Dylan;
  use Extensions;
  use streams;
  use Standard-IO;
end;

define method main (argv0 :: <byte-string>, #rest names)
  if (empty?(names))
    spew(*standard-input*);
  else
    for (name in names)
      let stream = if (name = "-")
		     make(<fd-stream>, fd: 0);
		   else
		     make(<file-stream>, locator: name);
		   end;
      spew(stream);
      close(stream);
    end;
  end;
end;

define method spew (stream :: <stream>)
  let buf :: false-or(<buffer>) = get-input-buffer(stream);
  while (buf)
    write(*standard-output*, buf, start: buf.buffer-next, end: buf.buffer-end);
    buf.buffer-next := buf.buffer-end;
    buf := next-input-buffer(stream);
  end while;
  release-input-buffer(stream);
end;
