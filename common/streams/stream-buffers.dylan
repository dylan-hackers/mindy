module: Streams
author: Bill Chiles, Ben Folk-Williams
synopsis: Buffered streams, Buffer Access Protocol, Stream Extension Protocol
RCS-header: $Header: /scm/cvs/src/common/streams/stream-buffers.dylan,v 1.1 1998/05/03 19:55:03 andreas Exp $
copyright: See below.

//======================================================================
//
// Copyright (c) 1994, 1996  Carnegie Mellon University
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

/// buffer -- Internal Interface.
///
/// This function returns the buffer or #f.  Streams should set the buffer to
/// #f when the stream is closed.  This function can be a test for whether the
/// stream is still open.
///
/// This function is typically implemented as a slot in the stream's class,
/// but some streams may want to implement it virtually (on demand) when
/// users insist on using the stream's buffer directly.
///
define generic buffer (stream :: <buffered-stream>)
    => buffer :: false-or(<buffer>);

/// buffer-setter -- Internal Interface.
///
define generic buffer-setter (value :: false-or(<buffer>), 
			      stream :: <buffered-stream>)
 => value :: false-or(<buffer>);

/// check-buffer-held -- Internal interface.
///
/// After calling this function, the executing thread is guaranteed to have
/// the stream locked and to hold the buffer.
///
define method check-buffer-held (stream :: <buffered-stream>);
  // Lock the stream to isolate checking whether the buffer is locked.
  lock-stream(stream);
  if (~stream.buffer-held?)
    unlock-stream(stream);
    error("Application does not hold stream's buffer -- %=.", stream);
  end;
  // Unlock the lock for checking buffer-held?.
  unlock-stream(stream);
  // Because the buffer was locked, and we were able to obtain a lock, the
  // calling thread must already hold a lock on the stream due to
  // get-output-buffer.  Therefore, code following the call to
  // check-buffer-held is still thread-safe, until the final lock is
  // dropped.
end method;

//// Buffer Access Protocol.
////

/// get-input-buffer -- Exported.
///
define method get-input-buffer 
    (stream :: <buffered-stream>,
     #key wait? :: <boolean> = #t,
          bytes :: false-or(<integer>))
 => buffer :: false-or(<buffer>);
  // Isolate the calling thread's access to the stream.
  lock-stream(stream);
  // Make sure the thread does not already hold the buffer.
  if (stream.buffer-held?)
    error("Application already holds stream's buffer -- %=.", stream);
  else
    stream.buffer-held? := #t;
  end;
  do-get-input-buffer(stream, wait?: wait?, bytes: bytes);
end method get-input-buffer;

/// release-input-buffer -- Exported.
///
define method release-input-buffer (stream :: <buffered-stream>);
  check-buffer-held(stream);
  do-release-input-buffer(stream);
  stream.buffer-held? := #f;
  // Unlock the lock obtained in get-input-buffer.
  unlock-stream(stream);
end method release-input-buffer;

/// with-input-buffer -- Exported.
///

/// next-input-buffer -- Exported.
///
define method next-input-buffer
    (stream :: <buffered-stream>, 
     #key wait? :: <boolean> = #t, 
          bytes :: false-or(<integer>))
 => buffer :: false-or(<buffer>);
  check-buffer-held(stream);
  do-next-input-buffer(stream, wait?: wait?, bytes: bytes);
end method next-input-buffer;

/// input-available-at-source? -- Exported.
///
define method input-available-at-source? (stream :: <buffered-stream>)
  => input-available? :: <boolean>;
  check-buffer-held(stream);
  do-input-available-at-source?(stream);
end method input-available-at-source?; 

/// get-output-buffer -- Exported.
///
define method get-output-buffer
    (stream :: <buffered-stream>, #key bytes :: <integer> = 1) 
 => buffer :: <buffer>;
  // Isolate the calling thread's access to the stream.
  lock-stream(stream);
  // Make sure the thread does not already hold the buffer.
  if (stream.buffer-held?)
    error("Application already holds stream's buffer -- %=.", stream);
  else
    stream.buffer-held? := #t;
  end;
  do-get-output-buffer(stream, bytes: bytes);
end method get-output-buffer;

/// release-output-buffer -- Exported.
///
define method release-output-buffer (stream :: <buffered-stream>);
  check-buffer-held(stream);
  do-release-output-buffer(stream);
  stream.buffer-held? := #f;
  // Unlock the lock obtained in get-input-buffer.
  unlock-stream(stream);
end method release-output-buffer;

/// with-output-buffer -- Exported.
///

/// next-output-buffer -- Exported.
///
define method next-output-buffer
    (stream :: <buffered-stream>, #key bytes :: <integer> = 1)
  => buffer :: <buffer>;
  check-buffer-held(stream);
  do-next-output-buffer(stream, bytes: bytes);
end method next-output-buffer;

/// force-ouput-buffers -- Exported.
///
define method force-output-buffers (stream :: <buffered-stream>);
  check-buffer-held(stream);
  do-force-output-buffers(stream);
end method force-output-buffers;

/// synchronize -- Exported.
///
define method synchronize (stream :: <buffered-stream>);
  check-buffer-held(stream);
  do-synchronize(stream);
end method synchronize;

/// buffer-subsequence -- Exported.
///
define open generic buffer-subsequence
    (buf :: <buffer>, result-class :: <class>,
     start :: <buffer-index>, stop :: <buffer-index>)
 => result :: <sequence>;

define sealed method buffer-subsequence
    (buf :: <buffer>,
     result-class :: one-of(<byte-string>, <byte-vector>, <buffer>),
     start :: <buffer-index>, stop :: <buffer-index>)
 => result :: type-union(<byte-string>, <byte-vector>, <buffer>);
  if (stop > buf.size)
    error("Bounds error in buffer -- %d.", stop);
  end;
  if (start < 0)
    error("Bounds error in buffer -- %d.", start);
  end;
  let len = (stop - start);
  let res = make(result-class, size: len);
  copy-bytes(res, 0, buf, start, len);
  res;
end method;

/// copy-into-buffer! -- Exported.
///
define open generic copy-into-buffer!
    (buf :: <buffer>, buf-start :: <buffer-index>, source :: <sequence>, 
     #key start :: <integer>, // = 0,
          end: src-end :: false-or(<integer>)); // = source.size);

define sealed method copy-into-buffer!
    (buf :: <buffer>, buf-start :: <buffer-index>,
     source :: type-union(<byte-string>, <byte-vector>, <buffer>),
     #key start :: <integer> = 0,
          end: stop :: false-or(<integer>));
  let source-size :: <integer> = source.size;
  let stop :: <integer> = stop | source-size;

  // Do lots of bounds checking.
  if (start < 0)
    error("Bounds error in source -- %d.", start);
  end;
  if (stop > source-size)
    error("Bounds error in source -- %d.", stop);
  end;
  if (start > stop)
    error("Start, %d, must be less than or equal to end, %d.", start, stop);
  end;
  if (buf-start < 0)
    error("Bounds error in buffer -- %d.", buf-start);
  end;
  if ((buf-start + (stop - start))  > buf.size)
    error("Insufficient number of bytes in buffer after specified start, %d.",
	  buf-start);
  end;
  // Do the copy.
  copy-bytes(buf, buf-start, source, start, (stop - start));
end method copy-into-buffer!;

/// copy-from-buffer! -- Exported.
///
define open generic copy-from-buffer!
    (buf :: <buffer>, buf-start :: <buffer-index>, destination :: <sequence>,
     #key start :: <integer>, // = 0
          end: dest-end :: <integer>); // = destination.size);

define method copy-from-buffer!
    (buf :: <buffer>,
     buf-start :: <buffer-index>,
     destination :: type-union(<byte-string>, <byte-vector>, <buffer>),
     #key start :: <integer> = 0,
          end: stop :: <integer> = destination.size);
  // Do lots of bounds checking.
  if ((buf-start + (stop - start))  > buf.size)
    error("Insufficient number of bytes in buffer after specified start, %d.",
	  buf-start);
  end;
  if (buf-start < 0)
    error("Bounds error in buffer -- %d.", buf-start);
  end;
  if (start < 0)
    error("Bounds error in destination -- %d.", start);
  end;
  if (stop > destination.size)
    error("Bounds error in destination -- %d.", stop);
  end;
  if (start > stop)
    error("Start, %d, must be less than or equal to end, %d.", start, stop);
  end;
  // Do the copy.
  copy-bytes(destination, start, buf, buf-start, (stop - start));
end method copy-from-buffer!;

//// Stream Extension Protocol
////

/// do-get-input-buffer -- Exported.
///
define open generic do-get-input-buffer 
    (stream :: <buffered-stream>, 
     #key wait? :: <boolean>, // = #t, 
          bytes :: false-or(<integer>))
 => buffer :: false-or(<buffer>);

/// do-release-input-buffer -- Exported.
///
define open generic do-release-input-buffer (stream :: <buffered-stream>);

/// do-next-input-buffer -- Exported.
///
define open generic do-next-input-buffer
    (stream :: <buffered-stream>,
     #key wait? :: <boolean>, // = #t, 
          bytes :: false-or(<integer>))
 => buffer :: false-or(<buffer>);

/// do-input-available-at-source? -- Exported.
///
define open generic do-input-available-at-source? (stream :: <buffered-stream>)
  => input-available? :: <boolean>;

/// do-get-output-buffer -- Exported.
///
define open generic do-get-output-buffer
    (stream :: <buffered-stream>, #key bytes :: <integer>) //  = 1) 
 => buffer :: <buffer>;

/// do-release-output-buffer -- Exported.
///
define open generic do-release-output-buffer (stream :: <buffered-stream>);

/// do-next-output-buffer -- Exported.
///
define open generic do-next-output-buffer
    (stream :: <buffered-stream>, #key bytes :: <integer>) // = 1)
  => buffer :: <buffer>;

/// do-force-ouput-buffers -- Exported.
///
define open generic do-force-output-buffers (stream :: <buffered-stream>);

/// synchronize -- Exported.
///
define open generic do-synchronize (stream :: <buffered-stream>);
