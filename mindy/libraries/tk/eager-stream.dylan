module: eager-stream

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
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

define library stream-extensions
  use dylan;
  use streams;

  export eager-stream;
end library stream-extensions;

define module eager-stream
  use dylan;
  use extensions;
  use streams;

  export <eager-stream>;
end module eager-stream;

define class <eager-stream> (<buffered-stream>)
  slot sub-stream :: <buffered-stream>, required-init-keyword: #"stream";
  slot buffer :: false-or(<buffer>), init-value: #f;
end class <eager-stream>;

define method initialize (obj :: <eager-stream>, #next next, #key, #all-keys)
  next();
  select (obj.stream-element-type)
    <byte> => #t;
    <byte-character> => #t;
    otherwise =>
      error("<eager-stream>s can only wrap streams that store bytes.");
  end select;
end method initialize;

define method stream-element-type
    (stream :: <eager-stream>) => (result :: <type>);
  stream.sub-stream.stream-element-type;
end method stream-element-type;

define method close (stream :: <eager-stream>, #key, #all-keys) => ();
  close(stream.sub-stream);
end method close;

define method do-get-input-buffer
    (stream :: <eager-stream>, #key wait?, bytes)
 => (buffer :: false-or(<buffer>));
  error("do-get-input-buffer not meaningful for <eager-stream>s.");
end method do-get-input-buffer;

define method do-release-input-buffer
    (stream :: <eager-stream>)
 => ();
  error("do-release-input-buffer not meaningful "
	  "for <eager-stream>s.");
end method do-release-input-buffer;

define method do-next-input-buffer
    (stream :: <eager-stream>, #key wait?, bytes)
 => (new :: false-or(<buffer>));
  error("do-next-input-buffer not meaningful "
	  "for <eager-stream>s.");
end method do-next-input-buffer;

define method do-input-available-at-source?
    (stream :: <eager-stream>) => (input-available? :: <boolean>);
  #f;
end method do-input-available-at-source?;

define method do-get-output-buffer
    (stream :: <eager-stream>, #key bytes)
 => (buffer :: <buffer>);
  stream.buffer := get-output-buffer(stream.sub-stream, bytes: bytes);
end method do-get-output-buffer;

define constant newline-value = as(<integer>, '\n');
define method do-release-output-buffer
    (stream :: <eager-stream>)
 => ();
  let buff = stream.buffer;
  let next = buff.buffer-next;
  if (~buff)
    error("Attempt to release a buffer which isn't held: %=.", stream);
  end if;
  stream.buffer := #f;

  for (i from next - 1 to 0 by -1, until: buff[i] == newline-value)
  finally
    if (i >= 0)
      // ERROR: fails if result-class is anything other than <byte-string>
      let extra = buffer-subsequence(buff, <byte-string>, i + 1, next);
      buff.buffer-next := i + 1;
      let new-buff = next-output-buffer(stream.sub-stream, bytes: extra.size);
      force-output-buffers(stream.sub-stream);
      synchronize(stream.sub-stream);
      copy-into-buffer!(new-buff, buff.buffer-next, extra);
      new-buff.buffer-next := extra.size;
      release-output-buffer(stream.sub-stream);
    else
      release-output-buffer(stream.sub-stream);
    end if;
  end for;
end method do-release-output-buffer;

define method do-next-output-buffer (stream :: <eager-stream>, #key bytes)
 => (result :: <buffer>);
  next-output-buffer(stream.sub-stream, bytes: bytes);
end method do-next-output-buffer;

define method do-force-output-buffers (stream :: <eager-stream>) => ();
  force-output-buffers(stream.sub-stream);
end method do-force-output-buffers;

define method do-synchronize
    (stream :: <eager-stream>) => ();
  synchronize(stream.sub-stream);
end method do-synchronize;
