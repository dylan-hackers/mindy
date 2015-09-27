module: Streams
author: Ben Folk-Williams, Bill Chiles
synopsis: Writing to streams.
copyright: see below

//======================================================================
//
// Copyright (c) 1994, 1996  Carnegie Mellon University
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

/// write-element
///

// Doug asks, "Isn't there a 'gotcha' when using a parameter called
// "element" when you want to do indexing of sequences? -- Changed
// parameter "element" to "elemnt"
define open generic write-element (stream :: <stream>, elemnt :: <object>)
 => ();

define method write-element (stream :: <buffered-stream>, elemnt :: <object>)
 => ();
  block ()
    let buf :: <buffer> = get-output-buffer(stream);
    let next :: <buffer-index> = buf.buffer-next;
    buf[next] := as(<integer>, elemnt);
    buf.buffer-next := next + 1;
  cleanup
    release-output-buffer(stream);
  end block;
end method write-element;

define sealed domain write-element(<fd-stream>, <object>);
define sealed domain write-element(<buffered-byte-string-output-stream>,
				   <object>);

define sealed method write-element (stream :: <simple-sequence-stream>,
				    elemnt :: <object>)
 => ();
  block ()
    lock-stream(stream);
    check-stream-open(stream);
    check-output-stream(stream);
    if (stream.position == stream.stream-end)
      if (stream.stream-end == stream.contents.size)
	grow-stream-sequence!(stream, 
			      stream.contents.size + $default-grow-amount);
      end if;
      stream.stream-end := stream.stream-end + 1;
    end if;
    stream.contents[stream.position] := elemnt;
    stream.position := stream.position + 1;
  cleanup
    unlock-stream(stream);
  end block;
end method;

/// write
///
define open generic write (stream :: <stream>, sequence :: <sequence>,
			   #key start :: <integer>, // = 0
			        end: stop :: <integer>) // = sequence.size
 => ();

define method write (stream :: <buffered-stream>, sequence :: <sequence>,
		     #key start :: <integer> = 0,
		          end: stop :: <integer> = sequence.size)
 => ();
  block (exit-loop)
    let buf :: <buffer> = get-output-buffer(stream);
    let buf-capacity :: <buffer-index> = (buf.buffer-end - buf.buffer-next);
    let buf-start :: <buffer-index> = buf.buffer-next;
    let partial-stop :: <integer> = (start + buf-capacity);
    while (#t)
      if (partial-stop >= stop)
	let this-copy :: <integer> = (stop - start);
	copy-sequence!(buf, buf-start, sequence, start, this-copy);
	buf.buffer-next := buf-start + this-copy;
	exit-loop();
      else
	copy-sequence!(buf, buf-start, sequence, start, buf-capacity);
	buf.buffer-next := buf.buffer-end;
	buf := next-output-buffer(stream);
	buf-start := buf.buffer-next;
	buf-capacity := buf.buffer-end - buf-start;
	start := partial-stop;
	partial-stop := partial-stop + buf-capacity;
      end if;
    end while;
  cleanup
    release-output-buffer(stream);
  end block;
end method write;

define sealed domain write(<fd-stream>, <sequence>);
define sealed domain write(<buffered-byte-string-output-stream>, <sequence>);

define sealed method write (stream :: <simple-sequence-stream>,
			    sequence :: <sequence>,
			    #key start :: <integer> = 0,
			         end: stop :: <integer> = sequence.size)
 => ();
  block ()
    lock-stream(stream);
    check-stream-open(stream);
    check-output-stream(stream);
    let needed :: <integer> = stop - start;
    let available :: <integer> = stream.contents.size - stream.position;
    let new-pos :: <integer> = stream.position + needed;
    if (needed > available)
      grow-stream-sequence!(stream, new-pos);
    end if;
    if (new-pos > stream.stream-end)
      stream.stream-end := new-pos;
    end if;
    copy-sequence!(stream.contents, stream.position, sequence, start, needed);
    stream.position := new-pos;
  cleanup
    unlock-stream(stream);
  end block;
end method write;
   
/// force-output
///
define open generic force-output (stream :: <stream>) => ();

define method force-output (stream :: <buffered-stream>) => ();
  block ()
    let buf :: <buffer> = get-output-buffer(stream);
    if (buf.buffer-next ~== 0)
      force-output-buffers(stream);
    end if;
  cleanup
    release-output-buffer(stream);
  end block;
end method force-output;

define sealed domain force-output(<fd-stream>);
define sealed domain force-output(<buffered-byte-string-output-stream>);

define inline sealed method force-output (stream :: <simple-sequence-stream>)
 => ();
end method force-output;

/// synchronize-output
///
define open generic synchronize-output (stream :: <stream>) => ();

define method synchronize-output (stream :: <buffered-stream>) => ();
  block ()
    let buf :: <buffer> = get-output-buffer(stream);
    if (buf.buffer-next ~== 0)
      force-output-buffers(stream);
    end if;
    synchronize(stream);
  cleanup
    release-output-buffer(stream);
  end block;
end method synchronize-output;

define sealed domain synchronize-output(<fd-stream>);
define sealed domain synchronize-output(<buffered-byte-string-output-stream>);

define inline sealed method synchronize-output
    (stream :: <simple-sequence-stream>)
 => ();
end method synchronize-output;

/// discard-ouput
///
define open generic discard-output (stream :: <stream>) => ();

define inline method discard-output (stream :: <stream>) => ();
end method;

define method discard-output (stream :: <buffered-stream>) => ();
  block ()
    let buf :: <buffer> = get-output-buffer(stream, bytes: 0);
    buf.buffer-next := 0;
  cleanup
    release-output-buffer(stream);
  end block
end method discard-output;

define sealed domain discard-output(<fd-stream>);
define sealed domain discard-output(<buffered-byte-string-output-stream>);

define sealed method discard-output (stream :: <simple-sequence-stream>) => ();
  block ()
    lock-stream(stream);
    check-stream-open(stream);
    check-output-stream(stream);
    stream.position := stream.stream-start;
  cleanup
    unlock-stream(stream);
  end block;
end method discard-output;
