module: Streams
author: Ben Folk-Williams
synopsis: Wrapper Streams.
copyright: See below.
RCS-header: $Header: /scm/cvs/src/common/streams/wrapper-streams.dylan,v 1.1 1998/05/03 19:55:04 andreas Exp $

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
  // ### This slot's init keyword should be required, but right now
  // I can't un-require it in subclasses with d2c.
  slot inner-stream :: <stream>, init-keyword: inner-stream:;
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

/// Default implementation
///

define inline method close (stream :: <wrapper-stream>, #rest keys, #all-keys)
  apply(close, stream.inner-stream, keys);
end method;

define inline method stream-open? (stream :: <wrapper-stream>)
 => open? :: <boolean>;
  stream-open?(stream.inner-stream);
end method;

define inline method stream-element-type (stream :: <wrapper-stream>) 
 => element-type :: <type>;
  stream-element-type(stream.inner-stream);
end method;

define inline method stream-at-end? (stream :: <wrapper-stream>)
 => at-end? :: <boolean>;
  stream-at-end?(stream.inner-stream);
end method;

define inline method read-element
    (stream :: <wrapper-stream>,
     #key on-end-of-stream :: <object> = $not-supplied)
 => element-or-eof :: <object>;
  if (on-end-of-stream == $not-supplied)
    read-element(stream.inner-stream);
  else
    read-element(stream.inner-stream, on-end-of-stream: on-end-of-stream);
  end if;
end method;

// Since GF unread-element is specialized on <positionable-stream>,
// I think it will be necessary to add wrapper stream functionality
// to the default <positionable-stream> method.
//
//define inline method unread-element (stream :: <wrapper-stream>,
//				     element :: <object>)
// => element :: <object>;
//  unread-element(stream.inner-stream, element);
//end method;

define inline method peek (stream :: <wrapper-stream>,
			   #key on-end-of-stream :: <object> = $not-supplied)
 => element-of-eof :: <object>;
  if (on-end-of-stream == $not-supplied)
    peek(stream.inner-stream);
  else
    peek(stream.inner-stream, on-end-of-stream: on-end-of-stream);
  end if;
end method;


define inline method read (stream :: <wrapper-stream>, n :: <integer>,
			   #key on-end-of-stream :: <object> = $not-supplied)
 => sequence-or-eof :: <object>;
  if (on-end-of-stream == $not-supplied)
    read(stream.inner-stream, n);
  else
    read(stream.inner-stream, n, on-end-of-stream: on-end-of-stream);
  end if;
end method;

define inline method read-into!
    (stream :: <wrapper-stream>,
     n :: <integer>,
     sequence :: <mutable-sequence>,
     #key start ::  <integer> = 0,
          on-end-of-stream :: <object> = $not-supplied)
 => count-or-eof :: <object>;
  if (on-end-of-stream == $not-supplied)
    read-into!(stream.inner-stream, n, sequence, start: start);
  else
    read-into!(stream.inner-stream, n, sequence, start: start,
	       on-end-of-stream: on-end-of-stream);
  end if;
end method;

define inline method discard-input (stream :: <wrapper-stream>) => ();
  discard-input(stream.inner-stream);
end method;

define inline method stream-input-available? (stream :: <wrapper-stream>)
 => input-available? :: <boolean>;
  stream-input-available?(stream.inner-stream);
end method;

define inline method write-element (stream :: <wrapper-stream>,
				    element :: <object>)
 => ();
  write-element(stream.inner-stream, element);
end method;

define inline method write (stream :: <wrapper-stream>, sequence :: <sequence>,
			    #key start :: <integer> = 0,
			         end: stop :: <integer> = sequence.size)
 => ();
  write(stream.inner-stream, sequence, start: start, end: stop);
end method;

define inline method force-output (stream :: <wrapper-stream>) => ();
  force-output(stream.inner-stream);
end method;

define inline method synchronize-output (stream :: <wrapper-stream>) => ();
  synchronize-output(stream.inner-stream);
end method;

define inline method discard-output (stream :: <wrapper-stream>) => ();
  discard-output(stream.inner-stream);
end method;

define inline method read-line 
    (stream :: <wrapper-stream>,
     #key on-end-of-stream :: <object> = $not-supplied)
 => (string-or-eof :: <object>, newline? :: <boolean>);
  if (on-end-of-stream == $not-supplied)
    read-line(stream.inner-stream);
  else
    read-line(stream.inner-stream, on-end-of-stream: on-end-of-stream);
  end if;
end method;

define inline method read-line-into!
    (stream :: <wrapper-stream>, string :: <string>,
     #key start :: <integer> = 0, grow? :: <boolean> = #f,
     on-end-of-stream :: <object> = $not-supplied)
 => (string-or-eof :: <object>, newline? :: <boolean>);
  if (on-end-of-stream == $not-supplied)
      read-line-into!(stream.inner-stream, string, start: start, grow?: grow?);
  else
      read-line-into!(stream.inner-stream, string, start: start, grow?: grow?,
		      on-end-of-stream: on-end-of-stream);
  end if;
end method;

define inline method write-line (stream :: <wrapper-stream>, 
				 string :: <string>,
				 #key start :: <integer> = 0,
				      end: stop :: <integer> = string.size)
 => ();
  write-line(stream.inner-stream, string, start: start, end: stop);
end method;

define inline method new-line (stream :: <wrapper-stream>) => ();
  new-line(stream.inner-stream);
end method;
