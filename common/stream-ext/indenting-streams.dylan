module: indenting-streams
author: William Lott
rcs-header: $Header: /scm/cvs/src/common/stream-ext/indenting-streams.dylan,v 1.1 1998/05/03 19:55:03 andreas Exp $

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

// Types:
//   <indenting-stream>
//      Wrapper stream which outputs indented text with conversions of
//      spaces into tabs.  Keywords include "inner-stream:" and
//      "indentation:".  indentation: is the initial indentation of
//      the stream (default 0); change with the indent() function.
//
// Functions:
//   indent(stream :: <indenting-stream>, delta :: <integer>)
//      Changes the current indentation for stream text.
//   inner-stream(stream :: <indenting-stream>)
//      Returns the inner-stream.



define sealed class <indenting-stream> (<buffered-stream>, <wrapper-stream>)
  slot is-buffer :: <buffer> = make(<buffer>, size: 1024);
  slot is-after-newline? :: <boolean> = #t;
  slot is-column :: <integer> = 0;
  slot is-indentation :: <integer> = 0, init-keyword: indentation:;
end;

define sealed domain make(singleton(<indenting-stream>));
define sealed domain initialize(<indenting-stream>);

define method stream-open? (stream :: <indenting-stream>)
 => open? :: <boolean>;
    stream.inner-stream.stream-open?;
end method stream-open?;

define method stream-element-type (stream :: <indenting-stream>)
 => type :: <type>;
  stream.inner-stream.stream-element-type;
end method stream-element-type;

define method stream-at-end? (stream :: <indenting-stream>)
 => at-end? :: <boolean>;
  stream.inner-stream.stream-at-end?;
end method stream-at-end?;

define method do-get-output-buffer (stream :: <indenting-stream>,
				    #key bytes :: <integer> = 1)
 => buf :: <buffer>;
  let buf :: <buffer> = stream.is-buffer;
  if (bytes > buf.size)
    error("Stream's buffer is not large enough to get %d bytes -- %=",
	  bytes, stream);
  end;
  buf.buffer-next := 0;
  buf.buffer-end := buf.size;
  buf;
end;

define constant $tab = as(<integer>, '\t');
define constant $space = as(<integer>, ' ');
define constant $newline = as(<integer>, '\n');

// This routine does the real work for indenting streams.  Whenever a
// buffer is released, it copies "normal" text from the wrapper's
// buffer to the wrapped stream's buffer.  However, at the same time,
// it looks for tabs, spaces, and newlines and does magical things
// with them.  Tabs are (perhaps temporarily) converted into spaces,
// spaces are inserted after newlines to produce indentation, and
// "initial" spaces are converted into tabs.
//
define method indenting-stream-spew-output
    (stream :: <indenting-stream>, stop :: <buffer-index>)
    => ();
  unless (zero?(stop))
    let target-buffer :: <buffer> = get-output-buffer(stream.inner-stream);
    let target-next :: <integer> = target-buffer.buffer-next;
    let target-size :: <integer> = target-buffer.buffer-end;
    let buffer = stream.is-buffer;
    local
      method spew-n-chars (n :: <integer>, char :: <integer>)
	let available :: <integer> = target-size - target-next;
	while (available < n)
	  for (i :: <integer> from target-next below target-size)
	    target-buffer[i] := char;
	  end;
	  target-buffer.buffer-next := target-size;
	  target-buffer := next-output-buffer(stream.inner-stream);
	  // target-buffer may be different
	  target-next := target-buffer.buffer-next;
	  target-size := target-buffer.buffer-end;
	  n := n - available;
	  available := target-size - target-next;
	end;
	for (i :: <integer> from target-next below target-next + n)
	  target-buffer[i] := char;
	finally
	  target-buffer.buffer-next := (target-next := i);
	end;
      end,
      method spew-range (finish :: <integer>, start :: <integer>)
	let n = finish - start;
	let available = target-size - target-next;
	if (available < n)
	  copy-bytes(target-buffer, target-next, buffer, start, available);
	  target-buffer.buffer-next := target-size;
	  target-buffer := next-output-buffer(stream.inner-stream);
	  // target-buffer may be different
	  target-next := target-buffer.buffer-next;
	  target-size := target-buffer.buffer-end;
	  spew-range(finish, start + available);
	else
	  copy-bytes(target-buffer, target-next, buffer, start, n);
	  target-buffer.buffer-next := (target-next := target-next + n);
	end if;
      end method spew-range;
    local
      method do-indentation (index :: <integer>, col :: <integer>)
// Return type declarations screw up tail calls
//       => (after-newline :: <boolean>, column :: <integer>);
	if (index == stop)
	  values(#t, col);
	else
	  let char :: <integer> = buffer[index];
	  if (char == $newline)
	    spew-range(index + 1, index);
	    do-indentation(index + 1, col);
	  elseif (char == $tab)
	    do-indentation(index + 1, col + 8 - modulo(col, 8));
	  elseif (char == $space)
	    do-indentation(index + 1, col + 1);
	  else
	    let (tabs, space) = floor/(stream.is-indentation + col, 8);
	    spew-n-chars(tabs, $tab);
	    spew-n-chars(space, $space);
	    do-text(index, index, col);
	  end if;
	end if;
      end method do-indentation,
      method do-text (index :: <integer>, start-index :: <integer>,
		      col :: <integer>)
// Return type declarations screw up tail calls
//       => (after-newline :: <boolean>, column :: <integer>);
	if (index == stop)
	  spew-range(index, start-index);
	  values(#f, col);
	else
	  let char :: <integer> = buffer[index];
	  if (char == $tab)
	    spew-range(index, start-index);
	    let spaces = 8 - modulo(col, 8);
	    spew-n-chars($space, spaces);
	    do-text(index + 1, index + 1, col + spaces);
	  elseif (char == $newline)
	    spew-range(index + 1, start-index);
	    do-indentation(index + 1, 0);
	  else
	    do-text(index + 1, start-index, col + 1);
	  end if;
	end if;
      end method do-text;
    let (new-after-newline?, new-column)
      = if (stream.is-after-newline?)
	  do-indentation(0, stream.is-column);
	else
	  do-text(0, 0, stream.is-column);
	end if;
    release-output-buffer(stream.inner-stream);
    stream.is-after-newline? := new-after-newline?;
    stream.is-column := new-column;
  end;
end;

define method do-release-output-buffer (stream :: <indenting-stream>)
 => ();
  let buf :: <buffer> = stream.is-buffer;
  indenting-stream-spew-output(stream, buf.buffer-next);
  buf.buffer-next := 0;
  buf.buffer-end := buf.size;
end;

define method do-next-output-buffer (stream :: <indenting-stream>,
				     #key bytes :: <integer> = 1)
 => buf :: <buffer>;
  let buf :: <buffer> = stream.is-buffer;
  if (bytes > buf.size)
    error("Stream's buffer is not large enough to get %d bytes -- %=",
	  bytes, stream);
  end;
  indenting-stream-spew-output(stream, buf.buffer-next);
  buf.buffer-next := 0;
  buf.buffer-end := buf.size;
  buf;
end;

define method do-force-output-buffers (stream :: <indenting-stream>)
 => ();
  force-output-buffers(stream.inner-stream);
end;  

define method do-synchronize (stream :: <indenting-stream>)
 => ();
  synchronize(stream.inner-stream);
end;

define method close (stream :: <indenting-stream>, #all-keys) => ();
  force-output(stream);
end;

define method indent (stream :: <indenting-stream>, delta :: <integer>)
    => ();
  stream.is-indentation := stream.is-indentation + delta;
end;
