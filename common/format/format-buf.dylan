module: format
author: Robert Stockton (rgs@cs.cmu.edu).
synopsis: This file implements a simple mechanism for formatting output.
copyright: See below.
rcs-header: $Header: /scm/cvs/src/common/format/format-buf.dylan,v 1.6 2003/10/07 22:29:46 housel Exp $

///======================================================================
///
/// Copyright (c) 1994  Carnegie Mellon University
/// All rights reserved.
/// 
/// Use and copying of this software and preparation of derivative
/// works based on this software are permitted, including commercial
/// use, provided that the following conditions are observed:
/// 
/// 1. This copyright notice must be retained in full on any copies
///    and on appropriate parts of any derivative works.
/// 2. Documentation (paper or online) accompanying any system that
///    incorporates this software, or any part of it, must acknowledge
///    the contribution of the Gwydion Project at Carnegie Mellon
///    University, and the Gwydion Dylan Maintainers.
/// 
/// This software is made available "as is".  Neither the authors nor
/// Carnegie Mellon University make any warranty about the software,
/// its performance, or its conformity to any specification.
/// 
/// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
/// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
/// Also, see http://www.gwydiondylan.org/ for updates and documentation. 
///
///======================================================================
///

/// ### Instances of '<integer> /***/' should be changed to '<byte>'
/// when d2c can deal with as(<byte>,...).
///

/// This code was modified at Harlequin, Inc. to work with the new Streams
/// Library designed by Harlequin and CMU.
///
/// MAINTAINER WARNING: Robert wrote this code to use a <buffer-desc> to
/// pass buffer state around between his functions, but he also uses local
/// variables to cache slot values.  Because the buffer, its size, and the
/// next available free element for writing can change across calls to
/// Robert's functions, you have to be very careful to update the local
/// variables after such calls.  Furthermore, he sometimes uses his local
/// variables and sometimes gets the values from the <buffer-desc> in the
/// same sequence of code.  I haven't taken the time to "clean" all this up.
/// I just tried to make sure the local variables that do get uses are kept
/// up to date.
///



/// <buffer-desc>, with-buffer-released, and write-to-buffer.
///

/// <Buffer-desc> -- Internal.
///
// This class encapsulates the status of the buffer so that it can be more
// easily passed around and updated.  By keeping the buffer at hand, we save a
// great deal of time that is otherwise wasted in the buffer access protocol.
//
define class <buffer-desc> (<object>)
  slot stream :: <stream>, required-init-keyword: #"stream";
  slot buffer :: <buffer>, required-init-keyword: #"buffer";
  // Bill changed the name from "next".  That name is often used as a local
  // variable, and the code is confusing in those places.  The code is more
  // prone to bugs when someone else comes along to modify it because that
  // later someone may call the 'next' function where "next" is bound to an
  // integer.
  slot next-ele :: <integer>, required-init-keyword: #"next";
  slot limit :: <integer>, required-init-keyword: #"limit";
end class <buffer-desc>;

/// with-buffer-released -- Internal.
///
// Release the buffer associated with the <buffer-desc>, call the given
// function, and then reacquire the buffer.  This allows us to call arbitrary
// streams code without "losing our place".
//
define function with-buffer-released (bd :: <buffer-desc>, fun :: <function>);
  bd.buffer.buffer-next := bd.next-ele;
  release-output-buffer(bd.stream);
  fun();
  let b = get-output-buffer(bd.stream);
  bd.buffer := b;
  bd.next-ele := b.buffer-next;
  bd.limit := b.buffer-end;
end function;

/// write-to-buffer -- Internal.
///
// Dump a string into a buffer, by whatever means seem easiest at the time.
// We aim to be very fast in the typical case, while sacrificing a bit in the
// rarer cases.
//
define function write-to-buffer (str :: <byte-string>, bd :: <buffer-desc>)
  let sz = str.size;
  let next = bd.next-ele;
  if (sz <= bd.limit - next)
    copy-into-buffer!(bd.buffer, next, str);
    bd.next-ele := next + sz;
  elseif (sz > bd.limit)
    // worst case -- punt to standard code.
    with-buffer-released(bd, curry(write, bd.stream, str));
  else
    // Request the number of bytes we know we need for str.  Even though most
    // streams will only have one buffer, since we know str.size is <= the
    // buffer's size, we shouldn't be causing new consing of buffers to write
    // str to a buffer all at once.
    bd.buffer.buffer-next := next;
    let buf :: <buffer> = next-output-buffer(bd.stream, bytes: sz);
    bd.buffer := buf;
    next := buf.buffer-next;
    copy-into-buffer!(buf, next, str);
    bd.next-ele := next + sz;
    bd.limit := buf.buffer-end;
  end if;
end function;



/// Format Method for <buffered-stream>.
///

define method format (stream :: <buffered-stream>,
		      control-string :: <byte-string>,
		      #rest args)
    => ();
  let control-len :: <integer> = control-string.size;
  let start :: <integer> = 0;
  let arg-i :: <integer> = 0;
  // Ensure all output is contiguous at stream's destination.
  // Getting the buffer is insufficient since we let go of it sometimes.
  lock-stream(stream);
  let buff = get-output-buffer(stream);
  // Robert's code sometimes referenced bd.limit and sometimes buff-size.
  // I ensure buff-size is kept up to date and use it.
  let buff-size :: <buffer-index> = buff.buffer-end;
  let bd = make(<buffer-desc>, stream: stream, buffer: buff,
		next: buff.buffer-next, limit: buff-size);
  block (exit)
    while (start < control-len)
      // Skip to dispatch char.
      for (i :: <integer> from start below control-len,
	   // Rebind buff, instead of using binding outside the 'block',
	   // because the buffer can change depending on the stream.
	   buff :: <buffer> = bd.buffer then buff,
	   buff-index :: <buffer-index> from bd.next-ele below bd.limit,
	   until: (control-string[i] == $dispatch-char)
	            | (control-string[i] == '\n'))
	buff[buff-index] := as(<integer> /***/, control-string[i]);
      finally
	bd.next-ele := buff-index;
	if (i == control-len)
	  exit();
	else
	  start := i;
	end;
      end for;

      if (bd.next-ele == buff-size)
	get-next-output-buffer(bd, buff-size);
	buff-size := bd.limit;
      elseif (control-string[start] == '\n')
	with-buffer-released(bd, curry(new-line, stream));
	start := start + 1;  // Skip % and dispatch char.
	buff-size := bd.limit;
      else
	// Parse for field within which to pad output.
	let (field, field-spec-end)
	  = if (char-classes[as(<integer> /***/, control-string[start + 1])] == #"digit")
	      parse-integer(control-string, start + 1);
	    end;
	if (field)
	  if (buf-do-dispatch-padded(field, control-string[field-spec-end],
				     bd, element(args, arg-i, default: #f)))
	    arg-i := arg-i + 1;
	  end if;
	  start := field-spec-end + 1;  // Add one to skip dispatch char.
	  // Since buf-do-dispatch-padded could have indirectly called
	  // next-output-buffer, Robert needs to keep buff-size up to date.
	  // He always gets the buffer and buffer-next from his bd buffer
	  // descriptor, so they don't need to be updated here.
	  buff-size := bd.limit;
	else
	  if (buf-do-dispatch(control-string[start + 1], bd,
			      element(args, arg-i, default: #f)))
	    arg-i := arg-i + 1;
	  end;
	  start := start + 2;  // Skip % and dispatch char.
	  // Must update buff-size.  See comment above.
	  buff-size := bd.limit;
	end;
      end if;
    end while;
  cleanup
    bd.buffer.buffer-next := bd.next-ele;
    release-output-buffer(stream);
    unlock-stream(stream);
  exception (condition :: <condition>)
    signal(condition);          // re-signal after running cleanup
  end;
end method;
    
/// get-next-output-buffer -- Internal.
///
/// This function calls next-output-buffer and resets all the slots in the
/// buffer descriptor that Robert passes around.
///
define function get-next-output-buffer (bd :: <buffer-desc>,
					next :: <buffer-index>)
    => ();
  bd.buffer.buffer-next := next;
  let buf = next-output-buffer(bd.stream);
  bd.buffer := buf;
  bd.next-ele := buf.buffer-next;
  bd.limit := buf.buffer-end;
end function;



/// Doing the Control String Dispatch.
///

/// buf-do-dispatch -- Internal.
///
/// This function dispatches on char, which should be a format directive.  The
/// return value indicates whether to consume one format argument; otherwise,
/// consume none.  The buffer description "bd" should be destructively
/// modified to reflect the final state of the stream's buffer.
///
define function buf-do-dispatch
    (char :: <byte-character>, bd :: <buffer-desc>, arg)
    => consumed-arg? :: <boolean>;
  let stream = bd.stream;
  select (char by \==)
    ('s'), ('S') =>
      if (instance?(arg, <byte-string>))
	// Simulate "write-message" upon the argument.  This code must be
	// changed if the semantics of "write-message" changes.
	write-to-buffer(arg, bd);
      else
	with-buffer-released(bd, curry(print-message, arg, stream));
      end if;
      #t;
    ('c'), ('C') =>
      select (arg by instance?)
	<byte-character> =>
	  // Simulate "write-message" upon the argument.  This code must be
	  // changed if the semantics of "write-message" changes.
	  let next = bd.next-ele;
	  if (next == bd.limit)
	    get-next-output-buffer(bd, next);
	    next := bd.next-ele;
	  end if;
	  bd.buffer[next] := as(<integer> /***/, arg);
	  bd.next-ele := next + 1;
	<character> =>
	  with-buffer-released(bd, curry(print-message, arg, stream));
	otherwise =>
	  error("The %%C format directive only works for characters: %=", arg);
      end select;
      #t;
    ('=') =>
      with-buffer-released(bd, curry(print, arg, stream));
      #t;
    ('d'), ('D') =>
      buf-format-integer(arg, 10, bd);
      #t;
    ('b'), ('B') =>
      buf-format-integer(arg, 2, bd);
      #t;
    ('o'), ('O') =>
      buf-format-integer(arg, 8, bd);
      #t;
    ('x'), ('X') =>
      buf-format-integer(arg, 16, bd);
      #t;
    ('m'), ('M') =>
      with-buffer-released(bd, curry(arg, stream));
      #t;
    ('%') =>
      let next = bd.next-ele;
      if (next == bd.limit)
	get-next-output-buffer(bd, next);
	next := bd.next-ele;
      end if;
      bd.buffer[next] := as(<integer> /***/, '%');
      bd.next-ele := next + 1;
      #f;
    otherwise =>
      error("Unknown format dispatch character, %c", char);
  end;
end function;

/// buf-do-dispatch-padded -- Internal.
///
/// This method performs the same operation as "buf-do-dispatch", but the
/// result will be padded according to the field specification in "field".
///
define function buf-do-dispatch-padded
    (field :: <integer>, char :: <byte-character>, bd :: <buffer-desc>, arg)
    => (consumed-arg? :: <boolean>);
  // Capture output in string and compute padding.
  // Assume the output is very small in length.
  let s = make(<byte-string-stream>,
	       contents: make(<byte-string>, size: 80),
	       direction: #"output");
  let consumed-arg? = do-dispatch(char, s, arg);
  let output = s.stream-contents;
  let output-len :: <integer> = output.size;
  let padding :: <integer> = (abs(field) - output-len);
  case
    (padding < 0) =>
      write-to-buffer(output, bd);
    (field > 0) =>
      let next = bd.next-ele;
      let span = next + padding;
      if (span > bd.limit)
	write-to-buffer(make(<byte-string>, size: padding, fill: ' '), bd);
      else
	fill!(bd.buffer, as(<integer> /***/, ' '), start: next, end: span);
	bd.next-ele := span;
      end if;
      write-to-buffer(output, bd);
    otherwise =>
      write-to-buffer(output, bd);
      let next = bd.next-ele;
      let span = next + padding;
      if (span > bd.limit)
	write-to-buffer(make(<byte-string>, size: padding, fill: ' '), bd);
      else
	fill!(bd.buffer, as(<integer> /***/, ' '), start: next, end: span);
	bd.next-ele := span;
      end if;
  end case;
  consumed-arg?;
end function;


define constant $ascii-digits
  = map-as(<byte-vector>, curry(as, <integer> /***/), $digits);

/// buf-format-integer -- internal
//
define method buf-format-integer
    (arg :: <extended-integer>, radix :: limited(<integer>, min: 2, max: 36),
     bd :: <buffer-desc>)
    => ();
  let buf = bd.buffer;
  let next = bd.next-ele;
  let size = bd.limit;
  
  // Print this digit (and those which precede it).
  local method next-digit (arg :: <extended-integer>) => ();
	  let (quotient, remainder) = floor/(arg, radix);
	  unless (zero?(quotient))
	    next-digit(quotient);
	  end unless;
	  if (next == size)
	    get-next-output-buffer(bd, next);
	    buf := bd.buffer;
	    next := bd.next-ele;
	    size := bd.limit;
	  end if;
	  buf[next] := as(<integer>, $digits[as(<integer>, remainder)]);
	  next := next + 1;
	end method next-digit;
  if (negative?(arg))
    if (next == size) 
      get-next-output-buffer(bd, next);
      buf := bd.buffer;
      next := bd.next-ele;
      size := bd.limit;
    end if;
    buf[next] := as(<integer>, '-');
    next := next + 1;
    next-digit(-arg);
  else
    next-digit(arg);
  end;
  bd.next-ele := next;
end method buf-format-integer;

/// buf-format-integer -- internal
//
// This is pretty much the same as above, but is specialized to fixed
// integers so that no generic operations will be required.
//
define method buf-format-integer (arg :: <integer>,
				  radix :: limited(<integer>, min: 2, max: 36),
				  bd :: <buffer-desc>)
    => ();
  let buf = bd.buffer;
  let next = bd.next-ele;
  let size = bd.limit;
  // Print this digit (and those which precede it).
  local
    method append-char (byte :: <byte>) => ();
      if (next == size)
	get-next-output-buffer(bd, next);
	buf := bd.buffer;
	next := bd.next-ele;
	size := bd.limit;
      end if;
      buf[next] := byte;
      next := next + 1;
    end method append-char,
    method next-digit (arg :: limited(<integer>, min: 0)) => ();
      let (quotient, remainder) = truncate/(arg, radix);
      unless (zero?(quotient))
	next-digit(quotient);
      end unless;
      append-char($ascii-digits[remainder]);
    end method next-digit;
  if (negative?(arg))
    append-char(as(<integer> /***/, '-'));
    let (quotient, remainder) = truncate/(arg, radix);
    unless (zero?(quotient))
      next-digit(- quotient);
    end unless;
    append-char($ascii-digits[- remainder]);
  else
    next-digit(arg);
  end;
  bd.next-ele := next;
end method buf-format-integer;

define method buf-format-integer
    (arg :: <float>,
     radix :: limited(<integer>, min: 2, max: 36),
     bd :: <buffer-desc>)
 => ();
  //--- Should we really be this compulsive?
  unless (radix == 10)
    error("Can only print floats in base 10");
  end;
  with-buffer-released(bd, curry(print, arg, bd.stream));
end method;

