module: format
author: chiles@cs.cmu.edu
synopsis: This file implements a simple mechanism for formatting output.
copyright: See below.
rcs-header: $Header: /home/housel/work/rcs/gd/src/common/format/format.dylan,v 1.1 1996/03/20 00:05:55 nkramer Exp $

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



/// format-to-string.
///

/// format-to-string -- Exported.
///
define generic format-to-string (control-string :: <string>, #rest args)
    => result :: <string>;

define method format-to-string (control-string :: <byte-string>, #rest args)
    => result :: <byte-string>;
  // Format-to-string will usually be called for fairly small stuff.
  // Allocating a massive buffer just forces frequent GCs.
  let s = make(<byte-string-output-stream>, size: 200);
  apply(format, s, control-string, args);
  s.string-output-stream-string;
end method;



/// Print-message.
///

/// print-message -- Exported.
///
define open generic print-message (object :: <object>, stream :: <stream>)
    => ();


define sealed method print-message
    (object :: type-union(<string>, <character>), stream :: <stream>)
    => ();
  // Note: the format code in do-dispatch emulates this code upon buffers.
  // Any changes in semantics should be reflected in that routine as well.
  write(object, stream);
end method;

define sealed method print-message (object :: <condition>, stream :: <stream>)
    => ();
  report-condition(object, stream);
end method;

define method print-message (object :: <symbol>, stream :: <stream>)
    => ();
  write(as(<string>, object), stream);
end method;



/// Format.
///
define constant $dispatch-char = '%';

define constant $char-classes :: <simple-object-vector>
  = begin
      let res = make(<vector>, size: 256, fill: #f);
      for (i :: <integer> from as(<integer>, '0') to as(<integer>, '9'))
	res[i] := #"digit";
      end;
      res[as(<integer>, '-')] := #"digit";
      res;
    end;

/// <Buffer-desc> -- Internal.
///
// This class encapsulates the status of the buffer so that it can be more
// easily passed around and updated.  By keeping the buffer at hand, we save a
// great deal of time that is otherwise wasted in the buffer access protocol.
//
define class <buffer-desc> (<object>)
  constant slot stream :: <stream>, required-init-keyword: #"stream";
  slot buffer :: <buffer>, required-init-keyword: #"buffer";
  slot next :: <integer>, required-init-keyword: #"next";
  slot limit :: <integer>, required-init-keyword: #"limit";
  slot buffer-held? :: <boolean> = #t;
end class <buffer-desc>;

define sealed domain make (singleton(<buffer-desc>));
define sealed domain initialize (<buffer-desc>);

/// with-buffer-released -- Internal.
///
// Release the buffer associated with the <buffer-desc>, call the given
// function, and then reacquire the buffer.  This allows us to call arbitrary
// streams code without "losing our place".
//
define inline method with-buffer-released
    (bd :: <buffer-desc>, fun :: <function>) => ();
  bd.buffer-held? := #f;
  release-output-buffer(bd.stream, bd.next);
  fun();
  let (b, n, l) = get-output-buffer(bd.stream);
  bd.buffer-held? := #t;
  bd.buffer := b;
  bd.next := n;
  bd.limit := l;
end method with-buffer-released;


/// write-to-buffer -- Internal.
///
// Dump a string into a buffer, by whatever means seem easiest at the time.
// We aim to be very fast in the typical case, while sacrificing a bit in the
// rarer cases.
//
define method write-to-buffer (str :: <byte-string>, bd :: <buffer-desc>)
    => ();
  let sz = str.size;
  let next = bd.next;
  if (sz <= bd.limit - next)
    copy-into-buffer!(str, bd.buffer, next);
    bd.next := next + sz;
  elseif (sz > bd.limit)
    // worst case -- punt to standard code.
    with-buffer-released(bd, curry(write, str, bd.stream));
  else
    empty-output-buffer(bd.stream, next);
    copy-into-buffer!(str, bd.buffer, 0);
    bd.next := sz;
  end if;
end method write-to-buffer;

define generic format (stream :: <stream>, control-string :: <string>,
		       #rest args)
    => ();

define method format (stream :: <stream>, control-string :: <byte-string>,
		      #rest args)
    => ();
  let control-len :: <integer> = control-string.size;
  let start :: <integer> = 0;
  let arg-i :: <integer> = 0;
  // Ensure all output is contiguous at stream's destination.
  lock-stream(stream);
  let (buff, buff-next, buff-size) = get-output-buffer(stream);
  let bd = make(<buffer-desc>, stream: stream, buffer: buff, next: buff-next,
		limit: buff-size);
  block (exit)
    while (start < control-len)
      // Skip to dispatch char.
      let buff = bd.buffer;
      for (i :: <integer> from start below control-len,
	   buff-index :: <integer> from bd.next below bd.limit,
	   until: (control-string[i] == $dispatch-char))
	buff[buff-index] := as(<integer>, control-string[i]);
      finally
	bd.next := buff-index;
	if (i = control-len)
	  exit();
	else
	  start := i;
	end;
      end for;

      if (bd.next == buff-size)
	empty-output-buffer(stream, bd.next);
	bd.next := 0;
      else
	// Parse for field within which to pad output.
	let (field, field-spec-end)
	  = if ($char-classes[as(<integer>, control-string[start + 1])]
		  == #"digit")
	      parse-integer(control-string, start + 1);
	    end;
	if (field)
	  if (do-dispatch-padded(field, control-string[field-spec-end], bd,
				 element(args, arg-i, default: #f)))
	    arg-i := arg-i + 1;
	  end if;
	  start := field-spec-end + 1;  // Add one to skip dispatch char.
	else
	  if (do-dispatch(control-string[start + 1], bd,
			  element(args, arg-i, default: #f)))
	    arg-i := arg-i + 1;
	  end;
	  start := start + 2;  // Skip % and dispatch char.
	end;
      end if;
    end while;
  cleanup
    if (bd.buffer-held?)
      release-output-buffer(stream, bd.next);
    end if;
    unlock-stream(stream);
  end;
end method;
    
/// do-dispatch -- Internal.
///
/// This function dispatches on char, which should be a format directive.  The
/// return value indicates whether to consume one format argument; otherwise,
/// consume none.  The buffer description "bd" should be destructively
/// modified to reflect the final state of the stream's buffer.
///
define method do-dispatch
    (char :: <byte-character>, bd :: <buffer-desc>, arg)
    => consumed-arg? :: <boolean>;
  let stream = bd.stream;
  select (char by \=)
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
	  let next = bd.next;
	  if (next == bd.limit)
	    empty-output-buffer(bd.stream, next);
	    next := bd.next := 0;
	  end if;
	  bd.buffer[next] := as(<integer>, arg);
	  bd.next := next + 1;
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
      format-integer(arg, 10, bd);
      #t;
    ('b'), ('B') =>
      format-integer(arg, 2, bd);
      #t;
    ('o'), ('O') =>
      format-integer(arg, 8, bd);
      #t;
    ('x'), ('X') =>
      format-integer(arg, 16, bd);
      #t;
    ('m'), ('M') =>
      with-buffer-released(bd, curry(arg, stream));
      #t;
    ('%') =>
      let next = bd.next;
      if (next == bd.limit)
	empty-output-buffer(bd.stream, next);
	next := bd.next := 0;
      end if;
      bd.buffer[next] := as(<integer>, '%');
      bd.next := next + 1;
      #f;
    otherwise =>
      error("Unknown format dispatch character, %=", char);
  end;
end method;

/// do-dispatch-padded -- Internal.
///
/// This method performs the same operation as "do-dispatch", but the result
/// will be padded according to the field specification in "field".
///
define method do-dispatch-padded
    (field :: <integer>, char :: <byte-character>,
     bd :: <buffer-desc>, arg)
 => (consumed-arg? :: <boolean>);
  // Capture output in string and compute padding.
  let s = make(<byte-string-output-stream>);
  let (b, n, l) = get-output-buffer(s);
  let new-bd = make(<buffer-desc>, stream: s, buffer: b,
		    next: n, limit: l);
  let consumed-arg? = do-dispatch(char, new-bd, arg);
  release-output-buffer(s, new-bd.next);
	  
  let output = s.string-output-stream-string;
  let output-len :: <integer> = output.size;
  let padding :: <integer> = (abs(field) - output-len);
  case
    (padding < 0) =>
      write-to-buffer(output, bd);
    (field > 0) =>
      let next = bd.next;
      let span = next + padding;
      if (span > bd.limit)
	write-to-buffer(make(<byte-string>, size: padding, fill: ' '), bd);
      else
	fill!(bd.buffer, as(<integer>, ' '), start: next, end: span);
	bd.next := span;
      end if;
      write-to-buffer(output, bd);
    otherwise =>
      write-to-buffer(output, bd);
      let next = bd.next;
      let span = next + padding;
      if (span > bd.limit)
	write-to-buffer(make(<byte-string>, size: padding, fill: ' '), bd);
      else
	fill!(bd.buffer, as(<integer>, ' '), start: next, end: span);
	bd.next := span;
      end if;
  end case;
  consumed-arg?;
end method do-dispatch-padded;

/// parse-integer -- Internal.
///
/// This function reads an integer from input starting at index.  Index must
/// be at the first digit or a leading negative sign.  This function reads
/// decimal representation, and it stops at the first character that is not
/// a decimal degit.  It returns the integer parsed and the index
/// immediately following the last decimal digit.
///
define method parse-integer (input :: <byte-string>, index :: <integer>)
    => (result :: false-or(<integer>), index :: <integer>);
  let result :: <integer> = 0;
  let negative? = if (input[index] == '-')
		    index := index + 1;
		  end;
  for (i :: <integer> = index then (i + 1),
       len :: <integer> = input.size then len,
       ascii-zero :: <byte> = as(<integer>, '0') then ascii-zero,
       until: ((i == len) |
		 (~ ($char-classes[as(<integer>, input[i])] == #"digit"))))
    result := ((result * 10) + (as(<integer>, input[i]) - ascii-zero));
  finally
    if (result == 0)
      values(#f, index);
    else
      values(if (negative?) (- result) else result end, i);
    end;
  end;
end method;


define constant $digits = "0123456789abcdefghijklmnopqrstuvwxyz";

/// format-integer -- internal
///
define method format-integer
    (arg :: <extended-integer>, radix :: limited(<integer>, min: 2, max: 36),
     bd :: <buffer-desc>)
    => ();
  let buffer = bd.buffer;
  let next = bd.next;
  let size = bd.limit;
  
  // Print this digit (and those which precede it).
  local method next-digit (arg :: <extended-integer>) => ();
	  let (quotient, remainder) = floor/(arg, radix);
	  unless (zero?(quotient))
	    next-digit(quotient);
	  end unless;
	  if (next == size)
	    empty-output-buffer(bd.stream, next);
	    next := 0;
	  end if;
	  buffer[next] := as(<integer>, $digits[as(<integer>, remainder)]);
	  next := next + 1;
	end method next-digit;
  if (negative?(arg))
    if (next == size) 
      empty-output-buffer(bd.stream, next);
      next := 0;
    end if;
    buffer[next] := as(<integer>, '-');
    next := next + 1;
    next-digit(-arg);
  else
    next-digit(arg);
  end;
  bd.next := next;
end;

/// format-integer -- internal
//
// This is pretty much the same as above, but is specialized to fixed
// integers so that no generic operations will be required.
//
define method format-integer
    (arg :: <integer>,
     radix :: limited(<integer>, min: 2, max: 36),
     bd :: <buffer-desc>)
    => ();
  let buffer = bd.buffer;
  let next = bd.next;
  let size = bd.limit;
  
  // Print this digit (and those which precede it).
  local
    method append-char (char :: <byte-character>) => ();
      if (next == size)
	empty-output-buffer(bd.stream, next);
	next := 0;
      end if;
      buffer[next] := as(<integer>, char);
      next := next + 1;
    end method append-char,
    method next-digit (arg :: limited(<integer>, min: 0)) => ();
      let (quotient, remainder) = truncate/(arg, radix);
      unless (zero?(quotient))
	next-digit(quotient);
      end unless;
      append-char($digits[remainder]);
    end method next-digit;
  if (negative?(arg))
    append-char('-');
    let (quotient, remainder) = truncate/(arg, radix);
    unless (zero?(quotient))
      next-digit(-quotient);
    end unless;
    append-char($digits[-remainder]);
  else
    next-digit(arg);
  end;
  bd.next := next;
end;


// Condition-Format method.
//
// Condition-Format is a generic function called by the condition system
// to service its formatting needs.  We supply method on <stream>s that call
// the <stream> specific format defined above.
//
define method condition-format
    (stream :: <stream>, string :: <string>, #rest args) => ();
  apply(format, stream, string, args);
end method condition-format;
