library: stream-extensions
module: stream-extensions
author: Russ Schaaf (rsbe@andrew.cmu.edu)
synopsis:   Extensions to the stream library.
copyright:  See below.
rcs-header: $Header: /home/housel/work/rcs/gd/src/common/stream-ext/stream-extensions.dylan,v 1.1 1996/03/15 06:43:03 rsbe Exp $

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


// These are two new error classes, they are used to make the
// stream-extensions errors match the Harlequin proposal as much as possible.

// End of stream error is signalled when the user wants to read past the
// end of the stream
//
define class <end-of-stream-error> (<error>)
  slot end-of-stream-stream :: <stream>, init-keyword: #"stream";
end class;

// incomplete read error is signalled when eof is encountered during a read
//
define class <incomplete-read-error> (<end-of-stream-error>)
  slot incomplete-read-sequence :: <sequence>, init-keyword: #"sequence";
  slot incomplete-read-count :: <integer>, init-keyword: #"count";
end class;

define constant $no-default = pair(#f, #f);

// This is the read-to function as described in the Harlequin streams document.
// it takes two required arguments, a stream, and an element.  It returns a 
// <byte-vector> containing all of the elements in the string from the current 
// position up to (but not including) the requested element.
//
define method read-to (stream :: <stream>, element,
		       #key on-end-of-stream = $no-default, test = \==)
 => (final-seq :: <sequence>, found :: <boolean>);
  let final-seq = make(<byte-vector>, size: 0);
  let which-pos = #f;
  let pred = curry(test, as(<byte>, element));
  block(exit)
    // most of the uglier code here is for manipulating buffers
    let (buf, next, stop) = get-input-buffer(stream);
    // if the buffer has been used before...
    if (next ~= stop)
      // search for the element, add it to the final sequence.
      if (which-pos := find-key(buf, pred, failure: #f))
	final-seq := concatenate(final-seq, copy-sequence(buf, start: next,
							  end: which-pos));
	release-input-buffer(stream, which-pos + 1, stop);
	exit(final-seq, #t);
      else
	final-seq := concatenate(final-seq, copy-sequence(buf, start: next,
							  end: stop));
      end if;
    end;
    // main loop.  Keep repeating until either we find the element or we
    // run out of stream.
    for (stop = fill-input-buffer(stream, 0) then fill-input-buffer(stream, 0),
	 until: stop = 0)
      if (which-pos := find-key(buf, pred, failure: #f))
	final-seq := concatenate(final-seq, copy-sequence(buf, start: next,
							  end: which-pos));
	release-input-buffer(stream, which-pos + 1, stop);
	exit(final-seq, #t);
      else
	final-seq := concatenate(final-seq, copy-sequence(buf, start: next,
							  end: stop));
      end if;
    end;
    release-input-buffer(stream, stop, stop);
    // some final error checking, to return the correct result.
    if (on-end-of-stream ~== $no-default)
      exit(on-end-of-stream, #f);
    else
      if (final-seq.empty?)
	error(make(<end-of-stream-error>, stream: stream));
      else
	error(make(<incomplete-read-error>, stream: stream,
		   sequence: final-seq, count: final-seq.size));
      end if;
    end if;
  end block;
end method read-to;

// This functions exacxtly like read-to but it includes the requested element
// in the returned vector.
//
define method read-through (stream :: <stream>, element,
			    #key on-end-of-stream = $no-default,
			    test = \==)
 => (final-seq :: <sequence>, found :: <boolean>);
  let final-seq = make(<byte-vector>, size: 0);
  let which-pos = #f;
  let pred = curry(test, as(<byte>, element));
  block(exit)
    let (buf, next, stop) = get-input-buffer(stream);
    if (next ~= stop)
      if (which-pos := find-key(buf, pred, failure: #f))
	final-seq := concatenate(final-seq, copy-sequence(buf, start: next,
							  end: which-pos + 1));
	release-input-buffer(stream, which-pos + 1, stop);
	exit(final-seq, #t);
      else
	final-seq := concatenate(final-seq, copy-sequence(buf, start: next,
							  end: stop));
      end if;
    end;
    for (stop = fill-input-buffer(stream, 0) then fill-input-buffer(stream, 0),
	 until: stop = 0)
      if (which-pos := find-key(buf, pred, failure: #f))
	final-seq := concatenate(final-seq, copy-sequence(buf, start: next,
							  end: which-pos + 1));
	release-input-buffer(stream, which-pos + 1, stop);
	exit(final-seq, #t);
      else
	final-seq := concatenate(final-seq, copy-sequence(buf, start: next,
							  end: stop));
      end if;
    end;
    release-input-buffer(stream, stop, stop);
    if (on-end-of-stream ~== $no-default)
      exit(on-end-of-stream, #f);
    else
      if (final-seq.empty?)
	error(make(<end-of-stream-error>, stream: stream));
      else
	error(make(<incomplete-read-error>, stream: stream,
		   sequence: final-seq, count: final-seq.size));
      end if;
    end if;
  end block;
end method read-through;

// Reads a stream all the way to eof and returns a <byte-vector> containing
// the result
//
define method read-to-end (stream :: <stream>) => (final-seq :: <sequence>);
  let final-seq = make(<byte-vector>, size: 0);
  let (buf, next, stop) = get-input-buffer(stream);
  if (next ~= stop)
    final-seq := concatenate(final-seq, copy-sequence(buf, start: next,
						      end: stop));
  end if;
  for (stop = fill-input-buffer(stream, 0) then fill-input-buffer(stream, 0),
       until: stop = 0)
    final-seq := concatenate(final-seq, copy-sequence(buf, start: next,
						      end: stop));
  end;
  release-input-buffer(stream, stop, stop);
  final-seq;
end method read-to-end;

// Skips over all elements in a stream until it encounters a requested element
//
define method skip-through (stream :: <stream>, element, #key test = \==) 
 => (found? :: <boolean>);
  let pred = curry(test, as(<byte>, element));
  let which-pos = #f;
  block(exit)
    let (buf, next, stop) = get-input-buffer(stream);
    if (next ~= stop)
      if (which-pos := find-key(buf, pred, failure: #f))
	release-input-buffer(stream, which-pos + 1, stop);
	exit(#t);
      end if;
    end;
    for (stop = fill-input-buffer(stream, 0) then fill-input-buffer(stream, 0),
	 until: stop = 0)
      if (which-pos := find-key(buf, pred, failure: #f))
	release-input-buffer(stream, which-pos + 1, stop);
	exit(#t);
      end if;
    end;
    release-input-buffer(stream, stop, stop);
    exit(#f);
  end block;
end method skip-through;

// writes the new-line character to the output-stream.  If given a keyword
// lines: it will write that many newline characters
//
define method new-line(output-stream :: <stream>, #key lines = 1)
  for (i from 1 to lines)
    write-line("", output-stream);
  end for;
end method new-line;

// read-as-list will read the elements of stream up through the until: element,
// and using the delimiter sequence, it will break the stream up into a list of
// <byte-vector>s.  For example, if the stream my-stream contained the
// following:
// Roger Corman
// Ed Wood
// Stephen Speilberg
// then read-as-list(my-stream, delimiters: #['\n', ' '], until: 'l')
// would produce #("Roger", "Corman", "Ed", "Wood", "Stephen", "Spei")
//
define method read-as-list(stream, #key delimiters = #['\n'],
			   until: end-on = #f, until-test = \==,
			   delimiter-test = \== )
  // ignore end-of-stream errors
  let handler <end-of-stream-error> = method (condition, next) end;
  let string-list = make(<list>, size: 0);
  let working-string = make(<byte-vector>, size: 0);
  if (~end-on)
    working-string := read-to-end(stream);
  else
    working-string := read-to(stream, end-on, test: until-test);
  end if;
  let old-delim = 0;
  // the predicate used to look for a delimiter
  let pred = rcurry(member?, map(curry(as, <byte>), delimiters),
		    test: delimiter-test);
  let i = 0;
  for (delim = find-key(working-string, pred, skip: i)
	 then find-key(working-string, pred, skip: i),
       until: ~delim)
    unless (delim == old-delim)
      string-list := concatenate(string-list,
				 list(copy-sequence(working-string,
						    start: old-delim,
						    end: delim)));
    end unless;
    // add 1 so that the delimiter character won't be part of the string
    old-delim := delim + 1;
    i := i + 1;
  finally
    string-list := concatenate(string-list,
			       list(copy-sequence(working-string,
						  start: old-delim)));
  end for;
  string-list;
end method read-as-list;

// fills a string with the next line in a stream.  This function works as
// described in the harlequin streams document.
//
define method read-line-into! (input-stream :: <stream>, string :: <string>,
			       #key start = 0,
			       on-end-of-stream = $no-default,
			       grow? = #t)
  block(exit)
    let handler <end-of-file> = method (condition, next)
				  if (on-end-of-stream ~== $no-default)
				    exit(on-end-of-stream);
				  else
				    error(make(<end-of-stream-error>,
					       stream: input-stream));
				  end if;
				end;
    let (one-line, newline) = read-line (input-stream, signal-eof?: #t);
    if (grow?)
      exit(replace-subsequence!(string, one-line, start: start), newline);
    else
      if (one-line.size > string.size - start)
	error("Line is too long for string");
      else
	exit(replace-subsequence!(string, one-line, start: start,
				  end: start + one-line.size), newline);
      end if;
    end if;
  end block;
end method read-line-into!;

