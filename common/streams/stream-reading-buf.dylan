module: Streams
author: Ben Folk-Williams
synopsis: Efficient buffered versions of the reading conveience functions.
RCS-header: $Header: /scm/cvs/src/common/streams/stream-reading-buf.dylan,v 1.1 1998/05/03 19:55:04 andreas Exp $
copyright: See below.

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

/// read-to 
///
define method read-to (stream :: <buffered-stream>, element :: <object>,
		       #key on-end-of-stream :: <object>
			      = $not-supplied,
		            test :: <function> = \==)
 => (sequence-or-eof :: <object>, found? :: <boolean>);
  block ()
    let buf :: false-or(<buffer>) = get-input-buffer(stream);
    if (~ buf)
      if (on-end-of-stream == $not-supplied)
	error(make(<end-of-stream-error>, stream: stream));
      else
	values(on-end-of-stream, #f);
      end if;
    else
      let start :: <integer> = buf.buffer-next;
      let stop :: <integer> = buf.buffer-end;
      let elt :: <byte> = as(<integer> /***/, element);
      for (i from start below stop, until: test(buf[i], elt))
      finally
	let seq-type :: <type> = type-for-sequence(stream.stream-element-type);
	if (i == stop)
	  buf.buffer-next := stop;
	  values(buffer-subsequence(buf, seq-type, start, stop), #f);
	else
	  buf.buffer-next := i + 1; // Consume boundary elt.
	  values(buffer-subsequence(buf, seq-type, start, i), #t);
	end if;
      end for;
    end if;
  cleanup
    release-input-buffer(stream);
  end block;
end method read-to;

/// read-through 
///
define method read-through (stream :: <buffered-stream>, element :: <object>,
				   #key on-end-of-stream :: <object>
				          = $not-supplied,
				        test :: <function> = \==)
 => (sequence-or-eof :: <object>, found? :: <boolean>);
  block ()
    let buf :: false-or(<buffer>) = get-input-buffer(stream);
    if (~ buf)
      if (on-end-of-stream == $not-supplied)
	error(make(<end-of-stream-error>, stream: stream));
      else
	values(on-end-of-stream, #f);
      end if;
    else
      let start :: <integer> = buf.buffer-next;
      let stop :: <integer> = buf.buffer-end;
      let elt :: <byte> = as(<integer> /***/, element);
      for (i from start below stop, until: test(buf[i], elt))
      finally
	let seq-type :: <type> = type-for-sequence(stream.stream-element-type);
	if (i == stop)
	  buf.buffer-next := stop;
	  values(buffer-subsequence(buf, seq-type, start, stop), #f);
	else
	  buf.buffer-next := i + 1; // Consume boundary elt.
	  values(buffer-subsequence(buf, seq-type, start, i + 1), #t);
	end if;
      end for;
    end if;
  cleanup
    release-input-buffer(stream);
  end block;
end method read-through;

/// read-to-end 
///
define method read-to-end (stream :: <buffered-stream>)
 => sequence :: <sequence>;
  block ()
    let buf :: false-or(<buffer>) = get-input-buffer(stream);
    let res-type :: <type> = type-for-sequence(stream.stream-element-type);
    if (~ buf)
      make(res-type, size: 0);
    else
      let res :: res-type = buffer-subsequence(buf, res-type,
					       buf.buffer-next,
					       buf.buffer-end);
      buf.buffer-next := buf.buffer-end;
      buf := next-input-buffer(stream);
      while (buf)
	res := concatenate(res, buffer-subsequence(buf, res-type,
						   buf.buffer-next,
						   buf.buffer-end)); 
	buf.buffer-next := buf.buffer-end;
	buf := next-input-buffer(stream);
      end while;
      res;
    end if;
  cleanup
    release-input-buffer(stream);
  end block;
end method read-to-end;

/// skip-through 
///
define method skip-through  (stream :: <buffered-stream>, element :: <object>,
				    #key test :: <function> = \==)
 => found? :: <boolean>;
  block ()
    let buf :: false-or(<buffer>) = get-input-buffer(stream);
    if (~ buf)
      #f;
    else
      let start :: <integer> = buf.buffer-next;
      let stop :: <integer> = buf.buffer-end;
      let elt :: <byte> = as(<integer> /***/, element);
      for (i from start below stop, until: test(buf[i], elt))
      finally
	let seq-type :: <type> = type-for-sequence(stream.stream-element-type);
	if (i == stop)
	  buf.buffer-next := stop;
	  #f;
	else
	  buf.buffer-next := i + 1; // Consume boundary elt.
	  #t;
	end if;
      end for;
    end if;
  cleanup
    release-input-buffer(stream);
  end block;
end method skip-through;
