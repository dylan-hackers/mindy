module: Streams
author: Ben Folk-Williams, Bill Chiles
synopsis: Reading from streams.
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

/// read-element -- Exported.
///
define open generic read-element (stream :: <stream>,
				  #key on-end-of-stream :: <object>)
 => element-or-eof :: <object>;

define method read-element (stream :: <buffered-stream>,
			    #key on-end-of-stream :: <object> = $not-supplied)
 => element-or-eof :: <object>;
  block ()
    let buf :: false-or(<buffer>) = get-input-buffer(stream);
    if (buf) // Assume since get-input-buffer returned a buffer
             // at least one byte is available
      let res = as(stream.stream-element-type, buf[buf.buffer-next]);
      buf.buffer-next := buf.buffer-next + 1;
      res;
    else
      if (on-end-of-stream == $not-supplied)
	error(make(<end-of-stream-error>, stream: stream));
      else
	on-end-of-stream;
      end if;
    end if;
  cleanup
    release-input-buffer(stream);
  end block;
end method read-element;

define sealed domain read-element(<fd-stream>);
define sealed domain read-element(<buffered-byte-string-output-stream>);

define sealed method read-element (stream :: <simple-sequence-stream>,
				   #key on-end-of-stream :: <object>
				     = $not-supplied)
 => element-or-eof :: <object>;
  block ()
    lock-stream(stream);
    check-stream-open(stream);
    check-input-stream(stream);
    if (stream.position == stream.stream-end)
      if (on-end-of-stream == $not-supplied)
	error(make(<end-of-stream-error>, stream: stream));
      else
	on-end-of-stream;
      end if;
    else
      let res = stream.contents[stream.position];
      stream.position := stream.position + 1;
      res;
    end if;
  cleanup
    unlock-stream(stream);
  end block;
end method read-element;

/// unread-element -- Exported.
///

// One should avoid using element as a variable name as it
// conflicts with the call to element() from [] (element-access syntax).
define open generic unread-element (stream :: <positionable-stream>,
				    elemnt :: <object>)
 => the-elemnt :: <object>;

/// This default method on <positionable-stream> is a little shady.
/// It doesn't catch any errors.
///
define method unread-element (stream :: <positionable-stream>,
			      elemnt :: <object>)
 => the-elemnt :: <object>;
  block ()
    lock-stream(stream);
    stream.stream-position := as(<integer>, stream.stream-position) - 1;
    elemnt;
  cleanup
    unlock-stream(stream);
  end block;
end method unread-element;

define sealed method unread-element (stream :: <simple-sequence-stream>,
				     elemnt :: <object>)
 => the-elemnt :: <object>;
  block ()
    lock-stream(stream);
    check-stream-open(stream);
    check-input-stream(stream);
    // I don't catch all conceivable errors here...
    if (stream.position == stream.stream-start)
      error("Stream at initial position: %=", stream);
    end if;
    let new-pos = stream.position - 1;
    if (stream.contents[new-pos] ~= elemnt)
      error("%= is not last element read from %=", elemnt, stream);
    else
      stream.position := new-pos;
      elemnt;
    end if;
  cleanup
    unlock-stream(stream);
  end block;
end method unread-element;

/// peek -- Exported.
///
define open generic peek (stream :: <stream>,
			  #key on-end-of-stream :: <object>)
 => element-of-eof :: <object>;

define method peek (stream :: <buffered-stream>,
		    #key on-end-of-stream :: <object> = $not-supplied)
 => element-of-eof :: <object>;
  block ()
    let buf :: false-or(<buffer>) = get-input-buffer(stream);
    if (buf)
      as(stream.stream-element-type, buf[buf.buffer-next]);
    else
      if (on-end-of-stream == $not-supplied)
	error(make(<end-of-stream-error>, stream: stream));
      else
	on-end-of-stream;
      end if;
    end if; 
  cleanup
    release-input-buffer(stream);
  end block;
end method peek;

define sealed domain peek(<fd-stream>);
define sealed domain peek(<buffered-byte-string-output-stream>);

define sealed method peek (stream :: <simple-sequence-stream>,
			   #key on-end-of-stream :: <object> = $not-supplied)
 => element-or-eof :: <object>;
  block ()
    lock-stream(stream);
    check-stream-open(stream);
    check-input-stream(stream);
    if (stream.position == stream.stream-end)
      if (on-end-of-stream == $not-supplied)
	error(make(<end-of-stream-error>, stream: stream));
      else
	on-end-of-stream;
      end if;
    else
      stream.contents[stream.position];
    end if;
  cleanup
    unlock-stream(stream);
  end block;
end method peek;

/// read -- Exported.
///
define open generic read (stream :: <stream>, n :: <integer>,
			  #key on-end-of-stream :: <object>)
 => sequence-or-eof :: <object>;

define method read (stream :: <buffered-stream>, n :: <integer>,
		    #key on-end-of-stream :: <object> = $not-supplied)
 => sequence-or-eof :: <object>;
  block (exit-loop)
    let buf :: false-or(<buffer>) = get-input-buffer(stream);
    if (~buf)
      if (on-end-of-stream == $not-supplied)
	error(make(<end-of-stream-error>, stream: stream));
      else
	on-end-of-stream;
      end if;
    else
      let result = make(type-for-sequence(stream.stream-element-type),
			size: n);
      let available :: <buffer-index> = (buf.buffer-end - buf.buffer-next);
      let result-start :: <integer> = 0;
      let buf-start :: <buffer-index> = buf.buffer-next;
      let result-stop :: <integer> = result-start + available;
      while (#t)
	if (result-stop >= n)
	  // Were almost done. Copy over the last chunk, then break out.
	  let copy-size = n - result-start;
	  copy-sequence!(result, result-start,
			 buf, buf-start,
			 copy-size);
	  buf.buffer-next := buf.buffer-next + copy-size;
	  exit-loop(result);
	else
	  // We'll need to continue. Copy a buffer load.
	  copy-sequence!(result, result-start, buf, buf-start, available);
	  buf.buffer-next := buf.buffer-end;
	end if;
	if (buf := next-input-buffer(stream))
	  // There's still more input to be had. Update out indices 
	  // and iterate.
	  available := buf.buffer-end - buf.buffer-next;
	  result-start := result-stop;
	  result-stop := result-start + available;
	  buf-start := buf.buffer-next;
	else
	  // We hit the end of the stream. 
	  // Check to see if we got everything we needed, else bail.
	  if (n == result-stop)
	    exit-loop(result);
	  elseif (on-end-of-stream == $not-supplied)
	    error(make(<incomplete-read-error>,
		       stream: stream,
		       sequence: copy-sequence(result, end: result-stop),
		       count: n));
	  else
	    exit-loop(on-end-of-stream);
	  end if;
	end if;
      end while;
    end if;
  cleanup
    release-input-buffer(stream);
  end;
end method read;

define sealed domain read(<fd-stream>, <integer>);
define sealed domain read(<buffered-byte-string-output-stream>, <integer>);

define sealed method read (stream :: <simple-sequence-stream>, n :: <integer>,
			   #key on-end-of-stream :: <object> = $not-supplied)
 => sequence-or-eof :: <object>;
  block ()
    lock-stream(stream);
    check-stream-open(stream);
    check-input-stream(stream);
    let available :: <integer> = stream.stream-end - stream.position;
    if (available == 0)
      if (on-end-of-stream == $not-supplied)
	error(make(<end-of-stream-error>, stream: stream));
      else
	on-end-of-stream;
      end if;
    elseif (available < n)
      if (on-end-of-stream ~== $not-supplied)
	on-end-of-stream;
      else
	let inc-seq = make(type-for-copy(stream.contents), size: available);
	copy-sequence!(inc-seq, 0,
		       stream.contents, stream.position,
		       available);
	stream.position := stream.stream-end;
	error(make(<incomplete-read-error>, stream: stream,
		   sequence: inc-seq, count: n));
      end if;
    else
      let res = make(type-for-copy(stream.contents), size: n);
      copy-sequence!(res, 0, stream.contents, stream.position, n);
      stream.position := stream.position + n;
      res;
    end if;
  cleanup
    unlock-stream(stream);
  end block;
end method read;

/// read-into! -- Exported.
///
define open generic read-into! 
    (stream :: <stream>, 
     n :: <integer>,
     sequence :: <mutable-sequence>,
     #key start ::  <integer>,
          on-end-of-stream :: <object>)
 => count-or-eof :: <object>;

define method read-into!
    (stream :: <buffered-stream>,
     n :: <integer>,
     sequence :: <mutable-sequence>,
     #key start ::  <integer> = 0,
          on-end-of-stream :: <object> = $not-supplied)
 => count-or-eof :: <object>;
  block (exit-loop)
    let buf :: false-or(<buffer>) = get-input-buffer(stream);
    if (~buf)
      // Hit eos right away.
      if (on-end-of-stream == $not-supplied)
	error(make(<end-of-stream-error>, stream: stream));
      else
	on-end-of-stream;
      end if;
    else
      let available :: <buffer-index> = (buf.buffer-end - buf.buffer-next);
      let seq-start :: <integer> = start;
      let buf-start :: <buffer-index> = buf.buffer-next;
      let seq-stop :: <integer> = seq-start + available;
      while (#t)
	if (seq-stop > sequence.size) // We've reached the end of sequence
	  let copy-size :: <integer> = sequence.size - seq-start;
	  copy-sequence!(sequence, seq-start, 
			 buf, buf-start,
			 copy-size);
	  buf.buffer-next := buf.buffer-next + copy-size;
	  exit-loop(sequence.size - start);
	elseif (seq-stop > n) // We've copied as many as were requested
	  let copy-size :: <integer> = n - seq-start;
	  copy-sequence!(sequence, seq-start, 
			 buf, buf-start,
			 copy-size);
	  buf.buffer-next := buf.buffer-next + copy-size;	  
	  exit-loop(n);
	else // We'll keep going
	  copy-sequence!(sequence, seq-start,
			 buf, buf-start, 
			 seq-stop - seq-start);
	  buf.buffer-next := buf.buffer-end;
	  if (buf := next-input-buffer(stream))
	    available := buf.buffer-end - buf.buffer-next;
	    seq-start := seq-stop;
	    seq-stop := seq-start + available;
	    buf-start := buf.buffer-next;
	  else // We've reached the end of the stream
	    if (on-end-of-stream ~== $not-supplied)
	      exit-loop(on-end-of-stream);
	    else 
	      let num-copied = seq-stop - start;
	      let inc-seq = make(type-for-copy(sequence), size: num-copied);
	      copy-sequence!(inc-seq, 0,
			     sequence, start,
			     num-copied);
	      error(make(<incomplete-read-error>,
			 stream: stream,
			 sequence: inc-seq,
			 count: n));
	    end if;
	  end if;
	end if;
      end while;
    end if;
  cleanup
    release-input-buffer(stream);
  end block;
end method read-into!;

define sealed domain read-into!(<fd-stream>, <integer>, <mutable-sequence>);
define sealed domain read-into!
  (<buffered-byte-string-output-stream>, <integer>, <mutable-sequence>);

define sealed method read-into!
    (stream :: <simple-sequence-stream>,
     n :: <integer>,
     sequence :: <mutable-sequence>,
     #key start ::  <integer> = 0,
          on-end-of-stream :: <object> = $not-supplied)
 => count-or-eof :: <object>;
  block ()
    lock-stream(stream);
    check-stream-open(stream);
    check-input-stream(stream);
    if (stream.stream-position == stream.stream-end)
      if (on-end-of-stream == $not-supplied)
	error(make(<end-of-stream-error>, stream: stream));
      else
	on-end-of-stream;
      end if;
    end if;
    let available :: <integer> = (sequence.size - start);
    let num-elts :: <integer> = if (n > available) available else n end;
    if ((stream.stream-end - stream.position) < num-elts)
      if (on-end-of-stream ~== $not-supplied)
	on-end-of-stream;
      else
	let inc-size = stream.stream-end - stream.position;
	let incomplete-seq = make(type-for-copy(stream.contents),
				  size: inc-size);
	copy-sequence!(incomplete-seq, 0,
		       stream.contents, stream.position,
		       inc-size);
	stream.position := stream.stream-end;
	error(make(<incomplete-read-error>, stream: stream,
		   sequence: incomplete-seq,
		   count: num-elts));
       end if;
    else
      copy-sequence!(sequence, start,
		     stream.contents, stream.position,
		     num-elts);
      stream.position := stream.position + num-elts;
      num-elts;
    end if;
  cleanup
    unlock-stream(stream);
  end block;
end method read-into!;

/// discard-input -- Exported.
///
define open generic discard-input (stream :: <stream>) => ();

// Default method does nothing.
//
define inline method discard-input (stream :: <stream>) => ();
end method discard-input;

define method discard-input (stream :: <buffered-stream>) => ();
  block ()
    let buf :: false-or(<buffer>) = get-input-buffer(stream);
    while (buf)
      buf.buffer-next := buf.buffer-end;
      buf := next-input-buffer(stream);
    end while;
  cleanup
    release-input-buffer(stream);
  end block;
end method discard-input;

define sealed domain discard-input(<fd-stream>);
define sealed domain discard-input(<buffered-byte-string-output-stream>);

define sealed method discard-input (stream :: <simple-sequence-stream>) => ();
  block ()
    lock-stream(stream);
    check-stream-open(stream);
    check-input-stream(stream);
    stream.position := stream.stream-end;
  cleanup
    unlock-stream(stream);
  end block;
end method discard-input;

/// stream-input-available? -- Exported.
///
define open generic stream-input-available? (stream :: <stream>)
 => input-available? :: <boolean>;

define method stream-input-available? (stream :: <buffered-stream>)
 => input-available? :: <boolean>;
  block ()
    let buf :: <buffer> = get-input-buffer(stream, wait?: #f);
    if (buf.buffer-next == buf.buffer-end)
      input-available-at-source?(stream);
    else
      #t;
    end if;
  cleanup
    release-input-buffer(stream);
  end block;
end method stream-input-available?;

define sealed domain stream-input-available?(<fd-stream>);
define sealed domain stream-input-available?
  (<buffered-byte-string-output-stream>);

define sealed method stream-input-available?
    (stream :: <simple-sequence-stream>)
 => input-avaiable? :: <boolean>;
  block ()
    lock-stream(stream);
    check-stream-open(stream);
    check-input-stream(stream);
    #t;
  cleanup
    unlock-stream(stream);
  end block;
end method stream-input-available?;


//// Conveniece functions for reading from streams.
//// These functions are implemented in terms of the more primitive functions
//// defined above.
////

/// read-to -- Exported.
///
define method read-to (stream :: <stream>, elemnt :: <object>,
		       #key on-end-of-stream :: <object>
			      = $not-supplied,
		            test :: <function> = \==)
 => (sequence-or-eof :: <object>, found? :: <boolean>);
  // Call read with n = 1 just to see what type of sequence we want.
  let first-elt-seq = read(stream, 1, on-end-of-stream: on-end-of-stream);
  let res-type = type-for-copy(first-elt-seq);
  // Use a list to collect everything
  let res :: <list> = as(<list>, first-elt-seq);
  block ()
    for (elt = read-element(stream) then read-element(stream),
	 until: test(elt, elemnt))
      res := pair(elt, res);
    end for;
    values(as(res-type, reverse(res)), #t);
  exception (<end-of-stream-error>)
    values(as(res-type, reverse(res)), #f);
  end block;
end method read-to;

/// read-through -- Exported.
///
define method read-through (stream :: <stream>, elemnt :: <object>,
			    #key on-end-of-stream :: <object>
			           = $not-supplied,
			         test :: <function> = \==)
 => (sequence-or-eof :: <object>, found? :: <boolean>);
  // Call read with n = 1 just to see what type of sequence we want.
  let first-elt-seq = read(stream, 1, on-end-of-stream: on-end-of-stream);
  let res-type = type-for-copy(first-elt-seq);
  // Use a list to collect everything
  let res :: <list> = as(<list>, first-elt-seq);
  block ()
    for (elt = read-element(stream) then read-element(stream),
	 until: test(elt, elemnt))
      res := pair(elt, res);
    finally
      res := pair(elt, res); // Include boundary elt    
    end for;
    values(as(res-type, reverse(res)), #t);
  exception (<end-of-stream-error>)
    values(as(res-type, reverse(res)), #f);
  end block;
end method read-through;

/// read-to-end -- Exported.
/*
/// Um, this is sort of a hack. Completely a hack. But it's the most efficient
/// way to do this, I think. This way we efficiently copy over chunks
/// as big as the streams underlying aggregate; the alternative 'clean' version
/// would go element by element, locking and checking the stream at each
/// element...
///
/// Probably it's only necessary to do get-next-chunk() once...I put it in the
/// loop cover situations were read() signals an <incomplete-read-error> 
/// because it can't read n elements at once, though there may be n elements
/// in the stream.
/// 
define method read-to-end (stream :: <stream>)
 => sequence :: <sequence>;
  block (exit-loop)
    let get-next-chunk = method (exit-val)
			   block ()
			     read(stream, $maximum-integer);
			   exception (inc-read :: <incomplete-read-error>)
			     inc-read.incomplete-read-sequence;
			   exception (<end-of-stream-error>)
			     exit-loop(exit-val);
			   end block;  
			 end method;
    let result = get-next-chunk(#());
    while (#t) 
      result := concatenate(result, get-next-chunk(result));
    end while;
  end block;
end method read-to-end;
*/
/// (clean version)
///
define method read-to-end (stream :: <stream>)
 => sequence :: <sequence>;
  // Call read with n = 1 just to see what type of sequence we want.
  let first-elt-seq = read(stream, 1, on-end-of-stream: #"eos");
  if (first-elt-seq == #"eos")
    #();
  else
    let res-type = type-for-copy(first-elt-seq);
    // Use a list to collect everything
    let res :: <list> = as(<list>, first-elt-seq); 
    block ()
      while (#t) res := pair(read-element(stream), res) end;
    exception (<end-of-stream-error>)
      as(res-type, reverse(res));
    end block;
  end if;
end method read-to-end;

/// skip-through -- Exported.
///
define method skip-through  (stream :: <stream>, elemnt :: <object>,
			     #key test :: <function> = \==)
 => found? :: <boolean>;
  block ()
    until (test(read-element(stream), elemnt)) end;
    #t;
  exception (<end-of-stream-error>)
    #f;
  end block;
end method skip-through;
