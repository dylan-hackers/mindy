module: Streams
author: chiles@cs.cmu.edu
synopsis: This file implements streams for the Gwydion implementation of Dylan.
copyright: See below.
rcs-header: $Header: /home/housel/work/rcs/gd/src/common/streams/streams.dylan,v 1.4 1996/07/11 16:07:53 nkramer Exp $

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



//// Constants.
////

define constant $default-buffer-size = 4096;


//// Some classes (including conditions).
////

/// <stream> Class -- Exported.
///
/// All other streams inherit from this class.
///
/// Though all streams have buffers, or appear to have buffers, subclasses
/// of the <stream> class cannot inherit the buffer slot from this class
/// because the stream interface makes no provision for implementors of new
/// streams to fetch the buffer.
///
define abstract primary open class <stream> (<object>)
  //
  // See the generic function stream-locked? for how this slot is used.
  slot stream-lock :: <multilock> = make(<multilock>);
  //
  // Yes, users of this module that implement their own streams will get
  // this slot and be unable to use it because there is no interface to it.
  // Oh well.
  slot buffer-locked? :: <boolean> = #f;
end class;

/// <random-access-stream> Class -- Exported.
///
/// All required streams inherit from this class.
///
define abstract open class <random-access-stream> (<stream>)
end class;


///
/// Conditions.
///

/// These are all exported.
///

define class <end-of-file> (<error>)
  slot end-of-file-stream :: <stream>, required-init-keyword: #"stream";
end class;

define sealed domain make (singleton(<end-of-file>));
define sealed domain initialize (<end-of-file>);

define sealed method report-condition (cond :: <end-of-file>, stream) => ();
  condition-format(stream, "Unexpected end of file on %=",
		   cond.end-of-file-stream);
end method report-condition;


define class <file-not-found> (<error>)
  slot file-not-found-filename :: <string>, required-init-keyword: #"filename";
end class;

define sealed domain make (singleton(<file-not-found>));
define sealed domain initialize (<file-not-found>);

define sealed method report-condition (cond :: <file-not-found>, stream) => ();
  condition-format(stream, "File not found: %=",
		   cond.file-not-found-filename);
end method report-condition;


define class <file-exists> (<error>)
  slot file-exists-filename :: <string>, required-init-keyword: #"filename";
end class;

define sealed domain make (singleton(<file-exists>));
define sealed domain initialize (<file-exists>);

define sealed method report-condition (cond :: <file-exists>, stream) => ();
  condition-format(stream, "File already exists: %=",
		   cond.file-exists-filename);
end method report-condition;



//// Stream locking.
////

/// stream-locked? -- Exported.
///
/// This function returns whether the stream is currently held (in use) by
/// the application.  Only one thread of the application may use the stream
/// at one time.  Having the stream locked is different than holding the
/// buffer.  The stream must be locked before a thread can get the buffer,
/// and the stream may be locked across multiple calls to functions that get
/// and release the buffer.  Streams use a <multilock> from the Threads
/// module of the Dylan library so that a single thread may repeatedly lock
/// the stream.
///
/// The distinction between locking the stream and the buffer provides a
/// couple of advantages.  The stream's lock prohibits more than one thread
/// from having the ability to hold the buffer.  Having a separate soft lock
/// for the buffer means that a single thread of execution is safe from
/// inadvertently calling a function that diddles the buffer while the
/// thread is already diddling the buffer.
///
define generic stream-locked? (stream :: <stream>) => locked? :: <boolean>;

define method stream-locked? (stream :: <stream>) => locked? :: <boolean>;
  locked?(stream.stream-lock);
end method;

/// lock-stream -- Exported.
///
define generic lock-stream (stream :: <stream>) => ();

define method lock-stream (stream :: <stream>) => ();
  grab-lock(stream.stream-lock);
end method;

/// unlock-stream -- Exported.
///
define generic unlock-stream (stream :: <stream>) => ();

define method unlock-stream (stream :: <stream>) => ();
  release-lock(stream.stream-lock);
end method;



//// Internal protocol for streams.
////

///
/// Buffer locking.
/// 

/// buffer-locked? -- Internal Interface.
///
/// This function returns whether the buffer is currently in use.  Only one
/// thread of the application may use the buffer at one time, which is
/// enforced by locking the stream.  Functions that lock the stream and then
/// get the buffer cannot call other functions that get the buffer, unless
/// the first function releases the buffer before calling the second
/// function; buffers are NOT locked with multilocking semantics.  See the
/// comment for the generic function stream-locked? for more information.
///
/// This function is implemented as a slot in the <stream> class.
/// Applications must lock the stream before calling this function.
///
define generic buffer-locked? (stream :: <stream>) => locked? :: <boolean>;

/// buffer-locked?-setter -- Internal Interface.
///
/// This function is implemented as a slot in the <stream> class.
/// Applications must lock the stream before calling this function.
///
define generic buffer-locked?-setter (value :: <boolean>, stream :: <stream>)
    => locked? :: <boolean>;


///
/// Buffer access, next values, and stop values.
///

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
define generic buffer (stream :: <stream>)
    => buffer :: false-or(<buffer>);

/// buffer-setter -- Internal Interface.
///
define generic buffer-setter (value :: false-or(<buffer>), stream :: <stream>)
    => value :: false-or(<buffer>);

/// buffer-next -- Internal Interface.
///
/// This function is implemented as slots in class definitions.  See the
/// class definitiond for what the return value means.
///
define generic buffer-next (stream :: <stream>) => next :: <buffer-index>;

/// buffer-stop -- Internal Interface.
///
/// This function is implemented as slots in class definitions.  See the
/// class definitiond for what the return value means.
///
define generic buffer-stop (stream :: <stream>) => stop :: <buffer-index>;


///
/// Output stream registration and forcing output upon Application exit.
///

/// This lock isolates access to *output-streams*.
///
define constant output-stream-registry-lock :: <semaphore> = make(<semaphore>);

/// This list contains all open output streams.  There is a function
/// registered on the exist hook that forces output on all streams when the
/// application exits.
///
define variable *output-streams* :: <list> = #();

/// register-output-stream -- Internal Interface.
///
/// This function registers output functions for the purpose of
/// synchronizing their output when an application exits.  The same registry
/// of streams could be used by a demon thread that periodically wakes up
/// and forces output on streams.
///
define method register-output-stream (stream :: <stream>)
    => stream :: <stream>;
  grab-lock(output-stream-registry-lock);
  *output-streams* := pair(stream, *output-streams*);
  release-lock(output-stream-registry-lock);
  stream;
end method;

/// unregister-output-stream -- Internal Interface.
///
/// This function removes stream from *output-streams*.
///
define method unregister-output-stream (stream :: <stream>)
    => stream :: <stream>;
  grab-lock(output-stream-registry-lock);
  *output-streams* := remove!(*output-streams*, stream);
  release-lock(output-stream-registry-lock);
  stream;
end method;

/// Register a function on the application exit hook.  This function forces
/// output for every output stream.  There's no reason to isolate access to
/// *output-streams* because exit functions run one at a time in the only
/// remaining thread.
///
on-exit(method ()
	  for (stream in *output-streams*)
	    synchronize-output(stream);
	  end;
	end);



//// Stream Extension Protocol.
////

/// All of these are exported.
///

define open generic close (stream :: <stream>) => ();

define open generic stream-extension-get-input-buffer (stream :: <stream>)
    => (buffer :: <buffer>, next :: <buffer-index>, stop :: <buffer-index>);

define open generic stream-extension-release-input-buffer
    (stream :: <stream>, next :: <buffer-index>, stop :: <buffer-index>)
    => ();

define open generic stream-extension-fill-input-buffer
    (stream :: <stream>, start :: <buffer-index>)
    => stop :: <buffer-index>;

define open generic stream-extension-input-available-at-source?
    (stream :: <stream>)
    => available? :: <boolean>;

define open generic stream-extension-get-output-buffer (stream :: <stream>)
    => (buffer :: <buffer>, next :: <buffer-index>, size :: <buffer-index>);

define open generic stream-extension-release-output-buffer
    (stream :: <stream>, next :: <buffer-index>)
    => ();

define open generic stream-extension-empty-output-buffer
    (stream :: <stream>, stop :: <buffer-index>)
    => ();


define open generic stream-extension-force-secondary-buffers
    (stream :: <stream>)
    => ();

/// stream-extension-force-secondary-buffers -- Method for Exported Interface.
///
/// This Stream Extension Protocol function gets a default method because
/// most streams will not need to extend this function.  Most streams do
/// not do secondary buffering, so most stream implementors can ignore this.
/// Unfortunately, uses cannot and must call it when forcing outout.
///
define method stream-extension-force-secondary-buffers (stream :: <stream>)
    => ();
end method;


define open generic stream-extension-synchronize (stream :: <stream>) => ();



//// Basic I/O Protocol.
////

/// All the generic functions on this page are exported.
///

define generic read-byte (stream :: <stream>,
			  #key signal-eof? :: <boolean>) // = #t
    => byte :: false-or(<byte>);

define method read-byte (stream :: <stream>,
			 #key signal-eof? :: <boolean> = #t)
    => byte :: false-or(<byte>);
  let (buf :: <buffer>, next :: <buffer-index>, stop :: <buffer-index>)
    = get-input-buffer(stream);
  if (next == stop)
    stop := fill-input-buffer(stream, 0);
    next := 0;
  end;
  if (stop ~= 0)
    let res = buf[next];
    release-input-buffer(stream, next + 1, stop);
    res;
  elseif (signal-eof?)
    release-input-buffer(stream, 0, 0);
    error(make(<end-of-file>, stream: stream));
  else
    release-input-buffer(stream, 0, 0);
    #f;
  end;
end method;


define generic peek-byte (stream :: <stream>) => byte :: false-or(<byte>);

define method peek-byte (stream :: <stream>) => byte :: false-or(<byte>);
  let (buf :: <buffer>, next :: <buffer-index>, stop :: <buffer-index>)
    = get-input-buffer(stream);
  if (next == stop)
    stop := fill-input-buffer(stream, 0);
    next := 0;
  end;
  if (stop ~= 0)
    let res = buf[next];
    release-input-buffer(stream, next, stop);
    res;
  else
    release-input-buffer(stream, 0, 0);
    #f;
  end;
end method;


define generic read-line (stream :: <stream>,
			  #key signal-eof? :: <boolean>) // = #t
    => (result :: false-or(<string>), eof? :: <boolean>);

define constant $newline-byte :: <integer> = as(<integer>, '\n');

#if (newlines-are-CRLF)
  define constant $return-byte :: <integer> = as(<integer>, '\r');
#endif

/// read-line -- Method for Exported Interface.
///
define method read-line (stream :: <stream>,
			 #key signal-eof? :: <boolean> = #t)
    => (result :: false-or(<string>), eof? :: <boolean>);
  let (buf :: <buffer>, next :: <buffer-index>, stop :: <buffer-index>)
    = get-input-buffer(stream);
  // Make sure we have input, if there is any.
  if (next == stop)
    stop := fill-input-buffer(stream, 0);
    next := 0;
  end;
  case
    (stop ~= 0) =>
      // We definitely have some input available.
      read-line-aux(stream, buf, next, stop);
    (signal-eof?) =>
      // Hit EOF immediately.
      release-input-buffer(stream, 0, 0);
      error(make(<end-of-file>, stream: stream));
    otherwise =>
      // Hit EOF immediately.
      release-input-buffer(stream, 0, 0);
      values(#f, #t);
  end case;
end method;

/// read-line-aux -- Internal.
///
/// This function does the work of read-line whenever there's input
/// available from the stream.  I broke this function off of read-line
/// primarily because read-line was getting too unwieldy to view.
///
/// This function cannot use a big global buffer to build the result because
/// of thread-safety.  The intermediate result consing should be rare or
/// minimal, assuming a reasonable relationship between line lengths and the
/// buffer's length.
///
define method read-line-aux (stream :: <stream>, buf :: <buffer>,
			     next :: <buffer-index>, stop :: <buffer-index>)
  block (exit-loop)
    let res = "";
    let collect = method (string :: <byte-string>, buf :: <buffer>,
			  start :: <buffer-index>, stop :: <buffer-index>)
		      => result :: <byte-string>;
		    let str-len = string.size;
		    let buf-len = (stop - start);
		    let res = make(<byte-string>,
				   size: (str-len + buf-len));
		    copy-bytes(res, 0, string, 0, str-len);
		    copy-bytes(res, str-len, buf, start, buf-len);
		    res;
		  end;
    while (#t)
      for (i :: <integer> from next below stop,
	   until: (buf[i] = $newline-byte))
      finally
	#if (newlines-are-CRLF)
	  res := read-line-aux-CRLF(stream, buf, next, stop, i, res,
				    collect, exit-loop);
	#else
	  // This else branch would be conditionally compiled in when
	  // we were on a platform that used linefeeds for newlines.
	  if (i = stop)
	    res := collect(res, buf, next, stop);
	  else
	    res := collect(res, buf, next, i);
	    // We don't return the newline, but we do consume it.
	    release-input-buffer(stream, (i + 1), stop);
	    exit-loop(res, #f);
	  end;
	#endif
      end for; 
      next := 0;
      stop := fill-input-buffer(stream, 0);
      if (stop = 0)
	release-input-buffer(stream, 0, 0);
	exit-loop(res, #t);
      end;
    end while;
  end block;
end method;

/// read-line-aux-CRLF -- Internal.
///
/// This function collects input and dumps return characters that precede
/// linefeed character.  This function is only called on platforms that use
/// the CR-LF sequence to represent newlines, and only from read-line-aux.
///
/// The collect and exit functions are local functions defined in
/// read-line-aux.
///
#if (newlines-are-CRLF)
define method read-line-aux-CRLF
    (stream :: <stream>, buf :: <buffer>, next :: <buffer-index>,
     stop :: <buffer-index>, linefeed-pos :: <buffer-index>,
     result :: <byte-string>, collect :: <method>, exit-loop :: <function>)
  => result :: <byte-string>;
  if (linefeed-pos = stop)
    // No linefeed in buffer from next to stop.  Collect it all.
    collect(result, buf, next, stop);
  else
    let result :: <byte-string>
      = case
	  (linefeed-pos = next) =>
	    // First char is linefeed.  There's nothing to collect, but make
	    // sure we didn't collect a CR in the last hunk o' stuff.
	    let len :: <integer> = result.size;
	    if ((len > 0) & (result[len - 1] = '\r'))
	      copy-sequence(result, end: (len - 1));
	    else
	      result;
	    end;
	  (buf[linefeed-pos - 1] = $return-byte) =>
	    collect(result, buf, next, (linefeed-pos - 1));
	  otherwise =>
	    // Probably should keep scanning and gobbling characters until we
	    // find a CRLF, but I'm not worrying about that now.
	    collect(result, buf, next, linefeed-pos);
	end;
    release-input-buffer(stream, (linefeed-pos + 1), stop);
    exit-loop(result, #f);
  end if;
end method;
#endif

define generic input-available? (stream :: <stream>) => result :: <boolean>;

define method input-available? (stream :: <stream>) => result :: <boolean>;
  let (buf :: <buffer>, next :: <buffer-index>, stop :: <buffer-index>)
    = get-input-buffer(stream);
  if (next == stop)
    let res = input-available-at-source?(stream);
    release-input-buffer(stream, 0, 0);
    res;
  else
    release-input-buffer(stream, next, stop);
    #t
  end;
end method;


define generic flush-input (stream :: <stream>) => ();

define method flush-input (stream :: <stream>) => ();
  let buf :: <buffer> = get-input-buffer(stream);
  for (until: (fill-input-buffer(stream, 0) = 0))
  end;
  release-input-buffer(stream, 0, 0);
end method;


define generic force-output (stream :: <stream>) => ();

define method force-output (stream :: <stream>) => ();
  let (buf :: <buffer>, next :: <buffer-index>) = get-output-buffer(stream);
  if (next ~= 0)
    empty-output-buffer(stream, next);
  end;
  force-secondary-buffers(stream);
  release-output-buffer(stream, 0);
end method;


define generic synchronize-output (stream :: <stream>) => ();

define method synchronize-output (stream :: <stream>) => ();
  let (buf :: <buffer>, next :: <buffer-index>) = get-output-buffer(stream);
  if (next ~= 0)
    empty-output-buffer(stream, next);
  end;
  force-secondary-buffers(stream);
  synchronize(stream);
  release-output-buffer(stream, 0);
end method;



//// Buffer Access Protocol.
////

/// All of these are exported.
///
/// This page contains the generic function declarations as well as a default
/// implementation for <stream>s.
///


/// get-input-buffer -- Exported.
///
define sealed generic get-input-buffer (stream :: <stream>)
    => (buffer :: <buffer>, next :: <buffer-index>, stop :: <buffer-index>);

define method get-input-buffer (stream :: <stream>)
    => (buffer :: <buffer>, next :: <buffer-index>, stop :: <buffer-index>);
  // Isolate the calling thread's access to the stream.
  lock-stream(stream);
  // Make sure the thread does not already hold the buffer.
  if (stream.buffer-locked?)
    error("Application already holds stream's buffer -- %=.", stream);
  else
    stream.buffer-locked? := #t;
  end;
  stream-extension-get-input-buffer(stream);
end method;

/// release-input-buffer -- Exported.
///
define sealed generic release-input-buffer
    (stream :: <stream>, next :: <buffer-index>, stop :: <buffer-index>)
    => ();

define method release-input-buffer
    (stream :: <stream>, next :: <buffer-index>, stop :: <buffer-index>)
    => ();
  check-buffer-held(stream);
  stream-extension-release-input-buffer(stream, next, stop);
  stream.buffer-locked? := #f;
  // Unlock the lock obtained in get-input-buffer.
  unlock-stream(stream);
end method;

/// fill-input-buffer -- Exported.
///
define sealed generic fill-input-buffer (stream :: <stream>,
					 start :: <buffer-index>)
    => stop :: <buffer-index>;

define method fill-input-buffer (stream :: <stream>, start :: <buffer-index>)
    => stop :: <buffer-index>;
  check-buffer-held(stream);
  stream-extension-fill-input-buffer(stream, start);
end method;

/// input-available-at-source? -- Exported.
///
define sealed generic input-available-at-source? (stream :: <stream>)
    => available? :: <boolean>;

define method input-available-at-source? (stream :: <stream>)
    => available? :: <boolean>;
  check-buffer-held(stream);
  stream-extension-input-available-at-source?(stream);
end method;

/// get-output-buffer -- Exported.
///
define sealed generic get-output-buffer (stream :: <stream>)
    => (buffer :: <buffer>, next :: <buffer-index>, size :: <buffer-index>);

define method get-output-buffer (stream :: <stream>)
    => (buffer :: <buffer>, next :: <buffer-index>, size :: <buffer-index>);
  // Isolate the calling thread's access to the stream.
  lock-stream(stream);
  // Make sure the thread does not already hold the buffer.
  if (stream.buffer-locked?)
    error("Application already holds stream's buffer -- %=.", stream);
  else
    stream.buffer-locked? := #t;
  end;
  stream-extension-get-output-buffer(stream);
end method;

/// release-output-buffer -- Exported.
///
define sealed generic release-output-buffer
    (stream :: <stream>, next :: <buffer-index>)
    => ();

define method release-output-buffer (stream :: <stream>,
				     next :: <buffer-index>)
    => ();
  check-buffer-held(stream);
  stream-extension-release-output-buffer(stream, next);
  stream.buffer-locked? := #f;
  // Unlock the lock obtained in get-input-buffer.
  unlock-stream(stream);
end method;

/// empty-output-buffer -- Exported.
///
define sealed generic empty-output-buffer (stream :: <stream>,
					   stop :: <buffer-index>)
    => ();

define method empty-output-buffer (stream :: <stream>, stop :: <buffer-index>)
    => ();
  check-buffer-held(stream);
  stream-extension-empty-output-buffer(stream, stop);
end method;

/// force-secondary-buffers -- Exported.
///
define sealed generic force-secondary-buffers (stream :: <stream>) => ();

define method force-secondary-buffers (stream :: <stream>) => ();
  check-buffer-held(stream);
  stream-extension-force-secondary-buffers(stream);
end method;

/// synchronize -- Exported.
///
define sealed generic synchronize (stream :: <stream>) => ();

define method synchronize (stream :: <stream>) => ();
  check-buffer-held(stream);
  stream-extension-synchronize(stream);
end method;

/// check-buffer-held -- Internal Interface.
///
/// After calling this function, the executing thread is guaranteed to have
/// the stream locked and to hold the buffer.
///
define method check-buffer-held (stream :: <stream>) => ();
  // Lock the stream to isolate checking whether the buffer is locked.
  lock-stream(stream);
  if (~ stream.buffer-locked?)
    unlock-stream(stream);
    error("Application does not hold stream's buffer -- %=.", stream);
  end;
  // Unlock the lock for checking buffer-locked?.
  unlock-stream(stream);
  // Because the buffer was locked, and we were able to obtain a lock, the
  // calling thread must already hold a lock on the stream due to
  // get-output-buffer.  Therefore, code following the call to
  // check-buffer-held is still thread-safe, until the final lock is
  // dropped.
end method;



//// Data Extension Protocol.
////

/// read-as -- Exported.
///
define generic read-as (result-class :: <class>, stream :: <stream>,
			#key signal-eof? :: <boolean>) // = #t
    => (result :: false-or(<object>), eof? :: <boolean>);

/*
### hack singleton doesn't entirely work...

define sealed method read-as
    (result-class :: singleton(<byte-character>), stream :: <stream>,
     #key signal-eof? :: <boolean> = #t)
    => (result :: false-or(<byte-character>), eof? :: <boolean>);
  let res :: false-or(<byte>)
    = read-byte(stream, signal-eof?: signal-eof?);
  // If read-byte returns, we either have a byte or signal-eof? was #f.
  if (res)
    values(as(<byte-character>, res), #f)
  else
    values(#f, #t);
  end;
end method;

define sealed method read-as
    (result-class :: singleton(<byte>), stream :: <stream>,
     #key signal-eof? :: <boolean> = #t)
    => (result :: false-or(<byte>), eof? :: <boolean>);
  let res :: false-or(<byte>) = read-byte(stream, signal-eof?: signal-eof?);
  values(res, if (res) #f else #t end);
end method;
*/


/// read-as for <byte-string> and <byte-vector> results from <stream>s.
///
/// Read-as for <byte-string> and <byte-vector> have the same definition.
/// There are two "define method" forms so that the distinct return types
/// can be distinctly declared.  If the "seal generic" form allowed you to
/// declare return types, there could be one method here with two "seal
/// generic" forms declaring the distinct specializations and their
/// associated return types.
///

define method read-as
    (result-class :: singleton(<byte-string>), stream :: <stream>,
     #key signal-eof? :: <boolean> = #t,
          count :: false-or(<integer>),
          to-eof? :: <boolean> = #f)
    => (result :: false-or(<byte-string>), eof? :: <boolean>);
  case
    (count) => read-as-required-vector-count(stream, result-class, signal-eof?,
					     count);
    (to-eof?) => read-as-required-vector-to-eof(stream, result-class);
    otherwise =>
      error("Count or to-eof? must be supplied to read a <byte-string>.");
  end;
end method;

define method read-as
    (result-class :: singleton(<byte-vector>), stream :: <stream>,
     #key signal-eof? :: <boolean> = #t,
          count :: false-or(<integer>),
          to-eof? :: <boolean> = #f)
    => (result :: false-or(<byte-vector>), eof? :: <boolean>);
  case
    (count) => read-as-required-vector-count(stream, result-class, signal-eof?,
					     count);
    (to-eof?) => read-as-required-vector-to-eof(stream, result-class);
    otherwise =>
      error("Count or to-eof? must be supplied to read a <byte-vector>.");
  end;
end method;

/// read-as-required-vector-count -- Internal.
///
/// This function implements read-as for <byte-string> and <byte-vector> for
/// any stream when the user supplied a count: argument.  This function
/// works for <buffer>s too due to the use of copy-bytes, but reading
/// buffers is implemented for each stream type to avoid double buffering.
///
define method read-as-required-vector-count
    (stream :: <stream>,
     result-class :: one-of(<byte-vector>, <byte-string>),
     signal-eof? :: <boolean>,
     count :: <integer>)
    => (result :: type-union(<byte-vector>, <byte-string>, singleton(#f)),
	eof? :: <boolean>);
  let (buf :: <buffer>, next :: <buffer-index>, stop :: <buffer-index>)
    = get-input-buffer(stream);
  let result = make(result-class, size: count);
  if (next == stop)
    stop := fill-input-buffer(stream, 0);
    next := 0;
  end;
  block (exit-loop)
    let available :: <buffer-index> = (stop - next);
    let result-start :: <integer> = 0;
    let buf-start :: <buffer-index> = next;
    for (until: (available = 0))
      let result-stop :: <integer> = (result-start + available);
      if (result-stop >= count)
	let this-copy = (count - result-start);
	copy-bytes(result, result-start, buf, buf-start, this-copy);
	release-input-buffer(stream, (buf-start + this-copy),
			     // Can't assume buf-start is 0 because we may
			     // come in here on the first iteration.
			     (buf-start + available));
	exit-loop(result, #f);
      else
	copy-bytes(result, result-start, buf, buf-start, available);
      end;
      available := fill-input-buffer(stream, 0);
      result-start := result-stop;
      buf-start := 0;
    finally
      // Whenever the loop terminates normally, we don't have enough input
      // to satisfy the request.
      release-input-buffer(stream, 0, 0);
      if (signal-eof?)
	error(make(<end-of-file>, stream: stream));
      else
	values(#f, #t);
      end;
    end for;
  end block;
end method;

/// read-as-required-vector-to-eof -- Internal.
///
/// This function implements read-as for <byte-string>, <byte-vector>, and
/// <buffer> for any stream when the user supplied a to-eof?: argument.
/// There are better methods for <random-access-stream> and
/// <fd-file-stream>.
///
/// If users satisfy the following three properties, they are probably doing
/// something wrong:
///    - They are not using a <random-access-stream> (that is, can't know the
///      size of the stream).
///    - They are using read-as to read to eof.
///    - They are asking for a <buffer> result.
/// This scenario is pretty unlikely, but if a user was in this situation,
/// he probably should be using buffers directly.
///
/// This function cannot assume the Random Access Protocol, so it must
/// repeatedly fill the buffer and build intermediate results to satisfy the
/// read request.  This function cannot use a big global buffer to build the
/// result because of thread-safety.
///
define inline method read-as-required-vector-to-eof
    (stream :: <stream>,
     result-class :: one-of(<byte-vector>, <byte-string>, <buffer>))
    => (result :: type-union(<byte-vector>, <byte-string>, <buffer>),
	eof? :: singleton(#t));
  let (buf :: <buffer>, next :: <buffer-index>, stop :: <buffer-index>)
    = get-input-buffer(stream);
  // Make sure we have input if there is any.
  if (next == stop)
    stop := fill-input-buffer(stream, 0);
    next := 0;
  end;
  let res :: type-union(<byte-vector>, <byte-string>, <buffer>)
    = make(result-class, size: 0);
  let res-len :: <integer> = 0;
  for (next = next then 0,
       stop = stop then fill-input-buffer(stream, 0),
       until: (stop = 0))
    let buf-len = (stop - next);
    let temp-len = (res-len + buf-len);
    let temp :: type-union(<byte-vector>, <byte-string>, <buffer>)
      = make(result-class, size: temp-len);
    copy-bytes(temp, 0, res, 0, res-len);
    copy-bytes(temp, res-len, buf, next, buf-len);
    res := temp;
    res-len := temp-len;
  finally
    release-input-buffer(stream, 0, 0);
    values(res, #t);
  end for;
end method;



/// read-as for <byte-string> and <byte-vector> results
///         from <random-access-stream>s
///
/// Read-as for <byte-string> and <byte-vector> have the same definition.
/// There are two "define method" forms so that the distinct return types
/// can be distinctly declared.  If the "seal generic" form allowed you to
/// declare return types, there could be one method here with two "seal
/// generic" forms declaring the distinct specializations and their
/// associated return types.
///

define method read-as
    (result-class :: singleton(<byte-string>),
     stream :: <random-access-stream>,
     #key signal-eof? :: <boolean> = #t,
          count :: false-or(<integer>),
          to-eof? :: <boolean> = #f)
    => (result :: false-or(<byte-string>), eof? :: <boolean>);
  case
    (count) => read-as-required-vector-count(stream, result-class, signal-eof?,
					     count);
    (to-eof?) =>
      // Isolate thread access across this call so that no thread intervenes
      // between the calls to stream-size, stream-position, and read-as-r....
      lock-stream(stream);
      let res = read-as-required-vector-count(stream, result-class, #f,
					      (stream.stream-size
						 - stream.stream-position));
      unlock-stream(stream);
      values((res | make(result-class, size: 0)), #t);
    otherwise =>
      error("Count or to-eof? must be supplied to read a <byte-string>.");
  end;
end method;

define method read-as
    (result-class :: singleton(<byte-vector>),
     stream :: <random-access-stream>,
     #key signal-eof? :: <boolean> = #t,
          count :: false-or(<integer>),
          to-eof? :: <boolean> = #f)
    => (result :: false-or(<byte-vector>), eof? :: <boolean>);
  case
    (count) => read-as-required-vector-count(stream, result-class, signal-eof?,
					     count);
    (to-eof?) =>
      // Isolate thread access across this call so that no thread intervenes
      // between the calls to stream-size, stream-position, and read-as-r....
      lock-stream(stream);
      let res = read-as-required-vector-count(stream, result-class, #f,
					      (stream.stream-size
						 - stream.stream-position));
      unlock-stream(stream);
      values((res | make(result-class, size: 0)), #t);
    otherwise =>
      error("Count or to-eof? must be supplied to read a <byte-vector>.");
  end;
end method;



/// read-as for <buffer> results from <fd-stream>s.
///
/// Read-as for <buffer> results needs to be implemented for each stream
/// class to avoid double buffering.
///

define method read-as
    (result-class :: singleton(<buffer>), stream :: <fd-stream>,
     #key signal-eof? :: <boolean> = #t,
          count :: false-or(<integer>),
          to-eof? :: <boolean> = #f)
    => (result :: false-or(<buffer>), eof? :: <boolean>);
  case
    (count) => read-as-buffer-count(stream, result-class, signal-eof?, count);
    (to-eof?) => read-as-required-vector-to-eof(stream, result-class);
    otherwise =>
      error("Count or to-eof? must be supplied to read a buffer.");
  end;
end method;

define method read-as-buffer-count
    (stream :: <fd-stream>, result-class :: singleton(<buffer>),
     signal-eof? :: <boolean>, count :: <integer>)
    => (result :: false-or(<buffer>), eof? :: <boolean>);
  let (buf :: <buffer>, next :: <buffer-index>, stop :: <buffer-index>)
    = get-input-buffer(stream);
  let result = make(<buffer>, size: count);
  let available :: <buffer-index> = (stop - next);
  if (available >= count)
    // All the input we need is already available in the stream's buffer.
    copy-bytes(result, 0, buf, next, count);
    release-input-buffer(stream, next, stop);
    values(result, #f);
  else
    // We need to iterate to get all the input we need.
    // First, copy what is available in the stream's buffer to the result.
    let start :: <buffer-index>
      = if (available ~= 0)
	  copy-bytes(result, 0, buf, next, available);
	  available;
	else
	  0;
	end;
    let fd = stream.file-descriptor;
    block (exit-loop)
      // Iterate, filling the result buffer directly.
      for (num-bytes :: <buffer-index>
	     = call-fd-function(fd-read, fd, result, start, (count - start))
	     then call-fd-function(fd-read, fd, result, start,
				   (count - start)),
	   until: (num-bytes = 0))
	start := start + num-bytes;
	if (start = count)
	  release-input-buffer(stream, 0, 0);
	  exit-loop(result, #f);
	end;
      finally
	// If we exit normally, then we hit eof.
	release-input-buffer(stream, 0, 0);
	if (signal-eof?)
	  error(make(<end-of-file>, stream: stream));
	else
	  values(#f, #t);
	end;
      end for;
    end block;
  end if;
end method;



/// read-as for <buffer> results from <fd-file-stream>s.
///
/// Read-as for <buffer> results needs to be implemented for each stream
/// class to avoid double buffering.
///
/// This method needs to exist even though there is a similar method on
/// <random-access-stream>s because of how applicable methods are sorted.
/// We need to make sure this method executes rather than the one for
/// <fd-stream>s.
///

define method read-as
    (result-class :: singleton(<buffer>), stream :: <fd-file-stream>,
     #key signal-eof? :: <boolean> = #t,
          count :: false-or(<integer>),
          to-eof? :: <boolean> = #f)
    => (result :: false-or(<buffer>), eof? :: <boolean>);
  case
    (count) =>
      read-as-buffer-count(stream, result-class, signal-eof?, count);
    (to-eof?) =>
      // Isolate thread access across this call so that no thread intervenes
      // between the calls to stream-size, stream-position, and read-as-b....
      lock-stream(stream);
      let res = read-as-buffer-count(stream, result-class, #f,
				     (stream.stream-size
					- stream.stream-position));
      unlock-stream(stream);
      values((res | make(result-class, size: 0)), #t);
    otherwise =>
      error("Count or to-eof? must be supplied to read a buffer.");
  end;
end method;



/// read-as for <buffer> results from <byte-string-input-stream>s.
///
/// Read-as for <buffer> results needs to be implemented for each stream
/// class to avoid double buffering.
///

define sealed method read-as
    (result-class :: singleton(<buffer>), stream :: <byte-string-input-stream>,
     #key signal-eof? :: <boolean> = #t,
          count :: false-or(<integer>),
          to-eof? :: <boolean> = #f)
    => (result :: false-or(<buffer>), eof? :: <boolean>);
  case
    (count) => read-as-buffer-count(stream, result-class, signal-eof?, count);
    (to-eof?) =>
      // Isolate thread access across this call so that no thread intervenes
      // between the calls to stream-size, stream-position, and read-as-b....
      lock-stream(stream);
      let res = read-as-buffer-count(stream, result-class, #f,
				     (stream.stream-size
					- stream.stream-position));
      unlock-stream(stream);
      values((res | make(result-class, size: 0)), #t);
    otherwise =>
      error("Count or to-eof? must be supplied to read a buffer.");
  end;
end method;

define sealed method read-as-buffer-count
    (stream :: <byte-string-input-stream>, result-class :: singleton(<buffer>),
     signal-eof? :: <boolean>, count :: <integer>)
    => (result :: false-or(<buffer>), eof? :: <boolean>);
  let (buf :: <buffer>, next :: <buffer-index>, stop :: <buffer-index>)
    = get-input-buffer(stream);
  let available :: <buffer-index> = (stop - next);
  if (available >= count)
    let result = make(result-class, size: count);
    copy-bytes(result, 0, buf, next, count);
    release-input-buffer(stream, next, stop);
    values(result, #f);
  else
    release-input-buffer(stream, 0, 0);
    if (signal-eof?)
      error(make(<end-of-file>, stream: stream));
    else
      values(#f, #t);
    end;
  end;
end method;



/// read-into! for <byte-string>, <byte-vector>, and <buffer> results
///            from <stream>s.
///

define generic read-into!
    (destination :: <object>, stream :: <stream>,
     #key signal-eof? :: <boolean>, // = #t
          start :: <integer>, // = 0,
          end: dest-end :: <integer>, // = destination.size,
          to-eof? :: <boolean>) // = #f)
    => (result :: false-or(<object>),
	eof?-or-how-much :: type-union(<boolean>, <integer>));

/// Read-into! for <byte-string>, <byte-vector>, and <buffer> have the same
/// definition.  There are three "define method" forms so that the distinct
/// return types can be distinctly declared.  If the "seal generic" form
/// allowed you to declare return types, there could be one method here
/// with two "seal generic" forms declaring the distinct specializations
/// and their associated return types.
///

define sealed method read-into!
    (destination :: <byte-string>, stream :: <stream>,
     #key signal-eof? :: <boolean> = #t,
          start :: <integer> = 0,
          end: stop :: <integer> = destination.size,
          to-eof? :: <boolean> = #f)
    => (result :: false-or(<byte-string>),
	eof?-or-how-much :: type-union(<boolean>, <integer>));
  read-into-required-vector(stream, destination, signal-eof?, to-eof?,
			    start, stop);
end method;

define sealed method read-into!
    (destination :: <byte-vector>, stream :: <stream>,
     #key signal-eof? :: <boolean> = #t,
          start :: <integer> = 0,
          end: stop :: <integer> = destination.size,
          to-eof? :: <boolean> = #f)
    => (result :: false-or(<byte-vector>),
	eof?-or-how-much :: type-union(<boolean>, <integer>));
  read-into-required-vector(stream, destination, signal-eof?, to-eof?,
			    start, stop);
end method;


#if (mindy)
// In the compiler, <byte-vector> == <buffer>, so we don't need this
// method
//
define sealed method read-into!
    (destination :: <buffer>, stream :: <stream>,
     #key signal-eof? :: <boolean> = #t,
          start :: <integer> = 0,
          end: stop :: <integer> = destination.size,
          to-eof? :: <boolean> = #f)
    => (result :: false-or(<buffer>),
	eof?-or-how-much :: type-union(<boolean>, <integer>));
  read-into-required-vector(stream, destination, signal-eof?, to-eof?,
			    start, stop);
end method;
#endif

/// read-into-required-vector -- Internal.
///
/// This function implements read-into! for <byte-string>, <byte-vector>,
/// and <buffer> for any stream.  There are better methods for <buffer>s on
/// <fd-stream>s and <byte-string-input-stream>s.
///
define sealed method read-into-required-vector
    (stream :: <stream>,
     destination :: type-union(<byte-vector>, <byte-string>, <buffer>),
     signal-eof? :: <boolean>,
     to-eof? :: <boolean>,
     start :: <integer>,
     stop :: <integer>)
    => (result :: type-union(<byte-vector>, <byte-string>, <buffer>,
			  singleton(#f)),
	eof?-or-how-much :: type-union(<boolean>, <integer>));
  let (buf :: <buffer>, buf-start :: <buffer-index>,
       buf-stop :: <buffer-index>)
    = get-input-buffer(stream);
  if (buf-start = buf-stop)
    buf-stop := fill-input-buffer(stream, 0);
    buf-start := 0;
  end;
  block (exit-loop)
    let available :: <buffer-index> = (buf-stop - buf-start);
    let count :: <integer> = available;
    let stop :: <integer> = if (to-eof?) destination.size else stop end;
    let dst-start :: <integer> = start;
    for (until: (available = 0))
      let dst-stop :: <integer> = (dst-start + available);
      if (dst-stop >= stop)
	if (to-eof?)
	  error("Destination not big enough to read to EOF -- %=.",
		destination);
	end;
	let this-copy = (stop - dst-start);
	copy-bytes(destination, dst-start, buf, buf-start, this-copy);
	release-input-buffer(stream, (buf-start + this-copy),
			     // Can't assume buf-start is 0 because we may
			     // come in here on the first iteration.
			     (buf-start + available));
	exit-loop(destination, #f);
      else
	copy-bytes(destination, dst-start, buf, buf-start, available);
      end;
      available := fill-input-buffer(stream, 0);
      count := (count + available);
      dst-start := dst-stop;
      buf-start := 0;
    finally
      // Whenever the loop terminates normally, we either successfully read
      // to EOF, or we failed to read the required data to fill destination
      // to stop.
      release-input-buffer(stream, 0, 0);
      case
	(to-eof?) => values(destination, (start + count));
	(signal-eof?) => error(make(<end-of-file>, stream: stream));
	otherwise => values(#f, #t);
      end;
    end for;
  end block;
end method;



/// read-into! for <buffer> destinations
///            on <fd-stream>s and <byte-string-input-stream>s.
///
/// This page contains read-into! methods that fill <buffer>s for
/// <fd-stream>s and <byte-string-input-stream>s.  Read-into! for <buffer>s
/// needs to be implemented for each stream class to avoid double
/// buffering.
///

define sealed method read-into!
    (destination :: <buffer>, stream :: <fd-stream>,
     #key signal-eof? :: <boolean> = #t,
          start :: <buffer-index> = 0,
          end: stop :: <buffer-index> = destination.size,
          to-eof? :: <boolean> = #f)
    => (result :: false-or(<buffer>),
	eof?-or-how-much :: type-union(<boolean>, <integer>));
  let (buf :: <buffer>, buf-start :: <buffer-index>,
       buf-stop :: <buffer-index>)
    = get-input-buffer(stream);
  let (start :: <buffer-index>, count :: <buffer-index>)
    = if (buf-start = buf-stop)
	values(start, 0);
      else
	let count = min((buf-stop - buf-start), (stop - start));
	copy-bytes(destination, start, buf, buf-start, count);
	values((start + count), count);
      end;
  let fd = stream.file-descriptor;
  block (exit-loop)
    let stop :: <buffer-index> = if (to-eof?) destination.size else stop end;
    for (num-bytes :: <buffer-index>
	   = call-fd-function(fd-read, fd, destination, start, (stop - start))
	 then call-fd-function(fd-read, fd, destination, start,
			       (stop - start)),
	 until: (num-bytes = 0))
      start := start + num-bytes;
      count := count + num-bytes;
      case
	(start ~= stop) =>
	  // Keep going and try to get more input.
	  #f;   // Case is broken in Mindy.
	(~ to-eof?) =>
	  // We got all the requested input, and we are not trying to read to
	  // EOF.  Just return everything.
	  release-input-buffer(stream, 0, 0);
	  exit-loop(destination, #f);
	(call-fd-function(fd-read, fd, buf, 0, buf.size) ~= 0) =>
	  // We're trying to read to EOF, and we've read everything the buffer
	  // can hold.  Furthermore, there is still input available, so error.
	  error("Destination not big enough to read to EOF -- %=.",
		destination);
	otherwise =>
	  // Everything's cool.  Return successfully.
	  release-input-buffer(stream, 0, 0);
	  exit-loop(destination, count);
      end;
    finally
      // Whenever the loop terminates normally, we either successfully read
      // to EOF, or we failed to read the required data to fill the
      // destination to stop.
      release-input-buffer(stream, 0, 0);
      case
	(to-eof?) => values(destination, count);
	(signal-eof?) => error(make(<end-of-file>, stream: stream));
	otherwise => values(#f, #t);
      end;
    end for;
  end block;
end method;

define sealed method read-into!
    (destination :: <buffer>, stream :: <byte-string-input-stream>,
     #key signal-eof? :: <boolean> = #t,
          start :: <buffer-index> = 0,
          end: stop :: <buffer-index> = destination.size,
          to-eof? :: <boolean>)
    => (result :: false-or(<buffer>),
	eof?-or-how-much :: type-union(<boolean>, <integer>));
  let (buf :: <buffer>, next :: <buffer-index>, buf-stop :: <buffer-index>)
    = get-input-buffer(stream);
  let available :: <buffer-index> = (buf-stop - next);
  if (to-eof?)
    if (available <= (destination.size - start))
      copy-bytes(destination, start, buf, next, available);
      values(destination, available);
    else
      release-input-buffer(stream, next, buf-stop);
      error("Destination not big enough to read to EOF -- %=.",
	    destination);
    end;
  else
    let need :: <buffer-index> = (stop - start);
    if (available >= need)
      copy-bytes(destination, 0, buf, next, need);
      release-input-buffer(stream, next, buf-stop);
      values(destination, #f);
    else
      release-input-buffer(stream, 0, 0);
      if (signal-eof?)
	error(make(<end-of-file>, stream: stream));
      else
	values(#f, #t);
      end;
    end;
  end;
end method;



/// write
///

define open generic write (object :: <object>, stream :: <stream>, #key)
    => stream :: <stream>;


define sealed method write (object :: <byte-character>, stream :: <stream>,
			    #key)
    => stream :: <stream>;
  let (buf :: <buffer>, next :: <buffer-index>, size :: <buffer-index>)
    = get-output-buffer(stream);
  if (next = size)
    empty-output-buffer(stream, size);
    next := 0;
  end;
  buf[next] := as(<integer>, object);
  release-output-buffer(stream, next + 1);
  stream;
end method;

define sealed method write (object :: <byte>, stream :: <stream>, #key)
    => stream :: <stream>;
  let (buf :: <buffer>, next :: <buffer-index>, size :: <buffer-index>)
    = get-output-buffer(stream);
  if (next = size)
    empty-output-buffer(stream, size);
    next := 0;
  end;
  buf[next] := object;
  release-output-buffer(stream, next + 1);
  stream;
end method;

/// This method implements the write function for <byte-string> and
/// <byte-vector>.  This function would work for <buffer>s too, but writing
/// buffers is implemented for each stream individually to avoid double
/// buffer.
///
define sealed method write (object :: type-union(<byte-vector>, <byte-string>),
			    stream :: <stream>,
			    #key start :: <integer> = 0,
			         end: stop :: <integer> = object.size)
    => stream :: <stream>;
  let (buf :: <buffer>, next :: <buffer-index>, size :: <buffer-index>)
    = get-output-buffer(stream);
  if (next = size)
    empty-output-buffer(stream, size);
    next := 0;
  end;
  block (exit-loop)
    let buf-capacity :: <buffer-index> = (size - next);
    let buf-start :: <buffer-index> = next;
    while (#t)
      let partial-stop :: <integer> = (start + buf-capacity);
      if (partial-stop >= stop)
	let this-copy = (stop - start);
	copy-bytes(buf, buf-start, object, start, this-copy);
	release-output-buffer(stream, (buf-start + this-copy));
	exit-loop(stream);
      else
	copy-bytes(buf, buf-start, object, start, buf-capacity);
      end;
      empty-output-buffer(stream, size);
      buf-capacity := size;
      buf-start := 0;
      start := partial-stop;
    end;
  end block;
  stream;
end method;

// Mindy does not parse "seal generic" forms currently.
// The streams spec requires sealed methods for these types.
//
define sealed domain write (<byte-vector>, <stream>);
define sealed domain write (<byte-string>, <stream>);
//



/// write for <buffer>s.
///
/// This page contains implementations of write for each stream type so that
/// writing buffers can avoid double buffering.
///

define sealed method write (object :: <buffer>, stream :: <fd-stream>,
			    #key start :: <integer> = 0,
			         end: stop :: <integer> = object.size)
    => stream :: <stream>;
  let (buf :: <buffer>, next :: <buffer-index>)
    = get-output-buffer(stream);
  if (next ~= 0)
    empty-output-buffer(stream, next);
  end;
  let fd = stream.file-descriptor;
  // Keep writing until fd-write manages to write everything.
  for (x :: <buffer-index>
	 = (start + call-fd-function(fd-write, fd, object, start, stop))
         then (x + call-fd-function(fd-write, fd, object, x, stop - x)),
       until: (x = stop))
  end;
  release-output-buffer(stream, 0);
  stream;
end method;

define sealed method write
    (object :: <buffer>, stream :: <byte-string-output-stream>,
     #key start :: <integer> = 0,
          end: stop :: <integer> = object.size)
    => stream :: <stream>;
  let (buf :: <buffer>, buf-stop :: <buffer-index>)
    = get-output-buffer(stream);
  let object-len :: <integer> = (stop - start);
  let backup :: false-or(<byte-string>) = stream.string-output-stream-backup;
  if (backup)
    // Collect all output into a new backup.
    let backup-len :: <integer> = backup.size;
    let new-backup-len = backup-len + object-len + buf-stop;
    let new-backup :: <byte-string>
      = make(<byte-string>, size: new-backup-len);
    copy-bytes(new-backup, 0, backup, 0, backup-len);
    let backup-and-buf-len = (backup-len + buf-stop);
    if (buf-stop ~= 0)
      copy-bytes(new-backup, backup-len, buf, 0, buf-stop);
    end;
    copy-bytes(new-backup, backup-and-buf-len, object, start, object-len);
    stream.string-output-stream-backup := new-backup;
  else
    // Collect any output into a backup and leave the stream's buffer empty.
    let backup-len = object-len + buf-stop;
    let backup :: <byte-string>
      = make(<byte-string>, size: backup-len);
    if (buf-stop ~= 0)
      copy-bytes(backup, 0, buf, 0, buf-stop);
    end;
    copy-bytes(backup, buf-stop, object, start, object-len);
    stream.string-output-stream-backup := backup;
  end;
  release-output-buffer(stream, 0);
  stream;
end method;



/// write-line
///

define generic write-line (object :: <object>, stream :: <stream>, #all-keys)
    => stream :: <stream>;


define method write-line (object :: <object>, stream :: <stream>,
			  #rest key-args, #all-keys)
    => stream :: <stream>;
  lock-stream(stream);
  apply(write, object, stream, key-args);
  #if (newlines-are-CRLF) 
     write('\r', stream);
  #endif
  write('\n', stream);
  unlock-stream(stream);
  stream;
end method;



//// Fd Streams -- class definition and Stream Extension Protocol.
///

/// <fd-stream> Class -- Exported.
///
/// All file descriptor based streams inherit from this class.
///
/// This is a non-standard class defined for Gwydion streams.  This stream
/// and <file-stream> are the superclasses of <fd-file-stream>s.
///
define class <fd-stream> (<stream>)
  //
  // This slot holds the direction of the file-descriptor.  <fd-stream>s have
  // a single direction, as presented to the user.  However, if the file
  // descriptor really refers to a file, then the <fd-stream> is actually
  // bidirectional.  For <fd-stream>s, this slot is used to enforce the
  // direction specified when making the stream.  For <fd-file-stream>s,
  // this slot indicates the direction the user last used the stream, and the
  // value of this slot changes as the user changes directions of the
  // <fd-file-stream>.
  slot fd-direction :: one-of(#"input", #"output");
  slot file-descriptor :: <integer>;
  //
  // This slot has a buffer when the stream is open, #f when closed.
  slot buffer :: false-or(<buffer>);
  //
  // Buffer-next for input: streams holds the next available byte for input.
  // For output: streams this slot holds the next available location for
  // placing output.
  slot buffer-next :: <buffer-index>;
  //
  // Buffer-stop for input: streams holds the end of the available input.
  // This slot holds no meaningful value for output: streams.
  slot buffer-stop :: <buffer-index>;
end class;

define sealed domain make (singleton(<fd-stream>));
define sealed domain initialize (<fd-stream>);

define sealed method close (stream :: <fd-stream>) => ();
  if (stream.fd-direction == #"input")
    // Get buffer to make sure no one holds it.
    get-input-buffer(stream);
    call-fd-function(fd-close, stream.file-descriptor);
    stream.buffer := #f;
    release-input-buffer(stream, 0, 0);
  else
    let (buf :: <buffer>, next :: <buffer-index>)
      =	get-output-buffer(stream);
    if (next ~= 0)
      empty-output-buffer(stream, next);
    end;
    synchronize(stream);
    call-fd-function(fd-close, stream.file-descriptor);
    stream.buffer := #f;
    unregister-output-stream(stream);
    release-output-buffer(stream, 0);
  end;
end method;
  
define sealed method initialize
    (stream :: <fd-stream>, #next next-method,
     #key direction :: one-of(#"input", #"output") = #"input",
          fd :: <integer>,
          size: length :: <buffer-index> = $default-buffer-size)
    => result :: <fd-stream>;
  next-method();
  stream.fd-direction := direction;
  stream.file-descriptor := fd;
  stream.buffer := make(<buffer>, size: length);
  if (direction == #"input")
    // Next and stop are the same so that the first read will fill the buffer.
    stream.buffer-next := (stream.buffer-stop := 0);
  else
    register-output-stream(stream);
    stream.buffer-next := 0;
  end;
  stream;
end method;

define sealed method stream-extension-get-input-buffer
    (stream :: <fd-stream>)
    => (buffer :: <buffer>, next :: <buffer-index>, stop :: <buffer-index>);
  let direction = stream.fd-direction;
  if (direction == #"output")
    error("Stream is an output stream -- %=.", stream);
  end;
  let buf = stream.buffer;
  // Since buffer is currently unheld by anyone, make sure it isn't closed.
  if (~ buf) error("Stream has been closed -- %=.", stream) end;
  values(buf, stream.buffer-next, stream.buffer-stop);
end method;

define sealed method stream-extension-release-input-buffer
    (stream :: <fd-stream>, next :: <buffer-index>, stop :: <buffer-index>)
    => ();
  let direction = stream.fd-direction;
  case
    (direction == #"output") =>
      error("Stream is an output stream -- %=.", stream);
    (stop < next) =>
      error("Returned buffer with stop, %d, less than next, %d.", stop, next);
    otherwise =>
      stream.buffer-next := next;
      stream.buffer-stop := stop;
  end;
end method;

define sealed method stream-extension-fill-input-buffer
    (stream :: <fd-stream>, start :: <buffer-index>)
    => stop :: <buffer-index>;
  let direction = stream.fd-direction;
  if (direction == #"output")
    error("Stream is an output stream -- %=.", stream);
  end;
  let buf :: <buffer> = stream.buffer;
  let count = call-fd-function(fd-read, stream.file-descriptor, buf,
			       start, (buf.size - start));
  // Don't bother updating stream's notion of next and stop because we
  // rely on what the users tell us when they return the buffer.  Just
  // return the value.
  if (count = 0)
    0;
  else
    start + count;
  end;
end method;

define sealed method stream-extension-input-available-at-source?
    (stream :: <fd-stream>)
    => available? :: <boolean>;
  let direction = stream.fd-direction;
  if (direction == #"output")
    error("Stream is an output stream -- %=.", stream);
  end;
  call-fd-function(fd-input-available?, stream.file-descriptor);
end method;

define sealed method stream-extension-get-output-buffer
    (stream :: <fd-stream>)
    => (buffer :: <buffer>, next :: <buffer-index>, size :: <buffer-index>);
  let direction = stream.fd-direction;
  if (direction == #"input")
    error("Stream is an input stream -- %=.", stream);
  end;
  let buf = stream.buffer;
  // Since no one holds the buffer, make sure the stream isn't closed.
  unless (buf) error("Stream has been closed -- %=.", stream); end;
  let buf :: <buffer> = buf;
  let next :: <buffer-index> = stream.buffer-next;
  let buf-size :: <buffer-index> = buf.size;
  if (next = buf-size)
    let fd = stream.file-descriptor;
    // Keep writing until fd-write manages to write everything.
    for (x :: <buffer-index>
	    = call-fd-function(fd-write, fd, buf, 0, next)
	    then (x + call-fd-function(fd-write, fd, buf, x, next - x)),
	 until: (x = next))
    end;
    values(buf, 0, buf-size)
  else
    values(buf, next, buf-size);
  end;
end method;

define sealed method stream-extension-release-output-buffer
    (stream :: <fd-stream>, next :: <buffer-index>)
    => ();
  let direction = stream.fd-direction;
  if (direction == #"input")
    error("Stream is an input stream -- %=.", stream);
  end;
  stream.buffer-next := next;
end method;

define sealed method stream-extension-empty-output-buffer
    (stream :: <fd-stream>, stop :: <buffer-index>)
    => ();
  if (stream.fd-direction == #"input")
    error("Stream is an input stream -- %=.", stream);
  end;
  let fd = stream.file-descriptor;
  let buf :: <buffer> = stream.buffer;
  // Keep writing until fd-write manages to write everything.
  for (x :: <buffer-index> = call-fd-function(fd-write, fd, buf, 0, stop)
         then (x + call-fd-function(fd-write, fd, buf, x, stop - x)),
       until: (x = stop))
  end;
end;

define sealed method stream-extension-synchronize (stream :: <fd-stream>)
    => ();
  call-fd-function(fd-sync-output, stream.file-descriptor);
end;



//// Random Access Streams --  generic function declarations.
////

/// All of these are exported.
///

define generic stream-position (stream :: <random-access-stream>)
    => position :: <integer>;

define generic stream-position-setter
    (position :: <integer>, stream :: <random-access-stream>)
    => position :: <integer>;

define generic adjust-stream-position
    (offset :: <integer>,
     stream :: <random-access-stream>,
     #key from :: one-of(#"start", #"current", #"end")) // = #"start"
    => position :: <integer>;

define generic stream-size (stream :: <random-access-stream>)
    => size :: <integer>;



//// Fd File Streams -- class declarations and Random Access Protocol.
////

/// <file-stream> Class -- Exported.
///
define abstract open class <file-stream> (<random-access-stream>)
end class;

/// <fd-file-stream> Class -- Internal.
///
/// This is the concrete class that is instantiated when users make a
/// <file-stream>.
///
define sealed class <fd-file-stream> (<fd-stream>, <file-stream>)
  slot file-name :: <byte-string>;
  slot file-direction :: one-of(#"input", #"output", #"input-output");
end class;

define sealed domain make (singleton(<fd-file-stream>));
// Don't need to seal initialize because it is sealed on <fd-stream>.

/// stream-position -- Method for Exported Interface.
///
define sealed method stream-position (stream :: <fd-file-stream>)
    => position :: <integer>;
  if (stream.file-direction == #"input")
    // Get the buffer to ensure no one else is using it and to make it
    // possible to correctly compute the actual file position.
    let (buf, next :: <buffer-index>, stop :: <buffer-index>)
      = get-input-buffer(stream);
    ignore(buf);
    // Get the current position as recorded by the file-descritor module
    // and subtract what input we have in the buffer but haven't actually
    // read.
    let pos = call-fd-function(fd-seek, stream.file-descriptor, 0,
			       fd-seek-current);
    release-input-buffer(stream, next, stop);
    pos - (stop - next);
  else
    // Direction is #"output" or #"input-output".
    // Get the buffer to ensure no one else is using it and to make it
    // possible to correctly compute the actual file position.
    let (buf, next :: <buffer-index>, stop)
      = get-output-buffer(stream);
    ignore(buf, stop);
    // Get the current position as recorded by the file-descritor module
    // and add what output we have in the buffer but haven't sent yet.
    let pos = call-fd-function(fd-seek, stream.file-descriptor, 0,
			       fd-seek-current);
    release-output-buffer(stream, next);
    pos + next;
  end;
end method;

/// stream-position-setter -- Method for Exported Interface.
///
define sealed method stream-position-setter
    (position :: <integer>, stream :: <fd-file-stream>)
    => position :: <integer>;
  let direction = file-direction(stream);
  // Get the buffer to ensure no one else is using it and to make it
  // possible to invalidate the buffer's contents.
  if (direction == #"input")
    get-input-buffer(stream);
  else
    let (buf :: <buffer>, next :: <buffer-index>, stop :: <buffer-index>)
      = get-output-buffer(stream);
    ignore(buf, stop);
    // Force any pending output so that we can later correctly test for the
    // file's size.
    if (next > 0)
      empty-output-buffer(stream, next);
    end;
  end;
  // Set the position.
  let fd = stream.file-descriptor;
  if ((position > 0) &
      (position < call-fd-function(fd-seek, fd, 0, fd-seek-end)))
    call-fd-function(fd-seek, fd, position, fd-seek-set);
  else
    error("Illegal stream position -- %d", position);
  end;
  // Cleanup.
  if (direction == #"input")
    release-input-buffer(stream, 0, 0);
  else
    release-output-buffer(stream, 0);
  end;
  position;
end method;

/// adjust-stream-position -- Method for Exported Interface.
///
define sealed method adjust-stream-position
    (offset :: <integer>, stream :: <fd-file-stream>, 
     #key from: reference :: one-of(#"start", #"current", #"end") = #"start")
    => position :: <integer>;
  let direction = file-direction(stream);
  if (direction == #"input")
    // Get the buffer to ensure no one else is using it and to make it
    // possible to invalidate the buffer's contents.
    let (buf :: <buffer>, next :: <buffer-index>, stop :: <buffer-index>)
      = get-input-buffer(stream);
    ignore(buf);
    if (reference == #"current")
      // If moving the position relative to the current position, then
      // adjust the offset to account for the unread input in the buffer.
      // Because of the unread input, the file-descriptor module's record
      // of the position is ahead of the actual position.
      offset := offset - (stop - next);
    end;
    let pos = call-fd-function(fd-seek, stream.file-descriptor, offset,
			       select (reference)
				 (#"start") => fd-seek-set;
				 (#"current") => fd-seek-current;
				 (#"end") => fd-seek-end;
			       end);
    release-input-buffer(stream, 0, 0);
    pos;
  else
    // Get the buffer to ensure no one else is using it and to make it
    // possible to invalidate the buffer's contents.
    let (buf :: <buffer>, next :: <buffer-index>, stop :: <buffer-index>)
      = get-output-buffer(stream);
    ignore(buf, stop);
    // Force out any pending output while the file position is still right
    // for the file to receive this output.
    if (next > 0)
      empty-output-buffer(stream, next);
    end;
    let pos = call-fd-function(fd-seek, stream.file-descriptor, offset,
			       select (reference)
				 (#"start") => fd-seek-set;
				 (#"current") => fd-seek-current;
				 (#"end") => fd-seek-end;
			       end);
    release-output-buffer(stream, 0);
    pos;
  end;
end method;

/// stream-size -- Method for Exported Interface.
///
define sealed method stream-size (stream :: <fd-file-stream>)
    => size :: <integer>;
  if (stream.file-direction == #"input")
    // Get the buffer to ensure no one else is using it and to make it
    // possible to correctly compute the actual file position.
    let (buf, next :: <buffer-index>, stop :: <buffer-index>)
      = get-input-buffer(stream);
    ignore(buf);
    let pos = call-fd-function(fd-seek, stream.file-descriptor, 0,
			       fd-seek-current);
    let size = call-fd-function(fd-seek, stream.file-descriptor, 0,
				fd-seek-end);
    call-fd-function(fd-seek, stream.file-descriptor, pos, fd-seek-set);
    release-input-buffer(stream, next, stop);
    size;
  else
    // Direction is #"output" or #"input-output".
    // Get the buffer to ensure no one else is using it and to make it
    // possible to correctly compute the actual file position and size.
    let (buf, next :: <buffer-index>, stop)
      = get-output-buffer(stream);
    ignore(buf, stop);
    // Force any pending output so that we can later correctly test for the
    // file's size.  We don't know if the current pending output is
    // overwriting part of the file or extending its length.
    if (next > 0)
      empty-output-buffer(stream, next);
    end;
    let pos = call-fd-function(fd-seek, stream.file-descriptor, 0,
			       fd-seek-current);
    let size = call-fd-function(fd-seek, stream.file-descriptor, 0,
				fd-seek-end);
    call-fd-function(fd-seek, stream.file-descriptor, pos, fd-seek-set);
    release-output-buffer(stream, next);
    size;
  end;
end method;



//// Fd File Streams -- Stream Extension Protocol.
////

/// The following methods from <fd-streams> work:
///    close
///    stream-extension-synchronize
///


/// file-buffer-last-use -- Internal.
/// file-buffer-last-use-setter -- Internal.
///
/// These are defined for readability.
///
define constant file-buffer-last-use = fd-direction;
define constant file-buffer-last-use-setter = fd-direction-setter;


define sealed inline method make
    (result-class :: singleton(<file-stream>), #rest keys, #all-keys)
    => result :: <fd-file-stream>;
  apply(make, <fd-file-stream>, keys);
end method;

define sealed method initialize
    (stream :: <fd-file-stream>, #next next-method, #rest rest-args,
     #key name :: false-or(<byte-string>),
          direction :: one-of(#"input", #"output", #"input-output")
                     = #"input",
          if-exists :: one-of(#"signal", #"replace", #"overwrite",
				  #"append")
	             = #"replace",
	  size: length :: <buffer-index> = $default-buffer-size)
    => result :: <fd-file-stream>;
  if (~ name)
    error("Must supply a filename when making a <file-stream>.");
  end;
  if (direction == #"input")
    let (fd, err) = fd-open(name, fd-o_rdonly);
    case
      (~ err) => #f;   // Case is broken in Mindy.
      (err = fd-enoent) => error(make(<file-not-found>, filename: name));
      otherwise => error(make(<syscall-error>, errno: err));
    end;
    stream.file-name := name;
    stream.file-direction := #"input";
    apply(next-method, stream, fd: fd, direction: #"input", rest-args); 
    stream;
  else
    // Make an #"output" or #"input-output" stream.
    let flags :: <integer> = fd-o_creat;
    flags := select (direction)
	       (#"output") => logior(flags, fd-o_wronly);
	       (#"input-output") => logior(flags, fd-o_rdwr);
	     end;
    flags := select (if-exists)
	       (#"signal") => logior(flags, fd-o_excl);
	       (#"replace") => logior(flags, fd-o_trunc);
	       otherwise => flags;
	     end;
    let (fd, err) = fd-open(name, flags);
    case
      (~ err) => #f;   // Case is broken in Mindy.
      (err = fd-eexist) => error(make(<file-exists>, filename: name));
      otherwise => error(make(<syscall-error>, errno: err));
    end;
    if (if-exists == #"append")
      call-fd-function(fd-seek, fd, 0, fd-seek-end);
    end;
    stream.file-name := name;
    stream.file-direction := direction;
    apply(next-method, stream, fd: fd,
	  direction: if (direction == #"output") #"output" else #"input" end,
	  rest-args); 
    register-output-stream(stream);
  end;
end method;

define sealed method close (stream :: <fd-file-stream>, #next next-method)
    => ();
  next-method();
  if ((stream.file-direction == #"input-output")
	& (stream.file-buffer-last-use == #"input"))
    unregister-output-stream(stream);
  end;
end method;

/// This method does not call next-method because this method does most of the
/// work determining what to do, and if it did call next-method, in one case
/// it would have to do extra work just to make next-method work.
///
/// This method does not have to check whether the stream or buffer is locked
/// because get-input-buffer does that.
/// 
define sealed method stream-extension-get-input-buffer
    (stream :: <fd-file-stream>)
    => (buffer :: <buffer>, next :: <buffer-index>, stop :: <buffer-index>);
  let direction = stream.file-direction;
  if (direction == #"output")
    error("Stream is an output stream -- %=.", stream);
  end;
  let buf = stream.buffer;
  // Since buffer is currently unheld by anyone, make sure it isn't closed.
  unless (buf) error("Stream has been closed -- %=.", stream); end;
  if ((direction == #"input") | (stream.file-buffer-last-use == #"input"))
    values(buf, stream.buffer-next, stream.buffer-stop);
  else
    // The stream is both #"input-output" and was last used for #"output".
    let next :: <buffer-index> = stream.buffer-next;
    if (next > 0)
      // Keep writing until fd-write manages to write everything.
      let fd = stream.file-descriptor;
      for (x :: <buffer-index>
	     = call-fd-function(fd-write, fd, buf, 0, next)
	     then (x + call-fd-function(fd-write, fd, buf, x, next - x)),
	   until: (x = next))
      end;
    end;
    stream.file-buffer-last-use := #"input";
    // There's no reason to update the stream's notion of next and stop
    // because we rely on the users' values when they return the buffer.
    values(buf, 0, 0);
  end;
end method;

/// This method does not call next-method because this method does most of the
/// work determining what to do, and then only sets two slots.
///
/// This method does not have to check whether the stream or buffer is locked
/// because release-input-buffer does that.
///
define sealed method stream-extension-release-input-buffer
    (stream :: <fd-file-stream>, next :: <buffer-index>,
     stop :: <buffer-index>)
    => ();
  let direction = stream.file-direction;
  case (direction == #"output") =>
      error("Stream is an output stream -- %=.", stream);
    (~ ((direction == #"input") |
	(stream.file-buffer-last-use == #"input"))) =>
      error("Buffer is currently held for output -- %=.", stream);
    (stop < next) =>
      error("Returned buffer with stop, %d, less than next, %d.", stop, next);
    otherwise =>
      stream.buffer-next := next;
      stream.buffer-stop := stop;
  end;
end method;

/// This method does not call next-method because it would waste time doing
/// some tests again and then only execute a few statements.
///
define sealed method stream-extension-fill-input-buffer
    (stream :: <fd-file-stream>, start :: <buffer-index>)
    => stop :: <buffer-index>;
  let direction = stream.file-direction;
  if (direction == #"output")
    error("Stream is an output stream -- %=.", stream);
  end;
  if ((direction == #"input") | (stream.file-buffer-last-use == #"input"))
    let buf = stream.buffer;
    let count = call-fd-function(fd-read, stream.file-descriptor, buf,
				 start, (buf.size - start));
    // Don't bother updating stream's notion of next and stop because we
    // rely on what the users tell us when they return the buffer.  Just
    // return the value.
    if (count = 0)
      0;
    else
      start + count;
    end;
  else
    error("Buffer is currently held for output -- %=.", stream);
  end;
end method;

/// This method does not call next-method because it would waste time doing
/// some tests again and then only execute one line.
///
define sealed method stream-extension-input-available-at-source?
    (stream :: <fd-file-stream>)
    => available? :: <boolean>;
  let direction = stream.file-direction;
  if (direction == #"output")
    error("Stream is an output stream -- %=.", stream);
  end;
  if ((direction == #"input") | (stream.file-buffer-last-use == #"input"))
    call-fd-function(fd-input-available?, stream.file-descriptor);
  else
    error("Buffer is currently held for output -- %=.", stream);
  end;
end method;

/// This method does not call next-method because this method does most of the
/// work determining what to do, and if it did call next-method, in one case
/// it would have to do extra work just to make next-method work.
/// 
/// This method does not have to check whether the stream or buffer is locked
/// because get-output-buffer does that.
///
define sealed method stream-extension-get-output-buffer
    (stream :: <fd-file-stream>)
    => (buffer :: <buffer>, next :: <buffer-index>, size :: <buffer-index>);
  let direction = stream.file-direction;
  if (direction == #"input")
    error("Stream is an input stream -- %=.", stream);
  end;
  let buf = stream.buffer;
  // Since buffer is unheld by anyone, make sure it isn't closed.
  unless (buf) error("Stream has been closed -- %=.", stream); end;
  let next :: <buffer-index> = stream.buffer-next;
  let buf-size :: <buffer-index> = buf.size;
  if ((direction == #"output") | (stream.file-buffer-last-use == #"output"))
    if (next = buf-size)
      let fd = stream.file-descriptor;
      // Keep writing until fd-write manages to write everything.
      for (x :: <buffer-index>
	     = call-fd-function(fd-write, fd, buf, 0, next)
	     then (x + call-fd-function(fd-write, fd, buf, x, next - x)),
	   until: (x = next))
      end;
      values(buf, 0, buf-size)
    else
      values(buf, next, buf-size);
    end;
  else
    // The stream is both #"input-output" and was last used for #"input".
    let stop :: <buffer-index> = stream.buffer-stop;
    if (stop > next)
      // Set the file position correctly relative to the actual reading done
      // on the stream so that when users force output, it goes to the right
      // location in the file.
      call-fd-function(fd-seek, stream.file-descriptor, next - stop,
		       fd-seek-current);
    end;
    stream.file-buffer-last-use := #"output";
    values(buf, 0, buf-size);
  end;
end method;

/// This method does not call next-method because this method does most of the
/// work determining what to do, and then only sets a slot.
///
/// This method does not have to check whether the stream or buffer is locked
/// because release-output-buffer does that.
///
define sealed method stream-extension-release-output-buffer
    (stream :: <fd-file-stream>, next :: <buffer-index>)
    => ();
  let direction = stream.file-direction;
  if (direction == #"input")
    error("Stream is an input stream -- %=.", stream);
  end;
  if ((direction == #"output") | (stream.file-buffer-last-use == #"output"))
    stream.buffer-next := next;
  else
    error("Buffer is currently held for input -- %=.", stream);
  end;
end method;

/// This method does not call next-method because it would waste time doing
/// some tests again and then only execute a few statements.
///
define sealed method stream-extension-empty-output-buffer
    (stream :: <fd-file-stream>, stop :: <buffer-index>)
    => ();
  if (stream.file-direction == #"input")
    error("Stream is an input stream -- %=.", stream);
  end;
  if ((stream.file-direction == #"input-output") &
      (stream.file-buffer-last-use == #"input"))
    error("Buffer last used for input -- %=.", stream);
  end;
  let fd = stream.file-descriptor;
  let buf = stream.buffer;
  // Keep writing until fd-write manages to write everything.
  for (x :: <buffer-index> = call-fd-function(fd-write, fd, buf, 0, stop)
         then (x + call-fd-function(fd-write, fd, buf, x, stop - x)),
       until: (x = stop))
  end;
end;



//// String Input Streams -- Stream Extension Protocol.
////

/// The <string-input-stream> class is the class from which all other
/// string-input streams inherit.  This class cannot define slots for
/// subclasses to inherit because the stream interface makes no provision
/// for implementors of new string-input streams to access whatever commonly
/// defined slots subclasses might have.
///
define abstract open class <string-input-stream> (<random-access-stream>)
end class;

define sealed method make (result-class :: singleton(<string-input-stream>),
			   #rest keys, #all-keys)
 => res :: <never-returns>;
  error("<string-input-stream> is not instantiable.  In this implementation "
	"of streams, you should call make on <byte-string-input-stream>.");
end method;

define class <byte-string-input-stream> (<string-input-stream>)
  slot buffer :: false-or(<buffer>);
  slot buffer-next :: <buffer-index>;
  slot buffer-stop :: <buffer-index>;
end class;

define sealed domain make (singleton(<byte-string-input-stream>));

define sealed method initialize
    (stream :: <byte-string-input-stream>,
     #next next-method,
     #key string :: <byte-string> = "",
          start :: <integer> = 0,
          end: stop :: <integer> = string.size,
	  size: length :: <buffer-index> = 0)
    => result :: <byte-string-input-stream>;
  ignore(length);
  // Do some bounds checking ...
  if (start < 0)
    error("Bounds error in string -- %d.", start);
  end;
  if (stop > string.size)
    error("Bounds error in string -- %d.", stop);
  end;
  if (start > stop)
    error("Start, %d, must be less than or equal to end, %d.", start, stop);
  end;
  next-method();
  // Fill in the stream's slots and copy the string into the buffer.
  let length :: <buffer-index> = stop - start;
  let buf :: <buffer> = make(<buffer>, size: length);
  stream.buffer := buf;
  copy-bytes(buf, 0, string, start, length);
  stream.buffer-next := 0;
  stream.buffer-stop := length;
  stream;
end method;

define sealed method close (stream :: <byte-string-input-stream>) => ();
  // Get buffer to make sure no one else holds it.
  get-input-buffer(stream);
  stream.buffer := #f;
  release-input-buffer(stream, 0, 0);
end method;

define sealed method stream-extension-get-input-buffer
    (stream :: <byte-string-input-stream>)
    => (buffer :: <buffer>, next :: <buffer-index>, stop :: <buffer-index>);
  let buf = stream.buffer;
  // Since buffer is currently unheld by anyone, make sure it isn't closed.
  unless (buf) error("Stream has been closed -- %=.", stream); end;
  values(buf, stream.buffer-next, stream.buffer-stop);
end method;

define sealed method stream-extension-release-input-buffer
    (stream :: <byte-string-input-stream>, next :: <buffer-index>,
     stop :: <buffer-index>)
    => ();
  if (stop < next)
    error("Returned buffer with stop, %d, less than next, %d.", stop, next);
  else
    stream.buffer-next := next;
    stream.buffer-stop := stop;
  end;
end method;

define sealed method stream-extension-fill-input-buffer
    (stream :: <byte-string-input-stream>, start :: <buffer-index>)
    => stop :: <buffer-index>;
  // You can never get more input for the buffer, so return zero.
  0;
end method;

define sealed method stream-extension-input-available-at-source?
    (stream :: <byte-string-input-stream>)
    => available? :: <boolean>;
  // You can never get more input for the buffer.
  #f;
end method;



//// String Input Streams -- Random Access Protocol.
////

/// All of these methods are for exported functions.
///

define sealed method stream-position (stream :: <byte-string-input-stream>)
    => position :: <integer>;
  // Get the buffer to ensure no one else is using it.
  let (buf, next :: <buffer-index>, stop :: <buffer-index>)
    = get-input-buffer(stream);
  ignore(buf);
  release-input-buffer(stream, next, stop);
  next;
end method;

define sealed method stream-position-setter
    (position :: <integer>, stream :: <byte-string-input-stream>)
    => position :: <integer>;
  // Get the buffer to ensure no one else is using it.
  let (buf, next :: <buffer-index>, stop :: <buffer-index>)
    = get-input-buffer(stream);
  ignore(buf, next);
  if ((position < 0) | (position > stop))
    error("Illegal stream position -- %d.", position);
  end;
  release-input-buffer(stream, position, stop);
  position;
end method;

/// This method does not call stream-position-setter because this method
/// does most of the work determining what to do, and then just releases
/// the buffer.
///
define sealed method adjust-stream-position
    (offset :: <integer>,
     stream :: <byte-string-input-stream>,
     #key from: reference :: one-of(#"start", #"current", #"end") = #"start")
    => position :: <integer>;
  // Get the buffer to ensure no one else is using it.
  let (buf, next :: <buffer-index>, stop :: <buffer-index>)
    = get-input-buffer(stream);
  ignore(buf);
  let position = select (reference)
		   (#"start") => offset;
		   (#"current") => (next + offset);
		   (#"end") => (stop + offset);
		 end;
  if ((position < 0) | (position > stop))
    error("Illegal stream position -- %d.", position);
  end;
  release-input-buffer(stream, position, stop);
  position;
end method;

define sealed method stream-size (stream :: <byte-string-input-stream>)
    => size :: <integer>;
  // Get the buffer to ensure no one else is using it.
  let (buf, next :: <buffer-index>, stop :: <buffer-index>)
    = get-input-buffer(stream);
  ignore(buf);
  release-input-buffer(stream, next, stop);
  stop;
end method;



//// String Output Streams -- classes, protocol, and Stream Extension Protocol.
////

/// The <string-output-stream> class is the class from which all other
/// string-output streams inherit.  This class cannot define slots for
/// subclasses to inherit because the stream interface makes no provision
/// for implementors of new string-output streams to access whatever commonly
/// defined slots subclasses might have.
///
define abstract open class <string-output-stream> (<random-access-stream>)
end class;

define sealed method make (result-class :: singleton(<string-output-stream>),
			   #rest keys, #all-keys)
 => res :: <never-returns>;
  error("<string-output-stream> is not instantiable.  In this implementation "
	"of streams, you should call make on <byte-string-output-stream>.");
end method;

/// This class collects its output in a buffer.  This makes mutual exclusion
/// easier because internal code can use the Buffer Access Protocol.  Also,
/// because the sequence operations in Dylan are nearly worthless, internal
/// code can use the <buffer> protocol to copy stuff around.  This saves
/// writing our own string to string copying routines.
///
define class <byte-string-output-stream> (<string-output-stream>)
  slot buffer :: false-or(<buffer>);
  slot string-output-stream-backup :: false-or(<byte-string>) = #f;
  //
  // This slot holds the current position for writing into the buffer.
  slot buffer-next :: <buffer-index> = 0;
  //
  // This slot holds the end of the output held in the buffer.  Because of the
  // Random Access Protocol buffer-next may not be at the end of all the output
  // written.
  slot buffer-stop :: <buffer-index> = 0;
end class;

define sealed domain make (singleton(<byte-string-output-stream>));

/// This method does not call register-output-stream because it is
/// meaningless to force output on a <byte-string-output-stream> when the
/// application exits.
///
define sealed method initialize
    (stream :: <byte-string-output-stream>,
     #next next-method,
     #key size: length :: <buffer-index> = $default-buffer-size)
    => result :: <byte-string-output-stream>;
  stream.buffer := make(<buffer>, size: length);
  stream;
end method;

/// string-output-stream-string -- Exported.
///
define open generic string-output-stream-string
    (stream :: <string-output-stream>)
    => output :: <string>;

/// string-output-stream-string -- Method for Exported Interface.
///
/// Collect the output backed up in the stream as a <byte-string> and
/// the pending output in the stream's buffer, and return this as a
/// <byte-string>.
///					    
define sealed method string-output-stream-string
    (stream :: <byte-string-output-stream>)
    => output :: <byte-string>;
  let buf :: <buffer> = get-output-buffer(stream);
  let backup :: false-or(<byte-string>) = stream.string-output-stream-backup;
  let output-len :: <integer> = stream.buffer-stop;
  let string
    = case
	(~ backup) =>
	  // The only output is what is in the buffer.
	  let res = make(<byte-string>, size: output-len);
	  copy-bytes(res, 0, buf, 0, output-len);
	  res;
	(output-len = 0) =>
	  // The only output is what is in the backup string.
	  backup;
	otherwise =>
	  // Get output from both the backup string and the buffer.
	  let backup-len :: <integer> = backup.size;
	  let res :: <byte-string>
	    = make(<byte-string>, size: (backup-len + output-len));
	  copy-bytes(res, 0, backup, 0, backup-len);
	  copy-bytes(res, backup-len, buf, 0, output-len);
	  res;
      end;
  stream.string-output-stream-backup := #f;
  stream.buffer-stop := 0;
  release-output-buffer(stream, 0);
  string;
end method;

/// close -- Method for Exported Interface.
///
define sealed method close (stream :: <byte-string-output-stream>) => ();
  // Get the buffer to make sure no one is using it.
  get-output-buffer(stream);
  stream.buffer := #f;
  unregister-output-stream(stream);
  release-output-buffer(stream, 0);
end method;

/// stream-extension-get-output-buffer -- Method for Exported Interface.
///
/// This must not return a full buffer.  When the buffer is full, this
/// creates a backup store using a <byte-string>.  If there is already a
/// backup string, then this function creates a new one to hold all the
/// previously backed up output and what is in the buffer.
///
define sealed method stream-extension-get-output-buffer
    (stream :: <byte-string-output-stream>)
    => (buffer :: <buffer>, next :: <buffer-index>, stop :: <buffer-index>);
  let buf :: <buffer> = stream.buffer;
  let buf-next :: <buffer-index> = stream.buffer-next;
  let buf-len :: <buffer-index> = buf.size;
  // Test buf-next rather that buffer-stop.  Though buffer-stop may indicate
  // the buffer is full, there's no reason to back up the buffer when the
  // buf-next says the user isn't writing off the end of the buffer.
  if (buf-next = buf-len)
    // Can't write further in the buffer.
    let backup :: false-or(<byte-string>) = stream.string-output-stream-backup;
    if (backup)
      // Concatenate the backup and buffer to form new backup string.
      let backup-len :: <integer> = backup.size;
      let new-backup-len = backup-len + buf-len;
      let res :: <byte-string>
	= make(<byte-string>, size: new-backup-len);
      copy-bytes(res, 0, backup, 0, backup-len);
      copy-bytes(res, backup-len, buf, 0, buf-len);
      stream.string-output-stream-backup := res;
    else
      // Just copy the buffer into a backup string.
      stream.string-output-stream-backup :=
        buffer-subsequence(buf, <byte-string>, 0, buf-len);
    end;
    // Make sure buffer-stop is maintained correctly, and we move any output
    // remaining in the buffer to the beginning of the buffer.  This ensure
    // the output is correctly placed to be overwritten.  We do not update
    // buffer-next since we must rely on the user's value when he releases
    // the buffer.
    let stop :: <buffer-index> = stream.buffer-stop;
    if (stop > buf-next)
      let new-stop :: <buffer-index> = (stop - buf-next);
      copy-bytes(buf, 0, buf, buf-next, new-stop);
      stream.buffer-stop := new-stop;
    else
      stream.buffer-stop := 0;
    end;
    values(buf, 0, buf-len);
  else
    // Just return the values, nothing special to do.
    values(buf, buf-next, buf-len);
  end;
end method;

define sealed method stream-extension-release-output-buffer
    (stream :: <byte-string-output-stream>, next :: <buffer-index>)
    => ();
  stream.buffer-next := next;
  if (next > stream.buffer-stop) stream.buffer-stop := next end;
end method;

define sealed method stream-extension-empty-output-buffer
    (stream :: <byte-string-output-stream>, stop :: <buffer-index>)
    => ();
  let buf :: <buffer> = stream.buffer;
  let backup :: false-or(<byte-string>) = stream.string-output-stream-backup;
  if (backup)
    // Add output in buffer to backup.
    let backup-len :: <integer> = backup.size;
    let new-backup-len = backup-len + stop;
    let res :: <byte-string>
      = make(<byte-string>, size: new-backup-len);
    copy-bytes(res, 0, backup, 0, backup-len);
    copy-bytes(res, backup-len, buf, 0, stop);
    stream.string-output-stream-backup := res;
  else
    // Just create a backup string.
    stream.string-output-stream-backup
      := buffer-subsequence(buf, <byte-string>, 0, stop);
  end;
  // Make sure buffer-stop is maintained correctly, and we move any left over
  // output to the beginning of the buffer to be overwritten.  We do not
  // update buffer-next since we must rely on the user's value when he releases
  // the buffer.
  let real-stop :: <buffer-index> = stream.buffer-stop;
  if (real-stop > stop)
    let new-stop :: <buffer-index> = (real-stop - stop);
    copy-bytes(buf, 0, buf, stop, new-stop);
    stream.buffer-stop := new-stop;
  else
    stream.buffer-stop := 0;
  end;
end method;

define sealed method stream-extension-synchronize
    (stream :: <byte-string-output-stream>)
    => ();
end method;



//// String output streams -- Random Access Protocol.
////

/// All of these methods are for exported functions.
///

define method stream-position (stream :: <byte-string-output-stream>)
    => position :: <integer>;
  // Get the output buffer to make sure the stream is not already in use.
  let (buf :: <buffer>, next :: <buffer-index>) = get-output-buffer(stream);
  ignore(buf);
  let backup :: false-or(<byte-string>) = stream.string-output-stream-backup;
  release-output-buffer(stream, next);
  if (backup)
    backup.size + next;
  else
    next;
  end;
end method;

define method stream-position-setter (position :: <integer>,
				      stream :: <byte-string-output-stream>)
    => position :: <integer>;
  // Get the output buffer to make sure the stream is not already in use.
  let (buf :: <buffer>, next :: <buffer-index>) = get-output-buffer(stream);
  let stop :: <buffer-index> = stream.buffer-stop;
  let backup :: false-or(<byte-string>) = stream.string-output-stream-backup;
  let backup-len :: <integer> = if (backup) backup.size else 0 end;
  let stream-len :: <integer> = backup-len + stop;
  if ((position < 0) | (position > stream-len))
    error("Illegal stream position -- %d.", position);
  end;
  if (position >= backup-len)
    // Reposition within the existing buffer.
    release-output-buffer(stream, (position - backup-len));
  else
    new-string-output-stream-backup(stream, buf, stop, backup, backup-len);
    release-output-buffer(stream, position);
  end;
  position;
end method;

define method adjust-stream-position
    (offset :: <integer>,
     stream :: <byte-string-output-stream>,
     #key from: reference :: one-of(#"start", #"current", #"end") = #"start")
    => position :: <integer>;
  // Get the output buffer to make sure the stream is not already in use.
  let (buf :: <buffer>, buf-next :: <buffer-index>)
    = get-output-buffer(stream);
  let stop :: <buffer-index> = stream.buffer-stop;
  let backup :: false-or(<byte-string>) = stream.string-output-stream-backup;
  let backup-len :: <integer> = if (backup) backup.size else 0 end;
  let stream-len :: <integer> = backup-len + stop;
  let position = select (reference)
		   (#"start") => offset;
		   (#"current") => (buf-next + offset);
		   (#"end") => (stream-len + offset);
		 end;
  case
    (position < 0) =>
      error("Illegal stream position -- %d.", position);
    ((position >= backup-len) & (position <= stream-len)) =>
      release-output-buffer(stream, (position - backup-len));
    (position > stream-len) =>
      // Get output from both the backup string and the buffer.
      let new-backup = make(<byte-string>, size: position);
      if (backup)
	copy-bytes(new-backup, 0, backup, 0, backup-len);
      end;
      copy-bytes(new-backup, backup-len, buf, 0, stop);
      for (i from (backup-len + stop) below position)
	new-backup[i] := '\0';
      end;
      stream.string-output-stream-backup := new-backup;
      stream.buffer-stop := 0;
      release-output-buffer(stream, 0);
    otherwise =>
      new-string-output-stream-backup(stream, buf, stop, backup, backup-len);
      release-output-buffer(stream, position);
  end;
  position;
end method;

/// new-string-output-stream-backup -- Internal
///
/// This function implements file-position-setter and adjust-file-position
/// when the new position is in the backup string.  This function just moves
/// everything into a new buffer and loses the backup.
///
/// This method assumes buffers can hold as much as backup strings; however,
/// the rest of this streams implementation uses <integer> indexes for strings
/// and <integer> indexes for buffers.  It could be that a backup string
/// could grow to a size that no buffer could hold it, but that's pretty
/// unlikely in most implementations.  If it should ever happen, the make call
/// to get a new buffer should flame out, and someone will have to write a
/// better implementation of <byte-string-output-stream>s.
///
define method new-string-output-stream-backup
    (stream :: <stream>, buf :: <buffer>, stop :: <buffer-index>,
     backup :: <byte-string>, backup-len :: <integer>)
  // Create a new buffer to hold the backup's, if any, and the current
  // buffer's contents.  Throw away the old buffer and backup.
  let new-buf = make(<buffer>, size: (backup-len + buf.size));
  if (backup)
    copy-bytes(new-buf, 0, backup, 0, backup-len);
  end;
  copy-bytes(new-buf, backup-len, buf, 0, stop);
  stream.buffer := new-buf;
  stream.buffer-stop := (backup-len + stop);
  stream.string-output-stream-backup := #f;
end method;

define method stream-size (stream :: <byte-string-output-stream>)
    => size :: <integer>;
  // Get the output buffer to make sure the stream is not already in use.
  let (buf :: <buffer>, next :: <buffer-index>) = get-output-buffer(stream);
  ignore(buf);
  let backup :: false-or(<byte-string>) = stream.string-output-stream-backup;
  release-output-buffer(stream, next);
  if (backup)
    backup.size + stream.buffer-stop;
  else
    stream.buffer-stop;
  end;
end method;



//// Buffer Protocol.
////

/// The <buffer> class as <vector> is implemented in the System module of
/// the Dylan library.
///

define generic buffer-subsequence
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


/// copy-from-buffer! -- Exported.
///
define generic copy-from-buffer!
    (destination :: <sequence>, buf :: <buffer>, buf-start :: <buffer-index>,
     #key start :: <integer>, // = 0
          end: dest-end :: <integer>) // = destination.size)
    => ();

define sealed method copy-from-buffer!
    (destination :: type-union(<byte-string>, <byte-vector>, <buffer>),
     buf :: <buffer>,
     buf-start :: <buffer-index>,
     #key start :: <integer> = 0,
          end: stop :: <integer> = destination.size)
    => ();
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
end method;



/// copy-into-buffer! -- Exported.
///
define generic copy-into-buffer!
    (source :: <sequence>, buf :: <buffer>, buf-start :: <buffer-index>,
     #key start :: <integer>, // = 0,
          end: src-end :: <integer>) // = source.size)
    => ();

define sealed method copy-into-buffer!
    (source :: type-union(<byte-string>, <byte-vector>, <buffer>),
     buf :: <buffer>, buf-start :: <buffer-index>,
     #key start :: <integer> = 0,
          end: stop :: <integer> = source.size)
    => ();
  // Do lots of bounds checking.
  if (start < 0)
    error("Bounds error in source -- %d.", start);
  end;
  if (stop > source.size)
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
end method;
