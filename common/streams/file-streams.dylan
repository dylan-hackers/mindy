module: Streams
author: Bill Chiles, Ben Folk-Williams
synopsis: This file implements <file-streams> for the Streams library
copyright: See below.
rcs-header: $Header: /scm/cvs/src/common/streams/file-streams.dylan,v 1.1 1998/05/03 19:55:04 andreas Exp $

//======================================================================
//
// Copyright (c) 1994, 1996  Carnegie Mellon University
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

#if (~mindy & (compiled-for-win32 | compiled-for-solaris))
// We need this because when fd-open is inlined into this file, the
// identifier O_BINARY will be undeclared in the C code
method () => ();
  c-include("fcntl.h");
  c-include("errno.h");
end();
#endif

//// Fd Streams -- class definition and querying.
////

/// <fd-stream> Class -- Exported.
///
/// All file descriptor based streams inherit from this class.
///
/// This is a non-standard class defined for Gwydion streams.  This stream
/// and <file-stream> are the superclasses of <fd-file-stream>s.
///

define class <fd-stream> (<buffered-stream>)
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
  // The buffer slot is set to #f when the stream is closed.
  slot buffer :: false-or(<buffer>);
end class;

define sealed domain make (singleton(<fd-stream>));

define sealed method close (stream :: <fd-stream>, #all-keys) => ();
  if (stream.fd-direction == #"input")
    // Get buffer to make sure no one holds it.
    get-input-buffer(stream, wait?: #f);
    call-fd-function(fd-close, stream.file-descriptor);
    stream.buffer := #f;
    release-input-buffer(stream);
  else
    let buf :: <buffer> = get-output-buffer(stream);
    if (buf.buffer-next ~== 0)
      force-output-buffers(stream);
    end;
    synchronize(stream);
    call-fd-function(fd-close, stream.file-descriptor);
    stream.buffer := #f;
    unregister-output-stream(stream);
    release-output-buffer(stream);
  end;
end method close;
  
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
    // Next and end are the same so that the first read will fill the buffer.
    stream.buffer.buffer-next := (stream.buffer.buffer-end := 0);
  else
    register-output-stream(stream);
    stream.buffer.buffer-next := 0;
    stream.buffer.buffer-end := stream.buffer.size;
  end;
  stream;
end method initialize;

define inline sealed method stream-open? (stream :: <fd-stream>) 
 => open? :: <boolean>;
  if (stream.buffer) #t else #f end;
end method;

#if (mindy)
define inline sealed method stream-element-type (stream :: <fd-stream>)
 => type :: singleton(<byte-character>);
  <byte-character>;
end method;
#else
// Until d2c can deal with singleton
define inline sealed method stream-element-type (stream :: <fd-stream>)
 => type :: <type>;
  <byte-character>;
end method;
#endif

define sealed method stream-at-end? (stream :: <fd-stream>)
 => at-end? :: <boolean>;
  if (stream.fd-direction == #"input")
    let res :: <boolean> = (~ get-input-buffer(stream));
    release-input-buffer(stream);
    res;
  else
    #f;
  end if;
end method;

//// Fd Streams -- Stream Extension Protocol.
////

define sealed method do-get-input-buffer 
    (stream :: <fd-stream>,
     #key wait? :: <boolean> = #t,
          bytes :: false-or(<integer>))
 => buffer :: false-or(<buffer>);
  fill-input-buffer(stream, wait?, bytes);
end method do-get-input-buffer;

define sealed method do-release-input-buffer (stream :: <fd-stream>);
  if (direction == #"output")
    error("Stream is an output stream -- %=.", stream);
  end;
end method;

define sealed method do-next-input-buffer
    (stream :: <fd-stream>,
     #key wait? :: <boolean> = #t, 
          bytes :: false-or(<integer>))
 => buffer :: false-or(<buffer>);
  fill-input-buffer(stream, wait?, bytes);
end method do-next-input-buffer;


define function fill-input-buffer
    (stream :: <fd-stream>, wait? :: <boolean>, bytes :: false-or(<integer>))
    => buffer :: false-or(<buffer>);
  let direction = stream.fd-direction;
  if (direction == #"output")
    error("Stream is an output stream -- %=.", stream);
  end;
  let buf :: false-or(<buffer>) = stream.buffer;
  // Since buffer is currently unheld by anyone, make sure it isn't closed.
  if (~ buf) error("Stream has been closed -- %=.", stream) end;
  let avail :: <buffer-index> = buf.buffer-end - buf.buffer-next;
  if (bytes)
    if (avail < bytes)
      // Fill input buffer
      if (buf.buffer-next == buf.buffer-end)
	// We don't have to worry about loosing valid input.
	buf.buffer-next := (buf.buffer-end := 0);
      elseif (buf.buffer-next > 0 & buf.size - buf.buffer-end < bytes)
	// Move the currently valid data to the start of the buffer.
	copy-bytes(buf, 0, buf, buf.buffer-next, avail);
	buf.buffer-next := 0;
	buf.buffer-end := avail;
      end if;
      let count	= call-fd-function(fd-read, stream.file-descriptor, buf,
				   buf.buffer-end,
				   buf.size - buf.buffer-end);
      let max-avail = avail + count;
      buf.buffer-end := buf.buffer-end + count;
      if (max-avail < bytes)
	// Still not enough!
	let inc-seq = make(<buffer>, size: max-avail);
	copy-bytes(inc-seq, 0, buf, buf.buffer-next, max-avail);
	buf.buffer-next := buf.buffer-end;
	error(make(<incomplete-read-error>, stream: stream,
		   sequence: inc-seq, count: bytes));
      end if;
    end if;
    buf;
  elseif (wait?)
    if (buf.buffer-next == buf.buffer-end)
      // There's no input at all, might as well fill as much as we can
      let count = call-fd-function(fd-read, stream.file-descriptor, buf,
				   0, buf.size);
      buf.buffer-next := 0;
      buf.buffer-end := count;
      if (count == 0) #f else buf end;
    else
      buf;
    end if;
  else
    buf;
  end if;
end function fill-input-buffer;

define inline sealed method do-input-available-at-source?
    (stream :: <fd-stream>)
 => input-available? :: <boolean>;
  if (stream.fd-direction == #"output")
    error("Stream is an output stream -- %=.", stream);
  end;
  call-fd-function(fd-input-available?, stream.file-descriptor);
end method do-input-available-at-source?;

define sealed method do-get-output-buffer
    (stream :: <fd-stream>, #key bytes :: <integer> = 1) 
 => buffer :: <buffer>;
  let direction = stream.fd-direction;
  if (direction == #"input")
    error("Stream is an input stream -- %=.", stream);
  end;
  let buf :: <buffer> = stream.buffer;
  // Since no one holds the buffer, make sure the stream isn't closed.
  unless (buf) error("Stream has been closed -- %=.", stream) end;
  let next :: <buffer-index> = buf.buffer-next;
  if (buf.size < bytes)
    // No way we can get that much space
    error("Can't get %d bytes for writing to stream -- %=", bytes, stream);
  elseif ((buf.buffer-end - next) < bytes)
    if (next > 0)
      // Keep writing until fd-write manages to write everything.
      let fd = stream.file-descriptor;
      for (x :: <buffer-index>
	     = call-fd-function(fd-write, fd, buf, 0, next)
	     then (x + call-fd-function(fd-write, fd, buf, x, next - x)),
	   until: (x = next))
      end;
    end;
    buf.buffer-next := 0;
    buf.buffer-end := buf.size;
  end if;
  buf;
end method do-get-output-buffer;

define inline sealed method do-release-output-buffer (stream :: <fd-stream>)
 => ();
  if (stream.fd-direction == #"input")
    error("Stream is an input stream -- %=.", stream);
  end;
end method do-release-output-buffer;

define sealed method do-next-output-buffer
    (stream :: <fd-stream>, #key bytes :: <integer> = 1)
  => buffer :: <buffer>;
  if (stream.fd-direction == #"input")
    error("Stream is an input stream -- %=.", stream);
  end;
  let buf :: <buffer> = stream.buffer;
  // Since no one holds the buffer, make sure the stream isn't closed.
  unless (buf) error("Stream has been closed -- %=.", stream) end;
  let next :: <buffer-index> = buf.buffer-next;
  if (buf.size < bytes)
    // No way we can get that much space
    error("Can't get %d bytes to write to stream -- %=", bytes, stream);
  elseif ((buf.buffer-end - next) < bytes)  
    if (next > 0)
      // Keep writing until fd-write manages to write everything.
      let fd = stream.file-descriptor;
      for (x :: <buffer-index>
	     = call-fd-function(fd-write, fd, buf, 0, next)
	     then (x + call-fd-function(fd-write, fd, buf, x, next - x)),
	   until: (x = next))
      end for;
    end if;
    buf.buffer-next := 0;
    buf.buffer-end := buf.size;
  end if;
  buf;
end method do-next-output-buffer;

define sealed method do-force-output-buffers (stream :: <fd-stream>) => ();
  if (stream.fd-direction == #"input")
    error("Stream is an input stream -- %=.", stream);
  end;
  let buf :: <buffer> = stream.buffer;
  let next :: <buffer-index> = buf.buffer-next;
  if (next > 0)
    // Keep writing until fd-write manages to write everything.
    let fd = stream.file-descriptor;
    for (x :: <buffer-index>
	   = call-fd-function(fd-write, fd, buf, 0, next)
	   then (x + call-fd-function(fd-write, fd, buf, x, next - x)),
	 until: (x = next))
    end;
  end;
  buf.buffer-next := 0;
  buf.buffer-end := buf.size;
end method do-force-output-buffers;

define sealed method do-synchronize (stream :: <fd-stream>) => ();
  if (stream.fd-direction == #"input")
    error("Stream is an input stream -- %=.", stream);
  end;
  call-fd-function(fd-sync-output, stream.file-descriptor);
end method do-synchronize;


//// Fd File Streams -- class declarations and Positionable Stream Protocol.
////

/// file-buffer-last-use -- Internal.
/// file-buffer-last-use-setter -- Internal.
///
/// These are defined for readability.
///
define constant file-buffer-last-use = fd-direction;
define constant file-buffer-last-use-setter = fd-direction-setter;

/// <fd-file-stream> Class -- Internal.
///
/// This is the concrete class that is instantiated when users make a
/// <file-stream>.
///
#if (mindy)
define sealed class <fd-file-stream> (<fd-stream>, <file-stream>)
  slot file-name :: <byte-string>;
  slot file-direction :: one-of(#"input", #"output", #"input-output");
  slot element-type :: one-of(<byte>, <byte-character>, <unicode-character>)
    = <byte-character>;
end class;
#else
// The compiler can't deal with singleton(<byte>)
define sealed class <fd-file-stream> (<fd-stream>, <file-stream>)
  slot file-name :: <byte-string>;
  slot file-direction;// :: one-of(#"input", #"output", #"input-output");
  slot element-type = <byte-character>;
end class;
#endif

define sealed domain make (singleton(<fd-file-stream>));


//// Fd File Streams -- Querying and Stream Extension Protocol.
////

/// The following methods from <fd-streams> work:
///    stream-open?
///    close
///    do-synchronize
///

/// Types used in initialize -- Internal.
///
define constant <file-direction> 
  = one-of(#"input", #"output", #"input-output");
define constant <if-exists-action>
  = one-of(#f, #"new-version", #"overwrite", #"replace",
	   #"append", #"truncate", #"signal");
define constant <if-does-not-exist-action>
  = one-of(#f, #"signal", #"create");
// ### d2c can't deal with singleton(<byte>)
// define constant <file-element-type>
//   = one-of(<byte>, <byte-character>, <unicode-character>);

/// initialize
///
define sealed method initialize
    (stream :: <fd-file-stream>, #next next-method, #rest rest-args,
     #key locator :: false-or(<byte-string>),
          direction :: <file-direction> = #"input",
          if-exists :: <if-exists-action>
            = if (direction == #"output") #"replace" else #f end,
          if-does-not-exist :: <if-does-not-exist-action>
            = if (direction == #"input") #"signal" else #"create" end,
          element-type // :: <file-element-type>
            = <byte-character>,
          encoding :: one-of(#f, #"ANSI", #"big-endian")
            = select (element-type) 
		(<byte>) => #f;
		(<byte-character>) => #"ANSI";
		(<unicode-character>) => #"big-endian"; // ?
		otherwise => error("Unknown element-type");
	      end,
          buffer-size :: <buffer-index> = $default-buffer-size)
    => result :: <fd-file-stream>;
  block (exit)
    if (~ locator)
      error("Must supply a filename when making a <file-stream>.");
    end;
    stream.element-type := element-type;
    if (direction == #"input")
      // Make an #"input" stream.
      let (fd, err) = fd-open(locator, fd-o_rdonly);
      select (err)
	(#f)
	  => #f; // No error, don't do anything.
	(fd-enoent)
	  => select (if-does-not-exist) 
	       (#f)
		 => exit();
	       (#"signal")
		 => error(make(<file-does-not-exist-error>, locator: locator));
	       (#"create")
		 => error("Why would you want to create an empty input file?");
	     end select;
	(fd-eacces)
	  => error(make(<invalid-file-permissions-error>, locator: locator));
	otherwise
	  => error(make(<syscall-error>, errno: err));
      end select;
      stream.file-name := locator;
      stream.file-direction := #"input";
      next-method(stream, fd: fd, direction: #"input", size: buffer-size); 
    else
      // Make an #"output" or #"input-output" stream.
      let flags :: <integer>
	= select (if-does-not-exist)
	    (#f) => exit();
	    (#"create") => fd-o_creat;
	    (#"signal") => 0;
	  end select;
      flags := select (direction)
		 (#"output") => logior(flags, fd-o_wronly);
		 (#"input-output") => logior(flags, fd-o_rdwr);
	       end;
      flags := select (if-exists)
		 (#"signal")
		   => logior(flags, fd-o_excl);
		 (#"replace")
		   => logior(flags, fd-o_trunc);
		 (#"truncate")
		   => logior(flags, fd-o_trunc);
		 (#"new-version") 
		   => error("<fd-file-stream> does not support new-version.");
		 otherwise
		   => flags;
	       end;
      let (fd, err) = fd-open(locator, flags);
      select (err)
	(#f)
	  => #f; // No error, don't do anything.
	(fd-eexist)
	  => error(make(<file-exists-error>, locator: locator));
	(fd-eacces)
	  => error(make(<invalid-file-permissions-error>, locator: locator));
	(fd-enoent)
	  => error(make(<file-does-not-exist-error>, locator: locator));
	otherwise
	  => error(make(<syscall-error>, errno: err));
      end select;
      select (if-exists)
	(#"append")
	  => call-fd-function(fd-seek, fd, 0, fd-seek-end);
	// Don't really need this, since it will be at the beginning anyway...
	// (#"overwrite")
	//  => call-fd-function(fd-seek, fd, 0, fd-seek-set);
	otherwise
	  => #f;
      end;
      stream.file-name := locator;
      stream.file-direction := direction;
      next-method(stream, fd: fd,
		  direction: if (direction == #"output")
			       #"output" 
			     else 
			       #"input" 
			     end,
		  size: buffer-size);
      register-output-stream(stream);
    end;
  end block;
end method;

define sealed method close (stream :: <fd-file-stream>, #next next-method, 
			    #all-keys)
 => ();
  next-method();
  if ((stream.file-direction == #"input-output")
	& (stream.file-buffer-last-use == #"input"))
    unregister-output-stream(stream);
  end;
end method;

/// ### When d2c can deal with singleton(<byte>), this method's return type
/// should be <file-element-type>
///
define inline sealed method stream-element-type (stream :: <fd-file-stream>)
 => type :: <type>;
  stream.element-type;
end method;

define sealed method stream-at-end? (stream :: <fd-file-stream>,
				     #next next-method)
 => at-end? :: <boolean>;
  // If the stream is input-output, we want to call next method with
  // it as and input stream, since next-method always returns #f for
  // output streams.
  //
  if  ((stream.file-direction == #"input-output")
	 & (stream.file-buffer-last-use == #"output"))
    let buf :: <buffer> = get-output-buffer(stream);
    let next :: <buffer-index> = buf.buffer-next;
    if (next > 0)
      // Keep writing until fd-write manages to write everything.
      let fd = stream.file-descriptor;
      for (x :: <buffer-index>
	     = call-fd-function(fd-write, fd, buf, 0, next)
	     then (x + call-fd-function(fd-write, fd, buf, x, next - x)),
	   until: (x = next))
      end;
    end;
    buf.buffer-next := buf.buffer-end;
    release-output-buffer(stream);
    stream.file-buffer-last-use := #"input";
  end if;
  next-method();
end method;


//// Fd File Streams -- Positionable Stream Protocol.
////

/// stream-position -- Method for Exported Interface.
///
define sealed method stream-position (stream :: <fd-file-stream>)
 => position :: <integer>;
  if (stream.file-direction == #"input")
    // Get the buffer to ensure no one else is using it and to make it
    // possible to correctly compute the actual file position.
    let buf :: <buffer> = get-input-buffer(stream, wait?: #f);
    // Get the current position as recorded by the file-descritor module
    // and subtract what input we have in the buffer but haven't actually
    // read.
    let pos = call-fd-function(fd-seek, stream.file-descriptor, 0,
			       fd-seek-current)
      - (buf.buffer-end - buf.buffer-next);
    release-input-buffer(stream);
    pos;
  else
    // Direction is #"output" or #"input-output".
    // Get the buffer to ensure no one else is using it and to make it
    // possible to correctly compute the actual file position.
    let buf :: <buffer> = get-output-buffer(stream);
    // Get the current position as recorded by the file-descritor module
    // and add what output we have in the buffer but haven't sent yet.
    let pos = call-fd-function(fd-seek, stream.file-descriptor, 0,
			       fd-seek-current) + buf.buffer-next;
    release-output-buffer(stream);
    pos;
  end;
end method;

/// stream-position-setter -- Method for Exported Interface.
///
define sealed method stream-position-setter
    (position :: type-union(<integer>, one-of(#"start", #"end")),
     stream :: <fd-file-stream>)
 => position :: <integer>;
  let direction = file-direction(stream);
  // Get the buffer to ensure no one else is using it and to make it
  // possible to invalidate the buffer's contents.
  if (direction == #"input")
    let buf :: <buffer> = get-input-buffer(stream, wait?: #f);
    buf.buffer-next := buf.buffer-end;
  else
    let buf :: <buffer> = get-output-buffer(stream);
    // Force any pending output so that we can later correctly test for the
    // file's size.
    if (buf.buffer-next > 0)
      force-output-buffers(stream);
    end;
  end;
  // Set the position.
  let fd = stream.file-descriptor;
  let fd-end = call-fd-function(fd-seek, fd, 0, fd-seek-end);
  if (position == #"start") 
    position := 0;
  elseif (position == #"end")
    position := fd-end;
  end if;
  if ((position >= 0) & (position <= fd-end))
    call-fd-function(fd-seek, fd, position, fd-seek-set);
  else
    error("Illegal stream position -- %d", position);
  end;
  // Cleanup.
  if (direction == #"input")
    release-input-buffer(stream);
  else
    release-output-buffer(stream);
  end;
  position;
end method;

/// adjust-stream-position -- Method for Exported Interface.
///
define sealed method adjust-stream-position
    (stream :: <fd-file-stream>, delta :: <integer>, 
     #key from :: one-of(#"start", #"current", #"end") = #"current")
    => position :: <integer>;
  if (stream.file-buffer-last-use == #"input")
    // Get the buffer to ensure no one else is using it and to make it
    // possible to invalidate the buffer's contents.
    let buf :: <buffer> = get-input-buffer(stream, wait?: #f);
    buf.buffer-next := buf.buffer-end;
    let pos = call-fd-function(fd-seek, stream.file-descriptor, delta,
			       select (from)
				 (#"start") => fd-seek-set;
				 (#"current") => fd-seek-current;
				 (#"end") => fd-seek-end;
			       end);
    release-input-buffer(stream);
    if (from == #"current")
      // If moving the position relative to the current position, then
      // adjust the offset to account for the unread input in the buffer.
      // Because of the unread input, the file-descriptor module's record
      // of the position is ahead of the actual position.
      pos - (buf.buffer-end - buf.buffer-next);
    else
      pos;
    end;
  else
    // Get the buffer to ensure no one else is using it and to make it
    // possible to invalidate the buffer's contents.
    let buf :: <buffer> = get-output-buffer(stream);
    // Force out any pending output while the file position is still right
    // for the file to receive this output.
    if (buf.buffer-next > 0)
      force-output-buffers(stream);
    end;
    let pos = call-fd-function(fd-seek, stream.file-descriptor, delta,
			       select (from)
				 (#"start") => fd-seek-set;
				 (#"current") => fd-seek-current;
				 (#"end") => fd-seek-end;
			       end);
    release-output-buffer(stream);
    pos;
  end;
end method;

/// stream-size -- Method for Exported Interface.
///
define sealed method stream-size (stream :: <fd-file-stream>)
    => size :: <integer>;
  if (stream.file-buffer-last-use == #"input")
    // Get the buffer to ensure no one else is using it and to make it
    // possible to correctly compute the actual file position.
    let buf :: <buffer> = get-input-buffer(stream, wait?: #f);
    let pos = call-fd-function(fd-seek, stream.file-descriptor, 0,
			       fd-seek-current);
    let size = call-fd-function(fd-seek, stream.file-descriptor, 0,
				fd-seek-end);
    call-fd-function(fd-seek, stream.file-descriptor, pos, fd-seek-set);
    release-input-buffer(stream);
    size;
  else
    // Direction is #"output" or #"input-output".
    // Get the buffer to ensure no one else is using it and to make it
    // possible to correctly compute the actual file position and size.
    let buf :: <buffer> = get-output-buffer(stream);
    // Force any pending output so that we can later correctly test for the
    // file's size.  We don't know if the current pending output is
    // overwriting part of the file or extending its length.
    if (buf.buffer-next > 0)
      force-output-buffers(stream);
    end;
    let pos = call-fd-function(fd-seek, stream.file-descriptor, 0,
			       fd-seek-current);
    let size = call-fd-function(fd-seek, stream.file-descriptor, 0,
				fd-seek-end);
    call-fd-function(fd-seek, stream.file-descriptor, pos, fd-seek-set);
    release-output-buffer(stream);
    size;
  end;
end method;

define sealed method stream-contents (stream :: <fd-file-stream>,
				      #key clear-contents? :: <boolean> = #t)
 => seq :: <sequence>;
  let fd :: <integer> = stream.file-descriptor;
  if (stream.file-direction == #"input")
    error("Stream is an input stream -- %=", stream);
  elseif (stream.file-direction == #"input-output")
    let buf :: <buffer> = get-input-buffer(stream, wait?: #f);
    let pos :: <integer> = call-fd-function(fd-seek, fd, 0, fd-seek-current);
    let size :: <integer> = call-fd-function(fd-seek, fd, 0, fd-seek-end);
    call-fd-function(fd-seek, fd, 0, fd-seek-set);
    // fd-read only works with buffers. Rather than hack that,
    // I'm just using an intermediate buffer. Could gain some
    // efficiency by writing a fd-read for <byte-string>, <byte-vector>,
    // and <unicode-string>.
    //
    let res :: <buffer> = make(<buffer>, size: size);
    call-fd-function(fd-read, fd, res, 0, size);
    if (clear-contents?)
      // Zero file by closing and then reopening a file of the same name,
      // truncating the old contents.
      call-fd-function(fd-close, fd);
      let (new-fd, err) = fd-open(stream.file-name, 
				  logior(fd-o_rdwr, fd-o_trunc));
      if (err) error(make(<syscall-error>, errno: err)) end;
      stream.file-descriptor := new-fd;
      buf.buffer-next := buf.buffer-end; // Invalidate buffer.
    else
      // Re-set postion to where it was.
      call-fd-function(fd-seek, fd, pos, fd-seek-set);
    end if;
    release-input-buffer(stream);
    as(type-for-sequence(stream.stream-element-type), res);
  else // It's an output only stream, we'll have to open it as an input file
    let buf :: <buffer> = get-output-buffer(stream);
    if (buf.buffer-next > 0)
      force-output-buffers(stream);
    end if;
    let temp-fd = fd-open(stream.file-name, fd-o_rdonly);
    let size :: <integer> = call-fd-function(fd-seek, temp-fd, 0, fd-seek-end);
    call-fd-function(fd-seek, temp-fd, 0, fd-seek-set);
    let res :: <buffer> = make(<buffer>, size: size);
    call-fd-function(fd-read, temp-fd, res, 0, size);
    call-fd-function(fd-close, temp-fd);
    if (clear-contents?)
      // Zero file by closing and then reopening a file of the same name,
      // truncating the old contents.
      call-fd-function(fd-close, fd);
      let (new-fd, err) = fd-open(stream.file-name, 
				  logior(fd-o_wronly, fd-o_trunc));
      if (err) error(make(<syscall-error>, errno: err)) end;
      stream.file-descriptor := new-fd;
      buf.buffer-next := 0;
    end if;
    release-output-buffer(stream);
    as(type-for-sequence(stream.stream-element-type), res);
  end if;
end method stream-contents;


//// Fd File Streams -- Stream Extension Protocol.
////

/// This method does not have to check whether the stream or buffer is locked
/// because get-input-buffer does that.
/// 
define sealed method do-get-input-buffer
    (stream :: <fd-file-stream>,
     #next next-method,
     #key wait? :: <boolean> = #t,
          bytes :: false-or(<integer>))
 => buffer :: false-or(<buffer>);
  let direction = stream.file-direction;
  if (direction == #"output")
    error("Stream is an output stream -- %=.", stream);
  end;
  let buf = stream.buffer;
  // Since buffer is currently unheld by anyone, make sure it isn't closed.
  unless (buf) error("Stream has been closed -- %=.", stream) end;
  if (stream.file-buffer-last-use == #"output")
    // The stream is both #"input-output" and was last used for #"output".
    let next :: <buffer-index> = buf.buffer-next;
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
    buf.buffer-next := buf.buffer-end;
  end if;
  next-method(); // Deal with bytes:, wait?: behavior and such
end method;

/// This method does not have to check whether the stream or buffer is locked
/// because release-input-buffer does that.
///
define sealed method do-release-input-buffer
    (stream :: <fd-file-stream>)
 => ();
  let direction = stream.file-direction;
  case (direction == #"output") =>
      error("Stream is an output stream -- %=.", stream);
    (~ ((direction == #"input") |
	(stream.file-buffer-last-use == #"input"))) =>
      error("Buffer is currently held for output -- %=.", stream);
  end;
end method;

/// This method does not call next-method because it would waste time doing
/// some tests again and then only execute a few statements.
///
define sealed method do-next-input-buffer
    (stream :: <fd-file-stream>,
     #next next-method,
     #key wait? :: <boolean> = #t, 
          bytes :: false-or(<integer>))
 =>  buffer :: false-or(<buffer>);
  let direction = stream.file-direction;
  if (direction == #"output")
    error("Stream is an output stream -- %=.", stream);
  end;
  let buf = stream.buffer;
  // Since buffer is currently unheld by anyone, make sure it isn't closed.
  unless (buf) error("Stream has been closed -- %=.", stream) end;
  if (stream.file-buffer-last-use == #"output")
    // The stream is both #"input-output" and was last used for #"output".
    let next :: <buffer-index> = buf.buffer-next;
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
    buf.buffer-next := buf.buffer-end;
  end if;
  next-method(); // Deal with bytes:, wait?: behavior and such
end method;

/// This method does not call next-method because it would waste time doing
/// some tests again and then only execute one line.
///
define sealed method do-input-available-at-source?
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

/// This method does not have to check whether the stream or buffer is locked
/// because get-output-buffer does that.
///
define sealed method do-get-output-buffer
    (stream :: <fd-file-stream>, 
     #next next-method,
     #key bytes :: <integer> = 1)
 => buffer :: <buffer>;
  let buf = stream.buffer;
  // Since buffer is currently unheld by anyone, make sure it isn't closed.
  unless (buf) error("Stream has been closed -- %=.", stream) end;
  if ((stream.file-direction == #"input-output")
	& (stream.file-buffer-last-use = #"input"))
    if (buf.buffer-end > buf.buffer-next)
      // Set the file position correctly relative to the actual reading done
      // on the stream so that when users force output, it goes to the right
      // location in the file.
      call-fd-function(fd-seek, stream.file-descriptor,
		       buf.buffer-next - buf.buffer-end,
		       fd-seek-current);
    end;
    buf.buffer-next := 0;
    buf.buffer-end := buf.size;
    stream.file-buffer-last-use := #"output";
  end if;
  next-method();
end method do-get-output-buffer;

/// This method does not have to check whether the stream or buffer is locked
/// because next-output-buffer does that.
///
define sealed method do-next-output-buffer
    (stream :: <fd-file-stream>, 
     #next next-method,
     #key bytes :: <integer> = 1)
 => buffer :: <buffer>;
  let buf = stream.buffer;
  // Since buffer is currently unheld by anyone, make sure it isn't closed.
  unless (buf) error("Stream has been closed -- %=.", stream) end;
  if ((stream.file-direction == #"input-output")
	& (stream.file-buffer-last-use = #"input"))
    if (buf.buffer-end > buf.buffer-next)
      // Set the file position correctly relative to the actual reading done
      // on the stream so that when users force output, it goes to the right
      // location in the file.
      call-fd-function(fd-seek, stream.file-descriptor,
		       buf.buffer-next - buf.buffer-end,
		       fd-seek-current);
    end;
    buf.buffer-next := 0;
    buf.buffer-end := buf.size;
    stream.file-buffer-last-use := #"output";
  end if;
  next-method();
end method do-next-output-buffer;


/// This method does not have to check whether the stream or buffer is locked
/// because release-output-buffer does that.
///
define sealed method do-release-output-buffer (stream :: <fd-file-stream>)
 => ();
  let direction = stream.file-direction;
  if (direction == #"input")
    error("Stream is an input stream -- %=.", stream);
  end;
  unless ((direction == #"output") 
	    | (stream.file-buffer-last-use == #"output"))
    error("Buffer is currently held for input -- %=.", stream);
  end;
end method;

define sealed method do-force-output-buffers (stream :: <fd-file-stream>,
					      #next next-method)
 => ();
  if ((stream.file-direction == #"input-output") &
      (stream.file-buffer-last-use == #"input"))
    error("Buffer last used for input -- %=.", stream);
  end;
  next-method();
end;


