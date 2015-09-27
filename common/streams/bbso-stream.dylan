module: Streams
author: Ben Folk-Williams, adapted from code by Bill Chiles
synopsis: <buffered-byte-string-output-stream> -- used inside d2c.
copyright: see below

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
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

/// <buffered-byte-string-output-stream> -- Exported.
/// This class provides an efficient output mechanism. Note that it does
/// not subclass <sequence-stream> nor any of that part of the heirarchy, so it
/// is (counter-intuitively) NOT a <string-stream>. This is to make it
/// easier to maintain efficiency.
///
define class <buffered-byte-string-output-stream>
    (<buffered-stream>, <positionable-stream>)
  slot buffer :: false-or(<buffer>) = make(<buffer>, 
					   size: $default-buffer-size,
					   end: $default-buffer-size);
  slot string-output-stream-backup :: false-or(<byte-string>) = #f;
  //
  // This slot holds the end of the output held in the buffer.  Because of the
  // Positionable Stream Protocol buffer-next may not be at the end of all
  // the output written. This is different from the buffer-end slot of the
  // buffer, which indicates the end of the space available for writing.
  slot buffer-stop :: <buffer-index> = 0;
end class;

define sealed domain make (singleton(<buffered-byte-string-output-stream>));
define sealed domain initialize(<buffered-byte-string-output-stream>);

//// Querying
//// Methods for exported interface.
////

/// stream-open?
///
define inline sealed method stream-open? 
    (stream :: <buffered-byte-string-output-stream>)
 => open? :: <boolean>;
  if (stream.buffer) #t else #f end;
end method stream-open?;

/// stream-element-type
///
define inline sealed method stream-element-type
    (stream :: <buffered-byte-string-output-stream>)
 => type :: <type>; // ### :: singleton(<byte-character>);
    <byte-character>;
end method stream-element-type;

/// stream-at-end?
///
define inline sealed method stream-at-end?
    (stream :: <buffered-byte-string-output-stream>)
 => at-end? :: <boolean>;
  #f;
end method stream-at-end?;

//// Stream Extension Protocol
//// Methods for exported interface.
////

/// do-get-output-buffer
///
/// This must not return a full buffer.  When the buffer is full, this
/// creates a backup store using a <byte-string>.  If there is already a
/// backup string, then this function creates a new one to hold all the
/// previously backed up output and what is in the buffer.
///
define inline sealed method do-get-output-buffer
    (stream :: <buffered-byte-string-output-stream>,
     #key bytes :: <integer> = 1)
 => buf :: <buffer>;
  do-next-output-buffer(stream, bytes: bytes); // They're the same
end method;

/// do-release-output-buffer
///
define inline sealed method do-release-output-buffer
    (stream :: <buffered-byte-string-output-stream>) 
 => ();
  // Maintain buffer-stop
  let next :: <buffer-index> = stream.buffer.buffer-next;
  if (stream.buffer-stop < next)
    stream.buffer-stop := next;
  end;
end method;

/// do-next-output-buffer
///
define sealed method do-next-output-buffer
    (stream :: <buffered-byte-string-output-stream>,
     #key bytes :: <integer> = 1)
 => buf :: <buffer>;
  let buf :: <buffer> = stream.buffer;
  if (bytes > buf.size) 
    error("Stream's buffer is not large enough to get %d bytes -- %=",
	  bytes, stream);
  end;
  buf.buffer-end := buf.size; // It should be that anyway, but we need to
                              // be sure
  let buf-next :: <buffer-index> = buf.buffer-next;
  // Maintain buffer-stop
  if (stream.buffer-stop < buf-next)
    stream.buffer-stop := buf-next;
  end;
  let stop :: <buffer-index> = stream.buffer-stop;
  // Test buf-next rather that buffer-stop.  Though buffer-stop may indicate
  // the buffer is full, there's no reason to back up the buffer when the
  // buf-next says the user isn't writing off the end of the buffer.
  if (bytes > (buf.size - buf-next))
    // Can't write further in the buffer.
    let backup :: false-or(<byte-string>) = stream.string-output-stream-backup;
    if (backup)
      // Concatenate the backup and buffer to form new backup string.
      let backup-len :: <integer> = backup.size;
      let new-backup-len = backup-len + stop;
      let res :: <byte-string> = make(<byte-string>, size: new-backup-len);
      copy-bytes(res, 0, backup, 0, backup-len);
      copy-bytes(res, backup-len, buf, 0, stop);
      stream.string-output-stream-backup := res;
    else
      // Just copy the buffer into a backup string.
      stream.string-output-stream-backup :=
        buffer-subsequence(buf, <byte-string>, 0, stop);
    end;
    // Make sure buffer-stop is maintained correctly, and we move any output
    // remaining in the buffer to the beginning of the buffer.  This ensure
    // the output is correctly placed to be overwritten.
    if (stop > buf-next)
      let new-stop :: <buffer-index> = (stop - buf-next);
      copy-bytes(buf, 0, buf, buf-next, new-stop);
      stream.buffer-stop := new-stop;
    else
      stream.buffer-stop := 0;
    end;
    buf.buffer-next := 0;
    buf;
  else
    // Just return the buffer, nothing special to do.
    buf;
  end;
end method;

/// do-force-output-buffers
/// This just pushes everything in the buffer into the backup.
/// Not really the true intent of this function (which is meaningless in
/// this context), but people might be expecting that the buffer will
/// be empty after calling this.
/// Perhaps that assumption would just be wrong, and this function should
/// do nothing.
///
define sealed method do-force-output-buffers
    (stream :: <buffered-byte-string-output-stream>) 
 => ();
  let buf :: <buffer> = stream.buffer;
  let stop :: <integer> = stream.buffer-stop;
  let backup :: false-or(<byte-string>) = stream.string-output-stream-backup;
  if (backup)
    // Concatenate the backup and buffer to form new backup string.
    let backup-len :: <integer> = backup.size;
    let new-backup-len = backup-len + stop;
    let res :: <byte-string> = make(<byte-string>, size: new-backup-len);
    copy-bytes(res, 0, backup, 0, backup-len);
    copy-bytes(res, backup-len, buf, 0, stop);
    stream.string-output-stream-backup := res;
  else
    // Just copy the buffer into a backup string.
    stream.string-output-stream-backup :=
      buffer-subsequence(buf, <byte-string>, 0, stop);
  end;
  // Make sure buffer-stop is maintained correctly, and we move any output
  // remaining in the buffer to the beginning of the buffer.  This ensure
  // the output is correctly placed to be overwritten.
  let buf-next :: <buffer-index> = buf.buffer-next;
  if (stop > buf-next)
    let new-stop :: <buffer-index> = (stop - buf-next);
    copy-bytes(buf, 0, buf, buf-next, new-stop);
    stream.buffer-stop := new-stop;
  else
    stream.buffer-stop := 0;
  end;
  buf.buffer-next := 0;
end method;

/// do-synchronize
///
define inline sealed method do-synchronize
    (stream :: <buffered-byte-string-output-stream>) 
 => ();
end method;

/// close
///
define sealed method close (stream :: <buffered-byte-string-output-stream>,
			    #key, #all-keys) 
 => ();
  // Get the buffer to make sure no one is using it.
  get-output-buffer(stream, bytes: 0);
  release-output-buffer(stream);
  stream.buffer := #f;
end method;

//// Positionable Stream Protocol
//// Methods for exported interface.
////

/// stream-position
///
define method stream-position (stream :: <buffered-byte-string-output-stream>)
 => position :: <integer>;
  // Get the output buffer to make sure the stream is not already in use.
  let buf :: <buffer> = get-output-buffer(stream, bytes: 0);
  let backup :: false-or(<byte-string>) = stream.string-output-stream-backup;
  release-output-buffer(stream);
  if (backup)
    backup.size + buf.buffer-next;
  else
    buf.buffer-next;
  end;
end method;

/// stream-position-setter
///
define method stream-position-setter
    (position :: type-union(one-of(#"start", #"end"), <integer>),
     stream :: <buffered-byte-string-output-stream>)
 => position :: <integer>;
  // Get the output buffer to make sure the stream is not already in use.
  let buf :: <buffer> = get-output-buffer(stream, bytes: 0);
  let backup :: false-or(<byte-string>) = stream.string-output-stream-backup;
  let backup-len :: <integer> = if (backup) backup.size else 0 end;
  let stream-len :: <integer> = backup-len + buf.size;
  if (position == #"start")
    position := 0;
  elseif (position == #"end")
    position := stream-len;
  elseif ((position < 0) | (position > stream-len))
    error("Illegal stream position -- %d.", position);
  end;
  if (position >= backup-len)
    // Reposition within the existing buffer.
    buf.buffer-next := position - backup-len;
  else
    new-string-output-stream-backup(stream, buf, stream.buffer-stop, 
				    backup, backup-len);
    buf.buffer-next := position;
  end;
  let next = buf.buffer-next;
  if (next > stream.buffer-stop)
    stream.buffer-stop := next;
  end;
  if (next > buf.buffer-end)
    buf.buffer-end := next;
  end;
  release-output-buffer(stream);
  position;
end method;

/// adjust-stream-position
///
define method adjust-stream-position
    (stream :: <buffered-byte-string-output-stream>,
     delta :: <integer>,
     #key from: reference :: one-of(#"start", #"current", #"end") = #"current")
    => position :: <integer>;
  // Get the output buffer to make sure the stream is not already in use.
  let buf :: <buffer> = get-output-buffer(stream, bytes: 0);
  let buf-next :: <buffer-index> = buf.buffer-next;
  let stop :: <buffer-index> = stream.buffer-stop;
  let backup :: false-or(<byte-string>) = stream.string-output-stream-backup;
  let backup-len :: <integer> = if (backup) backup.size else 0 end;
  let stream-len :: <integer> = backup-len + stop;
  let position = select (reference)
		   (#"start") => delta;
		   (#"current") => (buf-next + delta);
		   (#"end") => (stream-len + delta);
		 end;
  case
    (position < 0) =>
      error("Illegal stream position -- %d.", position);
    ((position >= backup-len) & (position <= stream-len)) =>
      buf.buffer-next := position - backup-len;
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
      buf.buffer-next := 0;
      buf.buffer-end := buf.size;
    otherwise =>
      new-string-output-stream-backup(stream, buf, stop, backup, backup-len);
      buf.buffer-next := position;
  end;
  release-output-buffer(stream);
  position;
end method;

/// stream-size
///
define sealed method stream-size 
    (stream :: <buffered-byte-string-output-stream>)
 => size :: <integer>;
  let buf :: <buffer> = get-output-buffer(stream, bytes: 0);
  let backup :: false-or(<byte-string>) = stream.string-output-stream-backup;
  release-output-buffer(stream);
  if (backup)
    backup.size + stream.buffer-stop;
  else
    stream.buffer-stop;
  end;
end method;

/// stream-contents
///
define sealed method stream-contents
    (stream :: <buffered-byte-string-output-stream>,
     #key clear-contents? :: <boolean> = #t)
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
	(output-len == 0) =>
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
  if (clear-contents?)
    stream.string-output-stream-backup := #f;
    buf.buffer-next := 0;
    buf.buffer-end := buf.size;
    stream.buffer-stop := 0;
  end;
  release-output-buffer(stream);
  string;
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
/// better implementation of <buffered-byte-string-output-stream>s.
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
