module: cw-output-streams
file: cw-output-streams.dylan
author: gabor@mac.com
synopsis: <cw-output-stream> -- used inside d2c CW plugin.
RCS-header: $Header: /scm/cvs/src/d2c/compiler/Macintosh/cw-output-streams.dylan,v 1.1 2004/04/13 21:04:04 gabor Exp $
copyright: see below

//======================================================================
//
// Copyright (c) 2000, 2001, 2002  Gwydion Dylan Maintainers
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

define constant $default-buffer-size = 4096;	// -- Internal

/// <cw-output-stream> -- Internal.
/// This class provides an efficient output mechanism into a CWHandle.
/// It is modelled after <buffered-byte-string-output-stream>
/// Note that common stuff from do-next-output-buffer and do-force-output-buffers
/// has been factored out. This same technique should be done for bbso-stream too.
///	Also: in bbso-stream stream-element-type(...) => <byte-character>.singleton
//	stream.buffer := #f;
//	stream.release-output-buffer;
/// in bbso-stream close is probably a bug, because do-release-output-buffer
//	will access stream.buffer.buffer-next which failsеее
///
define abstract class <cw-output-stream>(<buffered-stream>, <positionable-stream>)
	slot buffer :: false-or(<buffer>)
					= make(<buffer>, 
								size: $default-buffer-size,
								end: $default-buffer-size);

	//
	// This slot holds the end of the output held in the buffer.  Because of the
	// Positionable Stream Protocol buffer-next may not be at the end of all
	// the output written. This is different from the buffer-end slot of the
	// buffer, which indicates the end of the space available for writing.
	slot buffer-stop :: <buffer-index> = 0;
	constant slot cw-output-stream-backup :: <cw-vector>, required-init-keyword: backup:;
	constant slot output-location :: <file-spec>, required-init-keyword: locator:;
	constant slot proposed-name :: <byte-string>.false-or = #f, init-keyword: name:;
end class;

//// Querying
//// Methods for exported interface.
////

/// stream-open?
///
define inline sealed method stream-open?(stream :: <cw-output-stream>) => open? :: <boolean>;
  stream.buffer & #t
end method stream-open?;

/// stream-element-type
///
define inline sealed method stream-element-type(stream :: <cw-output-stream>) => type :: <type>;
	// should be: => <byte-character>.singleton; еее
	// specifying this results in warning:
	//	can't tell if <unknown> is subtype of <type>
	// looks like an improvement opportunity.
	//	It seems to work for #f.singleton though.. see below.
    <byte-character>
end method stream-element-type;

/// stream-at-end?
///
define inline sealed method stream-at-end?(stream :: <cw-output-stream>) => at-end? :: #f.singleton;
  #f
end method stream-at-end?;

//// Stream Extension Protocol
//// Methods for exported interface.
////

/// do-get-output-buffer
///
/// This must not return a full buffer.  When the buffer is full, this
/// dump to the backup store into a <cw-vector>.
///
define inline sealed method do-get-output-buffer(stream :: <cw-output-stream>, #key bytes :: <integer> = 1) => buf :: <buffer>;
  do-next-output-buffer(stream, bytes: bytes); // They're the same
end method;

/// do-release-output-buffer
///
define inline sealed method do-release-output-buffer(stream :: <cw-output-stream>) => ();
  // Maintain buffer-stop
  let next :: <buffer-index> = stream.buffer.buffer-next;
  if (stream.buffer-stop < next)
    stream.buffer-stop := next;
  end;
end method;

define function backup-buffer(stream :: <cw-output-stream>, stop :: <buffer-index>) => updated :: <buffer>; // -- Internal

	let (backup :: <cw-vector>, buf :: <buffer>) = values(stream.cw-output-stream-backup, stream.buffer);

	// Resize backup to hold the flushed bytes from the buffer.
	let backup-len :: <integer> = backup.size;
	backup.size := backup-len + stop;

	call-out("memcpy", void:, ptr: backup.starting-address + backup-len, ptr: buf.buffer-address, int: stop);

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
	buf;
end function ;

/// do-next-output-buffer
///
define sealed method do-next-output-buffer(stream :: <cw-output-stream>, #key bytes :: <integer> = 1) => buf :: <buffer>;
  let buf :: <buffer> = stream.buffer;
  if (bytes > buf.size) 
    error("Stream's buffer is not large enough to get %d bytes -- %=",
	  bytes, stream);
  end;
  buf.buffer-end := buf.size; // It should be that anyway, but we need to be sure
  let buf-next :: <buffer-index> = buf.buffer-next;
  // Maintain buffer-stop
  if (stream.buffer-stop < buf-next)
    stream.buffer-stop := buf-next;
  end;
  // Test buf-next rather than buffer-stop.  Though buffer-stop may indicate
  // the buffer is full, there's no reason to back up the buffer when the
  // buf-next says the user isn't writing off the end of the buffer.
  if (bytes > (buf.size - buf-next))
	let stop :: <buffer-index> = stream.buffer-stop;
    // Can't write further in the buffer.
    backup-buffer(stream, stop)
  else
    // Just return the buffer, nothing special to do.
    buf
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
define sealed method do-force-output-buffers(stream :: <cw-output-stream>) => ();
	let buf :: <buffer> = stream.buffer;
	let stop :: <integer> = stream.buffer-stop;

    backup-buffer(stream, stop);

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
define inline sealed method do-synchronize(stream :: <cw-output-stream>) => ();
end method;

// nice functional helper functions:	еее move into utilities module
define inline function related??(getter :: <function>, #key by :: <function> = \==) => predicate :: <function>;
	method(lhs, rhs) => related? :: <boolean>;
		by(lhs.getter, rhs.getter)
	end
end function related??;

define inline function equal??(getter :: <function>) => predicate :: <function>;
	method(lhs, rhs) => equal? :: <boolean>;
		lhs.getter = rhs.getter
	end
end function equal??;

define inline function identical??(getter :: <function>) => predicate :: <function>;
	method(lhs, rhs) => identical? :: <boolean>;
		lhs.getter == rhs.getter
	end
end function identical??;

define c-enumeration
	$max-filename-length = 31,
	$max-filename-length-without-c-suffix = $max-filename-length - 2,
	$max-filename-length-without-du-suffix = $max-filename-length - 3
end c-enumeration;

//// Positionable Stream Protocol
//// Methods for exported interface.
////

/// stream-position
///
define method stream-position(stream :: <cw-output-stream>) => position :: <integer>;
	// Get the output buffer to make sure the stream is not already in use.
	let buf :: <buffer> = get-output-buffer(stream, bytes: 0);
	let backup :: <cw-vector> = stream.cw-output-stream-backup;
	stream.release-output-buffer;
	backup.size + buf.buffer-next
end method;

/*
/// stream-position-setter
///
define method stream-position-setter(position :: type-union(one-of(#"start", #"end"), <integer>), stream :: <cw-output-stream>) => position :: <integer>;
  // Get the output buffer to make sure the stream is not already in use.
  let buf :: <buffer> = get-output-buffer(stream, bytes: 0);
  let backup :: <cw-vector> = stream.cw-output-stream-backup;
  let backup-len :: <integer> = backup.size;
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
*/

/*
/// adjust-stream-position
///
define method adjust-stream-position
    (stream :: <cw-output-stream>,
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
*/

/// stream-size
///
define sealed method stream-size(stream :: <cw-output-stream>) => size :: <integer>;
	let buf :: <buffer> = get-output-buffer(stream, bytes: 0);
	let backup :: <cw-vector> = stream.cw-output-stream-backup;
	stream.release-output-buffer;
	backup.size + stream.buffer-stop;
end method;

/*
/// stream-contents
///
define sealed method stream-contents
    (stream :: <cw-output-stream>,
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
/// better implementation of <cw-output-stream>s.
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
*/

define generic determine-file-name(stream :: <cw-output-stream>) => name :: <byte-string>;

define class <cw-binary-output-stream>(<cw-output-stream>)
end class;

define sealed domain make(<cw-binary-output-stream>.singleton);
define sealed domain initialize(<cw-binary-output-stream>);

define function determine-file-name-worker(stream :: <cw-output-stream>, suffix :: <byte-string>, max-filename-length-without-suffix :: <integer>) => name :: <byte-string>;
	let this = stream.output-location;
	let suffix-size :: <integer> = suffix.size;
	if (copy-sequence(this.spec-file-name, start: this.spec-file-name.size - suffix-size) = suffix)
		this.spec-file-name
	else
		let chosen-name = stream.proposed-name | this.spec-file-name;
		
		this.spec-file-name := if (chosen-name.size <= max-filename-length-without-suffix)
												concatenate(chosen-name, suffix)
											else
												concatenate(copy-sequence(chosen-name, end: max-filename-length-without-suffix),
																	suffix)
											end if;	// compiler ICE if I write "endif" instead of "end if" here еее
	end
end function determine-file-name-worker;

/// determine-file-name
///
//еее factor out stuff!!!
define method determine-file-name(stream :: <cw-binary-output-stream>) => name :: <byte-string>;
/* 	let this = stream.output-location;
	let suffix = ".du";
	let suffix-size :: <integer> = suffix.size;
	if (copy-sequence(this.spec-file-name, start: this.spec-file-name.size - suffix-size) = suffix)
		this.spec-file-name
	else
		let chosen-name = stream.proposed-name | this.spec-file-name;
		
		this.spec-file-name := if (chosen-name.size <= $max-filename-length-without-du-suffix)
												concatenate(chosen-name, suffix)
											else
												concatenate(copy-sequence(chosen-name, end: $max-filename-length-without-du-suffix),
																	suffix)
											end if;	// compiler ICE if I write "endif" instead of "end if" here еее
	end */
	determine-file-name-worker(stream, ".du", $max-filename-length-without-du-suffix)
end method determine-file-name;

/// close
///
define sealed method close(stream :: <cw-binary-output-stream>, #key, #all-keys) => ();
	// Get the buffer to make sure no one is using it.
	get-output-buffer(stream, bytes: 0);
	stream.force-output-buffers;
	stream.release-output-buffer;
	stream.buffer := #f;

	let plug :: <plugin-callback> = stream.cw-output-stream-backup.vector-plugin;

	block()

		let this = stream.output-location;
		stream.determine-file-name;

		block ()
			create-file(stream.cw-output-stream-backup, this, binary?: #t);
		exception (<plugin-duplicate-file-error>)
			let mod-date = write-file(stream.cw-output-stream-backup, this);
		end block;
		
	exception(err :: <error>)
		report-condition(err, *standard-output*);	// еее
	end block;

	stream.cw-output-stream-backup.free-vector-storage;
end method;


define class <cw-text-output-stream>(<cw-output-stream>)
end class;

define sealed domain make(<cw-text-output-stream>.singleton);
define sealed domain initialize(<cw-text-output-stream>);

define variable *memoized-prefs* :: <sequence>.false-or = #f;

define function get-gwydion-prefs(plug :: <plugin-callback>)
	=> (	always-next-method :: <boolean>,
			command-line :: <boolean>,
			use-group :: <boolean>,
			group-path :: <byte-string>,
			output-folder :: <file-spec>.false-or, /* really??? */
			debug-code :: <boolean>,
			id-base :: <integer>.false-or);

	if (*memoized-prefs*)
		apply(values, *memoized-prefs*)
	else
		let prefs-handle :: <mem-handle> = get-named-preferences(plug, "Dylan Compiler");

		let (#rest prefs) =
			block ()
				let locked = lock-mem-handle(plug, prefs-handle);
				
				let group-start = locked + (2 + 2 + 2 + 2 + 4 + #x304);	// use the c struct hereеее
				
				let output-folder :: <file-spec> = resolve-relative-path(plug, locked + (2 + 2 + 2 + 2 + 4));
				// catch err!?еее
				
				let group-path = make(<byte-string>, size: unsigned-byte-at(group-start));
				let id-base = signed-long-at(locked, offset: 2 + 2 + 2 + 2);
				for (i :: <integer> from 0 below group-path.size)	// establish pascal strings, use memcpyеее
					group-path[i] := as(<byte-character>, unsigned-byte-at(group-start, offset: 1 + i));
				end for;
				values(	~logbit?(7, unsigned-byte-at(locked, offset: 2)),	// ~explicitNextMethod
							logbit?(5, unsigned-byte-at(locked, offset: 4)),	// emitCommandLine
							logbit?(7, unsigned-byte-at(locked, offset: 4)),	// groupForGen
							group-path,
							output-folder,
							logbit?(6, unsigned-byte-at(locked, offset: 4)),	// emitDebug
							logbit?(7, unsigned-byte-at(locked, offset: 6)) & id-base ~== -1 & id-base)	// baseID
			cleanup
				unlock-mem-handle(plug, prefs-handle);
			end block;
			
			*memoized-prefs* := prefs;
			plug.get-gwydion-prefs
		end if;
end function get-gwydion-prefs;


/// determine-file-name
///

define method determine-file-name(stream :: <cw-text-output-stream>) => name :: <byte-string>;
/*	let this = stream.output-location;
	let suffix = ".c";
	let suffix-size :: <integer> = suffix.size;
	if (copy-sequence(this.spec-file-name, start: this.spec-file-name.size - suffix-size) = suffix)
		this.spec-file-name
	else
		let chosen-name = stream.proposed-name | this.spec-file-name;
		
		this.spec-file-name := if (chosen-name.size <= $max-filename-length-without-c-suffix)
												concatenate(chosen-name, suffix)
											else
												concatenate(copy-sequence(chosen-name, end: $max-filename-length-without-c-suffix),
																	suffix)
											end if;	// compiler ICE if I write "endif" instead of "end if" here еее
	end */
	determine-file-name-worker(stream, ".c", $max-filename-length-without-c-suffix)
end method determine-file-name;

/// close
///
define sealed method close(stream :: <cw-text-output-stream>, #key, #all-keys) => ();
	// Get the buffer to make sure no one is using it.
	get-output-buffer(stream, bytes: 0);
	stream.force-output-buffers;
	stream.release-output-buffer;
	stream.buffer := #f;

	let plug :: <plugin-callback> = stream.cw-output-stream-backup.vector-plugin;

	block()
	// procedere:
	// determine correct name if not already given
	// try to find it as project file first, then write in it and notify IDE of change
	// if not in project, then create it, and add to project
	
		let this = stream.output-location;
		stream.determine-file-name;
		
		let found =
			block (found)
				for (num from 0 below plug.get-project-file-count)
					block ()
						let spec = get-file-info(plug, num, #t).project-file-spec;

						if (spec-file-name.equal??(spec, this))	// compare other fields too! type&cr etc. use case-insensitive compare!ее
							this := spec;
							found(num)
						end;
					exception(<plugin-fnf-error>)
						format-out("file #%= could not be found, skipping...\n", num);	// еее
					end block;
				end;
			end block;
		
		if (found)
			block ()
				pre-file-action(plug, this);
				let mod-date = write-file(stream.cw-output-stream-backup, this);
				set-mod-date(plug, this, mod-date, #t);
			cleanup
				post-file-action(plug, this);
			end block;
		else
			block ()
				create-file(stream.cw-output-stream-backup, this);
				let (always-next-method :: <boolean>, command-line :: <boolean>, use-group :: <boolean>, group-path :: <byte-string>)
					= plug.get-gwydion-prefs;
				block ()
					add-project-entry(plug, this, #t, use-group & make(<cw-new-project-entry-info>, group-path: group-path));
				exception(<error>)	// $cw-request-failed-error? еее
					// no problem, could already be there... (maybe we should try to find it now?)
				end block;
			exception(err :: <error>)// catch dupFNErr onlyеее
				let guilty = stream.determine-file-name;
				let line1 = format-to-string("File \"%s\" exists, but not present in link order", guilty);
				report-message(plug, cw-maybe-find-file(plug, guilty, full-search: #t), line1, 1, $message-type-error, 0);
				// increment error count!еее
			end block;
		end if;
		
	exception(err :: <error>)
		report-condition(err, *standard-output*);	// еее
	end block;

	stream.cw-output-stream-backup.free-vector-storage;
end method;

