module: source
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/source.dylan,v 1.9 1996/01/15 12:51:16 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


// The <source-location> class.

define abstract class <source-location> (<object>)
end;

define method make (wot == <source-location>, #key)
    => res :: <source-location>;
  make(<unknown-source-location>);
end;

define class <unknown-source-location> (<source-location>)
end;

add-make-dumper(#"unknown-source-location", *compiler-dispatcher*,
		<unknown-source-location>, #());


define generic source-location-span (start-loc :: <source-location>,
				     end-loc :: <source-location>)
    => res :: <source-location>;


// The <source-location-mixin> class.

define open abstract class <source-location-mixin> (<object>)
  slot source-location :: <source-location>,
    setter: #f,
    init-keyword: source-location:,
    init-function: curry(make, <unknown-source-location>);
end;

// source-location -- exported.
//
// Return the location in the source where token came from, of #f if
// unknown.
// 
define generic source-location (thing :: <source-location-mixin>)
 => res :: <source-location>;


// Source files.

#if (mindy)

define constant $big-buffer-threshold = 64 * 1024;

define class <big-buffer> (<vector>)
  slot size :: <integer>, setter: #f,
    required-init-keyword: size:;
  slot buffers :: <simple-object-vector>,
    required-init-keyword: buffers:;
end class <big-buffer>;

define method make
    (class == <big-buffer>, #next next-method, #key size = 0, fill = 0)
    => res :: <big-buffer>;
  let (nbuffers, extra) = floor/(size, $big-buffer-threshold);
  let buffers = make(<simple-object-vector>,
		     size: if (zero?(extra)) nbuffers else nbuffers + 1 end);
  for (index from 0 below nbuffers)
    buffers[index] := make(<buffer>, size: $big-buffer-threshold, fill: fill);
  end for;
  unless (zero?(extra))
    buffers[nbuffers] := make(<buffer>, size: extra, fill: fill);
  end unless;
  next-method(class, size: size, buffers: buffers);
end method make;

define constant $butt-plug = list(#"x");

define method element
    (vec :: <big-buffer>, index :: <integer>, #key default = $butt-plug)
    => element;
  if (index >= 0 & index < vec.size)
    let (buf, extra) = floor/(index, $big-buffer-threshold);
    vec.buffers[buf][extra];
  elseif (default ~== $butt-plug)
    default;
  else
    error("No element %= in %=", index, vec);
  end if;
end method element;

define method element-setter
    (new :: <byte>, vec :: <big-buffer>, index :: <integer>)
    => new :: <byte>;
  if (index >= 0 & index < vec.size)
    let (buf, extra) = floor/(index, $big-buffer-threshold);
    vec.buffers[buf][extra] := new;
  else
    error("No element %= in %=", index, vec);
  end if;
end method element-setter;

define method fill-buffer (big-buf :: <big-buffer>, stream :: <stream>) => ();
  for (buf in big-buf.buffers)
    read-into!(buf, stream);
  end for;
end method fill-buffer;

define method copy-bytes
    (dst :: <byte-string>, dst-offset :: <integer>,
     src :: <big-buffer>, src-offset :: <integer>,
     count :: <integer>)
    => ();
  let (start-buf, start-byte) = floor/(src-offset, $big-buffer-threshold);
  let (end-buf, end-byte) = floor/(src-offset + count, $big-buffer-threshold);
  if (start-buf == end-buf)
    copy-bytes(dst, dst-offset, src.buffers[start-buf], start-byte, count);
  else
    local method partial-copy (buf, byte, bytes)
	    copy-bytes(dst, dst-offset, src.buffers[buf], byte, bytes);
	    dst-offset := dst-offset + bytes;
	  end method partial-copy;
    partial-copy(start-buf, start-byte, $big-buffer-threshold - start-byte);
    for (buf from start-buf + 1 below end-buf)
      partial-copy(start-buf, 0, $big-buffer-threshold);
    end for;
    partial-copy(end-buf, 0, end-byte);
  end if;
end method copy-bytes;

define constant <file-contents> = type-union(<buffer>, <big-buffer>);

define method make-buffer (size :: <integer>)
    => res :: <file-contents>;
  if (size > $big-buffer-threshold)
    make(<big-buffer>, size: size);
  else
    make(<buffer>, size: size);
  end if;
end method make-buffer;

#else // Not mindy.

define constant <file-contents> = <buffer>;

define inline method make-buffer (size :: <integer>)
    => res :: <file-contents>;
  make(<buffer>, size: size);
end method make-buffer;

#end

define method fill-buffer (buf :: <buffer>, stream :: <stream>) => ();
  read-into!(buf, stream);
end method fill-buffer;


// preserve identity for space sharing...
define class <source-file> (<identity-preserving-mixin>)
  slot name :: <string>, required-init-keyword: name:;
  slot %contents :: false-or(<file-contents>), init-value: #f;
end;

define method print-object (sf :: <source-file>, stream :: <stream>) => ();
  pprint-fields(sf, stream, name: sf.name);
end;

add-make-dumper(#"source-file", *compiler-dispatcher*, <source-file>,
		list(name, name:, #f));


define class <file-source-location> (<source-location>)
  slot source-file :: <source-file>, required-init-keyword: source:;
  slot start-posn :: <integer>, required-init-keyword: start-posn:;
  slot start-line :: <integer>, required-init-keyword: start-line:;
  slot start-column :: <integer>, required-init-keyword: start-column:;
  slot end-posn :: <integer>, required-init-keyword: end-posn:;
  slot end-line :: <integer>, required-init-keyword: end-line:;
  slot end-column :: <integer>, required-init-keyword: end-column:;
end;

define method print-object (sl :: <file-source-location>, stream :: <stream>)
    => ();
  pprint-fields(sl, stream,
		source-file: sl.source-file,
		start-line: sl.start-line,
		start-column: sl.start-column,
		end-line: sl.end-line,
		end-column: sl.end-column);
end;

define method source-location-span (start :: <file-source-location>,
				    stop :: <file-source-location>)
    => res :: <file-source-location>;
  assert(start.source-file == stop.source-file);
  make(<file-source-location>,
       source: start.source-file,
       start-posn: start.start-posn,
       start-line: start.start-line,
       start-column: start.start-column,
       end-posn: stop.end-posn,
       end-line: stop.end-line,
       end-column: stop.end-column);
end;

// contents -- exported
//
// Return the contents of the source file as a buffer.  If we have
// never done this before, we read the entire file and stash the
// results so that we don't have to read the file again.  And so that
// we don't have to come up with some silly interface to read parts of
// the file and switch back and forth.
// 
define method contents (source :: <source-file>)
  source.%contents
    | begin
	let file = make(<file-stream>, name: source.name);
	block ()
	  let result = make-buffer(file.stream-size);
	  fill-buffer(result, file);
	  source.%contents := result;
	cleanup
	  close(file);
	end block;
      end;
end method contents;

// extract-string -- exported.
//
// Extracts the text behind the source location and returns it as a string.
// Does no processing of the input.  The start: and end: keywords can
// be used to adjust exactly where to start and end the string.
// 
define method extract-string (source-location :: <file-source-location>,
			      #key start :: <integer>
				= source-location.start-posn,
			      end: finish :: <integer>
				= source-location.end-posn)
  let len = finish - start;
  let result = make(<string>, size: len);
  copy-bytes(result, 0, source-location.source-file.contents, start, len);
  result;
end;

define constant $source-file-location-words = 6;

// We represent most slots in file source locations as raw data to save
// space/time.  The only subobject is the source-file.
//
define method dump-od (obj :: <file-source-location>, buf :: <dump-state>)
 => ();
  let start-pos = buf.current-pos;
  dump-definition-header(#"file-source-location", buf, subobjects: #t,
  			 raw-data: $odf-word-raw-data-format);
  dump-word($source-file-location-words, buf);
  dump-word(obj.start-posn, buf);
  dump-word(obj.start-line, buf);
  dump-word(obj.start-column, buf);
  dump-word(obj.end-posn, buf);
  dump-word(obj.end-line, buf);
  dump-word(obj.end-column, buf);
  dump-od(obj.source-file, buf);
  dump-end-entry(start-pos, buf);
end method;

add-od-loader(*compiler-dispatcher*, #"file-source-location",
  method (state :: <load-state>) => res :: <file-source-location>;
    state.od-next := state.od-next + $word-bytes; // skip count
    let nbytes = $source-file-location-words * $word-bytes;
    let next = fill-at-least(nbytes, state);
    let buf = state.od-buffer;
    let s-posn = buffer-word(buf, next + (0 * $word-bytes));
    let s-line = buffer-word(buf, next + (1 * $word-bytes));
    let s-column = buffer-word(buf, next + (2 * $word-bytes));
    let e-posn = buffer-word(buf, next + (3 * $word-bytes));
    let e-line = buffer-word(buf, next + (4 * $word-bytes));
    let e-column = buffer-word(buf, next + (5 * $word-bytes));
    state.od-next := next + nbytes;
    let file = load-object-dispatch(state);
    assert-end-object(state);
    make(<file-source-location>,
         source: file,
         start-posn: s-posn,
	 start-line: s-line,
	 start-column: s-column,
	 end-posn: e-posn,
	 end-line: e-line,
	 end-column: e-column);
    end method
);

