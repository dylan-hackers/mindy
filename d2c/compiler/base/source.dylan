module: source
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/source.dylan,v 1.5 1995/12/15 16:16:36 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


// The <source-location> class.

define abstract class <source-location> (<object>)
end;

define method make (wot == <source-location>, #key)
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

define abstract class <source-location-mixin> (<object>)
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

// preserve identity for space sharing...
define class <source-file> (<identity-preserving-mixin>)
  slot name :: <string>, required-init-keyword: name:;
  slot %contents :: false-or(<buffer>), init-value: #f;
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
	let fd = fd-open(source.name, O_RDONLY);
	block ()
	  let len = fd-seek(fd, 0, SEEK_END);
	  fd-seek(fd, 0, SEEK_SET);
	  let result = make(<buffer>, size: len);
	  fd-read(fd, result, 0, len);
	  source.%contents := result;
	  result;
	cleanup
	  fd-close(fd);
	end;
      end;
end;

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

