module: source
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/source.dylan,v 1.1.1.1 1994/12/19 13:02:47 wlott Exp $
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

define variable *unknown-source-location* = #f;

define method make (class == <unknown-source-location>,
		    #next next-method, #key)
  *unknown-source-location*
    | (*unknown-source-location* := next-method());
end;

define method join-source-locations (loc1 :: <unknown-source-location>,
				     loc2 :: <unknown-source-location>)
    => res :: <unknown-source-location>;
  loc1;
end;



// <compound-source-location>

define class <compound-source-location> (<source-location>)
  slot pieces :: <simple-object-vector>, required-init-keyword: pieces:;
end;

define method compound-source-location (#rest locations)
  make(<compound-source-location>,
       pieces: as(<simple-object-vector>, locations));
end;

define method simplify-source-location (loc :: <source-location>)
    => res :: <source-location>;
  loc;
end;

define method simplify-source-location (loc :: <compound-source-location>)
  let results = make(<stretchy-vector>);
  simplify-aux(loc, results);
  if (results.size == 1)
    simplify-source-location(results[0]);
  else
    unless (empty?(results))
      results.last := simplify-source-location(results.last);
    end;
    make(<compound-source-location>,
	 pieces: as(<simple-object-vector>, results));
  end;
end;

define method simplify-aux (compound-loc :: <compound-source-location>,
			    results :: <stretchy-vector>)
    => ();
  for (loc in compound-loc.pieces)
    simplify-aux(loc, results);
  end;
end;

define method simplify-aux
    (loc :: <source-location>, results :: <stretchy-vector>)
    => ();
  if (empty?(results))
    add!(results, loc);
  else
    let last = results.last;
    let join = join-source-locations(last, loc);
    if (join)
      results.last := join;
    else
      results.last := simplify-source-location(last);
      add!(results, loc);
    end;
  end;
end;


define generic join-source-locations (loc1 :: <source-location>,
				      loc2 :: <source-location>)
    => res :: union(<source-location>, <false>);

define method join-source-locations (loc1 :: <source-location>,
				     loc2 :: <source-location>)
    => res :: union(<source-location>, <false>);
  #f;
end;

define method join-source-locations (loc1 :: <compound-source-location>,
				     loc2 :: <source-location>)
    => res :: union(<source-location>, <false>);
  error("join-source-locations called on a <compound-source-location>?");
  #f;
end;

define method join-source-locations (loc1 :: <source-location>,
				     loc2 :: <compound-source-location>)
    => res :: union(<source-location>, <false>);
  error("join-source-locations called on a <compound-source-location>?");
  #f;
end;

define method join-source-locations (loc1 :: <compound-source-location>,
				     loc2 :: <compound-source-location>)
    => res :: union(<source-location>, <false>);
  error("join-source-locations called on a <compound-source-location>?");
  #f;
end;



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

define class <source-file> (<object>)
  slot name :: <string>, required-init-keyword: name:;
  slot %contents :: union(<buffer>, <false>), init-value: #f;
end;

define method print-object (sf :: <source-file>, stream :: <stream>) => ();
  pprint-fields(sf, stream, name: sf.name);
end;

define class <file-source-location> (<source-location>)
  slot source-file :: <source-file>, required-init-keyword: source:;
  slot start-index :: <integer>, required-init-keyword: start-index:;
  slot start-posn :: <integer>, required-init-keyword: start-posn:;
  slot start-line :: <integer>, required-init-keyword: start-line:;
  slot start-column :: <integer>, required-init-keyword: start-column:;
  slot end-index :: <integer>, required-init-keyword: end-index:;
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

define method join-source-locations 
    (start :: <file-source-location>, stop :: <file-source-location>)
    => res :: union(<false>, <file-source-location>);
  if (start.source-file == stop.source-file
	& start.end-index == stop.start-index)
    make(<file-source-location>,
	 source: start.source-file,
	 start-index: start.start-index,
	 start-posn: start.start-posn,
	 start-line: start.start-line,
	 start-column: start.start-column,
	 end-index: stop.end-index,
	 end-posn: stop.end-posn,
	 end-line: stop.end-line,
	 end-column: stop.end-column);
  end;
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
	  let len = fd-seek(fd, 0, L_XTND);
	  fd-seek(fd, 0, L_SET);
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
