module: source
rcs-header: $Header: /scm/cvs/src/mindy/dbmc/source.dylan,v 1.1 2003/02/28 04:46:20 housel Exp $
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

// source-location -- exported.
//
// Return the location in the source where the thing came from, or #f if
// unknown.
// 
define open generic source-location (thing :: <object>)
    => res :: <source-location>;


// The <source-location-mixin> class.

define open abstract class <source-location-mixin> (<object>)
  sealed constant slot source-location :: <source-location>
      = make(<unknown-source-location>),
    init-keyword: source-location:;
end;


// The <source-location> class.

define open abstract class <source-location> (<object>)
end;

define sealed domain make (singleton(<source-location>));


define method make (wot == <source-location>, #key)
    => res :: <source-location>;
  make(<unknown-source-location>);
end;


define open generic describe-source-location
    (srcloc :: <source-location>, stream :: <stream>) => ();



// Unknown source locations.

define class <unknown-source-location> (<source-location>)
end;

define sealed domain make (singleton(<unknown-source-location>));
define sealed domain initialize (<unknown-source-location>);

define variable *unknown-srcloc* :: false-or(<unknown-source-location>) = #f;

define method make
    (class == <unknown-source-location>, #next next-method, #key)
    => res :: <unknown-source-location>;
  *unknown-srcloc* | (*unknown-srcloc* := next-method());
end method make;
  
define sealed method describe-source-location
    (srcloc :: <unknown-source-location>, stream :: <stream>)
    => ();
end method describe-source-location;


// Source files.

// <source> -- exported.
//
// Anything that gives us source code, be it a file, database
// or user input.
//
define abstract class <source> (<object>)
end class <source>;

define sealed domain make (singleton(<source>));
define sealed domain initialize (<source>);

// <source-file> -- exported.
// 
define class <source-file> (<source>)
  //
  // The name for this source file.
  constant slot full-file-name :: <byte-string>, 
    required-init-keyword: #"name";
  //
  // The contents, or #f if we haven't read them in yet.
  slot %contents :: false-or(<sequence>) = #f;
end;

define sealed domain make (singleton(<source-file>));
define sealed domain initialize (<source-file>);

define function file-name (source-file :: <source-file>) => name :: <string>;
  source-file.full-file-name; // .pathless-filename;
end function file-name;

define sealed method source-name (src :: <source-file>)
  src.file-name;
end method source-name;

/*
define method print-object (sf :: <source-file>, stream :: <stream>) => ();
  pprint-fields(sf, stream, name: sf.file-name);
end;
*/

// contents -- exported
//
// Return the contents of the source file as a buffer.  If we have
// never done this before, we read the entire file and stash the
// results so that we don't have to read the file again.  And so that
// we don't have to come up with some silly interface to read parts of
// the file and switch back and forth.
// 
define method contents (source :: <source-file>)
 => contents :: <sequence>;
  source.%contents
    | begin
	let file = make(<file-stream>, locator: source.full-file-name);
	block ()
	  source.%contents := read-to-end(file);
	cleanup
	  close(file);
	end block;
      end;
end method contents;


define class <known-source-location> (<source-location>)
  constant slot source :: <source>,
    required-init-keyword: source:;

  constant slot start-posn :: <integer>,
    required-init-keyword: start-posn:;
  
  constant slot start-line :: <integer>,
    required-init-keyword: start-line:;
  
  constant slot start-column :: <integer>,
    required-init-keyword: start-column:;
  
  constant slot end-posn :: <integer>,
    required-init-keyword: end-posn:;
  
  constant slot end-line :: <integer>,
    required-init-keyword: end-line:;
  
  constant slot end-column :: <integer>,
    required-init-keyword: end-column:;
end;

define sealed domain make (singleton(<unknown-source-location>));
define sealed domain initialize (<unknown-source-location>);

define sealed method describe-source-location
    (srcloc :: <known-source-location>, stream :: <stream>)
    => ();
  pprint-logical-block
    (stream,
     body:
       method (stream :: <stream>)
	 if (srcloc.end-line <= srcloc.start-line)
	   if (srcloc.end-column <= srcloc.start-column)
	     format(stream, "\"%s\", line %d, before character %d:",
		    srcloc.source.source-name, srcloc.start-line,
		    srcloc.start-column + 1);
	   elseif (srcloc.end-column == srcloc.start-column + 1)
	     format(stream, "\"%s\", line %d, character %d:",
		    srcloc.source.source-name, srcloc.start-line,
		    srcloc.start-column + 1);
	   else
	     format(stream, "\"%s\", line %d, characters %d through %d:",
		    srcloc.source.source-name, srcloc.start-line,
		    srcloc.start-column + 1, srcloc.end-column);
	   end if;
	 else
	   format(stream,
		  "\"%s\", line %d, character %d through "
		    "line %d, character %d:",
		  srcloc.source.source-name, srcloc.start-line,
		  srcloc.start-column + 1, srcloc.end-line,
		  srcloc.end-column);
	 end if;
       end method);
  write(stream, "  ");
end method describe-source-location;

/*
define method print-object (sl :: <known-source-location>, stream :: <stream>)
    => ();
  pprint-fields(sl, stream,
		source: sl.source,
		start-line: sl.start-line,
		start-column: sl.start-column,
		end-line: sl.end-line,
		end-column: sl.end-column);
end;
*/


// file source locations.


define method highlight-line
    (line :: <byte-string>, start-char :: <integer>, end-char :: <integer>,
     stream :: <stream>)
    => ();
  write(stream, "    ");
  local method repeat (index :: <integer>, column :: <integer>)
	  if (index < line.size)
	    let char = line[index];
	    if (char == '\t')
	      let new-column = floor/(column + 8, 8) * 8;
	      for (column from column below new-column)
		write-element(stream, ' ');
	      end for;
	      repeat(index + 1, new-column);
	    else
	      write-element(stream, char);
	      repeat(index + 1, column + 1);
	    end if;
	  end if;
	end method repeat;
  repeat(0, 0);
  pprint-newline(#"mandatory", stream);
  let start-column = compute-column(line, start-char);
  if (start-char < end-char)
    let end-column = compute-column(line, end-char);
    for (column from 0 below start-column + 4)
      write-element(stream, ' ');
    end for;
    for (column from start-column below end-column)
      write-element(stream, '^');
    end for;
  else
    for (column from 0 below start-column + 3)
      write-element(stream, ' ');
    end for;
    write(stream, "/\\");
  end if;
  pprint-newline(#"mandatory", stream);
end method highlight-line;


define method compute-column (line :: <byte-string>, target :: <integer>)
 => col :: <integer>;
  for (i from 0 below target,
       column = 0
	 then if (line[i] == '\t')
		floor/(column + 8, 8) * 8;
	      else
		column + 1;
	      end if)
  finally
    column;
  end for;
end method compute-column;


// extract-string -- exported.
//
// Extracts the text behind the source location and returns it as a string.
// Does no processing of the input.  The start: and end: keywords can
// be used to adjust exactly where to start and end the string.
// 
define method extract-string
    (source-location :: <known-source-location>,
     #key start :: <integer> = -1,
          end: finish :: <integer> = -1)
 => string :: <byte-string>;
  if (start < 0)
    start := source-location.start-posn;
  end;
  if (finish < 0)
    finish := source-location.end-posn;
  end;
  let len = finish - start;
  if (len.positive?)
    copy-sequence(source-location.source.contents, start: start, end: finish);
  else
    "";
  end if;
end;
