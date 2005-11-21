Module: source-utilities
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

// macro-sources and macro-source-locations

// <macro-source> -- exported.
//
// A handle on the fragment produced by a macro-expansion.  Right now all
// we keep track of is the location of the macro call (via source-location-
// mixin) and a string description of it.
//
define class <macro-source>
    (<source-location-mixin>, <identity-preserving-mixin>)
  //
  // Some string describing the original macro call that produced this
  // expansion.
  constant slot macro-source-description :: <byte-string>,
    required-init-keyword: description:;
end class <macro-source>;

define sealed domain make (singleton(<macro-source>));
define sealed domain initialize (<macro-source>);

add-make-dumper(#"macro-source", *compiler-dispatcher*, <macro-source>,
		list(source-location, source-location:, #f,
		     macro-source-description, description:, #f));

// <section-marker> -- exported.
//
// Marker used to indicate which ``section'' of the expansion of the macro
// a macro-srcloc is part of.  A section is one or more tokens that are
// copied as a block from one location to another.  This is a useful notion
// to maintain, because if some range of tokens stays all within one section
// of the expansion, then they where in that same order wherever they came
// from.
// 
define class <section-marker> (<identity-preserving-mixin>)
end class <section-marker>;

define sealed domain make (singleton(<section-marker>));
define sealed domain initialize (<section-marker>);

add-make-dumper
  (#"macro-section-marker", *compiler-dispatcher*, <section-marker>,
   #());

// <macro-source-location> -- exported.
//
// Some location within the expansion of a macro.
// 
define abstract class <macro-source-location> (<source-location>)
  //
  // The macro source this is part of.
  constant slot macro-srcloc-source :: <macro-source>,
    required-init-keyword: source:;
end class <macro-source-location>;

define sealed domain make (singleton(<macro-source-location>));
define sealed domain initialize (<macro-source-location>);


// <simple-macro-source-location> -- exported.
//
// A single token within the expansion of a macro.
// 
define class <simple-macro-source-location> (<macro-source-location>)
  //
  // The source location for the fragment this source location was
  // expanded from.
  constant slot macro-srcloc-came-from :: <source-location>,
    required-init-keyword: came-from:;
  //
  // Which token in the expansion this source location is for.
  constant slot macro-srcloc-token :: <integer>,
    required-init-keyword: token:;
  //
  // The marker for the section this source location is in.
  constant slot macro-srcloc-section :: <section-marker>,
    required-init-keyword: section:;
end class <simple-macro-source-location>;

define sealed domain make (singleton(<simple-macro-source-location>));

add-make-dumper
  (#"simple-macro-source-location", *compiler-dispatcher*,
   <simple-macro-source-location>,
   list(macro-srcloc-source, source:, #f,
	macro-srcloc-came-from, came-from:, #f,
	macro-srcloc-token, token:, #f,
	macro-srcloc-section, section:, #f));


// <compound-macro-source-location> -- exported.
//
// A range of tokens within the expansion of a macro.
// 
define class <compound-macro-source-location> (<macro-source-location>)
  //
  // simple-macro-srclocs for the start and end (both inclusive) of this
  // compound srcloc.
  constant slot macro-srcloc-first :: <simple-macro-source-location>,
    required-init-keyword: first:;
  constant slot macro-srcloc-last :: <simple-macro-source-location>,
    required-init-keyword: last:;
end class <compound-macro-source-location>;

define sealed domain make (singleton(<compound-macro-source-location>));

add-make-dumper
  (#"compound-macro-source-location", *compiler-dispatcher*,
   <compound-macro-source-location>,
   list(macro-srcloc-source, source:, #f,
	macro-srcloc-first, first:, #f,
	macro-srcloc-last, last:, #f));


// describe-source-location{<macro-source-location>}
//
// Just identify the macro call we are inside of.  Good enough for now.
// 
define sealed method describe-source-location
    (srcloc :: <macro-source-location>, stream :: <stream>)
    => ();
  pprint-logical-block
    (stream,
     body:
       method (stream :: <stream>)
	 format(stream, "Inside expansion of %s.",
		srcloc.macro-srcloc-source.macro-source-description);
	 pprint-newline(#"mandatory", stream);
	 describe-source-location
	   (srcloc.macro-srcloc-source.source-location, stream);
       end method);
end method describe-source-location;


// source-location-before -- exported.
//
// Return a source location for the point at the start of the supplied
// source location.
//
define generic source-location-before
    (source-loc :: <source-location>)
    => res :: <source-location>;

define method source-location-before
    (srcloc :: <unknown-source-location>)
    => res :: <source-location>;
  make(<unknown-source-location>);
end method source-location-before;

define method source-location-before (srcloc :: <known-source-location>)
    => res :: <known-source-location>;
  make(<known-source-location>,
       source: srcloc.source,
       start-posn: srcloc.start-posn,
       start-line: srcloc.start-line,
       start-column: srcloc.start-column,
       end-posn: srcloc.start-posn,
       end-line: srcloc.start-line,
       end-column: srcloc.start-column);
end method source-location-before;

define method source-location-before
    (srcloc :: <simple-macro-source-location>)
    => res :: <source-location>;
  make(<compound-macro-source-location>,
       source: srcloc.macro-srcloc-source,
       first: srcloc,
       last: make(<simple-macro-source-location>,
		  source: srcloc.macro-srcloc-source,
		  came-from: make(<unknown-source-location>),
		  token: srcloc.macro-srcloc-token - 1,
		  section: srcloc.macro-srcloc-section));
end method source-location-before;

define method source-location-before
    (srcloc :: <compound-macro-source-location>)
    => res :: <source-location>;
  source-location-before(srcloc.macro-srcloc-first);
end method source-location-before;



// source-location-after -- exported.
//
// Return a source location for the point at the end of the supplied
// source location.
//
define generic source-location-after
    (source-loc :: <source-location>)
    => res :: <source-location>;

define method source-location-after
    (srcloc :: <unknown-source-location>)
    => res :: <source-location>;
  make(<unknown-source-location>);
end method source-location-after;

define method source-location-after (srcloc :: <known-source-location>)
    => res :: <known-source-location>;
  make(<known-source-location>,
       source: srcloc.source,
       start-posn: srcloc.end-posn,
       start-line: srcloc.end-line,
       start-column: srcloc.end-column,
       end-posn: srcloc.end-posn,
       end-line: srcloc.end-line,
       end-column: srcloc.end-column);
end method source-location-after;

define method source-location-after
    (srcloc :: <simple-macro-source-location>)
    => res :: <source-location>;
  make(<compound-macro-source-location>,
       source: srcloc.macro-srcloc-source,
       first: make(<simple-macro-source-location>,
		   source: srcloc.macro-srcloc-source,
		   came-from: make(<unknown-source-location>),
		   token: srcloc.macro-srcloc-token + 1,
		   section: srcloc.macro-srcloc-section),
       last: srcloc);
end method source-location-after;

define method source-location-after
    (srcloc :: <compound-macro-source-location>)
    => res :: <source-location>;
  source-location-after(srcloc.macro-srcloc-last);
end method source-location-after;



// source-location-between -- exported.
//
// Return a source location for the point between left-loc and right-loc,
// which are guaranteed to be adjacent.
// 
define generic source-location-between
    (left-loc :: <source-location>, right-loc :: <source-location>)
    => res :: <source-location>;

define method source-location-between
    (left :: <source-location>, right :: <source-location>)
    => res :: <source-location>;
  make(<unknown-source-location>);
end method source-location-between;

define method source-location-between
    (left :: <known-source-location>, right :: <known-source-location>)
    => res :: <known-source-location>;
  assert(left.source == right.source);
  make(<known-source-location>,
       source: left.source,
       start-posn: right.start-posn,
       start-line: right.start-line,
       start-column: right.start-column,
       end-posn: left.end-posn,
       end-line: left.end-line,
       end-column: left.end-column);
end method source-location-between;

define method source-location-between
    (left :: <simple-macro-source-location>,
     right :: <simple-macro-source-location>)
    => res :: <source-location>;
  assert(left.macro-srcloc-source == right.macro-srcloc-source);
  assert(left.macro-srcloc-token == right.macro-srcloc-token - 1);
  make(<compound-macro-source-location>,
       source: left.macro-srcloc-source,
       first: right,
       last: left);
end method source-location-between;

define method source-location-between
    (left :: <simple-macro-source-location>,
     right :: <compound-macro-source-location>)
    => res :: <source-location>;
  assert(left.macro-srcloc-source == right.macro-srcloc-source);
  source-location-between(left, right.macro-srcloc-first);
end method source-location-between;

define method source-location-between
    (left :: <compound-macro-source-location>,
     right :: <macro-source-location>)
    => res :: <source-location>;
  assert(left.macro-srcloc-source == right.macro-srcloc-source);
  source-location-between(left.macro-srcloc-last, right);
end method source-location-between;



// source-location-spanning -- exported.
//
// Return a source location for the range of tokens including start-loc,
// end-loc, and everything in between.  end-loc is guaranteed to follow
// start-loc.
// 
define generic source-location-spanning
    (start-loc :: <source-location>, end-loc :: <source-location>)
    => res :: <source-location>;

define method source-location-spanning
    (start :: <source-location>, stop :: <source-location>)
    => res :: <source-location>;
  make(<unknown-source-location>);
end method source-location-spanning;

define method source-location-spanning
    (start :: <known-source-location>, stop :: <known-source-location>)
    => res :: <known-source-location>;
  assert(start.source == stop.source);
  make(<known-source-location>,
       source: start.source,
       start-posn: start.start-posn,
       start-line: start.start-line,
       start-column: start.start-column,
       end-posn: stop.end-posn,
       end-line: stop.end-line,
       end-column: stop.end-column);
end;

define method source-location-spanning
    (start :: <simple-macro-source-location>,
     stop :: <simple-macro-source-location>)
    => res :: <source-location>;
  assert(start.macro-srcloc-source == stop.macro-srcloc-source);
  assert(start.macro-srcloc-token <= stop.macro-srcloc-token);
  make(<compound-macro-source-location>,
       source: start.macro-srcloc-source,
       first: start,
       last: stop);
end method source-location-spanning;
  
define method source-location-spanning
    (start :: <simple-macro-source-location>,
     stop :: <compound-macro-source-location>)
    => res :: <source-location>;
  assert(start.macro-srcloc-source == stop.macro-srcloc-source);
  if (stop.macro-srcloc-first.macro-srcloc-token >
	stop.macro-srcloc-last.macro-srcloc-token)
    start;
  else
    source-location-spanning(start, stop.macro-srcloc-last);
  end if;
end method source-location-spanning;

define method source-location-spanning
    (start :: <compound-macro-source-location>,
     stop :: <macro-source-location>)
    => res :: <source-location>;
  assert(start.macro-srcloc-source == stop.macro-srcloc-source);
  if (start.macro-srcloc-first.macro-srcloc-token >
	start.macro-srcloc-last.macro-srcloc-token)
    stop;
  else
    source-location-spanning(start.macro-srcloc-first, stop);
  end if;
end method source-location-spanning;



// simplify-source-location -- exported.
//
// Return a simplified version of the supplied source location.  In practice,
// this means that if location is a macro source location and all of it came
// from the same section, return the original location of those tokens.
// 
define generic simplify-source-location
    (srcloc :: <source-location>) => res :: <source-location>;

define method simplify-source-location
    (srcloc :: <source-location>) => res :: <source-location>;
  srcloc;
end method simplify-source-location;

define method simplify-source-location
    (srcloc :: <simple-macro-source-location>) => res :: <source-location>;
  simplify-source-location(srcloc.macro-srcloc-came-from);
end method simplify-source-location;

define method simplify-source-location
    (srcloc :: <compound-macro-source-location>) => res :: <source-location>;
  let first = srcloc.macro-srcloc-first;
  let last = srcloc.macro-srcloc-last;
  if (first.macro-srcloc-section == last.macro-srcloc-section)
    simplify-source-location
      (if (first.macro-srcloc-token > last.macro-srcloc-token)
	 source-location-between(last.macro-srcloc-came-from,
				 first.macro-srcloc-came-from);
       else
	 source-location-spanning(first.macro-srcloc-came-from,
				  last.macro-srcloc-came-from);
       end if);
  else
    srcloc;
  end if;
end method simplify-source-location;

