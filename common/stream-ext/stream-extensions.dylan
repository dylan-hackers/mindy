library: stream-extensions
module: stream-extensions
author: Russ Schaaf (rsbe@andrew.cmu.edu)
synopsis:   Extensions to the stream library.
copyright:  See below.

//======================================================================
//
// Copyright (c) 1996  Carnegie Mellon University
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

// read-as-list will read the elements of stream up through the until: element,
// and using the delimiter sequence, it will break the stream up into a list of
// <byte-vector>s.  For example, if the stream my-stream contained the
// following:
// Roger Corman
// Ed Wood
// Stephen Speilberg
// then read-as-list(my-stream, delimiters: #['\n', ' '], until: 'l')
// would produce #("Roger", "Corman", "Ed", "Wood", "Stephen", "Spei")
//
define method read-as-list(stream, #key delimiters = #['\n'],
			   until: end-on = #f, until-test = \==,
			   delimiter-test = \== )
  // ignore end-of-stream errors
  let handler <end-of-stream-error> = method (condition, next) end;
  let string-list = make(<list>, size: 0);
  let working-string = make(<byte-vector>, size: 0);
  if (~end-on)
    working-string := read-to-end(stream);
  else
    working-string := read-to(stream, end-on, test: until-test);
  end if;
  let old-delim = 0;
  // the predicate used to look for a delimiter
  let pred = rcurry(member?, map(curry(as, <byte>), delimiters),
		    test: delimiter-test);
  let i = 0;
  for (delim = find-key(working-string, pred, skip: i)
	 then find-key(working-string, pred, skip: i),
       until: ~delim)
    unless (delim == old-delim)
      string-list := concatenate(string-list,
				 list(copy-sequence(working-string,
						    start: old-delim,
						    end: delim)));
    end unless;
    // add 1 so that the delimiter character won't be part of the string
    old-delim := delim + 1;
    i := i + 1;
  finally
    string-list := concatenate(string-list,
			       list(copy-sequence(working-string,
						  start: old-delim)));
  end for;
  string-list;
end method read-as-list;

