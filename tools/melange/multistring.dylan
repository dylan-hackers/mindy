module:  multistring-match
author:  Robert Stockton (rgs@cs.cmu.edu)
synopsis: Encapsulates the lexical conventions of the C language.  Along with
          c-lexer-cpp.dylan, this file also incorporates most of the
          functionality of CPP.
copyright: see below
	   This code was produced by the Gwydion Project at Carnegie Mellon
	   University.  If you are interested in using this code, contact
	   "Scott.Fahlman@cs.cmu.edu" (Internet).

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

//======================================================================
//
// Copyright (c) 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
// All rights reserved.
//
//======================================================================

//----------------------------------------------------------------------
// Multi-string matching
//
// These routines fill an intermediate niche in string matching.  Regular
// expressions are very general, but tend to be slow.  String searches are
// fine but not very versatile.  
//
// A multistring positioner simultaneously searches for several target
// strings, and returns the position of the first match.  It also returns the
// longest target string which matches at this position.
//
// A multistring checker finds the longest target string which matches at the
// given position.
//
//----------------------------------------------------------------------

#if (~mindy)
// multistring-checker-definer -- exported macro.
//
// By implementing this as a macro, we get better type checking and dispatch.
// By invoking
//   define inline multistring-checker fun ("string1", "string2", ....)
// you generate a function named
//   fun(big-string, #key start, end) => (false-or-match);
//
define macro multistring-checker-definer
  { define ?inline multistring-checker ?:name (?exprs:*) }
    => { begin
	   define constant "**" ## ?name ## "-dispatch**"
             :: <simple-object-vector> = compile-multistring(?exprs);
           define ?inline function ?name (big :: <string>,
					  #key start :: <integer> = 0,
					  end: big-end :: <integer> = big.size)
	    => (match :: false-or(<byte-string>));
	     check-multistring(big, "**" ## ?name ## "-dispatch**",
			       start, big-end);
	   end function ?name;
         end; }
  inline:
    { } => { }
    { inline } => { inline }
end macro;

// multistring-checker-definer -- exported macro.
//
// By implementing this as a macro, we get better type checking and dispatch.
//
// By implementing this as a macro, we get better type checking and dispatch.
// By invoking
//   define inline multistring-matcher fun ("string1", "string2", ....)
// you generate a function named
//   fun(big-string, #key start, end) => (false-or-position, false-or-match)
//
define macro multistring-positioner-definer
  { define ?inline multistring-positioner ?:name (?exprs:*) }
    => { begin
	   define constant "**" ## ?name ## "-dispatch**"
             :: <simple-object-vector> = compile-multistring(?exprs);
           define ?inline function ?name (big :: <string>,
					  #key start :: <integer> = 0,
					  end: big-end :: <integer> = big.size)
	    => (position :: false-or(<integer>),
		match-string :: false-or(<byte-string>));
	     find-multistring(big, "**" ## ?name ## "-dispatch**", 
			      start, big-end);
	   end function ?name;
	 end; }
  inline:
    { } => { }
    { inline } => { inline }
end macro;
#endif

// make-multistring-positioner -- exported function.
//
// A non-macro alternative to "define multistring-positioner"
//
define /* exported */ function make-multistring-positioner (#rest patterns)
  let dispatch = apply(compile-multistring, patterns);
  method (big :: <string>,
	  #key start :: <integer> = 0, end: big-end :: <integer> = big.size)
   => (position :: false-or(<integer>), match :: false-or(<byte-string>));
    find-multistring(big, dispatch, start, big-end);
  end method;
end function make-multistring-positioner;

// make-multistring-checker -- exported function.
//
// A non-macro alternative to "define multistring-checker"
//
define /* exported */ function make-multistring-checker (#rest patterns)
  let dispatch = apply(compile-multistring, patterns);
  method (big :: <string>,
	  #key start :: <integer> = 0, end: big-end :: <integer> = big.size)
   => (match :: false-or(<byte-string>));
    check-multistring(big, dispatch, start, big-end);
  end method;
end function make-multistring-checker;

// compile-multistring -- private function.
//
// Produces a vector of target strings indexed by their first characters.  Each
// cell contains a list of target strings arranged from longest to shortest.
//
define method compile-multistring
    (#rest patterns)
 => (result :: <simple-object-vector>);
  let dispatch = make(<simple-object-vector>, size: 256, fill: #());
  for (pattern :: <byte-string> in patterns)
    if (pattern.empty?)
      error("Null patterns not allowed in multistring searches.");
    end if;
    // Sort the list by descending size
    let index = as(<integer>, pattern.first);
    let old-list :: <list> = dispatch[index];
    if (old-list == #())
      dispatch[index] := list(pattern);
    elseif (pattern.size >= old-list.first.size)
      dispatch[index] := pair(pattern, old-list);
    else
      block (done)
	for (prev = old-list then this,
	     this = old-list.tail then this.tail,
	     until: this = #())
	  if (pattern.size >= this.head.size)
	    prev.tail := pair(pattern, this);
	    done();
	  end if;
	finally
	  prev.tail := list(pattern);
	end for;
      end block;
    end if;
  end for;
  dispatch;
end method compile-multistring;

// check-multistring -- private function.
//
// Checks for a match for one of the target strings at the position "key".
//
define method check-multistring
    (big :: <string>, dispatch :: <simple-object-vector>,
     key :: <integer>, big-end :: <integer>)
 => (result :: false-or(<byte-string>));
  block (return)
    if (key >= big-end) return(#f) end if;
    let char = big[key];
    let list :: <list> = element(dispatch, as(<integer>, char),
				 default: #());
    for (pat :: <byte-string> in list)
      for (pat-index from 1 below pat.size,
	   big-index from key + 1 below big-end,
	   while: pat[pat-index] == big[big-index])
      finally 
	if (pat-index == pat.size) return(pat) end if;
      end for;
    end for;
  end block;
end method check-multistring;

// find-multistring -- private function.
//
// Checks for a match for one of the target strings within the given range.
//
define method find-multistring
    (big :: <string>, dispatch :: <simple-object-vector>,
     start :: <integer>, big-end :: <integer>)
 => (position :: false-or(<integer>), match-string :: false-or(<byte-string>));
  block (return)
    for (key from start below big-end)
      let result = check-multistring(big, dispatch, key, big-end);
      if (result) return(key, result) end if;
    end for;
  end block;
end method find-multistring;

