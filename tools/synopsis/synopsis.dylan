module: synopsis
author: Nick Kramer

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

// Creates a synopsis of Dylan source files.  A synopsis is
// essentially the file without any of the code, just top-level
// comments, definitions, and slots.  If -exported is specified,
// definitions and slots will make the synopsis only if they look like
// they're exported.  (A comment/definition seems exported if it has
// either a make-exports style /*exported*/ or a William style "foobar
// -- exported" or "foobar -- method on imported GF".  For that second
// style, "exported" and "imported" are the key words.)
//
// This program uses a bunch of kluged together regular expressions
// instead of a real parser, so code that breaks the Gwydion style
// guide will also likely screw up this program.  However, keep in
// mind that the Emacs Dylan-mode is also regular-expression based, so
// if Dylan-mode doesn't choke on it, synopsis probably won't either.
//
// Happily, Emacs font-lock for Dylan works almost perfectly on
// synopsis files, even though none of the define method/define
// function forms have end method/end functions.
//
// Usage: See main() function at bottom of file.



// *file* -- internal
//
// Contains the contents of the current file being worked on.
// Normally, we'd pass this around as a parameter to those functions
// that need access to it, but today I'm really sick of being spammed
// by the Mindy debugger.
//
// We start it off with some dummy value and let main() give it a real
// one.
//
define variable *file* :: <string> = "";

// find-end-of-comment -- internal
//
// start-pos is the beginning of a top-level comment (one starting in
// column 0), and we'll return the position of the first character
// after the newline terminating this comment.  If there actually
// isn't any comment at start-pos, we return start-pos.
//
define function find-end-of-comment (start-pos :: <integer>) 
 => comment-end :: <integer>;
  // We use an explicit loop here instead of just putting a "*" at the
  // end of the regular expression, because if we use a "*", the
  // regexp library will blow out the stack when it matches long
  // comments.  (And "long" isn't all that long -- about 20 lines)
  local 
    method repeat (pos :: <integer>) => next-pos :: <integer>;
      let (found?, next-pos) 
	= regexp-position(*file*, "^/" "/[^\r\n]*\r?\n", start: pos);
      if (found?)
	repeat(next-pos);
      else
	pos;
      end if;
    end method repeat;

  repeat(start-pos);
end function find-end-of-comment;

// maybe-print -- internal
//
// If the comment/definition seems exported, or if we're printing
// non-exported stuff, this function prints it.  Otherwise, it
// doesn't.  This function is a convenience function rather than a
// generalized output mechanism; process-class-definition bypasses
// this for printing slots.  
// 
// A comment/definition seems exported if it has either a make-exports
// style /*exported*/ or a William style "foobar -- exported" or
// "foobar -- method on imported GF".  For that second style,
// "exported" and "imported" are the key words.
//
define function maybe-print 
    (out-stream :: <stream>, comment-start :: <integer>, 
     comment-end :: <integer>, printable-end :: <integer>, 
     show-non-exported :: <boolean>)
 => did-print :: <boolean>;
  if (show-non-exported
	| regexp-position(*file*, "/\\*\\s*exported\\s*\\*/", 
			  start: comment-end, end: printable-end)
	| regexp-position(*file*, 
			    "^/" "/[^\r\n]*--[^\r\n]*(exported|imported)",
			    start: comment-start, end: comment-end))
    new-line(out-stream);
    write(out-stream, 
	  copy-sequence(*file*, start: comment-start, end: printable-end));
    new-line(out-stream);
    #t;
  else
    #f;
  end if;
end function maybe-print;

// process-method-definition -- internal
//
// Now that the "easy" part of the definition has been found, process
// the rest of the definition and print it out if possible.  This
// function handles both method and function definitions.
//
define function process-method-definition 
    (comment-start :: <integer>, comment-end :: <integer>, 
     defn-easy-part-end :: <integer>, out-stream :: <stream>, 
     show-non-exported :: <boolean>) 
 => end-of-printed :: <integer>;
  // We find the end of the top of the definition by looking for the
  // first line indented two space, or for "end" in the first column
  // (in case the body is empty, if that's even legal Dylan).  That
  // oughta work on any Dylan method or function written with
  // dylan-mode.el.
  let header-end = regexp-position(*file*, "\r?\n(  \\S|end)", 
				   start: defn-easy-part-end);
  if (~header-end) error("Can't find end of header"); end;
  maybe-print(out-stream, comment-start, comment-end, 
	      header-end, show-non-exported);
  header-end;
end function process-method-definition;

// process-generic-definition -- internal
//
// Now that the "easy" part of the definition has been found, process
// the rest of the definition and print it out if possible.  This
// function handles generic function definitions.
//
define function process-generic-definition
    (comment-start :: <integer>, comment-end :: <integer>, 
     defn-easy-part-end :: <integer>, out-stream :: <stream>, 
     show-non-exported :: <boolean>) 
 => end-of-printed :: <integer>;
  // Look for the next line which is either blank or starts in column
  // zero.  That must not be part of the "define generic".
  let (printable-end) 
    = regexp-position(*file*, "\r?\n(\\S|\r?\n)", start: defn-easy-part-end);
  if (~printable-end) error("Can't find end of generic"); end;
  maybe-print(out-stream, comment-start, comment-end, 
	      printable-end, show-non-exported);
  printable-end;
end function process-generic-definition;

// process-variable-definition -- internal
//
// Now that the "easy" part of the definition has been found, process
// the rest of the definition and print it out if possible.  This
// function handles both constant and variable definitions.
//
define function process-variable-definition
    (comment-start :: <integer>, comment-end :: <integer>, 
     defn-easy-part-end :: <integer>, out-stream :: <stream>, 
     show-non-exported :: <boolean>) 
 => end-of-printed :: <integer>;
  // For variables and constants, the easy part is all we want to print.
  maybe-print(out-stream, comment-start, comment-end, 
	      defn-easy-part-end, show-non-exported);
  defn-easy-part-end;
end function process-variable-definition;

// process-slots -- internal
//
// The class header has been found, and the word "end" is known to be
// on its own line (because it didn't immediately follow the
// superclass list).  Now, process the slots.  Note that if the class
// wasn't considered print-worthy, this function will never be called.
//
define function process-slots
    (class-header-end :: <integer>, out-stream :: <stream>, 
     show-non-exported :: <boolean>) 
 => end-of-printed :: <integer>;
  local method find-nested-something-interesting (start-pos :: <integer>)
	 => (interesting-pos :: <integer>, end-of-class-marker :: <boolean>);
	  let (found?, ignored, interesting-pos, group-end)
	    = regexp-position(*file*, "\r?\n(end|  \\S)", start: start-pos);
	  if (~found?) error("Does this class have no end?"); end;
	  if (copy-sequence(*file*, start: interesting-pos, 
			    end: group-end) = "end")
	    values(interesting-pos, #t);
	  else
	    values(interesting-pos, #f);
	  end if;
	end method find-nested-something-interesting,
    
    method repeat 
	(interesting-pos :: <integer>, end-of-class-marker :: <boolean>) 
     => final-pos :: <integer>;
      if (end-of-class-marker)
	interesting-pos;
      else
	let (ignored, comment-end)
	  = regexp-position(*file*, "^(  /" "/[^\r\n]*\r?\n)*", 
			    start: interesting-pos);
	// Now, we gotta process the slot itself.  Each slot is on a new
	// line, and is indented 2 spaces.
	let (slot-found?, slot-easy-part-end)
	  = regexp-position(*file*, 
			    "^  ((virtual|constant|sealed|instance|class"
			      "|each-subclass"
			      "|/\\*\\s*exported\\s*\\*/)\\s+)*"
			      "slot",
			    start: comment-end);
	// in this next expression, we write "comment-end - 1" because
	// slot-easy-part doesn't include a newline character at the
	// end, but a comment-end does.
	let (next-interesting-pos, next-is-end-of-class)
	  = find-nested-something-interesting
	      (if (slot-found?) slot-easy-part-end else comment-end - 1 end);
	if (show-non-exported 
	      | (slot-found? 
		   & regexp-position(*file*, "/\\*\\s*exported\\s*\\*/", 
				start: comment-end, 
				end: slot-easy-part-end)))
	  write(out-stream, 
		copy-sequence(*file*, start: interesting-pos, 
			      end: next-interesting-pos));
	  // new-line(out-stream);
	end if;
	repeat(next-interesting-pos, next-is-end-of-class);
      end if;
    end method repeat;

  let (interesting-pos, end-of-class?) 
    = find-nested-something-interesting(class-header-end);
  repeat(interesting-pos, end-of-class?);
end function process-slots;

// process-class-definition -- internal
//
// Now that the "easy" part of the definition has been found, process
// the rest of the definition and print it out if possible.  This
// function handles both the class proper and its slots (if any).
//
// ### Currently, if show-non-exported is false and the class itself
// isn't exported, the slots will never be printed (even if the slots
// themselves are exported).  I can't decide if this is a bug or a
// feature, but it wouldn't be hard to change.
//
define function process-class-definition
    (comment-start :: <integer>, comment-end :: <integer>, 
     defn-easy-part-end :: <integer>, out-stream :: <stream>, 
     show-non-exported :: <boolean>) 
 => end-of-printed :: <integer>;
  // Classes are hard because they have slots in them.

  // First, we find the end of the superclass list by looking for the
  // first ( ) pair with a lot of gunk between it (gunk being defined
  // as identifier characters, commas, and whitespace).
  let (found?, header-end)
    = regexp-position(*file*, 
		      "\\s*\\([-,\\sA-Za-z0-9!&*<>|^$%@_?]*\\)",
		      start: defn-easy-part-end); 
  if (~found?) error("Can't find end of superclasses list"); end;
  let printed? = maybe-print(out-stream, comment-start, comment-end, 
			     header-end, show-non-exported);
  let return-val
    = if (printed? 
	    & ~regexp-position(*file*, "^\\s*end(\\s+|;)", start: header-end))
	// If the class was printed and if the class might have slots,
	// process slots.
	process-slots(header-end, out-stream, show-non-exported);
      else
	header-end;
      end if;
  if (printed?)
    write(out-stream, "end class;");
    new-line(out-stream);
  end if;
  return-val;
end function process-class-definition;

// find-something-interesting -- internal
//
// Finds the next top-level comment, top-level definition, or form
// feed (aka page break or ^L).  #f if there are no more interesting
// things in this file.
//
define function find-something-interesting (start-pos :: <integer>)
 => interesting-pos :: false-or(<integer>);
  let (ignored, ignored2, interesting-pos) 
    = regexp-position(*file*, "\r?\n(\f|/" "/|define)", start: start-pos);
  interesting-pos;
end function find-something-interesting;

// process-definition -- internal
//
// This function looks at comment-end, and decides if the comment (if
// there really was one) is followed by a definition.  If so, it
// processes that definition, and if the definition is exported or
// show-non-exported is true, it prints the definition and comment.
// If there was no definition, we use the same logic to decide if we
// should print the comment.
//
// end-of-printed is the end of what we would have printed, had we
// printed something.
//
define function process-definition
    (comment-start :: <integer>, comment-end :: <integer>, 
     out-stream :: <stream>, show-non-exported :: <boolean>) 
 => end-of-printed :: <integer>;
  let (defn-start, defn-easy-part-end, ignored1, ignored2, 
       ignored3, ignored4, defn-kind-start, defn-kind-end, 
       identifier-start, identifier-end)
    = regexp-position(*file*, 
		      "^define\\s+"
			"((sealed|open|abstract|concrete|primary"
			"|free|inline|movable|flushable|functional"
			"|/\\*\\s*exported\\s*\\*/)\\s+)*"
			"(method|generic|function|class|variable|constant)"
			"\\s+([-A-Za-z0-9!&*<>|^$%@_?]+)",
		      start: comment-end);

  if (~defn-start)
    // We print all top-level comments that are unattached to code, no
    // matter what show-non-exported says.
    maybe-print(out-stream, comment-start, comment-end, 
		comment-end, #t);
    comment-end;
  else
    let defn-kind = copy-sequence(*file*, start: defn-kind-start, 
				  end: defn-kind-end);
    select (defn-kind by case-insensitive-equal)
      "method", "function" => 
	process-method-definition(comment-start, comment-end, 
				  defn-easy-part-end, 
				  out-stream, show-non-exported);
      "class" => 
	process-class-definition(comment-start, comment-end, 
				 defn-easy-part-end, 
				 out-stream, show-non-exported);
      "generic" => 
	process-generic-definition(comment-start, comment-end, 
				   defn-easy-part-end, 
				   out-stream, show-non-exported);
      "variable", "constant" => 
	process-variable-definition(comment-start, comment-end, 
				    defn-easy-part-end, 
				    out-stream, show-non-exported);
      otherwise => 
	error("%s is no definition I've ever heard of.", defn-kind);
    end select;
  end if;
end function process-definition;
    
// process-file -- internal
//
// Creates a synopsis for a single file (whose contents are in
// *file*).
//
define function process-file
    (out-stream :: <stream>, show-non-exported :: <boolean>) => ();
  local method repeat (start-pos, last-was-page-break) => ();
	  let interesting-pos = find-something-interesting(start-pos);
	  if (interesting-pos)
	    if (*file*[interesting-pos] == '\f')
	      if (~last-was-page-break)
		write(out-stream, "\f");
		new-line(out-stream);
	      end if;
	      repeat(interesting-pos + 1, #t);
	    else
	      let comment-end = find-end-of-comment(interesting-pos);
	      let end-of-printed
		= process-definition(interesting-pos, comment-end, 
				     out-stream, show-non-exported);
	      repeat(end-of-printed, #f);
	    end if;
	  end if;
	end method repeat;
  repeat(0, #t);
end function process-file;



// main -- method on imported GF
//
// Does command line processing and opens files.  The real work is
// done in process-file.
//
define method main (ignored, #rest cmd-line-args) => ();
  let show-non-exported = #t;
  let filenames = make(<stretchy-vector>);
  for (arg in cmd-line-args)
    if (arg = "-exported")
      show-non-exported := #f;
    else
      add!(filenames, arg);
    end if;
  end for;

  if (filenames.empty?)
    write(*standard-error*, 
	  "Usage:\n"
	    "  synopsis [-exported] input1 [input2...]\n\n"
	    "where -exported means show only definitions that are exported,\n"
	    "and - as an output-file means to use standard output.\n");
    force-output(*standard-error*);
    exit(exit-code: 1);
  end if;

  let out-stream = *standard-output*;
  for (filename in filenames)
    let stream = make(<file-stream>, locator: filename);

    // ### Under Mindy, this next line breaks in the case of really
    // big files (128K, or about 3500 lines of "typical" Dylan).  But
    // slurping the entire file into a single string really makes
    // regexp operations a hell of a lot easier.
    *file* := read-to-end(stream);
    process-file(out-stream, show-non-exported);
  end for;
  force-output(out-stream);
end method main;
