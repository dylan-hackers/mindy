module: make-exports
author: Nick Kramer
copyright: Copyright (C) 1994, Carnegie Mellon University
	   All rights reserved.
	   This code was produced by the Gwydion Project at Carnegie Mellon
	   University.  If you are interested in using this code, contact
	   "Scott.Fahlman@cs.cmu.edu" (Internet).
rcs-header: $Header: /scm/cvs/src/tools/make-exports/make-exports.dylan,v 1.1 1998/05/03 19:55:58 andreas Exp $

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
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
//    University.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports, questions, comments, and suggestions should be sent by
// E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
//
//======================================================================

//======================================================================
//
// Copyright (c) 1997  Carnegie Mellon University
// All rights reserved.
//
//======================================================================

// Inspired by Rob MacLachlan's /* exported */ convention, this tool
// will grep through source code and create module export entry for
// each identifier that the author wants exported.
//
// Command line usage:
//    make-exports library.input file1.dylan file2.dylan ...
//
// The way you use it is, in the source code any name you want
// exported, you stick the string "/* exported */" into the name
// definition, as if /* exported */ were a flag.  (The spaces inside
// the /* */ are optional)  For example, we might write
//
// define abstract /* exported */ class <foobar> (<object>)
//   constant /* exported */ slot whatever;
// end class <foobar>;
//
// Then make-exports would know that <foobar> and whatever should be
// exported from this module.  You can put the /* exported */ flag
// before, after, or in between other flags, but the /* exported */
// flag must appear on the same line as the identifier being exported.
//
// Modules can be exported just like identifiers.
//
// In your module definition, where you want your exports, just insert
// the string "/* exports for some-module */".  Example:
//
// define module some-module
//   use dylan;
//   export
//     /* exports for some-module */;
// end module some-module;
//
// In a library definition's export list, we write "/* exports for
// library */" -- "library" is for the purposes of make-exports a
// magical module which other modules reside in.
//
// The exports are listed in the order they are found in the source
// file; if a name is listed as /*exported*/ more than once, it will
// appear in the exports list more than once.  (This is legal Dylan,
// so don't sweat it) Any time there is a page break (^L) in the
// source file, make-exports will insert a blank line in the exports
// list.
//
// ### Currently, there's no easy way to export a slot setter.  You
// can either do a define generic and export that, or manually list it
// in the exports file yourself.



// regexp-substrings -- internal
//
// This function is like regexp-position, except it returns the actual
// substrings instead of marks to them.  It's thus usually convenient
// to work with, but less efficient.
//
define function regexp-substrings
    (big :: <string>, regexp :: <string>)
 => (found? :: <boolean>, #rest strings :: false-or(<string>));
  let (found?, ignored, #rest marks) = regexp-position(big, regexp);
  if (~found?)
    #f;
  else
    let strings = make(<stretchy-vector>);
    for (i from 0 below marks.size by 2)
      add!(strings, 
	   if (marks[i] ~== #f)
	     copy-sequence(big, start: marks[i], end: marks[i + 1]);
	   else
	     #f;
	   end if);
    end for;
    apply(values, #t, strings);
  end if;
end function regexp-substrings;

// find-module -- internal
//
// Read the stream line by line until you see a line that starts with
// "module:".  Return the module name.
//
define function find-module (stream :: <stream>) 
 => (module-name :: <string>, line-number :: <integer>);
  let line = read-line(stream);
  let (found?, module-name) 
    = regexp-substrings(line, "^module: \\s*([-A-Za-z0-9!&*<>|^$%@_?]+)");
  if (found?)
    values(module-name, 1);
  else
    let (module-name, line-number) = find-module(stream);
    values(module-name, line-number + 1);
  end if;
end function find-module;

// parse-line -- internal
//
// Parse a line that contains /*exported*/.  The line must either be a
// slot definition or a define foobar definition.  Each form has its
// own set of flags; if the line uses a flag that we don't know about,
// we can't parse the line.
//
// The line number and filename parameters are only for giving useful
// error messages.
//
define function parse-line 
    (line :: <string>, line-number :: <integer>, filename :: <string>)
 => (definition-kind :: <string>, identifier-name :: <string>);
  // Try "define foobar" notation
  let (found?, garbage, last-flag, definition-kind, identifier-name)
    = regexp-substrings
        (line, 
	 // First, we parse 1 or more flags, where /*exported*/ is
	 // considered just another flag
	 "(\\s+(sealed|open|abstract|concrete|primary"
	   "|free|inline|movable|flushable|functional"
	   "|/\\*\\s*exported\\s*\\*/))+"
	   
	   // Next, we pick off the kind of definition we have
	   "\\s+([-A-Za-z0-9!&*<>|^$%@_?=]+)"
	   
	   // Finally, we get the name of the identifier
	   "\\s+([-A-Za-z0-9!&*<>|^$%@_?=]+)");
  
  // If it wasn't a normal definition, see if we can interpret it
  // as a slot.  Were it not for the "class" flag on slots, we
  // wouldn't need to split this parsing into slots and everything
  // else.
  if (found?)
    values(definition-kind, identifier-name);
  else
    let (found-as-slot?, garbage, last-flag, slot-name)
      = regexp-substrings
          (line, 
	   // First, we parse 1 or more flags, where /*exported*/ is
	   // considered just another flag
	   "(\\s+(virtual|constant|sealed|instance|class"
	     "|each-subclass|/\\*\\s*exported\\s*\\*/))+"
	     
	     // Next, we pick off the word "slot"
	     "\\s+slot"
	     
	     // Finally, we get the name of the identifier
	     "\\s+([-A-Za-z0-9!&*<>|^$%@_?=]+)");
    if (found-as-slot?)
      values("slot", slot-name);
    else
      error("I don't understand line %d of %s.  \n"
	      "Perhaps /*exported*/ isn't on the same line as "
	      "the identifier name?",
	    line-number, filename);
    end if;
  end if;
end function parse-line;

// $page-break -- internal
//
// The ^L character
//
define constant $page-break = as(<character>, 12);

// scan-for-exports -- internal
//
// Scan a single file for /*exported*/.
//
define function scan-for-exports (filename :: <string>) 
 => (module-name :: <string>, export-list :: <sequence>,
     exported-modules :: <sequence>);
  let stream = make(<file-stream>, locator: filename);
  let (module-name, starting-line-number) = find-module(stream);
  let export-list = make(<stretchy-vector>);
  let exported-modules = make(<stretchy-vector>);
  for (line-number from starting-line-number + 1,
       line = read-line(stream, on-end-of-stream: #f) 
	 then read-line(stream, on-end-of-stream: #f),
       until: line == #f)
    // see if line contains /* exported */
    if (regexp-position(line, "/\\*\\s*exported\\s*\\*/"))
      let (definition-kind, identifier-name) 
	= parse-line(line, line-number, filename);
      add!(if (definition-kind = "module")
	     exported-modules;
	   else
	     export-list;
	   end if,
	   identifier-name);
    elseif (member?($page-break, line))
      add!(export-list, #"page-break");
    end if;
  end for;
  close(stream);
  values(module-name, export-list, exported-modules);
end function scan-for-exports;

// print-exports-list -- internal
//
// Generate the output for the exports of a single module.
//
define function print-exports-list
    (output-stream :: <stream>, exports :: <sequence>) => ();
  pprint-newline(#"fill", output-stream);
  let last-non-page-break
    = block (break)
	for (index from exports.size - 1 to 0 by -1)
	  if (exports[index] ~== #"page-break")
	    break(index);
	  end if;
	finally
	  0;
	end for;
      end block;
  
  // Make sure we don't output two page breaks in a row
  let last-name-was-page-break? :: <boolean> = #t;
  for (name in exports, index from 0 to last-non-page-break)
    if (name == #"page-break")
      if (~ last-name-was-page-break?)
	pprint-newline(#"mandatory", output-stream);
	pprint-newline(#"mandatory", output-stream);
      end if;
    else
      write(output-stream, name);
      if (index ~== last-non-page-break)
	write(output-stream, ", ");
	pprint-newline(#"fill", output-stream);
      end if;
    end if;
    last-name-was-page-break? := (name == #"page-break");
  end for;
end function print-exports-list;

// generate-output -- internal
//
// Read in a library template, and wherever it says "/* exports for
// whatever */", insert the exports for the module named "whatever".
//
define function generate-output 
    (library-template :: <string>,  // filename of template
     modules :: <case-insensitive-string-table>, // table of seqs of strings
     exported-modules :: <sequence>) // seq. of strings
 => ();
  let input-stream = make(<file-stream>, locator: library-template);
  let output-filename 
    = concatenate(copy-sequence(library-template, 
				end: substring-position(library-template, 
							".input")),
		  ".dylan");
  let output-stream 
    = make(<file-stream>, locator: output-filename, direction: #"output");
  for (line-number from 1,
       line = read-line(input-stream, on-end-of-stream: #f) 
	 then read-line(input-stream, on-end-of-stream: #f),
       until: line == #f)
    let (found?, before-magic-phrase, module-name, after-magic-phrase)
      = regexp-substrings
          (line, 
           "^\\w*(.*)/\\*\\s*exports\\s+for\\s+" // find "/* exports for"
	     "([-A-Za-z0-9!&*<>|^$%@_?=]+)" // find module name
	     "\\s+\\*/(.*)$"); // find end of comment + rest of line
    if (found?)
      pprint-logical-block
	(output-stream,
	 prefix: before-magic-phrase,
	 body: method (output-stream :: <stream>) => ();
		 let exports = if (module-name = "library")
				 exported-modules;
			       else
				 modules[module-name];
			       end if;
		 print-exports-list(output-stream, exports);
		 write(output-stream, after-magic-phrase);
	       end method);
    else
      write(output-stream, line);
    end if;
    new-line(output-stream);
  end for;    
  close(input-stream);
  force-output(output-stream);
  close(output-stream);
end function generate-output;

// main -- method on imported GF
//
// Does command line processing, reads in the input files, and
// generates the output.
//
define method main (ignored, #rest filenames) => ();
  *default-pretty?* := #t;

  if (filenames.empty?)
    format(*standard-error*,
	   "Command line usage:\n"
	     "  make-exports library.input file1.dylan"
	     "file2.dylan ...\n");
    force-output(*standard-error*);
    exit(exit-code: 1);
  end if;

  // library-template is read twice, once as a normal file, and once
  // as a template for the final output
  let library-template = filenames.first;

  let modules = make(<case-insensitive-string-table>);
  let exported-modules :: <sequence> = #[];
  for (file in filenames)
    let (module-name, export-list, more-exported-modules) 
      = scan-for-exports(file);
    let (exists?, existing-exports) = key-exists?(modules, module-name);
    let new-export-list = if (exists?)
			    concatenate(existing-exports, export-list);
			  else
			    export-list;
			  end if;
    modules[module-name] := new-export-list;
    exported-modules := concatenate(exported-modules, more-exported-modules);
  end for;
  generate-output(library-template, modules, exported-modules);
end method main;
