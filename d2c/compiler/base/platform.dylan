module: platform
author: Nick Kramer
rcs-header: $Header: /scm/cvs/src/d2c/compiler/base/platform.dylan,v 1.1 1998/05/03 19:55:31 andreas Exp $
copyright: Copyright (c) 1995, 1996  Carnegie Mellon University
	   All rights reserved.

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

// This file defines stuff for reading in compiler platform information
// from disk and presenting it to the rest of the program in a
// reasonable way.

// <platform> -- exported
//
// Information about a specific platform.  Our definition of
// "platform" includes instruction set architecture, the operating
// system, and compilation tools like a C compiler and a make utility.
//
// Descriptions of all the slots can be found in ../platforms.descr.
//
// Not all info in platforms.descr is actually used by the compiler.
// Some of it is there for mk-build-tree/gen-makefile.  In those
// cases, we define slots in <platform> for that info, but we don't
// export it from this module.  (We have to come up with a way for the
// keywords to be acceptable to the class, and this seems like as good
// a way as any)
//
define sealed /* exported */ class <platform> (<object>)
  // platform-name is for internal use only.  The reason is, if you
  // conditionalize something based on the platform-name, then nobody
  // can create new platforms which inherit from the original platform.
  // For instance, someone writes
  //
  //    if (platform.platform-name == #"x86-win32")
  //      do whatever;
  //    end if;
  //
  // Then the platform x86-win32-vc, which is supposed to be another
  // name for x86-win32, wouldn't behave the same as x86-win32.
  //
  constant slot platform-name :: <symbol>, 
    required-init-keyword: #"platform-name";

  constant /* exported */ slot default-features :: <byte-string>,
    required-init-keyword: #"default-features";

  constant /* exported */ slot platform-integer-length :: <integer>,
    required-init-keyword: #"integer-length";

  constant /* exported */ slot heap-preamble :: <byte-string>,
    required-init-keyword: #"heap-preamble";
  constant /* exported */ slot align-directive :: <byte-string>, 
    required-init-keyword: #"align-directive";
  constant /* exported */ slot export-directive :: <byte-string>,
    required-init-keyword: #"export-directive";
  constant /* exported */ slot word-directive :: <byte-string>,
    required-init-keyword: #"word-directive";
  constant /* exported */ slot half-word-directive :: <byte-string>,
    required-init-keyword: #"half-word-directive";
  constant /* exported */ slot byte-directive :: <byte-string>, 
    required-init-keyword: #"byte-directive";
  constant /* exported */ slot comment-token :: <byte-string>,
    required-init-keyword: #"comment-token";
  constant /* exported */ slot mangled-name-prefix :: <byte-string>,
    required-init-keyword: #"mangled-name-prefix";

  constant /* exported */ slot object-filename-suffix :: <byte-string>,
    required-init-keyword: #"object-filename-suffix";
  constant /* exported */ slot library-filename-prefix :: <byte-string>,
    required-init-keyword: #"library-filename-prefix";
  constant /* exported */ slot library-filename-suffix :: <byte-string>,
    required-init-keyword: #"library-filename-suffix";
  constant /* exported */ slot executable-filename-suffix :: <byte-string>,
    required-init-keyword: #"executable-filename-suffix";

  constant /* exported */ slot compile-c-command :: <byte-string>,
    required-init-keyword: #"compile-c-command";
  constant /* exported */ slot default-c-compiler-flags :: <byte-string>,
    required-init-keyword: #"default-c-compiler-flags";
  constant /* exported */ slot assembler-command :: <byte-string>,
    required-init-keyword: #"assembler-command";
  constant /* exported */ slot link-library-command :: <byte-string>,
    required-init-keyword: #"link-library-command";
  constant /* exported */ slot link-executable-command :: <byte-string>,
    required-init-keyword: #"link-executable-command";
  constant /* exported */ slot link-executable-flags :: <byte-string>,
    required-init-keyword: #"link-executable-flags";
  constant /* exported */ slot make-command :: <byte-string>,
    required-init-keyword: #"make-command";
  constant /* exported */ slot delete-file-command :: <byte-string>,
    required-init-keyword: #"delete-file-command";
  constant /* exported */ slot compare-file-command :: <byte-string>,
    required-init-keyword: #"compare-file-command";
  constant /* exported */ slot move-file-command :: <byte-string>,
    required-init-keyword: #"move-file-command";

  constant /* exported */ slot path-separator :: <character>,
    required-init-keyword: #"path-separator";

  constant /* exported */ slot big-endian? :: <boolean>,
    required-init-keyword: #"big-endian?";

  // The next bunch of slots are unexported, because only
  // mk-build-tree/gen-makefile needs them.  They are required
  // keywords because gen-makefile doesn't do optional keywords.
  constant slot makefile-name :: <byte-string>, 
    required-init-keyword: #"makefile-name";
  constant slot make-supports-phony-targets? :: <boolean>, 
    required-init-keyword: #"make-supports-phony-targets?";
  constant slot recursive-make-command :: <byte-string>, 
    required-init-keyword: #"recursive-make-command";
  constant slot makefiles-can-rebuild-themselves? :: <boolean>,
    required-init-keyword: #"makefiles-can-rebuild-themselves?";
  constant slot uses-drive-letters? :: <boolean>, 
    required-init-keyword: #"uses-drive-letters?";
  constant slot environment-variables-can-be-exported? :: <boolean>,
    required-init-keyword: #"environment-variables-can-be-exported?";
  constant slot use-dbclink? :: <boolean>,
    required-init-keyword: #"use-dbclink?";

  // The remaining slots are really just a way for the compiler to
  // know when it needs to do black magic, but without knowing the
  // platform's name.
  constant /* exported */ slot link-doesnt-search-for-libs? :: <boolean> = #f,
    init-keyword: #"link-doesnt-search-for-libs?";
  constant /* exported */ slot import-directive-required? :: <boolean> = #f,
    init-keyword: #"import-directive-required?";
  // perhaps this next one should be supports-stabs...
  constant /* exported */ slot supports-debugging? :: <boolean> = #f,
    init-keyword: #"supports-debugging?";

  // used for debugging
  constant /* exported */ slot descriptor-type-string :: <byte-string>,
    init-keyword: #"descriptor-type-string";
  constant /* exported */ slot descriptor-reference-string :: <byte-string>,
    init-keyword: #"descriptor-reference-string";

  constant /* exported */ slot omit-colon-after-label-declarations? 
      :: <boolean> = #f,
    init-keyword: #"omit-colon-after-label-declarations?";
end class <platform>;

define sealed domain make(singleton(<platform>));
define sealed domain initialize(<platform>);

// string-to-boolean -- internal
//
// Converts a string into a boolean.  Signals an error if the string
// doesn't represent a Dylan boolean literal.
//
define function string-to-boolean (string :: <string>) => bool :: <boolean>;
  block (return)
    if (string.size == 2 & string.first == '#')
      let char = as-lowercase(string.second);
      if (char == 't')
	return(#t);
      elseif (char == 'f')
	return(#f);
      end if;
    end if;
    error("%s is no boolean I've ever heard of", string);
  end block;
end function string-to-boolean;

// string-to-character -- internal
//
// Takes a string, converts it into a character.  Signals an error if
// the string is not size 1.
//
define function string-to-character (string :: <string>)
 => char :: <character>;
  if (string.size ~== 1)
    error("If string.size isn't 1, how do you expect me to convert it\n"
	    "to a character? (string=%s)", string);
  else
    string.first;
  end if;
end function string-to-character;

// add-platform! -- internal
//
// Given a header (which describes a single platform), construct a
// sequence of keyword/values, and pass it to make().  Not only will
// this work, it will even catch duplicate and missing keywords
// (although the error message might not be readily understood by the
// most casual observer)
//
// defaults-table is used to implement platform inheritance.
// Conceptually it contains exactly the same data as the platforms
// table, but the platforms-table stores <platform>s, while the
// defaults-table stores sequences of keyword/value pairs.
//
define function add-platform!
    (header :: <header>, platforms-table :: <object-table>,
     defaults-table :: <object-table>)
 => ();
  // keyword-values must be some kind of sequence that add! adds
  // elements to the end of
  let keyword-values = #();  // It's critical that new keyword/value pairs 
                             // come before old keyword-value pairs
  let name :: false-or(<symbol>) = #f;
  let from :: <list> = #();
  for (val keyed-by key in header)
    let val = substring-replace(val, "\\t", "\t");
    let val = substring-replace(val, "\\n", "\n");
    let val = substring-replace(val, "\\\\", "\\");
    
    if (key == #"inherit-from")
      // Make sure we don't actually do the keyword-append until after
      // we've completely parsed this header.  Otherwise, the inherit
      // could override things in this header that came before the
      // "inherit-from" line
      from := element(defaults-table, as(<symbol>, val), default: #f);
      if (from == #f)
	error("Platform tries to inherit from %s, which isn't any "
		"platform I know of", val);
      end if;
    else
      // Do a select here to do conversions for the slot's type (and a
      // few other hacks)
      select (key)
	#"platform-name" =>
	  keyword-values := add!(keyword-values, as(<symbol>, val));
	#"make-supports-phony-targets?", #"makefiles-can-rebuild-themselves?",
	#"uses-drive-letters?", #"environment-variables-can-be-exported?",
	#"use-dbclink?", #"link-doesnt-search-for-libs?",
	#"import-directive-required?", #"supports-debugging?",
	#"omit-colon-after-label-declarations?", #"big-endian?" =>
	  keyword-values := add!(keyword-values, string-to-boolean(val));
	#"integer-length" =>
	  keyword-values := add!(keyword-values, string-to-integer(val));
	#"path-separator" =>
	  keyword-values := add!(keyword-values, string-to-character(val));
	otherwise =>
	  keyword-values := add!(keyword-values, val);
      end select;
      keyword-values := add!(keyword-values, key);
    end if;
  end for;
  keyword-values := concatenate(keyword-values, from);  // inherit!
  let platform = apply(make, <platform>, keyword-values);
  if (key-exists?(platforms-table, platform.platform-name))
    error("Redefinition of platform %s", platform.platform-name);
  end if;
  platforms-table[platform.platform-name] := platform;
  defaults-table[platform.platform-name] := keyword-values;
end function add-platform!;

// get-platforms -- exported
//
// Reads the platform information out of the specified file.  Returns
// a table mapping platform names (as symbols) to <platform>s.
//
define /* exported */ function get-platforms (filename :: <byte-string>)
 => platforms :: <object-table>;
  let source = make(<source-file>, name: filename);
  let result = make(<object-table>);
  let state = make(<object-table>);

  local 
    method repeat (old-line :: <integer>, old-posn :: <integer>)
     => platforms :: <object-table>;
      if (old-posn >= source.contents.size)
	result;
      else
	let (header, line, posn) 
	  = parse-header(source, line: old-line, position: old-posn);
	if (~ header.empty?)
	  // The "if" is so we can allow header blocks which are nothing
	  // but comments
	  add-platform!(header, result, state);
	end if;
	repeat(line, posn);
      end if;
    end method repeat;

  repeat(1, 0);
end function get-platforms;

// *current-target* -- exported
//
// The platform the compiler is generating code for.  A few different
// places in the compiler need this information, and it'll be a heck
// of a pain trying to propagate this information to them via
// parameter passing.  So we make it a global...  I'm not sure we need
// to define it in this particular library, but both cback and
// optimize need it, and I can't think of a better place.
//
define /* exported */ variable *current-target* :: false-or(<platform>) = #f;
