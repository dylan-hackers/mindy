module: platform
author: Nick Kramer
rcs-header: $Header: /scm/cvs/src/d2c/compiler/base/platform.dylan,v 1.14 2003/12/23 00:45:35 housel Exp $
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

// This file defines stuff for reading in compiler platform information
// from disk and presenting it to the rest of the program in a
// reasonable way.

// Usage:
//   parse-platforms-file("/usr/share/dylan/platforms.descr");
//   let platform :: <platform> = get-platform-named("alpha-linux");

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
  constant /* exported */ slot pointer-size :: <integer>,
    required-init-keyword: #"pointer-size";
  constant /* exported */ slot pointer-alignment :: false-or(<integer>),
    init-value: #f, init-keyword: #"pointer-alignment";
  constant /* exported */ slot short-size :: <integer>,
    required-init-keyword: #"short-size";
  constant /* exported */ slot short-alignment :: false-or(<integer>),
    init-value: #f, init-keyword: #"short-alignment";
  constant /* exported */ slot integer-size :: <integer>,
    required-init-keyword: #"integer-size";
  constant /* exported */ slot integer-alignment :: false-or(<integer>),
    init-value: #f, init-keyword: #"integer-alignment";
  constant /* exported */ slot long-size :: <integer>,
    required-init-keyword: #"long-size";
  constant /* exported */ slot long-alignment :: false-or(<integer>),
    init-value: #f, init-keyword: #"long-alignment";
  constant /* exported */ slot long-long-size :: false-or(<integer>),
    init-value: #f, init-keyword: #"long-long-size";
  constant /* exported */ slot long-long-alignment :: false-or(<integer>),
    init-value: #f, init-keyword: #"long-long-alignment";
  constant /* exported */ slot double-size :: <integer>,
    required-init-keyword: #"double-size";
  constant /* exported */ slot double-alignment :: false-or(<integer>),
    init-value: #f, init-keyword: #"double-alignment";
  constant /* exported */ slot single-size :: <integer>,
    required-init-keyword: #"single-size";
  constant /* exported */ slot single-alignment :: false-or(<integer>),
    init-value: #f, init-keyword: #"single-alignment";
  constant /* exported */ slot long-double-size :: <integer>,
    required-init-keyword: #"long-double-size";
  constant /* exported */ slot long-double-alignment :: false-or(<integer>),
    init-value: #f, init-keyword: #"long-double-alignment";

  constant /* exported */ slot single-mantissa-digits :: <integer>,
    required-init-keyword: #"single-mantissa-digits";
  constant /* exported */ slot double-mantissa-digits :: <integer>,
    required-init-keyword: #"double-mantissa-digits";
  constant /* exported */ slot long-double-mantissa-digits :: false-or(<integer>),
    init-value: #f, init-keyword: #"long-double-mantissa-digits";

  constant /* exported */ slot minimum-single-float-exponent :: <integer>,
    required-init-keyword: #"minimum-single-float-exponent";
  constant /* exported */ slot maximum-single-float-exponent :: <integer>,
    required-init-keyword: #"maximum-single-float-exponent";

  constant /* exported */ slot minimum-double-float-exponent :: <integer>,
    required-init-keyword: #"minimum-double-float-exponent";
  constant /* exported */ slot maximum-double-float-exponent :: <integer>,
    required-init-keyword: #"maximum-double-float-exponent";
  constant /* exported */ slot minimum-long-double-float-exponent :: false-or(<integer>),
    init-value: #f, init-keyword: #"minimum-long-double-float-exponent";
  constant /* exported */ slot maximum-long-double-float-exponent :: false-or(<integer>),
    init-value: #f, init-keyword: #"maximum-long-double-float-exponent";

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
  constant /* exported */ slot default-c-compiler-debug-flags :: <byte-string>,
    required-init-keyword: #"default-c-compiler-debug-flags";
  constant /* exported */ slot default-c-compiler-profile-flags :: <byte-string>,
    required-init-keyword: #"default-c-compiler-profile-flags";
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
  constant /* exported */ slot make-jobs-flag :: <byte-string>,
    required-init-keyword: #"make-jobs-flag";

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

  // if this is defined, search for shared libraries
  constant /* exported */ slot shared-library-filename-suffix
      :: false-or(<byte-string>) = #f,
    init-keyword: #"shared-library-filename-suffix";
  constant /* exported */ slot shared-object-filename-suffix
      :: false-or(<byte-string>) = #f,
    init-keyword: #"shared-object-filename-suffix";
  constant /* exported */ slot randomize-library-command
      :: false-or(<byte-string>) = #f,
    init-keyword: #"randomize-library-command";
  constant /* exported */ slot libtool-command
      :: false-or(<byte-string>) = #f,
    init-keyword: #"libtool-command";
  constant /* exported */ slot link-profile-flags
      :: false-or(<byte-string>) = #f,
    init-keyword: #"link-profile-flags";
  constant /* exported */ slot link-debug-flags
      :: false-or(<byte-string>) = #f,
    init-keyword: #"link-debug-flags";

  // if this is defined, we can build shared libraries
  constant /* exported */ slot link-shared-library-command
      :: false-or(<byte-string>) = #f,
    init-keyword: #"link-shared-library-command";
  constant /* exported */ slot link-shared-executable-command
      :: false-or(<byte-string>) = #f,
    init-keyword: #"link-shared-executable-command";
  constant /* exported */ slot compile-c-for-shared-command
      :: false-or(<byte-string>),
    init-keyword: #"compile-c-for-shared-command";

  // The remaining slots are really just a way for the compiler to
  // know when it needs to do black magic, but without knowing the
  // platform's name.
  constant /* exported */ slot link-doesnt-search-for-libs? :: <boolean> = #f,
    init-keyword: #"link-doesnt-search-for-libs?";
end class <platform>;

define sealed domain make(singleton(<platform>));
define sealed domain initialize(<platform>);

define variable *valid-properties* = make(<table>);
// Value defines whether or not property is required.
*valid-properties*[#"platform-name"] := #t;
*valid-properties*[#"default-features"] := #t;
*valid-properties*[#"integer-length"] := #t;
*valid-properties*[#"integer-size"] := #t;
*valid-properties*[#"integer-alignment"] := #f;
*valid-properties*[#"pointer-size"] := #t;
*valid-properties*[#"pointer-alignment"] := #f;
*valid-properties*[#"short-size"] := #t;
*valid-properties*[#"short-alignment"] := #f;
*valid-properties*[#"long-size"] := #t;
*valid-properties*[#"long-alignment"] := #f;
*valid-properties*[#"long-long-size"] := #f;
*valid-properties*[#"long-long-alignment"] := #f;
*valid-properties*[#"single-size"] := #t;
*valid-properties*[#"single-alignment"] := #f;
*valid-properties*[#"double-size"] := #t;
*valid-properties*[#"double-alignment"] := #f;
*valid-properties*[#"long-double-size"] := #t;
*valid-properties*[#"long-double-alignment"] := #f;
*valid-properties*[#"single-mantissa-digits"] := #t;
*valid-properties*[#"double-mantissa-digits"] := #t;
*valid-properties*[#"long-double-mantissa-digits"] := #f;
*valid-properties*[#"minimum-single-float-exponent"] := #t;
*valid-properties*[#"maximum-single-float-exponent"] := #t;
*valid-properties*[#"minimum-double-float-exponent"] := #t;
*valid-properties*[#"maximum-double-float-exponent"] := #t;
*valid-properties*[#"minimum-long-double-float-exponent"] := #f;
*valid-properties*[#"maximum-long-double-float-exponent"] := #f;
*valid-properties*[#"object-filename-suffix"] := #t;
*valid-properties*[#"shared-object-filename-suffix"] := #f;
*valid-properties*[#"library-filename-prefix"] := #t;
*valid-properties*[#"library-filename-suffix"] := #t;
*valid-properties*[#"executable-filename-suffix"] := #t;
*valid-properties*[#"compile-c-command"] := #t;
*valid-properties*[#"compile-c-for-shared-command"] := #f;
*valid-properties*[#"default-c-compiler-flags"] := #t;
*valid-properties*[#"default-c-compiler-debug-flags"] := #t;
*valid-properties*[#"default-c-compiler-profile-flags"] := #t;
*valid-properties*[#"assembler-command"] := #t;
*valid-properties*[#"link-library-command"] := #t;
*valid-properties*[#"link-executable-command"] := #t;
*valid-properties*[#"link-shared-executable-command"] := #f;
*valid-properties*[#"link-executable-flags"] := #t;
*valid-properties*[#"make-command"] := #t;
*valid-properties*[#"libtool-command"] := #f;
*valid-properties*[#"delete-file-command"] := #t;
*valid-properties*[#"compare-file-command"] := #t;
*valid-properties*[#"move-file-command"] := #t;
*valid-properties*[#"make-jobs-flag"] := #t;
*valid-properties*[#"path-separator"] := #t;
*valid-properties*[#"big-endian?"] := #t;
*valid-properties*[#"makefile-name"] := #t;
*valid-properties*[#"make-supports-phony-targets?"] := #t;
*valid-properties*[#"recursive-make-command"] := #t;
*valid-properties*[#"makefiles-can-rebuild-themselves?"] := #t;
*valid-properties*[#"uses-drive-letters?"] := #t;
*valid-properties*[#"environment-variables-can-be-exported?"] := #t;
*valid-properties*[#"use-dbclink?"] := #t;
*valid-properties*[#"shared-library-filename-suffix"] := #f;
*valid-properties*[#"randomize-library-command"] := #f;
*valid-properties*[#"link-shared-library-command"] := #f;
*valid-properties*[#"link-debug-flags"] := #f;
*valid-properties*[#"link-profile-flags"] := #f;
*valid-properties*[#"link-doesnt-search-for-libs?"] := #f;

// Contains a table of tables, keyed by platform-name, each of which contains a
// table of values keyed by property name.
define variable *platform-property-value-table* :: <table> = make(<table>);

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
    error("%s is no boolean I've ever heard of.", string);
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


// Used to do inheritance (swiped from main/main.dylan):
// Split a string at locations where test returns true, removing the delimiter
// characters.
define method split-at (test :: <function>, string :: <byte-string>)
    => res :: <list>;
  let size = string.size;
  local
    method scan (posn :: <integer>, results :: <list>)
        => res :: <list>;
      if (posn == size)
        results;
      elseif (test(string[posn]))
        scan(posn + 1, results);
      else
        copy(posn + 1, posn, results);
      end;
    end method scan,
    method copy (posn :: <integer>, start :: <integer>,
                 results :: <list>)
        => res :: <list>;
      if (posn == size | test(string[posn]))
        scan(posn,
             pair(copy-sequence(string, start: start, end: posn), results));
      else
        copy(posn + 1, start, results);
      end;
    end method copy;
  reverse!(scan(0, #()));
end method split-at;

// Considers anything with an ASCII value less than 32 (' ') to be
// whitespace.  This includes control characters as well as what we
// normally consider whitespace.
define method split-at-whitespace (string :: <byte-string>)
    => res :: <list>;
  split-at(method (x :: <character>) x <= ' ' end, string);
end method split-at-whitespace;


// add-platform! -- internal
//
// Given a header (which describes a single platform), construct a
// sequence of keyword/values, and fill it into *platform-property-value-table*.

define function add-platform! (header :: <header>)
 => ();
  // keyword-values must be some kind of sequence that add! adds
  // elements to the end of
  let name :: false-or(<symbol>) = #f;
  let local-platform-info :: <table> = make(<table>);
  let property-value-table :: <table> = make(<table>);
  
  local-platform-info[#"default-features"] := "";
  property-value-table[#"default-features"] := "";

  for (val keyed-by key in header)
    let val = substring-replace(val, "\\t", "\t");
    let val = substring-replace(val, "\\n", "\n");
    let val = substring-replace(val, "\\\\", "\\");
    
    if (key == #"inherit-from")
      // Make sure we don't actually do the keyword-append until after
      // we've completely parsed this header.  Otherwise, the inherit
      // could override things in this header that came before the
      // "inherit-from" line
      for (platform in reverse(split-at-whitespace(val)))
	let inherited-stuff =
	  element(*platform-property-value-table*, as(<symbol>, platform), 
		  default: #f);
	if (inherited-stuff == #f)
	  error("Platform tries to inherit from %s, which isn't any "
		  "platform I know of", platform);
	end if;

	for (inherited-val keyed-by inherited-key in inherited-stuff)
	  unless (inherited-key == #"default-features")
	    property-value-table[inherited-key] := inherited-val;
	  end unless;
	end for;
	property-value-table[#"default-features"] := 
	  concatenate(inherited-stuff[#"default-features"], 
		      property-value-table[#"default-features"]);
      end for;
    else
      // Do a select here to do conversions for the slot's type (and a
      // few other hacks)

      unless (key-exists?(*valid-properties*, key))
        signal("platforms.descr contains unknown property, '%s'.", key);
      end;

      select (key)
	#"platform-name" =>
	  local-platform-info[key] := (name := as(<symbol>, val));
	#"make-supports-phony-targets?", #"makefiles-can-rebuild-themselves?",
	#"uses-drive-letters?", #"environment-variables-can-be-exported?",
        #"use-dbclink?", #"link-doesnt-search-for-libs?",
        #"big-endian?" =>
          local-platform-info[key] := string-to-boolean(val);
        #"integer-length", #"pointer-alignment", #"pointer-size",
        #"short-alignment", #"short-size",
        #"integer-alignment", #"integer-size", #"long-alignment", #"long-size",
        #"long-long-alignment", #"long-long-size",
        #"single-alignment", #"single-size",
        #"double-alignment", #"double-size",
        #"long-double-alignment", #"long-double-size",
        #"single-mantissa-digits", #"double-mantissa-digits",
        #"long-double-mantissa-digits",
        #"minimum-single-float-exponent", #"maximum-single-float-exponent",
        #"minimum-double-float-exponent", #"maximum-double-float-exponent",
        #"minimum-long-double-float-exponent", 
        #"maximum-long-double-float-exponent" =>
	  local-platform-info[key] := string-to-integer(val);
	#"path-separator" =>
          local-platform-info[key] := string-to-character(val);
	otherwise =>
          local-platform-info[key] := val;
      end select;
    end if;
  end for;
  
  for (new-val keyed-by new-key in local-platform-info)
    unless (new-key == #"default-features")
      property-value-table[new-key] := new-val;
    end unless;
  end for;
  property-value-table[#"default-features"] := 
    concatenate(local-platform-info[#"default-features"], 
		property-value-table[#"default-features"]);

  *platform-property-value-table*[name] := property-value-table;
end function add-platform!;

// parse-platforms-file -- exported
//
// Reads the platform information out of the specified file.  Returns
// a table mapping platform names (as symbols) to <platform>s.
//
define /* exported */ function parse-platforms-file (filename :: <byte-string>);
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
	  add-platform!(header);
	end if;
	repeat(line, posn);
      end if;
    end method repeat;

  repeat(1, 0);
end function parse-platforms-file;

define variable *platforms-table* :: <table> = make(<table>);

// get-platform-named -- exported
// 
// Creates and returns a <platform> previously parsed.

define function get-platform-named(name :: type-union(<string>, <symbol>)) 
 => platform :: <platform>;
  let keyword-values :: <list> = make(<list>);
  let property-table = element(*platform-property-value-table*,
			       as(<symbol>, name), default: #f);
  if (property-table == #f)
    error("Unknown platform '%s'", name);
  end if;

  for (required? keyed-by key in *valid-properties*)
    if (required?)
      unless (key-exists?(property-table, key))
	error("Platform '%s' does not define required property, '%s'", 
	      name, key);
      end unless;
    end if;
  end for;

  for (val keyed-by key in property-table)
    keyword-values := add!(keyword-values, val);
    keyword-values := add!(keyword-values, key);
  end for;
			     
  apply(make, <platform>, keyword-values);

end function get-platform-named;

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
