module: target-environment
author: Nick Kramer
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/platform.dylan,v 1.10 1996/09/15 15:33:31 nkramer Exp $
copyright: Copyright (c) 1995, 1996  Carnegie Mellon University
	   All rights reserved.

// This file defines stuff for reading in compiler target information
// from disk and presenting it to the rest of the program in a
// reasonable way.

// If you add a slot, make sure you update the make method to keep the
// target inheritence working right
//
define sealed class <target-environment> (<object>)
  // target-name is for internal use only
  constant slot target-name :: <symbol>, required-init-keyword: #"target-name";

  constant slot default-features :: <byte-string>,
    required-init-keyword: #"default-features";

  constant slot target-integer-length :: <integer>,
    required-init-keyword: #"integer-length";

  constant slot heap-preamble :: <byte-string>,
    required-init-keyword: #"heap-preamble";
  constant slot align-directive :: <byte-string>, 
    required-init-keyword: #"align-directive";
  constant slot export-directive :: <byte-string>,
    required-init-keyword: #"export-directive";
  constant slot word-directive :: <byte-string>,
    required-init-keyword: #"word-directive";
  constant slot half-word-directive :: <byte-string>,
    required-init-keyword: #"half-word-directive";
  constant slot byte-directive :: <byte-string>, 
    required-init-keyword: #"byte-directive";
  constant slot comment-token :: <byte-string>,
    required-init-keyword: #"comment-token";
  constant slot mangled-name-prefix :: <byte-string>,
    required-init-keyword: #"mangled-name-prefix";

  constant slot object-filename-suffix :: <byte-string>,
    required-init-keyword: #"object-filename-suffix";
  constant slot library-filename-prefix :: <byte-string>,
    required-init-keyword: #"library-filename-prefix";
  constant slot library-filename-suffix :: <byte-string>,
    required-init-keyword: #"library-filename-suffix";
  constant slot executable-filename-suffix :: <byte-string>,
    required-init-keyword: #"executable-filename-suffix";

  constant slot compile-c-command :: <byte-string>,
    required-init-keyword: #"compile-c-command";
  constant slot default-c-compiler-flags :: <byte-string>,
    required-init-keyword: #"default-c-compiler-flags";
  constant slot assembler-command :: <byte-string>,
    required-init-keyword: #"assembler-command";
  constant slot link-library-command :: <byte-string>,
    required-init-keyword: #"link-library-command";
  constant slot link-executable-command :: <byte-string>,
    required-init-keyword: #"link-executable-command";
  constant slot link-executable-flags :: <byte-string>,
    required-init-keyword: #"link-executable-flags";
  constant slot make-command :: <byte-string>,
    required-init-keyword: #"make-command";
  constant slot delete-file-command :: <byte-string>,
    required-init-keyword: #"delete-file-command";
  constant slot compare-file-command :: <byte-string>,
    required-init-keyword: #"compare-file-command";
  constant slot move-file-command :: <byte-string>,
    required-init-keyword: #"move-file-command";

  constant slot path-separator :: <character>,
    required-init-keyword: #"path-separator";

  // The remainder are really just a way for the compiler to know
  // when it needs to do black magic, but without knowing the
  // target's name
  constant slot link-doesnt-search-for-libs? :: <boolean> = #f,
    init-keyword: #"link-doesnt-search-for-libs?";
  constant slot import-directive-required? :: <boolean> = #f,
    init-keyword: #"import-directive-required?";
  // perhaps this next one should be supports-stabs...
  constant slot supports-debugging? :: <boolean> = #f,
    init-keyword: #"supports-debugging?";
  constant slot uses-win32-stabs? :: <boolean> = #f,
    init-keyword: #"uses-win32-stabs?";
  // rather than unix-stabs, which are slightly different
end class <target-environment>;

define sealed domain make(singleton(<target-environment>));
define sealed domain initialize(<target-environment>);

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

define function string-to-character (string :: <string>)
 => char :: <character>;
  if (string.size ~== 1)
    error("If string.size isn't 1, how do you expect me to convert it\n"
	    "to a character? (string=%s)", string);
  else
    string.first;
  end if;
end function string-to-character;

// Construct a sequence of keyword/values, and pass it to make().  Not
// only will this work, it will even catch duplicate and missing
// keywords (although the error message might not be readily
// understood by the most casual observer)
//
define function add-target!
    (header :: <header>, targets-table :: <object-table>,
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
	error("Target tries to inherit from %s, which isn't any "
		"target I know of", val);
      end if;
    else
      // Do a select here to do conversions for the slot's type (and a
      // few other hacks)
      select (key)
	#"target-name" =>
	  keyword-values := add!(keyword-values, as(<symbol>, val));
	#"link-doesnt-search-for-libs?", #"import-directive-required?", 
	#"supports-debugging?", #"uses-win32-stabs?" =>
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
  let target = apply(make, <target-environment>, keyword-values);
  if (key-exists?(targets-table, target.target-name))
    error("Redefinition of target %s", target.target-name);
  end if;
  targets-table[target.target-name] := target;
  defaults-table[target.target-name] := keyword-values;
end function add-target!;

define method get-targets (filename :: <byte-string>)
 => targets :: <object-table>;
  let source = make(<source-file>, name: filename);
  let result = make(<object-table>);
  let state = make(<object-table>);

  local 
    method repeat (old-line :: <integer>, old-posn :: <integer>)
     => targets :: <object-table>;
      if (old-posn >= source.contents.size)
	result;
      else
	let (header, line, posn) 
	  = parse-header(source, line: old-line, position: old-posn);
	if (~ header.empty?)
	  // The "if" is so we can allow header blocks which are nothing
	  // but comments
	  add-target!(header, result, state);
	end if;
	repeat(line, posn);
      end if;
    end method repeat;

  repeat(1, 0);
end method get-targets;



define variable *current-target* :: false-or(<target-environment>) = #f;
