module: target-environment
author: Nick Kramer
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/platform.dylan,v 1.6 1996/08/12 14:01:49 nkramer Exp $
copyright: Copyright (c) 1995, 1996  Carnegie Mellon University
	   All rights reserved.

// This file defines stuff for reading in compiler target information
// from disk and presenting it to the rest of the program in a
// reasonable way.

define sealed class <target-environment> (<object>)
  constant slot target-name :: <symbol>, required-init-keyword: #"target-name";

  constant slot default-features :: <byte-string>,
    required-init-keyword: #"default-features";

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
end class <target-environment>;

define sealed domain make(singleton(<target-environment>));
define sealed domain initialize(<target-environment>);

// Construct a sequence of keyword/values, and pass it to make().  Not
// only will this work, it will even catch duplicate and missing
// keywords (although the error message might not be readily
// understood by the most casual observer)
//
define method as (cls == <target-environment>, header :: <header>)
 => target :: <target-environment>;
  // keyword-values must be some kind of sequence that add! adds
  // elements to the end of
  let keyword-values = make(<stretchy-vector>);
  for (val keyed-by key in header)
    let val = substring-replace(val, "\\t", "\t");
    let val = substring-replace(val, "\\n", "\n");
    
    add!(keyword-values, key);
    if (key == #"target-name")  // hack--target-name: wants a symbol, 
                                // but val is a byte-string.
      add!(keyword-values, as(<symbol>, val));
    else  // normal case
      add!(keyword-values, val);
    end if;
  end for;
  apply(make, <target-environment>, keyword-values);
end method as;

define method get-targets (filename :: <byte-string>)
 => targets :: <object-table>;
  let source = make(<source-file>, name: filename);
  let result = make(<object-table>);

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
	  let target = as(<target-environment>, header);
	  result[target.target-name] := target;
	end if;
	repeat(line, posn);
      end if;
    end method repeat;

  repeat(1, 0);
end method get-targets;
