module: target-environment
author: Nick Kramer
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/platform.dylan,v 1.4 1996/07/12 01:25:52 bfw Exp $
copyright: Copyright (c) 1995, 1996  Carnegie Mellon University
	   All rights reserved.

// This file defines stuff for reading in compiler target information
// from disk and presenting it to the rest of the program in a
// reasonable way.

define sealed class <target-environment> (<object>)
  constant slot target-name :: <symbol>, init-keyword: #"target-name";

  slot default-features :: <byte-string>;

  slot heap-preamble :: <byte-string>;
  slot align-directive :: <byte-string>; 
  slot export-directive :: <byte-string>;
  slot word-directive :: <byte-string>;
  slot half-word-directive :: <byte-string>;
  slot byte-directive :: <byte-string>; 
  slot comment-token :: <byte-string>;
  slot mangled-name-prefix :: <byte-string>;

  slot object-filename-suffix :: <byte-string>;
  slot library-filename-prefix :: <byte-string>;
  slot library-filename-suffix :: <byte-string>;
  slot executable-filename-suffix :: <byte-string>;

  slot compile-c-command :: <byte-string>;
  slot default-c-compiler-flags :: <byte-string>;
  slot assembler-command :: <byte-string>;
  slot link-library-command :: <byte-string>;
  slot link-executable-command :: <byte-string>;
  slot link-executable-flags :: <byte-string>;
  slot make-command :: <byte-string>;
  slot delete-file-command :: <byte-string>;
  slot compare-file-command :: <byte-string>;
  slot move-file-command :: <byte-string>;
end class <target-environment>;

define sealed domain make(singleton(<target-environment>));
define sealed domain initialize(<target-environment>);

define constant $target-attribute-description 
    = vector(#"default-features", default-features-setter,
	     #"heap-preamble", heap-preamble-setter,
	     #"align-directive", align-directive-setter,
	     #"export-directive", export-directive-setter,
	     #"word-directive", word-directive-setter,
	     #"half-word-directive", half-word-directive-setter,
	     #"byte-directive", byte-directive-setter,
	     #"comment-token", comment-token-setter,
	     #"mangled-name-prefix", mangled-name-prefix-setter,

	     #"object-filename-suffix", object-filename-suffix-setter,
	     #"library-filename-prefix", library-filename-prefix-setter,
	     #"library-filename-suffix", library-filename-suffix-setter,
	     #"executable-filename-suffix", 
	           executable-filename-suffix-setter,

	     #"compile-c-command", compile-c-command-setter,
	     #"default-c-compiler-flags", default-c-compiler-flags-setter,
	     #"assembler-command", assembler-command-setter,
	     #"link-library-command", link-library-command-setter,
	     #"link-executable-command", link-executable-command-setter,
	     #"link-executable-flags", link-executable-flags-setter,
	     #"make-command", make-command-setter,
	     #"delete-file-command", delete-file-command-setter,
	     #"compare-file-command", compare-file-command-setter,
	     #"move-file-command", move-file-command-setter);

define method get-targets (filename :: <byte-string>)
 => targets :: <object-table>;
  let stream = make(<file-stream>, direction: #"input", locator: filename);
  local method make-section (section-name :: <symbol>)
	 => section :: <target-environment>;
	  make(<target-environment>, target-name: section-name);
	end method make-section;
  parse-ini-file(stream, $target-attribute-description, make-section);
end method get-targets;
