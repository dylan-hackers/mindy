module: main
copyright: see below

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000, 2001  Gwydion Dylan Maintainers
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

define method file-tokenizer
    (lib :: <library>, name :: <byte-string>)
    => (tokenizer :: <tokenizer>, module :: <module>);
  let source = make(<source-file>, name: name);
  let (header, start-line, start-posn) = parse-header(source);
  values(make(<lexer>,
	      source: source,
	      start-posn: start-posn,
	      start-line: start-line),
	 find-module(lib, as(<symbol>, header[#"module"])));
end;

/* This looks like some leftover debugging code

define method test-lexer (file :: <byte-string>) => ();
  block ()
    let (tokenizer, module) = file-tokenizer($dylan-library, file);
    block (return)
      *Current-Module* := module;
      while (#t)
	let token = get-token(tokenizer);
	if (token.token-kind == $eof-token)
	  return();
	else
	  format(*debug-output*, "%=\n", token);
	end if;
      end while;
    cleanup
      *Current-Module* := #f;
    end block;
  exception (<fatal-error-recovery-restart>)
    #f;
  end block;
end method test-lexer;


define method set-module (module :: type-union(<false>, <module>)) => ();
  *current-module* := module;
end method set-module;

define method set-module (module :: <symbol>) => ();
  block ()
    *current-module*
      := find-module(*Current-Library* | $Dylan-library, module);
  exception (<fatal-error-recovery-restart>)
    #f;
  end block;
end method set-module;

define method set-library (library :: type-union(<false>, <library>)) => ();
  *current-library* := library;
end method set-library;

define method set-library (library :: <symbol>) => ();
  block ()
    *current-library* := find-library(library);
  exception (<fatal-error-recovery-restart>)
    #f;
  end block;
end method set-library;

define method test-parse
    (parser :: <function>, file :: <byte-string>,
     #key debug: debug? :: <boolean>)
    => result :: <object>;
  block ()
    let (tokenizer, module) = file-tokenizer($dylan-library, file);
    let orig-library = *current-library*;
    let orig-module = *current-module*;
    block ()
      *current-library* := $dylan-library;
      *current-module* := module;
      parser(tokenizer, debug: debug?);
    cleanup
      *current-library* := orig-library;
      *current-module* := orig-module;
    end block;
  exception (<fatal-error-recovery-restart>)
    #f;
  end block;
end method test-parse;

*/

// The identifier for the current directory
// Used in searching for files

define constant $this-dir = #if (macos)
			       "";
			    #else
			       ".";
			    #endif

define function translate-abstract-filename (abstract-name :: <byte-string>)
 => (physical-name :: <byte-string>)
  // XXX - We should eventually replace this with a routine that checks
  // for foo.dylan and then foo.dyl, preferably using some sort of abstract
  // locator translation. But for now, we keep it simple.
  concatenate(abstract-name, ".dylan");
end;

// Considers anything with an ASCII value less than 32 (' ') to be
// whitespace.  This includes control characters as well as what we
// normally consider whitespace.
define method split-at-whitespace (string :: <byte-string>)
    => res :: <list>;
  split-at(method (x :: <character>) x <= ' ' end, string);
end method split-at-whitespace;


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


define method process-feature (feature :: <byte-string>) => ();
  if (feature.empty? | feature[0] ~== '~')
    add-feature(as(<symbol>, feature));
  else
    remove-feature(as(<symbol>, copy-sequence(feature, start: 1)));
  end if;
end method process-feature;


// This function compares old-filename to new-filename.  If they are
// different, or if one doesn't exist (presumably old-filename), then
// new-filename will be renamed old-filename, and what used to be
// old-filename will be deleted.  Otherwise, new-filename will be
// deleted.  This allows us to avoid unnecessary recompilation of .c
// files.
//
define method pick-which-file
    (old-filename :: <string>, new-filename :: <string>, 
     target :: <platform>)
 => (used-new-file :: <boolean>);
  if (files-identical?(old-filename, new-filename))
    delete-file(new-filename);
    #f;
  else
    rename-file(new-filename, old-filename);
    #t;
  end if;
end method pick-which-file;
     
// Look up a header element with a boolean default.  If specified, the option
// must be "yes" or "no".
//
define function boolean-header-element 
    (name :: <symbol>, default :: <boolean>, state :: <main-unit-state>) 
 => res :: <boolean>;
  let found = element(state.unit-header, name, default: #f);
  if (found)
    select (as-uppercase(found) by \=)
      "YES" => #t;
      "NO" => #f;
      otherwise => 
	compiler-error("%s: header option is %s, not \"yes\" or \"no\".",
		       name, found);
    end select;
  else
    default;
  end if;
end function boolean-header-element;
     
define function use-correct-path-separator
    (string :: <byte-string>, target :: <platform>) 
 => new-string :: <byte-string>;
  map(method (c :: <character>) => new-char :: <character>;
	if (c == '/') target.path-separator else c end if;
      end method,
      string);
end function use-correct-path-separator;

define method split-at-colon (string :: <byte-string>)
    => (module :: <byte-string>, name :: <byte-string>);
  block (return)
    for (index :: <integer> from 0 below string.size)
      if (string[index] == ':')
	return(copy-sequence(string, end: index),
	       copy-sequence(string, start: index + 1));
      end if;
    end for;
    compiler-fatal-error
      ("Invalid entry point: %s -- must be of the form module:variable.",
       string);
  end block;
end method split-at-colon;

define constant $search-path-seperator =
#if (compiled-for-win32)
  ';';
#else
  #if (macos)
     '\t';
  #else
     ':';
  #endif
#endif

