module: header
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/header.dylan,v 1.3 1995/12/16 01:56:06 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


// <header> and support routines.

// <header> -- exported.
//
// An explicit key collection that represents the header of a source
// file.  Note: individual keys can appear multiple times.  This
// happens when the header contains the same keyword multiple times.
//
define class <header> (<explicit-key-collection>)
  //
  // The chain of entries.
  slot entries :: false-or(<header-entry>),
    required-init-keyword: entries:;
end;

// <header-entry> -- internal.
//
// A single entry in a header.
//
define class <header-entry> (<object>)
  //
  // The keyword.
  slot key :: <symbol>, required-init-keyword: key:;
  //
  // The value.
  slot value :: <byte-string>, required-init-keyword: value:;
  //
  // The next entry in the chain.
  slot next :: false-or(<header-entry>), init-value: #f;
end;

define method print-object (entry :: <header-entry>, stream :: <stream>) => ();
  pprint-fields(entry, stream, key: entry.key, value: entry.value);
end;

// forward-iteration-protocol -- gf method.
//
// <header>'s are a collection so we have to define a
// forward-iteration-protocol method for it.
//
// We iterate across a header by just running down the entries chain
// until we hit #f.
//
// We define all the utility routines as local methods so that they
// have names in the debugger.  Makes backtraces a little bit nicer if
// anything flames out.
//
define method forward-iteration-protocol (header :: <header>)
  local
    method header-fip-next (header :: <header>, state :: <header-entry>)
     => next-state :: false-or(<header-entry>);
      state.next;
    end,
    method header-fip-finished? (header :: <header>,
				 state :: false-or(<header-entry>),
				 limit :: <false>)
     => finished? :: <boolean>;
      ~ state;
    end,
    method header-fip-curkey (header :: <header>, state :: <header-entry>)
     => key :: <symbol>;
      state.key;
    end,
    method header-fip-curel (header :: <header>, state :: <header-entry>)
     => value :: <byte-string>;
      state.value;
    end,
    method header-fip-curel-setter (header :: <header>,
				    state :: <header-entry>)
     => res :: <false>;
      error("<header> objects are not mutable.");
      #f;
    end,
    method header-fip-copy (header :: <header>, state :: <header-entry>)
     => copy :: <header-entry>;
      state;
    end;

  values(header.entries, #f, header-fip-next, header-fip-finished?,
	 header-fip-curkey, header-fip-curel, header-fip-curel-setter,
	 header-fip-copy);
end method;

// key-test -- gf method.
//
// key-test is another method we have to define to be a collection.
// We just use ==.
// 
define method key-test (header :: <header>) => result :: <function>;
  \==;
end;

// element -- gf method.
//
// As there is no default method for element, we have to supply one
// ourselves.  We don't bother constraining the type of the keyword,
// because if it isn't a symbol, it just won't be == to any of the
// keys.  Likewise, we don't spec a return value, because we have no
// idea what the default might be.
//
define constant no-default = list(#"no-default");
//
define method element (header :: <header>, keywrd, #key default = no-default)
    => value;
  block (return)
    for (entry = header.entries then entry.next,
	 while: entry)
      if (entry.key == keywrd)
	return(entry.value);
      end;
    end;
    if (default == no-default)
      error("No keyword %= in %=", keywrd, header);
    else
      default;
    end;
  end;
end method;

// class-for-copy -- gf method.
//
// Because <header>s arn't mutable, we have to define a method for
// class-for-copy.  We return <object-table>, because it is a mutable
// explicit key collection that uses the same key test -- i.e. just
// what we need.
//
define method class-for-copy (header :: <header>) => result :: <class>;
  <object-table>;
end;


// Header parsing.

// alphabetic? -- internal.
//
// Return #t if char is an alphabetic (i.e. letter), #f if not.
//
define method alphabetic? (char :: <character>)
  (char >= 'A' & char <= 'Z') | (char >= 'a' & char <= 'z');
end;

// alphanumeric? -- internal.
//
// Return #t if char is an alphanumeric (i.e. letter or number), #f if not.
//
define method alphanumeric? (char :: <character>)
  alphabetic?(char) | (char >= '0' & char <= '9');
end;

// whitespace? -- internal.
//
// Return #t if char is whitespace (space, tab, or form-feed), #f if not.
//
define method whitespace? (char :: <character>)
  char == ' ' | char == '\t' | char == '\f';
end;

// skip-whitespace -- internal.
//
// Return the position of the first non-whitespace character at or
// after posn.
//
define method skip-whitespace (contents :: <buffer>, posn :: <integer>)
    => ws-end :: <integer>;
  if (posn < contents.size)
    let char = as(<character>, contents[posn]);
    if (whitespace?(char))
      skip-whitespace(contents, posn + 1);
    else
      posn;
    end;
  else
    posn;
  end;
end method;

// find-newline -- internal.
//
// Return the position of the next newline at or after posn.
//
define method find-newline (contents :: <buffer>, posn :: <integer>)
    => newline :: <integer>;
  if (posn < contents.size)
    let char = as(<character>, contents[posn]);
    if (char ~= '\n')
      find-newline(contents, posn + 1);
    else
      posn;
    end;
  else
    posn;
  end;
end method;

// scan-keyword -- internal.
//
// Find and return the end of the keyword starting at posn and the end
// of the keyword (past the colon).  If the keyword is invalid, return
// #f.  This is only ever called when we know that start holds an
// alphabetic, so we don't have to check that.
//
define method scan-keyword (contents :: <buffer>, start :: <integer>)
    => (keywrd :: false-or(<symbol>),
	keyword-end :: <integer>);
  local
    method repeat (posn :: <integer>)
      if (posn < contents.size)
	let char = as(<character>, contents[posn]);
	if (char == ':')
	  let len = posn - start;
	  let str = make(<byte-string>, size: len);
	  copy-bytes(str, 0, contents, start, len);
	  values(as(<symbol>, str), posn + 1);
	elseif (alphanumeric?(char) | char == '-')
	  repeat(posn + 1);
	else
	  values(#f, posn);
	end;
      else
	values(#f, posn);
      end;
    end;
  repeat(start + 1);
end method;

// scan-value -- internal.
//
// Extract the value starting at posn and return it.  Additionally,
// return the posn and line number for the next char after the value.
//
define method scan-value (contents :: <buffer>, start :: <integer>,
			  start-line :: <integer>)
    => (value :: <byte-string>, value-end :: <integer>, end-line :: <integer>);
  local
    method repeat (posn :: <integer>, line :: <integer>,
		   prefix :: <byte-string>)
      let ws-end = skip-whitespace(contents, posn);
      let newline = find-newline(contents, ws-end);
      let continued?
	= (newline + 1 < contents.size
	     & whitespace?(as(<character>, contents[newline + 1])));
      let len = if (continued?)
		  newline + 1 - ws-end;
		else
		  newline - ws-end;
		end;
      let result = make(<byte-string>, size: len + prefix.size);
      copy-bytes(result, 0, prefix, 0, prefix.size);
      copy-bytes(result, prefix.size, contents, ws-end, len);
      if (continued?)
	repeat(newline + 1, line + 1, result);
      elseif (newline < contents.size)
	values(result, newline + 1, line + 1);
      else
	values(result, newline, line);
      end;
    end;
  repeat(start, start-line, "");
end method;

// parse-header -- exported.
//
// Parse the header of the given source file and return a <header>
// mapping keywords to values.  Also return the line and posn of the
// start of the body.
// 
define method parse-header (source :: <source-file>)
     => (header :: <header>,
	 body-start-line :: <integer>,
	 body-start-posn :: <integer>);
  let contents = source.contents;
  let entries = #f;
  let prev = #f;
  local
    method repeat (posn :: <integer>, line :: <integer>)
      if (posn < contents.size)
	let char = as(<character>, contents[posn]);
	if (alphabetic?(char))
	  let (keywrd, key-end) = scan-keyword(contents, posn);
	  if (keywrd)
	    let (value, value-end, value-end-line)
	      = scan-value(contents, key-end, line);
	    let new = make(<header-entry>, key: keywrd, value: value);
	    if (prev)
	      prev.next := new;
	    else
	      entries := new;
	    end;
	    prev := new;
	    repeat(value-end, value-end-line);
	  else
	    error("Bogus header keyword on line %d", line);
	  end;
	elseif (char == '\n')
	  values(make(<header>, entries: entries), line + 1, posn + 1);
	else
	  error("Bogus header keyword on line %d", line);
	end;
      else
	values(make(<header>, entries: entries), line, posn);
      end;
    end;
  repeat(0, 1);
end method;
