module: header
copyright: see below


//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000, 2001, 2002  Gwydion Dylan Maintainers
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
  constant slot entries :: false-or(<header-entry>),
    required-init-keyword: entries:;
end;

// <header-entry> -- internal.
//
// A single entry in a header.
//
define class <header-entry> (<object>)
  //
  // The keyword.
  constant slot key :: <symbol>, required-init-keyword: key:;
  //
  // The value.
  constant slot value :: <byte-string>, required-init-keyword: value:;
  //
  // The next entry in the chain.
  slot next :: false-or(<header-entry>), init-value: #f, init-keyword: next:;
end;

/*
define method print-object (entry :: <header-entry>, stream :: <stream>) => ();
  pprint-fields(entry, stream, key: entry.key, value: entry.value);
end;
*/

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
    => (initial-state :: false-or(<header-entry>), done-state :: singleton(#f),
	next-state :: <function>, finished-state? :: <function>,
	current-key :: <function>, current-element :: <function>,
	current-element-setter :: <function>, copy-state :: <function>);
  local
    method header-fip-next (header :: <header>, state :: <header-entry>)
     => next-state :: false-or(<header-entry>);
      state.next;
    end,
    method header-fip-finished? (header :: <header>,
				 state :: false-or(<header-entry>),
				 limit :: singleton(#f))
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
     => (res :: singleton(#f));
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

// type-for-copy -- gf method.
//
// Because <header>s aren't mutable, we have to define a method for
// class-for-copy.  We return <object-table>, because it is a mutable
// explicit key collection that uses the same key test -- i.e. just
// what we need.
//
define method type-for-copy (header :: <header>) => result :: <class>;
  <object-table>;
end method type-for-copy;

// key-sequence -- gf method.
//
// Return the keywords in the header.
//
define method key-sequence (header :: <header>) => result :: <list>;
  local method header-entry-keys(entry :: false-or(<header-entry>))
      => result :: <list>;
    if (entry)
      pair(entry.key, entry.next.header-entry-keys)
    else
      #()
    end if;
  end method;

  remove-duplicates!(header-entry-keys(header.entries));
end method;


// Header extending.

// header-add -- exported.
//
// Similar to GF add, but with <headers>. Takes an existing header, a key and
// a value and returns a new header with an entry in the first position.
//
define function header-add
    (header :: <header>, front-key :: <symbol>, front-value :: <byte-string>)
  => result :: <header>;

  make(<header>, entries: make(<header-entry>,
			       key: front-key,
			       value: front-value,
			       next: header.entries))
end;

// header-add-new -- exported.
//
// Similar to GF add-new, but with <headers>. Takes an existing header, a key and
// a value and if the key was not yet contained, returns a new header with an
// entry in the first position. If the key was already in the header, the header
// is returned unchanged.
//
define function header-add-new
    (header :: <header>, front-key :: <symbol>, front-value :: <byte-string>)
  => result :: <header>;

  if (member?(front-key, header.key-sequence))
    header
  else
    header-add(header, front-key, front-value)
  end
end;

// header-concatenate -- exported.
//
// Similar to GF concatenate, but with <headers>. Takes two existing headers, and
// returns a header that contains the entries from both, respecting order.
// [Conses a bit, but suffices.]
//
define function header-concatenate
    (header1 :: <header>, header2 :: <header>)
  => result :: <header>;

  if (~header1.entries)
    header2
  elseif (~header2.entries)
    header1
  else
    local method prepend(entry :: false-or(<header-entry>), header :: <header>)
        => result :: <header>;
      if (entry)
      	header-add(prepend(entry.next, header), entry.key, entry.value);
      else
        header
      end if;
    end method;
    prepend(header1.entries, header2)
  end
end;


// Header parsing.

// skip-whitespace -- internal.
//
// Return the position of the first non-whitespace character at or
// after posn.
//
define method skip-whitespace (contents :: <sequence>, posn :: <integer>)
    => ws-end :: <integer>;
  if (posn < contents.size)
    let char = as(<character>, contents[posn]);
    if (char == ' '
	  | char == '\t'
	  | char == '\r'
	  | char == '\n'
	  | char == '\<0c>')
      skip-whitespace(contents, posn + 1);
    else
      posn;
    end;
  else
    posn;
  end;
end method;

// Return position of last whitespace (as seen going backwards)
//
define method skip-whitespace-backwards
    (contents :: <sequence>, posn :: <integer>)
 => text-end :: <integer>;
  if (posn == 0)
    0;
  else
    let char = as(<character>, contents[posn - 1]);
    if (char == ' '
	  | char == '\t'
	  | char == '\r'
	  | char == '\n'
	  | char == '\<0c>')
      skip-whitespace-backwards(contents, posn - 1);
    else
      posn;
    end if;
  end if;
end method skip-whitespace-backwards;

// find-newline -- internal.
//
// Return the position of the next newline at or after posn.
//
define function find-newline (contents :: <sequence>, posn :: <integer>)
    => newline :: <integer>;
  if (posn < contents.size)
    let char = as(<character>, contents[posn]);
    // Try to handle DOS-style line endings more intelligently
    if (char == '\r')
      let nxt_posn = posn + 1;
      if (nxt_posn < contents.size
          & as(<character>, contents[nxt_posn]) == '\n')
        nxt_posn;	// DOS style...
      else
        posn;		// No? ... then, Mac style.
      end;
    elseif (char == '\n')
      posn;		// Standard UNIX case
    else
      find-newline(contents, posn + 1);
    end;
  else
    posn;
  end;
end function;

// scan-keyword -- internal.
//
// Find and return the end of the keyword starting at posn and the end
// of the keyword (past the colon).  If the keyword is invalid, return
// #f.  This is only ever called when we know that start holds an
// alphabetic, so we don't have to check that.
//
define method scan-keyword (contents :: <sequence>, start :: <integer>)
    => (keywrd :: false-or(<symbol>),
	keyword-end :: <integer>);
  local
    method repeat (posn :: <integer>)
      if (posn < contents.size)
	let char = as(<character>, contents[posn]);
	if (char == ':')
	  let str = copy-sequence(contents, start: start, end: posn);
	  values(as(<symbol>, str), posn + 1);
	elseif (('a' <= char & char <= 'z')
		  | ('A' <= char & char <= 'Z')
		  | ('0' <= char & char <= '9')
		  | char == '-'
		  | char == '?')
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

// Tells you if the line starting at posn is a blank line (ie, nothing
// but whitespace).
//
define function blank-line? (contents :: <sequence>, posn :: <integer>)
 => answer :: <boolean>;
  skip-whitespace(contents, posn) >= find-newline(contents, posn);
end function blank-line?;

// scan-value -- internal.
//
// Extract the value starting at posn and return it.  Additionally,
// return the posn and line number for the next char after the value.
//
define method scan-value
    (contents :: <sequence>, start :: <integer>, start-line :: <integer>)
    => (value :: <byte-string>, value-end :: <integer>, end-line :: <integer>);
  local
    method repeat (posn :: <integer>, line :: <integer>,
		   prefix :: <byte-string>)
      let ws-end = skip-whitespace(contents, posn);
      let newline = find-newline(contents, posn);
      // Now, if the line has no value (only a keyword), then ws-end > newline
      let value-end = skip-whitespace-backwards(contents, newline);
      let continued?
	= (newline + 1 < contents.size 
	     & begin
		 let char = as(<character>, contents[newline + 1]);
		 char == ' '
		   | char == '\t'
		   | char == '\r'
		   | char == '\n'
		   | char == '\<0c>'
	       end
	     & ~blank-line?(contents, newline + 1));
      let value-start = min(ws-end, newline, value-end);
      let len = value-end - value-start + if (continued?) 1 else 0 end;
      let result = concatenate(prefix,
			       copy-sequence(contents,
					     start: value-start,
					     end: value-start + len));
      if (continued?)
	result.last := '\n';
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
define method parse-header (source :: <source-file>, 
			    #key line: init-line = 1, position: init-posn = 0)
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
	if (('a' <= char & char <= 'z')
	      | ('A' <= char & char <= 'Z'))
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
	elseif (char == '/' & (posn + 1 < contents.size) 
		  & as(<character>, contents[posn + 1]) == '/')
	  // We have a comment line (it must start at column 0).
	  // Don't treat it as a blank line, or that will end the
	  // header and move us into the file body.
	  repeat(1 + find-newline(contents, posn), line + 1);
	elseif (blank-line?(contents, posn))
	  values(make(<header>, entries: entries), line + 1, 
		 1 + find-newline(contents, posn));
	else
	  error("Bogus header keyword on line %d", line);
	end;
      else
	values(make(<header>, entries: entries), line, posn);
      end;
    end;
  repeat(init-posn, init-line);
end method;

// Seals for file header.dylan

// <header> -- subclass of <explicit-key-collection>
define sealed domain make(singleton(<header>));
define sealed domain initialize(<header>);
// <header-entry> -- subclass of <object>
define sealed domain make(singleton(<header-entry>));
define sealed domain initialize(<header-entry>);
