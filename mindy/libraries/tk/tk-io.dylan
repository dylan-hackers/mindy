module: tk-internal
author: Robert Stockton (rgs@cs.cmu.edu)

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
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
//
// This file provides low level support for communicating to and from the WISH
// shell.  This includes buffer level I/O routines "put-tk-line" and
// "read-tk-line", as well as the data conversion routines "tk-quote",
// "tk-unquote" and "tk-as".
//
//======================================================================


//======================================================================
//			   Data conversion routines
//======================================================================

// Performs conversions comparable to "as", but adds some which do not
// generalize outside tk -- for example, tk-as(<boolean>, "1") == #t
//
define generic tk-as
    (cls :: <class>, value :: <object>) => (result :: <object>);

define method tk-as
    (cls :: <class>, value :: <object>) => (result :: <object>);
  as(cls, value);
end method tk-as;

define method tk-as
    (cls == <boolean>, value :: <string>) => (result :: <boolean>);
  value ~= "0";
end method tk-as;

define method tk-as
    (cls == <string>, value :: <boolean>) => (result :: <string>);
  if (value) "1" else "0" end if;
end method tk-as;

define constant tk-symbol-table :: <mutable-explicit-key-collection> 
  = make(<object-table>);
define method tk-as
    (cls == <string>, value :: <symbol>) => (result :: <string>);
  element(tk-symbol-table, value, default: #f)
    | (tk-symbol-table[value] := as-lowercase(as(<string>, value)));
end method tk-as;

define method tk-as
    (cls == <string>, value :: <string>) => (result :: <string>);
  value;
end method tk-as;

define method tk-as
    (cls == <string>, value :: <sequence>) => (result :: <string>);
  apply(join-tk-args, value);
end method tk-as;

//======================================================================

// There are probably lots of implementations of this, but this is on the
// critical path for efficiency, so I wanted to make sure I squeezed out
// everything I could.
//
define constant zero-byte = as(<integer>, '0');
define method tk-as
    (cls == <string>, num :: <integer>) => (result :: <string>);
  if (num == 0)
    "0";
  else
    let (num, neg) = if (num < 0) values(-num, #t) else values(num, #f) end if;

    for (num = num then next,
	 result = #() then pair(as(<character>,remainder + zero-byte), result),
	 (next, remainder) = truncate/(num, 10) then truncate/(next, 10),
	 while: num > 0)
    finally
      as(<byte-string>, if (neg) pair('-', result) else result end if)
    end for;
  end if;
end method tk-as;

define method tk-as
    (cls == <integer>, str :: <string>) => (result :: <integer>);
  let negative = #f;
  for (result = 0 then if (ch == '-') negative := #t; result
		       else (result * 10) + as(<integer>, ch) - zero-byte
		       end if,
       ch in str, until: ch < '0' | ch > '9')
  finally if (negative) -result else result end if
  end for;
end method tk-as;

//======================================================================

// Performs whatever quotation is necessary to get the data to WISH intact.
// For most objects, this is the same as "tk-as(<string>, ...)", but it does
// extra quotation on strings to make sure brackets and such like come out OK.
//
define generic tk-quote (object :: <object>) => (result :: <string>);

define method tk-quote (object :: <object>) => (result :: <string>);
  tk-as(<string>, object);
end method tk-quote;

define method tk-quote (string :: <string>) => (result :: <string>);
  // We do some extra work to make sure that we have the right sort of string.
  // This is necessary because the streams package is rather picky about such
  // things. 
  let string = as(<byte-string>, string);
  let pieces 
    = for (pieces = #() then if (string[i] == '[' | string[i] == ']'
				   | string[i] == '\\' | string[i] == '"'
				   | string[i] == '$')
			       pair(i, pieces);
			     else
			       pieces;
			     end if,
	   i from 0 below string.size)
      finally
	pieces;
      end for;
  if (pieces ~= #())
    let result = make(<byte-string>, size: string.size + pieces.size);
    for (prev = 0 then index,
	 pos = 0 then pos + index - prev + 1,
	 index in reverse!(pieces))
      copy-bytes(result, pos, string, prev, index - prev);
      result[pos + index - prev] := '\\';
    finally
      copy-bytes(result, pos, string, prev, string.size - prev);
    end for;
    result;
  else
    string;
  end if;
end method tk-quote;

//======================================================================

// Strips away all quotation from a string.  This will undo the various
// escapes required to imbed brackets and such like inside a tk list.  This
// should be called for any user level string, just as "tk-as" might be
// called for any other data type.
//
define method tk-unquote (string :: <string>)
  let pieces 
    = for (pieces = #() then if (string[i] == '\\' & (pieces.head ~= (i - 1)))
			       pair(i, pieces);
			     else
			       pieces;
			     end if,
	   i from 0 below string.size)
      finally
	pieces;
      end for;
  if (pieces ~= #())
    let result = make(<byte-string>, size: string.size - pieces.size);
    for (prev = 0 then index + 1,
	 pos = 0 then pos + index - prev,
	 index in reverse!(pieces),
	 first = #t then #f)
      copy-bytes(result, pos, string, prev, index - prev);
      if (~first & result[pos] == 'n') result[pos] := '\n' end if;
    finally
      copy-bytes(result, pos, string, prev, string.size - prev);
      if (~first & result[pos] == 'n') result[pos] := '\n' end if;
    end for;
    result;
  else
    string;
  end if;
end method tk-unquote;


//======================================================================
//			          I/O routines
//======================================================================

// These variables are initialized by a call to tk-init
//
define variable tk-in :: false-or(<stream>) = #f;  // tk process's "stdin" 
define variable tk-out :: false-or(<stream>) = #f; // tk process's "stdout" 

// Utility definitions for read-tk-line
//
define constant lbrace = as(<integer>, '{');
define constant rbrace = as(<integer>, '}');
define constant slash = as(<integer>, '\\');
define constant newline = as(<integer>, '\n');
#if (newlines-are-CRLF)
  define constant carriage-return = as(<integer>, '\r');
#endif

// Reads one logical "line" of text from the WISH interpreter.  This may
// actually contain several newlines, since newlines within brackets don't
// count. 
//
define method read-tk-line () => (result :: false-or(<string>));
  let buffer :: false-or(<buffer>) = get-input-buffer(tk-out);
  

  if (~buffer)
    #f;
  else
    // This routine is, obviously, somewhat hairy.  However, it lies
    // in the critical path for data communication, so some clarity
    // must be traded for efficiency.
    local method read-stuff (buff-pos, brackets, pieces, quoted?)
	    if (buff-pos >= buffer.buffer-end)
	      // We've run out of data -- save what we got thus far and then
	      // refill the buffer before recursing.  If we've hit EOF, simply
	      // return #f.
	      let new-pieces
		= if (buffer.buffer-next == buffer.buffer-end)
		    pieces;
		  else
		    pair(buffer-subsequence(buffer, <byte-string>,
					    buffer.buffer-next,
					    buffer.buffer-end), pieces);
		  end if;
	      buffer.buffer-next := buff-pos;
	      buffer := next-input-buffer(tk-out);
	      if (buffer.buffer-next == buffer.buffer-end)
		#f;
	      else
		read-stuff(buffer.buffer-next, brackets, new-pieces, quoted?);
	      end if;
	    else
	      // Input is available -- see if it is a special character.  We
	      // keep a count of nested brackets and only exit on a newline
	      // which is outside of the brackets.  Any of special characters
	      // may be quoted by a backslash, in which case it is treated
	      // normally.
	      let ch = buffer[buff-pos];
	      case
		ch == lbrace & ~quoted? =>
		  read-stuff(buff-pos + 1, brackets + 1, pieces, #f);
		ch == rbrace & ~quoted? =>
		  read-stuff(buff-pos + 1, brackets - 1, pieces, #f);
		ch == slash & ~quoted? =>
		  read-stuff(buff-pos + 1, brackets, pieces, #t);
		#if (newlines-are-CRLF)
		ch == carriage-return & ~quoted? & brackets <= 0 =>
		  // same as newline except skips 2 characters (CRLF) 
		  // instead of one (LF).  Asumes 1 LF after every CR
                  // (not a bad assumption, really...)
		  let contents = buffer-subsequence(buffer, <byte-string>,
						    buffer.buffer-next,
						    buff-pos);
		  buffer.buffer-next := buff-pos + 2;
		  release-input-buffer(tk-out);
		  if (empty?(pieces))
		    contents;
		  else
		    apply(concatenate, reverse!(pair(contents, pieces)));
		  end if;
		#endif
		ch == newline & ~quoted? & brackets <= 0 =>
		  // We've finally reached the end.  We therefore grab the
		  // useful text in the buffer and concatenate it with saved
		  // data from before the last "fill-buffer" (if any).
		  let contents = buffer-subsequence(buffer, <byte-string>,
						    buffer.buffer-next,
						    buff-pos);
		  buffer.buffer-next := buff-pos + 1;
		  release-input-buffer(tk-out);
		  if (empty?(pieces))
		    contents;
		  else
		    apply(concatenate, reverse!(pair(contents, pieces)));
		  end if;
		otherwise =>
		  read-stuff(buff-pos + 1, brackets, pieces, #f);
	      end case;
	    end if;
	  end method read-stuff;
    read-stuff(buffer.buffer-next, 0, #(), #f);
  end if;
end method read-tk-line;

// Interprets each string argument as part of a line of tk input, and sends
// then to the running tk process.  Non-strings are converted to strings via
// "tk-as". This routine expects no results and therefore does not wait
// around.
//
define method put-tk-line (#rest strings) => ();
  // This is on the critical path, so do use the fastest known method.
  let buff = get-output-buffer(tk-in);

  for (object in strings)
    let str = tk-as(<string>, object);
//write(*standard-output*, str);
    let sz = str.size;
    if (sz > buff.buffer-end)
      // Hopefully not very common -- let existing code do the work.
      release-output-buffer(tk-in);
      write(tk-in, str);
      buff := get-output-buffer(tk-in);
    else
      if (buff.buffer-end - buff.buffer-next < sz)
	buff := next-output-buffer(tk-in, bytes: sz);
      end if;
      copy-into-buffer!(buff, buff.buffer-next, str);
      buff.buffer-next := buff.buffer-next + sz;
    end if;
  end for;
  if (buff.buffer-next == buff.buffer-end)
    buff := next-output-buffer(tk-in, bytes: 1);
  end if;
  #if (newlines-are-CRLF)
     buff[buff.buffer-next] := carriage-return;
     buff.buffer-next := buff.buffer-next + 1;
  #endif
  buff[buff.buffer-next] := newline;
  buff.buffer-next := buff.buffer-next + 1;
//write(*standard-output*, '\n');
//force-output(*standard-output*);

  force-output-buffers(tk-in);
  synchronize(tk-in);
  release-output-buffer(tk-in);
end method put-tk-line;

