module: New-Streams
author: Ben Folk-Williams, Bill Chiles
synopsis: Reading and writing by lines.
copyright: See below.

//======================================================================
//
// Copyright (c) 1994, 1996  Carnegie Mellon University
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

//// Each of these functions has two versions, depending on newlines-are-CRLF.
//// Edits should (probably) be done to both.
////

/// $newline -- Internal.
///
define constant $newline :: <byte-character> = '\n';
define constant $newline-byte :: <integer> = as(<integer>, $newline);

#if (newlines-are-CRLF)

define constant $return :: <byte-character> = '\r';
define constant $return-byte :: <integer> = as(<integer>, '\r');

#endif

/// read-line -- Exported.
///
define open generic read-line (stream :: <stream>,
			       #key on-end-of-stream :: <object>)
 => (string-or-eof :: <object>, newline? :: <boolean>);

#if (newlines-are-CRLF)

define method read-line (stream :: <buffered-stream>,
			 #key on-end-of-stream :: <object> = $not-supplied)
 => (string-or-eof :: <object>, newline? :: <boolean>);
  block (exit-loop)
    let buf :: false-or(<buffer>) = get-input-buffer(stream);
    if (~buf)
      // Hit eos right away.
      if (on-end-of-stream ~== $not-supplied)
	values(on-end-of-stream, #f);
      else
	error(make(<end-of-stream-error>, stream: stream));
      end if;
    else
      let res-type :: <type> = type-for-sequence(stream.stream-element-type);
      let res :: res-type = make(res-type);
      let collect = method (string :: res-type, buf :: <buffer>,
			    start :: <buffer-index>, stop :: <buffer-index>)
		     => result :: res-type;
		      let str-len = string.size;
		      let buf-len = (stop - start);
		      let res = make(res-type, size: (str-len + buf-len));
		      copy-sequence!(res, 0, string, 0, str-len);
		      copy-sequence!(res, str-len, buf, start, buf-len);
		      res;
		    end;
      while (#t)
	for (i :: <integer> from buf.buffer-next + 1 below buf.buffer-end,
	     until: ((buf[i] == $newline-byte) & (buf[i - 1] == $return-byte)))
	finally
	  if (i == buf.buffer-end)
	    res := collect(res, buf, buf.buffer-next, buf.buffer-end);
	    buf.buffer-next := buf.buffer-end;
	    buf := next-input-buffer(stream);
	    if (~buf) exit-loop(res, #f) end;
	  else
	    res := collect(res, buf, buf.buffer-next, i - 1);
	    // We don't return the newline, but we do consume it.
	    buf.buffer-next := i + 1;
	    exit-loop(res, #t);
	  end;
	end for; 
      end while;
    end if;
  cleanup
    release-input-buffer(stream);
  end block;
end method read-line;

define sealed method read-line (stream :: <simple-sequence-stream>,
				#key on-end-of-stream :: <object>
				       = $not-supplied)
 => (string-or-eof :: <object>, newline? :: <boolean>);
  block ()
    lock-stream(stream);
    check-stream-open(stream);
    check-input-stream(stream);
    if (stream.position == stream.stream-end)
      if (on-end-of-stream ~== $not-supplied)
	values(on-end-of-stream, #f);
      else
	error(make(<end-of-stream-error>, stream: stream));
      end if;
    else
      let newline = as(stream.stream-element-type, $newline);
      let return = as(stream.stream-element-type, $return);
      for (i from (stream.position + 1) below stream.stream-end,
	   until: ((stream.contents[i] = newline)
		     & stream.contents[i - 1] = return))
      finally
	if (i = stream.stream-end)	
	  let num-elts :: <integer> = i - stream.position;
	  let res = make(type-for-copy(stream.contents), size: num-elts);
	  copy-sequence!(res, 0,
			 stream.contents, stream.position,
			 num-elts);
	  stream.position := stream.stream-end;
	  values(res, #f);
	else
	  let num-elts :: <integer> = i - stream.position - 1; // -1 for the CR
	  let res = make(type-for-copy(stream.contents), size: num-elts);
	  copy-sequence!(res, 0,
			 stream.contents, stream.position,
			 num-elts);
	  stream.position := i + 1;
	  values(res, #t);
	end if;
      end for;
    end if;
  cleanup
    unlock-stream(stream);
  end block;
end method read-line;

#else

define method read-line (stream :: <buffered-stream>,
			 #key on-end-of-stream :: <object> = $not-supplied)
 => (string-or-eof :: <object>, newline? :: <boolean>);
  block (exit-loop)
    let buf :: false-or(<buffer>) = get-input-buffer(stream);
    if (~buf)
      // Hit eos right away.
      if (on-end-of-stream ~== $not-supplied)
	values(on-end-of-stream, #f);
      else
	error(make(<end-of-stream-error>, stream: stream));
      end if;
    else
      let res-type :: <type> = type-for-sequence(stream.stream-element-type);
      let res :: res-type = make(res-type);
      let collect = method (string :: res-type, buf :: <buffer>,
			    start :: <buffer-index>, stop :: <buffer-index>)
		     => result :: res-type;
		      let str-len = string.size;
		      let buf-len = (stop - start);
		      let res = make(res-type, size: (str-len + buf-len));
		      copy-sequence!(res, 0, string, 0, str-len);
		      copy-sequence!(res, str-len, buf, start, buf-len);
		      res;
		    end;
      while (#t)
	for (i :: <integer> from buf.buffer-next below buf.buffer-end,
	     until: (buf[i] == $newline-byte))
	finally
	  if (i == buf.buffer-end)
	    res := collect(res, buf, buf.buffer-next, buf.buffer-end);
	    buf.buffer-next := buf.buffer-end;
	    buf := next-input-buffer(stream);
	    if (~buf) exit-loop(res, #f) end;
	  else
	    res := collect(res, buf, buf.buffer-next, i);
	    // We don't return the newline, but we do consume it.
	    buf.buffer-next := i + 1;
	    exit-loop(res, #t);
	  end;
	end for; 
      end while;
    end if;
  cleanup
    release-input-buffer(stream);
  end block;
end method read-line;

define sealed method read-line (stream :: <simple-sequence-stream>,
				#key on-end-of-stream :: <object> 
				       = $not-supplied)
 => (string-or-eof :: <object>, newline? :: <boolean>);
  block ()
    lock-stream(stream);
    check-stream-open(stream);
    check-input-stream(stream);
    if (stream.position == stream.stream-end)
      if (on-end-of-stream ~== $not-supplied)
	values(on-end-of-stream, #f);
      else
	error(make(<end-of-stream-error>, stream: stream));
      end if;
    else
      for (i from stream.position below stream.stream-end,
	   until: stream.contents[i] = as(stream.stream-element-type,
					  $newline))
      finally
	let num-elts :: <integer> = i - stream.position;
	let res = make(type-for-copy(stream.contents), size: num-elts);
	copy-sequence!(res, 0,
		       stream.contents, stream.position,
		       num-elts);
	if (i = stream.stream-end)
	  stream.position := stream.stream-end;
	  values(res, #f);
	else
	  stream.position := i + 1;
	  values(res, #t);
	end if;
      end for;
    end if;
  cleanup
    unlock-stream(stream);
  end block;
end method read-line;

#endif

/// read-line-into! -- Exported.
///
define open generic read-line-into! (stream :: <stream>, string :: <string>,
				    #key start :: <integer>,
				         on-end-of-stream :: <object>,
				         grow? :: <boolean>)
 => (string-or-eof :: <object>, newline? :: <boolean>);

#if (newlines-are-CRLF)

define method read-line-into! (stream :: <buffered-stream>,
			       string :: <string>,
			       #key start :: <integer> = 0,
			            on-end-of-stream :: <object>
				      = $not-supplied,
			            grow? :: <boolean> = #f)
 => (string-or-eof :: <object>, newline? :: <boolean>);
  block (exit-loop)
    let buf :: false-or(<buffer>) = get-input-buffer(stream);
    if (~buf)
      if (on-end-of-stream ~== $not-supplied)
	values(on-end-of-stream, #f);
      else
	error(make(<end-of-stream-error>, stream: stream));
      end if;
    end if;
    let str-next :: <integer> = start;
    let str-end :: <integer> = string.size;
    while (#t)
      for (i :: <integer> from (buf.buffer-next + 1) below buf.buffer-end,
	   until: ((buf[i] == $newline-byte) & (buf[i - 1] == $return-byte)))
      finally
	let num-elts :: <integer> = if (i == buf.buffer-end)
				      i - buf.buffer-next;
				    else
				      // the last 1 is the return-byte
				      i - buf.buffer-next - 1;
				    end if;
	let str-size :: <integer> = str-next + num-elts;
	if (str-size > str-end)
	  if (grow?)
	    if (instance?(string, <stretchy-collection>))
	      string.size := str-size;
	    else
	      string := make(type-for-copy(string), size: str-size);
	    end if;
	  else
	    error("String %= not large enough to hold next line from stream %=", string, stream);
	  end if;
	end if;
	copy-sequence!(string, str-next, buf, buf.buffer-next, num-elts);
	if (i == buf.buffer-end)
	  buf.buffer-next := buf.buffer-end;
	  buf := next-input-buffer(stream);
	  if (~buf)
	    exit-loop(string, #f);
	  end if;
	  str-next := str-size;
	else
	  buf.buffer-next := buf.buffer-next + num-elts + 2; // +2 for newline
	  exit-loop(string, #t);
	end if;
      end for;
    end while;
  cleanup
    release-input-buffer(stream);
  end block;
end method read-line-into!;

/// This dies if string cannot hold {stream.stream-element-type}s
///
define sealed method read-line-into! (stream :: <simple-sequence-stream>,
				      string :: <string>,
				      #key start :: <integer> = 0,
				           on-end-of-stream :: <object>
					     = $not-supplied,
				           grow? :: <boolean> = #f)
 => (string-or-eof :: <object>, newline? :: <boolean>);
  block ()
    lock-stream(stream);
    check-stream-open(stream);
    check-input-stream(stream);
    if (stream.position == stream.stream-end)
      if (on-end-of-stream ~== $not-supplied)
	values(on-end-of-stream, #f);
      else
	error(make(<end-of-stream-error>, stream: stream));
      end if;
    else
      let newline = as(stream.stream-element-type, $newline);
      let return = as(stream.stream-element-type, $return);
      for (i from (stream.position + 1) below stream.stream-end,
	   until: ((stream.contents[i] = newline)
		     & stream.contents[i - 1] = return))
      finally
	let num-elts :: <integer> = if (i == stream.stream-end)
				      i - stream.position;
				    else
				      i - stream.position - 1;
				    end if;
	let str-size :: <integer> = start + num-elts;
	if (str-size > string.size)
	  if (grow?)
	    if (instance?(string, <stretchy-collection>))
	      string.size := str-size;
	    else
	      string := make(type-for-copy(string), size: str-size);
	    end if;
	  else
	    error("String %= not large enough to hold next line from stream %=", string, stream);
	  end if;
	end if;
	copy-sequence!(string, start,
		       stream.contents, stream.position,
		       num-elts);
	if (i = stream.stream-end)	
	  stream.position := stream.stream-end;
	  values(string, #f);
	else
	  stream.position := i + 1;
	  values(string, #t);
	end if;
      end for;
    end if;    
  cleanup
    unlock-stream(stream);
  end block;
end method read-line-into!;

#else

define method read-line-into! (stream :: <buffered-stream>,
			       string :: <string>,
			       #key start :: <integer> = 0,
			            on-end-of-stream :: <object>
				      = $not-supplied,
			            grow? :: <boolean> = #f)
 => (string-or-eof :: <object>, newline? :: <boolean>);
  block (exit-loop)
    let buf :: false-or(<buffer>) = get-input-buffer(stream);
    if (~buf)
      if (on-end-of-stream ~== $not-supplied)
	values(on-end-of-stream, #f);
      else
	error(make(<end-of-stream-error>, stream: stream));
      end if;
    end if;
    let str-next :: <integer> = start;
    let str-end :: <integer> = string.size;
    while (#t)
      for (i :: <integer> from buf.buffer-next below buf.buffer-end,
	   until: (buf[i] == $newline-byte))
      finally
	let num-elts :: <integer> = i - buf.buffer-next;
	let str-size :: <integer> = str-next + num-elts;
	if (str-size > str-end)
	  if (grow?)
	    if (instance?(string, <stretchy-collection>))
	      string.size := str-size;
	    else
	      string := make(type-for-copy(string), size: str-size);
	    end if;
	  else
	    error("String %= not large enough to hold next line from stream %=", string, stream);
	  end if;
	end if;
	copy-sequence!(string, str-next, buf, buf.buffer-next, num-elts);
	if (i == buf.buffer-end)
	  buf.buffer-next := buf.buffer-end;
	  buf := next-input-buffer(stream);
	  if (~buf)
	    exit-loop(string, #f);
	  end if;
	  str-next := str-size;
	else
	  buf.buffer-next := buf.buffer-next + num-elts + 1;
	  exit-loop(string, #t);
	end if;
      end for;
    end while;
  cleanup
    release-input-buffer(stream);
  end block;
end method read-line-into!;

/// This dies if string cannot hold {stream.stream-element-type}s
///
define sealed method read-line-into! (stream :: <simple-sequence-stream>,
				      string :: <string>,
				      #key start :: <integer> = 0,
				           on-end-of-stream :: <object>
					     = $not-supplied,
				           grow? :: <boolean> = #f)
 => (string-or-eof :: <object>, newline? :: <boolean>);
  block ()
    lock-stream(stream);
    check-stream-open(stream);
    check-input-stream(stream);
    if (stream.position == stream.stream-end)
      if (on-end-of-stream ~== $not-supplied)
	values(on-end-of-stream, #f);
      else
	error(make(<end-of-stream-error>, stream: stream));
      end if;
    else
      for (i from stream.position below stream.stream-end,
	   until: stream.contents[i] = $newline)
      finally
	let num-elts :: <integer> = i - stream.position;
	let str-size :: <integer> = start + num-elts;
	if (str-size > string.size)
	  if (grow?)
	    if (instance?(string, <stretchy-collection>))
	      string.size := str-size;
	    else
	      string := make(type-for-copy(string), size: str-size);
	    end if;
	  else
	    error("String %= not large enough to hold next line from stream %=", string, stream);
	  end if;
	end if;
	copy-sequence!(string, start,
		       stream.contents, stream.position,
		       num-elts);
	if (i = stream.stream-end)
	  stream.position := stream.stream-end;
	  values(string, #f);
	else
	  stream.position := i + 1;
	  values(string, #t);
	end if;
      end for;
    end if;    
  cleanup
    unlock-stream(stream);
  end block;
end method read-line-into!;

#endif

/// write-line -- Exported.
///
define open generic write-line (stream :: <stream>, string :: <string>,
				#key start :: <integer>,
			             end: stop :: <integer>)
 => ();

#if (newlines-are-CRLF)

define method write-line (stream :: <buffered-stream>, 
			  string :: <string>,
			  #key start :: <integer> = 0,
			       end: stop :: <integer> = string.size)
 => ();
  block (exit-loop)
    let buf :: <buffer> = get-output-buffer(stream);
    let buf-capacity :: <buffer-index> = (buf.buffer-end - buf.buffer-next);
    let buf-start :: <buffer-index> = buf.buffer-next;
    let partial-stop :: <integer> = (start + buf-capacity);
    while (#t)
      if (partial-stop >= (stop + 2)) // +2 for the newline
	let this-copy :: <integer> = (stop - start);
	copy-sequence!(buf, buf-start, string, start, this-copy);
	buf.buffer-next := buf-start + this-copy;
	buf[buf.buffer-next] := $return-byte;
	buf.buffer-next := buf.buffer-next + 1;
	buf[buf.buffer-next] := $newline-byte;
	buf.buffer-next := buf.buffer-next + 1;
	exit-loop();
      else
	copy-sequence!(buf, buf-start, string, start, buf-capacity);
	buf.buffer-next := buf.buffer-end;
	buf := next-output-buffer(stream);
	buf-capacity := buf.buffer-end - buf.buffer-next;
	buf-start := buf.buffer-next;
	start := partial-stop;
	partial-stop := partial-stop + buf-capacity;
      end if;
    end while;
  cleanup
    release-output-buffer(stream);
  end block;
end method write-line;

define sealed method write-line (stream :: <simple-sequence-stream>,
				 string :: <string>,
				 #key start :: <integer> = 0,
				      end: stop :: <integer> = string.size)
 => ();
  block ()
    lock-stream(stream);
    check-stream-open(stream);
    check-output-stream(stream);
    let needed :: <integer> = stop - start + 2; // + 2 for the newline
    let available :: <integer> = stream.contents.size - stream.position;
    let new-pos :: <integer> = stream.position + needed;
    if (needed > available)
      grow-stream-sequence!(stream, new-pos);
      stream.stream-end := new-pos;
    elseif (new-pos > stream.stream-end)
      stream.stream-end := new-pos;
    end if;
    copy-sequence!(stream.contents, stream.position,
		   string, start,
		   needed - 2); // The last 2 are the newline
    stream.contents[new-pos - 2] := as(stream.stream-element-type, $return);
    stream.contents[new-pos - 1] := as(stream.stream-element-type, $newline);
    stream.position := new-pos;
  cleanup
    unlock-stream(stream);
  end block;
end method write-line;

#else

define method write-line (stream :: <buffered-stream>, 
			  string :: <string>,
			  #key start :: <integer> = 0,
			       end: stop :: <integer> = string.size)
 => ();
  block (exit-loop)
    let buf :: <buffer> = get-output-buffer(stream);
    let buf-capacity :: <buffer-index> = (buf.buffer-end - buf.buffer-next);
    let buf-start :: <buffer-index> = buf.buffer-next;
    let partial-stop :: <integer> = (start + buf-capacity);
    while (#t)
      if (partial-stop >= (stop + 1)) // +1 for the newline
	let this-copy :: <integer> = (stop - start);
	copy-sequence!(buf, buf-start, string, start, this-copy);
	buf.buffer-next := buf-start + this-copy;
	buf[buf.buffer-next] := $newline-byte;
	buf.buffer-next := buf.buffer-next + 1;
	exit-loop();
      else
	copy-sequence!(buf, buf-start, string, start, buf-capacity);
	buf.buffer-next := buf.buffer-end;
	buf := next-output-buffer(stream);
	buf-capacity := buf.buffer-end - buf.buffer-next;
	buf-start := buf.buffer-next;
	start := partial-stop;
	partial-stop := partial-stop + buf-capacity;
      end if;
    end while;
  cleanup
    release-output-buffer(stream);
  end block;
end method write-line;

define sealed method write-line (stream :: <simple-sequence-stream>,
				 string :: <string>,
				 #key start :: <integer> = 0,
				      end: stop :: <integer> = string.size)
 => ();
  block ()
    lock-stream(stream);
    check-stream-open(stream);
    check-output-stream(stream);
    let needed :: <integer> = stop - start + 1; // + 1 for the newline
    let available :: <integer> = stream.contents.size - stream.position;
    let new-pos :: <integer> = stream.position + needed;
    if (needed > available)
      grow-stream-sequence!(stream, new-pos);
      stream.stream-end := new-pos;
    elseif (new-pos > stream.stream-end)
      stream.stream-end := new-pos;
    end if;
    copy-sequence!(stream.contents, stream.position,
		   string, start,
		   needed - 1); // The last 1 is the newline
    stream.contents[new-pos - 1] := as(stream.stream-element-type, $newline);
    stream.position := new-pos;
  cleanup
    unlock-stream(stream);
  end block;
end method write-line;

#endif

/// new-line -- Exported.
///
define open generic new-line (stream :: <stream>) => ();

#if (newlines-are-CRLF)

define inline  method new-line (stream :: <stream>) => ();
  // Could make this a bit more efficient for <buffered-stream>s.
  write-element(stream, as(stream.stream-element-type, $return));
  write-element(stream, as(stream.stream-element-type, $newline));
end method new-line;

#else

define inline method new-line (stream :: <stream>) => ();
  write-element(stream, as(stream.stream-element-type, $newline));
end method new-line;

#endif

define sealed domain new-line (<simple-sequence-stream>);
define sealed domain new-line (<fd-file-stream>);
