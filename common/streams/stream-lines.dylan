module: Streams
author: Ben Folk-Williams, Bill Chiles
synopsis: Reading and writing by lines.
copyright: See below.
rcs-header: $Header: /scm/cvs/src/common/streams/stream-lines.dylan,v 1.1 1998/05/03 19:55:04 andreas Exp $

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

/// $newline, $newline-byte -- Internal.
///
define constant $newline :: <byte-character> = '\n';
define constant $newline-byte :: <integer> = as(<integer>, $newline);

#if (newlines-are-CRLF)

/// $return, $return-byte -- Internal.
///
define constant $return :: <byte-character> = '\r';
define constant $return-byte :: <integer> = as(<integer>, '\r');

/// $newline-size -- Internal.
///
define constant $newline-size :: <integer> = 2;

#else

/// $newline-size -- Internal.
///
define constant $newline-size :: <integer> = 1;

#endif

/// read-line -- Exported.
///
define open generic read-line (stream :: <stream>,
			       #key on-end-of-stream :: <object>)
 => (string-or-eof :: <object>, newline? :: <boolean>);

define method read-line (stream :: <buffered-stream>,
			 #key on-end-of-stream :: <object> = $not-supplied)
 => (string-or-eof :: <object>, newline? :: <boolean>);
  block (exit-loop)
    let buf :: false-or(<buffer>) = get-input-buffer(stream);
    if (~ buf)
      // Hit eos right away.
      if (on-end-of-stream ~== $not-supplied)
	values(on-end-of-stream, #f);
      else
	error(make(<end-of-stream-error>, stream: stream));
      end if;
    else
      let buf-next :: <buffer-index> = buf.buffer-next;
      let buf-end :: <buffer-index> = buf.buffer-end;
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
	for (i :: <integer> from buf-next below buf-end,
	     until: (buf[i] == $newline-byte))
	finally
	  #if (newlines-are-CRLF)	  
	     let CR-pos :: <buffer-index> = i - 1;
	     let line-end
	       = if ((CR-pos >= buf-next) & (buf[CR-pos] == $return-byte))
		   CR-pos;
		 else
		   i;
		 end;
	     res := collect(res, buf, buf-next, line-end);
	  #else
	     res := collect(res, buf, buf-next, i);
	  #endif
	  if (i == buf-end)
	    buf.buffer-next := buf-end;
	    buf := next-input-buffer(stream);
	    if (~ buf) exit-loop(res, #f) end;
	    buf-end := buf.buffer-end;
	    buf-next := buf.buffer-next;
	  else
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

define sealed domain read-line(<fd-stream>);
define sealed domain read-line(<buffered-byte-string-output-stream>);

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
      for (i from (stream.position + 1) below stream.stream-end,
	   until: (stream.contents[i] == newline))
      finally
	#if (newlines-are-CRLF)	  
	   let return = as(stream.stream-element-type, $return);
	   let CR-pos :: <integer> = i - 1;
	   let line-size
	     = if ((CR-pos >= stream.position)
		     & (stream.contents[CR-pos] == return))
		 CR-pos - stream.position;
	       else
		 i - stream.position;
	       end;
	#else
	   let line-size = i - stream.position;
	#endif
	let res-type :: <type> = type-for-copy(stream.contents);
	let res :: res-type = make(res-type, size: line-size);
	copy-sequence!(res, 0, stream.contents, stream.position, line-size);
	if (i == stream.stream-end)	
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

/// read-line-into! -- Exported.
///
define open generic read-line-into! (stream :: <stream>, string :: <string>,
				    #key start :: <integer>,
				         on-end-of-stream :: <object>,
				         grow? :: <boolean>)
 => (string-or-eof :: <object>, newline? :: <boolean>);

/// I wrote slightly more efficient versions of this, which read directly
/// into string. They turned out to be a pain in the ass to maintain
/// though. (You need 4 versions, <buffered-stream>,<simple-sequence-stream>
/// cross newlines-are-CRLF.) They're in RCS rev 1.3, if there's a need...
///
define method read-line-into! (stream :: <stream>, string :: <string>,
                               #key start :: <integer> = 0,
			            on-end-of-stream :: <object> 
				      = $not-supplied,
			            grow? :: <boolean> = #f)
 => (string-or-eof :: <object>, newline? :: <boolean>);
  block(exit)
    let (one-line, newline?)
      = read-line(stream, on-end-of-stream: on-end-of-stream);
    if (grow?)
      exit(replace-subsequence!(string, one-line, start: start), newline?);
    else
      if (one-line.size > string.size - start)
	error("Supplied string not large enough to hold next line from stream %=",
	       stream);
      else
        exit(replace-subsequence!(string, one-line, start: start,
                                  end: start + one-line.size),
	     newline?);
      end if;
    end if;
  end block;
end method read-line-into!;

define sealed domain read-line-into!(<fd-stream>, <string>);
define sealed domain read-line-into!(<simple-sequence-stream>, <string>);
define sealed domain read-line-into!(<buffered-byte-string-output-stream>,
				     <string>);

/// write-line -- Exported.
///
define open generic write-line (stream :: <stream>, string :: <string>,
				#key start :: <integer>,
			             end: stop :: <integer>)
 => ();

define method write-line (stream :: <buffered-stream>, 
			  string :: <string>,
			  #key start :: <integer> = 0,
			       end: stop :: <integer> = string.size)
 => ();
  block (exit-loop)
    let buf :: <buffer> = get-output-buffer(stream);
    let buf-start :: <buffer-index> = buf.buffer-next;
    let buf-capacity :: <buffer-index> = (buf.buffer-end - buf-start);
    let partial-stop :: <integer> = (start + buf-capacity);
    while (#t)
      if (partial-stop >= (stop + $newline-size))
	let this-copy :: <integer> = (stop - start);
	copy-sequence!(buf, buf-start, string, start, this-copy);
	buf.buffer-next := buf-start + this-copy;
	#if (newlines-are-CRLF)
	   buf[buf.buffer-next] := $return-byte;
	   buf.buffer-next := buf.buffer-next + 1;
	#endif
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

define sealed domain write-line(<fd-stream>, <string>);
define sealed domain write-line(<buffered-byte-string-output-stream>,
				<string>);

define sealed method write-line (stream :: <simple-sequence-stream>,
				 string :: <string>,
				 #key start :: <integer> = 0,
				      end: stop :: <integer> = string.size)
 => ();
  block ()
    lock-stream(stream);
    check-stream-open(stream);
    check-output-stream(stream);
    let needed :: <integer> = stop - start + $newline-size;
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
		   needed - $newline-size);
    #if (newlines-are-CRLF)
       stream.contents[new-pos - 2] := as(stream.stream-element-type, $return);
    #endif
    stream.contents[new-pos - 1] := as(stream.stream-element-type, $newline);
    stream.position := new-pos;
  cleanup
    unlock-stream(stream);
  end block;
end method write-line;

/// new-line -- Exported.
///
define open generic new-line (stream :: <stream>) => ();

#if (newlines-are-CRLF)

define method new-line (stream :: <buffered-stream>) => ();
  let buf = get-output-buffer(stream, bytes: 2);
  let buf-next = buf.buffer-next;
  buf[buf-next] := $return-byte;
  buf[buf-next + 1] := $newline-byte;
  buf.buffer-next := buf-next + 2;
  release-output-buffer(stream);
end method new-line;

define sealed method new-line (stream :: <simple-sequence-stream>) => ();
  block ()
    lock-stream(stream);
    check-stream-open(stream);
    check-output-stream(stream);
    if (stream.stream-end >= (stream.contents.size - 1))
      grow-stream-sequence!(stream, 
			    stream.contents.size + $default-grow-amount);
    end if;
    let pos :: <integer> = stream.position;
    stream.contents[pos] := as(stream.stream-element-type, $return);
    stream.contents[pos + 1] := as(stream.stream-element-type, $newline);
    stream.position := pos + 2;
    if (stream.stream-end < stream.position) 
      stream.stream-end := stream.position;
    end if;
  cleanup
    unlock-stream(stream);
  end block;  
end method new-line;

#else

define inline method new-line (stream :: <buffered-stream>)
 => ();
  write-element(stream, $newline-byte);
end method new-line;

define sealed inline method new-line (stream :: <simple-sequence-stream>)
 => ();
  write-element(stream, as(stream.stream-element-type, $newline));
end method new-line;

#endif

define sealed domain new-line (<fd-stream>);
define sealed domain new-line (<buffered-byte-string-output-stream>);
