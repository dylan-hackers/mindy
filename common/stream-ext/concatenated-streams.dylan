module: concatenated-streams
author: Nick Kramer
rcs-header: $Header: /scm/cvs/src/common/stream-ext/concatenated-streams.dylan,v 1.1 1998/05/03 19:55:03 andreas Exp $

//======================================================================
//
// Copyright (c) 1996  Carnegie Mellon University
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

// An input stream which is a wrapper for several input streams.
// Reading from a <concatenated-stream> is equivalent to reading from
// the first component stream, then on eof silently rolling over into
// the second stream, etc.  Because wrapper-streams are broken, there
// must be at least one component stream.  (ie, because the
// inner-stream slot only accepts <stream> rather than
// false-or(<stream>)...)
//
define sealed class <concatenated-stream> (<wrapper-stream>)
  // get-next-stream takes no args and returns false-or(<stream>), the
  // false being if there are no more streams.  close(<combo-stream>)
  // will change this slot to be a function that returns a "stream
  // closed you moron" error.
  slot get-next-component-stream :: <function>, 
    required-init-keyword: #"get-next-stream";
  //
  // ### This next slot is a kluge for the fact that inner-stream
  // won't take #f as a legal value.  We set inner-stream-is-bogus to
  // #t whenever we'd like to set inner-stream to #f.
  slot inner-stream-is-bogus? :: <boolean> = #f;
end class <concatenated-stream>;

// This make method is the only reason we've sealed
// <concatenated-stream>.  If someone subclasses us, they'll never get
// all these nifty constructors...
//
// For streams: and locators:, we make sure that the sequence they
// pass us is full of instances of the correct type (ie, <stream>s or
// <string>s, respectively).  We can't really do much more than that,
// though, unless we want to screw with the component streams before
// it's time to read from them.
//
define method make (cls == <concatenated-stream>, #next next-method, 
		    #key get-next-stream :: false-or(<function>),
		    streams :: false-or(<sequence>), // seq of streams
		    locators :: false-or(<sequence>)) // seq of strings
 => stream :: <concatenated-stream>;
  local method cant-do-that () => ();
	  error("Can only use one of get-next-stream:, streams:, and\n"
		  "locators: when creating a <concatenated-stream>.");
	end method cant-do-that;
  let next-stream-function 
    = if (get-next-stream)
	if (streams | locators) cant-do-that(); else get-next-stream; end if;
      elseif (streams)
	if (locators)
	  cant-do-that(); 
	else 
	  if (~ every?(rcurry(instance?, <stream>), streams))
	    error("Your streams (%=) are not really all streams", streams);
	  end if;
	  let index = -1; // close over this variable
	  method () => stream :: false-or(<file-stream>);
	    if (index < locators.size)
	      index := index + 1;
	      make(<file-stream>, locator: locators[index]);
	    else
	      #f;
	    end if;
	  end method;
	end if;
      elseif (locators)
	if (~ every?(rcurry(instance?, <string>), locators))
	  error("Your locators (%=) are not all strings", locators);
	end if;
	let index = 0; // close over this variable
	method () => stream :: false-or(<file-stream>);
	  if (index < locators.size)
	    let res = make(<file-stream>, locator: locators[index]);
	    index := index + 1;
	    res;
	  else
	    #f;
	  end if;
	end method;
      else // ~get-next-stream & ~streams & ~locators
	error("Must specify one of get-next-stream:, streams:, and\n"
		"locators: when creating a <concatenated-stream>.");
      end if;

  next-method(cls, get-next-stream: next-stream-function,
	      inner-stream: next-stream-function());
end method make;

define sealed domain make (singleton(<concatenated-stream>));
define sealed domain initialize (<concatenated-stream>);

// Gets the next component stream of the concatenated stream
//
define inline function move-to-next-stream
    (stream :: <concatenated-stream>) => ();
  close(stream.inner-stream);
  let next-component = stream.get-next-component-stream();
  if (next-component == #f)
    stream.inner-stream-is-bogus? := #t;
  else
    stream.inner-stream := next-component;
    stream.inner-stream.outer-stream := stream;
  end if;
end function move-to-next-stream;

// ### Might be better as a macro, but we don't have those in Mindy.
// Implements the actual rolling over of component streams.
//
define inline function do-combo-stream-function
    (func :: <function>, stream :: <concatenated-stream>, 
     on-end-of-stream :: <object>, #rest args)
 => (#rest values :: <object>);
  block (return)
    while (#t)
      if (stream.inner-stream-is-bogus?)
	if (on-end-of-stream == $not-supplied)
	  signal(make(<end-of-stream-error>, stream: stream));
	else
	  return(on-end-of-stream);
	end if;
      end if;
      block ()
	if (on-end-of-stream == $not-supplied)
	  return(apply(func, stream.inner-stream, args));
	else
	  let args 
	    = concatenate(args, vector(on-end-of-stream: on-end-of-stream));
	  return(apply(func, stream.inner-stream, args));
	end if;
      exception (<end-of-stream-error>)
	move-to-next-stream(stream);
      end block;
    end while;
  end block;
end function do-combo-stream-function;

define inline method close 
    (stream :: <concatenated-stream>, #rest keys, #all-keys) => ();
  stream.get-next-component-stream 
    := method () error("Combo-stream was closed!");  end method;
  apply(close, stream.inner-stream, keys);
  stream.inner-stream-is-bogus? := #t;
end method;

define inline method stream-open? (stream :: <concatenated-stream>)
 => open? :: <boolean>;
  ~stream.inner-stream-is-bogus?;
end method;

// ### We have to assume that all the component streams have the same
// element type
//
define inline method stream-element-type (stream :: <concatenated-stream>) 
 => element-type :: <type>;
  stream-element-type(stream.inner-stream);
end method;

// ### This ain't right, and at the moment I don't even care.
//
define inline method stream-at-end? (stream :: <concatenated-stream>)
 => at-end? :: <boolean>;
  stream-at-end?(stream.inner-stream);
end method;

define inline method read-element
    (stream :: <concatenated-stream>,
     #key on-end-of-stream :: <object> = $not-supplied)
 => element-or-eof :: <object>;
  do-combo-stream-function(read-element, stream, on-end-of-stream);
end method;

// wrapper-stream has no unread-element, so neither do we.  Besides,
// it'd probably be a real bitch to implement...

define inline method peek (stream :: <concatenated-stream>,
			   #key on-end-of-stream :: <object> = $not-supplied)
 => element-of-eof :: <object>;
  do-combo-stream-function(peek, stream, on-end-of-stream);
end method;

define inline method read (stream :: <concatenated-stream>, n :: <integer>,
			   #key on-end-of-stream :: <object> = $not-supplied)
 => sequence-or-eof :: <object>;
  do-combo-stream-function(read, stream, on-end-of-stream, n);
end method;

define inline method read-into!
    (stream :: <concatenated-stream>, n :: <integer>,
     sequence :: <mutable-sequence>,
     #key start ::  <integer> = 0,
          on-end-of-stream :: <object> = $not-supplied)
 => count-or-eof :: <object>;
  do-combo-stream-function(read-into!, stream, on-end-of-stream, 
			   n, sequence, start: start);
end method;

// ### See also stream-at-end?
//
define inline method stream-input-available? (stream :: <concatenated-stream>)
 => input-available? :: <boolean>;
  stream-input-available?(stream.inner-stream);
end method;

define inline method read-line 
    (stream :: <concatenated-stream>,
     #key on-end-of-stream :: <object> = $not-supplied)
 => (string-or-eof :: <object>, newline? :: <boolean>);
  do-combo-stream-function(read-line, stream, on-end-of-stream);
end method;

define inline method read-line-into!
    (stream :: <concatenated-stream>, string :: <string>,
     #key start :: <integer> = 0, grow? :: <boolean> = #f,
     on-end-of-stream :: <object> = $not-supplied)
 => (string-or-eof :: <object>, newline? :: <boolean>);
  do-combo-stream-function(read-line-into!, stream, on-end-of-stream, string, 
			   start: start, grow?: grow?);
end method;
