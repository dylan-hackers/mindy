module: combination-streams
author: Nick Kramer
rcs-header: $Header: /home/housel/work/rcs/gd/src/common/stream-ext/concatenated-streams.dylan,v 1.4 1996/09/15 15:38:29 nkramer Exp $

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
// Reading from a <combination-stream> is equivalent to reading from
// the first component stream, then on eof silently rolling over into
// the second stream, etc.  Because wrapper-streams are broken, there
// must be at least one component stream.  (ie, because the
// inner-stream slot only accepts <stream> rather than
// false-or(<stream>)...)
//
define open class <combination-stream> (<wrapper-stream>)
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
end class <combination-stream>;

define method make (cls == <combination-stream>, #next next-method, 
		    #key get-next-stream)
 => stream :: <combination-stream>;
  next-method(cls, get-next-stream: get-next-stream, 
	      inner-stream: get-next-stream());
end method make;

define sealed domain make (singleton(<combination-stream>));
define sealed domain initialize (<combination-stream>);

// Gets the next component stream of the combination stream
//
define inline function move-to-next-stream
    (stream :: <combination-stream>) => ();
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
    (func :: <function>, stream :: <combination-stream>, 
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
    (stream :: <combination-stream>, #rest keys, #all-keys) => ();
  stream.get-next-component-stream 
    := method () error("Combo-stream was closed!");  end method;
  apply(close, stream.inner-stream, keys);
  stream.inner-stream-is-bogus? := #t;
end method;

define inline method stream-open? (stream :: <combination-stream>)
 => open? :: <boolean>;
  ~stream.inner-stream-is-bogus?;
end method;

// ### We have to assume that all the component streams have the same
// element type
//
define inline method stream-element-type (stream :: <combination-stream>) 
 => element-type :: <type>;
  stream-element-type(stream.inner-stream);
end method;

// ### This ain't right, and at the moment I don't even care.
//
define inline method stream-at-end? (stream :: <combination-stream>)
 => at-end? :: <boolean>;
  stream-at-end?(stream.inner-stream);
end method;

define inline method read-element
    (stream :: <combination-stream>,
     #key on-end-of-stream :: <object> = $not-supplied)
 => element-or-eof :: <object>;
  do-combo-stream-function(read-element, stream, on-end-of-stream);
end method;

// wrapper-stream has no unread-element, so neither do we.  Besides,
// it'd probably be a real bitch to implement...

define inline method peek (stream :: <combination-stream>,
			   #key on-end-of-stream :: <object> = $not-supplied)
 => element-of-eof :: <object>;
  do-combo-stream-function(peek, stream, on-end-of-stream);
end method;

define inline method read (stream :: <combination-stream>, n :: <integer>,
			   #key on-end-of-stream :: <object> = $not-supplied)
 => sequence-or-eof :: <object>;
  do-combo-stream-function(read, stream, on-end-of-stream, n);
end method;

define inline method read-into!
    (stream :: <combination-stream>, n :: <integer>,
     sequence :: <mutable-sequence>,
     #key start ::  <integer> = 0,
          on-end-of-stream :: <object> = $not-supplied)
 => count-or-eof :: <object>;
  do-combo-stream-function(read-into!, stream, on-end-of-stream, 
			   n, sequence, start: start);
end method;

// ### See also stream-at-end?
//
define inline method stream-input-available? (stream :: <combination-stream>)
 => input-available? :: <boolean>;
  stream-input-available?(stream.inner-stream);
end method;

define inline method read-line 
    (stream :: <combination-stream>,
     #key on-end-of-stream :: <object> = $not-supplied)
 => (string-or-eof :: <object>, newline? :: <boolean>);
  do-combo-stream-function(read-line, stream, on-end-of-stream);
end method;

define inline method read-line-into!
    (stream :: <combination-stream>, string :: <string>,
     #key start :: <integer> = 0, grow? :: <boolean> = #f,
     on-end-of-stream :: <object> = $not-supplied)
 => (string-or-eof :: <object>, newline? :: <boolean>);
  do-combo-stream-function(read-line-into!, stream, on-end-of-stream, string, 
			   start: start, grow?: grow?);
end method;


// ### This class is no longer used, but since I wrote it, I'm keeping
// it...

// This class really just provides an alterate make function for
// <combination-stream>.  Perhaps some day we could add full
// <file-stream> capabilities to this.  Takes keyword files:, which is
// a sequence of strings (each string being a file name).  Files are
// opened one at a time, so as not to use up all available file
// descriptors.  The downside is that this we won't notice an invalid
// file name until we're about ready to read that file.
//
define class <combination-file-stream> (<combination-stream>)
end class <combination-file-stream>;

define method make (cls == <combination-file-stream>, #next next-method,
		    #key files)
 => stream :: <combination-file-stream>;
  // close over the next two variables
  let index = 0;
  let filenames-vector = as(<simple-object-vector>, files);
  local method get-next-stream () => stream :: false-or(<file-stream>);
	  if (index < filenames-vector.size)
	    index := index + 1;
	    make(<file-stream>, locator: filenames-vector[index]);
	  else
	    #f;
	  end if;
	end method get-next-stream;

  next-method(cls, get-next-stream: get-next-stream);
end method make;
				      
define sealed domain make (singleton(<combination-file-stream>));
define sealed domain initialize (<combination-file-stream>);

