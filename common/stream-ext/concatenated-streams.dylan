module: combination-streams
author: Nick Kramer
rcs-header: $header$

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
// the second stream, etc.
//
define open class <combination-stream> (<wrapper-stream>)
  // get-next-stream takes no args and returns false-or(<stream>), the
  // false being if there are no more streams.
  slot get-next-component-stream :: <function>, 
    required-init-keyword: #"get-next-stream";
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
  stream.inner-stream := stream.get-next-component-stream();
  stream.inner-stream.outer-stream := stream;
end function move-to-next-stream;

// ### Might be better as a macro, but we don't have those in Mindy
//
define inline function do-combo-stream-function
    (stream :: <combination-stream>, on-end-of-stream :: <object>,
     func :: <function>, #rest args)
 => (#rest values :: <object>);
  block (return)
    while (#t)
      if (stream.inner-stream = #f)
	if (on-end-of-stream == $not-supplied)
	  signal(make(<end-of-stream-error>, stream: stream));
	else
	  return(on-end-of-stream);
	end if;
      end if;
      block ()
	return(apply(func, args));
      exception (<end-of-stream-error>)
	move-to-next-stream(stream);
      end block;
    end while;
  end block;
end function do-combo-stream-function;

define method read-element 
    (stream :: <combination-stream>, #key on-end-of-stream = $not-supplied)
 => elt :: <object>;
  do-combo-stream-function(stream, on-end-of-stream, read-element, stream);
end method read-element;

define method peek-element 
    (stream :: <combination-stream>, #key on-end-of-stream = $not-supplied)
 => elt :: <object>;
  do-combo-stream-function(stream, on-end-of-stream, peek-element, stream);
end method peek-element;

// I'm not so sure that <wrapper-stream> provides useful implementations
// of anything.  Here's what I hope it provides:

// ### Use wrapper-stream version of unread-element
// ### Use wrapper-stream version of read
// ### Use wrapper-stream version of read-into!
// ### Use wrapper-stream version of discard-input
// ### Use wrapper-stream version of stream-input-availabe
// ### Use wrapper-stream version of close


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

