module: Streams
author: Ben Folk-Williams
synopsis: Creating streams, Querying, Positionable Stream Protocol, Locking.
copyright: See below.
RCS-header: $Header: /scm/cvs/src/common/streams/streams.dylan,v 1.8 2003/04/11 20:12:19 andreas Exp $

//======================================================================
//
// Copyright (c) 1996  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
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


//// Right now this file is chopped up with #if's for workarounds to the
//// current d2c not dealing with singleton({<byte>, <byte-character>}). 
//// Once it does, the mindy versions of everything should be used.
//// Check file-streams.dylan, stream-writing.dylan, new-internals.dylan
//// for the same.
////

//// Constants.
////

define constant $default-buffer-size = 1024 * 64;

// Better delete this if we ever actually have a locators library!
//
define constant <locator> = <byte-string>;

// $not-supplied -- Internal.
// Used with on-end-of-stream keyword.
//
define constant $not-supplied = "not-supplied-no-siree";

// In mindy and d2c <character> is equivalent to unicode character.
//
define constant <unicode-character> = <character>;


//// Classes.
//// 

/// <stream> -- Exported.
///
/// All other streams inherit from this class.
///

define open abstract primary class <stream> (<object>)
  slot stream-lock :: <multilock> = make(<multilock>);
  slot outer-stream :: <stream>, init-keyword: outer-stream:;
end class <stream>;

/// <buffered-stream> -- Exported.
///

define open abstract primary class <buffered-stream> (<stream>)
  slot buffer-held? :: <boolean> = #f;
end class <buffered-stream>;

/// <positionable-stream> -- Exported.
///
define open abstract class <positionable-stream> (<stream>)
end class;

/// <file-stream> -- Exported.
///
define open class <file-stream> (<buffered-stream>, <positionable-stream>)
end class;

define open class <sequence-stream> (<positionable-stream>)
end class;

/// <string-stream> -- Exported.
///
define open class <string-stream> (<sequence-stream>)
end class;

/// <simple-sequence-stream> -- Internal.
/// This is the class on which the generic implementation for <sequence-stream>
/// is built.
/// 
/// contents: is required for make(<simple-sequence-stream>), only because
/// there's no obvious choice for a default sequence. Subclasses
/// have default values for contents, e.g. for <byte-string-stream> the
/// default is and empty <byte-string>.
///
/// This is maintained through every action on a <simple-sequence-stream>:
/// 0 <= stream-start <= position <= stream-end <= contents.size
/// 
define class <simple-sequence-stream> (<sequence-stream>)
  // The contents slot is set to #f when the stream is closed.
  slot contents :: false-or(<sequence>),
    required-init-keyword: contents:;
  slot direction :: one-of(#"input", #"output", #"input-output")
    = #"input", init-keyword: direction:;
  slot stream-start :: <integer> = 0, init-keyword: start:;
  slot stream-end :: <integer>, init-keyword: end:; // = contents.size
  slot position :: <integer> = 0;
end class;

// Older versions of d2c don't support the keyword clause. So in oder to support
// bootstrapping with them, we override the make method instead.
// XXX one day we want to get rid of that.

/// <byte-string-stream> -- Exported.
///
define class <byte-string-stream> (<simple-sequence-stream>, <string-stream>)
  // keyword contents: = make(<byte-string-stream>), type: <byte-string>;
end class;

/// <unicode-string-stream> -- Exported.
///
define class <unicode-string-stream> 
    (<simple-sequence-stream>, <string-stream>)
//  keyword contents: = make(<unicode-string>), type: <unicode-string>;
end class;


//// Creating Streams
////

/// type-for-sequence-stream -- Exported.
///
define open generic type-for-sequence-stream (seq :: <sequence>)
 => sequence-stream-type :: <type>;

/// if there's no more specific implementation, you get a
/// <simple-sequence-stream>
///
define inline method type-for-sequence-stream (seq :: <sequence>)
 => sequence-stream-type :: singleton(<simple-sequence-stream>);
  <simple-sequence-stream>;
end method type-for-sequence-stream;

define inline sealed method type-for-sequence-stream (seq :: <byte-string>)
 => sequence-stream-type :: singleton(<byte-string-stream>);
  <byte-string-stream>;
end method type-for-sequence-stream;

define inline sealed method type-for-sequence-stream (seq :: <unicode-string>)
 => sequence-stream-type :: singleton(<unicode-string-stream>);
  <unicode-string-stream>;
end method type-for-sequence-stream;

/// type-for-file-stream -- Exported.
///
define open generic type-for-file-stream
    (locator :: type-union(<locator>, <string>),
     element-type :: false-or(<type>),
     encoding :: false-or(<symbol>))
 => type :: <type>;

#if (mindy)
define inline method type-for-file-stream
    (locator :: <byte-string>,
     element-type :: one-of(#f, <byte-character>, <byte>),
     encoding :: one-of(#f, #"ANSI"))
 => type :: singleton(<fd-file-stream>);
  <fd-file-stream>;
end method;
#else
// The compiler can't deal with singleton(<byte>)
//
define inline method type-for-file-stream
    (locator :: <byte-string>, 
     element-type :: false-or(<type>),
     encoding :: one-of(#f, #"ANSI", #"big-endian"))
 => type :: <type>;
  select (element-type)
    <byte>, <byte-character> => <fd-file-stream>;
//    <unicode-character> => <unicode-fd-file-stream>;
    otherwise => <fd-file-stream>;
  end select;
end method;
#endif

/// make
///
define inline method make
    (result-class :: singleton(<sequence-stream>),
      #rest keys, #key contents :: <sequence>, #all-keys)
 => result :: <sequence-stream>;
    apply(make, type-for-sequence-stream(contents), keys);
end method make;

define inline method make
    (result-class :: singleton(<string-stream>),
     #rest keys, #key contents :: <string> = "", #all-keys)
 => result :: <string-stream>;
  apply(make, type-for-sequence-stream(contents), keys);
end method make;

define inline method make
    (result-class :: singleton(<file-stream>), #rest keys,
     #key locator :: type-union(<locator>, <string>),
          element-type :: false-or(<type>),
          encoding :: false-or(<symbol>),
     #all-keys)
 => result :: <file-stream>;
  apply(make, type-for-file-stream(locator, element-type, encoding), keys);
end method;

/// initialize
///
define method initialize (stream :: <stream>, #next next-method,
			  #key, #all-keys)
 => ();
  stream.outer-stream := stream;
  next-method();
end method;

define sealed method initialize
    (stream :: <simple-sequence-stream>,
     #next next-method,
     #key contents :: <sequence>, // Default depends on type of stream
          direction: dir :: one-of(#"input", #"output", #"input-output")
            = #"input",
          start :: type-union(singleton($not-supplied), <integer>)
            = $not-supplied, // = 0,
          end: stop :: type-union(singleton($not-supplied), <integer>)
            = $not-supplied, // = contents.size,
     #all-keys)
 => ();
  // Make sure they didn't try and give us start and stop on an output stream
  if (start ~== $not-supplied)
    if (dir ~== #"input") 
      error("Keyword start: only valid for input-only streams -- %=", stream);
    end;
  else
    start := 0;
  end;
  if (stop ~== $not-supplied)
    if (dir ~== #"input") 
      error("Keyword stop: only valid for input-only streams -- %=", stream);
    end;
  else
    // For #"input-output" streams, we assume there is valid input in
    // contents already.
    stop := if (dir == #"output") 0 else contents.size end;
  end;

  // Do some bounds checking ...
  if (start < 0)
    error("Bounds error in string -- %d.", start);
  end;
  if (stop > contents.size)
    error("Bounds error in string -- %d.", stop);
  end;
  if (start > stop)
    error("Start, %d, must be less than or equal to end, %d.", start, stop);
  end;
  next-method();
  stream.direction := dir;
  stream.stream-start := start;
  stream.stream-end := stop;
  stream.position := start;
end method initialize;

define sealed method make
    (stream == <byte-string-stream>,
     #next next-method,
     #rest all-parameters,
     #key contents :: type-union(singleton($not-supplied), <byte-string>) = $not-supplied, 
     #all-keys)
 => (result :: <byte-string-stream>)
  if(contents == $not-supplied)
    apply(next-method, stream, contents:, make(<byte-string>), all-parameters);
  else
    next-method();
  end if;
end method make;

define sealed method make
    (stream == <unicode-string-stream>,
     #next next-method,
     #rest all-parameters,
     #key contents :: type-union(singleton($not-supplied), <unicode-string>) = $not-supplied, 
     #all-keys)
 => (result :: <unicode-string-stream>)
  if(contents == $not-supplied)
    apply(next-method, stream, contents:, make(<unicode-string>), all-parameters);
  else
    next-method();
  end if;
end method make;


/// close -- Exported.
///
define open generic close (stream :: <stream>, #key, #all-keys);

define sealed method close (stream :: <simple-sequence-stream>,
			    #key, #all-keys);
  block ()
    lock-stream(stream);
    if (stream.direction ~== #"input")
      synchronize-output(stream);
    end if;
    stream.contents := #f;
  cleanup
    unlock-stream(stream);
  end block;
end method close;


//// Reading.
////
//// See file stream-reading.dylan
////

//// Convenience funtions for reading.
////
//// See file stream-reading.dylan
////

//// Writing.
////
//// See file stream-writing.dylan
////

//// Reading and writing by lines.
////
//// See file stream-lines.dylan
////


//// Querying Streams.
//// All of these are exported.
////

/// stream-open?
///
define open generic stream-open? (stream :: <stream>) => open? :: <boolean>;

define sealed method stream-open? (stream :: <simple-sequence-stream>)
 => open? :: <boolean>;
  block ()
    lock-stream(stream);
    // The if looks unnecessary, but we need a #t, not just non-#f
    if (stream.contents) #t else #f end;
  cleanup
    unlock-stream(stream);
  end block;
end method stream-open?;

/// stream-element-type
///
define open generic stream-element-type (stream :: <stream>) 
 => element-type :: <type>;

/// The default method on <simple-sequence-stream> looks at the first element
/// to determine the element type, and thus requires at least one element
/// be in the stream. More specific methods do not require this.
/// There must be a better way.
///
define sealed method stream-element-type (stream :: <simple-sequence-stream>) 
 => element-type :: <type>;
  block ()
    lock-stream(stream);
    check-stream-open(stream);
    if (stream.contents.empty?)
      error("Can't tell element type of empty stream -- %=", stream);
    else
      stream.contents.first.object-class;
    end if;
  cleanup
    unlock-stream(stream);
  end block
end method stream-element-type;

#if (mindy)
define inline sealed method stream-element-type 
    (stream :: <byte-string-stream>)
 => element-type :: singleton(<byte-character>);
  <byte-character>;
end method stream-element-type;

define inline sealed method stream-element-type
    (stream :: <unicode-string-stream>)
 => element-type :: singleton(<unicode-character>);
  <unicode-character>;
end method stream-element-type;
#else
define inline sealed method stream-element-type 
    (stream :: <byte-string-stream>)
 => element-type :: <type>;
  <byte-character>;
end method stream-element-type;

define inline sealed method stream-element-type
    (stream :: <unicode-string-stream>)
 => element-type :: <type>;
  <unicode-character>;
end method stream-element-type;
#endif

/// stream-at-end?
///
define open generic stream-at-end? (stream :: <stream>)
 => at-end? :: <boolean>;

define sealed method stream-at-end? (stream :: <simple-sequence-stream>)
 => at-end? :: <boolean>;
  block ()
    lock-stream(stream);
    stream.position == stream.stream-end;
  cleanup
    unlock-stream(stream);
  end block;
end method stream-at-end?;


//// Positionable Stream Protocol.
//// All of these are exported.
////

define abstract class <stream-position> (<object>)
end class;

/// stream-position
///
define open generic stream-position (stream :: <positionable-stream>)
 => position :: type-union(<stream-position>, <integer>);

define sealed method stream-position (stream :: <simple-sequence-stream>)
 => position :: <integer>;
  block ()
    lock-stream(stream);
    stream.position;
  cleanup
    unlock-stream(stream);
  end block;
end method stream-position;

/// stream-position-setter
///
define open generic stream-position-setter
    (pos :: type-union(<stream-position>, <integer>, 
		       one-of(#"start", #"end")),
     stream :: <positionable-stream>)
 => new-position :: type-union(<stream-position>, <integer>);

define sealed method stream-position-setter
    (pos :: type-union(<integer>, one-of(#"start", #"end")),
     stream :: <simple-sequence-stream>)
 => new-position :: <integer>;
  block ()
    lock-stream(stream);
    check-stream-open(stream);
    select (pos)
      #"start" => stream.position := stream.stream-start;
      #"end" => stream.position := stream.stream-end;
      otherwise // pos is an <integer>
	=> begin
	     if (pos < stream.stream-start | pos > stream.contents.size)
	       error("Position %d out of bounds -- %=", pos, stream);
	     elseif (pos > stream.stream-end)
	       stream.stream-end := pos;
	     end if;
	     stream.position := pos;
	   end;
    end select;
    stream.position;
  cleanup
    unlock-stream(stream);
  end block;
end method stream-position-setter;

/// adjust-stream-position
///
define open generic adjust-stream-position
    (stream :: <positionable-stream>, delta :: <integer>,
     #key from :: one-of(#"current", #"start", #"end")) // = #"current"
 => new-position :: type-union(<stream-position>, <integer>);

define sealed method adjust-stream-position
    (stream :: <simple-sequence-stream>, delta :: <integer>,
     #key from :: one-of(#"current", #"start", #"end") = #"current")
 => new-position :: <integer>;
  block ()
    lock-stream(stream);
    check-stream-open(stream);
    select (from)
      #"current"
	=> stream.position := (stream.position + delta);
      #"start"
	=> stream.position := (stream.stream-start + delta);
      #"end"
	=> stream.position := (stream.stream-end + delta);
    end select;
    if (stream.position < stream.stream-start)
      error("Stream cannot be positioned before start -- %=", stream);
    end if;
    if (stream.position > stream.contents.size)
      grow-stream-sequence!(stream, stream.position);
    end if;
    if (stream.position > stream.stream-end)
      stream.stream-end := stream.position;
    end if;
    // return
    stream.position;
  cleanup
    unlock-stream(stream);
  end block;
end method adjust-stream-position;

/// stream-size
///
define open generic stream-size (stream :: <positionable-stream>)
 => size :: <integer>;

define sealed method stream-size (stream :: <simple-sequence-stream>)
 => size :: <integer>;
  block ()
    lock-stream(stream);
    stream.stream-end - stream.stream-start;
  cleanup
    unlock-stream(stream);
  end block;
end method stream-size;

/// stream-contents
///
define open generic stream-contents (stream :: <positionable-stream>,
				     #key clear-contents? :: <boolean>)
 => contents :: <sequence>;

define sealed method stream-contents (stream :: <simple-sequence-stream>,
				      #key clear-contents? :: <boolean> = #t)
 => contents :: <sequence>;
  block ()
    lock-stream(stream);
    check-stream-open(stream);
    check-output-stream(stream);
    let start = stream.stream-start;
    let contents-size = stream.stream-end - start;
    let res = make(type-for-copy(stream.contents), size: contents-size);
    copy-sequence!(res, 0,
		   stream.contents, start,
		   contents-size);
    if (clear-contents?)
      stream.stream-end := start;
      stream.position := start;
    end if;
    res;
  cleanup
    unlock-stream(stream);
  end block;
end method stream-contents;


//// with-open-file
//// Sorry, no macros in mindy
#if (~mindy)
define macro with-open-file
  {
    with-open-file (?stream:variable = ?namestring:expression,
		    #rest ?parameters:expression)
      ?:body
    end
  }
 => {
      let stream = #f;
      block ()
	stream := make(<file-stream>, locator: ?namestring, ?parameters);
	let ?stream = stream;
	?body;
      cleanup
	if (stream)
	  close(stream);
	end if;
      end block
    }
end macro with-open-file;

define macro with-output-to-string
  { with-output-to-string(?s:variable) ?:body end }
    => { block ()
           let ?s = make(<string-stream>);
           ?body;
           ?s.stream-contents
         end }
end macro with-output-to-string;

#endif


//// Using File Streams.
////
//// See file file-streams.dylan
////


//// Stream locking.
////

/// stream-locked? -- Exported.
///
define method stream-locked? (stream :: <stream>) => locked? :: <boolean>;
  locked?(stream.stream-lock);
end method;

/// lock-stream -- Exported.
///
define method lock-stream (stream :: <stream>);
  grab-lock(stream.stream-lock);
end method;

/// unlock-stream -- Exported.
///
define method unlock-stream (stream :: <stream>);
  release-lock(stream.stream-lock);
end method;

/// with-stream-locked -- Exported.
///


//// Buffer Access Protocol
////
//// See file stream-buffers.dylan
////

//// Stream Extension Protocol
////
//// See file stream-buffers.dylan
////

//// Wrapper Stream Protocol
//// 
//// See file wrapper-streams.dylan
////


//// Conditions.
//// All of these are exported.
////

define class <end-of-stream-error> (<error>)
  slot end-of-stream-stream :: <stream>, required-init-keyword: stream:;
end class;

define sealed domain make (singleton(<end-of-stream-error>));
define sealed domain initialize (<end-of-stream-error>);

define inline sealed method report-condition
    (cond :: <end-of-stream-error>, stream)
 => ();
  condition-format(stream, "Unexpected end of stream -- %=",
		   cond.end-of-stream-stream);
end method;


define class <incomplete-read-error> (<end-of-stream-error>)
  slot incomplete-read-sequence :: <sequence>,
    required-init-keyword: sequence:;
  slot incomplete-read-count :: <integer>,
    required-init-keyword: count:;
end class;

define sealed domain make (singleton(<incomplete-read-error>));
define sealed domain initialize (<incomplete-read-error>);

define inline sealed method report-condition 
    (cond :: <incomplete-read-error>, stream)
 => ();
  condition-format(stream, "Incomplete read on %=. Sequence read: %= (%d elements requested)",
		   cond.end-of-stream-stream,
		   cond.incomplete-read-sequence,
		   cond.incomplete-read-count);
end method;

define class <file-error> (<error>)
  slot file-locator :: type-union(<byte-string>, <locator>),
    required-init-keyword: locator:;
end class;

define sealed domain make (singleton(<file-error>));
define sealed domain initialize (<file-error>);

define sealed method report-condition
    (cond :: <file-error>, stream) => ();
  condition-format(stream, "File error: %=",
		   cond.file-locator);
end method;

define class <file-exists-error> (<file-error>)
end class;

define sealed domain make (singleton(<file-exists-error>));
define sealed domain initialize (<file-exists-error>);

define inline sealed method report-condition
    (cond :: <file-exists-error>, stream)
 => ();
  condition-format(stream, "File already exists: %=",
		   cond.file-locator);
end method;

define class <file-does-not-exist-error> (<file-error>)
end class;

define sealed domain make (singleton(<file-does-not-exist-error>));
define sealed domain initialize (<file-does-not-exist-error>);

define inline sealed method report-condition
    (cond :: <file-does-not-exist-error>, stream)
 => ();
  condition-format(stream, "File does not exist: %=",
		   cond.file-locator);
end method;

define class <invalid-file-permissions-error> (<file-error>)
end class;

define sealed domain make (singleton(<invalid-file-permissions-error>));
define sealed domain initialize (<invalid-file-permissions-error>);

define inline sealed method report-condition 
    (cond :: <invalid-file-permissions-error>, stream) => ();
  condition-format(stream, "Invalid file permissions: %=",
		   cond.file-locator);
end method;


//// Misc internal stuff
////

/// type-for-sequence -- Internal interface.
///
/// Used by read and read-line for <buffered-stream> to determine
/// what type of sequence to return.
///
#if (mindy)
define inline sealed method type-for-sequence
    (element-type :: <type>)
 => type :: singleton(<vector>);
  <vector>;
end method;

define inline sealed method type-for-sequence
    (element-type :: singleton(<byte>))
 => type :: singleton(<byte-vector>);
  <byte-vector>;
end method;

define inline sealed method type-for-sequence
    (element-type :: singleton(<byte-character>))
 => type :: singleton(<byte-string>);
  <byte-string>;
end method;

define inline sealed method type-for-sequence
    (element-type :: singleton(<unicode-character>))
 => type :: singleton(<unicode-string>);
  <unicode-string>;
end method;
#else
// The compiler can't deal with singleton(<byte>).
//
define inline sealed method type-for-sequence (element-type)
 => type;
  select (element-type)
    <byte> => <byte-vector>;
    <byte-character> => <byte-string>;
    <unicode-character> => <unicode-string>;
    otherwise => <vector>;
  end select;
end method;
#endif

/// check-stream-open -- Internal interface
/// Makes sure stream is open, signals an error if not.
/// Assumes stream is held by caller.
///
define inline sealed method check-stream-open
    (stream :: <simple-sequence-stream>)
 => ();
  if (~stream.contents)
    error("Stream has been closed: %=", stream);
  end if;
end method;

/// check-input-stream -- Internal interface
/// Makes sure stream is an input stream, signals an error if not.
/// Assumes stream is held by caller.
///
define inline sealed method check-input-stream
    (stream :: <simple-sequence-stream>)
 => ();
  if (stream.direction == #"output")
    error("Stream is not an input stream: %=", stream);
  end if;
end method;

/// check-output-stream -- Internal interface
/// Makes sure stream is an output stream, signals an error if not.
/// Assumes stream is held by caller.
///
define inline sealed method check-output-stream
    (stream :: <simple-sequence-stream>) 
 => ();
  if (stream.direction == #"input")
    error("Stream is not an output stream: %=", stream);
  end if;
end method;

/// $default-grow-amount -- Internal.
///
define constant $default-grow-amount = 128;

/// grow-stream-sequence! -- Internal interface
/// Assumes stream open, locked, etc.
///
define sealed method grow-stream-sequence!
    (stream :: <simple-sequence-stream>, new-size :: <integer>)
 => ();
  let seq-type = type-for-copy(stream.contents);
  if (subtype?(seq-type, <stretchy-collection>))
    stream.contents.size := new-size;
  else
    let new-seq = make(seq-type, size: new-size);
    copy-sequence!(new-seq, stream.stream-start,
		   stream.contents, stream.stream-start,
		   stream.contents.size);
    stream.contents := new-seq;
  end if;
end method;

define inline sealed method grow-stream-sequence!
    (stream :: <byte-string-stream>, new-size :: <integer>)
 => ();
  let new-seq = make(<byte-string>, size: new-size);
  copy-bytes(new-seq, stream.stream-start,
	     stream.contents, stream.stream-start,
	     stream.contents.size);
  stream.contents := new-seq;
end method;

define inline sealed method grow-stream-sequence!
    (stream :: <unicode-string-stream>, new-size :: <integer>)
 => ();
  let new-seq = make(<unicode-string>, size: new-size);
  copy-bytes(new-seq, stream.stream-start,
	     stream.contents, stream.stream-start,
	     stream.contents.size);
  stream.contents := new-seq;
end method;

/// copy-sequence! -- Internal interface.
///
/// Uses whatever copy method is most effecient for the given types.
///

/// This is the default method for sequences we don't know how to do anything
/// better with.
///
define sealed method copy-sequence!
    (dest :: <mutable-sequence>, dest-start :: <integer>, 
     source :: <sequence>, source-start :: <integer>, 
     length :: <integer>)
 => ();
  for (i from dest-start below dest-start + length,
       j from source-start below source-start + length)
    dest[i] := source[j];
  end for;
end method;

define constant <byte-sequence> = type-union(<buffer>,
					     <byte-vector>,
					     <byte-string>, 
					     <unicode-string>);

define inline sealed method copy-sequence!
    (dest :: <byte-sequence>,
     dest-start :: <integer>, 
     source :: <byte-sequence>,
     source-start :: <integer>, 
     length :: <integer>)
 => ();
  copy-bytes(dest, dest-start, source, source-start, length);
end method;

//// Output stream registration and forcing output upon Application exit.
////

/// This lock isolates access to *output-streams*.
///
define constant output-stream-registry-lock :: <semaphore> = make(<semaphore>);

/// This list contains all open output streams.  There is a function
/// registered on the exist hook that forces output on all streams when the
/// application exits.
///
define variable *output-streams* :: <list> = #();

/// register-output-stream -- Internal Interface.
///
/// This function registers output functions for the purpose of
/// synchronizing their output when an application exits.  The same registry
/// of streams could be used by a demon thread that periodically wakes up
/// and forces output on streams.
///
define method register-output-stream (stream :: <stream>)
    => stream :: <stream>;
  grab-lock(output-stream-registry-lock);
  *output-streams* := pair(stream, *output-streams*);
  release-lock(output-stream-registry-lock);
  stream;
end method;

/// unregister-output-stream -- Internal Interface.
///
/// This function removes stream from *output-streams*.
///
define method unregister-output-stream (stream :: <stream>)
    => stream :: <stream>;
  grab-lock(output-stream-registry-lock);
  *output-streams* := remove!(*output-streams*, stream);
  release-lock(output-stream-registry-lock);
  stream;
end method;

/// Register a function on the application exit hook.  This function forces
/// output for every output stream.  There's no reason to isolate access to
/// *output-streams* because exit functions run one at a time in the only
/// remaining thread.
///
on-exit(method ()
	  for (stream in *output-streams*)
	    get-output-buffer(stream);
	    force-output-buffers(stream);
	    synchronize(stream);
	    release-output-buffer(stream);
	  end;
	end);
