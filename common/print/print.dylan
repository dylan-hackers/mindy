module: Print
author: chiles@cs.cmu.edu
synopsis: This file implements object printing.
copyright: See below.
rcs-header: $Header: /home/housel/work/rcs/gd/src/common/print/print.dylan,v 1.4 1996/05/08 15:58:08 nkramer Exp $

//======================================================================
//
// Copyright (c) 1994, 1995  Carnegie Mellon University
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



/// <print-stream> class.
///

/// <print-stream> Class -- Exported.
///
/// These streams hold print state so that the print function can do most
/// of the work maintaining print state, and the print-object function can
/// just print objects, querying the state of the stream as necessary.  Each
/// slot defaults to the value of a global variable upon creation (see the
/// comments for the print function).
///
define sealed class <print-stream> (<stream>)
  //
  // Print-target holds the real destination of the print-stream.
  slot print-target :: <stream>, required-init-keyword: #"stream";
  //
  // Print-level holds the maximum depth to which the user wants recursive
  // printing to go.
  slot print-level :: false-or(<integer>) = *default-level*,
    init-keyword: #"level";
  //
  // Print-depth holds the current level of printing.  When incremeting this
  // slot causes the depth to exceed print-level, then the print function
  // only outputs $print-level-exceeded-string.
  slot print-depth :: <integer> = -1;
  //
  // Print-length holds the maximum number of elements the user wants a
  // sequence to be printed.  This does not apply to some sequences, such as
  // strings.
  slot print-length :: false-or(<integer>) = *default-length*,
    init-keyword: #"length";
  //
  // Print-pretty? holds whether the user wants pretty printing.
  slot print-pretty? :: <boolean> = *default-pretty?*,
    init-keyword: #"pretty?";
  //
  // Print-circle? holds whether the user wants pretty printing.
  slot print-circle? :: <boolean> = *default-circle?*,
    init-keyword: #"circle?";
  //
  // Circular-first-pass? indicates to the print function whether it is on
  // the first pass of printing, in which it just builds a table of objects
  // referenced during the printing.  On the second pass of printing, print
  // actually generates output.
  slot circular-first-pass? :: <boolean> = #t;
  //
  // Circular-references is a table of objects referenced during printing
  // when print-circle? is #t.
  slot circular-references :: false-or(<object-table>) = #f;
  //
  // Circular-next-id holds the next ID to use when printing circularly.
  // Each time print sees an object for a second time during the first
  // printing pass, print assigns as the object's ID the current value of
  // this slot.
  slot circular-next-id :: <integer> = 0;
end class;

define sealed domain make (singleton(<print-stream>));
define sealed domain initialize (<print-stream>);



/// <print-reference> Class.
///

/// <print-reference> Class -- Internal.
///
/// These objects hold information about object references encountered when
/// print-circle? is #t.  The print function creates these objects in a fake
/// first printing pass, and then it uses these objects during a real second
/// printing pass to determine whether the object needs to be tagged,
/// printed normally, or printed by reference to the objects circular ID to
/// avoid infinite recursive printing.
///
define sealed class <print-reference> (<object>)
  //
  // This slot holds the object referenced during printing.
  slot print-reference-object, required-init-keyword: #"object";
  //
  // This slot holds the object's ID for circular references.  The object
  // prints as its ID after the first time.  Before the first time the object
  // is printed, this slot is #f.
  slot print-reference-id :: false-or(<byte-string>) = #f;
  //
  // This slot counts the number of references to the object.
  slot print-reference-count :: <integer> = 0;
end class;

define sealed domain make (singleton(<print-reference>));
define sealed domain initialize (<print-reference>);


/// Print-reference routines.
///

/// print-reference -- Internal Interface.
///
/// This function returns the print-reference object associated with object.
/// If none exists, then this creates a print-reference and installs it in
/// the circular-references table.
///
define method print-reference (object, stream :: <print-stream>)
    => ref :: <print-reference>;
  let table = stream.circular-references;
  let ref = element(table, object, default: #f);
  if (ref)
    ref;
  else
    let ref = make(<print-reference>, object: object);
    element(table, object) := ref;
  end;
end method;

/// new-print-reference-id -- Internal Interface.
///
/// This function gets the next circular print reference ID, assigns it to ref,
/// and updates the stream so that it doesn't return the same ID again.
///
define method new-print-reference-id (stream :: <print-stream>,
				      ref :: <print-reference>)
    => ID :: <byte-string>;
  let id = stream.circular-next-id;
  stream.circular-next-id := id + 1;
  ref.print-reference-id := integer-to-string(id);
end method;



define constant $digits = "0123456789abcdef";

/// integer-to-string -- Internal.
///
/// This converts a integer to a byte-string.
///
/// This function makes the trade off that consing and throwing away a list
/// (that probably never ascends to an elder GC generation) is better than
/// isolating access to a global vector that lies around across calls to
/// this function.  There was no profiling to validate this trade-off.
///
/// We have two versions, one for <extended-integer> and one for <integer> so
/// that we don't have to do lots of runtime dispatching.
/// 
define sealed method integer-to-string
    (arg :: <extended-integer>, #key radix :: <integer> = 10)
    => res :: <byte-string>;
  local method repeat (arg :: <extended-integer>, digits :: <list>)
	    => res :: <list>;
	  let (quotient, remainder) = floor/(arg, radix);
	  let digits = pair($digits[as(<integer>, remainder)], digits);
	  if (zero?(quotient))
	    digits;
	  else
	    repeat(quotient, digits);
	  end;
	end;
  as(<byte-string>,
     if (negative?(arg))
       pair('-', repeat(- arg, #()));
     else
       repeat(arg, #());
     end);
end;
///
define sealed method integer-to-string
    (arg :: <integer>, #key radix :: <integer> = 10)
    => res :: <byte-string>;
  local method repeat (arg :: <integer>, digits :: <list>)
	    => res :: <list>;
	  let (quotient, remainder) = floor/(arg, radix);
	  let digits = pair($digits[remainder], digits);
	  if (zero?(quotient))
	    digits;
	  else
	    repeat(quotient, digits);
	  end;
	end;
  as(<byte-string>,
     if (negative?(arg))
       // We do one division before negating arg in case arg is
       // $minimum-integer.
       let (quotient, remainder) = truncate/(arg, radix);
       pair('-', repeat(-quotient, list($digits[-remainder])));
     else
       repeat(arg, #());
     end);
end;




/// Print-{level,length,depth,pretty?,circle?} generics and default methods.
///

/// print-length -- Exported.
///
define sealed generic print-length (stream :: <stream>)
    => length :: false-or(<integer>);

define method print-length (stream :: <stream>)
    => length :: singleton(#f);
  #f;
end method;


/// print-level -- Exported.
///
define sealed generic print-level (stream :: <stream>)
    => level :: false-or(<integer>);

define method print-level (stream :: <stream>)
    => level :: singleton(#f);
  #f;
end method;


/// print-depth -- Exported.
///
define sealed generic print-depth (stream :: <stream>)
    => depth :: <integer>;

define method print-depth (stream :: <stream>)
    => depth :: singleton(0);
  0;
end method;


/// print-pretty? -- Exported.
///
define sealed generic print-pretty? (stream :: <stream>)
    => pretty? :: <boolean>;

define method print-pretty? (stream :: <stream>)
    => pretty? :: singleton(#f);
  #f;
end method;


/// print-circle? -- Exported.
///
define sealed generic print-circle? (stream :: <stream>)
    => circle? :: <boolean>;

define method print-circle? (stream :: <stream>)
    => circle? :: singleton(#f);
  #f;
end method;



/// Print and global defaults.
///

/// These provide the default values for the keywords to print.  #f means
/// there are no bounds, special checks for circularity, or pretty printing.
///
define variable *default-level* :: false-or(<integer>) = #f;
define variable *default-length* :: false-or(<integer>) = #f;
define variable *default-circle?* :: <boolean> = #f;
define variable *default-pretty?* :: <boolean> = #f;

/// What to print when the current depth exceeds the users requested print
/// level limit.
///
define constant $print-level-exceeded-string :: <byte-string> = "#";

/// What to print before a circular print ID.
///
define constant $circular-id-prestring :: <byte-string> = "#";

/// What to print after a circular print ID.
///
define constant $circular-id-poststring :: <byte-string> = "#";


/// Print -- Exported.
///
define generic print (object, stream :: <stream>,
		      #key level, length, circle?, pretty?)
    => ();


define constant <boolean-or-not-supplied>
  = type-union(<boolean>, singleton($not-supplied));
define constant <fixed-integer-false-or-not-supplied>
  = type-union(<integer>, one-of(#f, $not-supplied));

/// Print -- Method for Exported Interface.
///
/// This method must regard the values of the keywords and construct a
/// <print-stream> to hold the values for the requested print operation.
///
define method print (object, stream :: <stream>,
		     #key level :: <fixed-integer-false-or-not-supplied>
		            = $not-supplied,
		          length :: <fixed-integer-false-or-not-supplied>
		            = $not-supplied,
		          circle? :: <boolean-or-not-supplied> = $not-supplied,
		          pretty? :: <boolean-or-not-supplied> = $not-supplied)
    => ();
  block ()
    //
    // Lock the stream so that all the calls to print-object build output
    // contiguously, without intervening threads screwing up the print
    // request.
    lock-stream(stream);
    //
    // Make the stream defaulting the slots to the global default values for
    // the keyword arguments.  No need to lock this stream because only this
    // thread should have any references to it ... barring extreme user
    // silliness.
    let p-stream = make(<print-stream>, stream: stream);
    //
    // Set slots with those values supplied by the user.
    if (~ (level == $not-supplied)) p-stream.print-level := level end;
    if (~ (length == $not-supplied)) p-stream.print-length := length end;
    if (~ (circle? == $not-supplied)) p-stream.print-circle? := circle? end;
    if (~ (pretty? == $not-supplied)) p-stream.print-pretty? := pretty? end;
    //
    // When printing circularly, we first print to a "null stream" so that we
    // can find the circular references.
    if (p-stream.print-circle?)
      start-circle-printing(object, p-stream);
    end;
    //
    // Determine whether, and how, to print object.
    maybe-print-object(object, p-stream);
  cleanup
    unlock-stream(stream);
  end;
end method;

/// Print -- Method for Exported Interface.
///
/// This method must regard the values of the keywords and construct a
/// <print-stream> to hold the values for the requested print operation.
///
define method print (object, stream :: <print-stream>,
		     #key level :: <fixed-integer-false-or-not-supplied>
		            = $not-supplied,
			  length :: <fixed-integer-false-or-not-supplied>
		            = $not-supplied,
		          circle? :: <boolean-or-not-supplied> = $not-supplied,
		          pretty? :: <boolean-or-not-supplied> = $not-supplied)
    => ();
  let save-level = stream.print-level;
  let save-length = stream.print-length;
  let save-circle? = stream.print-circle?;
  let save-pretty? = stream.print-pretty?;
  block ()
    //
    // Establish changes in policy for this call to print.
    // If level is supplied, and there was already a level in effect, we
    // continue printing with the minimum effect of the two levels, assuming
    // that is the most careful thing to do.
    case
      (level == $not-supplied) => #f;   // Case is broken in Mindy.
      (save-level) =>
	stream.print-level := min(save-level, (level + stream.print-depth));
      otherwise => stream.print-level := level;
    end;
    // If length is supplied, and there was already a length in effect, we
    // continue printing with the minimum of the two lengths, assuming that
    // is the most careful thing to do.
    case
      (length == $not-supplied) => #f;   // Case is broken in Mindy.
      (save-length) => stream.print-length := min(save-length, length);
      otherwise => stream.print-length := length;
    end;
    // We never turn off circular printing, but if a recursive call to print
    // turns circular printing on, we print that object circularly.
    case
      ((circle? == $not-supplied) | (~ circle?)) =>
	#f;   // Case is broken in Mindy.
      (~ save-circle?) =>
	stream.print-circle? := #t;
	start-circle-printing(object, stream);
    end;
    // Printing pretty gets turned on and off for each user-supplied value
    // passed to print.  The assumption is that there is no harm in turning
    // it off for some object, and because it is odd to request no pretty
    // printing, the calling code probably has good reason to turn it off.
    if (~ (pretty? == $not-supplied)) stream.print-pretty? := pretty? end;
    //
    // Determine whether, and how, to print object.
    maybe-print-object(object, stream);
  cleanup
    stream.print-level := save-level;
    stream.print-length := save-length;
    stream.print-circle? := save-circle?;
    stream.print-pretty? := save-pretty?;
  end;
end method;

/// start-circle-printing -- Internal.
///
/// This function makes sure the stream has a circular-references table,
/// makes sure object has a print-reference, checks for circular references
/// within object, and considers what sort of output may be necessary to
/// define a tag for object or print object's tag.
///
/// This function is called both from the very first call to print and
/// recursive calls to print.  The calls to start-circle-printing within
/// recursive calls to print occur when the original call to print had
/// circular printing turned off, and the recursive calls to print turn
/// circular printing on.  Because of this function's use within recursive
/// calls to print, it cannot make certain assumptions:
///    Whether stream already has a circular-references table.
///    Whether there already is a print-reference for object.
///    What print-reference-count is for object.
///    Whether to do a first pass on object looking for circular references.
///    Whether object already has a print-reference-id.
///
/// Recursive calls to print cannot turn off circular printing, so we don't
/// have to account for that.
///
define method start-circle-printing (object, stream :: <print-stream>)
    => ();
  let table = stream.circular-references;
  if (~ table)
    table := make(<object-table>);
    stream.circular-references := table;
  end;
  let ref = print-reference(object, stream);
  let count :: <integer> = (ref.print-reference-count + 1);
  ref.print-reference-count := count;
  if (count = 1)
    // If this is the first time we've seen this object, then dive into it
    // looking for circular references.
    stream.circular-first-pass? := #t;
    print-object(object, stream);
    stream.circular-first-pass? := #f;
  end;
end method;

/// maybe-print-object -- Internal.
///
/// This function increments print-depth and regards print-level to see
/// whether it should print object.  If it should print object, then it
/// regards print-circle? and does the right thing.
///
define method maybe-print-object (object, stream :: <print-stream>)
  let depth :: <integer> = (stream.print-depth + 1);
  block ()
    stream.print-depth := depth;
    let requested-level :: false-or(<integer>) = stream.print-level;
    case
      (requested-level & (depth > requested-level)) =>
	write($print-level-exceeded-string, stream);
      (~ stream.print-circle?) =>
	print-object(object, stream);
      (stream.circular-first-pass?) =>
	// When printing circularly, we first print to a "null stream" so
	// that we can find the circular references.
	let ref = print-reference(object, stream);
	let ref-count = (ref.print-reference-count + 1);
	ref.print-reference-count := ref-count;
	if (ref-count = 1)
	  // If ref-count is already greater than one, then there's
	  // no reason to go further into the object gathering references.
	  print-object(object, stream);
	end;
      otherwise
	output-print-reference(print-reference(object, stream),
			       stream);
    end case;
  cleanup
    stream.print-depth := depth - 1;
  end;
end method;

/// output-print-reference -- Internal.
///
/// This function determines how to output a print-reference for circular
/// printing.
///
define method output-print-reference (ref :: <print-reference>,
				      stream :: <stream>)
    => ();
  let ref-id = ref.print-reference-id;
  case
    (ref.print-reference-count = 1) =>
      print-object(ref.print-reference-object, stream);
    (~ ref-id) =>
      write($circular-id-prestring, stream);
      write(new-print-reference-id(stream, ref), stream);
      write($circular-id-poststring, stream);
      write("=", stream);
      print-object(ref.print-reference-object, stream);
    otherwise =>
      write($circular-id-prestring, stream);
      write(ref-id, stream);
      write($circular-id-poststring, stream);
  end;
end method;



/// Print-object generic and default method.
///

/// print-object -- Exported.
///
define open generic print-object (object, stream :: <stream>)
    => ();

/// Any object.
///
define method print-object (object :: <object>, stream :: <stream>)
    => ();
  pprint-logical-block
    (stream,
     prefix: "{",
     body: method (stream)
	     let obj-class = object.object-class;
	     let name = obj-class.class-name;
	     if (name)
	       write(as(<byte-string>, name), stream);
	       write(" instance", stream);
	     else
	       write("instance of ", stream);
	       print-specializer(obj-class, stream);
	     end if;
	   end method,
     suffix: "}");
end method;


/// Print-object <byte-string>, <unicode-string> and <character> methods.
///

/// Characters.
///
define sealed method print-object (char :: <character>, stream :: <stream>)
    => ();
  write('\'', stream);
  write-char-maybe-escape(char, '\'', stream);
  write('\'', stream);
end method print-object;

// write-char-maybe-escape -- internal.
//
// Utility routine used for printing characters appropriately escaped.
// 
define method write-char-maybe-escape
    (char :: <character>, quote :: one-of('\'', '"'), stream :: <stream>)
    => ();
  case
    char < ' ' =>
      select (char)
	'\0' => write("\\0", stream);
	'\a' => write("\\a", stream);
	'\b' => write("\\b", stream);
	'\t' => write("\\t", stream);
	'\f' => write("\\f", stream);
	'\r' => write("\\r", stream);
	'\n' => write("\\n", stream);
	'\e' => write("\\e", stream);
	otherwise =>
	  write("\\<", stream);
	  write(integer-to-string(as(<integer>, char), radix: 16), stream);
	  write('>', stream);
      end select;
    char == quote =>
      write('\\', stream);
      write(char, stream);
    char == '\\' =>
      write("\\\\", stream);
    char > '~' =>
      write("\\<", stream);
      write(integer-to-string(as(<integer>, char), radix: 16), stream);
      write('>', stream);
    otherwise =>
      write(char, stream);
  end case;
end method write-char-maybe-escape;


/// strings.
///
define sealed method print-object
    (object :: type-union(<byte-string>, <unicode-string>), stream :: <stream>)
    => ();
  write-string-escaped(object, stream);
end method print-object;


// write-string-escaped -- internal.
//
// Utility used by <byte-string>, <unicode-string>, and <symbol> print-object
// methods to print the string with appropriate characters escaped.
// 
define generic write-string-escaped
    (object :: type-union(<byte-string>, <unicode-string>), stream :: <stream>)
    => ();

// write-string-escaped{<byte-string>}
//
// We try to write as much of the string as possible at once in order to
// keep from having to make lots of extra calls to write.  We scan the
// string for the next character that required special processing and then
// write all the skipped over characters in one chunk.
// 
define method write-string-escaped
    (object :: <byte-string>, stream :: <stream>)
    => ();
  let len :: <integer> = object.size;
  local

    method find-next-break (index :: <integer>)
	=> (next-break :: <integer>, char :: <byte-character>);
      if (index == len)
	// It doesn't matter what character we return, we just need to
	// return some character so the type matches.
	values(index, 'x');
      else
	let char = object[index];
	if (char < ' ' | char == '"' | char == '\\' | char > '~')
	  values(index, char);
	else
	  find-next-break(index + 1);
	end if;
      end if;
    end method find-next-break,

    method write-guts (from :: <integer>) => ();
      let (next-break, char) = find-next-break(from);
      unless (from == next-break)
	write(object, stream, start: from, end: next-break);
      end unless;
      unless (next-break == len)
	write-char-maybe-escape(char, '"', stream);
	write-guts(next-break + 1);
      end unless;
    end method write-guts;

  write('"', stream);
  write-guts(0);
  write('"', stream);
end method write-string-escaped;

// write-string-escaped{<unicode-string>}
//
// We can't write chunks of the string at once, so just pay the cost and
// write each character individually.
// 
define method write-string-escaped
    (object :: <unicode-string>, stream :: <stream>)
    => ();
  write('"', stream);
  for (char in object)
    write-char-maybe-escape(char, '"', stream);
  end for;
  write('"', stream);
end method write-string-escaped;


/// Print-object <list> method.
///

/// For circular printing to be correct, we need to count references to the
/// tail pointers as well as the head pointers.  Because we do not print lists
/// by calling print on the tail of each pair, we need to specially handle
/// the tail pointers in this method.  The object passed in and all head
/// pointers are handled naturally via calls to print.
///
define sealed method print-object (object :: <list>, stream :: <stream>) => ();
  pprint-logical-block(stream,
		       prefix: "#(",
		       body: method (stream)
			       if (~ (object == #()))
				 print-list(object, stream);
			       end;
			     end,
		       suffix: ")");
end method;

define method print-list (object :: <list>, stream :: <stream>) => ();
  block(exit)
    let length :: false-or(<integer>) = stream.print-length;
    if (length & (length <= 0))
      write("...", stream);
    else
      print(object.head, stream);
      let circle? = stream.print-circle?;
      let first-pass? = stream.circular-first-pass?;
      for (remaining = object.tail then remaining.tail,
	   count = 1 then (count + 1),
	   until: (remaining == #()))
	write(", ", stream);
	pprint-newline(#"fill", stream);
	case
	  (~ instance?(remaining, <list>)) =>
	    // Object was not a proper list, so print dot notation.
	    write(". ", stream);
	    pprint-newline(#"fill", stream);
	    print(remaining, stream);
	    exit();
	  (length & (count >= length)) =>
	    // We've exceeded print-length for this print request.
	    write("...", stream);
	    exit();
	  (~ circle?) =>
	    // No circular printing, so this is the simple and normal case.
	    print(remaining.head, stream);
	  (first-pass?) =>
	    // Get or create the print-reference for the remaining pointer.
	    let ref = print-reference(remaining, stream);
	    let ref-count = (ref.print-reference-count + 1);
	    ref.print-reference-count := ref-count;
	    if (ref-count = 1)
	      // First time through, so keep gathering references.
	      print(remaining.head, stream);
	    else
	      // If ref-count is already greater than one, then we've seen
	      // everything once.  Stop iterating.
	      exit();
	    end;
	  otherwise =>
	    // Circular printing on the second pass.
	    let ref = print-reference(remaining, stream);
	    let ref-id = ref.print-reference-id;
	    case
	      (ref.print-reference-count = 1) =>
		// Only one reference to the rest of the list, so print the
		// remaining elements normally.
		print(remaining.head, stream);
	      (~ ref-id) =>
		// Print the tag and its value with dot notation so that
		// the rest of the list does not appear to be a single
		// element of the list (that is, a nested list).
		write(". ", stream);
		pprint-newline(#"fill", stream);
		write($circular-id-prestring, stream);
		write(new-print-reference-id(stream, ref), stream);
		write($circular-id-poststring, stream);
		write("=", stream);
		print(remaining, stream);
	      otherwise =>
		// Print the tag with dot notation.  See previous cases's
		// comment.
		write(". ", stream);
		pprint-newline(#"fill", stream);
		write($circular-id-prestring, stream);
		write(ref-id, stream);
		write($circular-id-poststring, stream);
		exit();
	    end case;
	end case;
      end for;
    end if;
  end block;
end method;


/// Print-object <simple-object-vector> method.
///

/// Vectors.
///
define sealed method print-object
    (object :: <simple-object-vector>, stream :: <stream>)
    => ();
  pprint-logical-block(stream,
		       prefix: "#[",
		       body: method (stream)
			       print-items(object, print, stream);
			     end method,
		       suffix: "]");
end method;



/// Print-object <function> method.
///

/// Functions.
///
define sealed method print-object
    (object :: <generic-function>, stream :: <stream>)
    => ();
  pprint-logical-block
    (stream,
     prefix: "{",
     body: method (stream)
	     write("GF", stream);
	     let name = function-name(object);
	     if (name)
	       write(' ', stream);
	       pprint-newline(#"fill", stream);
	       write(as(<byte-string>, name), stream);
	     end;
	   end,
     suffix: "}");
end method;

define sealed method print-object (object :: <method>, stream :: <stream>)
    => ();
  pprint-logical-block
    (stream,
     prefix: "{",
     body: method (stream)
	     write("Method", stream);
	     let name = function-name(object);
	     if (name)
	       write(' ', stream);
	       pprint-newline(#"fill", stream);
	       write(as(<byte-string>, name), stream);
	     end;
	     let specializers = function-specializers(object);
	     write(' ', stream);
	     pprint-newline(#"fill", stream);
	     pprint-logical-block
	       (stream,
		prefix: "(",
		body: method (stream)
			print-items(specializers, print-specializer, stream);
		      end,
		suffix: ")");
	   end,
     suffix: "}");
end method;

/// print-items -- Internal Interface.
///
/// This function prints each element of items, separated by commas, using
/// print-fun.  This function also regards print-length.  Stream must be a
/// pretty printing stream or a <print-stream> whose target is a pretty
/// printing stream, so this function is basically good for use in body:
/// methods passed to pprint-logical-block.
///
/// Do not use this function for collections that may be tail-circular; it
/// will not terminate.
///
define method print-items (items :: <collection>, print-fun :: <function>,
			   stream :: <stream>)
    => ();
  block (exit)
    let length :: false-or(<integer>)
      = stream.print-length;
    let stream-for-apply = list(stream);
    for (x in items,
	 count = 0 then (count + 1))
      if (count ~= 0)
	write(", ", stream);
	pprint-newline(#"fill", stream);
      end;
      if (length & (count = length))
	write("...", stream);
	exit();
      end;
      apply(print-fun, x, stream-for-apply);
    end for;
  end block;
end method;



/// Print-specializer generic function and methods.
///

/// This function is used in printing methods.
///

define sealed generic print-specializer (type :: <type>, stream :: <stream>)
    => ();

define method print-specializer (type :: <type>, stream :: <stream>) => ();
  print(type, stream);
end method;

define method print-specializer (type :: <class>, stream :: <stream>)
    => ();
  let name = type.class-name;
  if (name)
    write(as(<byte-string>, name), stream);
  else
    print(type, stream);
  end if;
end method;

define method print-specializer (type :: <singleton>, stream :: <stream>)
    => ();
  write("singleton(", stream);
  print(type.singleton-object, stream);
  write(")", stream);
end method;

define method print-specializer (type :: <subclass>, stream :: <stream>)
    => ();
  write("subclass(", stream);
  print-specializer(type.subclass-of, stream);
  write(")", stream);
end method;

#if (~mindy)
define method print-specializer (type :: <direct-instance>, stream :: <stream>)
    => ();
  write("direct-instance(", stream);
  print-specializer(type.direct-instance-of, stream);
  write(")", stream);
end method;
#endif

#if (~mindy)
define method print-specializer
    (type :: <byte-character-type>, stream :: <stream>)
    => ();
  write("<byte-character>", stream);
end method;
#endif

define method print-specializer (type :: <limited-integer>, stream :: <stream>)
    => ();
  write("limited(", stream);
  print-specializer(type.limited-integer-base-class, stream);
  let min = type.limited-integer-minimum;
  if (min)
    write(", min: ", stream);
    write(integer-to-string(min), stream);
  end if;
  let max = type.limited-integer-maximum;
  if (max)
    write(", max: ", stream);
    write(integer-to-string(max), stream);
  end if;
  write(')', stream);
end method;

define method print-specializer (type :: <union>, stream :: <stream>)
    => ();
#if (mindy)
  let members = #();
  let singletons = #();
  for (member in type.union-members)
    if (instance?(member, <singleton>))
      singletons := pair(member.singleton-object, singletons);
    else
      members := pair(member, members);
    end if;
  end for;
  print-union(as(<simple-object-vector>, members),
	      as(<simple-object-vector>, singletons),
	      stream);
#else
  print-union(as(<simple-object-vector>, type.union-members),
	      as(<simple-object-vector>, type.union-singletons),
	      stream);
#endif
end method print-specializer;

define method print-union
    (members :: <simple-object-vector>, singletons :: <simple-object-vector>,
     stream :: <stream>)
  local
    method print-singletons (stream :: <stream>)
      if (singletons.size == 1)
	print-specializer(singleton(singletons.first), stream);
      else
	pprint-logical-block
	  (stream,
	   prefix: "one-of(",
	   body: method (stream :: <stream>) => ();
		   for (thing in singletons, first? = #t then #f)
		     unless (first?)
		       write(", ", stream);
		       pprint-newline(#"fill", stream);
		     end unless;
		     print(thing, stream);
		   end for;
		 end method,
	   suffix: ")");
      end if;
    end method print-singletons;
  if (members.empty?)
    if (singletons.empty?)
      write("{empty type}", stream);
    else
      print-singletons(stream);
    end if;
  else
    pprint-logical-block
      (stream,
       prefix: "type-union(",
       body: method (stream :: <stream>) => ();
	       let first? = #t;
	       for (member in members, first? = #t then #f)
		 unless (first?)
		   write(", ", stream);
		   pprint-newline(#"fill", stream);
		 end unless;
		 print-specializer(member, stream);
	       finally
		 unless (singletons.empty?)
		   unless (first?)
		     write(", ", stream);
		     pprint-newline(#"fill", stream);
		   end unless;
		   print-singletons(stream);
		 end unless;
	       end for;
	     end method,
       suffix: ")");
  end if;
end method;


/// Print-object methods for <type> and its subclasses.
///

// General method for lots of different kinds of types.  We don't
// specialize on <type>, because if print-specializer doesn't know how to
// print the type, it will call print, and we would just end up back here.
// Instead, we carefully enumerate the types that print-specializer can
// deal with.
// 
define sealed method print-object
    (object :: type-union(<singleton>, <limited-integer>, <union>),
     stream :: <stream>)
    => ();
  pprint-logical-block
    (stream,
     prefix: "{Type ",
     body: method (stream :: <stream>) => ();
	     print-specializer(object, stream);
	   end method,
     suffix: "}");
end method print-object;

// For classes, we just print the class name if there is one.
// 
define sealed method print-object (object :: <class>, stream :: <stream>)
    => ();
  let name = object.class-name;
  if (name)
    write("{Class ", stream);
    write(as(<byte-string>, name), stream);
    write("}", stream);
  else
    write("{Anonymous Class}", stream);
  end if;
end method;


/// Print-object miscellaneous methods.
///

/// #t.
///
define sealed method print-object (object :: singleton(#t), stream :: <stream>)
    => ();
  write("#t", stream);
end method;

/// #f.
///
define sealed method print-object (object :: singleton(#f), stream :: <stream>)
    => ();
  write("#f", stream);
end method;

/// Symbols.
///
define sealed method print-object (object :: <symbol>, stream :: <stream>)
    => ();
  write('#', stream);
  write-string-escaped(as(<string>, object), stream);
end method;

/// Integers.
///
define sealed method print-object
    (object :: <integer>, stream :: <stream>)
    => ();
  write(integer-to-string(object), stream);
end method;
///
define sealed method print-object
    (object :: <extended-integer>, stream :: <stream>)
    => ();
  write("#e", stream);
  write(integer-to-string(object), stream);
end method;

/// Ratios.
///
define sealed method print-object (object :: <ratio>, stream :: <stream>)
    => ();
  write(integer-to-string(object.numerator), stream);
  write('/', stream);
  write(integer-to-string(object.denominator), stream);
end;


/// Float printing.

define inline method print-float
    (num :: <float>, class :: <class>, digits :: <integer>,
     marker :: <character>, stream :: <stream>)
    => ();
  if (zero?(num))
    write("0.0", stream);
    write(marker, stream);
    write('0', stream);
  else
    if (negative?(num))
      num := -num;
      write('-', stream);
    end;
    let one = as(class, 1.0);
    let ten = as(class, 10.0);
    let one-tenth = as(class, 0.1);
    let (exponent, fraction)
      = if (num >= one)
	  for (exponent from 1,
	       fraction = num / ten then fraction / ten,
	       while: fraction >= one)
	  finally
	    values(exponent, fraction);
	  end;
	else
	  for (exponent from 0 by -1,
	       fraction = num then fraction * ten,
	       while: fraction < one-tenth)
	  finally
	    values(exponent, fraction);
	  end;
	end;
    fraction := fraction + as(class, 0.5) * ten ^ -digits;
    if (fraction >= one)
      fraction := fraction / ten;
      exponent := exponent + 1;
    end;
    let chars = make(<byte-string>, size: digits);
    let zeros = 0;
    for (i from 0 below digits)
      let (digit, remainder) = floor(fraction * ten);
      chars[i] := as(<character>, as(<integer>, digit) + 48);
      fraction := remainder;
      if (zero?(digit))
	zeros := zeros + 1;
      else
	zeros := 0;
      end;
    end;
    
    if (exponent > digits | exponent < -3)
      write(chars[0], stream);
      write('.', stream);
      write(chars, stream, start: 1, end: max(2, digits - zeros));
      write(marker, stream);
      write(integer-to-string(exponent - 1), stream);
    elseif (exponent == digits)
      write(chars, stream);
      write(".0", stream);
      write(marker, stream);
      write('0', stream);
    elseif (exponent > 0)
      write(chars, stream, start: 0, end: exponent);
      write('.', stream);
      write(chars, stream, start: exponent, end: max(exponent + 1, digits - zeros));
      write(marker, stream);
      write('0', stream);
    else
      write("0.", stream);
      for (i from exponent below 0)
	write('0', stream);
      end;
      write(chars, stream, end: digits - zeros);
      write(marker, stream);
      write('0', stream);
    end;
  end;
end;
  
define sealed method print-object (num :: <single-float>, stream :: <stream>)
    => ();
  print-float(num, <single-float>, 7, 's', stream);
end;

define sealed method print-object (num :: <double-float>, stream :: <stream>)
    => ();
  print-float(num, <double-float>, 15, 'd', stream);
end;

define sealed method print-object (num :: <extended-float>, stream :: <stream>)
    => ();
  print-float(num, <extended-float>, 34, 'x', stream);
end;



/// print-to-string -- Exported.
///
define generic print-to-string (object, #rest args,
				#key level, length, circle?, pretty?)
    => result :: <string>;

define method print-to-string (object, #rest args,
			       #key level :: false-or(<integer>),
			            length :: false-or(<integer>),
			            circle? :: <boolean>, pretty? :: <boolean>)
    => result :: <byte-string>;
  let s = make(<byte-string-output-stream>);
  apply(print, object, s, args);
  s.string-output-stream-string;
end method;



/// Streams protocol extensions for <print-stream>s.
///

/// These methods may change when pretty printing goes in.  In particular,
/// getting and releasing the buffer may interact with buffered pretty
/// printing stuff.
///

define constant bogus-buffer = make(<buffer>, size: 256);

define method stream-extension-get-output-buffer (stream :: <print-stream>)
    => (buffer :: <buffer>, next :: <buffer-index>, size :: <buffer-index>);
  if ((stream.print-circle?) & (stream.circular-first-pass?))
    values(bogus-buffer, 0, bogus-buffer.size);
  else
    stream-extension-get-output-buffer(stream.print-target);
  end;
end method;

define method stream-extension-release-output-buffer
    (stream :: <print-stream>, next :: <buffer-index>)
    => ();
  if (~ ((stream.print-circle?) & (stream.circular-first-pass?)))
    stream-extension-release-output-buffer(stream.print-target, next);
  end;
end method;

define method stream-extension-empty-output-buffer
    (stream :: <print-stream>, stop :: <buffer-index>)
    => ();
  if (~ ((stream.print-circle?) & (stream.circular-first-pass?)))
    stream-extension-empty-output-buffer(stream.print-target, stop);
  end;
end method;

define method stream-extension-force-secondary-buffers
    (stream :: <print-stream>)
    => ();
  if (~ ((stream.print-circle?) & (stream.circular-first-pass?)))
    stream-extension-force-secondary-buffers(stream.print-target);
  end;
end method;

define method stream-extension-synchronize (stream :: <print-stream>)
    => ();
  if (~ ((stream.print-circle?) & (stream.circular-first-pass?)))
    stream-extension-synchronize(stream.print-target);
  end;
end method;



/// Pretty-printer support.

/// The methods on this page extend the pprint interface to <print-stream>s.
/// Doing this allows users to write print-object methods that attempt to do
/// pretty printing, but when print is called with pretty?: #f, all the
/// pretty printing directions in the print-object method become no-ops.
///

/// pprint-logical-block -- Method for Exported Interface.
///
/// When pretty printing, we pass the print-target of the <print-stream> to
/// the recursive call to pprint-logical-block.  This causes
/// pprint-logical-block to wrap a pretty printing stream around the actual
/// target.  The body: method of the recursive call then wraps the
/// <print-stream> around the pretty printing stream, nesting the ultimate
/// target stream twice.
///
/// In the body: method of the recursive call, there is a check to see if
/// the target is the pretty-stream argument. They are == when the body
/// function passed to this <print-stream> method contains recursive calls
/// to pprint-logical-block.  The code works without the if test, but
/// besides saving a few stores into memory with the assignments, the code
/// seemed more clear with the if test; that is, it should be more clear to
/// future maintainers of this code that the method can be reentered on the
/// same stream and what happens when this method is reentered.
///
define sealed method pprint-logical-block
    (stream :: <print-stream>,
     #key column :: <integer> = 0, prefix :: false-or(<byte-string>),
          per-line-prefix :: false-or(<byte-string>),
	  body :: <function>, suffix :: false-or(<byte-string>))
    => ();
  if (prefix & per-line-prefix)
    error("Can't specify both a prefix: and a per-line-prefix:");
  end;
  case
    ((stream.print-circle?) & (stream.circular-first-pass?)) =>
      #f;   // Case is broken in Mindy.
    (stream.print-pretty?) =>
      let target = stream.print-target;
      pprint-logical-block(target,
			   column: column,
			   prefix: prefix,
			   per-line-prefix: per-line-prefix,
			   body: method (pretty-stream)
				   if (pretty-stream == target)
				     body(stream);
				   else
				     let orig-target = stream.print-target;
				     stream.print-target := pretty-stream;
				     body(stream);
				     stream.print-target := orig-target;
				   end;
				 end,
			   suffix: suffix);
    otherwise =>
      if (prefix | per-line-prefix)
	write(prefix | per-line-prefix, stream);
      end;
      body(stream);
      if (suffix)
	write(suffix, stream);
      end;
  end case;
end method;

/// pprint-newline -- Method for Exported Interface.
///
define sealed method pprint-newline
    (kind :: one-of(#"linear", #"miser", #"fill", #"mandatory"),
     stream :: <print-stream>)
    => ();
  case
    ((~ ((stream.print-circle?) & (stream.circular-first-pass?)))
       & stream.print-pretty?) =>
      pprint-newline(kind, stream.print-target);
    (kind == #"mandatory") =>
      write('\n', stream);
  end;
end;

define sealed method pprint-indent
    (relative-to :: one-of(#"block", #"current"), n :: <integer>,
     stream :: <print-stream>)
    => ();
  if ((~ ((stream.print-circle?) & (stream.circular-first-pass?)))
	& stream.print-pretty?)
    pprint-indent(relative-to, n, stream.print-target);
  end;
end;

define sealed method pprint-tab
    (kind :: one-of(#"line",#"section",#"line-relative",#"section-relative"),
     colnum :: <integer>, colinc :: <integer>, stream :: <print-stream>)
    => ();
  if ((~ ((stream.print-circle?) & (stream.circular-first-pass?)))
	& stream.print-pretty?)
    pprint-tab(kind, colnum, colinc, stream.print-target);
  end;
end;
