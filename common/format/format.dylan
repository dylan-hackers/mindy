module: format
author: Gwydion Project
synopsis: This file implements a simple mechanism for formatting output.
copyright: See below.
rcs-header: $Header: /scm/cvs/src/common/format/format.dylan,v 1.7 2003/06/02 07:16:55 housel Exp $

///======================================================================
///
/// Copyright (c) 1994  Carnegie Mellon University
/// All rights reserved.
/// 
/// Use and copying of this software and preparation of derivative
/// works based on this software are permitted, including commercial
/// use, provided that the following conditions are observed:
/// 
/// 1. This copyright notice must be retained in full on any copies
///    and on appropriate parts of any derivative works.
/// 2. Documentation (paper or online) accompanying any system that
///    incorporates this software, or any part of it, must acknowledge
///    the contribution of the Gwydion Project at Carnegie Mellon
///    University, and the Gwydion Dylan Maintainers.
/// 
/// This software is made available "as is".  Neither the authors nor
/// Carnegie Mellon University make any warranty about the software,
/// its performance, or its conformity to any specification.
/// 
/// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
/// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
/// Also, see http://www.gwydiondylan.org/ for updates and documentation. 
///
///======================================================================
///

/// ### Instances of 'as(<integer> /***/' should be changed to 'as(<byte>'
/// when d2c can deal.
///

/// This code was modified at Harlequin, Inc. to work with the new Streams
/// Library designed by Harlequin and CMU.
///



/// format-to-string.
///

/// format-to-string -- Exported.
///
define generic format-to-string (control-string :: <string>, #rest args)
    => result :: <string>;

define method format-to-string (control-string :: <byte-string>, #rest args)
    => result :: <byte-string>;
  // Format-to-string is typically used for small amounts of output, so
  // use a smaller string to collect the contents.
  let s = make(<byte-string-stream>, contents: make(<byte-string>, size: 200),
	       direction: #"output");
  apply(format, s, control-string, args);
  s.stream-contents;
end method;



/// Print-message.
///

/// print-message -- Exported.
///
define open generic print-message (object :: <object>, stream :: <stream>)
    => ();


define method print-message (object :: <object>, stream :: <stream>)
 => ();
  let name = object.object-class.class-name;
  if (name)
    format(stream, "{an instance of %s}", name);
  else
    print-message("{an instance of something}", stream);
  end if;
end method print-message;

define method print-message (class :: <class>, stream :: <stream>,
                             #next next-method)
 => ();
  let name = class.class-name;
  if (name)
    write(stream, name);
  else
    next-method();
  end if;
end method print-message;

define sealed method print-message (object :: <string>, stream :: <stream>)
    => ();
  // Buffer-do-dispatch hand codes the sealed semantics of this method and
  // the write method on <string> and <character> to maximize buffer
  // handling efficiency.  Any changes in these sealed semantics should be
  // reflected in buffer-do-dispatch.
  write(stream, object);
end method;

define sealed method print-message (object :: <character>, stream :: <stream>)
    => ();
  // Buffer-do-dispatch hand codes the sealed semantics of this method and
  // the write method on <string> and <character> to maximize buffer
  // handling efficiency.  Any changes in these sealed semantics should be
  // reflected in buffer-do-dispatch.
  write-element(stream, object);
end method;

define sealed method print-message (object :: <condition>, stream :: <stream>)
    => ();
  report-condition(object, stream);
end method;

define sealed method print-message (object :: <symbol>, stream :: <stream>)
    => ();
  write(stream, as(<string>, object));
end method;

#if(~mindy)
define sealed method print-message (object :: <raw-pointer>, stream :: <stream>)
 => ();
  format(stream, "%x", as(<integer>, <raw-pointer>));
end method print-message;
#endif


/// Format.
///

define constant $dispatch-char = '%';

define constant char-classes = make(<vector>, size: 256, fill: #f);
///
for (i from as(<integer> /***/, '0') below (as(<integer> /***/, '9') + 1))
  char-classes[i] := #"digit";
end;
char-classes[as(<integer> /***/, '-')] := #"digit";


define generic format (stream :: <stream>, control-string :: <string>,
		       #rest args)
    => ();

define method format (stream :: <stream>, control-string :: <byte-string>,
		      #rest args)
    => ();
  let control-len :: <integer> = control-string.size;
  block (exit)
    let start :: <integer> = 0;
    let arg-i :: <integer> = 0;
    // Ensure all output is contiguous at stream's destination.
    lock-stream(stream);
    while (start < control-len)
      // Skip to dispatch char.
      for (i = start then (i + 1),
	   until: ((i == control-len)
		     | (control-string[i] == $dispatch-char)
		     | (control-string[i] == '\n')))
      finally
	write(stream, control-string, start: start, end: i);
	if (i == control-len)
	  exit();
	else
	  start := i + 1;
	end;
      end for;

      if(control-string[start - 1] == '\n')
	new-line(stream);
      else
	// Parse for field within which to pad output.
	let (field, field-spec-end)
	  = if (char-classes[as(<integer>, control-string[start])] == #"digit")
	      parse-integer(control-string, start);
	    end;
	if (field)
	  // Capture output in string and compute padding.
	  // Assume the output is very small in length.
	  let s = make(<byte-string-stream>,
		       contents: make(<byte-string>, size: 80),
		       direction: #"output");
	  if (do-dispatch(control-string[field-spec-end], s,
			  element(args, arg-i, default: #f)))
	    arg-i := arg-i + 1;
	  end;
	  let output = s.stream-contents;
	  let output-len :: <integer> = output.size;
	  let padding :: <integer> = (abs(field) - output-len);
	  case
	    (padding < 0) =>
	      write(stream, output);
	    (field > 0) =>
	      write(stream, make(<byte-string>, size: padding, fill: ' '));
	      write(stream, output);
	    otherwise =>
	      write(stream, output);
	      write(stream, make(<byte-string>, size: padding, fill: ' '));
	  end;
	  start := field-spec-end + 1;  // Add one to skip dispatch char.
	else
	  if (do-dispatch(control-string[start], stream,
			  element(args, arg-i, default: #f)))
	    arg-i := arg-i + 1;
	  end;
	  start := start + 1;  // Add one to skip dispatch char.
	end;
      end if;
    end while;
  cleanup
    unlock-stream(stream);
  end;
end method;
    
/// do-dispatch -- Internal.
///
/// This function dispatches on char, which should be a format directive.
/// The return value indicates whether to consume one format argument;
/// otherwise, consume none.
///
define function do-dispatch (char :: <byte-character>, stream :: <stream>, arg)
    => consumed-arg? :: <boolean>;
  select (char by \==)
    ('s'), ('S'), ('c'), ('C') =>
      print-message(arg, stream);
      #t;
    ('=') =>
      print(arg, stream);
      #t;
    ('d'), ('D') =>
      format-integer(arg, 10, stream);
      #t;
    ('b'), ('B') =>
      format-integer(arg, 2, stream);
      #t;
    ('o'), ('O') =>
      format-integer(arg, 8, stream);
      #t;
    ('x'), ('X') =>
      format-integer(arg, 16, stream);
      #t;
    ('m'), ('M') =>
      apply(arg, list(stream));
      #t;
    ('%') =>
      write-element(stream, '%');
      #f;
    otherwise =>
      error("Unknown format dispatch character, %c", char);
  end;
end function;

/// parse-integer -- Internal.
///
/// This function reads an integer from input starting at index.  Index must
/// be at the first digit or a leading negative sign.  This function reads
/// decimal representation, and it stops at the first character that is not
/// a decimal digit.  It returns the integer parsed and the index
/// immediately following the last decimal digit.
///
define function parse-integer (input :: <byte-string>, index :: <integer>)
    => (result :: false-or(<integer>), index :: <integer>);
  let result :: <integer> = 0;
  let negative? = if (input[index] == '-')
		    index := index + 1;
		  end;
  for (i :: <integer> = index then (i + 1),
       len :: <integer> = input.size then len,
       ascii-zero :: <byte> = as(<integer> /***/, '0') then ascii-zero,
       until: ((i == len) |
		 (~ (char-classes[as(<integer> /***/, input[i])] == #"digit"))))
    result := ((result * 10) + (as(<integer> /***/, input[i]) - ascii-zero));
  finally
    if (result == 0)
      values(#f, index);
    else
      values(if (negative?) (- result) else result end, i);
    end;
  end;
end function;


define constant $digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

/// format-integer -- internal.
///
define method format-integer (arg :: <extended-integer>,
			      radix :: limited(<integer>, min: 2, max: 36),
			      stream :: <stream>)
 => ();
  // Define an iteration that collects the digits for the print
  // representation of arg.
  local method repeat (arg /* :: <general-integer> */, digits :: <list>)
	  let (quotient, remainder) = floor/(arg, radix);
	  // remainder is prob. an <extended-integer>, even though it's small.
	  let digits = pair($digits[as(<integer>, remainder)], digits);
	  if (zero?(quotient))
	    for (digit in digits)
	      write-element(stream, digit);
	    end;
	  else
	    repeat(quotient, digits);
	  end;
	end;
  // Set up for the iteration.
  if (negative?(arg))
    write-element(stream, '-');
    repeat(-arg, #());
  else
    repeat(arg, #());
  end;
end method format-integer;

/// format-integer -- internal.
///
define method format-integer (arg :: <integer>,
			      radix :: limited(<integer>, min: 2, max: 36),
			      stream :: <stream>)
 => ();
  // Define an iteration that collects the digits for the print
  // representation of arg.
  local method repeat (arg :: <integer>, digits :: <list>)
	  let (quotient :: <integer>, remainder :: <integer>)
	    = floor/(arg, radix);
	  let digits = pair($digits[remainder], digits);
	  if (zero?(quotient))
	    for (digit in digits)
	      write-element(stream, digit);
	    end;
	  else
	    repeat(quotient, digits);
	  end;
	end;
  // Set up for the iteration.
  if (negative?(arg))
    write-element(stream, '-');
    // Pick off one digit before beginning the iteration to ensure that we
    // don't need Generic-arithmetic.  If arg were the mininum signed
    // machine word, and we simply negated it and called repeat, then it
    // would turn into an integer that was one larger than the maximum
    // signed integer.
    let (quotient :: <integer>, remainder :: <integer>)
      = truncate/(arg, radix);
    if (~ zero?(quotient))
      repeat(- quotient, list($digits[- remainder]));
    else
      write-element(stream, $digits[- remainder]);
    end;
  else
    repeat(arg, #());
  end;
end method format-integer;

// borrowed from FD version
//
define method format-integer
    (arg :: <float>,
     radix :: limited(<integer>, min: 2, max: 36),
     stream :: <stream>)
 => ();
  //--- Should we really be this compulsive?
  unless (radix == 10)
    error("Can only print floats in base 10");
  end;
  print(arg, stream)
end method;


// Condition-Format and Condition-Force-Output methods.
//
// Condition-Format and Condition-Force-Output are generic functions that are
// called by the condition system to service its output needs.  We supply
// method on <stream>s that call the <stream> specific functions.
//
define method condition-format
    (stream :: <stream>, string :: <string>, #rest args) => ();
  apply(format, stream, string, args);
end method condition-format;
//
define method condition-force-output
    (stream :: <stream>) => ();
  force-output(stream);
end method condition-force-output;
