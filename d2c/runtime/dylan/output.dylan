rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/output.dylan,v 1.7 2002/10/31 16:05:51 housel Exp $
copyright: see below
module: dylan-viscera

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
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

c-system-include("stdio.h");

// XXX - We export this, instead of cheap-format, for reasons of backwards
// compatibility. Somebody should find out what Harlequin does, and fix
// this to work like their implementation.
define method format (str :: <byte-string>, #rest args) => ();
  apply(cheap-format, #"Cheap-IO", str, args);
end;

define method cheap-format
    (fake-stream :: <symbol>, str :: <byte-string>, #rest args)
 => ()
  let c-file = cheap-io-to-stdio(fake-stream);
  let finis = str.size;
  local
    method scan (index :: <integer>, next-arg :: <integer>)
      unless (index == finis)
	let char = str[index];
	if (char == '%')
	  let index = index + 1;
	  if (index == finis)
	    error("Bogus control string: %=", str);
	  end;
	  let char = str[index];
	  select (char)
	    'd', 'D' =>
	      cheap-write-integer(fake-stream, args[next-arg], 10);
	      scan(index + 1, next-arg + 1);
	    'b', 'B' =>
	      cheap-write-integer(fake-stream, args[next-arg], 2);
	      scan(index + 1, next-arg + 1);
	    'o', 'O' =>
	      cheap-write-integer(fake-stream, args[next-arg], 8);
	      scan(index + 1, next-arg + 1);
	    'x', 'X' =>
	      cheap-write-integer(fake-stream, args[next-arg], 16);
	      scan(index + 1, next-arg + 1);
	    'c', 'C' =>
	      fputs-internal(check-type(args[next-arg], <byte-character>),
			     c-file);
	      scan(index + 1, next-arg + 1);
	    's', 'S' =>
	      cheap-print-message(args[next-arg], fake-stream);
	      scan(index + 1, next-arg + 1);
	    '=' =>
	      cheap-print(args[next-arg], fake-stream);
	      scan(index + 1, next-arg + 1);
	    '%' =>
	      fputs-internal('%', c-file);
	      scan(index + 1, next-arg);
	  end;
	elseif (char == '\n')
#if (newlines-are-CRLF)
          fputs-internal('\r', c-file);
	  fputs-internal(char, c-file);
#else
	  fputs-internal(char, c-file);
#endif
	  scan(index + 1, next-arg);
	else
	  fputs-internal(char, c-file);
	  scan(index + 1, next-arg);
	end;
      end;
    end;
  scan(0, 0);
end method cheap-format;



define function print-message(thing :: <object>) => ()
  cheap-print-message(thing, #"Cheap-IO");
end function print-message;

define generic cheap-print-message
    (thing :: <object>, fake-stream :: <symbol>) => ();

define method cheap-print-message
    (str :: <byte-string>, fake-stream :: <symbol>) => ()
  fputs(str, fake-stream);
end;

define method cheap-print-message
    (sym :: <symbol>, fake-stream :: <symbol>) => ()
  fputs(as(<string>, sym), fake-stream);
end;

define method cheap-print-message
    (cond :: <condition>, fake-stream :: <symbol>) => ()
  report-condition(cond, fake-stream);
end;



define function print (thing :: <object>) => ()
  cheap-print(thing, #"Cheap-IO");
end function print;

define generic cheap-print
    (thing :: <object>, fake-stream :: <symbol>) => ();

define method cheap-print (thing :: <object>, fake-stream :: <symbol>) => ();
  let name = thing.object-class.class-name;
  if (name)
    cheap-format(fake-stream, "{an instance of %s}", name);
  else
    fputs("{an instance of something}", fake-stream);
  end if;
end;

define method cheap-print (char :: <character>, fake-stream :: <symbol>) => ();
  fputs('\'', fake-stream);
  write-maybe-escaping(fake-stream, char, '\'');
  fputs('\'', fake-stream);
end;

define method cheap-print
    (str :: <byte-string>, fake-stream :: <symbol>) => ();
  fputs('"', fake-stream);
  for (char in str)
    write-maybe-escaping(fake-stream, char, '"');
  end;
  fputs('"', fake-stream);
end;

define method write-maybe-escaping
    (fake-stream :: <symbol>, char :: <character>, quote :: <character>)
 => ()
  if (char < ' ')
    select (char)
      '\0' => fputs("\\0", fake-stream);
      '\a' => fputs("\\a", fake-stream);
      '\b' => fputs("\\b", fake-stream);
      '\t' => fputs("\\t", fake-stream);
      '\f' => fputs("\\f", fake-stream);
      '\r' => fputs("\\r", fake-stream);
      '\n' => fputs("\\n", fake-stream);
      '\e' => fputs("\\e", fake-stream);
      otherwise =>
	cheap-format(fake-stream, "\\{%x}", as(<integer>, char));
    end;
  elseif (char == quote)
    fputs('\\', fake-stream);
    fputs(char, fake-stream);
  elseif (char <= '~')
    fputs(char, fake-stream);
  else
    cheap-format(fake-stream, "\\{%x}", as(<integer>, char));
  end;
end;

define method cheap-print (sym :: <symbol>, fake-stream :: <symbol>) => ();
  fputs('#', fake-stream);
  cheap-print(as(<string>, sym), fake-stream);
end;

define method cheap-print
    (vec :: <simple-object-vector>, fake-stream :: <symbol>) => ();
  fputs("#[", fake-stream);
  block (return)
    for (count :: <integer> from 0, el in vec, first? = #t then #f)
      unless (first?)
	fputs(", ", fake-stream);
      end;
      if (count == 10)
	fputs("...", fake-stream);
	return();
      end;
      cheap-print(el, fake-stream);
    end;
  end;
  fputs(']', fake-stream);
end;

define method cheap-print (list :: <list>, fake-stream :: <symbol>) => ();
  fputs("#(", fake-stream);
  block (return)
    for (count :: <integer> from 0,
	 list = list then list.tail,
	 first? = #t then #f,
	 until: list == #())
      if (instance?(list, <pair>))
	unless (first?)
	  fputs(", ", fake-stream);
	end;
	if (count == 10)
	  fputs("...", fake-stream);
	  return();
	end;
	print(list.head);
      else
	fputs(" . ", fake-stream);
	cheap-print(list, fake-stream);
	return();
      end;
    end;
  end;
  fputs(')', fake-stream);
end;

define method cheap-print (func :: <function>, fake-stream :: <symbol>) => ();
  cheap-format(fake-stream, "{the %s %s}",
	       func.object-class.class-name, func.function-name);
end method cheap-print;

define method cheap-print (class :: <class>, fake-stream :: <symbol>) => ();
  let name = class.class-name;
  if (name)
    cheap-format(fake-stream, "{the class %s}", name);
  else
    fputs("{some random class}", fake-stream);
  end if;
end;

define method cheap-print (true == #t, fake-stream :: <symbol>) => ();
  fputs("#t", fake-stream);
end;

define method cheap-print (false == #f, fake-stream :: <symbol>) => ();
  fputs("#f", fake-stream);
end;

define method cheap-print (int :: <integer>, fake-stream :: <symbol>) => ();
  cheap-write-integer(fake-stream, int, 10);
end;

define method cheap-print
    (int :: <extended-integer>, fake-stream :: <symbol>) => ();
  fputs("#e", fake-stream);
  cheap-write-integer(fake-stream, int, 10);
end;

define method cheap-print
    (coll :: <limited-collection>, fake-stream :: <symbol>) => ()
  let base = coll.limited-integer-base-class;
  let of = coll.limited-element-type;
  let size = coll.limited-size-restriction;
  let dim = coll.limited-dimensions;

  cheap-format(fake-stream, "{limited %=", base);
  if (of) cheap-format(fake-stream, ", of: %=", of) end;
  if (size) cheap-format(fake-stream, ", size: %=", size) end;
  if (dim) cheap-format(fake-stream, ", dim: %=", dim) end;
  fputs("}", fake-stream);
end;

define method cheap-print
    (obj :: <limited-collection-mixin>, fake-stream :: <symbol>) => ()
  let class = obj.object-class.class-name;
  let base = obj.%limited-collection-type;
  cheap-format(fake-stream, "{an instance of %s, a %=}", class, base);
end;


define function write-integer
    (int :: <general-integer>, radix :: <integer>) => ()
  cheap-write-integer(#"Cheap-IO", int, radix);
end function write-integer;

define generic cheap-write-integer
    (fake-stream :: <symbol>, int :: <general-integer>, radix :: <integer>)
    => ();

define method cheap-write-integer
    (fake-stream :: <symbol>, int :: <integer>, radix :: <integer>)
 => ()
  local
    method repeat (int :: <integer>)
      let (remaining, digit) = floor/(int, radix);
      unless (zero?(remaining))
	repeat(remaining);
      end;
      if (digit < 10)
	fputs(digit + as(<integer>, '0'), fake-stream);
      else
	fputs(digit + as(<integer>, 'a') - 10, fake-stream);
      end;
    end;
  if (negative?(int))
    fputs('-', fake-stream);
    let (negative-remaining, negative-digit) = truncate/(int, radix);
    unless (zero?(negative-remaining))
      repeat(-negative-remaining);
    end unless;
    if (negative-digit > -10)
      fputs(as(<integer>, '0') - negative-digit, fake-stream);
    else
      fputs(as(<integer>, 'a') - negative-digit - 10, fake-stream);
    end if;
  else
    repeat(int);
  end;
end;

define method cheap-write-integer
    (fake-stream :: <symbol>, int :: <extended-integer>, radix :: <integer>)
 => ()
  local
    method repeat (int :: <extended-integer>, digits :: <list>)
      let (remaining, digit) = floor/(int, radix);
      let digit = as(<integer>, digit);
      let digits
	= pair(if (digit < 10)
		 digit + as(<integer>, '0');
	       else
		 digit + as(<integer>, 'a') - 10;
	       end,
	       digits);
      if (zero?(remaining))
	digits;
      else
	repeat(remaining, digits);
      end;
    end;
  let digits
    = if (negative?(int))
	fputs('-', fake-stream);
	repeat(-int, #());
      else
	repeat(int, #());
      end;
  for (digit :: <integer> in digits)
    fputs(digit, fake-stream);
  end;
end;


define function cheap-io-to-stdio(stream :: <symbol>)
 => (c-file :: <raw-pointer>)
  select (stream)
    #"Cheap-IO" => c-expr(ptr: "stdout");
    #"Cheap-Err" => c-expr(ptr: "stderr");
    otherwise =>
      fputs("cheap-io-to-stdio: bad Dylan Cheap-IO stream\n", #"Cheap-Err");
      cheap-force-output(#"Cheap-Err");
      call-out("abort", void:);
  end select;
end function cheap-io-to-stdio;

define function puts(thing :: <object>) => ()
  fputs(thing, #"Cheap-IO");
end function puts;

define function fputs(thing :: <object>, fake-stream :: <symbol>) => ()
  fputs-internal(thing, cheap-io-to-stdio(fake-stream));
end function fputs;

define generic fputs-internal
    (thing :: <object>, c-file :: <raw-pointer>)
 => ();

define inline method fputs-internal
    (int :: limited(<integer>, min: 0, max: 255),
     c-file :: <raw-pointer>)
 => ()
  c-system-include("stdio.h");
  call-out("fputc", int:, int: int, ptr: c-file);
end;

define inline method fputs-internal
    (char :: <byte-character>, c-file :: <raw-pointer>)
 => ()
  fputs-internal(as(<integer>, char), c-file);
end;

define method fputs-internal
    (str :: <byte-string>, c-file :: <raw-pointer>)
 => ()
  for (char in str)
    fputs-internal(char, c-file);
  end;
end;



define method cheap-force-output(fake-stream :: <symbol>) => ()
  call-out("fflush", int:, ptr: cheap-io-to-stdio(fake-stream));
end method cheap-force-output;
