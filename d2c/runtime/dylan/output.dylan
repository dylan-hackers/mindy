rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/output.dylan,v 1.1 1998/05/03 19:55:38 andreas Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
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

define method format (str :: <byte-string>, #rest args) => ();
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
	      write-integer(args[next-arg], 10);
	      scan(index + 1, next-arg + 1);
	    'b', 'B' =>
	      write-integer(args[next-arg], 2);
	      scan(index + 1, next-arg + 1);
	    'o', 'O' =>
	      write-integer(args[next-arg], 8);
	      scan(index + 1, next-arg + 1);
	    'x', 'X' =>
	      write-integer(args[next-arg], 16);
	      scan(index + 1, next-arg + 1);
	    'c', 'C' =>
	      puts(check-type(args[next-arg], <byte-character>));
	      scan(index + 1, next-arg + 1);
	    's', 'S' =>
	      print-message(args[next-arg]);
	      scan(index + 1, next-arg + 1);
	    '=' =>
	      print(args[next-arg]);
	      scan(index + 1, next-arg + 1);
	    '%' =>
	      puts('%');
	      scan(index + 1, next-arg);
	  end;
	else
	  puts(char);
	  scan(index + 1, next-arg);
	end;
      end;
    end;
  scan(0, 0);
end;



define generic print-message (thing :: <object>) => ();

define method print-message (str :: <byte-string>) => ();
  puts(str);
end;

define method print-message (sym :: <symbol>) => ();
  puts(as(<string>, sym));
end;

define method print-message (cond :: <condition>) => ();
  report-condition(cond, #"cheap-IO");
end;



define generic print (thing :: <object>) => ();

define method print (thing :: <object>) => ();
  let name = thing.object-class.class-name;
  if (name)
    format("{an instance of %s}", name);
  else
    puts("{an instance of something}");
  end if;
end;

define method print (char :: <character>) => ();
  puts('\'');
  write-maybe-escaping(char, '\'');
  puts('\'');
end;

define method print (str :: <byte-string>) => ();
  puts('"');
  for (char in str)
    write-maybe-escaping(char, '"');
  end;
  puts('"');
end;

define method write-maybe-escaping
    (char :: <character>, quote :: <character>) => ();
  if (char < ' ')
    select (char)
      '\0' => puts("\\0");
      '\a' => puts("\\a");
      '\b' => puts("\\b");
      '\t' => puts("\\t");
      '\f' => puts("\\f");
      '\r' => puts("\\r");
      '\n' => puts("\\n");
      '\e' => puts("\\e");
      otherwise =>
	format("\\{%x}", as(<integer>, char));
    end;
  elseif (char == quote)
    puts('\\');
    puts(char);
  elseif (char <= '~')
    puts(char);
  else
    format("\\{%x}", as(<integer>, char));
  end;
end;

define method print (sym :: <symbol>) => ();
  puts('#');
  print(as(<string>, sym));
end;

define method print (vec :: <simple-object-vector>) => ();
  puts("#[");
  block (return)
    for (count :: <integer> from 0, el in vec, first? = #t then #f)
      unless (first?)
	puts(", ");
      end;
      if (count == 10)
	puts("...");
	return();
      end;
      print(el);
    end;
  end;
  puts(']');
end;

define method print (list :: <list>) => ();
  puts("#(");
  block (return)
    for (count :: <integer> from 0,
	 list = list then list.tail,
	 first? = #t then #f,
	 until: list == #())
      if (instance?(list, <pair>))
	unless (first?)
	  puts(", ");
	end;
	if (count == 10)
	  puts("...");
	  return();
	end;
	print(list.head);
      else
	puts(" . ");
	print(list);
	return();
      end;
    end;
  end;
  puts(')');
end;

define method print (func :: <function>) => ();
  format("{the %s %s}", func.object-class.class-name, func.function-name);
end method print;

define method print (class :: <class>) => ();
  let name = class.class-name;
  if (name)
    format("{the class %s}", name);
  else
    puts("{some random class}");
  end if;
end;

define method print (true == #t) => ();
  puts("#t");
end;

define method print (false == #f) => ();
  puts("#f");
end;

define method print (int :: <integer>) => ();
  write-integer(int, 10);
end;

define method print (int :: <extended-integer>) => ();
  puts("#e");
  write-integer(int, 10);
end;


define generic write-integer (int :: <general-integer>, radix :: <integer>)
    => ();

define method write-integer (int :: <integer>, radix :: <integer>)
    => ();
  local
    method repeat (int :: <integer>)
      let (remaining, digit) = floor/(int, radix);
      unless (zero?(remaining))
	repeat(remaining);
      end;
      if (digit < 10)
	puts(digit + as(<integer>, '0'));
      else
	puts(digit + as(<integer>, 'a') - 10);
      end;
    end;
  if (negative?(int))
    puts('-');
    let (negative-remaining, negative-digit) = truncate/(int, radix);
    unless (zero?(negative-remaining))
      repeat(-negative-remaining);
    end unless;
    if (negative-digit > -10)
      puts(as(<integer>, '0') - negative-digit);
    else
      puts(as(<integer>, 'a') - negative-digit - 10);
    end if;
  else
    repeat(int);
  end;
end;

define method write-integer
    (int :: <extended-integer>, radix :: <integer>) => ();
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
	puts('-');
	repeat(-int, #());
      else
	repeat(int, #());
      end;
  for (digit :: <integer> in digits)
    puts(digit);
  end;
end;


define generic puts (thing :: <object>) => ();

define inline method puts
    (int :: limited(<integer>, min: 0, max: 255)) => ();
  call-out("putchar", void:, int: int);
end;

define inline method puts (char :: <byte-character>) => ();
  puts(as(<integer>, char));
end;

define method puts (str :: <byte-string>) => ();
  for (char in str)
    puts(char);
  end;
end;

