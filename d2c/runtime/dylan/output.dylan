rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/output.dylan,v 1.8 1996/01/12 02:10:50 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

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
	    'd' =>
	      write-integer(args[next-arg], 10);
	      scan(index + 1, next-arg + 1);
	    'b' =>
	      write-integer(args[next-arg], 2);
	      scan(index + 1, next-arg + 1);
	    'o' =>
	      write-integer(args[next-arg], 8);
	      scan(index + 1, next-arg + 1);
	    'x' =>
	      write-integer(args[next-arg], 16);
	      scan(index + 1, next-arg + 1);
	    'c' =>
	      write(check-type(args[next-arg], <byte-character>));
	      scan(index + 1, next-arg + 1);
	    's' =>
	      print-message(args[next-arg]);
	      scan(index + 1, next-arg + 1);
	    '=' =>
	      print(args[next-arg]);
	      scan(index + 1, next-arg + 1);
	    '%' =>
	      write('%');
	      scan(index + 1, next-arg);
	  end;
	else
	  write(char);
	  scan(index + 1, next-arg);
	end;
      end;
    end;
  scan(0, 0);
end;



define generic print-message (thing :: <object>) => ();

define method print-message (str :: <byte-string>) => ();
  write(str);
end;

define method print-message (sym :: <symbol>) => ();
  write(as(<string>, sym));
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
    write("{an instance of something}");
  end if;
end;

define method print (char :: <character>) => ();
  write('\'');
  write-maybe-escaping(char, '\'');
  write('\'');
end;

define method print (str :: <byte-string>) => ();
  write('"');
  for (char in str)
    write-maybe-escaping(char, '"');
  end;
  write('"');
end;

define method write-maybe-escaping
    (char :: <character>, quote :: <character>) => ();
  if (char < ' ')
    select (char)
      '\0' => write("\\0");
      '\a' => write("\\a");
      '\b' => write("\\b");
      '\t' => write("\\t");
      '\f' => write("\\f");
      '\r' => write("\\r");
      '\n' => write("\\n");
      '\e' => write("\\e");
      otherwise =>
	format("\\{%x}", as(<integer>, char));
    end;
  elseif (char == quote)
    write('\\');
    write(char);
  elseif (char <= '~')
    write(char);
  else
    format("\\{%x}", as(<integer>, char));
  end;
end;

define method print (sym :: <symbol>) => ();
  write('#');
  print(as(<string>, sym));
end;

define method print (vec :: <simple-object-vector>) => ();
  write("#[");
  block (return)
    for (count :: <integer> from 0, el in vec, first? = #t then #f)
      unless (first?)
	write(", ");
      end;
      if (count == 10)
	write("...");
	return();
      end;
      print(el);
    end;
  end;
  write(']');
end;

define method print (list :: <list>) => ();
  write("#(");
  block (return)
    for (count :: <integer> from 0,
	 list = list then list.tail,
	 first? = #t then #f,
	 until: list == #())
      if (instance?(list, <pair>))
	unless (first?)
	  write(", ");
	end;
	if (count == 10)
	  write("...");
	  return();
	end;
	print(list.head);
      else
	write(" . ");
	print(list);
	return();
      end;
    end;
  end;
  write(')');
end;

define method print (func :: <function>) => ();
  format("{the %s %s}", func.object-class.class-name, func.function-name);
end method print;

define method print (class :: <class>) => ();
  let name = class.class-name;
  if (name)
    format("{the class %s}", name);
  else
    write("{some random class}");
  end if;
end;

define method print (true == #t) => ();
  write("#t");
end;

define method print (false == #f) => ();
  write("#f");
end;

define method print (int :: <integer>) => ();
  write-integer(int, 10);
end;

define method print (int :: <extended-integer>) => ();
  write("#e");
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
	write(digit + as(<integer>, '0'));
      else
	write(digit + as(<integer>, 'a') - 10);
      end;
    end;
  if (negative?(int))
    write('-');
    repeat(-int);
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
	write('-');
	repeat(-int, #());
      else
	repeat(int, #());
      end;
  for (digit :: <integer> in digits)
    write(digit);
  end;
end;


define generic write (thing :: <object>) => ();

define inline method write
    (int :: limited(<integer>, min: 0, max: 255)) => ();
  %%primitive call-out ("putchar", void:, int: int);
end;

define inline method write (char :: <byte-character>) => ();
  write(as(<integer>, char));
end;

define method write (str :: <byte-string>) => ();
  for (char in str)
    write(char);
  end;
end;

