Module:       common-dylan-internals
Author:       Andy Armstrong
Synopsis:     Implementations of format-to-string and format-out
Copyright:    Original Code is Copyright (c) 1999-2000 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// String buffers

define class <string-buffer> (<stretchy-vector>)
  sealed slot size :: <integer>, init-value: 0, init-keyword: size:;
  slot %contents :: <byte-string> = make(<byte-string>, size: 32);
end class;

define sealed method add!
    (buffer :: <string-buffer>, e :: <byte-character>)
 => (buffer :: <string-buffer>);
  if (buffer.size + 1 > buffer.%contents.size)
    let new-contents = make(<byte-string>,
                            size: ash(buffer.%contents.size * 3, -1));
    %copy-bytes(new-contents, 0, buffer.%contents, 0, buffer.size);
    buffer.%contents := new-contents;
  end;
  buffer.%contents[buffer.size] := e;
  buffer.size := buffer.size + 1;
  buffer;
end method;

//---*** Is there a more efficient way to do this?
define function print-string
    (buffer :: <string-buffer>, string :: <byte-string>) => ()
  if(buffer.size + string.size > buffer.%contents.size)
    let new-size = max(ash(buffer.%contents.size * 3, -1),
                       buffer.size + string.size);
    let new-contents = make(<byte-string>, size: new-size);
    %copy-bytes(new-contents, 0, buffer.%contents, 0, buffer.size);
    buffer.%contents := new-contents;
  end if;
  %copy-bytes(buffer.%contents, buffer.size, string, 0, string.size);
  buffer.size := buffer.size + string.size;
end function print-string;

define function buffer-contents
    (buffer :: <string-buffer>) => (contents :: <byte-string>)
  copy-sequence(buffer.%contents, start: 0, end: buffer.size);
end function buffer-contents;


/// User assertions (maybe export this?)

/*
define macro user-assert
 { user-assert(?value:expression, ?format-string:expression, ?format-arguments:*) }
    => { unless (?value)
           user-assertion-error(?format-string, ?format-arguments)
	 end }
end macro user-assert;
*/

define class <user-assertion-error> (<error>, <format-string-condition>)
end class <user-assertion-error>;

define function user-assertion-error
    (format-string :: <string>, #rest format-arguments)
  error(make(<user-assertion-error>,
	     format-string: format-string,
	     format-arguments: format-arguments))
end function user-assertion-error;


/// format-to-string

define function print-format
    (buffer :: <string-buffer>, format-string :: <string>,
     #rest format-arguments)
 => ()
  let found-percent? = #f;
  let argument-index :: <integer> = 0;
  let no-of-arguments = size(format-arguments);
  local method argument 
	    (char :: <character>, class :: <class>) => (argument :: <object>)
          let current-index = argument-index;
          argument-index := argument-index + 1;
          unless (current-index < no-of-arguments)
            user-assertion-error("Too few arguments for format string %=: %=",
                                 format-string, format-arguments);
          end;
          let argument = format-arguments[current-index];
          unless (class == <object> | instance?(argument, class))
            user-assertion-error("Format argument for directive '%%%c' "
                                   "not of class %s: %=",
                                 char, class, argument);
          end;
          argument
        end;
  local method collect (string :: <string>) => ()
	  print-string(buffer, string)
        end method collect;
  local method collect-character (character :: <character>) => ()
	  add!(buffer, character)
	end method collect-character;
  for (char :: <character> in format-string)
    if (found-percent?)
      select (as-uppercase(char))
	'D' => collect(number-to-string(argument(char, <number>)));
	'B' => collect(integer-to-string(argument(char, <integer>), base: 2));
	'O' => collect(integer-to-string(argument(char, <integer>), base: 8));
	'X' => collect(integer-to-string(argument(char, <integer>), base: 16));
	'C' => collect-character(argument(char, <character>));
	'S' => print-pretty-name(buffer, argument(char, <object>));
	'=' => print-unique-name(buffer, argument(char, <object>));
	'%' => collect-character('%');
	otherwise =>
	  error("Invalid format directive '%s' in \"%s\"",
		char, format-string);
      end;
      found-percent? := #f;
    else
      if (char == '%')
        found-percent? := #t;
      else
	collect-character(char)
      end
    end
  end;
  if (found-percent?)
    user-assertion-error("Incomplete format directive in \"%s\"",
                         format-string);
  end;
end function print-format;

define function format-to-string
    (format-string :: <string>, #rest format-arguments)
 => (string :: <string>)
  let buffer :: <string-buffer> = make(<string-buffer>);
  apply(print-format, buffer, format-string, format-arguments);
  buffer-contents(buffer)
end function format-to-string;


/// Basic object printing

define method print-pretty-name
    (buffer :: <string-buffer>, object :: <object>)
 => ()
  let name = primitive-name(object);
  case
    name      => print-string(buffer, name);
    otherwise => print-unique-name(buffer, object);
  end
end method print-pretty-name;

define function object-class-name
    (object :: <object>) => (name :: false-or(<string>))
  select (object by instance?)
    <class>   => "<class>";
    otherwise => class-name(object-class(object))
  end
end function object-class-name;

define function print-basic-name
    (buffer :: <string-buffer>,
     #key object :: <object>,
          name :: false-or(<string>) = primitive-name(object),
          class-name :: <string> = object-class-name(object))
 => ()
  if (name)
    print-format(buffer, "{%s: %s}", class-name, name)
  else
    print-format(buffer, "{%s}", class-name)
  end
end function print-basic-name;

define method print-unique-name
    (buffer :: <string-buffer>, object :: <object>) => ()
  local method symbol-name (symbol :: <symbol>) => (name :: <string>)
	  as-lowercase(as(<string>, symbol))
	end method symbol-name;
  select (object by instance?)
    <byte-string>  => print-format(buffer, "\"%s\"", object);
    <symbol>       => print-format(buffer, "#\"%s\"", symbol-name(object));
    <character>    => print-format(buffer, "'%c'", object);
    <collection>   => print-collection(buffer, object);
    <boolean>      => print-string(buffer, if (object) "#t" else "#f" end);
    <integer>      => print-string(buffer, integer-to-string(object));
    <float>        => print-string(buffer, float-to-string(object));
    <machine-word> => print-string(buffer, machine-word-to-string(object));
    <method>       => print-method(buffer, object);
    otherwise      => print-basic-name(buffer, object: object);
  end
end method print-unique-name;

define function object-unique-name
    (object :: <object>) => (name :: <string>)
  let buffer :: <string-buffer> = make(<string-buffer>);
  print-unique-name(buffer, object);
  buffer-contents(buffer)
end function object-unique-name;

define function primitive-name
    (object :: <object>) => (name :: false-or(<string>))
  select (object by instance?)
    <byte-string>  => object;
    <character>    => make(<byte-string>, size: 1, fill: object);
    <condition>    => condition-to-string(object);
    <locator>      => as(<string>, object);
    <class>        => class-name(object);
    <function>     => function-name(object);
    otherwise      => #f;
  end
end function primitive-name;


/// Types

define method print-unique-name
    (buffer :: <string-buffer>, union :: <union>) => ()
  print-format(buffer, "{%s: ", object-class-name(union));
  unless (empty?(union.union-singletons))
    print-string(buffer, "one-of(");
    for (object in union.union-singletons, first? = #t then #f)
      unless (first?) print-string(buffer, ", ") end;
      print-unique-name(buffer, object);
    end for;
    print-string(buffer, ")");
  end;
  for(type in union.union-members,
      first? = union.union-singletons.empty? then #f)
    unless (first?) print-string(buffer, ", ") end;
    print-pretty-name(buffer, type);
  end;
  print-string(buffer, "}")
end method print-unique-name;

define method print-unique-name
    (buffer :: <string-buffer>, singleton :: <singleton>) => ()
  print-format(buffer, "{%s: ", object-class-name(singleton));
  print-unique-name(buffer, singleton-object(singleton));
  print-string(buffer, "}")
end method print-unique-name;


/// Machine-word/string conversion

define function machine-word-to-string
    (mw :: <machine-word>, #key prefix :: false-or(<string>) = "#x")
 => (string :: <string>)
  let halfword-size = ash($machine-word-size, -1);
  let digits-per-halfword = ash(halfword-size, -2);
  let high 
    = as(<integer>, u%shift-right(mw, halfword-size));
  let low 
    = as(<integer>, u%shift-right(u%shift-left(mw, halfword-size),
                                  halfword-size));
  concatenate-as(<string>,
		 prefix | "",
		 integer-to-string(high, base: 16, size: digits-per-halfword),
		 integer-to-string(low, base: 16, size: digits-per-halfword))
end function machine-word-to-string;

define function string-to-machine-word
    (str :: <string>, 
     #key start         :: <integer> = 0, 
          default = $unsupplied,
          end: stop     :: false-or(<integer>))
 => (n :: <machine-word>, next-key :: <integer>)
  let string-length :: <integer> = size(str);
  unless (start >= 0 & start < string-length)
    user-assertion-error("Start: %d is out of range [0, %d] for string %s",
                         start, string-length, str);
  end;
  if (stop)
    unless (stop >= start & stop <= string-length)
      user-assertion-error("Stop: %d is out of range [0, %d] for string %s.", 
                           stop, string-length, str);
    end;
  else
    stop := size(str)
  end;
  while (start < stop & member?(str[start], #(' ', '\n', '\r', '\f', '\t')))
    start := start + 1
  end;
  // Remove common prefixes (#x, 0x) ...
  if ((start < stop - 2)
	&((str[start] = '#' & str[start + 1] = 'x')
	    | (str[start] = '0' & str[start + 1] = 'x')))
    start := start + 2
  end;
  let (value, next-key)
    = string-to-integer(str, start: start, end: stop, base: 16);
  values(as(<machine-word>, value), next-key)
end function string-to-machine-word;


/// Condition/string conversion

define open generic condition-to-string
    (condition :: <condition>) => (string :: false-or(<string>));

define method condition-to-string
    (condition :: <condition>) => (string :: false-or(<string>))
  #f
end method condition-to-string;

define method condition-to-string
    (condition :: <format-string-condition>) => (string :: <string>)
  apply(format-to-string,
        condition-format-string(condition), 
        condition-format-arguments(condition))
end method condition-to-string;

define method condition-to-string
    (error :: <type-error>) => (string :: <string>)
  format-to-string("%= is not of type %=",
		   type-error-value(error),
		   type-error-expected-type(error))
end method condition-to-string;

define method print-pretty-name
    (buffer :: <string-buffer>, condition :: <condition>)
 => ()
  let message = condition-to-string(condition);
  if (message)
    print-string(buffer, message)
  else
    print-format(buffer, "Condition of class %s occurred",
		 object-class-name(condition))
  end
end method print-pretty-name;


/// Collection printing

define constant $collection-empty-text    = "size 0";
define variable *collection-print-length* = 10;

define method print-collection
    (buffer :: <string-buffer>, collection :: <collection>,
     #key print-length :: false-or(<integer>) = *collection-print-length*)
 => ()
  print-string(buffer, "{");
  print-string(buffer, object-class-name(collection));
  print-string(buffer, ": ");
  print-collection-contents(buffer, collection, print-length: print-length);
  print-string(buffer, "}");
end method print-collection;

define method print-collection-contents
    (buffer :: <string-buffer>, collection :: <collection>,
     #key print-length :: false-or(<integer>) = *collection-print-length*)
 => ()
  ignore(print-length);
  print-format(buffer, "size %d", size(collection))
end method print-collection-contents;

define method print-collection-contents
    (buffer :: <string-buffer>, array :: <array>,
     #key print-length :: false-or(<integer>) = *collection-print-length*)
 => ()
  let dimensions = dimensions(array);
  print-elements(buffer, dimensions,
		 print-length: print-length, separator: " x ")
end method print-collection-contents;

define method print-collection-contents
    (buffer :: <string-buffer>, range :: <range>,
     #key print-length :: false-or(<integer>) = *collection-print-length*)
 => ()
  ignore(print-length);
  local method print-range
	    (buffer :: <string-buffer>, from :: <real>, to :: <real>,
	     by :: <real>)
         => ()
	  select (by)
	    1         => print-format(buffer, "%d to %d", from, to);
	    otherwise => print-format(buffer, "%d to %d by %d", from, to, by);
	  end
	end method print-range;
  let range-size = size(range);
  if (range-size = 0)
    print-string(buffer, $collection-empty-text)
  else
    let from = range[0];
    let by   = if (~range-size | range-size > 1) range[1] - from else 1 end;
    select (range-size)
      1         => print-range(buffer, from, from, by);
      #f        => print-format(buffer, "%d by %d", from, by);
      otherwise => print-range(buffer, from, range[range-size - 1], by);
    end
  end
end method print-collection-contents;

define method print-collection-contents
    (buffer :: <string-buffer>, sequence :: <sequence>,
     #key print-length :: false-or(<integer>) = *collection-print-length*)
 => ()
  if (empty?(sequence))
    print-string(buffer, $collection-empty-text)
  else
    print-elements(buffer, sequence, print-length: print-length)
  end
end method print-collection-contents;

//---*** Needed to override the array method... can we avoid this?
define method print-collection-contents
    (buffer :: <string-buffer>, sequence :: <vector>,
     #key print-length :: false-or(<integer>) = *collection-print-length*)
 => ()
  if (empty?(sequence))
    print-string(buffer, $collection-empty-text)
  else
    print-elements(buffer, sequence, print-length: print-length)
  end
end method print-collection-contents;

define method print-collection-contents
    (buffer :: <string-buffer>, pair :: <pair>, #next next-method,
     #key print-length :: false-or(<integer>) = *collection-print-length*)
 => ()
  let tail-object = tail(pair);
  if (instance?(tail-object, <pair>))
    next-method()
  else
    print-format(buffer, "%=, %=", head(pair), tail-object)
  end     
end method print-collection-contents;

define function print-elements
    (buffer :: <string-buffer>, sequence :: <sequence>,
     #key print-length = *collection-print-length*,
          separator = ", ",
          print-function)
 => ()
  let current-separator = "";
  let sequence-size = size(sequence);
  let print-length = print-length | sequence-size;
  for (element in sequence,
       count from 0 below print-length)
    print-string(buffer, current-separator);
    current-separator := separator;
    case
      print-function =>
        print-function(buffer, element);
      instance?(element, <collection>) & ~instance?(element, <string>) =>
        print-basic-name(buffer, object: element, name: #f);
      otherwise =>
        print-unique-name(buffer, element);
    end
  end;
  if (sequence-size > print-length)
    print-string(buffer, separator);
    print-string(buffer, "...")
  end
end function print-elements;


/// Method printing

define function print-method
    (buffer :: <string-buffer>, object :: <method>) => ()
  print-string(buffer, "{");
  print-string(buffer, object-class-name(object));
  print-string(buffer, ": ");
  print-string(buffer, primitive-name(object));
  let specializers = function-specializers(object);
  print-string(buffer, " (");
  unless (empty?(specializers))
    print-elements(buffer, specializers, print-function: print-specializer)
  end;
  print-string(buffer, ")}");
end function print-method;

define method print-specializer
    (buffer :: <string-buffer>, type :: <type>) => ()
  print-unique-name(buffer, type)
end method print-specializer;

define method print-specializer
    (buffer :: <string-buffer>, object :: <class>) => ()
  print-pretty-name(buffer, object)
end method print-specializer;

define method print-specializer
    (buffer :: <string-buffer>, type :: <singleton>) => ()
  print-string(buffer, "singleton(");
  print-unique-name(buffer, singleton-object(type));
  print-string(buffer, ")")
end method print-specializer;

define method print-specializer
    (buffer :: <string-buffer>, type :: <subclass>) => ()
  print-string(buffer, "subclass(");
  print-pretty-name(buffer, subclass-class(type));
  print-string(buffer, ")")
end method print-specializer;


/// format-out

define function format-out
    (format-string :: <string>, #rest format-arguments)
 => ();
  let string :: <string>
    = apply(format-to-string, format-string, format-arguments);
  write-console(string);
end function format-out;

