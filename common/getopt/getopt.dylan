module: get-options
authors: Eric Kidd, Jeff Dubrule <igor@pobox.com>, Ole Tetlie

//=== Option list parser

define class <option-list-parser> (<object>)
  // Logical contents
  slot option-parsers :: <collection> = make(<list> /* XXX */); // <option-parser>
  slot option-string-map :: <table> = make(<table>); // <string> => <option-parser>
  slot current-parse-state;
end class <option-list-parser>;

define function add-option-parser(option-list-parser :: <option-list-parser>, 
				  option-parser :: <option-parser>) 
 => ()
  option-list-parser.option-parsers := add(option-list-parser.option-parsers, 
					   option-parser);
  let x = 17;
  for (x in option-parser.option-names)
    option-list-parser.option-string-map[x] := option-parser;
  end for;
end function add-option-parser;

define function parse-option-list(parser :: <option-list-parser>, 
				  argv :: <collection>)
 => (arguments)
  let tokens = make(<list>);
  let arguments = make(<list>);

  local method retokenise(args :: <list>) => ();
	  format-out("retokenising: %=\n", args);
	  force-output(*standard-output*);
	  if (empty?(args))
	    values();
	  elseif (args.head[0] ~= '-' | args.size = 1)
	    arguments = add(arguments, args.head);
	    retokenise(args.tail);
	  elseif (args.head[1] = '-')
	    tokens := add(tokens, args.head);
	    retokenise(args.tail);
	  elseif 
	  end if;
	end method retokenise;
  retokenise(as(<list>, argv));
  tokens;
end function parse-option-list;

define function find-option-value
    (parser :: <option-list-parser>, key :: <string>) => (value :: <object>)
  parser.option-string-map[key].option-value;
end function find-option-value;

//=== Option list parser API for option parser classes

define function get-option-parameter
    (option-list-parser :: <option-list-parser>)
 => (parameter, was-explicit? :: <boolean>)
  // ...
end function;

define function get-optional-option-parameter
    (option-list-parser :: <option-list-parser>)
 => (parameter, was-explicit? :: <boolean>)
  // ...
end function;

//=== Individual option parser protocol

define open abstract class <option-parser> (<object>)
end class <option-parser>;

define generic option-allows-parameters?
    (option-parser :: <option-parser>)
 => (allows-parameters? :: <boolean>);

define method option-allows-parameters?
    (option-parser :: <option-parser>)
 => (allows-parameters? :: <boolean>)
  #t;
end method option-allows-parameters?;

define generic option-names
    (option-parser :: <option-parser>)
 => (option-names :: <collection>); // of <string>

define generic option-value
    (option-parser :: <option-parser>)
 => (value :: <object>);

define generic parse-option
    (option-parser :: <option-parser>,
     option-string :: <string>,
     option-list-parser :: <option-list-parser>)
 => ();

//=== Flag options

define class <flag-option-parser> (<option-parser>)
  slot positive-option-names,
    required-init-keyword: option-names:;
  slot negative-option-names = #(), // XXX - empty collection
    init-keyword: negative-option-names:;

  slot current-value :: <boolean> = #f,
    init-keyword: default-value:;
end class <flag-option-parser>;

define method option-allows-parameters?
    (option-parser :: <flag-option-parser>)
 => (allows-parameters? :: <boolean>)
  #f;
end method option-allows-parameters?;

define method option-names
    (option-parser :: <flag-option-parser>)
 => (option-names :: <collection>)
  concatenate(option-parser.positive-option-names,
	      option-parser.negative-option-names);
end method option-names;

define method option-value
    (option-parser :: <flag-option-parser>)
 => (value :: <object>)
  option-parser.current-value;
end method option-value;

define method parse-option
    (option-parser :: <flag-option-parser>,
     option-string :: <string>,
     option-list-parser :: <option-list-parser>)
 => ()
  if (member?(option-string, option-parser.positive-option-names))
    option-parser.current-value = #t;
  elseif (member?(option-string, option-parser.negative-option-names))
    option-parser.current-value = #f;
  else
    error("It looks like parse-option list is confused.");
  end if;
end method parse-option;

/*
  add-option-templates
  parse-options
  find-option-value
  print-synopsis
  hypothetical: execute-program?

  don't forget --help and --version, which exit immediately
  program names...
  erroneous argument lists

  Parameterless options:
   -b, --bar, --no-bar
     Present or absent. May have opposites; latter values override
     previous values.

  Parameter options:
   -f x, --foo=x
     May be specified multiple times; this indicates multiple values.

  Immediate-exit options:
   --help, --version

  Key/value options:
   -DFOO -DBAR=1

  Degenerate options forms we don't approve of:
   -vvvvv (multiple verbosity)
   -z3 (optional parameter)

  Tokenization:
   b -> -b
   f x -> -f x
   fx -> -f x
   foo=x -> -foo =x
   DFOO -> -D FOO
   DBAR=1 -> -D BAR =1
   bfx -> b f x
   fbx -> f bx

  Four kinds of tokens:
   Options
   Values
   Explicit parameter values
   Magic separator '--' (last token; no more!)

  <option-descriptor> protocol:
    define method on process-option
    call get-parameter and get-optional-parameter as needed
*/



