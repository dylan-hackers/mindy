module: parse-arguments
synopsis: Individual option parsers.
authors: Eric Kidd, Jeff Dubrule <igor@pobox.com>
copyright: Copyright 1998 Eric Kidd and Jeff Dubrule

//======================================================================
//
//  Copyright (c) 1998 Eric Kidd, Jeff Dubrule
//  All rights reserved.
// 
//  Use and copying of this software and preparation of derivative
//  works based on this software are permitted, including commercial
//  use, provided that the following conditions are observed:
// 
//  1. This copyright notice must be retained in full on any copies
//     and on appropriate parts of any derivative works. (Other names
//     and years may be added, so long as no existing ones are removed.)
// 
//  This software is made available "as is".  Neither the authors nor
//  Carnegie Mellon University make any warranty about the software,
//  its performance, or its conformity to any specification.
// 
//  Bug reports, questions, comments, and suggestions should be sent by
//  E-mail to the Internet address "gd-bugs@gwydiondylan.org".
//
//======================================================================


//======================================================================
//  <negative-option-parser>
//======================================================================
//  Certain options may occur in positive and negative forms. This
//  absract class takes care of the details.

define abstract open primary class <negative-option-parser> (<option-parser>)
  slot negative-long-options :: <list> /* of: <string> */,
    init-keyword: negative-long-options:,
    init-value: #();
  slot negative-short-options :: <list> /* of: <string> */,
    init-keyword: negative-short-options:,
    init-value: #();  
end class <negative-option-parser>;

define method initialize
    (parser :: <negative-option-parser>, #next next-method, #key, #all-keys)
 => ()
  next-method();
  // We keep our own local lists of option names, because we support two
  // different types--positive and negative. So we need to explain about
  // our extra options to parse-options by adding them to the standard
  // list.
  parser.long-option-names := concatenate(parser.long-option-names,
					  parser.negative-long-options);
  parser.short-option-names := concatenate(parser.short-option-names,
					   parser.negative-short-options);
end method;

define method negative-option?
    (parser :: <negative-option-parser>, token :: <option-token>)
 => (negative? :: <boolean>)
  let negatives =
    select (token by instance?)
      <short-option-token> => parser.negative-short-options;
      <long-option-token> => parser.negative-long-options;
    end select;
  member?(token.token-value, negatives, test: \=)
end method negative-option?;


//======================================================================
//  <simple-option-parser>
//======================================================================
//  Simple options represent Boolean values. They may default to #t or
//  #f, and exist in both positive and negative forms ("--foo" and
//  "--no-foo"). In the case of conflicting options, the rightmost
//  takes precedence to allow for abuse of the shell's "alias" command.
//
//  Examples:
//    -q, -v, --quiet, --verbose

define class <simple-option-parser> (<negative-option-parser>)
  // Information used to reset our parse state.
  slot option-default-value :: <boolean>,
    init-keyword: default:,
    init-value: #f;  
end class <simple-option-parser>;

define method initialize
    (parser :: <simple-option-parser>, #next next-method, #key, #all-keys)
 => ()
  next-method();
  parser.option-might-have-parameters? := #f;
end method initialize;

define method reset-option-parser
    (parser :: <simple-option-parser>, #next next-method) => ()
  next-method();
  parser.option-value := parser.option-default-value;
end;

define method parse-option
    (parser :: <simple-option-parser>,
     arg-parser :: <argument-list-parser>)
 => ()
  let option = get-argument-token(arg-parser);
  parser.option-value := ~negative-option?(parser, option);
end method parse-option;


//======================================================================
//  <parameter-option-parser>
//======================================================================
//  Parameter options represent a single parameter with a string value.
//  If the option appears more than once, the rightmost value takes
//  precedence. If the option never appears, these will default to #f.
//
//  Examples:
//    -cred, -c=red, -c = red, --color red, --color=red

define class <parameter-option-parser> (<option-parser>)
end class <parameter-option-parser>;

define method parse-option
    (parser :: <parameter-option-parser>,
     arg-parser :: <argument-list-parser>)
 => ()
  get-argument-token(arg-parser);
  if (instance?(peek-argument-token(arg-parser), <equals-token>))
    get-argument-token(arg-parser);
  end if;
  parser.option-value := get-argument-token(arg-parser).token-value;
end method parse-option;


//======================================================================
//  <repeated-parameter-option-parser>
//======================================================================
//  Similar to the above, but these options may appear more than once.
//  The final value is a deque of parameter values in the order they
//  appeared on the command line. It defaults to the empty deque.
//  
//  Examples:
//    -wall, -w=all, -w = all, --warnings all, --warnings=all

define class <repeated-parameter-option-parser> (<option-parser>)
end class <repeated-parameter-option-parser>;

define method reset-option-parser
    (parser :: <repeated-parameter-option-parser>, #next next-method) => ()
  next-method();
  parser.option-value := make(<deque> /* of: <string> */);
end;

define method parse-option
    (parser :: <repeated-parameter-option-parser>,
     arg-parser :: <argument-list-parser>)
 => ()
  get-argument-token(arg-parser);
  if (instance?(peek-argument-token(arg-parser), <equals-token>))
    get-argument-token(arg-parser);
  end if;
  push-last(parser.option-value, get-argument-token(arg-parser).token-value);
end method parse-option;


//======================================================================
//  <optional-parameter-option-parser>
//======================================================================
//  Similar to <parameter-option-parser>, but the parameter is optional.
//  It must directly follow the option with no intervening whitespace,
//  or follow an "=" token. The value is #f if the option never appears,
//  #t if the option appears but the parameter does not, and the value
//  of the parameter otherwise.
//  
//  Examples:
//    -z, -z3, -z=3, -z = 3, --zip, --zip=3, --zip = 3
//  Counter-examples:
//    -z 3, --zip 3, --zip3

define class <optional-parameter-option-parser> (<option-parser>)
end class <optional-parameter-option-parser>;

define method parse-option
    (parser :: <optional-parameter-option-parser>,
     arg-parser :: <argument-list-parser>)
 => ()
  let option = get-argument-token(arg-parser);
  let next = argument-tokens-remaining?(arg-parser) &
    peek-argument-token(arg-parser);

  parser.option-value :=
    case
      instance?(next, <equals-token>) =>
	get-argument-token(arg-parser);
	get-argument-token(arg-parser).token-value;
      (instance?(option, <short-option-token>)
	 & option.tightly-bound-to-next-token?) =>
	get-argument-token(arg-parser).token-value;
      otherwise =>
	#t;
    end case;
end method parse-option;


//======================================================================
//  <keyed-option-parser>
//======================================================================
//  These are a bit obscure. The best example is d2c's '-D' flag, which
//  allows users to #define a C preprocessor name. The final value is a
//  <string-table> containing each specified key, with one of the
//  following values:
//    * #t: The user specified "-Dkey"
//    * a <string>: The user specified "-Dkey=value"
//  You can read this with element(table, key, default: #f) to get a
//  handy lookup table.
//
//  Examples:
//    -Dkey, -Dkey=value, -D key = value, --define key = value

define class <keyed-option-parser> (<option-parser>)
end class <keyed-option-parser>;

define method reset-option-parser
    (parser :: <keyed-option-parser>, #next next-method) => ()
  next-method();
  parser.option-value := make(<string-table>);
end;

define method parse-option
    (parser :: <keyed-option-parser>,
     arg-parser :: <argument-list-parser>)
 => ()
  get-argument-token(arg-parser);
  let key = get-argument-token(arg-parser).token-value;
  let value =
    if (instance?(peek-argument-token(arg-parser), <equals-token>))
      get-argument-token(arg-parser);
      get-argument-token(arg-parser).token-value;
    else
      #t;
    end if;
  parser.option-value[key] := value;
end method parse-option;
