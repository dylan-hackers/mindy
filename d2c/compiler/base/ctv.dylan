module: compile-time-values
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/ctv.dylan,v 1.2 1995/03/04 21:46:41 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

define abstract class <ct-value> (<object>)
end;

define abstract class <literal> (<ct-value>)
end;

define abstract class <eql-ct-value> (<ct-value>)
  slot ct-value-singleton :: union(<false>, <ctype>),
    init-value: #f;
end;

define abstract class <eql-literal> (<literal>, <eql-ct-value>)
end;


// Literal booleans.

define abstract class <literal-boolean> (<eql-literal>) end;
define class <literal-true> (<literal-boolean>) end;
define class <literal-false> (<literal-boolean>) end;

define variable *literal-true* = #f;
define variable *literal-false* = #f;

define method make (wot == <literal-true>, #next next-method, #key)
  *literal-true* | (*literal-true* := next-method());
end;

define method make (wot == <literal-false>, #next next-method, #key)
  *literal-false* | (*literal-false* := next-method());
end;

define method print-object (lit :: <literal-true>, stream :: <stream>) => ();
  write("{literal #t}", stream);
end;

define method print-object (lit :: <literal-false>, stream :: <stream>) => ();
  write("{literal #f}", stream);
end;

define method \= (x :: <true>, y :: <literal-true>) => res :: <boolean>;
  #t;
end;
    
define method \= (x :: <literal-true>, y :: <true>) => res :: <boolean>;
  #t;
end;
    
define method \= (x :: <false>, y :: <literal-false>) => res :: <boolean>;
  #t;
end;
    
define method \= (x :: <literal-false>, y :: <false>) => res :: <boolean>;
  #t;
end;


// Literal numbers.

define abstract class <literal-number> (<eql-literal>)
  slot literal-value :: <number>, required-init-keyword: value:;
end;

define method \= (x :: <number>, y :: <literal-number>) => res :: <boolean>;
  x = y.literal-value;
end;

define method \= (x :: <literal-number>, y :: <number>) => res :: <boolean>;
  x.literal-value = y;
end;

define method \~= (x :: <number>, y :: <literal-number>) => res :: <boolean>;
  x ~= y.literal-value;
end;

define method \~= (x :: <literal-number>, y :: <number>) => res :: <boolean>;
  x.literal-value ~= y;
end;

define method \< (x :: <number>, y :: <literal-number>) => res :: <boolean>;
  x < y.literal-value;
end;

define method \< (x :: <literal-number>, y :: <number>) => res :: <boolean>;
  x.literal-value < y;
end;

define method \<= (x :: <number>, y :: <literal-number>) => res :: <boolean>;
  x <= y.literal-value;
end;

define method \<= (x :: <literal-number>, y :: <number>) => res :: <boolean>;
  x.literal-value <= y;
end;

define abstract class <literal-real> (<literal-number>) end;
define abstract class <literal-rational> (<literal-real>) end;
define abstract class <literal-integer> (<literal-rational>) end;
define class <literal-fixed-integer> (<literal-integer>) end;
define class <literal-extended-integer> (<literal-integer>) end;
define class <literal-ratio> (<literal-rational>) end;
define abstract class <literal-float> (<literal-real>) end;
define class <literal-single-float> (<literal-float>) end;
define class <literal-double-float> (<literal-float>) end;
define class <literal-extended-float> (<literal-float>) end;
				       
define constant $literal-fixed-integer-memo = make(<table>);
define constant $literal-extended-integer-memo = make(<table>);
define constant $literal-ratio-memo = make(<table>);
define constant $literal-single-float-memo = make(<table>);
define constant $literal-double-float-memo = make(<table>);
define constant $literal-extended-float-memo = make(<table>);

define method make (class == <literal-fixed-integer>, #next next-method, #key value)
  element($literal-fixed-integer-memo, value, default: #f)
    | (element($literal-fixed-integer-memo, value) := next-method());
end;

define method make (class == <literal-extended-integer>, #next next-method,
		    #key value)
  element($literal-extended-integer-memo, value, default: #f)
    | (element($literal-extended-integer-memo, value) := next-method());
end;

define method make (class == <literal-ratio>, #next next-method, #key value)
  element($literal-ratio-memo, value, default: #f)
    | (element($literal-ratio-memo, value) := next-method());
end;

define method make (class == <literal-single-float>, #next next-method, #key value)
  element($literal-single-float-memo, value, default: #f)
    | (element($literal-single-float-memo, value) := next-method());
end;

define method make (class == <literal-double-float>, #next next-method, #key value)
  element($literal-double-float-memo, value, default: #f)
    | (element($literal-double-float-memo, value) := next-method());
end;

define method make (class == <literal-extended-float>, #next next-method, #key value)
  element($literal-extended-float-memo, value, default: #f)
    | (element($literal-extended-float-memo, value) := next-method());
end;

define method print-object (lit :: <literal-fixed-integer>, stream :: <stream>)
    => ();
  format(stream, "{literal fixed-integer %d}", lit.literal-value);
end;

define method print-object (lit :: <literal-extended-integer>, stream :: <stream>)
    => ();
  format(stream, "{literal extended-integer %d}", lit.literal-value);
end;

define method print-object (lit :: <literal-ratio>, stream :: <stream>) => ();
  format(stream, "{literal ratio %=}", lit.literal-value);
end;

define method print-object (lit :: <literal-single-float>, stream :: <stream>) => ();
  format(stream, "{literal single-float %=}",
	 as(<single-float>, lit.literal-value));
end;

define method print-object (lit :: <literal-double-float>, stream :: <stream>) => ();
  format(stream, "{literal double-float %=}",
	 as(<double-float>, lit.literal-value));
end;

define method print-object (lit :: <literal-extended-float>, stream :: <stream>)
    => ();
  format(stream, "{literal extended-float %=}",
	 as(<extended-float>, lit.literal-value));
end;


// Literal symbols.

define class <literal-symbol> (<literal>)
  slot literal-value :: <symbol>, required-init-keyword: value:;
end;

define constant $literal-symbol-memo = make(<table>);

define method make (class == <literal-symbol>, #next next-method, #key value)
  element($literal-symbol-memo, value, default: #f)
    | (element($literal-symbol-memo, value) := next-method());
end;

define method \= (x :: <symbol>, y :: <literal-symbol>) => res :: <boolean>;
  x = y.literal-value;
end;

define method \= (x :: <literal-symbol>, y :: <symbol>) => res :: <boolean>;
  x.literal-value = y;
end;


// Literal characters.

define class <literal-character> (<literal>)
  slot literal-value :: <character>, required-init-keyword: value:;
end;

define constant $literal-character-memo = make(<table>);

define method make (class == <literal-character>, #next next-method, #key value)
  element($literal-character-memo, value, default: #f)
    | (element($literal-character-memo, value) := next-method());
end;

define method \= (x :: <character>, y :: <literal-character>) => res :: <boolean>;
  x = y.literal-value;
end;

define method \= (x :: <literal-character>, y :: <character>) => res :: <boolean>;
  x.literal-value = y;
end;

define method \< (x :: <character>, y :: <literal-character>) => res :: <boolean>;
  x < y.literal-value;
end;

define method \< (x :: <literal-character>, y :: <character>) => res :: <boolean>;
  x.literal-value < y;
end;


// Literal sequences.

define abstract class <literal-sequence> (<literal>)
  slot literal-contents :: <vector>, required-init-keyword: contents:;
end;

define class <literal-list> (<literal-sequence>)
  slot literal-tail :: union(<false>, <literal>),
    init-value: #f, init-keyword: tail:;
end;

define method make (class == <literal-list>, #next next-method, #key contents, tail)
  if (empty?(contents))
    if (tail)
      error("Can't make an empty list with a tail.");
    end;
    make(<literal-empty-list>);
  else
    next-method();
  end;
end;

define class <literal-empty-list> (<literal-list>, <eql-literal>)
  keyword contents:, init-value: #[];
end;

define variable *literal-empty-list* = #f;

define method make (class == <literal-empty-list>, #next next-method, #key)
  *literal-empty-list* | (*literal-empty-list* := next-method());
end;

define method \= (x :: <empty-list>, y :: <literal-empty-list>) => res :: <boolean>;
  #t;
end;

define method \= (x :: <literal-empty-list>, y :: <empty-list>) => res :: <boolean>;
  #t;
end;

define class <literal-vector> (<literal-sequence>) end;
define class <literal-string> (<literal-vector>) end;

define method concatenate (str1 :: <literal-string>, #rest more)
    => res :: <literal-string>;
  make(<literal-string>,
       contents: apply(concatenate,
		       str1.literal-contents,
		       map(literal-contents, more)));
end;
