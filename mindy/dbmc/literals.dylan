module: literals
copyright: see below

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

/*

  literal {abstract}
      literal-sequence [identity-preserving-mixin] {abstract}
          literal-list {abstract}
             literal-pair
             literal-empty-list [eql-literal]
          literal-vector {abstract}
             literal-simple-object-vector
          literal-string {abstract}
             literal-byte-string

eql-literal [literal, eql-ct-value] {abstract}
    literal-boolean {abstract} [identity-preserving-mixin]
        literal-true
        literal-false
    literal-number {abstract}
        literal-rational {abstract}
            literal-ratio
        literal-real {abstract}
            literal-float {abstract}
                literal-single-float
                literal-double-float
                literal-extended-float
        literal-general-integer {abstract}
            literal-integer
            literal-extended-integer
    literal-symbol [identity-preserving-mixin]
        literal-byte-symbol
    literal-character
        literal-byte-character

*/


define abstract class <literal> (<object>)
end;

define abstract class <eql-literal> (<literal>)
end;

// Literal booleans.

define abstract class <literal-boolean> (<eql-literal>) 
end;
define class <literal-true> (<literal-boolean>) end;
define class <literal-false> (<literal-boolean>) end;

define method literal-value (wot :: <literal-true>) => res :: singleton(#t);
  #t;
end method;

define method literal-value (wot :: <literal-false>) => res :: singleton(#f);
  #f;
end method;

define variable *literal-true* = #f;
define variable *literal-false* = #f;

define method make (wot == <literal-true>, #next next-method, #key)
    => res :: <literal-true>;
  *literal-true* | (*literal-true* := next-method());
end;

define method make (wot == <literal-false>, #next next-method, #key)
    => res :: <literal-false>;
  *literal-false* | (*literal-false* := next-method());
end;

define method as (class == <literal>, thing == #t) => res :: <literal-true>;
  make(<literal-true>);
end;

define method as (class == <literal>, thing == #f) => res :: <literal-false>;
  make(<literal-false>);
end;

define method print-object (lit :: <literal-true>, stream :: <stream>) => ();
  write(stream, "{literal #t}");
end;

define method print-object (lit :: <literal-false>, stream :: <stream>) => ();
  write(stream, "{literal #f}");
end;

define method print-message (lit :: <literal-true>, stream :: <stream>) => ();
  write(stream, "#t");
end;

define method print-message (lit :: <literal-false>, stream :: <stream>) => ();
  write(stream, "#f");
end;

define method \= (x == #t, y :: <literal-true>) => res :: <boolean>;
  #t;
end;
    
define method \= (x :: <literal-true>, y == #t) => res :: <boolean>;
  #t;
end;
    
define method \= (x == #f, y :: <literal-false>) => res :: <boolean>;
  #t;
end;
    
define method \= (x :: <literal-false>, y == #f) => res :: <boolean>;
  #t;
end;


// Literal numbers.

define abstract class <literal-number> (<eql-literal>)
end;

define method \= (x :: <number>, y :: <literal-number>) => res :: <boolean>;
  x = y.literal-value;
end;

define method \= (x :: <literal-number>, y :: <number>) => res :: <boolean>;
  x.literal-value = y;
end;

define method \< (x :: <number>, y :: <literal-number>) => res :: <boolean>;
  x < y.literal-value;
end;

define method \< (x :: <literal-number>, y :: <number>) => res :: <boolean>;
  x.literal-value < y;
end;

define abstract class <literal-real> (<literal-number>) end;
define abstract class <literal-rational> (<literal-real>) end;
define abstract class <literal-general-integer> (<literal-rational>) end;

define class <literal-integer> (<literal-general-integer>)
  slot literal-value :: <integer>, required-init-keyword: value:;
end;

/*
define class <literal-extended-integer> (<literal-general-integer>)
  slot literal-value :: <extended-integer>, required-init-keyword: value:;
end;

define class <literal-ratio> (<literal-rational>)
  slot literal-value :: <ratio>, required-init-keyword: value:;
end;
*/

define abstract class <literal-float> (<literal-real>)
  slot literal-value :: <extended-float>, required-init-keyword: value:;
end;
define class <literal-single-float> (<literal-float>) end;
define class <literal-double-float> (<literal-float>) end;
define class <literal-extended-float> (<literal-float>) end;
				       
define constant $literal-fixed-integer-memo :: <object-table>
  = make(<table>);
/*
define constant $literal-extended-integer-memo :: <object-table>
  = make(<table>);
define constant $literal-ratio-memo :: <object-table>
  = make(<table>);
*/
define constant $literal-single-float-memo :: <object-table>
  = make(<table>);
define constant $literal-double-float-memo :: <object-table>
  = make(<table>);
define constant $literal-extended-float-memo :: <object-table>
  = make(<table>);

define method make (class == <literal-integer>, #next next-method,
		    #key value)
    => res :: <literal-integer>;
  element($literal-fixed-integer-memo, value, default: #f)
    | (element($literal-fixed-integer-memo, value) := next-method());
end;

/*
define method make (class == <literal-extended-integer>, #next next-method,
		    #key value)
    => res :: <literal-extended-integer>;
  element($literal-extended-integer-memo, value, default: #f)
    | (element($literal-extended-integer-memo, value) := next-method());
end;

define method make (class == <literal-ratio>, #next next-method, #key value)
    => res :: <literal-ratio>;
  element($literal-ratio-memo, value, default: #f)
    | (element($literal-ratio-memo, value) := next-method());
end;
*/

define method make (class == <literal-single-float>, #next next-method,
		    #key value)
    => res :: <literal-single-float>;
  element($literal-single-float-memo, value, default: #f)
    | (element($literal-single-float-memo, value) := next-method());
end;

define method make (class == <literal-double-float>, #next next-method,
		    #key value)
    => res :: <literal-double-float>;
  element($literal-double-float-memo, value, default: #f)
    | (element($literal-double-float-memo, value) := next-method());
end;

define method make (class == <literal-extended-float>, #next next-method,
		    #key value)
    => res :: <literal-extended-float>;
  element($literal-extended-float-memo, value, default: #f)
    | (element($literal-extended-float-memo, value) := next-method());
end;

define method print-object (lit :: <literal-integer>, stream :: <stream>)
    => ();
  format(stream, "{literal fixed-integer %d}", lit.literal-value);
end;

define method print-message
    (lit :: <literal-integer>, stream :: <stream>)
    => ();
  format(stream, "%d", lit.literal-value);
end;

/*
define method print-object
    (lit :: <literal-extended-integer>, stream :: <stream>)
    => ();
  format(stream, "{literal extended-integer %d}", lit.literal-value);
end;

define method print-message
    (lit :: <literal-extended-integer>, stream :: <stream>)
    => ();
  format(stream, "#e%d", lit.literal-value);
end;

define method print-object (lit :: <literal-ratio>, stream :: <stream>) => ();
  format(stream, "{literal ratio %=}", lit.literal-value);
end;

define method print-message (lit :: <literal-ratio>, stream :: <stream>)
    => ();
  format(stream, "%=", lit.literal-value);
end;
*/

define method print-object (lit :: <literal-single-float>, stream :: <stream>)
    => ();
  format(stream, "{literal single-float %=}",
	 as(<single-float>, lit.literal-value));
end;

define method print-message (lit :: <literal-single-float>, stream :: <stream>)
    => ();
  format(stream, "%=", as(<single-float>, lit.literal-value));
end;

define method print-object (lit :: <literal-double-float>, stream :: <stream>)
    => ();
  format(stream, "{literal double-float %=}",
	 as(<double-float>, lit.literal-value));
end;

define method print-message (lit :: <literal-double-float>, stream :: <stream>)
    => ();
  format(stream, "%=", as(<double-float>, lit.literal-value));
end;

define method print-object (lit :: <literal-extended-float>,
			    stream :: <stream>)
    => ();
  format(stream, "{literal extended-float %=}",
	 as(<extended-float>, lit.literal-value));
end;

define method print-message
    (lit :: <literal-extended-float>, stream :: <stream>)
    => ();
  format(stream, "%=", as(<extended-float>, lit.literal-value));
end;

define method as (class == <literal>, value :: <integer>)
    => res :: <literal-integer>;
  make(<literal-integer>, value: value);
end;

/*
define method as (class == <literal>, value :: <extended-integer>)
    => res :: <literal-extended-integer>;
  make(<literal-extended-integer>, value: value);
end;

define method as (class == <literal>, value :: <ratio>)
    => res :: <literal-ratio>;
  make(<literal-ratio>, value: value);
end;
*/

define method as (class == <literal>, value :: <single-float>)
    => res :: <literal-single-float>;
  make(<literal-single-float>, value: as(<extended-float>, value));
end;

define method as (class == <literal>, value :: <double-float>)
    => res :: <literal-double-float>;
  make(<literal-double-float>, value: as(<extended-float>, value));
end;

define method as (class == <literal>, value :: <extended-float>)
    => res :: <literal-extended-float>;
  make(<literal-extended-float>, value: as(<extended-float>, value));
end;


// Literal symbols.

define class <literal-symbol> (<eql-literal>)
  slot literal-value :: <symbol>, required-init-keyword: value:;
end;

define class <literal-byte-symbol> (<literal-symbol>)
end class;

define constant $literal-symbol-memo :: <object-table>
  = make(<table>);

define method make (class == <literal-symbol>, #next next-method, #key value)
    => (res :: <literal-symbol>);
  element($literal-symbol-memo, value, default: #f)
    | (element($literal-symbol-memo, value) 
         := make(<literal-byte-symbol>, value: value))
end;

define method print-object (lit :: <literal-symbol>, stream :: <stream>)
    => ();
  format(stream, "{literal symbol %=}", lit.literal-value);
end;

define method print-message (lit :: <literal-symbol>, stream :: <stream>)
    => ();
  print(lit.literal-value, stream);
end;

define method as (class == <literal>, sym :: <symbol>)
    => res :: <literal>;
  make(<literal-symbol>, value: sym);
end;

define method \= (x :: <symbol>, y :: <literal-symbol>) => res :: <boolean>;
  x = y.literal-value;
end;

define method \= (x :: <literal-symbol>, y :: <symbol>) => res :: <boolean>;
  x.literal-value = y;
end;


// Literal characters.

define class <literal-character> (<eql-literal>)
end;

define class <literal-byte-character> (<literal-character>)
  slot literal-value :: <byte-character>, required-init-keyword: value:;
end class;

define constant $literal-character-memo :: <object-table>
  = make(<table>);

define method make (class == <literal-character>, #next next-method,
		    #key value)
    => res :: <literal-character>;
  element($literal-character-memo, value, default: #f)
    | (element($literal-character-memo, value) 
         := make(<literal-byte-character>, value: value))
end;

define method print-object (lit :: <literal-character>, stream :: <stream>)
    => ();
  format(stream, "{literal character %=}", lit.literal-value);
end;

define method print-message (lit :: <literal-character>, stream :: <stream>)
    => ();
  print(lit.literal-value, stream);
end;

define method as (class == <literal>, char :: <character>)
    => res :: <literal>;
  make(<literal-character>, value: char);
end;

define method \= (x :: <character>, y :: <literal-character>)
    => res :: <boolean>;
  x = y.literal-value;
end;

define method \= (x :: <literal-character>, y :: <character>)
    => res :: <boolean>;
  x.literal-value = y;
end;

define method \< (x :: <character>, y :: <literal-character>)
    => res :: <boolean>;
  x < y.literal-value;
end;

define method \< (x :: <literal-character>, y :: <character>)
    => res :: <boolean>;
  x.literal-value < y;
end;


// Literal sequences.

define abstract class <literal-sequence> (<literal>)
end;

define abstract class <literal-list> (<literal-sequence>)
end;

define method make (class == <literal-list>, #next next-method,
		    #key sharable: sharable?, contents, tail)
    => res :: <literal-list>;
  local
    method repeat (index)
      if (index == contents.size)
	tail | make(<literal-empty-list>);
      else
	make(<literal-pair>,
	     sharable: sharable?,
	     head: contents[index],
	     tail: repeat(index + 1));
      end;
    end;
  repeat(0);
end;

define class <literal-pair> (<literal-list>)
  slot literal-sharable? :: <boolean>, init-value: #f, init-keyword: sharable:;
  slot literal-head :: <literal>,
    required-init-keyword: head:;
  slot literal-tail :: <literal>,
    required-init-keyword: tail:;
end;

define class <literal-pair-memo-table> (<table>)
end;
define sealed domain make(singleton(<literal-pair-memo-table>));
define sealed domain initialize(<literal-pair-memo-table>);

define sealed inline method table-protocol (table :: <literal-pair-memo-table>)
    => (tester :: <function>, hasher :: <function>);
  values(method (key1 :: <literal-pair>, key2 :: <literal-pair>)
	     => res :: <boolean>;
	   key1.literal-head == key2.literal-head
	     & key1.literal-tail == key2.literal-tail;
	 end,
	 method (key :: <literal-pair>, state) => (id :: <integer>, state);
	   let (head-id, head-state)
	     = object-hash(key.literal-head, state);
	   let (tail-id, tail-state)
	     = object-hash(key.literal-tail, head-state);
	   let id = merge-hash-ids(head-id, tail-id, ordered: #t);
	   values(id, tail-state);
	 end);
end;

define constant $literal-pair-memo :: <literal-pair-memo-table>
  = make(<literal-pair-memo-table>);

define method make (class == <literal-pair>, #next next-method,
		    #key sharable: sharable?, head, tail)
    => res :: <literal-pair>;
  if (sharable?)
    let key = next-method();
    element($literal-pair-memo, key, default: #f)
      | (element($literal-pair-memo, key) := key);
  else
    next-method();
  end;
end;

define method print-message (lit :: <literal-pair>, stream :: <stream>) => ();
  write(stream, "{a <pair>}");
end;

define method as (class == <literal>, thing :: <pair>)
    => res :: <literal>;
  make(<literal-pair>,
       sharable: #t,
       head: as(<literal>, thing.head),
       tail: as(<literal>, thing.tail));
end;

define class <literal-empty-list> (<literal-list>, <eql-literal>)
end;

define method literal-value (wot :: <literal-empty-list>)
 => res :: <empty-list>;
  #();
end method;

define variable *literal-empty-list* = #f;

define method make (class == <literal-empty-list>, #next next-method, #key)
    => res :: <literal-empty-list>;
  *literal-empty-list* | (*literal-empty-list* := next-method());
end;

define method print-message (lit :: <literal-empty-list>, stream :: <stream>)
    => ();
  write(stream, "#()");
end;

define method as (class == <literal>, thing :: <empty-list>)
    => res :: <literal>;
  make(<literal-empty-list>);
end;

define method literal-head (empty-list :: <literal-empty-list>)
    => res :: <literal-empty-list>;
  empty-list;
end;

define method literal-tail (empty-list :: <literal-empty-list>)
    => res :: <literal-empty-list>;
  empty-list;
end;

define method \= (x :: <empty-list>, y :: <literal-empty-list>)
    => res :: <boolean>;
  #t;
end;

define method \= (x :: <literal-empty-list>, y :: <empty-list>)
    => res :: <boolean>;
  #t;
end;


define abstract class <literal-vector> (<literal-sequence>)
end;

define class <literal-simple-object-vector> (<literal-vector>)
  slot literal-sharable? :: <boolean>, init-value: #f, init-keyword: sharable:;
  slot literal-value :: <simple-object-vector>,
    required-init-keyword: contents:;
end;

// <Shallow-equal-table>s hash vectors which are considered equivalent
// if each element is identical even if the vectors themselves are not.
//
define class <shallow-equal-table> (<table>) end class;
define sealed domain make(singleton(<shallow-equal-table>));
define sealed domain initialize(<shallow-equal-table>);

define sealed inline method table-protocol (table :: <shallow-equal-table>)
 => (test :: <function>, hash :: <function>);
  values(shallow-equal, shallow-hash);
end method table-protocol;

define method shallow-equal
    (vec1 :: <simple-object-vector>, vec2 :: <simple-object-vector>)
    => res :: <boolean>;
  if (vec1.size = vec2.size)
    block (return)
      for (i from 0 below vec1.size)
	if (vec1[i] ~== vec2[i]) return(#f) end if;
      end for;
      #t;
    end block;
  end if;
end method shallow-equal;
      
define method shallow-hash (vec :: <simple-object-vector>, state :: <object>)
    => (id :: <integer>, state :: <object>);
  let (current-id, current-state) = values(0, state);
  for (i from 0 below vec.size)
    let (id, state) = object-hash(vec[i], current-state);
    let captured-id
      = merge-hash-ids(current-id, id, ordered: #t);
    current-id := captured-id;
    current-state := state;
  end for;
  values(current-id, current-state);
end method shallow-hash;

define constant $literal-vector-memo :: <shallow-equal-table>
  = make(<shallow-equal-table>);

define method make (class == <literal-simple-object-vector>, #next next-method,
		    #key sharable: sharable?, contents)
    => res :: <literal-simple-object-vector>;
  do(rcurry(check-type, <literal>), contents);
  let contents = as(<simple-object-vector>, contents);
  if (sharable?)
    element($literal-vector-memo, contents, default: #f)
      | (element($literal-vector-memo, contents) :=
	   next-method(class, sharable: #t, contents: contents));
  else
    next-method(class, sharable: #f, contents: contents);
  end;
end;

define method print-message
    (lit :: <literal-simple-object-vector>, stream :: <stream>) => ();
  write(stream, "{a <simple-object-vector>}");
end;

define method as (class == <literal>, vec :: <simple-object-vector>)
    => res :: <literal>;
  make(<literal-simple-object-vector>,
       sharable: #t,
       contents: map(curry(as, <literal>), vec));
end;

define abstract class <literal-string> (<literal-vector>)
end class;

define class <literal-byte-string> (<literal-string>)
  slot literal-value :: <byte-string>, required-init-keyword: value:;
end;

define constant $literal-string-memo :: <string-table> = make(<string-table>);

define method make (class == <literal-string>, #next next-method,
		    #key value)
    => res :: <literal-string>;
  element($literal-string-memo, value, default: #f)
    | (element($literal-string-memo, value)
        := make(<literal-byte-string>, value: value));
end;

define method print-object (lit :: <literal-string>, stream :: <stream>)
    => ();
  format(stream, "{literal string %=}", lit.literal-value);
end;

define method print-message (lit :: <literal-string>, stream :: <stream>)
    => ();
  print(lit.literal-value, stream);
end;

define method as (class == <literal>, string :: <byte-string>)
    => res :: <literal>;
  make(<literal-string>, value: string);
end;

define method concat-strings (str1 :: <literal-string>, #rest more)
    => res :: <literal-string>;
  make(<literal-string>,
       value: apply(concatenate,
		    str1.literal-value,
		    map(literal-value, more)));
end;


// Seals for file ctv.dylan

// <literal-true> -- subclass of <literal-boolean>
define sealed domain make(singleton(<literal-true>));
// <literal-false> -- subclass of <literal-boolean>
define sealed domain make(singleton(<literal-false>));
// <literal-integer> -- subclass of <literal-general-integer>
define sealed domain make(singleton(<literal-integer>));
// <literal-extended-integer> -- subclass of <literal-general-integer>
/*
define sealed domain make(singleton(<literal-extended-integer>));
*/
// <literal-ratio> -- subclass of <literal-rational>
/*
define sealed domain make(singleton(<literal-ratio>));
*/
// <literal-single-float> -- subclass of <literal-float>
define sealed domain make(singleton(<literal-single-float>));
// <literal-double-float> -- subclass of <literal-float>
define sealed domain make(singleton(<literal-double-float>));
// <literal-extended-float> -- subclass of <literal-float>
define sealed domain make(singleton(<literal-extended-float>));
// <literal-symbol> -- subclass of <eql-literal>
define sealed domain make(singleton(<literal-symbol>));
// <literal-byte-symbol> -- subclass of <literal-symbol>
define sealed domain make(singleton(<literal-byte-symbol>));
// <literal-character> -- subclass of <eql-literal>
define sealed domain make(singleton(<literal-character>));
// <literal-byte-character> -- subclass of <literal-character>
define sealed domain make(singleton(<literal-byte-character>));
// <literal-pair> -- subclass of <literal-list>
define sealed domain make(singleton(<literal-pair>));
// <literal-empty-list> -- subclass of <literal-list>, <eql-literal>
define sealed domain make(singleton(<literal-empty-list>));
// <literal-simple-object-vector> -- subclass of <literal-vector>
define sealed domain make(singleton(<literal-simple-object-vector>));
// <literal-byte-string> -- subclass of <literal-string>
define sealed domain make(singleton(<literal-byte-string>));
