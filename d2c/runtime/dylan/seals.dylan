rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/seals.dylan,v 1.2 1996/01/12 02:10:53 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera


define constant <builtin-string>
  = type-union(<byte-string>, <unicode-string>);

seal generic \< (<builtin-string>, <builtin-string>);
seal generic \<= (<builtin-string>, <builtin-string>);
seal generic as-lowercase (<builtin-string>);
seal generic as-lowercase! (<builtin-string>);
seal generic as-uppercase (<builtin-string>);
seal generic as-uppercase! (<builtin-string>);
seal generic as (singleton(<symbol>), <builtin-string>);

define constant <builtin-vector>
  = type-union(<simple-vector>, <stretchy-object-vector>, <builtin-string>);

define constant <builtin-array>
  = type-union(<builtin-vector>, <simple-object-array>);

seal generic rank (<builtin-array>);
seal generic row-major-index (<builtin-array>);
seal generic aref (<builtin-array>);
seal generic aref-setter (<object>, <builtin-array>);
seal generic dimension (<builtin-array>, <integer>);
seal generic dimensions (<builtin-array>);

define constant <builtin-mutable-sequence>
  = type-union(<builtin-array>, <list>, <simple-object-deque>);

seal generic first-setter (<object>, <builtin-mutable-sequence>);
seal generic second-setter (<object>, <builtin-mutable-sequence>);
seal generic third-setter (<object>, <builtin-mutable-sequence>);
seal generic last-setter (<object>, <builtin-mutable-sequence>);

define constant <builtin-mutable-collection>
  = type-union(<builtin-mutable-sequence>);

seal generic element-setter
  (<object>, <builtin-mutable-collection>, <integer>);
seal generic replace-elements!
  (<builtin-mutable-collection>, <function>, <function>);
seal generic fill! (<builtin-mutable-collection>, <object>);

define constant <builtin-sequence>
  = type-union(<builtin-mutable-sequence>);

seal generic add (<builtin-sequence>, <object>);
seal generic add! (<builtin-sequence>, <object>);
seal generic add-new (<builtin-sequence>, <object>);
seal generic add-new! (<builtin-sequence>, <object>);
seal generic remove (<builtin-sequence>, <object>);
seal generic remove! (<builtin-sequence>, <object>);
seal generic choose (<function>, <builtin-sequence>);
seal generic choose-by (<function>, <builtin-sequence>, <builtin-sequence>);
seal generic intersection (<builtin-sequence>, <builtin-sequence>);
seal generic union (<builtin-sequence>, <builtin-sequence>);
seal generic remove-duplicates (<builtin-sequence>);
seal generic remove-duplicates! (<builtin-sequence>);
seal generic copy-sequence (<builtin-sequence>);
seal generic replace-subsequence! (<builtin-sequence>, <builtin-sequence>);
seal generic reverse (<builtin-sequence>);
seal generic reverse! (<builtin-sequence>);
seal generic sort (<builtin-sequence>);
seal generic sort! (<builtin-sequence>);
seal generic last (<builtin-sequence>);
seal generic subsequence-position (<builtin-sequence>, <builtin-sequence>);

define constant <builtin-stretchy-collection>
  = type-union(<stretchy-object-vector>, <simple-object-deque>);

seal generic size-setter (<integer>, <builtin-stretchy-collection>);

define constant <builtin-collection>
  = type-union(<builtin-sequence>);

seal generic initialize (<builtin-collection>);
seal generic shallow-copy (<builtin-collection>);
seal generic type-for-copy (<builtin-collection>);
seal generic element (<builtin-collection>, <integer>);
seal generic key-sequence (<builtin-collection>);
seal generic reduce (<function>, <object>, <builtin-collection>);
seal generic reduce1 (<function>, <builtin-collection>);
seal generic member? (<object>, <builtin-collection>);
seal generic find-key (<builtin-collection>, <function>);
seal generic key-test (<builtin-collection>);
seal generic forward-iteration-protocol (<builtin-collection>);
seal generic backward-iteration-protocol (<builtin-collection>);
seal generic \= (<builtin-collection>, <builtin-collection>);
seal generic \~= (<builtin-collection>, <builtin-collection>);
seal generic empty? (<builtin-collection>);
seal generic size (<builtin-collection>);
