rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/seals.dylan,v 1.4 2002/08/26 05:24:55 bruce Exp $
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

define constant <builtin-string>
  = type-union(<byte-string>, <unicode-string>);

define sealed domain \< (<builtin-string>, <builtin-string>);
define sealed domain \<= (<builtin-string>, <builtin-string>);
define sealed domain as-lowercase (<builtin-string>);
define sealed domain as-lowercase! (<builtin-string>);
define sealed domain as-uppercase (<builtin-string>);
define sealed domain as-uppercase! (<builtin-string>);
define sealed domain as (singleton(<symbol>), <builtin-string>);

define constant <builtin-vector>
  = type-union(<simple-vector>, <stretchy-object-vector>, <builtin-string>);

define constant <builtin-array>
  = type-union(<builtin-vector>, <simple-object-array>);

define sealed domain rank (<builtin-array>);
define sealed domain row-major-index (<builtin-array>);
define sealed domain aref (<builtin-array>);
define sealed domain aref-setter (<object>, <builtin-array>);
define sealed domain dimension (<builtin-array>, <integer>);
define sealed domain dimensions (<builtin-array>);

define constant <builtin-mutable-sequence>
  = type-union(<builtin-array>, <list>, <simple-object-deque>);

define sealed domain first-setter (<object>, <builtin-mutable-sequence>);
define sealed domain second-setter (<object>, <builtin-mutable-sequence>);
define sealed domain third-setter (<object>, <builtin-mutable-sequence>);
define sealed domain last-setter (<object>, <builtin-mutable-sequence>);

define constant <builtin-mutable-explicit-key-collection>
  = type-union(<simple-object-table>, <equal-table>);

define constant <builtin-explicit-key-collection>
  = type-union(<builtin-mutable-explicit-key-collection>);

define constant <builtin-mutable-collection>
  = type-union(<builtin-mutable-sequence>, <builtin-mutable-explicit-key-collection>);

define sealed domain element-setter
  (<object>, <builtin-mutable-collection>, <object>);
define sealed domain %element-setter
  (<object>, <builtin-mutable-collection>, <object>);

define sealed domain replace-elements!
  (<builtin-mutable-collection>, <function>, <function>);
define sealed domain fill! (<builtin-mutable-collection>, <object>);

define constant <builtin-sequence>
  = type-union(<builtin-mutable-sequence>, <builtin-range>);

define sealed domain first (<builtin-sequence>);
define sealed domain second (<builtin-sequence>);
define sealed domain third (<builtin-sequence>);
define sealed domain last (<builtin-sequence>);

define sealed domain add (<builtin-sequence>, <object>);
define sealed domain add! (<builtin-sequence>, <object>);
define sealed domain add-new (<builtin-sequence>, <object>);
define sealed domain add-new! (<builtin-sequence>, <object>);
define sealed domain remove (<builtin-sequence>, <object>);
define sealed domain remove! (<builtin-sequence>, <object>);
define sealed domain choose (<function>, <builtin-sequence>);
define sealed domain choose-by (<function>, <builtin-sequence>, <builtin-sequence>);
define sealed domain intersection (<builtin-sequence>, <builtin-sequence>);
define sealed domain union (<builtin-sequence>, <builtin-sequence>);
define sealed domain remove-duplicates (<builtin-sequence>);
define sealed domain remove-duplicates! (<builtin-sequence>);
define sealed domain copy-sequence (<builtin-sequence>);
define sealed domain replace-subsequence! (<builtin-sequence>, <builtin-sequence>);
define sealed domain reverse (<builtin-sequence>);
define sealed domain reverse! (<builtin-sequence>);
define sealed domain sort (<builtin-sequence>);
define sealed domain sort! (<builtin-sequence>);
define sealed domain last (<builtin-sequence>);
define sealed domain subsequence-position (<builtin-sequence>, <builtin-sequence>);

define constant <builtin-stretchy-sequence>
  = type-union(<stretchy-object-vector>, <simple-object-deque>);

define sealed domain size-setter (<integer>, <builtin-stretchy-sequence>);

define constant <builtin-stretchy-collection>
  = type-union(<builtin-stretchy-sequence>, <builtin-mutable-explicit-key-collection>);

define constant <builtin-collection>
  = type-union(<builtin-sequence>, <builtin-explicit-key-collection>);

define sealed domain initialize (<builtin-collection>);
define sealed domain shallow-copy (<builtin-collection>);
define sealed domain type-for-copy (<builtin-collection>);
define sealed domain element (<builtin-collection>, <object>);
define sealed domain %element (<builtin-collection>, <object>);
define sealed domain key-sequence (<builtin-collection>);
define sealed domain reduce (<function>, <object>, <builtin-collection>);
define sealed domain reduce1 (<function>, <builtin-collection>);
define sealed domain member? (<object>, <builtin-collection>);
define sealed domain find-key (<builtin-collection>, <function>);
define sealed domain key-test (<builtin-collection>);
define sealed domain forward-iteration-protocol (<builtin-collection>);
define sealed domain backward-iteration-protocol (<builtin-collection>);
define sealed domain \= (<builtin-collection>, <builtin-collection>);
define sealed domain \~= (<builtin-collection>, <builtin-collection>);
define sealed domain empty? (<builtin-collection>);
define sealed domain size (<builtin-collection>);

define sealed domain key-test (<sequence>);
