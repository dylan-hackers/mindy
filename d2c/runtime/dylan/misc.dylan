rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/misc.dylan,v 1.6 2002/11/01 21:40:56 andreas Exp $
copyright: see below
module: dylan-viscera


//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000, 2001, 2002  Gwydion Dylan Maintainers
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

// assert -- exported from Extensions.  Users of assert() should not
// have side-effects in the expression that is passed to assert(),
// because if we ever turn assertions off, that would mean the program
// runs differently in debug mode than it does in release mode.

define macro assert
    { assert( ?clauses ) }
     => { ?clauses }
clauses:
  { ?ok:expression }
    => { if( ~ ?ok ) error( "An assertion failed" ) end if; }
  { ?ok:expression, ?message:expression }
    => { if( ~ ?ok ) error( ?message ) end if; }
  { ?ok:expression, ?message:expression, ?args:* }
    => { if( ~ ?ok ) error(?message, ?args) end if; }
end macro assert;

define macro debug-assert
  { debug-assert(?value:expression, ?format-string:expression, ?format-args:*)}
    =>{ assert(?value, ?format-string, ?format-args) }
    { debug-assert(?value:expression, ?message:expression) }
    =>{ assert(?value, ?message) }
    { debug-assert(?value:expression) }
    =>{ assert(?value) }
end macro debug-assert;

// <not-supplied-marker> -- internal.
//
// The class of $not-supplied.
// 
define class <not-supplied-marker> (<object>)
end;

// $not-supplied -- exported from Extensions.
//
// a magic marker used to flag unsupplied keywords.
// 
define constant $not-supplied :: <not-supplied-marker>
    = make(<not-supplied-marker>);
define constant $unsupplied = $not-supplied;

define method unsupplied?( object :: <object> )
=> ( unsupplied? :: <boolean> )
    object = $unsupplied;
end method unsupplied?;

define method supplied?( object :: <object> )
=> ( unsupplied? :: <boolean> )
    ~ unsupplied?( object );
end method supplied?;

define method unsupplied()
=> ( unsupplied-marker :: <object> )
    $unsupplied;
end method unsupplied;

define class <not-found-marker> (<object>)
end;

define constant $unfound = make(<not-found-marker>);

define function found?( object :: <object> )
=> ( found? :: <boolean> )
    ~ unfound?( object );
end function found?;

define function unfound?( object :: <object> )
=> ( unfound? :: <boolean> )
    object = $unfound;
end function unfound?;

define function unfound()
=> ( unfound-marker :: <object> )
    $unfound;
end function unfound;

// <never-returns> -- exported from Extensions.
//
// The empty type.  When used as a function result type, it means the function
// never returns.
//
define constant <never-returns> :: <type> = type-union();

define flushable generic values-sequence (sequence :: <sequence>);

define inline method values-sequence (sequence :: <sequence>)
  let vec :: <simple-object-vector> = as(<simple-object-vector>, sequence);
  values-sequence(vec);
end;

define inline method values-sequence
    (vector :: <simple-object-vector>)
  %%primitive(values-sequence, vector);
end;


define movable generic values (#rest values);

define inline method values (#rest values)
  %%primitive(values-sequence, values);
end;


define inline method object-address (object :: <object>)
    => res :: <raw-pointer>;
  %%primitive(object-address, object);
end method object-address;


define inline method ignore (#rest noise) => ();
end method ignore;

define macro without-bounds-checks
  {without-bounds-checks () ?:body end}
    => {without-bounds-checks ?body end}

  {without-bounds-checks ?:body end}
    => {let ?=element = %element;
        let ?=element-setter = %element-setter;
        ?body}
end;

define macro fake-without-bounds-checks
  {fake-without-bounds-checks () ?:body end}
    => {fake-without-bounds-checks ?body end}

  {fake-without-bounds-checks ?:body end}
    => {?body}
end;

define macro with-bounds-checks
  {with-bounds-checks () ?:body end}
    => {with-bounds-checks ?body end}

  {with-bounds-checks ?:body end}
    => {let ?=element = element;
        let ?=element-setter = element-setter;
        ?body}
end;

// element-range-error
// Just throws an error on the sequence. 
// Should declare an <element-range-error> class and instantiate it

define method element-range-error
    (sequence :: <sequence>, index :: <integer>)
 => ()
  error( "range error (element %d of %=)", index, sequence );
end method;



%%primitive(magic-internal-primitives-placeholder);
