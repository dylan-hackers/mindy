rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/copy.dylan,v 1.1 1998/05/03 19:55:37 andreas Exp $
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

define open generic as (type :: <type>, object :: <object>) => object;

define open generic shallow-copy (object :: <object>) => new;

define open generic type-for-copy (object :: <object>) => type :: <type>;

define sealed inline method identity
    (object :: <object>) => (object :: <object>);
  object;
end method identity;

// Without subtype specializers, we cannot specify a default "as" method for
// all <collection> types.  Instead, we place support in this catch-all
// method.
//
define method as (type :: <type>, obj :: <object>) => (result :: <object>);
  case
    (instance?(obj, type)) => obj;
    (subtype?(type, <collection>) & instance?(obj, <collection>)) =>
      map-as(type, identity, obj);
    otherwise =>
      error("Object %= cannot be converted to type %=.", obj, type);
  end case;
end method as;

define inline method type-for-copy (object :: <object>) => type :: <type>;
  object-class(object);
end;
