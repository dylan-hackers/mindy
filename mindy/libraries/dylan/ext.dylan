module: extensions
rcs-header: $Header: /home/housel/work/rcs/gd/src/mindy/libraries/dylan/ext.dylan,v 1.2 1994/06/27 17:10:26 wlott Exp $

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
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
//
//  This file contains some random things that don't really fit anywhere
//  else.
//

/// ONE-OF
///
/// One-of returns a type that represents one of the argument things.  In
/// other words, a union of a bunch of singletons.
/// 
define constant one-of =
  method (thing, #rest more-things) => result :: <type>;
    reduce(union, singleton(thing), map(singleton, more-things));
  end;

/// TYPE-OR
///
/// Type-or returns the union of all the argument types.
define constant type-or =
  method (type :: <type>, #rest more-types) => result :: <type>;
  // Make sure all of more-types are <type>s.
    do(rcurry(check-type, <type>), more-types);
  // Make a union out of them.
    reduce(union, type, more-types);
  end;



define constant ignore =
  method (#rest noise)
    noise;
    #f;
  end;
