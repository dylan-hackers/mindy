module: extensions

//////////////////////////////////////////////////////////////////////
//
//  Copyright (C) 1994, Carnegie Mellon University
//  All rights reserved.
//
//  This code was produced by the Gwydion Project at Carnegie Mellon
//  University.  If you are interested in using this code, contact
//  "Scott.Fahlman@cs.cmu.edu" (Internet).
//
//////////////////////////////////////////////////////////////////////
//
//  $Header: /home/housel/work/rcs/gd/src/mindy/libraries/dylan/ext.dylan,v 1.1 1994/05/25 12:41:45 wlott Exp $
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
