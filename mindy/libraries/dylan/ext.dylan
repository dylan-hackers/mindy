module: extensions
rcs-header: $Header: /home/housel/work/rcs/gd/src/mindy/libraries/dylan/ext.dylan,v 1.8 1996/02/17 17:54:44 nkramer Exp $

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

/// One-of -- Exported.
///
/// One-of takes any number of objects as arguments.  It returns a type that
/// represents the argument values as an enumeration (singleton values in
/// Dylan).
///
define constant one-of =
  method (thing, #rest more-things) => result :: <type>;
    reduce(type-union, singleton(thing), map(singleton, more-things));
  end;

// false-or -- Exported.
//
// False-or takes a type and returns a type that is the union of the argument
// type and the type singleton(#f).
// There are two copies of false-or, one for the Dylan module and one for 
// the rest of the world.
//
define constant false-or
    = method (type :: <type>) => new-type :: <type>;
	type-union(type, singleton(#f));
      end;

/// Ignore -- Exported.
///
/// Ignore takes any number of arguments and ignores them.  This is useful
/// when extending functions that require arguments for which the new method
/// has no use.  This function provides documentation to the code reader,
/// and it provides a reference to the unneeded local variables so that
/// compilers do not flame about unused locals.
///
define constant ignore =
  method (#rest noise)
    noise;
    #f;
  end;

// Key-exists -- Exported
//
// If the given key is present in the collection, return #t and the value
// associated with the key.  Otherwise, return #f and an undefined value.
//
define constant undefined = pair(#f, #f);
define constant key-exists? =
  method (coll :: <collection>, key :: <object>)
   => (result :: <boolean>, value :: <object>);
    let value = element(coll, key, default: undefined);
    values(value ~= undefined, value);
  end method;
