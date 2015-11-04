module: extensions

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
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
// Can't use $unsupplied, because we're passing undefined as an
// argument to element(), which itself probably uses $unsupplied..
//
define constant undefined = pair(#f, #f);
define constant key-exists? =
  method (coll :: <collection>, key :: <object>)
   => (result :: <boolean>, value :: <object>);
    let value = element(coll, key, default: undefined);
    values(value ~= undefined, value);
  end method;

define constant <byte> = limited(<integer>, min: 0, max: 255);

define method as (cls == <byte>, char :: <character>) => c :: <character>;
  as(<integer>, char);
end method as;

// assert -- exported from Extensions.  Users of assert() should not
// have side-effects in the expression that is passed to assert(),
// because if we ever turn assertions off, that would mean the program
// runs differently in debug mode than it does in release mode.
//
define function assert (value :: <object>) => ();
  unless (value)
    let err = make(<simple-error>, format-string: "Assertion failed.");
    invoke-debugger(err);
    error(err);
  end;
end function assert;


// We define this function so that people can unconditionally import it
// from extensions so that it is accessible to be the d2c entry point.  It
// should never actually be called in Mindy, and needn't even really be a
// function.
//
define function %main (#rest ignore)
  error("%main got called in Mindy somehow?");
end function;
