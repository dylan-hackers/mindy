module: New-Internals
author: chiles@cs.cmu.edu
synopsis: This file implements some extensions to the Gwydion Dylan
          implementation.
copyright: See below.
rcs-header: $Header: /home/housel/work/rcs/gd/src/common/streams/Attic/new-internals.dylan,v 1.2 1996/06/13 18:10:08 bfw Exp $

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


#if (~mindy)
  // on-exit -- placeholder.
  //
  // The current dylan->c system doesn't support on-exit.  So we include a
  // placeholder that does nothing.
  // 
  define inline method on-exit (x) => ();
    ignore(x);
  end;
#endif


///
/// Classes and types.
///

///
/// As methods.
///

#if (mindy) // singleton doesn't work in compiler???
define method as (result :: singleton(<byte>), object :: <byte-character>)
    => result :: <byte>;
  as(<integer>, object);
end method;

define method as (result :: singleton(<byte-string>), object :: <byte-vector>)
 => result :: <byte-string>;
  let len :: <integer> = object.size;
  let res :: <byte-string> = make(<byte-string>, size: len);
  copy-bytes(res, 0, object, 0, len);
  res;
end method;

define method as (result :: singleton(<byte-vector>), object :: <byte-string>)
 => result :: <byte-vector>;
  let len :: <integer> = object.size;
  let res :: <byte-vector> = make(<byte-vector>, size: len);
  copy-bytes(res, 0, object, 0, len);
  res;
end method;
#endif

///
/// Utilities.
///

// <syscall-error> -- internal.
//
// The kind of error signaled by call-fd-function when the function fails.
// 

define class <syscall-error> (<error>)
  slot syscall-error-errno :: <integer>, required-init-keyword: errno:;
end class <syscall-error>;

define sealed domain make (singleton(<syscall-error>));
define sealed domain initialize (<syscall-error>);

define method report-condition (cond :: <syscall-error>, stream) => ();
  condition-format(stream, "%s", fd-error-string(cond.syscall-error-errno));
end method report-condition;


/// call-fd-function -- Exported.
///
/// This function applies the fd function to the arguments and tests the
/// error code.  If there is no error, return the function's values;
/// otherwise, signal an error with the unix description of the error.
///
/// If we had macros, this function would be a macro and require no
/// overhead.  It also would allow type propagation so that calls to these
/// fd functions within expression (that is, not bound to a type-declared
/// variable) would have the benefit of the return types of the fd
/// functions.  As it is, we have to allocate a rest argument, do multiple
/// function calls, and so on.  For now, assume the system call incurs more
/// overhead, especially if William really implemented rest args as
/// more-args.
///
/// IGNORE MULTIPLE VALUES FOR NOW.
///
define inline method call-fd-function
    (fun :: <function>, #rest args) => res :: <object>;
  let (res, err :: false-or(<integer>)) = apply(fun, args);
  if (err) error(make(<syscall-error>, errno: err)) end;
  res;
end;
