rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/main.dylan,v 1.1 1998/05/03 19:55:40 andreas Exp $
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

// The "main" facility for parsing argv a la Mindy.  Users should set
// extensions:%main as the entry point in their LID file and it will call
// extensions:main.
//
define open generic main (argv0, #rest more-args);
//
define method %main (argc :: <integer>, argv :: <raw-pointer>) => ();
  let args = make(<vector>, size: argc);
  for (index :: <integer> from 0 below argc)
    let argptr = pointer-deref(#"ptr", argv,
			       index * c-expr(#"int", "sizeof(void *)"));
    args[index] := import-string(argptr);
  end for;
  apply(main, args);
end method %main;
