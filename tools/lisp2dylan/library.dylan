module: dylan-user
author: Nick Kramer
copyright: Copyright (C) 1994, Carnegie Mellon University
	   All rights reserved.
	   This code was produced by the Gwydion Project at Carnegie Mellon
	   University.  If you are interested in using this code, contact
	   "Scott.Fahlman@cs.cmu.edu" (Internet).
rcs-header: $Header: /home/housel/work/rcs/gd/src/tools/lisp2dylan/library.dylan,v 1.1 1996/09/19 12:11:34 nkramer Exp $

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
// All rights reserved.
//
//======================================================================

define library lisp2dylan
  use dylan;
  use streams;
  use print;
  use format;
  use string-extensions;
  use regular-expressions;
  use standard-io;
end library lisp2dylan;

define module lisp2dylan
  use dylan;
  use streams;
  use print;
  use format;
  use extensions, exclude: { assert }; // we write our own assert
#if (~mindy)
  use system;
#endif
  use standard-io;
  use character-type;
  use string-hacking;
  use string-conversions;  // one of these has as(<string>, char)...
  use regular-expressions;
end module lisp2dylan;

