module: dylan-user
author: Nick Kramer
copyright: Copyright (C) 1997, Carnegie Mellon University
	   All rights reserved.
	   This code was produced by the Gwydion Project at Carnegie Mellon
	   University.  If you are interested in using this code, contact
	   "Scott.Fahlman@cs.cmu.edu" (Internet).
rcs-header: $Header: /home/housel/work/rcs/gd/src/tools/make-exports/library.dylan,v 1.1 1997/01/16 15:32:28 nkramer Exp $

//======================================================================
//
// Copyright (c) 1997  Carnegie Mellon University
// All rights reserved.
//
//======================================================================

define library make-exports
  use dylan;
  use streams;
  use standard-io;
  use print;
  use format;
  use table-extensions;
  use string-extensions;
  use regular-expressions;
end library make-exports;

define module make-exports
  use dylan;
  use extensions;
#if (~mindy)
  use system; // for %main
#endif
  use streams;
  use standard-io;
  use print;
  use pprint;
  use format;
  use table-extensions;
  use substring-search;
  use regular-expressions;
end module make-exports;
