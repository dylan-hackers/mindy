documented: #t
module: dylan-user
copyright: Copyright (C) 1994, Carnegie Mellon University
	   All rights reserved.
	   This code was produced by the Gwydion Project at Carnegie Mellon
	   University.  If you are interested in using this code, contact
	   "Scott.Fahlman@cs.cmu.edu" (Internet).
rcs-header: $Header: 

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
// All rights reserved.
//
//======================================================================

//======================================================================
// Library "melange" contains the complete functionality (except possibly for
// user extensions to name mappers) of the Melange interface generator (Mindy
// version).  Since it is intended as a stand-alone program, there is no need
// to export most of its functionality.  "Name-mappers" is exported to
// facilitate user extension of the program.  See "name-map.dylan" or the
// Melange documentation for further instructions on such extension.
//======================================================================

define library melange
  use dylan;
  use string-extensions;
  use collection-extensions;
  use streams;
  use format;
  export
    name-mappers;
end library melange;

