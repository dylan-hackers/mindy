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
// Melange versions:
//   b1.0: (04/28/95)
//     Initial "beta" release
//   b1.1: (05/17/95)
//     Added "superclasses" option for structure and union clauses
//     Bug fixes:
//       Allow enumeration literals in compile time expressions (including
//         specification of other literal values)
//       Show token string rather than token type in error messages
//       Report line numbers rather than character numbers in error messages
//       Report name of interface files in error messages
//       Fix handling of empty strings in interfaces
//       Fix handling of CPP '#include "foo"'
//       Allow CPP '#pragma'
//       Fix handling of CPP foo##bar
//   b1.2: (10/25/95)
//     Improved protability handling:
//       portable size constants
//       multiple alignment models
//     Improved vector handling:
//       vector operations now only apply to subclasses of <c-vector>
//       added "pointer" clause to interface declaration.
//       <c-strings> now have more correct (and documented) behavior
//     Bug fixes:
//       Fixed various routines to work if "members" is #f.
//       Fixed handling of equated typedefs.
//       Fixed bug in explit-ony? keyword for mapped-name.
//       Fixed handling of types which are mapped to themselves.
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

