documented: #t
module: dylan-user
copyright: see below
	   This code was produced by the Gwydion Project at Carnegie Mellon
	   University.  If you are interested in using this code, contact
	   "Scott.Fahlman@cs.cmu.edu" (Internet).

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
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

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
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
  use common-dylan;
  use table-extensions;
  use string-extensions;
  use collection-extensions;
  use regular-expressions;
  use io;
  use system;
  use command-line-parser;
  use melange-c;
  export
    name-mappers;
end library melange;

define module int-lexer
  use common-dylan,
    exclude: { format-to-string, position, split,
               string-to-integer, integer-to-string };
  use self-organizing-list;
  use string-conversions;
  use regular-expressions;
  use character-type;
  use streams;
  export
    <tokenizer>, get-token, unget-token, <token>, value, string-value,
    generator, parse-error, position, token-id,
    <reserved-word-token>, <name-token>, <punctuation-token>,
    <error-token>, <identifier-token>, <simple-token>,
    <integer-token>, <eof-token>, <keyword-token>,
    <symbol-literal-token>, <string-literal-token>, <comma-token>,
    <semicolon-token>, <lbrace-token>, <rbrace-token>, <arrow-token>,
    <define-token>, <interface-token>, <end-token>, <include-token>,
    <object-file-token>, <define-macro-token>, <undefine-token>,
    <name-mapper-token>, <import-token>, <prefix-token>, <exclude-token>,
    <exclude-file-token>, <rename-token>, <mapping-token>, <equate-token>,
    <superclass-token>, <all-token>, <all-recursive-token>, <none-token>,
    <function-token>, <map-result-token>, <equate-result-token>,
    <ignore-result-token>, <map-argument-token>, <equate-argument-token>,
    <input-argument-token>, <output-argument-token>,
    <input-output-argument-token>, <struct-token>, <union-token>,
    <pointer-token>, <constant-token>, <variable-token>, <getter-token>,
    <setter-token>, <read-only-token>, <seal-token>, <seal-functions-token>,
    <boolean-token>, <sealed-token>, <open-token>, <inline-token>,
    <value-token>, <function-type-token>, <callback-maker-token>,
    <callout-function-token>, <literal-token>, <mindy-inc-token>;
end module int-lexer;

define module int-parse
  use common-dylan, exclude: { format-to-string, position };
  use table-extensions;
  use self-organizing-list;
  use c-lexer, import: {include-path, file-in-include-path};
  use streams;
  use standard-io;
  use format;
  use int-lexer;
  export
    parse, <parse-state>, include-files, object-files, mindy-include-file,
    container-options, macro-defines, macro-undefines, clauses,
    <container-options>, name-mapper, global-imports, global-import-mode,
    file-imports, file-import-modes, prefix, exclude, excluded-files, rename,
    mappings, equates, read-only, seal-string, <clause>, <function-clause>,
    <struct-clause>, <union-clause>, <pointer-clause>, <constant-clause>,
    <variable-clause>, <function-type-clause>, name, options, <undefined>,
    undefined;
end module int-parse;

define module name-mappers
  use dylan;
  use character-type;
  export
    map-name, hyphenate-case-breaks;
end module name-mappers;

define module define-interface
  // From Dylan
  use common-dylan, exclude: { format-to-string, split, position };
  use table-extensions;
/*
  use %hash-tables;
#if (~mindy)
  use System,
     import: {import-string,
	      copy-bytes, call-out, c-expr, buffer-address, <raw-pointer>,
	      pointer-deref};
#endif
*/

  // From string-extensions
  use regular-expressions;
  use substring-search;
  use character-type;

  // From io
  use streams;
  
  // From io
  use format;

  // From io
  use standard-io;

  // From system
  use file-system;

  // From command-line-parser
  use command-line-parser;
  
  // local packages
  use int-lexer;
  use int-parse, rename: {rename => renames};
  use c-lexer, import: {include-path, file-in-include-path, *framework-paths*, find-frameworks};
  use c-declarations,
    rename: {parse => c-parse, <parse-state> => <c-parse-state>};
  use name-mappers;
  use portability;
  use parse-conditions, exclude: {parse-error};
end module define-interface;

