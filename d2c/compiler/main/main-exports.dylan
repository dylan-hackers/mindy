module: dylan-user
rcs-header: $Header: /scm/cvs/src/d2c/compiler/main/main-exports.dylan,v 1.1 1998/05/03 19:55:33 andreas Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

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

define library compiler-main
  use Dylan;
  use String-extensions;
  use Random;
  use compiler-base;
  use compiler-front;
  use compiler-optimize;
  use compiler-parser;
  use compiler-cback;
  use compiler-convert;
end;

define module main
  use common;
  use Extensions, exclude: {element-type};
#if (mindy)
  use System, import: {system, copy-bytes, getenv, collect-garbage};
#else
  use System, 
     import: {system, copy-bytes, getenv, <raw-pointer>, import-string, 
	      export-string, no-core-dumps,
	      c-expr, pointer-deref, pointer-deref-setter};
#endif
  use string-conversions, import: {string-to-integer};
  use character-type;
  use random;

  use utils;
  // use define-classes;
  use define-functions;
  use definitions, import: {*defn-dynamic-default*};
  use function-definitions;
  use variables;
  use parser;
  use format;
  use tokenize;
  use lexer;
  use header;
  use source;
  use top-level-forms;
  use flow;
  use builder-interface;
  // use fer-convert;
  use front;
  // use dump;
  use classes;
  use c-representation;
  use cback;
  use heap;
  use compile-time-values;
  use compile-time-functions;
  use signature-interface;
  use ctype;
  use cheese;
  use od-format;
  use standard-io;
  use tokens;
  use names;
  use errors;
  use policy;
  use macros;
  use fragments;
  use parse-tree, exclude: {primitive-name};
  use platform;
  use file-system;
  use extensions, import: {key-exists?};
end;
