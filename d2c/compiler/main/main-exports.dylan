module: dylan-user
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/main/main-exports.dylan,v 1.4 1996/02/08 02:23:40 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

define library compiler-main
  use Dylan;
#if (~mindy)
  use String-extensions;
#end
  use compiler-base;
  use compiler-front;
  use compiler-parser;
  use compiler-cback;
end;

define module lexer
  use common;

  use utils;
  use source;
  use compile-time-values;
  use tokens;
  use variables;

  export
    <lexer>,

    add-feature, remove-feature;
end;

define module init
  use common;
  use utils;
  use tokens;
  use variables;
  use lexer;
  use parser;
  use source;
  use header;
end;

define module parse-tree-dump
  use common;
  use od-format;
  use compile-time-values, import: { *compiler-dispatcher* };
  use source;
  use parse-tree;
end module parse-tree-dump;

define module tokens-dump
  use common;
  use od-format;
  use compile-time-values, import: { *compiler-dispatcher* };
  use source;
  use tokens;
end module tokens-dump;


#if (mindy)
// Part of the compiler library
//
define module autodump
  use common;
  use introspection;
  use regular-expressions;
  use streams;
  use string-conversions;
  use standard-io;

  use parse-tree;
  use tokens;
  use variables;

  export 
    autodump;
end module autodump;
#end


define module main
  use common;
#if (mindy)
  use System, import: {system, copy-bytes, getenv, collect-garbage};
#else
  use System,
     import: {copy-bytes, call-out, c-expr, buffer-address, <raw-pointer>,
	      pointer-deref};
#end
  use string-conversions, import: {string-to-integer};

  use utils;
  use define-classes;
  use define-functions;
  use variables;
  use parser;
  use format;
  use lexer;
  use header;
  use source;
  use top-level-forms;
  use flow;
  use builder-interface;
  use fer-convert;
  use front;
  use dump;
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
#if (mindy)
  use autodump;
#end
  use standard-io;
  use tokens;
  use names;
end;
