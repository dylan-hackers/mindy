module: dylan-user
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/main/main-exports.dylan,v 1.2 1996/01/31 23:59:47 ram Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

define library compiler-main
  use Dylan;
  use compiler-base;
  use compiler-front;
  use compiler-parser;
  use compiler-cback;
end;

define module lexer
  use common;
  use System, import: {};

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

define module main
  use common;
#if (mindy)
  use System, import: {system, copy-bytes, getenv};
#else
  use System, import: {copy-bytes};
#end

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
  use string-conversions;
  use autodump;
#end
  use standard-io;
  use tokens;
end;
