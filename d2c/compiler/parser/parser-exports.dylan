module: dylan-user
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/parser/parser-exports.dylan,v 1.1 1996/01/08 13:10:52 ram Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

define library compiler-parser
  use Dylan;
  use compiler-base;
  use compiler-front;
  export parser;
end;

define module parser
  use common;
  use self-organizing-list;
  use utils;
  use compile-time-values;
  use tokens;
  use variables;
  use fragments;
  use parse-tree;
  use top-level-forms;

  export
    parse-program, parse-expression, parse-variable, parse-body,
    parse-case-body, parse-property-list, parse-type;
end;

define module macros
  use common;
  use utils;
  use source;
  use compile-time-values;
  use tokens;
  use names;
  use definitions;
  use variables;
  use fragments;
  use parse-tree;
  use top-level-forms;
  use parser;
  use expand;
  use lexenv, exclude: {add-binding};
  use builder-interface, import: {<fer-builder>};
  use od-format;

  export
    <define-macro-definition>, <define-bindings-macro-definition>,
    <statement-macro-definition>, <function-macro-definition>;
end;
