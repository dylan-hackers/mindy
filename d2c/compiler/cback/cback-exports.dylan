module: dylan-user
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/cback/cback-exports.dylan,v 1.6 1996/02/09 01:37:43 rgs Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

define library compiler-cback
  use Dylan;
  use compiler-base;
  use compiler-front;
  export cback;
  export heap;
end library;


define module stack-analysis
  use common;
  use utils;
  use flow;
  use front;
  use ctype;
  use signature-interface;
  use definitions;
  use compile-time-functions;

  export
    analize-stack-usage;
end;


define module cback
  use system;
  
  use c-representation;
  use classes;
  use common;
  use compile-time-functions;
  use compile-time-values;
  use ctype;
  use define-constants-and-variables;
  use define-functions;
  use definitions;
  use forward-defn-classes;
  use flow;
  use front;
  use names;
  use od-format;
  use primitives;
  use representation;
  use signature-interface;
  use stack-analysis;
  use top-level-expressions;
  use top-level-forms;
  use utils;
  use variables;

  export
    <unit-state>, unit-init-roots, unit-root-names,
    <file-state>, 
    <unit-info>, unit-name, undumped-objects,
    emit-prologue, emit-tlf-gunk, emit-component,
    entry-point-c-name;
end;


define module heap
  use common;
  use utils;
  use errors;
  use names;
  use signature-interface;
  use compile-time-values;
  use variables;
  use representation;
  use c-representation;
  use ctype;
  use classes;
  use compile-time-functions;
  use definitions;
  use define-functions;
  use define-classes;
  use cback;

  export
    build-initial-heap, build-local-heap;
end;

