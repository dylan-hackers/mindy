module: dylan-user
rcs-header: $Header: /scm/cvs/src/d2c/compiler/cback/cback-exports.dylan,v 1.12 2002/08/28 13:06:57 bruce Exp $
copyright: see below

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000, 2001  Gwydion Dylan Maintainers
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

define library compiler-cback
  use Dylan;
  use stream-extensions;
  use compiler-base;
  use compiler-front;
  use compiler-convert;
  use compiler-parser;  // for <macro-source-location>
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
    analyze-stack-usage;
end;


define module cback
  use indenting-streams;
  use c-representation;
  use classes;
  use common;
  use errors;
  use compile-time-functions;
  use compile-time-values;
  use platform,
    import: {*current-target*, platform-integer-length};
  use ctype;
  use definitions;
  use function-definitions;
  use variable-definitions;
  use define-classes;
  use flow;
  use front;
  use names;
  use od-format;
  use primitives;
  use representation;
  use signature-interface;
  use stack-analysis;
  use top-level-forms;
  use utils;
  use variables;
  use source;
  use source-utilities; // For <macro-source-location>, which isn't yet handled
  use Extensions, import: {<stretchy-object-vector>};
  use abstract-optimizer;
  use xep-tools, import: {build-xep-component};

  export
    <unit-state>, unit-prefix, unit-init-roots, unit-eagerly-reference,
    <root>, root-name, root-init-value, root-comment,
    <file-state>, file-body-stream, file-guts-stream,
    file-prototypes-exist-for, get-string, $indentation-step,
    emit-prologue, emit-tlf-gunk, emit-component, maybe-emit-include,
    maybe-emit-prototype, gf-generic-entry-point,
    get-info-for, const-info-heap-labels, const-info-heap-labels-setter,
    const-info-dumped?, const-info-dumped?-setter,
    entry-point-c-name, *emit-all-function-objects?*,
    c-name, c-name-global, string-to-c-name, clean-for-comment,
    float-to-string,
    <global-heap-file-state>, <local-heap-file-state>,
    build-global-heap, build-local-heap;

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
  use function-definitions;
  use cback;
  use od-format;
  use platform;
  use indenting-streams;

end;

