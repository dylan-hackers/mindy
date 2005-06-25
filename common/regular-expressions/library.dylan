module:     dylan-user
author:     Nick Kramer (nkramer@cs.cmu.edu)
synopsis:   Contains the library and module definitions for the Regular
            Expressions library.
copyright: see below
rcs-header: $Header: /scm/cvs/src/common/regular-expressions/library.dylan,v 1.2 2000/01/24 04:54:50 andreas Exp $

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
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


define library regular-expressions
  use dylan;
  use collection-extensions;
  use string-extensions;
  use table-extensions;
  export
    regular-expressions;
end library regular-expressions;

define module regular-expressions
  use dylan;
  use extensions;
  use string-conversions;
  use character-type;
  use string-hacking;
  use subseq;
  use %do-replacement;
  use %parse-string;
  use substring-search;
  use table-extensions, import: { string-hash };
  export
    regexp-position, make-regexp-positioner,
    regexp-replace, make-regexp-replacer,
    translate, make-translator,
    split, make-splitter,
    join,
    <illegal-regexp>;
end module regular-expressions;
