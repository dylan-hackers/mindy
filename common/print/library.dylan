module: Dylan-User
author: chiles@cs.cmu.edu
synopsis: This file defines the Print library and modules.
copyright: See below.
rcs-header: $Header: /scm/cvs/src/common/print/library.dylan,v 1.3 2002/06/03 22:25:01 dauclair Exp $

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
//

define library print
  use dylan;
  use streams;
  export pprint, print;
end library;

define module internals
  use dylan;
  use extensions,
    import: {<general-integer>, <extended-integer>,
	     false-or, one-of},
    export: all;
  use introspection,
    import: {class-name, function-name, 
	     <subclass>, <limited-integer>, <union>,
#if (~mindy)
             <direct-instance>, direct-instance-of, <byte-character-type>,
             union-singletons,
#endif
	     singleton-object, subclass-of, limited-integer-base-class,
	     limited-integer-minimum, limited-integer-maximum, union-members},
    export: all;
end module;

define module pprint
  use Dylan;
  use internals;
  use System, import: { copy-bytes };
  use Streams;

  export
    pprint-logical-block, pprint-newline, pprint-indent, pprint-tab,
    *default-line-length*, *print-miser-width*;
end;

define module print
  use dylan;
  use streams;
  use pprint;
  use internals;
  use extensions,
    import: {$minimum-integer, $not-supplied, <byte-character>,
	     <ratio>, numerator, denominator};
  export
    print, print-object, print-to-string,
    *print-length*, *print-level*, *print-circle?*, *print-pretty*;
end module;
