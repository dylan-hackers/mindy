module: Dylan-User
author: chiles@cs.cmu.edu
synopsis: This file defines the Print library and modules.
copyright: See below.
rcs-header: $Header: /scm/cvs/src/common/print/library.dylan,v 1.1 1998/05/03 19:55:01 andreas Exp $

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
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
//


///
/// These definitions go into the Dylan-User module because this is how we
/// jumpstart a library.
///

define library print
  use dylan;
  use streams;
  export pprint, print;
end library;


define module pprint
  use Dylan;
  use Extensions;
  use System;
  use Streams;

  export
    <pretty-stream>, pprint-logical-block, pprint-newline, pprint-indent,
    pprint-tab, *default-line-length*, *print-miser-width*;
end;

/// The Internals Module exports everything that is necessary to make the
/// code in the Print Module run, but only those things that are of an
/// internals nature to a particular Dylan implementation.
///
/// There is inconsistent usage of the Internals module between the Print
/// and Pprint modules because different hackers wrote each.  We should
/// make Pprint use Internals and import what it needs, same for Print, but
/// we don't have time now to determine exactly what Pprint uses.
///
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

define module print
  use dylan;
  use streams;
  use pprint;
  use internals;
  use extensions,
    import: {$minimum-integer, $not-supplied, <byte-character>,
	     <ratio>, numerator, denominator};
  export
    print,
    print-object,
    print-to-string,

    print-length,
    print-level,
    print-depth,
    print-circle?,
    print-pretty?,
    *default-length*,
    *default-level*,
    *default-circle?*,
    *default-pretty?*;
end module;
