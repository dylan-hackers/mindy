module: dylan-user
rcs-header: $Header: /scm/cvs/src/d2c/compiler/optimize/optimize-exports.dylan,v 1.1 1998/05/03 19:55:35 andreas Exp $
copyright: Copyright (c) 1996  Carnegie Mellon University
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

define library compiler-optimize
  use Dylan;
  use compiler-base;
  use compiler-front;

  export
    cheese;

end library compiler-optimize;


define module cheese
  use common;
  use utils;
  use errors;
  use compile-time-values;
  use platform,
    import: {*current-target*, platform-integer-length};
  use names;
  use definitions;
  use variables, exclude: {<renaming>};
  use flow;
  use front;
  use ctype;
  use classes;
  use signature-interface;
  use source;
  use builder-interface;
  use policy;
  use primitives;
  use transformers;
  use compile-time-functions;
  use function-definitions;

  export
    *optimize-ncalls*;
end;
