rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/object.dylan,v 1.1 1998/05/03 19:55:38 andreas Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

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

// <object> -- exported.
//
define primary abstract open class <object> ()
  //
  // The class of the instance.  Non-abstract classes automatically override
  // the init-value to be the correct class.
  constant slot %object-class :: <class> = <object>;
end;

// object-class -- exported function.
//
// Return the class of thing.  We don't just call the slot object-class because
// we don't want people outside the dylan module to be able to name the slot
// (e.g. add their own override) which would just confuse the compiler.
//
define inline method object-class (thing :: <object>)
    => res :: <class>;
  %object-class(thing);
end;

define abstract open class <functional-object> (<object>)
end;
