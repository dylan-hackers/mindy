rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/boolean.dylan,v 1.1 1998/05/03 19:55:37 andreas Exp $
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

// <boolean> -- exported from Dylan.
//
define abstract class <boolean> (<object>)
end;

// Seal = on <boolean>s.  But not yet.
// 
//seal generic \= (<boolean>, <object>);
//seal generic \= (<object>, <boolean>);

// <true> -- exported from Extensions.
//
define class <true> (<boolean>)
end;

// make{singleton(<true>)} -- exported GF method.
//
// Don't allow anyone to make another #t.  That would be bad.
// 
define sealed method make (class == <true>, #key) => res :: <never-returns>;
  error("Poems are made by fools like me, but only God can make #t.");
end;

// <false> -- exported from Extensions.
//
define class <false> (<boolean>)
end;

// make{singleton(<false>)} -- exported GF method.
//
// Don't allow anyone to make another #f.  That would be bad.
// 
define sealed method make (class == <false>, #key) => res :: <never-returns>;
  error("Can't make new instances of <false>, #f is it.");
end;

// ~ -- exported from Dylan.
//
// We use the magic not primitive instead of ``if (thing) #f else #t end''
// so that the compiler can more easily identify ~~x.
//
define inline method \~ (thing :: <object>) => res :: <boolean>;
  %%primitive(not, thing);
end;

