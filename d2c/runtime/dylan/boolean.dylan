rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/boolean.dylan,v 1.4 1996/02/22 23:53:15 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera


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
  error("Can't make new instances of <true>, #t is it.");
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
  %%primitive not (thing);
end;

