rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/object.dylan,v 1.3 1996/03/17 00:11:23 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

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
