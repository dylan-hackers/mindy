rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/cmp.dylan,v 1.4 1995/12/09 20:59:12 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera


define generic \== (x :: <object>, y :: <object>) => answer :: <boolean>;

define sealed method \== (x :: <object>, y :: <object>)
    => answer :: <boolean>;
  let x-class :: <class> = x.object-class;
  if (~(x-class == y.object-class))
    #f;
  elseif (x-class.class-functional?)
    functional-==(x, y);
  else
    %%primitive \== (x, y);
  end;
end;


define sealed generic functional-== (x :: <object>, y :: <object>)
    => answer :: <boolean>;

define method functional-== (x :: <object>, y :: <object>)
    => answer :: <boolean>;
  #f;
end;


define generic \~== (x :: <object>, y :: <object>) => res :: <boolean>;

define inline method \~== (x :: <object>, y :: <object>) => res :: <boolean>;
  ~(x == y);
end;


define open generic \= (x :: <object>, y :: <object>) => answer :: <boolean>;

define inline method \= (x :: <object>, y :: <object>) => answer :: <boolean>;
  x == y;
end;


define open generic \< (x :: <object>, y :: <object>) => answer :: <boolean>;

// No default method for <.


define open generic \<= (x :: <object>, y :: <object>) => answer :: <boolean>;

define inline method \<= (x :: <object>, y :: <object>) => answer :: <boolean>;
  ~(y < x);
end method;


define open generic \~= (x :: <object>, y :: <object>) => answer :: <boolean>;

define inline method \~= (x :: <object>, y :: <object>) => answer :: <boolean>;
  ~(x = y);
end method;


define generic \>= (x :: <object>, y :: <object>) => answer :: <boolean>;

define sealed inline method \>= (x :: <object>, y :: <object>)
    => answer :: <boolean>;
  y <= x;
end method;


define generic \> (x :: <object>, y :: <object>) => answer :: <boolean>;

define sealed inline method \> (x :: <object>, y :: <object>)
    => answer :: <boolean>;
  y < x;
end method;


// min -- exported
//
// Return the minimum of a bunch of objects.
// 
define inline method min (object :: <object>, #rest more-objects)
    => res :: <object>;
  reduce(binary-min, object, more-objects);
end;
// 
define inline method binary-min (x :: <object>, y :: <object>)
    => res :: <object>;
  if (x < y) x else y end;
end;	  


// max -- exported
//
// Return the maximum of a bunch of objects.
// 
define inline method max (object :: <object>, #rest more-objects)
    => res :: <object>;
  reduce(binary-max, object, more-objects);
end;
//
define inline method binary-max (x :: <object>, y :: <object>)
    => max :: <object>;
  if (x < y) y else x end;
end;	  
