rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/boolean.dylan,v 1.2 1995/11/13 23:09:07 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

define abstract class <boolean> (<object>)
end;

define class <true> (<boolean>)
end;

define sealed method make (class == <true>, #key) => res :: type-or();
  error("Can't make new instances of <true>, #t is it.");
end;

define class <false> (<boolean>)
end;

define sealed method make (class == <false>, #key) => res :: type-or();
  error("Can't make new instances of <false>, #f is it.");
end;

define sealed inline method \~ (thing :: <object>) => res :: <boolean>;
  %%primitive not (thing);
end;

