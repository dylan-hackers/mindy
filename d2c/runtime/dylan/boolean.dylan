rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/boolean.dylan,v 1.3 1995/11/16 03:34:59 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

define abstract class <boolean> (<object>)
end;

define class <true> (<boolean>)
end;

define sealed method make (class == <true>, #key) => res :: <never-returns>;
  error("Can't make new instances of <true>, #t is it.");
end;

define class <false> (<boolean>)
end;

define sealed method make (class == <false>, #key) => res :: <never-returns>;
  error("Can't make new instances of <false>, #f is it.");
end;

define sealed inline method \~ (thing :: <object>) => res :: <boolean>;
  %%primitive not (thing);
end;

