rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/copy.dylan,v 1.2 1995/11/13 23:09:07 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

define open generic as (class :: <class>, object :: <object>) => object;

define open generic shallow-copy (object :: <object>) => new;

define open generic class-for-copy (object :: <object>) => class :: <class>;

define inline method class-for-copy (object :: <object>) => class :: <class>;
  object-class(object);
end;
