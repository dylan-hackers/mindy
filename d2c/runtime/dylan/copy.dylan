rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/copy.dylan,v 1.3 1995/12/06 21:34:54 rgs Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

define open generic as (class :: <class>, object :: <object>) => object;

define open generic shallow-copy (object :: <object>) => new;

define open generic class-for-copy (object :: <object>) => class :: <class>;

define sealed inline method identity
    (object :: <object>) => (object :: <object>);
  object;
end method identity;

// Without subtype specializers, we cannot specify a default "as" method for
// all <collection> types.  Instead, we place support in this catch-all
// method.
//
define method as (cls :: <class>, obj :: <object>) => (result :: <object>);
  case
    (instance?(obj, cls)) => obj;
    (subtype?(cls, <collection>) & instance?(obj, <collection>)) =>
      map-as(cls, identity, obj);
    otherwise =>
      error("Object %= cannot be converted to class %=.", obj, cls);
  end case;
end method as;

define inline method class-for-copy (object :: <object>) => class :: <class>;
  object-class(object);
end;
