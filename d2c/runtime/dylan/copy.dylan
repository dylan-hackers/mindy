rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/copy.dylan,v 1.4 1995/12/09 02:47:02 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

define open generic as (class :: <class>, object :: <object>) => object;

define open generic shallow-copy (object :: <object>) => new;

define open generic type-for-copy (object :: <object>) => type :: <type>;

define sealed inline method identity
    (object :: <object>) => (object :: <object>);
  object;
end method identity;

// Without subtype specializers, we cannot specify a default "as" method for
// all <collection> types.  Instead, we place support in this catch-all
// method.
//
define method as (class :: <class>, obj :: <object>) => (result :: <object>);
  case
    (instance?(obj, class)) => obj;
    (subtype?(class, <collection>) & instance?(obj, <collection>)) =>
      map-as(class, identity, obj);
    otherwise =>
      error("Object %= cannot be converted to class %=.", obj, class);
  end case;
end method as;

define inline method type-for-copy (object :: <object>) => type :: <type>;
  object-class(object);
end;
