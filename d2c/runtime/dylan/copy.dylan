module: dylan

define open generic as (class :: <class>, object :: <object>) => object;

define open generic shallow-copy (object :: <object>) => new;

define open generic class-for-copy (object :: <object>) => class :: <class>;

define inline method class-for-copy (object :: <object>) => class :: <class>;
  object-class(object);
end;
