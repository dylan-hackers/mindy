rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/condition.dylan,v 1.3 1995/11/13 23:09:07 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

define method check-type
    (object :: <object>, type :: <type>) => object :: <object>;
  %check-type(object, type);
end;

define inline method %check-type
    (object :: <object>, type :: <type>) => object :: <object>;
  if (instance?(object, type))
    object;
  else
    type-error(object, type);
  end;
end;

define method type-error
    (object :: <object>, type :: <type>) => res :: type-or();
  error("%= isn't of type %=", object, type);
end;

define method error
    (str :: <byte-string>, #rest args) => res :: type-or();
  write("error: ");
  apply(format, str, args);
  write('\n');
  %%primitive call-out ("abort", void:);
end;

define method lose
    (str :: <byte-string>, #rest args) => res :: type-or();
  write("internal error: ");
  apply(format, str, args);
  write('\n');
  %%primitive call-out ("abort", void:);
end;


// Internal errors.

define method uninitialized-slot-error () => res :: type-or();
  error("Slot is not initialized.");
end;

define method missing-required-init-keyword-error
    (keyword :: <symbol>, class :: <class>) => res :: type-or();
  error("Missing required-init-keyword %= in make of %=", keyword, class);
end;

define method wrong-number-of-arguments-error
    (fixed? :: <boolean>, wanted :: <fixed-integer>, got :: <fixed-integer>)
    => res :: type-or();
  error("Wrong number of arguments.  Wanted %s %d but got %d.",
	if (fixed?) "exactly" else "at least" end,
	wanted, got);
end;

define method odd-number-of-keyword/value-arguments-error ()
    => res :: type-or();
  error("Odd number of keyword/value arguments.");
end;

define method unrecognized-keyword-error (key :: <symbol>) => res :: type-or();
  error("Unrecognized keyword: %=.", key);
end;

define method no-applicable-methods-error () => res :: type-or();
  error("No applicable methods.");
end;

define method ambiguous-method-error (methods :: <list>) => res :: type-or();
  error("It is ambiguous which of these methods is most specific:\n  %s",
	methods);
end;

