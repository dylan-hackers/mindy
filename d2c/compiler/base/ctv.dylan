module: compile-time-values
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/ctv.dylan,v 1.1 1994/12/12 13:01:15 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

define abstract class <ct-value> (<object>)
end;

define abstract class <eql-ct-value> (<ct-value>)
  slot ct-value-singleton :: union(<false>, <ctype>),
    init-value: #f;
end;


// The unbound marker.

define class <ct-unbound-marker> (<ct-value>)
end;

define method make (wot == <ct-unbound-marker>, #next next-method,
		    #key trust-me-I-know-what-Im-doing)
  if (trust-me-I-know-what-Im-doing)
    next-method();
  else
    $Unbound-Marker-CT-Value;
  end;
end;

define constant $Unbound-Marker-CT-Value
  = make(<ct-unbound-marker>, trust-me-I-know-what-Im-doing: #t);


// Literals.

define class <ct-literal> (<ct-value>)
  slot ct-literal-value :: <object>, required-init-keyword: value:;
end;

define method print-object (ctv :: <ct-literal>, stream :: <stream>) => ();
  pprint-fields(ctv, stream, value: ctv.ct-literal-value);
end;

define method make (wot == <ct-literal>, #next next-method,
		    #key value = error("required keyword value: not supplied"))
  select (value by instance?)
    type-or(<integer>, <float>, <character>, <symbol>,
	    <boolean>, <empty-list>) =>
      make(<eql-ct-literal>, value: value);
    type-or(<byte-string>, <simple-object-vector>, <pair>) =>
      next-method();
    otherwise =>
      error("%= can't be represented as a compile-time literal.");
  end;
end;

define class <eql-ct-literal> (<ct-literal>, <eql-ct-value>)
end;

define constant $ct-literal-eql-memo = make(<table>);

define method make (wot == <eql-ct-literal>, #next next-method, #key value)
  element($ct-literal-eql-memo, value, default: #f)
    | (element($ct-literal-eql-memo, value) := next-method());
end;



// ct-value and dylan-value

// ct-value -- exported.
//
// Returns the compile-time value for some definition, of #f if that
// definition doesn't have a compile time value.
//
define generic ct-value (defn :: <definition>)
    => res :: union(<ct-value>, <false>);

define method ct-value (defn :: <definition>)
    => res :: <false>;
  #f;
end;

// dylan-value -- exported.
//
// Returns the compile-time value for the given name in the dylan module,
// or #f if it isn't defined.
// 
define method dylan-value (name :: <symbol>)
    => res :: union(<false>, <ct-value>);
  let defn = dylan-defn(name);
  defn & defn.ct-value;
end;

