module: c-parser
synopsis: Stuff which will only stay around during the integration process.

define class <stub-object> (<object>)
end;

define method initialize
    (object :: <stub-object>,
     #next next-method,
     #rest rest,
     #key,
     #all-keys)
  error("Cannot create stub object %=.", object);
end;

//=========================================================================
//  C Declarations from c-decl.dylan
//=========================================================================

// Used in rules for struct-or-union-specifier.
// Used in rules for Enum-specifier.
define function make-struct-type (#rest rest)
  error("STUB: make-struct-type");
end;

// Used when processing <SIZEOF-token>.
define function c-type-size (name :: <icky-type-name>)
  // XXX - need information about local C compiler
  error("cannot evaluate sizeof expressions");
end;

// Used when processing <end-include-token>.
// Used in the function parse.
define function add-cpp-declaration
    (state :: <parse-state>, macro-name :: <string>)
 => ()
  parse-warning(state, "Ignoring '#define %s' for now", macro-name);

/*
  block ()
    let value = parse-macro(macro-name, state);
    state.objects[macro-name] :=
      add-declaration(state, make(<macro-declaration>, name: macro-name,
				  value: value));
  exception (<error>)
    #f;
  end block;
*/
end function add-cpp-declaration;

// Used when constructing <arg-declaration>s.
// Used when constructing <varargs-declaration>s.
define constant unknown-type = #"stub-for-unknown-type";

// No longer used (except as a superclass in this file).
define class <declaration> (<stub-object>)
end;

// Used in rules on cast-expr.
define function true-type (#rest rest)
  error("STUB: true-type");
end;

// Used in rules for identifier-list.
// Used in rules for parameter-declaration.
define class <arg-declaration> (<declaration>)
end;

// Used in rules for Parameter2.
// Used in rules for Parameter-Identifier-List.
// Used in rules for parameter-type-list.
// Used in rules for abstract-declarator2.
define class <varargs-declaration> (<declaration>)
end;

// Used when calling int-value on value of <identifier-token>.
define class <enum-slot-declaration> (<declaration>)
end;

// Used when calling int-value on value of <identifier-token>.
define function constant-value (#rest rest)
  error("STUB: true-type");
end;

// Used in rules on cast-expr.
define class <integer-type-declaration> (<declaration>)
end;

// Used in rules on enumerator-list.
define function make-enum-slot (#rest rest)
  error("STUB: make-enum-slot");
end;
