module: c-parser
synopsis: Stuff which will only stay around during the integration process.

//=========================================================================
//  C Declarations from c-decl.dylan
//=========================================================================

// Used in rules for struct-or-union-specifier.
// Used in rules for Enum-specifier.
define function make-struct-type (#rest rest)
end;

// Used when processing <SIZEOF-token>.
define function c-type-size (#rest rest)
end;

// Used when processing <end-include-token>.
// Used in the function parse.
define function add-cpp-declaration (#rest rest)
end;

// Used when constructing <arg-declaration>s.
// Used when constructing <varargs-declaration>s.
define function unknown-type (#rest rest)
end;

// Used in the function parse-type.
define class <declaration> (<object>)

  // Used when calling parse-progress-report (to report progress, of course).
  slot canonical-name;

  // Used in rules on cast-expr.
  slot true-type;
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

  // Used when calling int-value on value of <identifier-token>.
  slot constant-value;
end;

// Used in rules on cast-expr.
define class <integer-type-declaration> (<declaration>)
end;

// Used in rules on enumerator-list.
define function make-enum-slot (#rest rest)
end;
