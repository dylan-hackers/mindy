module: c-parser
synopsis: Stuff which will only stay around during the integration process.

//=========================================================================
//  C Declarations from c-decl.dylan
//=========================================================================

define function make-struct-type (#rest rest)
end;

define function c-type-size (#rest rest)
end;

define function add-cpp-declaration (#rest rest)
end;

define function unknown-type (#rest rest)
end;

define class <declaration> (<object>)
  slot canonical-name;
  slot true-type;
end;

define class <arg-declaration> (<declaration>)
end;

define class <varargs-declaration> (<declaration>)
end;

define class <enum-slot-declaration> (<declaration>)
  slot constant-value;
end;

define class <integer-type-declaration> (<declaration>)
end;

define function make-enum-slot (#rest rest)
end;

define function fake-referent (#rest rest)
end;
