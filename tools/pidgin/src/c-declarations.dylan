Module: c-declarations

//=========================================================================
//  C Declarations
//=========================================================================
//  Declarations normally appear in the top level of level of header and
//  source files. They may also appear as members in structs and unions and
//  as parameters to functions.
//
//  Tagged type declaration
//  Typedef declaration
//  Object declaration
//    Member declaration
//    Variable declaration
//      Function definition (we don't care about this now)
//  Parameter declaration
//  define
//    integer define
//    string define
//    type alias define
//    unknown define

define abstract class <c-declaration> (<object>)
  // XXX - Eventually we want to add a source location
end class;

define generic c-declaration-name
    (decl :: <c-declaration>)
 => (name :: <string>);

define generic c-declaration-type
    (decl :: <c-declaration>)
 => (type :: false-or(<c-type>));


//=========================================================================
//  Tagged Type Declarations
//=========================================================================
//  Whenever 'struct', 'union' or 'enum' appears with a body, it defines a
//  new tagged type. If the tag is omitted, the type is a unique, anonymous
//  type.
//
//  Tagged type declarations take the following form:
//
//    (struct|union|enum) ... { ... }

define class <c-tagged-type-declaration> (<c-declaration>)
  slot c-tagged-type :: <c-tagged-type>,
    required-init-keyword: type:;
end;

define method c-declaration-name
    (decl :: <c-tagged-type-declaration>)
 => (name :: <string>)
  decl.c-type-name;
end;

define class <c-typedef-declaration> (<c-declaration>)
  slot c-typedef-type :: <c-typedef-type>,
    required-init-keyword: type:;
end;

define class <c-object-declaration> (<c-declaration>)
define class <c-variable-declaration> (<c-declaration>)
  slot c-variable-name :: <string>,
    required-init-keyword: name:;
  slot c-variable-type :: <c-type>,
    required-init-keyword: type:;
  slot c-variable-extern? :: <boolean>,
    required-init-keyword: extern:;
end;

define abstract class <c-define> (<c-declaration>)
  slot c-define-name :: <string>,
    required-init-keyword: name:;
end;

define class <c-integer-define> (<c-define>)
  slot c-integer-define-value :: <integer>,
    required-init-keyword: value:;
end;

define class <c-string-define> (<c-define>)
  slot c-string-define-value :: <string>,
    required-init-keyword: value:;
end;

define class <c-type-alias-define> (<c-define>)
  slot c-type-alias-define-type :: <string>,
    required-init-keyword: type:;
end;

define class <c-unknown-define> (<c-define>)
end;
