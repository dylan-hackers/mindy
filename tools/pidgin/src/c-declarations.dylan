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
//  Variable declaration
//    Function definition (we don't care about this now)
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

define generic format-c-declaration
    (decl :: <c-declaration>, #key multi-line? :: <boolean>)
 => (formatted :: <string>);


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
//
//  We pull all such declarations to the top level. If they don't have a
//  tag, we assign an "anonymous tag" similar to a LISP gensym.

define class <c-tagged-type-declaration> (<c-declaration>)
  slot c-tagged-type-declaration-type :: <c-tagged-type>,
    required-init-keyword: type:;
end;

define method c-declaration-name
    (decl :: <c-tagged-type-declaration>)
 => (name :: <string>)
  decl.c-tagged-type-declaration-type.c-type-name;
end;

define method c-declaration-type
    (decl :: <c-tagged-type-declaration>)
 => (type :: false-or(<c-type>))
  decl.c-tagged-type-declaration-type;
end;

define method format-c-declaration
    (decl :: <c-tagged-type-declaration>, #key multi-line? :: <boolean>)
 => (formatted :: <string>)
  concatenate(format-c-tagged-type(decl.c-tagged-type-declaration-type,
				   multi-line?: multi-line?),
	      ";");
end;


//=========================================================================
//  Typedef Declarations
//=========================================================================
//  In C, typedefs are transparent. We record all typedefs, however, in
//  case a higher-level tool wants to use them for some purpose.

define class <c-typedef-declaration> (<c-declaration>)
  slot c-typedef-declaration-type :: <c-typedef-type>,
    required-init-keyword: type:;
end;

define method c-declaration-name
    (decl :: <c-typedef-declaration>)
 => (name :: <string>)
  decl.c-typedef-declaration-type.c-typedef-name;
end;

define method c-declaration-type
    (decl :: <c-typedef-declaration>)
 => (type :: false-or(<c-type>))
  decl.c-typedef-declaration-type.c-typedef-type;
end;

define method format-c-declaration
    (decl :: <c-typedef-declaration>, #key multi-line? :: <boolean>)
 => (formatted :: <string>)
  concatenate("typedef ",
	      format-c-type(decl.c-declaration-type,
			    decl-name: decl.c-declaration-name),
	      ";");
end;


//=========================================================================
//  Variable Declarations
//=========================================================================
//  Variable declarations include variables, functions and any other kind
//  of "non-type", top-level declaration.

define class <c-variable-declaration> (<c-declaration>)
  slot c-variable-name :: <string>,
    required-init-keyword: name:;
  slot c-variable-type :: <c-type>,
    required-init-keyword: type:;
  slot c-variable-extern? :: <boolean>,
    init-keyword: extern?:,
    init-value: #t;
end;

define method c-declaration-name
    (decl :: <c-variable-declaration>)
 => (name :: <string>)
  decl.c-variable-name;
end;

define method c-declaration-type
    (decl :: <c-variable-declaration>)
 => (type :: false-or(<c-type>))
  decl.c-variable-type;
end;
  
define method format-c-declaration
    (decl :: <c-variable-declaration>, #key multi-line? :: <boolean>)
 => (formatted :: <string>)
  concatenate(if (decl.c-variable-extern?) "extern " else "static " end,
	      format-c-type(decl.c-declaration-type,
			    decl-name: decl.c-declaration-name),
	      ";");
end;


//=========================================================================
//  #define
//=========================================================================
//  C discards all #defines during the parse stage. We keep them around,
//  however, for use by higher-level tools. We attempt to interpret
//  #defines as integer expressions, strings or type aliases.
//
//  When formating #defines, we always wrap them in a comment. This is
//  because they have a new meaning--they're no longer preprocessor
//  declarations, they're now a new type of declaration. In particular,
//  it's *not* safe to dump them in their current form and reparse them
//  together with other declarations.

define abstract class <c-define> (<c-declaration>)
  slot c-define-name :: <string>,
    required-init-keyword: name:;
end;

define method c-declaration-name
    (decl :: <c-define>)
 => (name :: <string>)
  decl.c-define-name;
end;

define method c-declaration-type
    (decl :: <c-define>)
 => (type :: singleton(#f))
  #f;
end;

define method format-c-declaration
    (decl :: <c-define>, #key multi-line? :: <boolean>)
 => (formatted :: <string>)
  concatenate("/* #define ", decl.c-define-name, " ",
	      format-c-define-value(decl), " */");
end;

define generic format-c-define-value
    (decl :: <c-define>)
 => (formatted :: <string>);


//=========================================================================
//  Integer-valued #define
//=========================================================================
//  A constant-folded, integer-valued expression.

define class <c-integer-define> (<c-define>)
  slot c-integer-define-value :: <integer>,
    required-init-keyword: value:;
end;

define method c-declaration-type
    (decl :: <c-integer-define>)
 => (type :: <c-type>)
  #f; // XXX - some subclass of <c-integer-type>, but which?
end;

define method format-c-define-value
    (decl :: <c-integer-define>)
 => (formatted :: <string>)
  format-to-string("%d", decl.c-integer-define-value);
end;


//=========================================================================
//  String-valued #define
//=========================================================================
//  A constant string.

define class <c-string-define> (<c-define>)
  slot c-string-define-value :: <string>,
    required-init-keyword: value:;
end;

define method c-declaration-type
    (decl :: <c-string-define>)
 => (type :: <c-type>)
  #f; // XXX - should be canonical "char *" type, but how?
end;

define method format-c-define-value
    (decl :: <c-string-define>)
 => (formatted :: <string>)
  concatenate("\"", decl.c-string-define-value, "\"");
end;


//=========================================================================
//  Type-alias #define
//=========================================================================
//  Something which looked like an abstract type declarator.
//
//  I'm not really sure that we need to pay attention to type aliases, but
//  Melange did.

define class <c-type-alias-define> (<c-define>)
  slot c-type-alias-define-type :: <c-type>,
    required-init-keyword: type:;
end;

define method format-c-define-value
    (decl :: <c-type-alias-define>)
 => (formatted :: <string>)
  format-c-type(decl.c-type-alias-define-type);
end;


//=========================================================================
//  Unknown-valued #define
//=========================================================================
//  Some defines don't look like anything in particular.

define class <c-unknown-define> (<c-define>)
end;

define method format-c-define-value
    (decl :: <c-unknown-define>)
 => (formatted :: <string>)
  "...";
end;
