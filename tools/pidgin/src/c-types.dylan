Module: c-types


//=========================================================================
//  C Types
//=========================================================================
//  All C types are either primitive or derived from other types.

define abstract class <c-type> (<object>)
end class;

define abstract class <c-primitive-type> (<c-type>)
  slot c-primitive-type-name :: <string>,
    required-init-keyword: name:;
end class;

define abstract class <c-derived-type> (<c-type>)
end class;


//=========================================================================
//  Complete and Incomplete Types
//=========================================================================
//  Certain type declarations are incomplete and may not be used in other
//  kinds of declarations.

define generic c-type-complete? (type :: <c-type>) => (complete? :: <boolean>);

define method c-type-complete? (type :: <c-type>) => (complete? :: <boolean>)
  #t;
end;
// XXX - define more methods


//=========================================================================
//  Primitive Types
//=========================================================================
//  Primitive types are either numeric or void types.

define class <c-void-type> (<c-primitive-type>)
end;

define abstract class <c-numeric-type> (<c-primitive-type>)
end;

define class <c-integer-type> (<c-numeric-type>)
  // XXX - do something with <c-sign-specifier>
end;

define class <c-floating-point-type> (<c-numeric-type>)
end;

// Legal sign specifiers
define constant <c-sign-specifier> =
  one-of(#"signed", #"unsigned", #"unspecified");

// Void
define constant $c-void-type =
  make(<c-void-type>, name: "void");

// Integers
define constant $c-char-type =
  make(<c-integer-type>, name: "char");
define constant $c-signed-char-type =
  make(<c-integer-type>, name: "signed char");
define constant $c-unsigned-char-type =
  make(<c-integer-type>, name: "unsigned char");
define constant $c-short-type =
  make(<c-integer-type>, name: "short");
define constant $c-signed-short-type =
  make(<c-integer-type>, name: "signed short");
define constant $c-unsigned-short-type =
  make(<c-integer-type>, name: "unsigned short");
define constant $c-int-type =
  make(<c-integer-type>, name: "int");
define constant $c-signed-int-type =
  make(<c-integer-type>, name: "signed int");
define constant $c-unsigned-int-type =
  make(<c-integer-type>, name: "unsigned int");
define constant $c-long-type =
  make(<c-integer-type>, name: "long");
define constant $c-signed-long-type =
  make(<c-integer-type>, name: "signed long");
define constant $c-unsigned-long-type =
  make(<c-integer-type>, name: "unsigned long");
define constant $c-long-long-type =
  make(<c-integer-type>, name: "long long");
define constant $c-signed-long-long-type =
  make(<c-integer-type>, name: "signed long long");
define constant $c-unsigned-long-long-type =
  make(<c-integer-type>, name: "unsigned long long");

// Floats
define constant $c-float-type =
  make(<c-floating-point-type>, name: "float");
define constant $c-double-type =
  make(<c-floating-point-type>, name: "double");
define constant $c-long-double-type =
  make(<c-floating-point-type>, name: "long double");


//=========================================================================
//  Derived Types
//=========================================================================
//  C allows several kinds of derived types.

// XXX - make this thread safe
define variable *anonymous-tag-counter* = 0;

define abstract class <c-tagged-type> (<c-derived-type>)
  slot c-type-tag :: false-or(<string>),
    init-keyword: tag:,
    init-value: #f;
  slot c-type-anonymous-tag :: false-or(<integer>),
    init-value: #f;
end;

define method initialize
    (type :: <c-tagged-type>, #next next-method, #rest keys, #key, #all-keys)
  next-method();
  unless (type.c-type-tag)
    type.c-type-anonymous-tag := *anonymous-tag-counter*;
    *anonymous-tag-counter* := *anonymous-tag-counter* + 1;
    type.c-type-tag := format-to-string("$%d", *anonymous-tag-counter*);
  end;
end;

define abstract class <c-struct-or-union-type> (<c-tagged-type>)
  slot c-type-members :: false-or(<stretchy-vector>) = make(<stretchy-vector>),
    init-keyword: members:;
  // XXX - define members
end;

define class <c-struct-type> (<c-struct-or-union-type>)
end;

define class <c-union-type> (<c-struct-or-union-type>)
end;

define class <c-enum-type> (<c-tagged-type>)
  slot c-enum-members :: <stretchy-vector> = make(<stretchy-vector>);
  // XXX - define members
end;

define class <c-pointer-valued-type> (<c-derived-type>)
  slot c-pointer-referent-type :: <c-type>,
    required-init-keyword: referent:;
end;

define class <c-pointer-type> (<c-pointer-valued-type>)
end;

define class <c-array-type> (<c-pointer-valued-type>)
  slot c-array-size :: false-or(<integer>),
    init-keyword: size:,
    init-value: #f;
end;

define class <c-function-type> (<c-derived-type>)
  slot c-function-return-type :: <c-type>,
    required-init-keyword: return-type:;
  slot c-function-has-varargs? :: <boolean>,
    required-init-keyword: varargs?:;
  // XXX - ... need to think about this one
end;

define class <c-typedef-type> (<c-derived-type>)
  slot c-typedef-name :: <string>,
    required-init-keyword: name:;
  slot c-typedef-type :: <c-type>,
    required-init-keyword: type:;
end;


//=========================================================================
//  Type Names
//=========================================================================
//  Print out a simple name for a given type, when available. Otherwise,
//  return false.

define generic c-type-name
    (type :: <c-type>)
 => (name :: false-or(<string>));

define method c-type-name
    (type :: <c-type>)
 => (name :: singleton(#f))
  #f;
end;

define method c-type-name
    (type :: <c-primitive-type>)
 => (name :: <string>)
  type.c-primitive-type-name;
end;

define method c-type-name
    (type :: <c-struct-type>)
 => (name :: <string>)
  concatenate("struct ", type.c-type-tag);
end;

define method c-type-name
    (type :: <c-union-type>)
 => (name :: <string>)
  concatenate("union ", type.c-type-tag);
end;

define method c-type-name
    (type :: <c-enum-type>)
 => (name :: <string>)
  concatenate("enum ", type.c-type-tag);
end;

define method c-type-name
    (type :: <c-typedef-type>)
 => (name :: <string>)
  type.c-typedef-name;
end;

define constant <c-named-type> =
  type-union(<c-primitive-type>, <c-tagged-type>, <c-typedef-type>);


//=========================================================================
//  Formatting type declarators
//=========================================================================
//  Format a declarator for a given type and declaration name. If no
//  declaration name is specified, format an abstract declarator. This is a
//  really obnoxious problem, to say the least.
//
//  Oh, yeah--this conses like crazy, so don't expect it to run fast.

define function format-c-type-declarator
    (type :: <c-type>,
     #key decl-name :: false-or(<string>), parameter-names? :: <boolean>)
 => (decl :: <string>)
  let type-name = c-type-name(type);
  if (type-name)
    // Short-circuit the common cases for performance.
    if (decl-name)
      concatenate(type-name, " ", decl-name);
    else
      type-name;
    end if;
  else
    // Oh, well. This will take some work, then.
    let types = nested-type-list(type);

    let last-was-direct? = #t;
    let decl = decl-name | "";

    for (type in types)
      select (type by instance?)

	<c-pointer-type> =>
	  decl := concatenate("*", decl);
	  last-was-direct? := #f;

	<c-array-type> =>
	  let size = type.c-array-size;
	  let size-str =
	    if (size)
	      format-to-string("[%d]", size);
	    else
	      "[]"
	    end;
	  decl :=
	    if (last-was-direct?)
	      concatenate(decl, size-str);
	    else
	      concatenate("(", decl, ")", size-str);
	    end;
	  last-was-direct? := #t;

	<c-function-type> =>
	  let arguments = ""; // XXX - this is wrong
	  decl :=
	    if (last-was-direct?)
	      concatenate(decl, "(", arguments, ")");
	    else
	      concatenate("(", decl, ")(", arguments, ")");
	    end;
	  last-was-direct? := #t;

	<c-named-type> =>
	  // Always our final time through the loop...
	  decl := concatenate(c-type-name(type), " ", decl);

	otherwise =>
	  error("did not expect %= when formatting C declarator");

      end select;
    end for;
    // Ugh. That was *way* too hard.
    decl;
  end if;
end function format-c-type-declarator;

define generic nested-type-list
    (type :: <c-type>)
 => (nested :: <list>);

define method nested-type-list
    (type :: <c-named-type>)
 => (nested :: <list>)
  pair(type, #());
end;

define method nested-type-list
    (type :: <c-pointer-valued-type>)
 => (nested :: <list>)
  pair(type, nested-type-list(type.c-pointer-referent-type));
end;

define method nested-type-list
    (type :: <c-function-type>)
 => (nested :: <list>)
  pair(type, nested-type-list(type.c-function-return-type));
end;
