Module: c-types

//=========================================================================
//  C Type Repository
//=========================================================================
//  Every C type implicity defines an infinite number of additional types.
//  For example, 'char' defines:
//
//    char *, char **, char ***, ..
//    char [], char [10], char [30], ...
//    char (), char (char), char (char, char), ...
//
//  But we don't want a zillion 'char *' types. Instead, we store one
//  canonical example of each type in a type repository and fetch it when
//  needed.

define class <c-type-repository> (<object>)

  // We look up pointers by the type they point to.
  slot c-pointer-types :: <object-table> =
    make(<object-table>);

  // We look up typedefs by name.
  slot c-typedef-types :: <string-table> =
    make(<string-table>);

  // We look up structs, unions and enums by tag name.
  slot c-struct-types :: <string-table> =
    make(<string-table>);
  slot c-union-types :: <string-table> =
    make(<string-table>);
  slot c-enum-types :: <string-table> =
    make(<string-table>);

  // For all other types, we format an abstract declarator and hash it
  // as though it were an identifier. This is slow, but it should handle
  // anything, no matter how weird.
  slot c-other-types :: <string-table> =
    make(<string-table>);
end;


//=========================================================================
//  Finding canonical C types
//=========================================================================
//  This function looks up a type in the repository. If the type isn't
//  found, it gets added to the repository. If the type *is* found, the
//  existing copy is returned.
//
//  It is an error to call find-canonical-c-type on a derived type with
//  non-canonical constituent types. Consider yourself warned.
//
//  Instances of <c-primitive-type> are always canonical.

define generic find-canonical-c-type
    (repository :: <c-type-repository>, type :: <c-type>)
 => (canonical-type :: <c-type>);

// This function gets "pulled down" into each of the methods.
define inline function add-to-table-if-missing
    (table :: <table>, key :: <object>, value :: <c-type>)
 => (canonical-value :: <c-type>)
  let existing = element(table, key, default: #f);
  if (existing)
    // XXX - We might want to check 'existing' and 'value' for equivalence.
    existing;
  else
    table[key] := value;
    value;
  end;
end function;

define method find-canonical-c-type
    (repository :: <c-type-repository>, type :: <c-type>)
 => (canonical-type :: <c-type>)
  add-to-table-if-missing(repository.c-other-types,
			  format-c-type(type),
			  type);
end method;

define inline method find-canonical-c-type
    (repository :: <c-type-repository>, type :: <c-primitive-type>)
 => (canonical-type :: <c-primitive-type>)
  type;
end method;

define method find-canonical-c-type
    (repository :: <c-type-repository>, type :: <c-pointer-type>)
 => (canonical-type :: <c-pointer-type>)
  add-to-table-if-missing(repository.c-pointer-types,
			  type.c-pointer-referent-type,
			  type);
end;

define method find-canonical-c-type
    (repository :: <c-type-repository>, type :: <c-typedef-type>)
 => (canonical-type :: <c-typedef-type>)
  add-to-table-if-missing(repository.c-typedef-types,
			  type.c-typedef-name,
			  type);
end;

define method find-canonical-c-type
    (repository :: <c-type-repository>, type :: <c-struct-type>)
 => (canonical-type :: <c-struct-type>)
  add-to-table-if-missing(repository.c-struct-types,
			  type.c-type-tag,
			  type);
end;

define method find-canonical-c-type
    (repository :: <c-type-repository>, type :: <c-union-type>)
 => (canonical-type :: <c-union-type>)
  add-to-table-if-missing(repository.c-union-types,
			  type.c-type-tag,
			  type);
end;

define method find-canonical-c-type
    (repository :: <c-type-repository>, type :: <c-enum-type>)
 => (canonical-type :: <c-enum-type>)
  add-to-table-if-missing(repository.c-enum-types,
			  type.c-type-tag,
			  type);
end;


//=========================================================================
//  Making C types.
//=========================================================================
//  We override make(...) to force all derived types to be created in a
//  repository.
//
//  If have performance problems, we should optimize this method, probably
//  starting with pointer types.

define inline method make
    (class :: subclass(<c-derived-type>),
     #next next-method, // XXX - do we want next-method here?
     #rest keys,
     #key repository,
     #all-keys)
 => (type :: <c-derived-type>)
  find-canonical-c-type(repository, next-method());
end method make;


//=========================================================================
//  Iterating over the types in a repository.
//=========================================================================
//  A function to help iterate over all the items in a repository. This is
//  modeled after 'do' and 'do-handlers'.

define function do-c-type-repository-entries
    (function :: <function>, repository :: <c-type-repository>)
 => ()
  for (item in repository.c-pointer-types)
    function(item);
  end;
  for (item in repository.c-typedef-types)
    function(item);
  end;
  for (item in repository.c-struct-types)
    function(item);
  end;
  for (item in repository.c-union-types)
    function(item);
  end;
  for (item in repository.c-enum-types)
    function(item);
  end;
  for (item in repository.c-other-types)
    function(item);
  end;
end;

//=========================================================================
//  Retreiving types by name
//=========================================================================
//
define method c-tagged-type
    (repository :: <c-type-repository>, name :: <string>)
 => (type :: false-or(<c-tagged-type>));
  element(repository.c-struct-types, name, default: #f)
    | element(repository.c-union-types, name, default: #f)
    | element(repository.c-enum-types, name, default: #f);
end method;

define method c-named-type
    (repository :: <c-type-repository>, name :: <string>)
 => (type :: false-or(<c-type>));
  element(repository.c-typedef-types, name, default: #f);
end method;
