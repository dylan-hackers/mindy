Module: dylan-user

define library ansi-c
  use dylan;
  use format;
  use parser-utilities;
  use table-extensions;

  export
    c-types,
    c-declarations,
    ansi-c;
end library;

define module outside-dependencies
  use dylan, export: all;
  use extensions, export: all;
  use table-extensions, export: all;
  use format,
    import: {format-to-string},
    export: all;
end;

define module c-types
  use outside-dependencies;

  export
    <c-type>,
      c-type-complete?,
      c-type-name,
    <c-primitive-type>,
      c-primitive-type-name,
    <c-derived-type>,
    <c-void-type>,
    <c-numeric-type>,
    <c-integer-type>,
      c-integer-type-sign-specifier,
    <c-char-type>,
    <c-short-type>,
    <c-int-type>,
    <c-long-type>,
    <c-long-long-type>,
    <c-floating-point-type>,
    <c-tagged-type>,
      c-type-tag,
      c-type-anonymous-tag,
    <c-struct-or-union-type>,
      c-type-members, c-type-members-setter,
    <c-struct-type>,
    <c-union-type>,
    <c-enum-type>,
      c-enum-members,
    <c-pointer-valued-type>,
      c-pointer-referent-type,
    <c-pointer-type>,
    <c-array-type>,
      c-array-length,
    <c-function-type>,
      c-function-return-type,
      c-function-parameter-types,
      c-function-explicit-varargs?,
      c-function-explicit-void?,
    <c-typedef-type>,
      c-typedef-name,
      c-typedef-type;

  export
    <c-struct-member>,
    <c-member-variable>,
      c-member-variable-name,
      c-member-variable-type,
    <c-bit-field>,
      c-bit-field-name,
      c-bit-field-sign-specifier,
      c-bit-field-width,
    <c-enum-constant>,
      c-enum-constant-name,
      c-enum-constant-value;

  export
    format-c-type,
    format-c-tagged-type;

  export  
    <c-sign-specifier>,
    $c-void-type,
    $c-char-type,
    $c-signed-char-type,
    $c-unsigned-char-type,
    $c-short-type,
    $c-signed-short-type,
    $c-unsigned-short-type,
    $c-int-type,
    $c-signed-int-type,
    $c-unsigned-int-type,
    $c-long-type,
    $c-signed-long-type,
    $c-unsigned-long-type,
    $c-long-long-type,
    $c-signed-long-long-type,
    $c-unsigned-long-long-type,
    $c-float-type,
    $c-double-type,
    $c-long-double-type;

  // From c-type-repository.dylan
  export
    <c-type-repository>,
    do-c-type-repository-entries;
end module;

define module c-declarations
  use outside-dependencies;
  use source-locations, import: {<source-location>};
  use c-types;

  export
    <c-declaration>,
      c-declaration-name,
    <c-tagged-type-declaration>,
      c-tagged-type-declaration-type,
    <c-typedef-declaration>,
      c-typedef-declaration-type,
    <c-variable-declaration>,
      c-variable-name,
      c-variable-type,
      c-variable-extern?,
    <c-define>,
      c-define-name,
    <c-empty-define>,
    <c-integer-define>,
      c-integer-define-value,
    <c-string-define>,
      c-string-define-value,
    <c-type-alias-define>,
      c-type-alias-define-type,
    <c-unknown-define>;

  export
    format-c-declaration;
end module c-declarations;

define module c-file
  use outside-dependencies;
  use c-declarations; 
  
  export
    <c-file>,
      c-file-name,
      c-file-contents,
      add-c-declaration!,
      add-c-file!;
end module;

define module ansi-c
  use c-types, export: all;
  use c-declarations, export: all;
  use c-file, export: all;
end;
