Module: dylan-user

define library ansi-c
  use dylan;
  use format;

  export
    c-types;
end library;

define module c-types
  use dylan;
  use extensions;
  use format,
    import: {format-to-string};

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
      c-type-members,
    <c-struct-type>,
    <c-union-type>,
    <c-enum-type>,
      c-enum-members,
    <c-pointer-valued-type>,
      c-pointer-referent-type,
    <c-pointer-type>,
    <c-array-type>,
      c-array-size,
    <c-function-type>,
      c-function-return-type,
    <c-typedef-type>,
      c-typedef-name,
      c-typedef-type;

  export
    format-c-type-declarator;

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
end module;
