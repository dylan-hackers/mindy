Module:   dylan-user
Author:   Eric Kidd
Synopsis: An implementation of the low-level Harlequin C API, as faked on
          top of the low-level Gwydion API.

define library c-ffi
  use dylan;
  export
    c-ffi;
end library;

define module c-ffi
  use Dylan;
  use Extensions, import: { subclass };
  use Machine-words;
  use System, export: { size-of, alignment-of, referenced-type };
  export
    // Basic designator types & operations
    <C-value>, <C-void>;

/*
    // Numeric designator types. Is this enough float types for ANSI C?
    <C-number>, <C-float>, <C-double>, <C-float*>, <C-double*>,

    // Uninstantiable numeric type designators. Note that we have a
    // complete set, but that the table in Harlequin's documentation omits
    // some of the possible permutations.
    <C-int>, <C-raw-int>, <C-unsafe-int>,
    <C-signed-int>, <C-raw-signed-int>, <C-unsafe-signed-int>,
    <C-unsigned-int>, <C-raw-unsigned-int>, <C-unsafe-unsigned-int>,
    <C-long>, <C-raw-long>, <C-unsafe-long>,
    <C-signed-long>, <C-raw-signed-long>, <C-unsafe-signed-long>,
    <C-unsigned-long>, <C-raw-unsigned-long>, <C-unsafe-unsigned-long>,
    <C-short>, <C-raw-short>, <C-unsafe-short>,
    <C-signed-short>, <C-raw-signed-short>, <C-unsafe-signed-short>,
    <C-unsigned-short>, <C-raw-unsigned-short>, <C-unsafe-unsigned-short>,
    <C-char>, <C-raw-char>, <C-unsafe-char>,
    <C-signed-char>, <C-raw-signed-char>, <C-unsafe-signed-char>,
    <C-unsigned-char>, <C-raw-unsigned-char>, <C-unsafe-unsigned-char>,
  
    // Predefined pointer types for the above.
    <C-int*>, <C-raw-int*>, <C-unsafe-int*>,
    <C-signed-int*>, <C-raw-signed-int*>, <C-unsafe-signed-int*>,
    <C-unsigned-int*>, <C-raw-unsigned-int*>, <C-unsafe-unsigned-int*>,
    <C-long*>, <C-raw-long*>, <C-unsafe-long*>,
    <C-signed-long*>, <C-raw-signed-long*>, <C-unsafe-signed-long*>,
    <C-unsigned-long*>, <C-raw-unsigned-long*>, <C-unsafe-unsigned-long*>,
    <C-short*>, <C-raw-short*>, <C-unsafe-short*>,
    <C-signed-short*>, <C-raw-signed-short*>, <C-unsafe-signed-short*>,
    <C-unsigned-short*>, <C-raw-unsigned-short*>, <C-unsafe-unsigned-short*>,
    <C-char*>, <C-raw-char*>, <C-unsafe-char*>,
    <C-signed-char*>, <C-raw-signed-char*>, <C-unsafe-signed-char*>,
    <C-unsigned-char*>, <C-raw-unsigned-char*>, <C-unsafe-unsigned-char*>,
*/
    
    // Worry about Alpha 64-bit types here... We need to support such
    // things as long long.

  export
    // Pointer types
    <C-pointer>, pointer-address, pointer-cast, null-pointer,
    null-pointer?, <C-void*>, <C-statically-typed-pointer>,
    \C-pointer-type-definer, pointer-value,
    pointer-value-setter, pointer-value-address; 

/*
  export
    // Pointer accessors for integer types
    C-int-at, C-int-at-setter,
    C-signed-int-at, C-signed-int-at-setter,
    C-unsigned-int-at, C-unsigned-int-at-setter,
    C-long-at, C-long-at-setter,
    C-signed-long-at, C-signed-long-at-setter,
    C-unsigned-long-at, C-unsigned-long-at-setter,
    C-short-at, C-short-at-setter,
    C-signed-short-at, C-signed-short-at-setter,
    C-unsigned-short-at, C-unsigned-short-at-setter,
    C-char-at, C-char-at-setter,
    C-signed-char-at, C-signed-char-at-setter,
    C-unsigned-char-at, C-unsigned-char-at-setter,
    
    // Pointer accessors for other types
    C-float-at, C-float-at-setter,
    C-double-at, C-double-at-setter,
    C-pointer-at, C-pointer-at-setter,
*/
  export
    // Structs and unions
    <C-struct>, <C-union>,

    // Defining types
    \C-subtype-definer, \C-mapped-subtype-definer, \C-struct-definer,
    \C-union-definer, \C-function-definer, \C-callable-wrapper-definer,
    \C-variable-definer, \C-address-definer,

    // Allocating and deallocating storage.
    destroy, \with-stack-structure;

/*
    // Utility designator classes
    <C-boolean>, <C-string>, <C-character>, \with-c-string,
    clear-memory!, copy-bytes!, copy-into!, equal-memory?, <C-Dylan-object>,
    register-C-Dylan-object, unregister-C-Dylan-object,
    export-C-Dylan-object, import-C-Dylan-object;
*/
end module;
