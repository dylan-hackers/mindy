module: dylan-user

define library melange-support
  use dylan;
  export melange-support;
end library melange-support;

define module melange-support
  use dylan;
  use extensions;
  use system, export: {call-out, c-include, c-decl, c-expr};

  export
    <statically-typed-pointer>, raw-value, null-pointer,
    signed-byte-at, signed-byte-at-setter,
    unsigned-byte-at, unsigned-byte-at-setter, signed-short-at,
    signed-short-at-setter, unsigned-short-at, unsigned-short-at-setter,
    signed-long-at, signed-long-at-setter, unsigned-long-at,
    unsigned-long-at-setter, pointer-at, pointer-at-setter,

    destroy, pointer-value, pointer-value-setter, content-size,
    structure-size, export-value, import-value, <machine-pointer>,
    <c-string>, <c-vector>;
end module melange-support;
