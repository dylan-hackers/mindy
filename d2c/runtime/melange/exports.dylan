module: dylan-user
RCS-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/melange/exports.dylan,v 1.4 1996/09/28 20:28:06 rgs Exp $

define library melange-support
  use dylan;
  export melange-support;
end library melange-support;

define module melange-support
  use dylan;
  use extensions, export: {subclass};
  use system, export: {call-out, c-include, c-decl, c-expr};

  export
    c-variable-ref, c-variable-ref-setter,
    <statically-typed-pointer>, raw-value, null-pointer,
    signed-byte-at, signed-byte-at-setter,
    unsigned-byte-at, unsigned-byte-at-setter, signed-short-at,
    signed-short-at-setter, unsigned-short-at, unsigned-short-at-setter,
    signed-long-at, signed-long-at-setter, unsigned-long-at,
    unsigned-long-at-setter, pointer-at, pointer-at-setter,

    destroy, pointer-value, pointer-value-setter, content-size,
    structure-size, export-value, import-value, <machine-pointer>,
    <c-string>, <c-vector>, <function-pointer>;
end module melange-support;
