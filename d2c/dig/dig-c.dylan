module: d2c-gnu

c-include("dig-support.h");

define functional class <anonymous-1> (<c-vector>, <statically-typed-pointer>) end;

define method pointer-value
    (ptr :: <anonymous-1>, #key index = 0)
 => (result :: <byte-string>);
  import-value(<byte-string>, pointer-at(ptr, offset: index * 4, class: <c-string>));
end method pointer-value;

define method pointer-value-setter
    (value :: <byte-string>, ptr :: <anonymous-1>, #key index = 0)
 => (result :: <byte-string>);
  pointer-at(ptr, offset: index * 4, class: <c-string>) := export-value(<c-string>, value);
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-1>)) => (result :: <integer>);
  4;
end method content-size;

define constant <arg-vector> = <anonymous-1>;

define sealed method process-id () => (result :: <integer>);
  c-variable-ref(int: "&process_id");
end method process-id;

define sealed method process-id-setter (value :: <integer>) => (result :: <integer>);
  c-variable-ref(int: "&process_id") := value;
  value;
end method process-id-setter;

define functional class <anonymous-2> (<statically-typed-pointer>) end;

define method pointer-value
    (ptr :: <anonymous-2>, #key index = 0)
 => (result :: <integer>);
  signed-long-at(ptr, offset: index * 4);
end method pointer-value;

define method pointer-value-setter
    (value :: <integer>, ptr :: <anonymous-2>, #key index = 0)
 => (result :: <integer>);
  signed-long-at(ptr, offset: index * 4) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-2>)) => (result :: <integer>);
  4;
end method content-size;

define method fd-exec
    (arg1 :: <byte-string>)
 => (arg2 :: <integer>, arg3 :: <integer>);
  let arg2-ptr = make(<anonymous-2>);
  let arg3-ptr = make(<anonymous-2>);
  call-out("fd_exec", void:, ptr: (export-value(<c-string>, arg1)).raw-value, ptr: arg2-ptr.raw-value, ptr: arg3-ptr.raw-value);
  let arg2-value = pointer-value(arg2-ptr);
destroy(arg2-ptr);
  let arg3-value = pointer-value(arg3-ptr);
destroy(arg3-ptr);
  values(arg2-value, arg3-value);
end method fd-exec;

