module: d2c-gnu

#if (mindy)
define  class <anonymous-1> (<c-vector>, <statically-typed-pointer>) end;

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

define method content-size (value :: limited(<class>, subclass-of: <anonymous-1>)) => (result :: <integer>);
  4;
end method content-size;

define constant <arg-vector> = <anonymous-1>;

define constant anonymous-4 = find-c-pointer("process_id");
define sealed method process-id () => (result :: <integer>);
  signed-long-at(anonymous-4, offset: 0);
end method process-id;

define sealed method process-id-setter (value :: <integer>) => (result :: <integer>);
  signed-long-at(anonymous-4, offset: 0) := value;
  value;
end method process-id-setter;

define  class <anonymous-2> (<statically-typed-pointer>) end;

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

define method content-size (value :: limited(<class>, subclass-of: <anonymous-2>)) => (result :: <integer>);
  4;
end method content-size;

define constant anonymous-5 = find-c-function("mutant_fd_exec");
define method mutant-fd-exec
    (arg1 :: <byte-string>)
 => (arg2 :: <integer>, arg3 :: <integer>);
  let arg2-ptr = make(<anonymous-2>);
  let arg3-ptr = make(<anonymous-2>);
  anonymous-5(export-value(<c-string>, arg1), arg2-ptr, arg3-ptr);
  let arg2-value = pointer-value(arg2-ptr);
destroy(arg2-ptr);
  let arg3-value = pointer-value(arg3-ptr);
destroy(arg3-ptr);
  values(arg2-value, arg3-value);
end method mutant-fd-exec;

#else
c-include("/afs/cs/project/gwydion//dylan/src/d2c/dig/hp-dig-support.h");

define functional class <anonymous-1> (<c-vector>, <statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-1>));

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

define sealed domain make (singleton(<anonymous-2>));

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

define method mutant-fd-exec
    (arg1 :: <byte-string>)
 => (arg2 :: <integer>, arg3 :: <integer>);
  let arg2-ptr = make(<anonymous-2>);
  let arg3-ptr = make(<anonymous-2>);
  call-out("mutant_fd_exec", void:, ptr: (export-value(<c-string>, arg1)).raw-value, ptr: arg2-ptr.raw-value, ptr: arg3-ptr.raw-value);
  let arg2-value = pointer-value(arg2-ptr);
destroy(arg2-ptr);
  let arg3-value = pointer-value(arg3-ptr);
destroy(arg3-ptr);
  values(arg2-value, arg3-value);
end method mutant-fd-exec;

#endif
