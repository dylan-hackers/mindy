module: c-support

c-include(".//prototypes.h");

define sealed method application-argc () => (result :: <integer>);
  as(<integer>, c-variable-ref(int: "&application_argc"));
end method application-argc;

define sealed method application-argc-setter (value :: <integer>) => (result :: <integer>);
  c-variable-ref(int: "&application_argc") := value;
  value;
end method application-argc-setter;

define functional class <char**> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<char**>));

define inline method pointer-value
    (ptr :: <char**>, #key index = 0)
 => (result :: <byte-string>);
  import-value(<byte-string>, pointer-at(ptr, offset: index * 4, class: <c-string>));
end method pointer-value;

define inline method pointer-value-setter
    (value :: <byte-string>, ptr :: <char**>, #key index = 0)
 => (result :: <byte-string>);
  pointer-at(ptr, offset: index * 4, class: <c-string>) := export-value(<c-string>, value);
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<char**>)) => (result :: <integer>);
  4;
end method content-size;

define sealed method application-argv-internal () => (result :: <char**>);
  as(<char**>, c-variable-ref(ptr: "&application_argv"));
end method application-argv-internal;

define sealed method application-argv-internal-setter (value :: <char**>) => (result :: <char**>);
  c-variable-ref(ptr: "&application_argv") := value;
  value;
end method application-argv-internal-setter;

define functional class <long*> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<long*>));

define inline method pointer-value
    (ptr :: <long*>, #key index = 0)
 => (result :: <integer>);
  signed-long-at(ptr, offset: index * 4);
end method pointer-value;

define inline method pointer-value-setter
    (value :: <integer>, ptr :: <long*>, #key index = 0)
 => (result :: <integer>);
  signed-long-at(ptr, offset: index * 4) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<long*>)) => (result :: <integer>);
  4;
end method content-size;

define method cpu-time
    ()
 => (result :: <long*>);
  let result-value
    = call-out("cpu_time", ptr:);
  let result-value = make(<long*>, pointer: result-value);
  values(result-value);
end method cpu-time;

define function application-argv
    (index :: <integer>)
 => (string :: <byte-string>)
  pointer-value(application-argv-internal(), index: index);
end function application-argv;
  