module: c-support

#if (mindy)
define constant anonymous-2 = find-c-pointer("application_argc");
define sealed method application-argc () => (result :: <integer>);
  signed-long-at(anonymous-2, offset: 0);
end method application-argc;

define sealed method application-argc-setter (value :: <integer>) => (result :: <integer>);
  signed-long-at(anonymous-2, offset: 0) := value;
  value;
end method application-argc-setter;

define  class <anonymous-1> (<statically-typed-pointer>) end;

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

define constant anonymous-3 = find-c-pointer("application_argv");
define sealed method application-argv-internal () => (result :: <anonymous-1>);
  pointer-at(anonymous-3, offset: 0, class: <anonymous-1>);
end method application-argv-internal;

define sealed method application-argv-internal-setter (value :: <anonymous-1>) => (result :: <anonymous-1>);
  pointer-at(anonymous-3, offset: 0, class: <anonymous-1>) := value;
  value;
end method application-argv-internal-setter;

#else
c-include("./prototypes.h");

define sealed method application-argc () => (result :: <integer>);
  as(<integer>, c-variable-ref(int: "&application_argc"));
end method application-argc;

define sealed method application-argc-setter (value :: <integer>) => (result :: <integer>);
  c-variable-ref(int: "&application_argc") := value;
  value;
end method application-argc-setter;

define functional class <anonymous-1> (<statically-typed-pointer>) end;

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

define sealed method application-argv-internal () => (result :: <anonymous-1>);
  as(<anonymous-1>, c-variable-ref(ptr: "&application_argv"));
end method application-argv-internal;

define sealed method application-argv-internal-setter (value :: <anonymous-1>) => (result :: <anonymous-1>);
  c-variable-ref(ptr: "&application_argv") := value;
  value;
end method application-argv-internal-setter;

#endif
define function application-argv
    (index :: <integer>)
 => (string :: <byte-string>)
  pointer-value(application-argv-internal(), index: index);
end function application-argv;
  