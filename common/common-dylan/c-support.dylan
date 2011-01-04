module: c-support

c-include(".//support.h");

define sealed method application-argc () => (result :: <integer>);
  as(<integer>, c-variable(int: "&application_argc"));
end method application-argc;

define function application-argv
    (arg1 :: <integer>)
 => (result :: <byte-string>);
  let result-value
    = call-out("dylan_common_application_arg", ptr:, int:, arg1);
  let result-value = make(<c-string>, pointer: result-value);
  values(import-value(<byte-string>, result-value));
end function application-argv;

define functional class <profiling-timeval> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<profiling-timeval>));

define sealed inline method profiling-seconds-value
    (ptr :: <profiling-timeval>) => (result :: <integer>);
  signed-long-at(ptr, offset: 0);
end method profiling-seconds-value;

define sealed inline method profiling-microseconds-value
    (ptr :: <profiling-timeval>) => (result :: <integer>);
  signed-long-at(ptr, offset: 4);
end method profiling-microseconds-value;

define method pointer-value (value :: <profiling-timeval>, #key index = 0) => (result :: <profiling-timeval>);
  value + index * 8;
end method pointer-value;

define method content-size (value :: subclass(<profiling-timeval>)) => (result :: <integer>);
  8;
end method content-size;

define function cpu-time
    ()
 => (result :: <profiling-timeval>);
  let result-value
    = call-out("dylan_common_profiling_cpu_time", ptr:);
  let result-value = make(<profiling-timeval>, pointer: result-value);
  values(result-value);
end function cpu-time;

