module: c-support

define interface
  #include "support.h",
    exclude: {"struct dylan_common_timeval *"},
    equate: {"char *" => <c-string>},
    map: {"char *" => <byte-string>};

  struct "struct dylan_common_timeval" => <profiling-timeval>,
    read-only: #t,
    rename: {"tv_sec" => profiling-seconds-value,
             "tv_usec" => profiling-microseconds-value};

  variable "application_argc",
    read-only: #t;

  function "dylan_common_application_arg" => application-argv;

  function "dylan_common_profiling_cpu_time" => cpu-time;
end interface;

