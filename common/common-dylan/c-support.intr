module: c-support

define interface
  #include "prototypes.h",
    equate: {"char *" => <c-string>},
    map: {"char *" => <byte-string>};
  variable "application_argv" => application-argv-internal;
end interface;

define function application-argv
    (index :: <integer>)
 => (string :: <byte-string>)
  pointer-value(application-argv-internal(), index: index);
end function application-argv;
  