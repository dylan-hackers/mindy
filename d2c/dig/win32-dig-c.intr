module: d2c-gnu

define interface
  #include "win32-dig-support.h",
    equate: {"char *" => <c-string>},
    map: {"char *" => <byte-string>};
  function "ignore_interrupts";
  pointer "char **", superclasses: {<c-vector>};
end interface;
