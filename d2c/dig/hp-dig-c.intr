module: d2c-gnu

define interface
  #include "../../src/dig/hp-dig-support.h",
    equate: {"char *" => <c-string>},
    map: {"char *" => <byte-string>};
  function "fd_exec", output-argument: 2, output-argument: 3;
  pointer "char **", superclasses: {<c-vector>};
end interface;
    