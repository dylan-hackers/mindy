module: common-extensions

define interface
  #include "prototypes.h",
    equate: {"char *" => <c-string>},
    map: {"char *" => <byte-string>};
end interface;
