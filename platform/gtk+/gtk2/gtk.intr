module: gtk-internal

define interface
  #include "gtk-includes.h",
    name-mapper: c-to-dylan,
    import: all-recursive;
end interface;
