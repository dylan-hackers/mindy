module: debugger

define interface
  #include "dlfcn.h",
    import: {"dlopen", "dlclose", "dlsym", 
             "RTLD_LAZY", "RTLD_NOW", "RTLD_GLOBAL"},
    equate: {"char*" => <c-string>};
//    map: {"char*" => <byte-string>};
  pointer "void*" => <dll-handle>,
    superclasses: {<machine-word>};
  function "dlsym", map-result: <raw-pointer>;
end interface;

define functional class <dll-handle> (<statically-typed-pointer>)
end class <dll-handle>;

