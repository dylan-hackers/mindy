module: garbage-collection

define interface
  #include "gc.h",
    import: all-recursive,
    name-mapper: c-to-dylan,
    exclude: {"pthread_create", "pthread_sigmask", 
              "pthread_join", "pthread_detach", signal},
    define: { "LINUX_THREADS" };
  function "GC_register_finalizer",
    equate-argument: {2 => <function-pointer>},
    map-argument: {2 => <function>},
    output-argument: 4,
    output-argument: 5;
  function "GC_debug_register_finalizer",
    equate-argument: {2 => <function-pointer>},
    map-argument: {2 => <function>},
    output-argument: 4,
    output-argument: 5;
  function "GC_pthread_create",
    equate-argument: {3 => <function-pointer>},
    map-argument: {3 => <function>},
    output-argument: 1;
end interface;

define method export-value(cls == <function-pointer>, value :: <function>) => (result :: <function-pointer>);
  make(<function-pointer>, 
       pointer: callback-entry(callback-method(obj :: <object>) => () value(obj) end)); 
end method export-value;

define method import-value(cls == <function>, value :: <function-pointer>) => (result :: <function>);
  error("Is this possible?");
end method import-value;
