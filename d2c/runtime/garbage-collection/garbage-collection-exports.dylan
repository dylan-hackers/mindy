module: dylan-user

define library garbage-collection
  use dylan;
  use melange-support;

  export garbage-collection;
end library;

define module garbage-collection
  use dylan;
  use extensions;
  use melange-support;

  export GC-gc-no, 
    // GC-oom-fn, GC-oom-fn-setter,
    GC-quiet, GC-quiet-setter, 
    GC-dont-gc, GC-dont-gc-setter,
    GC-dont-expand, GC-dont-expand-setter, 
    GC-full-freq, GC-full-freq-setter,
    GC-non-gc-bytes, GC-non-gc-bytes-setter, 
    GC-free-space-divisor, GC-free-space-divisor-setter,
    GC-max-retries, GC-max-retries-setter,
    GC-stackbottom, GC-stackbottom-setter,
    GC-malloc, GC-malloc-atomic, GC-malloc-uncollectable,
    GC-malloc-stubborn, GC-malloc-atomic-uncollectable,
    GC-free, GC-change-stubborn, GC-end-stubborn-change,
    GC-base, GC-size, GC-realloc, GC-expand-hp,
    GC-set-max-heap-size, GC-exclude-static-roots,
    GC-add-roots, GC-register-displacement,
    GC-debug-register-displacement, GC-gcollect,
    // GC-stop-func, GC-stop-func-setter,
    GC-try-to-collect, GC-get-heap-size,
    GC-get-bytes-since-gc, GC-enable-incremental,
    GC-collect-a-little, GC-malloc-ignore-off-page,
    GC-malloc-atomic-ignore-off-page,
    // GC-register-finalizer,
    // GC-register-disappearing-link,
    // GC-general-register-disappearing-link,
    // GC-unregister-disappearing-link,
    // GC-invoke-finalizers,
    // GC-warn-proc, GC-warn-proc-setter, GC-set-warn-proc,
    // GC-pthread-join, GC-pthread-create, GC-pthread-sigmask;
end module;
