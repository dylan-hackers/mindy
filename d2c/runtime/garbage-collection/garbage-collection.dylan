module: garbage-collection

c-include("gc/include/gc.h");

define constant <GC-PTR> = <machine-pointer>;

define constant <GC-word> = <integer>;

define constant <GC-signed-word> = <integer>;

define sealed method GC-gc-no () => (result :: <GC-word>);
  as(<GC-word>, c-variable-ref(long: "&GC_gc_no"));
end method GC-gc-no;

define sealed method GC-gc-no-setter (value :: <GC-word>) => (result :: <GC-word>);
  c-variable-ref(long: "&GC_gc_no") := value;
  value;
end method GC-gc-no-setter;

define constant <size-t> = <integer>;

define functional class <anonymous-0> (<function-pointer>) end;

define sealed method GC-oom-fn () => (result :: <anonymous-0>);
  as(<anonymous-0>, c-variable-ref(ptr: "&GC_oom_fn"));
end method GC-oom-fn;

define sealed method GC-oom-fn-setter (value :: <anonymous-0>) => (result :: <anonymous-0>);
  c-variable-ref(ptr: "&GC_oom_fn") := value;
  value;
end method GC-oom-fn-setter;

define sealed method GC-quiet () => (result :: <integer>);
  as(<integer>, c-variable-ref(int: "&GC_quiet"));
end method GC-quiet;

define sealed method GC-quiet-setter (value :: <integer>) => (result :: <integer>);
  c-variable-ref(int: "&GC_quiet") := value;
  value;
end method GC-quiet-setter;

define sealed method GC-dont-gc () => (result :: <integer>);
  as(<integer>, c-variable-ref(int: "&GC_dont_gc"));
end method GC-dont-gc;

define sealed method GC-dont-gc-setter (value :: <integer>) => (result :: <integer>);
  c-variable-ref(int: "&GC_dont_gc") := value;
  value;
end method GC-dont-gc-setter;

define sealed method GC-dont-expand () => (result :: <integer>);
  as(<integer>, c-variable-ref(int: "&GC_dont_expand"));
end method GC-dont-expand;

define sealed method GC-dont-expand-setter (value :: <integer>) => (result :: <integer>);
  c-variable-ref(int: "&GC_dont_expand") := value;
  value;
end method GC-dont-expand-setter;

define sealed method GC-full-freq () => (result :: <integer>);
  as(<integer>, c-variable-ref(int: "&GC_full_freq"));
end method GC-full-freq;

define sealed method GC-full-freq-setter (value :: <integer>) => (result :: <integer>);
  c-variable-ref(int: "&GC_full_freq") := value;
  value;
end method GC-full-freq-setter;

define sealed method GC-non-gc-bytes () => (result :: <GC-word>);
  as(<GC-word>, c-variable-ref(long: "&GC_non_gc_bytes"));
end method GC-non-gc-bytes;

define sealed method GC-non-gc-bytes-setter (value :: <GC-word>) => (result :: <GC-word>);
  c-variable-ref(long: "&GC_non_gc_bytes") := value;
  value;
end method GC-non-gc-bytes-setter;

define sealed method GC-free-space-divisor () => (result :: <GC-word>);
  as(<GC-word>, c-variable-ref(long: "&GC_free_space_divisor"));
end method GC-free-space-divisor;

define sealed method GC-free-space-divisor-setter (value :: <GC-word>) => (result :: <GC-word>);
  c-variable-ref(long: "&GC_free_space_divisor") := value;
  value;
end method GC-free-space-divisor-setter;

define sealed method GC-max-retries () => (result :: <GC-word>);
  as(<GC-word>, c-variable-ref(long: "&GC_max_retries"));
end method GC-max-retries;

define sealed method GC-max-retries-setter (value :: <GC-word>) => (result :: <GC-word>);
  c-variable-ref(long: "&GC_max_retries") := value;
  value;
end method GC-max-retries-setter;

define functional class <anonymous-1> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-1>));

define method pointer-value
    (ptr :: <anonymous-1>, #key index = 0)
 => (result :: <integer>);
  signed-byte-at(ptr, offset: index * 1);
end method pointer-value;

define method pointer-value-setter
    (value :: <integer>, ptr :: <anonymous-1>, #key index = 0)
 => (result :: <integer>);
  signed-byte-at(ptr, offset: index * 1) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-1>)) => (result :: <integer>);
  1;
end method content-size;

define sealed method GC-stackbottom () => (result :: <anonymous-1>);
  as(<anonymous-1>, c-variable-ref(ptr: "&GC_stackbottom"));
end method GC-stackbottom;

define sealed method GC-stackbottom-setter (value :: <anonymous-1>) => (result :: <anonymous-1>);
  c-variable-ref(ptr: "&GC_stackbottom") := value;
  value;
end method GC-stackbottom-setter;

define method GC-malloc
    (arg1 :: <size-t>)
 => (result :: <GC-PTR>);
  let result-value
    = call-out("GC_malloc", ptr:, long: arg1);
  let result-value = make(<GC-PTR>, pointer: result-value);
  values(result-value);
end method GC-malloc;

define method GC-malloc-atomic
    (arg1 :: <size-t>)
 => (result :: <GC-PTR>);
  let result-value
    = call-out("GC_malloc_atomic", ptr:, long: arg1);
  let result-value = make(<GC-PTR>, pointer: result-value);
  values(result-value);
end method GC-malloc-atomic;

define method GC-malloc-uncollectable
    (arg1 :: <size-t>)
 => (result :: <GC-PTR>);
  let result-value
    = call-out("GC_malloc_uncollectable", ptr:, long: arg1);
  let result-value = make(<GC-PTR>, pointer: result-value);
  values(result-value);
end method GC-malloc-uncollectable;

define method GC-malloc-stubborn
    (arg1 :: <size-t>)
 => (result :: <GC-PTR>);
  let result-value
    = call-out("GC_malloc_stubborn", ptr:, long: arg1);
  let result-value = make(<GC-PTR>, pointer: result-value);
  values(result-value);
end method GC-malloc-stubborn;

define method GC-malloc-atomic-uncollectable
    (arg1 :: <size-t>)
 => (result :: <GC-PTR>);
  let result-value
    = call-out("GC_malloc_atomic_uncollectable", ptr:, long: arg1);
  let result-value = make(<GC-PTR>, pointer: result-value);
  values(result-value);
end method GC-malloc-atomic-uncollectable;

define method GC-free
    (arg1 :: <GC-PTR>)
 => ();
  call-out("GC_free", void:, ptr: (arg1).raw-value);
  values();
end method GC-free;

define method GC-change-stubborn
    (arg1 :: <GC-PTR>)
 => ();
  call-out("GC_change_stubborn", void:, ptr: (arg1).raw-value);
  values();
end method GC-change-stubborn;

define method GC-end-stubborn-change
    (arg1 :: <GC-PTR>)
 => ();
  call-out("GC_end_stubborn_change", void:, ptr: (arg1).raw-value);
  values();
end method GC-end-stubborn-change;

define method GC-base
    (arg1 :: <GC-PTR>)
 => (result :: <GC-PTR>);
  let result-value
    = call-out("GC_base", ptr:, ptr: (arg1).raw-value);
  let result-value = make(<GC-PTR>, pointer: result-value);
  values(result-value);
end method GC-base;

define method GC-size
    (arg1 :: <GC-PTR>)
 => (result :: <size-t>);
  let result-value
    = call-out("GC_size", long:, ptr: (arg1).raw-value);
  values(result-value);
end method GC-size;

define method GC-realloc
    (arg1 :: <GC-PTR>, arg2 :: <size-t>)
 => (result :: <GC-PTR>);
  let result-value
    = call-out("GC_realloc", ptr:, ptr: (arg1).raw-value, long: arg2);
  let result-value = make(<GC-PTR>, pointer: result-value);
  values(result-value);
end method GC-realloc;

define method GC-expand-hp
    (arg1 :: <size-t>)
 => (result :: <integer>);
  let result-value
    = call-out("GC_expand_hp", int:, long: arg1);
  values(result-value);
end method GC-expand-hp;

define method GC-set-max-heap-size
    (arg1 :: <GC-word>)
 => ();
  call-out("GC_set_max_heap_size", void:, long: arg1);
  values();
end method GC-set-max-heap-size;

define method GC-exclude-static-roots
    (arg1 :: <GC-PTR>, arg2 :: <GC-PTR>)
 => ();
  call-out("GC_exclude_static_roots", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values();
end method GC-exclude-static-roots;

define method GC-clear-roots
    ()
 => ();
  call-out("GC_clear_roots", void:);
  values();
end method GC-clear-roots;

define method GC-add-roots
    (arg1 :: <anonymous-1>, arg2 :: <anonymous-1>)
 => ();
  call-out("GC_add_roots", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values();
end method GC-add-roots;

define method GC-register-displacement
    (arg1 :: <GC-word>)
 => ();
  call-out("GC_register_displacement", void:, long: arg1);
  values();
end method GC-register-displacement;

define method GC-debug-register-displacement
    (arg1 :: <GC-word>)
 => ();
  call-out("GC_debug_register_displacement", void:, long: arg1);
  values();
end method GC-debug-register-displacement;

define method GC-gcollect
    ()
 => ();
  call-out("GC_gcollect", void:);
  values();
end method GC-gcollect;

define functional class <anonymous-21> (<function-pointer>) end;

define constant <GC-stop-func> = <anonymous-21>;

define method GC-try-to-collect
    (arg1 :: <GC-stop-func>)
 => (result :: <integer>);
  let result-value
    = call-out("GC_try_to_collect", int:, ptr: (arg1).raw-value);
  values(result-value);
end method GC-try-to-collect;

define method GC-get-heap-size
    ()
 => (result :: <size-t>);
  let result-value
    = call-out("GC_get_heap_size", long:);
  values(result-value);
end method GC-get-heap-size;

define method GC-get-bytes-since-gc
    ()
 => (result :: <size-t>);
  let result-value
    = call-out("GC_get_bytes_since_gc", long:);
  values(result-value);
end method GC-get-bytes-since-gc;

define method GC-enable-incremental
    ()
 => ();
  call-out("GC_enable_incremental", void:);
  values();
end method GC-enable-incremental;

define method GC-collect-a-little
    ()
 => (result :: <integer>);
  let result-value
    = call-out("GC_collect_a_little", int:);
  values(result-value);
end method GC-collect-a-little;

define method GC-malloc-ignore-off-page
    (arg1 :: <size-t>)
 => (result :: <GC-PTR>);
  let result-value
    = call-out("GC_malloc_ignore_off_page", ptr:, long: arg1);
  let result-value = make(<GC-PTR>, pointer: result-value);
  values(result-value);
end method GC-malloc-ignore-off-page;

define method GC-malloc-atomic-ignore-off-page
    (arg1 :: <size-t>)
 => (result :: <GC-PTR>);
  let result-value
    = call-out("GC_malloc_atomic_ignore_off_page", ptr:, long: arg1);
  let result-value = make(<GC-PTR>, pointer: result-value);
  values(result-value);
end method GC-malloc-atomic-ignore-off-page;

define method GC-debug-malloc
    (arg1 :: <size-t>, arg2 :: <anonymous-1>, arg3 :: <integer>)
 => (result :: <GC-PTR>);
  let result-value
    = call-out("GC_debug_malloc", ptr:, long: arg1, ptr: (arg2).raw-value, int: arg3);
  let result-value = make(<GC-PTR>, pointer: result-value);
  values(result-value);
end method GC-debug-malloc;

define method GC-debug-malloc-atomic
    (arg1 :: <size-t>, arg2 :: <anonymous-1>, arg3 :: <integer>)
 => (result :: <GC-PTR>);
  let result-value
    = call-out("GC_debug_malloc_atomic", ptr:, long: arg1, ptr: (arg2).raw-value, int: arg3);
  let result-value = make(<GC-PTR>, pointer: result-value);
  values(result-value);
end method GC-debug-malloc-atomic;

define method GC-debug-malloc-uncollectable
    (arg1 :: <size-t>, arg2 :: <anonymous-1>, arg3 :: <integer>)
 => (result :: <GC-PTR>);
  let result-value
    = call-out("GC_debug_malloc_uncollectable", ptr:, long: arg1, ptr: (arg2).raw-value, int: arg3);
  let result-value = make(<GC-PTR>, pointer: result-value);
  values(result-value);
end method GC-debug-malloc-uncollectable;

define method GC-debug-malloc-stubborn
    (arg1 :: <size-t>, arg2 :: <anonymous-1>, arg3 :: <integer>)
 => (result :: <GC-PTR>);
  let result-value
    = call-out("GC_debug_malloc_stubborn", ptr:, long: arg1, ptr: (arg2).raw-value, int: arg3);
  let result-value = make(<GC-PTR>, pointer: result-value);
  values(result-value);
end method GC-debug-malloc-stubborn;

define method GC-debug-free
    (arg1 :: <GC-PTR>)
 => ();
  call-out("GC_debug_free", void:, ptr: (arg1).raw-value);
  values();
end method GC-debug-free;

define method GC-debug-realloc
    (arg1 :: <GC-PTR>, arg2 :: <size-t>, arg3 :: <anonymous-1>, arg4 :: <integer>)
 => (result :: <GC-PTR>);
  let result-value
    = call-out("GC_debug_realloc", ptr:, ptr: (arg1).raw-value, long: arg2, ptr: (arg3).raw-value, int: arg4);
  let result-value = make(<GC-PTR>, pointer: result-value);
  values(result-value);
end method GC-debug-realloc;

define method GC-debug-change-stubborn
    (arg1 :: <GC-PTR>)
 => ();
  call-out("GC_debug_change_stubborn", void:, ptr: (arg1).raw-value);
  values();
end method GC-debug-change-stubborn;

define method GC-debug-end-stubborn-change
    (arg1 :: <GC-PTR>)
 => ();
  call-out("GC_debug_end_stubborn_change", void:, ptr: (arg1).raw-value);
  values();
end method GC-debug-end-stubborn-change;

define functional class <anonymous-37> (<function-pointer>) end;

define constant <GC-finalization-proc> = <anonymous-37>;

define functional class <anonymous-38> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-38>));

define method pointer-value
    (ptr :: <anonymous-38>, #key index = 0)
 => (result :: <GC-finalization-proc>);
  pointer-at(ptr, offset: index * 4, class: <GC-finalization-proc>);
end method pointer-value;

define method pointer-value-setter
    (value :: <GC-finalization-proc>, ptr :: <anonymous-38>, #key index = 0)
 => (result :: <GC-finalization-proc>);
  pointer-at(ptr, offset: index * 4, class: <GC-finalization-proc>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-38>)) => (result :: <integer>);
  4;
end method content-size;

define functional class <anonymous-39> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-39>));

define method pointer-value
    (ptr :: <anonymous-39>, #key index = 0)
 => (result :: <GC-PTR>);
  pointer-at(ptr, offset: index * 4, class: <GC-PTR>);
end method pointer-value;

define method pointer-value-setter
    (value :: <GC-PTR>, ptr :: <anonymous-39>, #key index = 0)
 => (result :: <GC-PTR>);
  pointer-at(ptr, offset: index * 4, class: <GC-PTR>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-39>)) => (result :: <integer>);
  4;
end method content-size;

define method GC-register-finalizer
    (arg1 :: <GC-PTR>, arg2 :: <GC-finalization-proc>, arg3 :: <GC-PTR>, arg4 :: <anonymous-38>, arg5 :: <anonymous-39>)
 => ();
  call-out("GC_register_finalizer", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, ptr: (arg4).raw-value, ptr: (arg5).raw-value);
  values();
end method GC-register-finalizer;

define method GC-debug-register-finalizer
    (arg1 :: <GC-PTR>, arg2 :: <GC-finalization-proc>, arg3 :: <GC-PTR>, arg4 :: <anonymous-38>, arg5 :: <anonymous-39>)
 => ();
  call-out("GC_debug_register_finalizer", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, ptr: (arg4).raw-value, ptr: (arg5).raw-value);
  values();
end method GC-debug-register-finalizer;

define method GC-register-finalizer-ignore-self
    (arg1 :: <GC-PTR>, arg2 :: <GC-finalization-proc>, arg3 :: <GC-PTR>, arg4 :: <anonymous-38>, arg5 :: <anonymous-39>)
 => ();
  call-out("GC_register_finalizer_ignore_self", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, ptr: (arg4).raw-value, ptr: (arg5).raw-value);
  values();
end method GC-register-finalizer-ignore-self;

define method GC-debug-register-finalizer-ignore-self
    (arg1 :: <GC-PTR>, arg2 :: <GC-finalization-proc>, arg3 :: <GC-PTR>, arg4 :: <anonymous-38>, arg5 :: <anonymous-39>)
 => ();
  call-out("GC_debug_register_finalizer_ignore_self", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, ptr: (arg4).raw-value, ptr: (arg5).raw-value);
  values();
end method GC-debug-register-finalizer-ignore-self;

define method GC-register-disappearing-link
    (arg1 :: <anonymous-39>)
 => (result :: <integer>);
  let result-value
    = call-out("GC_register_disappearing_link", int:, ptr: (arg1).raw-value);
  values(result-value);
end method GC-register-disappearing-link;

define method GC-general-register-disappearing-link
    (arg1 :: <anonymous-39>, arg2 :: <GC-PTR>)
 => (result :: <integer>);
  let result-value
    = call-out("GC_general_register_disappearing_link", int:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values(result-value);
end method GC-general-register-disappearing-link;

define method GC-unregister-disappearing-link
    (arg1 :: <anonymous-39>)
 => (result :: <integer>);
  let result-value
    = call-out("GC_unregister_disappearing_link", int:, ptr: (arg1).raw-value);
  values(result-value);
end method GC-unregister-disappearing-link;

define method GC-make-closure
    (arg1 :: <GC-finalization-proc>, arg2 :: <GC-PTR>)
 => (result :: <GC-PTR>);
  let result-value
    = call-out("GC_make_closure", ptr:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  let result-value = make(<GC-PTR>, pointer: result-value);
  values(result-value);
end method GC-make-closure;

define method GC-debug-invoke-finalizer
    (arg1 :: <GC-PTR>, arg2 :: <GC-PTR>)
 => ();
  call-out("GC_debug_invoke_finalizer", void:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values();
end method GC-debug-invoke-finalizer;

define method GC-invoke-finalizers
    ()
 => (result :: <integer>);
  let result-value
    = call-out("GC_invoke_finalizers", int:);
  values(result-value);
end method GC-invoke-finalizers;

define functional class <anonymous-50> (<function-pointer>) end;

define constant <GC-warn-proc> = <anonymous-50>;

define method GC-set-warn-proc
    (arg1 :: <GC-warn-proc>)
 => (result :: <GC-warn-proc>);
  let result-value
    = call-out("GC_set_warn_proc", ptr:, ptr: (arg1).raw-value);
  let result-value = make(<GC-warn-proc>, pointer: result-value);
  values(result-value);
end method GC-set-warn-proc;

define functional class <anonymous-52> (<function-pointer>) end;

define constant <GC-fn-type> = <anonymous-52>;

define method GC-call-with-alloc-lock
    (arg1 :: <GC-fn-type>, arg2 :: <GC-PTR>)
 => (result :: <GC-PTR>);
  let result-value
    = call-out("GC_call_with_alloc_lock", ptr:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  let result-value = make(<GC-PTR>, pointer: result-value);
  values(result-value);
end method GC-call-with-alloc-lock;

define method GC-same-obj
    (arg1 :: <GC-PTR>, arg2 :: <GC-PTR>)
 => (result :: <GC-PTR>);
  let result-value
    = call-out("GC_same_obj", ptr:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  let result-value = make(<GC-PTR>, pointer: result-value);
  values(result-value);
end method GC-same-obj;

define method GC-pre-incr
    (arg1 :: <anonymous-39>, arg2 :: <size-t>)
 => (result :: <GC-PTR>);
  let result-value
    = call-out("GC_pre_incr", ptr:, ptr: (arg1).raw-value, long: arg2);
  let result-value = make(<GC-PTR>, pointer: result-value);
  values(result-value);
end method GC-pre-incr;

define method GC-post-incr
    (arg1 :: <anonymous-39>, arg2 :: <size-t>)
 => (result :: <GC-PTR>);
  let result-value
    = call-out("GC_post_incr", ptr:, ptr: (arg1).raw-value, long: arg2);
  let result-value = make(<GC-PTR>, pointer: result-value);
  values(result-value);
end method GC-post-incr;

define method GC-is-visible
    (arg1 :: <GC-PTR>)
 => (result :: <GC-PTR>);
  let result-value
    = call-out("GC_is_visible", ptr:, ptr: (arg1).raw-value);
  let result-value = make(<GC-PTR>, pointer: result-value);
  values(result-value);
end method GC-is-visible;

define method GC-is-valid-displacement
    (arg1 :: <GC-PTR>)
 => (result :: <GC-PTR>);
  let result-value
    = call-out("GC_is_valid_displacement", ptr:, ptr: (arg1).raw-value);
  let result-value = make(<GC-PTR>, pointer: result-value);
  values(result-value);
end method GC-is-valid-displacement;

define functional class <anonymous-59> (<function-pointer>) end;

define sealed method GC-same-obj-print-proc () => (result :: <anonymous-59>);
  as(<anonymous-59>, c-variable-ref(ptr: "&GC_same_obj_print_proc"));
end method GC-same-obj-print-proc;

define sealed method GC-same-obj-print-proc-setter (value :: <anonymous-59>) => (result :: <anonymous-59>);
  c-variable-ref(ptr: "&GC_same_obj_print_proc") := value;
  value;
end method GC-same-obj-print-proc-setter;

define functional class <anonymous-60> (<function-pointer>) end;

define sealed method GC-is-valid-displacement-print-proc () => (result :: <anonymous-60>);
  as(<anonymous-60>, c-variable-ref(ptr: "&GC_is_valid_displacement_print_proc"));
end method GC-is-valid-displacement-print-proc;

define sealed method GC-is-valid-displacement-print-proc-setter (value :: <anonymous-60>) => (result :: <anonymous-60>);
  c-variable-ref(ptr: "&GC_is_valid_displacement_print_proc") := value;
  value;
end method GC-is-valid-displacement-print-proc-setter;

define functional class <anonymous-61> (<function-pointer>) end;

define sealed method GC-is-visible-print-proc () => (result :: <anonymous-61>);
  as(<anonymous-61>, c-variable-ref(ptr: "&GC_is_visible_print_proc"));
end method GC-is-visible-print-proc;

define sealed method GC-is-visible-print-proc-setter (value :: <anonymous-61>) => (result :: <anonymous-61>);
  c-variable-ref(ptr: "&GC_is_visible_print_proc") := value;
  value;
end method GC-is-visible-print-proc-setter;

