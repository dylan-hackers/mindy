module: dylan-user
rcs-header: $Header: /home/housel/work/rcs/gd/src/mindy/libraries/dylan/exports.dylan,v 1.60 1995/02/14 02:39:51 rgs Exp $

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
// All rights reserved.
// 
// Use and copying of this software and preparation of derivative
// works based on this software are permitted, including commercial
// use, provided that the following conditions are observed:
// 
// 1. This copyright notice must be retained in full on any copies
//    and on appropriate parts of any derivative works.
// 2. Documentation (paper or online) accompanying any system that
//    incorporates this software, or any part of it, must acknowledge
//    the contribution of the Gwydion Project at Carnegie Mellon
//    University.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports, questions, comments, and suggestions should be sent by
// E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
//
//======================================================================
//
//  This file contains the library and module definitions for the
// Dylan library.
//

define library Dylan
  export
    Dylan, Extensions, System, File-Descriptors, Threads, Introspection,
    Cheap-IO, Extern;
end Dylan;

define module Builtin-Stuff
  export
    \*, \+, \-, \/, \<, \<=, \=, \==, \~=,
    $maximum-fixed-integer, $minimum-fixed-integer,
    <array>,
    <boolean>, <buffer>, <byte-character>, <byte-string>, <byte-vector>,
    <character>, <class>, <collection>, <complex>,
    <defined-class>, <double-float>,
    <empty-list>, <event>, <explicit-key-collection>, <extended-float>,
    <extended-integer>,
    <false>, <fixed-integer>, <float>, <function>,
    <generic-function>,
    <integer>,
    <limited-integer>, <list>, <lock>,
    <method>, <mutable-collection>, <mutable-explicit-key-collection>,
    <mutable-sequence>,
    <number>,
    <object>,
    <pair>,
    <ratio>, <rational>, <real>,
    <sequence>, <simple-object-vector>, <single-float>, <singleton>,
    <slot-descriptor>, <spinlock>, <string>, <subclass>, <symbol>,
    <thread>, <true>, <type>,
    <unicode-string>, <union>,
    <vector>,
    <weak-pointer>,
    $permanent-hash-state, *print-GC-messages*,
    add-method, all-superclasses, applicable-method?, apply,
    apply-curry, as, ash,
    binary-logand, binary-logior, binary-logxor, broadcast-event,
    direct-subclasses, direct-superclasses,
    ceiling, ceiling/, class-name, copy-bytes, current-handler, current-thread,
    denominator, do-next-method,
    element, element-setter, enable-error-system,
    fflush, find-method, float-hash, floor, floor/, format, function-arguments,
    function-name,
    generic-function-mandatory-keywords, generic-function-methods,
    getc, grab-lock,
    handler-function, handler-init-args, handler-next, handler-test,
    handler-type, head, head-setter,
    initialize, instance?, invoke-debugger,
    kill-thread,
    limited, limited-integer-class, limited-integer-min, limited-integer-max,
    list, load, locked?, logand, logbit?, logior, lognot, logxor,
    main, make, make-generic-function, merge-hash-codes, method-specializers,
    negative, numerator,
    object-address, object-class, object-hash,
    pair, prin1, print, putc, puts,
    raw-exit, release-lock, remove-method, round, round/,
    signal-event, singleton, singleton-object, size, slot-allocation,
    slot-descriptors, slot-getter, slot-getter-method, slot-initialized?,
    slot-name, slot-setter, slot-setter-method, slot-type, slot-value,
    slot-value-setter, sorted-applicable-methods, spawn-thread, state-valid?,
    subclass-of, subtype?,
    tail, tail-setter, truncate, truncate/,
    union, union-members,
    values, vector,
    wait-for-event, weak-pointer-object,
    fd-close, fd-error-string, fd-input-available?, fd-open,
    fd-read, fd-seek, fd-sync-output, fd-write, fd-exec,
    L_SET, L_INCR, L_XTND, FNDELAY, FAPPEND, FCREAT, FTRUNC, FEXCL, O_RDONLY,
    O_WRONLY, O_RDWR, O_NDELAY, O_APPEND, O_CREAT, O_TRUNC, O_EXCL,
    ENOENT, EIO, ENXIO, EACCES, EFAULT, EEXIST, ENOTDIR, EISDIR,
    EINVAL, ENFILE, EMFILE, ETXTBSY, ENOSPC, EROFS, EOPNOTSUPP, ELOOP,
    ENAMETOOLONG, EDQUOT, EBADF, EINTR, EPIPE, EFBIG, // EWOULDBLOCK, 
    <foreign-file>, <c-function>, <statically-typed-pointer>, signed-byte-at,
    signed-short-at, signed-long-at, unsigned-byte-at, unsigned-short-at,
    unsigned-long-at, signed-byte-at-setter, signed-short-at-setter,
    signed-long-at-setter, unsigned-byte-at-setter, unsigned-short-at-setter,
    unsigned-long-at-setter, pointer-at, pointer-at-setter, c-pointer-slot,
    c-pointer-slot-setter, load-object-file, null-pointer, find-c-pointer,
    find-c-function, constrain-c-function;
  create
    aref, aref-setter, do, error, type-error,
    make-next-method-function, generic-apply,
    forward-iteration-protocol, backward-iteration-protocol, 
    table-protocol, negative?,
    debugger-flush, debugger-call, debugger-print, debugger-report-condition,
    debugger-abort, debugger-describe-restarts, debugger-restart,
    debugger-return, debugger-eval,
    on-exit, exit;
end Builtin-Stuff;


define module extras
  create
    *format-function*, *force-output-function*, *debug-output*,
    report-condition;
end;


define module Dylan
  use Builtin-Stuff,
    import: all,
    export: {\*, \+, \-, \/, \<, \<=, \=, \==, \~=,
	     <array>,
	     <byte-string>,
	     <character>, <class>, <collection>, <complex>,
	     <double-float>,
	     <empty-list>, <explicit-key-collection>, <extended-float>,
	     <float>, <function>,
	     <generic-function>,
	     <integer>,
	     <list>,
	     <method>, <mutable-collection>, <mutable-explicit-key-collection>,
	     <mutable-sequence>,
	     <number>,
	     <object>,
	     <pair>,
	     <ratio>, <rational>, <real>,
	     <sequence>, <simple-object-vector>, <single-float>, <singleton>,
	     <string>, <symbol>,
	     <type>,
	     <unicode-string>,
	     <vector>,
	     $permanent-hash-state,
	     add-method, all-superclasses, applicable-method?, apply,
	     aref, aref-setter, as, ash,
	     backward-iteration-protocol,
	     direct-subclasses, direct-superclasses,
	     ceiling, ceiling/,
	     denominator, do,
	     element, element-setter, error,
	     find-method, floor, floor/, forward-iteration-protocol,
	     function-arguments,
	     generic-function-mandatory-keywords, generic-function-methods,
	     head, head-setter,
	     initialize, instance?,
	     limited, list, logand, logbit?, logior, lognot, logxor,
	     make, merge-hash-codes, method-specializers,
	     negative, negative?, numerator,
	     object-class, object-hash,
	     pair,
	     remove-method, round, round/,
	     singleton, size, slot-initialized?, sorted-applicable-methods,
	     subtype?,
	     table-protocol, tail, tail-setter, truncate, truncate/,
	     union,
	     values, vector};
  use extras;
  export
    \>=, \>, \:=, \|, \&, \^,
    <abort>,
    <condition>,
    <deque>,
    <error>,
    <range>, <restart>,
    <serious-condition>, <simple-error>, <simple-restart>,
    <simple-warning>, <stretchy-collection>, <stretchy-vector>,
    <type-error>,
    <warning>,
    abort, abs, add, add!, add-new, add-new!, always, any?, as-lowercase,
    as-lowercase!, as-uppercase, as-uppercase!,
    break,
    cerror, check-type, choose, choose-by,
    class-for-copy, complement, compose, concatenate, concatenate-as,
    condition-format-string, condition-format-arguments, conjoin,
    copy-sequence, curry,
    default-handler, dimension, dimensions, disjoin, do-handlers,
    empty?, even?, every?,
    fill!, find-key, first, first-setter,
    gcd,
    identity, integral?, intersection,
    key-sequence, key-test,
    last, last-setter, lcm,
    map, map-as, map-into, max, member?, min, modulo,
    odd?,
    pop, pop-last, positive?, push, push-last,
    range, rank, rcurry, reduce, reduce1, remainder, remove,
    remove!, remove-duplicates, remove-duplicates!,
    replace-elements!, replace-subsequence!, restart-query,
    return-allowed?, return-description, return-query, reverse, reverse!,
    row-major-index,
    second, second-setter, shallow-copy, signal, size-setter, sort, sort!,
    subsequence-position,
    third, third-setter, type-error-value,
    type-error-expected-type,
    zero?;
  create
    /* <ratio>, rationalize, numerator, denominator, */
    <object-table>, <table>, remove-key!;
end Dylan;

define module Extensions
  use Dylan;
  use Builtin-Stuff,
    import: {main, exit, on-exit, load, *print-GC-messages*,
	     $maximum-fixed-integer, $minimum-fixed-integer,
	     <byte-character>, <byte-vector>,
	     <boolean>, <true>, <false>,
	     <fixed-integer>, <extended-integer>,
	     <weak-pointer>, weak-pointer-object},
    export: all;
  use extras,
    export: all;
  create
    <equal-table>, equal-hash, collection-hash, 
    <value-table>, value-hash, sequence-hash, string-hash;
  export
    one-of, type-or, false-or, ignore, key-exists?, <dictionary>;
end Extensions;

define module System
  use Dylan;
  use Builtin-Stuff,
    import: {<buffer>, copy-bytes},
    export: all;
end System;

define module File-Descriptors
  use Dylan;
  use Builtin-Stuff,
    import: {fd-close, fd-error-string, fd-input-available?,
	     fd-open, fd-read, fd-seek, fd-sync-output, fd-write, fd-exec,

	     // Lseek call.
	     //
	     L_SET, L_INCR, L_XTND,

	     // Flags also for fcntl call.
	     //
	     FNDELAY, FAPPEND,

	     // Open only modes.
	     //
	     FCREAT, FTRUNC, FEXCL,

	     // Open call.
	     //
	     O_RDONLY, O_WRONLY, O_RDWR, O_NDELAY, O_APPEND, O_CREAT, O_TRUNC,
	     O_EXCL,

	     // Open errors.
	     //
	     ENOENT, EIO, ENXIO, EACCES, EFAULT, EEXIST, ENOTDIR, EISDIR,
	     EINVAL, ENFILE, EMFILE, ETXTBSY, ENOSPC, EROFS, EOPNOTSUPP, ELOOP,
	     ENAMETOOLONG, EDQUOT,

	     // Close errors.
	     //
	     EBADF,

	     // Read errors (that are also not Open or Close errors).
	     //
	     EINTR,
	     // EWOULDBLOCK,

	     // Write errors (that are not also open, close, or read errors).
	     //
	     EPIPE,
	     EFBIG},
    export: all;
end File-Descriptors;


define module Threads
  use Dylan;
  use Extensions;
  use Builtin-Stuff,
    import: {<thread>, spawn-thread, current-thread, kill-thread,
	     <lock>, <spinlock>,
	     locked?, grab-lock, release-lock,
	     <event>, wait-for-event, signal-event, broadcast-event},
    export: all;
  export
    <multilock>, <semaphore>;
end;


define module Hash-Tables
  use Dylan;
  use Builtin-Stuff, import: {state-valid?, float-hash};
  use Extensions;
end;


define module Introspection
  use Builtin-Stuff,
    import: {<defined-class>, <slot-descriptor>,
	     class-name, function-name,
	     slot-allocation, slot-descriptors, slot-getter,
	     slot-getter-method, slot-name, slot-setter, slot-setter-method,
	     slot-type, slot-value, slot-value-setter,

	     object-address,

	     singleton-object,
	     <subclass>, subclass-of,
	     <limited-integer>, limited-integer-class, limited-integer-min,
	     limited-integer-max,
	     <union>, union-members},
    export: all;
end;


define module Cheap-IO
  use Builtin-Stuff,
    import: {fflush, format, prin1, print, putc, puts, getc},
    export: all;
end;

define module Extern
  use Dylan;
  use Builtin-Stuff,
    import: {<foreign-file>, <c-function>, <statically-typed-pointer>,
	     signed-byte-at, signed-short-at, signed-long-at, 
	     unsigned-byte-at, unsigned-short-at, unsigned-long-at, 
	     signed-byte-at-setter, signed-short-at-setter,
	     signed-long-at-setter, 
	     unsigned-byte-at-setter, unsigned-short-at-setter,
	     unsigned-long-at-setter,
	     pointer-at, pointer-at-setter,
	     c-pointer-slot, c-pointer-slot-setter,
	     load-object-file, null-pointer,
	     find-c-pointer, find-c-function, constrain-c-function},
    export: all;
  use Extensions;
  export
    <machine-pointer>, <c-string>, destroy, content-size, structure-size,
    import-value, export-value, pointer-value, pointer-value-setter;
end module Extern;
