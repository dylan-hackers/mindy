module: dylan-user

//////////////////////////////////////////////////////////////////////
//
//  Copyright (C) 1994, Carnegie Mellon University
//  All rights reserved.
//
//  This code was produced by the Gwydion Project at Carnegie Mellon
//  University.  If you are interested in using this code, contact
//  "Scott.Fahlman@cs.cmu.edu" (Internet).
//
//////////////////////////////////////////////////////////////////////
//
//  $Header: /home/housel/work/rcs/gd/src/mindy/libraries/dylan/exports.dylan,v 1.26 1994/05/19 22:37:13 wlott Exp $
//
//  This file does whatever.
//

define library Dylan
  export Dylan, Extensions, System, Threads;
end Dylan;

define module Builtin-Stuff
  export
    \*, \+, \-, \/, \<, \<=, \=, \==, \~=,
    <array>,
    <boolean>, <buffer>, <byte-string>, <byte-vector>,
    <character>, <class>, <collection>, <complex>,
    <double-float>,
    <empty-list>, <event>, <explicit-key-collection>, <extended-float>,
    <false>, <float>, <function>,
    <generic-function>,
    <integer>,
    <keyword>,
    <list>, <lock>,
    <method>, <mutable-collection>, <mutable-explicit-key-collection>,
    <mutable-sequence>,
    <number>,
    <object>,
    <pair>,
    <ratio>, <rational>, <real>,
    <sequence>, <simple-object-vector>, <single-float>, <singleton>,
    <string>, <symbol>,
    <thread>, <true>, <type>,
    <unicode-string>,
    <vector>,
    <weak-pointer>,
    $permanent-hash-state,
    add-method, all-superclasses, applicable-method?, apply,
    apply-curry, as, ash,
    broadcast-event,
    direct-subclasses, direct-superclasses,
    ceiling, ceiling/, copy-bytes, current-handler,
    do-next-method,
    element, element-setter, enable-error-system,
    find-method, floor, floor/, format, function-arguments,
    generic-function-methods, getc, grab-lock,
    handler-description, handler-function, handler-next, handler-test,
    handler-type, head, head-setter,
    initialize, instance?, invoke-debugger,
    kill-thread,
    limited, list, locked?, logand, logbit?, logior, lognot, logxor,
    main, make, merge-hash-codes, method-specializers,
    negative,
    object-class, object-hash,
    pair, prin1, print, putc, puts,
    raw-exit, release-lock, remove-method, round, round/,
    signal-event, singleton, size, slot-initialized?, spawn-thread,
    sorted-applicable-methods, state-valid?, subtype?,
    tail, tail-setter, truncate, truncate/,
    union, fd-close, fd-error-string, fd-input-available?, fd-open,
    fd-read, fd-seek, fd-sync-output, fd-write, fd-exec,
    values, vector,
    wait-for-event, weak-pointer-object;
  create
    aref, aref-setter, do, error, type-error,
    make-next-method-function, generic-apply,
    forward-iteration-protocol, backward-iteration-protocol, negative?,
    debugger-flush, debugger-call, debugger-print, debugger-report-condition,
    debugger-abort, debugger-describe-restarts, debugger-restart,
    debugger-return, debugger-eval,
    on-exit, exit, <=table>;
end Builtin-Stuff;

define module Dylan
  use Builtin-Stuff,
    import: all,
    export: (\*, \+, \-, \/, \<, \<=, \=, \==, \~=,
	     <array>,
	     <byte-string>,
	     <character>, <class>, <collection>, <complex>,
	     <double-float>,
	     <empty-list>, <explicit-key-collection>, <extended-float>,
	     <float>, <function>,
	     <generic-function>,
	     <integer>,
	     <keyword>,
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
	     do,
	     element, element-setter, error,
	     find-method, floor, floor/, forward-iteration-protocol,
	     function-arguments,
	     generic-function-methods,
	     head, head-setter,
	     initialize, instance?,
	     limited, list, logand, logbit?, logior, lognot, logxor,
	     make, merge-hash-codes, method-specializers,
	     negative, negative?,
	     object-class, object-hash,
	     pair,
	     remove-method, round, round/,
	     singleton, size, slot-initialized?, sorted-applicable-methods,
	     subtype?,
	     tail, tail-setter, truncate, truncate/,
	     union,
	     values, vector);
  export
    \>=, \>, \:=, \|, \&,
    <abort>,
    <condition>,
    <deque>,
    <error>,
    <object-table>,
    <range>, <restart>,
    <serious-condition>, <simple-error>, <simple-restart>,
    <simple-warning>, <stretchy-collection>, <stretchy-vector>,
    <table>, <type-error>,
    <warning>,
    abort, abs, add, add!, add-new, add-new!, always, any?, as-lowercase,
    as-lowercase!, as-uppercase, as-uppercase!,
    break,
    cerror, check-type, choose, choose-by,
    class-for-copy, complement, compose, concatenate, concatenate-as,
    condition-format-string, condition-format-arguments, conjoin,
    copy-sequence, curry,
    default-handler, denominator, dimensions, disjoin, do-handlers,
    empty?, even?, every?, expt,
    fill!, find-key, first, first-setter,
    gcd,
    identity, integral?, intersection,
    key-hash, key-sequence, key-test, key=,
    last, last-setter, lcm,
    map, map-as, map-into, max, member?, min, modulo,
    numerator,
    odd?,
    pop, pop-last, positive?, push, push-last,
    range, rationalize, rcurry, reduce, reduce1, remainder, remove,
    remove!, remove-duplicates, remove-duplicates!, remove-key!,
    replace-elements!, replace-subsequence!, restart-query,
    return-allowed?, return-description, return-query, reverse, reverse!,
    second, second-setter, shallow-copy, signal, size-setter, sort, sort!,
    subsequence-position,
    third, third-setter, type-error-value,
    type-error-expected-type,
    zero?;
end Dylan;

define module Extensions
  use Dylan;
  use Builtin-Stuff,
    import: (main, exit, on-exit,
	     format, prin1, print, putc, puts, getc, <byte-vector>,
	     <boolean>, <true>, <false>,
	     <weak-pointer>, weak-pointer-object, <=table>),
    export: all;
end Extensions;

define module System
  use Dylan;
  use Builtin-Stuff,
    import: (<buffer>, copy-bytes,
	     fd-close, fd-error-string, fd-input-available?,
	     fd-open, fd-read, fd-seek, fd-sync-output, fd-write, fd-exec),
    export: all;
end System;

define module Threads
  use Dylan;
  use Builtin-Stuff,
    import: (<thread>, spawn-thread, kill-thread,
	     <lock>, locked?, grab-lock, release-lock,
	     <event>, wait-for-event, signal-event, broadcast-event),
    export: all;
end;
