module: dylan-user

######################################################################
##
##  Copyright (C) 1994, Carnegie Mellon University
##  All rights reserved.
##
##  This code was produced by the Gwydion Project at Carnegie Mellon
##  University.  If you are interested in using this code, contact
##  "Scott.Fahlman@cs.cmu.edu" (Internet).
##
######################################################################
##
##  $Header: /home/housel/work/rcs/gd/src/mindy/libraries/dylan/exports.dylan,v 1.3 1994/03/28 11:33:48 wlott Exp $
##
##  This file does whatever.
##

define library Dylan
  export Dylan;
  export Extensions;
end Dylan;

define module Builtin-Stuff
  export
    `*`, `+`, `-`, `/`, `<`, `<=`, `=`, `==`, `/=`,
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
    add-method, all-superclasses, applicable-method?, apply, as, ash,
    direct-subclasses, direct-superclasses,
    ceiling/, current-handler,
    do-next-method,
    element, element-setter, enable-error-system,
    floor/, format, function-arguments,
    generic-function-methods, getc,
    handler-description, handler-function, handler-next, handler-test,
    handler-type, head, head-setter,
    initialize, instance?, invoke-debugger,
    limited, list, logand, logbit?, logior, lognot, logxor,
    main, make, method-specializers,
    negative,
    object-class,
    pair, prin1, print, putc, puts,
    remove-method, round/,
    singleton, size, slot-initialized?,
    sorted-applicable-methods, subtype?,
    tail, tail-setter, truncate/,
    union,
    values, vector;
  create
    do, error, type-error, make-next-method-function, generic-apply,
    forward-iteration-protocol, backward-iteration-protocol, negative?,
    debugger-print, debugger-report-condition, debugger-abort,
    debugger-describe-restarts, debugger-restart, debugger-return;
end Builtin-Stuff;

define module Dylan
  use Builtin-Stuff,
    exclude: (main, type-error, current-handler, handler-description,
	      handler-function, handler-next, handler-test, handler-type,
	      make-next-method-function, do-next-method, generic-apply,
	      format, prin1, print, putc, puts, getc, invoke-debugger,
	      debugger-print, debugger-report-condition, debugger-abort,
	      debugger-describe-restarts, debugger-restart, debugger-return,
	      enable-error-system),
    export: all;
  use Builtin-Stuff,
    import: (main, type-error, current-handler, handler-description,
	     handler-function, handler-next, handler-test, handler-type,
	     make-next-method-function, do-next-method, generic-apply,
	     format, prin1, print, putc, puts, getc, invoke-debugger, 
	     debugger-print, debugger-report-condition, debugger-abort,
	     debugger-describe-restarts, debugger-restart, debugger-return,
	     enable-error-system);
  export
    `>=`, `>`, `:=`, `|`, `&`,
    <abort>,
    <condition>,
    <deque>,
    <error>,
    <range>, <restart>,
    <serious-condition>, <simple-error>, <simple-restart>,
    <simple-warning>, <stretchy-vector>,
    <table>, <type-error>,
    <warning>,
    abort, abs, add, add!, add-new, add-new!, always, any?, aref,
    aref-setter, as-lowercase, as-lowercase!, as-uppercase,
    as-uppercase!,
    break,
    ceiling, cerror, check-type, choose, choose-by,
    class-for-copy, complement, compose, concatenate, concatenate-as,
    condition-format-string, condition-format-arguments, conjoin,
    copy-sequence, curry,
    default-handler, denominator, dimensions, disjoin, do-handlers,
    empty?, even?, every?, expt,
    fill!, find-key, find-method, first, first-setter, floor,
    gcd,
    identity, integral?, intersection,
    key-sequence,
    last, last-setter, lcm,
    map, map-as, map-into, max, member?, min, modulo,
    numerator,
    odd?,
    pop, pop-last, positive?, push, push-last,
    range, rationalize, rcurry, reduce, reduce1, remainder, remove,
    remove!, remove-duplicates, remove-duplicates!, remove-key!,
    replace-elements!, replace-subsequence!, restart-query,
    return-allowed?, return-description, return-query, reverse, reverse!,
    round,
    second, second-setter, shallow-copy, signal, size-setter, sort, sort!,
    subsequence-position,
    third, third-setter, truncate, type-error-value,
    type-error-expected-type,
    zero?;
end Dylan;

define module Extensions
  use Dylan;
  use Builtin-Stuff,
    import: (main, format, prin1, print, putc, puts, getc),
    export: all;
end Extensions;
