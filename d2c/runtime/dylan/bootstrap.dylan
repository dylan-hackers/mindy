rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/bootstrap.dylan,v 1.37 2003/07/02 16:56:30 housel Exp $
copyright: see below
module: bootstrap

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000, 2001  Gwydion Dylan Maintainers
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
//    University, and the Gwydion Dylan Maintainers.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
// Also, see http://www.gwydiondylan.org/ for updates and documentation. 
//
//======================================================================

// The first thing we need to do is define ``define module''.
//
define macro module-definer
    { define module ?:name ?clauses end }
      => make-define-module({ ?name }, { ?clauses })

  clauses:
    { } => { }
    { ?clause; ... } => { ?clause, ... }

  clause:
    {use ?:name, #key ?import = all, ?exclude = {}, ?prefix:token = "", 
		      ?rename = {}, ?export = {} }
      => make-use-clause({ ?name }, { ?import }, { ?exclude }, { ?prefix },
			 { ?rename }, { ?export })
    {export ?names }
      => make-export-clause({ ?names })
    {create ?names }
      => make-create-clause({ ?names })

  names:
    { } => { }
    { ?:name, ... } => { ?name, ... }

  import:
    { all } => { #t }
    { { ?variable-specs } } => { ?variable-specs }

  variable-specs:
    { } => { }
    { ?:name, ... } => { ?name, ... }
    { ?renaming, ... } => { ?renaming, ... }

  exclude:
    { { ?names } } => { ?names }

  rename:
    { { ?renamings } } => { ?renamings }

  renamings:
    { } => { }
    { ?renaming, ... } => { ?renaming, ... }

  renaming:
    { ?from:name => ?to:name } => make-renaming({ ?from }, { ?to })

  export:
    { all } => { #t }
    { { ?names } } => { ?names }

end;

// Then we can use it to define the dylan-viscera module.
//
define module dylan-viscera
  use bootstrap,
    export: all;

  export
    
    // Objects
    <object>,

    // Types
    <type>, <class>, <designator-class>, <singleton>,

    // Simple Objects
    <character>, <symbol>, <boolean>,

    // Numbers
    <number>, <complex>, <real>,
    <float>, <single-float>, <double-float>, <extended-float>,
    <rational>, <integer>,

    // Collections
    <collection>, <explicit-key-collection>, <sequence>,
    <mutable-collection>, <mutable-explicit-key-collection>,
    <mutable-sequence>, <stretchy-collection>, <array>, <vector>,
    <simple-vector>, <simple-object-vector>, <stretchy-vector>, <deque>,
    <list>, <pair>, <empty-list>, <range>, <string>, <byte-string>,
    <unicode-string>, <table>, <object-table>,

    // Functions
    <function>, <generic-function>, <method>,

    // Conditions
    <condition>, <serious-condition>, <error>, <simple-error>,
    <type-error>, <sealed-object-error>, <warning>, <simple-warning>,
    <restart>, <simple-restart>, <abort>,

    // Constructing and Initializing Instances
    make, initialize, slot-initialized?, list, pair, range, singleton,
    limited, type-union, vector,

    // Equality and Comparison
    \~, \==, \~==, \=, \~=, \<, \>, \<=, \>=, min, max,

    // Arithmetic Operations
    odd?, even?, zero?, positive?, negative?, integral?,
    \+, \*, \-, \/, negative, floor, ceiling, round, truncate,
    floor/, ceiling/, round/, truncate/, modulo, remainder,
    \^, abs, logior, logxor, logand, lognot, logbit?, ash,
    lcm, gcd,

    // Coercing and Copying Objects
    identity, values, as, as-uppercase, as-uppercase!,
    as-lowercase, as-lowercase!, shallow-copy, type-for-copy,

    // Collection Operations
    empty?, size, size-setter, rank, row-major-index, dimensions,
    dimension, key-test, key-sequence, element, element-setter,
    aref, aref-setter, first, second, third, first-setter, second-setter,
    third-setter, last, last-setter, head, tail, head-setter, tail-setter,
    add, add!, add-new, add-new!, remove, remove!, push, pop, push-last,
    pop-last, reverse, reverse!, sort, sort!, intersection, difference, union,
    remove-duplicates, remove-duplicates!, copy-sequence, concatenate,
    concatenate!, concatenate-as, replace-subsequence!, subsequence-position,
    do, map, map-as, map-into, any?, every?, reduce, reduce1, choose,
    choose-by, member?, find-key, remove-key!, replace-elements!, fill!,
    forward-iteration-protocol, backward-iteration-protocol, table-protocol,
    merge-hash-ids, object-hash,

    // Reflective Operations on Types
    instance?, subtype?, object-class, all-superclasses, direct-superclasses,
    direct-subclasses,

    // Functional Operations
    compose, complement, disjoin, conjoin, curry, rcurry, always,

    // Function Application
    apply,

    // Reflective Operations on Functions
    generic-function-methods, add-method, generic-function-mandatory-keywords,
    function-specializers, function-arguments, function-return-values,
    applicable-method?, sorted-applicable-methods, find-method, remove-method,

    // Operations on Conditions
    signal, error, cerror, break, check-type, abort, default-handler,
    restart-query, return-query, do-handlers, return-allowed?,
    return-description, condition-format-string, condition-format-arguments,
    type-error-value, type-error-expected-type,
    type-error-location,

    // Definitions
    variable-definer, constant-definer, domain-definer, function-definer,
    generic-definer, method-definer, class-definer, designator-class-definer,
    library-definer,

    // Statements
    \if, \unless, \case, \select, \while, \until, \for, \begin,
    \block, \method,

    // Function-macro operators
    \:=, \&, \|,
    
    // Extensions
    <general-integer>,  <double-integer>, <extended-integer>,
    $maximum-integer, $minimum-integer, integer-length,
    <ratio>, ratio, numerator, denominator,
    decode-float, scale-float, float-radix,
    float-digits, float-precision,
    integer-decode-float,
    $single-float-epsilon, $double-float-epsilon,
    $extended-float-epsilon,
    $minimum-single-float-exponent, $maximum-single-float-exponent,
    $minimum-double-float-exponent, $maximum-double-float-exponent,
    $minimum-extended-float-exponent, $maximum-extended-float-exponent,
    <byte-character>, <true>, <false>,
    false-or, one-of, <never-returns>, subclass, direct-instance,
    report-condition, condition-format, condition-force-output,
    *warning-output*, *gdb-output*,
    <debugger>, invoke-debugger, debugger-message, *debugger*, debug-message,
    <bit>, <byte>, <byte-vector>,
    <set>, <object-set>,
    $not-supplied, ignore,
    <simple-condition>, <format-string-condition>,
    functional-==, key-exists?, assert, debug-assert, 
    limited-collection-definer, limited-vector-class, element-type, 
    %elem, %elem-setter, limited-sv-class, ssv-data, ssv-data-setter,
    lsv-data-type, lsv-fill, %main, main,
    <stretchy-sequence>, <simple-object-deque>, <stretchy-object-vector>,
    <simple-object-table>, element-error,

    // Cheap IO
    format, print-message, print, write-integer, puts,

    // System stuff
    call-out, c-include, c-system-include, c-decl, c-local-decl,
    c-expr, c-literal, c-struct-field, c-struct-field-setter,
    callback-method, callback-entry,
    <raw-pointer>, pointer-deref, pointer-deref-setter,
    object-address, heap-object-at, general-object-at,
    <buffer>, <buffer-index>, $maximum-buffer-size,
    buffer-next, buffer-next-setter, buffer-end, buffer-end-setter,
    copy-bytes, buffer-address,

    system, import-string, export-string, getenv, 
    exit, on-exit, no-core-dumps, get-time-of-day,

    // Machine-word stuff
    <machine-word>, $machine-word-size, $machine-word-zero,
    $maximum-signed-machine-word, $minimum-signed-machine-word,
    $maximum-unsigned-machine-word, $minimum-unsigned-machine-word,
    // as-unsigned,
    %logior, %logxor, %logand, %lognot, %logbit?,
    // %count-low-zeros, %count-high-zeros, %+, %-, %*,
    // %floor/, %ceiling/, %ceiling/, %round/, %truncate/,
    // %negative, %abs,
    %shift-left, %shift-right,
    // so%+, so%-, so%*, so%negative, so%abs, so%shift-left,
    // d%floor/, d%ceiling/, d%truncate/, d%divide,
    // u%+, u%-, u%*, u%divide, 
    u%rotate-left, u%rotate-right, u%shift-left, u%shift-right,
    // u%<, ud%divide, ud%shift-left, ud%shift-right,

    // Introspection Stuff
    class-name, function-name,
    singleton-object,
    <limited-integer>, limited-integer-base-class,
    limited-integer-minimum, limited-integer-maximum,
    <union>, union-members, union-singletons,
    <subclass>, subclass-of,
    <direct-instance>, direct-instance-of,
    <byte-character-type>,
    size-of, alignment-of, referenced-type,		 

    // %Hash-Tables Stuff (Ultimately exported from Table-Extensions
    remove-all-keys!, uppercase?,
    <hash-state>, collection-hash,
    <equal-table>, equal-hash,
    <value-table>, value-hash,
    string-hash, sequence-hash,

    // Variables magically referenced by the compiler which we need to hang
    // around even though they aren't otherwise overtly exported.
    %check-type,
    %element,
    %element-setter,
    %instance?,
    %make-method,
    %make-next-method-cookie,
    %object-class,
    ambiguous-method-error,
    apply-safely,		 
    catch,
    check-types,
    class-all-slot-descriptors,
    class-bucket,
    class-maker-setter,
    class-new-slot-descriptors,
    class-row,
    closure-var,
    closure-var-setter,
    disable-catcher,
    find-slot-offset,
    fast-class-instance?,
    general-call,
    general-rep-getter,
    general-rep-setter,
    gf-call,
    heap-rep-getter,
    heap-rep-setter,
    <limited-object-table>,
    <limited-simple-vector>,
    make-catcher,
    make-closure,
    make-exit-function,
    make-limited-collection,
    make-rest-arg,
    maybe-do-deferred-evaluations,
    missing-required-init-keyword-error,
    no-applicable-methods-error,
    odd-number-of-keyword/value-arguments-error,
    override-init-function,
    override-init-function-setter,
    override-init-value,
    override-init-value-setter,
    pop-handler,
    pop-unwind-protect,
    push-handler,
    push-unwind-protect,
    select-error,
    <simple-integer-vector>,
    <simple-schar-vector>,
    <simple-sshort-vector>,
    <simple-uchar-vector>,
    <simple-ushort-vector>,
    slot-init-function,
    slot-init-function-setter,
    slot-init-value,
    slot-init-value-setter,
    slot-name,
    slot-type,
    slot-type-setter,
    slot-getter,
    slot-representation,
    slow-functional-==,
    throw,
    type-error,
    uninitialized-slot-error,
    unique-id,
    unrecognized-keyword-error,
    value,
    value-setter,
    values-sequence,
    verify-keywords,
    wrong-number-of-arguments-error;

end;
