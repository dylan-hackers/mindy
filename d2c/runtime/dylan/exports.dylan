rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/exports.dylan,v 1.29 2002/11/24 13:52:36 andreas Exp $
copyright: see below
module: dylan-viscera

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

define library Dylan
  export
    Dylan, Extensions, Cheap-IO, System, Machine-words,
    Introspection, Magic, %Hash-Tables;
end;

define module Dylan
  use Dylan-Viscera,
    import: {
	     // Objects
	     <object>,

	     // Types
	     <type>, <class>, <singleton>,

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
	     <simple-vector>, <simple-object-vector>, <stretchy-vector>,
	     <deque>, <list>, <pair>, <empty-list>, <range>,
	     <string>, <byte-string>, <unicode-string>,
	     <table>, <object-table>,

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
	     empty?, size, size-setter, rank, row-major-index,
	     dimensions, dimension, key-test, key-sequence, element,
	     element-setter, aref, aref-setter, first, second, third,
	     first-setter, second-setter, third-setter, last,
	     last-setter, head, tail, head-setter, tail-setter, add,
	     add!, add-new, add-new!, remove, remove!, push, pop,
	     push-last, pop-last, reverse, reverse!, sort, sort!,
	     intersection, union, remove-duplicates,
	     remove-duplicates!, copy-sequence, concatenate,
	     concatenate-as, replace-subsequence!,
	     subsequence-position, do, map, map-as, map-into, any?,
	     every?, reduce, reduce1, choose, choose-by, member?,
	     find-key, remove-key!, replace-elements!, fill!,
	     forward-iteration-protocol, backward-iteration-protocol,
	     table-protocol, merge-hash-ids, object-hash,

	     // Reflective Operations on Types
	     instance?, subtype?, object-class, all-superclasses,
	     direct-superclasses, direct-subclasses,

	     // Functional Operations
	     compose, complement, disjoin, conjoin, curry, rcurry, always,

	     // Function Application
	     apply,

	     // Reflective Operations on Functions
	     generic-function-methods, add-method,
	     generic-function-mandatory-keywords,
	     function-specializers, function-arguments,
	     function-return-values, applicable-method?,
	     sorted-applicable-methods, find-method, remove-method,

	     // Operations on Conditions
	     signal, error, cerror, break, check-type, abort,
	     default-handler, restart-query, return-query,
	     do-handlers, return-allowed?, return-description,
	     condition-format-string, condition-format-arguments,
	     type-error-value, type-error-expected-type, 
	     type-error-location,

	     // Definitions
	     variable-definer, constant-definer, domain-definer,
	     function-definer, generic-definer, method-definer, class-definer,
	     module-definer, library-definer,

	     // Statements
	     \if, \unless, \case, \select, \while, \until, \for, \begin,
	     \block, \method, \when, \iterate, 

             // why do we have to export these?
             \%iterate-aux, \%iterate-param-helper, \%iterate-value-helper,


	     // Function-macro operators
	     \:=, \&, \|,
    
             // Constants
             $maximum-integer, $minimum-integer
    },
    export: all;
end;

define module Extensions
  use Dylan-Viscera,
    import: {
	     // More integers.
	     <general-integer>, <double-integer>, <extended-integer>,
	     $maximum-integer, $minimum-integer, integer-length,

	     // Ratios.
	     <ratio>, ratio, numerator, denominator,

	     // More types.
	     <byte-character>, <true>, <false>, <stretchy-sequence>,
	     <stretchy-object-vector>, <simple-object-table>,
	     <simple-object-deque> => <object-deque>,

	     // Type extensions.
	     false-or, one-of, <never-returns>, subclass, direct-instance,

	     // Condition extensions.
	     <format-string-condition>, <simple-condition>,
	     report-condition, condition-format,
	     condition-force-output, *warning-output*,

	     // Debugger hooks.
	     <debugger>, invoke-debugger, *debugger*,

	     // Collection operations.
	     difference, concatenate!,

	     // Byte vector stuff.
	     <byte>, <byte-vector>,

	     // Misc other stuff.
	     $not-supplied, $unsupplied, unsupplied?,
             supplied?, unsupplied, $unfound, unfound?,
             found?, unfound,
             ignore, functional-==, key-exists?, assert,
             debug-assert, \with-bounds-checks, 
             \fake-without-bounds-checks => \without-bounds-checks, 
             element-range-error,
	     exit, on-exit, limited-collection-definer,
	     limited-vector-class, element-type, %elem, %elem-setter,
	     limited-sv-class, ssv-data, ssv-data-setter, lsv-data-type,
             lsv-fill, %main, main
    },
    export: all;
end;

define module Cheap-IO
  use Dylan-Viscera,
    import: {
	     // Cheap-IO stuff.
	     format, print-message, print, write-integer, puts

    },
    export: all;
end;

define module System
  use Dylan-Viscera,
    import: {\%%primitive,
	     
	     // Foreign interface stuff.
	     \call-out, \c-include, \c-system-include,
	     \c-decl, \c-local-decl, \c-expr, \c-literal,
	     \c-struct-field, \c-struct-field-setter,
	     \callback-method, \callback-entry,

	     // Designator-class (C-FFI) stuff.
	     <designator-class>, \designator-class-definer,
	     size-of, alignment-of, referenced-type,

	     // Nasty debugging hooks.
	     *gdb-output*,

	     // Raw pointer stuff.
	     <raw-pointer>, pointer-deref, pointer-deref-setter,

	     system, import-string, export-string, getenv, 
	     no-core-dumps, get-time-of-day,

	     // Buffers.
	     <buffer>, <buffer-index>, $maximum-buffer-size,
	     buffer-next, buffer-next-setter, buffer-end, buffer-end-setter,
	     copy-bytes, buffer-address},
    export: all;
end;

define module Machine-Words
  use Dylan-Viscera,
    import: {<machine-word>, $machine-word-size, $machine-word-zero,
             $maximum-signed-machine-word, $minimum-signed-machine-word,
             $maximum-unsigned-machine-word, $minimum-unsigned-machine-word,
             odd?, even?, zero?, negative?, positive?,
             // as-unsigned, %logior, %logxor, %logand, %lognot, %logbit?
             // %count-low-zeros, %count-high-zeros, %+, %-, %*,
             // %floor/, %ceiling/, %ceiling/, %round/, %truncate/,
             // %negative, %abs, %shift-left, %shift-right,
             // so%+, so%-, so%*, so%negative, so%abs, so%shift-left,
             // d%floor/, d%ceiling/, d%truncate/, d%divide,
             // u%+, u%-, u%*, u%divide, u%rotate-left, u%rotate-right,
             // u%shift-left. u%shift-right, u%<,
             // ud%divide, ud%shift-left, ud%shift-right
            },
    export: all;
end;

define module Introspection
  use Dylan-Viscera,
    import: {class-name, function-name,
	     singleton-object,
	     <limited-integer>, limited-integer-base-class,
	     limited-integer-minimum, limited-integer-maximum,
	     <union>, union-members, union-singletons,
	     <subclass>, subclass-of,
	     <direct-instance>, direct-instance-of,
	     <byte-character-type>,
	     object-address
},
    export: all;
end;

define module %Hash-Tables
  use Dylan-Viscera,
    import: {remove-all-keys!, uppercase?,
	     <hash-state>, collection-hash,
	     <equal-table>, equal-hash,
	     <value-table>, value-hash,
	     string-hash, sequence-hash},
    export: all;
end module %Hash-Tables;

define module magic
  use Dylan-Viscera,
    import: {%check-type,
             %element,
	     %element-setter,
             \without-bounds-checks,
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
	     \define-constant,
	     \define-generic,
	     \define-variable,
	     disable-catcher,
	     find-slot-offset,
	     \for-aux,
	     \for-aux2,
	     \for-clause,
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
	     \mv-call,
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
	     wrong-number-of-arguments-error},
    export: all;
end;
