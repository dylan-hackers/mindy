rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/exports.dylan,v 1.1 1998/05/03 19:55:38 andreas Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
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

define library Dylan
  export
    Dylan, Extensions, Cheap-IO, System,
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
	     table-protocol, merge-hash-codes, object-hash,

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

	     // Other Built-In Objects
	     $permanent-hash-state,

	     // Definitions
	     variable-definer, constant-definer, domain-definer,
	     function-definer, generic-definer, method-definer, class-definer,
	     module-definer, library-definer,

	     // Statements
	     \if, \unless, \case, \select, \while, \until, \for, \begin,
	     \block, \method,

	     // Function-macro operators.
	     \:=, \&, \|
    },
    export: all;
end;

define module Extensions
  use Dylan-Viscera,
    import: {
	     // More integers.
	     <general-integer>, <extended-integer>,
	     $maximum-integer, $minimum-integer, integer-length,

	     // Ratios.
	     <ratio>, ratio, numerator, denominator,

	     // More types.
	     <byte-character>, <true>, <false>,

	     // Type extensions.
	     false-or, one-of, <never-returns>, subclass, direct-instance,

	     // Condition extensions.
	     <format-string-condition>, report-condition, condition-format,
	     condition-force-output, *warning-output*,

	     // Debugger Hooks
	     <debugger>, invoke-debugger, *debugger*,

	     // Byte vector stuff.
	     <byte>, <byte-vector>,

	     // Misc other stuff.
	     $not-supplied, ignore, functional-==, key-exists?, assert,
	     exit, limited-collection-definer,
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
	     \call-out, \c-include, \c-decl, \c-expr,

	     // Raw pointer stuff.
	     <raw-pointer>, pointer-deref, pointer-deref-setter,

	     system, import-string, export-string, getenv, 
	     no-core-dumps, get-time-of-day,

	     // Buffers
	     <buffer>, <buffer-index>, $maximum-buffer-size,
	     buffer-next, buffer-next-setter, buffer-end, buffer-end-setter,
	     copy-bytes, buffer-address},
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
	     class-maker-setter,
	     class-new-slot-descriptors,
	     closure-var,
	     closure-var-setter,
	     \define-generic,
	     disable-catcher,
	     find-slot-offset,
	     \for-aux,
	     \for-aux2,
	     \for-clause,
	     general-call,
	     general-rep-getter,
	     general-rep-setter,
	     gf-call,
	     heap-rep-getter,
	     heap-rep-setter,
	     make-catcher,
	     make-closure,
	     make-exit-function,
	     make-rest-arg,
	     maybe-do-defered-evaluations,
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
	     slot-init-function,
	     slot-init-function-setter,
	     slot-init-value,
	     slot-init-value-setter,
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
