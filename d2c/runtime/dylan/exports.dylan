rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/exports.dylan,v 1.15 1995/12/15 05:32:32 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

define library Dylan
  export
    Dylan, Extensions, Cheap-IO, System, Introspection;
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
	     variable-definer, constant-definer, /* generic-definer, */
	     method-definer, class-definer, module-definer, library-definer,

	     // Statements
	     \if, \unless, \case, \select, \while, \until, \for, \begin,
	     \block, \method,

	     // Special Operators
	     \&, \|, \:=
    },
    export: all;
end;

define module Extensions
  use Dylan-Viscera,
    import: {
	     // More integers.
	     <fixed-integer>, <extended-integer>,
	     $maximum-fixed-integer, $minimum-fixed-integer,

	     // Ratios.
	     <ratio>, ratio, numerator, denominator,

	     // More types.
	     <byte-character>, <true>, <false>,

	     // Hash table extensions.
	     <equal-table>, equal-hash, 
	     <value-table>, value-hash,
	     <string-table>, string-hash,
	     collection-hash, sequence-hash,

	     // Type extensions.
	     false-or, one-of, <never-returns>,

	     // Condition extensions.
	     <format-string-condition>, report-condition, condition-format,
	     *warning-output*,

	     // Debugger Hooks
	     <debugger>, invoke-debugger, *debugger*,

	     // Byte vector stuff.
	     <byte>, <byte-vector>,

	     // Misc other stuff.
	     $not-supplied, ignore
    },
    export: all;
end;

define module Cheap-IO
  use Dylan-Viscera,
    import: {
	     // Cheap-IO stuff.
	     format, print-message, print, write-integer, write

    },
    export: all;
end;

define module System
  use Dylan-Viscera,
    import: {// Foreign interface stuff.
	     call-out, c-include, c-decl, c-expr,

	     // Raw pointer stuff.
	     <raw-pointer>, pointer-deref, pointer-deref-setter,

	     // Object address
	     object-address,

	     // Buffers
	     <buffer>, <buffer-index>, $maximum-buffer-size,
	     copy-bytes, buffer-address},
    export: all;
end;

define module Introspection
  use Dylan-Viscera,
    import: {class-name, function-name,
	     singleton-object,
	     <limited-integer>, limited-integer-base-class,
	     limited-integer-minimum, limited-integer-maximum,
	     <union>, union-members, union-singletons},
    export: all;
end;

