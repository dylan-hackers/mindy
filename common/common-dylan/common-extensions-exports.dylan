module: dylan-user

define library common-extensions
  use dylan;
  use melange-support;
  use format;
  use streams;
  use standard-io;
  use table-extensions;
  use format-out;

  // Only import transcendentals if we have them.
//#if (~compiled-for-solaris)
  use transcendental,
     import: { transcendental => transcendentals},
     export: all;
//#endif

  use random,
     import: all,
     export: all;
  use regular-expressions,
     import: all,
     export: all;

  export
    common-extensions,
    finalization,
    threads,
    simple-io,
    simple-random,
    simple-profiling,
    byte-vector,
    functional-extensions;
end library;

define module c-support
  use dylan;
  use extensions;
  use melange-support;

  export
    application-argc,
    application-argv;
end module c-support;

define module finalization
  // XXX - Needs definition. No-op stubs OK.
end module;

define module simple-io
  use format-out,
    export: {format-out};
end module;

define module simple-random
  // XXX - Needs definition.
end module;

define module simple-profiling
  // XXX - Needs definition.
end module;

define module byte-vector
  use extensions,
    export: {<byte>,
	     <byte-vector>};
end module;

define module functional-extensions
  use dylan;
  use Magic, import: {%element, %element-setter};
  use extensions, exclude: { position, assert };
  use common-extensions, import: { find-element };
  export with-bounds-checks, without-bounds-checks,
    find-value, // assert already in common-extensions 
    with-keywords-removed, dynamic-bind,
    <synchronization>, <exclusive-lock>,
    <semaphore>, <recursive-lock>,
    <read-write-lock>,
    <lock>, <simple-lock>, with-lock,
    <thread>, atomic-increment!, current-thread,
    <notification>, wait-for, release-all,
    put-property!, get-property, \remove-property!,
    element-range-error, \profiling;
end module;

define module threads
  use functional-extensions, 
    export: { dynamic-bind,
    <synchronization>, <exclusive-lock>,
    <semaphore>, <recursive-lock>,
    <read-write-lock>,
    <lock>, <simple-lock>, with-lock,
    <thread>, atomic-increment!, current-thread,
    <notification>, wait-for };
end module threads;
                                      

define module common-extensions
  use dylan;
  use system, import: { copy-bytes }, export: { copy-bytes };
  use extensions,
    exclude: { assert },
    rename: {$not-supplied => $unsupplied,
	     on-exit => register-exit-application-function},
    export: {$unsupplied,
             integer-length,
	     false-or,
	     one-of,
	     subclass,
	     <format-string-condition>, <simple-condition>,
	     ignore,
	     key-exists?,
	     difference,
	     register-exit-application-function,
	     <stretchy-sequence>,
	     <object-deque>,
	     <stretchy-object-vector>,
             <byte-character>};
  use %Hash-Tables,
    export: {remove-all-keys!};
  use table-extensions,
    export: {<string-table>};
  use c-support;
  use format, export: all;
  use streams, import: { new-line, force-output, <stream> },
    export: {<stream>};
  use standard-io;
  use random,
     export: all;
  use regular-expressions,
     export: all;
  use functional-extensions,
     export: all;

  export
    /* Numerics */
    //integer-length,

    /* Unsupplied, unfound */
    //$unsupplied,
    supplied?,
    unsupplied?,
    unsupplied,
    $unfound,
    found?,
    unfound?,
    unfound,

    /* Collections */
    //<object-deque>,
    //<stretchy-sequence>,
    //<stretchy-object-vector>,
    concatenate!,
    position,
    //remove-all-keys!,
    //difference,
    fill-table!,
    find-element,
    //key-exists?,

    /* Conditions */
    //<format-string-condition>,
    condition-to-string,

    /* Debugging */
    \assert,
    debug-message,
    \debug-assert,

    /* Types */
    //false-or,
    //one-of,
    //subclass,

    /* Ignoring */
    //ignore,
    ignorable,

    /* Converting to and from numbers */
    float-to-string,
    integer-to-string,
    string-to-integer,
    // Not part of common? number-to-string,

    /* Appliation runtime environment */
    application-name,
    application-filename,
    application-arguments,
    exit-application;
    //register-exit-application-function,

#if (~mindy)
  export
    \table-definer,
    \iterate,
    \when;

  export
    \%iterate-aux,
    \%iterate-param-helper,
    \%iterate-value-helper;
#endif
end module;
