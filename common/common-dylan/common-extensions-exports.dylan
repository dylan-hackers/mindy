module: dylan-user

define library common-extensions
  use dylan;
  use melange-support;
  use format;
  use streams;
  use standard-io;
  use table-extensions;
  use format-out;

  use transcendental,
    import: { transcendental => transcendentals },
    export: all;

  use random;
  use regular-expressions,
    import: all,
    export: all;

  export
    common-extensions,
    finalization,
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
  use random,
    import: { <random-state> => <random>, random },
    export: all;
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
  use extensions, exclude: { position };
  use common-extensions, import: { find-element };
  export 
    find-value,
    \profiling;
end module;

define module common-extensions
  use dylan;
  use system, import: { copy-bytes }, export: { copy-bytes };
  use extensions,
    rename: {on-exit => register-application-exit-function},
    export: {$unsupplied,
             supplied?,
             unsupplied?,
             unsupplied,
             $unfound,
             found?,
             unfound?,
             unfound,
             \assert,
             \debug-assert,
             integer-length,
             decode-float,
             scale-float,
             float-radix,
             float-digits,
             float-precision,
             $single-float-epsilon,
             $double-float-epsilon,
             $extended-float-epsilon,
             $minimum-single-float-exponent,
             $maximum-single-float-exponent,
             $minimum-double-float-exponent,
             $maximum-double-float-exponent,
             $minimum-extended-float-exponent,
             $maximum-extended-float-exponent,
	     false-or,
	     one-of,
	     subclass,
	     <format-string-condition>, <simple-condition>,
	     ignore,
	     key-exists?,
	     difference,
             concatenate!,
	     register-application-exit-function,
	     <stretchy-sequence>,
	     <object-deque>,
	     <stretchy-object-vector>,
             <byte-character>,
             \with-bounds-checks, 
             \without-bounds-checks,
             element-range-error};
  use %Hash-Tables,
    export: {remove-all-keys!};
  use table-extensions,
    export: {<string-table>};
  use transcendentals, import: { logn };
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

    /* Collections */
    //<object-deque>,
    //<stretchy-sequence>,
    //<stretchy-object-vector>,
    //concatenate!,
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
    debug-message,

    /* Types */
    //false-or,
    //one-of,
    //subclass,

    /* Ignoring */
    //ignore,
    ignorable,

    \table-definer,

    /* Converting to and from numbers */
    float-to-string,
    integer-to-string,
    number-to-string,
    string-to-integer,
    string-to-float,

    /* Appliation runtime environment */
    application-name,
    application-filename,
    application-arguments,
    exit-application;
    //register-exit-application-function,

end module;
