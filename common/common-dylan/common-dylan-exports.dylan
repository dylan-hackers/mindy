module: dylan-user

define library common-dylan
  use dylan, 
    export: { dylan, machine-words };
  use threads, export: { threads };

  use melange-support;
  use table-extensions;
  use random;
  use transcendental,
     import: { transcendental => transcendentals },
     export: all;

  export
    common-dylan,
    common-extensions,
    streams-protocol,
    locators-protocol,
    finalization,
    simple-random,
    simple-profiling,
    // simple-debugging,
    simple-io,
    byte-vector,
    functional-objects-extras,
    functional-extensions;
end library;

define module functional-extensions
  use dylan;
  use extensions, exclude: { position }, export: { element-range-error };
  use Magic, import: {%element, %element-setter};
  use common-extensions, import: { find-element };
  export 
    find-value;
  export
    element-range-check,
    \with-bounds-checks,
    \without-bounds-checks;
end module;

define module c-support
  use dylan;
  use extensions;
  use melange-support;

  export
    application-argc,
    application-argv,
    cpu-time;
end module c-support;

define module finalization
  // XXX - Needs definition. No-op stubs OK.
end module;

define module simple-io
  create format-out;
end module;

define module simple-random
  use random,
    import: { <random-state> => <random>, random },
    export: all;
end module;

define module simple-profiling
  create \profiling,
         \profiling-keywords,   // ###
         \profiling-results,    // ###
         <profiling-state>,
         start-profiling-type,
         stop-profiling-type,
         profiling-type-result;
end module simple-profiling;

define module simple-debugging
  use Extensions,
    import: { \assert, \debug-assert, debug-message },
    export: all;
end module;

define module byte-vector
  use extensions, export: { <byte>, <byte-vector> };
  create
    byte-vector-fill,
    byte-vector-ref,
    byte-vector-ref-setter,
    copy-bytes;
end module;

define module common-extensions
  use dylan;
  use extensions,
    rename: {$not-supplied => $unsupplied,
             <never-returns> => <bottom>,
             on-exit => register-application-exit-function},
    export: {$unsupplied,
             \assert,
             \debug-assert,
             debug-message,
             <bottom>,
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
	     difference,
             concatenate!,
	     register-application-exit-function,
	     <stretchy-sequence>,
	     <object-deque>,
	     <stretchy-object-vector>,
             <byte-character>,
             <byte-vector>};
  use %Hash-Tables,
    export: {remove-all-keys!};
  use table-extensions,
    export: {<string-table>};
  use transcendentals, import: { logn };
  use c-support;
  use simple-profiling,
    export: { \profiling, 
	      profiling-type-result };

  create
    <closable-object>,
    close,
    <stream>,
    true?,
    false?,
    position,
    split,
    fill-table!,
    find-element,
    machine-word-to-string,
    string-to-machine-word,
    condition-to-string,
    format-to-string;

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
    //concatenate!,
    //position,
    //remove-all-keys!,
    //difference,
    //fill-table!,
    //find-element,
    //key-exists?,

    /* Conditions */
    //<format-string-condition>,
    //condition-to-string,

    /* Debugging */
    //debug-message,

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
    number-to-string,
    string-to-integer,
    string-to-float,

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
    \%iterate-aux,              // ###
    \%iterate-param-helper,     // ###
    \%iterate-value-helper;     // ###
#endif
end module;

define module common-dylan
  use dylan,
    export: all;
  use extensions,
    import: { <general-integer> => <abstract-integer> },
    export: all;
  use common-extensions,
    export: all;
end module;
  
define module locators-protocol
  create <locator>;
  create supports-open-locator?,
         open-locator,
         supports-list-locator?,
         list-locator;
end module locators-protocol;

define module streams-protocol
  use common-extensions,
    import: { <stream>, close },
    export: all;
  // Conditions
  create <stream-error>,
           stream-error-stream,
         <end-of-stream-error>,
           <incomplete-read-error>,
             stream-error-sequence,
             stream-error-count,
           <incomplete-write-error>,
             stream-error-count;
  // Opening streams
  create open-file-stream;
  // Reading from streams
  create read-element,
         unread-element,
         peek,
         read,
         read-into!,
         discard-input,
         stream-input-available?,
         stream-contents,
         stream-contents-as;
  // Writing to streams
  create write-element,
         write,
         force-output,
         wait-for-io-completion,
         synchronize-output,
         discard-output;
  // Querying streams
  create stream-open?,
         stream-element-type,
         stream-at-end?,
         stream-size;
  // Positioning streams
  create <positionable-stream>,
         stream-position,
         stream-position-setter,
         adjust-stream-position;
end module streams-protocol;

define module functional-objects-extras
  use common-extensions,
    import: { <closable-object> },
    export: all;
  use cheap-io,
    import: { puts => write-console },
    export: all;
  create <locator-defaults>,
         <server-locator>,
	 <physical-locator>;
end module functional-objects-extras;

define module common-dylan-internals
  use common-dylan;
  use extensions;
  use functional-objects-extras;
  use system, import: { copy-bytes => %copy-bytes, vector-elements-address };
  use magic, import: { %element, %element-setter };
  use introspection, rename: { subclass-of => subclass-class };
  use machine-words;
  use melange-support;
  use c-support;
  use simple-io;
  use byte-vector;
  use simple-profiling;
  use locators-protocol;
  use streams-protocol;
end module common-dylan-internals;
