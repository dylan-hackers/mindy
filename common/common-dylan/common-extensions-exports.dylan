module: dylan-user

define library common-extensions
  use dylan;
  use melange-support;

  // Only import transcendentals if we have them.
  // XXX - library should be named 'transcendentals'
#if (~compiled-for-solaris)
  use transcendental,
    export: { transcendental };
#endif

  export
    common-extensions,
    finalization,
    simple-io,
    simple-random,
    simple-profiling,
    byte-vector,
    machine-word;
end library;

define module common-extensions
  use dylan;
  use melange-support;
  use extensions,
    rename: {$not-supplied => $unsupplied,
	     on-exit => register-exit-application-function},
    export: {integer-length,
	     false-or,
	     one-of,
	     subclass,
	     <format-string-condition>,
	     $unsupplied,
	     ignore,
	     key-exists?,
	     assert,
	     register-exit-application-function,
	     <stretchy-sequence>,
	     <object-deque>,
	     <stretchy-object-vector>};
  use %Hash-Tables,
    export: {remove-all-keys!};

  export
    /* Numerics */
    //integer-length,

    /* Unsupplied, unfound */
    //$unsupplied,
    $unfound,

    /* Collections */
    //<object-deque>,
    //<stretchy-sequence>,
    //<stretchy-object-vector>,
    concatenate!,
    position,
    //remove-all-keys!,
    difference,
    fill-table!,
    find-element,
    //key-exists?,

    /* Conditions */
    //<format-string-condition>,
    condition-to-string,

    /* Debugging */
    //assert,
    debug-message,
    debug-assert,

    /* Types */
    //false-or,
    //one-of,
    //subclass,

    /* Ignoring */
    //ignore,
    ignorable,

    /* Converting to and from numbers */
    /* UNIMPLEMENTED
    float-to-string,
    integer-to-string,
    number-to-string,
    string-to-integer,
    */

    /* Appliation runtime environment */
    application-name,
    application-filename,
    application-arguments,
    exit-application;
    //register-exit-application-function,

#if (d2c)
  export
    \table-definer,
    \iterate,
    \when;
#endif
end module;

define module finalization
  // XXX - Needs definition. No-op stubs OK.
end module;

define module simple-io
  // XXX - Needs definition.
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

define module machine-word
  // XXX - Needs definition.
end module;

/*
  Stream protocol.
*/

/*
  // Locators.
  export
    <locator>,
    supports-open-locator?,
    open-locator,
    supports-list-locator?,
    list-locator;
*/
