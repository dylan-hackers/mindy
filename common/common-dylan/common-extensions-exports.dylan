module: dylan-user

define library common-extensions
  use dylan;

  // Only import transcendentals if we have them.
#if (~compiled-for-solaris)
  use transcendental,
    export: { transcendental };
#endif

  export
    common-extensions,
    finalization,
    simple-debugging,
    simple-io,
    simple-random,
    machine-word,
    byte-vector;
end library;

define module common-extensions
  use dylan;
  use extensions,
    import: {integer-length,
	     false-or,
	     one-of,
	     subclass,
	     <format-string-condition>,
	     $not-supplied => $unsupplied,
	     ignore,
             key-exists?,
	     assert,
	     exit => exit-application,
	     on-exit => register-exit-application-function},
    export: all;

  // Unsupplied, unfound.
  export
    // $unsupplied,
    $unfound;

  // Locators.
  export
    <locator>,
    supports-open-locator?,
    open-locator,
    supports-list-locator?,
    list-locator;

/* UNIMPLEMENTED:
  export
    <object-deque>,
    <object-set>,
    <set>,
    <stretchy-sequence>,
    <stretchy-object-vector>,
    concatenate!,
    position,
    // key-exists?,
    remove-all-keys!,
    difference,
    fill-table,
    find-element,
*/

  export
    // <format-string-condition>,
    condition-to-string;

  export
    // assert,
    // UNIMPLEMENTED: debug-message,
    debug-assert;

  export
    // ignore,
    ignorable;

/* UNIMPLEMENTED:
  export
    float-to-string,
    integer-to-string,
    number-to-string,
    string-to-integer;
*/

#if (d2c)
  export
    // UNIMPLEMENTED: \table-definer,
    \iterate,
    \when;
#endif
end module;

define module finalization
  // XXX - Needs definition. No-op stubs OK.
end module;

define module simple-random
  // XXX - Needs definition.
end module;

define module machine-word
  // XXX - Needs definition.
end module;

define module byte-vector
  use extensions,
    export: {<byte>,
	     <byte-vector>};
end module;
