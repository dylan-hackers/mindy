Module: dylan-user

//=========================================================================
//  Parser utilities
//=========================================================================
//  These modules implement <source-location> and <parse-condition>, two
//  classes useful in many different kinds of parser programs.
//
//  This code was inherited from Melange (and parts of it date back to
//  d2c). You may notice some cruft--this can be rationalized and removed
//  if you're careful about it.

define library parser-utilities
  use dylan;
  use streams;
  use format;
  use standard-io;

  export
    source-locations,
    parse-conditions,
    multistring-match;
end library;

define module source-locations
  use dylan;
  use extensions;
  use streams;
  use format;
  use standard-io;
  export
    source-location,
    <source-location>,
    describe-source-location,
    <unknown-source-location>,
    <file-source-location>,
      source-file,
      source-line;
end module source-locations;

define module parse-conditions
  use dylan;
  use extensions;
  use source-locations;
  use streams;
  use format;
  use standard-io;
  export
    *show-parse-progress?*,
    <parse-condition>,
    <simple-parse-error>,
    <simple-parse-warning>,
    <parse-progress-report>,
    push-default-parse-context,
    pop-default-parse-context,
    // \with-default-parse-context,
    parse-error,
    parse-warning,
    parse-progress-report;
end module;

define module multistring-match
  use dylan;
  use extensions;
  export
#if (~mindy)
    multistring-checker-definer, multistring-positioner-definer,
#endif
    make-multistring-positioner, make-multistring-checker,
   
    // XXX - These cope with a problem in d2c, whereby the functions that a
    //       macro uses must be exported as well, if the user of the macro
    //       is in a different library.  For status on this bug, check bug #162
    //       at <http://www.randomhacks.com/dylan-cgi/bugs>.
    compile-multistring, check-multistring, find-multistring
end module multistring-match;
