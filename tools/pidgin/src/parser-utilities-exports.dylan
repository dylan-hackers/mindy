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
    parse-conditions;
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
