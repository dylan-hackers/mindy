library: get-options
module: dylan-user
author: Jeff Dubrule, Eric Kidd, and Ole Tetlie
copyright: ???
rcs-header: $Header: /scm/cvs/src/common/getopt/getopt-exports.dylan,v 1.4 1998/10/29 04:12:56 igor Exp $

define library get-options
  use dylan;
  use format-out; //test
  use streams;
  use standard-io;
  export get-options;
end library;

define module get-options
  use dylan;
  use format-out; //test
  use streams; //test
  use standard-io; //test
  export
    <option-list-parser>,
    // User methods:
    add-option-parser,
    parse-option-list,
    find-option-value,
    <flag-option-parser>,


    // <option-parser-list>'s API for <option-parser>'s use
    get-option-parameter,
    get-optional-option-parameter,

    <option-parser>,
    option-allows-parameters?,
    option-names,
    option-value,
    parse-option
end module;
