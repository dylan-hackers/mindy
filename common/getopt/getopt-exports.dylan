library: getopt
module: dylan-user
author: Jeff Dubrule and Ole Tetlie
copyright: LGPL
rcs-header: $Header: /scm/cvs/src/common/getopt/getopt-exports.dylan,v 1.2 1998/09/23 17:23:04 andreas Exp $

define library getopt
  use dylan;
  export getopt;
end library;

define module getopt
  use dylan;
  export
    <option-table>,
    <option>,
    add-option,
    has-option?,
    parse-options;
end module;
