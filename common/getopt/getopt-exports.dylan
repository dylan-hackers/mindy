library: getopt
module: dylan-user
author: Jeff Dubrule and Ole Tetlie
copyright: LGPL
rcs-header: $Header: /scm/cvs/src/common/getopt/getopt-exports.dylan,v 1.3 1998/09/25 12:48:45 igor Exp $

define library getopt
  use dylan;
  export getopt;
end library;

define module getopt
  use dylan;
  use %Hash-Tables;
  export
    <option-table>,
    <option>,
    add-option,
    has-option?,
    parse-options;
end module;
