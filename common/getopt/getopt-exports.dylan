library: getopt
module: dylan-user
author: Jeff Dubrule and Ole Tetlie
copyright: LGPL
rcs-header: $Header: /scm/cvs/src/common/getopt/getopt-exports.dylan,v 1.1 1998/09/22 18:49:26 olet Exp $

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
    element,
    element-setter,
    has-option?,
    parse-options;
end module;
