library: parse-arguments
module: dylan-user
author:  Eric Kidd
copyright: Copyright 1998 Eric Kidd

//======================================================================
//
//  Copyright (c) 1998 Eric Kidd
//  All rights reserved.
// 
//  Use and copying of this software and preparation of derivative
//  works based on this software are permitted, including commercial
//  use, provided that the following conditions are observed:
// 
//  1. This copyright notice must be retained in full on any copies
//     and on appropriate parts of any derivative works. (Other names
//     and years may be added, so long as no existing ones are removed.)
// 
//  This software is made available "as is".  Neither the authors nor
//  Carnegie Mellon University make any warranty about the software,
//  its performance, or its conformity to any specification.
// 
//  Bug reports, questions, comments, and suggestions should be sent by
//  E-mail to the Internet address "gd-bugs@gwydiondylan.org".
//
//======================================================================

define library parse-arguments
  use dylan;
  use table-extensions;

#if (~mindy)
  // for argument-parser-definer
  use streams;
  use format;
#endif

  export
    parse-arguments,
    option-parser-protocol;
end library;

// Only used when defining new option-parser subclasses.
define module option-parser-protocol
  create
    // <argument-list-parser>
      argument-tokens-remaining?,
      get-argument-token,
      peek-argument-token,

    // <option-parser>
      short-option-names, short-option-names-setter,
      long-option-names, long-option-names-setter,
      option-default-value, option-default-value-setter,
      option-might-have-parameters?, option-might-have-parameters?-setter,
      option-value-setter,
    reset-option-parser,
    parse-option,

    <negative-option-parser>,
    negative-option?,

    <argument-token>,
      token-value,
    <regular-argument-token>,
    <option-token>,
    <short-option-token>,
      tightly-bound-to-next-token?, // XXX - not implemented fully
    <long-option-token>,
    <equals-token>,

    usage-error;
end module;

// Used by most programs.
define module parse-arguments
  use dylan;
  use extensions;
  use table-extensions;
  use option-parser-protocol;

  export
    <argument-list-parser>,
      regular-arguments,
    add-option-parser,
    add-option-parser-by-type,
    parse-arguments,
    option-parser-by-long-name,
    option-present?-by-long-name,
    option-value-by-long-name,

    <option-parser>,
      option-present?,
      option-value,

    <simple-option-parser>,
    <parameter-option-parser>,
    <repeated-parameter-option-parser>,
    <optional-parameter-option-parser>,
    <keyed-option-parser>;

#if (~mindy)
  use streams;
  use format;

  export
    argument-parser-definer,
    \defargparser-rec,
    \defargparser-aux,
    \defargparser-class,
    \defargparser-init,
    \defargparser-accessors,
    \defargparser-synopsis;
#endif
end module parse-arguments;
