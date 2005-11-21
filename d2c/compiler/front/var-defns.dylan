module: variable-definitions


//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
// All rights reserved.
// 
// Use and copying of this software and preparation of derivative
// works based on this software are permitted, including commercial
// use, provided that the following conditions are observed:
// 
// 1. This copyright notice must be retained in full on any copies
//    and on appropriate parts of any derivative works.
// 2. Documentation (paper or online) accompanying any system that
//    incorporates this software, or any part of it, must acknowledge
//    the contribution of the Gwydion Project at Carnegie Mellon
//    University, and the Gwydion Dylan Maintainers.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
// Also, see http://www.gwydiondylan.org/ for updates and documentation. 
//
//======================================================================

// definition class definitions

define open abstract class <bindings-definition> (<definition>)
  //
  // The <ctype> for this definition if it is a compile-time constant.  Filled
  // in by finalize-top-level-form.
  slot defn-type :: false-or(<ctype>), init-keyword: type:;
  //
  // The initial value (or only value for constants) if it is a compile-time
  // value, #f if it isn't compile-time computable, and #"not-computed-yet"
  // if we haven't figured it out yet.  Filled in either by ct-value on a
  // constant or by finalize-top-level-form.
  slot %defn-init-value
    :: type-union(<ct-value>, one-of(#"not-computed-yet", #f)),
    init-value: #"not-computed-yet", init-keyword: value:,
    setter: defn-init-value-setter;
end;

define method defn-init-value (defn :: <bindings-definition>)
    => res :: false-or(<ct-value>);
  let res = defn.%defn-init-value;
  if (res == #"not-computed-yet")
    error("Asking for %s's init-value before it is computed.", defn.defn-name);
  else
    res;
  end if;
end method defn-init-value;


define class <variable-definition>
    (<bindings-definition>, <abstract-variable-definition>)
  //
  // The <constant-definition> for the type if the type isn't a compile-time
  // constant.  Filled in by finalize-top-level-form.
  slot var-defn-type-defn :: false-or(<abstract-constant-definition>),
    init-value: #f, init-keyword: type-defn:;
end;

define sealed domain make(singleton(<variable-definition>));
define sealed domain initialize(<variable-definition>);

// definition-kind{<variable-definition>} -- method on exported GF
//
define method definition-kind
    (defn :: <variable-definition>) => kind :: <byte-string>;
  "variable";
end method definition-kind;
