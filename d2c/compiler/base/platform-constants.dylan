module: platform-constants
author: Peter Housel
rcs-header: $Header: /scm/cvs/src/d2c/compiler/base/platform-constants.dylan,v 1.1 2003/05/25 15:39:16 housel Exp $
copyright: see below

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998-2003  Gwydion Dylan Maintainers
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

// <builtin-constant-definition> -- internal
//
define class <builtin-constant-definition> (<abstract-constant-definition>)
  constant slot ct-value :: <literal>, required-init-keyword: value:;
end class;

// defn-type{<builtin-constant-definition>} -- method on exported GF
//
define method defn-type
    (defn :: <builtin-constant-definition>)
 => (res :: <ctype>);
  defn.ct-value.ct-value-cclass;
end method;

// definition-kind{<builtin-constant-definition>} -- method on exported GF
//
define method definition-kind
    (defn :: <builtin-constant-definition>) => kind :: <byte-string>;
  "built-in constant";
end method definition-kind;


// add-bootstrap-constant -- internal
//
// Add the given name as a an exported constant in the bootstrap
// module.  Note that variables defined in this way can not be
// re-exported from the Dylan library, since they don't have
// corresponding top-level forms.
//
define method add-bootstrap-constant
    (sym :: <symbol>, value :: <object>)
 => ();
  add-bootstrap-export(sym);

  let name = make(<basic-name>, symbol: sym, module: $Bootstrap-Module);
  let defn = make(<builtin-constant-definition>,
                  name: name,
                  library: $Dylan-Library,
                  dynamic: #f,
                  value: as(<ct-value>, value));
  note-variable-definition(defn);
end method;


// define-platform-constants -- exported.
//
// Define platform-specific constant values in the Bootstrap module.
//
define method define-platform-constants(platform :: <platform>) => ();
  add-bootstrap-constant(#"$platform-fixed-integer-bits",
                         platform.platform-integer-length);

  add-bootstrap-constant(#"$platform-single-float-mantissa-digits",
                         platform.single-mantissa-digits);
  add-bootstrap-constant(#"$platform-double-float-mantissa-digits",
                         platform.double-mantissa-digits);
  add-bootstrap-constant(#"$platform-extended-float-mantissa-digits",
                         platform.long-double-mantissa-digits
                           | platform.double-mantissa-digits);

  add-bootstrap-constant(#"$platform-minimum-single-float-exponent",
                         platform.minimum-single-float-exponent);
  add-bootstrap-constant(#"$platform-maximum-single-float-exponent",
                         platform.maximum-single-float-exponent);
  add-bootstrap-constant(#"$platform-minimum-double-float-exponent",
                         platform.minimum-double-float-exponent);
  add-bootstrap-constant(#"$platform-maximum-double-float-exponent",
                         platform.maximum-double-float-exponent);
  add-bootstrap-constant(#"$platform-minimum-extended-float-exponent",
                         platform.minimum-long-double-float-exponent
                           | platform.minimum-double-float-exponent);
  add-bootstrap-constant(#"$platform-maximum-extended-float-exponent",
                         platform.maximum-long-double-float-exponent
                           | platform.maximum-double-float-exponent);
end method;
