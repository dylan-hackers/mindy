module: define-macros
rcs-header: $Header: /scm/cvs/src/d2c/compiler/convert/defmacro.dylan,v 1.4 2002/07/27 12:20:22 housel Exp $
copyright: see below


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

// <define-macro-tlf>
//
// The TLF for define macro forms.
// 
define class <define-macro-tlf> (<simple-define-tlf>)
  //
  // Make the definition required.
  required keyword defn:;
end class <define-macro-tlf>;

define sealed domain make (singleton(<define-macro-tlf>));
define sealed domain initialize (<define-macro-tlf>);

define method print-message
    (tlf :: <define-macro-tlf>, stream :: <stream>) => ();
  format(stream, "Define Macro %s", tlf.tlf-defn.defn-name);
end method print-message;

// process-top-level-form{<macro-call-parse>} -- method on imported GF.
//
// Macro-expand the macro call and try again.
// 
define method process-top-level-form (form :: <macro-call-parse>) => ();
  process-top-level-form(macro-expand(form));
end method process-top-level-form;

// process-top-level-form{<definition-macro-call-parse>} -- method on imported GF.
//
// Parse the expansion as a source-record, which will call
// process-top-level-from() on each of the resulting parsed forms.
// 
//
define method process-top-level-form
    (form :: <definition-macro-call-parse>)
 => ();
  parse-source-record(macro-expansion-tokenizer(form));
end method process-top-level-form;

// process-top-level-form{<define-macro-parse>} -- method on imported GF.
//
// Process a define macro form.  Most of the real work is done by
// make(<macro-definition>).
// 
define method process-top-level-form (defmacro :: <define-macro-parse>)
    => ();
  let defn = make(<macro-definition>, module: *Current-Module*,
		  library: *Current-Library*, defmacro: defmacro);
  note-variable-definition(defn);
  add!(	*Top-Level-Forms*,
        make(<define-macro-tlf>,	defn: defn, source-location: defn.source-location));
end method process-top-level-form;

// finalize-top-level-form{<define-macro-tlf>} -- method on imported GF.
//
define method finalize-top-level-form (tlf :: <define-macro-tlf>) => ();
  // Nothing to do.
end method finalize-top-level-form;

// convert-top-level-form{<define-macro-tlf>} -- method on imported GF.
//
define method convert-top-level-form
    (builder :: <fer-builder>, tlf :: <define-macro-tlf>) => ();
  // Nothing to do.
end method convert-top-level-form;

