module: define-macros
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/convert/defmacro.dylan,v 1.1 1996/03/17 00:57:25 wlott Exp $
copyright: Copyright (c) 1996  Carnegie Mellon University
	   All rights reserved.


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
  add!(*Top-Level-Forms*, make(<define-macro-tlf>, defn: defn));
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

