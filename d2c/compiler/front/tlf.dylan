module: top-level-forms
rcs-header: $Header: /scm/cvs/src/d2c/compiler/front/tlf.dylan,v 1.1 1998/05/03 19:55:27 andreas Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
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
//    University.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports, questions, comments, and suggestions should be sent by
// E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
//
//======================================================================

define variable *Top-Level-Forms* = make(<stretchy-vector>);

define open primary abstract class <top-level-form> (<source-location-mixin>)
end;

define open primary abstract class <define-tlf> (<top-level-form>)
end;

define open primary abstract class <simple-define-tlf> (<define-tlf>)
  slot tlf-defn :: <definition>, init-keyword: defn:;
end;

define method print-object (tlf :: <simple-define-tlf>, stream :: <stream>)
    => ();
  pprint-fields(tlf, stream, name: tlf.tlf-defn.defn-name);
end;

// finalize-top-level-form -- exported.
//
// Called by the main driver on each top level form in *Top-Level-Forms*
// after everything has been parsed.
//
define open generic finalize-top-level-form (tlf :: <top-level-form>) => ();

// convert-top-level-form
//
define open generic convert-top-level-form
    (builder :: <fer-builder>, tlf :: <top-level-form>)
    => ();


// Specific top level forms.

define open abstract class <define-generic-tlf> (<simple-define-tlf>)
  //
  // Make the definition required.
  required keyword defn:;
end class <define-generic-tlf>;


define open abstract class <define-method-tlf> (<simple-define-tlf>)
end class <define-method-tlf>;

define method print-message
    (tlf :: <define-method-tlf>, stream :: <stream>) => ();
  format(stream, "Define Method %s", tlf.tlf-defn.defn-name);
end;


define open abstract class <define-bindings-tlf> (<define-tlf>)
  constant slot tlf-required-defns :: <simple-object-vector>,
    required-init-keyword: required-defns:;
  constant slot tlf-rest-defn :: false-or(<bindings-definition>),
    required-init-keyword: rest-defn:;
end class <define-bindings-tlf>;


define class <define-class-tlf> (<simple-define-tlf>)
  //
  // Make the definition required.
  required keyword defn:;
  //
  // Stretchy vector of <init-function-definition>s.
  constant slot tlf-init-function-defns :: <stretchy-vector>
    = make(<stretchy-vector>);
end;

define method print-message
    (tlf :: <define-class-tlf>, stream :: <stream>) => ();
  format(stream, "Define Class %s", tlf.tlf-defn.defn-name);
end;



define class <magic-interal-primitives-placeholder> (<top-level-form>)
end;

define method print-message
    (tlf :: <magic-interal-primitives-placeholder>, stream :: <stream>) => ();
  write(stream, "Magic internal primitives.");
end;



// Dump stuff.

// If name's var isn't visible outside this library, don't bother dumping the
// definition.
//
define method dump-od
    (tlf :: <simple-define-tlf>, state :: <dump-state>) => ();
  let defn = tlf.tlf-defn;
  if (name-inherited-or-exported?(defn.defn-name))
    dump-simple-object(#"define-binding-tlf", state, defn);
  end if;
end;

add-od-loader(*compiler-dispatcher*, #"define-binding-tlf",
	      method (state :: <load-state>) => res :: <definition>;
		let defn = load-sole-subobject(state);
		note-variable-definition(defn);
		defn;
	      end);

// Seals for file tlf.dylan

// <define-class-tlf> -- subclass of <simple-define-tlf>
define sealed domain make(singleton(<define-class-tlf>));
define sealed domain initialize(<define-class-tlf>);
// <magic-interal-primitives-placeholder> -- subclass of <top-level-form>
define sealed domain make(singleton(<magic-interal-primitives-placeholder>));
define sealed domain initialize(<magic-interal-primitives-placeholder>);
