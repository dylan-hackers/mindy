module: compile-time-functions
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/ctfunc.dylan,v 1.3 1995/06/06 02:12:33 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.

define class <ct-function> (<ct-value>, <annotatable>)
  //
  // Some string useful for describing this function.  Used only for printing
  // and error messages.
  slot ct-function-name :: <string>,
    required-init-keyword: name:;
  //
  // The signature for this function.
  slot ct-function-signature :: <signature>,
    required-init-keyword: signature:;
  //
  // The definition this <ct-function> came from, or #f if it didn't come
  // from a definition.
  slot ct-function-definition :: false-or(<function-definition>),
    init-value: #f, init-keyword: definition:;
end;

define method print-object (ctv :: <ct-function>, stream :: <stream>) => ();
  pprint-fields(ctv, stream, name: ctv.ct-function-name);
end;

define method print-message (ctv :: <ct-function>, stream :: <stream>) => ();
  write(ctv.ct-function-name, stream);
end;

define method ct-value-cclass (ctv :: <ct-function>) => res :: <cclass>;
  specifier-type(#"<function>");
end;

define class <ct-generic-function> (<ct-function>, <eql-ct-value>)
end;

define method ct-value-cclass (ctv :: <ct-generic-function>)
    => res :: <cclass>;
  specifier-type(#"<generic-function>");
end;

define class <ct-method> (<ct-function>)
  //
  // List of the types for the closure vars for this method.  Only local
  // methods can have closure vars.
  slot ct-method-closure-var-types :: <list>,
    init-value: #(), init-keyword: closure-var-types:;
  //
  // True if this method is hidden inside a generic function so we don't
  // need to generate a general entry for it.
  slot ct-method-hidden? :: <boolean>,
    init-value: #f, init-keyword: hidden:;
end;

define method ct-value-cclass (ctv :: <ct-method>) => res :: <cclass>;
  specifier-type(#"<method>");
end;


define class <ct-entry-point> (<ct-value>, <annotatable>)
  //
  // The function this is an entry point for.
  slot ct-entry-point-for :: <ct-function>,
    required-init-keyword: for:;
  //
  // The kind of entry point.
  slot ct-entry-point-kind :: one-of(#"main", #"general", #"generic"),
    required-init-keyword: kind:;
end;

define method print-object (ctv :: <ct-entry-point>, stream :: <stream>) => ();
  pprint-fields(ctv, stream,
		for: ctv.ct-entry-point-for,
		kind: ctv.ct-entry-point-kind);
end;

define method print-message
    (ctv :: <ct-entry-point>, stream :: <stream>) => ();
  format(stream, "%s entry point for %s",
	 ctv.ct-entry-point-kind,
	 ctv.ct-entry-point-for.ct-function-name);
end;

define method ct-value-cclass (ctv :: <ct-entry-point>) => res :: <cclass>;
  specifier-type(#"<raw-pointer>");
end;

