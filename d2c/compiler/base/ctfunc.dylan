module: compile-time-functions
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/ctfunc.dylan,v 1.1 1995/06/04 01:22:06 wlott Exp $
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
end;

define method ct-value-cclass (ctv :: <ct-method>) => res :: <cclass>;
  specifier-type(#"<method>");
end;
