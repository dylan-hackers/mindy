module: lexenv
rcs-header: $Header: /scm/cvs/src/d2c/compiler/convert/lexenv.dylan,v 1.1 1998/05/03 19:55:36 andreas Exp $
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

define class <lexenv> (<object>)
  slot lexenv-bindings :: <list>, init-value: #();
  slot lexenv-policy :: <policy>, init-value: $Default-Policy;
  slot lexenv-method-name :: <name>, init-keyword: method-name:;
end;

define method initialize (lexenv :: <lexenv>, #next next-method, #key inside)
    => ();
  next-method();
  if (inside)
    lexenv.lexenv-bindings := inside.lexenv-bindings;
    lexenv.lexenv-policy := inside.lexenv-policy;
    lexenv.lexenv-method-name := inside.lexenv-method-name;
  end;
end;

// lexenv-for-tlf  --  Exported
//
// Shorthand function.
// Make a new empty lexenv, initializing its "method name" to an anonymous name
// derived from the TLF's source location.
//
define function lexenv-for-tlf (tlf :: <top-level-form>) => res :: <lexenv>;
  make(<lexenv>,
       method-name: make(<anonymous-name>, location: tlf.source-location));
end function;


define class <body-lexenv> (<lexenv>)
  slot lexenv-handlers :: <integer>, init-value: 0;
end;

define class <binding> (<object>)
  slot binding-name :: <identifier-token>, required-init-keyword: name:;
  slot binding-var :: <abstract-variable>, required-init-keyword: var:;
  slot binding-type-var :: false-or(<abstract-variable>),
    init-value: #f, init-keyword: type-var:;
end;

define method print-object (binding :: <binding>, stream :: <stream>) => ();
  pprint-fields(binding, stream, name: binding.binding-name);
end;

define method add-binding (lexenv :: <lexenv>, name :: <identifier-token>,
			   var :: <abstract-variable>, #key type-var)
    => ();
  let binding = make(<binding>, name: name, var: var, type-var: type-var);
  lexenv.lexenv-bindings := pair(binding, lexenv.lexenv-bindings);
end;

define method find-binding (lexenv :: <lexenv>, name :: <identifier-token>)
    => res :: false-or(<binding>);
  block (return)
    for (binding in lexenv.lexenv-bindings)
      if (same-id?(name, binding.binding-name))
	return(binding);
      end;
    end;
  end;
end;

// Seals for file lexenv.dylan

// <lexenv> -- subclass of <object>
define sealed domain make(singleton(<lexenv>));
define sealed domain initialize(<lexenv>);
// <body-lexenv> -- subclass of <lexenv>
define sealed domain make(singleton(<body-lexenv>));
// <binding> -- subclass of <object>
define sealed domain make(singleton(<binding>));
define sealed domain initialize(<binding>);
