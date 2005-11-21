module: lexenv
copyright: see below

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000, 2001  Gwydion Dylan Maintainers
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

// <general-lexenv>  --  Internal
//
// Abstract class providing lookup service for identifiers.
//
define abstract class <general-lexenv> (<object>)
  sealed slot lexenv-bindings :: <list>, init-value: #();
end;

// <top-level-lexenv>  --  Internal
//
// Concrete class providing access to module bindings.
//
define class <top-level-lexenv> (<general-lexenv>)
end;

// <lexenv>  --  Exported
//
// Concrete class providing access to enclosing local and module bindings.
//
define class <lexenv> (<general-lexenv>)
  constant slot lexenv-parent :: <general-lexenv> = make(<top-level-lexenv>), init-keyword: inside:;
  slot lexenv-policy :: <policy>, init-value: $Default-Policy, init-keyword: policy:;
  slot lexenv-method-name :: <name>, init-keyword: method-name:;
end;

define method initialize (lexenv :: <lexenv>, #next next-method, #key inside)
    => ();
  next-method();
  if (inside)
    lexenv.lexenv-policy := inside.lexenv-policy;
    lexenv.lexenv-method-name := inside.lexenv-method-name;
  end if;
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


// <body-lexenv>  --  Exported
//
define class <body-lexenv> (<lexenv>)
  slot lexenv-handlers :: <integer>, init-value: 0;
end;

// <general-binding>  --  Internal
//
// Associates an identifier with a variable.
//
define abstract class <general-binding>(<object>)
  constant slot binding-name :: <identifier-token>, required-init-keyword: name:;
end;

define method print-object (binding :: <general-binding>, stream :: <stream>) => ();
  pprint-fields(binding, stream, name: binding.binding-name);
end;

// <top-level-binding>  --  Exported
//
// A binding that refers to a module variable.
//
define class <top-level-binding>(<general-binding>)
  constant slot binding-var :: <variable>, required-init-keyword: var:;
end;

// <binding>  --  Exported
//
// A binding that refers to a local variable.
//
define class <binding> (<general-binding>)
  constant slot binding-var :: <abstract-variable>, required-init-keyword: var:;
  constant slot binding-type-var :: false-or(<abstract-variable>),
    init-value: #f, init-keyword: type-var:;
end;

// add-binding  --  Exported
//
// Extend the lexical environment.
//
define generic add-binding (lexenv :: <general-lexenv>,
			    name :: <identifier-token>,
			    var :: <object>, #key type-var);

define method add-binding (lexenv :: <top-level-lexenv>,
			   name :: <identifier-token>,
			   var :: <variable>, #key type-var)
 => ();
  let binding = make(<top-level-binding>, name: name, var: var);
  lexenv.lexenv-bindings := pair(binding, lexenv.lexenv-bindings);
end;


define method add-binding (lexenv :: <lexenv>, name :: <identifier-token>,
			   var :: <abstract-variable>, #key type-var)
 => ();
  let binding = make(<binding>, name: name, var: var, type-var: type-var);
  lexenv.lexenv-bindings := pair(binding, lexenv.lexenv-bindings);
end;

// local-binding?  --  Exported
//
// Does the identifier refer to a local binding?
//
define generic local-binding? (lexenv :: <general-lexenv>, name :: <identifier-token>)
 => res :: <boolean>;

define method local-binding? (lexenv :: <general-lexenv>, name :: <identifier-token>)
 => res :: <false>;
end;

define method local-binding? (lexenv :: <lexenv>, name :: <identifier-token>)
 => res :: <boolean>;
  block (return)
    for (binding in lexenv.lexenv-bindings)
      if (same-id?(name, binding.binding-name))
	return(#t);
      end;
    end;
  end;
end;

// find-binding  --  Exported
//
define generic find-binding (lexenv :: <general-lexenv>, name :: <identifier-token>)
 => res :: false-or(<general-binding>);

define method find-binding (lexenv :: <general-lexenv>, name :: <identifier-token>)
 => res :: false-or(<general-binding>);
  block (return)
    for (binding in lexenv.lexenv-bindings)
      if (same-id?(name, binding.binding-name))
	return(binding);
      end;
    end;
  end;
end;

define method find-binding (lexenv :: <top-level-lexenv>, name :: <identifier-token>, #next search-my-bindings)
 => res :: false-or(<top-level-binding>);
  search-my-bindings()
  | begin
      let var = find-variable(name.id-name);
      if (var)
      	add-binding(lexenv, name, var);
      	search-my-bindings();
      end if;
    end;
end;

define method find-binding (lexenv :: <lexenv>, name :: <identifier-token>, #next search-my-bindings)
 => res :: false-or(<general-binding>);
  search-my-bindings() | find-binding(lexenv.lexenv-parent, name);
end;

// Seals for file lexenv.dylan

// <top-level-lexenv> -- subclass of <general-lexenv>
define sealed domain make(singleton(<top-level-lexenv>));
define sealed domain initialize(<top-level-lexenv>);
// <lexenv> -- subclass of <general-lexenv>
define sealed domain make(singleton(<lexenv>));
define sealed domain initialize(<lexenv>);
// <body-lexenv> -- subclass of <lexenv>
define sealed domain make(singleton(<body-lexenv>));
define sealed domain initialize(<body-lexenv>);
// <binding> -- subclass of <general-binding>
define sealed domain make(singleton(<binding>));
define sealed domain initialize(<binding>);
// <top-level-binding> -- subclass of <general-binding>
define sealed domain make(singleton(<top-level-binding>));
define sealed domain initialize(<top-level-binding>);
