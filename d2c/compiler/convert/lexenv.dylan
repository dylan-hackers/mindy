module: lexenv
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/convert/lexenv.dylan,v 1.3 1995/12/15 16:16:36 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

define class <lexenv> (<object>)
  slot lexenv-bindings :: <list>, init-value: #();
  slot lexenv-policy :: <policy>, init-value: $Default-Policy;
end;

define method initialize (lexenv :: <lexenv>, #next next-method, #key inside)
    => ();
  next-method();
  if (inside)
    lexenv.lexenv-bindings := inside.lexenv-bindings;
    lexenv.lexenv-policy := inside.lexenv-policy;
  end;
end;

define class <body-lexenv> (<lexenv>)
  slot lexenv-handlers :: <fixed-integer>, init-value: 0;
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

define method add-binding (lexenv :: <lexenv>, name :: <name-token>,
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
