module: names
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/names.dylan,v 1.10 1996/03/17 00:30:07 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

// This stuff is mostly used for remembering where definitions came from for
// compiler debugging, generating function debug info, etc.

define abstract class <name> (<object>)
end;

define sealed domain make (singleton(<name>));
define sealed domain initialize (<name>);


define class <basic-name> (<name>)
  slot name-symbol :: <symbol>,
    required-init-keyword: symbol:;
  slot name-module :: <module>,
    required-init-keyword: module:;
end;

define sealed domain make (singleton(<basic-name>));

define method print-object (name :: <basic-name>, stream :: <stream>) => ();
  pprint-fields(name, stream,
		symbol: name.name-symbol,
		module: name.name-module);
end;

define method print-message (name :: <basic-name>, stream :: <stream>) => ();
  format(stream, "%s", name.name-symbol);
end;

define method id-name (token :: <identifier-token>) => res :: <basic-name>;
  make(<basic-name>, symbol: token.token-symbol, module: token.token-module);
end;


add-make-dumper(#"basic-name", *compiler-dispatcher*, <basic-name>,
		list(name-module, #f, #f,
		     name-symbol, #f, #f),
		dumper-only: #t);

define constant load-basic-name
    = method (state :: <load-state>) => res :: <basic-name>;
	let modu = load-object-dispatch(state);
	let obj-name = load-object-dispatch(state);
	assert-end-object(state);
	make(<basic-name>, symbol: obj-name, module: modu);
      end method;

add-od-loader(*compiler-dispatcher*, #"basic-name", load-basic-name);


define class <type-cell-name> (<name>)
  slot type-cell-name-base :: <basic-name>,
    required-init-keyword: base:;
end;

define sealed domain make (singleton(<type-cell-name>));

define method print-object (name :: <type-cell-name>, stream :: <stream>)
    => ();
  pprint-fields(name, stream, base: name.type-cell-name-base);
end;

define method print-message (name :: <type-cell-name>, stream :: <stream>)
    => ();
  format(stream, "type cell for %s", name.type-cell-name-base);
end;

add-make-dumper(#"type-cell-name", *compiler-dispatcher*, <type-cell-name>,
  list(type-cell-name-base, base:, #f)
);


define class <method-name> (<name>)
  slot method-name-generic-function :: <basic-name>,
    required-init-keyword: generic-function:;
  slot method-name-specializers :: <sequence>,
    required-init-keyword: specializers:;
end;

define sealed domain make (singleton(<method-name>));

define method print-object (name :: <method-name>, stream :: <stream>) => ();
  pprint-fields(name, stream,
		generic-function: name.method-name-generic-function,
		specializers: name.method-name-specializers);
end;

define method print-message (name :: <method-name>, stream :: <stream>) => ();
  let gf-name = name.method-name-generic-function;
  format(stream, "%s{", gf-name.name-symbol);
  for (spec in name.method-name-specializers,
       first? = #t then #f)
    unless (first?)
      write(", ", stream);
    end;
    print-message(spec, stream);
  end;
  format(stream, "}");
end;

add-make-dumper(#"method-name", *compiler-dispatcher*, <method-name>,
  list(method-name-generic-function, generic-function:, #f,
       method-name-specializers, specializers:, #f)
);


define class <generated-name> (<name>)
  slot generated-name-description :: <string>,
    required-init-keyword: description:;
end class <generated-name>;

define sealed domain make (singleton(<generated-name>));

define method print-object
    (name :: <generated-name>, stream :: <stream>) => ();
  pprint-fields(name, stream, description: name.generated-name-description);
end;

define method print-message
    (name :: <generated-name>, stream :: <stream>) => ();
  write(name.generated-name-description, stream);
end method print-message;

add-make-dumper
  (#"generated-name", *compiler-dispatcher*, <generated-name>,
   list(generated-name-description, description:, #f));
