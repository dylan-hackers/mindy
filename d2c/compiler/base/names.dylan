module: names
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/names.dylan,v 1.8 1996/02/09 21:04:19 ram Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

// This stuff is mostly used for remembering where definitions came from for
// compiler debugging, generating function debug info, etc.

define abstract class <name> (<object>)
end;

define class <basic-name> (<name>)
  slot name-symbol :: <symbol>,
    required-init-keyword: symbol:;
  slot name-module :: <module>,
    required-init-keyword: module:;
end;

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

// see misc-dump for basic-name dumping (module ordering problem.)

define class <type-cell-name> (<name>)
  slot type-cell-name-base :: <basic-name>,
    required-init-keyword: base:;
end;

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
