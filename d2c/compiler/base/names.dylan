module: names
rcs-header: $Header: /scm/cvs/src/d2c/compiler/base/names.dylan,v 1.1 1998/05/03 19:55:30 andreas Exp $
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

// This stuff is mostly used for remembering where definitions came from for
// compiler debugging, generating function debug info, etc.

define abstract class <name> (<object>)
end;

define sealed domain make (singleton(<name>));
define sealed domain initialize (<name>);

define generic name-unique? (name :: <name>) => res :: <boolean>;


// True if this name is guaranteed to be distinguishable from any other name
// solely on the basis of its contents.  This true of module variable names and
// names uniquely derived from them.
//
define method name-unique? (name :: <name>) => res :: <false>;
  #f;
end method;


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

define method name-unique? (name :: <basic-name>) => res :: <true>;
  #t;
end method;

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


// A derived name is used to name a helper definition or function needed by the
// definition of the base name.  There is a fixed set of "how" values
// describing the use of the derived name.

define class <derived-name> (<name>)
  slot derived-name-how
    :: one-of(#"type-cell", #"general-entry", #"generic-entry",
    	      #"discriminator",
    	      #"deferred-evaluation", #"init-function", #"setter", #"getter",
	      #"maker"
	      ),
    required-init-keyword: how:;
  slot derived-name-base :: <name>, required-init-keyword: base:;
end;

define sealed domain make (singleton(<derived-name>));

define method name-unique? (name :: <derived-name>) => res :: <boolean>;
  member?(name.derived-name-how,
  	  #(#"type-cell", #"general-entry", #"generic-entry", #"discriminator",
	    #"maker", #"setter", #"getter"))
    & name-unique?(name.derived-name-base);
end method;

define method print-object (name :: <derived-name>, stream :: <stream>)
    => ();
  pprint-fields(name, stream, how: name.derived-name-how,
  		base: name.derived-name-base);
end;

define method print-message (name :: <derived-name>, stream :: <stream>)
    => ();
  format(stream, "%s for %s", name.derived-name-how, name.derived-name-base);
end;

add-make-dumper(#"derived-name", *compiler-dispatcher*, <derived-name>,
  list(derived-name-base, base:, #f,
       derived-name-how, how:, #f)
);

// An <internal-name> is used to name bare methods that are embedded inside
// some other named thing.  The symbol is the local name.
//
define class <internal-name> (<name>)
  slot internal-name-symbol :: <symbol>, required-init-keyword: symbol:;
  slot internal-name-base :: <name>, required-init-keyword: base:;
end class;

define sealed domain make (singleton(<internal-name>));

define method print-object (name :: <internal-name>, stream :: <stream>)
    => ();
  pprint-fields(name, stream, symbol: name.internal-name-symbol,
  		base: name.internal-name-base);
end;

define method print-message (name :: <internal-name>, stream :: <stream>)
    => ();
  format(stream, "%s internal %s", name.internal-name-base,
  	 name.internal-name-symbol);
end;

add-make-dumper(#"internal-name", *compiler-dispatcher*, <internal-name>,
  list(internal-name-symbol, symbol:, #f,
       internal-name-base, base:, #f)
);


// <anonymous-name> is used to "name" top-level forms when we haven't seen any
// useful name yet.

define class <anonymous-name> (<name>)
  slot anonymous-name-location :: <source-location>,
    required-init-keyword: location:;
end class;

define sealed domain make (singleton(<anonymous-name>));

define method print-object (name :: <anonymous-name>, stream :: <stream>)
    => ();
  pprint-fields(name, stream, location: name.anonymous-name-location);
end;

define method print-message (name :: <anonymous-name>, stream :: <stream>)
    => ();
  format(stream, "form at %=", name.anonymous-name-location);
end;

add-make-dumper(#"anonymous-name", *compiler-dispatcher*, <anonymous-name>,
  list(anonymous-name-location, location:, #f)
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
      write(stream, ", ");
    end;
    print-message(spec, stream);
  end;
  format(stream, "}");
end;

add-make-dumper(#"method-name", *compiler-dispatcher*, <method-name>,
  list(method-name-generic-function, generic-function:, #f,
       method-name-specializers, specializers:, #f)
);
