module: names
rcs-header: $Header: /scm/cvs/src/mindy/dbmc/names.dylan,v 1.1 2003/02/28 04:46:20 housel Exp $
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
define method name-unique? (name :: <name>) => res :: singleton(#f);
  #f;
end method;


define class <basic-name> (<name>)
  constant slot name-symbol :: <symbol>,
    required-init-keyword: symbol:;
  constant slot name-module :: <module>,
    required-init-keyword: module:;
end;

define sealed domain make (singleton(<basic-name>));

/*
define method print-object (name :: <basic-name>, stream :: <stream>) => ();
  pprint-fields(name, stream,
		symbol: name.name-symbol,
		module: name.name-module);
end;
*/

define method print-message (name :: <basic-name>, stream :: <stream>) => ();
  format(stream, "%s", name.name-symbol);
end;

define function id-name (token :: <identifier-token>) => res :: <basic-name>;
  make(<basic-name>, symbol: token.token-symbol, module: token.token-module);
end;

define method name-unique? (name :: <basic-name>) => t :: singleton(#t);
  #t;
end method;


// A derived name is used to name a helper definition or function needed by the
// definition of the base name.  There is a fixed set of "how" values
// describing the use of the derived name.

define class <derived-name> (<name>)
  constant slot derived-name-how
    :: one-of(#"type-cell", #"general-entry", #"generic-entry",
	      #"callback-entry",
    	      #"discriminator",
    	      #"deferred-evaluation", #"init-function", #"setter", #"getter",
	      #"maker", #"key-defaulter",
	      #"class-meta", #"each-subclass-meta"),
    required-init-keyword: how:;
  constant slot derived-name-base :: <name>, required-init-keyword: base:;
end;

define sealed domain make (singleton(<derived-name>));

define method name-unique? (name :: <derived-name>) => res :: <boolean>;
  member?(name.derived-name-how,
  	  #[#"type-cell", #"general-entry", #"generic-entry", #"discriminator",
	    #"maker", #"key-defaulter", #"setter", #"getter",
	    #"class-meta", #"each-subclass-meta"])
    & name-unique?(name.derived-name-base);
end method;

/*
define method print-object (name :: <derived-name>, stream :: <stream>)
    => ();
  pprint-fields(name, stream, how: name.derived-name-how,
  		base: name.derived-name-base);
end;
*/

define method print-message (name :: <derived-name>, stream :: <stream>)
    => ();
  format(stream, "%s for %s", name.derived-name-how, name.derived-name-base);
end;


// An <internal-name> is used to name bare methods that are embedded inside
// some other named thing.  The symbol is the local name.
//
define class <internal-name> (<name>)
  constant slot internal-name-symbol :: <symbol>, required-init-keyword: symbol:;
  constant slot internal-name-base :: <name>, required-init-keyword: base:;
end class;

define sealed domain make (singleton(<internal-name>));

/*
define method print-object (name :: <internal-name>, stream :: <stream>)
    => ();
  pprint-fields(name, stream, symbol: name.internal-name-symbol,
  		base: name.internal-name-base);
end;
*/

define method print-message (name :: <internal-name>, stream :: <stream>)
    => ();
  format(stream, "%s internal %s", name.internal-name-base,
  	 name.internal-name-symbol);
end;


// <anonymous-name> is used to "name" top-level forms when we haven't seen any
// useful name yet.

define class <anonymous-name> (<name>)
  constant slot anonymous-name-location :: <source-location>,
    required-init-keyword: location:;
end class;

define sealed domain make (singleton(<anonymous-name>));

/*
define method print-object (name :: <anonymous-name>, stream :: <stream>)
    => ();
  pprint-fields(name, stream, location: name.anonymous-name-location);
end;
*/

define method print-message (name :: <anonymous-name>, stream :: <stream>)
    => ();
  format(stream, "form at %=", name.anonymous-name-location);
end;


define class <method-name> (<name>)
  constant slot method-name-generic-function :: <basic-name>,
    required-init-keyword: generic-function:;
  constant slot method-name-specializers :: <sequence>,
    required-init-keyword: specializers:;
end;

define sealed domain make (singleton(<method-name>));

/*
define method print-object (name :: <method-name>, stream :: <stream>) => ();
  pprint-fields(name, stream,
		generic-function: name.method-name-generic-function,
		specializers: name.method-name-specializers);
end;
*/

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

