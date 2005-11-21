module: compile-time-functions
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

/*

ct-value {abstract} (external)
    ct-function [annotatable, identity-preserving-mixin] {abstract}
        ct-raw-function
        ct-callback-function
        ct-generic-function [eql-ct-value] {abstract}
            ct-open-generic
            ct-sealed-generic
        ct-method
        ct-accessor-method
    ct-entry-point [identity-preserving-mixin]

*/


// A ct-function is a kind of compile-time value used to represent a function.
// ct-functions contain various linkage-related information needed to call the
// function, but don't reference the FER for the function (e.g. the
// <function-literal>.)  This information is used both by the backend and by
// the heap builder.
//
define abstract class <ct-function> 
    (<ct-value>, <annotatable>, <identity-preserving-mixin>)
  //
  // <name> object describing what this function is used for.  Used for
  // debugging, to generate the C name, etc.
  constant slot ct-function-name :: <name>,
    required-init-keyword: name:;
  //
  // The signature for this function.
  constant slot ct-function-signature :: <signature>,
    required-init-keyword: signature:;
  //
  // The definition this <ct-function> came from, or #f if it didn't come
  // from a definition.
  slot ct-function-definition :: false-or(<function-definition>),
    init-value: #f, init-keyword: definition:;
  //
  // List of the types for the closure vars for this function.  Only local
  // functions can have closure vars.
  slot ct-function-closure-var-types :: <list>,
    init-value: #(), init-keyword: closure-var-types:;

  slot has-general-entry? :: <boolean>, init-value: #f, init-keyword: #"general-entry?";
end;

define sealed method make (class == <ct-function>, #rest keys, #key)
    => ctv :: <ct-function>;
  apply(make, <ct-raw-function>, keys);
end method make;

define method print-object (ctv :: <ct-function>, stream :: <stream>) => ();
  pprint-fields(ctv, stream, name: ctv.ct-function-name);
end;

define method print-message (ctv :: <ct-function>, stream :: <stream>) => ();
  print-message(ctv.ct-function-name, stream);
end;

define constant $ct-function-dump-slots =
  list(info, #f, info-setter,
       ct-function-name, name:, #f,
       ct-function-signature, signature:, #f,
       ct-function-definition, definition:, ct-function-definition-setter,
       ct-function-closure-var-types, closure-var-types:, #f,
       has-general-entry?, general-entry?:, #f);


define class <ct-raw-function> (<ct-function>)
end;

define method ct-value-cclass (ctv :: <ct-raw-function>) => res :: <cclass>;
  specifier-type(#"<raw-function>");
end;

add-make-dumper(#"ct-function", *compiler-dispatcher*, <ct-raw-function>,
		$ct-function-dump-slots,
		load-external: #t);


define class <ct-callback-function> (<ct-function>)
  slot has-callback-entry? :: <boolean>, init-value: #f,
    init-keyword: #"callback-entry?";
end;

define method ct-value-cclass (ctv :: <ct-callback-function>)
    => res :: <cclass>;
  specifier-type(#"<callback-function>");
end;

add-make-dumper(#"ct-callback-function", *compiler-dispatcher*,
		<ct-callback-function>,
		concatenate($ct-function-dump-slots,
			    list(has-callback-entry?, callback-entry?:, #f)),
		load-external: #t);


define abstract class <ct-generic-function> (<ct-function>, <eql-ct-value>)
  constant slot ct-generic-sealed?, required-init-keyword: #"sealed?";
end;

define class <ct-open-generic> (<ct-generic-function>) end class;
define class <ct-sealed-generic> (<ct-generic-function>) end class;

define method make
    (class == <ct-generic-function>, #rest rest, #key sealed?)
 => (result :: <ct-generic-function>);
  if (sealed?)
    apply(make, <ct-sealed-generic>, rest);
  else
    apply(make, <ct-open-generic>, rest);
  end if;
end method make;

define method ct-value-cclass (ctv :: <ct-generic-function>)
    => res :: <cclass>;
  specifier-type(#"<generic-function>");
end;

// Add-make-dumper requires direct classes -- bletch.  If we
// accidentally fail to specify both concrete classes the dumper
// quietly omits the data, with unpleasant results.

add-make-dumper(#"ct-sealed-generic", *compiler-dispatcher*,
		<ct-sealed-generic>,
		concatenate($ct-function-dump-slots,
			    list(ct-generic-sealed?, #"sealed?", #f)),
		load-external: #t);

add-make-dumper(#"ct-open-generic", *compiler-dispatcher*,
		<ct-open-generic>,
		concatenate($ct-function-dump-slots,
			    list(ct-generic-sealed?, #"sealed?", #f)),
		load-external: #t);


define class <ct-method> (<ct-function>)
  //
  // True if this method is hidden inside a generic function so we don't
  // need to generate a general entry for it.
  slot ct-method-hidden? :: <boolean>,
    init-value: #f, init-keyword: hidden:;
  slot has-generic-entry? :: <boolean>, init-value: #f, init-keyword: #"generic-entry?";
end;

define method ct-value-cclass (ctv :: <ct-method>) => res :: <cclass>;
  specifier-type(#"<method>");
end;

define constant $ct-method-dump-slots
  = concatenate($ct-function-dump-slots,
		list(ct-method-hidden?, hidden:, #f,
		     has-generic-entry?, generic-entry?:, #f));

add-make-dumper(#"ct-method", *compiler-dispatcher*, <ct-method>,
		$ct-method-dump-slots, load-external: #t);


define class <ct-accessor-method> (<ct-method>)
  //
  // The slot being accessed.
  constant slot ct-accessor-method-slot-info :: <slot-info>,
    required-init-keyword: slot-info:;
  //
  // The shared accessor standin, or #f if we need a custom one.
  slot ct-accessor-standin :: false-or(<ct-function>),
    init-keyword: standin:, init-value: #f;
end;

define method ct-value-cclass (ctv :: <ct-accessor-method>) => res :: <cclass>;
  specifier-type(#"<accessor-method>");
end;

add-make-dumper
  (#"ct-accessor-method", *compiler-dispatcher*, <ct-accessor-method>,
   concatenate($ct-method-dump-slots,
	       list(ct-accessor-method-slot-info, slot-info:, #f,
		    ct-accessor-standin, standin:, #f)),
   load-external: #t);



// A <ct-entry-point> is a way that we can get our hands on the <raw-pointer>
// for particular entry-point functions (as opposed to actually referencing the
// function object.)  Among other things, this is used to describe the initial
// contents of the function object.
//
define class <ct-entry-point> (<ct-value>, <identity-preserving-mixin>)
  //
  // The function this is an entry point for.
  constant slot ct-entry-point-for :: <ct-function>,
    required-init-keyword: for:;
  //
  // The kind of entry point.
  constant slot ct-entry-point-kind
    :: one-of(#"main",
	      #"general",
	      #"generic",
	      #"callback"),
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

add-make-dumper(#"ct-entry-point", *compiler-dispatcher*, <ct-entry-point>,
		list(ct-entry-point-for, for:, #f,
		     ct-entry-point-kind, kind:, #f),
		dump-side-effect:
		  method (ep :: <ct-entry-point>, buf :: <dump-buffer>) => ();
		    signal("Yes, a <ct-entry-point> was actually dumped.");
		  end method,
		load-external: #t);

// Seals for file ctfunc.dylan

// <ct-raw-function> -- subclass of <ct-function>
define sealed domain make(singleton(<ct-raw-function>));
// <ct-open-generic> -- subclass of <ct-generic-function>
define sealed domain make(singleton(<ct-open-generic>));
// <ct-sealed-generic> -- subclass of <ct-generic-function>
define sealed domain make(singleton(<ct-sealed-generic>));
// <ct-method> -- subclass of <ct-function>
define sealed domain make(singleton(<ct-method>));
// <ct-accessor-method> -- subclass of <ct-method>
define sealed domain make(singleton(<ct-accessor-method>));
// <ct-entry-point> -- subclass of <ct-value>, <identity-preserving-mixin>
define sealed domain make(singleton(<ct-entry-point>));
define sealed domain initialize(<ct-entry-point>);
