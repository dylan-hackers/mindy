module: compile-time-functions
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/ctfunc.dylan,v 1.14 1996/07/12 01:08:06 bfw Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.

define abstract class <ct-function> 
    (<ct-value>, <annotatable>, <identity-preserving-mixin>)
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
  write(stream, ctv.ct-function-name);
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



define abstract class <ct-generic-function> (<ct-function>, <eql-ct-value>)
  slot ct-generic-sealed?, required-init-keyword: #"sealed?", setter: #f;
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
  slot ct-accessor-method-slot-info :: <slot-info>,
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



define class <ct-entry-point> (<ct-value>, <identity-preserving-mixin>)
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
