module: definitions
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/defns.dylan,v 1.14 1996/02/12 01:57:30 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


// <definition> -- exported.
//
// Abstract class that all definitions inherit from.  A definition is the
// compilers handle on some run-time value.  In addition to the obvious things,
// definitions exist for things like the type of a variable (when it isn't a
// compile-time constant).
// 
define open primary abstract class <definition>
    (<annotatable>, <identity-preserving-mixin>)
  //
  // The name this is the definition for.
  slot defn-name :: <name>, required-init-keyword: name:;
  //
  // The library making the definition.  Might be different than the library
  // burried inside the defn-name, because the defn-name might refer to a
  // variable pulled in from some other library.
  slot defn-library :: <library>, required-init-keyword: library:;
end;

define method print-object (defn :: <definition>, stream :: <stream>) => ();
  pprint-fields(defn, stream, name: defn.defn-name);
end;

define constant $definition-slots
  = list(info, #f, info-setter,
	 defn-name, name:, #f,
	 defn-library, library:, #f);
	 

define open generic defn-type (defn :: <definition>)
    => res :: false-or(<ctype>);

// ct-value -- exported.
//
// Returns the compile-time value for some definition, of #f if that
// definition doesn't have a compile time value.
//
define open generic ct-value (defn :: <definition>)
    => res :: false-or(<ct-value>);

define method ct-value (defn :: <definition>)
    => res :: <false>;
  #f;
end;


// install-transformers -- exported.
//
// Called by the variable system to install the transformers for a definition.
// 
define open generic install-transformers
    (defn :: <definition>, transformers :: <list>)
    => ();
//
// By default, we do nothing.
// 
define method install-transformers
    (defn :: <definition>, transformers :: <list>)
    => ();
end method install-transformers;



define open abstract class <implicit-definition> (<definition>)
end;

define open abstract class <abstract-constant-definition> (<definition>)
end;


define open primary abstract class <function-definition>
    (<abstract-constant-definition>)
  //
  // The signature.
  slot %function-defn-signature :: type-union(<signature>, <function>),
    setter: function-defn-signature-setter, init-keyword: signature:;
  //
  // #t if this definition requires special handling at loadtime.  Can be
  // because of something non-constant in the signature or in the case of
  // methods, can be because the generic is hairy.  Fill in during
  // finalization.
  slot function-defn-hairy? :: <boolean>,
    init-value: #f, init-keyword: hairy:;
  //
  // The ctv for this function.  #f if we can't represent it (because the
  // function is hairy) and #"not-computed-yet" if we haven't computed it yet.
  slot function-defn-ct-value
    :: type-union(<ct-function>, one-of(#f, #"not-computed-yet")),
    init-value: #"not-computed-yet";
  //
  // The FER transformers for this function.  Gets initialized from the
  // variable.
  slot function-defn-transformers :: <list>, init-value: #();
  //
  // True if we can drop calls to this function when the results isn't used
  // because there are no side effects.
  slot function-defn-flushable? :: <boolean>,
    init-value: #f, init-keyword: flushable:;
  //
  // True if we can move calls to this function around with impunity because
  // the result depends on nothing but the value of the arguments.
  slot function-defn-movable? :: <boolean>,
    init-value: #f, init-keyword: movable:;
end;

define method function-defn-signature
    (defn :: <function-definition>) => res :: <signature>;
  let sig-or-func = defn.%function-defn-signature;
  if (instance?(sig-or-func, <function>))
    defn.function-defn-signature := sig-or-func();
  else
    sig-or-func;
  end;
end;

define method install-transformers
    (defn :: <function-definition>, transformers :: <list>)
    => ();
  defn.function-defn-transformers
    := union(transformers, defn.function-defn-transformers);
end method install-transformers;


define open primary abstract class <class-definition> 
    (<abstract-constant-definition>)
end class;


// {check,make}-syntax-table-additions -- exported.
//
// These functions are called to check and make changes to a modules syntax
// table when some definition becomes accessable.
//
define open generic check-syntax-table-additions
    (table :: <table>, defn :: false-or(<definition>), name :: <symbol>)
    => ();
define open generic make-syntax-table-additions
    (table :: <table>, defn :: false-or(<definition>), name :: <symbol>)
    => ();


define method check-syntax-table-additions
    (table :: <table>, defn :: <false>, name :: <symbol>) => ();
end;

define method check-syntax-table-additions
    (table :: <table>, defn :: <definition>, name :: <symbol>)
    => ();
  // For almost all definitions name shouldn't have any special syntax.
  unless (merge-category(table, name, <name-token>))
    error("Inconsistent syntax for %=", name);
  end;
end;


define method make-syntax-table-additions
    (table :: <table>, defn :: <false>, name :: <symbol>) => ();
end;

define method make-syntax-table-additions
    (table :: <table>, defn :: <definition>, name :: <symbol>)
    => ();
  table[name] := merge-category(table, name, <name-token>);
end;
