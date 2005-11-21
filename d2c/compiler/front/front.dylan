Module: front
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

assignment
    fer-assignment {abstract}
        set-assignment
	let-assignment

operation
    primitive
    abstract-call {abstract}
	known-call
	general-call {abstract}
	    unknown-call
	    mv-call
            delayed-optimization-call
	error-call
    prologue
    self-tail-call
    module-var-access
	module-var-ref
	module-var-set
    slot-access {abstract}
	slot-ref
	slot-set
    truly-the
    instance?
    catch
    throw
    make-catcher
    disable-catcher

variable-info
    debug-named-info {abstract}
        lexical-var-info [source-location-mixin]
	values-cluster-info
	local-var-info

leaf
    constant {abstract}
	literal-constant
	definition-constant-leaf
        uninitialized-value

    abstract-function-literal {abstract}
	function-literal [queueable-mixin]
	    method-literal
        exit-function

function-region
    fer-function-region [queueable-mixin, annotatable]
	lambda

component
    fer-component

body-region
    unwind-protect-region

object
    environment [annotatable]
    tail-set [annotatable]

*/


// Basic front end (FE) SSA classes:

// The <fer-assignment> adds policy information.  This can't be associated
// with expressions, since different uses of the expression can have different
// policies.
//
define abstract class <fer-assignment> (<assignment>)
  //
  // The policy environment this <operation> was converted in.
  constant slot policy :: <policy>, required-init-keyword: policy:;
end class;

// Marks a create-point for a lexical variable.  We allow there to be more than
// one (useful for representing loops.)  Let-assignments should only be used on
// lexical variables.
//
define class <let-assignment> (<fer-assignment>)
  //
  // The next link in the chain of lets in this component.
  constant slot let-next :: false-or(<let-assignment>), required-init-keyword: next:;
end class;

// An assignment that doesn't create a new "variable" (that would side-effect
// any indirect value cell.)  Set-assignments can be used on temporary and
// global variables as well as lexical ones.
//
define class <set-assignment> (<fer-assignment>)
end class;


// The <primitive> operation represents some built in primitive operation.
//
define class <primitive> (<operation>)
  constant slot primitive-name :: <symbol>, required-init-keyword: name:;
  slot primitive-info :: <primitive-info>, init-keyword: info:;
end;

define method initialize (prim :: <primitive>, #next next-method, #key) => ();
  next-method();
  let info = primitive-info-or-lose(prim.primitive-name);
  prim.primitive-info := info;
  let type = info.priminfo-result-type;
  prim.derived-type := values-type-intersection(type, prim.derived-type);
end;


// The <abstract-call> operation represents any function call.
// In Operands, the called function is first, followed by the args.
//
define abstract class <abstract-call> (<operation>)
  constant slot ct-source-location :: false-or(<literal-constant>) = #f,
    init-keyword: ct-source-location:;
end class;

// A call where the function is known and all hairy argument stuff has been
// expanded away into positional arguments, hence is now a candidate for
// inlining and other magic.
// 
define class <known-call> (<abstract-call>)
end;

define abstract class <general-call> (<abstract-call>)
  constant slot use-generic-entry? :: <boolean>,
    required-init-keyword: use-generic-entry:;
end;

// An arbitrary function call where the function call might be computed and the
// argument syntax might be incorrect.
//
define class <unknown-call> (<general-call>)
end class;

// In a MV-Call, there is one argument which must be a values cluster.  This
// values cluster is spread out to form the actual arguments to the called
// function.  If the resulting actual parameters are not syntactically legal
// (e.g. not enough required args, etc.), then an error will be signalled.
//
define class <mv-call> (<general-call>)
end class;

// A call that is known to be in error.  Basically, the same as <unknown-call>
// but we've given up trying to improve it.
//
define class <error-call> (<abstract-call>)
end;

// A call that we shouldn't optimize prematurely. Again, this is basically
// the same as an <unknown-call>, but we don't want to improve it until we
// have enough information available.
//
// We create these when simplifying inline functions, and change them back
// into unknown calls after the functions have been inlined.
//
define class <delayed-optimization-call> (<general-call>)
end;

// A prologue is used to represent the incoming arguments to a function.
// 
define class <prologue> (<operation>)
  slot function :: <fer-function-region>, required-init-keyword: function:;
  slot preferred-names :: false-or(<vector>), init-value: #f;
end;

define abstract class <module-var-access> (<operation>)
  //
  // The definition for the variable being set.
  constant slot variable :: <definition>, required-init-keyword: var:;
end;  

define class <module-var-ref> (<module-var-access>)
end;

define constant no-values-ctype
  = method () => no-values-ctype :: <values-ctype>;
      make-values-ctype(#(), #f);
    end method;

// A set operation is used to change a global variable.
// 
define class <module-var-set> (<module-var-access>)
  //
  // Set operations return nothing.
  inherited slot derived-type, init-function: no-values-ctype;
end;

define abstract class <slot-access> (<operation>)
  constant slot slot-info :: <slot-info>, required-init-keyword: slot-info:;
end;

define sealed domain make (singleton(<slot-access>));
define sealed domain initialize (<slot-access>);

define abstract class <slot-ref> (<slot-access>)
end;

define sealed domain make (singleton(<slot-ref>));

define class <heap-slot-ref> (<slot-ref>)
end;

define sealed domain make (singleton(<heap-slot-ref>));

define class <data-word-ref> (<slot-ref>)
end;

define sealed domain make (singleton(<data-word-ref>));

define class <heap-slot-set> (<slot-access>)
  inherited slot derived-type, init-function: no-values-ctype;
end;

define sealed domain make (singleton(<heap-slot-set>));

define class <truly-the> (<operation>)
  constant slot guaranteed-type :: <ctype>, required-init-keyword: guaranteed-type:;
end;

define method initialize
    (op :: <truly-the>, #next next-method, #key guaranteed-type) => ();
  next-method();
  op.derived-type := guaranteed-type.ctype-extent;
end;

define class <instance?> (<operation>)
  inherited slot derived-type, init-function: boolean-ctype;
  constant slot type :: <ctype>, required-init-keyword: type:;
end;

define class <nlx-operation> (<operation>)
  constant slot nlx-info :: <nlx-info>, required-init-keyword: nlx-info:;
end;

define class <catch> (<nlx-operation>)
end;

define method initialize
    (catch :: <catch>, #next next-method, #key nlx-info) => ();
  next-method();
  nlx-info.nlx-catch := catch;
end;

define class <throw> (<nlx-operation>)
  inherited slot derived-type, init-function: empty-ctype;
  slot throw-next :: false-or(<throw>);
end;

define method initialize
    (op :: <throw>, #next next-method, #key nlx-info) => ();
  next-method();
  op.throw-next := nlx-info.nlx-throws;
  nlx-info.nlx-throws := op;
end;

define class <make-catcher> (<nlx-operation>)
  inherited slot derived-type = specifier-type(#"<catcher>").ctype-extent;
end;

define method initialize
    (op :: <make-catcher>, #next next-method, #key nlx-info) => ();
  next-method();
  nlx-info.nlx-make-catcher := op;
end;

define class <disable-catcher> (<nlx-operation>)
  inherited slot derived-type, init-function: no-values-ctype;
  slot disable-catcher-next :: false-or(<disable-catcher>);
end;

define method initialize
    (op :: <disable-catcher>, #next next-method, #key nlx-info)
    => ();
  next-method();
  op.disable-catcher-next := nlx-info.nlx-disable-catchers;
  nlx-info.nlx-disable-catchers := op;
end;



// Constants and variables:

// <Constant> objects represent known constant values.
//
define abstract class <constant> (<leaf>)
end class;

// <Literal-Constant> is a constant with a manifest compile-time value.
//
define class <literal-constant> (<constant>)

  // The value of the constant.
  constant slot value :: <ct-value>, required-init-keyword: value:;
end class;

// Represents a constant module var.  We point to the <module-var> for the
// actual info, and only represent the references for the data-flow framework.
//
define class <definition-constant-leaf> (<constant>)
  constant slot const-defn :: <definition>, required-init-keyword: const-defn:;
end class;


// Used as a placeholder for some value that isn't actually initialized.
// For example, unsupplied keywords with hairy defaults, and uninitialized
// slots.  The uninitialized-value cannot reliably be tested against, because
// in some representations, every bit pattern has an interpretation.  It is
// up to the user of the uninitialized-value to know when the value is
// initialized and when it isn't.
// 
define class <uninitialized-value> (<constant>)
end;


// Assignable variables:

// We don't have FER-specific subclasses of <abstract-variable>, instead we
// have subclasses of <variable-info>.

// This "variable" represents an arbitrary number of values which are the
// result an expression in a tail position or where multiple values are
// expected.
//

define abstract class <debug-named-info> (<variable-info>)
  constant slot debug-name :: <symbol>, required-init-keyword: debug-name:;
end class;

define class <values-cluster-info> (<debug-named-info>)
end class;

define class <local-var-info> (<debug-named-info>)
end class;

define class <lexical-var-info> (<debug-named-info>, <source-location-mixin>)
end class;



/// Function literals:


define abstract class <abstract-function-literal> (<leaf>)
  
  // All function literals are subclasses of <function>.
  inherited slot derived-type = function-ctype().ctype-extent;
  
end;

define constant <function-visibility>
  = one-of(#"global", #"local", #"closure", #"deleted");

define class <function-literal>
    (<abstract-function-literal>, <queueable-mixin>)

  // An indication of what kind of references to this function can exist:
  //
  // Local: all references are explicit dependents of the leaf.
  // Global: some other references might exist, so assume the worst.
  // Deleted: no references will ever exist from now on.
  // Closure: like local, but references are hidden behind
  //          make-closure calls.
  //
  slot visibility :: <function-visibility>,
    init-value: #"local", init-keyword: visibility:;

  // The signature of this function.
  constant slot signature :: <signature>,
    required-init-keyword: signature:;

  // The <ct-function> for this literal, if global.
  slot ct-function :: false-or(<ct-function>),
    init-value: #f, init-keyword: ct-function:;

  // The function-region for the main body of code.
  constant slot main-entry :: <fer-function-region>,
    required-init-keyword: main-entry:;

  // This is the general-case used when we can't statically analyze a call, due
  // to either the function or the arguments not being sufficiently constant.
  // It checks the argument syntax and type restrictions, then calls the main
  // entry.  No general entry is needed if we never generate a user-visible
  // Dylan object for this function, which happens with local functions and
  // sealed methods.  This is also false in methods that are themselves
  // entry-points.
  // 
  slot general-entry :: false-or(<fer-function-region>), init-value: #f;
end class;


define class <method-literal> (<function-literal>)

  // The generic entry is somewhat similar to the general-entry, but doesn't
  // have to deal with error cases that have been picked off by the generic
  // dispatch mechanism.  It takes the required arguments followed by
  // next-method info, followed by an argument context pointer and an argument
  // count.
  //
  // The required arguments are guaranteed to be of the correct type,
  // and more args will only be supplied if they are syntactically legal.
  // Keyword arg legality has already been done; unrecognized keywords are
  // quietly ignored.  This can also be false when not needed.
  //
  slot generic-entry :: false-or(<fer-function-region>), init-value: #f;
end class;

define class <callback-literal> (<function-literal>)
  slot callback-entry :: false-or(<fer-function-region>), init-value: #f;
end class;



// An <exit-function> is a magical function literal that represents
// the exit-function for a block in situations where a non-local exit is
// possible.  It depends-on the catcher for that block so that we don't think
// we can flush the catcher until after we've flushed all copies of the
// exit function.
//
define class <exit-function> (<abstract-function-literal>, <dependent-mixin>)
  constant slot nlx-info :: <nlx-info>, required-init-keyword: nlx-info:;
end class;

define method initialize
    (exit-fun :: <exit-function>, #next next-method, #key nlx-info) => ();
  next-method();
  nlx-info.nlx-exit-function := exit-fun;
end;


// FE region classes:

// <fer-function-region>
//
// A <fer-function-region> adds the FER specific information to control-flow's
// <function-region>.
//
define class <fer-function-region>
    (<function-region>, <queueable-mixin>, <annotatable>)

  // Name describing where this function came from, or #f if unknown.
  slot name :: false-or(<name>), init-value: #f, init-keyword: name:;

  // The prologue operation for this function.  The results of the prologue
  // are the arguments to the function.
  slot prologue :: <prologue>;

  // List of the argument types.
  slot argument-types :: <list>, required-init-keyword: argument-types:;

  // The result type of this function.
  slot result-type :: <values-ctype>,
    init-function: wild-ctype, init-keyword: result-type:;

  // A guess about the result type, used during optimistic type inference.
  slot guessed-result-type :: <values-ctype>, 
    init-function: empty-ctype;

  slot hidden-references? :: <boolean>,
    init-value: #f, init-keyword: hidden-references:;

  // The function-literal this region is the main-entry for.  Used to find
  // all the local references to this function.  Initialized when the function
  // literal is made, or stays #f if no literal is made (e.g. for xeps).
  slot literal :: false-or(<function-literal>) = #f;

  // The block self tail calls should exit to.  #f if we haven't inserted it
  // yet (i.e. haven't found any self tail calls yet).
  slot self-call-block :: false-or(<block-region>), init-value: #f;

  // temporary variables as introduced by self-tail-call conversion
  slot self-tail-call-temps :: <list>, init-value: #();

  // The calling convention used for this function.
  slot calling-convention :: one-of(#"standard", #"callback") = #"standard",
    init-keyword: calling-convention:;
end;

define method initialize
    (func :: <fer-function-region>, #next next-method, #key) => ();
  next-method();
  func.prologue
    := make(<prologue>, function: func, depends-on: #f,
	    // ### The depends-on: shouldn't be needed, but Mindy is broken.
	    derived-type:
	      make-values-ctype(func.argument-types, #f).ctype-extent);
end;

// <lambda>
//
// A <lambda> is a kind of <fer-function-region> that can close over values.
// It can only be used as the main-entry for local function literals.
//
define class <lambda> (<fer-function-region>)

  // The structure which represents the environment that this Function's
  // variables are allocated in.
  constant slot environment :: <environment> = make(<environment>);

end;


define class <fer-component> (<component>, <identity-preserving-mixin>)

  // All the function literals mentioned in this component.
  constant slot all-function-literals :: <stretchy-vector> = make(<stretchy-vector>);

  // Chain of all the lets (through let-next) in this component.  Used by
  // environment analysis.  Deleted lets are left in this chain, so beware.
  slot all-lets :: false-or(<let-assignment>), init-value: #f;

  // String that is some sort of name for the code in this component.
  constant slot name :: <byte-string>, init-value: "<unknown>", init-keyword: name:;

  // Table mapping <ct-value>s to <literal-constant>s.
  constant slot constants :: <object-table> = make(<object-table>);

end class;


// An <unwind-protect-region> represents the nesting of unwind protects.
// Any exit from inside the uwp-region to outside the uwp-region must
// run the cleanup code.
//
define class <unwind-protect-region> (<body-region>)
  //
  // The function that does the cleanup.
  constant slot uwp-region-cleanup-function :: <function-literal>,
    required-init-keyword: cleanup-function:;
end class;


// Misc drek:

// The Environment structure represents the result of Environment analysis.
//
define class <environment> (<annotatable>)
  slot closure-vars :: false-or(<closure-var>), init-value: #f;
end class;

define class <closure-var> (<object>)
  constant slot original-var :: <ssa-variable>, required-init-keyword: original:;
  constant slot copy-var :: <ssa-variable>, required-init-keyword: copy:;
  slot closure-next :: false-or(<closure-var>), required-init-keyword: next:;
end;

define class <nlx-info> (<identity-preserving-mixin>)
  slot nlx-hidden-references? :: <boolean>, init-value: #f,
      init-keyword: hidden-references:;
  slot nlx-catch :: false-or(<catch>), init-value: #f, init-keyword: catch:;
  slot nlx-make-catcher :: false-or(<make-catcher>), init-value: #f,
    init-keyword: make-catcher:;
  slot nlx-exit-function :: false-or(<exit-function>), init-value: #f,
    init-keyword: exit-function:;
  slot nlx-disable-catchers :: false-or(<disable-catcher>), init-value: #f,
    init-keyword: disable-catchers:;
  slot nlx-throws :: false-or(<throw>), init-value: #f,
    init-keyword: throws:;
end;



// Seals for file front.dylan

// <let-assignment> -- subclass of <fer-assignment>
define sealed domain make(singleton(<let-assignment>));
// <set-assignment> -- subclass of <fer-assignment>
define sealed domain make(singleton(<set-assignment>));
// <primitive> -- subclass of <operation>
define sealed domain make(singleton(<primitive>));
define sealed domain initialize(<primitive>);
// <known-call> -- subclass of <abstract-call>
define sealed domain make(singleton(<known-call>));
// <unknown-call> -- subclass of <general-call>
define sealed domain make(singleton(<unknown-call>));
// <mv-call> -- subclass of <general-call>
define sealed domain make(singleton(<mv-call>));
// <error-call> -- subclass of <abstract-call>
define sealed domain make(singleton(<error-call>));
// <delayed-optimization-call> -- subclass of <general-call>
define sealed domain make(singleton(<delayed-optimization-call>));
// <prologue> -- subclass of <operation>
define sealed domain make(singleton(<prologue>));
define sealed domain initialize(<prologue>);
// <module-var-ref> -- subclass of <module-var-access>
define sealed domain make(singleton(<module-var-ref>));
// <module-var-set> -- subclass of <module-var-access>
define sealed domain make(singleton(<module-var-set>));
// <truly-the> -- subclass of <operation>
define sealed domain make(singleton(<truly-the>));
define sealed domain initialize(<truly-the>);
// <instance?> -- subclass of <operation>
define sealed domain make(singleton(<instance?>));
define sealed domain initialize(<instance?>);
// <nlx-operation> -- subclass of <operation>
define sealed domain make(singleton(<nlx-operation>));
define sealed domain initialize(<nlx-operation>);
// <catch> -- subclass of <nlx-operation>
define sealed domain make(singleton(<catch>));
// <throw> -- subclass of <nlx-operation>
define sealed domain make(singleton(<throw>));
// <make-catcher> -- subclass of <nlx-operation>
define sealed domain make(singleton(<make-catcher>));
// <disable-catcher> -- subclass of <nlx-operation>
define sealed domain make(singleton(<disable-catcher>));
// <literal-constant> -- subclass of <constant>
define sealed domain make(singleton(<literal-constant>));
// <definition-constant-leaf> -- subclass of <constant>
define sealed domain make(singleton(<definition-constant-leaf>));
// <uninitialized-value> -- subclass of <constant>
define sealed domain make(singleton(<uninitialized-value>));
// <values-cluster-info> -- subclass of <debug-named-info>
define sealed domain make(singleton(<values-cluster-info>));
// <local-var-info> -- subclass of <debug-named-info>
define sealed domain make(singleton(<local-var-info>));
// <lexical-var-info> -- subclass of <debug-named-info>, <source-location-mixin>
define sealed domain make(singleton(<lexical-var-info>));
// <function-literal> -- subclass of <abstract-function-literal>, <queueable-mixin>
define sealed domain make(singleton(<function-literal>));
// <method-literal> -- subclass of <function-literal>
define sealed domain make(singleton(<method-literal>));
// <exit-function> -- subclass of <abstract-function-literal>, <dependent-mixin>
define sealed domain make(singleton(<exit-function>));
define sealed domain initialize(<exit-function>);
// <fer-function-region> -- subclass of <function-region>, <queueable-mixin>, <annotatable>
define sealed domain make(singleton(<fer-function-region>));
define sealed domain initialize(<fer-function-region>);
// <lambda> -- subclass of <fer-function-region>
define sealed domain make(singleton(<lambda>));
// <fer-component> -- subclass of <component>, <identity-preserving-mixin>
define sealed domain make(singleton(<fer-component>));
define sealed domain initialize(<fer-component>);
// <unwind-protect-region> -- subclass of <body-region>
define sealed domain make(singleton(<unwind-protect-region>));
define sealed domain initialize(<unwind-protect-region>);
// <environment> -- subclass of <annotatable>
define sealed domain make(singleton(<environment>));
define sealed domain initialize(<environment>);
// <closure-var> -- subclass of <object>
define sealed domain make(singleton(<closure-var>));
define sealed domain initialize(<closure-var>);
// <nlx-info> -- subclass of <identity-preserving-mixin>
define sealed domain make(singleton(<nlx-info>));
define sealed domain initialize(<nlx-info>);
