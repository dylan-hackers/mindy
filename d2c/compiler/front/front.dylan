Module: front
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/front/front.dylan,v 1.29 1995/05/08 11:43:23 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

/*

assignment
    fer-assignment {abstract}
        set-assignment
	let-assignment

operation
    primitive
    abstract-call [annotatable] {abstract}
        local-call
        unknown-call
	error-call
	mv-call
    prologue
    catcher
    set
    self-tail-call
    slot-access {abstract}
	slot-ref
	slot-set
    truly-the


variable-info
    module-var-info
	module-almost-constant-var-info
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

block-region
    fer-block-region

exit
    pitcher

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
  slot policy :: <policy>, required-init-keyword: policy:;
end class;

// Marks a create-point for a lexical variable.  We allow there to be more than
// one (useful for representing loops.)  Let-assignments should only be used on
// lexical variables.
//
define class <let-assignment> (<fer-assignment>)
  //
  // The next link in the chain of lets in this component.
  slot let-next :: false-or(<let-assignment>), required-init-keyword: next:;
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
  slot name :: <symbol>, required-init-keyword: name:;
end;


// The <abstract-call> operation represents any function call.
// In Operands, the called function is first, followed by the args.
//
define abstract class <abstract-call> (<operation>, <annotatable>)
end class;

// An arbitrary function call where the function call might be computed and the
// argument syntax might be incorrect.
//
define class <unknown-call> (<abstract-call>)
end class;

// A call where the function is known and all hairy argument stuff has been
// expanded away into positional arguments, hence is now a candidate for
// inlining and other magic.
// 
define class <known-call> (<abstract-call>)
end;

// A call that is known to be in error.  Basically, the same as <unknown-call>
// but we've given up trying to improve it.
//
define class <error-call> (<abstract-call>)
end;

// In a MV-Call, there is one argument which must be a values cluster.  This
// values cluster is spread out to form the actual arguments to the called
// function.  If the resulting actual parameters are not syntactically legal
// (e.g. not enough required args, etc.), then an error will be signalled.
//
define class <mv-call> (<abstract-call>)
end class;

// A prologue is used to represent the incomming arguments to a function.
// 
define class <prologue> (<operation>)
  slot function :: <fer-function-region>, required-init-keyword: function:;
end;

// A catcher is used to receive the values from an exit-function.
// 
define class <catcher> (<operation>)
  //
  // A catcher depends on nothing.
  inherited slot depends-on, init-value: #f;
  //
  // If there is an exit function that jumps to this block, then this is it.
  // If false, then all potential exits are explicitly represented by
  // <exit> regions.
  slot exit-function :: false-or(<exit-function>), init-value: #f;
  //
  // The block-region this catcher is for.
  slot target-region :: <fer-exit-block-region>,
    required-init-keyword: target-region:;
end;

// A set operation is used to change a global variable.
// 
define class <set> (<operation>)
  //
  // Set operations return nothing.
  inherited slot derived-type,
    init-function: curry(make-values-ctype, #(), #f);
  //
  // The definition for the variable being set.
  slot variable :: <definition>, required-init-keyword: var:;
end;

// A self-tail-call is used to represent the rebinding of the arguments
// once we have converted a tail-call of ourselves into a loop.
//
define class <self-tail-call> (<operation>)
  //
  // Self tail calls return nothing.
  inherited slot derived-type,
    init-function: curry(make-values-ctype, #(), #f);
  //
  // The function we are self tail calling.
  slot self-tail-call-of :: <fer-function-region>,
    required-init-keyword: of:;
  //
  // The next self tail call in this method.
  slot next-self-tail-call :: false-or(<self-tail-call>),
    required-init-keyword: next-self-tail-call:;
end;

define abstract class <slot-access> (<operation>)
  slot slot-info :: <slot-info>, required-init-keyword: slot-info:;
  slot slot-offset :: <fixed-integer>, required-init-keyword: slot-offset:;
end;

define class <slot-ref> (<slot-access>)
end;

define class <slot-set> (<slot-access>)
  inherited slot derived-type,
    init-function: curry(make-values-ctype, #(), #f);
end;

define class <truly-the> (<operation>)
  slot guaranteed-type :: <ctype>, required-init-keyword: derived-type:;
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
  slot value :: <ct-value>, required-init-keyword: value:;
end class;

// Represents a constant module var.  We point to the <module-var> for the
// actual info, and only represent the references for the data-flow framework.
//
define class <definition-constant-leaf> (<constant>)
  slot const-defn :: <definition>, required-init-keyword: const-defn:;
end class;


// Used as a placeholder for some value that isn't actually initialized.
// For example, unsupplied keywords with hairy defaults, and uninitialized
// slots.  The uninitialized-value cannot reliably be tested against, because
// in some representations, every bit pattern has an interpretation.  It is
// up to the user of the uninitialized-value to known when the value is
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
  slot debug-name :: <symbol>, required-init-keyword: debug-name:;
end class;

define class <values-cluster-info> (<debug-named-info>)
end class;

define class <local-var-info> (<debug-named-info>)
end class;

define class <lexical-var-info> (<debug-named-info>, <source-location-mixin>)
// ??? stuff to handle set & closure vars?
end class;

define class <module-var-info> (<variable-info>)
  slot var-defn :: <definition>, required-init-keyword: var-defn:;
end class;
  
define class <module-almost-constant-var-info> (<module-var-info>)
end class;
  



/// Function literals:


define abstract class <abstract-function-literal> (<leaf>)
  
  // All function literals are subclasses of <function>.
  inherited slot derived-type, init-function: function-ctype;
  
end;

define constant <function-visibility>
  = one-of(#"global", #"local", #"deleted");

define abstract class <function-literal>
    (<abstract-function-literal>, <queueable-mixin>)

  // An indication of what kind of references to this function can exist:
  //
  // Local: all references are explicit dependents of the leaf.
  // Global: some other references might exist, so assume the worst.
  // Deleted: no references will ever exist from now on.
  //
  slot visibility :: <function-visibility>,
    init-value: #"local", init-keyword: visibility:;

  // The signature of this function.
  slot signature :: <signature>,
    required-init-keyword: signature:;

  // The function-region for the main body of code.
  slot main-entry :: <fer-function-region>,
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



// An <exit-function> is a magical function literal that represents
// the exit-function for a block in situations where a non-local exit is
// possible.
//
define class <exit-function> (<abstract-function-literal>)
  //
  // The region that this exit function exits to.
  slot catcher :: <catcher>,
    required-init-keyword: catcher:
end class;


// FE region classes:

// <fer-function-region>
//
// A <fer-function-region> adds the FER specific information to control-flow's
// <function-region>.
//
define class <fer-function-region>
    (<function-region>, <queueable-mixin>, <annotatable>)

  // Some random descriptive string describing this function.
  slot name :: <byte-string>, init-value: "<unknown>", init-keyword: name:;

  // The prologue operation for this function.  The results of the prologue
  // are the arguments to the function.
  slot prologue :: <prologue>;

  // List of the argument types.
  slot argument-types :: <list>, required-init-keyword: argument-types:;

  // The result type of this function.
  slot result-type :: <values-ctype>, init-function: wild-ctype;

  // The block self tail calls should exit to.  #f if we haven't inserted it
  // yet (i.e. haven't found any self tail calls yet).
  slot self-call-block :: false-or(<block-region>), init-value: #f;

  // Chain of all the self tail calls in this function.  Linked via
  // next-self-tail-call.
  slot self-tail-calls :: false-or(<self-tail-call>), init-value: #f;
end;

define method initialize (func :: <fer-function-region>, #key) => ();
  func.prologue
    := make(<prologue>, function: func, depends-on: #f,
	    // ### The depends-on: shouldn't be needed, but Mindy is broken.
	    derived-type: make-values-ctype(func.argument-types, #f));
end;

// <lambda>
//
// A <lambda> is a kind of <fer-function-region> that can close over values.
// It can only be used as the main-entry for local function literals.
//
define class <lambda> (<fer-function-region>)

  // The function-literal this lambda is the main-entry for.  Used to find
  // all the references to this lambda.  Initialized when the function
  // literal is made.
  slot literal :: <function-literal>;

  // The structure which represents the environment that this Function's
  // variables are allocated in.
  slot environment :: <environment>,
    init-function: curry(make, <environment>);
end;


define class <fer-component> (<component>)

  // All the function literals mentioned in this component.
  slot all-function-literals :: <stretchy-vector>,
    init-function: curry(make, <stretchy-vector>);

  // Chain of all the lets (though let-next) in this component.  Used by
  // environment analysis.  Deleted lets are left in this chain, so beware.
  slot all-lets :: false-or(<let-assignment>), init-value: #f;

  // String that is some sort of name for the code in this component.
  slot name :: <byte-string>, init-value: "<unknown>";

  // Table mapping <ct-value>s to <literal-constant>s.
  slot constants :: <object-table>,
    init-function: curry(make, <object-table>);

end class;


// The FER-Block-Region is a subclass of Block-Region which holds information
// related to handling of non-local exits.
//
define abstract class <fer-block-region> (<block-region>)
end class;

// FER-Exit-Block-Region represents a user-level exit procedure target.
//
define class <fer-exit-block-region> (<fer-block-region>)
  //
  // The catcher operation for this block.  #f if we haven't created one
  // yet or if the catcher has been optimized away.
  slot catcher :: union(<false>, <catcher>), init-value: #f;
end class;

// <pitcher> -- an exit that throws some values also.
// 
define class <pitcher> (<exit>, <dependent-mixin>)
  //
  // The type being pitched.
  slot pitched-type :: <values-ctype>, init-function: wild-ctype;
end;

// FER-Cleanup-Block-Region represents a block/cleanup clause.  Somehow...
//
define class <fer-cleanup-block-region> (<fer-block-region>)
end class;


// Misc drek:

// The Environment structure represents the result of Environment analysis.
//
define class <environment> (<annotatable>)
  slot closure-vars :: false-or(<closure-var>), init-value: #f;
end class;

define class <closure-var> (<object>)
  slot original-var :: <ssa-variable>, required-init-keyword: original:;
  slot copy-var :: <ssa-variable>, required-init-keyword: copy:;
  slot closure-next :: false-or(<closure-var>), required-init-keyword: next:;
end;

