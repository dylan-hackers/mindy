Module: front
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/front/front.dylan,v 1.5 1994/12/16 16:35:48 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

/*
object
    annotatable {mixin}

assignment
    fer-assignment

operation
    abstract-call [annotatable] {abstract}
        call
	mv-call

variable-info
    module-var-info
	module-almost-constant-var-info
    debug-named-info {abstract}
        lambda-var-info [source-location-mixin]
	values-cluster-info
	local-var-info

leaf
    constant
	literal-constant
	definition-constant-leaf

    function-literal
	method-literal
	    lambda [method-region]
	    hairy-method-literal
        exit-function

component
    fer-component

block-region
    fer-block-region

object
    environment [annotatable]
    tail-set [annotatable]

*/

// Defines the Info slot used for back-end annotation.
//
define class <annotatable> (<object>)
  slot info, init-value: #f;
end class;


// Basic front end (FE) SSA classes:

// The <fer-assignment> adds policy information.  This can't be associated
// with expressions, since different uses of the expression can have different
// policies.
//
define class <fer-assignment> (<assignment>)
  //
  // The policy environment this <operation> was converted in.
  slot policy :: <policy>, required-init-keyword: policy:;
end class;


// The <call> operation represents a non-local function call (possibly a
// primitive.)
// ### KIND may want to be encoded by subclassification.
//
define abstract class <abstract-call> (<operation>, <annotatable>)
  //
  // In Operands, the called function is first, followed by real args, with any
  // dummy state variables last.
  //
  // Our degree of knowledge about the called function:
  //
  // "known"
  //        Function is a <function-constant>, summary information has been
  //        incorporated.
  //
  // "unknown"
  //        Function may be any leaf --- we don't know anything.
  //
  // "error"
  //        Effect is similar to "unknown", but the call has been detected to
  //        be illegal, and should be ignored by future optimization.
  //
  slot kind :: one-of(#"known", #"unknown", #"error"),
       init-value: #"unknown";
end class;

define class <call> (<abstract-call>)
end class;

define class <mv-call> (<abstract-call>)
end class;


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

// An external entry point lambda.  The function it is an entry for is
// in the Entry-Function.
define constant %function-xep = ash(1, 0);

// A top-level lambda, holding a compiled top-level form.  A top-level lambda
// should have *no* references.  Entry-Function is a self-pointer.
define constant %function-top-level = ash(1, 1);

// Any thing we make a callable function object for.
define constant %function-external = logior(%function-xep, %function-top-level);

// Represents a BLOCK cleanup clause.  Doesn't get an XEP even though it
// appears as the arg to a funny function.
define constant %function-cleanup = ash(1, 2);

// A BLOCK exit function.
//
define constant %function-exit = ash(1, 3);

// A helper function for a <hairy-method-literal>.
//
define constant %function-helper = ash(1, 4);

// An anonymous method for a GF literal.
//
define constant %function-anonymous-method = ash(1, 5);

// Any function that may have hidden references, so (for example), we don't
// want to delete it if there are no apparent references, and we can't infer
// anything about the argument types.
//
define constant %function-hidden-calls =
    logior(%function-external,
	   logior(%function-cleanup, 
		  logior(%function-helper, %function-anonymous-method)));


// This function has been found to be uncallable, and has been
// marked for deletion.
define constant %function-deleted = ash(1, 6);


define abstract class <function-literal> (<leaf>)
  inherited slot derived-type, init-value: <function>;

  // Some combination of %function-XXX flags as decribed above.
  slot flags :: <fixed-integer>, init-value: 0;

  // In a normal function, this is the external entry point (XEP) lambda for
  // this function, if any.  Each function that is used other than in a local
  // call has an XEP, and all of the non-local-call references are replaced
  // with references to the XEP.
  //
  // In an XEP lambda (indicated by the "External" kind), this is the function
  // that the XEP is an entry-point for.  The body contains local calls to all
  // the actual entry points in the function.
  slot entry-function :: false-or(<function-literal>), init-value: #f;

  // ??? arg documentation?
end class;


define abstract class <method-literal> (<function-literal>)

  // ??? inlinep/inline expansion?  An <expression>?  convert environment?
end class;


// The Lambda only deals with required arguments.  Keyword and rest arguments
// are represented by special helper lambdas and <hairy-method-literal> objects.
//
define class <lambda> (<method-literal>, <method-region>)

  // List of lexical varibles for args.
  slot vars :: <list>, required-init-keyword: vars:;

  // List of variables that receive the values of the body.  Might be a single
  // values-cluster.
  slot result :: <list>, required-init-keyword: result:;

  // A list of all the functions directly called from this function
  // using a non-let-converted local call.  May include deleted functions
  // because nobody bothers to clear them out.
  slot calls :: <list>, init-value: #();

  // The Tail-Set that this lambda is in.
  slot tail-set :: <tail-set>;

  // The structure which represents the environment that this Function's
  // variables are allocated in.  This is filled in by environment analysis.
  slot environment :: <environment>;

  // If this lambda was ever a helper function for a <hairy-method-literal>
  // or a method of a GF literal, then this is the associated
  // <function-literal>.  This is a source-tracking breadcrumb.
  slot subfunction-of :: false-or(<hairy-method-literal>), init-value: #f;
end class;


// The <Hairy-Method-Literal> leaf is used to represent hairy methods.  The
// value returned by the function is the value which results from calling the
// <Hairy-Method-Literal>.
// 
// Local call analysis parses the arguments to calls with only fixed
// arguments or recognizable keyword arguments, and turns it into a call to
// the more entry or main entry.
//
define class <hairy-method-literal> (<method-literal>, <source-location-mixin>)

  // The signature describing what arguments are actually used.
  slot signature :: <signature>, required-init-keyword: signature:;

  // An entry point which takes Required fixed arguments followed by
  // next-method info, followed by an argument context pointer and an argument
  // count.  This entry point deals with listifying rest args and
  // parsing keywords.  This is <false> we haven't bothered computing it
  // yet.
  slot more-entry :: false-or(<lambda>), init-value: #f;

  // The main entry-point into the function, which takes all arguments
  // including keywords as fixed arguments.  The format of the arguments must
  // be determined by examining the signature.  This may be used by callers
  // that supply at least Required arguments and know what they are doing.
  slot main-entry :: <lambda>, required-init-keyword: main-entry:;
end class;


// An <exit-function> is a magical function literal that represents
// the exit-function for a block in situations where a non-local exit is
// possible.
//
define class <exit-function> (<function-literal>)
  //
  // The region that this exit function exits to.
  slot target-region :: <fer-block-region>;
end class;


// FE region classes:

define class <fer-component> (<component>)
  // The kind of component:
  // 
  // #F
  //     An ordinary component.
  //
  // "Initial"
  //     The result of initial FE conversion, on which component analysis has
  //     not been done.
  //
  // "Deleted"
  //     Debris left over from component analysis.
  //
  slot kind :: one-of(#f, #"initial", #"deleted"), init-value: #f;

  // If true, then there is stuff in this component that could benefit from
  // further FE optimization.
  slot reoptimize :: <boolean>, init-value: #t;

  // If true, then the control flow in this component was messed up by FE
  // optimizations.  The DFO should be recomputed.
  slot reanalyze :: <boolean>, init-value: #f;

  // String that is some sort of name for the code in this component.
  slot name :: <byte-string>, init-value: "<unknown>";

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
  // If there is an exit function that jumps to this block, then this is it.
  // If false, then all potential exits are explicitly represented by
  // <exit-region> objects.
  slot exit-function :: false-or(<exit-function>), init-value: #f;
end class;

// FER-Cleanup-Block-Region represents a block/cleanup clause.  Somehow...
//
define class <fer-cleanup-block-region> (<fer-block-region>)
end class;


// Misc drek:

// The Environment structure represents the result of Environment analysis.
//
define class <environment> (<annotatable>)

  // The function that allocates this environment.
  slot lambda :: <lambda>, required-init-keyword: lambda:;

  // A list of all the <Lambdas> that allocate variables in this environment.
  slot lambdas :: <list>, init-value: #();

  // A list of all the <lambda-var>s and <exit-functions>s needed from
  // enclosing environments by code in this environment.
  slot closure :: <list>, init-value: #();
end class;


// The Tail-Set structure is used to accmumlate information about
// tail-recursive local calls.  The "tail set" is effectively the transitive
// closure of the "is called tail-recursively by" relation.
// 
// All functions in the same tail set share the same <Tail-Set> structure.
// When optimization discovers new TR calls, dinstinct tail sets will be
// merged.  The tail set is somewhat approximate, because it is too early to be
// sure which calls will be TR.  Any call that *might* end up TR causes
// tail-set merging.
//
define class <tail-set> (<annotatable>)

  // A list of all the lambdas in this tail set.
  slot lambdas :: <list>, required-init-keyword: lambdas:;

  // Our current best guess of the type returned by these functions.  This is
  // the union across all the functions of the return <operation>'s Result-Type.
  // excluding local calls.
  slot type :: <values-ctype>, init-function: wild-ctype;
end class;
