Module: front
Description: Interface to building the Front-End representation.
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/front/builder.dylan,v 1.13 1995/05/12 15:35:00 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

// We're going to be pseudo-abstract here and give a nod to the idea that parts
// of this interface could be used if we wanted to have a builder for other
// instances of the flow framework (like in the backend.)  The operations that
// aren't clearly FER dependent are first, and are specialized on
// <flow-builder>, not <fer-builder>.
//
// The main failure of modularity here is that these operations currently take
// source location and policy as an argument, whereas those probably only make
// sense in FER.
//
define abstract class <flow-builder> (<object>)
end class;


// General operations (theoretically useful outside of FER):


// Make a flow-builder from an existing builder or component.  The kind of
// builder is determined by the argument.  The result builder implicitly
// associates generated methods with the component (or the argument builder's
// component.)
//
define generic make-builder(thing :: type-or(<component>, <flow-builder>))
 => res :: <flow-builder>;


// Returns the region we have built.  This operation can only be called once.
//
define generic builder-result(builder :: <flow-builder>) => res :: <region>;


// Marks the end of the body of some FER statement that contains sub-regions.
// The operations needing END's are named BUILD-foo-BODY.
//
define generic end-body(builder :: <flow-builder>) => res :: <region>;


// Given a region (a previous builder-result value), splice that region into
// the control flow.
//
define generic build-region(builder :: <flow-builder>, region :: <region>)
 => ();


// BUILD-IF-BODY creates and IF and starts the IF then-clause.  BUILD-ELSE ends
// the consequent and starts the else-clause.  END-BODY ends the else clause.
//
define generic build-if-body
    (builder :: <flow-builder>, policy :: <policy>,
     source :: <source-location>,  predicate-leaf :: <leaf>)
 => ();

define generic build-else
    (builder :: <flow-builder>, policy :: <policy>,
     source :: <source-location>)
 => ();


// Marks the start of a block.  The block can be passed to MAKE-EXIT-FUNCTION
// to get an exit function for the block, or it can be used directly by
// BUILD-EXIT.  END-BODY marks the end of the block.
//
define generic build-block-body
    (builder :: <flow-builder>, policy :: <policy>,
     source :: <source-location>)
 => res :: <block-region>;


// Build a local exit, with the target represented by its <block-region>.
// Probably not called during initial FER conversion, since it takes some work
// to discover local exits.
//
define generic build-exit
    (builder :: <flow-builder>, policy :: <policy>,
     source :: <source-location>, target :: <block-region>)
 => ();


// Returns are just like exits, but return a set of values from the target
// method region.
//
define generic build-return
    (builder :: <flow-builder>, policy :: <policy>,
     source :: <source-location>, target :: <function-region>,
     operands :: union(<list>, <leaf>))
 => ();


// Build a loop.  Not called using initial FER conversion, since it takes some
// work to discover loops.
//
define generic build-loop-body
    (builder :: <flow-builder>, policy :: <policy>,
     source :: <source-location>)
 => ();


// Build an assignment.
//
define generic build-assignment
    (builder :: <flow-builder>, policy :: <policy>,
     source :: <source-location>,
     target-vars :: type-or(<leaf>, <list>),
     source-exp :: <expression>)
 => ();


// Build a join.  Joins of more than two paths are achieved by cascading
// two-way joins.  Not called during initial FER conversion, since joins are
// added as needed by SSA conversion
//
define generic build-join
    (builder :: <flow-builder>, policy :: <policy>,
     source :: <source-location>,
     target-var :: <ssa-variable>,
     source1-var :: <leaf>,
     source2-var :: <leaf>)
 => ();


// Get an <operation> to represent the application of the specified operands.
//
define generic make-operation
    (builder :: <flow-builder>, class :: <class>, operands :: <list>,
     #rest additional-make-arguments)
 => res :: <operation>;



// FER specific operations:

// call "make(<fer-component>)" in order to get the ball rolling.  This
// instantiable class is part of the builder interface.

define abstract class <fer-builder> (<flow-builder>)
end class;


// Starts building a <fer-function-region>.
//
define generic build-function-body
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     name :: <byte-string>, arg-vars :: <list>,
     return-convention :: one-of(#"best", #"cluster"))
 => res :: <fer-function-region>;

// Identical to build-function-body, except builds a <lambda> instead of a
// straight <fer-function-region>.
// 
define generic build-lambda-body
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     name :: <byte-string>, arg-vars :: <list>,
     return-convention :: one-of(#"best", #"cluster"))
 => res :: <lambda>;


// Like BUILD-ASSIGNMENT, but also indicates the creation point of the assigned
// lexical variables.
//
define generic build-let
    (builder :: <fer-builder>, policy :: <policy>,
     source :: <source-location>,
     target-vars :: type-or(<leaf>, <list>),
     source-exp :: <expression>)
 => ();


// Special purpose variant of MAKE-OPERATION because it occurs so often.
// 
define generic make-unknown-call
    (builder :: <fer-builder>, function :: <leaf>,
     next-method-info :: false-or(<leaf>), arguments :: <list>)
 => res :: <operation>;



// Return a literal constant leaf.
//
define generic make-literal-constant
    (builder :: <fer-builder>, value :: <ct-value>)
 => res :: <leaf>;


// Return a leaf to reference a named definition (the value of a module
// variable, a named method, or other time-time resolvable thing.)
//
define generic make-definition-leaf
    (builder :: <fer-builder>, def :: <definition>)
 => res :: <leaf>;


// Return a lexical variable (which can be closed over.)  The definition point
// of the variable must be designated by either using this as a formal
// parameter to a METHOD literal or by initializing with LET.
//
define generic make-lexical-var
    (builder :: <fer-builder>, name :: <symbol>, source :: <source-location>,
     of-type :: <ctype>)
 => res :: <abstract-variable>;


// Similar to MAKE-LEXICAL-VAR, but is used for variables known to be used only
// locally (e.g. single-value expression temporaries.)  Name is for internal
// compiler debugging use.
//
define generic make-local-var
    (builder :: <fer-builder>, name :: <symbol>, of-type :: <ctype>)
 => res :: <abstract-variable>;


// Similar to MAKE-LOCAL-VAR, but returns a values-cluster variable which can
// hold an abitrary number of values.
//
define generic make-values-cluster
    (builder :: <fer-builder>, name :: <symbol>, of-type :: <values-ctype>)
 => res :: <abstract-variable>;


// Make a copy of a local, lexical or values-cluster variable.
//
define generic copy-variable
    (builder :: <fer-builder>, var :: <abstract-variable>)
 => res :: <abstract-variable>;


// Return an exit function that exits to Block.
//
define generic make-exit-function
    (builder :: <fer-builder>, target :: <block-region>)
 => res :: <leaf>;


define generic make-function-literal
    (builder :: <fer-builder>, visibility :: <function-visibility>,
     signature :: <signature>, main-entry :: <fer-function-region>)
 => res :: <leaf>;

define generic make-method-literal
    (builder :: <fer-builder>, visibility :: <function-visibility>,
     signature :: <signature>, main-entry :: <fer-function-region>)
 => res :: <leaf>;
