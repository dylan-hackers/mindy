Module: front
Description: Interface to building the Front-End representation.
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
define generic make-builder(thing :: type-union(<component>, <flow-builder>))
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


// Marks the start of a block.  The block can be used by
// BUILD-EXIT.  END-BODY marks the end of the block.
//
define generic build-block-body
    (builder :: <flow-builder>, policy :: <policy>,
     source :: <source-location>)
 => res :: <block-region>;


// Build a local exit, with the target represented by its <block-region>.
// Probably not called during initial FER conversion, since it takes some work
// to discover local exits.  Target #f means does-not-return.
//
define generic build-exit
    (builder :: <flow-builder>, policy :: <policy>,
     source :: <source-location>, target :: false-or(<block-region-mixin>))
 => ();


// Returns are just like exits, but return a set of values from the target
// method region.
//
define generic build-return
    (builder :: <flow-builder>, policy :: <policy>,
     source :: <source-location>, target :: <function-region>,
     operands :: type-union(<list>, <leaf>))
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
     target-vars :: type-union(<leaf>, <list>),
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


define generic build-unwind-protect-body
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     cleanup-function :: <function-literal>)
    => res :: <unwind-protect-region>;

// Starts building a <fer-function-region> or <lambda>.
//
define generic build-function-body
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     lambda? :: <boolean>, name :: <name>, arg-vars :: <list>,
     result-type :: <values-ctype>, hidden-references? :: <boolean>,
     #key calling-convention)
 => res :: <fer-function-region>;


// Like BUILD-ASSIGNMENT, but also indicates the creation point of the assigned
// lexical variables.
//
define generic build-let
    (builder :: <fer-builder>, policy :: <policy>,
     source :: <source-location>,
     target-vars :: type-union(<leaf>, <list>),
     source-exp :: <expression>)
 => ();


// Special purpose variant of MAKE-OPERATION because it occurs so often.
// 
define generic make-unknown-call
    (builder :: <fer-builder>, function :: <leaf>,
     next-method-info :: false-or(<leaf>), arguments :: <list>, #key ct-source-location)
 => res :: <operation>;



// Return a literal constant leaf.
// If value is not a <ct-value> then it gets coerced first.
//
define generic make-literal-constant
    (builder :: <fer-builder>, value)
 => res :: <leaf>;


// Return a lexical variable (which can be closed over.)  The definition point
// of the variable must be designated by either using this as a formal
// parameter to a METHOD literal or by initializing with LET.
//
define generic make-lexical-var
    (builder :: <fer-builder>, name :: <symbol>, source :: <source-location>,
     of-type :: <ctype>)
 => res :: <initial-variable>;


// Similar to MAKE-LEXICAL-VAR, but is used for variables known to be used only
// locally (e.g. single-value expression temporaries.)  Name is for internal
// compiler debugging use.
//
define generic make-local-var
    (builder :: <fer-builder>, name :: <symbol>, of-type :: type-union(<ctype>, <class>))
 => res :: <initial-variable>;


// Similar to MAKE-LOCAL-VAR, but returns a values-cluster variable which can
// hold an arbitrary number of values.
//
define generic make-values-cluster
    (builder :: <fer-builder>, name :: <symbol>, of-type :: <values-ctype>)
 => res :: <initial-variable>;


// Make an initial-variable copy of a local, lexical or values-cluster
// variable.
//
define generic copy-variable
    (builder :: <fer-builder>, var :: <abstract-variable>)
 => res :: <initial-variable>;


// Make an initial-variable from an existing var-info.  Could be local,
// lexical, values-cluster, etc.
//
define generic make-initial-var
    (builder :: <fer-builder>, of-type :: <values-ctype>,
     var-info :: <variable-info>)
    => var :: <initial-variable>;


// Make a local SSA variable.  This is a local variable which must be assigned
// exactly once.
//
define generic make-ssa-var
    (builder :: <fer-builder>, name :: <symbol>, of-type :: <ctype>)
 => res :: <ssa-variable>;


// Make an ssa-variable that contains a reference to the class a
// {class, each-subclass} slot is located in.
//
define function build-slot-home
    (slot-name :: <symbol>,
     from :: <expression>,
     builder :: <internal-fer-builder>,
     policy :: <policy>,
     source :: <source-location>)
  => slot-home :: <ssa-variable>;
  let slot-home
    = make-ssa-var
	(builder,
	 symcat(slot-name, #"-home"),
	 class-ctype());
  build-assignment(builder, policy, source, slot-home, from);
  slot-home
end function;


// Return the exit function for the given nlx info, or make one if necessary.
//
define generic make-exit-function
    (builder :: <fer-builder>, nlx-info :: <nlx-info>,
     catcher :: <abstract-variable>)
 => res :: <leaf>;


define generic make-function-literal
    (builder :: <fer-builder>, ctv :: false-or(<ct-function>),
     kind :: one-of(#"function", #"method", #"callback"),
     visibility :: <function-visibility>,
     signature :: <signature>, main-entry :: <fer-function-region>)
 => res :: <leaf>;

