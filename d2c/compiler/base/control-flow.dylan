Module: flow
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/control-flow.dylan,v 1.3 1995/03/24 12:21:48 ram Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


/*
region [source-location-mixin] {abstract}
    block-region-mixin {abstract}
        component

    linear-region {abstract}
        simple-region
        compound-region

    join-region {abstract}
        if-region [dependent-mixin]
	body-region {abstract}
	    block-region [block-region-mixin]
	    method-region
	    loop-region

    exit
    call-site

*/

define abstract class <region> (<source-location-mixin>)
  //
  // The region that directly encloses this one.  #f in components (which form
  // the root of the tree.)
  slot parent :: false-or(<region>), init-keyword: parent:, init-value: #f;
end class;


// <linear-regions> contain code but don't introduce any join points (phi
// functions) in themselves.  Of course, a compound region can have anything
// inside it.
//
define abstract class <linear-region> (<region>)
end class;

// <simple-region> is a sequence of assignments (i.e. expression evaluations
// without any significant control flow changes.)
//
define class <simple-region> (<linear-region>)
  //
  // Double-linked list of assignments.
  slot first-assign :: false-or(<abstract-assignment>), init-value: #f;
  slot last-assign :: false-or(<abstract-assignment>), init-value: #f;
end class;

// <compound-region> is a sequence of arbitrary regions.
//
define class <compound-region> (<linear-region>)
  //
  // The nested regions, in order of evaluation.
  slot regions :: <list>, required-init-keyword: regions:;
end class;


// Join-Regions:
//
// Subclasses of <join-region> describe control flow than branches or joins.
//
define class <join-region> (<region>)
  //
  // Region containing join-assignments for this region.
  slot join-region :: <simple-region>;
end class;

// An <if-region> represents a conditional test.  The join function joins the
// values of the two branches.
//
define class <if-region> (<join-region>, <dependent-mixin>)
  //
  // Holds the dependency for the leaf whose value is tested.
  inherited slot depends-on;
  //
  // Regions holding the branches of the IF.
  slot then-region :: <region>, init-keyword: then-region:;
  slot else-region :: <region>, init-keyword: else-region:;
end class;

// A join-region that contains only one "body" region (no branching, only
// joins.)
//
define class <body-region> (<join-region>)
  slot body :: <region>, init-keyword: body:;
end;

// Inherited by things that can have exits to them (blocks and components.)
//
define class <block-region-mixin> (<region>)
  //
  // All the exits to this block.
  slot exits :: <list>, init-value: #();
end;

// A <block-region> wraps code which can exit to its endpoint.  The phi
// function joins the values arriving at the endpoint.
//
define class <block-region> (<body-region>, <block-region-mixin>)
end;

// A <method-region>'s Parent slot is the <component>, but conceptually it can
// have multiple parent regions (call sites).  The phi function joins the
// values coming from the different callers.
//
define class <method-region> (<body-region>)
 //
 // A list of all the <call-site> objects for local calls to this method.
 slot call-sites :: <list>, init-value: #();
end;

// A <loop-region> repeats execution of the body indefinitely (terminate by
// exit to an outer block.)  The phi function is at the head of the loop,
// joining values coming from the outside with values from previous iterations.
//
define class <loop-region> (<body-region>)
end;


// An <exit> represents a control transfer out of the current construct and
// back up to the end of some enclosing <block-region>.  It doesn't contain any
// code.
// 
define class <exit> (<region>)
  slot block-of :: <block-region-mixin>, required-init-keyword: block:;
end;


// Represents all the stuff we're currently compiling.  This is also a
// pseudo-block, in that it can have exits to it (representing expressions that
// unwind.)
//
define class <component> (<block-region-mixin>)
  keyword source-location:, init-value: make(<source-location>);
  //
  // Queue of dependencies that need to be updated (threaded by queue-next.)
  slot reoptimize-queue :: false-or(<dependency>), init-value: #f;
  //
  // List of all methods.
  slot all-methods :: <list>, init-value: #();
end;
