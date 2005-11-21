Module: flow
copyright: see below


//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
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
region [source-location-mixin] {abstract}
    block-region-mixin {abstract}
        component

    linear-region {abstract}
        simple-region
        compound-region
	    empty-region

    join-region {abstract}
        if-region [dependent-mixin]
	body-region {abstract}
	    block-region [block-region-mixin, queueable-mixin, annotatable]
	    function-region
	    loop-region

    exit
	return

*/

define open abstract primary class <region> (<source-location-mixin>)
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

define method make (class == <compound-region>,
		    #next next-method, #rest keys, #key regions)
    => res :: <compound-region>;
  let regions = choose(complement(rcurry(instance?, <empty-region>)), regions);
  if (empty?(regions))
    apply(make, <empty-region>, regions: regions, keys);
  else
    apply(next-method, class, regions: regions, keys);
  end;
end;

define class <empty-region> (<compound-region>)
end;

define method make
    (class == <empty-region>, #next next-method,
     #rest keys, #key regions = #())
    => res :: <empty-region>;
  unless (regions == #())
    error("Can't make a non-empty <empty-region>");
  end unless;
  apply(next-method, class, regions: #(), keys);
end method make;

// Join-Regions:
//
// Subclasses of <join-region> describe control flow that have branches
// or joins.
//
define open abstract primary class <join-region> (<region>)
  //
  // Region containing join-assignments for this region.
  slot join-region :: <simple-region>;
end class;

define method initialize (region :: <join-region>, #next next-method, #key)
  next-method();
  region.join-region := make(<simple-region>, parent: region);
end;

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
define open abstract primary class <body-region> (<join-region>)
  slot body :: <region>, init-keyword: body:;
end;

// Inherited by things that can have exits to them (blocks and components.)
//
define open abstract class <block-region-mixin> 
    (<region>, <identity-preserving-mixin>)
  //
  // Chain of all the exits to this block, threaded though exit-next.
  slot exits :: false-or(<exit>), init-value: #f;
end;

// A <block-region> wraps code which can exit to its endpoint.  The phi
// function joins the values arriving at the endpoint.
//
define open primary class <block-region>
    (<body-region>, <block-region-mixin>, <queueable-mixin>, <annotatable>)
end;

// A <function-region>'s Parent slot is the <component>, but
// conceptually it can have multiple parent regions (call sites).  The
// phi function joins the values coming from the different callers.
// The exits to a <function-region> must all be <return>s, and in fact
// indicate the return values.
//
// ### This wants to be abstract, but can't because <block-region> isn't
// abstract.
// 
define open primary class <function-region> (<block-region>)
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
  slot next-exit :: false-or(<exit>), required-init-keyword: next:;
end;


// A <return> is a special kind of exit that passes values.
// 
define class <return> (<exit>, <dependent-mixin>)
  slot returned-type :: <values-ctype>, init-function: wild-ctype;
  slot guessed-returned-type :: <values-ctype>, init-function: empty-ctype;
end;

// Represents all the stuff we're currently compiling.  This is also a
// pseudo-block, in that it can have exits to it (representing expressions that
// unwind.)
//
define open abstract primary class <component> (<block-region-mixin>)
  //
  // Queue of all the <initial-variable> variables that need to be ssa
  // converted (threaded through next-initial-variable).
  slot initial-variables :: false-or(<initial-variable>),
    init-value: #f;
  //
  // Queue of things that need to be updated (threaded by queue-next.)
  slot reoptimize-queue :: false-or(<queueable-mixin>), init-value: #f;
  //
  // List of all the <function-regions>s in this component.
  slot all-function-regions :: <stretchy-vector> = make(<stretchy-vector>);
end;

define method add-to-queue
    (component :: <component>, queueable :: <queueable-mixin>) => ();
  queueable.queue-next := component.reoptimize-queue;
  component.reoptimize-queue := queueable;
end;


// Seals for file control-flow.dylan

// <simple-region> -- subclass of <linear-region>
define sealed domain make(singleton(<simple-region>));
// <compound-region> -- subclass of <linear-region>
define sealed domain make(singleton(<compound-region>));
// <empty-region> -- subclass of <compound-region>
define sealed domain make(singleton(<empty-region>));
// <if-region> -- subclass of <join-region>, <dependent-mixin>
define sealed domain make(singleton(<if-region>));
define sealed domain initialize(<if-region>);
// <block-region> -- subclass of <body-region>, <block-region-mixin>, <queueable-mixin>, <annotatable>
define sealed domain make(singleton(<block-region>));
// <function-region> -- subclass of <block-region>
define sealed domain make(singleton(<function-region>));
// <loop-region> -- subclass of <body-region>
define sealed domain make(singleton(<loop-region>));
define sealed domain initialize(<loop-region>);
// <exit> -- subclass of <region>
define sealed domain make(singleton(<exit>));
define sealed domain initialize(<exit>);
// <return> -- subclass of <exit>, <dependent-mixin>
define sealed domain make(singleton(<return>));
