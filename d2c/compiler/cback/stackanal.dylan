module: stack-analysis
copyright: see below

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998 - 2003  Gwydion Dylan Maintainers
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

// Implementation note:
//
// These functions make extensive use of <list>.  Although <list> is
// usually an inferior data structure, this module makes good use of
// their data sharing properties, and doesn't actually *save* any data
// in <list>s.  It should, therefore, be fine.

// analyze-stack-usage -- exported.
//
// The external entry point into this module.  Returns the maximum number
// of clusters alive at any one time in the given function.  Also annotates
// the clusters used in the given function with what their depth is.
// 
define method analyze-stack-usage (func :: <fer-function-region>)
    => (max-depth :: <integer>);
  let state = make(<state>);
  let want = analyze(func.body, #(), state);
  unless (want == #())
    error("We start the function wanting stuff? %=", want);
  end;
  state.max-depth;
end;


// <state> -- internal.
//
// State during an analysis of a single function.  This info is wrapped
// up in a <state> object instead of being passed around as additional
// arguments and results because few of the methods actually care about
// this info.  It just makes the code a little simpler.
// 
define class <state> (<object>)
  //
  // The maximum depth we have hit so far.  Once we are done, this tells us
  // how many clusters will be on the stack.
  slot max-depth :: <integer> = 0;
  //
  // Table mapping blocks to the set of clusters we need at the end of that
  // block.  Used to determine what clusters should be on the stack at an
  // exit.
  constant slot block-wants :: <object-table> = make(<object-table>);
end;

define sealed domain make (singleton(<state>));
define sealed domain initialize (<state>);

// Theory of operation
//
// This is a backward analysis, i.e. it creates a
// consumer list starting out with the exits of
// a function, and going backwards in the FER
// to find a matching producer. The analysis only
// cares of clusters.
// The producers are abstract assignments that are
// known to receive clusters from function call etc.
//
// In the implementation the "want" list gets prepended
// by the consumer with a variable it depends on, and
// this list travels backward along the control-flow
// edges until it reaches the defining site of the
// variable (the assignment). Here is where the cluster
// is produced, so we can strip that variable from the
// (head of the) list. In case more than one subsequent
// cluster belongs to the same assignment, we strip the
// whole prefix off the list.
//
define generic analyze
    (entity, want :: <list>, state :: <state>)
 => want :: <list>;

define method analyze
    (dependency :: <dependency>, want :: <list>, state :: <state>)
    => want :: <list>;
  analyze(dependency.dependent-next,
	  analyze(dependency.source-exp, want, state),
	  state);
end;

define method analyze
    (dependency :: <false>, want :: <list>, state :: <state>)
    => want :: <list>;
  want;
end;

define method analyze
    (op :: <expression>, want :: <list>, state :: <state>)
    => want :: <list>;
  want;
end;

define method analyze
    (op :: <operation>, want :: <list>, state :: <state>)
    => want :: <list>;
  //
  // Record the stack depth.
  let depth = size(want);
  op.info := depth;
  //
  // Analyze the wants.
  analyze(op.depends-on, want, state);
end;

define method analyze
    (op :: <primitive>, want :: <list>, state :: <state>)
    => want :: <list>;
  //
  // Record the stack depth.
  let depth = size(want);
  op.info := depth;
  // HOTFIX: some primitives (need to figure out which)
  // need a cluster. Example is values-sequence.
  // Need to look at op.primitive-name ???
  let new-depth = depth + 1;
  if (new-depth > state.max-depth)
    state.max-depth := new-depth;
  end;
  //
  // Analyze the wants.
  analyze(op.depends-on, want, state);
end;

define method analyze
    (op :: <catch>, want :: <list>, state :: <state>)
    => want :: <list>;
  //
  // Record the stack depth.
  let depth = size(want);
  op.info := depth;
  //
  // Catch always returns a cluster.
  let new-depth = depth + 1;
  if (new-depth > state.max-depth)
    state.max-depth := new-depth;
  end;
  //
  // We don't need to scan the operands, because none of them will be clusters.
  want;
end;

define method analyze
    (op :: <known-call>, want :: <list>, state :: <state>)
    => want :: <list>;
  //
  // Record the stack depth.
  let depth = size(want);
  op.info := depth;
  //
  // We might know that the function is returning some fixed number of values
  // even when it returns a cluster.
  if (op.dependents.dependent.defines
	& function-returns-cluster?(op.depends-on.source-exp))
    let new-depth = depth + 1;
    if (new-depth > state.max-depth)
      state.max-depth := new-depth;
    end;
  end;
  // 
  // Don't need to scan the depends-on, because we can't depend-on any
  // clusters.
  want;
end;

define method function-returns-cluster?
    (leaf :: <function-literal>) => res :: <boolean>;
  non-fixed-values?(leaf.signature);
end;

define method function-returns-cluster?
    (leaf :: <literal-constant>) => res :: <boolean>;
  non-fixed-values?(leaf.value.ct-function-signature);
end;

define method function-returns-cluster?
    (leaf :: <definition-constant-leaf>) => res :: <boolean>;
  non-fixed-values?(leaf.const-defn.function-defn-signature);
end;

define method non-fixed-values? (signature :: <signature>) => res :: <boolean>;
  let returns = signature.returns;
  ~(returns.min-values == returns.positional-types.size
      & returns.rest-value-type == empty-ctype());
end;

define method analyze
    (op :: <abstract-call>, want :: <list>, state :: <state>)
    => want :: <list>;
  //
  // Record the depth so that we can use it during code generation.
  let depth = size(want);
  op.info := depth;
  //
  // If the result is used,
  if (op.dependents.dependent.defines)
    //
    // Dink the depth because the results really act like clusters
    // even if they aren't.
    let new-depth = depth + 1;
    if (new-depth > state.max-depth)
      state.max-depth := new-depth;
    end;
  end;
  // 
  // Scan the arguments, because some calls (mv and some error) take clusters.
  analyze(op.depends-on, want, state);
end;

define method analyze
    (op :: <abstract-variable>, want :: <list>, state :: <state>)
    => want :: <list>;
  if (instance?(op.var-info, <values-cluster-info>))
    let depth = size(want);
    op.info := depth;
    let new-depth = depth + 1;
    if (new-depth > state.max-depth)
      state.max-depth := new-depth;
    end;
    pair(op, want);
  else
    want;
  end;
end;

define method analyze
    (region :: <simple-region>, want :: <list>, state :: <state>)
    => want :: <list>;
  for (assign = region.last-assign then assign.prev-op,
       want = want
	 then analyze(assign.depends-on,
		      produce(assign.defines, want),
		      state),
       while: assign)
  finally
    want;
  end;
end;


// produce -- internal
//
// produce a cluster, that is wanted later on
// in the control-flow.
//
define generic produce (producer, want :: <list>)
    => want :: <list>;

define method produce (defn :: <false>, want :: <list>)
    => want :: <list>;
  want;
end;

define method produce (defn :: <initial-definition>, want :: <list>)
    => want :: <list>;
  produce(defn.definition-of, want);
end;

define method produce (var :: <abstract-variable>, want :: <list>)
    => want :: <list>;
  if (instance?(var.var-info, <values-cluster-info>))
    unless (want.head == var)
      error("The cluster we are producing isn't wanted?");
    end;

    if (member?(var, want.tail))
      // since the cluster seems duplicated
      // with a smaller number
      var.info := var.info - 1;
      // redo the production
      // this ensures var.info to be minimal
      produce(var, want.tail);
      // FIXME: state.max-depth may be left unupdated,
      //  but that is a minor annoyance.
    else
      want.tail;
    end;
  else
    want;
  end;
end;

define method analyze
    (region :: <compound-region>, want :: <list>, state :: <state>)
    => want :: <list>;
  for (subregion in reverse(region.regions),
       want = want then analyze(subregion, want, state))
  finally
    want;
  end;
end;

define method analyze
    (region :: <if-region>, want :: <list>, state :: <state>)
    => want :: <list>;
  let then-wants = analyze(region.then-region, want, state);
  let else-wants = analyze(region.else-region, want, state);

  merge-stacks(then-wants, else-wants);
end;

define method analyze
    (region :: <loop-region>, want :: <list>, state :: <state>)
    => want :: <list>;
  //
  // We analyze the body once, assuming that that we need no clusters.
  let body-wants = analyze(region.body, #(), state);
  //
  // But if the body wants some clusters (because some exit out of the
  // loop wants some clusters), then we have to reanalyze the body with
  // those wants to make sure that the repeat branch of the loop preserves
  // them.
  if (body-wants == #())
    #();
  else
    analyze(region.body, body-wants, state);
  end if;
end;

define method analyze
    (region :: <block-region>, want :: <list>, state :: <state>)
    => want :: <list>;
  state.block-wants[region] := want;
  analyze(region.body, want, state);
end;

define method analyze
    (region :: <unwind-protect-region>, want :: <list>, state :: <state>)
    => want :: <list>;
  analyze(region.body, want, state);
end;

define method analyze
    (region :: <exit>, want :: <list>, state :: <state>)
    => want :: <list>;
  let target = region.block-of;
  for (ancestor = region.parent then ancestor.parent,
       until: ancestor == #f | ancestor == target)
  finally
    if (ancestor & ~instance?(ancestor, <component>))
      state.block-wants[target];
    else
      #();
    end;
  end;
end;

define method analyze
    (region :: <return>, want :: <list>, state :: <state>)
    => want :: <list>;
  analyze(region.depends-on, #(), state);
end;



define method merge-stacks
    (stack1 :: <list>, stack2 :: <list>) => res :: <list>;
  if (stack1.head == stack2.head)
    stack1;
  else
    let len1 = stack1.size;
    let len2 = stack2.size;
    if (len1 < len2)
      assert(stack1 == #() | stack2[len2 - len1] == stack1.head);
      stack2;
    elseif (len2 < len1)
      assert(stack2 == #() | stack1[len1 - len2] == stack2.head);
      stack1;
    else
      error("Inconsistent wants via different routes?");
    end;
  end;
end;

