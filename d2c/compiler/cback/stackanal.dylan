module: stack-analysis


define class <state> (<object>)
  slot max-depth :: <integer>, init-value: 0;
  slot block-wants :: <table>, init-function: curry(make, <object-table>);
end;


define method analize-stack-usage (func :: <fer-function-region>)
    => (max-depth :: <integer>);
  let state = make(<state>);
  let want = analize(func.body, #(), state);
  unless (want == #())
    error("We start the function wanting stuff?");
  end;
  state.max-depth;
end;

define method analize
    (dependency :: <dependency>, want :: <list>, state :: <state>)
    => want :: <list>;
  analize(dependency.dependent-next,
	  analize(dependency.source-exp, want, state),
	  state);
end;

define method analize
    (dependency :: <false>, want :: <list>, state :: <state>)
    => want :: <list>;
  want;
end;

define method analize
    (op :: <expression>, want :: <list>, state :: <state>)
    => want :: <list>;
  want;
end;

define method analize
    (op :: <operation>, want :: <list>, state :: <state>)
    => want :: <list>;
  analize(op.depends-on, want, state);
end;

define method analize
    (op :: <catch>, want :: <list>, state :: <state>)
    => want :: <list>;
  //
  // Catch always returns a cluster.
  let new-depth = size(want) + 1;
  if (new-depth > state.max-depth)
    state.max-depth := new-depth;
  end;
  //
  // We don't need to scan the operands, because none of them will be clusters.
  want;
end;

define method analize
    (op :: <known-call>, want :: <list>, state :: <state>)
    => want :: <list>;
  //
  // We might know that the function is returning some fixed number of values
  // even when it returns a cluster.
  if (op.dependents.dependent.defines
	& function-returns-cluster?(op.depends-on.source-exp))
    let new-depth = size(want) + 1;
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

define method analize
    (op :: <abstract-call>, want :: <list>, state :: <state>)
    => want :: <list>;
  //
  // If the result is used,
  if (op.dependents.dependent.defines)
    //
    // Dink the depth because the results really act like clusters
    // even if they arn't.
    let new-depth = size(want) + 1;
    if (new-depth > state.max-depth)
      state.max-depth := new-depth;
    end;
  end;
  // 
  // Don't need to scan the depends-on, because we can't depend-on any
  // clusters.
  want;
end;

define method analize
    (op :: <mv-call>, want :: <list>, state :: <state>)
    => want :: <list>;
  // We don't need to worry about dinking the depth if the result is a cluster
  // because there will be a cluster in the arguments.
  analize(op.depends-on, want, state);
end;

define method analize
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

define method analize
    (region :: <simple-region>, want :: <list>, state :: <state>)
    => want :: <list>;
  for (assign = region.last-assign then assign.prev-op,
       want = want
	 then analize(assign.depends-on,
		      produce(assign.defines, want),
		      state),
       while: assign)
  finally
    want;
  end;
end;

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
    want.tail;
  else
    want;
  end;
end;

define method analize
    (region :: <compound-region>, want :: <list>, state :: <state>)
    => want :: <list>;
  for (subregion in reverse(region.regions),
       want = want then analize(subregion, want, state))
  finally
    want;
  end;
end;

define method analize
    (region :: <if-region>, want :: <list>, state :: <state>)
    => want :: <list>;
  let then-wants = analize(region.then-region, want, state);
  let else-wants = analize(region.else-region, want, state);

  merge-stacks(then-wants, else-wants);
end;

define method analize
    (region :: <loop-region>, want :: <list>, state :: <state>)
    => want :: <list>;
  analize(region.body, #(), state);
end;

define method analize
    (region :: <block-region>, want :: <list>, state :: <state>)
    => want :: <list>;
  state.block-wants[region] := want;
  analize(region.body, want, state);
end;

define method analize
    (region :: <unwind-protect-region>, want :: <list>, state :: <state>)
    => want :: <list>;
  analize(region.body, want, state);
end;

define method analize
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

define method analize
    (region :: <return>, want :: <list>, state :: <state>, #next next-method)
    => want :: <list>;
  analize(region.depends-on, #(), state);
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

