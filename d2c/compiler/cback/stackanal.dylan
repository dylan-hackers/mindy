module: stack-analysis


define class <state> (<object>)
  slot max-depth :: <fixed-integer>, init-value: 0;
  slot block-wants :: <table>, init-function: curry(make, <object-table>);
end;


define method analize-stack-usage (lambda :: <lambda>)
    => (max-depth :: <fixed-integer>);
  let state = make(<state>);
  let want = analize(lambda.body,
		     analize(lambda.depends-on, #(), state),
		     state);
  unless (want == #())
    error("We start the lambda wanting stuff?");
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
    (op :: <operation>, want :: <list>, state :: <state>)
    => want :: <list>;
  analize(op.depends-on, want, state);
end;

define method analize
    (op :: union(<unknown-call>, <error-call>), want :: <list>,
     state :: <state>)
    => want :: <list>;
  //
  // Dink the depth because the arguments and results really act like clusters
  // even though they arn't.
  let new-depth = size(want) + 1;
  if (new-depth > state.max-depth)
    state.max-depth := new-depth;
  end;
  // 
  // Don't need to scan the depends-on, because we can't depend-on any
  // clusters.
  want;
end;

define method analize
    (op :: <expression>, want :: <list>, state :: <state>)
    => want :: <list>;
  values(want, max-depth);
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
    (region :: <exit>, want :: <list>, state :: <state>)
    => want :: <list>;
  let target = region.block-of;
  for (ancestor = region.parent then ancestor.parent,
       until: ancestor == #f | ancestor == target)
  finally
    if (ancestor)
      state.block-wants[target];
    else
      #();
    end;
  end;
end;

define method analize
    (region :: <pitcher>, want :: <list>, state :: <state>, #next next-method)
    => want :: <list>;
  analize(region.depends-on, next-method(), state);
end;


define method merge-stacks
    (stack1 :: <list>, stack2 :: <list>) => res :: <list>;
  if (stack1.head == stack2.head)
    stack1;
  else
    let len1 = stack1.size;
    let len2 = stack2.size;
    if (len1 < len2)
      assert(stack2[len2 - len1] == stack1.head);
      stack2;
    elseif (len2 < len1)
      assert(stack1[len1 - len2] == stack2.head);
      stack1;
    else
      error("Inconsistent wants via different routes?");
    end;
  end;
end;

