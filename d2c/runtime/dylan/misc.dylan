module: dylan


define constant $not-supplied :: <list> = list("unsupplied keyword");


define flushable generic values-sequence (sequence :: <sequence>);

define inline method values-sequence (sequence :: <sequence>)
  values-sequence(as(<simple-object-vector>, sequence));
end;

define inline method values-sequence
    (vector :: <simple-object-vector>)
  %%primitive values-sequence (vector);
end;


define movable generic values (#rest values);

define inline method values (#rest values)
  %%primitive values-sequence (values);
end;


%%primitive magic-internal-primitives-placeholder ();
