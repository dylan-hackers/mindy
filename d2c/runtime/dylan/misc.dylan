rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/misc.dylan,v 1.5 1995/11/16 03:38:21 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera


define constant $not-supplied :: <list> = list("unsupplied keyword");


define constant <never-returns> :: <type> = type-union();


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
