module: policy
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/policy.dylan,v 1.2 1995/10/30 13:13:03 ram Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

// Captures compilation policy at some particular point.
//
// We don't actually need to preserve identity in dumping, but there's lots of
// sharing of policy objects that we can exploit.
//
define class <policy> (<identity-preserving-mixin>)
  slot speed :: <single-float>, required-init-keyword: speed:;
  slot space :: <single-float>, required-init-keyword: space:;
  slot safety :: <single-float>, required-init-keyword: safety:;
  slot brevity :: <single-float>, required-init-keyword: brevity:;
  slot debug :: <single-float>, required-init-keyword: debug:;
end class;

define method print-object (policy :: <policy>, stream :: <stream>) => ();
  pprint-fields(policy, stream,
		speed: policy.speed,
		space: policy.space,
		safety: policy.safety,
		brevity: policy.brevity,
		debug: policy.debug);
end;

define constant $Default-Policy
  = make(<policy>, speed: 1.0, space: 1.0, safety: 1.0, brevity: 1.0,
         debug: 1.0);


add-make-dumper(#"compiler-policy", *compiler-dispatcher*, <policy>,
  list(speed, speed:, #f,
       space, space:, #f,
       safety, safety:, #f,
       brevity, brevity:, #f,
       debug, debug:, #f)
);
