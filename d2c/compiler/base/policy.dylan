module: policy
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

define sealed domain make (singleton(<policy>));
define sealed domain initialize (<policy>);


define method print-object (policy :: <policy>, stream :: <stream>) => ();
  pprint-fields(policy, stream,
		speed: policy.speed,
		space: policy.space,
		safety: policy.safety,
		brevity: policy.brevity,
		debug: policy.debug);
end;

define constant $Default-Policy
  = make(<policy>, speed: 1.0s0, space: 1.0s0, safety: 1.0s0, brevity: 1.0s0,
         debug: 1.0s0);


add-make-dumper(#"compiler-policy", *compiler-dispatcher*, <policy>,
  list(speed, speed:, #f,
       space, space:, #f,
       safety, safety:, #f,
       brevity, brevity:, #f,
       debug, debug:, #f)
);
