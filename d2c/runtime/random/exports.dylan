rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/random/exports.dylan,v 1.3 1996/02/19 10:37:05 wlott Exp $
module: Dylan-User

define library Random
  use dylan;
  export random;
end library Random;

define module Random
  use dylan;
  use extensions;
  use system;
//  use transcendental;
//  use threads;
  export
    *random-state*, <random-state>, random,
//    random-float, random-gaussian, random-exponential,
    random-bits, $random-bits-count;
end module Random;

