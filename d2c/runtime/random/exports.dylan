rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/random/exports.dylan,v 1.1 1995/12/07 19:19:55 wlott Exp $
module: Dylan-User

define library Random
  use dylan;
  export random;
end library Random;

define module Random
  use dylan;
  use extensions;
//  use system;
//  use transcendental;
//  use threads;
  export
    <random-state>, random, random-bits, $random-bits-count /* ,
    random-float, random-gaussian, random-exponential */;
end module Random;

