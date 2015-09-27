module: craps

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

define method d6 () => res :: <integer>;
  random(6) + 1;
end;

define method craps () => ();
  let point = d6() + d6();
  format("You started with a %d.\n", point);
  if (point == 7 | point == 11)
    format("good throw, you win!\n");
  elseif (point == 2 | point == 3 | point == 12)
    format("you lose, bummer.\n");
  else
    block (return)
      while (#t)
	let roll = d6() + d6();
	format("You rolled a %d.\n", roll);
	if (roll == 7)
	  format("crapped out, you lose.\n");
	  return();
	elseif (roll == point)
	  format("you made your point!\n");
	  return();
	end;
      end while;
    end block;
  end if;
end method craps;

define method main (foo, #rest stuff);
  craps();
end method;
