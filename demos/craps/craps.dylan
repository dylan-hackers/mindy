rcs-header: $Header: /home/housel/work/rcs/gd/src/demos/craps/craps.dylan,v 1.2 1996/01/12 02:13:13 wlott Exp $
module: craps

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

craps();
