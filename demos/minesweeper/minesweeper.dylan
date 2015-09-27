module: mine-sweeper
author: Nick Kramer (nkramer@cs.cmu.edu)

//======================================================================
//
// Copyright (c) 1996  Carnegie Mellon University
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

define class <square> (<object>)
  slot button :: <button>, required-init-keyword: #"button";
  slot exposed? :: <boolean>, init-value: #f;
  slot mine? :: <boolean>, init-value: #f;
  slot internal-mine-count :: <integer>, 
    init-value: 0;  // Filled in during grid initialization
  slot square-row :: <integer>, required-init-keyword: #"row";
  slot square-column :: <integer>, required-init-keyword: #"column";
end class <square>;

define variable num-rows = 16;
define variable num-cols = 16;
define variable num-mines = 40;
define variable grid = make(<array>, dimensions: #[0, 0]);
define variable grid-frame = make(<frame>, relief: #"sunken", 
				  in: *root-window*);
define variable automation-level = 3;
define variable row-vector = make(<vector>, size: 0);

define method printf (#rest args)
  apply(format, *standard-output*, args);
  force-output(*standard-output*);
end method printf;

// This copies the existing widgets into the new grid, because it's a
// lot faster than creating new widgets.
//
define method create-grid () => ();
  printf("Creating grid\n");
  let old-grid = grid;
  let old-row-vector = row-vector;
  grid := make(<array>, dimensions: vector(num-rows, num-cols));
  row-vector := make(<vector>, size: num-rows);

  for (i from 0 below num-rows)
    let row = if (i < old-grid.dimensions.first)
		old-row-vector[i];
	      else
		make(<frame>, in: grid-frame, side: "bottom");
	      end if;
    row-vector[i] := row;
    for (j from 0 below num-cols)
      let widget
	= if (i < old-grid.dimensions.first & j < old-grid.dimensions.second)
	    let w = old-grid[i, j].button;
	    configure(old-grid[i, j].button, relief: "raised", text: " ");
	    w;
	  else
	    let w = make(<button>, relief: #"raised", width: 2,
			 command: curry(interactive-pick-square, i, j),
			 in: row, side: #"left", text: " ");		     
	    bind(w, "<Button-3>", curry(interactive-pick-mine, i, j));
	    w;
	  end if;
      grid[i, j] := make(<square>, button: widget, row: i, column: j);
    end for;
  end for;

  // destroy leftover widgets
  // Get the excess rows
  for (i from num-rows below old-grid.dimensions.first)
    for (j from 0 below grid.dimensions.second)
      destroy-window(old-grid[i, j].button);
    end for;
    destroy-window(old-row-vector[i]);
  end for;
  // Now get the excess columns
  for (i from 0 below num-rows)
    for (j from num-cols below old-grid.dimensions.second)
      destroy-window(old-grid[i, j].button);
    end for;
  end for;
end method create-grid;

define method initialize-grid () => ();
  create-grid();

  printf("Placing mines\n");
  for (count from 0 below num-mines)
    block (break)
      while (#t)
	let row = random(num-rows);
	let col = random(num-cols);
	if (~ grid[row, col].mine?)
	  grid[row, col].mine? := #t;
	  let neighbors = grid[row, col].all-neighbors;
	  for (square in neighbors)
	    square.internal-mine-count := square.internal-mine-count + 1;
	  end for;
	  break();
	end if;
      end while;
    end block;
  end for;

  printf("Done!\n");
end method initialize-grid;

define method new-game () => ();
  game-paused := #t;
  let options-window = make(<toplevel>);
  call-tk-function("wm title ", tk-as(<string>, options-window),
		   " \"", tk-quote("New Game"), "\"");
  let difficulty-frame = make(<frame>, in: options-window);
  make(<label>, text: "Board Size:", anchor: "w", relief: "flat",
        side: "top", in: difficulty-frame);
  let difficulty-level = make(<active-variable>, value: 0);
  num-rows := num-cols := num-mines := 8;
  make(<radiobutton>, in: difficulty-frame, variable: difficulty-level,
       command: method () num-rows := 8; num-cols := 8; num-mines := 8 end,
       value: 0, text: "Beginner (8x8, 10 mines)", side: "top",
       anchor: "w", relief: "flat");
  make(<radiobutton>, in: difficulty-frame, variable: difficulty-level,
       command: method () num-rows := 16; num-cols := 16; num-mines := 40 end,
       value: 1, text: "Intermediate (16x16, 40 mines)", side: "top",
       anchor: "w", relief: "flat");
  make(<radiobutton>, in: difficulty-frame, variable: difficulty-level,
       command: method () num-rows := 16; num-cols := 30; num-mines := 99 end,
       value: 2, text: "Expert (16x30, 99 mines)", side: "top",
       anchor: "w", relief: "flat");
//  make(<radiobutton>, in: difficulty-frame, variable: difficulty-level,
//       text: "Custom");
  
  let automation-frame = make(<frame>, in: options-window);
  make(<label>, text: "Tedium level:", anchor: "w", relief: "flat",
        side: "top", in: automation-frame);
  let automation-variable = make(<active-variable>, value: automation-level);
  make(<radiobutton>, in: automation-frame, variable: automation-variable,
       value: 0, text: "Maximum tedium", side: "top",
       command: method () automation-level := 0 end,
       anchor: "w", relief: "flat");
  make(<radiobutton>, in: automation-frame, variable: automation-variable,
       value: 1, text: "Just like Microsoft", side: "top",
       command: method () automation-level := 1 end,
       anchor: "w", relief: "flat");
  make(<radiobutton>, in: automation-frame, variable: automation-variable,
       value: 2, text: "Somewhat more automated", side: "top",
       command: method () automation-level := 2 end,
       anchor: "w", relief: "flat");
  make(<radiobutton>, in: automation-frame, variable: automation-variable,
       value: 3, text: "Almost full automation", side: "top",
       command: method () automation-level := 3 end,
       anchor: "w", relief: "flat");

  make(<button>, in: options-window, side: "top", text: "Start game",
       fill: "x",
       command: method ()
		  destroy-window(options-window);
		  initialize-grid();
		  game-paused := #f;
		end method);
  make(<button>, in: options-window, side: "top", text: "Quit Minesweeper", 
       command: exit);
end method new-game;

define method all-neighbors (square :: <square>) => neighbors :: <sequence>;
  let q = make(<deque>);
  let row = square.square-row;
  let col = square.square-column;
  if (row > 0)
    if (col > 0)  push(q, grid[row - 1, col - 1])  end;
    push(q, grid[row - 1, col]);
    if (col < num-cols - 1)  push(q, grid[row - 1, col + 1])  end;
  end if;

  if (col > 0) push(q, grid[row, col - 1]) end;
  if (col < num-cols - 1) push(q, grid[row, col + 1]) end;

  if (row < num-rows - 1)
    if (col > 0) push(q, grid[row + 1, col - 1]) end;
    push(q, grid[row + 1, col]);
    if (col < num-cols - 1) push(q, grid[row + 1, col + 1]) end;
  end if;
  q;
end method all-neighbors;

define method mine-count (square :: <square>) => count :: <integer>;
  if (square.mine?)
    error("Can't get the mine count of a square that has a mine itself");
  end if;
  square.internal-mine-count;
end method mine-count;

define method count-mines (square :: <square>) => count :: <integer>;
  let count = 0;
  let neighbors = square.all-neighbors;
  for (s in neighbors)
    if (s.mine?)
      count := count + 1;
    end if;
  end for;
  count;
end method count-mines;

// count # of mines exposed next to this square
//
define method exposed-mine-count (square :: <square>) => count :: <integer>;
  let count = 0;
  let neighbors = square.all-neighbors;
  for (s in neighbors)
    if (s.exposed? & s.mine?)
      count := count + 1;
    end if;
  end for;
  count;
end method exposed-mine-count;

// count # of neighbors that are unexposed
//
define method unexposed-neighbors (square :: <square>) => count :: <integer>;
  let count = 0;
  let neighbors = square.all-neighbors;
  for (s in neighbors)
    if (~ s.exposed?)
      count := count + 1;
    end if;
  end for;
  count;
end method unexposed-neighbors;

define variable game-paused = #f;

define method lose-game (square :: <square>) => ();
  game-paused := #t;
  new-game();
end method lose-game;

define method interactive-pick-square 
    (row :: <integer>, col :: <integer>) => ();
  if (~ game-paused)
    let square = grid[row, col];
    if (pick-square(square))
      auto-play(square);
    end if;
  end if;
end method interactive-pick-square;

define method interactive-pick-mine 
    (row :: <integer>, col :: <integer>) => ();
  if (~ game-paused)
    let square = grid[row, col];
    if (pick-mine(square))
      auto-play(square);
    end if;
  end if;
end method interactive-pick-mine;

define method pick-square (square :: <square>) => auto-play? :: <boolean>;
  if (square.mine?)
    lose-game(square);
    #f;
  elseif (square.exposed?)
    #f;
  else
    square.exposed? := #t;
    configure(square.button, relief: "flat", 
	      text: if (square.mine-count = 0)
		      " ";
		    else
		      integer-to-string(square.mine-count);
		    end if);
    #t;
  end if;
end method pick-square;

define method pick-mine (square :: <square>) => ();
  if (~ square.mine?)
    lose-game(square);
    #f;
  elseif (square.exposed?)
    #f;
  else
    square.exposed? := #t;
    configure(square.button, relief: "flat", text: "*");
    #t;
  end if;
end method pick-mine;

define method auto-play (square :: <square>) => ();
  select (automation-level)
    0 => #f;  // do nothing
    1 => auto-play-1(square);
    2 => auto-play-2(square);
    3 => auto-play-3(square);
    otherwise => error("Unknown automation level!");
  end select;
end method auto-play;

// If a blank square is selected, expand the gap.
//
define method auto-play-1 (s :: <square>) => exposed-any-squares? :: <boolean>;
  let did-something = #f;
  let q = make(<deque>);
  push(q, s);
  while (~ q.empty?)
    let square = pop(q);
    if (square.exposed? & ~square.mine? & square.mine-count = 0)
      let neighbors = square.all-neighbors;
      for (s in neighbors)
	if (~ s.exposed?)
	  pick-square(s);
	  did-something := #t;
	  push(q, s);
	end if;
      end for;
    end if;
  end while;
  did-something;
end method auto-play-1;

// Expose neighbors of those squares with mine count satisfied.
// Really just a generalized version of the above.
//
define method auto-play-2 (s :: <square>) => exposed-any-squares? :: <boolean>;
  let did-something = #f;
  let q = make(<deque>);
  push(q, s);
  let n = all-neighbors(s);
  for (sq in n)
    push(q, sq);
  end for;
  while (~ q.empty?)
    let square = pop(q);
    if (square.exposed? & ~square.mine?
	  & square.mine-count = square.exposed-mine-count)
      let neighbors = square.all-neighbors;
      for (s in neighbors)
	if (~ s.exposed?)
	  pick-square(s);
	  did-something := #t;
	  push(q, s);
	end if;
      end for;
    end if;
  end while;
  did-something;
end method auto-play-2;

// If a square has as many unexposed neighbors as it does unaccounted
// for mines, expose neighbors.
//
define method auto-play-3 (s :: <square>) => exposed-any-squares? :: <boolean>;
  auto-play-2(s);
  block (return)
    let did-something = #t;
    while (did-something)
      did-something := #f;
      for (square in grid)
      if (square.exposed? & ~square.mine?
	    & (square.exposed-mine-count ~= square.mine-count)
	    & (square.mine-count 
		 = square.exposed-mine-count + square.unexposed-neighbors))
	  let neighbors = square.all-neighbors;
	  for (s in neighbors)
	    if (~ s.exposed?)
	      pick-mine(s);
	      did-something := #t;
	      auto-play-2(s);
	    end if;
	  end for;
	end if;
      end for;
    end while;
  end block;
end method auto-play-3;

define method main (prog-name :: <byte-string>, #rest args)
  let button-bar = make(<frame>, in: *root-window*, side: "top");
  call-tk-function("wm title ", tk-as(<string>, *root-window*),
		   " \"", tk-quote("Minesweeper"), "\"");
  make(<button>, in: button-bar, text: "New game...", command: new-game,
       side: "top", fill: "both");
  make(<button>, in: button-bar, text: "Quit", command: exit,
       side: "top", fill: "both");
  initialize-grid();
  map-window(*root-window*);
end method main;
