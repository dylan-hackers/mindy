module:     class-diagram
author:     Nick Kramer (nkramer@cs.cmu.edu)

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

// This file is a kludge and a half.  The algorithm is really meant
// for trees, not directed acyclic graphs.  Even for trees, it sticks
// all nodes as far left as it can; this occasionally makes for ugly
// pictures.  (This wouldn't be hard to correct with one more pass
// through the tree)  And this whole implementation is one big kludge..
// The main function, view-class-hierarchy, is actually threadsafe,
// though, so that's one thing that works.


// Directed Acyclic Graph
//
// Invariants:  There is at most one <dag> for any <class>.
// dag-parents and dag-children contain no duplicate elements.
// dag-parents and dag-children contain a subset of the parents and
//   children they will eventually contain/should contain.
//
define class <dag> (<object>)
  slot dag-parents :: <sequence> = #();   // parents:
  slot dag-children :: <sequence> = #();  // children:
  slot dag-class :: <class>, required-init-keyword: #"class";
  slot dag-level :: <integer> = 0;
  slot dag-already-drawn :: <boolean> = #f;
  slot dag-position :: <integer>;    // The next three are intentionally
  slot dag-peers :: <sequence>;      // left uninitialized
  slot dag-left-peer :: false-or(<dag>);
  slot dag-unsquished? :: <boolean> = #f;
end class <dag>;

define method add-child! (parent :: <dag>, child :: <dag>) => ();
  child.dag-parents := add-new!(child.dag-parents, parent);
  parent.dag-children := add-new!(parent.dag-children, child);
end method add-child!;

define method create-dag
    (class :: <class>, direction :: one-of(#"up", #"down", #"both"))
 => dag :: <dag>;  // corresponds to cls
  let table = make(<object-table>);
  local
    method reuse-or-make-dag (class :: <class>) => res :: <dag>;
      element(table, class, default: #f)
	| (table[class] := make(<dag>, class: class));
    end method reuse-or-make-dag,
    method create-dag-for
	(class :: <class>, direction :: one-of(#"up", #"down", #"both"))
	=> res :: <dag>;
      let dag = reuse-or-make-dag(class);
      if (direction ~== #"down")
	for (parent in class.direct-superclasses)
	  add-child!(create-dag-for(parent, #"up"), dag);
	end for;
      end if;
      if (direction ~== #"up")
	for (child in class.direct-subclasses)
	  add-child!(dag, create-dag-for(child, #"down"));
	end for;
      end if;
      dag;
    end method create-dag-for;
  create-dag-for(class, direction);
end method create-dag;

  
define constant $horizontal-spacing = 25;
define constant $vertical-spacing = 50;

// Inspired by "Pretty Printing of Trees" by Jean G. Vaucher in
// Software--Practice and Experience, vol 10 p. 553-561, 1980.
// No code from that was actually used, though.
//
// This draws a dag of the bare minimum width, assuming nodes are
// ordered the way they are and levels are never split.
//
define method compute-dag-positions! (root :: <dag>) => ();
  // set-levels! only goes down
  local method set-levels! (dag, cur-level)
	  dag.dag-level := max(dag.dag-level, cur-level);
	  for (child in dag.dag-children)
	    set-levels!(child, cur-level + 1);
	  end for;
	end method set-levels!;

  set-levels!(root, 0);
  let peer-groups = make(<vector>, fill: #(), size: 500);  // Big enough...

  // It's critical here that add-new! add new elements to the front,
  // even though that's not strictly guarenteed by Dylan
  //
  local method form-peer-groups (dag)
	  peer-groups[dag.dag-level] 
	    := add-new!(peer-groups[dag.dag-level], dag);
	  for (child in dag.dag-children)
	    form-peer-groups(child);
	  end for;
	end method form-peer-groups;

  form-peer-groups(root);

  local method set-peers! (dag)
	  dag.dag-peers := peer-groups[dag.dag-level];
	  block (quit-loop)
	    for (ptr = peer-groups[dag.dag-level] then ptr.tail)
	      if (ptr.head == dag)
		dag.dag-left-peer := if (ptr.tail == #())
				   #f;
				 else
				   ptr.tail.head;
				 end if;
		quit-loop();
	      end if;
	    end for;
	  end block;
	  for (child in dag.dag-children)
	    set-peers!(child);
	  end for;
	end method set-peers!;

  set-peers!(root);

  local method set-pos! (dag)
	  if (~slot-initialized?(dag, dag-position))
	    for (child in dag.dag-children)
	      set-pos!(child);
	    end for;
	    let left-pos-suggestion    // Just to right of leftmost neighbor
	      = if (dag.dag-left-peer)
		  $horizontal-spacing + dag.dag-left-peer.dag-position
		    + dag.dag-left-peer.class-width + dag.class-width;
		else
		  dag.class-width;
		end if;
	    let child-pos-suggestion   // Average of children's position
	      = if (dag.dag-children.empty?)
		  dag.class-width;
		else 
		  truncate/(reduce(\+, 0, map(dag-position, dag.dag-children)),
			    dag.dag-children.size);
		end if;
	    dag.dag-position := max(left-pos-suggestion, child-pos-suggestion);
	  end if;
	end method set-pos!;

  set-pos!(root);
end method compute-dag-positions!;
	  
// I'm making this up as I go along
// Should actually be half the width.
// 3 as the magic number seems to be just a tiny bit too small.
//
define method class-width (dag :: <dag>) => width :: <integer>;
  round(3.2 * size(as(<string>, dag.dag-class.class-name)));
end method class-width;

// Compute-dag-positions! has already been called
//
define method draw-dag
    (state :: <inspector-state>, dag :: <dag>, canvas :: <canvas>) => ();
  draw-node(state, canvas, dag);
  for (child in dag.dag-children)
    draw-dag(state, child, canvas);
  end for;
end method draw-dag;

define method draw-node
    (state :: <inspector-state>, canvas :: <canvas>, dag :: <dag>)
    => ();
  if (~dag.dag-already-drawn)
    dag.dag-already-drawn := #t;
    let x-pos = dag.dag-position + 30;
    let y-pos = dag.dag-level * $vertical-spacing + 30;
    let text = create-text(canvas, x-pos, y-pos,
			   text: as(<string>, dag.dag-class.class-name),
			   anchor: "s");
    bind(text, "<Button-1>",
	 curry(xinspect-one-object, dag.dag-class, state));
    for (child in dag.dag-children)
      let line-coords = vector(x-pos, y-pos,
			       child.dag-position + 30,
			       child.dag-level * $vertical-spacing + 30 - 15);
      let line = create-line(canvas, line-coords);
    end for;
  end if;
end method draw-node;

define method find-max-level (dag :: <dag>) 
 => max-level :: <integer>;
  apply(max, dag.dag-level, map(find-max-level, dag.dag-children));
end method find-max-level;
  
define method find-max-width (dag :: <dag>) 
 => max-width :: <integer>;
  apply(max, dag.dag-position + dag.class-width,
	map(find-max-width, dag.dag-children));
end method find-max-width;

define constant $max-width = 640;
define constant $max-height = 512;

define function view-class-hierarchy
    (state :: <inspector-state>, root :: <class>, window-title :: <string>)
    => ();
  grab-lock(state.state-lock);
  if (state.state-done?)
    release-lock(state.state-lock);
  else
    let window = make(<toplevel>);
    add!(state.state-all-windows, window);
    release-lock(state.state-lock);
    unmap-window(window);
    
    call-tk-function("wm minsize ", tk-as(<string>, window), " 1 1");
    call-tk-function("wm title ", tk-as(<string>, window),
		     " \"", tk-quote(window-title), "\"");

    put-tk-line("wm protocol ", window, " \"WM_DELETE_WINDOW\" {",
		curry(close-command, state, window), "}");

    let dag = create-dag(root, #"down");

    compute-dag-positions!(dag);
    let max-level = find-max-level(dag);
    let height = (max-level + 1) * $vertical-spacing + 10;
    let width = find-max-width(dag) + 50;
    let canvas = make(<canvas>, in: window,
		      scrollregion:
			join-tk-args("0 0", width, height),
		      height: min(height, $max-height),
		      width: min(width, $max-width),
		      relief: #"sunken", expand: #t, fill: "both");
    if (height > $max-height)
      scroll(canvas, side: "right", before: canvas, fill: "y", 
	     orient: "vertical");
    end if;
    if (width > $max-width)
      scroll(canvas, side: "bottom", before: canvas, fill: "x", 
	     orient: "horizontal");
    end if;
    map-window(window);
    draw-dag(state, dag, canvas);
  end if;
end function view-class-hierarchy;


define method close-command
    (state :: <inspector-state>, window :: <window>)
    => ();
  grab-lock(state.state-lock);
  if (state.state-done? | ~member?(window, state.state-all-windows))
    release-lock(state.state-lock);
  else
    remove!(state.state-all-windows, window);
    let last-window? = state.state-all-windows.empty?;
    if (last-window?)
      state.state-done? := #t;
    end if;
    release-lock(state.state-lock);
    destroy-window(window);
    if (last-window?)
      signal-event(state.state-done);
    end if;
  end if;
end method close-command;
