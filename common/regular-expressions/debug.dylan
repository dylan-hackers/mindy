module: regular-expressions
author: Nick Kramer (nkramer@cs.cmu.edu)
synopsis: Useful code for debugging regular expressions
copyright: see below

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
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


define method print-coll (c :: <collection>)
  for (elt keyed-by key in c)
    format("[%=] %=\n", key, elt);
  end for;
end method print-coll;


define method print-seq (c :: <collection>)
  format("[");
  for (elt keyed-by key in c)
    format("%= ", elt);
  end for;
  format("]\n");
end method print-seq;


define method print-em-all (#rest args)
  for (elt in args)
    format("%= ", elt);
  end for;
end method print-em-all;



// Prints out a prefix representation of the parse tree
//
define method print-parse-tree (r :: <union>)
  format("|");
  print-parse-tree(r.left);
  print-parse-tree(r.right);
end method print-parse-tree;


define method print-parse-tree (r :: <alternative>)
  format("@");
  print-parse-tree(r.left);
  print-parse-tree(r.right);
end method print-parse-tree;


define method print-parse-tree (r :: <quantified-atom>)
  print-parse-tree(r.atom);
  format("*");
end method print-parse-tree;


define method print-parse-tree (r :: <parsed-character>)
  format("%c", r.character);
end method print-parse-tree;


define method print-parse-tree (r :: <parsed-backreference>)
  format("\\%d", r.group-number);
end method print-parse-tree;


define method print-parse-tree (r :: singleton(#f))
  format("_");
end method print-parse-tree;


define method print-parse-tree (r :: <parsed-set>)
  print-char-set(r.char-set);
end method print-parse-tree;


define method print-parse-tree (r :: <mark>)
  format("{mark %d}", r.group-number);
  print-parse-tree(r.child);
end method print-parse-tree;


define method print-parse-tree (r :: <parsed-assertion>)
  format("%=", r.asserts);
end method print-parse-tree;


define method print-char-set (set :: <character-set>)
  format("<");
  if (set.negated-set?)
    format("^");
  end if;
  for (range in set.char-ranges)
    format("%c-%c", head(range), tail(range));
  end for;
  for (c in set.single-chars)
    format("%c", c);
  end for;
  format(">");
end method print-char-set;


define method print-pointer(s)
  format("#f ");
end method print-pointer;


define method print-pointer(s :: <NFA-state>)
  format("%d ", s.number);
end method print-pointer;


define method print-nfa-state (s :: <e-state>)
  format("e-state ");
  print-pointer(s.other-next-state);
end method print-nfa-state;


define method print-nfa-state (s :: <character-atom>)
  format("char(%c) ", s.atom-char);
end method print-nfa-state;


define method print-nfa-state (s :: <set-atom>)
  format("set ");
end method print-nfa-state;


define method print-nfa-state (s :: <assertion>)
  format("assertion(%=) ", s.asserts);
end method print-nfa-state;


define method print-nfa ()
  for (index from 0 below state-count)
    let s = machine[index];
    format("[%d] ", index);
    print-pointer(s.next-state);
    print-nfa-state(s);
    format("\n");
  end for;
end method print-nfa;


define method print-dfa()
  for (i from 0 below next-dfa-state-number)
    format("State %d:", i);
    if (DFA-array[i].final-state)   format("   [final]");  
    else                            format("   [not final]"); end;
    format("\n");
    if (instance?(DFA-array[i], <DFA-assertion>))
      format("   [true]  -> %d\n", DFA-array[i].true-state.debug-number);
      format("   [false] -> %d\n", DFA-array[i].false-state.debug-number);
    else
      for (elt keyed-by c in DFA-array[i].next-state)
	if (elt = #f)
	  format("   [%c] is #f\n", c);
	else
	  unless (elt == dead-state)
	    format("   [%c] -> %d\n", c, elt.debug-number);
	  end unless;
	end if;
      end for;
    end if;
  end for;

  format("Dead state is %d\n", dead-state.debug-number);
end method print-dfa;
