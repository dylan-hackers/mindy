module:   regular-expressions
author:   Nick Kramer (nkramer@cs.cmu.edu)
synopsis: Everything that relates to finite automaton
          (build-NFA, NFA-to-DFA, sim-DFA)
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

// ### This code is not currently used.

// -----------------------------------------------------------------
// build-NFA:
// Takes a reg-exp parse tree and builds an NFA (nondeterministic
// finite automaton)
// -----------------------------------------------------------------

// The machine is a graph of <NFA-state>s.  The class hierarchy has
// some correspondence to parsing, but not all that much.  There are
// several subclasses of <NFA-state>.  There's the <e-state>, which
// consumes no input and has two next states.  There's <atom>, which
// consumes exactly one input.  <atom> has several subclasses, and
// contains every legal parse atom that's not a backreference.
// And there's <assertion>, which consume no input.

// Quantifiers are expanded into perfectly normal finite automata.  *,
// +, and ? are special cased; everything else is brutally stupid.
// "a{5}" becomes "aaaaa", and "a{2,4}" becomes "(aa)|(aaa)|(aaaa)"

// The regular expression must be a string that consists only of
// byte-characters.  (That's not the same thing as saying input has to
// be a byte-string) build-NFA doesn't really care, but NFA-to-DFA and
// sim-DFA do.

define class <NFA-state> (<object>)
  slot next-state :: false-or(<NFA-state>),
    init-value: #f, init-keyword: #"next-state";
//  slot number :: <integer>;         // Debugging purposes only
end class <NFA-state>;

define class <e-state> (<NFA-state>)
  slot other-next-state :: false-or(<NFA-state>), 
    init-value: #f, init-keyword: #"other-next-state";
end class <e-state>;

define class <atom> (<NFA-state>)
end class <atom>;

define class <character-atom> (<atom>)
  slot atom-char :: <character>, required-init-keyword: #"character";
end class <character-atom>;

define class <set-atom> (<atom>)
  slot atom-set :: <character-set>, required-init-keyword: #"set";
end class <set-atom>;

define class <assertion> (<NFA-state>)
  slot asserts :: <symbol>, required-init-keyword: #"assertion";
end class <assertion>;

// All debugging code
/*
define variable machine = make(<stretchy-vector>);
define variable state-count = 0;

define method initialize(s :: <NFA-state>, #next next-method,
			 #key, #all-keys);
  s.number := state-count;
  machine[s.number] := s;
  state-count := state-count + 1;
  next-method();
end method initialize;
*/

define method build-nfa (r :: <union>) 
 => (first-state :: <NFA-state>, last-state :: <NFA-state>);
  let (n1-front, n1-back) = build-nfa(r.left);
  let (n2-front, n2-back) = build-nfa(r.right);
  let first = make(<e-state>, 
		   next-state: n1-front, other-next-state: n2-front);
  let last = make(<e-state>);
  n1-back.next-state := last;
  n2-back.next-state := last;
  values(first, last);
end method build-nfa;

// Concatenation
//
define method build-nfa (r :: <alternative>) 
 => (first-state :: <NFA-state>, last-state :: <NFA-state>);
  let (n1-front, n1-back) = build-nfa(r.left);
  let (n2-front, n2-back) = build-nfa(r.right);
  n1-back.next-state := n2-front;
  values(n1-front, n2-back);
end method build-nfa;

define method build-nfa (r :: <parsed-assertion>) 
 => (first-state :: <NFA-state>, last-state :: <NFA-state>);
  let node = make(<assertion>, assertion: r.asserts);
  values(node, node);
end method build-nfa;

define method build-nfa (r :: <quantified-atom>) 
 => (first-state :: <NFA-state>, last-state :: <NFA-state>);
  build-quantified-nfa(r.atom, r.min-matches, r.max-matches);
end method build-nfa;

define method build-nfa (r :: <mark>)
 => (first-state :: <NFA-state>, last-state :: <NFA-state>);
  build-nfa(r.child);
end method build-nfa;

// This method should never be called, because true finite automaton
// can't handle backreferences.
//
define method build-nfa (r :: <parsed-backreference>) 
 => (first-state :: <NFA-state>, last-state :: <NFA-state>);
  error("Damn it, Jim, I'm a finite automaton, not a Turing machine!");
end method build-nfa;

define method build-nfa (r :: <parsed-character>) 
 => (first-state :: <NFA-state>, last-state :: <NFA-state>);
  let node = make(<character-atom>, character: r.character);
  values(node, node);
end method build-nfa;			   

define method build-nfa (r :: <parsed-set>) 
 => (first-state :: <NFA-state>, last-state :: <NFA-state>);
  let node = make(<set-atom>, set: r.char-set);
  values(node, node);
end method build-nfa;			   

// Handle the quantified parse element
//
define method build-quantified-nfa
    (r :: <parsed-regexp>, min :: <integer>, max :: false-or(<integer>))
 =>  (first-state :: <NFA-state>, last-state :: <NFA-state>);
  if (min = 0 & max = 1)       // ?
    let (n-front, n-back) = build-nfa(r);
    let e-back = make(<e-state>);
    let e-front = make(<e-state>, next-state: e-back, 
		       other-next-state: n-front);
    n-back.next-state := e-back;
    values(e-front, e-back);

  elseif (min = 0 & ~max)      // *
    let (n-front, n-back) = build-nfa(r);
    let first-last = make(<e-state>, other-next-state: n-front);
    n-back.next-state := first-last;
    values(first-last, first-last);
    
  elseif (min = 1 & ~max)      // +
    let (n-front, n-back) = build-nfa(r);
    let last = make(<e-state>, other-next-state: n-front);
    n-back.next-state := last;
    values(n-front, last);

  elseif (min = 0 & max = 0)    // {0}, which is a special case of {n} below
    let e = make(<e-state>);
    values(e, e);

  elseif (min = max)           // {n} where n is non-zero
    let (first-state, last-state) = build-nfa(r);
    for (i from 2 to min)
      let (another-begin, another-end) = build-nfa(r); 
      last-state.next-state := another-begin;
      last-state := another-end;
    end for;
    values(first-state, last-state);
      
  elseif (~max)                // {n,} where n is non-zero
    let (first, last) = build-quantified-nfa(r, min - 1, min - 1);
    let (another-begin, another-end) = build-nfa(r);
    last.next-state := another-begin;
    let e = make(<e-state>, other-next-state: another-begin);
    another-end.next-state := e;
    values(first, e);

  else                         // {n,m} with n < m
    let e-back = make(<e-state>);
    let (front1, back1) = build-quantified-nfa(r, max, max);
    let (front2, back2) = build-quantified-nfa(r, min, max - 1);
    back1.next-state := e-back;
    back2.next-state := e-back;
    let e-front = make(<e-state>, next-state: front1, 
		       other-next-state: front2);
    values(e-front, e-back);
  end if;
end method build-quantified-nfa;

// -----------------------------------------------------------------
// NFA-to-DFA:
// Converts a non-deterministic finite automata (NFA) to a
// deterministic finite automata (DFA).
// -----------------------------------------------------------------

define class <DFA-state> (<object>)
  slot final-state? :: <boolean>, init-keyword: #"final-state", init-value: #f;
end class <DFA-state>;

define class <DFA-character> (<DFA-state>)
  slot next-state :: <byte-character-table>, 
    init-function: method () make(<byte-character-table>) end;
end class <DFA-character>;

define class <DFA-assertion> (<DFA-state>)
  slot asserts :: <symbol>;
  slot true-state :: <DFA-state>;
  slot false-state :: <DFA-state>;
end class <DFA-assertion>;

// Define a <DFA-state-table> that's a subclass of <object-table>.
// The key is a set of NFA states, and the value is a DFA state.  (I
// needed a new type of table to operate like a set)
//
define class <dfa-state-table> (<object-table>)
end class <dfa-state-table>;

define method my-test-function (set1 :: <list>, set2 :: <list>)
 => answer :: <boolean>;
  size(union(set1, set2, test: \==)) = set1.size;
end method my-test-function;

define method my-hash-function (set :: <list>, initial-state :: <hash-state>)
  let id = 0;
  let state = initial-state;
  for (elt in set)
    let (elt-id, new-state) = object-hash(elt, state);
    let new-id = merge-hash-ids(id, elt-id, ordered: #f);
    id := new-id;
    state := new-state;
  end for;
  values(id, state);
end method my-hash-function;

define method table-protocol (table :: <dfa-state-table>)
    => (test :: <function>, hash :: <function>);
  values(my-test-function, my-hash-function);
end method table-protocol;

// e-closure takes a sequence of NFA states and returns another
// sequence of all the NFA states that can be reached from the first
// set using only e-transitions.
//
define method e-closure (nfa-states :: <sequence>) 
 => more-nfa-states :: <sequence>;
  let stack = as(<deque>, nfa-states);
  let reachable-states = #();
  while (~stack.empty?)
    let state = pop(stack);        // state is an NFA state (or #f)
    if (instance?(state, <e-state>))
      push(stack, state.next-state);
      push(stack, state.other-next-state);
    elseif (state = #f)
      #f;    // do nothing
    else
      reachable-states := add-new!(reachable-states, state, test: \==);
    end if;
  end while;
  reachable-states;
end method e-closure;

// Does this collection of NFA states contain an assertion?  If so,
// it'll have to be split into two collections of states, one for if
// the assertion turned out to be true, and one if the assertion turns
// out to be false.  (Assertions can only be tested at runtime)
//
define method has-assertions? (nfa-states :: <sequence>) 
 => answer :: <boolean>;
  local method test-elt (ignored, elt :: <object>)
	  instance?(elt, <assertion>)
	end method test-elt;
  member?(#f, nfa-states, test: test-elt);
end method has-assertions?;

// Takes a set of NFA states, and either finds an already made DFA
// equivalent state, or makes a new DFA state if no such state already
// exists.
//
define method get-dfa-state-equiv
    (nfa-states :: <sequence>, table :: <dfa-state-table>,
     nfa-end-state :: <nfa-state>, superstates-to-process :: <deque>)
 => dfa-state :: <DFA-state>;
  let result = element(table, nfa-states, default: #f);
  if (result)                              
    result;
  elseif (nfa-states.has-assertions?)     
    let new-dfa-state = 
      make(<DFA-assertion>,
	   final-state: member?(nfa-end-state, nfa-states, test: \==));
    table[nfa-states] := new-dfa-state;
    push(superstates-to-process, nfa-states);
    new-dfa-state;
  else
    let new-dfa-state = 
      make(<DFA-character>, 
	   final-state: member?(nfa-end-state, nfa-states, test: \==));
    table[nfa-states] := new-dfa-state;
    push(superstates-to-process, nfa-states);
    new-dfa-state;
  end if;
end method get-dfa-state-equiv;

// This finds an assertion and removes it.  The return values are the
// assertion found, and T minus the assertion.  (#f is returned for
// assertion if no assertion is found)
//
define method remove-an-assertion (T :: <sequence>)
 => (found :: false-or(<assertion>), new-T :: <sequence>);
  let found = #f;
  let new-list = #();
  for (elt in T)
    if (~found & instance?(elt, <assertion>))
      found := elt;
    else
      new-list := add!(new-list, elt);
    end if;
  end for;
  values(found, new-list);
end method remove-an-assertion;

// This follows the method described on p. 118 of Compilers by Aho,
// Sethi, and Ullman (the dragon book), with hacks to handle
// assertions.
//
define method nfa-to-dfa 
    (nfa-begin :: <NFA-state>, nfa-end :: <NFA-state>,
     case-sensitive? :: <boolean>) 
 => dfa :: <DFA-state>;
  let final-state = make(<assertion>, assertion: #"final-state");
  nfa-end.next-state := final-state;
           // Make a special final state we know we can identify

  let superstates-to-process = make(<deque>);
  let dfa-table = make(<dfa-state-table>);
  let dfa-version = rcurry(get-dfa-state-equiv, dfa-table, final-state,
			   superstates-to-process);
  let init-dfa-state = dfa-version(e-closure(list(nfa-begin)));
  
  while (~superstates-to-process.empty?)
    let T = pop(superstates-to-process);
    let dfa-T = dfa-version(T);

    if (instance?(dfa-T, <DFA-character>))
       // One of the nice things about a character jump table is it gives
       // a convenient way to step through all possible characters c.
       // Or, it would if we had keyed-by in the for macro.
      let coll = dfa-T.next-state;
      let (state, limit, next, done?, cur-key, cur-elem)
	= forward-iteration-protocol(coll);
      for (st = state then next(coll, st), until: done?(coll, st, limit))
	let c = cur-key(coll, st);
	let next-superstate = #();
	for (nfa-state in T)
	  if (atom-accepts?(nfa-state, c, case-sensitive?))
	    next-superstate := add!(next-superstate, nfa-state.next-state);
	  end if;
	end for;
	dfa-T.next-state [c] := dfa-version(e-closure(next-superstate));
      end for;

    else  // must be a <DFA-assertion>.  Add a runtime check for the assertion.
      let (assertion, T-false) = remove-an-assertion(T);
      let T-true = if (assertion.next-state ~= #f)
		     add(T-false, assertion.next-state);
		   else
		     T-false;
		   end if;
      dfa-T.false-state := dfa-version(T-false);
      dfa-T.true-state := dfa-version(e-closure(T-true));
      dfa-T.asserts := assertion.asserts;
    end if;
  end while;

    // return value
  init-dfa-state;
end method nfa-to-dfa;

// Says whether a character is accepted or not, given the
// atom to accept it.
//
define method atom-accepts? 
    (atom :: <character-atom>, c :: <character>,
     case-sensitive? :: <boolean>)
 => answer :: <boolean>;
  char-equal?(case-sensitive?, c, atom.atom-char);
end method atom-accepts?;

define method atom-accepts?
    (atom :: <set-atom>, c :: <character>,
     case-sensitive? :: <boolean>)
 => answer :: <boolean>;
  member?(c, atom.atom-set);
end method atom-accepts?;

// -----------------------------------------------------------------
// Sim-DFA:
// Simulates a deterministic finite automaton (DFA)
// -----------------------------------------------------------------

// If it ever touches a state marked as a final state, it answers #t.
// Input must be a string that consists only of byte-characters.
// (That's not the same thing as saying input has to be a byte-string)
//
define method sim-dfa (dfa-start :: <DFA-state>, target :: <substring>)
 => (result :: <boolean>);
  let dfa-state = dfa-start;
  let input = target.entire-string;
  let end-index = target.end-index;

  block (return)
    for (index from target.start-index below end-index)
      let char = input[index];
      if (dfa-state.final-state?)  return(#t) end if;
      while (instance?(dfa-state, <DFA-assertion>))
	dfa-state := 
	  if (assertion-true?(dfa-state.asserts, target, index))
	    dfa-state.true-state;
	  else
	    dfa-state.false-state;
	  end if;
	if (dfa-state.final-state?)	  return(#t);   	end if;
      end while;
      // dfa-state must be a <DFA-character> now.

      dfa-state := dfa-state.next-state [char];
    end for;
    while (instance?(dfa-state, <DFA-assertion>))
      if (dfa-state.final-state?)	  return(#t);   	end if;
      dfa-state := 
	if (assertion-true?(dfa-state.asserts, target, end-index))
	  dfa-state.true-state;
	else
	  dfa-state.false-state;
	end if;
    end while;
    dfa-state.final-state?;            // return value
  end block;
end method sim-dfa;

// Seals for file finite-automaton.dylan

// <NFA-state> -- subclass of <object>
define sealed domain make(singleton(<NFA-state>));
define sealed domain initialize(<NFA-state>);
// <e-state> -- subclass of <NFA-state>
define sealed domain make(singleton(<e-state>));
// <atom> -- subclass of <NFA-state>
define sealed domain make(singleton(<atom>));
// <character-atom> -- subclass of <atom>
define sealed domain make(singleton(<character-atom>));
// <set-atom> -- subclass of <atom>
define sealed domain make(singleton(<set-atom>));
// <assertion> -- subclass of <NFA-state>
define sealed domain make(singleton(<assertion>));
// <DFA-state> -- subclass of <object>
define sealed domain make(singleton(<DFA-state>));
define sealed domain initialize(<DFA-state>);
// <DFA-character> -- subclass of <DFA-state>
define sealed domain make(singleton(<DFA-character>));
// <DFA-assertion> -- subclass of <DFA-state>
define sealed domain make(singleton(<DFA-assertion>));
// <dfa-state-table> -- subclass of <object-table>
define sealed domain make(singleton(<dfa-state-table>));
define sealed domain initialize(<dfa-state-table>);
