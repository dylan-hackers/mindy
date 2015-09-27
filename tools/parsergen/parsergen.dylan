module: parsergen
author: William Lott, translated to Dylan by Nick Kramer

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

// **********************************************************************
// Copyright (c) 1994, 1996 Carnegie Mellon University, all rights reserved.
// 
//
// **********************************************************************
//
// This file contains a LALR parser generator.  It was originally
// written in Lisp in a rather inefficient style.  It was translated
// to Dylan as literally as possible; the only real differences are
// that a lot of type declarations were added in the process, and that
// some classes are now documented as being abstract.  The Dylan
// version uses the same inefficient algorithms as the original,
// though.
//



define class <grammar> (<object>)
  //
  // List of entry-point grammar symbols.
  slot grammar-entry-points :: <list> = #();
  //
  // Hash table of all the grammar symbols.
  constant slot grammar-symbols :: <object-table> = make(<object-table>);
  //
  // Next available token-id.
  slot grammar-next-token-id :: <integer> = 0;
  //
  // List of all the tokens.
  slot grammar-tokens :: <list> = #();
  //
  // List of all the token unions.
  slot grammar-token-unions :: <list> = #();
  //
  // List of all the non-terminals.
  slot grammar-nonterminals :: <list> = #();
  //
  // List of the start productions.  One for each entry point.
  slot grammar-start-productions :: <list> = #();
  //
  // Number of productions.
  slot grammar-num-productions :: <integer> = 0;
  //
  // List or start states, one for each entry point.
  slot grammar-start-states :: <list> = #();
  //
  // The complete set of states.
  slot grammar-all-states :: <list> = #();
  //
  // The number of states.
  slot grammar-num-states :: <integer> = 0;
end class <grammar>;

define abstract class <grammar-symbol> (<object>)
  //
  // List of tokens that can start this grammar symbol.
  slot grammar-symbol-first :: <list> = #();
  //
  // True iff there is some (foo -> epsilon) production for this grammar
  // symbol.
  slot grammar-symbol-nullable :: <boolean> = #f;
  //
  // The symbol name for this grammar symbol.
  slot grammar-symbol-name :: <symbol>, required-init-keyword: #"name";
  //
  // The type for this grammar symbol, or #f if not specified.
  slot grammar-symbol-type :: false-or(<symbol>) = #f, init-keyword: #"type";
end class <grammar-symbol>;

define abstract class <terminal> (<grammar-symbol>)
end class <terminal>;

define method print-object (symbol :: <terminal>, stream :: <stream>) => ();
  format(stream, "{terminal %s}", 
	 as-uppercase(as(<string>, symbol.grammar-symbol-name)));
end method print-object;

define method print-message (symbol :: <terminal>, stream :: <stream>) => ();
  write(stream, as-uppercase(as(<string>, symbol.grammar-symbol-name)));
end method print-message;

define class <token> (<terminal>)
  //
  // A unique integer corresponding to this terminal.
  constant slot token-id :: <integer>, required-init-keyword: #"id";
end class <token>;

define method initialize
    (result :: <token>, #next next-method, #key, #all-keys)
  next-method();
  result.grammar-symbol-first := list(result);
end method initialize;

define class <token-union> (<terminal>)
  //
  // List of the names for the terminals that make up this union.
  constant slot token-union-members :: <list>, 
    required-init-keyword: #"members";
end class <token-union>;

define class <nonterminal> (<grammar-symbol>)
  //
  // List of productions with this nonterminal on the left hand side.
  slot nonterminal-productions :: <list> = #();
end class <nonterminal>;

define method print-object
    (symbol :: <nonterminal>, stream :: <stream>) => ();
  format(stream, "{nonterminal %s}", 
	 as-lowercase(as(<string>, symbol.grammar-symbol-name)));
end method print-object;

define method print-message
    (symbol :: <nonterminal>, stream :: <stream>) => ();
  write(stream, as-lowercase(as(<string>, symbol.grammar-symbol-name)));
end method print-message;

define class <production> (<object>)
  //
  // Small integer uniquely identifing this production.
  constant slot production-number :: <integer>, 
    required-init-keyword: #"number";
  //
  // The nonterminal on the left.
  constant slot production-left-side :: <nonterminal>, 
    required-init-keyword: #"left-side";
  //
  // List of grammar symbols on the right.
  constant slot production-right-side :: <list>, 
    required-init-keyword: #"right-side";
  //
  // The forms that make up this production.
  constant slot production-body :: <list>, required-init-keyword: #"body";
  //
  // Vector of kernel-items indexed by dot-position, or #F if we haven't
  // allocated it yet. Also, individual elements can be #F if that
  // particular kernel-item yet.
  slot production-kernel-items :: false-or(<simple-vector>) = #f;
end class <production>;

define inline function make-production
    (number :: <integer>, left-side :: <nonterminal>, 
     right-side :: <list>, body :: <list>)
 => obj :: <production>;
  make(<production>, number: number, left-side: left-side, 
       right-side: right-side, body: body);
end function make-production;

define method print-object
    (production :: <production>, stream :: <stream>) 
 => ();
  format(stream, "{production %= ->", production.production-left-side);
  if (production.production-right-side == #())
    write(stream, " epsilon");
  else
    for (elt in production.production-right-side)
      format(stream, " %=", elt);
    end for;
  end if;
  write(stream, "}");
end method print-object;

define method print-message
    (production :: <production>, stream :: <stream>) 
 => ();
  format(stream, "%S ->", production.production-left-side);
  if (production.production-right-side == #())
    write(stream, " epsilon");
  else
    for (elt in production.production-right-side)
      format(stream, " %S", elt);
    end for;
  end if;
end method print-message;

define variable *item-counter* :: <integer> = 0;

define class <item> (<object>)
  //
  // The production this item is built from.
  constant slot item-production :: <production>, 
    required-init-keyword: #"production";
  //
  // The position of the dot in this item.
  constant slot item-dot-position :: <integer> = 0, 
    init-keyword: #"dot-position";
  //
  // Unique index arbitrarily assigned to items as they are crested.
  constant slot item-counter :: <integer> 
    = (*item-counter* := *item-counter* + 1);
end class <item>;

define method print-object (item :: <item>, stream :: <stream>) => ();
  let production = item.item-production;
  let right-side = production.production-right-side;
  let dot-position = item.item-dot-position;
  format(stream, "{item %= ->", production.production-left-side);
  for (elt in copy-sequence(right-side, start: 0, end: dot-position))
    format(stream, " %=", elt);
  end for;
  write(stream, " .");
  for (elt in copy-sequence(right-side, start: dot-position))
    format(stream, " %=", elt);
  end for;
  write(stream, "}");
end method print-object;

define class <kernel-item> (<item>)
  //
  // The lookahead terminals.
  slot kernel-item-lookaheads :: <list> = #();
  //
  // The lookahead terminals added after the last time we propagated
  // terminals.
  slot kernel-item-new-lookaheads :: <list> = #();
  //
  // List of items our lookaheads get propagated to.
  slot kernel-item-propagates-to :: <list> = #();
end class <kernel-item>;

define method print-object (item :: <kernel-item>, stream :: <stream>) 
 => ();
  let production = item.item-production;
  let right-side = production.production-right-side;
  let dot-position = item.item-dot-position;
  format(stream, "{kernel-item %= ->", production.production-left-side);
  for (elt in copy-sequence(right-side, start: 0, end: dot-position))
    format(stream, " %=", elt);
  end for;
  write(stream, " .");
  for (elt in copy-sequence(right-side, start: dot-position))
    format(stream, " %=", elt);
  end for;
  if (~ item.kernel-item-lookaheads.empty?)  
    write(stream, ", ");
    for (elt in item.kernel-item-lookaheads, i from 0)
      if (i ~== 0) write(stream, "/");  end if;
      format(stream, "%=", elt);
    end for;
  end if;
  write(stream, "}");
end method print-object;

define class <item-set> (<object>)
  //
  // List of items, sorted by item-counter.
  constant slot item-set-items :: <list>, required-init-keyword: #"items";
end class <item-set>;

define method print-object (set :: <item-set>, stream :: <stream>) => ();
  format(stream, "{item-set %=}", set.item-set-items);
end method print-object;

define method make (cls == <item-set>, #next next-method, #key items :: <list>)
 => obj :: <item-set>;
  next-method(<item-set>, 
	      items: sort(items, 
			  test: method (item1 :: <item>, item2 :: <item>)
				  item1.item-counter < item2.item-counter;
				end method));
end method make;

define function item-sets-= (set1 :: <item-set>, set2 :: <item-set>)
  set1.item-set-items = set2.item-set-items;
end function item-sets-=;

define class <state> (<object>)
  //
  // Small integer uniquly identifing this state.
  slot state-number :: <integer> = 0, init-keyword: #"number";
  //
  // List of kernel states.
  slot state-kernels :: <item-set>, required-init-keyword: #"kernels";
  //
  // A-list mapping grammar symbols to next states.
  slot state-gotos :: <list> = #();
  //
  // A-list mapping terminals to actions.  Each action is one of:
  //  (:accept) -- We are done.
  //  (:shift state) -- shift state onto the stack
  //  (:reduce production) -- reduce stack using production
  slot state-actions :: <list> = #();
end class <state>;

define constant <action-kind> = one-of(#"accept", #"shift", #"reduce");

define method print-object (state :: <state>, stream :: <stream>) => ();
  format(stream, "{state %d}", state.state-number);
end method print-object;


// Grammar parsing stuff.

define function find-grammar-symbol (grammar :: <grammar>, thing :: <symbol>)
 => val :: <grammar-symbol>;
  (element(grammar.grammar-symbols, thing, default: #f)
     | begin
	 let new = make(<nonterminal>, name: thing);
	 grammar.grammar-nonterminals 
	   := add!(grammar.grammar-nonterminals, new);
	 grammar.grammar-symbols[thing] := new;
	 new;
       end);
end function find-grammar-symbol;

define function parse-production
    (grammar :: <grammar>, left-side :: <symbol>, right-side :: <list>, 
     body :: <list>) 
 => prod :: <production>;
  let nonterminal = find-grammar-symbol(grammar, left-side);
  let num-productions = grammar.grammar-num-productions;
  let production 
    = make-production(num-productions, nonterminal, 
		      map(method (thing :: <symbol>) 
			   => val :: <grammar-symbol>;
			    find-grammar-symbol(grammar, thing);
			  end method, 
			  right-side), 
		      body);
  grammar.grammar-num-productions := 1 + num-productions;
  nonterminal.nonterminal-productions 
    := add!(nonterminal.nonterminal-productions, production);
  production;
end function parse-production;


// compute firsts.

define function compute-token-union-firsts (grammar :: <grammar>) => ();
  local 
    method maybe-expand-union (union :: <token-union>)
      (if (union.grammar-symbol-first ~== #())
	 union.grammar-symbol-first;
       else
	 let results :: <list> = #();
	 for (member-name in union.token-union-members)
	   let member = find-grammar-symbol(grammar, member-name);
	   select (member by instance?)
	     <nonterminal> =>
	       error("In union %S, member %S is a non-terminal.", 
		     union.grammar-symbol-name, member-name);
	     <token> =>
	       results := add-new!(results, member);
	     <token-union> =>
	       for (member-member in maybe-expand-union(member))
		 results := add-new!(results, member-member);
	       end for;
	   end select;
	 end for;
	 union.grammar-symbol-first := results;
       end if);
    end method maybe-expand-union;
  for (union in grammar.grammar-token-unions)
    maybe-expand-union(union);
  end for;
end function compute-token-union-firsts;


define function compute-nonterminal-firsts (grammar :: <grammar>) => ();
  block (return)
    while (#t)
      let anything-changed = #f;
      for (nonterminal in grammar.grammar-nonterminals)
	for (production in nonterminal.nonterminal-productions)
	  block (escape)
	    for (symbol in production.production-right-side)
	      for (first in symbol.grammar-symbol-first)
		unless (member?(first, nonterminal.grammar-symbol-first))
		  nonterminal.grammar-symbol-first 
		    := add!(nonterminal.grammar-symbol-first, first);
		  anything-changed := #t;
		end unless;
	      end for;
	      unless (symbol.grammar-symbol-nullable)
		escape();
	      end unless;
	    finally
	      unless (nonterminal.grammar-symbol-nullable)
		nonterminal.grammar-symbol-nullable := #t;
		anything-changed := #t;
	      end unless;
	    end for;
	  end block;
	end for;
      end for;
      unless (anything-changed)
	return();
      end unless;
    end while;
  end block;
end function compute-nonterminal-firsts;

define function compute-firsts (grammar :: <grammar>) => ();
  compute-token-union-firsts(grammar);
  compute-nonterminal-firsts(grammar);
end function compute-firsts;


// Compute-items

// MAP-ITEMS -- internal.
//
// Invoke function on each item in closure(item-set).
//
define inline function map-items
    (function :: <function>, item-set :: <item-set>) => ();
  let productions-added :: <list> = #();
  local method grovel (production :: <production>, dot-position :: <integer>)
	  function(production, dot-position);
	  let right-side = production.production-right-side;
	  if (dot-position < right-side.size)
	    let next-symbol = right-side[dot-position];
	    if (instance?(next-symbol, <nonterminal>))
	      for (prod in next-symbol.nonterminal-productions)
		unless (member?(prod, productions-added))
		  productions-added := add!(productions-added, prod);
		  grovel(prod, 0);
		end unless;
	      end for;
	    end if;
	  end if;
	end method grovel;
  for (item in item-set.item-set-items)
    grovel(item.item-production, item.item-dot-position);
  end for;
end function map-items;

define function item-equal (item-1 :: <item>, item-2 :: <item>)
 => answer :: <boolean>;
  item-1.item-production == item-2.item-production 
     & item-1.item-dot-position = item-2.item-dot-position;
end function item-equal;

define function find-kernel-item 
    (production :: <production>, dot-position :: <integer>)
  let vec 
    = production.production-kernel-items 
	 | (production.production-kernel-items 
	      := make(<vector>, 
		      size: 1 + size(production.production-right-side), 
		      fill: #f));
  vec[dot-position] 
    | (vec[dot-position] := make(<kernel-item>, 
				 production: production, 
				 dot-position: dot-position));
end function find-kernel-item;

define function compute-state (grammar :: <grammar>, kernel-items :: <list>)
 => state :: <state>;
  let item-set = make(<item-set>, items: kernel-items);
  (find(item-set, grammar.grammar-all-states, 
	key: state-kernels, test: item-sets-=)
     | begin
	 let state = make(<state>, number: grammar.grammar-num-states, 
			  kernels: item-set);
	 let gotos :: <list> = #();
	 grammar.grammar-all-states 
	   := add!(grammar.grammar-all-states, state);
	 grammar.grammar-num-states := 1 + grammar.grammar-num-states;
	 map-items(method (production :: <production>, 
			   dot-position :: <integer>)
		     let right-side = production.production-right-side;
		     if (dot-position < right-side.size)
		       let new-item 
			 = find-kernel-item(production, 1 + dot-position);
		       let next-symbol = right-side[dot-position];
		       let entry = assoc(next-symbol, gotos, test: \==);
		       if (entry)
			 entry.tail := add!(entry.tail, new-item)
		       else
			 gotos := add!(gotos, list(next-symbol, new-item))
		       end if;
		     end if;
		   end method,
		   item-set);
	 for (goto in gotos)
	   state.state-gotos 
	     := add!(state.state-gotos, 
		     pair(goto.head, compute-state(grammar, goto.tail)));
	 end for;
	 state;
       end);
end function compute-state;

define function compute-states (grammar :: <grammar>) => ();
  grammar.grammar-start-states 
    := map(method (start-production :: <production>)
	     let item = find-kernel-item(start-production, 0);
	     compute-state(grammar, list(item));
	   end method, grammar.grammar-start-productions);
  grammar.grammar-all-states := reverse!(grammar.grammar-all-states);
end function compute-states;


define sealed class <done-table> (<table>) end class <done-table>;

define inline function done-equal (x :: <pair>, y :: <pair>)
  (x.head == y.head & x.tail == y.tail);
end function done-equal;

define inline function done-hash (x :: <pair>, initial-state :: <hash-state>)
 => (id :: <integer>, state :: <hash-state>);
  let (id1, state1) = object-hash(x.head, initial-state);
  let (id2, state2) = object-hash(x.tail, state1);
  let id = merge-hash-ids(id1, id2, ordered: #t);
  values(id, state2);
end function done-hash;

define sealed inline method table-protocol (ht :: <done-table>) 
 => (key-test :: <function>, key-hash :: <function>);
  values(done-equal, done-hash);
end method table-protocol;

// Compute lookaheads.


define function compute-initial-lookaheads (grammar :: <grammar>) => ();
  begin
    let eof-symbol = find-grammar-symbol(grammar, #"EOF");
    unless (instance?(eof-symbol, <token>))
      error("EOF isn't defined as a token.");
    end unless;
    map(method (start-production :: <production>, start-state :: <state>)
	  let temp = find(start-production, 
			  start-state.state-kernels.item-set-items,
			  key: item-production);
	  temp.kernel-item-new-lookaheads 
	    := add!(temp.kernel-item-new-lookaheads, eof-symbol);
	end method,
	grammar.grammar-start-productions, grammar.grammar-start-states);
  end;
  for (state in grammar.grammar-all-states)
    for (kernel-item in item-set-items(state.state-kernels))
      let done :: <done-table> = make(<done-table>);
      local 
	method grovel (item :: <item>, lookahead :: false-or(<token>))
	 => ();
//	  format(*standard-output*, "grovel item=%= lookahead=%=\n",
//		 item, lookahead);
	  let a-prod = item.item-production;
	  let right-side = a-prod.production-right-side;
	  let dot-position = item.item-dot-position;
	  if (dot-position < right-side.size)
	    let next-symbol = right-side[dot-position];

	    let goto :: <state> 
	      = tail(assoc(next-symbol, state.state-gotos));
	    let other-item :: <kernel-item>
	      = find-if(method (item)
			  (a-prod == item.item-production 
			     & (1 + dot-position) == item.item-dot-position);
			end method, 
			goto.state-kernels.item-set-items);
	    if (lookahead)
	      other-item.kernel-item-new-lookaheads 
		:= add!(other-item.kernel-item-new-lookaheads, lookahead)
	    else
	      kernel-item.kernel-item-propagates-to
		:= add!(kernel-item.kernel-item-propagates-to, other-item)
	    end if;
	    if (instance?(next-symbol, <nonterminal>))
	      let tail = copy-sequence(right-side, start: 1 + dot-position);
	      for (b-prod in next-symbol.nonterminal-productions)
		block (return)
		  for (sym in tail)
		    for (first in sym.grammar-symbol-first)
		      maybe-grovel(b-prod, first);
		    end for;
		    unless (sym.grammar-symbol-nullable)
		      return();
		    end unless;
		  finally
		    maybe-grovel(b-prod, lookahead);
		  end for;
		end block;
	      end for;
	    end if;
	  end if;
	end method grovel,
	method maybe-grovel 
	    (production :: <production>, lookahead :: false-or(<token>))
	 => ();
//	  format(*standard-output*, 
//		 "maybe-grovel production=%= lookahead=%=\n",
//		 production, lookahead);
	  let new-entry = pair(production, lookahead);
	  unless (element(done, new-entry, default: #f))
	    done[new-entry] := new-entry;
	    grovel(make(<item>, production: production), lookahead);
	  end unless;
	end method maybe-grovel;
      grovel(kernel-item, #f);
    end for;
  end for;
end function compute-initial-lookaheads;

define function propagate-lookaheads (grammar :: <grammar>) => ();
  block (return)
    while (#t)
      let anything-changed = #f;
      for (state in grammar.grammar-all-states)
	for (item in state.state-kernels.item-set-items)
	  let new = item.kernel-item-new-lookaheads;
	  if (new ~== #())
	    anything-changed := #t;
	    item.kernel-item-new-lookaheads := #();
	    for (lookahead in new)
	      item.kernel-item-lookaheads 
		:= add!(item.kernel-item-lookaheads, lookahead);
	      for (to in item.kernel-item-propagates-to)
		unless (member?(lookahead, to.kernel-item-lookaheads))
		  to.kernel-item-new-lookaheads 
		    := add-new!(to.kernel-item-new-lookaheads, lookahead);
		end unless;
	      end for;
	    end for;
	  end if;
	end for;
      end for;
      if (~anything-changed)  return();  end if;
    end while;
  end block;
end function propagate-lookaheads;


// Compute actions.

define variable *conflicts* :: <integer> = 0;

define function describe-state (state :: <state>) => ();
  for (item in state.state-kernels.item-set-items)
    let production = item.item-production;
    let right-side = production.production-right-side;
    let dot-position = item.item-dot-position;
    format(*standard-output*, "  %= ->", production.production-left-side);
    for (elt in copy-sequence(right-side, start: 0, end: dot-position))
      print(elt, *standard-output*);
    end for; 
    format(*standard-output*, " *");
    for (elt in copy-sequence(right-side, start: dot-position))
      print(elt, *standard-output*);
    end for; 
    new-line(*standard-output*);
  end for;
end function describe-state;

define function describe-action
    (action :: <list>, final-newline :: <boolean>) => ();
  select (action.second)
    #"reduce" =>
      let production = action.third;
      format(*standard-output*, " reduction by production %D:\n  %= ->",
	     production.production-number, production.production-left-side);
      for (elt in production.production-right-side)
	format(*standard-output*, " %=", elt);
      end for;
    #"shift" =>
      let target = action.third;
      format(*standard-output*, " shift to state %D:\n", 
	     target.state-number);
      describe-state(target);
    #"accept" =>
      if (final-newline)
	format(*standard-output*, " accept\n")
      else
	format(*standard-output*, " accept ")
      end if;
    otherwise =>
      error("Strange action kind.");
  end select;
end function describe-action;

define function add-action (state :: <state>, action :: <list>) => ();
  local method add-token-action
	    (token :: <token>, action-kind :: <action-kind>, 
	     action-datum :: type-union(<false>, <state>, <production>))
	  // ### I'm not sure <state> is a possibility, but <false> and 
	  // <production> certainly are
	  let old-action = find(token, state.state-actions, key: head);
	  case
	    (~old-action) =>
	      state.state-actions 
		:= add!(state.state-actions, 
			list(token, action-kind, action-datum));
	    (action-kind == old-action.second 
		& action-datum == old-action.third) =>
	      #f;  // do nothing
	    otherwise =>
	      format(*standard-output*, "\n%=/%= conflict at state %D:\n", 
		     old-action.second, action-kind, state.state-number);
	      describe-state(state);
	      format(*standard-output*, "on token %S between", 
		     token.grammar-symbol-name);
	      describe-action(old-action, #f);
	      format(*standard-output*, "and");
	      describe-action(action, #t);
	      *conflicts* := 1 + *conflicts*;
	      force-output(*standard-output*);
	  end case;
	end method add-token-action;
  let terminal = action.head;
  let action-kind = action.second;
  // for action-datum, any old crap will do for the default (as long
  // as the signature for add-token-action is adjusted accordingly)
  let action-datum = element(action, 2, default: #f);  
  select (terminal by instance?)
    <token> =>
      add-token-action(terminal, action-kind, action-datum);
    <token-union> =>
      for (token in terminal.grammar-symbol-first)
	add-token-action(token, action-kind, action-datum);
      end for;
  end select;
end function add-action;

define function compute-actions (grammar :: <grammar>) => ();
  for (state in grammar.grammar-all-states)
    for (kernel-item in state.state-kernels.item-set-items)
      let done :: <done-table> = make(<done-table>);
      local
	method grovel (item :: <item>, lookahead :: <terminal>)
	  let a-prod = item.item-production;
	  let right-side = a-prod.production-right-side;
	  let dot-posn = item.item-dot-position;
	  if (dot-posn < right-side.size)
	    let next-symbol = right-side[dot-posn];
	    if (instance?(next-symbol, <nonterminal>))
	      let tail = copy-sequence(right-side, start: 1 + dot-posn);
	      for (b-prod in next-symbol.nonterminal-productions)
		block (return)
		  for (sym in tail)
		    for (first in sym.grammar-symbol-first)
		      maybe-grovel(b-prod, first);
		    end for;
		    unless (sym.grammar-symbol-nullable)
		      return();
		    end unless;
		  finally
		    maybe-grovel(b-prod, lookahead);
		  end for;
		end block;
	      end for;
	    else
	      let goto = assoc(next-symbol, state.state-gotos, test: \==);
	      if (goto)
		add-action(state, list(next-symbol, shift: goto.tail));
	      end if;
	    end if;
	  else
	    add-action(state, 
		       if (member?(a-prod, grammar.grammar-start-productions, 
				   test: \==))
			 list(lookahead, #"accept")
		       else
			 list(lookahead, reduce: a-prod)
		       end if);
	  end if;
	end method grovel,
	method maybe-grovel 
	    (production :: <production>, lookahead :: <terminal>)
	  let entry = pair(production, lookahead);
	  let thing = element(done, entry, default: #f);
	  if (thing == #() ) error("unexpected"); end if;
	  unless (thing)
	    done[entry] := entry;
	    grovel(make(<item>, production: production), lookahead);
	  end unless;
	end method maybe-grovel;
      for (lookahead in kernel-item.kernel-item-lookaheads)
	grovel(kernel-item, lookahead);
      end for;
    end for;
  end for;
end function compute-actions;



// Compute gotos

define function add-gotos (gotos :: <list>, number :: <integer>, new :: <list>)
 => gotos :: <list>;
  for (goto in new)
    let symbol = goto.head;
    if (instance?(symbol, <nonterminal>))
      let target = goto.tail.state-number;
      let name = symbol.grammar-symbol-name;
      let entry = assoc(name, gotos);
      if (entry)
	entry.tail := add!(entry.tail, pair(number, target))
      else
	gotos := add!(gotos, list(name, pair(number, target)))
      end if;
    end if;
  end for;
  gotos;
end function add-gotos;

define function compact-gotos (gotos :: <list>) => gotos :: <list>;
  map(method (goto :: <list>) => something :: <pair>;
	let nonterm = goto.head;
	let counts :: <list> = #();
	let most-common = #();
	let occurrences = 0;
	for (transition in goto.tail)
	  let entry = assoc(transition.tail, counts);
	  if (entry)
	    entry.tail := entry.tail + 1;
	  else
	    counts := add!(counts, pair(transition.tail, 1))
	  end if;
	end for;
	for (count in counts)
	  if (count.tail > occurrences)
	    occurrences := count.tail;
	    most-common := count.head;
	  end if;
	end for;
	let without-default
	  = remove(goto.tail, most-common, 
		   test: method (elt, common)
			   elt.tail = common;
			 end method);
	let length = without-default.size;
	case
	  (without-default == #()) =>
	    pair(nonterm, most-common);
	  (length <= 8) =>
	    pair(nonterm, concatenate(map(method (transition)
					    list(transition.head,
						 transition.tail);
					  end method, 
					  without-default),
				      list(list(#"otherwise", most-common))));
	  otherwise =>
	    let max-cur = 0;
	    for (transition in goto.tail)
	      if (transition.head > max-cur)
		max-cur := transition.head;
	      end if;
	    end for;
	    // ### Somehow the lisp version managed to work without
	    // specifying fill: 0 (or :initial-element, in lisp), but
	    // we can't get away with that.
	    let result = make(<vector>, size: 1 + max-cur, fill: 0);
	    for (transition in goto.tail)
	      result[transition.head] := transition.tail;
	    end for;
	    pair(nonterm, result);
	end case;
      end method, 
      gotos);
end function compact-gotos;

define function compute-gotos (grammar :: <grammar>) => gotos :: <list>;
  let gotos :: <list> = #();
  for (state in grammar.grammar-all-states)
    gotos := add-gotos(gotos, state.state-number, state.state-gotos);
  end for;
  compact-gotos(gotos);
end function compute-gotos;


// Emitter

define function dump-constant (thing :: <object>, ofile :: <stream>) => ();
  select (thing by instance?)
    <simple-vector> =>
      format(ofile, "#[");
      for (i from 0 below thing.size)
	unless (zero?(i))
	  format(ofile, ", ");
	end unless;
	dump-constant(thing[i], ofile);
      end for;
      format(ofile, "]");
    <list> =>
      format(ofile, "#(");
      if (~thing.empty?)
	dump-constant(thing.head, ofile);
	block (return)
	  for (remainder = thing.tail then remainder.tail,
	       until: remainder == #())
	    case
	      (instance?(remainder, <list>)) =>
		format(ofile, ", ");
		dump-constant(remainder.head, ofile);
	      otherwise =>
		format(ofile, " . ");
		dump-constant(remainder, ofile);
		return();
	    end case;
	  end for;
	end block;
      end if;
      format(ofile, ")");
//    type-union(<symbol>, <integer>, <string>, <boolean>) =>  // ### didn't have <boolean> before
    type-union(<symbol>, <integer>, <string>) =>
      print(thing, ofile);
  end select;
#if (compiled-for-cygnus)
  force-output(ofile); // cygnus dies if the buffer gets too big
#endif
end function dump-constant;

define function emit-production
    (production :: <production>, 
     gotos :: type-union(<integer>, <list>, <vector>), 
     ofile :: <stream>) 
 => ();
  let right-side = production.production-right-side;
  let left-side = production.production-left-side;
  format(ofile, "define method production-%D\n",
	 production.production-number);
  format(ofile, 
	 "    (prev-state :: <integer>, srcloc-0 :: <source-location>");

  for (grammar-sym in right-side, index from 1)
    format(ofile, ",\n     rhs-%D", index);
    if (grammar-sym.grammar-symbol-type)
      format(ofile, " :: %S", 
	     as-uppercase(as(<string>, grammar-sym.grammar-symbol-type)));
    end if;
    format(ofile, ", srcloc-%D :: <source-location>", index);
  end for;

  format(ofile, ")\n");
  format(ofile, "    => (new-state :: <integer>, new-symbol");
  if (left-side.grammar-symbol-type)
    format(ofile, " :: %S", 
	   as-uppercase(as(<string>, left-side.grammar-symbol-type)));
  end if;
  format(ofile, ");\n");
  format(ofile, "  /" "/ %S\n", production);
  format(ofile, "  values(");
  select (gotos by instance?)
    <integer> =>
      format(ofile, "%D", gotos);
    <list> =>
      format(ofile, "select (prev-state)\n");
      for (goto in gotos)
	if (goto.head == #"otherwise")
	  format(ofile, "           OTHERWISE => %=;\n", goto.tail.head);
	else
	  format(ofile, "           %= => %=;\n", goto.head, goto.tail.head);
	end if;
      end for;
      format(ofile, "         end");
    <vector> =>
      dump-constant(gotos, ofile);
      format(ofile, "[prev-state]");
    otherwise => 
      error("How'd we get here?");
  end select;
  format(ofile, ",\n");
  format(ofile, "         begin\n");
  for (form in production.production-body)
    if (~form.empty?)  // ### Why do we have empty forms in the first place?
      format(ofile, "         %s\n", form);
    end if;
  end for;
  format(ofile, "         end);\n");
  format(ofile, "end method production-%D;\n\n", 
	 production.production-number);
#if (compiled-for-cygnus)
  force-output(ofile);
#endif
end function emit-production;

define function encode-actions (actions :: <list>, grammar :: <grammar>)
 => encoded-actions :: <vector>;
  let vec = make(<vector>, size: grammar.grammar-next-token-id, fill: 0);
  for (action in actions)
    let (action-kind, datum) 
      = select (action.second)
	  #"accept" => values(1, 0);
	  #"reduce" => values(2, action.third.production-number);
	  #"shift" => values(3, action.third.state-number);
	end select;
    vec[action.first.token-id] := logior(ash(datum, 2), action-kind);
  end for;
  vec;
end function encode-actions;

define function emit-parser
    (grammar :: <grammar>, #key ofile = (*standard-output*))
 => ();
  begin
    let num-tokens = grammar.grammar-next-token-id;
    let tokens = make(<vector>, size: num-tokens);
    for (token in grammar.grammar-tokens)
      tokens[token.token-id] := token;
    end for;
    format(ofile, "define constant $action-bits = 2;\n");
    format(ofile, 
	   "define constant $action-mask = ash(1, $action-bits) - 1;\n\n");
    format(ofile, "define constant $error-action = 0;\n");
    format(ofile, "define constant $accept-action = 1;\n");
    format(ofile, "define constant $reduce-action = 2;\n");
    format(ofile, "define constant $shift-action = 3;\n\n");
  end;
  format(ofile, "define constant $action-table\n"
	   "  = #[");
#if (compiled-for-cygnus)
  force-output(ofile);
#endif

  for (state in grammar.grammar-all-states, index from 0)
    unless (index = state.state-number)
      error("State numbers got out of sync.");
    end unless;
    unless (zero?(index))
      format(ofile, ",\n      ");
    end unless;
    dump-constant(encode-actions(state.state-actions, grammar), ofile);
  end for;
  format(ofile, "];\n\n");
  begin
    let num-productions = grammar.grammar-num-productions;
    let productions-vector 
      = make(<vector>, size: num-productions, fill: #f);
    let num-pops-vector = make(<vector>, size: num-productions, fill: 0);
    let gotos-table = compute-gotos(grammar);
    for (nonterminal in grammar.grammar-nonterminals)
      let name = nonterminal.grammar-symbol-name;
      // ### I don't know what something is, but we need a variable here
      let something = assoc(name, gotos-table);
      if (something ~== #f)
	let gotos = something.tail;
	for (production in nonterminal.nonterminal-productions)
	  let prod-num = production.production-number;
	  unless (zero?(prod-num))
	    productions-vector[prod-num] := pair(production, gotos);
	    num-pops-vector[prod-num] 
	      := production.production-right-side.size;
	  end unless;
	end for
      else
	unless (find(nonterminal, grammar.grammar-start-productions, 
		     key: production-left-side))
	  warning("Nonterminal %S can't appear.", name);
	end unless
      end if;
    end for;
    for (i from 0 below num-productions)
      let info = productions-vector[i];
      if (info)
	emit-production(info.head, info.tail, ofile);
      end if;
    end for;
    format(ofile, "define constant $number-of-pops\n  = ");
    dump-constant(num-pops-vector, ofile);
    format(ofile, ";\n\n");
    format(ofile, 
	   "define constant $production-table :: <simple-object-vector>\n"
	     "  = vector(");
    for (i from 0 below num-productions)
      unless (zero?(i))
	format(ofile, ", ");
      end unless;
      if (productions-vector[i])
	format(ofile, "production-%D", i)
      else
	format(ofile, "#f")
      end if;
    end for;
    format(ofile, ");\n\n");
  end;
  map(method (entry-point, start-state :: <state>) => ();
	 format(ofile, "define constant $%S-start-state = %D;\n", 
		as-lowercase(as(<string>, entry-point)), 
		start-state.state-number);
       end method, 
       grammar.grammar-entry-points, 
       grammar.grammar-start-states);
#if (compiled-for-cygnus)
  force-output(ofile);
#endif
end function emit-parser;



// Log file emitter.

define function print-state
    (state :: <state>, line-prefix :: <string>, line-suffix :: <string>, 
     file :: <stream>)
 => ();
  for (item in state.state-kernels.item-set-items)
    let production = item.item-production;
    let right-side = production.production-right-side;
    let dot-position = item.item-dot-position;
    format(file, "%S%= ->", line-prefix, production.production-left-side);
    for (elt in copy-sequence(right-side, start: 0, end: dot-position))
      format(file, " %=", elt);
    end for;
    format(file, " *");
    for (elt in copy-sequence(right-side, start: dot-position))
      format(file, " %=", elt);
    end for;
    format(file, "\n%S", line-suffix);
  end for;
end function print-state;

define function emit-log-file (grammar :: <grammar>, file :: <stream>) => ();
  format(file, "%D tokens, %D non-terminals, %D productions, %D states.\n\n", 
	 grammar.grammar-next-token-id, 
	 grammar.grammar-nonterminals.size,
	 grammar.grammar-num-productions, grammar.grammar-num-states);
  format(file, "ID Token Type\n");
  for (token in grammar.grammar-tokens)
    format(file, "%D %S %S\n", token.token-id, token.grammar-symbol-name, 
	   token.grammar-symbol-type | "#f");
  end for;
  new-line(file);
  for (state in grammar.grammar-all-states)
    format(file, "State %D:\n", state.state-number);
    print-state(state, "    ", "", file);
    new-line(file);
    begin
      let actions :: <list> = #();
      for (action in state.state-actions)
	let entry = assoc(action.tail, actions, test: \=);
	if (entry)
	  entry.tail := add!(entry.tail, action.head)
	else
	  actions := add!(actions, list(action.tail, action.head))
	end if;
      end for;
      for (entry in actions)
	format(file, "  on:");
	for (token in sort(entry.tail, 
			   test: method (obj1, obj2)
				   obj1.token-id < obj2.token-id
				 end method))
	  format(file, " %=", token);
	end for;
	let action = entry.head;
	select (action.first)
	  #"accept" => 
	    format(file, "\n   accept\n");
	  #"shift" =>
	    format(file, "\n   shift to:\n");
	    print-state(action.second, "     ", "", file);
	  #"reduce" => 
	    format(file, "\n   reduce by:\n     %=\n", action.second);
	end select;
	new-line(file);
      end for;
      new-line(file);
    end;
  end for;
#if (compiled-for-cygnus)
  force-output(file);
#endif
end function emit-log-file;


// Source file groveler.

define function grovel-header (ifile :: <stream>, ofile :: <stream>) => ();
  block (return)
    while (#t)
      let line = read-line(ifile);
      if (line = "%%")
	return();
      end if;
      write-line(ofile, line);
    end while;
  end block;
#if (compiled-for-cygnus)
  force-output(ofile);
#endif
end function grovel-header;

define function parse-grammar (grammar :: <grammar>, ifile :: <stream>) => ();
  block (return)
    while (#t)
      let lhs = lisp-read(ifile);
      if (lhs == #"%%")
	return();
      end if;
      select (lhs)
	#"entry-point" =>
	  let ep = lisp-read(ifile);
	  let prime = as(<symbol>, 
			 concatenate-as(<string>, as(<string>, ep), "-PRIME"));
	  grammar.grammar-entry-points 
	    := add!(grammar.grammar-entry-points, ep);
	  grammar.grammar-start-productions 
	    := add!(grammar.grammar-start-productions,
		    parse-production(grammar, prime, list(ep), #()));
	#"token" =>
	  let name = lisp-read(ifile);
	  let type = lisp-read(ifile);
	  let old = element(grammar.grammar-symbols, name, default: #f);
	  select (old by instance?)
	    <token> => 
	      error("token %S multiply defined.", name);
	    <token-union> =>
	      error("%S previously defined to be a token-union", name);
	    <nonterminal> => 
	      if (old.nonterminal-productions)
		error("%S was previously defined to be a non-terminal", name)
	      else
		error("%S was previously assumed to be a non-terminal", name)
	      end if;
	    otherwise => 
	      #f;  // do nothing
	  end select;
	  let id = grammar.grammar-next-token-id;
	  let new = make(<token>, name: name, type: type, id: id);
	  grammar.grammar-tokens := add!(grammar.grammar-tokens, new);
	  grammar.grammar-next-token-id := 1 + id;
	  grammar.grammar-symbols[name] := new;
	#"union" =>
	  let name = lisp-read(ifile);
	  let type = lisp-read(ifile);
	  let members = lisp-read(ifile);
	  let old = element(grammar.grammar-symbols, name, default: #f);
	  select (old by instance?)
	    <token> => 
	      error("%S previously defined to be an atomic token", name);
	    <token-union> => 
	      error("token-union %S multiply defined", name);
	    <nonterminal> => 
	      if (old.nonterminal-productions)
		error("%S was previously defined to be a non-terminal", name);
	      else
		error("%S was previously assumed to be a non-terminal", name);
	      end if;
	    otherwise => 
	      #f; // do nothing
	  end select;
	  let new = make(<token-union>, 
			 name: name, type: type, members: members);
	  grammar.grammar-token-unions
	    := add!(grammar.grammar-token-unions, new);
	  grammar.grammar-symbols[name] := new;
	#"type" =>
	  let name = lisp-read(ifile);
	  let type = lisp-read(ifile);
	  let nonterminal = find-grammar-symbol(grammar, name);
	  unless (instance?(nonterminal, <nonterminal>))
	    error("%S is a terminal, change change its type now.", name);
	  end unless;
	  if (nonterminal.grammar-symbol-type)
	    warning("Multiple types defined for nonterminal %S", name);
	  end if;
	  nonterminal.grammar-symbol-type := type;
	otherwise =>
	  let rhs = lisp-read(ifile);
	  let body = make(<stretchy-vector>);
	  block (return)
	    while (#t)
	      let line = read-line(ifile);
	      if (line = "%")
		return();
	      end if;
	      body := add!(body, expand-percents-and-ats(line));
	    end while;
	  end block;
	  parse-production(grammar, lhs, rhs, as(<list>, body));
      end select;
    end while;
  end block;
  let undefined
    = remove(grammar.grammar-nonterminals, #(), 
	     test: method (elt, empty-list)
		     elt.nonterminal-productions ~== #();
		   end method);
  if (~undefined.empty?)
    error("Undefined nonterminals: %=", 
	  map(grammar-symbol-name, undefined));
  end if;
  grammar.grammar-entry-points := reverse!(grammar.grammar-entry-points);
  grammar.grammar-tokens := reverse!(grammar.grammar-tokens);
  grammar.grammar-start-productions 
    := reverse!(grammar.grammar-start-productions);
end function parse-grammar;

define function expand-percents-and-ats (line :: <byte-string>) 
 => line :: <byte-string>;
  let end-pos :: <integer> = line.size;
  let escaped :: <boolean> = #f;
  let in-string :: <boolean> = #f;
  // You can't write this as a simple "for (index from 0 below end-pos)"
  // because end-pos is being modified mid-loop
  for (index = 0 then index + 1, until: index >= end-pos)
    let char = line[index];
    case
      (char == '%') =>
	unless (in-string)
	  line := concatenate-as(<string>, 
				 copy-sequence(line, start: 0, end: index),
				 "rhs-", 
				 copy-sequence(line, start: 1 + index));
	  end-pos := line.size;
	end unless;
	escaped := #f;
      (char == '@') =>
	unless (in-string)
	  line := concatenate-as(<string>, 
				 copy-sequence(line, start: 0, end: index),
				 "srcloc-", 
				 copy-sequence(line, start: 1 + index));
	  end-pos := line.size;
	end unless;
	escaped := #f;
      (char == '\\') =>
	escaped := ~escaped;
      (char == '"') =>
	unless (escaped)
	  in-string := ~in-string;
	end unless;
	escaped := #f;
    end case;
  end for;
  line;
end function expand-percents-and-ats;


define function grovel-trailer (ifile :: <stream>, ofile :: <stream>) => ();
  block (return)
    while (#t)
      let line = read-line(ifile, on-end-of-stream: #f);
      if (~line) return() end if;
      write-line(ofile, line);
    end while;
  end block;
#if (compiled-for-cygnus)
  force-output(ofile);
#endif
end function grovel-trailer;

define function grovel-file
    (iname :: <string>, oname :: false-or(<string>), 
     logname :: false-or(<string>)) 
 => ();
  let ifile = make(<file-stream>, locator: iname);
  let ofile = if (oname)
		make(<file-stream>, locator: oname, direction: #"output");
	      else
		*standard-output*;
	      end if;
  grovel-header(ifile, ofile);
  let grammar = make(<grammar>);
  parse-grammar(grammar, ifile);
  compute-firsts(grammar);
  compute-states(grammar);
  compute-initial-lookaheads(grammar);
  propagate-lookaheads(grammar);
  compute-actions(grammar);
  emit-parser(grammar, ofile: ofile);
  if (logname)
    let logfile = make(<file-stream>, direction: #"output", locator: logname);
    emit-log-file(grammar, logfile);
    close(logfile);
  end if;
  grovel-trailer(ifile, ofile);
  if (*conflicts* ~== 0)
    warning("%d conflicts.", *conflicts*);
  end if;
  close(ifile);
  if (oname)
    close(ofile);
  end if;
end function grovel-file;


// Main

define method main (ignored, #rest args)
  if (args.size == 0 | args.size > 3)
    format
      (*standard-error*, 
       "Usage: parsergen input-file.input [output-file.dylan [log-file.log]]\n"
	 "\n"
	 "\tIf no output-file is specified, output defaults to standard\n"
	 "\toutput.  If no log-file is specified, a log is not outputted.\n");
    force-output(*standard-error*);
    exit(exit-code: 1);
  end if;
  let input = args.first;
  let output = if (args.size > 1) args.second else #f end if;
  let log = if (args.size > 2) args.third else #f  end if;
  format(*standard-error*, "Creating parser...\n");
  force-output(*standard-error*); // ### Do we need to force output on stderr?
  grovel-file(input, output, log);
  exit(exit-code: 0);
end method main;


// Seals
define sealed domain make (singleton(<grammar>));
define sealed domain initialize (<grammar>);
define sealed domain make (singleton(<grammar-symbol>));
define sealed domain initialize (<grammar-symbol>);
define sealed domain make (singleton(<terminal>));
define sealed domain initialize (<terminal>);
define sealed domain make (singleton(<token>));
define sealed domain initialize (<token>);
define sealed domain make (singleton(<token-union>));
define sealed domain initialize (<token-union>);
define sealed domain make (singleton(<nonterminal>));
define sealed domain initialize (<nonterminal>);
define sealed domain make (singleton(<production>));
define sealed domain initialize (<production>);
define sealed domain make (singleton(<item>));
define sealed domain initialize (<item>);
define sealed domain make (singleton(<kernel-item>));
define sealed domain initialize (<kernel-item>);
define sealed domain make (singleton(<item-set>));
define sealed domain initialize (<item-set>);
define sealed domain make (singleton(<state>));
define sealed domain initialize (<state>);


// Lisp emulation (See lisp-read.dylan for the lisp-read() function)

define inline function find (item :: <object>, sequence :: <sequence>,
			     #key key = identity, test = \==)
 => item-found :: <object>;
  local method equals? (elt, item)
	  test(elt.key, item);
	end method equals?;
  block (return)
    for (elt in sequence)
      if (equals?(elt, item))
	return(elt);
      end if;
    end for;
    #f;
  end block;
end function find;

define inline function find-if
    (predicate? :: <function>, sequence :: <sequence>)
 => item-found :: <object>;
  find(#f, sequence, test: method (item, ignored) item.predicate? end method);
end function find-if;

define function warning (string :: <string>, #rest args) => ();
  let condition = make(<simple-warning>, 
		       format-string: string, format-arguments: args);
  signal(condition);
end function warning;

define inline function assoc (item :: <object>, alist :: <list>, 
			      #key test = \==, key = identity)
 => stuff-found :: false-or(<pair>);
  local method equals? (elt, obj)
	  test(elt.key, obj);
	end method equals?;
  block (return)
    for (elt in alist)
      if (equals?(elt.head, item))
	return(elt);
      end if;
    end for;
    #f;
  end block;
end function assoc;
