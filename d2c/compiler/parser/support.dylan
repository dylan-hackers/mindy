module: parser
rcs-header: $Header: /scm/cvs/src/d2c/compiler/parser/support.dylan,v 1.1 1998/05/03 19:55:29 andreas Exp $
copyright: Copyright (c) 1996  Carnegie Mellon University
	   All rights reserved.



//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
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
//    University.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports, questions, comments, and suggestions should be sent by
// E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
//
//======================================================================

// process-top-level-form -- exported.
//
// This open generic is called by the parser for each top level form it
// encounters.  Other modules should define methods on it to pick off
// cases they know how to deal with.
// 
define open generic process-top-level-form (form :: <constituent-parse>) => ();



// Utility functions used by semantic actions.

// stretchy-vector -- internal.
//
// Like vector, but make a stretchy vector.
//
define constant stretchy-vector =
  method (#rest things) => res :: <stretchy-vector>;
    as(<stretchy-vector>, things);
  end;


// make-body -- internal.
//
// Return a expression-parse for the body containting the supplied parts.
// If there is only one part at it is already an expression-parse, just
// return it.  Otherwise, make a body-parse.
// 
define method make-body
    (parts :: <stretchy-vector>, srcloc :: <source-location>)
    => res :: <expression-parse>;
  if (parts.size == 1)
    let form = parts[0];
    if (instance?(form, <expression-parse>))
      form;
    else
      make(<body-parse>, source-location: simplify-source-location(srcloc),
	   parts: vector(form));
    end;
  else
    make(<body-parse>, source-location: simplify-source-location(srcloc),
	 parts: as(<simple-object-vector>, parts));
  end;
end method make-body;



// first-word-in -- internal to the parser.
//
// Extracts the first word in a fragment, or returns #f if the fragment
// doesn't start with a word.  Used by definition-macro-call to check
// the definition-tail.
//
define generic first-word-in (fragment :: <fragment>)
    => res :: false-or(<identifier-token>);

// first-word-in{<fragment>}
//
// Catch-all method that just returns #f.  Exceptions explicitly enumerated
// below.
// 
define method first-word-in (fragment :: <fragment>)
    => res :: false-or(<identifier-token>);
  #f;
end method first-word-in;

// first-word-in{<compound-fragment>}
//
// Just return the first word in the fragment's head.
// 
define method first-word-in (fragment :: <compound-fragment>)
    => res :: false-or(<identifier-token>);
  first-word-in(fragment.fragment-head);
end method first-word-in;

// first-word-in{<token-fragment>}
//
// Check to see if the token is some kind of name token.  If so, return it.
// Otherwise, return #f.
// 
define method first-word-in (fragment :: <token-fragment>)
    => res :: false-or(<identifier-token>);
  let token = fragment.fragment-token;
  let kind = token.token-kind;
  if (kind >= $define-token & kind <= $quoted-name-token)
    token;
  else
    #f;
  end if;
end method first-word-in;


// binary operand series
//
// Binop-series are used to represent the pending binary operations that we
// haven't be able to convert into function calls yet because of precedence
// rules.
// 

// <binop-series> -- internal to the parser.
//
// Both the abstract class for binop-series of arbitrary length and the
// concrete (direct) class for binop-series that are just one long.  Kinda
// sleezy, but I don't feel like defining a <binop-series-leaf> just to
// reduce the sleeze.
// 
define class <binop-series> (<object>)
  //
  // The operand, as an expression parse.
  slot binop-series-operand :: <expression-parse>,
    required-init-keyword: operand:;
  //
  // That operand's source location.  Not the same as operand.source-location,
  // because the source-location slot holds a minimized source location,
  // while this is the full compound location in all its glory.  We need this
  // so we can use source-location-spanning to compute the source location
  // for function calls.
  slot binop-series-operand-srcloc :: <source-location>,
    required-init-keyword: operand-srcloc:;
end class <binop-series>;

define sealed domain make (singleton(<binop-series>));
define sealed domain initialize (<binop-series>);

// <binop-series-stack> -- internal to the parser.
//
// An additional operator and operand tacked onto the stuff to our left.
// 
define class <binop-series-stack> (<binop-series>)
  //
  // The stuff to our left.
  slot binop-series-to-left :: <binop-series>,
    required-init-keyword: to-left:;
  //
  // The operator combining the stuff to our left with this operand (inherited
  // from <binop-series>).
  slot binop-series-operator :: <operator-token>,
    required-init-keyword: operator:;
end class <binop-series-stack>;

define sealed domain make (singleton(<binop-series-stack>));

// reduce-binop-series -- internal to the parser.
//
// Reduce the remaining operations into function calls and return the
// final expression.
// 
define generic reduce-binop-series
    (series :: <binop-series>) => res :: <expression-parse>;

// reduce-binop-series{<binop-series>}
//
// Just return the operand, because it is reduced as far as it can go.
// 
define method reduce-binop-series
    (series :: <binop-series>) => res :: <expression-parse>;
  series.binop-series-operand;
end method reduce-binop-series;

// reduce-binop-series{<binop-series-stack>}
//
// Use reduce-once to peal off one layer and try again.  Eventually we will
// end up in the other method, and hence ground out.
// 
define method reduce-binop-series
    (series :: <binop-series-stack>) => res :: <expression-parse>;
  reduce-binop-series(reduce-once(series));
end method reduce-binop-series;

// add-binop -- internal to the parser.
//
// Add a operator and operand to the series, reducing whatever we can
// into function calls.
// 
define method add-binop
    (series :: <binop-series>, operator :: <operator-token>,
     operand :: <expression-parse>, operand-srcloc :: <source-location>)
    => series :: <binop-series>;
  make(<binop-series-stack>,
       to-left: reduce-some(series, operator.operator-precedence,
			    operator.operator-associativity),
       operator: operator, operand: operand,
       operand-srcloc: operand-srcloc);
end method add-binop;
       
// reduce-some -- internal this page
//
// Reduce as much of the series as possible given that it is followed by an
// operator with the given precedence and associativity.
// 
define generic reduce-some
    (series :: <binop-series>, precedence :: <integer>,
     associativity :: one-of(#"left", #"right"))
    => res :: <binop-series>;

// reduce-some{<binop-series>}
//
// We are at the end, so we can't reduce it any further.
// 
define method reduce-some
    (series :: <binop-series>, precedence :: <integer>,
     associativity :: one-of(#"left", #"right"))
    => res :: <binop-series>;
  series;
end method reduce-some;

// reduce-some{<binop-series-stack>}
//
// Compare the operator on the top of the stack with precedence.  If the
// previous operator has a higher precedence (i.e. binds tighter), then
// reduce it and try again.  Otherwise, return the series as is.
// 
define method reduce-some
    (series :: <binop-series-stack>, precedence :: <integer>,
     associativity :: one-of(#"left", #"right"))
    => res :: <binop-series>;
  let prev-precedence = series.binop-series-operator.operator-precedence;
  if (select (associativity)
	#"left" => prev-precedence >= precedence;
	#"right" => prev-precedence > precedence;
      end select)
    reduce-some(reduce-once(series), precedence, associativity);
  else
    series;
  end if;
end method reduce-some;

// reduce-once -- internal to this page.
//
// Pop a layer off the stack replacing the exposed second layer operand
// with the top operator applied to the second and top operands.
// 
define method reduce-once (series :: <binop-series-stack>)
    => res :: <binop-series>;
  let to-left = series.binop-series-to-left;
  let (call, srcloc)
    = make-binary-function-call(to-left.binop-series-operand,
				to-left.binop-series-operand-srcloc,
				series.binop-series-operator,
				series.binop-series-operand,
				series.binop-series-operand-srcloc);
  to-left.binop-series-operand := call;
  to-left.binop-series-operand-srcloc := srcloc;
  to-left;
end method reduce-once;

// make-binary-function-call -- internal.
//
// Make a function call out of the operator and the two arguments.
// 
define method make-binary-function-call
    (left :: <expression-parse>, left-srcloc :: <source-location>,
     operator :: <operator-token>,
     right :: <expression-parse>, right-srcloc :: <source-location>)
    => (res :: <expression-parse>, srcloc :: <source-location>);
  //
  // Compute the call source location.
  let call-srcloc = source-location-spanning(left-srcloc, right-srcloc);
  //
  // First, check to see if we are expanding into a function-macro call or
  // a regular function call.
  let symbol = operator.token-symbol;
  let module = operator.token-module;
  let (kind, categories) = syntax-for-name(module.module-syntax-table, symbol);
  if (member?(#"function", categories))
    //
    // It is a function-word.  So make a function-macro-call.
    let id = make(<identifier-token>,
		  source-location:
		    simplify-source-location(operator.source-location),
		  kind: $raw-function-word-token,
		  symbol: symbol,
		  module: module,
		  uniquifier: operator.token-uniquifier);
    let left-frag
      = make-parsed-fragment(left, source-location: left-srcloc);
    let comma-srcloc = source-location-before(right-srcloc);
    let comma-token
      = make(<token>, source-location: comma-srcloc, kind: $comma-token);
    let comma-frag
      = make(<token-fragment>, source-location: comma-srcloc,
	     token: comma-token);
    let right-frag
      = make-parsed-fragment(right, source-location: right-srcloc);
    values(make(<function-macro-call-parse>,
		source-location: simplify-source-location(call-srcloc),
		word: id,
		fragment:
		  append-fragments!(append-fragments!(left-frag, comma-frag),
				    right-frag)),
	   call-srcloc);
  else
    //
    // It's not a function-word, so treat it like a regular function
    // call to the quoted name.  If some bozo defines an operator to be a
    // statement macro, they will get the same error as they would if they
    // tried to call \begin(1, 2, 3).  Letting that code deal with it is
    // easier than dealing with it here also.
    let id = make(<identifier-token>,
		  source-location:
		    simplify-source-location(operator.source-location),
		  kind: $quoted-name-token,
		  symbol: operator.token-symbol,
		  module: module,
		  uniquifier: operator.token-uniquifier);
    values(make(<funcall-parse>,
		source-location: simplify-source-location(call-srcloc),
		function: make(<varref-parse>, id: id),
		arguments: vector(left, right)),
	   call-srcloc);
  end if;
end method make-binary-function-call;


// <case-body-parse-state> -- internal to the parser.
//
// Convenient handle on the state during a parse of a case-body.  Case-bodies
// are rather hairy, and can't easily be described with a LALR grammar.  So
// we have to bend over backwards to get them to parse at all.  And as a
// result, the semantic actions don't cleanly correspond to the components of
// the case-body.  So we use this structure and these utility functions to
// accumulate the results.
// 
define class <case-body-parse-state> (<object>)
  //
  // As much of the fragment as we have accumulated so far.  Will end in a
  // case label.  Except when we are all done, at which time the final
  // body gets appended to it.
  slot fragment :: <fragment>,
    required-init-keyword: fragment:;
  //
  // The constituents in the next branch that we have accumulated so far.
  // Possibly empty.
  slot partial-body :: <stretchy-vector> = make(<stretchy-vector>),
    init-keyword: constituents:;
  //
  // The source location for the partial body.
  slot partial-body-srcloc :: false-or(<source-location>) = #f,
    init-keyword: constituents-srcloc:;
  //
  // The semicolon that terinated the last constituent.  Might also terminate
  // the whole branch, but we don't know that until we figure out if the next
  // thing is anothing constituent or a new case label.
  slot semicolon :: false-or(<token-fragment>) = #f;
end class <case-body-parse-state>;

define sealed domain make (singleton(<case-body-parse-state>));
define sealed domain initialize (<case-body-parse-state>);

define method push-case-fragment
    (state :: <case-body-parse-state>, frag :: <fragment>) => ();
  state.fragment := append-fragments!(state.fragment, frag);
end method push-case-fragment;

define method push-case-constituent
    (state :: <case-body-parse-state>,
     constituent :: <constituent-parse>,
     srcloc :: <source-location>)
    => ();
  add!(state.partial-body, constituent);
  state.partial-body-srcloc
    := if (state.partial-body-srcloc)
	 source-location-spanning(state.partial-body-srcloc, srcloc);
       else
	 srcloc;
       end if;
end method push-case-constituent;

define method finish-case-body (state :: <case-body-parse-state>) => ();
  let body = as(<simple-object-vector>, state.partial-body);
  let srcloc
    = (state.partial-body-srcloc
	 | source-location-after(state.fragment.source-location));
  state.partial-body.size := 0;
  state.partial-body-srcloc := #f;
  push-case-fragment
    (state,
     make-parsed-fragment
       (make(<body-parse>, source-location: srcloc, parts: body),
	source-location: srcloc));
  if (state.semicolon)
    push-case-fragment(state, state.semicolon);
    state.semicolon := #f;
  end if;
end method finish-case-body;


// remove-optional-semi-and-end -- internal.
//
// Check to see if pattern ends in ``;-opt END'' and if so, return
// a new pattern that contains everything but them.
// 
define generic remove-optional-semi-and-end (pattern :: <pattern>)
    => (new-pattern :: <pattern>, found-end? :: <boolean>);

define method remove-optional-semi-and-end (pattern :: <semicolon-pattern>)
    => (new-pattern :: <pattern>, found-end? :: <boolean>);
  let right = pattern.pattern-right;
  if (instance?(right, <name-pattern>)
	& right.pattern-name.token-symbol == #"end")
    values(pattern.pattern-left, #t);
  else
    let (new-right, found-end?) = remove-optional-semi-and-end(right);
    if (found-end?)
      values(make(<semicolon-pattern>,
		  left: pattern.pattern-left,
		  right: new-right,
		  last: ~instance?(new-right, <semicolon-pattern>)),
	     #t);
    else
      values(pattern, #f);
    end if;
  end if;
end method remove-optional-semi-and-end;

define method remove-optional-semi-and-end (pattern :: <comma-pattern>)
    => (new-pattern :: <pattern>, found-end? :: <boolean>);
  let (new-right, found-end?)
    = remove-optional-semi-and-end(pattern.pattern-right);
  if (found-end?)
    values(make(<comma-pattern>, left: pattern.pattern-left, right: new-right,
		last: ~instance?(new-right, <comma-pattern>)),
	   #t);
  else
    values(pattern, #f);
  end if;
end method remove-optional-semi-and-end;

define method remove-optional-semi-and-end (pattern :: <sequential-pattern>)
    => (new-pattern :: <pattern>, found-end? :: <boolean>);
  let (new-right, found-end?)
    = remove-optional-semi-and-end(pattern.pattern-right);
  if (found-end?)
    let left = pattern.pattern-left;
    if (instance?(new-right, <empty-pattern>))
      values(left, #t);
    else
      values(make(<sequential-pattern>, left: left, right: new-right,
		  last: ~instance?(new-right, <sequential-pattern>)),
	     #t);
    end if;
  else
    values(pattern, #f);
  end if;
end method remove-optional-semi-and-end;

define method remove-optional-semi-and-end (pattern :: <simple-pattern>)
    => (new-pattern :: <pattern>, found-end? :: <boolean>);
  values(pattern, #f);
end method remove-optional-semi-and-end;

define method remove-optional-semi-and-end (pattern :: <name-pattern>)
    => (new-pattern :: <pattern>, found-end? :: <boolean>);
  if (pattern.pattern-name.token-symbol == #"end")
    values(make(<empty-pattern>), #t);
  else
    values(pattern, #f);
  end if;
end method remove-optional-semi-and-end;

define method remove-optional-semi-and-end
    (pattern :: <property-list-pattern>)
    => (new-pattern :: <pattern>, found-end? :: <boolean>);
  values(pattern, #f);
end method remove-optional-semi-and-end;



define method make-define-rule (pattern :: <pattern>, rhs :: <template>)
    => rule :: <main-rule>;
  let (new-pattern, found-end?) = remove-optional-semi-and-end(pattern);
  if (found-end?)
    make(<body-style-define-rule>, pattern: new-pattern, template: rhs);
  else
    make(<list-style-define-rule>, pattern: new-pattern, template: rhs);
  end if;
end method make-define-rule;


define method make-statement-or-function-rule
    (name :: <identifier-token>, pattern :: <pattern>, rhs :: <template>)
    => rule :: <main-rule>;
  let (new-pattern, found-end?) = remove-optional-semi-and-end(pattern);
  if (found-end?)
    make(<statement-rule>, name: name, pattern: new-pattern, template: rhs);
  elseif (instance?(new-pattern, <bracketed-pattern>)
	    & new-pattern.pattern-left-token.token-kind == $left-paren-token)
    make(<function-rule>, name: name, pattern: new-pattern.pattern-guts,
	 template: rhs);
  else
    compiler-fatal-error-location(name, "Invalid rule syntax.");
  end if;
end method make-statement-or-function-rule;



// Making parsed fragments.

// make-parsed-fragment -- exported.
//
// Return a parsed fragment for the given constituent.
// 
define generic make-parsed-fragment (thing :: <object>, #key source-location)
    => res :: <token-fragment>;

define method make-parsed-fragment
    (defn :: <definition-parse>,
     #key source-location: srcloc :: <source-location>
	    = make(<unknown-source-location>))
    => res :: <token-fragment>;
  make(<token-fragment>,
       source-location: srcloc,
       token: make(<pre-parsed-token>,
		   source-location: defn.source-location,
		   kind: $parsed-special-definition-token,
		   parse-tree: defn));
end method make-parsed-fragment;

define method make-parsed-fragment
    (defn :: <definition-macro-call-parse>,
     #key source-location: srcloc :: <source-location>
	    = make(<unknown-source-location>))
    => res :: <token-fragment>;
  make(<token-fragment>,
       source-location: srcloc,
       token: make(<pre-parsed-token>,
		   source-location: defn.source-location,
		   kind: $parsed-definition-macro-call-token,
		   parse-tree: defn));
end method make-parsed-fragment;

define method make-parsed-fragment
    (decl :: <local-declaration-parse>,
     #key source-location: srcloc :: <source-location>
	    = make(<unknown-source-location>))
    => res :: <token-fragment>;
  make(<token-fragment>,
       source-location: srcloc,
       token: make(<pre-parsed-token>,
		   source-location: decl.source-location,
		   kind: $parsed-local-declaration-token,
		   parse-tree: decl));
end method make-parsed-fragment;

define method make-parsed-fragment
    (varref :: <varref-parse>,
     #key source-location: srcloc :: <source-location>
	    = make(<unknown-source-location>))
    => res :: <token-fragment>;
  make(<token-fragment>,
       source-location: srcloc,
       token: varref.varref-id);
end method make-parsed-fragment;

define method make-parsed-fragment
    (expr :: <literal-ref-parse>,
     #key source-location: srcloc :: <source-location>
	    = make(<unknown-source-location>))
    => res :: <token-fragment>;
  let lit = expr.litref-literal;
  make(<token-fragment>,
       source-location: srcloc,
       token: if (instance?(lit, <literal-symbol>))
		make(<literal-token>,
		     source-location: expr.source-location,
		     kind: $symbol-token,
		     literal: lit);
	      elseif (instance?(lit, <literal-string>))
		make(<literal-token>,
		     source-location: expr.source-location,
		     kind: $string-token,
		     literal: lit);
	      elseif (instance?(lit, <literal-list>)
			| instance?(lit, <literal-simple-object-vector>))
		make(<pre-parsed-token>,
		     source-location: expr.source-location,
		     kind: $parsed-constant-token,
		     parse-tree: lit);
	      else
		make(<literal-token>,
		     source-location: expr.source-location,
		     kind: $literal-token,
		     literal: lit);
	      end if);
end method make-parsed-fragment;

define method make-parsed-fragment
    (expr :: <expression-parse>,
     #key source-location: srcloc :: <source-location>
	    = make(<unknown-source-location>))
    => res :: <token-fragment>;
  make(<token-fragment>,
       source-location: srcloc,
       token: make(<pre-parsed-token>,
		   source-location: expr.source-location,
		   kind: $parsed-expression-token,
		   parse-tree: expr));
end method make-parsed-fragment;

define method make-parsed-fragment
    (expr :: type-union(<statement-parse>, <function-macro-call-parse>),
     #key source-location: srcloc :: <source-location>
	    = make(<unknown-source-location>))
    => res :: <token-fragment>;
  make(<token-fragment>,
       source-location: srcloc,
       token: make(<pre-parsed-token>,
		   source-location: expr.source-location,
		   kind: $parsed-macro-call-token,
		   parse-tree: expr));
end method make-parsed-fragment;

define method make-parsed-fragment
    (paramlist :: <parameter-list>,
     #key source-location: srcloc :: <source-location>
	    = make(<unknown-source-location>))
    => res :: <token-fragment>;
  make(<token-fragment>,
       source-location: srcloc,
       token: make(<pre-parsed-token>,
		   kind: $parsed-parameter-list-token,
		   parse-tree: paramlist));
end method make-parsed-fragment;
  
define method make-parsed-fragment
    (varlist :: <variable-list>,
     #key source-location: srcloc :: <source-location>
	    = make(<unknown-source-location>))
    => res :: <token-fragment>;
  make(<token-fragment>,
       source-location: srcloc,
       token: make(<pre-parsed-token>,
		   kind: $parsed-variable-list-token,
		   parse-tree: varlist));
end method make-parsed-fragment;
  


// Driver loop.

// The initial size of the parser stacks.
// 
define constant $initial-stack-size = 200;

// grow -- internal.
//
// Grow a stack.  Make a new vector twice as long, copy the old elements
// across, and return it.
// 
define method grow (vec :: <simple-object-vector>)
    => new :: <simple-object-vector>;
  let old-size = vec.size;
  let new-size = old-size * 2;
  let new = make(<simple-object-vector>, size: new-size);
  for (index from 0 below old-size)
    new[index] := vec[index];
  end for;
  new;
end method grow;

// parse -- internal.
//
// The actual parser loop.  Drive the state machine and maintain the stacks
// until we hit an accept action or until be hit a bogus token.
// 
define method parse
    (tokenizer :: <tokenizer>, start-state :: <integer>, debug? :: <boolean>)
    => result :: <object>;
  block (return)
    let state-stack = make(<simple-object-vector>, size: $initial-stack-size);
    let symbol-stack = make(<simple-object-vector>, size: $initial-stack-size);
    let srcloc-stack = make(<simple-object-vector>, size: $initial-stack-size);

    state-stack[0] := start-state;
    let top :: <integer> = 1;
    let (lookahead, lookahead-srcloc) = get-token(tokenizer);

    unless (lookahead.token-kind == $eof-token)
      let actions :: <simple-object-vector> = $action-table[start-state];
      let action :: <integer> = actions[$eof-token];
      unless (action == $error-action)
	note-potential-end-point(tokenizer);
	if (debug?)
	  dformat("potential end point\n");
	end if;
      end unless;
    end unless;

    while (#t)
      let state :: <integer> = state-stack[top - 1];

      if (debug?)
	dformat("top = %d, state = %d, lookahead = %s\n",
		top, state, lookahead);
      end if;

      let actions :: <simple-object-vector> = $action-table[state];
      let action :: <integer> = actions[lookahead.token-kind];
      let (action-datum, action-kind)
	= truncate/(action, ash(1, $action-bits));
      select (action-kind)
	$error-action =>
	  compiler-fatal-error("Parse error at or before %s", lookahead);

	$accept-action =>
	  if (debug?)
	    dformat("  accepting.\n");
	  end if;
	  unget-token(tokenizer, lookahead, lookahead-srcloc);
	  if (top ~== 2)
	    error("stack didn't get reduced all the way?");
	  end if;
	  return(symbol-stack[1]);

	$shift-action =>
	  if (top == state-stack.size)
	    state-stack := grow(state-stack);
	    symbol-stack := grow(symbol-stack);
	    srcloc-stack := grow(srcloc-stack);
	  end if;
	  if (debug?)
	    dformat("  shifting to state %d.\n", action-datum);
	  end if;
	  state-stack[top] := action-datum;
	  symbol-stack[top] := lookahead;
	  srcloc-stack[top] := lookahead-srcloc;
	  top := top + 1;
	  let (new-lookahead, new-srcloc) = get-token(tokenizer);
	  lookahead := new-lookahead;
	  lookahead-srcloc := new-srcloc;

	  unless (lookahead.token-kind == $eof-token)
	    let actions :: <simple-object-vector>
	      = $action-table[action-datum];
	    let action :: <integer> = actions[$eof-token];
	    unless (action == $error-action)
	      note-potential-end-point(tokenizer);
	      if (debug?)
		dformat("potential end point\n");
	      end if;
	    end unless;
	  end unless;

	$reduce-action =>
	  let semantic-action :: <function>
	    = $production-table[action-datum];
	  let number-pops :: <integer>
	    = $number-of-pops[action-datum];
	  if (debug?)
	    dformat("  reducing by production %d, num pops = %d\n",
		    action-datum, number-pops);
	  end if;
	  let old-top = top - number-pops;
	  let extra-args = make(<simple-object-vector>, size: number-pops * 2);
	  for (index from 0 below number-pops)
	    extra-args[index * 2] := symbol-stack[old-top + index];
	    extra-args[index * 2 + 1] := srcloc-stack[old-top + index];
	  end for;
	  let new-srcloc
	    = if (zero?(number-pops))
		let left = srcloc-stack[top - 1];
		if (left)
		  source-location-between(left, lookahead-srcloc);
		else
		  source-location-before(lookahead-srcloc);
		end if;
	      elseif (number-pops == 1)
		srcloc-stack[old-top];
	      else
		source-location-spanning
		  (srcloc-stack[old-top], srcloc-stack[top - 1]);
	      end if;
	  let (new-state :: <integer>, new-symbol)
	    = apply(semantic-action, state-stack[old-top - 1], new-srcloc,
		    extra-args);
	  if (old-top == state-stack.size)
	    state-stack := grow(state-stack);
	    symbol-stack := grow(symbol-stack);
	    srcloc-stack := grow(srcloc-stack);
	  end if;
	  state-stack[old-top] := new-state;
	  symbol-stack[old-top] := new-symbol;
	  srcloc-stack[old-top] := new-srcloc;
	  top := old-top + 1;

      end select;
    end while;
  end block;
end method parse;



// External entry points to the parser.

// parse-source-record -- exported.
// 
define method parse-source-record
    (tokenizer :: <tokenizer>, #key debug: debug? :: <boolean>)
    => ();
  parse(tokenizer, $source-record-start-state, debug?);
end;

// parse-expression -- exported.
// 
define method parse-expression
    (tokenizer :: <tokenizer>, #key debug: debug? :: <boolean>)
    => res :: <expression-parse>;
  parse(tokenizer, $expression-start-state, debug?);
end;

// parse-variable -- exported.
// 
define method parse-variable
    (tokenizer :: <tokenizer>, #key debug: debug? :: <boolean>)
    => res :: <parameter>;
  parse(tokenizer, $variable-start-state, debug?);
end;

// parse-bindings -- exported.
// 
define method parse-bindings
    (tokenizer :: <tokenizer>, #key debug: debug? :: <boolean>)
    => res :: <bindings-parse>;
  parse(tokenizer, $bindings-start-state, debug?);
end;

// parse-body -- exported.
// 
define method parse-body
    (tokenizer :: <tokenizer>, #key debug: debug? :: <boolean>)
    => res :: <expression-parse>;
  parse(tokenizer, $body-opt-start-state, debug?);
end;

// parse-case-body -- exported.
// 
define method parse-case-body
    (tokenizer :: <tokenizer>, #key debug: debug? :: <boolean>)
    => res :: <fragment>;
  parse(tokenizer, $case-body-opt-start-state, debug?);
end;

// parse-property-list -- exported.
// 
define method parse-property-list
    (tokenizer :: <tokenizer>, #key debug: debug? :: <boolean>)
    => res :: <simple-object-vector>;
  parse(tokenizer, $property-list-opt-start-state, debug?);
end;

// parse-parameter-list -- exported.
// 
define method parse-parameter-list
    (tokenizer :: <tokenizer>, #key debug: debug? :: <boolean>)
    => res :: <parameter-list>;
  parse(tokenizer, $parameter-list-opt-start-state, debug?);
end;

// parse-variable-list -- exported.
// 
define method parse-variable-list
    (tokenizer :: <tokenizer>, #key debug: debug? :: <boolean>)
    => res :: <variable-list>;
  parse(tokenizer, $variable-list-opt-start-state, debug?);
end;

// parse-macro-call -- exported.
// 
define method parse-macro-call
    (tokenizer :: <tokenizer>, #key debug: debug? :: <boolean>)
    => res :: <macro-call-parse>;
  parse(tokenizer, $macro-call-start-state, debug?);
end;

