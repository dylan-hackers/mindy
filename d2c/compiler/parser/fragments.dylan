module: fragments
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/parser/fragments.dylan,v 1.6 1996/03/20 19:31:27 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

//
// Program fragments.
//
// The DRM says that:
//
//   The input to, and output from, a macro expansion is a fragment, which
//   is a sequence of elementary fragments.  An elementary fragment is one
//   of the following:
//
//     * A token: the output of the lexical grammar. ...
//     * A bracketed fragment: balanced brackets ( (), [], or {} )
//       enclosing a fragment.
//     * A macro-call fragment: a macro call.
//     * A parsed fragment: a single unit that is not decomposable into
//       its component tokens.  It has been fully parsed by the phrase
//       grammar.  A parsed fragment is either an expression, a definition,
//       or a local declaration.
//
// So the parser needs to be able to produce fragments, the macro expander
// needs to be able to destructure and reconstruct fragments, and then
// the parser needs to be able to grovel the final results.  This file
// implements all that.
//

// <fragment> -- exported.
//
// Abstract superclass of all the different kinds of fragments we can have.
// 
define abstract class <fragment> (<object>)
end class <fragment>;

define sealed domain make (singleton(<fragment>));
define sealed domain initialize (<fragment>);


// <empty-fragment> -- exported.
//
// A fragment that has nothing in it.
// 
define class <empty-fragment> (<fragment>)
end class <empty-fragment>;

define sealed domain make (singleton(<empty-fragment>));

// make{<empty-fragment>}
//
// Empty-fragments are all equivalent, so we only make one to keep memory
// usage low.
// 
define variable *empty-fragment* :: false-or(<empty-fragment>) = #f;
//
define method make (class == <empty-fragment>, #next next-method, #key)
    => res :: <empty-fragment>;
  *empty-fragment* | (*empty-fragment* := next-method());
end method make;

// <compound-fragment> -- exported.
//
// A doubly linked list of elementary-fragments.
//
define class <compound-fragment> (<fragment>)
  //
  // The first fragment in this fragment.
  slot fragment-head :: <elementary-fragment>,
    required-init-keyword: head:;
  //
  // The last (inclusive) fragment this compound fragment.
  // empty.
  slot fragment-tail :: <elementary-fragment>,
    required-init-keyword: tail:;
end class <compound-fragment>;

define sealed domain make (singleton(<compound-fragment>));

define sealed method print-object
    (frag :: <compound-fragment>, stream :: <stream>) => ();
  pprint-fields(frag, stream,
		head: frag.fragment-head,
		tail: frag.fragment-tail);
end method print-object;

// <elementary-fragment> -- exported.
//
// Abstract class for all elementary fragments.  Elementary fragments can
// be linked together to form the guts of a <compound-fragment>.
// 
define abstract class <elementary-fragment> (<fragment>)
  //
  // The previous fragment in the compound fragment this fragment is part of.
  slot fragment-prev :: false-or(<fragment>),
    init-value: #f, init-keyword: prev:;
  //
  // The next fragment in the compound fragment this fragment is part of.
  slot fragment-next :: false-or(<fragment>),
    init-value: #f, init-keyword: next:;
end class <elementary-fragment>;

define sealed domain make (singleton(<elementary-fragment>));

// <token-fragment> -- exported.
//
// A token elementary fragment.  We use token-fragments for what the DRM talks
// about as token fragments, parsed fragments, and macro-call fragments.
// 
define class <token-fragment> (<elementary-fragment>)
  //
  // The token for this fragment.
  slot fragment-token :: <token>,
    required-init-keyword: token:;
end class <token-fragment>;

define sealed domain make (singleton(<token-fragment>));

define sealed method print-object
    (piece :: <token-fragment>, stream :: <stream>) => ();
  pprint-fields(piece, stream, token: piece.fragment-token);
end method print-object;

// <bracketed-fragment> -- exported.
//
// A balanced pair of general brackets and the stuff between them.
// 
define class <bracketed-fragment> (<elementary-fragment>)
  //
  // The token that makes up the left bracket.
  slot fragment-left-token :: <token>,
    required-init-keyword: left-token:;
  //
  // The stuff between the brackets.
  slot fragment-contents :: <fragment>,
    required-init-keyword: contents:;
  //
  // The token that makes up the right backet.
  slot fragment-right-token :: <token>,
    required-init-keyword: right-token:;
end class <bracketed-fragment>;

define sealed domain make (singleton(<bracketed-fragment>));

define sealed method print-object
    (fragment :: <bracketed-fragment>, stream :: <stream>) => ();
  pprint-fields(fragment, stream,
		left-token: fragment.fragment-left-token,
		contents: fragment.fragment-contents,
		right-token: fragment.fragment-right-token);
end method print-object;



// Fragment duplication.

// copy-fragment -- exported.
//
// Clone the fragment.
//
define generic copy-fragment (frag :: <fragment>) => res :: <fragment>;

// copy-fragment{<empty-fragment>}
//
// Empty fragments don't actually need to be copied because there is
// really only one.
// 
define method copy-fragment
    (frag :: <empty-fragment>) => res :: <empty-fragment>;
  frag;
end method copy-fragment;

// copy-fragment{<compound-fragment>}
//
// Copy each component elementary fragment, linking them together.  Then
// make a new compound fragment.
// 
define method copy-fragment
    (frag :: <compound-fragment>) => res :: <compound-fragment>;
  let new-head = copy-fragment(frag.fragment-head);

  let prev = new-head;
  let stop = frag.fragment-tail.fragment-next;
  for (old = frag.fragment-head.fragment-next then old.fragment-next,
       until: old == stop)
    let new = copy-fragment(old);
    prev.fragment-next := new;
    new.fragment-prev := prev;
    prev := new;
  end for;

  make(<compound-fragment>, head: new-head, tail: prev);
end method copy-fragment;

// copy-fragment{<token-fragment>}
//
// Make a new token-fragment with the same token.
//
define method copy-fragment
    (frag :: <token-fragment>) => res :: <token-fragment>;
  make(<token-fragment>, token: frag.fragment-token);
end method copy-fragment;

// copy-fragment{<bracketed-fragment>}
//
// Copy the contents fragment, but just use the same tokens because they
// can't be destructivly modified.
//
define method copy-fragment
    (frag :: <bracketed-fragment>) => res :: <bracketed-fragment>;
  make(<bracketed-fragment>,
       left-token: frag.fragment-left-token,
       contents: copy-fragment(frag.fragment-contents),
       right-token: frag.fragment-right-token);
end method copy-fragment;


// Construction of compound fragments.

// append-fragments! -- exported.
//
// Build a new fragment that appends the two argument fragments and return
// it.  We can destroy either argument fragment in the process.
// 
define generic append-fragments! (frag1 :: <fragment>, frag2 :: <fragment>)
    => res :: <fragment>;

// append-fragments!{<empty-fragment>,<fragment>}
//
// Just return the second fragment.
// 
define method append-fragments!
    (frag1 :: <empty-fragment>, frag2 :: <fragment>)
    => res :: <fragment>;
  frag2;
end method append-fragments!;

// append-fragments!{<fragment>,<empty-fragment>}
//
// Just return the first fragment.
// 
define method append-fragments!
    (frag1 :: <fragment>, frag2 :: <empty-fragment>)
    => res :: <fragment>;
  frag1;
end method append-fragments!;

// append-fragments!{<empty-fragment>,<empty-fragment>}
//
// Return either fragment.  We need this method because it would be ambiguous
// which of the previous two to invoke when given two empty fragments.
// 
define method append-fragments!
    (frag1 :: <empty-fragment>, frag2 :: <empty-fragment>)
    => res :: <fragment>;
  frag1;
end method append-fragments!;

// append-fragments!{<compound-fragment>,<compound-fragment>}
// 
define method append-fragments!
    (frag1 :: <compound-fragment>, frag2 :: <compound-fragment>)
    => res :: <fragment>;
  let left = frag1.fragment-tail;
  let right = frag2.fragment-head;
  left.fragment-next := right;
  right.fragment-prev := left;
  frag1.fragment-tail := frag2.fragment-tail;
  frag1;
end method append-fragments!;

// append-fragments!{<compound-fragment>,<elementary-fragment>}
//
// Tack the elementary fragment onto the end of the compound fragment and
// return the compound fragment.
// 
define method append-fragments!
    (frag1 :: <compound-fragment>, frag2 :: <elementary-fragment>)
    => res :: <fragment>;
  let old-tail = frag1.fragment-tail;
  old-tail.fragment-next := frag2;
  frag2.fragment-prev := old-tail;
  frag1.fragment-tail := frag2;
  frag1;
end method append-fragments!;

// append-fragments!{<elementary-fragment>,<compound-fragment>}
// 
// Tack the elementary fragment onto the start of the compound fragment and
// return the compound fragment.
// 
define method append-fragments!
    (frag1 :: <elementary-fragment>, frag2 :: <compound-fragment>)
    => res :: <fragment>;
  let old-head = frag2.fragment-head;
  frag1.fragment-next := old-head;
  old-head.fragment-prev := frag1;
  frag2.fragment-head := frag1;
  frag2;
end method append-fragments!;

// append-fragments!{<elementary-fragment>,<elementary-fragment>}
//
// Link the two elementary fragments together and return a new compound
// fragment.
// 
define method append-fragments!
    (frag1 :: <elementary-fragment>, frag2 :: <elementary-fragment>)
    => res :: <fragment>;
  frag1.fragment-next := frag2;
  frag2.fragment-prev := frag1;
  make(<compound-fragment>, head: frag1, tail: frag2);
end method append-fragments!;


// Fragment tokenizer.

// <bracketed-fragment-stack> -- internal.
//
// Used to keep track of the bracketed fragments the fragment-tokenizer has
// descended into so that we can pop back out of them.
// 
define class <bracketed-fragment-stack> (<object>)
  //
  // The bracketed fragment that was descended into.
  constant slot stack-fragment :: <bracketed-fragment>,
    required-init-keyword: fragment:;
  //
  // The tokenizer-end that was in effect when we desceded into it.
  constant slot stack-end :: false-or(<elementary-fragment>),
    required-init-keyword: end:;
  //
  // The saved backeted fragments above this one.
  constant slot stack-prev :: false-or(<bracketed-fragment-stack>),
    required-init-keyword: prev:;
end class <bracketed-fragment-stack>;

define sealed domain make (singleton(<bracketed-fragment-stack>));
define sealed domain initialize (<bracketed-fragment-stack>);

// <fragment-tokenzier> -- exported.
//
// Offers a tokenizer interface to fragments.
//
define class <fragment-tokenizer> (<tokenizer>)
  //
  // The token we've looked ahead at but put back with unget-token.
  slot tokenizer-lookahead :: false-or(<token>) = #f;
  //
  // The current fragment we are working.
  slot tokenizer-current :: false-or(<elementary-fragment>) = #f;
  //
  // Where to stop in the .fragment-next chain.
  slot tokenizer-end :: false-or(<elementary-fragment>) = #f;
  //
  // Stack of <bracketed-fragment>s we've decended into.
  slot tokenizer-stack :: false-or(<bracketed-fragment-stack>) = #f;
  //
  // The previous elementary fragment we looked at, if we know what it was.
  slot tokenizer-previous :: false-or(<elementary-fragment>) = #f;
  //
  // Guess at the potential end.
  slot tokenizer-potential-end-point :: false-or(<elementary-fragment>) = #f;
end class <fragment-tokenizer>;

define sealed domain make (singleton(<fragment-tokenizer>));
define sealed domain initialize (<fragment-tokenizer>);

define method initialize
    (tokenizer :: <fragment-tokenizer>, #next next-method,
     #key fragment :: <fragment>)
    => ();
  next-method();
  prime-tokenizer(tokenizer, fragment)
end method initialize;

define sealed method print-object
    (tokenizer :: <fragment-tokenizer>, stream :: <stream>)
    => ();
  pprint-fields(tokenizer, stream, current: tokenizer.tokenizer-current);
end method print-object;

// get-token -- method on imported gf
//
// Extract the next token.  This handles pushed-back lookahead tokens, pops
// the bracketed token stack if necessary, or calls advance-tokenizer to
// handle the real work.
// 
define sealed method get-token (tokenizer :: <fragment-tokenizer>)
    => (token :: <token>, srcloc :: <source-location>);
  let lookahead = tokenizer.tokenizer-lookahead;
  values
    (if (lookahead)
       tokenizer.tokenizer-lookahead := #f;
       lookahead;
     else
       let current = tokenizer.tokenizer-current;
       tokenizer.tokenizer-previous := current;
       if (current == tokenizer.tokenizer-end)
	 //
	 // We've hit the end of the current layer.  If we are inside some
	 // bracketed fragment, restore the state to just after that bracketed
	 // fragment and return the right bracket token.  If we are not inside
	 // a bracketed fragment, then we are at the every end, so return an
	 // EOF token.
	 let stack = tokenizer.tokenizer-stack;
	 if (stack)
	   let bracketed-frag = stack.stack-fragment;
	   tokenizer.tokenizer-current := bracketed-frag.fragment-next;
	   tokenizer.tokenizer-end := stack.stack-end;
	   tokenizer.tokenizer-stack := stack.stack-prev;
	   bracketed-frag.fragment-right-token;
	 else
	   make(<token>, kind: $eof-token);
	 end if;
       else
	 advance-tokenizer(tokenizer, current);
       end if;
     end if,
     make(<unknown-source-location>));
end method get-token;

// unget-token -- method on imported gf.
//
// Push back the lookahead token.  But not if it was an EOF.
// 
define sealed method unget-token
    (tokenizer :: <fragment-tokenizer>, token :: <token>,
     srcloc :: <source-location>)
    => ();
  unless (token.token-kind == $eof-token)
    tokenizer.tokenizer-lookahead := token;
    tokenizer.tokenizer-previous := #f;
  end unless;
end method unget-token;

// note-potential-end-point -- method on imported gf.
//
// Record the current fragment.
// 
define sealed method note-potential-end-point
    (tokenizer :: <fragment-tokenizer>) => ();
  unless (tokenizer.tokenizer-stack)
    let prev = tokenizer.tokenizer-previous;
    if (prev)
      tokenizer.tokenizer-potential-end-point := prev;
    end if;
  end unless;
end method note-potential-end-point;

// prime-tokenizer -- internal
//
// Set up the tokenizer to iterate though the tokens in the supplied fragment.
// 
define generic prime-tokenizer
    (tokenizer :: <fragment-tokenizer>, fragment :: <fragment>) => ();

// prime-tokenizer{<empty-fragment>}
//
// There are no tokens in an empty fragment, so iterating though them is easy.
//
define method prime-tokenizer
    (tokenizer :: <fragment-tokenizer>, fragment :: <empty-fragment>) => ();
  tokenizer.tokenizer-current := #f;
  tokenizer.tokenizer-end := #f;
end method prime-tokenizer;

// prime-tokenizer{<compound-fragment>}
//
// Set the current to the fragment's head and the end to one past the
// fragment's tail.
//
define method prime-tokenizer
    (tokenizer :: <fragment-tokenizer>, fragment :: <compound-fragment>) => ();
  tokenizer.tokenizer-current := fragment.fragment-head;
  tokenizer.tokenizer-end := fragment.fragment-tail.fragment-next;
end method prime-tokenizer;
  
// prime-tokenizer{<elementary-fragment>}
//
// Set the current to this fragment and the end to whatever follows this
// fragment.
//
define method prime-tokenizer
    (tokenizer :: <fragment-tokenizer>, fragment :: <elementary-fragment>)
    => ();
  tokenizer.tokenizer-current := fragment;
  tokenizer.tokenizer-end := fragment.fragment-next;
end method prime-tokenizer;


// advance-tokenizer -- internal.
//
// Advance the tokenizer one token and return that token.  We dispatch off
// of the current fragment because we advance into them differently.
// 
define generic advance-tokenizer
    (tokenizer :: <fragment-tokenizer>, current :: <elementary-fragment>)
    => next :: <token>;

// advance-tokenizer{<token-fragment>}
//
// Pretty easy: just dink current to the next, and return current's token.
//
define method advance-tokenizer
    (tokenizer :: <fragment-tokenizer>, current :: <token-fragment>)
    => next :: <token>;
  tokenizer.tokenizer-current := current.fragment-next;
  current.fragment-token;
end method advance-tokenizer;

// advance-tokenizer{<bracketed-fragment>}
//
// Push this bracketed fragment onto the bracketed fragment stack, prime
// the tokenizer with the bracketed fragment contents, and then return the
// left bracket.
//
define method advance-tokenizer
    (tokenizer :: <fragment-tokenizer>, current :: <bracketed-fragment>)
    => next :: <token>;
  tokenizer.tokenizer-stack
    := make(<bracketed-fragment-stack>,
	    fragment: current,
	    end: tokenizer.tokenizer-end,
	    prev: tokenizer.tokenizer-stack);
  prime-tokenizer(tokenizer, current.fragment-contents);
  current.fragment-left-token;
end method advance-tokenizer;

