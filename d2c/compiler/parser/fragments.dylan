module: fragments
copyright: see below

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
// We mix in <source-location-mixin> so we can keep track of where
// this empty fragment came from.
// 
define class <empty-fragment> (<fragment>, <source-location-mixin>)
end class <empty-fragment>;

define sealed domain make (singleton(<empty-fragment>));

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
  pprint-logical-block
    (stream,
     prefix: "{",
     body:
       method (stream :: <stream>)
	 pprint-indent(#"block", 2, stream);
	 write-class-name(frag, stream);
	 let stop = frag.fragment-tail.fragment-next;
	 for (subfrag = frag.fragment-head then subfrag.fragment-next,
	      until: subfrag == stop)
	   write-element(stream, ' ');
	   pprint-newline(#"linear", stream);
	   print(subfrag, stream);
	 end for;
       end method,
     suffix: "}");
end method print-object;

// source-location{<compound-fragment>}
//
// Return the source location spanning from the head to the tail.  We
// could store it, but why bother?
// 
define method source-location (fragment :: <compound-fragment>)
    => res :: <source-location>;
  source-location-spanning(fragment.fragment-head.source-location,
			   fragment.fragment-tail.source-location);
end method source-location;

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
// We mix in <source-location-mixin> instead of just using the token's source
// location, because the token's source location will have been minimized,
// and we want the source location in all its glory.
// 
define class <token-fragment> (<elementary-fragment>, <source-location-mixin>)
  //
  // The token for this fragment.
  constant slot fragment-token :: <token>,
    required-init-keyword: token:;
end class <token-fragment>;

define sealed domain make (singleton(<token-fragment>));

define sealed method print-object
    (frag :: <token-fragment>, stream :: <stream>) => ();
  pprint-logical-block
    (stream,
     prefix: "{",
     body:
       method (stream :: <stream>)
	 write-class-name(frag, stream);
	 write-element(stream, ' ');
	 pprint-indent(#"block", 2, stream);
	 pprint-newline(#"fill", stream);
	 print-message(frag.fragment-token, stream);
       end method,
     suffix: "}");
end method print-object;

// <bracketed-fragment> -- exported.
//
// A balanced pair of general brackets and the stuff between them.
// 
define class <bracketed-fragment> (<elementary-fragment>)
  //
  // The token that makes up the left bracket.
  constant slot fragment-left-token :: <token>,
    required-init-keyword: left-token:;
  //
  // The source location for the left token.  We don't use the token's
  // source location, because that will have been simplified, and we want
  // the source location in all its glory.
  constant slot fragment-left-srcloc :: <source-location>
      = make(<unknown-source-location>),
    init-keyword: left-srcloc:;
  //
  // The stuff between the brackets.
  constant slot fragment-contents :: <fragment>,
    required-init-keyword: contents:;
  //
  // The token that makes up the right backet.
  constant slot fragment-right-token :: <token>,
    required-init-keyword: right-token:;
  //
  // The source location for the right token.  We don't use the token's
  // source location, because that will have been simplified, and we want
  // the source location in all its glory.
  constant slot fragment-right-srcloc :: <source-location>
      = make(<unknown-source-location>),
    init-keyword: right-srcloc:;
end class <bracketed-fragment>;

define sealed domain make (singleton(<bracketed-fragment>));

define sealed method print-object
    (fragment :: <bracketed-fragment>, stream :: <stream>) => ();
  pprint-logical-block
    (stream,
     prefix: "{",
     body:
       method (stream :: <stream>)
	 pprint-indent(#"block", 2, stream);
	 write-class-name(fragment, stream);
	 write-element(stream, ' ');
	 pprint-newline(#"fill", stream);
	 print(fragment.fragment-left-token, stream);
	 write-element(stream, ' ');
	 pprint-newline(#"fill", stream);
	 print(fragment.fragment-contents, stream);
	 write-element(stream, ' ');
	 pprint-newline(#"fill", stream);
	 print(fragment.fragment-right-token, stream);
       end method,
     suffix: "}");
end method print-object;

// source-location{<bracketed-fragment>}
//
// Return the source location spanning from the left token to the right
// token.  We could store it, but why bother?
// 
define method source-location (fragment :: <bracketed-fragment>)
    => res :: <source-location>;
  source-location-spanning(fragment.fragment-left-srcloc,
			   fragment.fragment-right-srcloc);
end method source-location;


// Fragment duplication.

// copy-fragment -- exported.
//
// Clone the fragment.
//
define generic copy-fragment
    (frag :: <fragment>, #key preserve-source-locations)
    => res :: <fragment>;

// copy-fragment{<empty-fragment>}
//
// Empty fragments don't actually need to be copied because they are
// immutable.
//
define method copy-fragment
    (frag :: <empty-fragment>, #key preserve-source-locations :: <boolean>)
    => res :: <empty-fragment>;
  frag;
end method copy-fragment;

// copy-fragment{<compound-fragment>}
//
// Copy each component elementary fragment, linking them together.  Then
// make a new compound fragment.
// 
define method copy-fragment
    (frag :: <compound-fragment>, #key preserve-source-locations :: <boolean>)
    => res :: <compound-fragment>;
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
    (frag :: <token-fragment>, #key preserve-source-locations :: <boolean>)
    => res :: <token-fragment>;
  make(<token-fragment>,
       source-location:
         frag.source-location,
       token: frag.fragment-token);
end method copy-fragment;

// copy-fragment{<bracketed-fragment>}
//
// Copy the contents fragment, but just use the same tokens because they
// can't be destructivly modified.
//
define method copy-fragment
    (frag :: <bracketed-fragment>, #key preserve-source-locations :: <boolean>)
    => res :: <bracketed-fragment>;
  make(<bracketed-fragment>,
       left-token: frag.fragment-left-token,
       left-srcloc:
         frag.fragment-left-srcloc,
       contents: copy-fragment(frag.fragment-contents),
       right-token: frag.fragment-right-token,
       right-srcloc:
         frag.fragment-right-srcloc);
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
// Return a new empty fragment that has a source location spanning the
// two source fragments.
// 
define method append-fragments!
    (frag1 :: <empty-fragment>, frag2 :: <empty-fragment>)
    => res :: <fragment>;
  make(<empty-fragment>,
       source-location:
	 source-location-spanning(frag1.source-location,
				  frag2.source-location));
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
  // The fragment we are tokenizing.
  constant slot tokenizer-fragment :: <fragment>,
    required-init-keyword: fragment:;
  //
  // The token we've looked ahead at but put back with unget-token.
  slot tokenizer-lookahead :: false-or(<token>) = #f;
  //
  // The source location for the ungot token.
  slot tokenizer-lookahead-srcloc :: false-or(<source-location>) = #f;
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
  if (lookahead)
    let srcloc = tokenizer.tokenizer-lookahead-srcloc;
    tokenizer.tokenizer-lookahead := #f;
    tokenizer.tokenizer-lookahead-srcloc := #f;
    values(lookahead, srcloc);
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
	values(bracketed-frag.fragment-right-token,
	       bracketed-frag.fragment-right-srcloc);
      else
	let frag = tokenizer.tokenizer-fragment;
	let srcloc = source-location-after(frag.source-location);
	values(make(<token>, source-location: srcloc, kind: $eof-token),
	       srcloc);
      end if;
    else
      advance-tokenizer(tokenizer, current);
    end if;
  end if;
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
    tokenizer.tokenizer-lookahead-srcloc := srcloc;
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
    => (next :: <token>, srcloc :: <source-location>);

// advance-tokenizer{<token-fragment>}
//
// Pretty easy: just dink current to the next, and return current's token.
//
define method advance-tokenizer
    (tokenizer :: <fragment-tokenizer>, current :: <token-fragment>)
    => (next :: <token>, srcloc :: <source-location>);
  tokenizer.tokenizer-current := current.fragment-next;
  values(current.fragment-token, current.source-location);
end method advance-tokenizer;

// advance-tokenizer{<bracketed-fragment>}
//
// Push this bracketed fragment onto the bracketed fragment stack, prime
// the tokenizer with the bracketed fragment contents, and then return the
// left bracket.
//
define method advance-tokenizer
    (tokenizer :: <fragment-tokenizer>, current :: <bracketed-fragment>)
    => (next :: <token>, srcloc :: <source-location>);
  tokenizer.tokenizer-stack
    := make(<bracketed-fragment-stack>,
	    fragment: current,
	    end: tokenizer.tokenizer-end,
	    prev: tokenizer.tokenizer-stack);
  prime-tokenizer(tokenizer, current.fragment-contents);
  values(current.fragment-left-token, current.fragment-left-srcloc);
end method advance-tokenizer;

