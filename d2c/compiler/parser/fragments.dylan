module: fragments
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/parser/fragments.dylan,v 1.4 1995/12/15 16:16:36 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


//// Program fragments.

define class <fragment> (<object>)
  slot fragment-head :: false-or(<piece>),
    init-value: #f, init-keyword: head:;
  slot fragment-tail :: false-or(<piece>),
    init-value: #f, init-keyword: tail:;
end;

define method print-object (frag :: <fragment>, stream :: <stream>) => ();
  pprint-logical-block
    (stream,
     prefix: "{",
     body: method (stream)
	     write-class-name(frag, stream);
	     write(' ', stream);
	     write-address(frag, stream);
	     pprint-indent(#"block", 2, stream);
	     let tail = frag.fragment-tail;
	     for (piece = frag.fragment-head then piece.piece-next,
		  while: piece & piece.piece-prev ~= tail)
	       write(' ', stream);
	       pprint-newline(#"linear", stream);
	       print(piece.piece-token, stream);
	     end;
	   end,
     suffix: "}");
end;

define class <piece> (<object>)
  slot piece-prev :: false-or(<piece>),
    init-value: #f, init-keyword: prev:;
  slot piece-next :: false-or(<piece>),
    init-value: #f, init-keyword: next:;
  slot piece-token :: <token>,
    required-init-keyword: token:;
end;

define method print-object (piece :: <piece>, stream :: <stream>) => ();
  pprint-fields(piece, stream, token: piece.piece-token);
end;

define class <balanced-piece> (<piece>)
  slot piece-other :: false-or(<piece>),
    init-value: #f, init-keyword: other:;
end;

define method prepend-piece (piece :: <piece>, frag :: <fragment>)
  let cur-head = frag.fragment-head;
  if (cur-head)
    cur-head.piece-prev := piece;
    piece.piece-next := cur-head;
  else
    frag.fragment-tail := piece;
  end;
  frag.fragment-head := piece;
  frag;
end;

define method postpend-piece (frag :: <fragment>, piece :: <piece>)
  let cur-tail = frag.fragment-tail;
  if (cur-tail)
    cur-tail.piece-next := piece;
    piece.piece-prev := cur-tail;
  else
    frag.fragment-head := piece;
  end;
  frag.fragment-tail := piece;
  frag;
end;

define method append-fragments (frag1 :: <fragment>, frag2 :: <fragment>)
  if (~frag1.fragment-head)
    frag2;
  elseif (~frag2.fragment-head)
    frag1;
  else
    let frag1-tail = frag1.fragment-tail;
    let frag2-head = frag2.fragment-head;
    frag1-tail.piece-next := frag2-head;
    frag2-head.piece-prev := frag1-tail;
    frag1.fragment-tail := frag2.fragment-tail;
    frag1;
  end;
end;

define class <fragment-tokenizer> (<tokenizer>)
  slot current-piece :: false-or(<piece>);
  slot tail-piece :: false-or(<piece>);
end;

define method print-object (tokenizer :: <fragment-tokenizer>,
			    stream :: <stream>)
    => ();
  pprint-fields(tokenizer, stream, current: tokenizer.current-piece);
end;

define method initialize
    (tokenizer :: <fragment-tokenizer>, #next next-method, #key fragment)
    => ();
  next-method();
  tokenizer.current-piece := fragment.fragment-head;
  tokenizer.tail-piece := fragment.fragment-tail;
end;

define method get-token (tokenizer :: <fragment-tokenizer>)
    => token :: <token>;
  let cur = tokenizer.current-piece;
  if (cur & cur.piece-prev ~= tokenizer.tail-piece)
    tokenizer.current-piece := cur.piece-next;
    cur.piece-token;
  else
    make(<eof-token>);
  end;
end;

define method unget-token (tokenizer :: <fragment-tokenizer>, token :: <token>)
    => ();
  tokenizer.current-piece := tokenizer.current-piece.piece-prev;
end;

define method unget-token (tokenizer :: <fragment-tokenizer>,
			   token :: <eof-token>)
    => ();
end;


