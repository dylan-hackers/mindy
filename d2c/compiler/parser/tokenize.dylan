module: tokenize
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/parser/tokenize.dylan,v 1.1 1996/03/21 03:41:22 rgs Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


// Tokenizer interface.

define primary abstract class <tokenizer> (<object>)
end class <tokenizer>;

define generic get-token (tokenizer :: <tokenizer>)
    => (token :: <token>, srcloc :: <source-location>);

define generic unget-token
    (tokenizer :: <tokenizer>, token :: <token>, srcloc :: <source-location>)
    => ();

define generic note-potential-end-point (tokenizer :: <tokenizer>) => ();

define method note-potential-end-point (tokenizer :: <tokenizer>) => ();
end method note-potential-end-point;
