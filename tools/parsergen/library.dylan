module: dylan-user
library: parsergen

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

define library parsergen
  use dylan;
  use streams;
  use print;
  use format;
  use standard-io;
  use string-extensions;
  use regular-expressions;
end library parsergen;

define module lisp-read
  use dylan;
  use streams;
  use print;
  use extensions;
  use character-type;
  use standard-io;
  export
    <token>, <identifier>, <string-literal>, <character-literal>,
    <keyword>, <macro-thingy>, <lparen>, $lparen,
    <rparen>, $rparen, <list-start>, $list-start,
    lex, peek-lex, lisp-read;
end module lisp-read;

define module parsergen
  use dylan;
  use extensions;
  use streams;
  use print;
  use format;
  use standard-io;
  use regular-expressions;
  use %hash-tables;
  use lisp-read, import: { lisp-read };
#if (~mindy)
  use system;
#endif
end module parsergen;
