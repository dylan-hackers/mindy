module: dylan-user
library: parsergen

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
