module: dylan-user

define library compiler-parser-test
  use common-dylan;
  use io;
  use testworks;

  use compiler-base;
  use compiler-parser;
end library compiler-parser-test;

define module compiler-parser-test
  use common-dylan;
  use byte-vector;
  use format-out;
  use testworks;

  use source;
  use tokens;
  use platform;

  use tokenize;
  use source-utilities;
  use lexer;
  use parse-tree;
  use fragments;
  use parser;
  use macros;
end module compiler-parser-test;
