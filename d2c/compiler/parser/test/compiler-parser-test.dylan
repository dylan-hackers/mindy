module: compiler-parser-test

define test lexer-test ()
  let tokenizer = make(<lexer>, 
                       source: make(<source-buffer>,
                                    buffer:
                                      as(<byte-vector>, "2 + 3")),
                       start-posn: 0,
                       start-line: 0);
  for(token = get-token(tokenizer)
        then get-token(tokenizer),
      until: token.token-kind == $eof-token)
    format-out("Token: %=\n", token);
  end for;
end test lexer-test;

define suite compiler-parser-test-suite ()
  test lexer-test;
end suite compiler-parser-test-suite;

parse-platforms-file("/usr/local/share/dylan/platforms.descr");
*current-target* := get-platform-named("x86-linux-gcc");
run-test-application(compiler-parser-test-suite);
