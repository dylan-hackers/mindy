module: PR-28

define variable has-errors = #f;

define method run-several-tests (test-name :: <string>, 
                                 test :: <function>)
 => ();
  format-out("%s ... ", test-name);
  let temp-has-errors = has-errors;
  has-errors := #f;
  test();
  if (has-errors == #f)
    format-out("ok.\n");
  end if;
  has-errors := temp-has-errors | has-errors;
end method run-several-tests;

define method run-test (input, expected-result, test-name :: <string>)
 => passed? :: <boolean>;
  if (input ~= expected-result)
    has-errors := #t;
    format-out("Failed %s!\n", test-name);
    format-out("     Got %=\n", input);
    format-out("     when we expected %=\n", expected-result);
    #f;
  else
    #t;
  end if;
end method run-test;

define method integer-length-test ()
  run-test(integer-length(0), 0, "integer-length(0)");
  run-test(integer-length(1), 1, "integer-length(1)");
  run-test(integer-length(3), 2, "integer-length(3)");
  run-test(integer-length(4), 3, "integer-length(4)");
  run-test(integer-length(7), 3, "integer-length(7)");
  run-test(integer-length(-1), 0, "integer-length(-1)");
  run-test(integer-length(-4), 2, "integer-length(-4)");
  run-test(integer-length(-7), 3, "integer-length(-7)");
  run-test(integer-length(-8), 3, "integer-length(-8)");
  run-test(integer-length(512), 10, "integer-length(512)");
  run-test(integer-length(511), 9, "integer-length(511)");
  run-test(integer-length(-512), 9, "integer-length(-512)");
  run-test(integer-length(-513), 10, "integer-length(-513)");
end method integer-length-test;

run-several-tests("integer-length", integer-length-test);