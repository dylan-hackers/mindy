module: Dylan-User

define library string-ext-test
  use dylan;
  use string-extensions;
end library string-ext-test;

define module string-ext-test
  use dylan;
  use extensions;            // need main
  use regular-expressions;
  use substring-search;
  use string-hacking;
  use cheap-io;
end module string-ext-test;
