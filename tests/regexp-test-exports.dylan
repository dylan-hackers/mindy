module: Dylan-User
rcs-header: $Header: /home/housel/work/rcs/gd/src/tests/regexp-test-exports.dylan,v 1.2 1996/08/10 21:42:02 nkramer Exp $

define library regexp-test
  use dylan;
  use string-extensions;
  use regular-expressions;
end library regexp-test;

define module regexp-test
  use dylan;
  use extensions;            // need main
  use regular-expressions;
  use substring-search;
  use string-hacking;
  use cheap-io;
end module regexp-test;
