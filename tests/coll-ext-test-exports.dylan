module: Dylan-user

define library coll-ext-test
  use Dylan;
  use Collection-Extensions;
  use Print;
end library coll-ext-test;

define module coll-ext-test
  use Dylan;
  use Extensions;
  use Self-Organizing-list;
  use Subseq;
  use Vector-Search;
  use Cheap-io;
end module coll-ext-test;
