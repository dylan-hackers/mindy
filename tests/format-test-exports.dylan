module: Dylan-User

define library format-test
  use Dylan;
  use Format;
  use Print;
end library format-test;

define module format-test
  use Dylan;
  use Extensions;
  use Format;
  use Cheap-io, prefix: "cheap-io-";
end module format-test;
