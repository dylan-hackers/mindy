module: Dylan-User

define library print-test
  use Dylan;
  use Print;
end library print-test;

define module print-test
  use Dylan;
  use Extensions;
  use Print;
  use Cheap-io, exclude: {print};
end module print-test;
