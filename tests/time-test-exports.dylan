module: Dylan-User

define library time-test
  use Dylan;
  use Time;
  use Streams;
  use Standard-io;
end library time-test;

define module time-test
  use Dylan;
  use Extensions;
  use Time;
  use Time-io;
  use Cheap-io;
  use Streams, import: {force-output};
  use Standard-io;
end module time-test;
