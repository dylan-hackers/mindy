module: Dylan-User

define library matrix-test
  use Dylan;
  use Matrix;
  use Streams;
  use Standard-IO;
end library matrix-test;

define module matrix-test
  use Dylan;
  use Extensions;
  use Matrix;
  use Cheap-io;
  use Streams, import: {force-output};
  use Standard-IO;
end module matrix-test;
