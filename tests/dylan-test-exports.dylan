module: Dylan-user

define library dylan-test
  use dylan;
//  use streams;
//  use standard-io;
end library dylan-test;

define module dylan-test
  use Dylan;
  use Extensions;
  use Cheap-IO;
//  use streams, import: {force-output};
//  use standard-io;
end module dylan-test;
