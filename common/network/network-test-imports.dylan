module: dylan-user

define library network-test
  use common-dylan;
  use io;
  use network;
end library network-test;

define module network-test
  use common-dylan;
  use format-out;
  use streams;
  use standard-io;
  use network-internal;
end module network-test;

