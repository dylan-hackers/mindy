module: dylan-user

define library network-test
  use common-dylan;
  use io;
  use network;
  use melange-support;
end library network-test;

define module network-test
  use common-dylan;
  use format-out;
  use streams;
  use standard-io;
  use network-internal;
  use melange-support;
end module network-test;

