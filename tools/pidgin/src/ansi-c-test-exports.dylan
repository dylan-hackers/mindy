Module: dylan-user

define library ansi-c-test
  use dylan;
  use streams;
  use format;
  use standard-io;

  use ansi-c;
end;

define module ansi-c-test
  use dylan;
  use extensions;
  use streams;
  use format;
  use standard-io;

  use c-types;
  use c-declarations;
end;
