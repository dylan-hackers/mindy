Module: dylan-user

define library ansi-c-test
  use dylan;
  use streams;
  use format;
  use standard-io;
  use command-line-parser;
  use melange-support;

  use parser-utilities;
  use ansi-c;
  use c-parser;

  use c-ffi-output;
end;

define module ansi-c-test
  use dylan;
  use extensions;
  use streams;
  use format;
  use standard-io;
  use command-line-parser;
  use melange-support, import: {call-out};

  use parse-conditions;
  use ansi-c;
  use c-parser;

use c-ffi-output;
end;
