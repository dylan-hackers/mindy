module: dylan-user

define library pidgin
  use common-dylan;
  use collection-extensions;
  use io;
  use dylan;
  use streams;
  use format;
  use standard-io;
  use string-extensions;
  use parse-arguments;
  use parser-utilities;
  use ansi-c;
  use c-parser;
  use c-ffi-output;
end library;

define module pidgin
  use common-dylan, exclude: { format-to-string };
  use format-out;
  use dylan;
  use streams;
  use piped-exec;
  use format;
  use standard-io;
  use parse-arguments;
  use substring-search;
  use subseq;
  use parse-conditions;
  use ansi-c;
  use c-parser;
use c-ffi-output;

end module;
