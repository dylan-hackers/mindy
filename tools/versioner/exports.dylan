module: dylan-user
library: versioner
rcs-header: $header$

define library versioner
  use dylan;
  use streams;
  use standard-io;
  use print;
  use format;
  use string-extensions;
  use regular-expressions;
  use table-extensions;
  use stream-extensions;
  use time;
end library versioner;

define module versioner
  use dylan;
  use extensions;
  use streams;
  use standard-io;
  use print;
  use pprint;
  use format;
  use regular-expressions;
  use substring-search;
  use string-conversions;
  use table-extensions;
  use piped-exec;
  use concatenated-streams;
  use time;
  use time-io;
  use Extensions, import: {exit};
#if (~mindy)
  use System,
     import: {pointer-deref, c-expr, <raw-pointer>, import-string};
#endif
end module versioner;

