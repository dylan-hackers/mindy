module: dylan-user
RCS-header: $Header: /home/housel/work/rcs/gd/src/d2c/dig/dig-exports.dylan,v 1.5 1996/09/15 15:51:59 nkramer Exp $

define library d2c-gnu
  use dylan;
  use streams;
  use standard-io;
  use format;
#if (~mindy)
  use melange-support;
#endif
  use table-extensions;
  use string-extensions;
  use regular-expressions;
end library d2c-gnu;

define module d2c-gnu
  use dylan;
  use extensions;		// for "subclass"
  use system;
  use streams;
  use format;
  use standard-io;
#if (~mindy)
  use melange-support;
#else
  use file-descriptors;
#endif
  use regular-expressions;
  use string-conversions;
  use substring-search;
  use table-extensions;
  use piped-exec;
end module d2c-gnu;
