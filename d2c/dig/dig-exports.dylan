module: dylan-user

define library d2c-gnu
  use dylan;
  use new-streams;
  use standard-io;
  use format;
#if (~mindy)
  use melange-support;
#endif
  use table-extensions;
  use string-extensions;
end library d2c-gnu;

define module d2c-gnu
  use dylan;
  use extensions;		// for "subclass"
  use system;
  use new-streams;
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
end module d2c-gnu;
