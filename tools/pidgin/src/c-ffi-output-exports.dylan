module: dylan-user

define library c-ffi-output
  use common-dylan;
  use string-extensions;
  use ansi-c;

  export c-ffi-output;
end library;

define module c-ffi-output
  use common-dylan;
  use string-conversions;
  use substring-search;
  use c-types;
  use c-declarations;

  export c-output;
end module;
