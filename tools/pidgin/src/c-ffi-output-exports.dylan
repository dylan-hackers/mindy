module: dylan-user

define library c-ffi-output
  use common-dylan;
  use string-extensions;
  use ansi-c;

  export c-ffi-output;
end library;

define module c-ffi-output
  use common-dylan;
  use substring-search;
  use c-types;
  use c-declarations;
  use ansi-c;

  export c-output,
    exported-names;
end module;
