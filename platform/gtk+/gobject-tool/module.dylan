Module: Dylan-user
Author: Peter S. Housel

define module scheme-read
  use common-dylan;
  use streams;
  export scheme-read, scheme-eof-object?;
end module;

define module defs-file
  use common-dylan;
  use streams;
  use standard-io;
  use format;
  use scheme-read;
  export
    <defs-module>, defs-module-locator, defs-module-imports,
    defs-module-types, defs-module-functions, defs-module-field-accessors,
    defs-module-written, defs-module-options;
  export defs-type, defs-module-includes;
  export <defs-clause>, clause-name, clause-options;
  export <defs-alias>, alias-clause;
  export <defs-enum>, enum-choices;
  export <defs-flags>, flags-choices;
  export <defs-composite>, composite-fields;
  export <defs-boxed>, <defs-struct>, <defs-object>, object-super;
  export <defs-func>, func-return, func-arguments;
  export import-defs;
end module;

define module output-melange
  use common-dylan;
  use streams;
  use format;
  use standard-io;
  use defs-file;
  use ansi-c;
  export output-melange, output-melange-exports;
end module;

define module gobject-tool
  use common-dylan;
  use Extensions, import: { *warning-output* };
  use parse-arguments;
  use streams;
  use standard-io;
  use format;
  use defs-file;
  use ansi-c;
  use c-parser;
  use output-melange;
end module;
