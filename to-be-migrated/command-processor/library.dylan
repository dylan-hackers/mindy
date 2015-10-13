module: dylan-user

define library command-processor
  use common-dylan;
  use io;
  use melange-support;
  use collection-extensions;
  use string-extensions;

  export command-processor;
end library;

define module command-processor
  use common-dylan;
  use format-out;
  use streams;
  use standard-io;
  use melange-support;
  use subseq;
  use string-hacking;

  export <command>, run-command-processor;
end module;
