module: dylan-user

define library system
  use common-dylan;
  use melange-support;

  export
    operating-system,
    file-system,
    date,
    settings;
end library;

define module operating-system
  use common-dylan,
    export: {application-name,
	     application-arguments,
	     exit-application,
	     register-exit-application-function};
  use melange-support;
  
  export
    $architecture-little-endian?,
    $os-name,
    $os-variant,
    $os-version,
    $machine-name,
    $platform-name;

  export
    environment-variable,
    environment-variable-setter,
    tokenize-environment-variable;

  export
    login-name,
    login-group,
    owner-name,
    owner-organization;

  export
    run-application,
    application-filename,

  export
    tokenize-command-string;
end module;
