module: dylan-user

define library common-dylan
  use dylan;
  use common-extensions,
    export: {finalization,
	     simple-debugging,
	     simple-io,
	     simple-random,
	     transcendentals,
	     machine-word,
	     byte-vector};

  export
    common-dylan;
end library;

define module common-dylan
  use dylan, export: all;
  use common-extensions, export: all;
end module;
