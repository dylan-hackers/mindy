module: dylan-user

define library common-dylan
  use dylan, export: all;
  use common-extensions,
    export: { finalization,
	      simple-debugging,
	      simple-io,
	      simple-random,
	      simple-profiling,
	      simple-debugging,
	      machine-words,
	      byte-vectors };

  use transcendental,
     import: { transcendental => transcendentals },
     export: all;

  export
    common-dylan;
end library;

define module common-dylan
  use dylan,
    export: all;
  use common-extensions,
    export: all;
end module;
