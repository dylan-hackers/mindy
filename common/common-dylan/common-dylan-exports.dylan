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

  // Only import transcendentals if we have them.
#if (~compiled-for-solaris)
  use transcendental,
     import: { transcendental => transcendentals },
     export: all;
#endif

  export
    common-dylan;
end library;

define module common-dylan
  use dylan,
    export: all;
  use common-extensions,
    export: all;
end module;
