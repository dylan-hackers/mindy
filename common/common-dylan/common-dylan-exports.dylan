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
	      byte-vectors,
              functional-extensions };

  use transcendental,
     import: { transcendental => transcendentals },
     export: all;

  use streams,
     export: all;

  export
    common-dylan,
end library;

define module common-dylan
  use dylan,
    export: all;
  use common-extensions,
    export: all;
  use streams,
    export: all;
end module;
  
