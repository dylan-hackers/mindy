module: dylan-user

define library common-dylan
  use dylan, 
    export: { dylan,
              extensions,
              introspection,
              machine-words };
  use common-extensions,
    export: { common-extensions,
              finalization,
	      simple-random,
	      simple-profiling,
	      simple-debugging,
	      simple-io,
	      byte-vector,
              transcendentals,
              functional-extensions };

  use threads, export: { threads };

  use transcendental,
     import: { transcendental => transcendentals },
     export: all;

  export
    common-dylan,
    locators-protocol,
    streams-protocol;
end library;

define module common-dylan
  use dylan,
    export: all;
  use extensions,
    import: { <general-integer> => <abstract-integer> },
    export: all;
  use common-extensions,
    export: all;
end module;
  
define module locators-protocol
  create <locator>;
  create supports-open-locator?,
         open-locator,
         supports-list-locator?,
         list-locator;
end module locators-protocol;

define module streams-protocol
  // Conditions
  create <stream-error>,
           stream-error-stream,
         <end-of-stream-error>,
           <incomplete-read-error>,
             stream-error-sequence,
             stream-error-count,
           <incomplete-write-error>,
             stream-error-count;
  // Opening streams
  create open-file-stream;
  // Reading from streams
  create read-element,
         unread-element,
         peek,
         read,
         read-into!,
         discard-input,
         stream-input-available?,
         stream-contents,
         stream-contents-as;
  // Writing to streams
  create write-element,
         write,
         force-output,
         wait-for-io-completion,
         synchronize-output,
         discard-output;
  // Querying streams
  create stream-open?,
         stream-element-type,
         stream-at-end?,
         stream-size;
  // Positioning streams
  create <positionable-stream>,
         stream-position,
         stream-position-setter,
         adjust-stream-position;
end module streams-protocol;

define module common-dylan-internals
  use common-dylan;
  use locators-protocol;
  use streams-protocol, export: all;
end module common-dylan-internals;
