module: threads

// dynamic-bind 
// shadow the given variables within a block we add

define macro dynamic-bind 
    { dynamic-bind (?var:name = ?val:expression)
        ?:body
      end }
     => { begin
            let old-value = ?var;
            block()
              ?var := ?val;
              ?body
            cleanup
              ?var := old-value;
            end
          end }
    { dynamic-bind (?var:name = ?val:expression, ?others:*)
        ?:body
      end }
     => { begin
            let old-value = ?var;
            block()
              ?var := ?val;
              dynamic-bind(?others) ?body end;
            cleanup
              ?var := old-value;
            end
          end }
    { dynamic-bind(?:name(?arg:expression) ?eq:token ?val:expression)
        ?:body
      end }
     => { ?name ## "-dynamic-binder"(?val,
                                     method() ?body end,
                                     ?arg) }
    { dynamic-bind(?:name(?arg:expression) ?eq:token ?val:expression,?others:*)
        ?:body
      end }
     => { ?name ## "-dynamic-binder"(?val,
                                     method()
                                         dynamic-bind(?others)
                                           ?body
                                         end;
                                     end,
                                     ?arg) }
end macro dynamic-bind;

// Dummy multithreading support implementations

define abstract class <synchronization> (<object>) end class;
define open abstract class <lock> (<synchronization>) end class;
define class <notification> (<synchronization>) end class;
define open class <exclusive-lock> (<lock>) end class;
define primary class <semaphore> (<lock>) end class;
define primary class <recursive-lock> (<exclusive-lock>) end class;
define primary class <read-write-lock> (<exclusive-lock>) end class;
define primary class <simple-lock> ( <exclusive-lock> ) end class;

define method make
    (class == <lock>, #rest init-args, #key, #all-keys)
 => (instance :: <simple-lock>)
  apply(make, <simple-lock>, init-args)
end method;

// with-lock
// do-nothing version

define macro with-lock
	{ with-lock( ?lock:expression ) ?lock-body:body end }
	 => { ?lock-body }
	{ with-lock( ?lock:expression ) ?lock-body:body failure ?fail-body:body end }
	 => { ?lock-body }
end macro with-lock;


// <thread>

define class <thread> ( <object> )
  constant slot thread-name :: <string> = "dummy thread";
end class;


// current-thread

define method current-thread()
=> ( result :: <thread> )
    make( <thread> );
end method current-thread;


// atomic-increment!
// increments without worrying about atomicity
// Since we don't need to worry about atomicity, we just increment

define macro atomic-increment!
    { atomic-increment!( ?to:expression ) } //- Danger of multiple evaluation
     => { ?to := ?to + 1 }
end macro atomic-increment!;


// wait-for
// do-nothing implementation

define method wait-for
    (notification :: <notification>, #key timeout :: <integer> = 1000)
 => ()
    values();
end method wait-for;


// release-all
// do-nothing implementation
define method release-all
    (notification :: <notification>)
 => ()
    values();
end method release-all;

// join-thread
define method join-thread
    (thread :: <thread>)
 => ();
  error("attempt to call join-thread()");
end method;