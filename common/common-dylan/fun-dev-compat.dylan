Module:       functional-extensions
Synopsis:     Functional Developer compatibility functions
Author:       Rob Myers
Copyright:    Gwydion Dylan Maintainers 2000
License:      GNU Library General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*
    NOTES
*/

// Change define /*thread*/ variable to define variable;
// d2c has no threads yet

// Change define /*sideways*/ method to define method;
// this may require that create be changed to export in some places.
// Watch for compile errors.


//
//	Utilities
//

// without-bounds-checks
// Do-nothing version
// We can't tell the collections library to ignore bounds checks 
// so we ignore this

define macro without-bounds-checks
  { without-bounds-checks ?:body end }
    => {let (element, element-setter) = values(%element, %element-setter);
        ?body}
end;


// element-range-error
// Just throws an error on the sequence. 
// Should declare an <element-range-error> class and instantiate it

define method element-range-error
    (sequence :: <sequence>, index :: <integer>)
 => ()
  error( "range error (element %d of %=)", index, sequence );
end method;


// assert
// Gwydion defines a traditional assert on <boolean> in misc in extentions, 
// exported from common-dylan.
// We exclude that version and declare our own here.
// FIXME:   3rd clause should have a #rest as its 3rd argument for 
//          format arguments and apply this to format-string

define macro assert
    { assert( ?clauses ) }
     => { ?clauses }
clauses:
  { ?ok:expression }
    => { if( ~ ?ok ) error( "An assertion failed" ); end if; }
  { ?ok:expression, ?message:expression }
    => { if( ~ ?ok ) error( ?message ); end if; }
  { ?ok:expression, ?message:expression, ?arg:expression }
    => { if( ~ ?ok ) error( format-to-string( ?message, ?arg ) ); end if; }
  { ?ok:expression, ?message:expression, ?arg:expression, ?arg2:expression }
    => { if( ~ ?ok ) error( format-to-string( ?message, ?arg, ?arg2 ) ); end if; }
  { ?ok:expression, ?message:expression, ?arg:expression, ?arg2:expression, ?arg3:expression }
    => { if( ~ ?ok ) error( format-to-string( ?message, ?arg, ?arg2, ?arg3 ) ); end if; }
  { ?ok:expression, ?message:expression, ?arg:expression, ?arg2:expression, ?arg3:expression, ?arg4:expression }
    => { if( ~ ?ok ) error( format-to-string( ?message, ?arg, ?arg2, ?arg3, ?arg4 ) ); end if; }
end macro assert;


// find-value
// simple alias to differently named equivalent d2c method

define constant find-value = find-element;

// with-keywords-removed

define macro with-keywords-removed
   { with-keywords-removed( ?new-rest:name = ?fun-rest:expression,
                            ?remove:expression )
       ?:body
     end }
     => { begin
            let remove = ?remove;
            let ?new-rest
              = for(plist = as(<list>, ?fun-rest) then tail(tail(plist)),
                    new = #()
                      then if(member?(head(plist), remove))
                             new
                           else
                             pair(head(plist), pair(head(tail(plist)), new));
                           end,
                    until: empty?(plist))
                finally
                  new;
                end for;
            ?body 
          end }
end;


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

/*
	sheets
*/

// Dummy multithreading support implementations

define abstract class <synchronization> (<object>) end class;
define open class <lock> (<synchronization>) end class;
define class <notification> (<synchronization>) end class;
define open class <exclusive-lock> (<lock>) end class;
define primary class <semaphore> (<lock>) end class;
define primary class <recursive-lock> (<exclusive-lock>) end class;
define primary class <read-write-lock> (<exclusive-lock>) end class;
define primary class <simple-lock> ( <exclusive-lock> ) end class;


// with-lock
// do-nothing version

define macro with-lock
	{ with-lock( ?lock:expression ) ?lock-body:body end }
	 => { ?lock-body }
	{ with-lock( ?lock:expression ) ?lock-body:body failure ?fail-body:body end }
	 => { ?lock-body }
end macro with-lock;


// <thread>

define class <thread> ( <object> ) end class;


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


// put-property!

define method put-property!
    (properties :: <stretchy-object-vector>,
     property :: <object>,
     value :: <object>)
 => ()
  add!(properties, property);
  add!(properties, value);
  values();
end method put-property!;

// get-property

define method get-property
    (properties :: <sequence>, key :: <object>, #key default)
 => (result :: <object>);
  block(return)
    for(item in properties,
        key? :: <boolean> = #t then ~key?,
        found? :: <boolean> = #f then key? & item == key)
      if(found?)
        return(item);
      end if;
    end for;
    default;
  end block;
end method get-property;

// remove-property!
define macro remove-property!
  { remove-property!(?place:expression, ?key:expression) }
    => { begin
           let result = #f;
           let key = ?key;
           ?place := for(plist = as(<list>, ?place) then tail(tail(plist)),
                         new = #()
                           then if(head(plist) == key)
                                  result := head(tail(plist));
                                  new;
                                else
                                  pair(head(plist),
                                       pair(head(tail(plist)), new));
                                end,
                         until: empty?(plist))
                     finally
                       new;
                     end for;
           result;
         end }
end macro;

define inline method element-range-error (sequence, index)
  error("Range check error!\n");
end method element-range-error;


/// Profiling macro
//  Do-nothing version
define macro profiling
  { profiling
        (?options:*)
      ?body:body
    results
      ?result-body:body
    end }
 => { ?body;
      ?result-body }
end macro profiling;
