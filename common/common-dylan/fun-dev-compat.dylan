Module:       functional-extensions
Synopsis:     Functional Developer compatibility functions
Author:       Rob Myers
Copyright:    Gwydion Dylan Maintainers 2000
License:      GNU Library General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// assert
// Gwydion defines a traditional assert on <boolean> in misc in extentions, 
// exported from common-dylan.
// We exclude that version and declare our own here.


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
