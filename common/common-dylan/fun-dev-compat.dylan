Module:       duim-utilities
Synopsis:     DUIM utilities
Author:       Rob Myers
Copyright:    Gwydion Dylan Maintainers 2000
License:      GNU Library General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*
    NOTES
*/

// Change define /*thread*/ variable to define variable : d2c has no threads yet

// Change define /*sideways*/ method to define method: this may require that create
// be changed to export in some places. Watch for compile errors.


/*
	Utilities
*/

// redefine destructuring-let as follows:
// NOTE: This needs more work
/*
define macro destructuring-let
  { destructuring-let (?pattern = ?value:expression) ?:body end } //- (?pattern:*)
    => { begin
	   let _destructuring-let-body = method (?pattern) ?body end;
	   apply(_destructuring-let-body, ?value)
	 end }
    pattern:
    { (?var:name, #key ?rest:name) }
     => { ?var, #rest ?rest }
    { (?var:name) }
     => { ?var }
end macro destructuring-let;
*/


// <byte-character>
// simple alias to <character>

define constant <byte-character> = <character>;


// without-bounds-checks
// Do-nothing version
// We can't tell the collections library to ignore bounds checks 
// so we ignore this

define macro without-bounds-checks
    { without-range-checks ?:body end }
     => { ?body }
end;


// element-range-error
// Just throws an error on the sequence. 
// Should declare an <element-range-error> class and instantiate it

define method element-range-error( sequence :: <sequence>, index :: <integer> )
=> ()

	error( sequence );	//- Make meaningful error and throw

end method;


/*
	Geometry
*/


// Comment out sealed domain make(subclass(<bounding-box>)); to avoid corrupted .du file


// assert
// Gwydion defines a traditional assert on <boolean> in misc in extentions, 
// exported from common-dylan.
// We exclude that version and declare our own here.
// FIXME:   3rd clause should have a #rest as its 3rd argument for format arguments
//          and apply this to format-string

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


/*
    duim-dcs
*/


// module.dylan: change create to export for dynamic-color-palettes,... Need to be created elsewhere...
// styles.dylan: unseal sealed slots of <style-descriptor> . "Sealed methods not part of a GF"...

// Change named-color-definer to:
/*
define macro named-color-definer
  { define named-color ?:name ?equal-sign:token ( ?red:expression, ?green:expression, ?blue:expression ) }
    => { $default-named-color-table[?#"name"]
           := make-rgb-color ( ?red / 255.0, ?green / 255.0, ?blue / 255.0 )   }
end macro named-color-definer;

*/


// with-keywords-removed
// do-nothing version
// FIXME:   Implementation required.

define macro with-keywords-removed
    { with-keywords-removed( ?new-rest:name = ?fun-rest:variable, ?remove:expression ) ?:body end }
      => { begin
            let ?new-rest = ?fun-rest;
            ?body 
           end}
end;


// unsupplied?

define method unsupplied?( parameter )
=> ( result :: <boolean> )

    parameter = $unsupplied;

end method unsupplied?;


// supplied?

define method supplied?( parameter )
=> ( result :: <boolean> )

    parameter ~= $unsupplied;

end method supplied?;


// dynamic-bind 
// shadow the given variables within a block we add
// FIXME:   Implement properly. Change = to ?equals:token  ?

/*define macro dynamic-bind 
    { dynamic-bind (?var:expression = ?val:expression)
        ?:body
      end }
     => {   begin
                let old-value = ?var;
                block()
                    ?var := ?val;
	                ?binding
	                ?body
	            cleanup
	                ?var := old-value;
	            end
	        end }
end macro dynamic-bind;*/

// dummy version

define macro dynamic-bind 
    { dynamic-bind (?:*)
        ?:body
      end }
     => {   begin
	            ?body
	        end }
end macro dynamic-bind;


/*
	sheets
*/

// Dummy multithreading support implementations


// <lock>

define class <lock> ( <object> ) end class;


// <simple-lock>

define class <simple-lock> ( <lock> ) end class;


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


// atomic-increment!
// increments without worrying about atomicity
// Since we don't need to worry about atomicity, we just increment

define macro atomic-increment!
    { atomic-increment!( ?to:expression ) } //- Danger of multiple evaluation
     => { ?to := ?to + 1 }
end macro atomic-increment!;


// <notification>

define class <notification> ( <object> ) end class;

// wait-for
// do-nothing implementation

define method wait-for( notification :: <notification>, #key timeout :: <integer> = 1000 )
=> ()

    values();
    
end method wait-for;


// release-all
// do-nothing implementation

define method release-all( notification :: <notification> )
=> ()

    values();
    
end method release-all;


// get-property

define method get-property( properties :: <collection>, property :: <object> )
=> ( result :: <object> )

    local method is?( item ) item = property end;

    let key-index = find-key( properties, is?, failure: #f );
    if( key-index )
        element( properties, key-index + 1, default: #f );
    else
        #f;
    end if;

end method get-property;


// remove-property!

define method remove-property!( properties :: <collection>, property :: <object> )
=>( result :: <collection> )

    local method is?( item ) item = property end;
    let key-index = find-key( properties, is?, failure: #f );
    if( key-index )
    let new-collection = make( <stretchy-vector> );
	    for( i :: <integer> from 0 below properties.size() )
	        unless( ( i = key-index ) | i =  ( i = key-index + 1 ))
	            new-collection := add!( new-collection, properties[ i ] );
	        end unless;
	    end for;
    else
        properties;
    end if;
    
end method remove-property!;


/*
    Gadgets
*/

// These should all be exported from the print or format internals through common-dylan


// integer-to-string

define method integer-to-string( int :: <integer> )
=> ( result :: <string> )
    format-to-string( "%i", int );
end method integer-to-string;


// float-to-string

define method float-to-string( flo :: <float> )
=> ( result :: <string> )
    format-to-string( "%f", flo );
end method float-to-string;


// string-to-integer

define method string-to-integer (string :: <sequence>, #key base = 10)
 => int :: <integer>;
  let number :: <integer> = 0;
  let sign = if (string[0] == '-')  -1  else  1  end if;
  let start-index = if (sign = -1)  1  else  0  end if;
  for (i from start-index below string.size)
    let digit = select( string[i] )
				    '0' => 0;
				    '1' => 1;
				    '2' => 2;
				    '3' => 3;
				    '4' => 4;
				    '5' => 5;
				    '6' => 6;
				    '7' => 7;
				    '8' => 8;
				    '9' => 9;
				    otherwise =>
				      error("Invalid digit %=", string[i]);
                end select;
    if (digit >= base)
      error("\"%s\" isn't in base %d\n", string, base);
    else
      number := number * base  + digit;
    end if;
  end for;
  number := sign * number;
  if (number < $minimum-integer | number > $maximum-integer)
    number;
  else
    as(<integer>, number);
  end if;
end method string-to-integer;


// true?

define method true?( value :: <object> )
=>( result :: <boolean> )

    value == #t;
    
end method true?;
