module: common-extensions


//=========================================================================
//  Streams protocol (will move elsewhere)
//=========================================================================

define open abstract class <stream> (<object>)
end class;


//=========================================================================
//  Unsupplied, unfound.
//=========================================================================
//  Unique objects which can be used as default values for keywords and
//  passed to 'default:'. These cannot be confused with any other Dylan
//  values.

define method unsupplied?( object :: <object> )
=> ( unsupplied? :: <boolean> )
    object = $unsupplied;
end method unsupplied?;

define method supplied?( object :: <object> )
=> ( unsupplied? :: <boolean> )
    ~ unsupplied?( object );
end method supplied?;

define method unsupplied()
=> ( unsupplied-marker :: <object> )
    $unsupplied;
end method unsupplied;

define class <not-found-marker> (<object>)
end;

define constant $unfound = make(<not-found-marker>);

define function found?( object :: <object> )
=> ( found? :: <boolean> )
    ~ unfound?( object );
end function found?;

define function unfound?( object :: <object> )
=> ( unfound? :: <boolean> )
    object = $unfound;
end function unfound?;

define function unfound()
=> ( unfound-marker :: <object> )
    $unfound;
end function unfound;


//=========================================================================
//  Locators
//=========================================================================
//  A very abstract interface to locators.

define open abstract class <locator> (<object>)
end class;

define open generic supports-open-locator?
    (locator :: <locator>)
 => (supported? :: <boolean>);
  
define method supports-open-locator?
    (locator :: <locator>)
 => (supported? :: <boolean>)
  #f;                                                                         
end method;

define open generic open-locator
    (locator :: <locator>)
 => (stream :: <stream>);

define open generic supports-list-locator?
    (locator :: <locator>)
 => (supported? :: <boolean>);

define method supports-list-locator?
    (locator :: <locator>)
 => (supported? :: <boolean>)
  #f;                                                                         
end method;

define open generic list-locator
    (locator :: <locator>)
 => (listing :: <sequence>);


//=========================================================================
//  Condition string conversion
//=========================================================================
//  condition-to-string

define open generic condition-to-string
    (condition :: <condition>)
 => (string :: <string>);

// XXX - need method on <format-string-condition>
// XXX - need method on any conditions defined by Dylan library


//=========================================================================
//  Application environment functions.
//=========================================================================
//  Finding yourself and your arguments; exiting.

define function get-argc() => (argc :: <integer>)
  let argc = application-argc();
  if (argc < 1)
    error("Runtime is corrupted: application_argc = %d", argc());
  end;
  argc;
end;

define function application-name () => (string :: <byte-string>)
  get-argc();
  application-argv(0);
end;

define function application-filename () => (filename :: false-or(<string>))
  // XXX - I don't know how to find the application's filename under Unix.
  #f;
end;

define function application-arguments () => (string :: <simple-object-vector>)
  // XXX - Should this be our official return type? Harlequin says so.
  let argc = get-argc();
  let result = make(<simple-object-vector>, size: argc - 1);
  for (i from 1 below argc)
    result[i - 1] := application-argv(i);
  end for;
  result;
end;

define function exit-application (exit-code :: <integer>) => ()
  exit(exit-code: exit-code);
end;


//=========================================================================
//  Debugging & assertions
//=========================================================================
//  Some of this may move to simple-debugging.

define function debug-message
    (format-string, #rest format-arguments)
 => ()
  format( *standard-error*, format-string, format-arguments );
end function;

#if(mindy)

  define method assert(value :: <boolean>, #rest noise)
  =>(result :: <boolean>)
      if(value)
          error("An assertion failed.");
      end if;
    
      #f;
  end method assert;

  define constant debug-assert = assert;

#else

  define macro assert 
  { assert(?value:expression, ?format-string:expression, ?format-args:*) }
  =>{	block()
              unless (?value)
                  let error-string :: <string> = format-to-string(?format-string, ?format-args);
                  error(error-string);
              end unless;
          #f;
          end block; } 
  { assert(?value:expression, ?message:expression) }
  =>{	block()
              unless (?value)
                  error(?message);
              end unless;
          #f;
          end block; }
  { assert(?value:expression) }
  =>{	block()
              unless (?value)
                  error("An assertion failed.");
              end unless;
              #f;
          end block; }
  end macro assert;

  define macro debug-assert
      { debug-assert(?value:expression, ?format-string:expression, ?format-args:*) }
  =>{ assert(?value, ?format-string, ?format-args) }
      { debug-assert(?value:expression, ?message:expression) }
  =>{ assert(?value, ?message) }
      { debug-assert(?value:expression) }
  =>{ assert(?value) }
  end macro debug-assert;

#endif

//=========================================================================
//  Ignore & ignorable
//=========================================================================
//  Control compiler warnings about unused variables.

define function ignorable (#rest noise) => ()
  // XXX - This has the right API, but does nothing. Also fix ignore.
  // XXX - Does it? Should it be #rest?
end;


//=========================================================================
//  Conversions
//=========================================================================
//  Convert numbers to and from strings.
//  XXX - Much hackiness lurks here:
//		No scientific notation
//		No optimization
//		No error handling to speak of
//		String-to-number functions are very tolerant of input

define method float-to-string( float :: <float> ) 
=> ( string :: <string> )
    
    let string = format-to-string( "%=", float );
    
    // Take off the marker
    string := copy-sequence( string, start: 0, end: string.size - 2 );
    
    string;
end method float-to-string;

define method integer-to-string(    integer :: <integer>,
                                    #key base :: type-union(	limited(<integer>, min: 2, max: 36), 
                                                                object-class( $unsupplied ) ) = $unsupplied,
                                    desired-size :: <integer> = 0,
                                    fill :: <character> = '0')
 => ( string :: <byte-string> )
  if( supplied?( base ) )
    // Convert to base-10
    let( body :: <integer>, digits :: <integer> ) = floor/( integer, base );
    integer := (body * 10) + digits;
  end if;
  
  // Format
  let formatted-string = format-to-string("%d", integer);
  
  // Pad if needed
  if( formatted-string.size < desired-size )
    let fill-string :: <string> = make( <string>, size: desired-size - formatted-string.size, fill: fill );
        formatted-string := concatenate!( fill-string, formatted-string );
  end if;
  
  formatted-string;
end method integer-to-string;

// XXX - number-to-string isn't in common-dylan? ASK FUN-O: about this.

define method string-to-integer( string :: <byte-string>, #key base :: <integer> = 10, 
                                    start-position :: <integer> = 0, 
                                    end-position :: type-union( <integer>, object-class( $unsupplied ) ) = $unsupplied,
                                    default :: type-union( <integer>, object-class( $unsupplied ) ) = $unsupplied )
=> ( result :: <integer> )
    // Get the string we're actually parsing
    if( ~ supplied?( end-position ) )
        end-position := string.size();
    end if;
    let substring :: <string> = copy-sequence( string, start: start-position, end: end-position );
    
    // Set the state and parse
    let negative :: <boolean> = #f;
    let this-base :: <integer> = 1;
    let integer :: <integer> = 0;
    
    if( substring.size = 0 )
        base := string-to-number-error( default );
    end if;
    
    block( exit-loop )
        for( i :: <integer> from substring.size - 1 to 0 by - 1   )
            let char :: <character> = substring[ i ];
            select( char )
                '-' => negative := #t;		// Check we haven't set already?
                '0' => #f;
                '1' => integer := integer + (1 * this-base);
                '2' => integer := integer + (2 * this-base);
                '3' => integer := integer + (3 * this-base);
                '4' => integer := integer + (4 * this-base);
                '5' => integer := integer + (5 * this-base);
                '6' => integer := integer + (6 * this-base);
                '7' => integer := integer + (7 * this-base);
                '8' => integer := integer + (8 * this-base);
                '9' => integer := integer + (9 * this-base);
                ',' => #f;
                otherwise => integer := string-to-number-error( default );
            end select; 
            this-base := this-base * base;
        end for;
        // If the string is negative, make the integer negative as well
        if( negative )
            integer := - integer;
        end if;
    end block;
    
    integer;
end method string-to-integer;

define method string-to-number-error( default :: type-union( <number>, object-class( $unsupplied ) ) )
=> ( result :: <number> )
    let result :: <number> = 0;	// This will never be returned
    
    if( supplied?( default ) )
        result := default;
    else
        break("Invalid string for conversion to number.");
    end if;
    
    result;
end method string-to-number-error;

// XXX - ASK FUN-O: Not part of common-dylan, but used in DUIM?
// Rewrite along the same lines as string-to-integer: find the decimal then work left & right

define method string-to-float( string :: <string>, #key base :: <integer> = 10, start-position :: <integer> = 0, 
                                end-position :: type-union( <integer>, object-class( $unsupplied ) ) = $unsupplied,
                                default :: type-union( <float>, object-class( $unsupplied ) ) = $unsupplied   )
=>( result :: <float> )
    // Get the string we're actually parsing
    if( ~ supplied?( end-position ) )
        end-position := string.size;
    end if;
    let substring :: <string> = copy-sequence( string, start: start-position, end: end-position );
    
    // Set up the parsing state
    let base-float :: <float> = as( <float>, base );
    let float :: <float> = 0.0;
    let left :: <stretchy-vector>  = make( <stretchy-vector> );
    let right :: <stretchy-vector>  = make( <stretchy-vector> );
    let current :: <stretchy-vector> = left;
    let negative :: <boolean> = #f;
    
    if( substring.size = 0 )
        float := string-to-number-error( default );
    end if;
    
    block( exit-loop )
        for( char in substring  )
            select( char )
                '-' => negative := #t;		//TODO Check we haven't set already
                '0' => add!( current, 0.0 );
                '1' => add!( current, 1.0 );
                '2' => add!( current, 2.0 );
                '3' => add!( current, 3.0 );
                '4' => add!( current, 4.0 );
                '5' => add!( current, 5.0 );
                '6' => add!( current, 6.0 );
                '7' => add!( current, 7.0 );
                '8' => add!( current, 8.0 );
                '9' => add!( current, 9.0 );
                '.' => current := right;		//TODO Use decimal-separator
                ',' => #f;				//TODO Use thousands-separator
                otherwise => float := string-to-number-error( default );
            end select; 
        end for;
    
        if( left.size > 0 )
            //OPTIMIZE to go from right to left, multiplying
            let multiple = as( <float>, (base ^ (left.size - 1)) );
            for( digit in left )
                float := float + (digit * multiple);
                multiple := multiple / base-float;
            end for;
        end if;
        
        if( right.size > 0 )
            let fraction = 1.0 / base-float;
            for( digit in right )
                float := float + (digit * fraction);
                fraction := fraction / base-float;
            end for
        end if;
        
        if( negative )
            float := float * -1.0;
        end if;
    end block;
    
    if( ~ float )
        break("Invalid string in string-to-float.");
    end if;
    
    float;
end method string-to-float;


//=========================================================================
//  Macros
//=========================================================================
//  Miscellaneous macros exported from common-extensions. These are not
//  available under Mindy.
//
//  XXX - table-definer conses excessively. With more macrology, it could
//  run much faster.
//  XXX - can the name bound by 'iterate' return?

#if (~mindy)

define macro table-definer
  { define table ?:name ?equals:token {?keys-and-values:*} }
    => { define constant ?name :: <table> = make(<table>);
         fill-table!(?name, list(?keys-and-values)); }
  { define table ?:name :: ?type:* ?equals:token { } }
    => { define constant ?name :: ?type = make(?type);
         fill-table!(?name, list(?keys-and-values)); }
keys-and-values:
  { ?key:expression => ?value:expression, ... } => { ?key, ?value, ... }
  { } => { }
end macro;

define macro iterate
  { iterate ?:name (?clauses:*) ?:body end }
    => { %iterate-aux ?name
	   %iterate-param-helper(?clauses)
           %iterate-value-helper(?clauses)
	   ?body
         end }
end;

define macro %iterate-aux
  { %iterate-aux ?:name
      ?param-clauses:macro
      ?value-clauses:macro
      ?:body
    end }
    => { local method ?name (?param-clauses)
                 ?body
	       end;
         ?name(?value-clauses) }
end macro;

define macro %iterate-param-helper
  { %iterate-param-helper(?clauses) }
    => { ?clauses }
clauses:
  { ?:name :: ?type:*, ... }
    => { ?name :: ?type, ... }
  { ?:name :: ?type:* = ?value:*, ... }
    => { ?name :: ?type, ... }
  { } => { }
end;

define macro %iterate-value-helper
  { %iterate-value-helper(?clauses) }
    => { ?clauses }
clauses:
  { ?:name :: ?type:*, ... }
    => { #f, ... }
  { ?:name :: ?type:* = ?value:*, ... }
    => { ?value, ... }
  { } => { }
end;

define macro when
  { when (?:expression) ?:body end }
    => { if (?expression) ?body end }
end macro;

#endif
