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
  apply(format, *standard-error*, format-string, format-arguments);
  new-line(*standard-error*);
  force-output(*standard-error*);
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
                                    size: desired-size :: <integer> = 0,
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

define method string-to-integer
    (string :: <byte-string>,
     #key base :: <integer> = 10, 
          start :: <integer> = 0, 
          end: _end :: <integer> = size(string),
          default = $unsupplied )
 => (result :: <integer>, next-key :: <integer>);
  // Set initial state
  let negative? :: <boolean> = #f;
  let sign?  :: <boolean> = #f;
  let integer :: <integer> = 0;
  
  block (return)
    for (i :: <integer> from start below _end)
      let char :: <character> = string[i];
      let digit :: false-or(<integer>)
	= select (char)
	    '-' =>
	      if(i = start)
		negative? := #t;
		sign? := #t;
	      elseif (sign? & i = start + 1)
		return(default, i);
	      else
		return(if (negative?) - integer else integer end, i);
	      end if;
	      #f;
	    '+' =>
	      if(i = start)
		sign? := #t;
	      elseif (sign? & i = start + 1)
		return(default, i);
	      else
		return(if (negative?) - integer else integer end, i);
	      end if;
	      #f;
	    '0'      => 0;
	    '1'      => 1;
	    '2'      => 2;
	    '3'      => 3;
	    '4'      => 4;
	    '5'      => 5;
	    '6'      => 6;
	    '7'      => 7;
	    '8'      => 8;
	    '9'      => 9;
	    'A', 'a' => 10;
	    'B', 'b' => 11;
	    'C', 'c' => 12;
	    'D', 'd' => 13;
	    'E', 'e' => 14;
	    'F', 'f' => 15;
	    'G', 'g' => 16;
	    'H', 'h' => 17;
	    'I', 'i' => 18;
	    'J', 'j' => 19;
	    'K', 'k' => 20;
	    'L', 'l' => 21;
	    'M', 'm' => 22;
	    'N', 'n' => 23;
	    'O', 'o' => 24;
	    'P', 'p' => 25;
	    'Q', 'q' => 26;
	    'R', 'r' => 27;
	    'S', 's' => 28;
	    'T', 't' => 29;
	    'U', 'u' => 30;
	    'V', 'v' => 31;
	    'W', 'w' => 32;
	    'X', 'x' => 33;
	    'Y', 'y' => 34;
	    'Z', 'z' => 35;
	    otherwise =>
	      if (i = start)
		return(default, i);
	      elseif (sign? & i = start + 1)
		return(default, i);
	      else
		return(if (negative?) - integer else integer end, i);
	      end if;
	  end select;
      if(digit)
	if(digit < base)
	  integer := integer * base + digit;
	elseif (i = start)
	  return(default, i);
	elseif (sign? & i = start + 1)
	  return(default, i);
	else
	  return(if (negative?) - integer else integer end, i);
	end if;
      end if;
    end for;
    values(if (negative?) - integer else integer end, _end);
  end block;
end method string-to-integer;

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
  { define table ?:name ?eq:token { ?keys-and-values } }
    => { define constant ?name :: <table> ?eq make(<table>);
         fill-table!(?name, list(?keys-and-values)); }
  { define table ?:name :: ?type:expression ?eq:token { ?keys-and-values } }
    => { define constant ?name :: ?type ?eq make(?type);
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
