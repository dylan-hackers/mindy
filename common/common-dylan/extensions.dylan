module: common-extensions


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

define constant $digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

define method integer-to-string
    (integer :: <integer>,
     #key base :: type-union(limited(<integer>, min: 2, max: 36)) = 10,
          size: desired-size :: false-or(<integer>),
          fill :: <character> = '0')
 => (string :: <byte-string>);
  local
    method collect
        (value :: <integer>, digits :: <list>, count :: <integer>)
     => (digits :: <list>, count :: <integer>);
      let (quotient, remainder) = floor/(value, base);
      let digits = pair($digits[as(<integer>, remainder)], digits);
      if(zero?(quotient))
        values(digits, count + 1);
      else
        collect(quotient, digits, count + 1);
      end if;
    end method;
  
  let (digits, count) =
    if (integer < 0)
      // strip off last digit to avoid overflow in $minimum-integer case
      let (quotient :: <integer>, remainder :: <integer>)
        = truncate/(integer, base);
      if (zero?(quotient))
        values(list($digits[- remainder]), 1);
      else
        collect(- quotient, list($digits[- remainder]), 1);
      end if;
    else
      collect(integer, #(), 0);
    end if;

  let min-size = if(integer < 0) count + 1 else count end;
  let string-size
    = if(desired-size) max(desired-size, min-size) else min-size end;
  let returned-string
    = make(<byte-string>, size: string-size, fill: fill);
  
  if(integer < 0)
    returned-string[0] := '-';
  end if;

  for(digit in digits, index from string-size - count)
    returned-string[index] := digit;
  end for;
  returned-string;
end method integer-to-string;

define method string-to-integer
    (string :: <byte-string>,
     #key base :: <integer> = 10, 
          start :: <integer> = 0, 
          end: _end :: <integer> = size(string),
          default = $unsupplied )
 => (result :: <integer>, next-key :: <integer>);
  // Set initial state
  let valid? :: <boolean> = #f;
  let negative? :: <boolean> = #f;
  let integer :: <integer> = 0;
  
  block (return)
    for (i :: <integer> from start below _end)
      let char :: <character> = string[i];
      let digit :: false-or(<integer>)
	= select (char)
	    '-' =>
	      if (i = start)
		negative? := #t;
	      elseif (valid?)
		return(if (negative?) - integer else integer end, i);
              elseif (supplied?(default))
		return(default, i);
              else
                error("not a valid integer");
	      end if;
	      #f;
	    '+' =>
	      if (i = start)
                negative? := #f;
	      elseif (valid?)
		return(if (negative?) - integer else integer end, i);
              elseif (supplied?(default))
		return(default, i);
              else
                error("not a valid integer");
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
              if (valid?)
                return(if (negative?) - integer else integer end, i);
              elseif (supplied?(default))
                return(default, i);
              else
                error("not a valid integer");
              end if;
	  end select;
      if (digit)
	if(digit < base)
	  integer := integer * base + digit;
          valid? := #t;
	elseif (valid?)
	  return(if (negative?) - integer else integer end, i);
        elseif(supplied?(default))
          return(default, i);
        else
          error("not a valid integer");
	end if;
      end if;
    end for;

    if (valid?)
      values(if (negative?) - integer else integer end, _end);
    elseif(supplied?(default))
      return(default, _end);
    else
      error("not a valid integer");
    end if;
  end block;
end method string-to-integer;

