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

define open generic number-to-string
    (number :: <number>) => (string :: <string>);

define method number-to-string
    (float :: <float>) => (string :: <string>)
  float-to-string(float)
end method number-to-string;

define method number-to-string
    (integer :: <integer>) => (string :: <string>)
  integer-to-string(integer, base: 10)
end method number-to-string;

define method string-to-integer
    (string :: <byte-string>,
     #key base :: <integer> = 10, 
          start :: <integer> = 0, 
          end: _end :: <integer> = size(string),
          default = $unsupplied)
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

define method string-to-float
    (string :: <byte-string>,
     #key _start :: <integer> = 0, 
          end: _end :: <integer> = size(string),
          default-class :: subclass(<float>) = <double-float>)
 => (result :: <float>, next-key :: <integer>);
  local
    method integer-part
        (index :: <integer>, neg? :: <boolean>, mantissa :: <extended-integer>)
     => (result :: <float>, next-key :: <integer>);
      if(index >= _end)
        finish-float(index, neg?, mantissa, 0, #f, 0, default-class);
      else
        select(string[index])
          '0' => integer-part(index + 1, neg?, mantissa * 10 + 0);
          '1' => integer-part(index + 1, neg?, mantissa * 10 + 1);
          '2' => integer-part(index + 1, neg?, mantissa * 10 + 2);
          '3' => integer-part(index + 1, neg?, mantissa * 10 + 3);
          '4' => integer-part(index + 1, neg?, mantissa * 10 + 4);
          '5' => integer-part(index + 1, neg?, mantissa * 10 + 5);
          '6' => integer-part(index + 1, neg?, mantissa * 10 + 6);
          '7' => integer-part(index + 1, neg?, mantissa * 10 + 7);
          '8' => integer-part(index + 1, neg?, mantissa * 10 + 8);
          '9' => integer-part(index + 1, neg?, mantissa * 10 + 9);
            
          '.' => fraction-part(index + 1, neg?, mantissa, 0);

          'e', 'E' =>
            exponent-sign(index + 1, neg?, mantissa, 0, default-class);
          's', 'S' =>
            exponent-sign(index + 1, neg?, mantissa, 0, <single-float>);
          'd', 'D' =>
            exponent-sign(index + 1, neg?, mantissa, 0, <double-float>);
          'x', 'X' =>
            exponent-sign(index + 1, neg?, mantissa, 0, <extended-float>);
          
          otherwise =>
            finish-float(index, neg?, mantissa, 0, #f, 0, default-class);
        end select;
      end if;
    end,
    method fraction-part
        (index :: <integer>, neg? :: <boolean>, mantissa :: <extended-integer>,
         scale :: <integer>)
     => (result :: <float>, next-key :: <integer>);
      if(index >= _end)
        finish-float(index, neg?, mantissa, scale, #f, 0, default-class);
      else
        select(string[index])
          '0' => fraction-part(index + 1, neg?, mantissa * 10 + 0, scale + 1);
          '1' => fraction-part(index + 1, neg?, mantissa * 10 + 1, scale + 1);
          '2' => fraction-part(index + 1, neg?, mantissa * 10 + 2, scale + 1);
          '3' => fraction-part(index + 1, neg?, mantissa * 10 + 3, scale + 1);
          '4' => fraction-part(index + 1, neg?, mantissa * 10 + 4, scale + 1);
          '5' => fraction-part(index + 1, neg?, mantissa * 10 + 5, scale + 1);
          '6' => fraction-part(index + 1, neg?, mantissa * 10 + 6, scale + 1);
          '7' => fraction-part(index + 1, neg?, mantissa * 10 + 7, scale + 1);
          '8' => fraction-part(index + 1, neg?, mantissa * 10 + 8, scale + 1);
          '9' => fraction-part(index + 1, neg?, mantissa * 10 + 9, scale + 1);
      
          'e', 'E' =>
            exponent-sign(index + 1, neg?, mantissa, scale, default-class);
          's', 'S' =>
            exponent-sign(index + 1, neg?, mantissa, scale, <single-float>);
          'd', 'D' =>
            exponent-sign(index + 1, neg?, mantissa, scale, <double-float>);
          'x', 'X' =>
            exponent-sign(index + 1, neg?, mantissa, scale, <extended-float>);
          
          otherwise =>
            finish-float(index, neg?, mantissa, scale, #f, 0, default-class);
        end select;
      end if;
    end,
    method exponent-sign
        (index :: <integer>, neg? :: <boolean>, mantissa :: <extended-integer>,
         scale :: <integer>,
         class :: subclass(<float>))
     => (result :: <float>, next-key :: <integer>);
      if (index >= _end)
        error("unrecognized floating-point number");
      else
        select(string[index])
          '-' =>
            if (index + 1 >= _end)
              error("unrecognized floating-point number");
            else
              exponent-part(index + 1, neg?, mantissa, scale, #t, 0, class);
            end;
          '+' =>
            if (index + 1 >= _end)
              error("unrecognized floating-point number");
            else
              exponent-part(index + 1, neg?, mantissa, scale, #f, 0, class);
            end;

          '0' => exponent-part(index + 1, neg?, mantissa, scale, #f, 0, class);
          '1' => exponent-part(index + 1, neg?, mantissa, scale, #f, 1, class);
          '2' => exponent-part(index + 1, neg?, mantissa, scale, #f, 2, class);
          '3' => exponent-part(index + 1, neg?, mantissa, scale, #f, 3, class);
          '4' => exponent-part(index + 1, neg?, mantissa, scale, #f, 4, class);
          '5' => exponent-part(index + 1, neg?, mantissa, scale, #f, 5, class);
          '6' => exponent-part(index + 1, neg?, mantissa, scale, #f, 6, class);
          '7' => exponent-part(index + 1, neg?, mantissa, scale, #f, 7, class);
          '8' => exponent-part(index + 1, neg?, mantissa, scale, #f, 8, class);
          '9' => exponent-part(index + 1, neg?, mantissa, scale, #f, 9, class);

          otherwise =>
            finish-float(index, neg?, mantissa, scale, #f, 0, class);
        end select;
      end if;
    end,
    method exponent-part
        (index :: <integer>, neg? :: <boolean>, mantissa :: <extended-integer>,
         scale :: <integer>, eneg? :: <boolean>, exponent :: <integer>,
         class :: subclass(<float>))
     => (result :: <float>, next-key :: <integer>);
      if(index >= _end)
        finish-float(index, neg?, mantissa, scale, eneg?, exponent, class);
      else
        select(string[index])
          '0' => exponent-part(index + 1, neg?, mantissa, scale,
                               eneg?, exponent * 10 + 0, class);
          '1' => exponent-part(index + 1, neg?, mantissa, scale,
                               eneg?, exponent * 10 + 1, class);
          '2' => exponent-part(index + 1, neg?, mantissa, scale,
                               eneg?, exponent * 10 + 2, class);
          '3' => exponent-part(index + 1, neg?, mantissa, scale,
                               eneg?, exponent * 10 + 3, class);
          '4' => exponent-part(index + 1, neg?, mantissa, scale,
                               eneg?, exponent * 10 + 4, class);
          '5' => exponent-part(index + 1, neg?, mantissa, scale,
                               eneg?, exponent * 10 + 5, class);
          '6' => exponent-part(index + 1, neg?, mantissa, scale,
                               eneg?, exponent * 10 + 6, class);
          '7' => exponent-part(index + 1, neg?, mantissa, scale,
                               eneg?, exponent * 10 + 7, class);
          '8' => exponent-part(index + 1, neg?, mantissa, scale,
                               eneg?, exponent * 10 + 8, class);
          '9' => exponent-part(index + 1, neg?, mantissa, scale,
                               eneg?, exponent * 10 + 9, class);
          otherwise =>
            finish-float(index, neg?, mantissa, scale, eneg?, exponent, class);
        end select;
      end if;
    end,
    method finish-float
        (index :: <integer>, neg? :: <boolean>, mantissa :: <extended-integer>,
         scale :: <integer>, eneg? :: <boolean>, exponent :: <integer>,
         class :: subclass(<float>))
     => (result :: <float>, next-key :: <integer>);
      let exponent = if(eneg?) -exponent else exponent end;
      let bits
        = select(class)
            <single-float> => float-digits(1.0s0);
            <double-float> => float-digits(1.0d0);
            <extended-float> => float-digits(1.0x0);
          end;
      values(if(neg?)
               -bellerophon(mantissa, exponent - scale, class, bits);
             else
               bellerophon(mantissa, exponent - scale, class, bits);
             end, index);
    end,

    method bellerophon
        (f :: <extended-integer>,
         e :: <integer>,
         class :: subclass(<float>),
         bits :: <integer>)
     => (result :: <float>);
      if (zero?(f))
        make-float(class, #e0, 0);
      else
        algorithm-M(f, e, class, bits);
      end;
    end,

    // Implements Algorithm M from William Clinger's "How to Read Floating-
    // Point Numbers Accurately" (PLDI 1990)
    //
    // ### Algorithm Bellerophon is much faster, need to implement it
    //
    method algorithm-M
        (f :: <extended-integer>,
         e :: <integer>,
         class :: subclass(<float>),
         bits :: <integer>)
     => (result :: <float>);
      let low = ash(#e1, bits - 1) - 1;
      let high = ash(#e1, bits) - 1;
      local
        method loop
            (u :: <extended-integer>, v :: <extended-integer>, k :: <integer>)
         => (result :: <float>);
          let x = floor/(u, v);
          if (low <= x & x < high)
            ratio-to-float(class, u, v, k);
          elseif (x < low)
            loop(u * 2, v, k - 1);
          else // x <= high
            loop(u, v * 2, k + 1);
          end if;
        end;
      if (negative?(e))
        loop(f, #e10 ^ -e, 0);
      else
        loop(f * #e10 ^ e, #e1, 0);
      end;
    end,
    method ratio-to-float
        (class :: subclass(<float>),
         u :: <extended-integer>, v :: <extended-integer>, k :: <integer>)
     => (result :: <float>);
      let (q, r) = floor/(u, v);
      let v-r = v - r;
      if (r < v-r)
        make-float(class, q, k);
      elseif (r > v-r)
        make-float(class, q + 1, k);
      elseif (even?(q))
        make-float(class, q, k);
      else
        make-float(class, q + 1, k);
      end if;
    end,
    method make-float
        (class :: subclass(<float>), q :: <extended-integer>, k :: <integer>)
     => (result :: <float>);
      scale-float(as(class, q), k);
    end method;

  if (_start >= _end)
    error("unrecognized floating-point number");
  elseif (string[_start] == '-')
    integer-part(_start + 1, #t, #e0);
  elseif (string[_start] == '+')
    integer-part(_start + 1, #f, #e0);
  else
    integer-part(_start, #f, #e0);
  end if;
end method;
