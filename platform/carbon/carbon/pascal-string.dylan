module: carbon

/*
	Include the Macintosh Type Declaration Header so we know what a Str255 is.
*/

c-include("Carbon.h");


/*
	Pascal Strings.
*/


/*
	<pascal-string>
	A 256-byte char string with the first byte containing the length of the string.
	Pass raw-value into Toolbox call-out() calls.
*/

define open functional class <pascal-string> ( <string>, <statically-typed-pointer> ) 
end class <pascal-string>;


/*
	content-size
	The size of object a <pascal-string> contains
*/

define method content-size( cls == <pascal-string> )
=> ( result :: <integer> )
	256;
end method content-size;


/*
	initialize <pascal-string>
*/

define method initialize( str :: <pascal-string>, #key )
	unsigned-byte-at( str, offset: 0 ) := 0;
end method initialize;


/*
	as <pascal-string>
	Do-nothing coercion.
*/

define method as (cls == <Pascal-string>, str :: <Pascal-string>) 
=> (result :: <Pascal-string>)
  str;
end method as;


/*
	forward-iteration-protocol <pascal-string>
*/

define method forward-iteration-protocol(str :: <Pascal-string>)
=>   (initial-state          :: <integer>,
      limit                  :: <integer>,
      next-state             :: <function>,  finished-state? :: <function>,
      current-key            :: <function>,  current-element :: <function>,
      current-element-setter :: <function>,  copy-state      :: <function>)
  values(0, #f,
	 method (str, state) state + 1 end method,
	 method (str, state, limit)
	   limit >= unsigned-byte-at(str);
	 end method,
	 method (str, state) state end method,
	 method (str, state)
	   as(<character>, unsigned-byte-at(str, offset: state + 1));
	 end method,
	 method (value :: <character>, str, state)
	   unsigned-byte-at(str, offset: state + 1) := as(<integer>, value);
	 end method,
	 method (str, state) state end method
	 );
end method forward-iteration-protocol;

/*
define method \< (str1 :: <Pascal-string>, str2 :: <Pascal-string>)
 => result :: <object>;
  for (c1 in str1, c2 in str2, while c1 < c2)
  finally
    #t;
  end for;
end method \<;
*/

/*
	content-size <pascal-string>
	Returns the size of each element in a pascal string.
*/

define sealed method content-size( value :: subclass(<pascal-string>) )
 => (result :: <integer>)
  1;
end method content-size;


/*
	size <pascal-string>
	Gets the size of the pascal string.
*/

define method size (string :: <Pascal-string>) 
=> ( result :: <integer> )
	unsigned-byte-at(string, offset: 0);
end method size;


/*
	size <pascal-string>
	Sets the size of the pascal string.
*/

define method size-setter (new-size :: <integer>, string :: <Pascal-string>)
 => ( new-size :: <object> )
	unsigned-byte-at(string, offset: 0) := new-size;
end method size-setter;


/*
	element <pascal-string>
	Gets a char from of the pascal string.
	The zeroth character is the first char in the string, not the length byte.
*/


define method element (string :: <Pascal-string>, index :: <integer>, #key default: def) 
=> ( value :: <character> )
  as(<character>, unsigned-byte-at(string, offset: index + 1));
end method element;


/*
	element-setter <pascal-string>
	Sets a char in the pascal string.
	The zeroth character is the first char in the string, not the length byte.
	Catches out of range indices.
	Doesn't change the pascal string's length.
*/

define method element-setter (value :: <character>, string :: <Pascal-string>, index :: <integer>)
=> ( value :: <object> )
  if( index < 255 )
  	unsigned-byte-at(string, offset: index + 1) := as(<integer>, value);
  	if( index > unsigned-byte-at(string, offset: 0) )
  		unsigned-byte-at(string, offset: 0) := index;
  	end if;
  else
    #f;
  end if;
end method element-setter;


/*
	As from <byte-string>
	Coercion from <byte-string>
	Will truncate if needed.
*/

define method as (cls == <Pascal-string>, str :: <byte-string>)
 => ( result :: <Pascal-String> )
  let sz =  case 
  				str.size < 256	=>	str.size;
  				otherwise => 255;
  			end case;
  let result = make( <pascal-string> );
  for (i from 1 to sz)
    unsigned-byte-at(result, offset: i) := as(<integer>, str[i - 1]);
  end for;
  unsigned-byte-at(result, offset: 0) := sz;
  result;
end method as;


/*
	As <string>
	Coercion to <byte-string>
*/

define method as (cls == <string>, str :: <Pascal-string>)
 => ( result :: <string> )
  let sz = str.size;
  let result = make(<byte-string>, size: sz);
  for (i from 0 below sz)
    result[i] := as(<character>, unsigned-byte-at(str, offset: i + 1));
  end for;
  result;
end method as;
