module: Toolbox
author: Patrick Beard, maintenance by Rob Myers (yarrel@netscape.net)
copyright: (c)1999 Patrick Beard

// Pascal Strings.

define class <Pascal-string> (<string>, <Ptr>) 
end class <Pascal-string>;

define method as (cls == <Pascal-string>, str :: <Pascal-string>) 
=> (result :: <Pascal-string>);
  str;
end method as;

define method make(cls :: limited(<class>, subclass-of: <Pascal-string>),
		   				#key size: sz = 0, fill = ' ')
=> (result :: <Pascal-string>);
  let result = as(cls, NewPtr(256));
  let fill-byte = as(<integer>, fill);
  for (i from 1 to sz)
    unsigned-byte-at(result, offset: i) := fill-byte;
  end for;
  unsigned-byte-at(result, offset: 0) := sz;
  result;
end method make;

define method forward-iteration-protocol(str :: <Pascal-string>)
=>   (initial-state          :: <integer>,
      limit                  :: <integer>,
      next-state             :: <function>,  finished-state? :: <function>,
      current-key            :: <function>,  current-element :: <function>,
      current-element-setter :: <function>,  copy-state      :: <function>);
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

define method size (string :: <Pascal-string>) => result :: <integer>;
	unsigned-byte-at(string, offset: 0);
end method size;

define method size-setter (new-size :: <integer>, string :: <Pascal-string>)
 => new-size :: <object>;
	unsigned-byte-at(string, offset: 0) := new-size;
end method size-setter;

define method element (string :: <Pascal-string>, index :: <integer>, #key default: def) 
=> value :: <character>;
  as(<character>, unsigned-byte-at(string, offset: index + 1));
end method element;

define method element-setter (value :: <character>, string :: <Pascal-string>, index :: <integer>)
=> value :: <object>;
  unsigned-byte-at(string, offset: index + 1) := as(<integer>, value);
end method element-setter;

// This is a very common operation, so let's make it fast.

define method as (cls == <Pascal-string>, str :: <byte-string>)
 => result :: <Pascal-String>;
  let sz = str.size;
  let result = as(<Pascal-string>, NewPtr(256));
  for (i from 1 to sz)
    unsigned-byte-at(result, offset: i) := as(<integer>, str[i - 1]);
  end for;
  unsigned-byte-at(result, offset: 0) := sz;
  result;
end method as;

// This is a very common operation, so let's make it fast.
//
define method as (cls == <byte-string>, str :: <Pascal-string>)
 => result :: <byte-string>;
  let sz = str.size;
  let result = make(<string>, size: sz);
  for (i from 0 below sz)
    result[i] := as(<character>, unsigned-byte-at(str, offset: i + 1));
  end for;
  result;
end method as;