rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/char.dylan,v 1.7 1996/03/17 00:11:23 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

//
// Characters.
//
// This file contains the support for characters.  Unicode support is kinda
// sub-standard at the moment, but thats okay because we don't use them yet.
// 


// $max-char-code -- internal.
//
// The maximum integer code for a unicode character.
// 
define constant $max-char-code = #xffff;

// <char-code> -- internal.
//
// The valid character codes.
// 
define constant <char-code>
  = limited(<integer>, min: 0, max: $max-char-code);

// <character> -- exported from Dylan.
//
define functional class <character> (<object>)
  //
  // The unicode code for this character.
  slot value :: <char-code>, required-init-keyword: code:;
end;

// Seal make and initialize so that we can fully inline construction of
// characters.
// 
define sealed domain make (singleton(<character>));
define sealed domain initialize (<character>);

// The fact that we are a functional class should automatically define this,
// but it doesn't yet.
//
define inline method functional-==
    (class == <character>, a :: <character>, b :: <character>)
    => res :: <boolean>;
  a.value == b.value;
end;
//
define sealed domain functional-== (singleton(<character>), <object>, <object>);

// as{singleton(<character>),<integer>} -- exported GF method.
//
// Convert the integer into a character.
// 
define sealed inline method as (class == <character>, code :: <integer>)
    => res :: <character>;
  make(<character>, code: code);
end;

// as{singleton(<integer>),<character>} -- exported GF method.
//
// Convert the character into an integer.
// 
define sealed inline method as (class == <integer>, char :: <character>)
    => res :: <integer>;
  char.value;
end;

// Seal = on characters.  But not yet.
//
//seal generic \= (<character>, <object>);
//seal generic \= (<object>, <character>);

// < -- exported GF method.
//
// Compare the two characters by character code.
// 
define inline method \< (char1 :: <character>, char2 :: <character>)
    => res :: <boolean>;
  char1.value < char2.value;
end;

// Seal < on characters.
// 
define sealed domain \< (<character>, <object>);
define sealed domain \< (<object>, <character>);

// as-uppercase -- exported GF method.
//
// Convert the character to uppercase.  Currently, we only support this for
// ASCII characters.
// 
define sealed method as-uppercase (char :: <character>)
    => res :: <character>;
  if ('a' <= char & char <= 'z')
    as(<character>, as(<integer>, char) - 32);
  else
    char;
  end;
end;

// as-lowercase -- exported GF method.
//
// Convert the character to lowercase.  Currently, we only support this for
// ASCII characters.
// 
define sealed method as-lowercase (char :: <character>)
    => res :: <character>;
  if ('A' <= char & char <= 'Z')
    as(<character>, as(<integer>, char) + 32);
  else
    char;
  end;
end;
