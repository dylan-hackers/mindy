rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/char.dylan,v 1.3 2003/09/30 16:30:26 housel Exp $
copyright: see below
module: dylan-viscera

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
// All rights reserved.
// 
// Use and copying of this software and preparation of derivative
// works based on this software are permitted, including commercial
// use, provided that the following conditions are observed:
// 
// 1. This copyright notice must be retained in full on any copies
//    and on appropriate parts of any derivative works.
// 2. Documentation (paper or online) accompanying any system that
//    incorporates this software, or any part of it, must acknowledge
//    the contribution of the Gwydion Project at Carnegie Mellon
//    University, and the Gwydion Dylan Maintainers.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
// Also, see http://www.gwydiondylan.org/ for updates and documentation. 
//
//======================================================================

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

define sealed inline method as (type :: <limited-integer>, char :: <character>)
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
