module:   character-type
author:   Nick Kramer (nkramer@cs.cmu.edu)
synopsis: Implements character tests that are in C's ctype library, 
          as well as predecessor and successor (which help preserve
          sanity while stepping through all possible characters)
copyright:  Copyright (C) 1994, Carnegie Mellon University.
            All rights reserved.
rcs-header: $Header: /home/housel/work/rcs/gd/src/common/string-ext/character-type.dylan,v 1.1 1996/02/17 16:12:26 nkramer Exp $

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
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
//    University.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports, questions, comments, and suggestions should be sent by
// E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
//
//======================================================================

// isalpha
//
define method alpha? (c :: <character>) => answer :: <boolean>;
  (c >= 'a' & c <= 'z')  |  (c >= 'A' & c <= 'Z');
end method alpha?;

// isdigit
//
define method digit? (c :: <character>) => answer :: <boolean>;
  (c >= '0' & c <= '9');
end method digit?;

// isalnum
//
define method alphanumeric? (c :: <character>) => answer :: <boolean>;
  (c >= 'a' & c <= 'z')  |  (c >= 'A' & c <= 'Z')  |  (c >= '0' & c <= '9');
end method alphanumeric?;

// isspace
//
define method whitespace? (c :: <character>) => answer :: <boolean>;
  select (c)
    ' ', '\t', '\n', '\f', '\r' => #t;       
                        // Space, tab, newline, formfeed, carriage return
    otherwise => #f;
  end select;
end method whitespace?;

// isupper
//
define method uppercase? (c :: <character>) => answer :: <boolean>;
  c >= 'A' & c <= 'Z';
end method uppercase?;

// islower
//
define method lowercase? (c :: <character>) => answer :: <boolean>;
  c >= 'a' & c <= 'z';
end method lowercase?;

// isxdigit
//
define method hex-digit? (c :: <character>) => answer :: <boolean>;
  (c >= '0' & c <= '9')  |  (c >= 'a' & c <= 'f')  |  (c >= 'A' & c <= 'F');
end method hex-digit?;

// isgraph -- printing character that's not space
//
define method graphic? (c :: <character>) => answer :: <boolean>;
  alphanumeric?(c) | punctuation?(c);
end method graphic?;

// isprint
//
define method printable? (c :: <character>) => answer :: <boolean>;
  graphic?(c) | whitespace?(c);
end method printable?;

// ispunct
//
define method punctuation? (c :: <character>) => answer :: <boolean>;
  select (c)
    ',', '.', '/', '<', '>', '?', ';', '\'', ':', '"',
    '|', '\\', '[', ']', '{', '}',
    '!', '@', '#', '$', '%', '^', '&', '*', '(', ')',
    '-', '=', '_', '+', '`', '~'
      => #t;
    otherwise => #f;
  end select;
end method punctuation?;

// iscntrl
//
define method control? (c :: <character>) => answer :: <boolean>;
  ~ printable?(c);
end method control?;

// byte-character? is the only function here that has no C equivalent.
//
// As soon as we accept the concept of <byte-character>, we can change
// this definition.
//
define method byte-character? (c :: <character>) => answer :: <boolean>;
  as(<integer>, c) < 256;
end method byte-character?;


