module:     dylan-user
author:     Nick Kramer (nkramer@cs.cmu.edu)
synopsis:   Contains the library and module definitions for the String
            Extensions library.
copyright:  Copyright (C) 1994, Carnegie Mellon University.
            All rights reserved.
rcs-header: $Header: /home/housel/work/rcs/gd/src/common/string-ext/library.dylan,v 1.1 1996/02/17 16:12:26 nkramer Exp $

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


define library string-extensions
  use dylan;
  use collection-extensions;
  export
    string-conversions, character-type, string-hacking,
    substring-search, regular-expressions;
end library string-extensions;


// Except for the names, these are identical to the functions found in
// the C library ctype.
//
define module character-type
  use dylan;
  use extensions;
  export
    alpha?, digit?, alphanumeric?, whitespace?,
    uppercase?, lowercase?, hex-digit?, graphic?, printable?,
    punctuation?, control?, byte-character?;
end module character-type;


define module parse-string
  use dylan;
  use extensions;
  export <parse-string>, consume, lookahead, parse-string-done?;
end module parse-string;


// Contains various useful string and character functions
//
define module string-hacking
  use dylan;
  use extensions;
  use character-type;
  use parse-string;
  export
    predecessor, successor, add-last, case-insensitive-equal,
    <character-set>, <case-sensitive-character-set>, 
    <case-insensitive-character-set>, 
    <byte-character-table>;
end module string-hacking;


// Contains various conversions that one would think would be part of
// Dylan, such as int to string and string to int.
// Also contains an "as" method for converting a character to a string.
//
define module string-conversions
  use dylan;
  use extensions, 
    import: {<general-integer>, <extended-integer>, $maximum-integer,
	       $minimum-integer};
  use string-hacking;
  use character-type;
  export
    string-to-integer, integer-to-string, 
    digit-to-integer, integer-to-digit;
end module string-conversions;


define module do-replacement
  use dylan;
  use extensions;
  use character-type;
  use string-conversions;
  export do-replacement;
end module do-replacement;


// Robert's Boyer-Moore implementation
//
define module substring-search
  use dylan;
  use extensions;
  use subseq;
  use string-hacking;
  use do-replacement;
  export
    substring-position, make-substring-positioner, 
    substring-replace, make-substring-replacer;
end module substring-search;


define module regular-expressions
  use dylan;
  use extensions;
  use string-conversions;
  use character-type;
  use string-hacking;
  use subseq;
  use do-replacement;
  use parse-string;
  use substring-search;
  export
    regexp-position, make-regexp-positioner,
    regexp-replace, make-regexp-replacer,
    translate, make-translator,
    split, make-splitter,
    join;
end module regular-expressions;

