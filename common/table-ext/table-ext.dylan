module: Table-Extensions
author: Nick Kramer (nkramer@cs.cmu.edu), David Watson (dwatson@cmu.edu)
rcs-header: $Header: /scm/cvs/src/common/table-ext/table-ext.dylan,v 1.1 1998/05/03 19:55:05 andreas Exp $

//======================================================================
//
// Copyright (c) 1994, 1996  Carnegie Mellon University
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


// A value table whose keys are strings.
//
define class <string-table> (<value-table>)
end class <string-table>;

define method table-protocol (ht :: <string-table>)
 => (key-test :: <function>, key-hash :: <function>);
  values(\=, string-hash);
end method table-protocol;


// A value table whose keys are strings, compared without regard to case.
//
define class <case-insensitive-string-table> (<value-table>)
end class <case-insensitive-string-table>;

define method table-protocol (ht :: <case-insensitive-string-table>)
 => (key-test :: <function>, key-hash :: <function>);
  values(case-insensitive-equal, case-insensitive-string-hash);
end method table-protocol;


// This function hashes each object in the #rest arguments using
// elt-hash-function, merging the resulting hash codes in order.
//
define function values-hash (elt-hash-function :: <function>, #rest args)
 => (hash-id :: <integer>, hash-state :: <hash-state>);
  let (current-id, current-state) = values(0, $permanent-hash-state);
  for (elt in args)
    let (id, state) = elt-hash-function(elt);
    let (captured-id, captured-state)
      = merge-hash-codes(current-id, current-state, id, state, ordered: #t);
    current-id := captured-id;
    current-state := captured-state;
  end for;
  values(current-id, current-state);
end function values-hash;

// This function produces hash codes for strings using the equality test
// case-insensitive-equal.
//
define method case-insensitive-string-hash (s :: <string>)
 => (id :: <integer>, hash-state :: <hash-state>);
  string-hash(as-lowercase(s));
end method case-insensitive-string-hash;

define method case-insensitive-equal (o1 :: <object>, o2 :: <object>)
 => answer :: <boolean>;
  #f;
end method case-insensitive-equal;

// This is useful for converting from uppercase to lowercase
//
define constant a-minus-A 
  = as(<integer>, 'a') - as(<integer>, 'A');

// Only works for ASCII and Unicode, and the case folding part works
// only for English.
//
// The idea is to do equality checks first, and only if they are
// somehow equal do further computation to see if the equality
// actually meant anything.
//
define method case-insensitive-equal (c1 :: <character>, c2 :: <character>)
 => answer :: <boolean>;
  c1 == c2
    | (as(<integer>, c1)
	  == as(<integer>, c2) + a-minus-A & uppercase?(c2))
    | (as(<integer>, c1) + a-minus-A
	 == as(<integer>, c2) & uppercase?(c1));
end method case-insensitive-equal;

define method case-insensitive-equal (s1 :: <string>, s2 :: <string>)
 => answer :: <boolean>;
  if (s1.size ~== s2.size)
    #f;
  else
    block (return)
      for (c1 in s1, c2 in s2)
	if (~ case-insensitive-equal(c1, c2))
	  return(#f);
	end if;
      end for;
      #t;
    end block;
  end if;
end method case-insensitive-equal;


// This method implements remove-all-keys! by repeated calls to remove-key!
//
define method remove-all-keys! (coll :: <mutable-explicit-key-collection>)
 => (coll :: <mutable-explicit-key-collection>);
#if (~mindy)
  let (state, limit, next, done?, cur-key, cur-elem)
    = forward-iteration-protocol(coll);
  for (st = state then next(coll, st), until: done?(coll, st, limit))
    let key = cur-key(coll, st);
#else
  for (elt keyed-by key in coll)
#endif
    remove-key!(coll, key);
  end for;
  coll;
end method remove-all-keys!;
