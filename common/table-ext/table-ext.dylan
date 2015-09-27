module: Table-Extensions
author: Nick Kramer (nkramer@cs.cmu.edu), David Watson (dwatson@cmu.edu)

//======================================================================
//
// Copyright (c) 1994, 1996  Carnegie Mellon University
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


// A value table whose keys are strings.
//
define sealed class <string-table> (<value-table>)
end class <string-table>;

define sealed domain make(singleton(<string-table>));
define sealed domain initialize(<string-table>);

define sealed inline method table-protocol (ht :: <string-table>)
 => (key-test :: <function>, key-hash :: <function>);
  values(\= , string-hash);
end method table-protocol;


define sealed class <byte-string-table> (<value-table>)
end class <byte-string-table>;

define sealed domain make(singleton(<byte-string-table>));
define sealed domain initialize(<byte-string-table>);

define sealed inline method table-protocol (ht :: <byte-string-table>)
 => (key-test :: <function>, key-hash :: <function>);
  values(method(a :: <byte-string>, b :: <byte-string>) a = b end,
         string-hash);
end method table-protocol;


// A value table whose keys are strings, compared without regard to case.
//
define sealed class <case-insensitive-string-table> (<value-table>)
end class <case-insensitive-string-table>;

define sealed domain make(singleton(<case-insensitive-string-table>));
define sealed domain initialize(<case-insensitive-string-table>);

define sealed inline method table-protocol (ht :: <case-insensitive-string-table>)
 => (key-test :: <function>, key-hash :: <function>);
  values(case-insensitive-equal, case-insensitive-string-hash);
end method table-protocol;


define sealed class <case-insensitive-byte-string-table> (<value-table>)
end class <case-insensitive-byte-string-table>;

define sealed domain make(singleton(<case-insensitive-byte-string-table>));
define sealed domain initialize(<case-insensitive-byte-string-table>);

define sealed inline method table-protocol (ht :: <case-insensitive-byte-string-table>)
 => (key-test :: <function>, key-hash :: <function>);
  values(case-insensitive-equal, case-insensitive-string-hash);
end method table-protocol;


// This function hashes each object in the #rest arguments using
// elt-hash-function, merging the resulting hash codes in order.
//
define function values-hash (elt-hash-function :: <function>,
			     initial-state :: <hash-state>, #rest args)
 => (hash-id :: <integer>, hash-state :: <hash-state>);
  let (current-id, current-state) = values(0, initial-state);
  for (elt in args)
    let (id, state) = elt-hash-function(elt, current-state);
    let captured-id
      = merge-hash-ids(current-id, id, ordered: #t);
    current-id := captured-id;
    current-state := state;
  end for;
  values(current-id, current-state);
end function values-hash;

// This function produces hash codes for strings using the equality test
// case-insensitive-equal.
//
define method case-insensitive-string-hash
    (s :: <string>, initial-state :: <hash-state>)
 => (id :: <integer>, hash-state :: <hash-state>);
  string-hash(as-lowercase(s), initial-state);
end method case-insensitive-string-hash;

define method case-insensitive-string-hash
    (s :: <byte-string>, initial-state :: <hash-state>)
 => (id :: <integer>, hash-state :: <hash-state>);
  string-hash(as-lowercase(s), initial-state);
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

define method case-insensitive-equal (s1 :: <byte-string>, s2 :: <byte-string>)
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
