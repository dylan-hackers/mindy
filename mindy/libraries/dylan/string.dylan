module: Dylan
rcs-header: $Header: /scm/cvs/src/mindy/libraries/dylan/string.dylan,v 1.1 1998/05/03 19:55:21 andreas Exp $

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
//
//  This file contains the support for strings that isn't built in.
//

// By adding this method, we insure that the one which follows isn't
// erroneously applied to things which are already strings.
//
define method as (clas == <string>, collection :: <string>)
 => string :: <string>;
  collection;
end method as;

define method as (clas == <string>, collection :: <collection>)
 => string :: <string>;
  as(<byte-string>, collection)
end as;

define method \< (string1 :: <string>, string2 :: <string>)
 => answer :: <boolean>;
  block (return)
    let (init, limit, next, done?, key, elem) =
       forward-iteration-protocol(string2);
    for (char1 in string1,
	 state = init then next(string2, state),
	 until: done?(string2, state, limit))
      let char2 = elem(string2, state);
      case
	char1 < char2 => return(#t);
	char1 > char2 => return(#f);
 	otherwise => #f;
      end case
    finally
      if (done?(string2, state, limit)) #f else #t end
    end for
  end block
end \<;

define generic as-uppercase (object :: <object>) => new :: <object>;
define generic as-lowercase (object :: <object>) => new :: <object>;
define generic as-uppercase! (object :: <object>) => same :: <object>;
define generic as-lowercase! (object :: <object>) => same :: <object>;

define method as-lowercase (string :: <string>)
 => new-string :: <string>;
  map(as-lowercase, string)
end as-lowercase;

define method as-uppercase (string :: <string>)
 => new-string :: <string>;
  map(as-uppercase, string)
end as-uppercase;

define method as-lowercase! (string :: <string>)
 => string :: <string>;
  map-into(string, as-lowercase, string)
end as-lowercase!;

define method as-uppercase! (string :: <string>)
 => string :: <string>;
  map-into(string, as-uppercase, string)
end as-uppercase!;

// Provide a type error rather than a no applicable methods error when
// someone tries to put something illegal into a <string>
//
define method element-setter
    (new, string :: <byte-string>, index :: <integer>)
 => new :: <object>;
  error(make(<type-error>, value: new, type: <byte-character>));
end;

define method element-setter 
    (new, string :: <unicode-string>, index :: <integer>)
 => new :: <object>;
  error(make(<type-error>, value: new, type: <character>));
end;

define method copy-sequence
    (vector :: <byte-string>, #key start = 0, end: last)
 => copy :: <byte-string>;
  let src-sz = size(vector);
  let last = if (last & last < src-sz) last else src-sz end if;
  let sz = last - start;
  let result = make(<byte-string>, size: sz);
  copy-bytes(result, 0, vector, start, sz);
  result;
end method copy-sequence;

// Specialized method which takes advantage of "copy-bytes".  Yields ~15%
// speedup for some apps.
define method concatenate-as
    (cls == <byte-string>, vector :: <byte-string>, #next next-method,
     #rest more_vectors)
 => new-string :: <byte-string>;
  let vector-count = size(more_vectors);
  case
    vector-count == 0 =>
      // We must check for this case
      copy-sequence(vector);
    vector-count == 1 =>
      // We can get big wins in the common two-string case.
      let second-vector = first(more_vectors);
      if (instance?(second-vector, <byte-string>))
	let size1 = size(vector);
	let size2 = size(second-vector);
      
	let result = make(cls, size: size1 + size2);
	copy-bytes(result, 0, vector, 0, size1);
	copy-bytes(result, size1, second-vector, 0, size2);
	result;
      else
	next-method();
      end if;
    otherwise =>
      block (return)
	// Strange test.  By combining size computation and checking that all
	// args are byte-strings, we gain time in the typical case, while not
	// losing much in the odd cases.  The main cost is in code clarity.
	let length = for (sz = vector.size then sz + seq.size,
			  seq in more_vectors)
		       if (~instance?(seq, <byte-string>))
			 return(next-method())
		       end if;
		     finally sz;
		     end for;
	let result = make(cls, size: length);
	for (next in more_vectors,
	     src = vector then next,
	     sz = size(vector) then size(next),
	     index = 0 then index + sz)
	  copy-bytes(result, index, src, 0, sz);
	finally
	  copy-bytes(result, index, src, 0, sz);
	end for;
	result;
      end block;
  end case;
end method concatenate-as;
