module: Dylan
rcs-header: $Header: /home/housel/work/rcs/gd/src/mindy/libraries/dylan/string.dylan,v 1.5 1994/06/27 17:10:38 wlott Exp $

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

define method as (clas == <string>, collection :: <collection>)
  as(<byte-string>, collection)
end as;

define method \< (string1 :: <string>, string2 :: <string>)
  block (return)
    let (init, limit, next, done?, key, elem) =
       forward-iteration-protocol(string2);
    for (char1 in string1,
	 state = init then next(string2, state),
	 until done?(string2, state, limit))
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

define method as-lowercase (string :: <string>)
  map(as-lowercase, string)
end as-lowercase;

define method as-uppercase (string :: <string>)
  map(as-uppercase, string)
end as-uppercase;

define method as-lowercase! (string :: <string>)
  map-into(string, as-lowercase, string)
end as-lowercase!;

define method as-uppercase! (string :: <string>)
  map-into(string, as-uppercase, string)
end as-uppercase!;



define method element-setter (new, string :: <byte-string>, index :: <integer>)
  error(make(<type-error>, value: new, type: <character>));
end;
