module: Dylan

//////////////////////////////////////////////////////////////////////
//
//  Copyright (C) 1993, 1994, Carnegie Mellon University
//  All rights reserved.
//
//  This code was produced by the Gwydion Project at Carnegie Mellon
//  University.  If you are interested in using this code, contact
//  "Scott.Fahlman@cs.cmu.edu" (Internet).
//
//////////////////////////////////////////////////////////////////////
//
//  $Header: /home/housel/work/rcs/gd/src/mindy/libraries/dylan/string.dylan,v 1.2 1994/03/30 06:07:29 wlott Exp $
//
//  This file does whatever.
//

define method make (clas == <string>, #rest keyword-value-pairs)
  apply(make, <byte-string>, keyword-value-pairs)
end make;

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

