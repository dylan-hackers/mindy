rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/string.dylan,v 1.4 2003/02/03 12:32:45 gabor Exp $
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

// General string stuff.

define open abstract class <string> (<mutable-sequence>)
end;

define sealed inline method make (class == <string>, #key size = 0, fill = ' ')
    => res :: <string>;
  make(<byte-string>, size: size, fill: fill);
end;

define sealed inline method as (class == <string>, collection :: <collection>)
    => res :: <string>;
  as(<byte-string>, collection);
end;

define inline method as (class == <string>, string :: <string>)
    => res :: <string>;
  string;
end;

define method \< (str1 :: <string>, str2 :: <string>) => res :: <boolean>;
  block (return)
    for (char1 in str1, char2 in str2)
      if (char1 < char2)
	return(#t);
      elseif (char2 < char1)
	return(#f);
      end;
    end;
    str1.size < str2.size;
  end;
end;

define method \< (str1 :: <byte-string>, str2 :: <byte-string>) => res :: <boolean>;
  block (return)
    for (char1 in str1, char2 in str2)
      if (char1 < char2)
	return(#t);
      elseif (char2 < char1)
	return(#f);
      end;
    end;
    str1.size < str2.size;
  end;
end;

define method as-lowercase (str :: <string>)
    => res :: <string>;
  map(as-lowercase, str);
end;

define method as-lowercase! (str :: <string>)
    => res :: <string>;
  map-into(str, as-lowercase, str);
end;

define method as-uppercase (str :: <string>)
    => res :: <string>;
  map(as-uppercase, str);
end;

define method as-uppercase! (str :: <string>)
    => res :: <string>;
  map-into(str, as-uppercase, str);
end;

define method as-lowercase (str :: <byte-string>)
    => res :: <byte-string>;
  map(as-lowercase, str);
end;

define method as-lowercase! (str :: <byte-string>)
    => res :: <byte-string>;
  map-into(str, as-lowercase, str);
end;

define method as-uppercase (str :: <byte-string>)
    => res :: <byte-string>;
  map(as-uppercase, str);
end;

define method as-uppercase! (str :: <byte-string>)
    => res :: <byte-string>;
  map-into(str, as-uppercase, str);
end;


// Built-in strings.


// Unicode strings.

define class <unicode-string> (<string>, <vector>)
  sealed slot %element :: <character>,
    init-value: ' ', init-keyword: fill:,
    sizer: size, size-init-value: 0, size-init-keyword: size:;
end;

define sealed domain make (singleton(<unicode-string>));

define sealed method as (class == <unicode-string>, collection :: <collection>)
    => res :: <unicode-string>;
  let res = make(<unicode-string>, size: collection.size);
  for (index :: <integer> from 0, element in collection)
    res[index] := element;
  end;
  res;
end;

define inline method as (class == <unicode-string>, string :: <unicode-string>)
    => res :: <unicode-string>;
  string;
end;

define inline method element
    (vec :: <unicode-string>, index :: <integer>,
     #key default = $not-supplied)
    => element :: <object>; // because of default:
  if (index >= 0 & index < vec.size)
    %element(vec, index);
  elseif (default == $not-supplied)
    element-error(vec, index);
  else
    default;
  end;
end;

define inline method element-setter
    (new-value :: <character>, vec :: <unicode-string>,
     index :: <integer>)
    => new-value :: <character>;
  if (index >= 0 & index < vec.size)
    %element(vec, index) := new-value;
  else
    element-error(vec, index);
  end;
end;

// This method is identical to the one in "array.dylan", except that it
// is more tightly specialized to a single sealed class.  If you need to 
// make a general change, you should probably grep for "outlined-iterator" 
// and change all matching locations.
//
define inline method forward-iteration-protocol (array :: <unicode-string>)
    => (initial-state :: <integer>,
	limit :: <integer>,
	next-state :: <function>,
	finished-state? :: <function>,
	current-key :: <function>,
	current-element :: <function>,
	current-element-setter :: <function>,
	copy-state :: <function>);
  values(0,
	 array.size,
	 method (array :: <unicode-string>, state :: <integer>)
	     => new-state :: <integer>;
	   state + 1;
	 end,
	 method (array :: <unicode-string>, state :: <integer>,
		 limit :: <integer>)
	     => done? :: <boolean>;
	   // We use >= instead of == so that the constraint propagation
	   // stuff can tell that state is < limit if this returns #f.
	   state >= limit;
	 end,
	 method (array :: <unicode-string>, state :: <integer>)
	     => key :: <integer>;
	   state;
	 end,
	 method (array :: <unicode-string>, state :: <integer>)
	     => element :: <object>;
	   element(array, state);
	 end,
	 method (new-value :: <object>, array :: <unicode-string>,
		 state :: <integer>)
	     => new-value :: <object>;
	   element(array, state) := new-value;
	 end,
	 method (array :: <unicode-string>, state :: <integer>)
	     => state-copy :: <integer>;
	   state;
	 end);
end;


// Byte strings.

define class <byte-string> (<string>, <vector>)
  sealed slot %element :: <byte-character>,
    init-value: ' ', init-keyword: fill:,
    sizer: size, size-init-value: 0, size-init-keyword: size:;
end;

define sealed domain make (singleton(<byte-string>));

define sealed method as (class == <byte-string>, collection :: <collection>)
    => res :: <byte-string>;
  let res = make(<byte-string>, size: collection.size);
  for (index :: <integer> from 0, element in collection)
    res[index] := element;
  end;
  res;
end;

define inline method as (class == <byte-string>, string :: <byte-string>)
    => res :: <byte-string>;
  string;
end;

define inline method element
    (vec :: <byte-string>, index :: <integer>,
     #key default = $not-supplied)
    => element :: <object>; // because of default:
  if (index >= 0 & index < vec.size)
    %element(vec, index);
  elseif (default == $not-supplied)
    element-error(vec, index);
  else
    default;
  end;
end;

define inline method element-setter
    (new-value :: <byte-character>, vec :: <byte-string>,
     index :: <integer>)
    => new-value :: <byte-character>;
  if (index >= 0 & index < vec.size)
    %element(vec, index) := new-value;
  else
    element-error(vec, index);
  end;
end;

// This method is identical to the one in "array.dylan", except that it
// is more tightly specialized to a single sealed class.  If you need to 
// make a general change, you should probably grep for "outlined-iterator" 
// and change all matching locations.
//
define inline method forward-iteration-protocol (array :: <byte-string>)
    => (initial-state :: <integer>,
	limit :: <integer>,
	next-state :: <function>,
	finished-state? :: <function>,
	current-key :: <function>,
	current-element :: <function>,
	current-element-setter :: <function>,
	copy-state :: <function>);
  values(0,
	 array.size,
	 method (array :: <byte-string>, state :: <integer>)
	     => new-state :: <integer>;
	   state + 1;
	 end,
	 method (array :: <byte-string>, state :: <integer>,
		 limit :: <integer>)
	     => done? :: <boolean>;
	   // We use >= instead of == so that the constraint propagation
	   // stuff can tell that state is < limit if this returns #f.
	   state >= limit;
	 end,
	 method (array :: <byte-string>, state :: <integer>)
	     => key :: <integer>;
	   state;
	 end,
	 method (array :: <byte-string>, state :: <integer>)
	     => element :: <object>;
	   element(array, state);
	 end,
	 method (new-value :: <object>, array :: <byte-string>,
		 state :: <integer>)
	     => new-value :: <object>;
	   element(array, state) := new-value;
	 end,
	 method (array :: <byte-string>, state :: <integer>)
	     => state-copy :: <integer>;
	   state;
	 end);
end;

define method \= (str1 :: <byte-string>, str2 :: <byte-string>)
 => (res :: <boolean>);
  block (return)
    // the obvious shortcuts
    if (str1 == str2) return(#t) end if;
    if (str1.size ~== str2.size) return(#f) end if;
    //
    // char-by-char compare
    for (char1 in str1, char2 in str2)
      if (char1 ~== char2)
	return(#f);
      end if;
    finally
      #t;
    end for;
  end;
end;

define method copy-sequence
    (vector :: <byte-string>, #key start :: <integer> = 0, end: last :: false-or(<integer>))
 => (result :: <byte-string>);
  let src-sz :: <integer> = size(vector);
  let last :: <integer>
    = if (last & last < src-sz) last else src-sz end if;
  let start :: <integer> = if (start < 0) 0 else start end if;
  let sz :: <integer> = last - start;

  let result :: <byte-string> = make(<byte-string>, size: sz);
  for (from-index :: <integer> from start below last,
       to-index :: <integer> from 0)
    %element(result, to-index) := %element(vector, from-index);
  end for;
  result;
end method copy-sequence;
