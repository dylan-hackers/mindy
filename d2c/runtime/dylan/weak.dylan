rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/weak.dylan,v 1.1 1998/05/03 19:55:39 andreas Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
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

define class <weak-pointer> (<object>)
end;

define method make (class == <weak-pointer>,
		    #key object = required-keyword(object:))
  %%primitive(make-weak-pointer, object);
end;

define method weak-pointer-object (weak-ptr :: <weak-pointer>)
    => (object :: <object>, broken :: <boolean>);
  %%primitive(weak-pointer-object, weak-ptr);
end;



// Weak lists.

define class <weak-list> (<mutable-sequence>)
  slot contents :: <list>, required-init-keyword: contents:;
end;

define method make (class == <weak-list>, #key contents = $not-supplied,
		    size = $not-supplied, fill)
  if (contents == $not-supplied)
    next-method(class,
		contents: if (size == $not-supplied)
			    #();
			  else
			    make(<list>, size: size,
				 fill: make(<weak-pointer>, object: fill));
			  end);
  elseif (size == $not-supplied)
    next-method(class,
		contents:
		  map-as(<list>, curry(make, <weak-pointer>, object:)));
  else
    error("Can't supply both size: and contents: to make of <weak-list>");
  end;
end;

define method add (weak-list :: <weak-list>, element :: <object>)
    => res :: <weak-list>;
  make(<weak-list>, contents: pair(element, as(<list>, weak-list)));
end;

define method add! (weak-list :: <weak-list>, element :: <object>)
    => res :: <weak-list>;
  weak-list.contents := add!(make(<weak-pointer>, object: element),
			     weak-list.contents);
  weak-list;
end;

define method remove (weak-list :: <weak-list>, element :: <object>,
		      #key test = \==, count = $not-supplied)
  make(<weak-list>,
       contents: if (count == $not-supplied)
		   remove(as(<list>, weak-list), element, test: test);
		 else
		   remove(as(<list>, weak-list), element,
			  test: test, count: count);
		 end);
end;

define method remove! (weak-list :: <weak-list>, element :: <object>,
		       #key test = \==, count = $not-supplied)
  local method new-test (weak-ptr, element)
	  let (object, broken?) = weak-pointer-object(weak-ptr);
	  // We can't remove the broken ones because doing so would mess up
	  // the count.
	  ~broken? & test(object, element);
	end;
  weak-list.contents
    := if (count == $not-supplied)
	 remove!(weak-list.contents, element, test: new-test);
       else
	 remove!(weak-list.contents, element, test: new-test, count: count);
       end;
  weak-list;
end;

define method reverse (weak-list :: <weak-list>)
    => res :: <weak-list>;
  make(<weak-list>, contents: reverse(as(<list>, weak-list.contents)));
end;

define method reverse! (weak-list :: <weak-list>)
  weak-list.contents := reverse!(weak-list.contents);
end;


define method forward-iteration-protocol (weak-list :: <weak-list>)
  let list = as(<list>, weak-list);
  values(list,
	 #(),
	 tail,
	 \==,
	 method (weak-list, 
	 head,
	 head-setter,
	 identity);
end;
