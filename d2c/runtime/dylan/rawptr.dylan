rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/rawptr.dylan,v 1.4 2003/07/11 03:20:04 housel Exp $
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

define functional class <raw-pointer> (<object>)
  slot value :: <raw-pointer>, required-init-keyword: value:;
end;

define sealed method make (class == <raw-pointer>, #key)
    => res :: <never-returns>;
  error("Can't make instances of <raw-pointer>, they just are.");
end;

define sealed inline method as
    (class == <raw-pointer>, address :: <integer>)
    => res :: <raw-pointer>;
  %%primitive(make-raw-pointer, address);
end;

define sealed inline method as (class == <integer>, ptr :: <raw-pointer>)
    => res :: <integer>;
  %%primitive(raw-pointer-address, ptr);
end;

define inline method \== (ptr1 :: <raw-pointer>, ptr2 :: <raw-pointer>)
    => res :: <boolean>;
  %%primitive(pointer-=, ptr1, ptr2);
end;

define inline method \== (a :: <raw-pointer>, b :: <object>)
    => res :: <boolean>;
  #f;
end;

define sealed domain \= (<raw-pointer>, <object>);
define sealed domain \= (<object>, <raw-pointer>);

define inline method \< (ptr1 :: <raw-pointer>, ptr2 :: <raw-pointer>)
    => res :: <boolean>;
  %%primitive(pointer-<, ptr1, ptr2);
end;

define sealed domain \< (<raw-pointer>, <object>);
define sealed domain \< (<object>, <raw-pointer>);

define sealed inline method \+ (ptr :: <raw-pointer>, bytes :: <integer>)
    => res :: <raw-pointer>;
  %%primitive(pointer-+, ptr, bytes);
end;

define sealed inline method \+ (bytes :: <integer>, ptr :: <raw-pointer>)
    => res :: <raw-pointer>;
  %%primitive(pointer-+, ptr, bytes);
end;

define sealed inline method \- (ptr1 :: <raw-pointer>, ptr2 :: <raw-pointer>)
    => res :: <integer>;
  %%primitive(pointer--, ptr1, ptr2);
end;


define macro pointer-deref
    { pointer-deref (?kind:expression, ?ptr:expression, ?offset:expression) }
      => { %%primitive(\pointer-deref, ?kind, ?ptr, ?offset) }
end;

define macro pointer-deref-setter
    { pointer-deref-setter (?new-val:expression, ?kind:expression,
			    ?ptr:expression, ?offset:expression) }
      => { %%primitive(\pointer-deref-setter, ?new-val, ?kind, ?ptr, ?offset) }
end;

define sealed inline-only method vector-elements-address
    (vec :: <vector>)
 => (res :: <raw-pointer>);
  %%primitive(vector-elements, vec);
end method;
