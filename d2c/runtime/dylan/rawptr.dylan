rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/rawptr.dylan,v 1.5 1996/01/12 02:10:52 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

define functional class <raw-pointer> (<object>)
  slot value :: <raw-pointer>, required-init-keyword: value:;
end;

define sealed method make (class == <raw-pointer>, #key)
    => res :: <never-returns>;
  error("Can't make instances of <raw-poniter>, they just are.");
end;

// The fact that we are a functional class should automatically define this,
// but it doesn't yet.
//
define sealed inline method functional-==
    (a :: <raw-pointer>, b :: <raw-pointer>)
    => res :: <boolean>;
  a.value == b.value;
end;

seal generic functional-== (<raw-pointer>, <object>);
seal generic functional-== (<object>, <raw-pointer>);

define sealed inline method as
    (class == <raw-pointer>, address :: <integer>)
    => res :: <raw-pointer>;
  %%primitive make-raw-pointer (address);
end;

define sealed inline method as (class == <integer>, ptr :: <raw-pointer>)
    => res :: <integer>;
  %%primitive raw-pointer-address (ptr);
end;

define inline method \== (ptr1 :: <raw-pointer>, ptr2 :: <raw-pointer>)
    => res :: <boolean>;
  %%primitive pointer-= (ptr1, ptr2);
end;

define inline method \== (a :: <raw-pointer>, b :: <object>)
    => res :: <boolean>;
  #f;
end;

/* Damn ambiguity rules.
define inline method \== (a :: <object>, b :: <raw-pointer>)
    => res :: <boolean>;
  #f;
end;
*/

seal generic \= (<raw-pointer>, <raw-pointer>);

define sealed inline method \< (ptr1 :: <raw-pointer>, ptr2 :: <raw-pointer>)
    => res :: <boolean>;
  %%primitive pointer-< (ptr1, ptr2);
end;

seal generic \~= (<raw-pointer>, <raw-pointer>);
seal generic \<= (<raw-pointer>, <raw-pointer>);

define sealed inline method \+ (ptr :: <raw-pointer>, bytes :: <integer>)
    => res :: <raw-pointer>;
  %%primitive pointer-+ (ptr, bytes);
end;

define sealed inline method \+ (bytes :: <integer>, ptr :: <raw-pointer>)
    => res :: <raw-pointer>;
  %%primitive pointer-+ (ptr, bytes);
end;

define sealed inline method \- (ptr1 :: <raw-pointer>, ptr2 :: <raw-pointer>)
    => res :: <integer>;
  %%primitive pointer-- (ptr1, ptr2);
end;


define macro pointer-deref
    // Should really match exactly two args, but the macro system is broken.
    { pointer-deref (?args) } => { %%primitive pointer-deref (?args) }
end;

define macro pointer-deref-setter
    // Should really match exactly three args, but the macro system is broken.
    { pointer-deref-setter (?args) }
      => { %%primitive pointer-deref-setter (?args) }
end;
