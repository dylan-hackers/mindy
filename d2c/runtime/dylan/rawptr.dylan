rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/rawptr.dylan,v 1.7 1996/03/17 00:11:23 wlott Exp $
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
