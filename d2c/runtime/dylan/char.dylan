rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/char.dylan,v 1.5 1996/01/12 02:10:42 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

define constant $max-char-code = #xffff;

define constant <char-code>
  = limited(<integer>, min: 0, max: $max-char-code);

define functional class <character> (<object>)
  slot value :: <char-code>, required-init-keyword: code:;
end;

seal generic make (singleton(<character>));
seal generic initialize (<character>);

// The fact that we are a functional class should automatically define this,
// but it doesn't yet.
//
define sealed inline method functional-== (a :: <character>, b :: <character>)
    => res :: <boolean>;
  a.value == b.value;
end;

seal generic functional-== (<character>, <object>);
seal generic functional-== (<object>, <character>);

define sealed inline method as (class == <character>, code :: <integer>)
    => res :: <character>;
  make(<character>, code: code);
end;

define sealed inline method as (class == <integer>, char :: <character>)
    => res :: <integer>;
  char.value;
end;

define sealed inline method \< (char1 :: <character>, char2 :: <character>)
    => res :: <boolean>;
  char1.value < char2.value;
end;

seal generic \<= (<character>, <character>);
seal generic \= (<character>, <character>);
seal generic \~= (<character>, <character>);

define sealed method as-uppercase (char :: <character>)
    => res :: <character>;
  if ('a' <= char & char <= 'z')
    as(<character>, as(<integer>, char) - 32);
  else
    char;
  end;
end;

define sealed method as-lowercase (char :: <character>)
    => res :: <character>;
  if ('A' <= char & char <= 'Z')
    as(<character>, as(<integer>, char) + 32);
  else
    char;
  end;
end;

