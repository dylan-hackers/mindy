module: dylan

define constant $max-char-code = #xffff;

define constant <char-code>
  = limited(<fixed-integer>, min: 0, max: $max-char-code);

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

define sealed inline method as (class == <character>, code :: <char-code>)
    => res :: <character>;
  make(<character>, code: code);
end;

define sealed inline method as (class :: one-of(<integer>, <fixed-integer>),
				char :: <character>)
    => res :: <fixed-integer>;
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
