module: dylan

define functional class <raw-pointer> (<object>)
  slot value :: <raw-pointer>, required-init-keyword: value:;
end;

define sealed method make (class == <raw-pointer>, #key) => res :: type-or();
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
    (class == <raw-pointer>, address :: <fixed-integer>)
    => res :: <raw-pointer>;
  %%primitive make-raw-pointer (address);
end;

define sealed inline method as
    (class :: one-of(<integer>, <fixed-integer>), ptr :: <raw-pointer>)
    => res :: <fixed-integer>;
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

define sealed inline method \+ (ptr :: <raw-pointer>, bytes :: <fixed-integer>)
    => res :: <raw-pointer>;
  %%primitive pointer-+ (ptr, bytes);
end;

define sealed inline method \+ (bytes :: <fixed-integer>, ptr :: <raw-pointer>)
    => res :: <raw-pointer>;
  %%primitive pointer-+ (ptr, bytes);
end;

define sealed inline method \- (ptr1 :: <raw-pointer>, ptr2 :: <raw-pointer>)
    => res :: <fixed-integer>;
  %%primitive pointer-- (ptr1, ptr2);
end;
