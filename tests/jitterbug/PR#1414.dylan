module: PR-1414

define generic foo(i :: limited(<integer>, min: 0, max: 63))
=> res :: <boolean>;

define method foo(i :: limited(<integer>, min: 0, max: 63))
=> res :: <boolean>;
end;

define method foo(i == 18) 
=> res :: <boolean>;
  #t
end;

begin
  for (i from 0 below 64)
    format-out("%d: it is: %=\n", i, foo(i));
  end for;
end;
