module: PR-63

define generic foo(a, b);

define method foo(a :: <number>, b)
end;

define method foo(a :: <integer>, b, #next next-method)
  next-method();
  next-method(a);
end;
