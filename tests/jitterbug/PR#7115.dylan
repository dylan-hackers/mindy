module: PR-7115

define function foo(x, y :: <integer>)
  if (x == y)
    format-out("%=, %=\n", x, y);
  end
end;

foo(42, 42)

