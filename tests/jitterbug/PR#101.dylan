module: PR-101

define variable foo :: <integer> = 42;

define function bar()
  foo := bar;
end;
