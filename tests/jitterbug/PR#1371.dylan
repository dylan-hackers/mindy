module: PR-1371
invoke-with: d2c --no-binaries PR#1371.dylan

define not-inline method foo(o, b == #t) end;
define not-inline method foo(i :: <integer>, b == #t)  end;
define not-inline method bar(n :: <number>) 0 end;
define not-inline method bar(i :: <boolean>) 42 end;

define sealed domain foo(<number>, <boolean>);
define sealed domain bar(<object>);

define function baz(some-number :: <number>, other)
  foo(some-number, other);
  bar(other);
end;
