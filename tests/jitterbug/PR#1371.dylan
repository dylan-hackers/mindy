module: PR-1371
invoke-with: d2c --no-binaries PR#1371.dylan

define not-inline method foo(o) end;
define not-inline method foo(i :: <integer>)  end;
define not-inline method bar(n :: <number>) 0 end;
define not-inline method bar(i :: <integer>) 42 end;

define sealed domain foo(<number>);
define sealed domain bar(<number>);

define function baz(some-number :: <number>)
  foo(some-number);
  bar(some-number);
end;
