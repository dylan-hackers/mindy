module: PR-1373
invoke-with: d2c --no-binaries PR#1373.dylan

define not-inline method foo(o) end;
define not-inline method foo(i :: <integer>)  end;

define sealed domain foo(<number>);

define function baz(some-number :: <number>)
  foo(some-number);
end;
