module: PR-1372
invoke-with: d2c --no-binaries PR#1372.dylan

define not-inline method foo(i :: <integer>)  end;

define sealed domain foo(<number>);

define function baz(some-object)
  foo(some-object);
end;
