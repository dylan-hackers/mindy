module: bug

define variable junk = #();

define method foo(fn :: <function>)
  let (#rest vals) = fn();
  junk := vals;
  apply(values, vals);
end;
