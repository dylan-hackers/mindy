module: test
reproduce-with: d2c --debug-optimizer --no-binaries PR#708.dylan

define method countdown(n :: <integer>) => (val :: <integer>)
  #("one", "two")[n]
end method countdown;
