module: PR-100


define open generic countdown(n :: <integer>) => (val :: <integer>);


define method countdown(n :: <integer>) => (val :: <integer>)
  if (n = 0)
    1
  else
    countdown(n - 1)
  end if;
end method countdown;
