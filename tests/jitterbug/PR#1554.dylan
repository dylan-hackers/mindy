module: PR-1554

define variable (cond, use) = values();

begin
  let a = #f;

  if (cond)
    a := 0;
  else
    a := 1;
  end if;

  use(a);
end;
