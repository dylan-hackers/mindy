module: PR-26

// all three functions should be tail-recursive, but only the last is


define method countdown3(n :: <integer>) => (val :: <integer>)
  if (n = 0)
    1
  else
    countdown3(n - 1)
  end if;
end method countdown3;


define function countdown2(n :: <integer>) => (val :: <integer>)
  if (n = 0)
    1
  else
    countdown2(n - 1)
  end if;
end function countdown2;


define function countdown(n :: <integer>) => (val :: <integer>)
  local method foo(n :: <integer>) => (val :: <integer>)
          if (n = 0)
            1
          else
            foo(n - 1)
          end if;
        end method foo;
  foo(n)
end function countdown;


