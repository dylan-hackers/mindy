module: PR-26

// both functions should be tail-recursive, but only the second is

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


