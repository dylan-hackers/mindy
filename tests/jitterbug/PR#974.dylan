module: bug

define function main(name, arguments)
  local
    method f (n, x, y)
      format-out("in f(%d, %d, %d)\n", n, x, y);
      if (n > 0) f(n - 1, y, x) else x end ;
    end method;
  format-out("f(0, 1, 2) = %d\n", f(0, 1, 2));
  format-out("f(1, 1, 2) = %d\n", f(1, 1, 2));
  format-out("f(2, 1, 2) = %d\n", f(2, 1, 2));
  format-out("test = %=\n", f(0, 1, 2) = 1 & f(1, 1, 2) = 2 & f(2, 1, 2) = 1);
  exit-application(0);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
