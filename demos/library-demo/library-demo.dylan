rcs-header: $Header: /home/housel/work/rcs/gd/src/demos/library-demo/library-demo.dylan,v 1.3 1995/11/14 14:39:44 wlott Exp $
module: dylan-user

write("Hello, World.\n");

define method fact (x :: <integer>) => res :: <integer>;
  for (i :: <integer> from x to 2 by -1,
       result :: <integer> = 1 then result * i)
  finally
    result;
  end;
end;

format("fact(5) = %=\n", fact(5));
format("fact(10) = %=\n", fact(10));
