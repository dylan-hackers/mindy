rcs-header: $Header: /home/housel/work/rcs/gd/src/demos/library-demo/fact.dylan,v 1.1 1995/11/15 20:21:15 ram Exp $
module: fact

define method fact (x :: <integer>) => res :: <integer>;
  for (i :: <integer> from x to 2 by -1,
       result :: <integer> = 1 then result * i)
  finally
    result;
  end;
end;
