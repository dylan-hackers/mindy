rcs-header: $Header: /home/housel/work/rcs/gd/src/demos/stream-demo/stream-demo.dylan,v 1.1 1995/12/06 19:56:57 ram Exp $
module: test

define method main ()
  write("Hello, World.\n", *standard-output*);
end method;

/*
format("fact(5) = %=\n", fact(5));
format("fact(10) = %=\n", fact(10));
format("fact(30) = %=\n", fact(30));
*/

main();
