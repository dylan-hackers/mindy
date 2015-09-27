module: library-demo

puts("Hello, World.\n");
format("fact(5) = %=\n", fact(5));
format("fact(10) = %=\n", fact(10));
format("fact(30) = %=\n", fact(30));

// You can actually do everything as top-level side-effects, but this empty main
// keeps mindy from throwing you into the debugger.

define method main (foo, #rest stuff)
end method;
