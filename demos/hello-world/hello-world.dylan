module: Hello-World
rcs-header: $Header: /home/housel/work/rcs/gd/src/demos/hello-world/hello-world.dylan,v 1.6 1997/05/31 01:20:15 ram Exp $

// This is the canonical ``hello, world'' demo.
//
// Gwydion Dylan invokes the generic function main with the command line
// arguments as strings.  Given that we don't care what they are, we just
// ignore them.
//
// Note that even something this simplistic must have its own module and a
// seperate hello-world-exports.dylan file to set up the library and module.
//
define method main (argv0 :: <byte-string>, #rest noise)
  puts("Hello, World.\n");
end;
