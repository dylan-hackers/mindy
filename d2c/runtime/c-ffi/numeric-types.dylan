Module:   c-ffi
Author:   Eric Kidd, Peter Housel
Synopsis: Primitive numeric types used by the FFI.

/*
  Take care of all the long, short, int and char types here.
*/
define designator-class <C-char> (<C-number>)
  options c-rep: #"char";
end designator-class;
