Module:   c-ffi
Author:   Eric Kidd, Peter Housel
Synopsis: Primitive numeric types used by the FFI.

/*
  Take care of all the long, short, int and char types here.
*/

define designator-class <C-raw-int> (<C-number>)
  options c-rep: #"int",
          import-type: <machine-word>,
          export-type: <machine-word>,
          pointer-type-name: <C-raw-int*>;
end designator-class;

define designator-class <C-int> (<C-raw-int>)
  options import-type: <integer>,
          import-function: method(value :: <machine-word>)
                              => (value :: <integer>);
                             as(<integer>, value);
                           end,
          export-type: <integer>,
          export-function: method(value :: <integer>)
                              => (value :: <machine-word>);
                             as(<machine-word>, value);
                           end,
          pointer-type-name: <C-int*>;
end designator-class;

define designator-class <C-unsafe-int> (<C-raw-int>)
  options import-type: <integer>,
          import-function: method(value :: <machine-word>)
                              => (value :: <integer>);
                             as(<integer>, value);
                           end,
          export-type: <integer>,
          export-function: method(value :: <integer>)
                              => (value :: <machine-word>);
                             as(<machine-word>, value);
                           end,
          pointer-type-name: <C-unsafe-int*>;
end designator-class;

