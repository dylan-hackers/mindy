module: test

define library test
  use dylan;
  use tk;
end library test;

define module test
  use dylan;
  use extensions;
  use tk;
end module test;

define method main (program-name :: <string>, #rest args);
  let b1 = make(<button>, text: "Okay", relief: "raised",
		command: curry(destroy-window, *root-window*),
		expand: #t, in: *root-window*);
  make(<message>, text: "Hello, world!", aspect: 500,
       side: "top", before: b1);
  map-window(*root-window*);
end method main;
