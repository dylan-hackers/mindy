Module: ansi-c-test

define method print-c-type (type :: <c-type>) => ()
  format(*standard-output*, "%s\n", format-c-type-declarator(type));
  force-output(*standard-output*);
end;

define method main(appname, #rest args)
  print-c-type($c-int-type);
  print-c-type(make(<c-pointer-type>, referent: $c-signed-long-type));
  print-c-type(make(<c-array-type>, referent: $c-char-type));
  print-c-type(make(<c-array-type>, referent: $c-char-type, size: 10));
  print-c-type(make(<c-pointer-type>,
		    referent: make(<c-array-type>,
				   referent: $c-char-type, size: 10)));
  print-c-type(make(<c-array-type>,
		    size: 10,
		    referent: make(<c-pointer-type>,
				   referent: $c-char-type)));

  let char* =
    make(<c-pointer-type>, referent: $c-char-type);
  let array-10-char* =
    make(<c-array-type>, referent: char*, size: 10);
  let array-10-char*-* =
    make(<c-pointer-type>, referent: array-10-char*);
  let func =
    make(<c-function-type>, return-type: array-10-char*-*, varargs?: #f);
  print-c-type(func);
  let func-ptr =
    make(<c-pointer-type>, referent: func);
  print-c-type(func-ptr);

  print-c-type($c-long-double-type);
  print-c-type($c-long-long-type);

  print-c-type(make(<c-struct-type>, tag: "foo"));
  print-c-type(make(<c-struct-type>));
  print-c-type(make(<c-union-type>, tag: "bar"));
  print-c-type(make(<c-union-type>));
  print-c-type(make(<c-enum-type>, tag: "baz"));
  print-c-type(make(<c-enum-type>));

  print-c-type(make(<c-typedef-type>,
		    name: "typedefed_type",
		    type: $c-int-type));
end;
