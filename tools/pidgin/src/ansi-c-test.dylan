Module: ansi-c-test

define method print-c-type (type :: <c-type>) => ()
  format(*standard-output*, "%s\n", format-c-type(type));
  force-output(*standard-output*);
end;

define method main(appname, #rest args)

  format(*standard-output*, "----- C Types -----\n");

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
    make(<c-function-type>, return-type: array-10-char*-*);
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


  print-c-type(make(<c-function-type>,
		    return-type: $c-void-type,
		    explicit-void?: #t));
  print-c-type(make(<c-function-type>,
		    return-type: $c-void-type,
		    explicit-varargs?: #t));

  local method build-argument-list (f :: <c-function-type>)
	  add!(f.c-function-parameter-types, $c-int-type);
	  add!(f.c-function-parameter-types, $c-char-type);
	  add!(f.c-function-parameter-types, $c-long-long-type);
	end;
  let func2 = make(<c-function-type>,
		   return-type: $c-void-type);
  build-argument-list(func2);
  print-c-type(func2);
  let func3 = make(<c-function-type>,
		   return-type: $c-void-type,
		   explicit-varargs?: #t);
  build-argument-list(func3);
  print-c-type(func3);

  // Display a struct with all its members.
  let int* = make(<c-pointer-type>, referent: $c-int-type);
  let m1 = make(<c-member-variable>, name: "foo", type: int*);
  let m2 = make(<c-member-variable>, name: "bar", type: int*);
  let b1 = make(<c-bit-field>, name: "baz", sign: #"signed", width: 10);
  let b2 = make(<c-bit-field>, name: #f, sign: #"unspecified", width: 2);
  let struct = make(<c-struct-type>, tag: "my_struct");
  add!(struct.c-type-members, m1);
  add!(struct.c-type-members, m2);
  add!(struct.c-type-members, b1);
  add!(struct.c-type-members, b2);
  format(*standard-output*, "%s\n", format-c-tagged-type(struct));
  force-output(*standard-output*);
  format(*standard-output*, "%s\n",
	 format-c-tagged-type(struct, multi-line?: #t));
  force-output(*standard-output*);

  // Display a union
  let union = make(<c-union-type>, tag: "my_union");
  add!(union.c-type-members, m1);
  add!(union.c-type-members, m2);
  format(*standard-output*, "%s\n", format-c-tagged-type(union));
  force-output(*standard-output*);

  // Display an enumeration
  let e1 = make(<c-enum-constant>, name: "quux", value: 2);
  let e2 = make(<c-enum-constant>, name: "quuux", value: 3);
  let enum = make(<c-enum-type>, tag: "my_enum");
  add!(enum.c-enum-members, e1);
  add!(enum.c-enum-members, e2);
  format(*standard-output*, "%s\n", format-c-tagged-type(enum));
  force-output(*standard-output*);
  format(*standard-output*, "%s\n",
	 format-c-tagged-type(enum, multi-line?: #t));
  force-output(*standard-output*);

  // Now print some complete declarations
  // format(*standard-output*, "----- C Type Declarations -----\n");
end;
