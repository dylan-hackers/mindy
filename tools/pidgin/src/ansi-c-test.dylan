Module: ansi-c-test

//=========================================================================
//  Utility functions
//=========================================================================

define function test-section-header (name :: <string>) => ()
  format(*standard-output*, "----- %s -----\n", name);
  force-output(*standard-output*);
end;  

define function print-c-type (type :: <c-type>) => ()
  format(*standard-output*, "%s\n", format-c-type(type));
  force-output(*standard-output*);
end;

define function print-c-declaration (decl :: <c-declaration>) => ()
  format(*standard-output*, "%s\n", format-c-declaration(decl));
  force-output(*standard-output*);
end;

define function sv (#rest items) => (sv :: <stretchy-vector>)
  let result = make(<stretchy-vector>);
  for (item in items)
    add!(result, item);
  end;
  result;
end;

//=========================================================================
//  C Types & Declarations
//=========================================================================

define function test-c-types-and-declarations () => ()
  let r = make(<c-type-repository>);

  test-section-header("C Types");

  print-c-type($c-int-type);
  print-c-type(make(<c-pointer-type>, repository: r,
		    referent: $c-signed-long-type));
  print-c-type(make(<c-array-type>, repository: r,
		    referent: $c-char-type));
  print-c-type(make(<c-array-type>, repository: r,
		    referent: $c-char-type, size: 10));
  print-c-type(make(<c-pointer-type>, repository: r,
		    referent: make(<c-array-type>, repository: r,
				   referent: $c-char-type, size: 10)));
  print-c-type(make(<c-array-type>, repository: r,
		    size: 10,
		    referent: make(<c-pointer-type>, repository: r,
				   referent: $c-char-type)));

  let char* =
    make(<c-pointer-type>, repository: r, referent: $c-char-type);
  let array-10-char* =
    make(<c-array-type>, repository: r, referent: char*, size: 10);
  let array-10-char*-* =
    make(<c-pointer-type>, repository: r, referent: array-10-char*);
  let func =
    make(<c-function-type>, repository: r, return-type: array-10-char*-*);
  print-c-type(func);
  let func-ptr =
    make(<c-pointer-type>, repository: r, referent: func);
  print-c-type(func-ptr);

  print-c-type($c-long-double-type);
  print-c-type($c-long-long-type);

  print-c-type(make(<c-struct-type>, repository: r, tag: "foo"));
  print-c-type(make(<c-struct-type>, repository: r));
  print-c-type(make(<c-union-type>, repository: r, tag: "bar"));
  print-c-type(make(<c-union-type>, repository: r));
  print-c-type(make(<c-enum-type>, repository: r, tag: "baz", members: sv()));
  print-c-type(make(<c-enum-type>, repository: r, members: sv()));

  let typedef = make(<c-typedef-type>, repository: r,
		    name: "typedefed_type",
		    type: $c-int-type);
  print-c-type(typedef);

  print-c-type(make(<c-function-type>, repository: r,
		    return-type: $c-void-type,
		    explicit-void?: #t));
  print-c-type(make(<c-function-type>, repository: r,
		    return-type: $c-void-type,
		    explicit-varargs?: #t));

  local method make-argument-list ()
	  sv($c-int-type, $c-char-type, $c-long-long-type);
	end;
  let func2 = make(<c-function-type>, repository: r,
		   return-type: $c-void-type,
		   parameter-types: make-argument-list());
  print-c-type(func2);
  let func3 = make(<c-function-type>, repository: r,
		   return-type: $c-void-type,
		   parameter-types: make-argument-list(),
		   explicit-varargs?: #t);
  print-c-type(func3);

  // Display a struct with all its members.
  let int* = make(<c-pointer-type>, repository: r, referent: $c-int-type);
  let m1 = make(<c-member-variable>, name: "foo", type: int*);
  let m2 = make(<c-member-variable>, name: "bar", type: int*);
  let b1 = make(<c-bit-field>, name: "baz", sign: #"signed", width: 10);
  let b2 = make(<c-bit-field>, sign: #"unspecified", width: 2);
  let struct = make(<c-struct-type>, repository: r,
		    tag: "my_struct",
		    members: sv(m1, m2, b1, b2));
  format(*standard-output*, "%s\n", format-c-tagged-type(struct));
  force-output(*standard-output*);
  format(*standard-output*, "%s\n",
	 format-c-tagged-type(struct, multi-line?: #t));
  force-output(*standard-output*);

  // Display a union (and test c-type-members-setter & incomplete types)
  // XXX - What happens when we try to print an incomplete type?
  let union = make(<c-union-type>, repository: r, tag: "my_union");
  union.c-type-members := sv(m1, m2);
  format(*standard-output*, "%s\n", format-c-tagged-type(union));
  force-output(*standard-output*);

  // Display an enumeration
  let e1 = make(<c-enum-constant>, name: "quux", value: 2);
  let e2 = make(<c-enum-constant>, name: "quuux", value: 3);
  let enum = make(<c-enum-type>, repository: r,
		  tag: "my_enum",
		  members: sv(e1, e2));
  format(*standard-output*, "%s\n", format-c-tagged-type(enum));
  force-output(*standard-output*);
  format(*standard-output*, "%s\n",
	 format-c-tagged-type(enum, multi-line?: #t));
  force-output(*standard-output*);

  // Now print some complete declarations
  test-section-header("C Declarations");

  // Tagged type declarations
  print-c-declaration(make(<c-tagged-type-declaration>, type: struct));
  print-c-declaration(make(<c-tagged-type-declaration>, type: union));
  print-c-declaration(make(<c-tagged-type-declaration>, type: enum));

  // Typedef declarations
  print-c-declaration(make(<c-typedef-declaration>, type: typedef));

  // Variable declarations
  print-c-declaration(make(<c-variable-declaration>,
			   name: "v1", type: $c-int-type, extern?: #t));
  print-c-declaration(make(<c-variable-declaration>,
			   name: "v2", type: typedef, extern?: #f));
  print-c-declaration(make(<c-variable-declaration>,
			   name: "v3", type: enum));
/*
  print-c-declaration(make(<c-variable-declaration>,
			   name: "f2", type: func2));
  print-c-declaration(make(<c-variable-declaration>,
			   name: "f3", type: func3));
  print-c-declaration(make(<c-variable-declaration>,
			   name: "f", type: func));
  print-c-declaration(make(<c-variable-declaration>,
			   name: "fp", type: func-ptr));
*/

  // Defines
  print-c-declaration(make(<c-integer-define>, name: "d1", value: 3));
  print-c-declaration(make(<c-string-define>, name: "d2", value: "Hi!"));
  print-c-declaration(make(<c-type-alias-define>, name: "d3", type: struct));
  print-c-declaration(make(<c-unknown-define>, name: "d4"));

  // See what we've accumulated.
  test-section-header("Dump of type repository");

  local
    method dump-entry(type :: <c-type>) => ()
      format(*standard-output*, "  %s\n", format-c-type(type));
      force-output(*standard-output*);
    end;
  do-c-type-repository-entries(dump-entry, r);
  
end function test-c-types-and-declarations;


//=========================================================================
//  Test C parser
//  Inputs: type-rep, header name, header-file-finder object
//  Outputs: c-file object.
//=========================================================================

define function test-c-parser(args)
  test-section-header("C Parser");
  let r :: <c-type-repository> = make(<c-type-repository>);
  /* let c-f :: <c-file> = */ parse-c-file(r, args[0]);
  
  // Print out results (c-f & r) ...
end function test-c-parser;


//=========================================================================
//  Test program
//=========================================================================

define method main(appname, #rest args)
  test-c-types-and-declarations();
  if (~empty?(args))
    test-c-parser(args);
  end if;
end;
