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

//=========================================================================
//  C Types & Declarations
//=========================================================================

define function test-c-types-and-declarations () => ()
  test-section-header("C Types");

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

  let typedef = make(<c-typedef-type>,
		    name: "typedefed_type",
		    type: $c-int-type);
  print-c-type(typedef);

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
  print-c-declaration(make(<c-variable-declaration>,
			   name: "f2", type: func2));
  print-c-declaration(make(<c-variable-declaration>,
			   name: "f3", type: func3));
  print-c-declaration(make(<c-variable-declaration>,
			   name: "f", type: func));
  print-c-declaration(make(<c-variable-declaration>,
			   name: "fp", type: func-ptr));

  // Defines
  print-c-declaration(make(<c-integer-define>, name: "d1", value: 3));
  print-c-declaration(make(<c-string-define>, name: "d2", value: "Hi!"));
  print-c-declaration(make(<c-type-alias-define>, name: "d3", type: struct));
  print-c-declaration(make(<c-unknown-define>, name: "d4"));
end function test-c-types-and-declarations;

//=========================================================================
//  C Type Repositories
//=========================================================================

define function test-c-type-repositories () => ()
  test-section-header("C Type Repository");

  let r = make(<c-type-repository>);
  local
    method add-type(type :: <c-type>) => (type :: <c-type>)
      format(*standard-output*, "Checking repository for '%s'... ",
	     format-c-type(type));
      force-output(*standard-output*);
      let canonical = find-canonical-c-type(r, type);
      format(*standard-output*,
	     if (type == canonical)
	       "already canonical.\n"
	     else
	       "found.\n"
	     end);
      force-output(*standard-output*);
      canonical;
    end,
    method add-pointer-to-type(type :: <c-type>) => (type :: <c-type>)
      format(*standard-output*, "Finding pointer to '%s'.\n",
	     format-c-type(type));
      force-output(*standard-output*);
      find-canonical-pointer-to-c-type(r, type);
    end,
    method dump-entry(type :: <c-type>) => ()
      format(*standard-output*, "  %s\n", format-c-type(type));
      force-output(*standard-output*);
    end;

  add-type($c-char-type);
  add-type($c-int-type);
  add-type($c-void-type);

  add-pointer-to-type($c-char-type);
  let t1 = add-type(make(<c-pointer-type>, referent: $c-char-type));
  let t2 = add-type(make(<c-pointer-type>, referent: $c-int-type));

  add-type(make(<c-typedef-type>, name: "foo", type: $c-int-type));
  add-type(make(<c-typedef-type>, name: "foo", type: $c-int-type));
  add-type(make(<c-typedef-type>, name: "bar", type: $c-int-type));

  let t3 = add-type(make(<c-struct-type>, tag: "baz"));
  add-type(make(<c-struct-type>, tag: "baz"));
  let t4 = add-type(make(<c-union-type>, tag: "baz"));
  let t5 = add-type(make(<c-enum-type>, tag: "baz"));

  let t6 = add-type(make(<c-array-type>, referent: t1, size: 10));  
  let t7 = add-type(make(<c-array-type>, referent: t1));  
  add-type(make(<c-array-type>, referent: t1, size: 10));  

  format(*standard-output*, "Dumping type repository:\n");
  do-c-type-repository-entries(dump-entry, r);
end function test-c-type-repositories;


//=========================================================================
//  Test program
//=========================================================================

define method main(appname, #rest args)
  test-c-types-and-declarations();
  test-c-type-repositories();
end;
