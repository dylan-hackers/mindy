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

define function dump-type-repository
    (repository :: <c-type-repository>, #key indent? :: <boolean>)
 => ()
  let format-string = if (indent?) "  %s\n" else "%s\n" end;
  local
    method dump-entry(type :: <c-type>) => ()
      format(*standard-output*, format-string, format-c-type(type));
      force-output(*standard-output*);
    end;
  do-c-type-repository-entries(dump-entry, repository);
end function;


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

  // See what we've accumulated.
  test-section-header("Dump of type repository");
  dump-type-repository(r);
end function test-c-types-and-declarations;


//=========================================================================
//  Test C parser
//=========================================================================
//  XXX - Need to add support for some kind of "header finder object".

define generic print-item
    (item :: type-union(<c-file>, <c-declaration>))
 => ();

define method print-item (file :: <c-file>) => ()
  let name = file.c-file-name | "nameless header";
  format(*standard-output*, "/* >>> Entering %s >>> */\n", name);
  for (item in file.c-file-contents)
    print-item(item);
  end for;
  format(*standard-output*, "/* <<< Exiting  %s <<< */\n", name);
  force-output(*standard-output*);
end method print-item;

define method print-item (decl :: <c-declaration>) => ()
  format(*standard-output*, "%s\n",
	 format-c-declaration(decl, multi-line?: #t));
  force-output(*standard-output*);
end method print-item;

define function test-c-parser(args)
  test-section-header("C Parser");
  if (~empty?(args))
    let argp = make(<argument-list-parser>);
    add-option-parser-by-type(argp,
			      <repeated-parameter-option-parser>,
			      short-options: #("I"),
			      long-options: #("includedir"));
    unless (parse-arguments(argp, args))
      format(*standard-error*,
	     "usage: ansi-c-test [[-Iincludedir...] file]\n");
      exit(exit-code: 1);
    end unless;
    let extra-includes = option-value-by-long-name(argp, "includedir");

    format(*standard-output*, "Constructing include path.\n");
    force-output(*standard-output*);
    let include-path =
      make(<gcc-include-path>,
	   standard-include-directories:
	     $i386-linux-platform.c-platform-default-include-path,
	   extra-include-directories: extra-includes,
	   extra-user-include-directories: #());

    format(*standard-output*, "Running C parser.\n");
    let repository :: <c-type-repository> = make(<c-type-repository>);
    force-output(*standard-output*);
    let c-file :: <c-file> = parse-c-file(repository,
					  argp.regular-arguments[0],
					  include-path: include-path,
					  platform: $i386-linux-platform);
    force-output(*standard-output*);
    format(*standard-output*, "Parser finished.\n");
    force-output(*standard-output*);

    test-section-header("Contents of top-level file");
    print-item(c-file);
    test-section-header("Contents of type repository");
    dump-type-repository(repository);
  else
    format(*standard-output*, "No input supplied; skipping test.\n");
  end;
end function test-c-parser;


//=========================================================================
//  Test program
//=========================================================================

// Set up our I/O.
define class <better-debugger> (<debugger>)
end class <better-debugger>;
define method invoke-debugger
    (debugger :: <better-debugger>, condition :: <condition>)
 => res :: <never-returns>;
  //fresh-line(*warning-output*);
  condition-format(*warning-output*, "%s\n", condition);
  force-output(*warning-output*);
  call-out("abort", void:);
end method invoke-debugger;
*debugger* := make(<better-debugger>);
*warning-output* := *standard-output*;
*show-parse-progress?* := #t;

define method main(appname, #rest args)
  test-c-types-and-declarations();
  test-c-parser(args);
end;
