rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/main.dylan,v 1.1 1997/05/09 23:40:38 ram Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera


// The "main" facility for parsing argv a la Mindy.  Users should set
// extensions:%main as the entry point in their LID file and it will call
// extensions:main.
//
define open generic main (argv0, #rest more-args);
//
define method %main (argc :: <integer>, argv :: <raw-pointer>) => ();
  let args = make(<vector>, size: argc);
  for (index :: <integer> from 0 below argc)
    let argptr = pointer-deref(#"ptr", argv,
			       index * c-expr(#"int", "sizeof(void *)"));
    args[index] := import-string(argptr);
  end for;
  apply(main, args);
end method %main;
