module: bootstrap
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/main/bootstrap-exports.dylan,v 1.2 1996/03/17 00:50:44 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.

// The first thing we need to do is define ``define module''.
//
define macro module-definer
    { define module ?:name ?clauses end }
      => make-define-module({ ?name }, { ?clauses })

  clauses:
    { } => { }
    { ?clause; ... } => { ?clause, ... }

  clause:
    {use ?:name, #key ?import = all, ?exclude = none, ?prefix:token = "", 
		      ?rename = none, ?export = none }
      => make-use-clause({ ?name }, { ?import }, { ?exclude }, { ?prefix },
			 { ?rename }, { ?export })
    {export ?names }
      => make-export-clause({ ?names })
    {create ?names }
      => make-create-clause({ ?names })

  names:
    { } => { }
    { ?:name, ... } => { ?name, ... }

  import:
    { all } => { #t }
    { { ?variable-specs } } => { ?variable-specs }

  variable-specs:
    { } => { }
    { ?:name, ... } => { ?name, ... }
    { ?renaming, ... } => { ?renaming, ... }

  exclude:
    { none } => { }
    { { ?names } } => { ?names }

  rename:
    { none } => { }
    { { ?renamings } } => { ?renamings }

  renamings:
    { } => { }
    { ?renaming, ... } => { ?renaming, ... }

  renaming:
    { ?from:name => ?to:name } => make-renaming({ ?from }, { ?to })

  export:
    { none } => { }
    { all } => { #t }
    { { ?names } } => { ?names }

end;

// Then we can use it to define the dylan-viscera module.
//
define module dylan-viscera
  use bootstrap;
end module dylan-viscera;
