module: bootstrap
rcs-header: $Header: /scm/cvs/src/d2c/compiler/main/Attic/bootstrap-exports.dylan,v 1.1 1998/05/03 19:55:33 andreas Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// All rights reserved.
// 
// Use and copying of this software and preparation of derivative
// works based on this software are permitted, including commercial
// use, provided that the following conditions are observed:
// 
// 1. This copyright notice must be retained in full on any copies
//    and on appropriate parts of any derivative works.
// 2. Documentation (paper or online) accompanying any system that
//    incorporates this software, or any part of it, must acknowledge
//    the contribution of the Gwydion Project at Carnegie Mellon
//    University.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports, questions, comments, and suggestions should be sent by
// E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
//
//======================================================================

// The first thing we need to do is define ``define module''.
//
define macro module-definer
    { define module ?:name ?clauses end }
      => make-define-module({ ?name }, { ?clauses })

  clauses:
    { } => { }
    { ?clause; ... } => { ?clause, ... }

  clause:
    {use ?:name, #key ?import = all, ?exclude = {}, ?prefix:token = "", 
		      ?rename = {}, ?export = {} }
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
    { { ?names } } => { ?names }

  rename:
    { { ?renamings } } => { ?renamings }

  renamings:
    { } => { }
    { ?renaming, ... } => { ?renaming, ... }

  renaming:
    { ?from:name => ?to:name } => make-renaming({ ?from }, { ?to })

  export:
    { all } => { #t }
    { { ?names } } => { ?names }

end;

// Then we can use it to define the dylan-viscera module.
//
define module dylan-viscera
  use bootstrap;
end module dylan-viscera;
