module: define-libraries-and-modules
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/convert/deflibmod.dylan,v 1.4 1995/03/23 22:05:58 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


// define library and define module

define method process-top-level-form (form :: <define-library-parse>) => ();
  let (uses, exports, creates) = extract-clauses(form.deflibrary-clauses);
  unless (empty?(creates))
    error("How did any creates get into a define library?");
  end;
  note-library-definition(form.deflibrary-name.token-symbol, uses, exports);
end;

define method process-top-level-form (form :: <define-module-parse>) => ();
  let (uses, exports, creates) = extract-clauses(form.defmodule-clauses);
  note-module-definition(*Current-Library*, form.defmodule-name.token-symbol,
			 uses, exports, creates);
end;

define method extract-clauses (clauses :: <simple-object-vector>)
    => (uses :: <vector>, exports :: <vector>, creates :: <vector>);
  let uses = make(<stretchy-vector>);
  let exports = make(<stretchy-vector>);
  let creates = make(<stretchy-vector>);
  for (clause in clauses)
    process-clause(clause, uses, exports, creates);
  end;
  values(uses, exports, creates);
end;

define method process-clause (clause :: <use-clause>,
			      uses :: <stretchy-vector>,
			      exports :: <stretchy-vector>,
			      creates :: <stretchy-vector>)
    => ();
  add!(uses,
       make(<use>,
	    name: clause.use-name.token-symbol,
	    imports: extract-names-or-all(clause.use-import, "import"),
	    prefix: extract-prefix(clause.use-prefix),
	    excludes: extract-names(clause.use-exclude, "exclude"),
	    renamings: extract-renamings(clause.use-rename),
	    exports: extract-names-or-all(clause.use-export, "export")));
end;

define method extract-names (set :: <property-set>, where :: <string>)
  let names = make(<stretchy-vector>);
  for (member in set.property-set-members)
    select (member by instance?)
      <word-token> => add!(names, member.token-symbol);
      <pair> => error("Bogus thing in %s clause: %=", where, member);
    end;
  end;
  as(<simple-object-vector>, names);
end;

define method extract-names (expr :: <expression>, where :: <string>)
  error("Bogus thing in %s clause: %=", where, expr);
end;

define method extract-names-or-all (set :: <property-set>, where :: <string>)
  extract-names(set, where);
end;

define method extract-names-or-all (expr :: <expression>, where :: <string>)
  error("Bogus thing in %s clause: %=", where, expr);
end;

define method extract-names-or-all (expr :: <varref>, where :: <string>)
  if (expr.varref-id.token-symbol == #"all")
    #t;
  else
    error("Bogus thing in %s clause: %=", where, expr);
  end;
end;

define method extract-prefix (expr :: <literal-ref>)
  let literal = expr.litref-literal;
  select (literal by instance?)
    <literal-false> => #f;
    <literal-string> => literal.literal-value;
    otherwise =>
      error("Bogus thing in prefix clause: %=", expr);
  end;
end;

define method extract-prefix (whatever :: union(<property-set>, <expression>))
  error("Bogus thing in prefix clause: %=", whatever);
end;

define method extract-renamings (set :: <property-set>)
  let renamings = make(<stretchy-vector>);
  for (member in set.property-set-members)
    select (member by instance?)
      <pair> =>
	add!(renamings,
	     make(<renaming>,
		  orig-name: member.head.token-symbol,
		  new-name: member.tail.token-symbol));
      <word-token> =>
	error("Bogus thing in rename clause: %=", member);
    end;
  end;
  as(<simple-object-vector>, renamings);
end;

define method extract-renames (expr :: <expression>)
  error("Bogus thing in rename clause: %=", expr);
end;

define method process-clause (clause :: <export-clause>,
			      uses :: <stretchy-vector>,
			      exports :: <stretchy-vector>,
			      creates :: <stretchy-vector>)
    => ();
  for (token in clause.export-names)
    let name = token.token-symbol;
    unless (member?(name, exports))
      add!(exports, name);
    end;
  end;
end;

define method process-clause (clause :: <create-clause>,
			      uses :: <stretchy-vector>,
			      exports :: <stretchy-vector>,
			      creates :: <stretchy-vector>)
    => ();
  for (token in clause.create-names)
    let name = token.token-symbol;
    unless (member?(creates, name))
      add!(name, creates);
    end;
  end;
end;

