module: define-libraries-and-modules
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/convert/deflibmod.dylan,v 1.10 1996/01/14 02:06:00 wlott Exp $
copyright: Copyright (c) 1994, 1995  Carnegie Mellon University
	   All rights reserved.



// Define library.

define class <define-library-tlf> (<top-level-form>)
  slot define-library-name :: <symbol>, required-init-keyword: name:;
  slot define-library-uses :: <simple-object-vector>,
    required-init-keyword: uses:;
  slot define-library-exports :: <simple-object-vector>,
    required-init-keyword: exports:;
end;

define method print-message
    (tlf :: <define-library-tlf>, stream :: <stream>) => ();
  format(stream, "Define Library %s.", tlf.define-library-name);
end;

define method process-top-level-form (form :: <define-library-parse>) => ();
  let (uses, exports, creates) = extract-clauses(form.deflibrary-clauses);
  unless (empty?(creates))
    error("How did any creates get into a define library?");
  end;
  let name = form.deflibrary-name.token-symbol;
  note-library-definition(name, uses, exports);
  add!(*Top-Level-Forms*,
       make(<define-library-tlf>, name: name, uses: uses, exports: exports));
end;

define method finalize-top-level-form (tlf :: <define-library-tlf>) => ();
  // Nothing to do.
end;

define method convert-top-level-form
    (builder :: <fer-builder>, tlf :: <define-library-tlf>) => ();
  // Nothing to do.
end;

define method dump-od (tlf :: <define-library-tlf>, state :: <dump-state>)
    => ();
  dump-simple-object(#"define-library-tlf", state, tlf.define-library-name,
		     tlf.define-library-uses, tlf.define-library-exports);
end;

add-od-loader(*compiler-dispatcher*, #"define-library-tlf",
	      method (state :: <load-state>) => res :: <symbol>;
		let name = load-object-dispatch(state);
		let uses = load-object-dispatch(state);
		let exports = load-object-dispatch(state);
		assert-end-object(state);
		note-library-definition(name, uses, exports);
		name;
	      end);


// Define module.

define class <define-module-tlf> (<top-level-form>)
  slot define-module-name :: <symbol>,
    required-init-keyword: name:;
  slot define-module-module :: false-or(<module>),
    init-value: #f, init-keyword: module:;
  slot define-module-uses :: <simple-object-vector>,
    required-init-keyword: uses:;
  slot define-module-exports :: <simple-object-vector>,
    required-init-keyword: exports:;
  slot define-module-creates :: <simple-object-vector>,
    required-init-keyword: creates:;
end;

define method print-message
    (tlf :: <define-module-tlf>, stream :: <stream>) => ();
  format(stream, "Define Module %s.", tlf.define-module-name);
end;

define method process-top-level-form (form :: <define-module-parse>) => ();
  let name = form.defmodule-name.token-symbol;
  let (uses, exports, creates) = extract-clauses(form.defmodule-clauses);
  note-module-definition(*Current-Library*, name, uses, exports, creates);
  add!(*Top-Level-Forms*,
       make(<define-module-tlf>,
	    name: name, module: find-module(*Current-Library*, name),
	    uses: uses, exports: exports, creates: creates));
end;

define method finalize-top-level-form (tlf :: <define-module-tlf>) => ();
  let mod = tlf.define-module-module;
  for (name in tlf.define-module-exports)
    let var = find-variable(make(<basic-name>, symbol: name, module: mod));
    unless (var & var.variable-definition)
      compiler-warning("%s in %s is exported but never defined.", name, mod);
    end;
  end;
  for (name in tlf.define-module-creates)
    let var = find-variable(make(<basic-name>, symbol: name, module: mod));
    unless (var & var.variable-definition)
      compiler-warning("%s in %s is created but never defined.", name, mod);
    end;
  end;
end;

define method convert-top-level-form
    (builder :: <fer-builder>, tlf :: <define-module-tlf>) => ();
  // Nothing to do.
end;

define method dump-od (tlf :: <define-module-tlf>, state :: <dump-state>)
    => ();
  dump-simple-object(#"define-module-tlf", state, tlf.define-module-name,
		     tlf.define-module-uses, tlf.define-module-exports,
		     tlf.define-module-creates);
end;

add-od-loader(*compiler-dispatcher*, #"define-module-tlf",
	      method (state :: <load-state>) => res :: <symbol>;
		let name :: <symbol> = load-object-dispatch(state);
		let uses :: <simple-object-vector>
		  = load-object-dispatch(state);
		let exports :: <simple-object-vector>
		  = load-object-dispatch(state);
		let creates :: <simple-object-vector>
		  = load-object-dispatch(state);
		assert-end-object(state);
		note-module-definition(*Current-Library*, name, uses, exports,
				       creates);
		name;
	      end);


// Clause processing utilities.

define method extract-clauses (clauses :: <simple-object-vector>)
    => (uses :: <vector>, exports :: <vector>, creates :: <vector>);
  let uses = make(<stretchy-vector>);
  let exports = make(<stretchy-vector>);
  let creates = make(<stretchy-vector>);
  for (clause in clauses)
    process-clause(clause, uses, exports, creates);
  end;
  values(as(<simple-object-vector>, uses),
	 as(<simple-object-vector>, exports),
	 as(<simple-object-vector>, creates));
end;

define method process-clause
    (clause :: <use-clause>, uses :: <stretchy-vector>,
     exports :: <stretchy-vector>, creates :: <stretchy-vector>)
    => ();
  let name = clause.use-name.token-symbol;
  let imports = extract-names-or-all(clause.use-import, "import");
  let prefix = extract-prefix(clause.use-prefix);
  let excludes = extract-names(clause.use-exclude, "exclude");
  let (orig-names, new-names) = extract-renamings(clause.use-rename);
  unless (imports == #t)
    imports := union(imports, orig-names);
  end unless;
  let renamings = map(method (orig :: <symbol>, new :: <symbol>)
			  => renaming :: <renaming>;
			make(<renaming>, orig-name: orig, new-name: new);
		      end method,
		      orig-names, new-names);
  let exports = extract-names-or-all(clause.use-export, "export");
  add!(uses,
       make(<use>, name: name, imports: imports, prefix: prefix,
	    excludes: excludes, renamings: renamings, exports: exports));
end;

define method extract-names (set :: <property-set>, where :: <string>)
    => names :: <simple-object-vector>;
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
    => names :: <simple-object-vector>;
  error("Bogus thing in %s clause: %=", where, expr);
end;

define method extract-names-or-all (set :: <property-set>, where :: <string>)
    => names :: type-union(<simple-object-vector>, <true>);
  extract-names(set, where);
end;

define method extract-names-or-all (expr :: <expression>, where :: <string>)
    => names :: type-union(<simple-object-vector>, <true>);
  error("Bogus thing in %s clause: %=", where, expr);
end;

define method extract-names-or-all (expr :: <varref>, where :: <string>)
    => names :: type-union(<simple-object-vector>, <true>);
  if (expr.varref-id.token-symbol == #"all")
    #t;
  else
    error("Bogus thing in %s clause: %=", where, expr);
  end;
end;

define method extract-prefix (expr :: <literal-ref>)
    => prefix :: false-or(<byte-string>);
  let literal = expr.litref-literal;
  select (literal by instance?)
    <literal-false> => #f;
    <literal-string> => literal.literal-value;
    otherwise =>
      error("Bogus thing in prefix clause: %=", expr);
  end;
end;

define method extract-prefix
    (whatever :: type-union(<property-set>, <expression>))
    => prefix :: false-or(<byte-string>);
  error("Bogus thing in prefix clause: %=", whatever);
end;

define method extract-renamings (set :: <property-set>)
    => (orig-names :: <simple-object-vector>,
	new-names :: <simple-object-vector>);
  let orig-names = make(<stretchy-vector>);
  let new-names = make(<stretchy-vector>);
  for (member in set.property-set-members)
    select (member by instance?)
      <pair> =>
	add!(orig-names, member.head.token-symbol);
	add!(new-names, member.tail.token-symbol);
      <word-token> =>
	error("Bogus thing in rename clause: %=", member);
    end;
  end;
  values(as(<simple-object-vector>, orig-names),
	 as(<simple-object-vector>, new-names));
end;

define method extract-renames (expr :: <expression>)
    => renamings :: <simple-object-vector>;
  error("Bogus thing in rename clause: %=", expr);
end;

define method process-clause
    (clause :: <export-clause>, uses :: <stretchy-vector>,
     exports :: <stretchy-vector>, creates :: <stretchy-vector>)
    => ();
  for (token in clause.export-names)
    let name = token.token-symbol;
    unless (member?(name, exports))
      add!(exports, name);
    end;
  end;
end;

define method process-clause
    (clause :: <create-clause>, uses :: <stretchy-vector>,
     exports :: <stretchy-vector>, creates :: <stretchy-vector>)
    => ();
  for (token in clause.create-names)
    let name = token.token-symbol;
    unless (member?(name, creates))
      add!(creates, name);
    end;
  end;
end;

