module: define-libraries-and-modules
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/convert/deflibmod.dylan,v 1.12 1996/03/20 22:32:20 rgs Exp $
copyright: Copyright (c) 1994, 1995  Carnegie Mellon University
	   All rights reserved.


// Define library.

define class <define-library-tlf> (<top-level-form>, <definition-parse>)
  //
  constant slot define-library-name :: <symbol>,
    required-init-keyword: name:;
  //
  constant slot define-library-uses :: <simple-object-vector>,
    required-init-keyword: uses:;
  //
  constant slot define-library-exports :: <simple-object-vector>,
    required-init-keyword: exports:;
end;

define method print-message
    (tlf :: <define-library-tlf>, stream :: <stream>) => ();
  format(stream, "Define Library %s.", tlf.define-library-name);
end;

define method process-top-level-form (form :: <define-library-tlf>) => ();
  note-library-definition(form.define-library-name, form.define-library-uses,
			  form.define-library-exports);
  add!(*Top-Level-Forms*, form);
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

define class <define-module-tlf> (<top-level-form>, <definition-parse>)
  //
  constant slot define-module-name :: <symbol>,
    required-init-keyword: name:;
  //
  slot define-module-module :: false-or(<module>) = #f,
    init-keyword: module:;
  //
  constant slot define-module-uses :: <simple-object-vector>,
    required-init-keyword: uses:;
  //
  constant slot define-module-exports :: <simple-object-vector>,
    required-init-keyword: exports:;
  //
  constant slot define-module-creates :: <simple-object-vector>,
    required-init-keyword: creates:;
end;

define method print-message
    (tlf :: <define-module-tlf>, stream :: <stream>) => ();
  format(stream, "Define Module %s.", tlf.define-module-name);
end;

define method process-top-level-form (form :: <define-module-tlf>) => ();
  let name = form.define-module-name;
  note-module-definition(*Current-Library*, name, form.define-module-uses,
			 form.define-module-exports,
			 form.define-module-creates);
  form.define-module-module := find-module(*Current-Library*, name);
  add!(*Top-Level-Forms*, form);
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
		note-module-definition
		  (*Current-Library*, name, uses, exports, creates);
		name;
	      end);


// Clause processing utilities.



define class <export-clause> (<object>)
  constant slot export-names :: <simple-object-vector>,
    required-init-keyword: names:;
end class <export-clause>;

define-procedural-expander
  (#"make-export-clause",
   method (names-frag :: <fragment>)
       => result :: <fragment>;
     let names = split-fragment-at-commas(names-frag);
     make-magic-fragment(make(<export-clause>,
			      names: map(extract-name, names)));
   end method);


define class <create-clause> (<object>)
  constant slot create-names :: <simple-object-vector>,
    required-init-keyword: names:;
end class <create-clause>;

define-procedural-expander
  (#"make-create-clause",
   method (names-frag :: <fragment>)
       => result :: <fragment>;
     let names = split-fragment-at-commas(names-frag);
     make-magic-fragment(make(<create-clause>,
			      names: map(extract-name, names)));
   end method);



define-procedural-expander
  (#"make-use-clause",
   method (name-frag :: <fragment>, import-frag :: <fragment>,
	   exclude-frag :: <fragment>, prefix-frag :: <fragment>,
	   rename-frag :: <fragment>, export-frag :: <fragment>)
       => result :: <fragment>;
     let name = extract-name(name-frag);
     let import = (is-all?(import-frag)
		     | map(method (frag)
			     maybe-extract-renaming(frag) | extract-name(frag);
			   end method,
			   split-fragment-at-commas(import-frag)));
     let exclude = map(extract-name, split-fragment-at-commas(exclude-frag));
     let prefix = extract-prefix(prefix-frag);
     let rename = map(extract-renaming, split-fragment-at-commas(rename-frag));
     let export = (is-all?(export-frag)
		     | map(extract-name,
			   split-fragment-at-commas(export-frag)));
     make-magic-fragment
       (make(<use>,
	     name: name.token-symbol,
	     imports:
	       if (import == #t)
		 #t;
	       else
		 remove-duplicates!
		   (union(map(method (name-or-rename) => res :: <symbol>;
				if (instance?(name-or-rename, <renaming>))
				  name-or-rename.orig-name;
				else
				  name-or-rename.token-symbol;
				end if;
			      end method,
			      import),
			  map(orig-name, rename)));
	       end if,
	     excludes: map(token-symbol, exclude),
	     prefix: prefix,
	     renamings:
	       if (import == #t)
		 rename;
	       else
		 concatenate(choose(rcurry(instance?, <renaming>), import),
			     rename);
	       end if,
	     exports: (export == #t) | map(token-symbol, export)));
   end method);


define method is-all? (frag :: <fragment>)
    => res :: <boolean>;
  #f;
end method is-all?;

define method is-all? (frag :: <token-fragment>)
    => res :: <boolean>;
  frag.fragment-token.token-kind == $true-token;
end method is-all?;


define method maybe-extract-renaming (frag :: <token-fragment>)
    => res :: false-or(<renaming>);
  let token = frag.fragment-token;
  let kind = token.token-kind;
  if (instance?(token, <pre-parsed-token>)
	& kind == $error-token
	& instance?(token.token-parse-tree, <renaming>))
    token.token-parse-tree;
  else
    #f;
  end if;
end method maybe-extract-renaming;

define method extract-renaming (frag :: <token-fragment>)
    => res :: false-or(<renaming>);
  maybe-extract-renaming(frag)
    | error("bug in built in macro: %= isn't a renaming.", frag);
end method extract-renaming;

define method extract-prefix (frag :: <token-fragment>)
    => res :: false-or(<byte-string>);
  let token = frag.fragment-token;
  let kind = token.token-kind;
  if (kind == $string-token)
    let str = token.token-literal.literal-value;
    ~str.empty? & str;
  else
    compiler-error-location
      (token, "Invalid prefix: %s.  Must be a string.",
       token);
  end if;
end method extract-prefix;


define-procedural-expander
  (#"make-renaming",
   method (from-frag :: <fragment>, to-frag :: <fragment>)
       => result :: <fragment>;
     make-magic-fragment(make(<renaming>,
			      orig-name: extract-name(from-frag).token-symbol,
			      new-name: extract-name(to-frag).token-symbol));
   end method);


define-procedural-expander
  (#"make-define-module",
   method (name-frag :: <fragment>, clauses-frag :: <fragment>)
       => result :: <fragment>;
     let name = extract-name(name-frag);
     let (uses, exports, creates) = extract-clauses(clauses-frag);
     make-parsed-fragment(make(<define-module-tlf>,
			       name: name.token-symbol, uses: uses,
			       exports: exports, creates: creates));
   end method);
			      

define-procedural-expander
  (#"make-define-library",
   method (name-frag :: <fragment>, clauses-frag :: <fragment>)
       => result :: <fragment>;
     let name = extract-name(name-frag);
     let (uses, exports, creates) = extract-clauses(clauses-frag);
     unless (creates.empty?)
       error("bug in define library macro: somehow some creates showed up.");
     end unless;
     make-parsed-fragment(make(<define-library-tlf>,
			       name: name.token-symbol, uses: uses,
			       exports: exports));
   end method);
			      

define method extract-clauses (clauses-frag :: <fragment>)
    => (uses :: <simple-object-vector>, exports :: <simple-object-vector>,
	creates :: <simple-object-vector>);
  let uses = make(<stretchy-vector>);
  let exports = make(<stretchy-vector>);
  let creates = make(<stretchy-vector>);
  for (clause in split-fragment-at-commas(clauses-frag))
    process-clause(extract-clause(clause), uses, exports, creates);
  end;
  values(as(<simple-object-vector>, uses),
	 as(<simple-object-vector>, exports),
	 as(<simple-object-vector>, creates));
end;

define method extract-clause (frag :: <token-fragment>)
    => res :: type-union(<use>, <export-clause>, <create-clause>);
  let token = frag.fragment-token;
  let kind = token.token-kind;
  if (instance?(token, <pre-parsed-token>)
	& kind == $error-token
	& instance?(token.token-parse-tree,
		    type-union(<use>, <export-clause>, <create-clause>)))
    token.token-parse-tree;
  else
    error("bug in built in macro: %= isn't a define module/library clause.",
	  frag);
  end if;
end method extract-clause;

define method process-clause
    (clause :: <use>, uses :: <stretchy-vector>,
     exports :: <stretchy-vector>, creates :: <stretchy-vector>)
    => ();
  add!(uses, clause);
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


add-bootstrap-export(#"module-definer");

// Seals for file deflibmod.dylan

// <define-library-tlf> -- subclass of <top-level-form>, <definition-parse>
define sealed domain make(singleton(<define-library-tlf>));
define sealed domain initialize(<define-library-tlf>);
// <define-module-tlf> -- subclass of <top-level-form>, <definition-parse>
define sealed domain make(singleton(<define-module-tlf>));
define sealed domain initialize(<define-module-tlf>);
// <export-clause> -- subclass of <object>
define sealed domain make(singleton(<export-clause>));
define sealed domain initialize(<export-clause>);
// <create-clause> -- subclass of <object>
define sealed domain make(singleton(<create-clause>));
define sealed domain initialize(<create-clause>);
