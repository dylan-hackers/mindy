Module: convert
Synopsis: Dylan-based Mindy Compiler
Author:   Peter S. Housel

// utility functions

// split-fragment-at-commas -- exported.
// 
define generic split-fragment-at-commas (fragment :: <fragment>)
    => results :: <simple-object-vector>;

define method split-fragment-at-commas (fragment :: <empty-fragment>)
    => results :: <simple-object-vector>;
  #[];
end method split-fragment-at-commas;

define method split-fragment-at-commas (fragment :: <compound-fragment>)
    => results :: <simple-object-vector>;
  let results = make(<stretchy-vector>);
  let new-head = fragment.fragment-head;
  let stop = fragment.fragment-tail.fragment-next;
  for (sub-fragment = new-head then sub-fragment.fragment-next,
       until: sub-fragment == stop)
    if (instance?(sub-fragment, <token-fragment>)
	  & sub-fragment.fragment-token.token-kind == $comma-token)
      add!(results,
	   if (sub-fragment == new-head)
	     make(<empty-fragment>);
	   elseif (new-head == sub-fragment.fragment-prev)
	     new-head;
	   else
	     make(<compound-fragment>,
		  head: new-head,
		  tail: sub-fragment.fragment-prev);
	   end if);
      new-head := sub-fragment.fragment-next;
    end if;
  end for;
  add!(results,
       if (new-head == stop)
	 make(<empty-fragment>);
       elseif (new-head == fragment.fragment-tail)
	 new-head;
       else
	 make(<compound-fragment>,
	      head: new-head,
	      tail: fragment.fragment-tail);
       end if);
  as(<simple-object-vector>, results);
end method split-fragment-at-commas;

define method split-fragment-at-commas (fragment :: <token-fragment>)
    => results :: <simple-object-vector>;
  if (fragment.fragment-token.token-kind == $comma-token)
    vector(make(<empty-fragment>), make(<empty-fragment>));
  else
    vector(fragment);
  end if;
end method split-fragment-at-commas;

define method split-fragment-at-commas (fragment :: <bracketed-fragment>)
    => results :: <simple-object-vector>;
  vector(fragment);
end method split-fragment-at-commas;


// Other utilities.

define method expression-from-fragment (fragment :: <fragment>)
    => res :: <expression-parse>;
  parse-expression(make(<fragment-tokenizer>, fragment: fragment));
end method expression-from-fragment;



define method extract-name (fragment :: <fragment>)
    => name :: <identifier-token>;
  error("bug in built in macro, %= isn't a name.", fragment);
end method extract-name;

define method extract-name (fragment :: <token-fragment>)
    => name :: <identifier-token>;
  let token = fragment.fragment-token;
  let kind = token.token-kind;
  if (kind < $define-token | kind > $quoted-name-token)
    error("bug in built in macro, %= isn't a name.", fragment);
  end if;
  token;
end method extract-name;

define method extract-boolean (fragment :: <token-fragment>)
    => res :: <boolean>;
  let token = fragment.fragment-token;
  select (token.token-kind)
    $false-token =>
      #f;
    $true-token =>
      #t;
    otherwise =>
      compiler-fatal-error
	("invalid boolean: %s", token);
  end select;
end method extract-boolean;

define method extract-identifier-or-false (fragment :: <token-fragment>)
    => res :: false-or(<identifier-token>);
  let token = fragment.fragment-token;
  select (token.token-kind)
    $false-token =>
      #f;
    $raw-ordinary-word-token, $ordinary-define-body-word-token,
    $ordinary-define-list-word-token, $quoted-name-token =>
      token;
    otherwise =>
      compiler-fatal-error
	("invalid identifier: %s", token);
  end select;
end method extract-identifier-or-false;

define method extract-identifier (fragment :: <token-fragment>)
    => res :: false-or(<identifier-token>);
  let token = fragment.fragment-token;
  select (token.token-kind)
    $raw-ordinary-word-token, $ordinary-define-body-word-token,
    $ordinary-define-list-word-token, $quoted-name-token =>
      token;
    otherwise =>
      compiler-fatal-error
	("invalid identifier: %s", token);
  end select;
end method extract-identifier;

define method extract-properties
    (plist :: <simple-object-vector>, #rest names)
  local method find-property (name :: <symbol>)
	    => res :: false-or(<fragment>);
	  block (return)
	    for (property in plist)
	      if (property.prop-keyword.token-literal.literal-value == name)
		return(property.prop-value);
	      end;
	    end;
	    #f;
	  end;
	end;
  apply(values, map(find-property, names));
end method extract-properties;

define method make-magic-fragment
    (thing :: <object>,
     #key source-location = make(<unknown-source-location>))
    => res :: <token-fragment>;
  make(<token-fragment>,
       source-location: source-location,
       token: make(<pre-parsed-token>,
		   source-location: source-location,
		   kind: $error-token,
		   parse-tree: thing));
end method make-magic-fragment;


// define class

define class <define-class-parse> (<definition-parse>)
  constant slot defclass-name :: <identifier-token>,
    required-init-keyword: name:;
  constant slot defclass-superclass-exprs :: <simple-object-vector>,
    required-init-keyword: superclass-exprs:;
  constant slot defclass-slots :: <simple-object-vector>,
    required-init-keyword: slots:;
  constant slot defclass-options :: <simple-object-vector>,
    required-init-keyword: options:;
end class <define-class-parse>;

define-procedural-expander
  (#"make-define-class",
   method (generator :: <expansion-generator>, name-frag :: <fragment>,
	   supers-frag :: <fragment>, slots-frag :: <fragment>,
	   options-frag :: <fragment>)
       => ();
     generate-fragment
       (generator,
	make-parsed-fragment
	  (make(<define-class-parse>,
		name: extract-name(name-frag),
		source-location: generator.generator-call.source-location,
		superclass-exprs: map(expression-from-fragment,
				      split-fragment-at-commas(supers-frag)),
		slots: map(extract-slot, split-fragment-at-commas(slots-frag)),
		options: parse-property-list(make(<fragment-tokenizer>,
						  fragment: options-frag))),
	   source-location: generate-token-source-location(generator)));
   end method);

define method extract-slot (frag :: <fragment>)
    => res :: <abstract-slot-parse>;
  if (instance?(frag, <token-fragment>)
	& frag.fragment-token.token-kind == $error-token
	& instance?(frag.fragment-token, <pre-parsed-token>)
	& instance?(frag.fragment-token.token-parse-tree,
		    <abstract-slot-parse>))
    frag.fragment-token.token-parse-tree;
  else
    error("bug in define class macro: %= isn't a slot parse", frag);
  end if;
end method extract-slot;


define abstract class <abstract-slot-parse> (<object>)
end class <abstract-slot-parse>;

define class <slot-parse> (<abstract-slot-parse>, <source-location-mixin>)
  constant slot slot-parse-name :: <identifier-token>,
    required-init-keyword: name:;
  constant slot slot-parse-options :: <simple-object-vector>,
    required-init-keyword: options:;
end class <slot-parse>;

define-procedural-expander
  (#"make-slot",
   method (generator :: <expansion-generator>, name-frag :: <token-fragment>,
	   options-frag :: <fragment>)
       => ();
     generate-fragment
       (generator,
	make-magic-fragment
	  (make(<slot-parse>,
		name: extract-name(name-frag),
		source-location: name-frag.fragment-token.source-location,
		options: parse-property-list(make(<fragment-tokenizer>,
						  fragment: options-frag))),
	   source-location: generate-token-source-location(generator)))
   end method);

define class <inherited-slot-parse> (<abstract-slot-parse>)
  constant slot inherited-slot-parse-name :: <identifier-token>,
    required-init-keyword: name:;
  constant slot inherited-slot-parse-options :: <simple-object-vector>,
    required-init-keyword: options:;
end class <inherited-slot-parse>;

define-procedural-expander
  (#"make-inherited-slot",
   method (generator :: <expansion-generator>, name-frag :: <fragment>,
	   options-frag :: <fragment>)
       => ();
     generate-fragment
       (generator,
	make-magic-fragment
	  (make(<inherited-slot-parse>,
		name: extract-name(name-frag),
		options: parse-property-list(make(<fragment-tokenizer>,
						  fragment: options-frag))),
	   source-location: generate-token-source-location(generator)))
   end method);

define class <init-arg-parse> (<abstract-slot-parse>)
  constant slot init-arg-parse-keyword :: <symbol>,
    required-init-keyword: keyword:;
  constant slot init-arg-parse-options :: <simple-object-vector>,
    required-init-keyword: options:;
end class <init-arg-parse>;

define-procedural-expander
  (#"make-init-arg",
   method (generator :: <expansion-generator>, keyword-frag :: <fragment>,
	   options-frag :: <fragment>)
       => ();
     let keyword
       = if (instance?(keyword-frag, <token-fragment>)
               & keyword-frag.fragment-token.token-kind == $symbol-token)
           keyword-frag.fragment-token.token-literal.literal-value;
         else
           compiler-fatal-error-location
             (keyword-frag,
              "Argument to keyword (%=) must be a keyword", keyword-frag);
         end;
     generate-fragment
       (generator,
	make-magic-fragment
	  (make(<init-arg-parse>,
		keyword: keyword,
		options: parse-property-list(make(<fragment-tokenizer>,
						  fragment: options-frag))),
	   source-location: generate-token-source-location(generator)))
   end method);

define method extract-keyword (frag :: <fragment>)
    => keyword :: <symbol>;
  if (instance?(frag, <token-fragment>)
	& frag.fragment-token.token-kind == $symbol-token)
    frag.fragment-token.token-literal.literal-value;
  else
    error("Bug in define class macro: %= isn't a keyword.", frag);
  end if;
end method extract-keyword;

define method process-top-level-form
    (form :: <define-class-parse>)
 => ();
  format(*standard-output*, "define class %s\n", form.defclass-name);
end method;


// define constant, define variable

define abstract class <define-binding-parse> (<definition-parse>)
  constant slot defbinding-variables :: <variable-list>,
    required-init-keyword: variables:;
  slot defbinding-expression :: <expression-parse>,
    required-init-keyword: expression:;
end class <define-binding-parse>;

define sealed domain make (singleton(<define-binding-parse>));
define sealed domain initialize (<define-binding-parse>);

define class <define-constant-parse> (<define-binding-parse>)
end class <define-constant-parse>;

define sealed domain make (singleton(<define-constant-parse>));

define-procedural-expander
  (#"make-define-constant",
   method (generator :: <expansion-generator>, variables-frag :: <fragment>,
	   expression-frag :: <fragment>)
       => ();
     generate-fragment
       (generator,
	make-parsed-fragment
	  (make(<define-constant-parse>,
		source-location: generator.generator-call.source-location,
		variables: parse-variable-list(make(<fragment-tokenizer>,
						    fragment: variables-frag)),
		expression: expression-from-fragment(expression-frag)),
	   source-location: generate-token-source-location(generator)));
   end method);

define method process-top-level-form
    (form :: <define-constant-parse>)
 => ();
  format(*standard-output*, "define constant %=\n", form.defbinding-variables);
end method;

define class <define-variable-parse> (<define-binding-parse>)
end class <define-variable-parse>;

define sealed domain make (singleton(<define-variable-parse>));

define-procedural-expander
  (#"make-define-variable",
   method (generator :: <expansion-generator>, variables-frag :: <fragment>,
	   expression-frag :: <fragment>)
       => ();
     generate-fragment
       (generator,
	make-parsed-fragment
	  (make(<define-variable-parse>,
		source-location: generator.generator-source.source-location,
		variables: parse-variable-list(make(<fragment-tokenizer>,
						    fragment: variables-frag)),
		expression: expression-from-fragment(expression-frag)),
	   source-location: generate-token-source-location(generator)));
   end method);

define method process-top-level-form
    (form :: <define-variable-parse>)
 => ();
  format(*standard-output*, "define variable %=\n", form.defbinding-variables);
end method;


// define generic

// <define-generic-parse> -- internal.
//
// Special subclass of <definition-parse> that ``define generic'' expands
// into.
// 
define class <define-generic-parse> (<definition-parse>)
  //
  // The name being defined.
  constant slot defgeneric-name :: <identifier-token>,
    required-init-keyword: name:;
  //
  // The parameters.
  constant slot defgeneric-parameters :: <parameter-list>,
    required-init-keyword: parameters:;
  //
  // The results.
  constant slot defgeneric-results :: <variable-list>,
    required-init-keyword: results:;
  //
  // The options, a vector of <property> objects.
  constant slot defgeneric-options :: <simple-object-vector>,
    required-init-keyword: options:;
end class <define-generic-parse>;

define-procedural-expander
  (#"make-define-generic",
   method (generator :: <expansion-generator>, name-frag :: <fragment>,
	   params-frag :: <fragment>, results-frag :: <fragment>,
	   options-frag :: <fragment>)
       => ();
     generate-fragment
       (generator,
	make-parsed-fragment
	  (make(<define-generic-parse>,
		name: extract-name(name-frag),
		source-location: generator.generator-call.source-location,
		parameters: parse-parameter-list(make(<fragment-tokenizer>,
						      fragment: params-frag)),
		results: parse-variable-list(make(<fragment-tokenizer>,
						  fragment: results-frag)),
		options: parse-property-list(make(<fragment-tokenizer>,
						  fragment: options-frag))),
	   source-location: generate-token-source-location(generator)));
   end method);

define method process-top-level-form
    (form :: <define-generic-parse>)
 => ();
  format(*standard-output*, "define generic %s\n", form.defgeneric-name);
end method;


// define method

// <define-method-parse> -- internal.
// 
define class <define-method-parse> (<definition-parse>)
  //
  // The method guts.  Includes the name, parameters, results, and body.
  constant slot defmethod-method :: <method-parse>,
    required-init-keyword: method:;
  //
  // Extra options, a vector of <property> objects.
  constant slot defmethod-options :: <simple-object-vector>,
    required-init-keyword: options:;
end class <define-method-parse>;

// *implicitly-define-next-method* -- exported.
//
// When set to #t, implicitly define next-method.  When set to #f, don't.
// Mindycomp has always taken the non-standard approach of not implicitly
// defining next-method.
//
define variable *implicitly-define-next-method* :: <boolean> = #f;

define-procedural-expander
  (#"make-define-method",
   method (generator :: <expansion-generator>, name-frag :: <fragment>,
	   method-frag :: <fragment>, options-frag :: <fragment>)
       => ();
     let method-parse
       = for (method-expr = expression-from-fragment(method-frag)
		then macro-expand(method-expr),
	      while: instance?(method-expr, <macro-call-parse>))
	 finally
	   unless (instance?(method-expr, <method-ref-parse>))
	     error("bug in define method macro: guts didn't show up as "
		     "a method-ref");
	   end unless;
	   method-expr.method-ref-method;
	 end for;
     method-parse.method-name := extract-name(name-frag);

     if (*implicitly-define-next-method*)
       let params = method-parse.method-parameters;
       unless (params.paramlist-next)
	 params.paramlist-next
	   := make(<identifier-token>,
		   kind: $raw-ordinary-word-token,
		   symbol: #"next-method",
		   module: *current-module*);
       end unless;
     end if;
     
     generate-fragment
       (generator,
	make-parsed-fragment
	  (make(<define-method-parse>,
		method: method-parse,
		source-location: generator.generator-call.source-location,
		options: parse-property-list(make(<fragment-tokenizer>,
						  fragment: options-frag))),
	   source-location: generate-token-source-location(generator)));
   end method);

define method process-top-level-form
    (form :: <define-method-parse>)
 => ();
  format(*standard-output*, "define method %s\n",
	 form.defmethod-method.method-name);
end method;


// method

define-procedural-expander
  (#"make-anonymous-method",
   method (generator :: <expansion-generator>, parameters-frag :: <fragment>,
           results-frag :: <fragment>, body-frag :: <fragment>,
           options-frag :: <fragment>)
    => ();
     generate-fragment
       (generator,
        make-parsed-fragment
          (make(<method-ref-parse>,
                method:
                  make(<method-parse>,
                       parameters:
                         parse-parameter-list(make(<fragment-tokenizer>,
                                                   fragment: parameters-frag)),
                       returns:
                         parse-variable-list(make(<fragment-tokenizer>,
                                                  fragment: results-frag)),
                       body: expression-from-fragment(body-frag)),
                options: parse-property-list(make(<fragment-tokenizer>,
                                                  fragment: options-frag))),
           source-location: generate-token-source-location(generator)));
   end method);


// Define library.

define class <define-library-parse> (<definition-parse>)
  //
  constant slot define-library-name :: <symbol-token>,
    required-init-keyword: name:;
  //
  constant slot define-library-uses :: <simple-object-vector>,
    required-init-keyword: uses:;
  //
  constant slot define-library-exports :: <simple-object-vector>,
    required-init-keyword: exports:;
end;

define method print-message
    (tlf :: <define-library-parse>, stream :: <stream>) => ();
  format(stream, "Define Library %s.", tlf.define-library-name.token-symbol);
end;

define-procedural-expander
  (#"make-define-library",
   method (generator :: <expansion-generator>, name-frag :: <fragment>,
	   clauses-frag :: <fragment>)
        => ();
     let name = extract-name(name-frag);
     let (uses, exports, creates) = extract-clauses(clauses-frag);
     unless (creates.empty?)
       error("bug in define library macro: somehow some creates showed up.");
     end unless;
     generate-fragment
       (generator,
	make-parsed-fragment
	  (make(<define-library-parse>,
		name: name, uses: uses, exports: exports),
	   source-location: generate-token-source-location(generator)));
   end method);

define method process-top-level-form (form :: <define-library-parse>) => ();
  format(*standard-output*, "define library\n");
  note-context(form);
  note-library-definition(form.define-library-name, form.define-library-uses,
			  form.define-library-exports);
  end-of-context();
end;


// define module, define library

define class <define-module-parse> (<definition-parse>)
  //
  constant slot define-module-name :: <symbol-token>,
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
    (tlf :: <define-module-parse>, stream :: <stream>) => ();
  format(stream, "Define Module %s.", tlf.define-module-name.token-symbol);
end;

define-procedural-expander
  (#"make-define-module",
   method (generator :: <expansion-generator>, name-frag :: <fragment>,
	   clauses-frag :: <fragment>)
       => ();
     let name = extract-name(name-frag);
     let (uses, exports, creates) = extract-clauses(clauses-frag);
     generate-fragment
       (generator,
	make-parsed-fragment
	  (make(<define-module-parse>,
		name: name, uses: uses, exports: exports, creates: creates),
	   source-location: generate-token-source-location(generator)));
   end method);
			      
define method process-top-level-form (form :: <define-module-parse>) => ();
  let name = form.define-module-name;
  format(*standard-output*, "define module %s\n", name.token-symbol);
  note-context(form);
  note-module-definition(*Current-Library*, name, form.define-module-uses,
			 form.define-module-exports,
			 form.define-module-creates);
  end-of-context();
  //
  // This call to find-module can't fail because note-module-definition
  // creates the module.
  form.define-module-module
    := find-module(*Current-Library*, name.token-symbol);
end;

define class <export-clause> (<object>)
  constant slot export-names :: <simple-object-vector>,
    required-init-keyword: names:;
end class <export-clause>;

define-procedural-expander
  (#"make-export-clause",
   method (generator :: <expansion-generator>, names-frag :: <fragment>)
       => ();
     let names = split-fragment-at-commas(names-frag);
     generate-fragment
       (generator,
	make-magic-fragment
	  (make(<export-clause>,
		names: map(extract-name, names)),
	   source-location: generate-token-source-location(generator)));
   end method);


define class <create-clause> (<object>)
  constant slot create-names :: <simple-object-vector>,
    required-init-keyword: names:;
end class <create-clause>;

define-procedural-expander
  (#"make-create-clause",
   method (generator :: <expansion-generator>, names-frag :: <fragment>)
       => ();
     let names = split-fragment-at-commas(names-frag);
     generate-fragment
       (generator,
	make-magic-fragment
	  (make(<create-clause>,
		names: map(extract-name, names)),
	   source-location: generate-token-source-location(generator)));
   end method);



define-procedural-expander
  (#"make-use-clause",
   method (generator :: <expansion-generator>, name-frag :: <fragment>,
	   import-frag :: <fragment>, exclude-frag :: <fragment>,
	   prefix-frag :: <fragment>, rename-frag :: <fragment>,
	   export-frag :: <fragment>)
       => ();
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
     generate-fragment
       (generator,
	make-magic-fragment
	  (make(<use>,
		name: name,
		imports:
		  if (instance?(import, <all-marker>))
		    import;
		  else
		    concatenate
		      (map(method (name-or-rename) => res :: <symbol-token>;
			     if (instance?(name-or-rename, <renaming>))
			       name-or-rename.renaming-orig-name;
			     else
			       name-or-rename;
			     end if;
			   end method,
			   import),
		       map(renaming-orig-name, rename));
		  end if,
		excludes: exclude,
		prefix: prefix,
		renamings:
		  if (instance?(import, <all-marker>))
		    rename;
		  else
		    concatenate(choose(rcurry(instance?, <renaming>), import),
				rename);
		  end if,
		exports: export),
	   source-location: generate-token-source-location(generator)));
   end method);


define method is-all? (frag :: <fragment>)
    => res :: <boolean>;
  #f;
end method is-all?;

define method is-all? (frag :: <token-fragment>)
    => res :: false-or(<all-marker>);
  if (frag.fragment-token.token-kind == $true-token)
    make(<all-marker>);
  else
    #f;
  end if;
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
    compiler-fatal-error-location
      (token, "Invalid prefix: %s.  Must be a string.",
       token);
  end if;
end method extract-prefix;


define-procedural-expander
  (#"make-renaming",
   method (generator :: <expansion-generator>, from-frag :: <fragment>,
	   to-frag :: <fragment>)
       => ();
     generate-fragment
       (generator,
	make-magic-fragment
	  (make(<renaming>,
		orig-name: extract-name(from-frag),
		new-name: extract-name(to-frag)),
	   source-location: generate-token-source-location(generator)));
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
    block (already-there)
      for (old in exports)
	if (old.token-symbol == token.token-symbol)
	  already-there();
	end if;
      end for;
      add!(exports, token);
    end block;
  end for;
end method process-clause;

define method process-clause
    (clause :: <create-clause>, uses :: <stretchy-vector>,
     exports :: <stretchy-vector>, creates :: <stretchy-vector>)
    => ();
  for (token in clause.create-names)
    let name = token.token-symbol;
    block (already-there)
      for (old in creates)
	if (old.token-symbol == token.token-symbol)
	  already-there();
	end if;
      end for;
      add!(creates, token);
    end block;
  end for;
end method process-clause;

add-bootstrap-export(#"module-definer");


// top-level macro calls 

define method process-top-level-form
    (form :: <macro-call-parse>) => ();
  format(*standard-output*, "(top-level macro-call)\n");
  process-top-level-form(macro-expand(form));
end method process-top-level-form;


// top-level expressions, bodies, etc.

// Puke if any local declarations appear at top level.
//
define method process-top-level-form (form :: <local-declaration-parse>) => ();
  compiler-fatal-error-location
    (form, "Local declarations cannot appear directly at top level.");
end;



define method process-top-level-form (form :: <expression-parse>) => ();
  /*
  add!(*Top-Level-Forms*,
       make(<expression-tlf>,
	    expression: form,
	    source-location: form.source-location));
  */
end;

define method process-top-level-form (form :: <body-parse>) => ();
  local
    method process (forms :: <simple-object-vector>)
	=> new-body :: false-or(<simple-object-vector>);
      block (return)
	for (subform in forms,
	     index from 0)
	  while (instance?(subform, <macro-call-parse>))
	    subform := macro-expand(subform);
	  end while;
	  if (instance?(subform, <body-parse>))
	    let new-body = process(subform.body-parts);
	    if (new-body)
	      let result = copy-sequence(forms, start: index);
	      result[0] := make(<body-parse>, parts: new-body);
	      return(result);
	    end;
	  elseif (instance?(subform, <local-declaration-parse>))
	    return(copy-sequence(forms, start: index));
	  else
	    process-top-level-form(subform);
	  end;
	finally
	  #f;
	end for;
      end block;
    end method process;
  let new-body = process(form.body-parts);
  if (new-body)
    let expr
	= make(<body-parse>,
	       parts: new-body,
	       source-location: form.source-location);
    /*
    add!(*Top-Level-Forms*,
	 make(<expression-tlf>,
	      expression: expr,
	      source-location: expr.source-location));
    */
  end;
end;
