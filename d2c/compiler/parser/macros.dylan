module: macros
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/parser/macros.dylan,v 1.12 1996/02/05 01:18:27 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

define abstract class <macro-definition> (<definition>)
  slot macro-intermediate-words :: <simple-object-vector>,
    init-keyword: intermediate-words:;
  slot macro-main-rule-set :: <simple-object-vector>,
    required-init-keyword: main-rule-set:;
  slot macro-auxiliary-rule-sets :: <simple-object-vector>,
    required-init-keyword: auxiliary-rule-sets:;
end;

define class <define-macro-definition> (<macro-definition>)
end;

define class <define-bindings-macro-definition> (<macro-definition>)
end;

define class <statement-macro-definition> (<macro-definition>)
end;

define class <function-macro-definition> (<macro-definition>)
end;

define class <define-macro-tlf> (<simple-define-tlf>)
  //
  // Make the definition required.
  required keyword defn:;
end;

define method print-message
    (tlf :: <define-macro-tlf>, stream :: <stream>) => ();
  format(stream, "Define Macro %s", tlf.tlf-defn.defn-name);
end;


// Syntax table manipulation routines.

define method sans-definer (name :: <symbol>)
    => res :: false-or(<symbol>);
  let name-str = as(<string>, name);
  let name-size = name-str.size;
  if (name-size > 8)
    block (return)
      for (i from name-size - 8, char in "-definer")
	unless (as-lowercase(name-str[i]) == char)
	  return(#f);
	end;
      end;
      as(<symbol>, copy-sequence(name-str, end: name-size - 8));
    end;
  else
    #f;
  end;
end;

define method check-syntax-table-additions (table :: <table>,
					    defn :: <define-macro-definition>,
					    name :: <symbol>)
    => ();
  let name = name.sans-definer;
  if (name)
    unless (merge-category(table, name, <define-word-token>))
      error("Inconsistent syntax for %=", name);
    end;
  end;
end;

define method make-syntax-table-additions (table :: <table>,
					   defn :: <define-macro-definition>,
					   name :: <symbol>)
    => ();
  let name = name.sans-definer;
  if (name)
    table[name] := merge-category(table, name, <define-word-token>);
  end;
end;

define method check-syntax-table-additions
    (table :: <table>,
     defn :: <define-bindings-macro-definition>,
     name :: <symbol>)
    => ();
  let name = name.sans-definer;
  if (name)
    unless (merge-category(table, name, <define-bindings-word-token>))
      error("Inconsistent syntax for %=", name);
    end;
  end;
end;

define method make-syntax-table-additions
    (table :: <table>,
     defn :: <define-bindings-macro-definition>,
     name :: <symbol>)
    => ();
  let name = name.sans-definer;
  if (name)
    table[name] := merge-category(table, name, <define-bindings-word-token>);
  end;
end;

define method check-syntax-table-additions
    (table :: <table>,
     defn :: <statement-macro-definition>,
     name :: <symbol>)
    => ();
  unless (merge-category(table, name, <begin-word-token>))
    error("Inconsistent syntax for %=", name);
  end;
end;

define method make-syntax-table-additions
    (table :: <table>,
     defn :: <statement-macro-definition>,
     name :: <symbol>)
    => ();
  table[name] := merge-category(table, name, <begin-word-token>);
end;



// process-top-level-form methods.
// 

define method process-top-level-form (form :: <macro-statement>) => ();
  let expansion = expand(form, #f);
  if (expansion)
    do(process-top-level-form, expansion);
  else
    error("Macro statement didn't expand?");
  end;
end;

define method process-top-level-form (form :: <define-parse>) => ();
  let expansion = expand(form, #f);
  if (expansion)
    do(process-top-level-form, expansion);
  else
    error("Use of define macro %s didn't expand?",
	  form.define-word.token-symbol);
  end;
end;

define method process-top-level-form (form :: <define-bindings-parse>) => ();
  let expansion = expand(form, #f);
  if (expansion)
    do(process-top-level-form, expansion);
  else
    error("Use of define bindings macro %s didn't expand?",
	  form.define-word.token-symbol);
  end;
end;


define method process-top-level-form
    (defmacro :: <define-statement-macro-parse>)
    => ();
  define-macro(defmacro, <statement-macro-definition>);
end;

define method process-top-level-form
    (defmacro :: <define-function-macro-parse>)
    => ();
  define-macro(defmacro, <function-macro-definition>);
end;

define method process-top-level-form
    (defmacro :: <define-define-macro-parse>)
    => ();
  fix-define-rules(defmacro);
  define-macro(defmacro, <define-macro-definition>);
end;

define method process-top-level-form
    (defmacro :: <define-define-bindings-macro-parse>)
    => ();
  fix-define-rules(defmacro);
  define-macro(defmacro, <define-bindings-macro-definition>);
end;


define method finalize-top-level-form (tlf :: <define-macro-tlf>) => ();
  // Nothing to do.
end;

define method convert-top-level-form
    (builder :: <fer-builder>, tlf :: <define-macro-tlf>) => ();
  // Nothing to do.
end;


// define-macro & fix-define-rules

define method define-macro (defmacro :: <define-macro-parse>,
			    defn-class :: <class>)
    => ();
  let name = defmacro.defmacro-name;
  let defn = make(defn-class,
		  name: make(<basic-name>,
			     symbol: name.token-symbol,
			     module: *Current-Module*),
		  main-rule-set: defmacro.defmacro-main-rule-set,
		  auxiliary-rule-sets: defmacro.defmacro-auxiliary-rule-sets);
  find-wildcards(defn);
  find-end-variables(defn, #t);
  find-intermediate-words(defn);
  note-variable-definition(defn);
  add!(*Top-Level-Forms*, make(<define-macro-tlf>, defn: defn));
end;

define method fix-define-rules (defmacro :: <define-macro-parse>) => ();
  let name = sans-definer(defmacro.defmacro-name.token-symbol);
  unless (name)
    error("Name of define macro doesn't end with -definer: %s",
	  defmacro.defmacro-name.token-symbol);
  end;
  for (rule in defmacro.defmacro-main-rule-set)
    let pattern = rule.rule-pattern;
    let pattern-list = first(pattern.pattern-pieces);
    let pattern-sequence = first(pattern-list.pattern-list-pieces);
    if (instance?(pattern-sequence, <pattern-sequence>))
      block (return)
	let pieces = pattern-sequence.pattern-sequence-pieces;
	for (piece in pieces, index from 0)
	  if (instance?(piece, <identifier-pattern>)
		& piece.pattern-identifier.token-symbol == name)
	    rule.define-rule-modifiers-pattern
	      := make(<pattern-sequence>,
		      pieces: copy-sequence(pieces, end: index));
	    pattern-sequence.pattern-sequence-pieces
	      := copy-sequence(pieces, start: index + 1);
	    return();
	  end;
	end;
	error("Can't find macro name (%s) in rule.", name);
	#f;
      end;
    else
      error("Can't find macro name (%s) in rule.", name);
      #f;
    end;
  end;
end;



// expand methods.

define method expand (form :: <define-parse>,
		      lexenv :: false-or(<lexenv>))
    => results :: false-or(<simple-object-vector>);
  let define-word = form.define-word;
  let name = make(<basic-name>,
		  module: define-word.token-module,
		  symbol: as(<symbol>,
			     concatenate(as(<string>, define-word.token-symbol),
					 "-definer")));
  let var = find-variable(name);
  unless (var)
    error("syntax table and variable table inconsistent.");
  end;
  let defn = var.variable-definition;
  unless (instance?(defn, <define-macro-definition>))
    error("syntax table and variable table inconsistent.");
  end;
  expand-macro-aux(form, form.define-fragment, defn);
end;

define method expand (form :: <define-bindings-parse>,
		      lexenv :: false-or(<lexenv>))
    => results :: false-or(<simple-object-vector>);
  let define-word = form.define-word;
  let name = make(<basic-name>,
		  module: define-word.token-module,
		  symbol: as(<symbol>,
			     concatenate(as(<string>, define-word.token-symbol),
					 "-definer")));
  let var = find-variable(name);
  unless (var)
    error("syntax table and variable table inconsistent.");
  end;
  let defn = var.variable-definition;
  unless (instance?(defn, <define-bindings-macro-definition>))
    error("syntax table and variable table inconsistent.");
  end;
  let piece = make(<piece>, token: form.define-bindings);
  let fragment = make(<fragment>, head: piece, tail: piece);
  expand-macro-aux(form, fragment, defn);
end;

define method expand (form :: <macro-statement>,
		      lexenv :: false-or(<lexenv>))
    => results :: false-or(<simple-object-vector>);
  let var = find-variable(id-name(form.statement-begin-word));
  unless (var)
    error("syntax table and variable table inconsistent.");
  end;
  let defn = var.variable-definition;
  unless (instance?(defn, <statement-macro-definition>))
    error("syntax table and variable table inconsistent.");
  end;
  expand-macro-aux(form, form.statement-fragment, defn);
end;

define method expand (form :: <funcall>,
		      lexenv :: false-or(<lexenv>))
    => results :: false-or(<simple-object-vector>);
  let fun = form.funcall-function;
  if (instance?(fun, <varref>))
    let id = fun.varref-id;
    if (id.token-module)
      let var = find-variable(id-name(id));
      if (var)
	let defn = var.variable-definition;
	if (instance?(defn, <function-macro-definition>))
	  //
	  // Build a fragment that looks like the function call.
	  //
	  let fragment = make(<fragment>);
	  let args = form.funcall-arguments;
	  let need-comma? = #f;
	  for (arg in args,
	       possible-keyword? = even?(args.size) then ~possible-keyword?)
	    if (need-comma?)
	      postpend-piece(fragment,
			     make(<piece>, token: make(<comma-token>)));
	    end;
	    if (possible-keyword?
		  & instance?(arg, <literal-ref>)
		  & instance?(arg.litref-literal, <literal-symbol>))
	      postpend-piece(fragment,
			     make(<piece>,
				  token: make(<keyword-token>,
					      literal: arg.litref-literal)));
	      need-comma? := #f;
	    else
	      postpend-piece(fragment, make(<piece>, token: arg));
	      need-comma? := #t;
	    end;
	  end;
	  //
	  // Now feed it to the pattern matcher
	  expand-macro-aux(form, fragment, defn);
	end;
      end;
    end;
  end;
end;

define method expand-macro-aux (form :: <constituent>,
				fragment :: <fragment>,
				defn :: <macro-definition>)
    => results :: false-or(<simple-object-vector>);
  let intermediate-words = defn.macro-intermediate-words;
  block (return)
    for (rule in defn.macro-main-rule-set)
      let results = match-rule(rule, form, fragment, intermediate-words);
      if (results)
	let replacement = expand-template(rule.rule-template, results, #f,
					  defn.macro-auxiliary-rule-sets,
					  intermediate-words,
					  make(<uniquifier>));
	let tokenizer = make(<fragment-tokenizer>, fragment: replacement);
	return(parse-body(tokenizer));
      end;
    end;
    error("Syntax error in %=", form);
  end;
end;


define method find-intermediate-words (defn :: <macro-definition>)
    => res :: <simple-object-vector>;
  let aux-rule-sets = defn.macro-auxiliary-rule-sets;
  find-body-variable-rule-sets(aux-rule-sets);
  let results = #();
  for (rule in defn.macro-main-rule-set)
    results := find-intermediate-words-in(rule.rule-pattern, #f, aux-rule-sets,
					  results);
  end;
  for (aux-rule-set in aux-rule-sets)
    for (rule in aux-rule-set.rule-set-rules)
      results := find-intermediate-words-in(rule.rule-pattern, aux-rule-set,
					    aux-rule-sets, results);
    end;
  end;
  defn.macro-intermediate-words := as(<simple-object-vector>, results);
end;

define method find-body-variable-rule-sets
    (aux-rule-sets :: <simple-object-vector>)
    => ();
  let again? = #t;
  while (again?)
    again? := #f;
    for (rule-set in aux-rule-sets)
      for (rule in rule-set.rule-set-rules,
	   until: rule-set.rule-set-body-variable?)
	let pieces = rule.rule-pattern.pattern-pieces;
	unless (empty?(pieces))
	  let pattern-list = pieces.last;
	  let list-tail = pattern-list.pattern-list-pieces.last;
	  if (instance?(list-tail, <pattern-sequence>)
		& body-variable?(list-tail.pattern-sequence-pieces.last,
				 rule-set, aux-rule-sets))
	    rule-set.rule-set-body-variable? := #t;
	    again? := #t;
	  end;
	end;
      end;
    end;
  end;
end;

define method body-variable?
    (thing, cur-set :: false-or(<auxiliary-rule-set>),
     aux-rule-sets :: <simple-object-vector>)
    => res :: <boolean>;
  #f;
end;

define method body-variable?
    (patvar :: <pattern-variable>, cur-set :: false-or(<auxiliary-rule-set>),
     aux-rule-sets :: <simple-object-vector>)
    => res :: <boolean>;
  let constraint = patvar.patvar-constraint;
  if (constraint == #"body" | constraint == #"case-body")
    #t;
  else
    let aux-rule-set
      = if (patvar.patvar-name)
	  find-aux-rule-set(patvar.patvar-name, aux-rule-sets);
	else
	  cur-set;
	end;
    aux-rule-set & aux-rule-set.rule-set-body-variable?;
  end;
end;

define method find-aux-rule-set
    (name :: <symbol>, aux-rule-sets :: <simple-object-vector>)
    => res :: false-or(<auxiliary-rule-set>);
  block (return)
    for (aux-rule-set in aux-rule-sets)
      if (aux-rule-set.rule-set-name == name)
	return(aux-rule-set);
      end;
    end;
    #f;
  end;
end;

define method find-intermediate-words-in
    (pattern :: <pattern>, cur-set :: false-or(<auxiliary-rule-set>),
     aux-rule-sets :: <simple-object-vector>, results :: <list>)
    => res :: <list>;
  for (piece in pattern.pattern-pieces)
    results := find-intermediate-words-in(piece, cur-set, aux-rule-sets,
					  results);
  end;
  results;
end;

define method find-intermediate-words-in
    (pattern :: <pattern-list>, cur-set :: false-or(<auxiliary-rule-set>),
     aux-rule-sets :: <simple-object-vector>, results :: <list>)
    => res :: <list>;
  for (piece in pattern.pattern-list-pieces)
    results := find-intermediate-words-in(piece, cur-set, aux-rule-sets,
					  results);
  end;
  results;
end;

define method find-intermediate-words-in
    (pattern :: <pattern-sequence>, cur-set :: false-or(<auxiliary-rule-set>),
     aux-rule-sets :: <simple-object-vector>, results :: <list>)
    => res :: <list>;
  for (piece in pattern.pattern-sequence-pieces,
       prev = #f then piece)
    if (body-variable?(prev, cur-set, aux-rule-sets))
      results := find-intermediate-words-in(piece, cur-set, aux-rule-sets,
					    results);
    end;
  end;
  results;
end;

define method find-intermediate-words-in
    (pattern :: <simple-pattern>, cur-set :: false-or(<auxiliary-rule-set>),
     aux-rule-sets :: <simple-object-vector>, results :: <list>)
    => res :: <list>;
  results;
end;

define method find-intermediate-words-in
    (pattern :: <identifier-pattern>,cur-set :: false-or(<auxiliary-rule-set>),
     aux-rule-sets :: <simple-object-vector>, results :: <list>)
    => res :: <list>;
  let sym = pattern.pattern-identifier.token-symbol;
  if (member?(sym, results))
    results;
  else
    pair(sym, results);
  end;
end;

define method find-intermediate-words-in
    (pattern :: <pattern-variable>, cur-set :: false-or(<auxiliary-rule-set>),
     aux-rule-sets :: <simple-object-vector>, results :: <list>)
    => res :: <list>;
  if (pattern.patvar-name)
    let aux-rule-set = find-aux-rule-set(pattern.patvar-name, aux-rule-sets);
    if (aux-rule-set & ~aux-rule-set.rule-set-processed-intermediate-words?)
      aux-rule-set.rule-set-processed-intermediate-words? := #t;
      for (rule in aux-rule-set.rule-set-rules)
	results := find-intermediate-words-at-start
	             (rule.rule-pattern, cur-set, aux-rule-sets, results);
      end;
    end;
  end;
  results;
end;

define method find-intermediate-words-in
    (pattern :: <property-list-pattern>,
     cur-set :: false-or(<auxiliary-rule-set>),
     aux-rule-sets :: <simple-object-vector>, results :: <list>)
    => res :: <list>;
  results;
end;

define method find-intermediate-words-in
    (pattern :: <details-pattern>, cur-set :: false-or(<auxiliary-rule-set>),
     aux-rule-sets :: <simple-object-vector>, results :: <list>)
    => res :: <list>;
  find-intermediate-words-in(pattern.pattern-sub-pattern, cur-set,
			     aux-rule-sets, results);
end;

define method find-intermediate-words-at-start
    (pattern :: <pattern>, cur-set :: false-or(<auxiliary-rule-set>),
     aux-rule-sets :: <simple-object-vector>, results :: <list>)
    => res :: <list>;
  if (empty?(pattern.pattern-pieces))
    results;
  else
    find-intermediate-words-at-start(pattern.pattern-pieces.first,
				     cur-set, aux-rule-sets, results);
  end;
end;

define method find-intermediate-words-at-start
    (pattern :: <pattern-list>, cur-set :: false-or(<auxiliary-rule-set>),
     aux-rule-sets :: <simple-object-vector>, results :: <list>)
    => res :: <list>;
  if (empty?(pattern.pattern-list-pieces))
    results;
  else
    find-intermediate-words-at-start(pattern.pattern-list-pieces.first,
				     cur-set, aux-rule-sets, results);
  end;
end;

define method find-intermediate-words-at-start
    (pattern :: <pattern-sequence>, cur-set :: false-or(<auxiliary-rule-set>),
     aux-rule-sets :: <simple-object-vector>, results :: <list>)
    => res :: <list>;
  if (empty?(pattern.pattern-sequence-pieces))
    results;
  else
    find-intermediate-words-in(pattern.pattern-sequence-pieces.first,
			       cur-set, aux-rule-sets, results);
  end;
end;



define generic find-end-variables (within, at-end?) => ();

define method find-end-variables (defn :: <macro-definition>,
				  at-end? :: <boolean>)
    => ();
  for (rule in defn.macro-main-rule-set)
    find-end-variables(rule, at-end?);
  end;
  for (rule-set in defn.macro-auxiliary-rule-sets)
    for (rule in rule-set.rule-set-rules)
      find-end-variables(rule, at-end?);
    end;
  end;
end;

define method find-end-variables (rule :: <rule>, at-end? :: <boolean>) => ();
  find-end-variables(rule.rule-pattern, at-end?);
end;

define method find-end-variables (rule :: <abstract-define-rule>,
				  at-end? :: <boolean>,
				  #next next-method)
    => ();
  find-end-variables(rule.define-rule-modifiers-pattern, at-end?);
  next-method();
end;

define method find-end-variables (pattern :: <pattern>, at-end? :: <boolean>)
    => ();
  let pieces = pattern.pattern-pieces;
  let length = pieces.size;
  unless (length == 0)
    for (index from 0 below length - 1)
      find-end-variables(pieces[index], #f);
    finally
      find-end-variables(pieces[index], at-end?);
    end;
  end;
end;
				 
define method find-end-variables (pattern :: <pattern-list>,
				  at-end? :: <boolean>)
    => ();
  let pieces = pattern.pattern-list-pieces;
  let length = pieces.size;
  unless (length == 0)
    for (index from 0 below length - 1)
      find-end-variables(pieces[index], #f);
    finally
      find-end-variables(pieces[index], at-end?);
    end;
  end;
end;

define method find-end-variables (pattern :: <pattern-sequence>,
				  at-end? :: <boolean>)
    => ();
  let pieces = pattern.pattern-sequence-pieces;
  let length = pieces.size;
  unless (length == 0)
    for (index from 0 below length - 1)
      find-end-variables(pieces[index], #f);
    finally
      find-end-variables(pieces[index], at-end?);
    end;
  end;
end;

define method find-end-variables (pattern :: <property-list-pattern>,
				  at-end? :: <boolean>)
    => ();
end;

define method find-end-variables (pattern :: <simple-pattern>,
				  at-end? :: <boolean>)
    => ();
end;

define method find-end-variables (pattern :: <details-pattern>,
				  at-end? :: <boolean>)
    => ();
  find-end-variables(pattern.pattern-sub-pattern, #t);
end;

define method find-end-variables (pattern :: <pattern-variable>,
				  at-end? :: <boolean>)
    => ();
  pattern.patvar-at-end? := at-end?;
end;


define generic find-wildcards (within) => ();

define method find-wildcards (defn :: <macro-definition>) => ();
  do(find-wildcards, defn.macro-main-rule-set);
  for (rule-set in defn.macro-auxiliary-rule-sets)
    do(find-wildcards, rule-set.rule-set-rules);
  end;
end;

define method find-wildcards (rule :: <rule>) => ();
  find-wildcards(rule.rule-pattern);
end;

define method find-wildcards (rule :: <abstract-define-rule>,
			      #next next-method)
    => ();
  find-wildcards(rule.define-rule-modifiers-pattern);
  next-method();
end;

define method find-wildcards (pattern :: <pattern>) => ();
  for (piece in pattern.pattern-pieces)
    find-wildcards(piece);
  end;
end;
				 
define method find-wildcards (pattern :: <pattern-list>) => ();
  for (piece in pattern.pattern-list-pieces)
    find-wildcards(piece);
  end;
end;

define method find-wildcards (pattern :: <pattern-sequence>) => ();
  let wildcard = #f;
  for (piece in pattern.pattern-sequence-pieces)
    if (instance?(piece, <pattern-variable>) & ~piece.patvar-constraint)
      if (~piece.patvar-name | ~wildcard)
	wildcard := piece;
      end;
    elseif (instance?(piece, <details-pattern>))
      find-wildcards(piece.pattern-sub-pattern);
    end;
  end;
  if (wildcard)
    wildcard.patvar-wildcard? := #t;
  end;
end;

define method find-wildcards (pattern :: <property-list-pattern>) => ();
end;


// Stuff to figure extents of things.

define constant <type-part-type>
  = type-union(<left-paren-token>, <left-bracket-token>, <dot-token>,
	    <literal-token>, <string-token>, <true-token>, <false-token>,
	    <literal-ref>, <expression>, <name-token>);
define constant <var-part-type>
  = type-union(<type-part-type>, <double-colon-token>);
define constant <expr-part-type>
  = type-union(<type-part-type>, <operator-token>, <abstract-literal-token>);
define constant <plist-part-type>
  = type-union(<expr-part-type>, <comma-token>, <property-set>);


define method guess-extent-of (fragment :: <fragment>, type :: <type>)
  let tail = fragment.fragment-tail;
  for (prev = #f then piece,
       piece = fragment.fragment-head then piece.piece-next,
       while: (piece & piece.piece-prev ~= tail
		 & instance?(piece.piece-token, type)))
    if (instance?(piece, <balanced-piece>))
      piece := piece.piece-other;
    end;
  finally
    if (prev)
      values(make(<fragment>, head: fragment.fragment-head, tail: prev),
	     make(<fragment>, head: piece, tail: fragment.fragment-tail));
    else
      values(make(<fragment>), fragment);
    end;
  end;
end;


define method find-intermediate-word
    (fragment :: <fragment>,
     intermediate-words :: <simple-object-vector>)
  let tail = fragment.fragment-tail;
  for (prev = #f then piece,
       piece = fragment.fragment-head then piece.piece-next,
       while: (piece & piece.piece-prev ~= tail
		 & ~(instance?(piece.piece-token, <word-token>)
		       & member?(piece.piece-token.token-symbol,
				 intermediate-words))))
    if (instance?(piece, <balanced-piece>))
      piece := piece.piece-other;
    end;
  finally
    if (prev)
      values(make(<fragment>, head: fragment.fragment-head, tail: prev),
	     make(<fragment>, head: piece, tail: fragment.fragment-tail));
    else
      values(make(<fragment>), fragment);
    end;
  end;
end;


define method trim-until-parsable (fragment :: <fragment>,
				   remaining :: <fragment>,
				   parser :: <function>)
    => (result :: <object>,
	fragment :: false-or(<fragment>),
	remaining :: false-or(<fragment>));
  block (return)
    while (#t)
      block ()
	let result = parser(make(<fragment-tokenizer>, fragment: fragment));
	return(result, fragment, remaining);
      exception <error>
	if (more?(fragment))
	  let prev-piece = fragment.fragment-tail;
	  if (instance?(prev-piece, <balanced-piece>))
	    prev-piece := prev-piece.piece-other;
	  end;
	  remaining.fragment-head := prev-piece;
	  fragment.fragment-tail := prev-piece.piece-prev;
	else
	  return(#f, #f, #f);
	end;
      end;
    end;
  end;
end;



define method match-rule (rule :: <rule>,
			  form :: false-or(<constituent>),
			  fragment :: <fragment>,
			  intermediate-words :: <simple-object-vector>)
    => res :: false-or(<list>);
  match(rule.rule-pattern, fragment, intermediate-words,
	method () #f end,
	method (fragment, fail, results)
	  if (fragment.more?) fail() else results end;
	end,
	#());
end;

define method match-rule (rule :: <abstract-define-rule>,
			  form :: <defining-form>,
			  fragment :: <fragment>,
			  intermediate-words :: <simple-object-vector>)
    => res :: false-or(<list>);
  let modifiers-fragment = make(<fragment>);
  for (modifier in form.define-modifiers)
    postpend-piece(modifiers-fragment,
		   make(<piece>, token: modifier));
  end;
  match(rule.define-rule-modifiers-pattern, modifiers-fragment, #[],
	method () #f end,
	method (modifiers-fragment, fail, results)
	  if (modifiers-fragment.more?)
	    fail();
	  else
	    match(rule.rule-pattern, fragment, intermediate-words,
		  method () #f end,
		  method (fragment, fail, results)
		    if (fragment.more?) fail() else results end;
		  end,
		  results);
	  end;
	end,
	#());
end;

define method more? (fragment :: <fragment>) => res :: <boolean>;
  let head = fragment.fragment-head;
  head & head.piece-prev ~= fragment.fragment-tail;
end;

define method next-token (fragment :: <fragment>) => res;
  fragment.fragment-head.piece-token;
end;

define method consume-token (fragment :: <fragment>) => res :: <fragment>;
  make(<fragment>,
       head: fragment.fragment-head.piece-next,
       tail: fragment.fragment-tail);
end;

define method add-binding (name :: false-or(<symbol>),
			   fragment :: type-union(<fragment>, <vector>),
			   other-results :: <list>)
    => res :: <list>;
  pair(pair(name, fragment), other-results);
end;

define method add-binding (var :: <pattern-variable>, fragment :: <fragment>,
			   other-results :: <list>)
    => res :: <list>;
  add-binding(var.patvar-name, fragment, other-results);
end;

define method add-binding (var :: <pattern-keyword>, fragment :: <fragment>,
			   other-results :: <list>)
    => res :: <list>;
  add-binding(var.patkey-name, fragment, other-results);
end;



define method match (pattern :: <pattern>, fragment :: <fragment>,
		     intermediate-words :: <simple-object-vector>,
		     fail :: <function>, continue :: <function>,
		     results :: <list>)
    => res :: false-or(<list>);
  match-pieces(pattern.pattern-pieces, <semicolon-token>, fragment,
	       intermediate-words, fail, continue, results);
end;

define method match (pattern :: <pattern-list>, fragment :: <fragment>,
		     intermediate-words :: <simple-object-vector>,
		     fail :: <function>, continue :: <function>,
		     results :: <list>)
    => res :: false-or(<list>);
  match-pieces(pattern.pattern-list-pieces, <comma-token>, fragment,
	       intermediate-words, fail, continue, results);
end;

define method match (pattern :: <pattern-sequence>, fragment :: <fragment>,
		     intermediate-words :: <simple-object-vector>,
		     fail :: <function>, continue :: <function>,
		     results :: <list>)
    => res :: false-or(<list>);
  match-pieces(pattern.pattern-sequence-pieces, #f, fragment,
	       intermediate-words, fail, continue, results);
end;


define method match-pieces (pieces, separator, fragment,
			    intermediate-words :: <simple-object-vector>,
			    fail, continue, results)
  local
    method match-pieces-aux (this-piece, fragment, fail, results)
      match(pieces[this-piece], fragment, intermediate-words, fail,
	    method (fragment, fail, results)
	      let next-piece = this-piece + 1;
	      if (next-piece == pieces.size)
		continue(fragment, fail, results);
	      elseif (~separator)
		match-pieces-aux(next-piece, fragment, fail, results);
	      elseif (fragment.more?
			& instance?(fragment.next-token, separator))
		match-pieces-aux(next-piece, consume-token(fragment),
				 fail, results);
	      elseif (next-piece == pieces.size - 1)
		match-empty(pieces[next-piece], fail,
			    method (results)
			      continue(fragment, fail, results);
			    end,
			    results);
	      else
		fail();
	      end;
	    end,
	    results);
    end;
  if (pieces.empty?)
    continue(fragment, fail, results);
  else
    match-pieces-aux(0, fragment, fail, results);
  end;
end;


define method match-empty (pattern :: <pattern>, fail, continue, results)
  match-empty-pieces (pattern.pattern-pieces, fail, continue, results);
end;

define method match-empty (pattern :: <pattern-list>, fail, continue, results)
  match-empty-pieces (pattern.pattern-list-pieces, fail, continue, results);
end;

define method match-empty (pattern :: <pattern-sequence>, fail, continue,
			   results)
  match-empty-pieces(pattern.pattern-sequence-pieces, fail, continue,
		     results);
end;

define method match-empty-pieces (pieces, fail, continue, results)
  if (pieces.size == 1)
    match-empty(pieces[0], fail, continue, results);
  else
    fail();
  end;
end;

define method match-empty (pattern :: <simple-pattern>, fail, continue,
			   results)
  fail();
end;

define method match-empty (pattern :: <pattern-variable>, fail, continue,
			   results)
  if (pattern.patvar-wildcard?
	| pattern.patvar-constraint == #"body"
	| pattern.patvar-constraint == #"case-body")
    continue(add-binding(pattern, make(<fragment>), results));
  else
    fail();
  end;
end;

define method match-empty (pattern :: <property-list-pattern>, fail, continue,
			   results)
  block (return)
    if (pattern.plistpat-rest)
      results := add-binding(pattern.plistpat-rest, make(<fragment>), results);
    end;
    if (pattern.plistpat-keys)
      for (key in pattern.plistpat-keys)
	if (key.patkey-default)
	  let fragment = make(<fragment>);
	  postpend-piece(fragment, make(<piece>, token: key.patkey-default));
	  results := add-binding(key.patkey-name,
				 if (key.patkey-all?)
				   vector(fragment);
				 else
				   fragment;
				 end,
				 results);
	else
	  if (key.patkey-all?)
	    results := add-binding(key.patkey-name, #[], results);
	  else
	    return(fail());
	  end;
	end;
      end;
    end;
    continue(results);
  end;
end;

define method match (pattern :: <details-pattern>, fragment :: <fragment>,
		     intermediate-words :: <simple-object-vector>,
		     fail :: <function>, continue :: <function>,
		     results :: <list>)
    => res :: false-or(<list>);
  if (fragment.more? & instance?(fragment.next-token, <left-paren-token>))
    let left = fragment.fragment-head;
    let right = left.piece-other;
    match(pattern.pattern-sub-pattern,
	  make(<fragment>, head: left.piece-next, tail: right.piece-prev),
	  intermediate-words, fail,
	  method (remaining-guts-fragment, fail, results)
	    if (remaining-guts-fragment.more?)
	      fail();
	    else
	      continue(make(<fragment>,
			    head: right.piece-next,
			    tail: fragment.fragment-tail),
		       fail,
		       results);
	    end;
	  end,
	  results);
  else
    fail();
  end;
end;

define method match (pattern :: <identifier-pattern>, fragment :: <fragment>,
		     intermediate-words :: <simple-object-vector>,
		     fail :: <function>, continue :: <function>,
		     results :: <list>)
    => res :: false-or(<list>);
  if (fragment.more?)
    let token = fragment.next-token;
    if (instance?(token, <identifier-token>)
	  & token.token-symbol == pattern.pattern-identifier.token-symbol)
      continue(consume-token(fragment), fail, results);
    else
      fail();
    end;
  else
    fail();
  end;
end;

define method match (pattern :: <variable-pattern>, fragment :: <fragment>,
		     intermediate-words :: <simple-object-vector>,
		     fail :: <function>, continue :: <function>,
		     results :: <list>)
    => res :: false-or(<list>);
  if (fragment.more? & instance?(fragment.next-token, <name-token>))
    let results = add-binding(pattern.variable-name-pattern,
			      make(<fragment>,
				   head: fragment.fragment-head,
				   tail: fragment.fragment-head),
			      results);
    let fragment = consume-token(fragment);
    local method no-type ()
	    let object-token = make(<name-token>,
				    symbol: #"<object>",
				    module: $Dylan-Module);
	    let piece = make(<piece>, token: object-token);
	    continue(fragment, fail,
		     add-binding(pattern.variable-type-pattern,
				 make(<fragment>, head: piece, tail: piece),
				 results));
	  end;
    if (fragment.more? & instance?(fragment.next-token, <double-colon-token>))
      let (type-guess, remaining-guess)
	= guess-extent-of(consume-token(fragment), <type-part-type>);
      let (type, type-fragment, remaining)
	= trim-until-parsable(type-guess, remaining-guess, parse-type);
      if (type)
	continue(remaining, fail,
		 add-binding(pattern.variable-type-pattern,
			     type-fragment, results));
      else
	no-type();
      end;
    else
      no-type();
    end;
  else
    fail();
  end;
end;

define method match (pattern :: <bound-variable-pattern>,
		     fragment :: <fragment>,
		     intermediate-words :: <simple-object-vector>,
		     fail :: <function>,
		     continue :: <function>, results :: <list>)
    => res :: false-or(<list>);
  match(pattern.bound-variable-variable, fragment, intermediate-words, fail,
	method (fragment, fail, results)
	  if (fragment.more? & instance?(fragment.next-token, <equal-token>))
	    match(pattern.bound-variable-value, consume-token(fragment),
		  intermediate-words, fail, continue, results);
	  else
	    fail();
	  end;
	end,
	results);
end;

define method match (pattern :: <pattern-variable>, fragment :: <fragment>,
		     intermediate-words :: <simple-object-vector>,
		     fail :: <function>, continue :: <function>,
		     results :: <list>)
    => res :: false-or(<list>);
  select (pattern.patvar-constraint)
    #"expr" =>
      let (expr-guess, remaining-guess)
	= guess-extent-of(fragment, <expr-part-type>);
      let (expr, expr-fragment, remaining)
	= trim-until-parsable(expr-guess, remaining-guess, parse-expression);
      if (expr)
	continue(remaining, fail,
		 add-binding(pattern, expr-fragment, results));
      else
	fail();
      end;
    #"var" =>
      let (var-guess, remaining-guess)
	= guess-extent-of(fragment, <var-part-type>);
      let (var, var-fragment, remaining)
	= trim-until-parsable(var-guess, remaining-guess, parse-variable);
      if (var)
	continue(remaining, fail, add-binding(pattern, var-fragment, results));
      else
	fail();
      end;
    #"name" =>
      if (fragment.more? & instance?(fragment.next-token, <name-token>))
	continue(consume-token(fragment), fail,
		 add-binding(pattern,
			     make(<fragment>,
				  head: fragment.fragment-head,
				  tail: fragment.fragment-head),
			     results));
      else
	fail();
      end;
    #"body" =>
      let (body-guess, remaining-guess)
	= find-intermediate-word(fragment, intermediate-words);
      match-variable(pattern, body-guess, remaining-guess, parse-body,
		     fail, continue, results);
    #"case-body" =>
      let (body-guess, remaining-guess)
	= find-intermediate-word(fragment, intermediate-words);
      match-variable(pattern, body-guess, remaining-guess, parse-case-body,
		     fail, continue, results);
    #f =>
      if (pattern.patvar-wildcard?)
	if (pattern.patvar-at-end?)
	  continue(make(<fragment>), fail,
		   add-binding(pattern, fragment, results));
	else
	  match-wildcard(pattern, fragment, fail, continue, results);
	end;
      elseif (fragment.more?)
	let head = fragment.fragment-head;
	if (instance?(head, <balanced-piece>))
	  let tail = head.piece-other;
	  continue(make(<fragment>,
			head: tail.piece-next,
			tail: fragment.fragment-tail),
		   fail,
		   add-binding(pattern,
			       make(<fragment>, head: head, tail: tail),
			       results));
	else
	  continue(consume-token(fragment), fail,
		   add-binding(pattern,
			       make(<fragment>, head: head, tail: head),
			       results));
	end;
      else
	fail();
      end;
  end;
end;

define method match-variable (pattern :: <pattern-variable>,
			      fragment :: <fragment>, remaining :: <fragment>,
			      parser :: <function>, fail :: <function>,
			      continue :: <function>, results :: <list>)
    => res :: false-or(<list>);
  let fail = if (fragment.more?)
	       method ()
		 let prev-piece = fragment.fragment-tail;
		 if (instance?(prev-piece, <balanced-piece>))
		   prev-piece := prev-piece.piece-other;
		 end;
		 match-variable(pattern,
				make(<fragment>,
				     head: fragment.fragment-head,
				     tail: prev-piece.piece-prev),
				make(<fragment>,
				     head: prev-piece,
				     tail: remaining.fragment-tail),
				parser, fail, continue, results);
	       end;
	     else
	       fail;
	     end;
  block ()
    parser(make(<fragment-tokenizer>, fragment: fragment));
    continue(remaining, fail, add-binding(pattern, fragment, results));
  exception <error>
    fail();
  end;
end;

define method match-wildcard (pattern :: <pattern-variable>,
			      fragment :: <fragment>, fail :: <function>,
			      continue :: <function>, results :: <list>)
    => res :: false-or(<list>);
  local method match-wildcard-aux (split :: <piece>)
	  if (instance?(split, <balanced-piece>))
	    split := split.piece-other;
	  end;
	  let matched-fragment
	    = make(<fragment>, head: fragment.fragment-head, tail: split);
	  let remaining-fragment
	    = make(<fragment>,
		   head: split.piece-next,
		   tail: fragment.fragment-tail);
	  continue(remaining-fragment,
		   if (split == fragment.fragment-tail)
		     fail;
		   else
		     curry(match-wildcard-aux, split.piece-next);
		   end,
		   add-binding(pattern, matched-fragment, results));
	end;
  continue(fragment,
	   if (fragment.more?)
	     curry(match-wildcard-aux, fragment.fragment-head);
	   else
	     fail;
	   end,
	   add-binding(pattern, make(<fragment>), results));
end;

define method match (pattern :: <otherwise-pattern>, fragment :: <fragment>,
		     intermediate-words :: <simple-object-vector>,
		     fail :: <function>, continue :: <function>,
		     results :: <list>)
    => res :: false-or(<list>);
  if (fragment.more? & instance?(fragment.next-token, <otherwise-token>))
    let fragment = consume-token(fragment);
    if (fragment.more? & instance?(fragment.next-token, <arrow-token>))
      continue(consume-token(fragment), fail, results);
    else
      continue(fragment, fail, results);
    end;
  else
    fail();
  end;
end;

define method match (pattern :: <arrow-pattern>, fragment :: <fragment>,
		     intermediate-words :: <simple-object-vector>,
		     fail :: <function>, continue :: <function>,
		     results :: <list>)
    => res :: false-or(<list>);
  if (fragment.more? & instance?(fragment.next-token, <arrow-token>))
    continue(consume-token(fragment), fail, results);
  else
    fail();
  end;
end;

define method match (pattern :: <property-list-pattern>,
		     fragment :: <fragment>,
		     intermediate-words :: <simple-object-vector>,
		     fail :: <function>, continue :: <function>,
		     results :: <list>)
    => res :: false-or(<list>);
  block (return)
    let (plist-frag-guess, remaining-guess)
      = guess-extent-of(fragment, <plist-part-type>);
    let (plist, plist-frag, remaining)
      = trim-until-parsable(plist-frag-guess, remaining-guess,
			    parse-property-list);
    unless (plist)
      return(fail());
    end;
    if (pattern.plistpat-rest)
      results := add-binding(pattern.plistpat-rest, plist-frag, results);
    end;
    if (pattern.plistpat-keys)
      unless (pattern.plistpat-all-keys?)
	for (prop in plist)
	  block (okay)
	    for (key in pattern.plistpat-keys)
	      if (prop.prop-keyword.token-literal.literal-value
		    == key.patkey-name)
		okay();
	      end;
	    end;
	    return(fail());
	  end;
	end;
      end;
      for (key in pattern.plistpat-keys)
	if (key.patkey-all?)
	  let this-result = make(<stretchy-vector>);
	  for (prop in plist)
	    if (prop.prop-keyword.token-literal.literal-value
		  == key.patkey-name)
	      let piece = make(<piece>, token: prop.prop-value);
	      let frag = make(<fragment>, head: piece, tail: piece);
	      add!(this-result, frag);
	    end;
	  end;
	  if (empty?(this-result) & key.patkey-default)
	    add!(this-result, key.patkey-default);
	  end;
	  results := add-binding(key.patkey-name,
				 as(<simple-object-vector>, this-result),
				 results);
	else
	  block (found-key)
	    for (prop in plist)
	      if (prop.prop-keyword.token-literal.literal-value == key.patkey-name)
		let piece = make(<piece>, token: prop.prop-value);
		let frag = make(<fragment>, head: piece, tail: piece);
		results := add-binding(key.patkey-name, frag, results);
		found-key();
	      end;
	    end;
	    if (key.patkey-default)
	      let piece = make(<piece>, token: key.patkey-default);
	      let frag = make(<fragment>, head: piece, tail: piece);
	      results := add-binding(key.patkey-name, frag, results);
	    else
	      return(fail());
	    end;
	  end;
	end;
      end;
    end;
    continue(remaining, fail, results);
  end;
end;




define method expand-template (template :: <template>,
			       bindings :: <list>,
			       this-rule-set :: false-or(<symbol>),
			       aux-rule-sets :: <simple-object-vector>,
			       intermediate-words :: <simple-object-vector>,
			       uniquifier :: <uniquifier>)
  //
  // First, expand out any recursive rules.
  for (binding in bindings)
    let name = binding.head | this-rule-set;
    unless (name)
      error("Can't use ... in the main rule set.");
    end;
    let aux-rule-set = find-aux-rule-set(name, aux-rule-sets);
    if (aux-rule-set)
      block (return)
	for (rule in aux-rule-set.rule-set-rules)
	  let results = match-rule(rule, #f, binding.tail, intermediate-words);
	  if (results)
	    binding.tail := expand-template(rule.rule-template, results, name,
					    aux-rule-sets, intermediate-words,
					    uniquifier);
	    return();
	  end;
	end;
	error("None of the rules for auxiliary rule set %= matched.", name);
      end;
    end;
  end;
  //
  // Produce a new fragment built from the template.
  let result = make(<fragment>);
  for (piece in template.template-parts)
    expand-template-aux(piece, bindings, this-rule-set, uniquifier, result);
  end;
  result;
end;

define method expand-template-aux (piece :: <pattern-variable-reference>,
				   bindings :: <list>,
				   this-rule-set :: false-or(<symbol>),
				   uniquifier :: <uniquifier>,
				   result :: <fragment>)
    => ();
  block (return)
    let name = piece.patvarref-name | this-rule-set;
    for (binding in bindings)
      if ((binding.head | this-rule-set) == name)
	local
	  method append-frag (frag :: <fragment>)
	    let tail-piece = frag.fragment-tail;
	    for (new-piece = frag.fragment-head then new-piece.piece-next,
		 while: (new-piece & new-piece.piece-prev ~= tail-piece))
	      postpend-piece(result,
			     make(<piece>, token: new-piece.piece-token));
	    end;
	  end,
	  method maybe-nuke-separator ()
	    if (result.fragment-tail)
	      let prev-token = result.fragment-tail.piece-token;
	      if (instance?(prev-token, <binary-operator-token>)
		    | instance?(prev-token, <semicolon-token>)
		    | instance?(prev-token, <comma-token>))
		result.fragment-tail := result.fragment-tail.piece-prev;
	      end;
	    end;
	  end;
	if (instance?(binding.tail, <fragment>))
	  if (more?(binding.tail))
	    append-frag(binding.tail);
	  else
	    maybe-nuke-separator();
	  end;
	else
	  if (empty?(binding.tail))
	    maybe-nuke-separator();
	  else
	    let separator = piece.patvarref-separator;
	    for (frag in binding.tail,
		 first? = #t then #f)
	      append-frag(frag);
	      if (separator & ~first?)
		postpend-piece(result, make(<piece>, token: separator));
	      end;
	    end;
	  end;
	end;
	return();
      end;
    end;
    error("Unbound pattern variable: %=", name);
  end;
end;

define method expand-template-aux (piece :: <paren-template>,
				   bindings :: <list>,
				   this-rule-set :: false-or(<symbol>),
				   uniquifier :: <uniquifier>,
				   result :: <fragment>)
    => ();
  let left = make(<balanced-piece>, token: piece.template-left-token);
  let right = make(<balanced-piece>, token: piece.template-right-token);
  left.piece-other := right;
  right.piece-other := left;
  postpend-piece(result, left);
  for (sub-piece in piece.template-parts)
    expand-template-aux(sub-piece, bindings, this-rule-set,
			uniquifier, result);
  end;
  postpend-piece(result, right);
end;

define method expand-template-aux (piece :: <token>,
				   bindings :: <list>,
				   this-rule-set :: false-or(<symbol>),
				   uniquifier :: <uniquifier>,
				   result :: <fragment>)
    => ();
  postpend-piece(result, make(<piece>, token: piece));
end;

define method expand-template-aux (piece :: <identifier-token>,
				   bindings :: <list>,
				   this-rule-set :: false-or(<symbol>),
				   uniquifier :: <uniquifier>,
				   result :: <fragment>)
    => ();
  let mod = piece.token-module;
  postpend-piece(result,
		 make(<piece>,
		      token: make(if (mod)
				    element(mod.module-syntax-table,
					    piece.token-symbol,
					    default: <name-token>);
				  else
				    object-class(piece);
				  end,
				  source-location: piece.source-location,
				  symbol: piece.token-symbol,
				  module: mod,
				  uniquifier: uniquifier)));
end;

define method expand-template-aux (piece :: <quoted-name-token>,
				   bindings :: <list>,
				   this-rule-set :: false-or(<symbol>),
				   uniquifier :: <uniquifier>,
				   result :: <fragment>)
    => ();
  postpend-piece(result,
		 make(<piece>,
		      token: make(<quoted-name-token>,
				  source-location: piece.source-location,
				  symbol: piece.token-symbol,
				  module: piece.token-module,
				  uniquifier: uniquifier)));
end;

define method expand-template-aux (piece :: <operator-token>,
				   bindings :: <list>,
				   this-rule-set :: false-or(<symbol>),
				   uniquifier :: <uniquifier>,
				   result :: <fragment>)
    => ();
  postpend-piece(result,
		 make(<piece>,
		      token: make(object-class(piece),
				  source-location: piece.source-location,
				  symbol: piece.token-symbol,
				  module: piece.token-module,
				  uniquifier: uniquifier)));
end;


// Dump/Load stuff.

define constant $define-macro-slots
  = concatenate($definition-slots,
		list(macro-intermediate-words, intermediate-words:, #f,
		     macro-main-rule-set, main-rule-set:, #f,
		     macro-auxiliary-rule-sets, auxiliary-rule-sets:, #f));

add-make-dumper(#"define-bindings-macro-definition", *compiler-dispatcher*,
		<define-bindings-macro-definition>,
		$define-macro-slots, load-external: #t);
add-make-dumper(#"define-macro-definition", *compiler-dispatcher*,
		<define-macro-definition>,
		$define-macro-slots, load-external: #t);
add-make-dumper(#"function-macro-definition", *compiler-dispatcher*,
		<function-macro-definition>,
		$define-macro-slots, load-external: #t);
add-make-dumper(#"statement-macro-definition", *compiler-dispatcher*,
		<statement-macro-definition>,
		$define-macro-slots, load-external: #t);
