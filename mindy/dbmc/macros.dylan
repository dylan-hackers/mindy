module: macros
copyright: see below

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000, 2001  Gwydion Dylan Maintainers
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
//    University, and the Gwydion Dylan Maintainers.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
// Also, see http://www.gwydiondylan.org/ for updates and documentation. 
//
//======================================================================

// symcat -- internal
//
define method symcat (#rest things) => res :: <symbol>;
  as(<symbol>, apply(concatenate, "", map(curry(as, <string>), things)));
end;

// <macro-definition>
//
// Some kind of macro definition.
// 
define abstract class <macro-definition> (<definition>)
  //
  // The intermediate words.  Computed when the top level form is processed.
  slot macro-intermediate-words :: <simple-object-vector> = #[],
    init-keyword: intermediate-words:;
  //
  // The main rule set.
  constant slot macro-main-rule-set :: <main-rule-set>,
    required-init-keyword: main-rule-set:;
  //
  // The auxiliary rule sets.
  constant slot macro-auxiliary-rule-sets :: <simple-object-vector>,
    required-init-keyword: auxiliary-rule-sets:;
end class <macro-definition>;

define sealed domain make (singleton(<macro-definition>));
define sealed domain initialize (<macro-definition>);

// <define-body-macro-definition>
//
// A body-style define macro definition.
// 
define class <define-body-macro-definition> (<macro-definition>)
end class <define-body-macro-definition>;

define sealed domain make (singleton(<define-body-macro-definition>));

// <define-list-macro-definition>
//
// A list-style define macro definition.
// 
define class <define-list-macro-definition> (<macro-definition>)
end class <define-list-macro-definition>;

define sealed domain make (singleton(<define-list-macro-definition>));

// <statement-macro-definition>
//
// A statement macro definition.
// 
define class <statement-macro-definition> (<macro-definition>)
end class <statement-macro-definition>;

define sealed domain make (singleton(<statement-macro-definition>));

// <function-macro-definition>
//
// A function macro definition.
// 
define class <function-macro-definition> (<macro-definition>)
end class <function-macro-definition>;

define sealed domain make (singleton(<function-macro-definition>));


// Definition information query routines.

// definition-syntax-info{<define-body-macro-definition>}
//   -- method on imported GF
//
// For body-style define macros, strip off the -definer.
// 
define method definition-syntax-info
    (defn :: <define-body-macro-definition>, name :: <symbol>)
    => (real-name :: false-or(<symbol>), category :: <symbol>);
  values(sans-definer(name), #"define-body");
end method definition-syntax-info;

// definition-syntax-info{<define-list-macro-definition>}
//   -- method on imported GF
//
// For list-style define macros, strip off the -definer.
// 
define method definition-syntax-info
    (defn :: <define-list-macro-definition>, name :: <symbol>)
    => (real-name :: false-or(<symbol>), category :: <symbol>);
  values(sans-definer(name), #"define-list");
end method definition-syntax-info;

// definition-syntax-info{<function-macro-definition>} -- method on imported GF
//
// For function macros, use the name as is.
// 
define method definition-syntax-info
    (defn :: <function-macro-definition>, name :: <symbol>)
    => (real-name :: <symbol>, category :: <symbol>);
  values(name, #"function");
end method definition-syntax-info;

// definition-syntax-info{<statement-macro-definition>}
//   -- method on imported GF
//
// For statement macros, use the name as is.
// 
define method definition-syntax-info
    (defn :: <statement-macro-definition>, name :: <symbol>)
    => (real-name :: <symbol>, category :: <symbol>);
  values(name, #"begin");
end method definition-syntax-info;


// sans-definer -- internal.
//
// Strip off the -definer if it is there and return #f if it isn't.
// 
define method sans-definer (name :: <symbol>)
    => res :: false-or(<symbol>);
  let name-str = as(<string>, name);
  let name-size = name-str.size;
  if (name-size > 8)
    block (return)
      for (i from name-size - 8, char in "-definer")
	unless (as-lowercase(name-str[i]) == char)
	  return(#f);
	end unless;
      end for;
      as(<symbol>, copy-sequence(name-str, end: name-size - 8));
    end block;
  else
    #f;
  end if;
end method sans-definer;


// definition-kind{<define-body-macro-definition>} -- method on imported GF
//
define method definition-kind
    (defn :: <define-body-macro-definition>) => kind :: <byte-string>;
  "body-style define macro";
end method definition-kind;

// definition-kind{<define-list-macro-definition>} -- method on imported GF
//
define method definition-kind
    (defn :: <define-list-macro-definition>) => kind :: <byte-string>;
  "list-style define macro";
end method definition-kind;

// definition-kind{<function-macro-definition>} -- method on imported GF
//
define method definition-kind
    (defn :: <function-macro-definition>) => kind :: <byte-string>;
  "function macro";
end method definition-kind;

// definition-kind{<statement-macro-definition>} -- method on imported GF
//
define method definition-kind
    (defn :: <statement-macro-definition>) => kind :: <byte-string>;
  "statement macro";
end method definition-kind;



// make(<macro-definition>) -- method on imported GF.
//
define method make
    (class == <macro-definition>,
     #key module :: <module>, library :: <library>,
          defmacro :: <define-macro-parse>,
          source-location :: <source-location> = defmacro.source-location)
    => defn :: <macro-definition>;
  let rules = defmacro.defmacro-main-rule-set.rule-set-rules;
  let first-rule = rules.first;

  for (rule in rules)
    unless (rule.object-class == first-rule.object-class)
      compiler-fatal-error-location(defmacro, "Inconsistent rule styles");
    end unless;
  end for;

  let defn-class
    = select (first-rule by instance?)
	<body-style-define-rule> =>
	  fix-define-rules(defmacro);
	  <define-body-macro-definition>;
	<list-style-define-rule> =>
	  fix-define-rules(defmacro);
	  <define-list-macro-definition>;
	<function-rule> =>
	  <function-macro-definition>;
	<statement-rule> =>
	  <statement-macro-definition>;
      end select;

  let name = defmacro.defmacro-name;
  let defn = make(defn-class,
		  name: make(<basic-name>,
			     symbol: name.token-symbol,
			     module: module),
		  source-location: source-location,
		  library: library,
		  main-rule-set: defmacro.defmacro-main-rule-set,
		  auxiliary-rule-sets: defmacro.defmacro-auxiliary-rule-sets);
  annotate-variables(defn);
  find-intermediate-words(defn);
  defn;
end method make;



// fix-define-rules

// fix-define-rules -- internal.
//
// Extract the modifiers pattern and the name from the head of each rule.
// Puke if we can't find the name.
//
define method fix-define-rules (defmacro :: <define-macro-parse>) => ();
  let name = sans-definer(defmacro.defmacro-name.token-symbol);
  unless (name)
    compiler-fatal-error("Name of define macro doesn't end with -definer: %s",
			 defmacro.defmacro-name);
  end unless;
  for (rule in defmacro.defmacro-main-rule-set.rule-set-rules)
    let (modifiers-pattern, name, remaining-pattern)
      = trim-modifiers-pattern(rule.rule-pattern, name);
    rule.define-rule-modifiers-pattern := modifiers-pattern;
    rule.rule-pattern := remaining-pattern;
  end for;
end method fix-define-rules;


// trim-modifiers-pattern -- internal.
//
// Grovel though pattern trying to find name.  Return the modifiers pattern
// (the stuff that preceeds name), the token that matched name, and the
// remaining stuff from the pattern.
// 
define generic trim-modifiers-pattern
    (pattern :: <pattern>, name :: <symbol>)
    => (modifiers-pattern :: <pattern>, name :: <symbol-token>,
	remaining-pattern :: <pattern>);

// trim-modifiers-pattern{<pattern>}
//
// Catch-all method for patterns that can't be part of the modifier-pattern.
// Puke because it means we couldn't find the name.
// 
define method trim-modifiers-pattern
    (pattern :: <pattern>, name :: <symbol>)
    => (modifiers-pattern :: <pattern>, name :: <symbol-token>,
	remaining-pattern :: <pattern>);
  compiler-fatal-error("Can't find the macro name (%s) in the rule.", name);
end method trim-modifiers-pattern;

// trim-modifiers-pattern{<semicolon-pattern>}
//
// A semicolon can't occur in the modifiers pattern so the name must be down
// the left side, so extract the modifiers pattern from the left.  When
// returning, wrap the stuff left over on the left with a new semicolon pattern
// that concatenates it with the original right hand side.
// 
define method trim-modifiers-pattern
    (pattern :: <semicolon-pattern>, name :: <symbol>)
    => (modifiers-pattern :: <pattern>, name :: <symbol-token>,
	remaining-pattern :: <pattern>);
  let (modifiers-pattern, name, remaining-pattern)
    = trim-modifiers-pattern(pattern.pattern-left, name);
  values(modifiers-pattern,
	 name,
	 make(<semicolon-pattern>,
	      left: remaining-pattern,
	      right: pattern.pattern-right,
	      last: pattern.pattern-last?));
end method trim-modifiers-pattern;

// trim-modifiers-pattern{<comma-pattern>}
//
// A comma can't occur in the modifiers pattern so the name must be down
// the left side, so extract the modifiers pattern from the left.  When
// returning, wrap the stuff left over on the left with a new comma pattern
// that concatenates it with the original right hand side.
// 
define method trim-modifiers-pattern
    (pattern :: <comma-pattern>, name :: <symbol>)
    => (modifiers-pattern :: <pattern>, name :: <symbol-token>,
	remaining-pattern :: <pattern>);
  let (modifiers-pattern, name, remaining-pattern)
    = trim-modifiers-pattern(pattern.pattern-left, name);
  values(modifiers-pattern,
	 name,
	 make(<comma-pattern>,
	      left: remaining-pattern,
	      right: pattern.pattern-right,
	      last: pattern.pattern-last?));
end method trim-modifiers-pattern;

// trim-modifiers-pattern{<sequential-pattern>}
//
// Does most of the real work for this generic function.
//
define method trim-modifiers-pattern
    (pattern :: <sequential-pattern>, name :: <symbol>)
    => (modifiers-pattern :: <pattern>, name :: <symbol-token>,
	remaining-pattern :: <pattern>);
  let left = pattern.pattern-left;
  if (instance?(left, <name-pattern>))
    //
    // If the left is a name pattern, then it is either a literal modifier
    // (i.e. part of the modifier pattern) or it is the name.  
    if (left.pattern-name.token-symbol == name)
      //
      // The name matches.
      values(make(<empty-pattern>), left.pattern-name, pattern.pattern-right);
    else
      //
      // Recurse into the right hand side and prepend the name pattern onto
      // the start of the returned modifier pattern.  But take care that
      // we don't make a <sequential-pattern> with an empty right hand side.
      let (modifiers-pattern, name, remaining-pattern)
	= trim-modifiers-pattern(pattern.pattern-right, name);
      values(if (instance?(modifiers-pattern, <empty-pattern>))
	       left;
	     else
	       make(<sequential-pattern>,
		    left: left, right: modifiers-pattern,
		    last: ~instance?(modifiers-pattern, <sequential-pattern>));
	     end if,
	     name,
	     remaining-pattern);
    end if;
  elseif (instance?(left, <pattern-variable>))
    //
    // Otherwise, if the left is a pattern-variable, it is part of the
    // modifiers pattern.  So Recurse into the right hand side and
    // prepend the name pattern onto the start of the returned
    // modifier pattern.  But take care that we don't make a
    // <sequential-pattern> with an empty right hand side.
    let (modifiers-pattern, name, remaining-pattern)
      = trim-modifiers-pattern(pattern.pattern-right, name);
    values(if (instance?(modifiers-pattern, <empty-pattern>))
	     left;
	   else
	     make(<sequential-pattern>,
		  left: left, right: modifiers-pattern,
		  last: ~instance?(modifiers-pattern, <sequential-pattern>));
	   end if,
	   name,
	   remaining-pattern);
  else
    //
    // If the left is neither a name nor a pattern-varible, then there is
    // something wrong with the macro definition.
    compiler-fatal-error("Can't find the macro name (%s) in the rule.", name);
  end if;
end method trim-modifiers-pattern;

// trim-modifiers-pattern{<name-pattern>}
//
// If the name matches, then great.  Otherwise, puke because it means that
// we couldn't find the name anywhere.  We don't have to worry about this
// name being part of the modifiers-pattern, because that case is picked off
// above us in the <sequential-pattern> method.
// 
define method trim-modifiers-pattern
    (pattern :: <name-pattern>, name :: <symbol>)
    => (modifiers-pattern :: <pattern>, name :: <symbol-token>,
	remaining-pattern :: <pattern>);
  if (pattern.pattern-name.token-symbol == name)
    values(make(<empty-pattern>),
	   pattern.pattern-name,
	   make(<empty-pattern>));
  else
    compiler-fatal-error("Can't find the macro name (%s) in the rule.", name);
  end if;
end method trim-modifiers-pattern;


// find-intermediate-words
//
// This stuff finds all the intermediate words in a macro definition.  From
// the DRM:
//
//     * Define a body-variable to be a pattern variable that either has a
//       constraint of body or case-body, or names an auxiliary rule-set
//       where some left-hand side in that rule-set ends in a body-variable.
//       This is a least fixed point, so a recursive auxiliary rule-set does
//       not automatically make its name into a body-variable!  Note that an
//       ellipsis that stands for a pattern variable is a body-variable when
//       that pattern variable is one.
//
//     * Define an intermediate-variable to be a pattern variable that
//       either immediately follows a body-variable in a left-hand side,
//       or appears at the beginning of a left-hand side in an auxiliary
//       rule-set named by an itermediate-variable.
//
//     * An intermediate word is a name that either immediately follows a
//       body-variable in a left-hand side, or occurs at the beginning of a
//       left-hand side in an auxiliary rule-set named by an intermediate-
//       variable.  Intermediate words are not reserved, they are just used
//       as delimiters during the parsing for a pattern-variable with body
//       or case-body constraint.
//
// From that description, we derive the following algorithm.
// 
// The first thing we do is find all the auxiliary rule sets that end
// in body variables.  Note that when we identify a rule-set as ending
// in a body-variable, that means that any variable naming that
// rule-set is now a body-variable, so we have to iterate until we find
// no more rule sets ending in body variables.
//
// Then we scan over all the patterns looking for things that follow body
// variables.  We are interested in two: names (i.e. intermediate words) and
// pattern variables (i.e. intermediate-variables).  For intermediate-
// variables discovered in the manner, if it names some auxiliary rule set,
// then all of the patterns in that rule set also follow a body variable.
// 
define method find-intermediate-words (defn :: <macro-definition>)
    => res :: <simple-object-vector>;
  let aux-rule-sets = defn.macro-auxiliary-rule-sets;
  //
  // Find the rule sets that are named by body variables.
  let again? = #t;
  while (again?)
    again? := #f;
    for (rule-set in aux-rule-sets)
      for (rule in rule-set.rule-set-rules,
	   until: rule-set.rule-set-body-variable?)
	if (ends-in-body-variable?(rule.rule-pattern, rule-set.rule-set-name,
				   aux-rule-sets))
	  rule-set.rule-set-body-variable? := #t;
	  again? := #t;
	end if;
      end for;
    end for;
  end while;
  //
  // Find all the intermediate words.
  let results = #();
  for (rule in defn.macro-main-rule-set.rule-set-rules)
    results := find-intermediate-words-in
      (rule.rule-pattern, #f, aux-rule-sets, results);
  end for;
  for (aux-rule-set in aux-rule-sets)
    for (rule in aux-rule-set.rule-set-rules)
      results := find-intermediate-words-in
	(rule.rule-pattern, aux-rule-set.rule-set-name,
	 aux-rule-sets, results);
    end for;
  end for;
  //
  // Store 'em away for posterity.
  defn.macro-intermediate-words := as(<simple-object-vector>, results);
end method find-intermediate-words;

// ends-in-body-variable? -- internal.
//
// Return #t iff the pattern ends in a body variable.  Return #f iff not.
// 
define generic ends-in-body-variable?
    (pattern :: <pattern>, this-rule-set :: false-or(<symbol>),
     aux-rule-sets :: <simple-object-vector>)
    => res :: <boolean>;

// ends-in-body-variable?{<pattern>}
//
// Catch-all method that returns #f.  We explicitly enumerate the exceptions.
//
define method ends-in-body-variable?
    (pattern :: <pattern>, this-rule-set :: false-or(<symbol>),
     aux-rule-sets :: <simple-object-vector>)
    => res :: <boolean>;
  #f;
end method ends-in-body-variable?;

// ends-in-body-variable?{<binary-pattern>}
//
// A binary pattern ends in a body-variable if the right hand side ends
// in a body variable.
// 
define method ends-in-body-variable?
    (pattern :: <binary-pattern>, this-rule-set :: false-or(<symbol>),
     aux-rule-sets :: <simple-object-vector>)
    => res :: <boolean>;
  ends-in-body-variable?(pattern.pattern-right, this-rule-set, aux-rule-sets);
end method ends-in-body-variable?;

// ends-in-body-variable?{<pattern-variable>}
//
// A pattern variable ends in a body-variable if it is a body variable.
// And it is a body variable if the constraint is either body or case-body
// or it names a rule-set we have previously determined ends in a body
// variable.
// 
define method ends-in-body-variable?
    (patvar :: <pattern-variable>, this-rule-set :: false-or(<symbol>),
     aux-rule-sets :: <simple-object-vector>)
    => res :: <boolean>;
  let constraint = patvar.patvar-constraint;
  if (constraint == #"body" | constraint == #"case-body")
    #t;
  else
    let name = patvar.patvar-name | this-rule-set;
    if (name)
      let aux-rule-set = find-aux-rule-set(name, aux-rule-sets);
      aux-rule-set & aux-rule-set.rule-set-body-variable?;
    else
      #f;
    end if;
  end if;
end method ends-in-body-variable?;

// ends-in-body-variable?{<variable-pattern>}
//
// A typed-variable-pattern ends in a body variable if the type pattern
// ends in a body variable.  That would be a rather strange pattern, but
// the description of intermediate words doesn't disallow it, so we have
// to support it.
// 
define method ends-in-body-variable?
    (pattern :: <variable-pattern>, this-rule-set :: false-or(<symbol>),
     aux-rule-sets :: <simple-object-vector>)
    => res :: <boolean>;
  ends-in-body-variable?
    (pattern.variable-type-pattern, this-rule-set, aux-rule-sets);
end method ends-in-body-variable?;

// ends-in-body-variable?{<bindings-pattern>}
//
// A typed-variable-pattern ends in a body variable if the type pattern
// ends in a body variable.  That would be a rather strange pattern, but
// the description of intermediate words doesn't disallow it, so we have
// to support it.
// 
define method ends-in-body-variable?
    (pattern :: <bindings-pattern>, this-rule-set :: false-or(<symbol>),
     aux-rule-sets :: <simple-object-vector>)
    => res :: <boolean>;
  ends-in-body-variable?
    (pattern.bindings-value-pattern, this-rule-set, aux-rule-sets);
end method ends-in-body-variable?;


// find-aux-rule-set -- internal.
//
// Finds the auxiliary rule set named by symbol. This function is also used
// way down below in do-replacement, but this is as good of a place as any
// to define it.
// 
define method find-aux-rule-set
    (name :: <symbol>, aux-rule-sets :: <simple-object-vector>)
    => res :: false-or(<auxiliary-rule-set>);
  block (return)
    for (aux-rule-set in aux-rule-sets)
      if (aux-rule-set.rule-set-name == name)
	return(aux-rule-set);
      end if;
    end for;
    #f;
  end block;
end method find-aux-rule-set;


// find-intermediate-words-in -- internal.
//
// Find places in pattern where something follows a body-variable and call
// find-intermediate-words-at-start on that something.
// 
define generic find-intermediate-words-in
    (pattern :: <pattern>, this-rule-set :: false-or(<symbol>),
     aux-rule-sets :: <simple-object-vector>, results :: <list>)
    => res :: <list>;

// find-intermediate-words-in{<pattern>}
//
// Catch-all method for patterns that either can't have two adjacent pattern
// variables or a whole sub-pattern.  We explicitly enumerate the exceptions
// below.
// 
define method find-intermediate-words-in
    (pattern :: <pattern>, this-rule-set :: false-or(<symbol>),
     aux-rule-sets :: <simple-object-vector>, results :: <list>)
    => res :: <list>;
  results;
end method find-intermediate-words-in;

// find-intermediate-words-in{<binary-pattern>}
//
// Descend into the two halves.  This method assumes that there is a separator
// between the two halves so that it doesn't matter what the first half ends
// with.  The exception (<sequential-pattern>) is delt with below.
// 
define method find-intermediate-words-in
    (pattern :: <binary-pattern>, this-rule-set :: false-or(<symbol>),
     aux-rule-sets :: <simple-object-vector>, results :: <list>)
    => res :: <list>;
  find-intermediate-words-in
    (pattern.pattern-right, this-rule-set, aux-rule-sets,
     find-intermediate-words-in
       (pattern.pattern-left, this-rule-set, aux-rule-sets, results));
end method find-intermediate-words-in;

// find-intermediate-words-in{<sequential-pattern>}
//
// The two sub-patterns are adjacent, so first check to see if the left ends
// in a body variable.  If so, find the intermediate words at the start of
// the right sub-pattern.
//
// And in any case, descend into the two subpatterns just like the
// <binary-pattern> method above.
// 
define method find-intermediate-words-in
    (pattern :: <sequential-pattern>, this-rule-set :: false-or(<symbol>),
     aux-rule-sets :: <simple-object-vector>, results :: <list>)
    => res :: <list>;
  let left = pattern.pattern-left;
  let right = pattern.pattern-right;
  find-intermediate-words-in
    (right, this-rule-set, aux-rule-sets,
     find-intermediate-words-in
       (left, this-rule-set, aux-rule-sets,
	if (ends-in-body-variable?(left, this-rule-set, aux-rule-sets))
	  find-intermediate-words-at-start
	    (right, this-rule-set, aux-rule-sets, results);
	else
	  results;
	end if));
end method find-intermediate-words-in;

// find-intermediate-words-in{<bracketed-pattern>}
//
// Descend into the guts pattern.
// 
define method find-intermediate-words-in
    (pattern :: <bracketed-pattern>, this-rule-set :: false-or(<symbol>),
     aux-rule-sets :: <simple-object-vector>, results :: <list>)
    => res :: <list>;
  find-intermediate-words-in
    (pattern.pattern-guts, this-rule-set, aux-rule-sets, results);
end method find-intermediate-words-in;



// find-intermediate-words-at-start -- internal.
//
// Called whenever we have determined that some pattern ``follows'' a body
// variable.  A pattern can follow a body variable by physically being
// next to it in the pattern, or it can be the pattern for some rule in
// an auxiliary rule set named by a pattern variable that follows a body
// variable.
//
define generic find-intermediate-words-at-start
    (pattern :: <pattern>, this-rule-set :: false-or(<symbol>),
     aux-rule-sets :: <simple-object-vector>, results :: <list>)
    => res :: <list>;

// find-intermediate-words-at-start{<pattern>}
//
// Catch all method for patterns that can't start with intermediate words
// or variables.  The exceptions are explicitly enumerated below.
// 
define method find-intermediate-words-at-start
    (pattern :: <pattern>, this-rule-set :: false-or(<symbol>),
     aux-rule-sets :: <simple-object-vector>, results :: <list>)
    => res :: <list>;
  results;
end method find-intermediate-words-at-start;

// find-intermediate-words-at-start{<binary-pattern>}
//
// Find the intermediate words at the start of the left half.
// 
define method find-intermediate-words-at-start
    (pattern :: <binary-pattern>, this-rule-set :: false-or(<symbol>),
     aux-rule-sets :: <simple-object-vector>, results :: <list>)
    => res :: <list>;
  find-intermediate-words-at-start
    (pattern.pattern-left, this-rule-set, aux-rule-sets, results);
end method find-intermediate-words-at-start;

// find-intermediate-words-at-start{<name-pattern>}
//
// Hey, we found one!  If we don't already know about it, add it to the
// results.
// 
define method find-intermediate-words-at-start
    (pattern :: <name-pattern>, this-rule-set :: false-or(<symbol>),
     aux-rule-sets :: <simple-object-vector>, results :: <list>)
    => res :: <list>;
  let sym = pattern.pattern-name.token-symbol;
  if (member?(sym, results))
    results;
  else
    pair(sym, results);
  end if;
end method find-intermediate-words-at-start;

// find-intermediate-words-at-start{<pattern-variable>}
//
// Okay, we have an intermediate variable.  Check the start of the patterns
// for the corresponding auxiliary-rule-set (if there is one).
//
// We set a flag in the rule set before recursing so that we will eventually
// terminate.  Otherwise a right-recursive rule set would cause use to hang.
// For as long as we had stack space, that is.
// 
define method find-intermediate-words-at-start
    (pattern :: <pattern-variable>, this-rule-set :: false-or(<symbol>),
     aux-rule-sets :: <simple-object-vector>, results :: <list>)
    => res :: <list>;
  let name = pattern.patvar-name | this-rule-set;
  if (name)
    let aux-rule-set = find-aux-rule-set(name, aux-rule-sets);
    if (aux-rule-set & ~aux-rule-set.rule-set-processed-intermediate-words?)
      aux-rule-set.rule-set-processed-intermediate-words? := #t;
      for (rule in aux-rule-set.rule-set-rules)
	results := find-intermediate-words-at-start
	  (rule.rule-pattern, this-rule-set, aux-rule-sets, results);
      end for;
    end if;
  end if;
  results;
end method find-intermediate-words-at-start;

// find-intermediate-words-at-start{<variable-pattern>}
//
// We have to deal with this case even though it is rather strange.  Just
// recurse on the name pattern.
// 
define method find-intermediate-words-at-start
    (pattern :: <variable-pattern>, this-rule-set :: false-or(<symbol>),
     aux-rule-sets :: <simple-object-vector>, results :: <list>)
    => res :: <list>;
  find-intermediate-words-at-start
    (pattern.variable-name-pattern, this-rule-set, aux-rule-sets, results);
end method find-intermediate-words-at-start;

// find-intermediate-words-at-start{<bindings-pattern>}
//
// We have to deal with this case even though it is rather strange.  Just
// recurse on the variable pattern.
// 
define method find-intermediate-words-at-start
    (pattern :: <bindings-pattern>, this-rule-set :: false-or(<symbol>),
     aux-rule-sets :: <simple-object-vector>, results :: <list>)
    => res :: <list>;
  find-intermediate-words-at-start
    (pattern.bindings-variables-pattern, this-rule-set, aux-rule-sets,
     results);
end method find-intermediate-words-at-start;



// annotate-variables -- internal.
//
// We make some annotations on variables when the macro is defined so that
// we don't have to expensively recompute these values each time the macro
// is used.  The two annotations we currently make are ``at-end?'' for wildcard
// variables that must consume the entire fragment they are matched against
// and defaulting the constraint to wildcard for variables that name
// auxiliary rule sets.
//
define method annotate-variables
    (defn :: <macro-definition>) => ();
  let aux-rule-sets = defn.macro-auxiliary-rule-sets;
  for (rule in defn.macro-main-rule-set.rule-set-rules)
    annotate-variables-rule(rule, aux-rule-sets);
  end for;
  for (rule-set in aux-rule-sets)
    for (rule in rule-set.rule-set-rules)
      annotate-variables-rule(rule, aux-rule-sets);
    end for;
  end for;
end method annotate-variables;


// annotate-variables-rule
//
// Annotate a rule.
// 
define generic annotate-variables-rule
    (within :: <rule>, aux-rule-sets :: <simple-object-vector>) => ();

// annotate-variables-rule{<rule>}
//
// By default, just annotate the rule's pattern.
// 
define method annotate-variables-rule
    (rule :: <rule>, aux-rule-sets :: <simple-object-vector>) => ();
  annotate-variables-pattern(rule.rule-pattern, aux-rule-sets, #t);
end method annotate-variables-rule;

// annotate-variables-rule{<abstract-define-rule>}
//
// But for define-rules, we want to annotate the modifiers pattern also.
// 
define method annotate-variables-rule
    (rule :: <define-rule>, aux-rule-sets :: <simple-object-vector>)
    => ();
  annotate-variables-pattern
    (rule.define-rule-modifiers-pattern, aux-rule-sets, #t);
  annotate-variables-pattern(rule.rule-pattern, aux-rule-sets, #t);
end method annotate-variables-rule;


// annotate-variables-pattern
//
// Annotate the variables in some pattern.
// 
define generic annotate-variables-pattern
    (within :: <pattern>, aux-rule-sets :: <simple-object-vector>,
     at-end? :: <boolean>)
    => ();

// annotate-variables-pattern{<pattern>}
//
// Catch-all method for patterns that contain no variables.  Exceptions below.
// 
define method annotate-variables-pattern
    (pattern :: <pattern>, aux-rule-sets :: <simple-object-vector>,
     at-end? :: <boolean>)
    => ();
end method annotate-variables-pattern;

// annotate-variables-pattern{type-union(<semicolon-pattern>, <comma-pattern>)}
//
// Annotate the two sub-patterns.  The ; or , form hard delimiters that
// wildcards can't pass, pass at-end down as #t.
// 
define method annotate-variables-pattern
    (pattern :: type-union(<semicolon-pattern>, <comma-pattern>),
     aux-rule-sets :: <simple-object-vector>,
     at-end? :: <boolean>)
    => ();
  assert(at-end?);
  annotate-variables-pattern(pattern.pattern-left, aux-rule-sets, #t);
  annotate-variables-pattern(pattern.pattern-right, aux-rule-sets, #t);
end method annotate-variables-pattern;
				 
// annotate-variables-pattern{<sequential-pattern>}
//
// Annotate the two sub-patterns.
// 
define method annotate-variables-pattern
    (pattern :: <sequential-pattern>, aux-rule-sets :: <simple-object-vector>,
     at-end? :: <boolean>)
    => ();
  assert(at-end?);
  annotate-variables-pattern(pattern.pattern-left, aux-rule-sets, #f);
  annotate-variables-pattern(pattern.pattern-right, aux-rule-sets, #t);
end method annotate-variables-pattern;

// annotate-variables-pattern{<bracketed-pattern>}
//
// Annotate the guts.
// 
define method annotate-variables-pattern
    (pattern :: <bracketed-pattern>, aux-rule-sets :: <simple-object-vector>,
     at-end? :: <boolean>)
    => ();
  annotate-variables-pattern(pattern.pattern-guts, aux-rule-sets, #t);
end method annotate-variables-pattern;

// annotate-variables-pattern{<variable-pattern>}
//
// Annotate the two pattern variables.  Note: the parser handles figuring
// out how much this pattern should match, so any contained wildcards must
// consume everything they are given.
// 
define method annotate-variables-pattern
    (pattern :: <variable-pattern>,
     aux-rule-sets :: <simple-object-vector>, at-end? :: <boolean>)
    => ();
  annotate-variables-pattern(pattern.variable-name-pattern, aux-rule-sets, #t);
  annotate-variables-pattern(pattern.variable-type-pattern, aux-rule-sets, #t);
end method annotate-variables-pattern;

// annotate-variables-pattern{<bindings-pattern>}
//
// Annotate the two pattern variables.  Note: the parser handles figuring
// out how much this pattern should match, so any contained wildcards must
// consume everything they are given.
// 
define method annotate-variables-pattern
    (pattern :: <bindings-pattern>,
     aux-rule-sets :: <simple-object-vector>, at-end? :: <boolean>)
    => ();
  annotate-variables-pattern
    (pattern.bindings-variables-pattern, aux-rule-sets, #t);
  annotate-variables-pattern
    (pattern.bindings-value-pattern, aux-rule-sets, #t);
end method annotate-variables-pattern;

// annotate-variables-pattern{<pattern-variable>}
//
// Annotate the variable.  Set the at-end flag and default the constraint
// to wildcard if it names an auxiliary rule.
// 
define method annotate-variables-pattern
    (pattern :: <pattern-variable>, aux-rule-sets :: <simple-object-vector>,
     at-end? :: <boolean>)
    => ();
  pattern.patvar-at-end? := at-end?;
  if (pattern.patvar-constraint == #f
	& find-aux-rule-set(pattern.patvar-name, aux-rule-sets))
    pattern.patvar-constraint := #"*";
  end if;
end method annotate-variables-pattern;

// annotate-variables-pattern{<property-list-pattern>}
//
// Annotate the pattern variables buried inside this property list pattern.
// 
define method annotate-variables-pattern
    (pattern :: <property-list-pattern>,
     aux-rule-sets :: <simple-object-vector>,
     at-end? :: <boolean>)
    => ();
  if (pattern.plistpat-rest)
    annotate-variables-pattern(pattern.plistpat-rest, aux-rule-sets, #t);
  end if;
  if (pattern.plistpat-keys)
    for (key in pattern.plistpat-keys)
      annotate-variables-pattern(key, aux-rule-sets, #t);
    end for;
  end if;
end method annotate-variables-pattern;


// Bindings

// <pattern-binding> -- internal.
//
// Used to represent the binding of some pattern variable.
// 
define class <pattern-binding> (<object>)
  //
  // The pattern variable this is the binding for.
  constant slot pattern-binding-variable :: <pattern-variable>,
    required-init-keyword: variable:;
  //
  // The fragment or a vector of fragments that this variable is bound to.
  slot pattern-binding-value
      :: type-union(<fragment>, <template>, <simple-object-vector>),
    required-init-keyword: value:;
  //
  // Link for chaining pattern bindings together.
  constant slot pattern-binding-next :: false-or(<pattern-binding>),
    required-init-keyword: next:;
end class <pattern-binding>;

define sealed domain make (singleton(<pattern-binding>));
define sealed domain initialize (<pattern-binding>);


// <pattern-binding-set>
//
// The type of a set of pattern variable bindings.
// 
define constant <pattern-binding-set>
  = false-or(<pattern-binding>);

// add-binding
//
// Adds a binding for variable to value to the set of bindings and returns
// the new set.  Note: it is critical that this operation does not modify the
// old set of bindings.  Otherwise, the retry stuff would puke.
// 
define inline method add-binding
    (variable :: <pattern-variable>,
     value :: type-union(<fragment>, <template>, <simple-object-vector>),
     bindings :: <pattern-binding-set>)
    => res :: <pattern-binding-set>;
  make(<pattern-binding>,
       variable: variable,
       value: value,
       next: bindings);
end method add-binding;

// find-binding -- internal.
//
// Scan the bindings set trying to find a binding for name.
// 
define method find-binding
    (bindings :: <pattern-binding-set>, varref :: <pattern-variable-reference>,
     name :: <symbol>, this-rule-set :: false-or(<symbol>))
    => res :: <pattern-binding>;
  block (return)
    for (binding = bindings then binding.pattern-binding-next,
	 while: binding)
      let bound-name = binding.pattern-binding-variable.patvar-name;
      if ((bound-name | this-rule-set) == name)
	return(binding);
      end if;
    end for;
    compiler-fatal-error-location
      (varref.patvarref-name, "Unbound pattern variable: %s", name);
  end block;
end method find-binding;


// Fragment hacking utilities.

// more? -- internal.
//
// Utility function that checks to see if we've consumed the entire fragment.
// Note: we don't use generic function dispatch because it will be a lot
// faster to just compare the class with ==.
//
define inline method more? (fragment :: <fragment>) => res :: <boolean>;
  ~instance?(fragment, <empty-fragment>);
end method more?;

// consume-elementary-fragment
//
// Consume the next elementary fragment in the given fragment and return
// it and whatever is left over.  Note that the argument fragment might
// itself be an elementary fragment, in which case nothing is left over.
//
define generic consume-elementary-fragment (fragment :: <fragment>)
    => (res :: <elementary-fragment>, remainder :: <fragment>);

// consume-elementary-fragment{<empty-fragment>}
//
// Puke because this shouldn't happen.
// 
define method consume-elementary-fragment (fragment :: <empty-fragment>)
    => (res :: <elementary-fragment>, remainder :: <fragment>);
  error("Someone called consume-elementary-fragment on an empty fragment.");
end method consume-elementary-fragment;

// consume-elementary-fragment{<elementary-fragment>}
//
// Consume it all.
// 
define method consume-elementary-fragment (fragment :: <elementary-fragment>)
    => (res :: <elementary-fragment>, remainder :: <fragment>);
  values(fragment,
	 make(<empty-fragment>,
	      source-location:
		source-location-after(fragment.source-location)));
end method consume-elementary-fragment;

// consume-elementary-fragment{<compound-fragment>}
//
// If the fragment only has two elementary fragments inside it, return them.
// Otherwise return the current head and make a new compound fragment that
// contains everything except the current head.
//
define method consume-elementary-fragment (fragment :: <compound-fragment>)
    => (res :: <elementary-fragment>, remainder :: <fragment>);
  let head = fragment.fragment-head;
  let new-head = head.fragment-next;
  let tail = fragment.fragment-tail;
  values(head,
	 if (new-head == tail)
	   new-head;
	 else
	   make(<compound-fragment>, head: new-head, tail: tail);
	 end if);
end method consume-elementary-fragment;


// split-at-separator -- internal.
//
// Split the fragment into two sub-fragments, the stuff before the separator
// and the stuff after the separator.  If the separator doesn't appear in the
// fragment, return the entire fragment as before the separator and an empty
// fragment as after the separator.  Neither fragment contains the separator
// after the split.
//
define generic split-at-separator
    (fragment :: <fragment>, separator :: <integer>)
    => (found? :: <boolean>, before :: <fragment>, after :: <fragment>);

// split-at-separator{<fragment>}
//
// Catch-all method that gives up.
// 
define method split-at-separator
    (fragment :: <fragment>, separator :: <integer>)
    => (found? :: <boolean>, before :: <fragment>, after :: <fragment>);
  values(#f,
	 fragment,
	 make(<empty-fragment>,
	      source-location:
		source-location-after(fragment.source-location)));
end method split-at-separator;

// split-at-separator{<token-fragment>}
//
// If the token is the separator, return two empty fragments.  If the token
// is not the separator, then return it as the before fragment.
//
define method split-at-separator
    (fragment :: <token-fragment>, separator :: <integer>, #next next-method)
    => (found? :: <boolean>, before :: <fragment>, after :: <fragment>);
  if (fragment.fragment-token.token-kind == separator)
    values(#t,
	   make(<empty-fragment>,
		source-location:
		  source-location-before(fragment.source-location)),
	   make(<empty-fragment>,
		source-location:
		  source-location-after(fragment.source-location)));
  else
    next-method();
  end if;
end method split-at-separator;

// split-at-separator{<compound-fragment>}
//
// Scan down the fragment looking for the separator.  When we find it, take
// care to preserve the <compound-fragment> invariants when creating the
// subfragments.
//
define method split-at-separator
    (fragment :: <compound-fragment>, separator :: <integer>, #next next-method)
    => (found? :: <boolean>, before :: <fragment>, after :: <fragment>);
  block (return)
    let head-fragment = fragment.fragment-head;
    let tail-fragment = fragment.fragment-tail;
    let end-fragment = tail-fragment.fragment-next;
    for (sub-fragment = head-fragment then sub-fragment.fragment-next,
	 until: sub-fragment == end-fragment)
      if (instance?(sub-fragment, <token-fragment>)
	    & sub-fragment.fragment-token.token-kind == separator)
	return(#t,
	       if (head-fragment == sub-fragment)
		 make(<empty-fragment>,
		      source-location:
			source-location-before(head-fragment.source-location));
	       elseif (head-fragment.fragment-next == sub-fragment)
		 head-fragment;
	       else
		 make(<compound-fragment>, head: head-fragment,
		      tail: sub-fragment.fragment-prev);
	       end if,
	       if (sub-fragment == tail-fragment)
		 make(<empty-fragment>,
		      source-location:
			source-location-after(tail-fragment.source-location));
	       elseif (sub-fragment == tail-fragment.fragment-prev)
		 tail-fragment;
	       else
		 make(<compound-fragment>, head: sub-fragment.fragment-next,
		      tail: tail-fragment);
	       end if);
      end if;
    end for;
    next-method();
  end block;
end method split-at-separator;


// fragment-from-variables -- internal.
//
// Return a fragment that corresponds to results of parsing a variables-list.
// But return it in such a form that it can be parsed as a variable if
// there is only one variable.
// 
define method fragment-from-variables
    (varlist :: <variable-list>) => res :: <fragment>;
  let res = make(<empty-fragment>);
  for (var in varlist.varlist-fixed)
    res := append-fragments!(res, fragment-from-variable(var));
  end for;
  if (varlist.varlist-rest)
    let rest-frag
      = append-fragments!(make(<token-fragment>,
			       token: make(<token>, kind: $rest-token)),
			  fragment-from-variable(varlist.varlist-rest));
  end if;
  if (varlist.varlist-fixed.size ~== 1 | varlist.varlist-rest)
    make(<bracketed-fragment>,
	 left-token: make(<token>, kind: $left-paren-token),
	 contents: res,
	 right-token: make(<token>, kind: $right-paren-token));
  else
    res;
  end if;
end method fragment-from-variables;


define method fragment-from-variable
    (variable :: <parameter>) => res :: <fragment>;
  let var-fragment = make(<token-fragment>, token: variable.param-name);
  if (variable.param-type)
    let double-colon-fragment
      = make(<token-fragment>,
	     token: make(<token>, kind: $double-colon-token));
    let type-fragment = make-parsed-fragment(variable.param-type);
    append-fragments!(append-fragments!(var-fragment, double-colon-fragment),
		      type-fragment);
  else
    var-fragment;
  end if;
end method fragment-from-variable;


// find-intermediate-word -- internal.
//
// Scan through the fragment looking for the first occurance of one of the
// intermediate words.  Return the <token-fragment> that is that occurance.
// Or #f if there are no intermediate words in the fragment.
// 
define generic find-intermediate-word
    (fragment :: <fragment>, intermediate-words :: <simple-object-vector>)
    => res :: false-or(<token-fragment>);

// find-intermediate-word{<empty-fragment>}
//
// Pretty easy, huh?
// 
define method find-intermediate-word
    (fragment :: <empty-fragment>,
     intermediate-words :: <simple-object-vector>)
    => res :: false-or(<token-fragment>);
  #f;
end method find-intermediate-word;

// find-intermediate-word{<compound-fragment>}
//
// Scan the contents for an elementary fragment that is the intermediate word.
//
define method find-intermediate-word
    (fragment :: <compound-fragment>,
     intermediate-words :: <simple-object-vector>)
    => res :: false-or(<token-fragment>);
  block (return)
    let stop = fragment.fragment-tail.fragment-next;
    for (frag = fragment.fragment-head then frag.fragment-next,
	 until: frag == stop)
      let res = find-intermediate-word(frag, intermediate-words);
      if (res)
	return(res);
      end if;
    end for;
    #f;
  end block;
end method find-intermediate-word;

// find-intermediate-word{<token-fragment>}
//
// Return this fragment if it matches any of the intermediate words, or #f
// if not.
//
define method find-intermediate-word
    (fragment :: <token-fragment>,
     intermediate-words :: <simple-object-vector>)
    => res :: false-or(<token-fragment>);
  let token = fragment.fragment-token;
  let kind = token.token-kind;
  if (kind >= $define-token & kind <= $quoted-name-token
	& member?(token.token-symbol, intermediate-words))
    fragment;
  else
    #f;
  end if;
end method find-intermediate-word;

// find-intermediate-word{<bracketed-fragment>}
//
// Return #f because no bracketed fragment can be an intermediate word.
// 
define method find-intermediate-word
    (fragment :: <bracketed-fragment>,
     intermediate-words :: <simple-object-vector>)
    => res :: false-or(<token-fragment>);
  #f;
end method find-intermediate-word;


// split-fragment-at -- internal.
//
// Split the fragment into two sub-fragments, the stuff before the split point
// and the stuff from the split point and on.  A split point of #f means
// at the very end (allowed just to make boundary conditions easier).
// 
define generic split-fragment-at
    (fragment :: <fragment>, split-point :: false-or(<elementary-fragment>))
    => (before :: <fragment>, at-and-after :: <fragment>);

// split-fragment-at{<fragment>, singleton(#f)}
//
// Just return the whole fragment as before the split point.
// 
define method split-fragment-at
    (fragment :: <fragment>, split-point == #f)
    => (before :: <fragment>, at-and-after :: <fragment>);
  values(fragment,
	 make(<empty-fragment>,
	      source-location:
		source-location-after(fragment.source-location)));
end method split-fragment-at;

// split-fragment-at{<empty-fragment>,<elementary-fragment>}
//
// If we are trying to split an empty fragment at anything other than the
// end (handled above), something when wrong somewhere.
// 
define method split-fragment-at
    (fragment :: <empty-fragment>, split-point :: <elementary-fragment>)
    => (before :: <fragment>, at-and-after :: <fragment>);
  error("Splitting fragment at an elementary-fragment it doesn't contain.");
end method split-fragment-at;

// split-fragment-at{<compound-fragment>,<elementary-fragment>}
//
define method split-fragment-at
    (fragment :: <compound-fragment>, split-point :: <elementary-fragment>)
    => (before :: <fragment>, at-and-after :: <fragment>);
  let head = fragment.fragment-head;
  let tail = fragment.fragment-tail;
  //
  // Verify that the split point is valid.
  block (return)
    let stop = tail.fragment-next;
    for (frag = head then frag.fragment-next,
	 until: frag == stop)
      if (frag == split-point)
	return();
      end if;
    end for;
    error("Splitting fragment at an elementary-fragment it doesn't contain.");
  end block;
  //
  // Now make the split.
  if (split-point == head)
    values(make(<empty-fragment>,
		source-location:
		  source-location-before(head.source-location)),
	   fragment);
  else
    values(if (split-point == head.fragment-next)
	     head;
	   else
	     make(<compound-fragment>,
		  head: head,
		  tail: split-point.fragment-prev);
	   end if,
	   if (split-point == tail)
	     fragment.fragment-tail;
	   else
	     make(<compound-fragment>,
		  head: split-point,
		  tail: tail);
	   end if);
  end if;
end method split-fragment-at;

// split-fragment-at{<elementary-fragment>,<elementary-fragment>}
//
define method split-fragment-at
    (fragment :: <elementary-fragment>, split-point :: <elementary-fragment>)
    => (before :: <fragment>, at-and-after :: <fragment>);
  if (fragment == split-point)
    values(make(<empty-fragment>,
		source-location:
		  source-location-before(split-point.source-location)),
	   fragment);
  else
    error("Splitting fragment at an elementary-fragment it doesn't contain.");
  end if;
end method split-fragment-at;


// last-elementary-fragment -- internal.
//
// Return the last elementary fragment that makes up fragment.
// Used as the split point in trim-until-parsable if the parser didn't give
// us a hint.
//
define generic last-elementary-fragment (fragment :: <fragment>)
    => result :: <elementary-fragment>;

define method last-elementary-fragment (fragment :: <empty-fragment>)
    => result :: <elementary-fragment>;
  error("last-elementary-fragment called on an empty fragment?");
end method last-elementary-fragment;

define method last-elementary-fragment (fragment :: <compound-fragment>)
    => result :: <elementary-fragment>;
  fragment.fragment-tail;
end method last-elementary-fragment;

define method last-elementary-fragment (fragment :: <elementary-fragment>)
    => result :: <elementary-fragment>;
  fragment;
end method last-elementary-fragment;


// trim-until-parsable -- internal.
//
// Find the initial prefix of fragment that is parsable by parser.
// 
define method trim-until-parsable
    (fragment :: <fragment>, end-point :: false-or(<elementary-fragment>),
     parser :: <function>)
    => (result :: <object>,
	fragment :: false-or(<fragment>),
	remaining :: false-or(<fragment>));
  block (return)
    while (#t)
      let (before, remaining) = split-fragment-at(fragment, end-point);
      let tokenizer = make(<fragment-tokenizer>, fragment: before);
      block ()
	return(parser(tokenizer), before, remaining);
      exception (<compiler-error>)
	if (before.more?)
	  end-point := (tokenizer.tokenizer-potential-end-point
			  | last-elementary-fragment(before));
	else
	  return(#f, #f, #f);
	end if;
      end block;
    end while;
  end block;
end method trim-until-parsable;


// Expansion generators.

define class <expansion-generator> (<object>)
  //
  // The <macro-call-parse> being expanded via this generator.
  constant slot generator-call :: <macro-call-parse>,
    required-init-keyword: call:;
  //
  // The macro-source for the expansion we are generating.
  constant slot generator-source :: <macro-source>,
    required-init-keyword: source:;
  //
  // The definition of the macro being expanded.
  constant slot generator-macro-defn :: <macro-definition>,
    required-init-keyword: definition:;
  //
  // The uniquifier we are using for this expansion.
  constant slot generator-uniquifier :: <uniquifier> = make(<uniquifier>),
    init-keyword: uniquifier:;
  //
  // The fragment we are currently working on generating.  We start with
  // it being some random empty fragment and then overwrite it in the
  // initialize method with an empty-fragment with a reasonable source
  // location.  This way it is guaranteed to be initialized.
  slot generator-fragment :: <fragment> = make(<empty-fragment>);
  //
  // Stack of pending fragments.
  slot generator-fragment-stack :: <list> = #();
  //
  // The index for the next token.
  slot generator-next-token :: <integer> = 0;
  //
  // The current section marker.
  slot generator-section :: <section-marker> = make(<section-marker>);
end class <expansion-generator>;

define sealed domain make (singleton(<expansion-generator>));
define sealed domain initialize (<expansion-generator>);


define method initialize
    (generator :: <expansion-generator>, #next next-method, #key) => ();
  next-method();
  generator.generator-fragment := generate-empty-fragment(generator);
end method initialize;


define method generate-empty-fragment (generator :: <expansion-generator>)
    => res :: <empty-fragment>;
  make(<empty-fragment>,
       source-location:
	 source-location-after
	   (make(<simple-macro-source-location>,
		 source: generator.generator-source,
		 came-from: make(<unknown-source-location>),
		 token: generator.generator-next-token,
		 section: generator.generator-section)));
end method generate-empty-fragment;


define method start-sub-fragment
    (generator :: <expansion-generator>) => ();
  generator.generator-fragment-stack
    := pair(generator.generator-fragment, generator.generator-fragment-stack);
  generator.generator-fragment := generate-empty-fragment(generator);
end method start-sub-fragment;


define method finish-sub-fragment (generator :: <expansion-generator>)
    => res :: <fragment>;
  let res = generator.generator-fragment;
  let stack = generator.generator-fragment-stack;
  generator.generator-fragment := stack.head;
  generator.generator-fragment-stack := stack.tail;
  res;
end method finish-sub-fragment;


define method generate-fragment
    (generator :: <expansion-generator>, fragment :: <fragment>) => ();
  generator.generator-fragment
    := append-fragments!(generator.generator-fragment, fragment);
end method generate-fragment;


define method generate-token-source-location
    (generator :: <expansion-generator>,
     #key came-from :: <source-location> = make(<unknown-source-location>))
    => res :: <macro-source-location>;
  let index = generator.generator-next-token;
  generator.generator-next-token := index + 1;
  make(<simple-macro-source-location>,
       source: generator.generator-source,
       came-from: came-from,
       token: index,
       section: generator.generator-section);
end method generate-token-source-location;


define method generate-token
    (generator :: <expansion-generator>, token :: <token>,
     came-from :: <source-location>)
    => ();
  generate-fragment
    (generator,
     make(<token-fragment>,
	  source-location:
	    generate-token-source-location(generator, came-from: came-from),
	  token: token));
end method generate-token;


define method nuke-separator (generator :: <expansion-generator>) => ();
  let fragment = generator.generator-fragment;
  let last = last-elementary-fragment(fragment);
  assert(instance?(last, <token-fragment>));
  generator.generator-fragment := split-fragment-at(fragment, last);
  generator.generator-next-token := generator.generator-next-token - 1;
end method nuke-separator;


define method generate-new-section (generator :: <expansion-generator>) => ();
  generator.generator-section := make(<section-marker>);
end method generate-new-section;


// fix-source-location.

// fix-source-location -- internal.
//
// Examine a <expression-parse> if it is a <macro-call-parse> dissect it
// and reassemble with correct source-location. This allows for some magic
// too like keeping a stack of expansion locations rooting at the original
// source. But this is not yet implemented, and should be done in a space-
// saving manner.
// We need this because source-location is a constant slot and is filled in
// at make time with the location of the template.
// 

define generic fix-source-location (parsed :: <expression-parse>, orig-loc :: <source-location>)
    => fixed :: <expression-parse>;

define method fix-source-location (parsed :: <expression-parse>, orig-loc :: <source-location>)
    => fixed :: <expression-parse>;

  parsed
end;

define method fix-source-location
    (parsed :: <function-macro-call-parse>, orig-loc :: <source-location>)
    => fixed :: <expression-parse>;

  make(<function-macro-call-parse>,
	source-location: orig-loc,
	word: parsed.macro-call-word,
	fragment: parsed.macro-call-fragment)
end;
// TBI: <statement-parse> with type-union and object-class ?

/* TBI:
define method fix-source-location
    (parsed :: <definition-macro-call-parse>, orig-loc :: <source-location>)
    => fixed :: <expression-parse>;

  make(parsed.object-class,
	source-location: orig-loc,
	word: parsed.macro-call-word,
	fragment: parsed.macro-call-fragment,
	modifiers: parsed.definition-modifiers)
end;
*/


// macro-expand.

// macro-expand -- exported.
//
// Expand the form returning an <expression-parse>.  The real work is done
// by macro-expand-aux which is separated out so that we can use it for
// macro constraints.
// 
define method macro-expand (form :: <macro-call-parse>)
    => expansion :: <expression-parse>;

  fix-source-location(parse-body(macro-expansion-tokenizer(form)),
                      form.source-location);
end method macro-expand;

// macro-expansion-tokenizer -- exported.
//
define method macro-expansion-tokenizer (form :: <macro-call-parse>)
 => (tokenizer :: <tokenizer>);
  make(<fragment-tokenizer>, fragment: macro-expand-aux(form));
end method;

// macro-expand-aux -- internal.
//
// Does the real work of a macro expansion.  Find the definition and run its
// main rules until one of them gives us a match.  Once we have a match,
// compute the replacement and return it.
// 
define method macro-expand-aux (form :: <macro-call-parse>)
    => replacement :: <fragment>;
  let name = name-for-macro-call(form);
  let var = find-variable(name);
  unless (var)
    error("syntax table and variable table inconsistent.");
  end unless;
  let defn = var.variable-definition;
  unless (defn)
    error("syntax table and variable table inconsistent.");
  end unless;
  let intermediate-words = defn.macro-intermediate-words;
  block (return)
    let fragment = form.macro-call-fragment;
    for (rule in defn.macro-main-rule-set.rule-set-rules)
      let results = match-rule(rule, form, fragment, intermediate-words);
      unless (results == #"failed")
	let generator
	      = make(<expansion-generator>,
		     call: form,
		     source:
		     make(<macro-source>,
			  description: format-to-string("%s", form),
			  source-location: form.source-location),
		     definition: defn);
	expand-template(generator, rule.rule-template, results, #f);
	return(generator.generator-fragment);
      end unless;
    end for;
    compiler-fatal-error-location
      (form, "Syntax error in %s.  None of the main rules matched.", form);
  end block;
end method macro-expand-aux;


// name-for-macro-call -- internal.
//
// Return the <name> of the macro being called by this macro call parse.
// 
define generic name-for-macro-call (form :: <macro-call-parse>)
    => name :: <basic-name>;

// name-for-macro-call{type-union(<statement-parse>, <fn-macro-call-parse>)}
//
// For statement and function-macro calls, we just use the word as is.
// 
define method name-for-macro-call
    (form :: type-union(<statement-parse>, <function-macro-call-parse>))
    => name :: <basic-name>;
  id-name(form.macro-call-word);
end method name-for-macro-call;

// name-for-macro-call{<definition-macro-call-parse>}
//
// For definition macro calls, we tack -definer onto the word.
// 
define method name-for-macro-call (form :: <definition-macro-call-parse>)
    => name :: <basic-name>;
  let define-word = form.macro-call-word;
  make(<basic-name>,
       module: define-word.token-module,
       symbol: symcat(define-word.token-symbol, "-definer"));
end method name-for-macro-call;



// Match-rule
//
// Main entry to the pattern matcher.  Takes a rule, the form, the fragment,
// and the set of intermediate-words, and returns either #"failed" of a set of
// pattern variable bindings.
//
define method match-rule
    (rule :: <rule>, form :: false-or(<macro-call-parse>),
     fragment :: <fragment>, intermediate-words :: <simple-object-vector>)
    => res :: type-union(<pattern-binding-set>, singleton(#"failed"));
  match(rule.rule-pattern, fragment, intermediate-words,
	method ()
	  #"failed"
	end,
	method (fragment :: <fragment>, fail :: <function>,
		results :: <pattern-binding-set>)
	  if (fragment.more?) fail() else results end;
	end,
	#f);
end method match-rule;

define method match-rule
    (rule :: <define-rule>, form :: <definition-macro-call-parse>,
     fragment :: <fragment>, intermediate-words :: <simple-object-vector>)
    => res :: type-union(<pattern-binding-set>, singleton(#"failed"));
  let modifiers-fragment = make(<empty-fragment>);
  for (modifier in form.definition-modifiers)
    modifiers-fragment
      := append-fragments!(modifiers-fragment,
			   make(<token-fragment>, token: modifier));
  end for;
  match(rule.define-rule-modifiers-pattern, modifiers-fragment, #[],
	method () #"failed" end,
	method (modifiers-fragment, fail, results)
	  if (modifiers-fragment.more?)
	    fail();
	  else
	    match(rule.rule-pattern, fragment, intermediate-words,
		  method () #"failed" end,
		  method (fragment, fail, results)
		    if (fragment.more?) fail() else results end;
		  end,
		  results);
	  end if;
	end,
	#f);
end method match-rule;

// match
//
// The actual pattern matcher.
//
// Match recursively decends into the pattern and the fragment building up
// the set of pattern variable bindings as it goes.  The backup-and-retry
// semantics of wildcards are implemented by passing a ``fail'' function to
// match.  When a match fails, the fail function is called and it can
// retry the match based on state it closed over.  But because we don't have
// a true call-with-current-continuation in Dylan, we also have to pass in
// a continue function that is basically the current continuation.
//
// So if some pattern matcher wants to match two parts, it calls match on
// the first part with a continue function that calls match on the second
// part.  The second call to match should pass in the original continue
// function so that matching continues correctly once the second sub-match
// is done.
// 
// Note: we don't declare the return value for any of these functions because
// doing so would keep the compiler from being able to use any tail calls.
// This is because the compiler can't prove the result types are guaranteed
// because we have no way of declaring the result type of the fail and continue
// arguments.
// 
define generic match
    (pattern :: <pattern>, fragment :: <fragment>,
     intermediate-words :: <simple-object-vector>,
     fail :: <function>, continue :: <function>,
     results :: <pattern-binding-set>);

// match{<empty-pattern>}
//
// The empty pattern consumes nothing, so just continue with the whole
// fragment.
// 
define method match
    (pattern :: <empty-pattern>, fragment :: <fragment>,
     intermediate-words :: <simple-object-vector>,
     fail :: <function>, continue :: <function>,
     results :: <pattern-binding-set>);
  continue(fragment, fail, results);
end method match;

// match{<semicolon-pattern>}
//
// Match some stuff divided at a semicolon.  The real work is done by
// match-separated-patterns.
// 
define method match
    (pattern :: <semicolon-pattern>, fragment :: <fragment>,
     intermediate-words :: <simple-object-vector>,
     fail :: <function>, continue :: <function>,
     results :: <pattern-binding-set>)
  match-separated-patterns(pattern, $semicolon-token, fragment,
			   intermediate-words, fail, continue, results);
end method match;

// match{<comma-pattern>}
//
// Match some stuff divided at a comma.  The real work is done by
// match-separated-patterns.
// 
define method match
    (pattern :: <comma-pattern>, fragment :: <fragment>,
     intermediate-words :: <simple-object-vector>,
     fail :: <function>, continue :: <function>,
     results :: <pattern-binding-set>)
  match-separated-patterns(pattern, $comma-token, fragment,
			   intermediate-words, fail, continue, results);
end method match;

// match-separated-patterns -- internal.
//
// Does the real work of matching <semicolon-pattern>s and <comma-pattern>s.
// We pass in the original fail function when calling match on the right
// hand side because we don't want to reconsider any wildcards that
// precede the separator if the right hand side fails.
//

define class <auxiliary-rule-name-query>(<condition>)
end;

define method match-separated-patterns
    (pattern :: <binary-pattern>, separator :: <integer>,
     fragment :: <fragment>, intermediate-words :: <simple-object-vector>,
     fail :: <function>, continue :: <function>,
     results :: <pattern-binding-set>)
  let (found?, before, after) = split-at-separator(fragment, separator);
  if (found? | pattern.pattern-last?)
    match(pattern.pattern-left, before, intermediate-words, fail,
	  method (remaining :: <fragment>, left-fail :: <function>,
		  results :: <pattern-binding-set>)
	    if (remaining.more?)
	      left-fail();
	    else
	      // detect infinite recursion
	      if (~found?
		  & pattern.pattern-last?
		  & ~before.more?)
		compiler-fatal-error-location
		  (fragment,
		   "Infinite recursion in auxiliary rules %s.",
		   <auxiliary-rule-name-query>.make.signal);
	      end if;
	      // match behind separator
	      match(pattern.pattern-right, after, intermediate-words, fail,
		    continue, results);
	    end if;
	  end method,
	  results);
  else
    fail();
  end if;
end method match-separated-patterns;

// match{<sequential-pattern>}
//
// Match the two sub-patterns sequentially.  We pass the fail function
// on though from the first call to the second call so that if the second
// sub-pattern fails, we will reconsider any wildcards in the first
// sub-pattern.
// 
define method match
    (pattern :: <sequential-pattern>, fragment :: <fragment>,
     intermediate-words :: <simple-object-vector>,
     fail :: <function>, continue :: <function>,
     results :: <pattern-binding-set>)
  match(pattern.pattern-left, fragment, intermediate-words, fail,
	method (remaining :: <fragment>, fail :: <function>,
		results :: <pattern-binding-set>)
	  match(pattern.pattern-right, remaining, intermediate-words, fail,
		continue, results);
	end method,
	results);
end method match;

// match{<name-pattern>}
//
// Match a literal name.  If name is ``otherwise'' and it is followed by
// an arrow (``=>'') in the fragment, skip over that also.  This special
// case makes writing forgiving case-body macros a bit easier.
// 
define method match
    (pattern :: <name-pattern>, fragment :: <fragment>,
     intermediate-words :: <simple-object-vector>,
     fail :: <function>, continue :: <function>,
     results :: <pattern-binding-set>)
  block (return)
    if (fragment.more?)  
      let (name-frag, after-name) = consume-elementary-fragment(fragment);
      if (instance?(name-frag, <token-fragment>))
	let token = name-frag.fragment-token;
	let kind = token.token-kind;
	if (kind >= $define-token & kind <= $quoted-name-token
	      & token.token-symbol == pattern.pattern-name.token-symbol)
	  if (token.token-symbol == #"otherwise" & fragment.more?)
	    let (arrow-frag, after-arrow)
	      = consume-elementary-fragment(after-name);
	    if (instance?(arrow-frag, <token-fragment>)
		  & arrow-frag.fragment-token.token-kind == $arrow-token)
	      return(continue(after-arrow, fail, results));
	    else
	      return(continue(after-name, fail, results));
	    end if;
	  else
	    return(continue(after-name, fail, results));
	  end if;
	end if;
      end if;
    end if;
    fail();
  end block;
end method match;

// match{<arrow-pattern>}
//
// Match a literal arrow.
// 
define method match
    (pattern :: <arrow-pattern>, fragment :: <fragment>,
     intermediate-words :: <simple-object-vector>,
     fail :: <function>, continue :: <function>,
     results :: <pattern-binding-set>)
  if (fragment.more?)
    let (arrow-frag, after-arrow) = consume-elementary-fragment(fragment);
    if (instance?(arrow-frag, <token-fragment>)
	  & arrow-frag.fragment-token.token-kind == $arrow-token)
      continue(after-arrow, fail, results);
    else
      fail();
    end if;
  else
    fail();
  end if;
end method match;

// match{<bracketed-pattern>}
//
// Match a bracketed-pattern.  If the fragment starts with a bracketed
// fragment, match it against the pattern, otherwise fail.  A bracketed
// fragment matches a bracketed pattern if the brackets are the same kind
// and the sub-pattern matches the sub-fragment in total.
//
// When the match succeeds, we pass the original fail function to the
// continue function because we don't want to reconsider any wildcards
// that are inside the sub-fragment if something chokes after the
// bracketed pattern.
// 
define method match
    (pattern :: <bracketed-pattern>, fragment :: <fragment>,
     intermediate-words :: <simple-object-vector>,
     fail :: <function>, continue :: <function>,
     results :: <pattern-binding-set>)
  if (fragment.more?)
    local method same-kind?
	      (token1 :: <token>, token2 :: <token>) => res :: <boolean>;
	    token1.token-kind == token2.token-kind;
	  end method same-kind?;
    let (bracketed-frag, after-bracketed)
      = consume-elementary-fragment(fragment);
    if (instance?(bracketed-frag, <bracketed-fragment>)
	  & same-kind?(pattern.pattern-left-token,
		       bracketed-frag.fragment-left-token)
	  & same-kind?(pattern.pattern-right-token,
		       bracketed-frag.fragment-right-token))
      match(pattern.pattern-guts, bracketed-frag.fragment-contents,
	    intermediate-words, fail,
	    method (remaining :: <fragment>, sub-fail :: <function>,
		    results :: <pattern-binding-set>)
	      if (remaining.more?)
		sub-fail();
	      else
		continue(after-bracketed, fail, results);
	      end if;
	    end method,
	    results);
    else
      fail();
    end if;
  else
    fail();
  end if;
end method match;


// match{<variable-pattern>}
//
// Match an optionally typed variable.  We use the parser to find the extent
// of the variable and then fail if we can't bind the var and type pattern
// variables to the variable name and type expression.  (Those bindings
// might fail if someone supplies a strange constraint on them.)
//
// Once we have bound up the pattern variables, we continue with the original
// fail function, because we don't want later failures to cause us to
// reconsider our pattern variables if any of them happen to have wildcard
// constraints.
//
define method match
    (pattern :: <variable-pattern>, fragment :: <fragment>,
     intermediate-words :: <simple-object-vector>,
     fail :: <function>, continue :: <function>,
     results :: <pattern-binding-set>)
  let (variable, variable-fragment, variable-remaining)
    = trim-until-parsable(fragment, #f, parse-variable);
  if (variable)
    match(pattern.variable-name-pattern,
	  make(<token-fragment>, token: variable.param-name),
	  intermediate-words, fail,
	  method (remaining :: <fragment>, sub-fail :: <function>,
		  results :: <pattern-binding-set>)
	    if (remaining.more?)
	      sub-fail();
	    else
	      let type = variable.param-type;
	      let type-frag
		= if (type)
		    make-parsed-fragment(type);
		  else
		    make(<token-fragment>,
			 token: make(<identifier-token>,
				     kind: $raw-ordinary-word-token,
				     symbol: #"<object>",
				     module: $Dylan-Module,
				     uniquifier: make(<uniquifier>)));
		  end if;
	      match(pattern.variable-type-pattern, type-frag,
		    intermediate-words, fail,
		    method (remaining :: <fragment>, sub-fail :: <function>,
			    results :: <pattern-binding-set>)
		      if (remaining.more?)
			sub-fail();
		      else
			continue(variable-remaining, fail, results);
		      end if;
		    end method,
		    results);
	    end if;
	  end method,
	  results);
  else
    fail();
  end if;
end method match;

// match{<bindings-pattern>}
//
// Match against ``variable = expression''.  We use the parser to parse
// the binding, and then match our pattern variables against the various
// parts of that binding.  If either of those matches fail (because of
// strange constraints) then fail the whole bindings-pattern.
//
// When we continue, we pass in the original fail function so that we don't
// reconsider our pattern variables if either happen to be wildcards.
// 
define method match
    (pattern :: <bindings-pattern>, fragment :: <fragment>,
     intermediate-words :: <simple-object-vector>,
     fail :: <function>, continue :: <function>,
     results :: <pattern-binding-set>)
  let (bindings :: false-or(<bindings-parse>), bindings-fragment,
       bindings-remaining)
    = trim-until-parsable(fragment, #f, parse-bindings);
  if (bindings)
    let variables = bindings.bindings-variables;
    let variables-fragment = fragment-from-variables(variables);
    match(pattern.bindings-variables-pattern, variables-fragment,
	  intermediate-words, fail,
	  method (remainder :: <fragment>, sub-fail :: <function>,
		  results :: <pattern-binding-set>)
	    if (remainder.more?)
	      sub-fail();
	    else
	      match(pattern.bindings-value-pattern,
		    make-parsed-fragment(bindings.bindings-expression),
		    intermediate-words, fail,
		    method (remainder :: <fragment>, sub-fail :: <function>,
			    results :: <pattern-binding-set>)
		      if (remainder.more?)
			sub-fail();
		      else
			continue(bindings-remaining, fail, results);
		      end if;
		    end method,
		    results);
	    end if;
	  end method,
	  results);
  else    
    fail();
  end if;
end method match;

// match{<pattern-variable>}
//
// Bind a pattern variable.  All the real work is done either in match-wildcard
// or extract-constrained-fragment.  Note: we use extract-constrained-fragment
// for wildcards if the wildcard is the last thing in some wildcard scope.
// Doing so is much more efficient because match-wildcard will just have to
// keep trying one more token in the match until it uses them all up.  So if
// we know we are going to have to match them all, why not just match them
// all?
// 
define method match
    (pattern :: <pattern-variable>, fragment :: <fragment>,
     intermediate-words :: <simple-object-vector>,
     fail :: <function>, continue :: <function>,
     results :: <pattern-binding-set>)
  if (pattern.patvar-constraint == #"*" & ~pattern.patvar-at-end?)
    match-wildcard(pattern, fragment, fail, continue, results);
  else
    let (result, remaining)
      = extract-constrained-fragment(pattern, fragment, intermediate-words);
    if (result)
      continue(remaining, fail, add-binding(pattern, result, results));
    else
      fail();
    end if;
  end if;
end method match;

// match-wildcard
//
// Match a wildcard against some fragment, retrying the match with
// successively longer matches until the remainder matches.
//
define generic match-wildcard
    (pattern :: <pattern-variable>, fragment :: <fragment>,
     fail :: <function>, continue :: <function>,
     results :: <pattern-binding-set>);

// match-wildcard{<compound-fragment>}
//
// The hard case.  Start off matching nothing.  If that fails, try again
// after consuming the first token.  If we end up consuming all the tokens
// and the later match still fails, then fail this match as well.
//
define method match-wildcard
    (pattern :: <pattern-variable>, fragment :: <compound-fragment>,
     fail :: <function>, continue :: <function>,
     results :: <pattern-binding-set>)
  let head = fragment.fragment-head;
  let tail = fragment.fragment-tail;
  local
    method match-wildcard-aux (split :: <elementary-fragment>)
      let matched-fragment
	= if (split == head)
	    split;
	  else
	    make(<compound-fragment>, head: head, tail: split);
	  end if;
      let results = add-binding(pattern, matched-fragment, results);
      if (split == tail)
	continue(make(<empty-fragment>,
		      source-location:
			source-location-after(fragment.source-location)),
		 fail, results);
      else
	let next = split.fragment-next;
	let remaining-fragment
	  = if (next == tail)
	      next;
	    else
	      make(<compound-fragment>, head: next, tail: tail);
	    end if;
	continue(remaining-fragment, curry(match-wildcard-aux, next), results);
      end if;
    end method match-wildcard-aux;
  continue(fragment,
	   curry(match-wildcard-aux, head),
	   add-binding(pattern,
		       make(<empty-fragment>,
			    source-location:
			      source-location-before
			        (fragment.source-location)),
		       results));
end method match-wildcard;

// match-wildcard{<empty-fragment>}
//
// The easy case.  If there is nothing to match, then how much to match
// is an easy question.
// 
define method match-wildcard
    (pattern :: <pattern-variable>, fragment :: <empty-fragment>,
     fail :: <function>, continue :: <function>,
     results :: <pattern-binding-set>)
  continue(fragment, fail, add-binding(pattern, fragment, results));
end method match-wildcard;

// match-wildcard{<elementary-fragment>}
//
// We either match the elementary-fragment, or we don't.
//
define method match-wildcard
    (pattern :: <pattern-variable>, fragment :: <elementary-fragment>,
     fail :: <function>, continue :: <function>,
     results :: <pattern-binding-set>)
  let srcloc = fragment.source-location;
  continue(fragment,
	   method ()
	     continue(make(<empty-fragment>,
			   source-location: source-location-after(srcloc)),
		      fail, add-binding(pattern, fragment, results));
	   end method,
	   add-binding(pattern,
		       make(<empty-fragment>,
			    source-location: source-location-before(srcloc)),
		       results));
end method match-wildcard;

// extract-constrained-fragment
//
// Handles the real work of pattern variable constraints.
// 
define method extract-constrained-fragment
    (patvar :: <pattern-variable>, fragment :: <fragment>,
     intermediate-words :: <simple-object-vector>)
    => (result :: false-or(<fragment>), remaining :: false-or(<fragment>));
  let constraint = patvar.patvar-constraint;
  select (constraint)
    //
    // Expression constrains are pretty much handled by the parser.
    #"expression" =>
      let (expr, expr-fragment, remaining)
	= trim-until-parsable(fragment, #f, parse-expression);
      if (expr)
	values(make-parsed-fragment
		 (expr, source-location: expr-fragment.source-location),
	       remaining);
      else
	values(#f, #f);
      end if;

    // Variable constraints are also pretty much handled by the parser.
    #"variable" =>
      let (var, var-fragment, remaining)
	= trim-until-parsable(fragment, #f, parse-variable);
      if (var)
	values(fragment-from-variable(var), remaining);
      else
	values(#f, #f);
      end if;

    // Name constraints are pretty easy.  If the thing is a name token,
    // then match it.  If not, fail.
    #"name" =>
      block (return)
	if (fragment.more?)
	  let (name-frag, after-name) = consume-elementary-fragment(fragment);
	  if (instance?(name-frag, <token-fragment>))
	    let token = name-frag.fragment-token;
	    let kind = token.token-kind;
	    if (kind >= $define-token & kind <= $quoted-name-token)
	      return(name-frag, after-name);
	    end if;
	  end if;
	end if;
	values(#f, #f);
      end block;

    // Now this is an easy constraint.
    #"token" =>
      if (fragment.more?)
	let (token-frag, after-token) = consume-elementary-fragment(fragment);
	if (instance?(token-frag, <token-fragment>)
	      & ~instance?(token-frag.fragment-token, <pre-parsed-token>)
	      & begin
		  let kind = token-frag.fragment-token.token-kind;
		  kind < $define-token
		    | kind = $otherwise-token
		    | kind = $raw-ordinary-word-token
		    | kind = $ordinary-define-body-word-token
		    | kind = $ordinary-define-list-word-token
		    | kind >= $quoted-name-token;
		end)
	  values(token-frag, after-token);
	else
	  values(#f, #f);
	end if;
      else
	values(#f, #f);
      end if;

    // Parse a body, stopping before any of the intermediate words.
    #"body" =>
      let word = find-intermediate-word(fragment, intermediate-words);
      let (body, body-fragment, remaining)
	= trim-until-parsable(fragment, word, parse-body);
      if (body)
	values(make-parsed-fragment
		 (body, source-location: body-fragment.source-location),
	       remaining);
      else
	values(#f, #f);
      end if;

    // Parse a case-body, stopping before any of the intermediate words.
    // Note: parse-case-body returns a fragment, so we don't have to
    // convert it into one.
    #"case-body" =>
      let word = find-intermediate-word(fragment, intermediate-words);
      let (body, body-fragment, remaining)
	= trim-until-parsable(fragment, word, parse-case-body);
      if (body)
	values(body, remaining);
      else
	values(#f, #f);
      end if;

    // Parameter-list constraints are a Gwydion extension.  They consume and
    // match as much of the fragment that parses as a parameter-list-opt.
    #"parameter-list" =>
      let (paramlist, paramlist-fragment, remaining)
	= trim-until-parsable(fragment, #f, parse-parameter-list);
      if (paramlist)
	values(make-parsed-fragment
		 (paramlist,
		  source-location: paramlist-fragment.source-location),
	       remaining);
      else
	values(#f, #f);
      end if;

    // Variable-list constraints are a Gwydion extension.  They consume and
    // match as much of the fragment that parses as a variable-list-opt.
    #"variable-list" =>
      let (varlist, varlist-fragment, remaining)
	= trim-until-parsable(fragment, #f, parse-variable-list);
      if (varlist)
	values(make-parsed-fragment
		 (varlist, source-location: varlist-fragment.source-location),
	       remaining);
      else
	values(#f, #f);
      end if;

    #"macro" =>
      let (macro-call, macro-fragment, remaining)
	= trim-until-parsable(fragment, #f, parse-macro-call);
      if (macro-call)
	values(macro-expand-aux(macro-call), remaining);
      else
	values(#f, #f);
      end if;

    // If someone called us with a wildcard constraint that means that they
    // know that the entire fragment must match, so lets just accommodate them.
    #"*" =>
      values(fragment,
	     make(<empty-fragment>,
		  source-location:
		    source-location-after(fragment.source-location)));

    #f =>
      compiler-fatal-error-location
	(patvar, "Pattern variable without a constraint: %s",
	 patvar.patvar-name | "...");

    otherwise =>
      compiler-fatal-error-location
	(patvar, "Unknown constraint on %s: %s",
	 patvar.patvar-name | "...",
	 patvar.patvar-constraint);
  end select;
end method extract-constrained-fragment;


// match{<property-list-pattern>}
//
// Match a property-list against a property-list-pattern.
// 
define method match
    (pattern :: <property-list-pattern>, fragment :: <fragment>,
     intermediate-words :: <simple-object-vector>,
     fail :: <function>, continue :: <function>,
     results :: <pattern-binding-set>)
  block (return)
    //
    // First, parse a property list and puke if the whole thing doesn't parse.
    let plist
      = block ()
	  parse-property-list(make(<fragment-tokenizer>, fragment: fragment));
	exception (<compiler-error>)
	  return(fail());
	end block;
    //
    // If there is a #rest var, bind it to the whole fragment.
    let restvar = pattern.plistpat-rest;
    if (restvar)
      let constraint = restvar.patvar-constraint;
      if (constraint == #"*")
	//
	// No processing necessary (or desired), use the original fragment.
	results := add-binding(pattern.plistpat-rest, fragment, results);
      else
	//
	// We need to constrain each value in the property list and build
	// a new plist fragment that has the results of the constraint
	// for each value.
	let frag = make(<empty-fragment>);
	local method append! (new-frag :: <fragment>) => ();
		frag := append-fragments!(frag, new-frag);
	      end method append!;
	for (prop in plist)
	  if (prop.prop-comma)
	    append!(make(<token-fragment>, token: prop.prop-comma,
			 source-location: prop.prop-comma-srcloc));
	  end if;
	  append!(make(<token-fragment>, token: prop.prop-keyword,
		       source-location: prop.prop-keyword-srcloc));
	  let (val, remaining)
	    = extract-constrained-fragment(restvar, prop.prop-value,
					   intermediate-words);
	  if (val == #f | remaining.more?)
	    return(fail());
	  end if;
	  //
	  // We copy the fragment because it would be impolite to destructivly
	  // modify the fragment found in the property list.
	  append!(copy-fragment(val, preserve-source-locations: #t));
	end for;
	results := add-binding(restvar, frag, results);
      end if;
    end if;
    //
    // Now deal with the #key variables.
    if (pattern.plistpat-keys)
      //
      // If they didn't also spec #all-keys make sure all the supplied
      // properties are valid.
      unless (pattern.plistpat-all-keys?)
	for (prop in plist)
	  block (okay)
	    let sym = prop.prop-keyword.token-literal.literal-value;
	    for (key in pattern.plistpat-keys)
	      if (sym == key.patvar-name)
		okay();
	      end if;
	    end for;
	    return(fail());
	  end block;
	end for;
      end unless;
      //
      // Now bind up each keyword variable.
      for (key in pattern.plistpat-keys)
	let sym = key.patvar-name;
	if (key.patkey-all?)
	  //
	  // It was a ?? variable, so it gets a vector of all the values.
	  let this-result = make(<stretchy-vector>);
	  for (prop in plist)
	    if (prop.prop-keyword.token-literal.literal-value == sym)
	      let (val, remaining)
		= extract-constrained-fragment(key, prop.prop-value,
					       intermediate-words);
	      if (val == #f | remaining.more?)
		return(fail());
	      end if;
	      add!(this-result, val);
	    end if;
	  end for;
	  //
	  // If the keyword wasn't supplied and there is a default, then
	  // act like the default showed up once.
	  if (empty?(this-result) & key.patkey-default)
	    add!(this-result, key.patkey-default);
	  end if;
	  //
	  // Bind it up.
	  results := add-binding(key,
				 as(<simple-object-vector>, this-result),
				 results);
	else
	  //
	  // It was a ? variable, so it gets the first one.
	  block (found-key)
	    for (prop in plist)
	      if (prop.prop-keyword.token-literal.literal-value == sym)
		let (val, remaining)
		  = extract-constrained-fragment(key, prop.prop-value,
						 intermediate-words);
		if (val == #f | remaining.more?)
		  return(fail());
		end if;
		results := add-binding(key, val, results);
		found-key();
	      end if;
	    end for;
	    //
	    // It wasn't there, so use the default or fail if there isn't one.
	    // Note: the default is *not* supposed to be subject to the
	    // constraint.
	    if (key.patkey-default)
	      results := add-binding(key, key.patkey-default, results);
	    else
	      return(fail());
	    end if;
	  end block;
	end if;
      end for;
    end if;
    //
    // Wow, everything worked.  So continue with the empty fragment (because
    // property-lists have to match everything) and the bindings we have
    // accumulated.
    continue(make(<empty-fragment>,
		  source-location:
		    source-location-after(fragment.source-location)),
	     fail, results);
  end block;
end method match;



// Expanders.

define generic expand-template
    (generator :: <expansion-generator>, template :: <template>,
     bindings :: <pattern-binding-set>, this-rule-set :: false-or(<symbol>))
    => ();

define method expand-template
    (generator :: <expansion-generator>, template :: <literal-template>,
     bindings :: <pattern-binding-set>, this-rule-set :: false-or(<symbol>))
    => ();
  for (piece in template.template-elements,
       prev-separator? = #f
	 then append-element!(generator, piece, bindings, this-rule-set,
			      prev-separator?))
  end for;
end method expand-template;


define generic append-element!
    (generator :: <expansion-generator>,
     element :: type-union(<token>, <bracketed-element>,
			   <pattern-variable-reference>),
     bindings :: <pattern-binding-set>, this-rule-set :: false-or(<symbol>),
     prev-was-separator? :: <boolean>)
    => ends-in-separator? :: <boolean>;


define method find-atomic-value
    (varref :: <pattern-variable-reference>, bindings :: <pattern-binding-set>,
     name :: <symbol>, this-rule-set :: false-or(<symbol>))
    => res :: type-union(<fragment>, <template>);
  let binding = find-binding(bindings, varref, name, this-rule-set);
  let var = binding.pattern-binding-variable;
  if (instance?(var, <pattern-keyword>) & var.patkey-all?)
    compiler-fatal-error-location
      (varref.patvarref-name,
       "%s was bound using ?? so must be referenced with ??, not ?",
       name);
  end if;
  binding.pattern-binding-value;
end method find-atomic-value;


// expand-rule-set-reference -- internal
//
//  called from append-element! for simple and
//  ellipsis <pattern-variable-reference>s
//
define function expand-rule-set-reference
    (generator :: <expansion-generator>,
     varref :: <pattern-variable-reference>,
     bindings :: <pattern-binding-set>,
     this-rule-set :: false-or(<symbol>),
     prev-was-separator? :: <boolean>,
     name :: <symbol>)
    => ends-in-separator? :: singleton(#f);
  generate-new-section(generator);
  let value = find-atomic-value(varref, bindings, name, this-rule-set);
  let aux-rule-sets = generator.generator-macro-defn.macro-auxiliary-rule-sets;
  let aux-rule-set = find-aux-rule-set(name, aux-rule-sets);
  let orig-token = generator.generator-next-token;
  expand-value(generator, value, aux-rule-set);
  if (prev-was-separator? & orig-token == generator.generator-next-token)
    nuke-separator(generator);
  end if;
  generate-new-section(generator);
  #f;
end;


define method append-element!
    (generator :: <expansion-generator>,
     varref :: <unhygienic-pattern-variable-reference>,
     bindings :: <pattern-binding-set>, this-rule-set :: false-or(<symbol>),
     prev-was-separator? :: <boolean>)
    => ends-in-separator? :: singleton(#f);
  let token = varref.patvarref-name;
  let token = make(<identifier-token>,
         	   source-location: token.source-location,
         	   kind: token.token-kind,
         	   symbol: token.token-symbol,
         	   module: *Current-Module*);
  generate-token(generator, token, token.source-location);
end method append-element!;


define method append-element!
    (generator :: <expansion-generator>,
     varref :: <simple-pattern-variable-reference>,
     bindings :: <pattern-binding-set>, this-rule-set :: false-or(<symbol>),
     prev-was-separator? :: <boolean>)
    => ends-in-separator? :: singleton(#f);
  expand-rule-set-reference
    (generator, varref, bindings, this-rule-set,
     prev-was-separator?, varref.patvarref-name.token-symbol);
end method append-element!;


define method append-element!
    (generator :: <expansion-generator>,
     varref :: <ellipsis-pattern-variable-reference>,
     bindings :: <pattern-binding-set>, this-rule-set :: false-or(<symbol>),
     prev-was-separator? :: <boolean>)
    => ends-in-separator? :: singleton(#f);
  unless (this-rule-set)
    compiler-fatal-error-location
      (varref.patvarref-name, "Can't use ... in the main rule set.");
  end unless;
  expand-rule-set-reference
    (generator, varref, bindings, this-rule-set,
     prev-was-separator?, this-rule-set);
end method append-element!;
  

define method append-element!
    (generator :: <expansion-generator>,
     varref :: <sequence-pattern-variable-reference>,
     bindings :: <pattern-binding-set>, this-rule-set :: false-or(<symbol>),
     prev-was-separator? :: <boolean>)
    => ends-in-separator? :: <boolean>;
  let name = varref.patvarref-name.token-symbol;
  let binding = find-binding(bindings, varref, name, this-rule-set);
  let var = binding.pattern-binding-variable;
  unless (instance?(var, <pattern-keyword>) & var.patkey-all?)
    compiler-fatal-error-location
      (varref.patvarref-name,
       "%s was bound using ? so must be referenced with ?, not ??",
       name);
  end unless;
  let aux-rule-sets = generator.generator-macro-defn.macro-auxiliary-rule-sets;
  let aux-rule-set = find-aux-rule-set(name, aux-rule-sets);
  let separator = varref.patvarref-separator;
  let orig-token = generator.generator-next-token;
  for (value in binding.pattern-binding-value,
       first? = #t then #f)
    generate-new-section(generator);
    if (separator & ~first?)
      generate-token(generator, separator, separator.source-location);
      generate-new-section(generator);
    end if;
    expand-value(generator, value, aux-rule-set);
  end for;
  if (prev-was-separator? & orig-token == generator.generator-next-token)
    nuke-separator(generator);
  end if;
  generate-new-section(generator);
  #f;
end method append-element!;


define method append-element!
    (generator :: <expansion-generator>,
     varref :: <concatenating-pattern-variable-reference>,
     bindings :: <pattern-binding-set>, this-rule-set :: false-or(<symbol>),
     prev-was-separator? :: <boolean>)
    => ends-in-separator? :: <boolean>;
  generate-new-section(generator);
  start-sub-fragment(generator);
  let name-string-or-symbol = varref.patvarref-name;
  let name
    = select (name-string-or-symbol.token-kind)
	$string-token =>
	  as(<symbol>, name-string-or-symbol.token-literal.literal-value);
	$symbol-token =>
	  name-string-or-symbol.token-literal.literal-value;
	otherwise =>
	  name-string-or-symbol.token-symbol;
      end select;
  let value = find-atomic-value(varref, bindings, name, this-rule-set);
  let aux-rule-sets = generator.generator-macro-defn.macro-auxiliary-rule-sets;
  let aux-rule-set = find-aux-rule-set(name, aux-rule-sets);
  expand-value(generator, value, aux-rule-set);
  let fragment = finish-sub-fragment(generator);
  unless (instance?(fragment, <token-fragment>))
    compiler-fatal-error-location
      (varref.patvarref-name,
       "Pattern variable %s is not bound to a single NAME token.", 
       name);
  end unless;
  let token = fragment.fragment-token;
  unless (token.token-kind >= $define-token
	    & token.token-kind <= $quoted-name-token)
    compiler-fatal-error-location
      (varref.patvarref-name,
       "Pattern variable %s is not bound to a single NAME token.", 
       name);
  end unless;
  let orig = as(<string>, token.token-symbol);
  let with-suffix
    = if (varref.patvarref-suffix)
	concatenate(orig, varref.patvarref-suffix);
      else
	orig;
      end if;
  let string
    = if (varref.patvarref-prefix)
	concatenate(varref.patvarref-prefix, with-suffix);
      else
	with-suffix;
      end if;
  let srcloc = fragment.source-location;
  let new-token
    = select (name-string-or-symbol.token-kind)
	$string-token =>
	  make(<literal-token>,
	       source-location: srcloc,
	       kind: $string-token,
	       literal: make(<literal-string>, value: string));
	$symbol-token =>
	  make(<literal-token>,
	       source-location: srcloc,
	       kind: $symbol-token,
	       literal: make(<literal-symbol>, value: as(<symbol>, string)));
	otherwise =>
	  let symbol = as(<symbol>, string);
	  let module = token.token-module;
	  let new-kind = syntax-for-name(module.module-syntax-table, symbol);
	  make(<identifier-token>, source-location: srcloc,
	       kind: new-kind, symbol: symbol, module: module,
	       uniquifier: generator.generator-uniquifier);
      end select;
  generate-fragment(generator,
		    make(<token-fragment>,
			 source-location: srcloc,
			 token: new-token));
  generate-new-section(generator);
end method append-element!;
    


define generic expand-value
    (generator :: <expansion-generator>,
     value :: type-union(<fragment>, <template>),
     aux-rule-set :: false-or(<auxiliary-rule-set>))
    => ();

define method expand-value
    (generator :: <expansion-generator>, value :: <template>,
     aux-rule-set :: false-or(<auxiliary-rule-set>))
    => ();
  let desc = generator.generator-source.macro-source-description;
  let srcloc = generator.generator-source.source-location;
  let newsrc = make(<macro-source>, description: desc,
		    source-location: srcloc);
  let sub-generator = make(<expansion-generator>,
			   call: generator.generator-call,
			   source: newsrc,
			   definition: generator.generator-macro-defn,
			   uniquifier: generator.generator-uniquifier);
  expand-template(sub-generator, value, #f, #f);
  expand-value(generator, sub-generator.generator-fragment, aux-rule-set);
end method expand-value;

define method expand-value
    (generator :: <expansion-generator>, value :: <fragment>,
     aux-rule-set :: <auxiliary-rule-set>)
    => ();
  block (return)
    let intermediate-words
      = generator.generator-macro-defn.macro-intermediate-words;
    let handler <auxiliary-rule-name-query>
      = method(#rest ignore)
          aux-rule-set.rule-set-name
        end;
    for (rule in aux-rule-set.rule-set-rules)
      let results = match-rule(rule, #f, value, intermediate-words);
      unless (results == #"failed")
	expand-template(generator, rule.rule-template, results,
			aux-rule-set.rule-set-name);
	return();
      end unless;
    end for;
    let call = generator.generator-call;
    compiler-fatal-error-location
      (call,
       "Syntax error in %s.  None of the auxiliary rules for %s matched.",
       call, aux-rule-set.rule-set-name);
  end block;
end method expand-value;

define method expand-value
    (generator :: <expansion-generator>, value :: <fragment>,
     aux-rule-set == #f)
    => ();
  expand-fragment(generator, value);
end method expand-value;

define generic expand-fragment
    (generator :: <expansion-generator>, fragment :: <fragment>) => ();

define method expand-fragment
    (generator :: <expansion-generator>, fragment :: <empty-fragment>) => ();
end method expand-fragment;

define method expand-fragment
    (generator :: <expansion-generator>, fragment :: <compound-fragment>)
    => ();
  let stop = fragment.fragment-tail.fragment-next;
  for (subfrag = fragment.fragment-head then subfrag.fragment-next,
       until: subfrag == stop)
    expand-fragment(generator, subfrag);
  end for;
end method expand-fragment;

define method expand-fragment
    (generator :: <expansion-generator>, fragment :: <bracketed-fragment>)
    => ();
  let left-srcloc
    = generate-token-source-location
	 (generator, came-from: fragment.fragment-left-srcloc);
  start-sub-fragment(generator);
  expand-fragment(generator, fragment.fragment-contents);
  let contents = finish-sub-fragment(generator);
  let right-srcloc
    = generate-token-source-location
	 (generator, came-from: fragment.fragment-right-srcloc);
  generate-fragment(generator,
		    make(<bracketed-fragment>,
			 left-token: fragment.fragment-left-token,
			 left-srcloc: left-srcloc,
			 contents: contents,
			 right-token: fragment.fragment-right-token,
			 right-srcloc: right-srcloc));
end method expand-fragment;

define method expand-fragment
    (generator :: <expansion-generator>, fragment :: <token-fragment>)
    => ();
  generate-token(generator, fragment.fragment-token, fragment.source-location);
end method expand-fragment;


define method append-element!
    (generator :: <expansion-generator>, piece :: <bracketed-element>,
     bindings :: <pattern-binding-set>, this-rule-set :: false-or(<symbol>),
     prev-was-separator? :: <boolean>)
    => ends-in-separator? :: <boolean>;
  let left-token = piece.bracketed-element-left-token;
  let left-srcloc
    = generate-token-source-location
	 (generator, came-from: left-token.source-location);
  start-sub-fragment(generator);
  expand-template(generator, piece.bracketed-element-guts, bindings,
		  this-rule-set);
  let contents = finish-sub-fragment(generator);
  let right-token = piece.bracketed-element-right-token;
  let right-srcloc
    = generate-token-source-location
	 (generator, came-from: right-token.source-location);
  generate-fragment(generator,
		    make(<bracketed-fragment>,
			 left-token: left-token,
			 left-srcloc: left-srcloc,
			 contents: contents,
			 right-token: right-token,
			 right-srcloc: right-srcloc));
  #f;
end method append-element!;

define method append-element!
    (generator :: <expansion-generator>, token :: <token>,
     bindings :: <pattern-binding-set>, this-rule-set :: false-or(<symbol>),
     prev-was-separator? :: <boolean>)
    => ends-in-separator? :: <boolean>;
  generate-token(generator, token, token.source-location);
  token-is-separator?(token);
end method append-element!;

define method append-element!
    (generator :: <expansion-generator>, token :: <identifier-token>,
     bindings :: <pattern-binding-set>, this-rule-set :: false-or(<symbol>),
     prev-was-separator? :: <boolean>)
    => ends-in-separator? :: <boolean>;
  let kind = token.token-kind;
  let symbol = token.token-symbol;
  let module = token.token-module;
  let new-kind
    = if (kind == $quoted-name-token | kind == $tilde-token | module == #f)
	kind;
      else
	syntax-for-name(module.module-syntax-table, symbol);
      end if;
  let new-token
    = make(<identifier-token>, source-location: token.source-location,
	   kind: new-kind, symbol: symbol, module: module,
	   uniquifier: generator.generator-uniquifier);
  generate-token(generator, new-token, token.source-location);
  token-is-separator?(new-token);
end method append-element!;

define method append-element!
    (generator :: <expansion-generator>, token :: <operator-token>,
     bindings :: <pattern-binding-set>, this-rule-set :: false-or(<symbol>),
     prev-was-separator? :: <boolean>)
    => ends-in-separator? :: <boolean>;
  let new-token
    = make(<operator-token>, source-location: token.source-location,
	   kind: token.token-kind, symbol: token.token-symbol,
	   module: token.token-module,
	   uniquifier: generator.generator-uniquifier);
  generate-token(generator, new-token, token.source-location);
  token-is-separator?(new-token);
end method append-element!;


define method token-is-separator? (token :: <token>) => res :: <boolean>;
  select (token.token-kind)
    $minus-token, $equal-token, $double-equal-token,
    $other-binary-operator-token, $comma-token, $semicolon-token =>
      #t;
    otherwise =>
      #f;
  end select;
end method token-is-separator?;



define method expand-template
    (generator :: <expansion-generator>, template :: <procedural-template>,
     bindings :: <pattern-binding-set>, this-rule-set :: false-or(<symbol>))
    => ();
  generate-new-section(generator);
  let var = find-variable(id-name(template.template-name));
  let expander = var & var.variable-fragment-expander;
  unless (expander)
    compiler-fatal-error-location
      (template.template-name, "Unknown procedural template expander: %s",
       template.template-name.token-symbol);
  end unless;
  let desc = generator.generator-source.macro-source-description;
  let srcloc = generator.generator-source.source-location;
  apply(expander,
	generator,
	map(method (argument :: <template>) => frag :: <fragment>;
	      let generator = make(<expansion-generator>,
				   call: generator.generator-call,
				   source:
				     make(<macro-source>,
					  description: desc,
					  source-location: srcloc),
				   definition: generator.generator-macro-defn,
				   uniquifier: generator.generator-uniquifier);
	      expand-template(generator, argument, bindings, this-rule-set);
	      generator.generator-fragment;
	    end method,
	    template.template-arguments));
  generate-new-section(generator);
end method expand-template;


// Defining procedural expanders.

define method define-procedural-expander
    (name :: <symbol>, expander :: <function>) => ();
  add-bootstrap-export(name);
  let var = find-variable(make(<basic-name>, symbol: name,
			       module: $Bootstrap-Module),
			  create: #t);
  var.variable-fragment-expander := expander;
end method define-procedural-expander;


// Testing code.

define open generic recursively-macro-expand (thing :: <object>)
    => new-thing :: <object>;

define method recursively-macro-expand
    (thing :: <method-parse>) => replacement :: <method-parse>;
  make(<method-parse>,
       name: thing.method-name,
       parameters: recursively-macro-expand(thing.method-parameters),
       returns: recursively-macro-expand(thing.method-returns),
       body: recursively-macro-expand(thing.method-body));
end method recursively-macro-expand;

define method recursively-macro-expand
    (thing :: <variable-list>) => replacement :: <variable-list>;
  make(<variable-list>,
       fixed: map(recursively-macro-expand, thing.varlist-fixed),
       rest: (thing.varlist-rest
		& recursively-macro-expand(thing.varlist-rest)));
end method recursively-macro-expand;

define method recursively-macro-expand
    (thing :: <parameter-list>) => replacement :: <parameter-list>;
  make(<parameter-list>,
       fixed: map(recursively-macro-expand, thing.varlist-fixed),
       rest: thing.varlist-rest & recursively-macro-expand(thing.varlist-rest),
       next: thing.paramlist-next,
       keys: (thing.paramlist-keys
		& map(recursively-macro-expand, thing.paramlist-keys)),
       all-keys: thing.paramlist-all-keys?);
end method recursively-macro-expand;

define method recursively-macro-expand
    (thing :: <parameter>) => replacement :: <parameter>;
  make(<parameter>,
       name: thing.param-name,
       type: thing.param-type & recursively-macro-expand(thing.param-type));
end method recursively-macro-expand;

define method recursively-macro-expand
    (thing :: <keyword-parameter>) => replacement :: <keyword-parameter>;
  make(<keyword-parameter>,
       name: thing.param-name,
       type: thing.param-type & recursively-macro-expand(thing.param-type),
       keyword: thing.param-keyword,
       default: (thing.param-default
		   & recursively-macro-expand(thing.param-default)));
end method recursively-macro-expand;

define method recursively-macro-expand
    (thing :: <definition-parse>) => thing :: <definition-parse>;
  thing;
end method recursively-macro-expand;

define method recursively-macro-expand
    (thing :: <macro-call-parse>) => replacement :: <expression-parse>;
  recursively-macro-expand(macro-expand(thing));
end method recursively-macro-expand;

define method recursively-macro-expand
    (thing :: <let-parse>) => replacement :: <let-parse>;
  make(<let-parse>,
       variables: recursively-macro-expand(thing.let-variables),
       expression: recursively-macro-expand(thing.let-expression));
end method recursively-macro-expand;

define method recursively-macro-expand
    (thing :: <handler-parse>) => replacement :: <handler-parse>;
  make(<handler-parse>,
       type: recursively-macro-expand(thing.handler-type),
       options: map(recursively-macro-expand, thing.handler-options),
       handler: recursively-macro-expand(thing.handler-expression));
end method recursively-macro-expand;

define method recursively-macro-expand
    (thing :: <local-parse>) => replacement :: <local-parse>;
  make(<local-parse>,
       methods: map(recursively-macro-expand, thing.local-methods));
end method recursively-macro-expand;

define method recursively-macro-expand
    (thing :: <literal-ref-parse>) => replacement :: <literal-ref-parse>;
  thing;
end method recursively-macro-expand;

define method recursively-macro-expand
    (thing :: <varref-parse>) => replacement :: <varref-parse>;
  thing;
end method recursively-macro-expand;

define method recursively-macro-expand
    (thing :: <varset-parse>) => replacement :: <varset-parse>;
  make(<varset-parse>, id: thing.varset-id,
       value: recursively-macro-expand(thing.varset-value));
end method recursively-macro-expand;

define method recursively-macro-expand
    (thing :: <funcall-parse>) => replacement :: <funcall-parse>;
  make(<funcall-parse>,
       function: recursively-macro-expand(thing.funcall-function),
       arguments: map(recursively-macro-expand, thing.funcall-arguments));
end method recursively-macro-expand;

define method recursively-macro-expand
    (thing :: <dot-parse>) => replacement :: <dot-parse>;
  make(<dot-parse>,
       operand: recursively-macro-expand(thing.dot-operand),
       name: thing.dot-name);
end method recursively-macro-expand;

define method recursively-macro-expand
    (thing :: <body-parse>) => replacement :: <body-parse>;
  make(<body-parse>,
       parts: map(recursively-macro-expand, thing.body-parts));
end method recursively-macro-expand;

define method recursively-macro-expand
    (thing :: <bind-exit-parse>) => replacement :: <bind-exit-parse>;
  make(<bind-exit-parse>,
       name: thing.exit-name,
       body: recursively-macro-expand(thing.exit-body));
end method recursively-macro-expand;

define method recursively-macro-expand
    (thing :: <if-parse>) => replacement :: <if-parse>;
  make(<if-parse>,
       condition: recursively-macro-expand(thing.if-condition),
       consequent: recursively-macro-expand(thing.if-consequent),
       alternate: recursively-macro-expand(thing.if-alternate));
end method recursively-macro-expand;

define method recursively-macro-expand
    (thing :: <method-ref-parse>) => replacement :: <method-ref-parse>;
  make(<method-ref-parse>,
       method: recursively-macro-expand(thing.method-ref-method));
end method recursively-macro-expand;

define method recursively-macro-expand
    (thing :: <primitive-parse>) => replacement :: <primitive-parse>;
  make(<primitive-parse>,
       name: thing.primitive-name,
       operands: map(recursively-macro-expand, thing.primitive-operands));
end method recursively-macro-expand;

define method recursively-macro-expand
    (thing :: <unwind-protect-parse>) => replacement :: <unwind-protect-parse>;
  make(<unwind-protect-parse>,
       body: recursively-macro-expand(thing.uwp-body),
       cleanup: recursively-macro-expand(thing.uwp-cleanup));
end method recursively-macro-expand;


