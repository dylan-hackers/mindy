module: variables
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/variables.dylan,v 1.21 1996/01/25 00:25:34 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

// $Dylan-User-Uses -- internal.
//
// Sequence of modules (in the Dylan library) that are automatically used
// by the implicit Dylan-User module that gets created in each library.
//
define constant $Dylan-User-Uses :: <vector> = #[#"Dylan", #"Extensions"];


// <library> -- exported.
//
define class <library> (<object>)
  //
  // The name of this library, as a symbol.
  slot library-name :: <symbol>, required-init-keyword: name:;
  //
  // #t once the defn for this library has been processed, #f otherwise.
  slot defined? :: <boolean>, init-value: #f;
  //
  // Vector of <use> structures for all the libraries this library
  // uses.  Uninitialized until the defn for this library has been
  // processed.
  slot used-libraries :: <simple-object-vector>;
  //
  // Hash table mapping names to modules for modules exported directly
  // from this library.  Modules re-exported after being imported from
  // somewhere else are not listed in here.  Not filled in until the
  // library is actually defined.
  slot exported-modules :: <object-table>,
    init-function: curry(make, <object-table>);
  //
  // Hash table mapping names to modules for modules homed in this
  // library.
  slot local-modules :: <object-table>,
    init-function: curry(make, <object-table>);
end;

define method print-object (lib :: <library>, stream :: <stream>) => ();
  pprint-fields(lib, stream, name: lib.library-name);
end;

// library-name -- exported.
//
define generic library-name (lib :: <library>) => name :: <symbol>;

// <module> -- exported.
//
define class <module> (<object>)
  //
  // The name of this module, as a symbol.
  slot module-name :: <symbol>, required-init-keyword: name:;
  //
  // The library this module lives in.
  slot module-home :: <library>, required-init-keyword: home:;
  //
  // #t once the defn for this module has been processed, #f otherwise.
  slot defined? :: <boolean>, init-value: #f;
  //
  // Vector of <use> structures for all the modules this module
  // uses.  Uninitialized until the defn for this module has been
  // processed.
  slot used-modules :: <simple-object-vector>;
  //
  // Hash table mapping names to syntactic categories.
  slot module-syntax-table :: <table>,
    init-function: curry(make, <table>);
  //
  // Hash table mapping names to variables for variables accessable
  // in this module.
  slot variables :: <object-table>,
    init-function: curry(make, <object-table>);
  //
  // Reverse hash-table for mapping variables to names.  Since duplication is
  // possible, each element is a sequence of names.
  slot variable-names :: <object-table>,
    init-function: curry(make, <object-table>);
  //
  // Hash table mapping names to variables for all variables exported
  // from this module.
  slot exported-variables :: <object-table>,
    init-function: curry(make, <object-table>);
end;

define method print-object (mod :: <module>, stream :: <stream>) => ();
  pprint-fields(mod, stream, name: mod.module-name);
end;

define method print-message (mod :: <module>, stream :: <stream>) => ();
  format(stream, "module %s, library %s",
	 mod.module-name,
	 mod.module-home.library-name);
end;

define method initialize
    (mod :: <module>, #next next-method, #key magic-tokens: magic-tokens?)
    => ();
  next-method();
  //
  // Fill in the built in core words.
  //
  let table = mod.module-syntax-table;
  table[#"define"] := <define-token>;
  table[#"end"] := <end-token>;
  table[#"generic"] := <generic-token>;
  table[#"handler"] := <handler-token>;
  table[#"let"] := <let-token>;
  table[#"local"] := <local-token>;
  table[#"macro"] := <macro-token>;
  table[#"otherwise"] := <otherwise-token>;
  table[#"seal"] := <seal-token>;

  if (magic-tokens?)
    table[#"%%begin"] := <begin-token>;
    table[#"%%bind-exit"] := <bind-exit-token>;
    table[#"%%class"] := <class-token>;
    table[#"%%cleanup"] := <cleanup-token>;
    table[#"%%constant"] := <constant-token>;
    table[#"%%create"] := <create-token>;
    table[#"%%finally"] := <finally-token>;
    table[#"%%for"] := <for-token>;
    table[#"%%from"] := <from-token>;
    table[#"%%else"] := <else-token>;
    table[#"%%export"] := <export-token>;
    table[#"%%if"] := <if-token>;
    table[#"%%in"] := <in-token>;
    table[#"%%library"] := <library-token>;
    table[#"%%method"] := <method-token>;
    table[#"%%module"] := <module-token>;
    table[#"%%mv-call"] := <mv-call-token>;
    table[#"%%primitive"] := <primitive-token>;
    table[#"%%set"] := <set-token>;
    table[#"%%unwind-protect"] := <uwp-token>;
    table[#"%%use"] := <use-token>;
    table[#"%%variable"] := <variable-token>;
    table[#"%%while"] := <while-token>;
  end;
end;

// module-name -- exported.
//
define generic module-name (mod :: <module>) => name :: <symbol>;

// <variable> -- exported.
// 
define class <variable> (<object>)
  //
  // The name of the variable, as a symbol.
  slot variable-name :: <symbol>, required-init-keyword: name:;
  // 
  // The module this variable lives in.  Note: this is not necessarily
  // the same as where it is defined, because the create clause in
  // define module forms creates a variable, but requires it to be
  // defined elsewhere.
  slot variable-home :: <module>, required-init-keyword: home:;
  //
  // All the modules where this variable is accessable.
  slot accessing-modules :: <stretchy-vector>,
    init-function: curry(make, <stretchy-vector>, size: 0);
  //
  // #t if originally in an export or create clause, #f otherwise.
  slot exported? :: <boolean>, init-value: #f, init-keyword: exported:;
  slot created? :: <boolean>, init-value: #f, init-keyword: created:;
  //
  // The definition for this variable, or #f if not yet defined.
  slot variable-definition :: false-or(<definition>),
    init-value: #f;
  //
  // List of FER transformers for this variable.  Gets propagated to the defn
  // when the defn is installed.
  slot variable-transformers :: <list>, init-value: #();
  //
  // Function to compile-time evaluate calls to this function.
  slot variable-ct-evaluator :: false-or(<function>), init-value: #f;
end;

define method print-object (var :: <variable>, stream :: <stream>) => ();
  pprint-fields(var, stream, name: var.variable-name);
end;

// variable-name -- exported.
//
define generic variable-name (var :: <variable>) => name :: <symbol>;

// variable-definition -- exported.
//
define generic variable-definition (var :: <variable>)
    => defn :: false-or(<definition>);

// <use> -- exported.
//
define class <use> (<object>)
  //
  // The name of the library/module being used.
  slot name-used :: <symbol>,
    required-init-keyword: name:;
  //
  // Either a vector of names to import, of #t for all.
  slot imports :: type-union(<simple-object-vector>, <true>),
    required-init-keyword: imports:;
  //
  // Either a string prefix or #f if none.
  slot prefix :: false-or(<string>),
    required-init-keyword: prefix:;
  //
  // Vector of names to exclude.  Only non-empty if import is #t.
  slot excludes :: <simple-object-vector>,
    required-init-keyword: excludes:;
  //
  // Vector of renamings.  Any name in here is also in imports.
  slot renamings :: <simple-object-vector>,
    required-init-keyword: renamings:;
  //
  // Either a vector of names to re-export, or #t for all.
  slot exports :: type-union(<simple-object-vector>, <true>),
    required-init-keyword: exports:;
end;

define method print-object (u :: <use>, stream :: <stream>) => ();
  pprint-fields(u, stream,
		name-used: u.name-used,
		imports: u.imports,
		prefix: u.prefix,
		excludes: u.excludes,
		renamings: u.renamings,
		exports: u.exports);
end;

// <renaming> -- exported.
//
define class <renaming> (<object>)
  //
  // The name in the module/library being imported.
  slot orig-name :: <symbol>, required-init-keyword: orig-name:;
  //
  // The name the module/library is being imported as.
  slot new-name :: <symbol>, required-init-keyword: new-name:;
end;

define method print-object (ren :: <renaming>, stream :: <stream>) => ();
  pprint-fields(ren, stream, orig-name: ren.orig-name, new-name: ren.new-name);
end;


// Library access stuff.

// $Libraries -- internal.
//
// Hash table mapping names to <library> structures.
//
define constant $Libraries :: <object-table> = make(<object-table>);

// find-library -- exported.
//
// Find the library with the given name.  If we haven't asked for it
// before, create it.
//
define method find-library (name :: <symbol>) => result :: <library>;
  let lib = element($Libraries, name, default: #f);
  if (lib)
    lib;
  else
    //
    // Make a new library and stuff it into the global table.
    //
    let new = make(<library>, name: name);
    element($Libraries, name) := new;

    // Create the Dylan-User module.  Even though the dylan-user
    // module is defined in this library, we record the home of the
    // dylan-user module as the dylan library, so that the dylan-user
    // module's uses (dylan, extensions, etc.) get looked up in the
    // correct (i.e. dylan) library.
    let dylan-user = make(<module>,
			  name: #"Dylan-User",
			  home: find-library(#"Dylan"),
			  magic-tokens: name == #"Dylan");
    new.local-modules[#"Dylan-User"] := dylan-user;
    note-module-definition(new, #"Dylan-User",
			   if (name == #"Dylan")
			     #[];
			   else
			     map(method (name)
				   make(<use>, name: name, imports: #t,
					prefix: #f, excludes: #[],
					renamings: #[], exports: #[]);
				 end,
				 $Dylan-User-Uses);
			   end,
			   #[], #[]);

    // And return the new library.
    new;
  end;
end method;


// note-library-definition -- exported.
//
// Establish the definition for the named library.  Uses is a sequence
// of <use> structures, and exports is a sequence of names from export
// clauses.
//
define method note-library-definition
    (name :: <symbol>, uses :: <sequence>, exports :: <sequence>)
    => ();
  let lib = find-library(name);
  unless (lib == *Current-Library*)
    compiler-error("Defining strange library.");
  end;
  if (lib.defined?)
    compiler-error("Library %s is already defined.", name);
  end;
  lib.defined? := #t;
  lib.used-libraries := as(<simple-object-vector>, uses);
  //
  // Fill in exported-modules, creating modules as needed.
  //
  for (name in exports)
    lib.exported-modules[name] := find-module(lib, name, create: #t);
  end;
  //
  // ### Check to see that there are no ambiguities.
  //
end method;


// Module access stuff.

// find-module -- exported.
//
// Return the named module in the given library, or #f if there is no
// such module.  If create? is true, then create it instead of
// returning #f.
//
define method find-module (lib :: <library>, name :: <symbol>,
			   #key create: create?)
    => result :: false-or(<module>);
  let mod = element(lib.local-modules, name, default: #f);
  if (mod)
    mod;
  elseif (create?)
    let new = make(<module>, name: name, home: lib,
		   magic-tokens: lib == $Dylan-Library);
    lib.local-modules[name] := new;
    if (lib.defined?)
      // ### Check to see if this name classes with any of the
      // imported names.
      #f;
    end;
    new;
  elseif (lib.defined?)
    find-in-library-uses(lib, name, #f);
  elseif (lib == *Current-Library*)
    compiler-error("Can't look up modules in library %s "
		     "before it is defined.",
		   lib.library-name);
  else
    load-library(lib);
    find-module(lib, name);
  end;
end method;

// use-module -- exported.
//
// Flame out if we can't use the supplied module (because it isn't defined
// yet).
//
define method use-module (mod :: <module>) => ();
  unless (mod.defined?)
    compiler-error("Can't use module %s before it is defined.",
		   mod.module-name);
  end unless;
end method use-module;

// find-exported-module -- internal.
//
// Find the module in the library, but only if it is exported.
//
define method find-exported-module (lib :: <library>, name :: <symbol>)
    => result :: false-or(<module>);
  unless (lib.defined?)
    load-library(lib);
  end;
  element(lib.exported-modules, name, default: #f)
    | find-in-library-uses(lib, name, #t);
end;

// find-in-library-uses -- internal.
//
// Search through the use clauses for this library trying to find one
// that imports the named module.  If exported-only is true, then
// ignore any modules that are not re-exported.
//
define method find-in-library-uses (lib :: <library>, name :: <symbol>,
				    exported-only :: <boolean>)
    => result :: false-or(<module>);
  block (return)
    for (u in lib.used-libraries)
      let orig-name = guess-orig-name(u, name, exported-only);
      if (orig-name)
	let imported = find-exported-module(find-library(u.name-used),
					    orig-name);
	if (imported)
	  return(imported);
	end;
      end;
    end for;
    #f;
  end block;
end method;

// guess-orig-name -- internal.
//
// If name could be imported via the given use, return the original
// name of the exported variable.  In other words, apply any renaming
// or prefixing in reverse.  If name isn't imported via this use, then
// return #f.
//
define method guess-orig-name (u :: <use>, name :: <symbol>,
			       exported-only :: <boolean>)
    => result :: false-or(<symbol>);
  if (~exported-only | u.exports == #t | member?(name, u.exports))
    block (return)
      //
      // First check the renamings.
      // 
      for (ren in u.renamings)
	if (ren.new-name == name)
	  return(ren.orig-name);
	end;
      end;
      //
      // Next, remove the prefix, if there is one.
      //
      let guess = remove-prefix(u.prefix, name);
      unless (guess)
	return(#f);
      end;
      //
      // Now check it against the imports and excludes.
      //
      if (u.imports == #t)
	~member?(guess, u.excludes) & guess;
      else
	member?(guess, u.imports) & guess;
      end;
    end;
  end;
end method;

// remove-prefix -- internal.
//
// Either return name with the prefix removed, or #f if name isn't
// prefixed with prefix.  We also have a method for a prefix of #f
// just so that we don't have to test for that case before calling
// remove-prefix.
// 
define method remove-prefix (prefix :: <string>, name :: <symbol>)
    => result :: <symbol>;
  let name-str = as(<string>, name);
  if (name-str.size > prefix.size)
    block (return)
      for (name-char in name-str, prefix-char in prefix)
	unless (as-uppercase(name-char) == as-uppercase(prefix-char))
	  return(#f);
	end;
      finally
	as(<symbol>, copy-sequence(name-str, start: prefix.size));
      end;
    end;
  end;
end;
//
define method remove-prefix (prefix :: <false>, name :: <symbol>)
    => result :: <symbol>;
  name;
end;

// note-module-definition -- exported.
//
// Establish the definition for the named module in the given library.
// Uses is a sequence of <use> objects, and exports and creates are
// the names from the exports and creates options.
//
define method note-module-definition
    (lib :: <library>, name :: <symbol>, uses :: <sequence>,
     new-exports :: <sequence>, new-creates :: <sequence>)
    => ();
  let mod = find-module(lib, name, create: #t);
  unless (mod.module-home == lib | name == #"Dylan-User")
    // We suppress this test for dylan-user modules, because they are
    // homed in the dylan library.  Hence this test would always flame out.
    compiler-error("Can't define module %s in library %s, "
		     "because library %s already imports a module %s.",
		   name, lib.library-name, lib.library-name, name);
  end;
  if (mod.defined?)
    compiler-error("Module %s is already defined in library %s.",
		   name, lib.library-name);
  end;
  //
  // Mark it as defined, and record the uses for later.
  mod.defined? := #t;
  mod.used-modules := as(<simple-object-vector>, uses);
  //
  // Make variables for all the names in the export clauses.
  for (name in new-exports)
    let old = element(mod.variables, name, default: #f);
    if (old)
      mod.exported-variables[name] := old;
      old.exported? := #t;
    else
      let new = make(<variable>, name: name, home: mod, exported: #t);
      mod.variables[name] := new;
      mod.variable-names[new] := pair(name, element(mod.variable-names, new,
						    default: #()));
      mod.exported-variables[name] := new;
      add!(new.accessing-modules, mod);
    end;
  end;
  //
  // Make variables for all the names in the create clauses.
  for (name in new-creates)
    let old = element(mod.variables, name, default: #f);
    if (old)
      if (old.exported?)
	compiler-error
	  ("%s in both a create clause and an export clause in module %s",
	   name, mod.module-name);
      elseif (old.defined?)
	compiler-error("%s in create clause for module %s, so must be "
			 "defined elsewhere.",
		       name, mod.module-name);
      else
	mod.exported-variables[name] := old;
	old.created? := #t;
      end;
    else
      let new = make(<variable>, name: name, home: mod, created: #t);
      mod.variables[name] := new;
      mod.variable-names[new] := pair(name, element(mod.variable-names, new,
						    default: #()));
      mod.exported-variables[name] := new;
      add!(new.accessing-modules, mod);
    end;
  end;
  //
  // Pull in everything from the uses.
  for (u in mod.used-modules)
    let used-mod = find-module(mod.module-home, u.name-used);
    unless (used-mod)
      compiler-error("No module %s in library %s",
		     u.name-used, mod.module-home.library-name);
    end;
    if (used-mod == mod)
      compiler-error("Module %s can't use itself.", u.name-used);
    end if;
    unless (used-mod.defined?)
      compiler-error("Attempt to use module %s before it is defined.",
		     u.name-used);
    end unless;

    local
      method do-import (var :: false-or(<variable>), orig-name :: <symbol>,
			new-name :: <symbol>)
	//
	// Make sure it was even there.
	unless (var)
	  compiler-error("Can't import %s from module %s because it "
			   "isn't exported.",
			 orig-name, used-mod.module-name);
	end;
	//
	// Check to see if the new import causes a clash.
	// 
	let old = element(mod.variables, new-name, default: #f);
	if (old & ~(old == var))
	  if (new-name == orig-name)
	    compiler-error
	      ("Importing %s from module %s into module %s clashes.",
	       new-name, used-mod.module-name, mod.module-name);
	  else
	    compiler-error
	      ("Importing %s from module %s into module %s as %s clashes.",
	       orig-name, used-mod.module-name, mod.module-name, new-name);
	  end;
	end;
	//
	// Verify that any changes to the syntax table that need to be
	// made because of this import are okay.
	//
	check-syntax-table-additions(mod.module-syntax-table,
				     var.variable-definition,
				     new-name);
	//
	// No clash, so stick the variable in our table of local
	// variables, and our table of exported variables if this
	// import is being re-exported.
	//
	mod.variables[new-name] := var;
	mod.variable-names[var] := pair(new-name,
					element(mod.variable-names, var,
						default: #()));
	if (u.exports == #t | member?(new-name, u.exports))
	  mod.exported-variables[new-name] := var;
	end;
	//
	// Next, actually modify the syntax table.
	//
	make-syntax-table-additions(mod.module-syntax-table,
				    var.variable-definition,
				    new-name);
	//
	// And finally, stick us in the set of modules who can access this
	// variable.
	//
	unless (member?(mod, var.accessing-modules))
	  add!(var.accessing-modules, mod);
	end;
      end method;

    if (u.imports == #t)
      //
      // Import all the exported variables, unless compute-new-name
      // tells us it should be skipped.
      for (orig-name in key-sequence(used-mod.exported-variables))
	let var = used-mod.exported-variables[orig-name];
	let new-name = compute-new-name(u, orig-name);
	if (new-name)
	  do-import(var, orig-name, new-name);
	end;
      end;
    else
      //
      // Import everything listed.
      for (orig-name in u.imports)
	let var = find-exported-variable(used-mod, orig-name);
	let new-name = compute-new-name(u, orig-name);
	do-import(var, orig-name, new-name);
      end;
    end;
  end for;
end method;
	
// compute-new-name -- internal
//
// Figure out how name gets renamed or prefixed when imported via use.
// Return #f if it should be excluded.
//
define method compute-new-name (u :: <use>, name :: <symbol>)
    => result :: false-or(<symbol>);
  block (return)
    //
    // First, check the renamings.
    //
    for (ren in u.renamings)
      if (ren.orig-name == name)
	return(ren.new-name);
      end;
    end;
    //
    // Punt if the name should be excluded.
    //
    if (member?(name, u.excludes))
      return(#f);
    end;
    //
    // Next, add the prefix if there is one.
    //
    if (u.prefix)
      as(<symbol>, concatenate(u.prefix, as(<string>, name)));
    else
      name;
    end;
  end;
end method;


// Variable stuff.

// find-varible -- exported.
//
// Return the named variable from the given module.  If it doesn't
// already exist, either create it (if create is true) or return #f
// (if create is false).
//
define method find-variable (name :: <basic-name>, #key create: create?)
    => result :: false-or(<variable>);
  let mod = name.name-module;
  let sym = name.name-symbol;
  let var = element(mod.variables, sym, default: #f);
  if (var)
    var;
  elseif (create?)
    let new = make(<variable>, name: sym, home: mod);
    mod.variables[sym] := new;
    mod.variable-names[new] := pair(sym, element(mod.variable-names, new,
						 default: #()));
    add!(new.accessing-modules, mod);
    new;
  else
    #f;
  end;
end method;

// find-exported-variable -- internal.
//
// Return the named variable if it is there and exported, or #f if
// not.
// 
define method find-exported-variable (mod :: <module>, name :: <symbol>)
    => result :: false-or(<variable>);
  unless (mod.defined?)
    compiler-error("Module %s is not defined.", mod.module-name);
  end;
  element(mod.exported-variables, name, default: #f);
end method;

// note-variable-definition -- exported.
//
// Note that name is defined in module.
// 
define method note-variable-definition (defn :: <definition>)
    => ();
  //
  // Get the variable, creating it if necessary.
  //
  let name = defn.defn-name;
  let mod = name.name-module;
  let var = find-variable(defn.defn-name, create: #t);
  //
  // Make sure this module either is or is not the varibles home,
  // depending on whether the variable was in a create define module
  // clause or not.
  //
  if (var.created?)
    if (var.variable-home == mod)
      compiler-error("%s in create clause for module %s, so must be "
		       "defined elsewhere.",
		     name.name-symbol, mod.module-name);
    end;
  else
    unless (var.variable-home == mod)
      compiler-error
	("%s is imported into module %s, so can't be defined locally.",
	 name.name-symbol, mod.module-name);
    end;
  end;
  //
  // Make sure the variable isn't already defined.
  //
  if (var.variable-definition)
    unless (instance?(var.variable-definition, <implicit-definition>))
      compiler-error("%s in module %s multiply defined.",
		     name.name-symbol, mod.module-name);
    end;
  end;
  //
  // Make sure this defn doesn't introduce any problems in the
  // syntax tables of modules that can access this variable.
  //
  for (accessing-module in var.accessing-modules)
    for (imported-name in element(accessing-module.variable-names, var,
				  default: #()))
      check-syntax-table-additions(accessing-module.module-syntax-table,
				   defn, imported-name);
    end;
  end;
  //
  // Okay, record the definition and adjust the syntax tables.
  //
  var.variable-definition := defn;
  for (accessing-module in var.accessing-modules)
    for (imported-name in element(accessing-module.variable-names, var,
				  default: #()))
      make-syntax-table-additions(accessing-module.module-syntax-table,
				  defn, imported-name);
    end;
  end;
  //
  // And if it is a function definition and we have some function info,
  // propagate it over.
  if (~empty?(var.variable-transformers)
	& instance?(defn, <function-definition>))
    defn.function-defn-transformers := var.variable-transformers;
  end;
end;
//
// We ignore implicit definitions for variables already defined or from outside
// the module (unless the variable was set up with a create clause).
// 
define method note-variable-definition (defn :: <implicit-definition>,
					#next next-method)
  let var = find-variable(defn.defn-name, create: #t);
  unless (var.variable-definition)
    if (var.variable-home == defn.defn-name.name-module | var.created?)
      next-method();
    end;
  end;
end;


// Loading stuff.

define method load-library (lib :: <library>) => ();
  find-data-unit(lib.library-name, $library-summary-unit-type,
		 dispatcher: *compiler-dispatcher*);
end;

define variable *load-depth* :: <integer> = 0;

define method find-data-unit
    (name :: <symbol>, type == $library-summary-unit-type,
     #next next-method, #key)
    => res :: <object>;
  let lib = find-library(name);
  if (lib.defined?)
    next-method();
  else
    let previous-library = *Current-Library*;
    let previous-depth = *load-depth*;
    block ()
      *Current-Library* := lib;
      *load-depth* := previous-depth + 1;
      unless (zero?(previous-depth))
	write('\n', *debug-output*);
	for (i from 0 below *load-depth*)
	  write(' ', *debug-output*);
	end for;
      end unless;
      format(*debug-output*, "[Loading library %s...", name);
      force-output(*debug-output*);
      let res = next-method();
      unless (lib.defined?)
	compiler-error("Loaded library %s but it wasn't ever defined.", name);
      end unless;
      write(']', *debug-output*);
      if (zero?(previous-depth))
	write('\n', *debug-output*);
      end if;
      force-output(*debug-output*);
      res;
    cleanup
      *Current-Library* := previous-library;
      *load-depth* := previous-depth;
    end block;
  end if;
end method find-data-unit;



// Initilization stuff.

// *Current-Library* and *Current-Module* -- exported.
// 
// The Current Library and Module during a parse or load, or #f if we arn't
// parsing or loading at the moment.
// 
define variable *Current-Library* :: false-or(<library>) = #f;
define variable *Current-Module* :: false-or(<module>) = #f;

// $Dylan-Library and $Dylan-Module -- exported.
//
// The Dylan library and Dylan-Viscera module.
//
define constant $Dylan-Library = find-library(#"Dylan");
define constant $Dylan-Module
  = find-module($Dylan-Library, #"Dylan-Viscera", create: #t);


// Shorthands

// dylan-name -- ???
// 
define method dylan-name (sym :: <symbol>) => res :: <basic-name>;
  make(<basic-name>, symbol: sym, module: $Dylan-module);
end;

// dylan-var -- exported.
//
// Return the variable for name in the dylan module.
// 
define method dylan-var (name :: <symbol>, #key create: create?)
    => res :: false-or(<variable>);
  find-variable(dylan-name(name), create: create?);
end;

// dylan-defn -- exported.
//
// Return the definition for name in the dylan module.
// 
define method dylan-defn (name :: <symbol>)
    => res :: false-or(<definition>);
  let var = dylan-var(name);
  var & var.variable-definition;
end;

// dylan-value -- exported.
//
// Returns the compile-time value for the given name in the dylan module,
// or #f if it isn't defined.
// 
define method dylan-value (name :: <symbol>)
    => res :: false-or(<ct-value>);
  let defn = dylan-defn(name);
  defn & defn.ct-value;
end;

