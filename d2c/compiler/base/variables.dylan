module: variables
rcs-header: $Header: /scm/cvs/src/d2c/compiler/base/variables.dylan,v 1.7 2003/05/25 15:39:16 housel Exp $
copyright: see below

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000, 2001, 2002  Gwydion Dylan Maintainers
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

// $Dylan-User-Uses -- internal.
//
// Sequence of modules (in the Dylan library) that are automatically used
// by the implicit Dylan-User module that gets created in each library.
//
define constant $Dylan-User-Uses :: <vector> = #[#"Dylan", #"Extensions"];



// Definition interface classes.

// <use> -- exported.
//
// Used to represent a use clause in a define library or define module.
// 
define class <use> (<object>)
  //
  // The name of the library/module being used.
  constant slot use-name :: <symbol-token>,
    required-init-keyword: name:;
  //
  // Either a vector of names (<symbol-token>s) to import, or an <all-marker>.
  constant slot use-imports
      :: type-union(<simple-object-vector>, <all-marker>),
    required-init-keyword: imports:;
  //
  // Either a string prefix or #f if none.
  constant slot use-prefix :: false-or(<string>),
    required-init-keyword: prefix:;
  //
  // Vector of names (<symbol-token>s) to exclude.  Only non-empty if import
  // is #t.
  constant slot use-excludes :: <simple-object-vector>,
    required-init-keyword: excludes:;
  //
  // Vector of <renaming>s.  Any name in here is also in imports.
  constant slot use-renamings :: <simple-object-vector>,
    required-init-keyword: renamings:;
  //
  // Either a vector of names (<symbol-token>s) to re-export, or an
  // <all-marker>.
  constant slot use-exports
      :: type-union(<simple-object-vector>, <all-marker>),
    required-init-keyword: exports:;
end class <use>;

define sealed domain make (singleton(<use>));
define sealed domain initialize (<use>);

define method print-object (u :: <use>, stream :: <stream>) => ();
  pprint-fields
    (u, stream,
     name: u.use-name.token-symbol,
     imports:
       if (instance?(u.use-imports, <all-marker>))
	 #"all";
       else
	 map(token-symbol, u.use-imports);
       end if,
     prefix: u.use-prefix,
     excludes: map(token-symbol, u.use-excludes),
     renamings: u.use-renamings,
     exports:
       if (instance?(u.use-exports, <all-marker>))
	 #"all";
       else
	 map(token-symbol, u.use-exports);
       end if);
end method print-object;


// <all-marker> -- exported.
//
// Used in place of a vector of names to indicate that it should be all names.
// 
define class <all-marker> (<source-location-mixin>)
end class <all-marker>;


// <renaming> -- exported.
//
// Used to represent a single renaming in a module or library use clause.
//
define class <renaming> (<object>)
  //
  // The name in the module/library being imported.
  constant slot renaming-orig-name :: <symbol-token>,
    required-init-keyword: orig-name:;
  //
  // The name the module/library is being imported as.
  constant slot renaming-new-name :: <symbol-token>,
    required-init-keyword: new-name:;
end class <renaming>;

define sealed domain make (singleton(<renaming>));
define sealed domain initialize (<renaming>);

define method print-object (ren :: <renaming>, stream :: <stream>) => ();
  pprint-fields(ren, stream,
		orig-name: ren.renaming-orig-name.token-symbol,
		new-name: ren.renaming-new-name.token-symbol);
end method print-object;



// General namespace support.

// <namespace> -- internal.
//
// Shared superclass for libraries and modules.
// 
define abstract class <namespace> (<object>)
  //
  // The name of this namespace.
  constant slot namespace-name :: <symbol>,
    required-init-keyword: name:;
  //
  // #t once the namespace has been defined, #f until then.
  slot defined? :: <boolean> = #f;
  //
  // Sequence of all the names exported from this namespace.
  constant slot exported-names :: <stretchy-vector> = make(<stretchy-vector>);
  //
  // Hash table mapping symbols to entries for all the names visible in
  // this namespace.  Populated incrementally.
  constant slot entries :: <object-table> = make(<object-table>);
  //
  // A function to run at finalization time, that imports all bindings
  // that could not be looked up when the namespace was noted.
  slot deferred-importers :: false-or(<function>) = #f;
end class <namespace>;


// namespace-kind -- internal.
//
// Returns a string identifing the kind of namespace this is.  Used
// when generating error messages.
// 
define generic namespace-kind
    (namespace :: <namespace>) => res :: <byte-string>;


define class <entry> (<object>)
  //
  // The namespace this entry is part of.
  constant slot entry-namespace :: <namespace>,
    required-init-keyword: namespace:;
  //
  // The name this is an entry for.
  constant slot entry-name :: <symbol>,
    required-init-keyword: name:;
  //
  // #t if this name is exported, #f otherwise.
  slot entry-exported? :: <boolean> = #f;
  //
  // Where this entry came from.
  constant slot entry-origin :: <byte-string>,
    required-init-keyword: origin:;
  //
  // The thing being referenced.
  constant slot entry-constituent :: <namespace-constituent>,
    required-init-keyword: constituent:;
  //
  // The next entry in a linked list of all entries for the constituent.
  constant slot entry-next :: false-or(<entry>),
    required-init-keyword: next:;
end class <entry>;


// <namespace-constituent> -- internal.
//
// Shared superclass for things that can be found in namespaces -- i.e.
// modules and variables.
// 
define abstract class <namespace-constituent> (<object>)
  //
  // Linked list of all the entries refering to this constituent.
  slot constituent-entries :: false-or(<entry>) = #f;
  //
  // #t if originally in an export or create clause, #f otherwise.
  slot exported? :: <boolean> = #f;
  slot created? :: <boolean> = #f;
end class <namespace-constituent>;



define method add-entry
    (namespace :: <namespace>, name :: <symbol>,
     origin :: <byte-string>, constituent :: <namespace-constituent>)
    => res :: <entry>;
  let old = element(namespace.entries, name, default: #f);
  if (old & old.entry-constituent ~== constituent)
    error("Trying to overwrite the entry for %s in %s %s",
	  name, namespace.namespace-kind, namespace.namespace-name);
  else
    let new = make(<entry>, namespace: namespace, name: name, origin: origin,
		   constituent: constituent,
		   next: constituent.constituent-entries);
    constituent.constituent-entries := new;
    element(namespace.entries, name) := new;
    new;
  end if;
end method add-entry;


define method note-namespace-definition
    (namespace :: <namespace>, uses :: <simple-object-vector>,
     exports :: <simple-object-vector>, creates :: <simple-object-vector>)
    => ();
  //
  // Flag the namespace as defined.  We don't have to worry about redefining
  // a previously defined namespace because that is the responsibility of
  // whoever calls this.  Cause they can produce a better error message.
  namespace.defined? := #t;
  //
  // Process the exports.
  for (token in exports)
    let name = token.token-symbol;
    let old = element(namespace.entries, name, default: #f);
    if (old)
      //
      // We've already created the entry for some reason, so just export it.
      unless (old.entry-exported?)
	add!(namespace.exported-names, name);
	old.entry-exported? := #t;
      end unless;
      old.entry-constituent.exported? := #t;
    else
      let constituent = make-constituent(namespace, name);
      constituent.exported? := #t;
      add!(namespace.exported-names, name);
      namespace.entries[name].entry-exported? := #t;
    end if;
  end for;
  //
  // Process the creates.
  for (token in creates)
    let name = token.token-symbol;
    let old = element(namespace.entries, name, default: #f);
    if (old)
      if (old.entry-constituent.exported?)
	compiler-error-location
	  (token, "%s in both a create clause and an export clause in %s %s",
	   name, namespace.namespace-kind, namespace.namespace-name);
      else
	unless (old.entry-exported?)
	  add!(namespace.exported-names, name);
	  old.entry-exported? := #t;
	end unless;
	old.entry-constituent.created? := #t;
      end if;
    else
      let constituent = make-constituent(namespace, name);
      constituent.created? := #t;
      add!(namespace.exported-names, name);
      namespace.entries[name].entry-exported? := #t;
    end if;
  end for;
  //
  // Process the uses.
  for (use in uses)
    block (skip-use)
      let used-namespace
	= block ()
	    lookup-use(namespace, use.use-name);
	  exception (<fatal-error-recovery-restart>)
	    skip-use();
	  end block;

      if (used-namespace == namespace)
	compiler-error-location
	  (use.use-name, "%s %s can't use itself.",
	   namespace.namespace-kind, use.use-name.token-symbol);
	skip-use();
      end if;

      let imports = use.use-imports;
      if (instance?(imports, <all-marker>))
	//
	// Import all the exported variables.
	local method import-all() => ();
		let srcloc = imports.source-location;
		for (orig-name in used-namespace.exported-names)
		  do-import(namespace, used-namespace, orig-name, srcloc, use);
		end for;
	      end;
	
	if (used-namespace.exported-names.empty?)
	  // this is a good indication to retry later...
	  let importers-so-far = namespace.deferred-importers;
	  namespace.deferred-importers
	    := method(ns :: <namespace>) => ();
		   importers-so-far(ns);
		   import-all();
	       end method;
	else
	  import-all();
	end if;
      else
	//
	// Import everything listed.
	for (token in use.use-imports)
	  do-import(namespace, used-namespace, token.token-symbol,
		    token.source-location, use);
	end for;
      end if;
    end block;
  end for;
end method note-namespace-definition;


// make-constituent -- internal.
//
// Used by definition processing stuff to lookup the namespace used by a
// use clause.
// 
define generic make-constituent
    (namespace :: <namespace>, name :: <symbol>)
    => constituent :: <namespace-constituent>;


// lookup-use -- internal.
//
// Used by definition processing stuff to lookup the namespace used by a
// use clause.
// 
define generic lookup-use
    (namespace :: <namespace>, token :: <symbol-token>)
    => used-namespace :: <namespace>;


define method do-import
    (into :: <namespace>, from :: <namespace>, orig-name :: <symbol>,
     srcloc :: <source-location>, use :: <use>)
    => ();
  block (return)
    let (new-name, srcloc) = compute-new-name(use, orig-name, srcloc);

    unless (new-name)
      return();
    end unless;

    let entry = element(from.entries, orig-name, default: #f);
    unless (entry & entry.entry-exported?)
      // we try to defer the import first...
      let importers-so-far = into.deferred-importers;
      if (importers-so-far)
	into.deferred-importers
	  := method(ns :: <namespace>) => ();
		 importers-so-far(ns);
		 do-import(into, from, orig-name, srcloc, use);
	     end method;
      else
	compiler-error-location
	  (srcloc, "Can't import %s from %s %s because it isn't exported.",
	   orig-name, from.namespace-kind, from.namespace-name);
      end if;
      return();
    end unless;
    let constituent = entry.entry-constituent;

    // Check to see if there is already an entry for that name.
    let old = element(into.entries, new-name, default: #f);
    if (old)
      //
      // There is.  Check to see if it is just a duplicate, or if it is
      // a problem.
      if (old.entry-constituent ~== constituent)
	if (new-name == orig-name)
	  compiler-error-location
	    (srcloc,
	     "Can't import %s from %s %s into %s %s because it would "
	       "clash with %s %s.",
	     new-name, from.namespace-kind, from.namespace-name,
	     into.namespace-kind, into.namespace-name,
	     new-name, old.entry-origin);
	else
	  compiler-error-location
	    (srcloc,
	     "Can't import %s as %s from %s %s into %s %s because it would "
	       "clash with %s %s.",
	     orig-name, new-name, from.namespace-kind, from.namespace-name,
	     into.namespace-kind, into.namespace-name,
	     new-name, old.entry-origin);
	end if;
      end if;
      //
      // Either way, we don't have to do anything more.
      return();
    end if;

    do-import-aux(into, from, constituent, orig-name, new-name, srcloc, use);
  end block;
end method do-import;


// compute-new-name -- internal
//
// Figure out how name gets renamed or prefixed when imported via use.
// Return #f if it should be excluded.
//
define method compute-new-name
    (use :: <use>, name :: <symbol>, srcloc :: <source-location>)
    => (result :: false-or(<symbol>), srcloc :: false-or(<source-location>));
  block (return)
    //
    // First, check the renamings.
    //
    for (ren in use.use-renamings)
      if (ren.renaming-orig-name.token-symbol == name)
	let new-name = ren.renaming-new-name;
	return(new-name.token-symbol, new-name.source-location);
      end if;
    end for;
    //
    // Punt if the name should be excluded.
    //
    for (exclude in use.use-excludes)
      if (exclude.token-symbol == name)
	return(#f, #f);
      end if;
    end for;
    //
    // Next, add the prefix if there is one.
    //
    if (use.use-prefix)
      values(symcat(use.use-prefix, name), srcloc);
    else
      values(name, srcloc);
    end if;
  end block;
end method compute-new-name;


// do-import-aux -- internal.
//
// Called once we've verified all the stuff common to namespaces in general.
// It should make whatever namespace specific checks it needs to and then
// actually install the import.
//
define generic do-import-aux
    (into :: <namespace>, from :: <namespace>,
     constituent :: <namespace-constituent>,
     orig-name :: <symbol>, new-name :: <symbol>,
     srcloc :: <source-location>, use :: <use>)
    => ();

// do-import-aux{<namespace>} -- method in internal GF.
//
// By default, we just install the import.
//
define method do-import-aux
    (into :: <namespace>, from :: <namespace>,
     constituent :: <namespace-constituent>,
     orig-name :: <symbol>, new-name :: <symbol>,
     srcloc :: <source-location>, use :: <use>)
    => ();
  let new = add-entry(into, new-name,
		      stringify("imported from ", from.namespace-kind, ' ',
				as(<byte-string>, from.namespace-name)),
		      constituent);
  if (instance?(use.use-exports, <all-marker>)
	| block (exported)
	    for (export in use.use-exports)
	      if (export.token-symbol == new-name)
		exported(#t);
	      end if;
	    end for;
	    #f;
	  end block)
    add!(into.exported-names, new-name);
    new.entry-exported? := #t;
  end if;
end method do-import-aux;



// Libraries.

// <library> -- exported.
//
define class <library> (<namespace>)
  //
  // Set to #t if we barf while trying to load it.
  slot broken? :: <boolean> = #f;
end class <library>;

define sealed domain make (singleton(<library>));
define sealed domain initialize (<library>);

define method library-name (lib :: <library>) => name :: <symbol>;
  lib.namespace-name;
end method library-name;

define method print-object (lib :: <library>, stream :: <stream>) => ();
  pprint-fields(lib, stream, name: lib.library-name);
end method print-object;

define method namespace-kind (lib :: <library>) => res :: <byte-string>;
  "library";
end method namespace-kind;



// $Libraries -- internal.
//
// Hash table mapping names to <library> structures.
//
define constant $Libraries :: <object-table> = make(<object-table>);

// find-library -- exported.
//
// Find the library with the given name.  If it doesn't already exist and
// create is true, create it.
//
define method find-library
    (name :: <symbol>, #key create: create? :: <boolean>)
    => result :: false-or(<library>);
  let lib = element($Libraries, name, default: #f);
  
  if (lib)
    //
    // The library already exists, so return it.
    lib;

  elseif (create?)
    //
    // Make a new library and stuff it into the global table.
    let new = make(<library>, name: name);
    element($Libraries, name) := new;
    //
    // The Dylan library does not have a Dylan-User module.
    unless (name == #"Dylan")
      //
      // Create the Dylan-User module.
      let dylan-user = make(<module>, name: #"Dylan-User", home: new);
      add-entry(new, #"Dylan-User",
		"magically created by the system", dylan-user);
      //
      // We use note-namespace-definition instead of note-module-definition
      // because we don't need to do any of the error checks in n-m-d, and
      // this way we don't need to come up with a token for the name.
      note-namespace-definition
	(dylan-user,
	 map(method (name :: <symbol>) => use :: <use>;
	       make(<use>, name: token-for-symbol(name),
		    imports: make(<all-marker>), prefix: #f, excludes: #[],
		    renamings: #[], exports: #[]);
	     end,
	     $Dylan-User-Uses),
	 #[], #[]);
    end unless;
    //
    // Return the newly created library.
    new;

  else
    //
    // Doesn't exist, and we don't want to create it.  Return #f.
    #f;
  end if;
end method find-library;


define method make-constituent
    (namespace :: <library>, name :: <symbol>)
    => res :: <module>;
  find-module(namespace, name, create: #t);
end method make-constituent;


define method lookup-use
    (namespace :: <library>, token :: <symbol-token>)
    => used-namespace :: <library>;
  let lib = find-library(token.token-symbol, create: #t);
  assure-loaded(lib);
  if (lib.broken?)
    compiler-fatal-error-location
      (token, "Using broken library %s", token.token-symbol);
  end if;
  lib;
end method lookup-use;


define method assure-loaded (lib :: <library>) => ();
  unless (lib.defined? | lib.broken?)
    block ()
      find-data-unit(lib.library-name, $library-summary-unit-type,
		     dispatcher: *compiler-dispatcher*);
    exception (<fatal-error-recovery-restart>)
      #f;
    end block;
  end unless;
end method assure-loaded;



// note-library-definition -- exported.
//
// Establish the definition for the named library.  Uses is a sequence
// of <use> structures, and exports is a sequence of names from export
// clauses.
//
define method note-library-definition
    (token :: <symbol-token>, uses :: <simple-object-vector>,
     exports :: <simple-object-vector>)
    => ();
  let name = token.token-symbol;
  let lib = *Current-Library*;
  if (lib.library-name ~== name)
    compiler-error-location
      (token, "Defining strange library: %s isn't %s.",
       name, lib.library-name);
  elseif (lib.defined?)
    compiler-error-location
      (token, "Duplicate definition for library %s.", name);
  else
    note-namespace-definition(lib, uses, exports, #[]);
  end if;
end method note-library-definition;




// Module access stuff.

// <module> -- exported.
//
define class <module> (<namespace>, <namespace-constituent>,
		       <identity-preserving-mixin>)
  inherited slot deferred-importers
    = method(module :: <module>) => (); module.deferred-importers := #f end;
  //
  // The library this module lives in.
  constant slot module-home :: <library>, required-init-keyword: home:;
  //
  // Hash table mapping names to syntactic categories.
  constant slot module-syntax-table :: <syntax-table> = make(<syntax-table>);
end class <module>;

define sealed domain make (singleton(<module>));
define sealed domain initialize (<module>);

define method print-object (mod :: <module>, stream :: <stream>) => ();
  pprint-fields(mod, stream, name: mod.module-name);
end method print-object;

define method print-message (mod :: <module>, stream :: <stream>) => ();
  format(stream, "module %s:%s",
	 mod.module-home.library-name,
	 mod.module-name);
end method print-message;

define method namespace-kind (lib :: <module>) => res :: <byte-string>;
  "module";
end method namespace-kind;

// module-name -- exported.
//
define method module-name (mod :: <module>) => name :: <symbol>;
  mod.namespace-name;
end method module-name;


// find-module -- exported.
//
// Return the named module in the given library, or flame out if there is no
// such module.  If create? is true, then create it instead of flaming.
//
define method find-module
    (lib :: <library>, name :: <symbol>,
     #key create: create? :: <boolean>,
          srcloc :: false-or(<source-location>))
    => result :: <module>;
  let entry = element(lib.entries, name, default: #f);
  if (entry)
    entry.entry-constituent;
  elseif (create?)
    let new = make(<module>, name: name, home: lib);
    add-entry(lib, name,
	      stringify("defined inside library ",
			as(<byte-string>, lib.library-name)),
	      new);
    new;
  else
    let srcloc = srcloc | make(<unknown-source-location>);
    if (lib.defined?)
      compiler-fatal-error-location
	(srcloc, "No such module %s in library %s.",
	 name, lib.library-name);
    else
      compiler-fatal-error-location
	(srcloc, "Attempting to use library %s before it is defined.",
	 lib.library-name);
    end if;
  end if;
end method find-module;


define method make-constituent
    (namespace :: <module>, name :: <symbol>)
    => res :: <variable>;
  find-variable(make(<basic-name>, module: namespace, symbol: name),
		create: #t);
end method make-constituent;


// lookup-use{<module>} -- method on internal GF.
//
// If it is a dylan-user module, look up the name in the Dylan library.
// Otherwise, look up the name in the library the module lives in.
//
define method lookup-use
    (module :: <module>, token :: <symbol-token>)
    => used-namespace :: <module>;
  let lib
    = if (module.module-name == #"Dylan-User")
	assure-loaded($Dylan-Library);
	if ($Dylan-Library.broken?)
	  compiler-fatal-error
	    ("Skipping use of %s in module Dylan-User because library Dylan "
	       "is broken.",
	     token.token-symbol);
	end if;
	$Dylan-Library;
      else
	module.module-home;
      end if;
  find-module(lib, token.token-symbol, srcloc: token.source-location);
end method lookup-use;


// note-module-definition -- exported.
//
// Establish the definition for the named module in the given library.
// Uses is a sequence of <use> objects, and exports and creates are
// the names from the exports and creates options.
//
define method note-module-definition
    (lib :: <library>, name :: <symbol-token>, uses :: <simple-object-vector>,
     exports :: <simple-object-vector>, creates :: <simple-object-vector>)
    => ();
  let mod = find-module(lib, name.token-symbol, create: #t);
  if (mod.module-home ~== lib)
    compiler-error-location
      (name,
       "Can't define module %s in library %s, "
	 "because library %s already imports a module %s.",
       name.token-symbol, lib.library-name, lib.library-name,
       name.token-symbol);
  elseif (mod.defined?)
    compiler-error-location
      (name,
       "Duplicate definition for module %s in library %s.",
       name.token-symbol, lib.library-name);
  else
    note-namespace-definition(mod, uses, exports, creates);
  end if;
end method note-module-definition;


// Variable stuff.

// <variable> -- exported.
// 
define class <variable> (<namespace-constituent>)
  //
  // The name of the variable, as a symbol.
  constant slot variable-name :: <symbol>, required-init-keyword: name:;
  // 
  // The module this variable lives in.  Note: this is not necessarily
  // the same as where it is defined, because the create clause in
  // define module forms creates a variable, but requires it to be
  // defined elsewhere.
  constant slot variable-home :: <module>, required-init-keyword: home:;
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
  //
  // Function to build some parse tree out of fragments.  Called because of
  // references in procedural macros.
  slot variable-fragment-expander :: false-or(<function>) = #f;
end class <variable>;

define sealed domain make (singleton(<variable>));
define sealed domain initialize (<variable>);

define method print-object (var :: <variable>, stream :: <stream>) => ();
  pprint-fields(var, stream, name: var.variable-name);
end method print-object;

// variable-name -- exported.
//
define generic variable-name (var :: <variable>) => name :: <symbol>;

// variable-definition -- exported.
//
define generic variable-definition (var :: <variable>)
    => defn :: false-or(<definition>);


// find-variable -- exported.
//
// Return the named variable from the given module.  If it doesn't
// already exist, either create it (if create is true) or return #f
// (if create is false).
//
define method find-variable (name :: <basic-name>, #key create: create?)
    => result :: false-or(<variable>);
  let mod = name.name-module;
  let sym = name.name-symbol;
  let entry = element(mod.entries, sym, default: #f);
  if (entry)
    entry.entry-constituent;
  elseif (create?)
    let new = make(<variable>, name: sym, home: mod);
    add-entry(mod, sym,
	      stringify("defined inside module ",
			as(<byte-string>, mod.module-name)),
	      new);
    new;
  end if;
end method find-variable;


// name-inherited-or-exported?  --  exported
//
// Return #t if the variable named by name is inherited from another library or
// is exported from this library.  This function is used to determine which
// definitions might semantically be visible to other libraries, hence need to
// be dumped in the library summary.  A name is exposed if:
//  1] The variable's home library is different from the referencing library.
//  2] The variable is exported from some exported module.
//
// Determining whether the variable is exported from some module that it is
// visible in is pretty inefficient, since we have no idea what name(s) it
// might be exported under.
//
define method name-inherited-or-exported? (name :: <basic-name>)
  => res :: <boolean>;
  block (return)
    let var = find-variable(name);

    if (var == #f)
      return(#f);
    end if;

    if (var.variable-home.module-home ~== name.name-module.module-home)
      return(#t);
    end if;

    if (var.exported? | var.created?)
      for (entry = var.constituent-entries then entry.entry-next,
	   while: entry)
	let module = entry.entry-namespace;
	if (module.exported? | module.created?)
	  return(#t);
	end if;
      end for;
    end if;

    #f;
  end block;
end method name-inherited-or-exported?;
//
define method name-inherited-or-exported? (name :: <method-name>)
    => res :: <boolean>;
  name-inherited-or-exported?(name.method-name-generic-function);
end method name-inherited-or-exported?;



define method do-import-aux
    (into :: <module>, from :: <module>, var :: <variable>,
     orig-name :: <symbol>, new-name :: <symbol>, srcloc :: <source-location>,
     use :: <use>, #next next-method)
    => ();
  let defn = var.variable-definition;
  let (word, category) = defn & definition-syntax-info(defn, new-name);
  let table = into.module-syntax-table;
  let problem = word & problem-with-category-merge(table, word, category);

  if (problem)
    if (new-name == orig-name)
      compiler-error-location
	(srcloc,
	 "Can't import %s into module %s because doing so would make %s "
	   "be a %s word, but it is already a %s word.",
	 new-name, into.module-name, word, category, problem);
    else
      compiler-error-location
	(srcloc,
	 "Can't import %s as %s into module %s because doing so would "
	   "make %s be a %s word, but it is already a %s word.",
	 orig-name, new-name, into.module-name, word, category, problem);
    end if;
  else
    next-method();

    if (word)
      merge-category(table, word, category);
    end if;
  end if;
end method do-import-aux;


// note-variable-definition -- exported.
//
// Note that name is defined in module.
// 
define method note-variable-definition (defn :: <definition>)
    => ();
  block (return)
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
	compiler-error-location
	  (defn,
	   "%s is in a create clause for module %s, so must be "
	     "defined elsewhere.",
	   name.name-symbol, mod.module-name);
	return();
      end if;
    else
      unless (var.variable-home == mod)
	compiler-error-location
	  (defn, "%s is imported into module %s, so can't be defined locally.",
	   name.name-symbol, mod.module-name);
	return();
      end unless;
    end if;
    //
    // Make sure the variable isn't already defined.
    //
    if (var.variable-definition)
      unless (instance?(var.variable-definition, <implicit-definition>))
	compiler-error-location
	  (defn, "Duplicate definition for %s in module %s.",
	   name.name-symbol, mod.module-name);
	return();
      end unless;
    end if;
    //
    // Make sure this defn doesn't introduce any problems in the
    // syntax tables of modules that can access this variable.
    //
    for (entry = var.constituent-entries then entry.entry-next,
	 while: entry)
      let (word, category) = definition-syntax-info(defn, entry.entry-name);
      if (word)
	let table = entry.entry-namespace.module-syntax-table;
	let problem = problem-with-category-merge(table, word, category);
	if (problem)
	  compiler-error-location
	    (defn,
	     "Can't define %s in module %s as a %s because doing so would make"
	       " %s in module %s be a %s word, but it is already a %s word.",
	     name.name-symbol, mod.module-name, defn.definition-kind,
	     word, entry.entry-namespace.module-name, category, problem);
	end if;
      end if;
    end for;
    //
    // Okay, record the definition.
    //
    var.variable-definition := defn;
    //
    // And adjust the syntax tables.
    //
    for (entry = var.constituent-entries then entry.entry-next,
	 while: entry)
      let (word, category) = definition-syntax-info(defn, entry.entry-name);
      if (word)
	let table = entry.entry-namespace.module-syntax-table;
	merge-category(table, word, category);
      end if;
    end for;
    //
    // If we have some transformers, propagate them over.
    // 
    if (~empty?(var.variable-transformers))
      install-transformers(defn, var.variable-transformers);
    end if;
  end block;
end method note-variable-definition;
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
    end if;
  end unless;
end method note-variable-definition;


// Loading stuff.

define variable *load-depth* :: <integer> = 0;

define method find-data-unit
    (name :: <symbol>, type == $library-summary-unit-type,
     #next next-method, #key)
    => res :: <object>;
  let lib = find-library(name, create: #t);
  if (lib.defined?)
    next-method();
  else
    let previous-library = *Current-Library*;
    let previous-depth = *load-depth*;
    block ()
      *Current-Library* := lib;
      *load-depth* := previous-depth + 1;
      unless (zero?(previous-depth))
	new-line(*debug-output*);
	for (i from 0 below *load-depth*)
	  write-element(*debug-output*, ' ');
	end for;
      end unless;
      format(*debug-output*, "[Loading library %s...", name);
      force-output(*debug-output*);
      let handler (<error>)
	= method (cond :: <error>, next :: <function>)
	      => res :: <never-returns>;
	    lib.broken? := #t;
	    compiler-fatal-error
	      ("Puked loading library %s:\n  %s", name, cond);
	  end method;
      let res = next-method();
      unless (lib.defined?)
	error("Loaded library %s but it wasn't ever defined.", name);
      end unless;
      res;
    cleanup
      write-element(*debug-output*, ']');
      if (zero?(previous-depth))
	new-line(*debug-output*);
      end if;
      force-output(*debug-output*);
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
define constant $Dylan-Library = find-library(#"Dylan", create: #t);
define constant $Dylan-Module
  = find-module($Dylan-Library, #"Dylan-Viscera", create: #t);


// Bootstrap module stuff.

// $Bootstrap-Module -- exported.
//
// Handle on the bootstrap module.
//
define constant $Bootstrap-Module
  = find-module($Dylan-Library, #"Bootstrap", create: #t);

// $bootstrap-exports -- internal.
//
// Names to export from the bootstrap module.
// 
define constant $bootstrap-exports :: <stretchy-vector>
  = make(<stretchy-vector>);

// add-bootstrap-export -- exported.
//
// Record that name is supposed to be exported from the bootstrap module.
// 
define method add-bootstrap-export (name :: <symbol>) => ();
  if ($bootstrap-module.defined?)
    error("Trying to add an export to the bootstrap module after it has"
	    " been defined.");
  end if;
  add!($bootstrap-exports, token-for-symbol(name));
end method add-bootstrap-export;

// define-bootstrap-module -- exported.
//
// Actually define the bootstrap module.
// 
define method define-bootstrap-module () => ();
  note-module-definition
    ($Dylan-Library, token-for-symbol(#"Bootstrap"), #[],
     as(<simple-object-vector>, $bootstrap-exports), #[]);
end method define-bootstrap-module;


// Shorthands

define method token-for-symbol (sym :: <symbol>) => res :: <symbol-token>;
  make(<symbol-token>, kind: $error-token, symbol: sym);
end method token-for-symbol;

// dylan-name -- ???
// 
define method dylan-name (sym :: <symbol>) => res :: <basic-name>;
  make(<basic-name>, symbol: sym, module: $Dylan-module);
end method dylan-name;

// dylan-var -- exported.
//
// Return the variable for name in the dylan module.
// 
define method dylan-var (name :: <symbol>, #key create: create?)
    => res :: false-or(<variable>);
  find-variable(dylan-name(name), create: create?);
end method dylan-var;

// dylan-defn -- exported.
//
// Return the definition for name in the dylan module.
// 
define method dylan-defn (name :: <symbol>)
    => res :: false-or(<definition>);
  let var = dylan-var(name);
  var & var.variable-definition;
end method dylan-defn;

// dylan-value -- exported.
//
// Returns the compile-time value for the given name in the dylan module,
// or #f if it isn't defined.
// 
define method dylan-value (name :: <symbol>)
    => res :: false-or(<ct-value>);
  let defn = dylan-defn(name);
  defn & defn.ct-value;
end method dylan-value;


// Dumping stuff.

add-make-dumper(#"library", *compiler-dispatcher*, <library>,
		list(library-name, #f, #f),
		dumper-only: #t);

add-od-loader(*compiler-dispatcher*, #"library",
  method (state :: <load-state>) => res :: <library>;
    find-library(load-sole-subobject(state));
  end method
);


add-make-dumper(#"module", *compiler-dispatcher*, <module>,
		list(module-home, #f, #f,
		     module-name, #f, #f),
		dumper-only: #t);

add-od-loader(*compiler-dispatcher*, #"module",
  method (state :: <load-state>) => res :: <module>;
    let lib = load-object-dispatch(state);
    let mod-name = load-object-dispatch(state);
    assert-end-object(state);
    let mod = find-module(lib, mod-name, create: #t);
    mod.deferred-importers := #f;
    mod
  end method
);


add-make-dumper(#"module-variable", *compiler-dispatcher*, <variable>,
		list(variable-home, #f, #f,
		     variable-name, #f, #f),
		dumper-only: #t);

add-od-loader(*compiler-dispatcher*, #"module-variable", 
  method (state :: <load-state>) => res :: <variable>;
    find-variable(load-basic-name(state), create: #t);
  end method
);



add-make-dumper(#"use", *compiler-dispatcher*, <use>, 
		list(use-name, #"name", #f,
		     use-imports, #"imports", #f,
		     use-prefix, #"prefix", #f,
		     use-excludes, #"excludes", #f,
		     use-renamings, #"renamings", #f,
		     use-exports, #"exports", #f));

add-make-dumper(#"all-marker", *compiler-dispatcher*, <all-marker>,
		list(source-location, #"source-location", #f));

add-make-dumper(#"renaming", *compiler-dispatcher*, <renaming>, 
		list(renaming-orig-name, #"orig-name", #f,
		     renaming-new-name, #"new-name", #f));
