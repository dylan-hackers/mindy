module: definitions
copyright: see below




//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
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

// Value used for defn-dynamic? when the init keyword is not specified.  Note
// that the init keyword is always supplied by the OD loader, so this doesn't
// affect inherited definitions.
define variable *defn-dynamic-default* :: <boolean> = #f;

// <definition> -- exported.
//
// Abstract class that all definitions inherit from.  A definition is the
// compilers handle on some run-time value.  In addition to the obvious things,
// definitions exist for things like the type of a variable (when it isn't a
// compile-time constant).
// 
define open primary abstract class <definition> (<source-location-mixin>)
  //
  // The name this is the definition for.
  slot defn-name :: <name>, required-init-keyword: name:;
  //
  // The library making the definition.  Might be different than the library
  // burried inside the defn-name, because the defn-name might refer to a
  // variable pulled in from some other library.
  slot defn-library :: <library>, required-init-keyword: library:;
  //
  // True if this definition is allowed to be dynamically redefined, and thus
  // we can't really make use of any info except for warnings.
  slot defn-dynamic? :: <boolean> = *defn-dynamic-default*,
       init-keyword: dynamic:;
end;

/*
define method print-object (defn :: <definition>, stream :: <stream>) => ();
  pprint-fields(defn, stream, name: defn.defn-name);
end;
*/

/*
define open generic defn-type (defn :: <definition>)
    => res :: false-or(<ctype>);
*/


// install-transformers -- exported.
//
// Called by the variable system to install the transformers for a definition.
// 
define open generic install-transformers
    (defn :: <definition>, transformers :: <list>)
    => ();
//
// By default, we do nothing.
// 
define method install-transformers
    (defn :: <definition>, transformers :: <list>)
    => ();
end method install-transformers;



// definition-syntax-info -- exported.
//
// Return the word and category for that word that this definition imposes
// syntax on.
// 
define open generic definition-syntax-info
    (defn :: <definition>, name :: <symbol>)
    => (word :: false-or(<symbol>), category :: false-or(<symbol>));
//
define method definition-syntax-info
    (defn :: <definition>, name :: <symbol>)
    => (word :: false-or(<symbol>), category :: false-or(<symbol>));
  values(name, #"ordinary");
end method definition-syntax-info;


// definition-kind -- exported.
//
// Return some string that can be used in error messages to talk about the
// kind of definition this is.
// 
define open generic definition-kind
    (defn :: <definition>) => kind :: <byte-string>;



define open abstract class <implicit-definition> (<definition>)
end;

define open abstract class <abstract-constant-definition> (<definition>)
end;

define open abstract class <abstract-variable-definition> (<definition>)
end;


define open primary abstract class <function-definition>
    (<abstract-constant-definition>)
  //
end;

define open primary abstract class <class-definition> 
    (<abstract-constant-definition>)
end class;

// definition-kind{<class-definition>} -- method on exported GF
//
define method definition-kind
    (defn :: <class-definition>) => kind :: <byte-string>;
  "class";
end method definition-kind;

