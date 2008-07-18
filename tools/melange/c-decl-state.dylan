documented: #t
module: c-declarations
copyright: see below
	   This code was produced by the Gwydion Project at Carnegie Mellon
	   University.  If you are interested in using this code, contact
	   "Scott.Fahlman@cs.cmu.edu" (Internet).

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

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
// All rights reserved.
//
//======================================================================

//======================================================================
// c-decl-state.dylan attempts to encapsulate the interface between low-level
// parsing and the higher level set of "declarations" derived from that
// parsing.  This includes declarations for the "parse-state" which is first
// created and populated by the parser and later updated with user preferences
// from the interface definition.
//
// This also includes several functions designed to be called from within the
// parser in order to create new declarations.  They are defined here because
// they include "higher level" knowledge of <declaration>s than the parser
// really needs.
//======================================================================

//----------------------------------------------------------------------
// <Parse-state> definitions
// 
// <Parse-state> encapsulates all information required to parse a file or
// expression, includeing the tokenizer, and may also stores the "results" of
// the parse for later processing.
//
// <parse-file-state> is a subclass used for parsing entire files of
// declarations.  The resulting state will then be passed up to the
// "define-interface" layer for further manipulation.
//
// <parse-value-state> is used for parsing simple expressions or type names.
// Since these parses simply aim to compute a single value, they have less
// internal structure and are thrown away as soon as the parse is complete.
//----------------------------------------------------------------------

// All <parse-state> objects share these slots.
//
define abstract class <parse-state> (<object>)
  slot objects :: <table>;
  slot structs :: <table>;
  slot tokenizer :: <tokenizer>, required-init-keyword: #"tokenizer";
  slot pointers :: <table>;
  slot vectors :: <table>;
  slot verbose :: <boolean>;
end class <parse-state>;

// <parse-file-state> is used for heavy duty parsing of full include files.
// The "declarations" slot is used by higher level functions to actually act
// upon the results of the parse.  The "declarations-stack" is simply used to
// keep track of recursive includes, and need not be visible at the higher
// level.
//
define class <parse-file-state> (<parse-state>) 
  // Declarations is an ordered list of all declarations made withing a single
  // ".h" file.
  slot declarations :: <deque> = make(<deque>);
  slot current-file :: <string> = "<top-level>";
  slot recursive-files-stack :: <deque> = make(<deque>);
  // maps a filename into a sequence of files which it recursively includes
  slot recursive-include-table :: <table> = make(<string-table>);
  // maps a filename into a sequence of declarations from that file
  slot recursive-declaration-table :: <table> = make(<string-table>);
end class;

define method initialize (value :: <parse-file-state>, #key)
  value.objects := make(<string-table>);
  value.structs := make(<string-table>);
  value.pointers := make(<object-table>);
  value.vectors := make(<my-sequence-table>);
  value.pointers[void-type] := make(<pointer-declaration>, referent: void-type,
				    dylan-name: "<machine-pointer>",
				    equated: #t,
				    name: "statically-typed-pointer");
  exclude-decl(value.pointers[void-type]);
end method initialize;

// table that chains the elements of a sequence as key
// wonder why this isn't built in. Also wonder why
// equal-hash isn't defined for general objects, even
// though \= is defined for them.
//
define class <my-sequence-table> (<value-table>)
end class <my-sequence-table>;

define sealed inline method table-protocol (ht :: <my-sequence-table>)
 => (key-test :: <function>, key-hash :: <function>);
  values(\=, curry(sequence-hash, object-hash));
end method table-protocol;



// Push-include-level informs the <parse-state> that it is now processing a
// recursive include file and should therefore treat declarations somewhat
// differently.
//
define method push-include-level
    (state :: <parse-file-state>, file :: <string>)
 => (state :: <parse-file-state>);
  let old-file = state.current-file;
  state.recursive-include-table[old-file] :=
    pair(file, element(state.recursive-include-table, old-file, default: #()));
  state.recursive-declaration-table[old-file] := state.declarations;
  state.declarations :=
    (element(state.recursive-declaration-table, file, default: #f)
       | make(<deque>));
  push(state.recursive-files-stack, old-file);
  state.current-file := file;
  state;
end method push-include-level;

// Pop-include-level informs the <parse-state> that it is finished processing
// a recursive include file.
//
define method pop-include-level
    (state :: <parse-file-state>) => (state :: <parse-file-state>);
  if (state.recursive-files-stack.empty?)
    parse-error(state, "Bad pop-include-level");
  end if;
  state.recursive-declaration-table[state.current-file] := state.declarations;
  state.current-file := pop(state.recursive-files-stack);
  state.declarations := state.recursive-declaration-table[state.current-file];
  state;
end method pop-include-level;

// <parse-value-state> is used for evaluating type names or expressions.  The
// parse will simply return a value, and thus we don't need any of the
// "declarations" tracking stuff that <parse-file-state> has.
//
// In order to gain access to the types and values declared in an include
// file, you can pass the "parent:" keyword into make to specify a
// <parse-file-state> which was produced by an earlier parse of an include
// file.
//
define class <parse-value-state> (<parse-state>) end class;
define class <parse-type-state> (<parse-value-state>) end class;
define class <parse-macro-state> (<parse-value-state>) end class;
define class <parse-cpp-state> (<parse-value-state>) end class;

define method initialize
    (value :: <parse-value-state>,
     #key parent :: false-or(<parse-state>))
  if (parent)
    value.objects := parent.objects;
    value.structs := parent.structs;
    value.pointers := parent.pointers;
    value.vectors := parent.vectors;
  else
    value.objects := make(<string-table>);
    value.structs := make(<string-table>);
    value.pointers := make(<object-table>);
    value.vectors := make(<my-sequence-table>);
  end if;
end method initialize;

//----------------------------------------------------------------------
// Functions to be called from within c-parse
//----------------------------------------------------------------------

// Another method for the "source-location" generic.  This one accepts a
// <parse-state> and tries to use it to figure out the error location.
//
define method source-location (state :: <parse-state>)
 => (srcloc :: <source-location>)
  source-location(state.tokenizer);
end method;

// We may have a jumble of type specifiers.  Rationalize them into a
// predefined type or user defined type.
//
define method process-type-list
    (types :: <list>, state :: <parse-state>)
 => (result :: <type-declaration>);
  // This is just an ad-hoc state machine.  It could have been incorporated
  // into the grammar, but since it wasn't, we have to sort out the mess by
  // hand. 
  let type = unknown-type;
  for (specifier in types)
    type := select (specifier by instance?)
// We are now using the preprocessor to eliminate these tokens before they
// ever occur.
//	      <const-token>,
//	      <volatile-token> =>
//		// At present we simply ignore these.
//		type;
	      <char-token> =>
		select (type)
		  unknown-type, signed-type => char-type;
		  unsigned-type => unsigned-char-type;
		  otherwise => parse-error(state, "Bad type specifier, expected <char-token>, got %=", type);
		end select;
	      <short-token> =>
		select (type)
		  unknown-type, signed-type => short-type;
		  unsigned-type => unsigned-short-type;
		  otherwise => parse-error(state, "Bad type specifier, expected <short-token>, got %=", type);
		end select;
	      <long-token> =>
		// "long long" is an idiom supported by gcc, so we'll
		// recognize it, without actually supporting access.
		select (type)
		  long-type => longlong-type;
		  unsigned-long-type => unsigned-longlong-type;
		  unknown-type, signed-type => long-type;
		  unsigned-type => unsigned-long-type;
		  otherwise => parse-error(state, "Bad type specifier, expected <long-token>, got %=", type);
		end select;
	      <int-token> =>
		select (type)
		  unknown-type, signed-type => int-type;
		  unsigned-type => unsigned-int-type;
		  longlong-type, unsigned-longlong-type,
		  long-type, unsigned-long-type,
		  short-type, unsigned-short-type => type;
		  otherwise => parse-error(state, "Bad type specifier, expected <int-token>, got %=", type);
		end select;
	      <signed-token> =>
		select (type)
		  unknown-type => signed-type;
		  long-type => long-type;
		  char-type => char-type;
		  short-type => short-type;
		  otherwise => parse-error(state, "Bad type specifier, expected <signed-token>, got %=", type);
		end select;
	      <unsigned-token> =>
		select (type)
		  unknown-type => unsigned-type;
		  long-type => unsigned-long-type;
		  char-type => unsigned-char-type;
		  short-type => unsigned-short-type;
		  otherwise => parse-error(state, "Bad type specifier, expected <unsigned-token>, got %=", type);
		end select;
	      <float-token> =>
		select (type)
		  unknown-type => float-type;
		  otherwise => parse-error(state, "Bad type specifier, expected <float-token>, got %=", type);
		end select;
	      <double-token> =>
		select (type)
		  unknown-type => double-type;
		  long-type => long-double-type;
		  otherwise => parse-error(state, "Bad type specifier, expected <double-token>, got %=", type);
		end select;
	      <void-token> =>
		select (type)
		  unknown-type => void-type;
		  otherwise => parse-error(state, "Bad type specifier, expected <void-token>, got %=", type);
		end select;
	      otherwise =>
		// user defined types are passed on unmodified
		select (type)
		  unknown-type => specifier;
		  otherwise => parse-error(state, "Bad type specifier for user type, got %=", type);
		end select;
	    end select;
  end for;
  select (type)
    unknown-type => parse-error(state, "Bad type specifier (unknown type)");
    unsigned-type => unsigned-int-type;
    signed-type => int-type;
    otherwise => type;
  end select;
end method process-type-list;

// Deals with the odd idiomatic data structures which result from the LALR
// parser generator.  These might take the form of 
// #((#"pointer", #"pointer", ...) . name) or
// #(#"function", args . name) or
// #(#"vector", length . name)
// #(#"bitfield", bits . name)
//
define method process-declarator
    (tp :: <type-declaration>, declarator :: <pair>, state :: <parse-state>)
 => (new-type :: <type-declaration>, name :: <object>);
  case 
    (instance?(declarator.head, <list>)) =>
      for (tp = tp
	     then if (ptr ~= #"pointer")
		    parse-error(state, "unknown type modifier");
		  else
		    pointer-to(tp, state);
		  end if,
	   ptr in head(declarator))
      finally
	process-declarator(tp, tail(declarator), state);
      end for;
    (declarator.head == #"bitfield") =>
      if (instance?(tp.true-type, <integer-type-declaration>))
	let decl = make(<bitfield-declaration>, bits: second(declarator),
			base: tp, name: anonymous-name(),
			dylan-name: "<integer>");
	process-declarator(decl, declarator.tail.tail, state);
      else
	parse-error(state, "Bit-fields must be of an integral type.  "
		      "This is of type %=.", tp);
      end if;
    (declarator.head == #"vector") =>
      let length = second(declarator);
      // Vector types are represented the same as the corresponding pointer
      // types, but are accessed differently, so make sure that we share names
      // with the corresponding pointer type.
      let decl = vector-of(tp, state, length: length);
      process-declarator(decl, declarator.tail.tail, state);
    (declarator.head == #"function") =>
      // rgs: For now, we simple equate all function types to
      // <function-pointer>.  At some later date, we will actually
      // provide distinct types canonicalized by their signatures.
      // XXX - this is starting to change... There's a horrible problem
      // in C with the semantics of 'typedef void (foo)(void)' versus
      // 'typedef void (*foo)()'. For now, we only allow the latter form,
      // which dates back to K&R C. When somebody explains the ANSI C
      // semantics very precisely to me, I'll fix this code to handle all
      // cases.
      let params = second(declarator);
      let real-params = if (params.size == 1 & first(params).type == void-type)
			  #();
			else
			  params;
			end if;
      for (count from 1,
	   param in params)
	param.dylan-name := format-to-string("arg%d", count);
      end for;

      // Force K&R semantics only (see above).
//      let nested-type = declarator.tail.tail;
//      unless (instance?(nested-type, <pair>)
//		& instance?(nested-type.head, <list>)
//		& nested-type.head.size == 1
//		& nested-type.head.head == #"pointer"
//		& instance?(nested-type.tail, <identifier-token>))
//	parse-error(state, "function types must be of form 'void (*foo)()'");
//      end unless;
      
//      let new-name = nested-type.tail;
//      let new-type = make(<function-type-declaration>,
//			  name: new-name.value,

      let new-type = make(<function-type-declaration>, name: anonymous-name(),
			  result: make(<result-declaration>,
				       name: "result", type: tp),
			  params: real-params);
      // XXX - We used to call process declarator here:
      // Instead, we handle the terminal case ourselves. If anyone
      // figures out ANSI C function pointers, we'll need to re-examine
      // this.
      // process-declarator(new-type, declarator.tail.tail, state);
      //values(new-type, new-name);
      process-declarator(new-type, declarator.tail.tail, state);
    otherwise =>
      parse-error(state, "unknown type modifier");
  end case;
end method process-declarator;

// This handles the trivial case in which we are down to the bare "name" and
// are therefore done.
//
define method process-declarator
    (type :: <type-declaration>, declarator :: <object>,
     state :: <parse-state>)
 => (new-type :: <type-declaration>, name :: <object>);
  values(type, declarator);
end method process-declarator;

// Walks through the "parse tree" for a c declaration and adds the
// declared names and their types into the state's typedef or object table. 
//
define method declare-objects
    (state :: <parse-state>, new-type :: <type-declaration>, names :: <list>,
     is-typedef? :: <boolean>)
 => ();
  for (name in names)
    let (new-type, name) = process-declarator(new-type, name, state);
    let (nameloc) = if (instance?(name, <token>))
			    name;
			  else
			    state;
			  end if;
    if (instance?(name, <typedef-declaration>))
      unless (is-typedef? & new-type == name.type)
	parse-error(state, "illegal redefinition of typedef.");
      end unless;
    elseif (is-typedef?)
      if (element(state.objects, name.value, default: #f) == #f)
        state.objects[name.value] 
          := add-declaration(state, make(<typedef-declaration>, 
                                         name: name.value,
                                         type: new-type));
        parse-progress-report(nameloc, "Processed typedef %s", name.value);
      end if;
      add-typedef(state.tokenizer, name);
    else
      let decl-type = if (instance?(new-type, <function-type-declaration>))
			<function-declaration>;
		      else
			<variable-declaration>;
		      end if;
      // If there multiple copies of the same declaration, we simply
      // use the first.  They are most likely identical anyway.
      // rgs: We should probably (eventually) check that they are identical
      //      rather than assuming it.
      if (element(state.objects, name.value, default: #f) == #f)
	state.objects[name.value]
	  := add-declaration(state, make(decl-type, name: name.value,
					 type: new-type));
	parse-progress-report(nameloc, "Processed declaration %s", name.value);
      end if;
    end if;
  end for;
end method declare-objects;

//----------------------------------------------------------------------
// "High level" functions for manipulating the parse state.
//----------------------------------------------------------------------

// This adds a new declaration to the "declarations" slot, and label it with
// the appropriate source file name (taken from the state).
//
define method add-declaration
    (state :: <parse-file-state>, declaration :: <declaration>)
 => (declaration :: <declaration>);
  push-last(state.declarations, declaration);
  declaration;
end method add-declaration;

define constant null-table = make(<table>);

// This is the exported routine for determining which declarations to include
// in Melange's output routine.  It walks through all of the non-excluded top
// level declarations and explicitly imported non-top level declarations and
// invokes "compute-closure" (documented in "c-decls.dylan") to determine
// other declarations are required to have a complete & consistent interface.
//
define method declaration-closure
    (state :: <parse-file-state>,
     files :: <sequence>, excluded-files :: <sequence>,
     imports :: <table>, file-imports :: <table>,
     import-mode :: <symbol>, file-import-modes :: <table>)
 => (ordered-decls :: <deque>);
  let files-processed :: <table> = make(<string-table>);
  for (file in excluded-files)
    // By saying that this file has already been processed, we stop it from
    // being included in a recursive file walk.
    files-processed[file] := file;
    for (decl in state.recursive-declaration-table[file])
      // By saying that each declaration has been processed, we prevent it
      // from being emitted, even if it is explicitly used by an imported
      // symbol.
      decl.declared? := #t;
    end for;
  end for;    
  let ordered-decls = make(<deque>);
  let recursive-files? = (import-mode == #"all-recursive");
  local method declaration-closure-aux
	    (decls :: <sequence>, file-import-table :: <table>,
	     subfiles :: <sequence>) => ();
	  for (decl in decls)
	    compute-closure(ordered-decls, decl);
	    let import = (element(imports, decl, default: #f)
			    | element(file-import-table, decl, default: #f));
	    if (instance?(import, <string>)) rename(decl, import) end if;
	  end for;
	  for (file in subfiles)
	    unless (element(files-processed, file, default: #f))
	      files-processed[file] := file;
	      let sub = if (recursive-files?)
			  element(state.recursive-include-table, file,
				  default: #());
			else
			  #();
			end if;
	      let decls = if (element(file-import-modes, file, default: #"all")
				== #"all")
			    state.recursive-declaration-table[file];
			  else
			    element(file-imports, file, default: null-table)
			      .key-sequence;
			  end if;
	      declaration-closure-aux(decls, element(file-imports, file,
						     default: null-table),
				      sub);
	    end unless;
	  end for;
	end method declaration-closure-aux;

  let subfiles = if (import-mode == #"none") #() else files end if;
  declaration-closure-aux(key-sequence(imports), null-table, subfiles);
			  
  ordered-decls;
end method declaration-closure;

// Seals for file c-decl-state.dylan

// <parse-file-state> -- subclass of <parse-state>
define sealed domain make(singleton(<parse-file-state>));
// <parse-value-state> -- subclass of <parse-state>
define sealed domain make(singleton(<parse-value-state>));
// <parse-type-state> -- subclass of <parse-value-state>
define sealed domain make(singleton(<parse-type-state>));
// <parse-macro-state> -- subclass of <parse-value-state>
define sealed domain make(singleton(<parse-macro-state>));
// <parse-cpp-state> -- subclass of <parse-value-state>
define sealed domain make(singleton(<parse-cpp-state>));
