module: c-parser
copyright: Copyright (C) 1994, Carnegie Mellon University
	   All rights reserved.
	   This code was produced by the Gwydion Project at Carnegie Mellon
	   University.  If you are interested in using this code, contact
	   "Scott.Fahlman@cs.cmu.edu" (Internet).

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
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
//    University.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports, questions, comments, and suggestions should be sent by
// E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
//
//======================================================================

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
// All rights reserved.
//
//======================================================================

//======================================================================
// c-parser.dylan attempts to encapsulate the interface between
// low-level parsing and the higher level set of "declarations" derived
// from that parsing.  This includes declarations for the "parse-state"
// which is first created and populated by the parser and later updated
// with user preferences from the interface definition.
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
  slot repository :: <c-type-repository>;
  slot objects :: <table>;
  slot structs :: <table>;  // XXX - may go away
  slot tokenizer :: <tokenizer>,
    required-init-keyword: tokenizer:;
  slot pointers :: <table>; // XXX - will probably go away
  slot verbose :: <boolean>;

  // *HACK*: We temporarily use a global variable to figure out whether a
  // given declaration is an object declaration or a typedef declaration.
  // This appears to be simpler than trying to propogate detailed information
  // upwards.
  slot processing-typedef? :: <boolean> = #f;
end class <parse-state>;

// <parse-file-state> is used for heavy duty parsing of full include files.
// The "declarations" slot is used by higher level functions to actually act
// upon the results of the parse.  The "declarations-stack" is simply used to
// keep track of recursive includes, and need not be visible at the higher
// level.
//
define class <parse-file-state> (<parse-state>) 
  slot recent-declarations :: <list> = #();

  // XXX - Old stuff. Maybe some of this should go away.
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

define method initialize (value :: <parse-file-state>, #key repository: r)
  value.objects := make(<string-table>);
  value.structs := make(<string-table>);
  value.pointers := make(<object-table>);
  if (r)
    value.repository := r;
  else
    error("required initialization argument 'repository:' not found");
  end if;
/*
  value.pointers[void-type] := make(<pointer-declaration>, referent: void-type,
				    dylan-name: "<machine-pointer>",
				    equated: #t,
				    name: "statically-typed-pointer");
*/
end method initialize;

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
     #key repository: r, parent :: type-union(<parse-state>, <false>))
  if (parent)
    value.objects := parent.objects;
    value.structs := parent.structs;
    value.pointers := parent.pointers;
    value.repository := parent.repository;
  else
    value.objects := make(<string-table>);
    value.structs := make(<string-table>);
    value.pointers := make(<object-table>);
    if (r)
      value.repository := r;
    else
      error("required initialization argument 'repository:' not found");
    end if;
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


// Some C type specifiers that do not themselves represent complete types.
// These are used in process-type-list below.
//
define constant <c-type-specifier> =
  type-union(<c-type>, <incomplete-type-specifier>);
define class <incomplete-type-specifier> (<object>) end;
define constant $c-unknown-type = make(<incomplete-type-specifier>);
define constant $c-signed-type = make(<incomplete-type-specifier>);
define constant $c-unsigned-type = make(<incomplete-type-specifier>);

// We may have a jumble of type specifiers.  Rationalize them into a
// predefined type or user defined type.
//
define function process-type-list
    (types :: <list>, state :: <parse-state>)
 => (result :: <c-type>)

  // This is just an ad-hoc state machine.  It could have been incorporated
  // into the grammar, but since it wasn't, we have to sort out the mess by
  // hand.
  let type :: <c-type-specifier> = $c-unknown-type;
  for (specifier in types)
    // XXX - HACK! Convert typedef declaration to typedef type. We need to
    // clean this issue up globally. Related to <icky-type-name>?
    if (instance?(specifier, <c-typedef-declaration>))
      specifier := specifier.c-typedef-declaration-type;
    end;
    type := select (specifier by instance?)
// We are now using the preprocessor to eliminate these tokens before they
// ever occur.
//	      <const-token>,
//	      <volatile-token> =>
//		// At present we simply ignore these.
//		type;
	      <char-token> =>
		select (type)
		  $c-unknown-type => $c-char-type;
		  $c-signed-type => $c-signed-char-type;
		  $c-unsigned-type => $c-unsigned-char-type;
		  otherwise => parse-error(state, "Bad type specifier, expected <char-token>, got %=", type);
		end select;
	      <short-token> =>
		select (type)
		  $c-unknown-type => $c-short-type;
		  $c-signed-type => $c-signed-short-type;
		  $c-unsigned-type => $c-unsigned-short-type;
		  otherwise => parse-error(state, "Bad type specifier, expected <short-token>, got %=", type);
		end select;
	      <long-token> =>
		// "long long" is an idiom supported by gcc, so we'll
		// recognize it, without actually supporting access.
		select (type)
		  $c-long-type => $c-long-long-type;
		  $c-signed-long-type => $c-signed-long-long-type;
		  $c-unsigned-long-type => $c-unsigned-long-long-type;
		  $c-unknown-type => $c-long-type;
		  $c-signed-type => $c-signed-long-type;
		  $c-unsigned-type => $c-unsigned-long-type;
		  otherwise => parse-error(state, "Bad type specifier, expected <long-token>, got %=", type);
		end select;
	      <int-token> =>
		select (type)
		  $c-unknown-type => $c-int-type;
		  $c-signed-type => $c-signed-int-type;
		  $c-unsigned-type => $c-unsigned-int-type;
		  $c-long-long-type, $c-signed-long-long-type,
		  $c-unsigned-long-long-type,
		  $c-long-type, $c-signed-long-type,
		  $c-unsigned-long-type,
		  $c-short-type, $c-signed-short-type,
		  $c-unsigned-short-type => type;
		  otherwise => parse-error(state, "Bad type specifier, expected <int-token>, got %=", type);
		end select;
	      <signed-token> =>
		select (type)
		  $c-unknown-type => $c-signed-type;
		  $c-long-type => $c-signed-long-type;
		  $c-char-type => $c-signed-char-type;
		  $c-short-type => $c-signed-short-type;
		  $c-long-long-type =>  $c-signed-long-long-type;
		  otherwise => parse-error(state, "Bad type specifier, expected <signed-token>, got %=", type);
		end select;
	      <unsigned-token> =>
		select (type)
		  $c-unknown-type => $c-unsigned-type;
		  $c-long-type => $c-unsigned-long-type;
		  $c-char-type => $c-unsigned-char-type;
		  $c-short-type => $c-unsigned-short-type;
		  $c-long-long-type =>  $c-unsigned-long-long-type;
		  otherwise => parse-error(state, "Bad type specifier, expected <unsigned-token>, got %=", type);
		end select;
	      <float-token> =>
		select (type)
		  $c-unknown-type => $c-float-type;
		  $c-long-type => $c-double-type; // Old idiom, not ANSI
		  otherwise => parse-error(state, "Bad type specifier, expected <float-token>, got %=", type);
		end select;
	      <double-token> =>
		select (type)
		  $c-unknown-type => $c-double-type;
		  $c-long-type => $c-long-double-type;
		  otherwise => parse-error(state, "Bad type specifier, expected <double-token>, got %=", type);
		end select;
	      <void-token> =>
		select (type)
		  $c-unknown-type => $c-void-type;
		  otherwise => parse-error(state, "Bad type specifier, expected <void-token>, got %=", type);
		end select;
	      otherwise =>
		// user defined types are passed on unmodified
		select (type)
		  $c-unknown-type => specifier;
		  otherwise => parse-error(state, "Bad type specifier for user type, got %=", type);
		end select;
	    end select;
  end for;
  select (type)
    $c-unknown-type => parse-error(state, "Bad type specifier (unknown type)");
    $c-unsigned-type => $c-unsigned-int-type;
    $c-signed-type => $c-signed-int-type;
    otherwise => type;
  end select;
end function process-type-list;

// XXX - This particular combination of identifiers and typedefs appears
// everywhere. This constant should be used to document it wherever it is
// discovered. Let's hope that no other types belong in this union.
//
// This may correspond to 'type-name' in the formal grammar. If something
// fails to typecheck against <icky-type-name>, investigate it thoroughly
// and take appropriate steps.
//
// TIME - This typecheck has a negligible impact on performance.
//
define constant <icky-type-name> =
  type-union(<identifier-token>,      // A regular type name
	     <c-typedef-declaration>, // Potential typedef redeclaration
	     <empty-list>);           // Abstract declarator
	     // <c-typedef-type>);       // doesn't seem to be required

// Deals with the odd idiomatic data structures which result from the LALR
// parser generator.  These might take the form of 
// #((#"pointer", #"pointer", ...) . name) or
// #(#"function", args . name) or
// #(#"vector", length . name) or
// #(#"bitfield", bits . name)
//
// XXX bitfields seem to be unimplemented.
//
// The 'args' field for functions may be:
//   #($c-void-type)         Explicit void.
//   #(args...)              Zero or more arguments.
//   #(args..., #"varargs")  Zero or more arguments, explicit varargs.
//
define generic process-declarator
    (type :: <c-type>,
     declarator :: type-union(<icky-type-name>, <pair>),
     state :: <parse-state>)
 => (new-type :: <c-type>,
     name :: <icky-type-name>);

define method process-declarator
    (tp :: <c-type>, declarator :: <pair>, state :: <parse-state>)
 => (new-type :: <c-type>, name :: <icky-type-name>)
  real-process-declarator(tp, declarator.head, declarator, state);
end method process-declarator;

define method real-process-declarator
    (tp :: <c-type>, declarator-type :: <list>, declarator :: <pair>, state :: <parse-state>)
 => (new-type :: <c-type>, name :: <icky-type-name>)
    // Process pointer types.
  for (tp = tp
	 then if (ptr ~= #"pointer")
		parse-error(state, "unknown type modifier");
	      else
		make(<c-pointer-type>,
		     repository: state.repository,
		     referent: tp);
	      end if,
       ptr in head(declarator))
  finally
    process-declarator(tp, tail(declarator), state);
  end for;
end method real-process-declarator;
      
define method real-process-declarator
    (tp :: <c-type>, declarator-type == #"vector", declarator :: <pair>, state :: <parse-state>)
 => (new-type :: <c-type>, name :: <icky-type-name>)
    // Process vector types.
  let length = second(declarator);
  // Vector types are represented the same as the corresponding pointer
  // types, but are accessed differently, so make sure that we share names
  // with the corresponding pointer type.
  let decl = make(<c-array-type>,
		  repository: state.repository,
		  referent: tp,
		  length: length);
  process-declarator(decl, declarator.tail.tail, state);
end method real-process-declarator;
      
define method real-process-declarator
    (tp :: <c-type>, declarator-type == #"function", declarator :: <pair>, state :: <parse-state>)
 => (new-type :: <c-type>, name :: <icky-type-name>)
  local method get-type (arg) => (type :: <c-type>)
	  if (instance?(arg, <c-variable-declaration>))
	    // XXX - I fixed c-parser-engine so this won't happen.
	    arg.c-variable-type;
	  else
	    arg;
	  end if;
	  // XXX - check for void arguments.
	end method;
  let params = second(declarator);
  
  // Look for (void) argument lists.
  let (params, explicit-void?) =
    if (params.size == 1 & first(params) == $c-void-type)
      values(#(), #t);
    else
      values(params, #f);
    end if;
  
  // Look for argument lists with ANSI C varargs.
  let (params, explicit-varargs?) =
    if (member?(#"varargs", params))
      values(choose(curry(\~=, #"varargs"), params), #t);
    else
      values(params, #f);
    end if;
  
  // Construct the function type.
  // XXX - we eventually want typedef-like types to hold argument names.
  let new-type = make(<c-function-type>,
		      repository: state.repository,
		      return-type: tp,
		      parameter-types: map-as(<stretchy-vector>,
					      get-type, params),
		      explicit-void?: explicit-void?,
		      explicit-varargs?: explicit-varargs?);
  process-declarator(new-type, declarator.tail.tail, state);
end method real-process-declarator;

define method real-process-declarator
    (tp :: <c-type>, declarator-type, declarator :: <pair>, state :: <parse-state>)
 => (new-type :: <c-type>, name :: <icky-type-name>)
  parse-error(state, "unknown type modifier");
end method real-process-declarator;

// This handles the trivial case in which we are down to the bare "name" and
// are therefore done.
//
define method process-declarator
    (type :: <c-type>,
     declarator :: <icky-type-name>,
     state :: <parse-state>)
 => (new-type :: <c-type>,
     name :: <icky-type-name>)
  values(type, declarator);
end method process-declarator;

// Walks through the "parse tree" for a c declaration and adds the
// declared names and their types into the state's typedef or object table. 
//
define function declare-objects
    (state :: <parse-state>, new-type :: <c-type>, names :: <list>,
     is-typedef? :: <boolean>)
 => ();
  for (name in names)
    // At a minimum 'name' can be a <token> or a <c-typedef-declaration>.
    // XXX - It might also be other things--find out if & what.
    let (new-type, name) = process-declarator(new-type, name, state);
    let (nameloc) = if (instance?(name, <token>)) name else state end;
    if (instance?(name, <c-typedef-declaration>))
      // MSVC apparently allows typedefs to appear more than once.
      unless (is-typedef? & new-type ==
		name.c-typedef-declaration-type.c-typedef-type)
	parse-error(state, "illegal redefinition of typedef.");
      end unless;
    elseif (is-typedef?)
      // This is a new typedef.
      state.objects[name.value] 
	:= add-declaration(state,
			   make(<c-typedef-declaration>,
				type: make(<c-typedef-type>,
					   repository: state.repository,
					   name: name.value,
					   type: new-type)));
      parse-progress-report(nameloc, "Processed typedef %s", name.value);
      add-typedef(state.tokenizer, name);
    else
      // This is a new object (i.e., variable or function) declaration.
      // XXX - rename <c-variable-declaration> to <c-object-declaration>.
      // If there multiple copies of the same declaration, we simply
      // use the first.  They are most likely identical anyway.
      // rgs: We should probably (eventually) check that they are identical
      //      rather than assuming it.
      if (element(state.objects, name.value, default: #f) == #f)
	state.objects[name.value]
	  := add-declaration(state,
			     make(<c-variable-declaration>,
				  name: name.value,
				  type: new-type));
	parse-progress-report(nameloc, "Processed declaration %s", name.value);
      end if;
    end if;
  end for;
end function declare-objects;

// Add file contents to a <c-file> object, unwinding the fun data structures
// built up by the parser.
//
define function add-contents-to-c-file!
    (file :: <c-file>, contents :: <list>) => ()
  for (item in reverse!(contents))
    select (item by instance?)
      <c-file> =>
	add-c-file!(file, item);
      <list> =>
	for (decl in reverse!(item))
	  add-c-declaration!(file, decl);
	end for;
    end select;
  end for;
end function add-contents-to-c-file!;

// Get the most recent declarations and reset our list.
//
define function retrieve-recent-declarations
    (state :: <parse-file-state>)
 => (decls :: <list>)
  let decls = state.recent-declarations;
  state.recent-declarations := #();
  decls;
end function;

//----------------------------------------------------------------------
// "High level" functions for manipulating the parse state.
//----------------------------------------------------------------------

// This adds a new declaration to the "declarations" slot, and label it with
// the appropriate source file name (taken from the state).
//
define method add-declaration
    (state :: <parse-file-state>, declaration :: <c-declaration>)
 => (declaration :: <c-declaration>);
  push-last(state.declarations, declaration);
  state.recent-declarations := pair(declaration, state.recent-declarations);
  declaration;
end method add-declaration;


//----------------------------------------------------------------------
// Public interface to c-parser library.
//----------------------------------------------------------------------
// Actually, this is logically separate from the rest of this module.
// It's in here because it connects the parser to its clients. The rest
// of the file connects the parser to it's abstraction layer. There two
// things are similar in an abstract sort of way, but don't read too
// much into it.

// Main parser routine:
define function parse-c-file
    (repository :: <c-type-repository>, filename :: <string>,
     #key platform :: <c-platform>, include-path :: <c-include-path>)
 => (c-file :: <c-file>)
  // XXX - We ignore typedefs and structs already in the repository.
  // We should probably either (1) honor them or (2) create and return
  // a repository.
  // XXX - Why isn't this re-entrant?
  // XXX - We need to accept a parameter which knows about sizeof and
  // the header search path.

  // Preload our default defines.
  let defines = make(<string-table>);
  let default-defines = platform.c-platform-default-defines;
  for (i from 0 below default-defines.size by 2)
    defines[default-defines[i]] := default-defines[i + 1];
  end for;

  parse(repository,
	list(filename),
	defines: defines,
	include-path: include-path);
end function;


// Seals for file c-parser-interface.dylan

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
// <string-table> -- subclass of <value-table>
define sealed domain make(singleton(<string-table>));
define sealed domain initialize(<string-table>);
