documented: #t
module: c-declarations
copyright: Copyright (C) 1994, Carnegie Mellon University
	   All rights reserved.
	   This code was produced by the Gwydion Project at Carnegie Mellon
	   University.  If you are interested in using this code, contact
	   "Scott.Fahlman@cs.cmu.edu" (Internet).
rcs-header: $Header: 

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
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
  slot declarations :: <deque>;
  // The following slots are used to change and restore "per-include-file"
  // slots.  The other slots remain valid across all include files.
  slot declarations-stack :: <deque>;
end class;

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

//----------------------------------------------------------------------

// <String-table> is highly optimized for the sort of string lookups we get in
// this application.  The hash function is very fast, but will fail for empty
// strings.  Luckily, no such strings should show up in C declarations.
//
define class <string-table> (<value-table>) end class;

define method fast-string-hash (string :: <string>)
  values(string.size * 256 + as(<integer>, string.first),
	 $permanent-hash-state);
end method fast-string-hash;

define method table-protocol (table :: <string-table>)
	=> (equal :: <function>, hash :: <function>);
  values(\=, fast-string-hash);
end method;

define method initialize (value :: <parse-file-state>, #key)
  value.objects := make(<string-table>);
  value.structs := make(<string-table>);
  value.pointers := make(<object-table>);
  value.pointers[void-type] := make(<pointer-declaration>, referent: void-type,
				    dylan-name: "<machine-pointer>",
				    equated: #t,
				    name: "statically-typed-pointer");
  value.declarations := make(<deque>);
  value.declarations-stack := make(<deque>);
end method initialize;

define method initialize
    (value :: <parse-value-state>,
     #key parent :: type-union(<parse-file-state>, <false>))
  if (parent)
    value.objects := parent.objects;
    value.structs := parent.structs;
    value.pointers := parent.pointers;
  else
    value.objects := make(<string-table>);
    value.structs := make(<string-table>);
    value.pointers := make(<object-table>);
  end if;
end method initialize;

//----------------------------------------------------------------------
// Functions to be called from within c-parse
//----------------------------------------------------------------------

// Push-include-level informs the <parse-state> that it is now processing a
// recursive include file and should therefore treat declarations somewhat
// differently.
//
define method push-include-level
    (state :: <parse-state>) => (state :: <parse-state>);
  push(state.declarations-stack, state.declarations);
  state.declarations := make(<deque>);
  state;
end method push-include-level;

// Pop-include-level informs the <parse-state> that it is finished processing
// a recursive include file.
//
define method pop-include-level
    (state :: <parse-state>) => (state :: <parse-state>);
  if (state.declarations-stack.size == 0)
    parse-error(state, "Bad pop-include-level");
  end if;
  state.declarations := pop(state.declarations-stack);
  state;
end method pop-include-level;

// Another method for the "parse-error" generic.  This one accepts a
// <parse-state> and tries to use it to figure out the error location.
//
define method parse-error
    (state :: <parse-state>, format :: <string>, #rest args)
 => (); // Never returns
  apply(parse-error, state.tokenizer, format, args);
end method parse-error;

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
	      <const-token>,
	      <volatile-token> =>
		// At present we simply ignore these.
		type;
	      <char-token> =>
		select (type)
		  unknown-type, signed-type => char-type;
		  unsigned-type => unsigned-char-type;
		  otherwise => parse-error(state, "Bad type specifier");
		end select;
	      <short-token> =>
		select (type)
		  unknown-type, signed-type => short-type;
		  unsigned-type => unsigned-short-type;
		  otherwise => parse-error(state, "Bad type specifier");
		end select;
	      <long-token> =>
		select (type)
		  unknown-type, signed-type => long-type;
		  unsigned-type => unsigned-long-type;
		  otherwise => parse-error(state, "Bad type specifier");
		end select;
	      <int-token> =>
		select (type)
		  unknown-type, signed-type => int-type;
		  unsigned-type => unsigned-int-type;
		  long-type, unsigned-long-type,
		  short-type, unsigned-short-type => type;
		  otherwise => parse-error(state, "Bad type specifier");
		end select;
	      <signed-token> =>
		select (type)
		  unknown-type => signed-type;
		  otherwise => parse-error(state, "Bad type specifier");
		end select;
	      <unsigned-token> =>
		select (type)
		  unknown-type => unsigned-type;
		  otherwise => parse-error(state, "Bad type specifier");
		end select;
	      <float-token> =>
		select (type)
		  unknown-type => float-type;
		  otherwise => parse-error(state, "Bad type specifier");
		end select;
	      <double-token> =>
		select (type)
		  unknown-type => double-type;
		  long-type => long-double-type;
		  otherwise => parse-error(state, "Bad type specifier");
		end select;
	      <void-token> =>
		select (type)
		  unknown-type => void-type;
		  otherwise => parse-error(state, "Bad type specifier");
		end select;
	      otherwise =>
		// user defined types are passed on unmodified
		select (type)
		  unknown-type => specifier;
		  otherwise => parse-error(state, "Bad type specifier");
		end select;
	    end select;
  end for;
  select (type)
    unknown-type => parse-error(state, "Bad type specifier");
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
//
define method process-declarator
    (tp :: <type-declaration>, declarator :: <pair>, state :: <parse-state>)
 => (new-type :: <type-declaration>, name :: <object>);
  case 
    instance?(head(declarator), <list>) =>
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
    head(declarator) == #"vector" =>
      let length = second(declarator);
      // Vector types are represented the same as the corresponding pointer
      // types, but are accessed differently, so make sure that we share names
      // with the corresponding pointer type.
      let decl = make(<vector-declaration>, length: length, 
		      name: anonymous-name(), equiv: pointer-to(tp, state));
      process-declarator(decl, tail(tail(declarator)), state);
    head(declarator) == #"function" =>
      // rgs: We must add code later to canonicalize these and declare the
      // ones which we have pointers to.  On the other hand, we can't easily
      // create c function pointers anyway, so it probably doesn' matter in
      // the short run. 
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
      let new-type = make(<function-type-declaration>, name: anonymous-name(),
			  result: make(<result-declaration>,
				       name: "result", type: tp),
			  params: real-params);
      process-declarator(new-type, tail(tail(declarator)), state);
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
    (state :: <parse-state>, type :: <type-declaration>, names :: <list>,
     is-typedef? :: <boolean>)
 => ();
  for (name in names)
    let (type, name) = process-declarator(type, name, state);
    if (is-typedef?)
      state.objects[name.value] 
	:= add-declaration(state, make(<typedef-declaration>, name: name.value,
				       type: type));
      add-typedef(state.tokenizer, name);
    else
      let decl-type = if (instance?(type, <function-type-declaration>))
			<function-declaration>;
		      else
			<variable-declaration>;
		      end if;
      // If there multiple copies of the same declaration, we simply
      // use the first.  They are most likely identical anyway.
      // rgs: We should probably (eventually) check that they are identical
      //      rather than assuming it.
      if (~key-exists?(state.objects, name.value))
	state.objects[name.value]
	  := add-declaration(state, make(decl-type, name: name.value,
					 type: type));
      end if;
    end if;
  end for;
end method declare-objects;

//----------------------------------------------------------------------
// "High level" functions for manipulating the parse state.
//----------------------------------------------------------------------

// This adds a new declaration to the "declarations" slot.  Push-include-level
// and Pop-include-level, make sure that any declarations arising from
// recursive include files will be conveniently misplaced so that
// "define-interface" module will only see the "top-level" declarations.
//
define method add-declaration
    (state :: <parse-state>, declaration :: <declaration>)
 => (declaration :: <declaration>);
  push-last(state.declarations, declaration);
  declaration;
end method add-declaration;

// This is the exported routine for determining which declarations to include
// in Melange's output routine.  It walks through all of the non-excluded top
// level declarations and explicitly imported non-top level declarations and
// invokes "compute-closure" (documented in "c-decls.dylan") to determine
// other declarations are required to have a complete & consistent interface.
//
define method declaration-closure
    (state :: <parse-state>, imports :: <explicit-key-collection>,
     import-all? :: <boolean>)
 => (ordered-decls :: <deque>);
  let ordered-decls = make(<deque>);

  // Because we might explicitly import things that are not top-level
  // declarations, we must look at every item included in "imports" as well as
  // the list of top-level declarations.
  for (decl in concatenate(key-sequence(imports), state.declarations))
    let import = element(imports, decl, default: import-all?);
    if (import)
      compute-closure(ordered-decls, decl);
      if (instance?(import, <string>)) rename(decl, import) end if;
    end if;
  end for;
  ordered-decls;
end method declaration-closure;
