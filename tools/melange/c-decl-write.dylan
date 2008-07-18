documented: #t
module: c-declarations
copyright: see below
	   This code was produced by the Gwydion Project at Carnegie Mellon
	   University.  If you are interested in using this code, contact
	   "Scott.Fahlman@cs.cmu.edu" (Internet).

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998 - 2004  Gwydion Dylan Maintainers
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
// "C-decl-write.dylan" contains code to write out Dylan code corresponding to
// various c declarations.  This will likeley be replaced in later versions by
// code which simply builds a parse tree or produces native code.  The current
// version has the advantage of being highly portable -- the Dylan code it
// writes depends upon a relatively small set of primitives defined in module
// "extern". 
//======================================================================

//------------------------------------------------------------------------
// Exported variables
//------------------------------------------------------------------------

// melange-target, which determines what compiler Melange is producing code
//   for, defaults to Mindy.
define variable melange-target :: one-of(#"mindy", #"d2c") = #"mindy";

//------------------------------------------------------------------------
// Mindy/d2c incompatibility fixes
//------------------------------------------------------------------------

define method subclass-type(type :: <byte-string>) => result :: <byte-string>;
  select (melange-target)
    #"mindy" => format-to-string("limited(<class>, subclass-of: %s)", type);
    #"d2c" => format-to-string("subclass(%s)", type);
    otherwise => error("This should never happen in subclass-type()");
  end select;
end method subclass-type;

define method class-sealing() => result :: <byte-string>;
  select (melange-target)
    #"mindy" => "";
    #"d2c" => "functional";
    otherwise => error("This should never happen in class-sealing()");
  end select;
end method class-sealing;

//------------------------------------------------------------------------
// Exported function declarations.
//------------------------------------------------------------------------

// Writes out a list of symbols which must be defined, in a format compatible
// with make-init.pl.  This should provide a portable mechanism for handling
// static linking of libraries.
//
define generic write-mindy-includes
    (file :: false-or(<string>), decls :: <sequence>) => ();

// Writes out appropriate code to load object file and insure that all desired
// objects are included.  Returns a string which can be included in a
// "find-c-function" call so that the symbols will be found.
//
define generic write-file-load
    (include-files :: <sequence>,
     object-file :: false-or(<sequence>),
     decls :: <sequence>,
     stream :: <stream>)
 => (load-string :: <string>);

// Writes out all the Dylan code corresponding to one <declaration>.  The
// exact behavior can, of course, vary widely depending on the variety of
// declaration.  "Load-string" is a magic cookie which is passed to any calss
// to "find-c-pointer" or "find-c-function" -- it specifies the
// <foreign-file> (if any) which should contain the desired definition. 
//
// Most of the code in this file goes to support this single operations.
//
define generic write-declaration
    (decl :: <declaration>, written-names :: <written-name-record>,
     load-string :: <string>, stream :: <stream>) => ();


//------------------------------------------------------------------------
//  <written-name-record> -- maintains a set of all names written so far,
//  and detects duplicate names.
//------------------------------------------------------------------------

define class <written-name-record> (<object>)
  slot written-name-table :: <table> /* <symbol> => <declaration> */
    = make(<table>);
end class <written-name-record>;

define method written-names( record :: <written-name-record> )
=> ( names :: <collection> )
	key-sequence( record.written-name-table );
end method written-names;

define class <written-declaration> (<object>)
  constant slot written-declaration :: <declaration>,
    required-init-keyword: declaration:;
  constant slot written-name :: <string>,
    required-init-keyword: name:;
end class <written-declaration>;

define function register-written-name
    (rec :: <written-name-record>, name :: <string>,
     decl :: <declaration>, #key subname? = #f)
 => (duplicate? :: <boolean>);

  let interned-name = as(<symbol>, name);
  let table = rec.written-name-table;
  let existing = element(table, interned-name, default: #f);

  // If there is already a symbol by that name, there is a potential
  // conflict if the case is different, or if either of the names is
  // not a structure accessor.  If both are struct slot accessors then
  // it's probably okay.
  if (existing
	& (name ~= existing.written-name
             | ~instance?(decl, <structured-type-declaration>)
             | ~instance?(existing.written-declaration,
                          <structured-type-declaration>)))
    // XXX - We should try to extract a source location from each record
    // when printing these error messages. We should also give the C and
    // Dylan forms of each name. But doing so will be a pain.
    signal(make(<simple-warning>,
		format-string: "melange: %s has multiple definitions",
		format-arguments: list(name)));
    #t;
  else 
    element(table, interned-name)
      := make(<written-declaration>, declaration: decl, name: name);
    #f;
  end if;
end function register-written-name;


//------------------------------------------------------------------------
// Support code
//------------------------------------------------------------------------

// C accessor returns a string with the appropriate Dylan code for
// "dereferencing" the named parameter, assuming that the result of the
// operation will have the characteristics of "type".  "Equated" specifies the
// name of the actual type for non-static (i.e. pointer) values which have
// been "equate:"ed, or #f otherwise.
//
// The offset (which may be an integer or a string which contains an
// integral Dylan expression) ends up being added to the address.  This is
// useful for accessing a slot of a larger structure.
//
define generic c-accessor
    (type :: <type-declaration>, offset :: type-union(<integer>, <string>),
     parameter :: <string>, equated :: <string>)
 => (result :: <string>);

// import-value returns a string which contains dylan code for making a Dylan
// value out of the raw value in variable "var".  This will either be a call
// to the appropriate mapping function for the type named in "decl" or (if no
// mapping is defined) can just be the raw variable
//
define method import-value (decl :: <declaration>, var :: <string>)
 => (result :: <string>);
  if (mapped-name(decl, explicit-only?: #t))
    format-to-string("import-value(%s, %s)", decl.mapped-name, var);
  else
    var;
  end if;
end method import-value;

// See import-value above.  This method does the equivalent for converting
// Dylan values into raw "C" values.
//
define method export-value (decl :: <declaration>, var :: <string>)
 => (result :: <string>);
  if (mapped-name(decl, explicit-only?: #t))
    format-to-string("export-value(%s, %s)", decl.type-name, var);
  else
    var;
  end if;
end method export-value;

// Private variable used (and modified) by anonymous-name.
define variable anonymous-count :: <integer> = 0;

// Generates a new name for a function or type.  We don't actually check that
// the user hasn't generated an identical name, but rely instead upon the
// relative obscurity of a variable named "anonymous-???".
//
define method anonymous-name () => (name :: <string>);
  let name = format-to-string("anonymous-%d", anonymous-count);
  anonymous-count := anonymous-count + 1;
  name;
end method anonymous-name;

// catch-all method, returning just a comment
//
define method c-accessor
    (type :: <type-declaration>,  offset :: type-union(<integer>, <string>),
     parameter :: <string>, equated :: <string>)
 => (result :: <string>);
  format-to-string("/* FIXME: no c-accessor defined for %=, named %=. */",
                   type, type.type-name);
end method c-accessor;

// This method simply converts integral parameters into strings for later
// processing by other methods.
//
define method c-accessor
    (type :: <type-declaration>, offset :: <integer>, parameter :: <string>,
     equated :: <string>)
 => (result :: <string>);
  c-accessor(type, format-to-string("%d", offset), parameter, equated);
end method c-accessor;

define method c-accessor
    (type :: <integer-type-declaration>, offset :: <string>,
     parameter :: <string>, equated :: <string>)
 => (result :: <string>);
  // Each builtin integer type specifies its own accessor function.  We can
  // safely ignore "equated".
  format-to-string("%s(%s, offset: %s)",
		   type.accessor-name, parameter, offset);
end method c-accessor;

define method c-accessor
    (type :: <float-type-declaration>, offset :: <string>,
     parameter :: <string>, equated :: <string>)
 => (result :: <string>);
  format-to-string("%s(%s, offset: %s)",
		   type.accessor-name, parameter, offset);
end method c-accessor;

define method c-accessor
    (type :: <enum-declaration>, offset :: <string>,
     parameter :: <string>, equated :: <string>)
 => (result :: <string>);
  format-to-string("unsigned-long-at(%s, offset: %s)",
		   parameter, offset);
end method c-accessor;

define method c-accessor
    (type :: type-union(<pointer-declaration>, <function-type-declaration>),
     offset :: <string>, parameter :: <string>,
     equated :: <string>)
 => (result :: <string>);
  format-to-string("pointer-at(%s, offset: %s, class: %s)",
		   parameter, offset, equated);
end method c-accessor;

define constant <non-atomic-types>
    = type-union(<struct-declaration>, <union-declaration>,
		 <vector-declaration>, <function-type-declaration>);
define constant <pointer-rep-types>
    = type-union(<pointer-declaration>, <non-atomic-types>);

define method c-accessor
    (type :: <non-atomic-types>,
     offset :: <string>, parameter :: <string>, equated :: <string>)
 => (result :: <string>);
  // This one is non-intuitive.  When you "dereference" a pointer or a slot
  // whose contents are a "structure" or "vector" (as distinct from a
  // pointer), you just get a pointer to it, since the actual "contents" can't
  // be expressed.  You can, of course, get a portion of the "contents" by
  // accessing a slot or element.
  //
  // Note that the only way you should get a "vector" is as a structure slot.
  // If we had a declaration like "(*)(foo [])" (i.e. a "pointer" to a
  // "vector") then this routine could produce bad results.  However, this
  // sort of declaration is either impossible or very uncommon.
  format-to-string("as(%s, %s + %s)", equated, parameter, offset);
end method c-accessor;

define method c-accessor
    (alias :: <typedef-declaration>, offset :: <string>, parameter :: <string>,
     equated :: <string>)
 => (result :: <string>);
  // Push past an alias to get the real accessor.
  c-accessor(alias.type, offset, parameter, equated);
end method c-accessor;

// This method writes out accessors for a single slot.  All non-excluded slots
// get "getter" methods, but there may not be a setter method if the slot is
// declared "read-only" or if the value is something unsettable like a
// struct or vector.  (Note that you can set *pointers* to structs or pointers
// to "vectors" -- the check only applies to inline structs, unions, and
// vectors.) 
//
define method write-c-accessor-method
    (compound-type :: <type-declaration>,
     slot-type :: <slot-declaration>, offset :: <integer>,
     written-names :: <written-name-record>, stream :: <stream>)
 => ();
  let real-type = true-type(slot-type.type);
  let slot-name = slot-type.dylan-name;
  // Write getter method
  unless(real-type.abstract-type?)
    format(stream,
           "define %s inline method %s\n"
             "    (ptr :: %s) => (result :: %s);\n"
             "  %s;\n"
             "end method %s;\n\n",
           slot-type.sealed-string, slot-name, compound-type.type-name,
           slot-type.mapped-name,
           import-value(slot-type, c-accessor(slot-type.type, offset,
                                              "ptr", slot-type.type-name)),
           slot-name);
    register-written-name(written-names, slot-name, compound-type);
    
    if (~slot-type.read-only
          & ~instance?(real-type, <non-atomic-types>))
      // Write setter method
      format(stream,
             "define %s inline method %s-setter\n"
               "    (value :: %s, ptr :: %s) => (result :: %s);\n"
               "  %s := %s;\n"
               "  value;\n"
               "end method %s-setter;\n\n",
             slot-type.sealed-string, slot-name, slot-type.mapped-name,
             compound-type.type-name, slot-type.mapped-name,
             c-accessor(slot-type.type, offset,
                        "ptr", slot-type.type-name),
             export-value(slot-type, "value"), slot-name);
      register-written-name(written-names, concatenate(slot-name, "-setter"),
                            compound-type);
    end if;
  end unless;
end method write-c-accessor-method;

// write-c-accessor-method -- internal
//
// This writes a series of accessor methods for the various bitfields
// which are stored in a single <coalesced-bitfields> pseudo-slot.  It
// is roughly parallel to "write-c-accessor-method" above, but care must
// be taken to distinguish between the "glob-type" which is the
// pseudo-slot holding some kind of integer, and "slot-type" which is
// the actual bitfield slot that must be extracted from that integer.
//
// No attempt has (yet) been made to optimize bitfield access.
//
define method write-c-accessor-method
    (compound-type :: <structured-type-declaration>, 
     glob-type :: <coalesced-bitfields>, offset :: <integer>,
     written-names :: <written-name-record>, stream :: <stream>)
 => ();
  for (slot-type in glob-type.fields)
    unless (slot-type.excluded?)
      let slot-name = slot-type.dylan-name;
      let real-type = true-type(slot-type.type);

      let extractor =
	format-to-string("logand(ash(%s, -%d), (2 ^ %d) - 1)",
			 c-accessor(glob-type.type, offset,
				    "ptr", compound-type.type-name),
			 real-type.start-bit, real-type.bits-in-field);
      // Write getter method
      format(stream,
	     "define %s inline method %s\n"
	       "    (ptr :: %s) => (result :: %s);\n"
	       "  %s;\n"
	       "end method %s;\n\n",
	     slot-type.sealed-string, slot-name, compound-type.type-name,
	     slot-type.mapped-name, import-value(slot-type, extractor),
	     slot-name);
      register-written-name(written-names, slot-name, compound-type);

      if (~slot-type.read-only
	    & ~instance?(real-type, <non-atomic-types>))
	// Write setter method
	format(stream,
	       "define %s inline method %s-setter\n"
		 "    (value :: %s, ptr :: %s) => (result :: %s);\n"
		 "  let mask = lognot(ash((2 ^ %d) - 1, %d));\n"
		 "  %s := logand(%s, mask) + ash(%s, %d);\n"
		 "  value;\n"
		 "end method %s-setter;\n\n",
	       // header
	       slot-type.sealed-string, slot-name, slot-type.mapped-name,
	       compound-type.type-name, slot-type.mapped-name,
	       // mask
	       real-type.bits-in-field, real-type.start-bit,
	       // setter
	       c-accessor(glob-type.type, offset,
			  "ptr", compound-type.type-name),
	       c-accessor(glob-type.type, offset,
			  "ptr", compound-type.type-name),
	       export-value(slot-type, "value"), real-type.start-bit,
	       // footer
	       slot-name);
	register-written-name(written-names, concatenate(slot-name, "-setter"),
			      compound-type);
      end if;
    end unless;
  end for;
end method write-c-accessor-method;

define method d2c-type-tag
    (type :: type-union(<vector-declaration>, <function-type-declaration>, 
                        <pointer-declaration>))
 => (result :: <byte-string>);
  "ptr:";
end method d2c-type-tag;

define method d2c-type-tag
    (type :: type-union(<struct-declaration>, <union-declaration>))
 => (result :: <byte-string>);
  concatenate("#\"", type.canonical-name, "\"");
end method d2c-type-tag;

define method d2c-type-tag
    (t :: <typedef-declaration>) => (result :: <byte-string>);
  t.type.d2c-type-tag;
end method d2c-type-tag;

define method d2c-type-tag
    (type :: <enum-declaration>) => (result :: <byte-string>);
  "int:";
end method d2c-type-tag;

define method d2c-type-tag (type :: <predefined-type-declaration>)
 => (result :: <byte-string>);
  select (type)
    int-type => "int:";
    unsigned-int-type => "unsigned-int:";
    short-type => "short:";
    unsigned-short-type => "unsigned-short:";
    long-type => "long:";
    unsigned-long-type => "long:";
    longlong-type => "long-long:";
    unsigned-longlong-type => "long-long:";
    char-type => "char:";
    unsigned-char-type => "unsigned-char:";
    float-type => "float:";
    double-type => "double:";
    long-double-type => "long-double:";
    void-type => "void:";
    otherwise => error("unknown-type: %s", type.simple-name);
  end select;
end method d2c-type-tag;

define method d2c-arg
    (type :: <type-declaration>, expr :: <string>) => (result :: <string>);
  concatenate(type.d2c-type-tag, ", ", expr);
end method d2c-arg;

define method d2c-arg
    (type :: <pointer-rep-types>, expr :: <string>)
 => (result :: <string>);
  concatenate(type.d2c-type-tag, ", (", expr, ").raw-value");
end method d2c-arg;

define method d2c-arg
    (t :: <typedef-declaration>, expr :: <string>)
 => (result :: <string>);
  d2c-arg(t.type, expr);
end method d2c-arg;

//------------------------------------------------------------------------
// Methods definitions for exported functions
//------------------------------------------------------------------------

// For structures, we must define the basic class, write accessors for each of
// the slots, write an "identity" accessor function, and specify the size of
// the structure.  "Write-c-accessor-method" will do all the real work of
// creating slot accessors.
//

define variable *inhibit-struct-accessors?* = #f;

define method write-declaration
    (decl :: <struct-declaration>, written-names :: <written-name-record>,
     load-string :: <string>, stream :: <stream>)
 => ();
  if (~decl.equated?)
    let supers = decl.superclasses | #("<statically-typed-pointer>");
    format(stream, "define %s class %s (%s) end;\n\n",
	   class-sealing(),
	   decl.dylan-name,
	   as(<byte-string>, apply(join, ", ", supers)));
    register-written-name(written-names, decl.dylan-name, decl);
    if (melange-target == #"d2c")
      format(stream, "define sealed domain make (singleton(%s));\n\n",
	     decl.dylan-name)
    end if;

    local method slot-accessors
	      (end-offset :: <integer>, c-slot :: <declaration>)
	   => (end-offset :: <integer>);
	    let (end-offset, start-offset)
	      = aligned-slot-position(end-offset, c-slot.type);
            unless(c-slot.excluded?)
              write-c-accessor-method(decl, c-slot,
                                      start-offset, written-names, stream);
            end unless;
	    end-offset;
	  end method slot-accessors;

    // This may still be an "incomplete type".  If so, we define the class,
    // but don't write any slot accessors.
    ~*inhibit-struct-accessors?*
      & decl.members 
      & reduce(slot-accessors, 0, decl.coalesced-members);

    if (~*inhibit-struct-accessors?*)
      format(stream,
             "define method pointer-value (value :: %s, #key index = 0) "
               "=> (result :: %s);\n"
               "  value + index * %d;\nend method pointer-value;\n\n",
             decl.dylan-name, decl.dylan-name, decl.c-type-size);
      
      // Finally write out a "content-size" function for use by "make", etc.
      format(stream,
             "define method content-size "
               "(value :: %s) "
               "=> (result :: <integer>);\n"
               "  %d;\nend method content-size;\n\n",
             subclass-type(decl.dylan-name), decl.c-type-size);
    end if;
  end if;
end method write-declaration;

// Unions are just like structs (see above) except that the size and offsets
// are calculated differently.
//
define method write-declaration
    (decl :: <union-declaration>, written-names :: <written-name-record>,
     load-string :: <string>, stream :: <stream>)
 => ();
  if (~decl.equated?)
    let supers = decl.superclasses | #("<statically-typed-pointer>");
    format(stream, "define %s class %s (%s) end;\n\n",
	   class-sealing(),
	   decl.dylan-name,
	   as(<byte-string>, apply(join, ", ", supers)));
    register-written-name(written-names, decl.dylan-name, decl);
    if (melange-target == #"d2c")
      format(stream, "define sealed domain make (singleton(%s));\n\n",
	     decl.dylan-name)
    end if;

    // This may still be an "incomplete type".  If so, we define the class, but
    // don't write any slot accessors.
    if (~ *inhibit-struct-accessors?* & decl.members)
      for (c-slot in decl.coalesced-members)
	if (~c-slot.excluded?)
	  write-c-accessor-method(decl, c-slot, 0, written-names,
				  stream);
	end if;
      end for;
    end if;

    if (~*inhibit-struct-accessors?*)
      format(stream,
             "define method pointer-value (value :: %s, #key index = 0) "
               "=> (result :: %s);\n"
               "  value + index * %d;\nend method pointer-value;\n\n",
             decl.dylan-name, decl.dylan-name, decl.c-type-size);
      
      // Finally write out a "content-size" function for use by "make", etc.
      format(stream,
             "define method content-size "
               "(value :: %s) "
               " => (result :: <integer>);\n  %d;\n"
               "end method content-size;\n\n",
             subclass-type(decl.dylan-name), decl.c-type-size);
    end if;
  end if;
end method write-declaration;

// Enums are defined to be a limited subtype of <integer>, and constants
// values are written for each literal.
//
define method write-declaration
    (decl :: <enum-declaration>, written-names :: <written-name-record>,
     load-string :: <string>, stream :: <stream>)
 => ();
  if (~decl.equated?)
    let type-name = decl.dylan-name;
    
    // This may still be an "incomplete type".  If so, we just define the class
    // as a synonym for <integer>
    if (decl.members)
      let min-enum = reduce(method (a, b) min(a, b.constant-value) end method,
			    $maximum-integer, decl.members);
      let max-enum = reduce(method (a, b) max(a, b.constant-value) end method,
			    $minimum-integer, decl.members);
      format(stream,
	     "define constant %s = limited(<integer>, min: %d, max: %d);\n",
	     type-name, min-enum, max-enum);
      register-written-name(written-names, type-name, decl);

      for (literal in decl.members)
	let name = literal.dylan-name;
	let int-value = literal.constant-value;
	format(stream, "define constant %s :: %s = %d;\n",
	       name, type-name, int-value);
	register-written-name(written-names, name, decl, subname?: #t);
      finally
	new-line(stream);
      end for;
    else
      format(stream, "define constant %s = <integer>;\n\n",
	     type-name);
      register-written-name(written-names, type-name, decl);
    end if;
  end if;
end method write-declaration;

define method write-declaration
    (decl :: <enum-slot-declaration>, written-names :: <written-name-record>,
     load-string :: <string>, stream :: <stream>)
 => ();
  // The routine for <enum-declaration> will already have written these, so we
  // need do nothing.
  #f;
end method write-declaration;


// We write getter functions for global variables and write setter functions
// if appropriate (see comments for "write-c-accessor-method" above).
//
define method write-declaration
    (decl :: <variable-declaration>, written-names :: <written-name-record>,
     load-string :: <string>, stream :: <stream>)
 => ();
  let name = decl.dylan-name;
  let raw-name = anonymous-name();
  let real-type = true-type(decl.type);

  if(melange-target = #"mindy")
    // First get the address of the c object...
    format(stream, "define constant %s = find-c-pointer(\"%s\"%s);\n",
	   raw-name, decl.simple-name, load-string);
  end if;

  // Write a getter method (with an empty parameter list)
  format(stream, "define %s method %s () => (result :: %s);\n  %s;\n"
	   "end method %s;\n\n",
	 decl.sealed-string, decl.getter, decl.mapped-name,
	 import-value(decl,
		      select (melange-target)
			#"mindy" => c-accessor(decl.type,
					       0, raw-name, decl.type-name);
			#"d2c" => concatenate("as(", decl.type-name,
					      ", c-variable(",
					      decl.type.d2c-type-tag, " \"&",
					      decl.simple-name, "\"))");
		      end select),
	 decl.getter);
  register-written-name(written-names, decl.getter, decl);

  // Write a setter method
  if (~decl.read-only 
	& ~instance?(real-type, <non-atomic-types>))
    format(stream,
	   // XXX - Broken when assigning to pointer variables.
	   "define %s method %s (value :: %s) => (result :: %s);\n"
	     "  %s := %s;\n  value;\nend method %s;\n\n",
	   decl.sealed-string, decl.setter, decl.type.mapped-name,
	   decl.mapped-name,
	   select (melange-target)
	     #"mindy" => c-accessor(decl.type, 0, raw-name, decl.type-name);
	     #"d2c" => concatenate("c-variable(",
				   decl.type.d2c-type-tag, " \"&",
				   decl.simple-name, "\")");
	   end select,
	   export-value(decl, "value"), decl.setter);
    register-written-name(written-names, decl.setter, decl);
  end if;
end method write-declaration;

// Separates the parameters between those used as input values and those used
// as result values.  In-out parameters will show up in both sequences.
//
define method split-parameters (decl :: <function-type-declaration>)
 => (in-params :: <sequence>, out-params :: <sequence>);
  let params = as(<list>, decl.parameters);
  let in-params
    = choose(method (p)
	       (p.direction == #"default" | p.direction == #"in"
		  | p.direction == #"in-out");
	     end method, params);
  let out-params
    = choose(method (p) p.direction == #"in-out" | p.direction == #"out" end,
	     params);
  if (decl.result.type ~= void-type)
    values(in-params, pair(decl.result, out-params));
  else
    values(in-params, out-params);
  end if;
end method split-parameters;

// Functions are tricky.  We must find the raw C routine, handle type
// selection for parameters, do special handling for "out" parameters, and
// call any appropriate type mapping routines.  Most of this is pretty
// straightforward, but rather long and tedious.
//
define method write-declaration
    (decl :: <function-declaration>, written-names :: <written-name-record>,
     load-string :: <string>, stream :: <stream>)
 => ();
  let raw-name = anonymous-name();
  let (in-params, out-params) = split-parameters(decl.type);
  let params = decl.type.parameters;

  if (melange-target = #"mindy")
    // First get the raw c function ...
    if (decl.type.result.type == void-type)
      format(stream, "define constant %s = find-c-function(\"%s\"%s);\n",
	     raw-name, decl.simple-name, load-string)
    else
      format(stream,
	     "define constant %s\n  = constrain-c-function("
	       "find-c-function(\"%s\"%s), #(), #t, list(%s));\n",
	     raw-name, decl.simple-name, load-string,
	     decl.type.result.type-name);
    end if;
  end if;

  // ... then create a more robust method as a wrapper.
  format(stream, "define function %s\n    (", decl.dylan-name);
  register-written-name(written-names, decl.dylan-name, decl);
  for (arg in in-params, count from 1)
    if (count > 1) write(stream, ", ") end if;
    case
      instance?(arg, <varargs-declaration>) =>
	format(stream, "#rest %s", arg.dylan-name);
      otherwise =>
	format(stream, "%s :: %s", arg.dylan-name, arg.mapped-name);
    end case;
  end for;
  format(stream, ")\n => (");
  for (arg in out-params, count from 1)
    if (count > 1) write(stream, ", ") end if;
    format(stream, "%s :: %s", arg.dylan-name, arg.mapped-name);
  end for;
  format(stream, ");\n");

  for (arg in out-params)
    // Don't create a new variable if the existing variable is already the
    // right sort of pointer.
    if (instance?(arg, <arg-declaration>)
	  & (arg.direction == #"out"
	       | arg.type.dylan-name ~= arg.original-type.dylan-name))
      format(stream, "  let %s-ptr = make(%s);\n",
	     arg.dylan-name, arg.original-type.type-name);
      if (arg.direction == #"in-out")
	format(stream, "  %s-ptr.pointer-value := %s;\n",
	       arg.dylan-name, export-value(arg, arg.dylan-name));
      end if;
    end if;
  end for;

  let result-type = decl.type.result.type;
  if (result-type ~= void-type)
    format(stream, "  let result-value\n    = ");
  else
    write(stream, "  ");
  end if;

  select(melange-target)
    #"mindy" =>
      begin
	if (~params.empty? & instance?(last(params), <varargs-declaration>))
	  format(stream, "apply(%s, ", raw-name);
	else
	  format(stream, "%s(", raw-name);
	end if;
	for (count from 1, arg in params)
	  if (count > 1) write(stream, ", ") end if;
	  if (instance?(arg, <varargs-declaration>))
	    write(stream, arg.dylan-name);
	  elseif (arg.direction == #"in-out" | arg.direction == #"out")
	    format(stream, "%s-ptr", arg.dylan-name);
	  else
	    write(stream, export-value(arg, arg.dylan-name));
	  end if;
	end for;
	format(stream, ");\n");
      end;
    #"d2c" =>
      begin
	if (~params.empty? & instance?(last(params), <varargs-declaration>))
	  format(stream, "if (~empty?(%s))\n"
		   "    error(\"Variable arguments not yet supported\");\n"
		   "  else\n    ", last(params).dylan-name);
	  format(stream, "call-out(\"%s\", %s", decl.simple-name,
		 decl.type.result.type.d2c-type-tag);
	  for (count from 1, arg in params)
	    if (instance?(arg, <varargs-declaration>))
	      // print nothing (values needed by Mindy)
	      values();
	    elseif (arg.direction == #"in-out" | arg.direction == #"out")
		format(stream, ", ptr: %s-ptr.raw-value", arg.dylan-name);
	    else
	      format(stream, ", %s",
		     d2c-arg(arg.type, export-value(arg, arg.dylan-name)));
	    end if;
	  end for;
	  format(stream, ");\n");
	  format(stream, "  end if;\n");
	else
	  format(stream, "call-out(\"%s\", %s", decl.simple-name,
		 decl.type.result.type.d2c-type-tag);
	  for (count from 1, arg in params)
	    if (arg.direction == #"in-out" | arg.direction == #"out")
		format(stream, ", ptr: %s-ptr.raw-value", arg.dylan-name);
	    else
	      format(stream, ", %s",
		     d2c-arg(arg.type, export-value(arg, arg.dylan-name)));
	    end if;
	  end for;
	  format(stream, ");\n");
	end if;
      end;
  end select;

  if(melange-target = #"d2c")
    if (instance?(result-type.true-type, <pointer-rep-types>))
      format(stream, "  let result-value = make(%s, pointer: result-value);\n",
	     result-type.dylan-name);
    end if;
  end if;

  for (arg in out-params)
    if (instance?(arg, <arg-declaration>))
      format(stream, "  let %s-value = %s;\n",
	     arg.dylan-name,
	     import-value(arg, format-to-string("pointer-value(%s-ptr)",
						arg.dylan-name)));
      if (arg.type.dylan-name ~= arg.original-type.dylan-name)
	format(stream, "destroy(%s-ptr);\n", arg.dylan-name);
      end if;
    end if;
  end for;

  write(stream, "  values(");
  for (arg in out-params, count from 1)
    if (count > 1) write(stream, ", ") end if;
    if (instance?(arg, <arg-declaration>))
      format(stream, "%s-value", arg.dylan-name);
    else
      write(stream, import-value(arg, "result-value"));
    end if;
  end for;

  format(stream, ");\nend function %s;\n\n", decl.dylan-name);
end method write-declaration;

// XXX - Callback and function pointer support is currently in transition.
// We now create distinct types, but we don't yet generate code for
// callback and callout support.

define method write-declaration
    (decl :: <function-type-declaration>,
     written-names :: <written-name-record>, load-string :: <string>,
     stream :: <stream>)
 => ();
  if (~decl.equated?)
    format(stream, "define %s class %s (<function-pointer>) end;\n\n",
	   class-sealing(), decl.dylan-name);
    register-written-name(written-names, decl.dylan-name, decl);
  end if;

  // XXX - Hack alert. Because we haven't integrated the local name mapper
  // with the standard name mapper, we must check for it's existence. This
  // means that the user must explicity specify a function-type clause to
  // get the default maker and caller names. This is inconsistent. See
  // interface.dylan's handling of <function-type-clause> for more info.
  if (decl.local-name-mapper)
    local method get-name(name, alternate-prefix)
	    if (name)
	      as(<string>, name);
	    else
	      decl.local-name-mapper(alternate-prefix, decl.simple-name);
	    end if;
	  end method get-name;
    
    let maker = get-name(decl.callback-maker-name, "make");
    let caller = get-name(decl.callout-function-name, "call");
    select (melange-target)
      #"d2c" =>
	format(stream, "/* binding for %s goes here */\n\n", maker);
	format(stream, "/* binding for %s goes here */\n\n", caller);    
      #"mindy" =>
	format(stream, "/* skipping bindings for %s, %s */\n\n",
	       maker, caller);
	signal(make(<simple-warning>,
		    format-string:
		      "melange: skipping mindy bindings for %s, %s",
		    format-arguments: list(maker, caller)));
      otherwise =>
	error("melange: so, you wrote a new compiler?");
    end select;
    register-written-name(written-names, maker, decl);
    register-written-name(written-names, caller, decl);
  end if;
end method write-declaration;

// Vectors likely still need some work.  Fake it for now.
//
define method write-declaration
    (decl :: <vector-declaration>,  written-names :: <written-name-record>,
     load-string :: <string>, stream :: <stream>)
 => ();
  if (~decl.equated?)
    // Create a new class -- we must insure that the class is a subclass of
    // both the appropriate pointer class and of <c-vector>.  
    let supers = decl.superclasses | list(decl.pointer-equiv.dylan-name,
					  "<c-vector>");
					  
    format(stream, "define %s class %s (%s) end class;\n\n",
	   class-sealing(),
	   decl.dylan-name,
	   as(<byte-string>, apply(join, ", ", supers)));
    register-written-name(written-names, decl.dylan-name, decl);
    if (melange-target == #"d2c")
      format(stream, "define sealed domain make (singleton(%s));\n\n",
	     decl.dylan-name);
      // Douglas Auclair:  we'll trust the C code for its size declarations. 
      // Instead of querying an instance (which may need memory 
      // (de)allocation), we'll trustingly ask the class its size 
      // (particularly if it's anonymous). 
      if (decl.length) 
  	  format(stream, "define inline method content-size (value == %s) " 
	  	   " => (result :: <integer>);\n" 
		   "  %=;\nend method content-size;\n\n", 
	         decl.dylan-name, decl.length); 
      end if; 
    end if;
  end if;
end method write-declaration;

// Typedefs are just aliases.  Define a constant which is initialized to the
// original type.  Because "typedef struct foo foo" is such a common case and
// would lead to conflicts, we check for it specially and ignore the typedef
// if it occurs. 
//
define method write-declaration
    (decl :: <typedef-declaration>, written-names :: <written-name-record>,
     load-string :: <string>, stream :: <stream>)
 => ();
  // We must special case this one since there are so many declarations of the
  // form "typedef struct foo foo".
  if (~decl.equated? 
        & decl.simple-name ~= decl.type.simple-name)
    format(stream, "define constant %s = %s;\n\n",
	   decl.dylan-name, decl.type.dylan-name);
    register-written-name(written-names, decl.dylan-name, decl);
  end if;
end method write-declaration;

// Only "simple" macros will appear amongst the declarations, and even those
// are not guaranteed to be compile time values.  None-the-less, we run them
// through the parser and see if it can come up with either a single specific
// declaration (in which case we treat it as an alias) or with a compile time
// value, which we will declare as a constant.  In other words,
//   #define foo 3
// will yield
//   define constant $foo 3
// and 
//   #define bar "char *"
// might yield
//   define constant <bar> = <c-string>
// (but only if the user had equated "char *" to <c-string>).  Some other
// routine has the task of figuring out what sort of a declaration we are
// aliasing and compute the appropriate sort of name.
//
define method write-declaration
    (decl :: <macro-declaration>, written-names :: <written-name-record>,
     load-string :: <string>, stream :: <stream>)
 => ();
  let raw-value = decl.constant-value;
  let value = select (raw-value by instance?)
		<declaration> => raw-value.dylan-name;
		<abstract-integer>, <float> => format-to-string("%=", raw-value);
		<string> => format-to-string("\"%s\"", 
                                             escape-characters(raw-value));
		<token> => raw-value.string-value;
		<character> => "1"; // for #define FOO\n, suggested by dauclair
	      end select;
  unless(decl.dylan-name = value)
    unless(register-written-name(written-names, decl.dylan-name, decl))
      format(stream, "define constant %s = %s;\n\n", decl.dylan-name, value);
    end unless;
  end unless;
end method write-declaration;

define method escape-characters(s :: <string>) => (s* :: <string>)
  let new = make(<stretchy-vector>);

  for(char in s)
    select(char)
      '\\'      => do(curry(add!, new), "\\\\");
      '"'       => do(curry(add!, new), "\\\"");
      '\n'      => do(curry(add!, new), "\\n\"\n\"");
      '\r'      => do(curry(add!, new), "\\r");
      otherwise => add!(new, char);
    end select;
  end for;
  as(<string>, new);
end method escape-characters;

// For pointers, we need "dereference" and "content-size" functions.  This is
// pretty strightforward.
//
define method write-declaration
    (decl :: <pointer-declaration>, written-names :: <written-name-record>,
     load-string :: <string>, stream :: <stream>)
 => ();
  if (decl.equated? | decl.simple-name = decl.referent.simple-name)
    values();
  else 
    let target-type = decl.referent;
    let target-name = target-type.dylan-name;
    let target-map = target-type.mapped-name;

    // First get the raw c function ...
    let supers = decl.superclasses | #("<statically-typed-pointer>");
    format(stream, "define %s class %s (%s) end;\n\n",
	   class-sealing(),
	   decl.dylan-name,
	   as(<byte-string>, apply(join, ", ", supers)));
    register-written-name(written-names, decl.dylan-name, decl);
    if (melange-target == #"d2c")
      format(stream, "define sealed domain make (singleton(%s));\n\n",
	     decl.dylan-name)
    end if;
    unless(target-type.true-type.abstract-type?)
      format(stream,
             "define inline method pointer-value\n"
               "    (ptr :: %s, #key index = 0)\n => (result :: %s);\n  ",
             decl.dylan-name, target-map);
      write(stream,
            import-value(target-type,
                         c-accessor(target-type,
                                    format-to-string
                                      ("index * %d",
                                       target-type.c-type-size),
                                    "ptr", target-type.type-name)));
      format(stream, ";\nend method pointer-value;\n\n");
      
      // Write setter method, if applicable.
      unless (instance?(true-type(target-type), <non-atomic-types>))
        format(stream,
               "define inline method pointer-value-setter\n"
                 "    (value :: %s, ptr :: %s, #key index = 0)\n"
                 " => (result :: %s);\n  ",
               target-map, decl.dylan-name, target-map);
        write(stream,
              c-accessor(target-type,
                         format-to-string("index * %d",
                                          target-type.c-type-size),
                         "ptr", target-type.type-name));
        format(stream, " := %s;\n  value;\nend method pointer-value-setter;\n\n",
               export-value(target-type, "value"));
      end unless;
    end unless;

    // Finally write out a "content-size" function for use by "make", etc.
    format(stream,
           "define method content-size "
             "(value :: %s) "
             "=> (result :: <integer>);\n  %d;\n"
             "end method content-size;\n\n",
           subclass-type(decl.dylan-name), target-type.c-type-size);
  end if;
end method write-declaration;

// Writes out appropriate code to load object file and insure that all desired
// objects are included.  Returns a string which can be included in a
// "find-c-function" call so that the symbols will be found.
//

// LAST CALL
define method write-file-load
    (include-files :: <sequence>,
     object-files :: false-or(<sequence>),
     decls :: <sequence>,
     stream :: <stream>)
 => (load-string :: <string>);
  select(melange-target)
    #"mindy" =>
      begin
	if (~empty?(object-files))
	  let names = map(simple-name,
			  choose(rcurry(instance?, <value-declaration>),
				 decls));
	  let file-name = anonymous-name();
	  format(stream, "define constant %s\n  "
		         "= load-object-file(#(", file-name);
	  for (comma = #f then #t, file in object-files)
	    if (comma) write(stream, ", ") end if;
	    format(stream, "\"%s\"", file);
	  end for;
	  write(stream, "), include: #(");
	  for (comma = #f then #t, name in names)
	    if (comma) write(stream, ", ") end if;
	    format(stream, "\"%s\"", name);
	  end for;
	  format(stream, "));\n\n");
	  concatenate(", file: ", file-name);
	else
	  ""
	end if;
      end;
    #"d2c" =>
      begin
	// Ignore object files, since they will be statically linked into
	// the d2c compiled binaries.
	for (file in include-files)
	  format(stream, "c-include(\"%s\");\n", file);
	end for;
	format(stream, "\n");
	"";
      end;
  end select;
end method write-file-load;

// Writes out a list of symbols which must be defined, in a format compatible
// with make-init.pl.  This should provide a portable mechanism for handling
// static linking of libraries.
//
define method write-mindy-includes
    (file :: false-or(<string>), decls :: <sequence>) => ();
  if (file)
    let stream = make(<file-stream>, locator: file, direction: #"output");
    for (decl in decls)
      select (decl by instance?)
	<function-declaration> => format(stream, "%s()\n", decl.simple-name);
	<object-declaration> => format(stream, "%s\n", decl.simple-name);
	otherwise => #f;
      end select;
    end for;
    close(stream);
  end if;
end method write-mindy-includes;
