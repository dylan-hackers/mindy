module: c-ffi-output
synopsis: C-FFI output module for Pidgin
author: Nolan Darilek <nolan_d@bigfoot.com>
copyright: 

//==============================================================================
// C-FFI output module.
//==============================================================================

// Pidgin output modules define one method: c-output.
// c-output performs different actions when called on different objects.
// The eventual goal is to simply call c-output on a <c-file> and watch it
// recursively call different variations of itself.

// First we'll handle mapping Pidgin types into C-FFI types.

define method c-output
    (type :: <c-void-type>)
 => (result :: <string>)
  "<C-void>";
end method;

define method c-output
    (type :: <c-integer-type>)
 => (result :: <string>)
  "<C-int>";
end method;

define method c-output
    (type :: <c-long-type>)
 => (result :: <string>)
  "<C-long>";
end method;

define method c-output
    (type :: <c-floating-point-type>)
 => (result :: <string>)
  "<C-float>";
end method;

define method c-output
    (type :: <c-short-type>)
 => (result :: <string>)
  "<C-short>";
end method;

define method c-output
    (type :: <c-char-type>)
 => (result :: <string>)
  "<C-char>";
end method;

// Handle pointer types.

/*
define method c-output
    (type :: <c-pointer-type>)
 => (result :: <string>)
  let vtype :: <c-type> = type.c-pointer-referent-type;
  if (vtype = <c-integer-type>)
    "<C-int*>";
  elseif (vtype = <c-long-type>)
    "<C-long*>";
  elseif (vtype = <c-floating-point-type>)
    "<C-float*>";
  elseif (vtype = <c-short-type>)
    "<C-short*>";
  elseif (vtype = <c-char-type>)
    "<C-char*>";
  else
    "Unknown type";
  end if;
end method;
*/

define method c-output
    (type :: <c-pointer-type>)
 => (result :: <string>)
  // XXX: I can't believe I'm releasing code that does this!!! This just screams "Fix me or die!" = doesn't work for <c-type> it seems, so we'll mangle strings.
  let result = c-output(type.c-pointer-referent-type);
  result := substring-replace(result, ">", "*>");
end method;

// Handle unknown types gracelessly.

define method c-output
    (type :: <c-type>)
 => (result :: <string>)
  "<C-void> /* Unknown type */";
end method;

// Structures and unions.

define method c-output
    (type :: <c-struct-type>)
 => (result :: <string>)
  let def :: <string> = concatenate("define C-struct <", type.c-type-tag, ">\n");
  for (counter :: <integer> from 0 to size(type.c-type-members) - 1 )
    if (type.c-type-members[counter] = <c-bit-field>)
      def := concatenate(def, "  // <bit-field> not yet supported.\n");
    else
      def := concatenate(def, "  slot ", type.c-type-members[counter].c-member-variable-name, " :: ", c-output(type.c-type-members[counter].c-member-variable-type), ";\n");
    end if;
  end for;
  concatenate(def, "end C-struct;\n");
end method;

define method c-output
    (type :: <c-union-type>)
 => (result :: <string>)
  let def :: <string> = concatenate("define C-union <", type.c-type-tag, ">\n");
  for (counter :: <integer> from 0 to size(type.c-type-members) - 1 )
    if (type.c-type-members[counter] = <c-bit-field>)
      def := concatenate(def, "  // <bit-field> not yet supported.\n");
    else
      def := concatenate(def, "  slot ", type.c-type-members[counter].c-member-variable-name, " :: ", c-output(type.c-type-members[counter].c-member-variable-type), ";\n");
    end if;
  end for;
  concatenate(def, "end C-union;\n");
end method;

//==============================================================================
// Declarations
//==============================================================================

// Structs and unions.

/*
define method c-output
    (decl :: <c-tagged-type-declaration>)
 => (result :: <string>)
  // In this case, c-output has already been defined.
  c-output(decl.c-tagged-type-declaration-type);
end method;
*/

// Variable declarations.

define method c-output
    (decl :: <c-variable-declaration>)
 => (result :: <string>)
  // Since most items are variables, we act differently based on the variable's type.
  if (object-class(decl.c-variable-type) = <c-function-type>)
    let def :: <string> = concatenate("define C-function ", decl.c-variable-name, "\n  c-name: \"", decl.c-variable-name, "\";\n");
    for (i :: <integer> from 0 to size(decl.c-variable-type.c-function-parameter-types) - 1 )
      def := concatenate(def, "  parameter arg", integer-to-string(i), " :: ", c-output(decl.c-variable-type.c-function-parameter-types[i]), ";\n");
    end for;
    def := concatenate(def, "  result res :: ", c-output(decl.c-variable-type.c-function-return-type), ";\n");
    def := concatenate(def, "end C-function;\n");
  else
    concatenate("define C-variable ", decl.c-variable-name, " :: ", c-output(decl.c-variable-type), "\n  c-name: ", decl.c-variable-name, ";\nend C-variable;\n");
  end if;
end method;

// Defines.

define method c-output
    (decl :: <c-integer-define>)
 => (result :: <string>)
  concatenate("define constant ", decl.c-define-name, " :: <integer> = ", integer-to-string(decl.c-integer-define-value), ";\n");
end method;

define method c-output
    (decl :: <c-string-define>)
 => (result :: <string>)
  concatenate("define constant ", decl.c-define-name, " :: <string> = ", decl.c-string-define-value, ";\n");
end method;

define method c-output
    (decl :: <c-unknown-define>)
 => (result :: <string>)
  concatenate("define constant ", decl.c-define-name, ";\n");
end method;

// Handle unknown declarations.
define method c-output
    (decl :: <c-declaration>)
 => (result :: <string>)
  "// Unknown declaration.";
end method;
