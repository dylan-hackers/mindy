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
    (type)
 => (result :: <string>)
  "unknown";
end method;

// Structures and unions.

define method c-output
    (type :: <c-struct-type>)
 => (result :: <string>)
  let def :: <string> = concatenate("define C-struct <", type.c-type-tag, ">\n");
  for (counter :: <integer> from 0 to size(type.c-type-members) - 1 )
    def := concatenate(def, "  slot ", type.c-type-members[counter].c-member-variable-name, " :: ", c-output(type.c-type-members[counter].c-member-variable-type), ";\n");
  end for;
  concatenate(def, "end C-struct;\n");
end method;

define method c-output
    (type :: <c-union-type>)
 => (result :: <string>)
  let def :: <string> = concatenate("define C-union <", type.c-type-tag, ">\n");
  for (counter :: <integer> from 0 to size(type.c-type-members) - 1 )
    def := concatenate(def, "  slot ", type.c-type-members[counter].c-member-variable-name, " :: ", c-output(type.c-type-members[counter].c-member-variable-type), ";\n");
  end for;
  concatenate(def, "end C-union;\n");
end method;

//==============================================================================
// Declarations
//==============================================================================

// Variable declarations.

define method c-output
    (decl :: <c-variable-declaration>)
 => (result :: <string>)
  concatenate("define C-variable ", decl.c-variable-name, " :: ", c-output(decl.c-variable-type), "\n  c-name: ", decl.c-variable-name, ";\nend C-variable;\n");
end method;
