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

define method c-output
    (type :: <c-pointer-type>)
 => (result :: <string>)
  // XXX This can probably be done in a more dignified manner. :)
  let result = c-output(type.c-pointer-referent-type);
  result := substring-replace(result, ">", "*>");
end method;

// Typedefs.

define method c-output
    (type :: <c-typedef-type>)
 => (result :: <string>)
  let tname :: <string> = "";
  let header :: <string> = "";
  if (instance?(type.c-typedef-type, <c-tagged-type>))
    tname := type.c-typedef-type.c-type-tag;
    header := c-output(type.c-typedef-type);
  else
    tname := c-output(type.c-typedef-type);
  end if;
  concatenate(header, "define C-mapped-subtype <", type.c-typedef-name, "> (", tname, ")\n  pointer-type-name: ", concatenate("<", type.c-typedef-name, "*>"), ";\nend C-subtype;\n");
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
    if (object-class(type.c-type-members[counter]) = <c-bit-field>)
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
    if (object-class(type.c-type-members[counter]) = <c-bit-field>)
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

define method c-output
    (decl :: <c-tagged-type-declaration>)
 => (result :: <string>)
  // In this case, c-output has already been defined.
if (object-class(decl.c-tagged-type-declaration-type) ~= <c-enum-type>)
  c-output(decl.c-tagged-type-declaration-type);
  else
    "// Unknown declaration.";
  end if;
end method;

// Variable declarations.

define method c-output
    (decl :: <c-variable-declaration>)
 => (result :: <string>)
  // Since most items are variables, we act differently based on the variable's type.
  // XXX This one's a mess, folks! It screams "Optimize me!"
  // This variable is used below.
  let pointer :: <boolean> = #f;
  if (object-class(decl.c-variable-type) = <c-function-type>)
    let def :: <string> = concatenate("define C-function ", decl.c-variable-name, "\n  c-name: \"", decl.c-variable-name, "\";\n");
    for (i :: <integer> from 0 to size(decl.c-variable-type.c-function-parameter-types) - 1 )
      // XXX The following bit of code is UGLY and repeated. Can be fixed.
      let ctype = decl.c-variable-type.c-function-parameter-types[i];
      if ((object-class(ctype) = <c-pointer-type>) & (object-class(ctype.c-pointer-referent-type) = <c-typedef-type>))
        ctype := ctype.c-pointer-referent-type;
       pointer := #t;
      end if;
      if (object-class(ctype) = <c-typedef-type>)
        if (pointer)
          ctype := concatenate("<", ctype.c-typedef-name, "*>");
        else
            ctype := concatenate("<", ctype.c-typedef-name, ">");
        end if;
      else
        ctype := c-output(ctype);
      end if;
      def := concatenate(def, "  parameter arg", integer-to-string(i), " :: ", ctype, ";\n");
      pointer := #f;
    end for;
    let ctype = decl.c-variable-type.c-function-return-type;
    if ((object-class(ctype) = <c-pointer-type>) & (object-class(ctype.c-pointer-referent-type) = <c-typedef-type>))
      ctype := ctype.c-pointer-referent-type;
     pointer := #t;
    end if;
    if (object-class(ctype) = <c-typedef-type>)
      if (pointer)
        ctype := concatenate("<", ctype.c-typedef-name, "*>");
      else
        ctype := concatenate("<", ctype.c-typedef-name, ">");
    end if;
    else
      ctype := c-output(ctype);
    end if;
    def := concatenate(def, "  result res :: ", ctype, ";\n");
    def := concatenate(def, "end C-function;\n");
  else
    let ctype = decl.c-variable-type;
    if ((object-class(ctype) = <c-pointer-type>) & (object-class(ctype.c-pointer-referent-type) = <c-typedef-type>))
      ctype := ctype.c-pointer-referent-type;
     pointer := #t;
    end if;
    if (object-class(ctype) = <c-typedef-type>)
      if (pointer)
        ctype := concatenate("<", ctype.c-typedef-name, "*>");
      else
        ctype := concatenate("<", ctype.c-typedef-name, ">");
    end if;
    else
      ctype := c-output(ctype);
    end if;
    concatenate("define C-variable ", decl.c-variable-name, " :: ", ctype, "\n  c-name: ", decl.c-variable-name, ";\nend C-variable;\n");
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

// Typedefs.
define method c-output
    (decl :: <c-typedef-declaration>)
  // Here again, everything is done at a lower level.
  c-output(decl.c-typedef-declaration-type);
end method;

// Handle unknown declarations.
define method c-output
    (decl :: <c-declaration>)
 => (result :: <string>)
  "// Unknown declaration.";
end method;
