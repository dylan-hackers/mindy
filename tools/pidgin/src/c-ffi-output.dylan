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

// We also export 'exported-names', a vector of names which the generated
// module should probably export.
define variable exported-names :: <vector> = make(<vector>);

// This helper function registers an exported name and transparently returns it.
define function register-exported-name
    (name :: <string>)
 => (result :: <string>)
  if (~member?(exported-names, name))
    exported-names := add(exported-names, name);
  end if;
  name;
end function;

// Just a quick helper function for converting names into more Dylanish identifiers.
define function output-name
    (name :: <string>, #key class :: <boolean> = #f, pointer :: <boolean> = #f)
 => (result :: <string>)
    name := substring-replace(name, "_", "-");
  // <>'s for classes.
  if (class)
    name := concatenate("<", name);
    // *'s for pointers.
    if (pointer)
      name := concatenate(name, "*");
    end if;
    name := concatenate(name, ">");
  end if;
  name;
end function;

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
    // header := c-output(type.c-typedef-type, name: type.c-typedef-name);
  else
    tname := c-output(type.c-typedef-type);
  end if;
  concatenate(header, "define C-subtype ", register-exported-name(output-name(type.c-typedef-name, class: #t)), " (", tname, ")\n  pointer-type-name: ", register-exported-name(output-name(type.c-typedef-name, class: #t, pointer: #t)), ";\nend C-subtype;\n");
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
  // XXX: This is a bit hacky.
  if(type.c-type-members) // type not incomplete
    if (type.c-type-tag[0] ~= '$')
      register-exported-name(output-name(type.c-type-tag, class: #t));
    end if;
    let def :: <string> = concatenate("define C-struct ", output-name(type.c-type-tag, class: #t), "\n");
    for (counter :: <integer> from 0 to size(type.c-type-members) - 1 )
      if (object-class(type.c-type-members[counter]) = <c-bit-field>)
        def := concatenate(def, "  // <bit-field> not yet supported.\n");
      else
        let t = type.c-type-members[counter].c-member-variable-type;
        if (object-class(t) = <c-typedef-type>)
          t := register-exported-name(output-name(t.c-typedef-name, class: #t));
        else
          t := c-output(t);
        end if;
        def := concatenate(def, "  slot ", register-exported-name(output-name(type.c-type-members[counter].c-member-variable-name)), " :: ", t, ";\n");
      end if;
    end for;
    concatenate(def, "end C-struct;\n");
  else
    ""
  end if;
end method;

define method c-output
    (type :: <c-union-type>)
 => (result :: <string>)
  let def :: <string> = concatenate("define C-union ", register-exported-name(output-name(type.c-type-tag, class: #t)), "\n");
  for (counter :: <integer> from 0 to size(type.c-type-members) - 1 )
    if (object-class(type.c-type-members[counter]) = <c-bit-field>)
      def := concatenate(def, "  // <bit-field> not yet supported.\n");
    else
      def := concatenate(def, "  slot ", register-exported-name(output-name(type.c-type-members[counter].c-member-variable-name)), " :: ", c-output(type.c-type-members[counter].c-member-variable-type), ";\n");
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
    let def :: <string> = concatenate("define C-function ", register-exported-name(output-name(decl.c-variable-name)), "\n  c-name: \"", decl.c-variable-name, "\";\n");
    for (i :: <integer> from 0 to size(decl.c-variable-type.c-function-parameter-types) - 1 )
      // XXX The following bit of code is UGLY and repeated. Can be fixed.
      let ctype = decl.c-variable-type.c-function-parameter-types[i];
      if ((object-class(ctype) = <c-pointer-type>) & (object-class(ctype.c-pointer-referent-type) = <c-typedef-type>))
        ctype := ctype.c-pointer-referent-type;
       pointer := #t;
      end if;
      if (object-class(ctype) = <c-typedef-type>)
        if (pointer)
          ctype := output-name(ctype.c-typedef-name, class: #t, pointer: #t);
        else
            ctype := output-name(ctype.c-typedef-name, class: #t);
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
        ctype := output-name(ctype.c-typedef-name, class: #t, pointer: #t);
      else
        ctype := output-name( ctype.c-typedef-name, class: #t);
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
        ctype := register-exported-name(output-name( ctype.c-typedef-name, class: #t, pointer: #t));
      else
        ctype := output-name(output-name(ctype.c-typedef-name, class: #t));
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
  concatenate("define constant ", register-exported-name(output-name(decl.c-define-name)), " :: <integer> = ", integer-to-string(decl.c-integer-define-value), ";\n");
end method;

define method c-output
    (decl :: <c-string-define>)
 => (result :: <string>)
  concatenate("define constant ", register-exported-name(output-name(decl.c-define-name)), " :: <string> = ", decl.c-string-define-value, ";\n");
end method;

define method c-output
    (decl :: <c-unknown-define>)
 => (result :: <string>)
  // Ignore this.
  "";
end method;

define method c-output
    (decl :: <c-empty-define>)
 => (result :: <string>)
  // Ignore this.
  "";
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

// Handle <c-file> objects.
define method c-output
    (file :: <c-file>)
 => (result :: <string>)
  concatenate("c-include(\"", file.c-file-name, "\");");
end method;
