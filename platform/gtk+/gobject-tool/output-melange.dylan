Module: output-melange

define function output-melange
    (mod :: <defs-module>,
     repository :: <c-type-repository>,
     stream :: <stream>)
 => ();
  output-includes(mod, stream);
  
  for(type keyed-by name in mod.defs-module-types)
    output-melange-clause(type, mod, repository, stream);
  end;

  for(func in mod.defs-module-functions)
    output-melange-clause(func, mod, repository, stream);
    force-output(stream);
  end;
end function;

define method output-melange-clause
    (clause :: <defs-alias>,
     mod :: <defs-module>,
     repository :: <c-type-repository>,
     stream :: <stream>)
 => ();
  format(stream, "define constant %s = %s;\n\n",
         type-name(clause),
         type-name(clause.alias-clause));
end method;

define method type-name-base(clause :: <defs-alias>) => (name :: <string>);
  if(instance?(clause.alias-clause, <defs-composite>))
    concatenate(as(<string>, clause.clause-name), "*");
  else
    as(<string>, clause.clause-name);
  end if;
end method;

define method type-rep(clause :: <defs-alias>) => (name :: <string>);
  type-rep(clause.alias-clause);
end method;

define method output-melange-clause
    (clause :: <defs-enum>,
     mod :: <defs-module>,
     repository :: <c-type-repository>,
     stream :: <stream>)
 => ();
  format(stream, "define constant %s = <integer>;\n", type-name(clause));
  let typedef = c-named-type(repository, as(<string>, clause.clause-name));
  let enum-type = typedef.c-typedef-type;
  let member-values = make(<object-table>);
  for(member in enum-type.c-enum-members)
    member-values[as(<symbol>, member.c-enum-constant-name)]
      := member.c-enum-constant-value;
  end for;
  for(choice in clause.enum-choices)
    let value = element(member-values, choice[1], default: #f);
    if(value)
      format(stream, "define constant $%s = %d;\n",
             melange-name(choice[1]),
             value);
    else
      error("enum %s is missing choice %s",
            as(<string>, clause.clause-name),
            as(<string>, choice[1]));
    end if;
  end for;
  new-line(stream);
end method;

define method type-name-base(clause :: <defs-enum>) => (name :: <string>);
  as(<string>, clause.clause-name);
end method;

define method type-rep(clause :: <defs-enum>) => (name :: <string>);
  "int";
end method;

define method output-melange-clause
    (clause :: <defs-flags>,
     mod :: <defs-module>,
     repository :: <c-type-repository>,
     stream :: <stream>)
 => ();
  format(stream, "define constant %s = <integer>;\n", type-name(clause));
  let typedef = c-named-type(repository, as(<string>, clause.clause-name));
  let enum-type = typedef.c-typedef-type;
  let member-values = make(<object-table>);
  for(member in enum-type.c-enum-members)
    member-values[as(<symbol>, member.c-enum-constant-name)]
      := member.c-enum-constant-value;
  end for;
  for(choice in clause.flags-choices)
    let value = element(member-values, choice[1], default: #f);
    if(value)
      format(stream, "define constant $%s = %d;\n",
             melange-name(choice[1]),
             value);
    else
      error("enum %s is missing choice %s",
            as(<string>, clause.clause-name),
            as(<string>, choice[1]));
    end if;
  end for;
  new-line(stream);
end method;

define method type-name-base(clause :: <defs-flags>) => (name :: <string>);
  as(<string>, clause.clause-name);
end method;

define method type-rep(clause :: <defs-flags>) => (name :: <string>);
  "int";
end method;

define method output-melange-clause
    (clause :: <defs-composite>,
     mod :: <defs-module>,
     repository :: <c-type-repository>,
     stream :: <stream>)
 => ();
  for(field in clause.composite-fields)
    if(instance?(field[0], <list>) & field[0][0] == #"include")
      output-melange-included-struct-field(clause, field, mod,
                                           repository, stream);
    elseif(instance?(field[0], <list>) & field[0][0] == #"fvec")
      output-melange-vector-field(clause, field, mod, repository, stream);
    else
      output-melange-data-field(clause, field, mod, repository, stream);
    end if;
  end for;
end method;

define method output-melange-included-struct-field
    (clause :: <defs-composite>,
     field :: <object>,
     mod :: <defs-module>,
     repository :: <c-type-repository>,
     stream :: <stream>)
 => ();
  unless(any?(method(mod :: <defs-module>)
                  element(mod.defs-module-field-accessors, field[1],
                          default: #f);
              end, mod.defs-module-imports)
           | element(mod.defs-module-written, field[1], default: #f))
    format(stream, "define open generic %s-value\n", melange-name(field[1]));
    format(stream, "    (obj :: <statically-typed-pointer>)\n");
    format(stream, " => (value :: <object>);\n\n");
    
    mod.defs-module-written[field[1]] := #t;
  end unless;

  let field-type-name = type-name(defs-type(mod, field[0][1]));

  format(stream, "define inline method %s-value\n", melange-name(field[1]));
  format(stream, "    (obj :: %s)\n", type-name(clause));
  format(stream, " => (value :: %s);\n", field-type-name);
  output-includes(mod, stream);
  format(stream, "  make(%s,\n", field-type-name);
  format(stream, "       pointer: obj.raw-value\n");
  format(stream, "                  + c-expr(int:, \"offsetof(%s, %s)\"));\n",
         clause.clause-name, field[1]);
  format(stream, "end;\n\n");
end method;

define method output-melange-vector-field
    (clause :: <defs-composite>,
     field :: <object>,
     mod :: <defs-module>,
     repository :: <c-type-repository>,
     stream :: <stream>)
 => ();
  unless(any?(method(mod :: <defs-module>)
                  element(mod.defs-module-field-accessors, field[1],
                          default: #f);
              end, mod.defs-module-imports)
           | element(mod.defs-module-written, field[1], default: #f))
    format(stream, "define open generic %s-value\n", melange-name(field[1]));
    format(stream, "    (obj :: <statically-typed-pointer>)\n");
    format(stream, " => (value :: <object>);\n\n");
    
    mod.defs-module-written[field[1]] := #t;
  end unless;

  let field-type-name = melange-map-type(field[0], mod);

  format(stream, "define inline method %s-value\n", melange-name(field[1]));
  format(stream, "    (obj :: %s)\n", type-name(clause));
  format(stream, " => (value :: %s);\n", field-type-name);
  output-includes(mod, stream);
  format(stream, "  make(%s,\n", field-type-name);
  format(stream, "       pointer: obj.raw-value\n");
  format(stream, "                  + c-expr(int:, \"offsetof(%s, %s)\"));\n",
         clause.clause-name, field[1]);
  format(stream, "end;\n\n");
end method;

define method output-melange-data-field
    (clause :: <defs-composite>,
     field :: <object>,
     mod :: <defs-module>,
     repository :: <c-type-repository>,
     stream :: <stream>)
 => ();
  let rep = melange-rep(field[0], mod);
  let accessor = field[1];
  
  unless(any?(method(mod :: <defs-module>)
                  element(mod.defs-module-field-accessors, field[1],
                          default: #f);
              end, mod.defs-module-imports)
           | element(mod.defs-module-written, field[1], default: #f))
    format(stream, "define open generic %s-value\n",
           melange-name(field[1]));
    format(stream, "    (obj :: <statically-typed-pointer>)\n");
    format(stream, " => (value :: <object>);\n\n");
    
    format(stream, "define open generic %s-value-setter\n",
           melange-name(field[1]));
    format(stream, "    (value :: <object>, "
             "obj :: <statically-typed-pointer>)\n");
    format(stream, " => (value :: <object>);\n\n");
    
    mod.defs-module-written[field[1]] := #t;
  end unless;
  
  format(stream, "define inline method %s-value\n", melange-name(field[1]));
  format(stream, "    (obj :: %s)\n", type-name(clause));
  format(stream, " => (value :: %s);\n", melange-map-type(field[0], mod));
  output-includes(mod, stream);
  if(rep = "ptr")
    format(stream, "  make(%s,\n", melange-map-type(field[0], mod));
    format(stream, "       pointer: c-struct-field(%s:, obj.raw-value,\n",
           rep);
    format(stream, "                               \"%s\", \"%s\"));\n",
           clause.clause-name, accessor);
  else
    format(stream, "  c-struct-field(%s:, obj.raw-value,\n", rep);
    format(stream, "                 \"%s\", \"%s\");\n",
           clause.clause-name, accessor);
  end if;
  format(stream, "end;\n\n");
  
  format(stream, "define inline method %s-value-setter\n",
         melange-name(field[1]));
  format(stream, "    (value :: %s, obj :: %s)\n",
         melange-map-type(field[0], mod), type-name(clause));
  format(stream, " => (value :: %s);\n", melange-map-type(field[0], mod));
  output-includes(mod, stream);
  if(rep = "ptr")
    format(stream, "  c-struct-field(%s:, obj.raw-value,\n", rep);
    format(stream, "                 \"%s\", \"%s\")",
           clause.clause-name, accessor);
    format(stream, " := value.raw-value;\n");
    format(stream, "  value;\n");
  else
    format(stream, "  c-struct-field(%s:, obj.raw-value,\n", rep);
    format(stream, "                 \"%s\", \"%s\")",
           clause.clause-name, accessor);
    format(stream, " := value;\n");
  end if;
  format(stream, "end;\n\n");
end method;

define method output-melange-clause
    (clause :: <defs-boxed>,
     mod :: <defs-module>,
     repository :: <c-type-repository>,
     stream :: <stream>)
 => ();
  let size-expr = #f;
  for(option in clause-options(clause))
    if(option[0] == #"size")
      size-expr := option[1];
    end if;
  end for;

  if(size-expr)
    format(stream,
           "define functional class %s (<indexable-statically-typed-pointer>) "
             "end;\n", type-name(clause));
    format(stream, "define sealed domain make(singleton(%s));\n\n",
           type-name(clause));

    format(stream,
           "define sealed inline method content-size\n");
    format(stream,
           "    (class == %s) => (size :: <integer>);\n", type-name(clause));
    output-includes(mod, stream);
    format(stream,
           "  c-expr(int:, \"%s\");\n", size-expr);
    format(stream,
           "end method;\n\n");
  else
    format(stream,
           "define functional class %s (<statically-typed-pointer>) end;\n",
           type-name(clause));
    format(stream, "define sealed domain make(singleton(%s));\n\n",
           type-name(clause));
  end if;

  next-method();
end method;

define method output-melange-clause
    (clause :: <defs-struct>,
     mod :: <defs-module>,
     repository :: <c-type-repository>,
     stream :: <stream>)
 => ();
  let size-expr = #f;
  for(option in clause-options(clause))
    if(option[0] == #"size")
      size-expr := option[1];
    end if;
  end for;

  if(size-expr)
    format(stream,
           "define functional class %s (<indexable-statically-typed-pointer>) "
             "end;\n", type-name(clause));
    format(stream, "define sealed domain make(singleton(%s));\n\n",
           type-name(clause));

    format(stream,
           "define sealed inline method content-size\n");
    format(stream,
           "    (class == %s) => (size :: <integer>);\n", type-name(clause));
    output-includes(mod, stream);
    format(stream,
           "  c-expr(int:, \"%s\");\n", size-expr);
    format(stream,
           "end method;\n\n");
  else
    format(stream,
           "define functional class %s (<statically-typed-pointer>) end;\n",
           type-name(clause));
    format(stream, "define sealed domain make(singleton(%s));\n\n",
           type-name(clause));
  end if;

  next-method();
end method;

define method output-melange-clause
    (clause :: <defs-object>,
     mod :: <defs-module>,
     repository :: <c-type-repository>,
     stream :: <stream>)
 => ();
  if(empty?(clause.object-super))
    format(stream,
           "define functional class %s (<statically-typed-pointer>) end;\n",
           type-name(clause));
  else
    format(stream,
           "define functional class %s (%s) end;\n",
           type-name(clause),
           type-name(defs-type(mod, clause.object-super.first)));
  end if;
  format(stream, "define sealed domain make(singleton(%s));\n\n",
         type-name(clause));
  next-method();
end method;

define method type-name-base(clause :: <defs-composite>) => (name :: <string>);
  concatenate(as(<string>, clause.clause-name), "*");
end method;

define method type-rep(clause :: <defs-composite>) => (name :: <string>);
  "ptr";
end method;

define method output-melange-clause
    (func :: <defs-func>,
     mod :: <defs-module>,
     repository :: <c-type-repository>,
     stream :: <stream>)
 => ();
  format(stream, "define inline-only method %s\n",
	 melange-name(func.clause-name));
  format(stream, "    (");

  for(arg in func.func-arguments, first? = #t then #f)
    unless(first?)
      format(stream, ",\n     ");
    end;
    if(arg[0] == #"full-callback")
      format(stream, "%s :: <GtkFunction>", arg[1]);
      format(stream, ",\n     ");
      format(stream, "marshall :: <GtkCallbackMarshal>");
      format(stream, ",\n     ");
      format(stream, "data :: <gpointer>");
      format(stream, ",\n     ");
      format(stream, "destroy :: <GtkDestroyNotify>");
    elseif(instance?(arg[0], <list>) & arg[0][0] == #"cvec")
      format(stream, "n%s :: <integer>", as(<string>, arg[1]));
      format(stream, ",\n     ");
      format(stream, "%s :: %s",
             as(<string>, arg[1]),
             melange-map-type(list(#"cvec", arg[0][1]), mod));
    elseif(instance?(arg[0], <list>) & arg[0][0] == #"cvecr")
      format(stream, "%s :: %s",
             as(<string>, arg[1]),
             melange-map-type(list(#"cvec", arg[0][1]), mod));
      format(stream, ",\n     ");
      format(stream, "n%s :: <integer>", as(<string>, arg[1]));
    else
      format(stream, "%s :: %s",
             as(<string>, arg[1]),
             melange-map-type(arg[0], mod));
    end if;
  end for;
  format(stream, ")\n");

  format(stream, " => (");

  let return = if(instance?(func.func-return, <list>))
                 first(func.func-return);
               else
                 func.func-return;
               end if;

  unless(return == #"none")
    format(stream, "return :: %s", melange-map-type(return, mod));
  end unless;

  format(stream, ");\n");

  output-includes(mod, stream);

  if(return == #"none")
    format(stream, "  call-out(\"%s\", void:", func.clause-name);
  else
    format(stream,
           "  let return-value = call-out(\"%s\", %s:",
           func.clause-name, melange-rep(return, mod));
  end if;

  for(arg in func.func-arguments, first? = #t then #f)
    format(stream, ",\n           ");

    if(arg[0] == #"full-callback")
      format(stream, "ptr: %s", arg[1]);
      format(stream, ",\n           ");
      format(stream, "ptr: marshall");
      format(stream, ",\n           ");
      format(stream, "ptr: data.raw-value");
      format(stream, ",\n           ");
      format(stream, "ptr: destroy");
    elseif(instance?(arg[0], <list>) & arg[0][0] == #"cvec")
      format(stream, "int: n%s", as(<string>, arg[1]));
      format(stream, ",\n           ");
      format(stream, "ptr: %s.raw-value", as(<string>, arg[1]));
    elseif(instance?(arg[0], <list>) & arg[0][0] == #"cvecr")
      format(stream, "ptr: %s.raw-value", as(<string>, arg[1]));
      format(stream, ",\n           ");
      format(stream, "int: n%s", as(<string>, arg[1]));
    else
      let rep = melange-rep(arg[0], mod);
      if(rep = "ptr")
          format(stream, "%s: %s.raw-value", rep, as(<string>, arg[1]));
      else
          format(stream, "%s: %s", rep, as(<string>, arg[1]));
      end if;
    end if;
  end for;
  format(stream, ");\n");

  if(return == #"none")
    format(stream, "  values();\n");
  elseif(melange-rep(return, mod) = "ptr")
    format(stream,
           "  make(%s, pointer: return-value);\n",
           melange-map-type(return, mod));
  else
    format(stream, "  return-value;\n");
  end if;

  format(stream, "end method;\n\n");
end method;

define method output-melange-clause
    (clause :: <defs-clause>,
     mod :: <defs-module>,
     repository :: <c-type-repository>,
     stream :: <stream>)
 => ();
  format(stream, "// %s\n\n", clause.clause-name);
end method;

define method type-name(clause :: <defs-clause>) => (name :: <string>);
  concatenate("<", clause.type-name-base, ">");
end method;

define method melange-map-type
    (type :: <object>, mod :: <defs-module>)
 => (name :: <string>);
  concatenate("<", melange-map-type-base(type, mod), ">");
end method;

define method melange-map-type-base
    (type :: <object>, mod :: <defs-module>)
 => (name :: <string>);
  select(type)
    #"string" =>
      "gchar*";
    #"static_string" =>
      "gchar*";
    #"int" =>
      "gint";
    #"uint" =>
      "guint";
    #"long" =>
      "glong";
    #"ulong" =>
      "gulong";
    #"float" =>
      "gfloat";
    #"double" =>
      "gdouble";
    #"bool" =>
      "gboolean";
    #"point" =>
      "GdkPoint";
    #"rect" =>
      "GtkRectangle*";
    #"type" =>
      "GtkType";
    #"atom" =>
      "GdkAtom";
    #"file-descriptor" =>
      "gint";
    #"callback" =>
      "GtkSignalFunc";
    #"pointer" =>
      "gpointer";
    otherwise =>
      if(instance?(type, <symbol>))
        let clause = defs-type(mod, type);
        if(clause)
          clause.type-name-base;
        else
          as(<string>, type);
        end if;
      elseif(instance?(type, <list>)
               & (type[0] == #"cvec" | type[0] == #"fvec" | type[0] == #"ret"))
        concatenate(melange-map-type-base(type[1], mod), "*");
      elseif(instance?(type, <list>) & type[0] == #"list")
        "GList*";
      elseif(instance?(type, <list>) & type[0] == #"slist")
        "GSList*";
      else
        "XXX";
      end if;
  end select;
end method;

define method melange-rep
    (type :: <object>, mod :: <defs-module>)
 => (name :: <string>);
  select(type)
    #"string" =>
      "ptr";
    #"static_string" =>
      "ptr";
    #"int" =>
      "int";
    #"uint" =>
      "int";
    #"long" =>
      "int";
    #"ulong" =>
      "int";
    #"float" =>
      "float";
    #"double" =>
      "double";
    #"bool" =>
      "int";
    #"rect" =>
      "ptr";
    #"type" =>
      "int";
    #"atom" =>
      "int";
    #"file-descriptor" =>
      "int";
    otherwise =>
      if(instance?(type, <symbol>))
        let clause = defs-type(mod, type);
        if(clause)
          clause.type-rep;
        else
          "ptr";
        end if;
      elseif(instance?(type, <list>)
               & (type[0] == #"cvec" | type[0] == #"fvec" | type[0] == #"ret"))
        "ptr";
      elseif(instance?(type, <list>) & type[0] == #"list")
        "ptr";
      elseif(instance?(type, <list>) & type[0] == #"slist")
        "ptr";
      else
        "int"; // error("unknown defs-file type %=", type);
      end if;
  end select;
end method;

define function output-melange-exports
    (mod :: <defs-module>,
     module-name :: <string>,
     imports :: <sequence>,
     stream :: <stream>)
 => ();
  format(stream, "Module: Dylan-user\n\n");

  format(stream, "define module %s\n", module-name);
  for(import in imports)
    format(stream, "  use %s;\n", import);
  end for;

  for(type keyed-by name in mod.defs-module-types)
    output-melange-exports-clause(type, mod, stream);
  end;

  for(func in mod.defs-module-functions)
    output-melange-exports-clause(func, mod, stream);
  end;

  format(stream, "end module;\n\n");
end function;

define method output-melange-exports-clause
    (clause :: <defs-alias>,
     mod :: <defs-module>,
     stream :: <stream>)
 => ();
  format(stream, "  export %s;\n", type-name(clause));
end method;

define method output-melange-exports-clause
    (clause :: <defs-enum>,
     mod :: <defs-module>,
     stream :: <stream>)
 => ();
  format(stream, "  export %s;\n", type-name(clause));
  for(choice in clause.enum-choices)
    format(stream, "  export $%s;\n", melange-name(choice[1]));
  end for;
end method;

define method output-melange-exports-clause
    (clause :: <defs-flags>,
     mod :: <defs-module>,
     stream :: <stream>)
 => ();
  format(stream, "  export %s;\n", type-name(clause));
  for(choice in clause.flags-choices)
    format(stream, "  export $%s;\n", melange-name(choice[1]));
  end for;
end method;

define method output-melange-exports-clause
    (clause :: <defs-composite>,
     mod :: <defs-module>,
     stream :: <stream>)
 => ();
  format(stream, "  export %s;\n", type-name(clause));
  for(field in clause.composite-fields)
    let field-name = melange-name(field[1]);
    unless(any?(method(mod :: <defs-module>)
                  element(mod.defs-module-field-accessors, field[1],
                          default: #f);
                end, mod.defs-module-imports)
             | element(mod.defs-module-written, field[1], default: #f))
      if(instance?(field[0], <list>)
           & (field[0][0] == #"include" | field[0][0] == #"fvec"))
        format(stream, "  export %s-value;\n", field-name);
      else
        format(stream, "  export %s-value, %s-value-setter;\n",
               field-name, field-name);
      end if;
      mod.defs-module-written[field[1]] := #t;
    end unless;
  end for;
end method;

define method output-melange-exports-clause
    (func :: <defs-func>,
     mod :: <defs-module>,
     stream :: <stream>)
 => ();
  format(stream, "  export %s;\n", melange-name(func.clause-name));
end method;

define method output-includes
    (mod :: <defs-module>,
     stream :: <stream>)
 => ();
  format(stream, "  c-include(\"stddef.h\");\n");
  for(file in defs-module-includes(mod))
    format(stream, "  c-include(\"%s\");\n", file);
  end for;
end method;

define method melange-name
    (name :: <symbol>)
 => (new-name-string :: <string>);
  let pname = as(<string>, name);
  let new-name = make(<stretchy-vector>);
  for(c in pname)
    if(c = '_')
      add!(new-name, '-');
    else
      add!(new-name, c);
    end if;
  end for;
  as(<string>, new-name);
end method;
