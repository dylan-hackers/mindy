Module: defs-file

define class <defs-module> (<object>)
  constant slot defs-module-locator :: <string>,
    required-init-keyword: locator:;
  constant slot defs-module-imports :: <stretchy-vector>
    = make(<stretchy-vector>);
  constant slot defs-module-types :: <object-table>
    = make(<object-table>);
  constant slot defs-module-functions :: <stretchy-vector>
    = make(<stretchy-vector>);
  constant slot defs-module-field-accessors :: <object-table>
    = make(<object-table>);
  constant slot defs-module-written :: <object-table>
    = make(<object-table>);
  slot defs-module-options :: <list> = #();
end class;

define abstract class <defs-clause> (<object>)
  constant slot clause-name :: <symbol>,
    required-init-keyword: name:;
  constant slot clause-options :: <list>,
    required-init-keyword: options:;
end class;

define class <defs-alias> (<defs-clause>)
  constant slot alias-clause :: <defs-clause>,
    required-init-keyword: alias:;
end class;

define class <defs-enum> (<defs-clause>)
  constant slot enum-choices :: <list>,
    required-init-keyword: choices:;
end class;

define class <defs-flags> (<defs-clause>)
  constant slot flags-choices :: <list>,
    required-init-keyword: choices:;
end class;

define class <defs-composite> (<defs-clause>)
  constant slot composite-fields :: <list>,
    required-init-keyword: fields:;
end class;

define class <defs-boxed> (<defs-composite>)
  // no additional slots
end class;

define class <defs-struct> (<defs-composite>)
  // no additional slots
end class;

define class <defs-object> (<defs-composite>)
  constant slot object-super :: <list>,
    required-init-keyword: super:;
end class;

define class <defs-func> (<defs-clause>)
  constant slot func-return :: <object>,
    required-init-keyword: return:;
  constant slot func-arguments :: <list>,
    required-init-keyword: arguments:;
end class;

define function import-defs(name :: <string>) => (mod :: <defs-module>);
  let mod = make(<defs-module>, locator: name);

  format(*standard-error*, "[%s", name);
  force-output(*standard-error*);

  let input = make(<file-stream>, locator: name);
  for(form = scheme-read(input)
        then scheme-read(input),
      until: scheme-eof-object?(form))
    if(instance?(form, <pair>))
      select(first(form))
        #"import" =>
          add!(mod.defs-module-imports, import-defs(form[1]));

        #"define-type-alias" =>
          let clause = make(<defs-alias>,
                            name: form[1],
                            alias: defs-type(mod, form[2]),
                            options: #());
          mod.defs-module-types[form[1]] := clause;
          

        #"define-enum" =>
          let clause = make(<defs-enum>,
                            name: form[1],
                            choices: nth-tail(form, 2),
                            options: #());
          mod.defs-module-types[form[1]] := clause;
          
        #"define-flags" =>
          let clause = make(<defs-flags>,
                            name: form[1],
                            choices: nth-tail(form, 2),
                            options: #());
          mod.defs-module-types[form[1]] := clause;

        #"define-boxed" =>
          let options = nth-tail(form, 2);
          if(options.size >= 1
               & options.first.size >= 1
               & first(first(options)) == #"fields")
            let clause = make(<defs-boxed>,
                              name: form[1],
                              fields: tail(first(options)),
                              options: tail(options));
            mod.defs-module-types[form[1]] := clause;
            for(field in tail(first(options)))
              mod.defs-module-field-accessors[field[1]] := #t;
            end for;
          else
            let clause = make(<defs-boxed>,
                              name: form[1],
                              fields: #(),
                              options: options);
            mod.defs-module-types[form[1]] := clause;
          end if;
                              

        #"define-object" =>
          let options = nth-tail(form, 3);
          if(options.size >= 1
               & options.first.size >= 1
               & first(first(options)) == #"fields")
            let clause = make(<defs-object>,
                              name: form[1],
                              super: form[2],
                              fields: tail(first(options)),
                              options: tail(options));
            mod.defs-module-types[form[1]] := clause;
            for(field in tail(first(options)))
              mod.defs-module-field-accessors[field[1]] := #t;
            end for;
          else
            let clause = make(<defs-object>,
                              name: form[1],
                              super: form[2],
                              fields: #(),
                              options: options);
            mod.defs-module-types[form[1]] := clause;
          end if;

        #"define-struct" =>
          let options = nth-tail(form, 2);
          if(options.size >= 1
               & options.first.size >= 1
               & first(first(options)) == #"fields")
            let clause = make(<defs-struct>,
                              name: form[1],
                              fields: tail(first(options)),
                              options: tail(options));
            mod.defs-module-types[form[1]] := clause;
            for(field in tail(first(options)))
              mod.defs-module-field-accessors[field[1]] := #t;
            end for;
          else
            let clause = make(<defs-struct>,
                              name: form[1],
                              fields: #(),
                              options: options);
            mod.defs-module-types[form[1]] := clause;
          end if;

        #"define-type" =>
          #f;

        #"define-func" =>
          let clause = make(<defs-func>,
                            name: form[1],
                            return: form[2],
                            arguments: form[3],
                            options: nth-tail(form, 4));
          add!(mod.defs-module-functions, clause);

        #"options" =>
          mod.defs-module-options := tail(form);

        #"add-options" =>
          #f;

        otherwise =>
          error("Unrecognized .defs file form: %=", form)
      end select;
    else
      error("Unrecognized .defs file form: %=", form)
    end if;
  end for;
  close(input);

  format(*standard-error*, "]");
  force-output(*standard-error*);

  mod;
end function;

define method defs-type
    (mod :: <defs-module>, name :: <symbol>)
 => (type :: false-or(<defs-clause>));
  let type = element(mod.defs-module-types, name, default: #f);
  if(type)
    type;
  else
    any?(method(mod :: <defs-module>) defs-type(mod, name) end,
         mod.defs-module-imports);
  end if;
end method;

define method defs-module-includes
    (mod :: <defs-module>)
 => (includes :: <sequence>);
  block(return)
    for(option in mod.defs-module-options)
      if(instance?(option, <pair>) & head(option) == #"includes")
        let includes = make(<stretchy-vector>);
        for(line in tail(option))
          force-output(*standard-error*);
          let i = for(i from 0 below line.size, until: line[i] == '<')
                  finally i;
                  end for;
          let j = for(j from i + 1 below line.size, until: line[i] == '>')
                  finally j;
                  end for;
          let file = copy-sequence(line, start: i + 1, end: j - 1);
          add!(includes, file);
        end for;
        return(includes);
      end if;
    end for;
    #[];
  end block;
end method;

define function nth-tail
    (the-list :: <list>, n :: <integer>)
 => (nth :: <list>);
  if(n = 0) the-list else nth-tail(tail(the-list), n - 1) end if;
end function;
