module: dylan

define class <symbol> (<object>)
  slot symbol-string :: <string>, setter: #f, required-init-keyword: string:;
  // slot symbol-hashing :: <fixed-integer>;
end;

/*
begin
  for (symbol in %%primitive initial-symbols ())
    $symbol-table[symbol.symbol-string] := symbol;
  end;
end;
*/

define sealed method make (class == <symbol>, #key string :: <string>)
    => res :: <symbol>;
/*
  element($symbol-table, string, default: #f)
    | element($symbol-table, string) := next-method();
*/
  error("Can't make symbols yet.");
end;

define sealed method as (class == <symbol>, string :: <string>)
    => res :: <symbol>;
  make(<symbol>, string: string);
end;

define sealed inline method as (class == <string>, symbol :: <symbol>)
    => res :: <string>;
  symbol.symbol-string;
end;
