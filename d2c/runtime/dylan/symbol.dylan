rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/symbol.dylan,v 1.6 1995/12/15 14:06:02 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

define class <symbol> (<object>)
  slot symbol-string :: <string>, setter: #f, required-init-keyword: string:;
  slot symbol-hashing :: <fixed-integer>, init-value: 0;
  slot symbol-next :: false-or(<symbol>), init-value: #f;
end;

define class <symbol-table> (<object>)
  slot sym-count :: <fixed-integer>, init-value: 0;
  slot cell-count :: <fixed-integer>, required-init-keyword: #"size";
  slot cells :: <simple-object-vector>;
  slot resize-threshold :: <fixed-integer>, init-value: 0;
end class <symbol-table>;

define method initialize
    (table :: <symbol-table>, #next next-method, #key size, #all-keys)
  next-method();
  table.cells := make(<simple-object-vector>, size: size, fill: #f);
  table.resize-threshold := floor/(size * 3, 2);
end method initialize;

define method symbol-hash
    (str :: <byte-string>) => (result :: <fixed-integer>);
  for (hash :: <fixed-integer> = 0
	 then abs(logxor(logior(ash(hash, 5), ash(hash, -27)),
			 logior(as(<fixed-integer>, char), #x20))),
       char in str)
  finally
    hash;
  end for;
end method symbol-hash;

define method symbol-equal
    (str1 :: <byte-string>, str2 :: <byte-string>) => (result :: <boolean>);
  if (str1.size = str2.size)
    block (return)
      for (char1 in str1, char2 in str2)
	if (char1 ~= char2 & as-lowercase(char1) ~= as-lowercase(char2))
	  return(#f);
	end if;
      end for;
      #t;
    end block;
  end if;
end method symbol-equal;

define method rehash-symbols (table :: <symbol-table>) => ();
  let new-size = table.cell-count * 2 - 1;
  let new-cells = make(<simple-object-vector>, size: new-size, fill: #f);

  local method iterate (sym :: false-or(<symbol>)) => ();
	  if (sym ~= #f)
	    let sym :: <symbol> = sym;
	    let next = sym.symbol-next;
	    let cell-index = modulo(sym.symbol-hashing, new-size);
	    sym.symbol-next := new-cells[cell-index];
	    new-cells[cell-index] := sym;
	    iterate(next);
	  end if;
	end method iterate;
  for (sym-list in table.cells)
    iterate(sym-list);
  end for;
  table.cell-count := new-size;
  table.cells := new-cells;
  table.resize-threshold := floor/(new-size * 3, 2);
end method rehash-symbols;

define variable $symbol-table :: <symbol-table>
  = begin
      let sz = for (sym = %%primitive initial-symbols () then sym.symbol-next,
		    count :: <fixed-integer> from 0,
		    until: sym == #f)
	       finally count;
	       end for;
      let table = make(<symbol-table>, size: sz);
      table.sym-count := sz;
      local method iterate (symbol :: false-or(<symbol>)) => ();
	      if (symbol)
		let symbol :: <symbol> = symbol;
		let next = symbol.symbol-next;
		let hash = symbol-hash(symbol.symbol-string);
		let cell-index = modulo(hash, table.cell-count);
		symbol.symbol-hashing := hash;
		symbol.symbol-next := table.cells[cell-index];
		table.cells[cell-index] := symbol;
		iterate(next);
	      end if;
	    end method iterate;
      iterate(%%primitive initial-symbols ());
      table;
    end;

define sealed method make
    (class == <symbol>, #next next-method, #key string :: <string>)
 => (res :: <symbol>);
//  let string :: <byte-string> = as(<byte-string>, string);
  let hash :: <fixed-integer> = symbol-hash(string);
  let cell-index :: <fixed-integer> = modulo(hash, $symbol-table.cell-count);

  block (return)
    for (sym = $symbol-table.cells[cell-index]
	   then sym.symbol-next,
	 until: sym == #f)
      if (symbol-equal(string, sym.symbol-string))
	return(sym);
      end if;
    end for;
    // Not found -- create.
    if ($symbol-table.sym-count > $symbol-table.resize-threshold)
      rehash-symbols($symbol-table);
    end if;
    let new-sym :: <symbol> = next-method();
    new-sym.symbol-next := $symbol-table.cells[cell-index];
    new-sym.symbol-hashing := hash;
    $symbol-table.cells[cell-index] := new-sym;
    $symbol-table.sym-count := $symbol-table.sym-count + 1;
    new-sym;
  end block;
end method make;

define method as (class == <symbol>, string :: <string>)
    => res :: <symbol>;
  make(<symbol>, string: string);
end;

define sealed inline method as
    (class :: one-of(<string>, <byte-string>), symbol :: <symbol>)
    => res :: <string>;
  symbol.symbol-string;
end;
