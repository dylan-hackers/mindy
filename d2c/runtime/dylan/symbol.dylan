rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/symbol.dylan,v 1.4 2002/10/31 10:17:10 andreas Exp $
copyright: see below
module: dylan-viscera

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
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

//
// Symbols.
//
// This file defines the support code for symbols.
//
// ### Note: currently, there is no support for unicode symbols.  We'll
// have to fix that someday.  Also, we should probably consider storing
// the symbol-string directly in a vector slot instead of a seperate
// string object.  Or at least copying the string before returning it in
// as(<string>, symbol).
//


// <symbol-table> -- internal.
//
// The set of all symbols in the system.  Some day we might export symbol
// tables, but for now we don't.
// 
define class <symbol-table> (<object>)
  //
  // The number of symbols in this table.  Used to decide when to grow the
  // table.
  slot sym-count :: <integer>, init-value: 0;
  //
  // The length of the bucket vector.  Duplicated here for effeciency.
  slot cell-count :: <integer>, required-init-keyword: #"size";
  //
  // The bucket vector.  Each element is a chain (though symbol-next) of
  // the symbols whos modulo(symbol-hashing, cell-count) == the index.  We
  // give it a spurious init-value that gets overwritten in the initialize
  // method just so the compiler will believe that this slot is always
  // initialized.
  slot cells :: <simple-object-vector>, init-value: #[];
  //
  // Grow the table when the number of symbols exceeds this value.  Again,
  // given a spurious init-value so that it is always initialized.
  slot resize-threshold :: <integer>, init-value: 0;
end class <symbol-table>;

// Seal make on <symbol-table>.
// 
define sealed domain make (singleton(<symbol-table>));

// initialize -- exported GF method.
//
// Make the cells vector and compute the resize-threshold.
// 
define sealed method initialize
    (table :: <symbol-table>, #next next-method, #key size, #all-keys)
  next-method();
  table.cells := make(<simple-object-vector>, size: size, fill: #f);
  table.resize-threshold := floor/(size * 3, 2);
end method initialize;

// rehash-symbols -- internal.
//
// Grow the table and redistribute the symbols in the bucket vector.
// 
define method rehash-symbols (table :: <symbol-table>) => ();
  let new-size = table.cell-count * 2 - 1;
  let new-cells = make(<simple-object-vector>, size: new-size, fill: #f);

  local method iterate* (sym :: false-or(<symbol>)) => ();
	  if (sym)
	    let sym :: <symbol> = sym;
	    let next = sym.symbol-next;
	    let cell-index = modulo(sym.symbol-hashing, new-size);
	    sym.symbol-next := new-cells[cell-index];
	    new-cells[cell-index] := sym;
	    iterate*(next);
	  end if;
	end method iterate*;
  for (sym-list in table.cells)
    iterate*(sym-list);
  end for;
  table.cell-count := new-size;
  table.cells := new-cells;
  table.resize-threshold := floor/(new-size * 3, 2);
end method rehash-symbols;


// <symbol> -- exported from Dylan.
// 
define class <symbol> (<object>)
  //
  // The ``name'' of this symbol.  DO NOT MODIFY.
  slot symbol-string :: <string>, setter: #f, required-init-keyword: string:;
  //
  // A case-independent hashing of the name.  We pre-compute it for effeciency.
  slot symbol-hashing :: <integer>, init-value: 0;
  //
  // The next symbol in the bucket chain in the symbol table.
  slot symbol-next :: false-or(<symbol>), init-value: #f;
end;

// make{singleton(<symbol>)} -- exported GF method
//
// Dylan doesn't define what happens when you call make on <symbol>, but
// we do anyway.  We need to somehow define how symbols are made.
// 
define sealed method make
    (class == <symbol>, #next next-method,
     #key string :: <string>, table :: <symbol-table>)
 => (res :: <symbol>);
//  let string :: <byte-string> = as(<byte-string>, string);
  let hash :: <integer> = symbol-hash(string);
  let cell-index :: <integer> = modulo(hash, table.cell-count);

  block (return)
    for (sym = table.cells[cell-index]
	   then sym.symbol-next,
	 while: sym)
      if (symbol-equal(string, sym.symbol-string))
	return(sym);
      end if;
    end for;
    // Not found -- create.
    if (table.sym-count > table.resize-threshold)
      rehash-symbols(table);
      // Recompute the cell-index because the cell-count will have changed.
      cell-index := modulo(hash, table.cell-count);
    end if;
    let new-sym :: <symbol> = next-method();
    new-sym.symbol-next := table.cells[cell-index];
    new-sym.symbol-hashing := hash;
    table.cells[cell-index] := new-sym;
    table.sym-count := table.sym-count + 1;
    new-sym;
  end block;
end method make;

// Seal initialize on <symbol>.
// 
define sealed domain initialize (<symbol>);

// Seal = on symbols.
// 
//seal generic \= (<symbol>, <object>);
//seal generic \= (<object>, <symbol>);

// symbol-hash -- internal.
//
// Compute the hashing for a symbol.
// 
define method symbol-hash
    (str :: <byte-string>) => (result :: <integer>);
  for (hash :: <integer> = 0
	 then abs(logxor(logior(ash(hash, 5), ash(hash, -27)),
			 logior(as(<integer>, char), #x20))),
       char in str)
  finally
    hash;
  end for;
end method symbol-hash;

// symbol-equal -- internal.
//
// Compare two strings ignoring case.
// 
define method symbol-equal
    (str1 :: <byte-string>, str2 :: <byte-string>) => (result :: <boolean>);
  if (str1.size == str2.size)
    block (return)
      for (char1 in str1, char2 in str2)
	if (char1 ~== char2 & as-lowercase(char1) ~== as-lowercase(char2))
	  return(#f);
	end if;
      end for;
      #t;
    end block;
  end if;
end method symbol-equal;


// $symbol-table -- internal.
//
// The default (and currently, only) symbol table.  Pre-populate it with the
// initial symbols.
// 
define constant $symbol-table :: <symbol-table>
  = begin
      let sz = for (sym = %%primitive(initial-symbols) then sym.symbol-next,
		    count :: <integer> from 0,
		    while: sym)
	       finally count;
	       end for;
      let table = make(<symbol-table>, size: sz);
      table.sym-count := sz;
      local method iterate* (symbol :: false-or(<symbol>)) => ();
	      if (symbol)
		let symbol :: <symbol> = symbol;
		let next = symbol.symbol-next;
		let hash = symbol-hash(symbol.symbol-string);
		let cell-index = modulo(hash, table.cell-count);
		symbol.symbol-hashing := hash;
		symbol.symbol-next := table.cells[cell-index];
		table.cells[cell-index] := symbol;
		iterate*(next);
	      end if;
	    end method iterate*;
      iterate*(%%primitive(initial-symbols));
      table;
    end;


// as{singleton(<symbol>),<string>} -- exported GF method.
//
// Return the symbol corresponding to the given string.
// 
define sealed method as (class == <symbol>, string :: <string>)
    => res :: <symbol>;
  make(<symbol>, string: string, table: $symbol-table);
end;

// as{one-of(<string>,<byte-string>),<symbol>} -- exported GF method.
//
// Return the symbol's name.
// 
define sealed inline method as
    (class == <string>, symbol :: <symbol>)
    => res :: <string>;
  symbol.symbol-string;
end;

define sealed inline method as
    (class == <byte-string>, symbol :: <symbol>)
    => res :: <byte-string>;
  symbol.symbol-string;
end;
