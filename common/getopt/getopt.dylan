library: getopt
module: getopt
author: Jeff Dubrule & Ole Tetlie
copyright: LGPL
rcs-header: $Header: /scm/cvs/src/common/getopt/getopt.dylan,v 1.3 1998/09/23 17:23:05 andreas Exp $

define class <option-table> (<object-table>)
  // perhaps we'll need something here later
end;

define method add-internal (table :: <option-table>, name :: <string>,
			    option :: <option>)
    // Note: names can't be symbols, as symbols are case-insensitive
  table[name] := option;
end method;

define class <option> (<object>)
  slot names :: <list>, init-keyword: names:;
  slot doc-string :: <string>, init-keyword: doc-string:;
  slot value :: <object>, init-keyword: value:;
end;

define method add-option (table :: <option-table>, doc-string :: <string>,
			  value :: <object>, #rest names)
  let real-names :: <list> = make (<list>);
  for (name in names)
    if (instance? (name, <string>))
      real-names := add! (real-names, name);
    end if;
  end for;

  let option = make (<option>, names: real-names, doc-string: doc-string,
		     value: value);
  for (name in real-names)
    add-internal (table, name, option);
  end for;
end method;

define method element (table :: <option-table>, key :: <string>,
		       #key default = #f)
 => (element :: <object>);
  element (table, key, default: default).value;
end method;

define method element-setter (new-value :: <object>, table :: <option-table>,
			      key :: <string>)
 => (new-value :: <object>);
  table[<symbol>].value := new-value;
end method;

define constant option-not-there = pair(#f, #f);

define method has-option? (table :: <option-table>, key :: <string>)
  element (table, key, default: option-not-there)
    ~= option-not-there;
end method;

define method parse-options (table :: <option-table>, argv :: <list>)
// options with argument is not handled yet
// i'm not sure yet how i should iterate and skip arguments
  for (arg in argv)
    if (arg.size > 0 & arg[0] = '-')
      if (arg.size > 1 & arg[1] = '-')
	//parse long args
	1;
      elseif (arg.size > 1)
	let the-arg = copy-sequence (arg, start: 0, end: 2);
	if (has-option? (table, the-arg))
	  table[the-arg] := #t;
	end if;
      end if;
    end if;
  end for;
end method;


