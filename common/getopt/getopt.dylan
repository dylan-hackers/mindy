library: getopt
module: getopt
author: Jeff Dubrule <igor@pobox.com> & Ole Tetlie
copyright: LGPL
rcs-header: $Header: /scm/cvs/src/common/getopt/getopt.dylan,v 1.6 1998/09/25 12:48:45 igor Exp $

define class <option-table> (<value-table>)
  // perhaps we'll need something here later
end;

define method table-protocol (ht :: <option-table>)
 => (key-test :: <function>, key-hash :: <function>);
  values(\=, string-hash);
end method table-protocol;

define method as (c == <string>, char :: <character>) => str :: <string>;
  make(<string>, size: 1, fill: char);
end method;

define method add-internal (table :: <option-table>, option :: <option>)
  // Note: names can't be symbols, as symbols are case-insensitive
  for (name in option.names)
    table[name] := option;
    // This still works because we aren't trying to 
    // assign a string or boolean into table[name].
  end for;
end method;

define class <option> (<object>)
  slot names :: <list>, init-keyword: names:;
  slot doc-string :: <string>, init-keyword: doc-string:;
  slot value :: type-union(<string>, <boolean>), init-keyword: value:;
end;

define method add-option (table :: <option-table>, doc-string :: <string>,
			  value :: type-union(<string>, <boolean>), 
			  #rest names)
 => ();
  let real-names :: <list> = make (<list>);
  for (name in names)
    if (instance? (name, <string>))
      real-names := add! (real-names, name);
      // Should we be blowing up if we're not given a string/character?
    end if;
  end for;

  let option = make (<option>, names: real-names, doc-string: doc-string,
		     value: value);

  add-internal (table, option);
end method add-option;

// OK, so we need a way of retrieving the actual <option>, but element() 
// is totally overloaded.  So, by defining this magic constant, we can
// suck out the option so that element-setter() can play with it.
// If anyone knows a better approach here, let me know, eh?  (igor@pobox.com)
define constant $secret-decoder-constant = pair(#f, #f);

// All values in table are <string> or <boolean>, however the default may be
// any type, so <object> is all we know we can return.
define method element (table :: <option-table>, 
                       key :: type-union(<string>, <character>),
		       #next next-method,
                       #key default: default = #f)
 => value :: <object>;

  let option = next-method(table, as(<string>, key), default: default);
  if (option == default)
    default;
  elseif (default == $secret-decoder-constant)
    option;
  else
    option.value;
  end if;
end method element;

define method element-setter (new-value :: type-union(<string>, <boolean>), 
			      table :: <option-table>,
			      key :: type-union(<string>, <character>))
 => new-value :: type-union(<string>, <boolean>);

  if (has-option?(table, key))
    element(table, key, default: $secret-decoder-constant).value := new-value;
  else
    // This can't happen as a result of a parse-options() argument, but only
    // as a programming error (*options*["non-existant"] := "foobie"), so its
    // OK to just throw an error here.
    error("Attempt to set a non-existant option.");
  end if;
end method;

define constant $option-not-there = pair(#f, #f);

define method has-option? (table :: <option-table>, key :: type-union(<string>, <character>))
  element(table, key, default: $option-not-there) ~= $option-not-there;
end method;

define method parse-options (table :: <option-table>, argv :: <collection>) => non-options :: <list>;
  let non-options = make(<list>);
  let option-wannabes = make(<list>);
  let missing-vals = make(<list>);

  let pos = as(<list>, argv);

  local method parse-option () => ();
	  let arg = pos.head;
	  if (arg[1] = '-')
	    let equalindex = find-key(arg, curry(\=, '='), failure: -1);
	    let argname = copy-sequence(arg, start: 2);
	    if (equalindex = -1)
	      if (has-option?(table, argname) = #f)
	        option-wannabes := add!(option-wannabes, argname);	      
	      elseif (instance?(table[argname], <boolean>))
		table[argname] := #t;
	      else
		missing-vals := add!(missing-vals, argname);
	      end if;
	    else
	      let value = copy-sequence(argname, start: equalindex - 1);
	      argname := copy-sequence(argname, end: equalindex - 2);
  	      if (has-option?(table, argname) = #f)
	        option-wannabes := add!(option-wannabes, argname);
	      elseif (instance?(table[argname], <boolean>))
		option-wannabes := add!(option-wannabes, argname);
	      else 
	        table[argname] := value;
	      end if;
	    end if;
	  else 
	    if (has-option?(table, arg[1]) = #f)
	      option-wannabes := add!(option-wannabes, as(<string>, arg[1]));
	    elseif (instance?(table[arg[1]], <boolean>))
	      table[arg[1]] := #t;
	    elseif (empty?(pos.tail) | pos.tail.head[0] = "-")
	      missing-vals := add!(missing-vals, as(<string>, arg[1]));
	    else
	      table[arg[1]] := pos.tail.head;
	      pos := pos.tail;
	    end if;
	  end if;
	end method;

  local method parse-arg-and-continue () => ();
	  unless (empty?(pos))
	    let arg = pos.head;
	    if (arg = "--")
	      // The rest are non-options.
	      for (arg in pos.tail)
		non-options := add!(non-options, arg);
	      end for;
	    elseif (arg.size > 1 & arg[0] = '-')
	      parse-option();
	      pos := pos.tail;
	      parse-arg-and-continue();
	    else
	      non-options := add!(non-options, arg);
	      pos := pos.tail;
	      parse-arg-and-continue();
	    end if;
	  end unless;
	end method;

  parse-arg-and-continue();

  // if option-wannabes or missing-vals has anything in it, we should die
  // and emit the help text or something.

  // Since add!() adds things to the front of the list, flip it back over:
  reverse!(non-options);
end method parse-options;

