module: common-extensions

//=========================================================================
//  position
//=========================================================================
//  Find the key corresponding to a particular value in a sequence.
//
//  XXX - Added 'default' as an argument until Harlequin documents what to
//  do when no value can be found.
//  SPEED - This implementation is slow and needs to be "pulled down" into
//  methods for each built-in sequence class.
//
//  XXX - could we write this in terms of find-key?

define open generic position
    (sequence :: <sequence>, value :: <object>,
     #key predicate :: <function>, skip :: <integer>)
 => (key :: <object>);

define method position
    (sequence :: <sequence>, value :: <object>,
     #key predicate :: <function> = \==, skip :: <integer> = 0)
 => (key :: false-or(<integer>))
  block (return)
    let (initial-state :: <object>,
	 limit :: <object>,
	 next-state :: <function>,
	 finished-state? :: <function>,
	 current-key :: <function>,
	 current-element :: <function>,
	 current-element-setter :: <function>,
	 copy-state :: <function>) =
      forward-iteration-protocol(sequence);

    // Iterate by hand over the collection.
    let skipped :: <integer> = 0;
    for (state = initial-state
	   then next-state(sequence, state),
	 until: finished-state?(sequence, state, limit))
      if (predicate(current-element(sequence, state), value))
	if (skipped == skip)
	  return(current-key(sequence, state));
	else
	  skipped := skipped + 1;
	end if;
      end if;
    end for;

    // Because collections can only be keyed by integers, #f is a unique
    // return value. (Thanks to Andrew Shalit.)
    #f;
  end block;
end method position;

//=========================================================================
//  remove-all-keys!
//=========================================================================
//  Removes all keys from a mutable collection.
//
//  XXX - We suck in a function of the same name from %Hash-Tables, but it
//  is defined on <mutable-explicit-key-collection> instead of
//  <mutable-collection>. What collection types should this apply to,
//  anyway?

/*
define generic remove-all-keys!
    (collection :: <mutable-collection>)
 => ();
*/


//=========================================================================
//  fill-table!
//=========================================================================
//  Insert a sequence of alternating keys and elements into a table.
//
//  XXX - Do we 'remove-all-keys!' before running 'fill-table!'?
//  XXX - What error do we signal if we are passed an odd number of
//  keys and values?

define function fill-table!
    (table :: <table>, keys-and-elements :: <sequence>)
 => (table :: <table>)
  let (initial-state :: <object>,
       limit :: <object>,
       next-state :: <function>,
       finished-state? :: <function>,
       current-key :: <function>,
       current-element :: <function>,
       current-element-setter :: <function>,
       copy-state :: <function>) =
    forward-iteration-protocol(keys-and-elements);

  // A little bit of fancy iteration here. We'd use the iterate macro, but
  // we want to compile under Mindy some day.
  local
    method next-pair (state)
      unless (finished-state?(keys-and-elements, state, limit))
	let key = current-element(keys-and-elements, state);
	if (finished-state?(keys-and-elements, state, limit))
	  error("Incomplete key/value pair in call of fill-table!: %=", key);
	end;
	let new-state = next-state(keys-and-elements, state);
	let value = current-element(keys-and-elements, new-state);
	table[key] := value;
	next-pair(next-state(keys-and-elements, new-state));
      end unless;
    end method;
  next-pair(initial-state);

  // Return our newly updated table.
  table;
end function fill-table!;


//=========================================================================
//  find-element
//=========================================================================
//  Like find-key, but returns the element.
//
//  XXX - why is this generic?

define open generic find-element
    (collection :: <collection>, function :: <function>,
     #key skip :: <integer> = 0, failure :: <object> = #f)
 => (element :: <object>);

define method find-element
    (collection :: <collection>, function :: <function>,
     #key skip :: <integer> = 0, failure :: <object> = #f)
 => (element :: <object>)
  let key = find-key(collection, function, skip: skip, failure: $unfound);
  if (key = $unfound)
    failure;
  else
    element(collection, key);
  end if;
end method find-element;
