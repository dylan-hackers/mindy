module: dylan


// <array> and generics

define open abstract class <array> (<mutable-sequence>)
end;

define open generic dimensions (array :: <array>) => dims :: <sequence>;

define open generic rank (array :: <array>) => rank :: <integer>;

define open generic row-major-index
    (array :: <array>, #rest subscripts) => index :: <integer>;

define open generic aref (array :: <array>, #rest indices)
    => element :: <object>;

define open generic aref-setter
    (new-value :: <object>, array :: <array>, #rest indices)
    => new-value :: <object>;

define open generic dimension (array :: <array>, axis :: <integer>)
    => dimension :: <integer>;


// Default methods.

define inline method forward-iteration-protocol (array :: <array>)
    => (initial-state :: <integer>,
	limit :: <integer>,
	next-state :: <function>,
	finished-state? :: <function>,
	current-key :: <function>,
	current-element :: <function>,
	current-element-setter :: <function>,
	copy-state :: <function>);
  values(0,
	 array.size,
	 method (array :: <array>, state :: <integer>)
	   state + 1;
	 end,
	 method (array :: <array>, state :: <integer>, limit :: <integer>)
	   state == limit;
	 end,
	 method (array :: <array>, state :: <integer>)
	   state;
	 end,
	 method (array :: <array>, state :: <integer>)
	   element(array, state);
	 end,
	 method (new-value :: <object>, array :: <array>, state :: <integer>)
	   element(array, state) := new-value;
	 end,
	 method (array :: <array>, state :: <integer>)
	   state;
	 end);
end;

define inline method backward-iteration-protocol (array :: <array>)
    => (initial-state :: <integer>,
	limit :: <integer>,
	next-state :: <function>,
	finished-state? :: <function>,
	current-key :: <function>,
	current-element :: <function>,
	current-element-setter :: <function>,
	copy-state :: <function>);
  values(array.size - 1,
	 -1,
	 method (array :: <array>, state :: <integer>)
	   state - 1;
	 end,
	 method (array :: <array>, state :: <integer>, limit :: <integer>)
	   state == limit;
	 end,
	 method (array :: <array>, state :: <integer>)
	   state;
	 end,
	 method (array :: <array>, state :: <integer>)
	   element(array, state);
	 end,
	 method (new-value :: <object>, array :: <array>, state :: <integer>)
	   element(array, state) := new-value;
	 end,
	 method (array :: <array>, state :: <integer>)
	   state;
	 end);
end;

define inline method rank (array :: <array>) => rank :: <integer>;
  array.dimensions.size;
end;

define method row-major-index (array :: <array>, #rest indices)
    => index :: <integer>;
  let dims = dimensions(array);
  if (size(indices) ~= size(dims))
    error("Number of indices not equal to rank. Got %=, wanted %d indices",
	  indices, size(dims));
  else
    for (index in indices,
	 dim   in dims,
	 sum = 0 then (sum * dim) + index)
      if (index < 0 | index >= dim)
	error("Array index out of bounds: %= in %=", index, indices);
      end if;
    finally
      sum;
    end for;
  end if;
end;

define inline method aref (array :: <array>, #rest indices)
    => element :: <object>;
  element(array, apply(row-major-index, array, indices));
end;

define inline method aref-setter
    (new-value :: <object>, array :: <array>, #rest indices)
    => new-value :: <object>;
  element(array, apply(row-major-index, array, indices)) := new-value;
end;

define inline method dimension (array :: <array>, axis :: <integer>)
    => dimension :: <integer>;
  array.dimensions[axis];
end;


// Support for builtin arrays.

define open abstract class <builtin-array> (<array>)
end;

define inline sealed method forward-iteration-protocol
    (array :: <builtin-array>)
    => (initial-state :: <fixed-integer>,
	limit :: <fixed-integer>,
	next-state :: <function>,
	finished-state? :: <function>,
	current-key :: <function>,
	current-element :: <function>,
	current-element-setter :: <function>,
	copy-state :: <function>);
  values(0,
	 array.size,
	 method (array :: <builtin-array>, state :: <fixed-integer>)
	   state + 1;
	 end,
	 method (array :: <builtin-array>, state :: <fixed-integer>,
		 limit :: <fixed-integer>)
	   state == limit;
	 end,
	 method (array :: <builtin-array>, state :: <fixed-integer>)
	   state;
	 end,
	 method (array :: <builtin-array>, state :: <fixed-integer>)
	   element(array, state);
	 end,
	 method (new-value :: <object>, array :: <builtin-array>,
		 state :: <fixed-integer>)
	   element(array, state) := new-value;
	 end,
	 method (array :: <builtin-array>, state :: <fixed-integer>)
	   state;
	 end);
end;

define inline sealed method backward-iteration-protocol
    (array :: <builtin-array>)
    => (initial-state :: <object>,
	limit :: <object>,
	next-state :: <function>,
	finished-state? :: <function>,
	current-key :: <function>,
	current-element :: <function>,
	current-element-setter :: <function>,
	copy-state :: <function>);
  values(array.size - 1,
	 -1,
	 method (array :: <builtin-array>, state :: <fixed-integer>)
	   state - 1;
	 end,
	 method (array :: <builtin-array>, state :: <fixed-integer>,
		 limit :: <fixed-integer>)
	   state == limit;
	 end,
	 method (array :: <builtin-array>, state :: <fixed-integer>)
	   state;
	 end,
	 method (array :: <builtin-array>, state :: <fixed-integer>)
	   element(array, state);
	 end,
	 method (new-value :: <object>, array :: <builtin-array>,
		 state :: <fixed-integer>)
	   element(array, state) := new-value;
	 end,
	 method (array :: <builtin-array>, state :: <fixed-integer>)
	   state;
	 end);
end;

seal generic reduce (<function>, <object>, <builtin-array>);

seal generic member? (<object>, <builtin-array>);
