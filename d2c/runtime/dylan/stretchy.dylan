rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/stretchy.dylan,v 1.4 1996/03/20 01:44:03 rgs Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

//  This file implements stretchy-vectors.


// <stretchy-vector>

define open abstract primary class <stretchy-vector>
    (<vector>, <stretchy-collection>)
end class <stretchy-vector>;

define sealed inline method make
    (class == <stretchy-vector>, #key size = 0, fill)
    => res :: <stretchy-object-vector>;
  make(<stretchy-object-vector>, size: size, fill: fill);
end method;


// <stretchy-object-vector>

// Invariants:
//   1. ssv-current-size <= ssv-data.size
//   2. elements [ssv-current-size..ssv-data.size] are always set to #f.  
//   3. Elements [0..ssv-current-size] contain user supplied data.
//
define class <stretchy-object-vector> (<stretchy-vector>)
  //
  // A <simple-object-vector> holding the vector elements.  Obviously
  // at least as long as the stretchy vector.
  slot ssv-data :: <simple-object-vector> = #[];
  //
  // The current size of the stretchy vector.
  slot ssv-current-size :: <integer> = 0;
end class <stretchy-object-vector>;
define sealed domain make(singleton(<stretchy-object-vector>));

define sealed method initialize
    (object :: <stretchy-object-vector>, #key size :: <integer> = 0, fill)
 => ();
  let data-size = case
		    size < 0 =>
		      error("size: can't be negative.");
		    size < 16 => 16;
		    size < 1024 =>
		      for (data-size = 16 then data-size * 2,
			   until: size < data-size)
		      finally data-size;
		      end for;
		    otherwise =>
		      ceiling/(size + 1024, 1024) * 1024;
		  end case;
  // The "fill:" keyword assures that elements above ssv-current-size
  // will be #f...
  let data = make(<simple-object-vector>, size: data-size, fill: #f);
  // ...and then we manually fill the other elements if necessary.
  if (fill) fill!(data, fill, end: size) end if;
  object.ssv-data := data;
  object.ssv-current-size := size;
end method initialize;

define sealed inline method size (ssv :: <stretchy-object-vector>)
    => size :: <integer>;
  ssv.ssv-current-size;
end method size;

define method size-setter
    (new :: <integer>, ssv :: <stretchy-object-vector>)
    => new :: <integer>;
  let current = ssv.ssv-current-size;
  let data = ssv.ssv-data;
  if (new > current)
    let len = data.size;
    if (new > len)
      let new-len = if (new < 1024)
		      for (new-len = 16 then new-len * 2,
			   until: new < new-len)
		      finally new-len;
		      end for;
		    else 
		      ceiling/(new + 1024, 1024) * 1024;
		    end if;
      let new-data = make(<simple-object-vector>, size: new-len);
      for (index :: <integer> from 0 below current)
	new-data[index] := data[index];
      end for;
      ssv.ssv-data := fill!(new-data, #f, start: current);
    end if;
  else
    fill!(data, #f, start: new, end: current);
  end if;
  ssv.ssv-current-size := new;
end method size-setter;


define sealed inline method element
    (ssv :: <stretchy-object-vector>, key :: <integer>,
     #key default = $not-supplied)
    => result :: <object>;
  case
    (key >= 0 & key < ssv.size) =>
      // Warning: unchecked reference.  However, if we're *sure* we
      // satisfy the invariants, we're be safe.
      %element(ssv.ssv-data, key);
    (default == $not-supplied) =>
      element-error(ssv, key);
    otherwise =>
      default;
  end case;
end method element;

define sealed inline method element-setter
    (value, ssv :: <stretchy-object-vector>, key :: <integer>)
    => value :: <object>;
  if (key < 0)
    element-error(ssv, key);
  else
    if (key >= ssv.size)
      ssv.size := key + 1;
    end if;
    // Warning: unchecked reference.  However, if we're *sure* we
    // satisfy the invariants, we're be safe.
    %element(ssv.ssv-data, key) := value;
  end if;
end method element-setter;

// This method is identical to the one in "array.dylan", except that it
// is more tightly specialized to a single sealed class.  If you need to 
// make a general change, you should probably grep for "outlined-iterator" 
// and change all matching locations.
//
define inline method forward-iteration-protocol
    (array :: <stretchy-object-vector>)
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
	 method (array :: <stretchy-object-vector>, state :: <integer>)
	     => new-state :: <integer>;
	   state + 1;
	 end,
	 method (array :: <stretchy-object-vector>, state :: <integer>,
		 limit :: <integer>)
	     => done? :: <boolean>;
	   state == limit;
	 end,
	 method (array :: <stretchy-object-vector>, state :: <integer>)
	     => key :: <integer>;
	   state;
	 end,
	 method (array :: <stretchy-object-vector>, state :: <integer>)
	     => element :: <object>;
	   element(array, state);
	 end,
	 method (new-value :: <object>, array :: <stretchy-object-vector>,
		 state :: <integer>)
	     => new-value :: <object>;
	   element(array, state) := new-value;
	 end,
	 method (array :: <stretchy-object-vector>, state :: <integer>)
	     => state-copy :: <integer>;
	   state;
	 end);
end;

define method add! (ssv :: <stretchy-object-vector>, new-element)
    => ssv :: <stretchy-object-vector>;
  let data = ssv.ssv-data;
  let current = ssv.size;
  if (current == data.size)
    let data-size = if (current < 1024)
		      current * 2;
		    else 
		      current + 1024;
		    end if;
    let new-data = replace-subsequence!(make(<simple-object-vector>,
					     size: data-size),
					data, end: current);
    ssv.ssv-data := new-data;
    new-data[current] := new-element;
  else 
    data[current] := new-element;
  end if;
  ssv.ssv-current-size := current + 1;
  ssv;
end method add!;

define method remove! (ssv :: <stretchy-object-vector>, elem,
		       #key test :: false-or(<function>) = \==,
		            count :: false-or(<integer>))
    => ssv :: <stretchy-object-vector>;
  unless (count & (count == 0))
    let data = ssv.ssv-data;
    let sz = ssv.size;
    local
      method copy (src :: <integer>, dst :: <integer>,
		   deleted :: <integer>)
	  => ();
	case
	  src == sz =>
	    ssv.ssv-current-size := sz - deleted;
	  otherwise =>
	    data[dst] := data[src];
	    copy(src + 1, dst + 1, deleted);
	end case;
      end method copy,
      method search-and-copy (src :: <integer>, dst :: <integer>,
			      deleted :: <integer>)
	  => ();
	if (src == sz)
	  ssv.ssv-current-size := sz - deleted;
	else 
	  let this-element = data[src];
	  case
	    test(this-element, elem) =>
	      let deleted = deleted + 1;
	      if (count & (deleted == count))
		copy(src + 1, dst, deleted);
	      else
		search-and-copy(src + 1, dst, deleted);
	      end if;
	    otherwise =>
	      data[dst] := data[src];
	      search-and-copy(src + 1, dst + 1, deleted);
	  end case;
	end if;
      end method search-and-copy,
      method search (src :: <integer>) => ();
	unless (src == sz)
	  let this-element = data[src];
	  if (test(this-element, elem))
	    if (count & (count == 1))
	      copy(src + 1, src, 1);
	    else 
	      search-and-copy(src + 1, src, 1);
	    end if;
	  else
	    search(src + 1);
	  end if;
	end unless;
      end method search;
    search(0);
  end unless;
  ssv;
end method remove!;

define sealed method as
    (class == <stretchy-vector>, collection :: <collection>)
 => (res :: <stretchy-object-vector>);
  let res = make(<stretchy-object-vector>, size: collection.size);
  for (index :: <integer> from 0, element in collection)
    res[index] := element;
  end;
  res;
end;

define inline method as
    (class == <stretchy-vector>, vector :: <stretchy-vector>)
 => (res :: <stretchy-vector>);
  vector;
end;

define method map-into (destination :: <stretchy-object-vector>,
			proc :: <function>, sequence :: <sequence>,
			#next next-method, #rest more-sequences)
    => res :: <stretchy-vector>;
  if (empty?(more-sequences))
    let sz = size(sequence);
    if (sz == #f)
      error("Cannot map unbounded sequences into stretchy-vectors.");
    elseif (sz > size(destination))
      size(destination) := sz
    end if;
    let data = ssv-data(destination);
    for (key :: <integer> from 0, elem in sequence)
      destination[key] := proc(elem);
    end for;
    destination;
  else
    next-method();
  end if;
end method map-into;

