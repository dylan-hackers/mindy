rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/stretchy.dylan,v 1.2 1996/01/12 02:10:54 wlott Exp $
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

define sealed method as
    (class == <stretchy-vector>, collection :: <collection>)
    => res :: <stretchy-vector>;
  let res = make(<stretchy-vector>, size: collection.size);
  for (index :: <integer> from 0, element in collection)
    res[index] := element;
  end;
  res;
end;

define inline method as
    (class == <stretchy-vector>, vector :: <stretchy-vector>)
    => res :: <stretchy-vector>;
  vector;
end;

define method map-into (destination :: <stretchy-vector>,
			proc :: <function>, sequence :: <sequence>,
			#next next-method, #rest more-sequences)
    => res :: <stretchy-vector>;
  if (empty?(more-sequences))
    let sz = size(sequence);
    if (sz > size(destination)) size(destination) := sz end if;
    let data = ssv-data(destination);
    for (key from 0, elem in sequence)
      destination[key] := proc(elem);
    end for;
    destination;
  else
    next-method();
  end if;
end method map-into;


// <stretchy-object-vector>

define class <stretchy-object-vector> (<stretchy-vector>)
  //
  // A <simple-object-vector> holding the vector elements.  Obviously
  // at least as long as the stretchy vector.
  slot ssv-data :: <simple-object-vector>,
    required-init-keyword: data:;
  //
  // The current size of the stretchy vector.
  slot ssv-fill :: <integer>,
    required-init-keyword: fill:;
end class <stretchy-object-vector>;
  

define sealed method make (class == <stretchy-object-vector>,
			   #next next-method, #key size = 0, fill)
    => res :: <stretchy-object-vector>;
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
  let data = make(<simple-object-vector>, size: data-size);
  fill!(data, fill, end: data-size);
  next-method(class, fill: size, data: data);
end method make;

define inline method size (ssv :: <stretchy-object-vector>)
    => size :: <integer>;
  ssv-fill(ssv);
end method size;

define method size-setter
    (new :: <integer>, ssv :: <stretchy-object-vector>)
    => new :: <integer>;
  let fill = ssv-fill(ssv);
  let data = ssv-data(ssv);
  if (new > fill)
    let len = size(data);
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
      for (index from 0 below fill)
	new-data[index] := data[index];
      end for;
      ssv-data(ssv) := new-data;
    end if;
    fill!(data, #f, start: fill);
  else
    fill!(data, #f, start: new, end: fill);
  end if;
  ssv-fill(ssv) := new;
end method size-setter;


define method element
    (ssv :: <stretchy-object-vector>, key :: <integer>,
     #key default = $not-supplied)
    => result :: <object>;
  case
    key >= 0 & key < size(ssv) =>
      ssv-data(ssv)[key];
    default == $not-supplied =>
      element-error(ssv, key);
    otherwise =>
      default;
  end case;
end method element;

define method element-setter (value, ssv :: <stretchy-object-vector>,
			      key :: <integer>)
    => value :: <object>;
  if (key < 0)
    element-error(ssv, key);
  else
    if (key >= size(ssv))
      size(ssv) := key + 1;
    end if;
    ssv-data(ssv)[key] := value;
  end if;
end method element-setter;

define method add! (ssv :: <stretchy-object-vector>, new-element)
    => ssv :: <stretchy-object-vector>;
  let data = ssv-data(ssv);
  let fill = size(ssv);
  if (fill = size(data))
    let data-size = if (fill < 1024)
		      fill * 2;
		    else 
		      fill + 1024;
		    end if;
    let new-data = replace-subsequence!(make(<simple-object-vector>,
					     size: data-size),
					data, end: fill);
    ssv-data(ssv) := new-data;
    new-data[fill] := new-element;
  else 
    data[fill] := new-element;
  end if;
  ssv-fill(ssv) := fill + 1;
  ssv;
end method add!;

define method remove! (ssv :: <stretchy-object-vector>, elem,
		       #key test :: false-or(<function>) = \==,
		            count :: false-or(<integer>))
    => ssv :: <stretchy-object-vector>;
  unless (count & (count == 0))
    let data = ssv-data(ssv);
    let sz = size(ssv);
    local
      method copy (src :: <integer>, dst :: <integer>,
		   deleted :: <integer>)
	  => ();
	case
	  src == sz =>
	    ssv-fill(ssv) := sz - deleted;
	  otherwise =>
	    data[dst] := data[src];
	    copy(src + 1, dst + 1, deleted);
	end case;
      end method copy,
      method search-and-copy (src :: <integer>, dst :: <integer>,
			      deleted :: <integer>)
	  => ();
	if (src == sz)
	  ssv-fill(ssv) := sz - deleted;
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
