module: dylan

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
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
//  This file implements stretchy-vectors.
//


//// <stretchy-vector>

define class <stretchy-vector> (<stretchy-collection>, <vector>)
  //
  // No slots in the abstract class <stretchy-vector>
end class <stretchy-vector>;

define method make
    (cls == <stretchy-vector>, #rest keys, #key, #all-keys)
 => vec :: <simple-stretchy-vector>;
  apply(make, <simple-stretchy-vector>, keys);
end method;



//// <simple-stretchy-vector>

define class <simple-stretchy-vector> (<stretchy-vector>)
  slot ssv-data :: <simple-object-vector>, required-init-keyword: data:;
  slot ssv-fill :: <integer>, required-init-keyword: fill:;
end class <simple-stretchy-vector>;
  

define method make(cls == <simple-stretchy-vector>,
		   #next next-method,
		   #key size: sz = #f, fill, dimensions)
 => vec :: <simple-stretchy-vector>;
  if (sz & dimensions)
    error("Can't supply both a size: and dimensions:");
  else
    let size = case
		 sz => sz;
		 ~dimensions => 0;
		 size(dimensions) = 1 =>
		   first(dimensions);
		 otherwise =>
		   error("Vectors can only have one dimension.");
	       end case;
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
    fill!(data, fill, start: 0, end: size);
    next-method(cls, fill: size, data: data);
  end if;
end method make;

define method size(ssv :: <simple-stretchy-vector>) => size :: <integer>;
  ssv-fill(ssv);
end method size;

define method size-setter(new :: <integer>, ssv :: <simple-stretchy-vector>)
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
      let new-data = make(<simple-object-vector>, size: new-len, fill: #f);
      for (index from 0 below fill)
	new-data[index] := data[index];
      end for;
      ssv-data(ssv) := new-data;
    end if;
  else
    fill!(data, #f, start: new, end: fill);
  end if;
  ssv-fill(ssv) := new;
end method size-setter;

define method dimensions(ssv :: <simple-stretchy-vector>) 
 => dimensions :: <list>;
  list(size(ssv));
end method dimensions;


define method element(ssv :: <simple-stretchy-vector>, key :: <integer>,
		      #key default = $not-supplied)
 => elt :: <object>;
  case
    key >= 0 & key < size(ssv) =>
      ssv-data(ssv)[key];
    default == $not-supplied =>
      error("Element %d not in %=", key, ssv);
    otherwise =>
      default;
  end case;
end method element;

define method element-setter(value, ssv :: <simple-stretchy-vector>,
			     key :: <integer>)
 => value :: <object>;
  if (key < 0)
    error("Element %d not in %=", key, ssv);
  else
    if (key >= size(ssv))
      size(ssv) := key + 1;
    end if;
    ssv-data(ssv)[key] := value;
  end if;
end method element-setter;

define method add!(ssv :: <simple-stretchy-vector>, new-element)
 => ssv :: <simple-stretchy-vector>;
  let data = ssv-data(ssv);
  let fill = size(ssv);
  if (fill = size(data))
    let data-size = if (fill < 1024)
		      fill * 2;
		    else 
		      fill + 1024;
		    end if;
    let new-data = make(<simple-object-vector>, size: data-size, fill: #f);
    for (i from 0 below fill) new-data[i] := data[i] end for;
    ssv-data(ssv) := new-data;
    new-data[fill] := new-element;
  else 
    data[fill] := new-element;
  end if;
  ssv-fill(ssv) := fill + 1;
  ssv;
end method add!;

define method remove!(ssv :: <simple-stretchy-vector>, elem,
		      #key test = \==, count)
 => ssv :: <simple-stretchy-vector>;
  unless (count & (count = 0))
    let data = ssv-data(ssv);
    let sz = size(ssv);
    local
      method copy(src, dst, deleted)
	case
	  src = sz =>
	    ssv-fill(ssv) := sz - deleted;
	    fill!(data, #f, start: dst, end: sz);
	  otherwise =>
	    data[dst] := data[src];
	    copy(src + 1, dst + 1, deleted);
	end case;
      end method copy,
      method search-and-copy(src, dst, deleted)
	if (src = sz)
	  ssv-fill(ssv) := dst;
	  fill!(data, #f, start: dst, end: sz);
	else 
	  let this-element = data[src];
	  case
	    test(this-element, elem) =>
	      let deleted = deleted + 1;
	      if (count & (deleted = count))
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
      method search(src)
	unless (src = sz)
	  let this-element = data[src];
	  if (test(this-element, elem))
	    if (count & (count = 1))
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

define method map-into(destination :: <stretchy-vector>,
		       proc :: <function>, sequence :: <sequence>,
		       #next next_method, #rest more_sequences)
  if (empty?(more_sequences))
    let sz = size(sequence);
    if (sz > size(destination)) size(destination) := sz end if;
    let data = ssv-data(destination);
    for (key from 0, elem in sequence)
      destination[key] := proc(elem);
    end for;
    destination;
  else
    next_method();
  end if;
end method map-into;
