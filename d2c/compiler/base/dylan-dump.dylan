module: dylan-dump
copyright: see below

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

// This file contains ODF dumping and loading methods for various simple dylan
// values (integers, etc.)  Since most uses of ODF will want to build on these
// values, the loader methods are added to *default-dispatcher*.
//
// Currently supported classes:
//    <list>, <simple-object-vector>, <boolean>, <symbol>,
//    <byte-string>, <byte-character>, <general-integer>, <ratio>
//
// The <list> and <simple-object-vector> dumpers don't recognize sharing or
// circularity.


// Boolean methods:

define method dump-od(obj == #t, buf :: <dump-state>) => ();
  dump-definition-header(#"true", buf);
end method;

add-od-loader(*default-dispatcher*, #"true", 
  method (state :: <load-state>)
   => res :: <boolean>;
    #t;
  end method
);

// <dump-buffer> for extern-handle dumping.  
//
define method dump-od(obj == #f, buf :: <dump-buffer>) => ();
  dump-definition-header(#"false", buf);
end method;

add-od-loader(*default-dispatcher*, #"false", 
  method (state :: <load-state>)
   => res :: <boolean>;
    #f;
  end method
);


// Fixed-integer methods:

define method dump-od(obj :: <integer>, buf :: <dump-state>)
 => ();
  dump-definition-header(#"fixed-integer", buf, 
  			 raw-data: $odf-word-raw-data-format);
  dump-word(1, buf);
  dump-word(obj, buf);
end method;

define constant $e1 = as(<extended-integer>, 1);


// Sign-extend a word-integer.
//
define method sign-extend (val :: <general-integer>)
    => res :: <general-integer>;
  let val = as(<extended-integer>, val);
  if (logand(val, ash($e1, $word-bits - 1)) = 0)
    val;
  else
    logior(ash(-$e1, $word-bits), val);
  end;
end method;


// Load a signed fixed integer.
//
add-od-loader(*default-dispatcher*, #"fixed-integer", 
  method (state :: <load-state>)
   => res :: <integer>;
    state.od-next := state.od-next + $word-bytes; // skip count word
    let res =
      as(<integer>,
         sign-extend(buffer-word(state.od-buffer,
       			         fill-at-least($word-bytes, state))));
    state.od-next := state.od-next + $word-bytes; // skip data word
    res;
  end method
);


// Extended integer methods:

define constant $word-mask = lognot(ash(-$e1, $word-bits));

define method dump-od(obj :: <extended-integer>, buf :: <dump-state>)
 => ();
  dump-definition-header(#"extended-integer", buf, 
  			 raw-data: $odf-word-raw-data-format);
  let len = ceiling/(integer-length(obj) + 1, $word-bits);
  dump-word(len, buf);
  for (i :: <integer> from 0 below len,
       x :: <extended-integer> = obj then ash(x, - $word-bits))
    let word = logand(x, $word-mask);
#if (~mindy)
    let word :: <integer>
      = if (word <= $maximum-integer)
	  as(<integer>, word);
	else
	  as(<integer>, logior(word, ash(-$e1, $word-bits)));
	end if;
#endif
    dump-word(word, buf);
  end;
end method;

add-od-loader(*default-dispatcher*, #"extended-integer", 
  method (state :: <load-state>)
   => res :: <extended-integer>;
    let buffer = state.od-buffer;
    let next = state.od-next;
    let len = buffer-word(buffer, next); // count word
    let bsize = $word-bytes * len;
    state.od-next := next + $word-bytes;

    let next = fill-at-least(bsize, state);
    let res
      = sign-extend(buffer-word(buffer, next + ((len - 1) * $word-bytes)));

    for (i from len - 2 to 0 by -1)
      res := logior(logand(buffer-word(buffer, (i * $word-bytes) + next),
			   $word-mask),
		    ash(res, $word-bits));
    end for;

    state.od-next := next + bsize;
    res;
  end method
);


// Ratio methods:

define method dump-od(obj :: <ratio>, buf :: <dump-state>) => ();
  let start-posn = buf.current-pos;
  dump-definition-header(#"ratio", buf, subobjects: #t);
  dump-od(obj.numerator, buf);
  dump-od(obj.denominator, buf);
  dump-end-entry(start-posn, buf);
end method;

add-od-loader(*default-dispatcher*, #"ratio", 
  method (state :: <load-state>) => res :: <ratio>;
    
    let npart = load-object-dispatch(state);
    let dpart = load-object-dispatch(state);
    assert-end-object(state);
    ratio(npart, dpart);
  end method
);


// Real inefficient for large exponents, but probably correct.
//
define method integer-decode-float
    (float :: <float>, precision :: <integer>)
 => (frac :: <extended-integer>, exp :: <integer>);
  if (zero?(float))
    values(as(<extended-integer>, 0), 0);
  else
    let fclass = object-class(float);
    let lim = as(fclass, ash(1, precision));
    let two = as(fclass, 2);
    let exponent = 0;
    let current = float;
    if (current >= lim)
      while (current >= lim)
	current := current / two;
	exponent := exponent + 1;
      end;
    else
      while (current < lim)
	current := current * two;
	exponent := exponent - 1;
      end;
      current := current / two;
      exponent := exponent + 1;
    end if;
    values(as(<extended-integer>, truncate(current)), exponent);
  end if;
end method;

    
// Float methods:

define method dump-od(obj :: <single-float>, buf :: <dump-buffer>) => ();
  let (frac, exp) = integer-decode-float(obj, 23);
  dump-simple-object(#"single-float", buf, frac, exp);
end method;

add-od-loader(*default-dispatcher*, #"single-float",
  method (state :: <load-state>) => res :: <single-float>;
    let frac :: <extended-integer> = load-object-dispatch(state);
    let exp :: <integer> = load-object-dispatch(state);
    assert-end-object(state);
    as(<single-float>, frac) * 2.0s0 ^ exp;
  end method
);


// Byte-string methods:

// <dump-buffer> for extern-handle dumping.  
// ### Too bad about EQness.
// 
define method dump-od(obj :: <byte-string>, buf :: <dump-buffer>) => ();
  dump-definition-header(#"byte-string", buf,
  			 raw-data: $odf-byte-raw-data-format);
  dump-raw-data(obj, obj.size, buf);
end method;

add-od-loader(*default-dispatcher*, #"byte-string", 
  method (state :: <load-state>)
   => res :: <byte-string>;
    let next = state.od-next;
    let bsize = buffer-word(state.od-buffer, next);
    state.od-next := next + $word-bytes;
    let res = make(<byte-string>, size: bsize);
    load-raw-data(res, bsize, state);
    res;
  end method
);


// Symbol methods:

// This method works on <dump-buffer> because we need it for extern-handle
// dumping.  This is o.k., because symbol semantics preserve EQ'ness.
//
define method dump-od(obj :: <symbol>, buf :: <dump-buffer>) => ();
  dump-definition-header(#"byte-symbol", buf,
  			 raw-data: $odf-byte-raw-data-format);
  let lval = as(<byte-string>, obj);
  dump-raw-data(lval, lval.size, buf);
end method;

add-od-loader(*default-dispatcher*, #"byte-symbol", 
  method (state :: <load-state>) => res :: <symbol>;
    let next = state.od-next;
    let bsize = buffer-word(state.od-buffer, next);
    state.od-next := state.od-next + $word-bytes;
    let res = make(<byte-string>, size: bsize);
    load-raw-data(res, bsize, state);
    as(<symbol>, res);
  end method
);


// Character methods:
//
// We load/dump like a one-char string so we can reuse the mechanism.

define method dump-od(obj :: <byte-character>, buf :: <dump-state>)
 => ();
  dump-definition-header(#"byte-character", buf,
  			 raw-data: $odf-byte-raw-data-format);
  let lval = make(<byte-string>, size: 1, fill: obj);
  dump-raw-data(lval, 1, buf);
end method;

add-od-loader(*default-dispatcher*, #"byte-character", 
  method (state :: <load-state>) => res :: <byte-character>;
    let res = make(<byte-string>, size: 1);
    state.od-next := state.od-next + $word-bytes; // skip count
    load-raw-data(res, 1, state);
    res[0];
  end method
);


// List methods:

// To dump a list, figure out if it is improper (so we know whether to use list
// or list*), then recurse on elements.
//
define method dump-od(obj :: <list>, buf :: <dump-state>) => ();
  let improper
    = for (el = obj then el.tail, 
    	   while: instance?(el, <pair>))
	finally if (el == #()) #f else el end;
      end for;
 
  let start-pos = buf.current-pos;
  dump-definition-header(if (improper) #"list*" else #"list" end,
  		 	 buf, subobjects: #t);
  
  for (el = obj then el.tail, while: instance?(el, <pair>))
    dump-od(el.head, buf);
  end;

  if (improper)
    dump-od(improper, buf);
  end;
  
  dump-end-entry(start-pos, buf);

end method;


// Look at the head of a pair, and if it is a forward reference, request
// backpatching.  Also, if the forward reference has already been resolved, we
// eagerly backpatch.
// 
define method head-maybe-backpatch (pair :: <pair>) => ();
  let x = pair.head;
  if (x.obj-resolved?)
    pair.head := x.actual-obj;
  else
    request-backpatch(x, method (actual) pair.head := actual end);
  end;
end method;


// To load a list, get the subobjects and convert to a list, possibly
// backpatching.
//
add-od-loader(*default-dispatcher*, #"list", 
  method (state :: <load-state>) => res :: <list>;
    let contents = load-subobjects-vector(state, #f);
    let res = #();
    for (i from contents.size - 1 to 0 by -1)
      res := pair(contents[i], res);
      head-maybe-backpatch(res);
      finally res;
    end;
  end method
);


// To load an improper list, get subobjects and build list in reverse order,
// starting with tail.
//
add-od-loader(*default-dispatcher*, #"list*", 
  method (state :: <load-state>) => res :: <list>;
    let contents = load-subobjects-vector(state, #f);
    let last-idx = contents.size - 1;
    let last-pair = pair(contents[last-idx - 1], contents[last-idx]);
    head-maybe-backpatch(last-pair);
    let x = last-pair.tail;
    if (x.obj-resolved?)
      last-pair.tail := x.actual-obj;
    else
      request-backpatch(x, method (actual) last-pair.tail := actual end);
    end;
    
    for (i from last-idx - 2 to 0 by -1, 
         res = last-pair then pair(contents[i], res))
      head-maybe-backpatch(res);
      finally res;
    end;
  end method
);



// Vector methods:

// To dump a vector, we just recursively dump the entire contents.
//
define method dump-od
  (obj :: <simple-object-vector>, buf :: <dump-state>)
 => ();
  let start-pos = buf.current-pos;
  dump-definition-header(#"simple-object-vector", buf, subobjects: #t);
  for (el in obj)
    dump-od(el, buf);
  end;
  dump-end-entry(start-pos, buf);
end method;


// Get subobjects and convert to a simple object vector.
//
add-od-loader(*default-dispatcher*, #"simple-object-vector", 
  method (state :: <load-state>) => res :: <simple-object-vector>;
    let contents = load-subobjects-vector(state, #f);
    let rsize = contents.size;
    let res = make(<simple-object-vector>, size: rsize);
    for (i from 0 below rsize)
      let el = contents[i];
      if (el.obj-resolved?)
        res[i] := el.actual-obj;
      else
        res[i] := el;
	request-backpatch(el, method (actual) res[i] := actual end);
      end;
    end;
    res;
  end method
);
