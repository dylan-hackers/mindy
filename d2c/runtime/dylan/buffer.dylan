module: dylan-viscera
author: ram+@cs.cmu.edu
synopsis: <buffer> and <byte-vector>
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/buffer.dylan,v 1.3 1995/12/06 19:58:52 ram Exp $


%%primitive c-include ("string.h");

define /* exported */ constant <byte> =
  limited(<fixed-integer>, min: 0, max: 255);

define /* exported */ class <byte-vector> (<builtin-vector>)
  sealed slot %element :: <byte>,
    init-value: ' ', init-keyword: fill:,
    sizer: size, required-size-init-keyword: size:;
end;

seal generic make (singleton(<byte-vector>));
seal generic initialize (<byte-vector>);

define /* exported */ constant <buffer> = <byte-vector>;
define /* exported */ constant <buffer-index> = <fixed-integer>;
define /* exported */ constant $maximum-buffer-size = $maximum-fixed-integer;


define sealed inline method element
    (vec :: <byte-vector>, index :: <fixed-integer>,
     #key default = $not-supplied)
    => element :: <object>; // because of default:
  if (index >= 0 & index < vec.size)
    %element(vec, index);
  elseif (default == $not-supplied)
    element-error(vec, index);
  else
    default;
  end;
end;

define sealed inline method element-setter
    (new-value :: <byte>, vec :: <byte-vector>, index :: <fixed-integer>)
    => new-value :: <object>;
  if (index >= 0 & index < vec.size)
    %element(vec, index) := new-value;
  else
    element-error(vec, index);
  end;
end;

define constant <byte-like> = type-union(<byte-string>, <byte-vector>);

// Copy bytes from src to dest (which may overlap.)  
define /* exported */ generic copy-bytes 
  (dest :: <byte-like>, dest-start :: <fixed-integer>, 
   src :: <byte-like>, src-start :: <fixed-integer>, src-end :: <fixed-integer>)
 => ();

// These methods are all the same modulo specializers, but are replicated so
// that the vector-elements works.

define method copy-bytes 
    (dest :: <byte-vector>, dstart :: <fixed-integer>,
     src :: <byte-vector>, sstart :: <fixed-integer>, send :: <fixed-integer>)
 => ();
  %%primitive call-out
    ("memmove", void:,
     ptr: %%primitive vector-elements(dest) + dstart,
     ptr: %%primitive vector-elements(src) + sstart,
     int: send - sstart);
end method;

define method copy-bytes 
    (dest :: <byte-string>, dstart :: <fixed-integer>,
     src :: <byte-vector>, sstart :: <fixed-integer>, send :: <fixed-integer>)
 => ();
  %%primitive call-out
    ("memmove", void:,
     ptr: %%primitive vector-elements(dest) + dstart,
     ptr: %%primitive vector-elements(src) + sstart,
     int: send - sstart);
end method;

define method copy-bytes 
    (dest :: <byte-vector>, dstart :: <fixed-integer>,
     src :: <byte-string>, sstart :: <fixed-integer>, send :: <fixed-integer>)
 => ();
  %%primitive call-out
    ("memmove", void:,
     ptr: %%primitive vector-elements(dest) + dstart,
     ptr: %%primitive vector-elements(src) + sstart,
     int: send - sstart);
end method;

define method copy-bytes 
    (dest :: <byte-string>, dstart :: <fixed-integer>,
     src :: <byte-string>, sstart :: <fixed-integer>, send :: <fixed-integer>)
 => ();
  %%primitive call-out
    ("memmove", void:,
     ptr: %%primitive vector-elements(dest) + dstart,
     ptr: %%primitive vector-elements(src) + sstart,
     int: send - sstart);
end method;

define /* exported */ method buffer-address (x :: <buffer>)
 => res :: <raw-pointer>;
  %%primitive vector-elements(x);
end method;
