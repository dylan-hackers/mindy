Module:       common-dylan-internals
Author:       Toby Weinberg, Jonathan Bachrach + Eliot Miranda, Scott McKay
Synopsis:     Native byte vectors
Copyright:    Original Code is Copyright (c) 1999-2000 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/////
///// BYTE-VECTOR
/////

c-system-include("string.h");

/// Fast byte vector copying

define function byte-vector-ref
    (byte-vector :: <byte-vector>, index :: <integer>)
  element(byte-vector, index)
end function byte-vector-ref;

define function byte-vector-ref-setter
    (value, byte-vector :: <byte-vector>, index :: <integer>)
  element(byte-vector, index) := value;
end function byte-vector-ref-setter;


define sealed method byte-vector-fill
    (target :: <byte-vector>, value :: <integer>,
     #key start :: <integer> = 0, end: last :: false-or(<integer>)) => ()
  let target-sz :: <integer> = size(target);
  let last :: <integer>
    = if (last & last < target-sz) last else target-sz end if;
  let start :: <integer> = if (start < 0) 0 else start end if;
  
  call-out("memset", ptr:,
           ptr: vector-elements-address(target) + start,
           int: value,
           int: last - start);
end;

define sealed method byte-vector-fill
    (target :: <byte-vector>, value :: <byte-character>,
     #key start :: <integer> = 0, end: last :: false-or(<integer>)) => ()
  let target-sz :: <integer> = size(target);
  let last :: <integer>
    = if (last & last < target-sz) last else target-sz end if;
  let start :: <integer> = if (start < 0) 0 else start end if;
  call-out("memset", ptr:,
           ptr: vector-elements-address(target) + start,
           int: as(<integer>, value),
           int: last - start);
end method;

//---*** It would sure be nice to have low-level run-time support for this
define open generic copy-bytes (src, src-start, dst, dst-start, n) => ();

define open method copy-bytes
    (src :: <sequence>, src-start :: <integer>, dst :: <sequence>, 
     dst-start :: <integer>, n :: <integer>)
 => ()
  for (i :: <integer> from 0 below n)
    dst[dst-start + i] := src[src-start + i]
  end
end method;

define open method copy-bytes
    (src :: <vector>, src-start :: <integer>, dst :: <vector>, 
     dst-start :: <integer>, n :: <integer>)
 => ()
  for (i :: <integer> from 0 below n)
    dst[dst-start + i] := src[src-start + i]
  end
end method;

define open method copy-bytes
    (src :: <string>, src-start :: <integer>, dst :: <string>,
     dst-start :: <integer>, n :: <integer>)
 => ()
  for (i :: <integer> from 0 below n)
    dst[dst-start + i] := src[src-start + i]
  end
end method;

define open method copy-bytes
    (src :: <vector>, src-start :: <integer>, dst :: <string>,
     dst-start :: <integer>, n :: <integer>)
 => ()
  for (i :: <integer> from 0 below n)
    dst[dst-start + i] := as(<character>, src[src-start + i])
  end
end method;

define open method copy-bytes
    (src :: <string>, src-start :: <integer>, dst :: <vector>, 
     dst-start :: <integer>, n :: <integer>)
 => ()
  for (i :: <integer> from 0 below n)
    dst[dst-start + i] := as(<integer>, src[src-start + i])
  end
end method;

define function copy-bytes-range-error
    (src, src-start :: <integer>, dst, dst-start :: <integer>, n :: <integer>)
 => ()
  error("SRC-START %d DST-START %d AND N %d OUTSIDE OF SRC %= AND DST %=",
	src-start, dst-start, n, src, dst);
end function;

define sealed method copy-bytes
    (src :: <byte-vector>, src-start :: <integer>, dst :: <byte-vector>, 
     dst-start :: <integer>, n :: <integer>) => ()
  let src-end :: <integer> = src-start + n;
  let dst-end :: <integer> = dst-start + n;
  if (n >= 0 & src-start >= 0 & dst-start >= 0 & src-end <= size(src) & dst-end <= size(dst))
    %copy-bytes(dst, dst-start, src, src-start, n);
  else
    copy-bytes-range-error(src, src-start, dst, dst-start, n);
  end if; 
end method;

define sealed method copy-bytes
    (src :: <byte-vector>, src-start :: <integer>, dst :: <byte-string>, 
     dst-start :: <integer>, n :: <integer>) => ()
  let src-end :: <integer> = src-start + n;
  let dst-end :: <integer> = dst-start + n;
  if (n >= 0 & src-start >= 0 & dst-start >= 0 & src-end <= size(src) & dst-end <= size(dst))
  else
    copy-bytes-range-error(src, src-start, dst, dst-start, n);
  end if; 
end method;

define sealed method copy-bytes
    (src :: <byte-string>, src-start :: <integer>, dst :: <byte-vector>, 
     dst-start :: <integer>, n :: <integer>) => ()
  let src-end :: <integer> = src-start + n;
  let dst-end :: <integer> = dst-start + n;
  if (n >= 0 & src-start >= 0 & dst-start >= 0 & src-end <= size(src) & dst-end <= size(dst))
    %copy-bytes(dst, dst-start, src, src-start, n);
  else
    copy-bytes-range-error(src, src-start, dst, dst-start, n);
  end if; 
end method;

define sealed method copy-bytes
    (src :: <byte-string>, src-start :: <integer>, dst :: <byte-string>, 
     dst-start :: <integer>, n :: <integer>) => ()
  let src-end :: <integer> = src-start + n;
  let dst-end :: <integer> = dst-start + n;
  if (n >= 0 & src-start >= 0 & dst-start >= 0 & src-end <= size(src) & dst-end <= size(dst))
    %copy-bytes(dst, dst-start, src, src-start, n);
  else
    copy-bytes-range-error(src, src-start, dst, dst-start, n);
  end if; 
end method;

define sealed method copy-bytes
    (src :: <simple-object-vector>, src-start :: <integer>, 
     dst :: <byte-vector>, dst-start :: <integer>, n :: <integer>) => ()
  let src-end :: <integer> = src-start + n;
  let dst-end :: <integer> = dst-start + n;
  if (n >= 0 & src-start >= 0 & dst-start >= 0 & src-end <= size(src) & dst-end <= size(dst))
    for (src-i :: <integer> from src-start below src-end,
         dst-i :: <integer> from dst-start)
      %element(dst, dst-i) := %element(src, src-i);
    end for;
  else
    copy-bytes-range-error(src, src-start, dst, dst-start, n);
  end if; 
end method;

define sealed method copy-bytes
    (src :: <byte-vector>, src-start :: <integer>, 
     dst :: <simple-object-vector>, dst-start :: <integer>, n :: <integer>)
 => ()
  let src-end :: <integer> = src-start + n;
  let dst-end :: <integer> = dst-start + n;
  if (n >= 0 & src-start >= 0 & dst-start >= 0 & src-end <= size(src) & dst-end <= size(dst))
    for (src-i :: <integer> from src-start below src-end,
         dst-i :: <integer> from dst-start)
      %element(dst, dst-i) := %element(src, src-i);
    end for;
  else
    copy-bytes-range-error(src, src-start, dst, dst-start, n);
  end if; 
end method;

// eof
