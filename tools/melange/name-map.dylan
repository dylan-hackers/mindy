documented: #t
module: name-mappers
copyright: see below
	   This code was produced by the Gwydion Project at Carnegie Mellon
	   University.  If you are interested in using this code, contact
	   "Scott.Fahlman@cs.cmu.edu" (Internet).

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

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
// All rights reserved.
//
//======================================================================

//======================================================================
// name-map.dylan defines the "standard" Creole/Melange name mapper functions
// and provides hooks for users to implement more.
//
// All of the "standard" map functions except identity-name-mapping have been
// extended to map non-initial underlines into hyphens.  This seems to be a
// logical transformation from typical UNIX/C coding style into typical Dylan
// coding style.
//
// Extensions may be added by any package which exports this one.  They simply
// create an appropriate mapper function and add it to "name-mapper-table",
// keyed by a symbol which "names" the function.  The standard method
// "hyphenate-case-breaks" is provided for use in new mapping functions.
//======================================================================

define generic map-name
    (selector :: <symbol>,
     category :: <symbol>, prefix :: <string>, name :: <string>,
     sequence-of-classes :: <sequence>)
 => (result :: <string>);

//----------------------------------------------------------------------
// Utility functions
//----------------------------------------------------------------------

// Adds all of the elements of "seq" to the end of the <stretchy-vector>
// "buffer".  Something like this would have been handy in the definition of
// <stretchy-vector> but it isn't hard to define it ourselves.
//
define method append! (buffer :: <stretchy-vector>, seq :: <sequence>)
 => (buffer :: <stretchy-vector>);
  for (elem in seq)
    add!(buffer, elem);
  end for;
  buffer;
end method append!;

// This is ugly, but should be correct.  It could be done more cleanly via
// regexps, but it seems to be on the critical path for efficiency.
//
define constant hyphenate-case-breaks =
  method (string :: <string>) => (result :: <string>);
    if (string.empty?)
      string;
    else 
      let buffer = make(<stretchy-vector>);
      let old-status = #"neither";
      for (char in string,
	   old-char = #f then char)
	let status = if (~alpha?(char))
		       #"neither";
		     elseif (~uppercase?(char))
		       #"lower";
		     else
		       #"upper";
		     end if;
	if (~old-char)
	  old-status := #"neither";
	elseif (status == #"upper" & old-status == #"lower")
	  add!(buffer, old-char);
	  add!(buffer, '-');
	  old-status := #"neither";
	elseif (status == #"lower" & old-status == #"upper")
	  if (alphanumeric?(buffer.last)) add!(buffer, '-') end if;
	  add!(buffer, old-char);
	  old-status := status;
	else
	  add!(buffer, old-char);
	  old-status := status;
	end if;
      finally
	add!(buffer, old-char);
      end for;
      as(<byte-string>, buffer);
    end if;
  end method;

//----------------------------------------------------------------------
// "Standard" methods for map-name
//----------------------------------------------------------------------

define method map-name
    (selector == #"minimal-name-mapping-with-structure-prefix",
     category :: <symbol>, prefix :: <string>, name :: <string>,
     sequence-of-classes :: <sequence>)
 => (result :: <string>);
  let buffer = make(<stretchy-vector>);

  if (category == #"type")
    add!(buffer, '<') 
  elseif (category == #"constant")
    add!(buffer, '$');
  end if;

  append!(buffer, prefix);
  for (cls in sequence-of-classes)
    append!(buffer, cls);
    add!(buffer, '$');
  end for;

  for (non-underline = #f then non-underline | char ~= '_',
       char in name)
    add!(buffer, if (non-underline & char == '_') '-' else char end if);
  end for;

  if (category == #"type") add!(buffer, '>') end if;
  as(<byte-string>, buffer);
end method map-name;

define method map-name
    (selector == #"minimal-name-mapping",
     category :: <symbol>, prefix :: <string>, name :: <string>,
     sequence-of-classes :: <sequence>)
 => (result :: <string>);
  let buffer = make(<stretchy-vector>);

  if (category == #"type")
    add!(buffer, '<') 
  elseif (category == #"constant")
    add!(buffer, '$');
  end if;

  append!(buffer, prefix);

  for (non-underline = #f then non-underline | char ~= '_',
       char in name)
    add!(buffer, if (non-underline & char == '_') '-' else char end if);
  end for;

  if (category == #"type") add!(buffer, '>') end if;
  as(<byte-string>, buffer);
end method map-name;

define method map-name
    (selector == #"c-to-dylan",
     category :: <symbol>, prefix :: <string>, name :: <string>,
     sequence-of-classes :: <sequence>)
 => (result :: <string>);
  let buffer = make(<stretchy-vector>);

  if (category == #"type")
    add!(buffer, '<') 
  elseif (category == #"constant")
    add!(buffer, '$');
  end if;

  append!(buffer, prefix);
  if (~empty?(sequence-of-classes) & category == #"variable")
    // We have a slot getter name.
    append!(buffer, "get-");
  end if;

  for (non-underline = #f then non-underline | char ~= '_',
       char in hyphenate-case-breaks(name))
    add!(buffer, if (non-underline & char == '_') '-' else char end if);
  end for;

  if (category == #"type") add!(buffer, '>') end if;
  as(<byte-string>, buffer);
end method map-name;

define method map-name
    (selector == #"identity-name-mapping",
     category :: <symbol>, prefix :: <string>, name :: <string>,
     sequence-of-classes :: <sequence>)
 => (result :: <string>);
  name;
end method map-name;

//----------------------------------------------------------------------
// Fun-O c-ffi compatible methods for map-name
//----------------------------------------------------------------------

define method map-name
    (selector == #"c-ffi",
     category :: <symbol>, prefix :: <string>, name :: <string>,
     sequence-of-classes :: <sequence>)
 => (result :: <string>)
   let buffer = make(<stretchy-vector>);

  if (category == #"type")
    add!(buffer, '<') 
  elseif (category == #"constant")
    add!(buffer, '$');
  end if;

  for (non-underline = #f then non-underline | char ~= '_',
       char in hyphenate-case-breaks(name))
    add!(buffer, if (non-underline & char == '_') '-' else char end if);
  end for;

  append!(buffer, prefix);
  if (~empty?(sequence-of-classes) & category == #"variable")
    // We have a slot getter name.
    append!(buffer, "-value");
  end if;

  if (category == #"type") append!(buffer, "*>") end if;
  as(<byte-string>, buffer);
end method map-name;
