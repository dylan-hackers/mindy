module: transformers
rcs-header: $Header: /scm/cvs/src/d2c/compiler/base/transdef.dylan,v 1.1 1998/05/03 19:55:31 andreas Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
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
//    University.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports, questions, comments, and suggestions should be sent by
// E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
//
//======================================================================

define class <transformer> (<object>)
  //
  // The name of the function this transformer is for.  For printing purposes
  // only.
  slot transformer-name :: <symbol>, required-init-keyword: name:;
  //
  // The type specifiers for the specializers, #"gf" if applicable only to the
  // generic function, or #f if unrestricted.
  slot transformer-specializer-specifiers
    :: type-union(<list>, one-of(#f, #"gf")),
    required-init-keyword: specializers:;
  //
  // The ctypes for the specialiers, #"gf" if applicable only to the generic
  // function, #f if unrestricted, or #"not-computed-yet"
  // if we haven't computed it yet from the specifiers.
  slot %transformer-specializers
    :: type-union(<list>, one-of(#f, #"gf", #"not-computed-yet")),
    init-value: #"not-computed-yet";
  //
  // The actual transformer function.  Takes the component and the call
  // operation, returns a boolean indicating whether or not it did anything.
  slot transformer-function :: <function>,
    required-init-keyword: function:;
end;

define method print-object (trans :: <transformer>, stream :: <stream>) => ();
  pprint-fields(trans, stream, name: trans.transformer-name);
end;


define method define-transformer
    (name :: <symbol>, specializers :: type-union(<list>, one-of(#f, #"gf")),
     function :: <function>)
    => ();
  let trans = make(<transformer>, name: name, specializers: specializers,
		   function: function);
  let var = dylan-var(name, create: #t);
  var.variable-transformers := pair(trans, var.variable-transformers);
end;

define method transformer-specializers
    (trans :: <transformer>) => res :: type-union(<list>, one-of(#f, #"gf"));
  let res = trans.%transformer-specializers;
  if (res == #"not-computed-yet")
    let specs = trans.transformer-specializer-specifiers;
    trans.%transformer-specializers
      := if (specs == #f | specs == #"gf")
	   specs;
	 else
	   map(specifier-type, specs);
	 end if;
  else
    res;
  end if;
end method transformer-specializers;


// Seals for file transdef.dylan

// <transformer> -- subclass of <object>
define sealed domain make(singleton(<transformer>));
define sealed domain initialize(<transformer>);
