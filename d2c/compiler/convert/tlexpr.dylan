module: top-level-expressions
copyright: see below

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000, 2001  Gwydion Dylan Maintainers
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

define class <expression-tlf> (<top-level-form>)
  constant slot tlf-expression :: <expression-parse>,
    required-init-keyword: expression:;
end;

define method print-message
    (tlf :: <expression-tlf>, stream :: <stream>) => ();
  write(stream, "Top level form.");
end;



// process-top-level-form -- method on imported GF.
//
// Puke if any local declarations appear at top level.
//
define method process-top-level-form (form :: <local-declaration-parse>) => ();
  compiler-fatal-error-location
    (form, "Local declarations cannot appear directly at top level.");
end;



define method process-top-level-form (form :: <expression-parse>) => ();
  add!(*Top-Level-Forms*,
       make(<expression-tlf>,
	    expression: form,
	    source-location: form.source-location));
end;

define method process-top-level-form (form :: <body-parse>) => ();
  local
    method process (forms :: <simple-object-vector>)
	=> new-body :: false-or(<simple-object-vector>);
      block (return)
	for (subform in forms,
	     index from 0)
	  while (instance?(subform, <macro-call-parse>))
	    subform := macro-expand(subform);
	  end while;
	  if (instance?(subform, <body-parse>))
	    let new-body = process(subform.body-parts);
	    if (new-body)
	      let result = copy-sequence(forms, start: index);
	      result[0] := make(<body-parse>, parts: new-body);
	      return(result);
	    end;
	  elseif (instance?(subform, <local-declaration-parse>))
	    return(copy-sequence(forms, start: index));
	  else
	    process-top-level-form(subform);
	  end;
	finally
	  #f;
	end for;
      end block;
    end method process;
  let new-body = process(form.body-parts);
  if (new-body)
    let expr
	= make(<body-parse>,
	       parts: new-body,
	       source-location: form.source-location);
    add!(*Top-Level-Forms*,
	 make(<expression-tlf>,
	      expression: expr,
	      source-location: expr.source-location));
  end;
end;

define method finalize-top-level-form (tlf :: <expression-tlf>) => ();
  // Nothing to do.
end;

define method convert-top-level-form
    (builder :: <fer-builder>, tlf :: <expression-tlf>) => ();
  fer-convert(builder, tlf.tlf-expression, lexenv-for-tlf(tlf), #"nothing", #f);
end;

define method dump-od (tlf :: <expression-tlf>, state :: <dump-state>)
    => ();
  // Do nothing.
end;


// Magic internal primitives placeholder.

define method process-top-level-form
    (form :: <primitive-parse>, #next next-method)
    => ();
  if (form.primitive-name.token-symbol
	== #"magic-internal-primitives-placeholder")
    add!(*Top-Level-Forms*, make(<magic-internal-primitives-placeholder>));
  else
    next-method();
  end;
end;

define method finalize-top-level-form
    (tlf :: <magic-internal-primitives-placeholder>) => ();
  // Nothing to do.
end;

define method convert-top-level-form
    (builder :: <fer-builder>, tlf :: <magic-internal-primitives-placeholder>)
    => ();
  // Nothing to do.
end;

define method dump-od
    (tlf :: <magic-internal-primitives-placeholder>, state :: <dump-state>)
    => ();
  // Do nothing.
end;

// Seals for file tlexpr.dylan

// <expression-tlf> -- subclass of <top-level-form>
define sealed domain make(singleton(<expression-tlf>));
define sealed domain initialize(<expression-tlf>);
