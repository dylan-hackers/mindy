module: top-level-expressions
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/convert/tlexpr.dylan,v 1.9 1996/03/17 00:56:29 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

define class <expression-tlf> (<top-level-form>)
  slot tlf-expression :: <expression-parse>,
    required-init-keyword: expression:;
end;

define method print-message
    (tlf :: <expression-tlf>, stream :: <stream>) => ();
  write("Top level form.", stream);
end;



// process-top-level-form -- method on imported GF.
//
// Puke if any local declarations appear at top level.
//
define method process-top-level-form (form :: <local-declaration-parse>) => ();
  compiler-error-location
    (form, "Local declarations cannot appear directly at top level.");
end;



define method process-top-level-form (form :: <expression-parse>) => ();
  add!(*Top-Level-Forms*, make(<expression-tlf>, expression: form));
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
	      apply(return,
		    make(<body-parse>, parts: new-body),
		    copy-sequence(forms, start: index + 1));
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
    let expr = make(<body-parse>, parts: new-body);
    add!(*Top-Level-Forms*, make(<expression-tlf>, expression: expr));
  end;
end;

define method finalize-top-level-form (tlf :: <expression-tlf>) => ();
  // Nothing to do.
end;

define method convert-top-level-form
    (builder :: <fer-builder>, tlf :: <expression-tlf>) => ();
  fer-convert(builder, tlf.tlf-expression, make(<lexenv>), #"nothing", #f);
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
    add!(*Top-Level-Forms*, make(<magic-interal-primitives-placeholder>));
  else
    next-method();
  end;
end;

define method finalize-top-level-form
    (tlf :: <magic-interal-primitives-placeholder>) => ();
  // Nothing to do.
end;

define method convert-top-level-form
    (builder :: <fer-builder>, tlf :: <magic-interal-primitives-placeholder>)
    => ();
  // Nothing to do.
end;

define method dump-od
    (tlf :: <magic-interal-primitives-placeholder>, state :: <dump-state>)
    => ();
  // Do nothing.
end;
