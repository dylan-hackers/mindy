module: top-level-expressions
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/convert/tlexpr.dylan,v 1.8 1995/12/04 16:23:36 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

define class <expression-tlf> (<top-level-form>)
  slot tlf-expression :: <expression>,
    required-init-keyword: expression:;
end;

define method print-message
    (tlf :: <expression-tlf>, stream :: <stream>) => ();
  write("Top level form.", stream);
end;

define method process-top-level-form (form :: <expression>) => ();
  let expansion = expand(form, #f);
  if (expansion)
    do(process-top-level-form, expansion);
  else
    add!(*Top-Level-Forms*, make(<expression-tlf>, expression: form));
  end;
end;

define method process-top-level-form (form :: <begin>) => ();
  local
    method process (forms :: <simple-object-vector>)
	=> new-body :: false-or(<simple-object-vector>);
      block (return)
	for (subform in forms,
	     index from 0)
	  let expansion = expand(subform, #f);
	  if (expansion)
	    let new-body = process(expansion);
	    if (new-body)
	      return(concatenate(new-body,
				 copy-sequence(forms, start: index + 1)));
	    end;
	  elseif (instance?(subform, <local-declaration>))
	    return(copy-sequence(forms, start: index));
	  else
	    process-top-level-form(subform);
	  end;
	finally
	  #f;
	end;
      end;
    end;
  let new-body = process(form.begin-body);
  if (new-body)
    let expr = make(<begin>, body: new-body);
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

define class <magic-interal-primitives-placeholder> (<top-level-form>)
end;

define method print-message
    (tlf :: <magic-interal-primitives-placeholder>, stream :: <stream>) => ();
  write("Magic internal primitives.", stream);
end;

define method process-top-level-form
    (form :: <primitive>, #next next-method)
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
