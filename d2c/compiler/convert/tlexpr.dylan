module: top-level-expressions
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/convert/tlexpr.dylan,v 1.2 1994/12/16 11:53:55 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

define class <expression-tlf> (<top-level-form>)
  slot tlf-expression :: <expression>,
    required-init-keyword: expression:;
end;

define method process-top-level-form (form :: <expression>) => ();
  let expansion = expand(form, #f);
  if (expansion)
    do(process-top-level-form, expansion);
  else
    add!($Top-Level-Forms, make(<expression-tlf>, expression: form));
  end;
end;

define method process-top-level-form (form :: <begin>) => ();
  block (return)
    for (subform in form.begin-body,
	 index from 0)
      if (instance?(subform, <local-declaration>))
	let body = copy-sequence(form.begin-body, start: index);
	let expr = make(<begin>, body: body);
	add!($Top-Level-Forms, make(<expression-tlf>, expression: expr));
	return();
      else
	process-top-level-form(subform);
      end;
    end;
  end;
end;

define method finalize-top-level-form (tlf :: <expression-tlf>) => ();
  // Nothing to do.
end;

define method convert-top-level-form
    (builder :: <fer-builder>, tlf :: <expression-tlf>) => ();
  fer-convert(builder, tlf.tlf-expression, make(<lexenv>), #());
end;
