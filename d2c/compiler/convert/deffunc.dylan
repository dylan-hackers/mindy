module: define-functions
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/convert/deffunc.dylan,v 1.1 1994/12/12 13:01:19 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


define class <function-definition> (<definition>)
end;

define class <generic-definition> (<function-definition>)
end;

define class <method-definition> (<function-definition>, <implicit-definition>)
end;


define class <define-generic-tlf> (<simple-define-tlf>)
end;

define class <define-method-tlf> (<simple-define-tlf>)
end;



// process-top-level-form

define method process-top-level-form (form :: <define-generic-parse>) => ();
  let name = form.defgen-name.token-symbol;
  let (open?, sealed?)
    = extract-modifiers("define generic", name, form.define-modifiers,
			#"open", #"sealed");
  if (open? & sealed?)
    error("define generic %s can't be both open and sealed", name);
  end;
  extract-properties("define generic", form.defgen-plist);
  let defn = make(<generic-definition>,
		  name: make(<basic-name>,
			     symbol: name,
			     module: *Current-Module*),
		  sealed: sealed?);
  note-variable-definition(defn);
  add!($Top-Level-Forms, make(<define-generic-tlf>, defn: defn));
end;

define method process-top-level-form (form :: <define-method-parse>) => ();
  let name = form.defmethod-method.method-name.token-symbol;
  let (open?, sealed?)
    = extract-modifiers("define method", name, form.define-modifiers,
			#"open", #"sealed");
  if (open? & sealed?)
    error("define method %s can't be both open and sealed", name);
  end;
  let defn = make(<method-definition>,
		  name: make(<basic-name>,
			     symbol: name,
			     module: *Current-Module*),
		  sealed: sealed?);
  note-variable-definition(defn);
  add!($Top-Level-Forms, make(<define-method-tlf>, defn: defn));
end;


// finalize-top-level-form

define method finalize-top-level-form (tlf :: <define-generic-tlf>) => ();
  // ### Need to actually do something.
end;

define method finalize-top-level-form (tlf :: <define-method-tlf>) => ();
  // ### Need to actually do something.
end;


// Compile-top-level-form

define method convert-top-level-form
    (builder :: <fer-builder>, tlf :: <define-generic-tlf>) => ();
  // ### Need to actually do something.
end;

define method convert-top-level-form
    (builder :: <fer-builder>, tlf :: <define-method-tlf>) => ();
  // ### Need to actually do something.
end;
