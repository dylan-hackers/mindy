module: define-classes
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

define class <define-designator-class-parse> (<define-class-parse>)
  // No additional slots
end class;

define-procedural-expander
  (#"make-define-designator-class",
   method (generator :: <expansion-generator>, name-frag :: <fragment>,
	   supers-frag :: <fragment>, slots-frag :: <fragment>,
	   options-frag :: <fragment>)
       => ();
     generate-fragment
       (generator,
	make-parsed-fragment
	  (make(<define-designator-class-parse>,
		name: extract-name(name-frag),
		superclass-exprs: map(expression-from-fragment,
				      split-fragment-at-commas(supers-frag)),
		slots: map(extract-slot, split-fragment-at-commas(slots-frag)),
		options: parse-property-list(make(<fragment-tokenizer>,
						  fragment: options-frag)),
	   source-location: generate-token-source-location(generator))));
   end method);

define class <struct-slot-parse> (<abstract-slot-parse>)
  constant slot struct-slot-parse-name :: <identifier-token>,
    required-init-keyword: name:;
  constant slot struct-slot-parse-options :: <simple-object-vector>,
    required-init-keyword: options:;
end class <struct-slot-parse>;

define-procedural-expander
  (#"make-struct-slot",
   method (generator :: <expansion-generator>, name-frag :: <fragment>,
	   options-frag :: <fragment>)
       => ();
     generate-fragment
       (generator,
	make-magic-fragment
	  (make(<struct-slot-parse>,
		name: extract-name(name-frag),
		options: parse-property-list(make(<fragment-tokenizer>,
						  fragment: options-frag))),
	   source-location: generate-token-source-location(generator)))
   end method);

define class <real-designator-class-definition> (<real-class-definition>)
  // no additional slots
end;

define method defn-type
    (defn :: <real-designator-class-definition>)
 => (res :: <cclass>);
  dylan-value(#"<designator-class>");
end;

define class <local-designator-class-definition>
    (<real-designator-class-definition>, <local-class-definition>)
  //
  // The c-name of the class
  slot class-defn-c-name :: false-or(<expression-parse>),
    required-init-keyword: c-name:;
  //
  // Pointer type referenced by a pointer class
  slot class-referenced-type :: false-or(<expression-parse>),
    required-init-keyword: referenced-type:;
  //
  // Struct packing alignment (power of 2 bytes, or 0 for union)
  slot class-pack :: false-or(<expression-parse>),
    required-init-keyword: pack:;
  //
  // The c-rep key
  slot class-defn-c-rep :: false-or(<symbol>),
    required-init-keyword: c-rep:;
  //
  // The import type
  slot class-defn-import-type :: false-or(<expression-parse>),
    required-init-keyword: import-type:;
  //
  // The export type
  slot class-defn-export-type :: false-or(<expression-parse>),
    required-init-keyword: export-type:;
  //
  // The import function
  slot class-defn-import-function :: false-or(<expression-parse>),
    required-init-keyword: import-function:;
  //
  // The export function
  slot class-defn-export-function :: false-or(<expression-parse>),
    required-init-keyword: export-function:;
  //
  // A superclass for generating a pointer type that refers to this type
  slot class-defn-pointer-type-superclass :: false-or(<expression-parse>),
    required-init-keyword: pointer-type-superclass:;
  //
  // The indirect getter function
  slot class-defn-indirect-getter-function :: false-or(<expression-parse>),
    required-init-keyword: indirect-getter:;
  //
  // The indirect setter function
  slot class-defn-indirect-setter-function :: false-or(<expression-parse>),
    required-init-keyword: indirect-setter:;
  // 
  // pointer value getter and setter functions
  slot class-defn-pointer-value-getter-function :: false-or(<expression-parse>),
    required-init-keyword: pointer-value-getter:;
  slot class-defn-pointer-value-setter-function :: false-or(<expression-parse>),
    required-init-keyword: pointer-value-setter:;
end class;

define method process-top-level-form
    (form :: <define-designator-class-parse>)
 => ();
  let name = form.defclass-name.token-symbol;
  let (class-functional?-frag, class-sealed?-frag, class-primary?-frag,
       class-abstract?-frag,
       c-name-frag, referenced-type-frag, pack-frag, c-rep-frag,
       import-type-frag, export-type-frag,
       import-function-frag, export-function-frag,
       pointer-type-superclass-frag,
       indirect-getter-frag, indirect-setter-frag,
       pointer-value-getter-frag, pointer-value-setter-frag)
    = extract-properties(form.defclass-options, #"functional", #"sealed",
			 #"primary", #"abstract", #"c-name",
			 #"referenced-type", #"pack", #"c-rep",
			 #"import-type", #"export-type",
			 #"import-function", #"export-function",
			 #"pointer-type-superclass",
			 #"indirect-getter", #"indirect-setter",
			 #"pointer-value-getter", #"pointer-value-setter");
  let class-functional?
    = class-functional?-frag & extract-boolean(class-functional?-frag);
  let class-sealed?
    = ~class-sealed?-frag | extract-boolean(class-sealed?-frag);
  let class-primary?
    = class-primary?-frag & extract-boolean(class-primary?-frag);
  let class-abstract?
    = class-abstract?-frag & extract-boolean(class-abstract?-frag);

  let class-c-name = c-name-frag & expression-from-fragment(c-name-frag);
  let class-referenced-type
    = referenced-type-frag & expression-from-fragment(referenced-type-frag);
  let class-pack = pack-frag & expression-from-fragment(pack-frag);
  let class-c-rep = c-rep-frag & extract-keyword(c-rep-frag);
  let class-import-type
    = import-type-frag & expression-from-fragment(import-type-frag);
  let class-export-type
    = export-type-frag & expression-from-fragment(export-type-frag);
  let class-import-function
    = import-function-frag & expression-from-fragment(import-function-frag);
  let class-export-function
    = export-function-frag & expression-from-fragment(export-function-frag);
  let class-pointer-type-superclass
    = pointer-type-superclass-frag
        & expression-from-fragment(pointer-type-superclass-frag);
  let class-indirect-getter
    = indirect-getter-frag & expression-from-fragment(indirect-getter-frag);
  let class-indirect-setter
    = indirect-setter-frag & expression-from-fragment(indirect-setter-frag);
  let class-pointer-value-getter
    = pointer-value-getter-frag
        & expression-from-fragment(pointer-value-getter-frag);
  let class-pointer-value-setter
    = pointer-value-setter-frag
        & expression-from-fragment(pointer-value-setter-frag);
  let slots = make(<stretchy-vector>);
  let overrides = make(<stretchy-vector>);
  let keywords = make(<stretchy-vector>);
  let struct-slots = make(<stretchy-vector>);
  unless (class-abstract? | empty?(form.defclass-superclass-exprs))
    add!(overrides,
	 make(<override-defn>,
	      getter-name: make(<basic-name>, symbol: #"%object-class",
				module: $Dylan-Module),
	      init-value: make(<varref-parse>, id: form.defclass-name)));
  end;
  for (option in form.defclass-slots)
    block ()
      process-designator-slot(name, class-functional?, slots, overrides,
			      keywords, struct-slots, option);
    exception (<fatal-error-recovery-restart>)
      #f;
    end block;
  end for;
  let slots = as(<simple-object-vector>, slots);
  let overrides = as(<simple-object-vector>, overrides);
  let keywords = as(<simple-object-vector>, keywords);
  let defn = make(<local-designator-class-definition>,
		  name: make(<basic-name>,
			     symbol: name,
			     module: *Current-Module*),
		  library: *Current-Library*,
		  supers: form.defclass-superclass-exprs,
		  functional: class-functional?,
		  sealed: class-sealed?,
		  primary: class-primary?,
		  abstract: class-abstract?,
		  slots: slots,
		  overrides: overrides,
		  keywords: keywords,
		  c-name: class-c-name,
		  referenced-type: class-referenced-type,
		  pack: class-pack,
		  c-rep: class-c-rep,
		  import-type: class-import-type,
		  export-type: class-export-type,
		  import-function: class-import-function,
		  export-function: class-export-function,
		  pointer-type-superclass: class-pointer-type-superclass,
		  indirect-getter: class-indirect-getter,
		  indirect-setter: class-indirect-setter,
		  pointer-value-getter: class-pointer-value-getter,
		  pointer-value-setter: class-pointer-value-getter);
  for (slot in slots)
    slot.slot-defn-class := defn;
    //
    // Implicitly define the accessor generics.
    if (slot.slot-defn-sizer-defn)
      implicitly-define-generic
	(*Current-Library*, slot.slot-defn-getter-name, 2, #f, #f);
      if (slot.slot-defn-setter-name)
	implicitly-define-generic
	  (*Current-Library*, slot.slot-defn-setter-name, 3, #f, #f);
      end;
    else
      implicitly-define-generic
	(*Current-Library*, slot.slot-defn-getter-name, 1, #f, #f);
      if (slot.slot-defn-setter-name)
	implicitly-define-generic
	  (*Current-Library*, slot.slot-defn-setter-name, 2, #f, #f);
      end;
    end;
  end;
  for (override in overrides)
    override.override-defn-class := defn;
  end for;
  for (keyword in keywords)
    keyword.keyword-defn-class := defn;
  end for;
  note-variable-definition(defn);
  add!(*Top-Level-Forms*, make(<define-class-tlf>, defn: defn));
end method process-top-level-form;

define generic process-designator-slot
    (class-name :: <symbol>, class-functional? :: <boolean>,
     slots :: <stretchy-vector>, overrides :: <stretchy-vector>,
     keywords :: <stretchy-vector>, struct-slots :: <stretchy-vector>,
     slot :: <abstract-slot-parse>)
    => ();

define method process-designator-slot
    (class-name :: <symbol>, class-functional? :: <boolean>,
     slots :: <stretchy-vector>, overrides :: <stretchy-vector>,
     keywords :: <stretchy-vector>, struct-slots :: <stretchy-vector>,
     slot :: <abstract-slot-parse>)
    => ();
  process-slot(class-name, class-functional?, slots, overrides, keywords,
	       slot);
end;

define method process-designator-slot
    (class-name :: <symbol>, class-functional? :: <boolean>,
     slots :: <stretchy-vector>, overrides :: <stretchy-vector>,
     keywords :: <stretchy-vector>, struct-slots :: <stretchy-vector>,
     slot :: <struct-slot-parse>)
    => ();
  
end;

define class <struct-slot-defn> (<object>)
  //
  // The designator-class that introduces this struct-slot
  slot struct-slot-defn-class :: <real-class-definition>;
  //
  // The expression to compute the declared type
  slot struct-slot-defn-c-type :: <real-class-definition>,
    required-init-keyword: c-type:;
  // 
  // #t if this slot is sealed, #f if not.  This really means that the getter
  // generic function is sealed on this class, the setter (if any) is sealed
  // on object and this class, and the address-getter (if any) is sealed on XXX
  slot struct-slot-defn-sealed? :: <boolean>,
    required-init-keyword: sealed:;
  //
  // The expression to compute the type.
  slot struct-slot-defn-type :: false-or(<expression-parse>),
    required-init-keyword: type:;
  //
  // The name of the getter generic function.
  slot struct-slot-defn-getter-name :: <name>,
    required-init-keyword: getter-name:;
  //
  // The getter method.  Filled in when computed.
  slot struct-slot-defn-getter-method :: <getter-method-definition>;
  //
  // The name of the setter generic function, or #f if there is no setter.
  slot struct-slot-defn-setter-name :: false-or(<name>),
    required-init-keyword: setter-name:;
  //
  // The setter method.  Filled in when computed.
  slot struct-slot-defn-setter-method :: false-or(<setter-method-definition>);
  //
  // The slot-info for this slot, or #f if we haven't computed it or don't know
  // enough about the class to compute it at all.
  slot struct-slot-defn-info :: false-or(<struct-slot-info>),
    init-value: #f;
end class;

define method compute-cclass
    (defn :: <real-designator-class-definition>,
     #next next-method)
    => (cclass-class :: false-or(<class>), init-args :: <sequence>);
  let (cclass-class, init-args) = next-method();

  if (cclass-class)
    let direct-superclasses
      = apply(method(#key direct-superclasses, #all-keys)
                direct-superclasses
              end,
              init-args);

    let designator-super = #f;
    for(super in direct-superclasses)
      if(instance?(super, <cdclass>))
        if(designator-super)
          compiler-error-location(defn,
                                  "designator-class %s inherits from more "
                                    "one designator-class superclass",
                                  defn.defn-name);
        else
          designator-super := super;
        end if;
      end if;
    end for;

    let designated-representation
      = (defn.class-defn-c-rep & c-rep(defn.class-defn-c-rep))
      | (designator-super & designator-super.designated-representation);
    let referenced-type
      = (defn.class-referenced-type
           & ct-eval(defn.class-referenced-type, #f))
      | (designator-super & designator-super.referenced-type);
    let pointer-type-superclass
      = (defn.class-defn-pointer-type-superclass
           & ct-eval(defn.class-defn-pointer-type-superclass, #f))
      | (designator-super & designator-super.pointer-type-superclass);
    let import-type
      = (defn.class-defn-import-type
           & ct-eval(defn.class-defn-import-type, #f))
      | (designator-super & designator-super.import-type);
    let export-type
      = (defn.class-defn-export-type
           & ct-eval(defn.class-defn-export-type, #f))
      | (designator-super & designator-super.export-type);

    if (referenced-type & ~instance?(referenced-type, <cdclass>))
      compiler-error-location(defn,
                              "the referenced-type for a designator-class "
                                "must be a designator-class");
    end if;

    if (designated-representation)
      init-args
        := apply(list,
                 size: designated-representation.representation-size,
                 alignment: designated-representation.representation-alignment,
                 init-args);
    end if;
    
    values(<defined-cdclass>,
           apply(list,
                 representation: designated-representation,
                 referenced-type: referenced-type,
                 pointer-type-superclass: pointer-type-superclass,
                 import-type: import-type,
                 export-type: export-type,
                 pointer-type-superclass: pointer-type-superclass,
                 init-args));
  else
    values(#f, #());
  end if;
end method;

define method class-defn-struct-slot-infos
    (defn :: <real-designator-class-definition>)
 => (res :: <simple-object-vector>);
  let class :: <cdclass> = defn.class-defn-cclass;
  class & class.struct-slot-infos;
end;

define method class-defn-struct-slot-infos-setter
    (vec :: false-or(<simple-object-vector>),
     defn :: <real-designator-class-definition>)
    => ();
  if (vec)
    let class :: <cdclass> = defn.class-defn-cclass;
    class.struct-slot-infos := vec;
  end;
end;

define constant $designator-class-definition-slots =
  concatenate($class-definition-slots,
              list(class-defn-struct-slot-infos, #f,
                   class-defn-struct-slot-infos-setter));

add-make-dumper(#"designator-class-definition", *compiler-dispatcher*,
		<real-designator-class-definition>,
		$designator-class-definition-slots,
		load-external: #t,
		load-side-effect:
		  method (defn :: <real-class-definition>) => ();
		    let class = defn.class-defn-cclass;
		    if (class)
		      class.class-defn := defn;
		    end;
		  end);

add-make-dumper(#"designator-class-definition", *compiler-dispatcher*,
		<local-designator-class-definition>,
		$designator-class-definition-slots,
		dumper-only: #t);
