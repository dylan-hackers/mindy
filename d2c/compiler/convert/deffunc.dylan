module: define-functions
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/convert/deffunc.dylan,v 1.18 1995/05/09 16:15:25 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


define abstract class <function-definition> (<abstract-constant-definition>)
  //
  // The signature.  Filled in during definition finalization.
  slot function-defn-signature :: <signature>, init-keyword: signature:;
  //
  // #t if this definition requires special handling at loadtime.  Can be
  // because of something non-constant in the signature or in the case of
  // methods, can be because the generic is hairy.  Fill in during
  // finalization.
  slot function-defn-hairy? :: <boolean>,
    init-value: #f, init-keyword: hairy:;
end;

define method defn-type (defn :: <function-definition>) => res :: <cclass>;
  dylan-value(#"<function>");
end;

define class <generic-definition> (<function-definition>)
  //
  // #f iff the open adjective wasn't supplied.
  slot generic-defn-sealed? :: <boolean>, required-init-keyword: sealed:;
  //
  // All the <method-definition>s defined on this generic function.
  slot generic-defn-methods :: <list>,
    init-value: #();
  //
  // Information about sealed methods of this GF.  This is filled in on demand.
  // Use generic-defn-seal-info instead.  See "method-tree".
  slot %generic-defn-seal-info :: <list>;
  //
  // The leaf for the discriminator function, or #f if the generic function
  // isn't sufficiently static to be able to build one.
  slot generic-defn-discriminator-leaf :: union(<function-literal>, <false>),
    init-value: #f;
end;

define method defn-type (defn :: <generic-definition>) => res :: <cclass>;
  dylan-value(#"<generic-function>");
end;

define class <implicit-generic-definition>
    (<generic-definition>, <implicit-definition>)
  //
  // Implicit generic definitions are sealed.
  keyword sealed:, init-value: #t;
end;

define abstract class <abstract-method-definition> (<function-definition>)
  //
  // The leaf for this method, of #f if it is sufficiently hairy that
  // it can't me represented as a <method-literal>.  Filled in by fer
  // conversion.
  slot method-defn-leaf :: union(<method-literal>, <false>),
    init-value: #f;
  //
  // The <method-parse> if we are to inline this method, #f otherwise.
  slot method-defn-inline-expansion :: false-or(<method-parse>),
    init-value: #f, init-keyword: inline-expansion:;
end;

define method defn-type (defn :: <abstract-method-definition>)
    => res :: <cclass>;
  dylan-value(#"<method>");
end;

define class <method-definition> (<abstract-method-definition>)
  //
  // #f iff the open adjective wasn't supplied.
  slot method-defn-sealed? :: <boolean>, required-init-keyword: sealed:;
  //
  // The generic function this method is part of, or #f if the base-name is
  // undefined or not a generic function.
  slot method-defn-of :: union(<generic-definition>, <false>),
    required-init-keyword: method-of:;
end;

define abstract class <accessor-method-definition> (<method-definition>)
  slot accessor-method-defn-slot-info :: union(<false>, <slot-info>),
    required-init-keyword: slot:;
end;

define class <getter-method-definition> (<accessor-method-definition>)
end;

define class <setter-method-definition> (<accessor-method-definition>)
end;

define class <define-generic-tlf> (<simple-define-tlf>)
  //
  // Make the definition required.
  required keyword defn:;
  //
  // The param list for the generic function.
  slot generic-tlf-param-list :: <parameter-list>,
    required-init-keyword: param-list:;
  //
  // The returns list for the generic function.
  slot generic-tlf-returns :: <parameter-list>,
    required-init-keyword: returns:;
end;

define class <define-implicit-generic-tlf> (<simple-define-tlf>)
  //
  // Make the definition required.
  required keyword defn:;
end;

define class <define-method-tlf> (<simple-define-tlf>)
  //
  // The name being defined.  Note: this isn't the name of the method, it is
  // the name of the generic function.
  slot method-tlf-base-name :: <name>, required-init-keyword: base-name:;
  //
  // True if the define method is sealed, false if open.
  slot method-tlf-sealed? :: <boolean>, required-init-keyword: sealed:;
  //
  // True if the define method was declared inline, #f if not.
  slot method-tlf-inline? :: <boolean>, required-init-keyword: inline:;
  //
  // The guts of the method being defined.
  slot method-tlf-parse :: <method-parse>, required-init-keyword: parse:;
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
		  sealed: ~open?);
  note-variable-definition(defn);
  add!($Top-Level-Forms,
       make(<define-generic-tlf>,
	    defn: defn,
	    param-list: form.defgen-param-list,
	    returns: form.defgen-returns));
end;

define method process-top-level-form (form :: <define-method-parse>) => ();
  let name = form.defmethod-method.method-name.token-symbol;
  let (open?, sealed?, inline?)
    = extract-modifiers("define method", name, form.define-modifiers,
			#"open", #"sealed", #"inline");
  if (open? & sealed?)
    error("define method %s can't be both open and sealed", name);
  end;
  let base-name = make(<basic-name>, symbol: name, module: *Current-Module*);
  let parse = form.defmethod-method;
  let params = parse.method-param-list;
  implicitly-define-generic(base-name, params.paramlist-required-vars.size,
			    params.paramlist-rest & ~params.paramlist-keys,
			    params.paramlist-keys & #t);
  let tlf = make(<define-method-tlf>, base-name: base-name, sealed: ~open?,
		 inline: inline?, parse: parse);
  add!($Top-Level-Forms, tlf);
end;

define method implicitly-define-generic
    (name :: <basic-name>, num-required :: <integer>,
     variable-args? :: <boolean>, keyword-args? :: <boolean>)
    => ();
  let var = find-variable(name);
  unless (var & var.variable-definition)
    let defn
      = make(<implicit-generic-definition>,
	     name: name,
	     signature:
	       make(<signature>,
		    specializers:
		      make(<list>, size: num-required, fill: object-ctype()),
		    rest-type: variable-args? & object-ctype(),
		    keys: keyword-args? & #(),
		    all-keys: #f,
		    returns: wild-ctype()));
    note-variable-definition(defn);
    add!($Top-Level-Forms, make(<define-implicit-generic-tlf>, defn: defn));
  end;
end;


// finalize-top-level-form

define method finalize-top-level-form (tlf :: <define-generic-tlf>) => ();
  let defn = tlf.tlf-defn;
  let (signature, anything-non-constant?)
    = compute-signature(tlf.generic-tlf-param-list, tlf.generic-tlf-returns);
  defn.function-defn-signature := signature;
  if (anything-non-constant?)
    defn.function-defn-hairy? := #t;
    for (meth in defn.generic-defn-methods)
      meth.function-defn-hairy? := #t;
    end;
  end;
end;

define method finalize-top-level-form (tlf :: <define-implicit-generic-tlf>)
    => ();
  // Nothing to finalize.
end;

define method finalize-top-level-form (tlf :: <define-method-tlf>)
    => ();
  let name = tlf.method-tlf-base-name;
  let (signature, anything-non-constant?)
    = compute-signature(tlf.method-tlf-parse.method-param-list,
			tlf.method-tlf-parse.method-returns);
  tlf.tlf-defn := make(<method-definition>,
		       base-name: name,
		       signature: signature,
		       hairy: anything-non-constant?,
		       sealed: tlf.method-tlf-sealed?,
		       inline-expansion: tlf.method-tlf-inline?
			 & ~anything-non-constant?
			 & tlf.method-tlf-parse);
end;

define method make (wot :: limited(<class>, subclass-of: <method-definition>),
		    #next next-method, #rest keys,
		    #key base-name, signature, hairy: hairy?)
    => res :: <method-definition>;
  let var = find-variable(base-name);
  let generic-defn
    = if (var & instance?(var.variable-definition, <generic-definition>))
	var.variable-definition;
      end;
  let defn = apply(next-method, wot,
		   name: make(<method-name>,
			      generic-function: base-name,
			      specializers: signature.specializers),
		   hairy: hairy? | generic-defn == #f
		     | generic-defn.function-defn-hairy?,
		   method-of: generic-defn,
		   keys);
  if (generic-defn)
    if (slot-initialized?(generic-defn, %generic-defn-seal-info))
      error("Adding a method to a GF which has already been sealed:\n"
            "  %=",
	    defn);
    end;
    generic-defn.generic-defn-methods
      := pair(defn, generic-defn.generic-defn-methods);
  end;
  defn;
end;

define method compute-signature
    (param-list :: <parameter-list>, returns :: <parameter-list>)
    => (signature :: <signature>, anything-non-constant? :: <boolean>);
  let anything-non-constant? = #f;
  local
    method maybe-eval-type (param)
      let type = param.param-type;
      if (type)
	let ctype = ct-eval(type, #f);
	select (ctype by instance?)
	  <false> =>
	    anything-non-constant? := #t;
	    make(<unknown-ctype>);
	  <ctype> =>
	    ctype;
	  otherwise =>
	    // ### Should just be a warning.
	    error("%= isn't a type.", ctype);
	end;
      else
	object-ctype();
      end;
    end,
    method make-key-info (param)
      make(<key-info>,
	   key-name: param.param-keyword,
	   type: maybe-eval-type(param));
    end;
  values(make(<signature>,
	      specializers: map-as(<list>, maybe-eval-type,
				   param-list.paramlist-required-vars),
	      rest-type: param-list.paramlist-rest & object-ctype(),
	      keys: (param-list.paramlist-keys
		       & map-as(<list>, make-key-info,
				param-list.paramlist-keys)),
	      all-keys: param-list.paramlist-all-keys?,

	      returns:
	        make-values-ctype(map-as(<list>, maybe-eval-type,
			                 returns.paramlist-required-vars),
				  returns.paramlist-rest & object-ctype())),
	 anything-non-constant?);
end;


// Compile-top-level-form

define method convert-top-level-form
    (builder :: <fer-builder>, tlf :: <define-generic-tlf>) => ();
  convert-generic-definition(builder, tlf.tlf-defn);
end;

define method convert-top-level-form
    (builder :: <fer-builder>, tlf :: <define-implicit-generic-tlf>) => ();
  let defn = tlf.tlf-defn;
  let name = defn.defn-name;
  let var = find-variable(name);
  if (var & var.variable-definition == defn)
    convert-generic-definition(builder, tlf.tlf-defn);
  end;
end;

define method convert-generic-definition
    (builder :: <fer-builder>, defn :: <generic-definition>) => ();
  //
  // Ensure summary analysis of method set.
  generic-defn-seal-info(defn);

  if (defn.function-defn-hairy?)
    let policy = $Default-Policy;
    let source = make(<source-location>);
    let args = make(<stretchy-vector>);
    // ### compute the args.
    let temp = make-local-var(builder, #"gf", object-ctype());
    build-assignment
      (builder, policy, source, temp,
       make-unknown-call(builder, dylan-defn-leaf(builder, #"%make-gf"), #f,
			 as(<list>, args)));
    build-assignment(builder, policy, source, #(),
		     make-operation(builder, <set>, list(temp), var: defn));
  else
    maybe-make-discriminator(builder, defn);
  end;
end;  

define method convert-top-level-form
    (builder :: <fer-builder>, tlf :: <define-method-tlf>) => ();
  let defn = tlf.tlf-defn;
  let lexenv = make(<lexenv>);
  let name = format-to-string("Define Method %s", defn.defn-name);
  let leaf = fer-convert-method(builder, tlf.method-tlf-parse, name,
				#"global", lexenv, lexenv);
  let literal-method? = instance?(leaf, <method-literal>);
  defn.method-defn-leaf := literal-method? & leaf;
  if (defn.function-defn-hairy? | ~literal-method?)
    // We don't use method-defn-of, because that is #f if there is a definition
    // but it isn't a define generic.
    let gf-name = tlf.method-tlf-base-name;
    let gf-var = find-variable(gf-name);
    let gf-defn = gf-var & gf-var.variable-definition;
    if (gf-defn)
      let policy = $Default-Policy;
      let source = make(<source-location>);
      build-assignment
	(builder, policy, source, #(),
	 make-unknown-call
	   (builder, dylan-defn-leaf(builder, #"add-method"), #f,
	    list(make-definition-leaf(builder, gf-defn), leaf)));
    else
      error("No definition for %=, and can't implicitly define it.",
	    gf-name);
    end;
  end;
end;



// Generic function discriminator functions.

define method maybe-make-discriminator
    (builder :: <fer-builder>, gf :: <generic-definition>) => ();
  if (gf.generic-defn-sealed? & ~gf.function-defn-hairy?
	& every?(method (meth)
		   ~meth.function-defn-hairy?
		     & every?(method (method-spec, gf-spec)
				method-spec == gf-spec
				  | (instance?(method-spec, <cclass>)
				       & method-spec.sealed?);
			      end,
			      meth.function-defn-signature.specializers,
			      gf.function-defn-signature.specializers);
		 end,
		 gf.generic-defn-methods))
    let policy = $Default-Policy;
    let source = make(<source-location>);
    let sig = gf.function-defn-signature;
    let vars = make(<stretchy-vector>);
    for (specializer in sig.specializers,
	 index from 0)
      let var = make-local-var(builder,
			       as(<symbol>, format-to-string("arg%d", index)),
			       specializer);
      add!(vars, var);
    end;
    let nspecs = vars.size;
    let rest-type = sig.rest-type | (sig.key-infos & object-ctype());
    if (rest-type)
      let var = make-local-var(builder, #"rest", rest-type);
      add!(vars, var);
    end;

    let discriminator-sig
      = make(<signature>,
	     specializers: sig.specializers,
	     rest-type: rest-type,
	     key-infos: sig.key-infos & #(),
	     all-keys: sig.key-infos & #t,
	     returns: sig.returns);

    let region = build-function-body(builder, policy, source,
				     format-to-string("Discriminator for %s",
						      gf.defn-name),
				     as(<list>, vars));
    let results = make-values-cluster(builder, #"results", wild-ctype());
    build-discriminator-tree
      (builder, policy, source, as(<list>, vars), rest-type & #t, results,
       as(<list>, make(<range>, from: 0, below: nspecs)),
       sort-methods(gf.generic-defn-methods,
		    make(<vector>, size: nspecs, fill: #f),
		    empty-ctype()),
       gf);
    build-return(builder, policy, source, region, results);
    end-body(builder);

    gf.generic-defn-discriminator-leaf
      := make-function-literal(builder, #"global", discriminator-sig, region);
  end;
end;

define method build-discriminator-tree
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     arg-vars :: <list>, rest? :: <boolean>, results :: <abstract-variable>,
     remaining-discriminations :: <list>,
     method-set :: <method-set>, gf :: <generic-definition>)
    => ();
  if (empty?(method-set.all-methods))
    build-assignment(builder, policy, source, results,
		     make-error-operation(builder, "No applicable methods."));
  elseif (empty?(remaining-discriminations))
    let ordered = map(curry(make-definition-leaf, builder),
		      method-set.ordered-methods);
    let ambig = map(curry(make-definition-leaf, builder),
		    method-set.ambiguous-methods);
    let ambig-leaf
      = if (~empty?(ambig))
	  let var = make-local-var(builder, #"ambiguous", object-ctype());
	  build-assignment
	    (builder, policy, source, var,
	     make-unknown-call(builder, dylan-defn-leaf(builder, #"list"), #f,
			       ambig));
	  var;
	end;
    if (~empty?(ordered))
      let func = ordered.head;
      let next-leaf = make-local-var(builder, #"next-methods", object-ctype());
      build-assignment
	(builder, policy, source, next-leaf,
	 make-unknown-call(builder, dylan-defn-leaf(builder, #"list"), #f,
			   if (ambig-leaf)
			     concatenate(ordered.tail, list(ambig-leaf));
			   else
			     ordered.tail;
			   end));
      if (rest?)
	let apply-leaf = dylan-defn-leaf(builder, #"apply");
	build-assignment
	  (builder, policy, source, results,
	   make-unknown-call(builder, apply-leaf, next-leaf,
			     pair(func, arg-vars)));
      else
	build-assignment(builder, policy, source, results,
			 make-unknown-call(builder, func, next-leaf,
					   arg-vars));
      end;
    elseif (ambig-leaf)
      let op = make-error-operation
	(builder, "Ambiguous method: %s", ambig-leaf);
      build-assignment(builder, policy, source, results, op);
    else
      error("Where did all the methods go?");
    end;
  else
    //
    // Figure out which of the remaining positions would be the best one to
    // specialize on.
    let discriminate-on
      = if (remaining-discriminations.tail == #())
	  remaining-discriminations.head;
	else
	  let discriminate-on = #f;
	  let max-distinct-specializers = 0;
	  for (posn in remaining-discriminations)
	    let distinct-specializers
	      = count-distinct-specializers(method-set.all-methods, posn);
	    if (distinct-specializers > max-distinct-specializers)
	      max-distinct-specializers := distinct-specializers;
	      discriminate-on := posn;
	    end;
	  end;
	  discriminate-on;
	end;
    let remaining-discriminations
      = remove(remaining-discriminations, discriminate-on);
    //
    // Divide up the methods based on that one argument.
    let ranges = discriminate-on-one-arg(discriminate-on, method-set, gf);
    //
    // Extract the unique id for this argument.
    let class-temp = make-local-var(builder, #"class", object-ctype());
    let obj-class-leaf = dylan-defn-leaf(builder, #"%object-class");
    build-assignment(builder, policy, source, class-temp,
		     make-unknown-call(builder, obj-class-leaf, #f,
				       list(arg-vars[discriminate-on])));
    let id-temp = make-local-var(builder, #"id", object-ctype());
    let unique-id-leaf = dylan-defn-leaf(builder, #"unique-id");
    build-assignment(builder, policy, source, id-temp,
		     make-unknown-call(builder, unique-id-leaf, #f,
				       list(class-temp)));
    let less-then = dylan-defn-leaf(builder, #"<");
    //
    // Recursivly build an if tree based on that division of the methods.
    local
      method split-range (min, max)
	if (min == max)
	  let method-set = ranges[min].third;
	  let arg = arg-vars[discriminate-on];
	  let temp = copy-variable(builder, arg);
	  build-assignment
	    (builder, policy, source, temp,
	     make-operation(builder, <truly-the>, list(arg),
			    guaranteed-type: method-set.restriction-type));
	  arg-vars[discriminate-on] := temp;
	  build-discriminator-tree
	    (builder, policy, source, arg-vars, rest?, results,
	     remaining-discriminations, method-set, gf);
	  arg-vars[discriminate-on] := arg;
	else
	  let half-way-point = ash(min + max, -1);
	  let cond-temp = make-local-var(builder, #"cond", object-ctype());
	  let ctv = make(<literal-fixed-integer>,
			 value: ranges[half-way-point].second + 1);
	  let bound = make-literal-constant(builder, ctv);
	  build-assignment(builder, policy, source, cond-temp,
			   make-unknown-call(builder, less-then, #f,
					     list(id-temp, bound)));
	  build-if-body(builder, policy, source, cond-temp);
	  split-range(min, half-way-point);
	  build-else(builder, policy, source);
	  split-range(half-way-point + 1, max);
	  end-body(builder);
	end;
      end;
    split-range(0, ranges.size - 1);
  end;
end;


define method count-distinct-specializers
    (methods :: <list>, arg-posn :: <fixed-integer>)
    => count :: <fixed-integer>;
  // ### so what if we don't dispatch off the better args first?
  1;
end;


define class <method-set> (<object>)
  slot arg-classes :: <simple-object-vector>,
    required-init-keyword: arg-classes:;
  slot ordered-methods :: <list>,
    required-init-keyword: ordered:;
  slot ambiguous-methods :: <list>,
    required-init-keyword: ambiguous:;
  slot all-methods :: <list>,
    required-init-keyword: all:;
  slot restriction-type :: <ctype>,
    required-init-keyword: restriction-type:;
end;


define method discriminate-on-one-arg
    (discriminate-on :: <fixed-integer>, method-set :: <method-set>,
     gf :: <generic-definition>)
    => res :: <simple-object-vector>;
  //
  // For each method, associate it with all the direct classes for which that
  // method will be applicable.  Applicable is an object table mapping class
  // objects to sets of methods.  Actually, it maps to pairs where the head
  // is the class again and the tail is the set because portable dylan doesn't
  // include keyed-by.
  let applicable = make(<object-table>);
  let always-applicable = #();
  let gf-spec = gf.function-defn-signature.specializers[discriminate-on];
  for (meth in method-set.all-methods)
    let specializer
      = meth.function-defn-signature.specializers[discriminate-on];
    let direct-classes = find-direct-classes(specializer);
    if (direct-classes)
      for (direct-class in direct-classes)
	let entry = element(applicable, direct-class, default: #f);
	if (entry)
	  entry.tail := pair(meth, entry.tail);
	else
	  applicable[direct-class] := list(direct-class, meth);
	end;
      end;
    elseif (specializer == gf-spec)
      always-applicable := pair(meth, always-applicable);
    end;
  end;
  //
  // Grovel over the direct-class -> applicable-methods mapping producing
  // an equivalent mapping that has direct classes with consecutive unique
  // ids and equivalent method sets merged.
  //
  // Each entry in ranges is a vector of [min, max, method-set].  If max is
  // #f then that means unbounded.  We maintain the invariant that there are
  // no holes.
  //
  let ranges
    = begin
	let possible-direct-classes = find-direct-classes(gf-spec);
	if (possible-direct-classes)
	  for (direct-class in possible-direct-classes.tail,
	       min-id = possible-direct-classes.head.unique-id
		 then min(min-id, direct-class.unique-id),
	       max-id = possible-direct-classes.head.unique-id
		 then max(max-id, direct-class.unique-id))
	  finally
	    list(vector(min-id, max-id, #f));
	  end;
	else
	  let arg-classes = copy-sequence(method-set.arg-classes);
	  arg-classes[discriminate-on] := gf-spec;
	  let method-set = sort-methods(always-applicable, arg-classes,
					gf-spec);
	  list(vector(0, #f, method-set));
	end;
      end;
  for (entry in applicable)
    let direct-class = entry.head;
    let arg-classes = copy-sequence(method-set.arg-classes);
    arg-classes[discriminate-on] := direct-class;
    let method-set
      = sort-methods(concatenate(entry.tail, always-applicable),
		     arg-classes, direct-class.direct-type);
    let this-id = direct-class.unique-id;
    for (remaining = ranges then remaining.tail,
	 prev = #f then remaining,
	 while: begin
		  let range :: <simple-object-vector> = remaining.head;
		  let max = range.second;
		  max & max < this-id;
		end)
    finally
      let range :: <simple-object-vector> = remaining.head;
      let other-set = range.third;
      if (method-set = other-set)
	other-set.restriction-type
	  := ctype-union(other-set.restriction-type,
			 method-set.restriction-type);
      else
	let min = range.first;
	let max = range.second;
	let new = if (this-id == max)
		    if (remaining.tail == #())
		      list(vector(this-id, this-id, method-set));
		    else
		      let next-range :: <simple-object-vector>
			= remaining.tail.head;
		      let next-set = next-range.third;
		      if (method-set = next-set)
			method-set.restriction-type
			  := ctype-union(method-set.restriction-type,
					 next-set.restriction-type);
			pair(vector(this-id, next-range.second, method-set),
			     remaining.tail.tail);
		      else
			pair(vector(this-id, this-id, method-set),
			     remaining.tail);
		      end;
		    end;
		  else
		    pair(vector(this-id, this-id, method-set),
			 pair(vector(this-id + 1, max, other-set),
			      remaining.tail));
		  end;
	if (this-id == min)
	  if (prev)
	    let prev-range :: <simple-object-vector> = prev.head;
	    let prev-set = prev-range.third;
	    if (method-set = prev-set)
	      prev-set.restriction-type
		:= ctype-union(prev-set.restriction-type,
			       method-set.restriction-type);
	      prev-range.second = new.head.second;
	      prev.tail := new.tail;
	    else
	      prev.tail := new;
	    end;
	  else
	    ranges := new;
	  end;
	else
	  range.second := this-id - 1;
	  remaining.tail := new;
	end;
      end;
    end;
  end;
  //
  // Convert ranges into a vector and return it.
  as(<simple-object-vector>, ranges);
end;
    

// sort-methods
//
// This routine takes a set of methods and sorts them by some subset of the
// arguments.
// 
define method sort-methods
    (methods :: <list>, arg-classes :: <simple-object-vector>,
     restriction-type :: <ctype>)
    => res :: <method-set>;

  // Ordered accumulates the methods we can tell the ordering of.  Each
  // element in this list is either a method or a list of equivalent methods.
  let ordered = #();

  // Ambiguous accumulates the set of methods of which it is unclear which
  // follows next after ordered.  These methods will all be mutually ambiguous
  // or equivalent.
  let ambiguous = #();

  for (meth in methods)
    block (done-with-method)
      for (remaining = ordered then remaining.tail,
	   prev = #f then remaining,
	   until: remaining == #())
	//
	// Grab the method to compare this method against.  If the next element
	// in ordered is a list of equivalent methods, grab the first one
	// as characteristic.
	let other
	  = if (instance?(remaining.head, <pair>))
	      remaining.head.head;
	    else
	      remaining.head;
	    end;
	select (compare-methods(meth, other, arg-classes))
	  //
	  // Our method is more specific, so insert it in the list of ordered
	  // methods and go on to the next method.
	  #"more-specific" =>
	    if (prev)
	      prev.tail := pair(meth, remaining);
	    else
	      ordered := pair(meth, remaining);
	    end;
	    done-with-method();
	  #"less-specific" =>
	    //
	    // Our method is less specific, so we can't do anything at this
	    // time.
	    #f;
	  #"unordered" =>
	    //
	    // Our method is equivalent.  Add it to the set of equivalent
	    // methods, making such a set if necessary.
	    if (instance?(remaining.head, <pair>))
	      remaining.head := pair(meth, remaining.head);
	    else
	      remaining.head := list(meth, remaining.head);
	    end;
	    done-with-method();
	  #"ambiguous" =>
	    //
	    // We know that the other method is more specific than anything
	    // in the current ambiguous set, so throw it away making a new
	    // ambiguous set.  Taking into account that we might have a set
	    // of equivalent methods on our hands.
	    remaining.tail := #();
	    if (instance?(remaining.head, <pair>))
	      ambiguous := pair(meth, remaining.head);
	    else
	      ambiguous := list(meth, remaining.head);
	    end;
	    done-with-method();
	end;
      finally
	//
	// Our method was less specific than any method in the ordered list.
	// This either means that our method needs to be tacked onto the end
	// of the ordered list, added to the ambiguous list, or ignored.
	// Compare the method against all the methods in the ambiguous list
	// to figure out which.
	let ambiguous-with = #();
	for (remaining = ambiguous then remaining.tail,
	     until: remaining == #())
	  select (compare-methods(meth, remaining.head, arg-classes))
	    #"more-specific" =>
	      #f;
	    #"less-specific" =>
	      done-with-method();
	    #"unordered" =>
	      ambiguous := pair(meth, ambiguous);
	      done-with-method();
	    #"ambiguous" =>
	      ambiguous-with := pair(remaining.head, ambiguous-with);
	  end;
	end;
	//
	// Ambiguous-with is only #() if we are more specific than anything
	// currently in the ambigous set.  So tack us onto the end of the
	// ordered set.  Otherwise, set the ambigous set to us and everything
	// we are ambiguous with.
	if (ambiguous-with == #())
	  if (prev)
	    prev.tail := list(meth);
	  else
	    ordered := list(meth);
	  end;
	else
	  ambiguous := pair(meth, ambiguous-with);
	end;
      end;
    end;
  end;

  make(<method-set>, arg-classes: arg-classes, ordered: ordered,
       ambiguous: ambiguous, all: methods, restriction-type: restriction-type);
end;


define method compare-methods
    (meth1 :: <method-definition>, meth2 :: <method-definition>,
     arg-classes :: <vector>)
    => res :: one-of(#"more-specific", #"less-specific",
		     #"unordered", #"ambiguous");
  block (return)
    let result = #"unordered";
    for (arg-class in arg-classes,
	 spec1 in meth1.function-defn-signature.specializers,
	 spec2 in meth2.function-defn-signature.specializers)
      //
      // If this is an argument that we are actually sorting by,
      if (arg-class)
	//
	// If the two specializers are the same, then this argument offers no
	// ordering.
	let this-one
	  = unless (spec1 == spec2)
	      if (csubtype?(spec1, spec2))
		#"more-specific";
	      elseif (csubtype?(spec2, spec1))
		#"less-specific";
	      elseif (instance?(spec1, <cclass>) & instance?(spec2, <cclass>))
		// Neither argument is a subclass of the other.  So we have to
		// base it on the precedence list of the actual argument class.
		let cpl = arg-class.precedence-list;
		block (found)
		  for (super in cpl)
		    if (super == spec1)
		      found(#"more-specific");
		    elseif (super == spec2)
		      found(#"less-specific");
		    end;
		  finally
		    error("%= isn't applicable", arg-class);
		  end;
		end;
	      else
		// Neither argument is a subtype of the other and we have a
		// non-class specializers.  That's ambiguous, folks.
		return(#"ambiguous");
	      end;
	    end;
	unless (result == this-one)
	  if (result == #"unordered")
	    result := this-one;
	  else
	    return(#"ambiguous");
	  end;
	end;
      end;
    end;
    result;
  end;
end;


// = on <method-set>s
// 
// Two method sets are ``the same'' if they have the same methods, the same
// ordered methods (in the same order), and the same ambigous methods.
// 
define method \= (set1 :: <method-set>, set2 :: <method-set>)
    => res :: <boolean>;
  set1.ordered-methods = set2.ordered-methods
    & same-unordered?(set1.all-methods, set2.all-methods)
    & same-unordered?(set1.ambiguous-methods, set2.ambiguous-methods);
end;

// same-unordered?
//
// Return #t if the two lists have the same elements in any order.
// We assume that there are no duplicates in either list.
// 
define method same-unordered? (list1 :: <list>, list2 :: <list>)
    => res :: <boolean>;
  list1.size == list2.size
    & block (return)
	for (elem in list1)
	  unless (member?(elem, list2))
	    return(#f);
	  end;
	end;
	#t;
      end;
end;

