module: front


define generic convert-to-ssa (region :: <region>) => ();

define method convert-to-ssa (region :: <component>) => ();
  do(convert-to-ssa, region.all-methods);
end;

define method convert-to-ssa (region :: <simple-region>) => ();
  for (assign = region.first-assign then assign.next-op,
       while: assign)
    for (defn :: false-or(<initial-definition>)
	   = assign.defines then defn.definer-next,
	 prev = #f then defn,
	 while: defn)
      let var = defn.definition-of;
      if (instance?(var, <initial-variable>) & var.definitions.size == 1)
	let ssa = make(<ssa-variable>,
		       dependents: var.dependents,
		       var-info: var.var-info,
		       definer: assign,
		       definer-next: defn.definer-next);
	if (prev)
	  prev.definer-next := ssa;
	else
	  assign.defines := ssa;
	end;
	for (dep = var.dependents then dep.dependent-next,
	     while: dep)
	  if (dep.source-exp == var)
	    dep.source-exp := ssa;
	  else
	    error("The dependent's source-exp wasn't the var we were trying "
		    "to replace?");
	  end;
	end;
      end;
    end;
  end;
end;

define method convert-to-ssa (region :: <compound-region>) => ();
  do(convert-to-ssa, region.regions);
end;

define method convert-to-ssa (region :: <if-region>) => ();
  convert-to-ssa(region.then-region);
  convert-to-ssa(region.else-region);
end;

define method convert-to-ssa (region :: <body-region>) => ();
  convert-to-ssa(region.body);
end;


// Cheesy derive type stuff.

define method derive-types (component :: <component>) => ();
  for (lambda in component.all-methods)
    for (arg in lambda.vars)
      initial-derive-type-of(component, arg);
    end;
    initial-derive-type-pass(component, lambda);
  end;
  let dependent = #f;
  while (dependent := component.reoptimize-queue)
    component.reoptimize-queue := dependent.queue-next;
    dependent.queue-next := #"absent";
    derive-type(component, dependent);
  end;
end;

define method initial-derive-type-pass (component :: <component>,
					region :: <simple-region>)
    => ();
  for (assign = region.first-assign then assign.next-op,
       while: assign)
    initial-derive-type-of(component, assign.depends-on.source-exp);
  end;
end;

define method initial-derive-type-pass (component :: <component>,
					region :: <compound-region>)
    => ();
  for (subregion in region.regions)
    initial-derive-type-pass(component, subregion);
  end;
end;

define method initial-derive-type-pass (component :: <component>,
					region :: <if-region>)
    => ();
  initial-derive-type-pass(component, region.then-region);
  initial-derive-type-pass(component, region.else-region);
end;

define method initial-derive-type-pass (component :: <component>,
					region :: <body-region>)
    => ();
  initial-derive-type-pass(component, region.body);
end;


define method initial-derive-type-of (component :: <component>,
				      leaf :: <literal-constant>)
    => ();
  maybe-restrict-type(component, leaf, make-canonical-singleton(leaf.value));
end;

define method initial-derive-type-of (component :: <component>,
				      leaf :: <definition-constant-leaf>)
    => ();
  let type = leaf.const-defn.defn-type;
  if (type)
    maybe-restrict-type(component, leaf, type);
  end;
end;

define method initial-derive-type-of (component :: <component>,
				      leaf :: <function-literal>)
    => ();
  maybe-restrict-type(component, leaf, function-ctype());
end;

define method initial-derive-type-of (component :: <component>,
				      var :: <abstract-variable>)
    => ();
  maybe-restrict-type(component, var, var.var-info.asserted-type);
end;

define method initial-derive-type-of (component :: <component>,
				      expr :: <operation>)
    => ();
  if (expr.queue-next == #"absent")
    expr.queue-next := component.reoptimize-queue;
    component.reoptimize-queue := expr;
  end;
end;


define method maybe-restrict-type (component :: <component>,
				   expr :: <expression>,
				   type :: <values-ctype>)
    => ();
  let old-type = expr.derived-type;
  unless (values-subtype?(old-type, type))
    if (values-subtype?(type, old-type))
      expr.derived-type := type;
      queue-dependents(component, expr);
    end;
  end;
end;

define method maybe-restrict-type (component :: <component>,
				   var :: <abstract-variable>,
				   type :: <values-ctype>,
				   #next next-method)
    => ();
  next-method(component, var,
	      values-type-intersection(type,
				       var.var-info.asserted-type));
end;

define method queue-dependents (component :: <component>,
				expr :: <expression>)
    => ();
  for (dependency = expr.dependents then dependency.source-next,
       while: dependency)
    let dependent = dependency.dependent;
    if (dependent.queue-next == #"absent")
      dependent.queue-next := component.reoptimize-queue;
      component.reoptimize-queue := dependent;
    end;
  end;
end;

define method derive-type (component :: <component>,
			   dependent :: <dependent-mixin>)
    => ();
end;

define method derive-type (component :: <component>,
			   assignment :: <assignment>)
    => ();
  let values-type = assignment.depends-on.source-exp.derived-type;
  let defines = assignment.defines;
  if (defines & instance?(defines.var-info, <values-cluster-info>))
    maybe-restrict-type(component, defines, values-type);
  else
    for (var = defines then var.definer-next,
	 index from 0 below values-type.min-values,
	 positionals = values-type.positional-types then positionals.tail,
	 while: var)
      maybe-restrict-type(component, var, positionals.head);
    finally
      if (var)
	let false-type = dylan-value(#"<false>");
	for (var = var then var.definer-next,
	     positionals = positionals then positionals.tail,
	     while: var)
	  let type = if (positionals == #())
		       values-type.rest-value-type;
		     else
		       positionals.head;
		     end;
	  maybe-restrict-type(component, var, ctype-union(type, false-type));
	end;
      end;
    end;
  end;
end;

define method derive-type (component :: <component>,
			   operation :: <operation>)
    => ();
end;


define constant $primitive-type-derivers = make(<object-table>);

define method derive-type (component :: <component>,
			   primitive :: <primitive>)
    => ();
  let deriver = element($primitive-type-derivers, primitive.name, default: #f);
  if (deriver)
    let type = deriver(component, primitive);
    maybe-restrict-type(component, primitive, type);
  end;
end;

define method define-primitive-deriver
    (name :: <symbol>, deriver :: <function>)
    => ();
  $primitive-type-derivers[name] := deriver;
end;

define method boolean-result
    (component :: <component>, primitive :: <primitive>)
    => res :: <values-ctype>;
  dylan-value(#"<boolean>");
end;

define-primitive-deriver(#"fixnum-=", boolean-result);
define-primitive-deriver(#"fixnum-<", boolean-result);

define method fixnum-result
    (component :: <component>, primitive :: <primitive>)
    => res :: <values-ctype>;
  dylan-value(#"<fixed-integer>");
end;

for (name in #[#"fixnum-+", #"fixnum-*", #"fixnum--", #"fixnum-negative",
		 #"fixnum-logior", #"fixnum-logxor", #"fixnum-logand",
		 #"fixnum-lognot", #"fixnum-ash"])
  define-primitive-deriver(name, fixnum-result);
end;


// Cheesy type check stuff.


define generic add-type-checks (region :: <region>) => ();

define method add-type-checks (region :: <component>) => ();
  do(add-type-checks, region.all-methods);
end;

define method add-type-checks (region :: <simple-region>) => ();
  for (assign = region.first-assign then assign.next-op,
       while: assign)
    let source-type = derived-type(assign.depends-on.source-exp);
    let defns = assign.defines;
    if (defns & instance?(defns.var-info, <values-cluster-info>))
      unless (values-subtype?(source-type, defns.var-info.asserted-type))
	// ### just how are we going to check the types of value cluster
	// assignments?
	#f;
      end;
    else
      local
	method insert-type-check (var, prev)
	  // ### need to actually do something.
	end;

      for (var = defines then var.definer-next,
	   prev = #f then var,
	   index from 0 below source-type.min-values,
	   positionals = source-type.positional-types then positionals.tail,
	   while: var)
	unless (csubtype?(positionals.head, var.var-info.asserted-type))
	  insert-type-check(var, prev);
	end;
      finally
	if (var)
	  let false-type = dylan-value(#"<false>");
	  for (var = var then var.definer-next,
	       prev = prev then var,
	       positionals = positionals then positionals.tail,
	       while: var)
	    let type
	      = ctype-union(if (positionals == #())
			      source-type.rest-value-type;
			    else
			      positionals.head;
			    end,
			    false-type);
	    unless (csubtype?(type, var.var-info.asserted-type))
	      insert-type-check(var, prev);
	    end;
	  end;
	end;
      end;
    end;
  end;
end;

define method add-type-checks (region :: <compound-region>) => ();
  do(add-type-checks, region.regions);
end;

define method add-type-checks (region :: <if-region>) => ();
  add-type-checks(region.then-region);
  add-type-checks(region.else-region);
end;

define method add-type-checks (region :: <body-region>) => ();
  add-type-checks(region.body);
end;
