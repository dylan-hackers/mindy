module: heap

define class <state> (<object>)
  slot stream :: <stream>, required-init-keyword: stream:;
  slot next-id :: <fixed-integer>, init-value: 0;
  slot object-queue :: <deque>, init-function: curry(make, <deque>);
  slot symbols :: type-union(<literal-false>, <literal-symbol>),
    init-function: curry(make, <literal-false>);
end;

define method build-initial-heap (roots :: <vector>, stream :: <stream>)
    => ();
  let state = make(<state>, stream: stream);
  format(stream, "\t.data\n\t.align\t8\n");
  for (unit in roots)
    let prefix :: <byte-string> = unit[0];
    let roots :: <simple-object-vector> = unit[1];
    format(stream, "\n\t.export\t%s_roots, DATA\n%s_roots\n", prefix, prefix);
    for (ctv in roots, index from 0)
      spew-reference(ctv, *general-rep*,
		     stringify(prefix, "_roots[", index, ']'),
		     state);
    end;
  end;
  until (state.object-queue.empty?)
    let object = pop(state.object-queue);
    format(stream, "\n; %s\n\t.align\t8\n%s",
	   object, object.ct-value-heap-label);
    spew-object(object, state);
  end;
  format(stream,
	 "\n\n\t.align\t8\n\t.export\tinitial_symbols, DATA\n"
	   "initial_symbols\n");
  spew-reference(state.symbols, *heap-rep*, "Initial Symbols", state);
end;


define generic spew-reference
    (object :: false-or(<ct-value>), rep :: <representation>,
     tag :: <byte-string>, state :: <state>)
    => ();

define method spew-reference
    (object :: <false>, rep :: <representation>,
     tag :: <byte-string>, state :: <state>)
    => ();
  format(state.stream, "\t.blockz\t%d\t; %s\n", rep.representation-size, tag);
end;

define method spew-reference
    (object :: <literal>, rep :: <immediate-representation>,
     tag :: <byte-string>, state :: <state>)
    => ();
  let bits = raw-bits(object);
  select (rep.representation-size)
    1 => format(state.stream, "\t.byte\t%d\t; %s\n", bits, tag);
    2 => format(state.stream, "\t.half\t%d\t; %s\n", bits, tag);
    4 => format(state.stream, "\t.word\t%d\t; %s\n", bits, tag);
    8 =>
      format(state.stream, "\t.word\t%d, %d\t; %s\n",
	     ash(bits, -32),
	     logand(bits, ash(as(<extended-integer>, 1), 32) - 1),
	     tag)
  end;
end;

define method spew-reference
    (object :: <ct-value>, rep :: <general-representation>,
     tag :: <byte-string>, state :: <state>)
    => ();
  let cclass = object.ct-value-cclass;
  let best-rep = pick-representation(cclass, #"speed");
  let (heapptr, dataword)
    = if (instance?(best-rep, <data-word-representation>))
	values(make(<proxy>, for: cclass), raw-bits(object));
      else
	values(object, 0);
      end;
  format(state.stream, "\t.word\t%s, %d\t; %s\n",
	 object-name(heapptr, state),
	 dataword, tag);
end;

define method spew-reference
    (object :: <proxy>, rep :: <general-representation>,
     tag :: <byte-string>, state :: <state>)
    => ();
  format(state.stream, "\t.word\t%s, 0\t; %s\n",
	 object-name(object, state), tag);
end;

define method spew-reference
    (object :: <ct-value>, rep :: <heap-representation>,
     tag :: <byte-string>, state :: <state>) => ();
  format(state.stream, "\t.word\t%s\t; %s\n", object-name(object, state), tag);
end;

define method spew-reference
    (object :: <ct-entry-point>, rep :: <immediate-representation>,
     tag :: <byte-string>, state :: <state>)
    => ();
  format(state.stream, "\t.word\t%s\t; %s\n", object-name(object, state), tag);
end;

define method spew-reference
    (object :: <ct-entry-point>, rep :: <general-representation>,
     tag :: <byte-string>, state :: <state>)
    => ();
  format(state.stream, "\t.word\t%s, %s\t; %s\n",
	 object-name(make(<proxy>, for: object.ct-value-cclass), state),
	 object-name(object, state), tag);
end;


define method object-name (object :: <ct-value>, state :: <state>)
    => name :: <string>;
  object.ct-value-heap-label
    | begin
	let name = stringify('L', state.next-id);
	state.next-id := state.next-id + 1;
	object.ct-value-heap-label := name;
	push-last(state.object-queue, object);
	name;
      end;
end;

define method object-name (object :: <ct-entry-point>, state :: <state>)
    => name :: <string>;
  object.ct-value-heap-label
    | begin
	let name = object.entry-point-c-name;
	format(state.stream, "\t.import\t%s, code\n", name);
	object.ct-value-heap-label := name;
	name;
      end;
end;
	


define generic raw-bits (ctv :: <literal>) => res :: <integer>;

define method raw-bits (ctv :: <literal-true>) => res :: <integer>;
  1;
end;

define method raw-bits (ctv :: <literal-false>) => res :: <integer>;
  0;
end;

define method raw-bits (ctv :: <literal-fixed-integer>) => res :: <integer>;
  ctv.literal-value;
end;

define method raw-bits (ctv :: <literal-single-float>) => res :: <integer>;
  raw-bits-for-float(ctv, 24, 127, 8);
end;

define method raw-bits (ctv :: <literal-double-float>) => res :: <integer>;
  raw-bits-for-float(ctv, 53, 1023, 11);
end;

define method raw-bits (ctv :: <literal-extended-float>) => res :: <integer>;
  // ### gcc doesn't use extended floats for long doubles.
  // raw-bits-for-float(ctv.literal-value, 113, 16383, 15);
  raw-bits-for-float(ctv, 53, 1023, 11);
end;

define method raw-bits (ctv :: <literal-character>) => res :: <integer>;
  as(<integer>, ctv.literal-value);
end;

define method raw-bits-for-float
    (ctv :: <literal-float>, precision :: <fixed-integer>,
     bias :: <fixed-integer>, exponent-bits :: <fixed-integer>)
    => res :: <integer>;
  let num = as(<ratio>, ctv.literal-value);
  if (zero?(num))
    0;
  else
    let (num, neg?)
      = if (negative?(num))
	  values(-num, #t);
	else
	  values(num, #f);
	end;
    let (exponent, fraction)
      = if (num >= 1)
	  for (exponent from 1,
	       fraction = num / 2 then fraction / 2,
	       while: fraction >= 1)
	  finally
	    values(exponent, fraction);
	  end;
	else
	  for (exponent from 0 by -1,
	       fraction = num then fraction * 2,
	       while: fraction < ratio(1,2))
	  finally
	    values(exponent, fraction);
	  end;
	end;
    let biased-exponent = exponent + bias;
    if (biased-exponent >= ash(1, exponent-bits))
      // Overflow.
      error("%s is too big.", ctv);
    end;
    if (biased-exponent <= 0)
      if (-biased-exponent >= precision - 1)
	// Underflow.
	error("%s is too small.", ctv);
      end;
      fraction := fraction / ash(1, -biased-exponent);
      biased-exponent := 0;
    end;
    let shifted-fraction
      = round/(ash(numerator(fraction), precision),
	       denominator(fraction));
    let bits = logior(ash(as(<extended-integer>, biased-exponent),
			  precision - 1),
		      logand(shifted-fraction,
			     ash(as(<extended-integer>, 1), precision - 1)
			       - 1));
    if (neg?)
      logior(bits,
	     ash(as(<extended-integer>, 1),
		 precision + exponent-bits - 1));
    else
      bits;
    end;
  end;
end;


define generic spew-object (object :: <ct-value>, state :: <state>) => ();


define method spew-object
    (object :: <ct-not-supplied-marker>, state :: <state>) => ();
  spew-instance(specifier-type(#"<not-supplied-marker>"), state);
end;

define method spew-object
    (object :: <literal-boolean>, state :: <state>) => ();
  spew-instance(object.ct-value-cclass, state);
end;

define method spew-object
    (object :: <literal-extended-integer>, state :: <state>) => ();
  let digits = make(<stretchy-vector>);
  local
    method repeat (remainder :: <extended-integer>);
      let (remainder :: <extended-integer>, digit :: <integer>)
	= floor/(remainder, 256);
      add!(digits,
	   make(<literal-fixed-integer>,
		value: as(<extended-integer>, digit)));
      unless (if (logbit?(7, digit))
		remainder = -1;
	      else
		remainder = 0;
	      end)
	repeat(remainder);
      end;
    end;
  repeat(object.literal-value);
  spew-instance(specifier-type(#"<extended-integer>"), state,
		bignum-size: as(<ct-value>, digits.size),
		bignum-digit: digits);
end;

define method spew-object (object :: <literal-ratio>, state :: <state>) => ();
  let num = as(<ratio>, object.literal-value);
  spew-instance(object.ct-value-cclass, state,
		numerator:
		  make(<literal-extended-integer>, num.numerator),
		denominator:
		  make(<literal-extended-integer>, num.denominator));
end;

define method spew-object (object :: <literal-float>, state :: <state>) => ();
  spew-instance(object.ct-value-cclass, state, value: object);
end;

define method spew-object
    (object :: <literal-symbol>, state :: <state>) => ();
  spew-instance(specifier-type(#"<symbol>"), state,
		symbol-string:
		  as(<ct-value>, as(<string>, object.literal-value)),
		symbol-next: state.symbols);
  state.symbols := object;
end;

define method spew-object
    (object :: <literal-pair>, state :: <state>) => ();
  spew-instance(specifier-type(#"<pair>"), state,
		head: object.literal-head,
		tail: object.literal-tail);
end;

define method spew-object
    (object :: <literal-empty-list>, state :: <state>) => ();
  spew-instance(specifier-type(#"<empty-list>"), state,
		head: object, tail: object);
end;

define method spew-object
    (object :: <literal-simple-object-vector>, state :: <state>) => ();
  let contents = object.literal-value;
  spew-instance(specifier-type(#"<simple-object-vector>"), state,
		size: as(<ct-value>, contents.size),
		%element: contents);
end;

define constant *spewed-string* = as(<stretchy-vector>, "\t.string\t\"");
define constant *spewed-string-size* = *spewed-string*.size;

define method spew-object
    (object :: <literal-string>, state :: <state>) => ();
  let str = object.literal-value;
  let class = specifier-type(#"<byte-string>");
  let fields = get-class-fields(class);
  for (field in fields)
    select (field by instance?)
      <false> => #f;
      <fixed-integer> =>
	format(state.stream, "\t.blockz\t%d\n", field);
      <instance-slot-info> =>
	select (field.slot-getter.variable-name)
	  #"%object-class" =>
	    spew-reference(class, field.slot-representation, "%object-class",
			   state);
	  #"size" =>
	    spew-reference(as(<ct-value>, str.size), field.slot-representation,
			   "size", state);
	  #"%element" =>
	    let stream = state.stream;
	    // The following ugly code should be immensely faster than
	    // writing a character at a time to a stream.
	    for (i from 0 below str.size)
	      let char = str[i];
	      select (char)
		'\\' =>
		  add!(*spewed-string*, '\\');
		  add!(*spewed-string*, '\\');
		'"' =>
		  add!(*spewed-string*, '\\');
		  add!(*spewed-string*, '"');
		'\0' =>
		  add!(*spewed-string*, '\\');
		  add!(*spewed-string*, '0');
		'\n' =>
		  add!(*spewed-string*, '\\');
		  add!(*spewed-string*, 'n');
		'\t' =>
		  add!(*spewed-string*, '\\');
		  add!(*spewed-string*, 't');
		'\b' =>
		  add!(*spewed-string*, '\\');
		  add!(*spewed-string*, 'b');
		'\r' =>
		  add!(*spewed-string*, '\\');
		  add!(*spewed-string*, 'r');
		'\f' =>
		  add!(*spewed-string*, '\\');
		  add!(*spewed-string*, 'f');
		otherwise =>
		  if (char >= ' ' & char <= '~')
		    add!(*spewed-string*, char);
		  else
		    let code = as(<integer>, char);
		    let substr = format-to-string("\\x%x%x", ash(code, -16),
						  logand(code, 15));
		    map(method (c) add!(*spewed-string*, c) end, substr);
		  end if;
	      end select;
	    end for;
	    add!(*spewed-string*, '"');
	    add!(*spewed-string*, '\n');
	    write(as(<byte-string>, *spewed-string*), stream);
	    *spewed-string*.size := *spewed-string-size*;
	end select;
    end select;
  end for;
end method spew-object;

define method spew-object
    (object :: <union-ctype>, state :: <state>) => ();
  let mems = #();
  let sings = #();
  for (member in object.members)
    if (instance?(member, <singleton-ctype>))
      sings := pair(member.singleton-value, sings);
    else
      mems := pair(member, mems);
    end;
  end;
  spew-instance(specifier-type(#"<union>"), state,
		union-members: make(<literal-list>,
				    contents: mems,
				    sharable: #t),
		union-singletons: make(<literal-list>,
				       contents: sings,
				       sharable: #t));
end;

define method spew-object
    (object :: <limited-integer-ctype>, state :: <state>) => ();
  local method make-lit (x :: false-or(<integer>))
	  if (x == #f)
	    as(<ct-value>, x);
	  elseif (x < runtime-$minimum-fixed-integer
		    | x > runtime-$maximum-fixed-integer)
	    make(<literal-extended-integer>, value: x);
	  else
	    make(<literal-fixed-integer>, value: x);
	  end;
	end;
  spew-instance(specifier-type(#"<limited-integer>"), state,
		limited-integer-base-class: object.base-class,
		limited-integer-minimum: make-lit(object.low-bound),
		limited-integer-maximum: make-lit(object.high-bound));
end;

define method spew-object
    (object :: <singleton-ctype>, state :: <state>) => ();
  spew-instance(specifier-type(#"<singleton>"), state,
		singleton-object: object.singleton-value);
end;

define method spew-object
    (object :: <byte-character-ctype>, state :: <state>) => ();
  spew-instance(specifier-type(#"<byte-character-type>"), state);
end;

define method spew-object
    (object :: <defined-cclass>, state :: <state>) => ();
  let defn = object.class-defn;
  spew-instance(specifier-type(#"<class>"), state,
		class-name:
		  make(<literal-symbol>,
		       value: object.cclass-name.name-symbol),
		unique-id:
		  as(<ct-value>, object.unique-id | -1),
		direct-superclasses:
		  make(<literal-simple-object-vector>,
		       contents: object.direct-superclasses,
		       sharable: #t),
		all-superclasses:
		  make(<literal-simple-object-vector>,
		       contents: object.precedence-list,
		       sharable: #t),
		closest-primary-superclass: object.closest-primary-superclass,
		direct-subclasses:
		  make(<literal-list>, contents: object.direct-subclasses),
		class-functional?: as(<ct-value>, object.functional?),
		class-primary?: as(<ct-value>, object.primary?),
		class-abstract?: as(<ct-value>, object.abstract?),
		class-sealed?: as(<ct-value>, object.sealed?),
		class-defered-evaluations:
		  defn.class-defn-defered-evaluations-function
		  | as(<ct-value>, #f),
		class-maker: defn.class-defn-maker-function
		  | as(<ct-value>, #f),
		class-new-slot-descriptors:
		  make(<literal-simple-object-vector>,
		       contents: object.new-slot-infos,
		       sharable: #t),
		class-all-slot-descriptors:
		  make(<literal-simple-object-vector>,
		       contents: object.all-slot-infos,
		       sharable: #t));
end;

define method spew-object
    (object :: <slot-info>, state :: <state>) => ();
  spew-instance(specifier-type(#"<slot-descriptor>"), state,
		slot-allocation:
		  as(<ct-value>,
		     select (object by instance?)
		       <instance-slot-info> => #"instance";
		       <class-slot-info> => #"class";
		       <each-subclass-slot-info> => #"each-subclass";
		       <virtual-slot-info> => #"virtual";
		     end),
		slot-type:
		  unless (instance?(object.slot-type, <unknown-ctype>))
		    object.slot-type;
		  end,
		slot-init-function:
		  if (instance?(object.slot-init-function, <ct-value>))
		    object.slot-init-function;
		  end,
		slot-init-value:
		  if (instance?(object.slot-init-value, <ct-value>))
		    object.slot-init-value;
		  end,
		slot-init-keyword:
		  as(<ct-value>, object.slot-init-keyword),
		slot-init-keyword-required?:
		  as(<ct-value>, object.slot-init-keyword-required?),
		slot-positions:
		  if (instance?(object, <instance-slot-info>))
		    as(<ct-value>, object.slot-positions);
		  end if);
end method spew-object;

define method spew-object (object :: <proxy>, state :: <state>) => ();
  spew-reference(object.proxy-for, *heap-rep*, "%object-class", state);
end;

define method spew-object (object :: <ct-function>, state :: <state>) => ();
  spew-function(object, state,
		general-entry:
		  make(<ct-entry-point>, for: object, kind: #"general"));
end;

define method spew-object
    (object :: <ct-generic-function>, state :: <state>) => ();
  let defn = object.ct-function-definition;
  spew-function(object, state,
		general-entry:
		  begin
		    let discriminator = defn.generic-defn-discriminator;
		    if (discriminator)
		      make(<ct-entry-point>, for: discriminator,
			   kind: #"general");
		    else
		      let dispatch = dylan-defn(#"gf-call");
		      if (dispatch)
			make(<ct-entry-point>,
			     for: dispatch.ct-value,
			     kind: #"main");
		      else
			#f;
		      end;
		    end;
		  end,
		generic-function-methods:
		  make(<literal-list>,
		       contents:
			 remove(map(ct-value, generic-defn-methods(defn)), #f),
		       sharable: #f));
end;

define method spew-object (object :: <ct-method>, state :: <state>) => ();
  spew-function(object, state,
		general-entry:
		  if (object.ct-method-hidden?)
		    let tramp = dylan-defn(#"general-call");
		    if (tramp)
		      make(<ct-entry-point>,
			   for: tramp.ct-value,
			   kind: #"main");
		    else
		      #f;
		    end;
		  else
		    make(<ct-entry-point>, for: object, kind: #"general");
		  end,
		generic-entry:
		  make(<ct-entry-point>, for: object, kind: #"generic"));
end;

define method spew-function
    (func :: <ct-function>, state :: <state>, #rest slots) => ();
  let sig = func.ct-function-signature;
  let returns = sig.returns;
  let positionals = returns.positional-types;
  let min-values = returns.min-values;
  apply(spew-instance, func.ct-value-cclass, state,
	function-name:
	  make(<literal-string>, value: func.ct-function-name),
	function-specializers:
	  make(<literal-simple-object-vector>,
	       contents: sig.specializers,
	       sharable: #t),
	function-rest?: as(<ct-value>, sig.rest-type & #t),
	function-keywords:
	  if (sig.key-infos)
	    make(<literal-simple-object-vector>,
		 contents: map(compose(curry(as, <ct-value>), key-name),
			       sig.key-infos),
		 sharable: #t);
	  else
	    as(<ct-value>, #f);
	  end,
	function-all-keys?: as(<ct-value>, sig.all-keys?),
	function-values:
	  make(<literal-simple-object-vector>,
	       contents: copy-sequence(positionals, end: min-values),
	       sharable: #t),
	function-rest-value:
	  reduce(ctype-union, returns.rest-value-type,
		 copy-sequence(positionals, start: min-values)),
	slots);
end;


define method spew-instance
    (class :: <cclass>, state :: <state>, #rest slots) => ();
  for (field in get-class-fields(class))
    select (field by instance?)
      <false> => #f;
      <fixed-integer> =>
	format(state.stream, "\t.blockz\t%d\n", field);
      <instance-slot-info> =>
	let init-value = find-init-value(class, field, slots);
	let getter = field.slot-getter;
	let name = if (getter)
		     as(<string>, getter.variable-name);
		   else
		     "???";
		   end;
	if (instance?(field, <vector-slot-info>))
	  let len-ctv = find-init-value(class, field.slot-size-slot, slots);
	  unless (len-ctv)
	    compiler-warning("Length of a variable length instance"
			       " unspecified?");
	    len-ctv := as(<ct-value>, 0);
	  end;
	  unless (instance?(len-ctv, <literal-fixed-integer>))
	    error("Bogus length: %=", len-ctv);
	  end;
	  let len = as(<fixed-integer>, len-ctv.literal-value);
	  if (instance?(init-value, <sequence>))
	    unless (init-value.size == len)
	      error("Size mismatch.");
	    end;
	    for (element in init-value,
		 index from 0)
	      spew-reference(element, field.slot-representation,
			     stringify(name, '[', index, ']'),
			     state);
	    end;
	  else
	    for (index from 0 below len)
	      spew-reference(init-value, field.slot-representation,
			     stringify(name, '[', index, ']'),
			     state);
	    end;
	  end;
	else
	  spew-reference(init-value, field.slot-representation, name, state);
	end;
    end;
  end;
end;

define method get-class-fields (class :: <cclass>)
    => res :: <simple-object-vector>;
  if (class.class-heap-fields)
    class.class-heap-fields;
  else
    if (class.abstract?)
      error("Spewing an abstract class?");
    end;
    let layout = class.instance-slots-layout;
    let fields = make(<vector>, size: layout.layout-length + 1, fill: #f);
    for (slot in class.all-slot-infos)
      if (instance?(slot, <instance-slot-info>))
	block (return)
	  for (entry in slot.slot-positions)
	    if (csubtype?(class, entry.head))
	      fields[entry.tail] := slot;
	      return();
	    end;
	  end;
	  error("Can't find the position for %= in %=?", slot, class);
	end;
      end;
    end;
    for (hole in layout.layout-holes)
      fields[hole.head] := hole.tail;
    end;
    class.class-heap-fields := fields;
  end if;
end method get-class-fields;

define method find-init-value
    (class :: <cclass>, slot :: <instance-slot-info>,
     slots :: <simple-object-vector>)
    => res :: type-union(<ct-value>, <sequence>, <false>);
  block (return)
    let object-type = object-ctype();

    // This is very magical.  If the slot was introduced by <object>,
    // it must be %object-class, and its value must be the class.  We
    // should double-check the validity of this assumption, but this
    // is an extremely expensive special case, so the potential
    // savings are large.
    if (slot.slot-introduced-by == object-type)
      return(class);
    end if;

    let getter = slot.slot-getter;
    let slot-name = getter & getter.variable-name;
    if (getter)
      for (index from 0 below slots.size by 2)
	if (slots[index] == slot-name)
	  let val = slots[index + 1];
	  if (val)
	    return(val);
	  end;
	end;
      end;
    end;
    for (override in slot.slot-overrides)
      let intro = override.override-introduced-by;
      if (intro == object-type | csubtype?(class, intro))
	if (override.override-init-value == #t
	      | override.override-init-function)
	  compiler-warning("Init value for %s in %= not set up.",
			   slot-name, class);
	  return(#f);
	end;
	return(override.override-init-value);
      end;
    end;
    if (slot.slot-init-value == #t | slot.slot-init-function)
      compiler-warning("Init value for %s in %= not set up.",
		       slot-name, class);
    end;
    slot.slot-init-value;
  end;
end;
