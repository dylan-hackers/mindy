module: heap

define class <state> (<object>)
  slot stream :: <stream>, required-init-keyword: stream:;
  slot next-id :: <fixed-integer>, init-value: 0;
  slot object-queue :: <deque>, init-function: curry(make, <deque>);
  slot object-names :: <table>, init-function: curry(make, <table>);
end;

define method build-initial-heap
    (roots :: <vector>, stream :: <stream>) => ();
  let state = make(<state>, stream: stream);
  format(stream, "\t.data\n\t.align\t8\n\t.export\troots, DATA\nroots");
  for (ctv in roots)
    spew-reference(ctv, $general-rep, state);
  end;
  until (state.object-queue.empty?)
    let object = pop(state.object-queue);
    format(stream, "\n# %s\n\t.align\t8\n%s",
	   object, state.object-names[object]);
    spew-object(object, state);
  end;
end;


define generic spew-reference
    (object :: false-or(<ct-value>), rep :: <representation>, state :: <state>)
    => ();

define method spew-reference
    (object :: <false>, rep :: <representation>, state :: <state>)
    => ();
  format(state.stream, "\t.blockz\t%d\n", rep.representation-size);
end;

define method spew-reference
    (object :: <literal>, rep :: <immediate-representation>, state :: <state>)
    => ();
  let bits = raw-bits(object);
  select (rep.representation-size)
    1 => format(state.stream, "\t.byte\t%d\n", bits);
    2 => format(state.stream, "\t.half\t%d\n", bits);
    4 => format(state.stream, "\t.word\t%d\n", bits);
    8 =>
      format(state.stream, "\t.word\t%d, %d\n",
	     ash(bits, -32),
	     logand(bits, ash(as(<extended-integer>, 1), 32) - 1))
  end;
end;

define method spew-reference
    (object :: <ct-value>, rep :: <general-representation>, state :: <state>)
    => ();
  let cclass = object.ct-value-cclass;
  let best-rep = pick-representation(cclass, #"speed");
  let (heapptr, dataword)
    = if (instance?(best-rep, <data-word-representation>))
	values(make(<proxy>, for: cclass), raw-bits(object));
      else
	values(object, 0);
      end;
  format(state.stream, "\t.word\t%s, %d\n",
	 object-name(heapptr, state),
	 dataword);
end;

define method spew-reference
    (object :: <proxy>, rep :: <general-representation>, state :: <state>)
    => ();
  format(state.stream, "\t.word\t%s, 0\n", object-name(object, state));
end;

define method spew-reference
    (object :: <ct-value>, rep == $heap-rep, state :: <state>) => ();
  format(state.stream, "\t.word\t%s\n", object-name(object, state));
end;


define method object-name (object :: <ct-value>, state :: <state>)
    => name :: <string>;
  element(state.object-names, object, default: #f)
    | begin
	let name = format-to-string("L%d", state.next-id);
	state.next-id := state.next-id + 1;
	element(state.object-names, object) := name;
	push-last(state.object-queue, object);
	name;
      end;
end;


define generic raw-bits (ctv :: <literal>) => res :: <integer>;

define method raw-bits (ctv :: <literal-true>) => res :: <integer>;
  1;
end;

define method raw-bits (ctv :: <literal-false>) => res :: <integer>;
  1;
end;

define method raw-bits (ctv :: <literal-fixed-integer>) => res :: <integer>;
  ctv.literal-value;
end;

define method raw-bits (ctv :: <literal-single-float>) => res :: <integer>;
  raw-bits-for-float(ctv.literal-value, 24, 127, 8);
end;

define method raw-bits (ctv :: <literal-double-float>) => res :: <integer>;
  raw-bits-for-float(ctv.literal-value, 53, 1023, 11);
end;

define method raw-bits (ctv :: <literal-extended-float>) => res :: <integer>;
  // ### gcc doesn't use extended floats for long doubles.
  // raw-bits-for-float(ctv.literal-value, 113, 16383, 15);
  raw-bits-for-float(ctv.literal-value, 53, 1023, 11);
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
    (object :: <literal-boolean>, state :: <state>) => ();
  spew-instance(object.ct-value-cclass, state);
end;

define method spew-object
    (object :: <literal-extended-integer>, state :: <state>) => ();
  spew-instance(specifier-type(#"<extended-integer>"), state);
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
		  as(<ct-value>, as(<string>, object.literal-value)));
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
  spew-instance(specifier-type(#"<simple-object-vector>"), state);
end;

define method spew-object
    (object :: <literal-string>, state :: <state>) => ();
  spew-instance(specifier-type(#"<byte-string>"), state);
end;

define method spew-object
    (object :: <union-ctype>, state :: <state>) => ();
  let members = #();
  let singletons = #();
  for (member in object.members)
    if (instance?(member, <singleton-ctype>))
      singletons := pair(member.singleton-value, singletons);
    else
      members := pair(member, members);
    end;
  end;
  spew-instance(specifier-type(#"<union>"), state,
		union-members: make(<literal-list>, contents: members),
		union-singletons: make(<literal-list>, contents: singletons));
end;

define method spew-object
    (object :: <limited-integer-ctype>, state :: <state>) => ();
  spew-instance(specifier-type(#"<limited-integer>"), state,
		limited-integer-base-class: object.base-class,
		limited-integer-minimum: as(<ct-value>, object.low-bound),
		limited-integer-maximum: as(<ct-value>, object.high-bound));
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
    (object :: <cclass>, state :: <state>) => ();
  spew-instance(specifier-type(#"<class>"), state);
end;

define method spew-object (object :: <proxy>, state :: <state>) => ();
  spew-reference(object.proxy-for, $heap-rep, state);
end;


define method spew-instance
    (class :: <cclass>, state :: <state>, #rest slots) => ();
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
  for (field in fields)
    select (field by instance?)
      <false> => #f;
      <fixed-integer> =>
	format(state.stream, "\t.blockz\t%d\n", field);
      <instance-slot-info> =>
	let init-value
	  = block (return)
	      let getter = field.slot-getter;
	      let slot-name = getter & getter.variable-name;
	      if (getter)
		for (index from 0 below slots.size by 2)
		  if (slots[index] == slot-name)
		    return(slots[index + 1]);
		  end;
		end;
	      end;
	      for (override in field.slot-overrides)
		if (csubtype?(class, override.override-introduced-by))
		  if (override.override-init-value == #t
			| override.override-init-function)
		    error("Init value for %s in %= not set up.",
			  slot-name, class);
		  end;
		  return(override.override-init-value);
		end;
	      end;
	      if (field.slot-init-value == #t | field.slot-init-function)
		error("Init value for %s in %= not set up.", slot-name, class);
	      end;
	      field.slot-init-value;
	    end;
	spew-reference(init-value, field.slot-representation, state);
    end;
  end;
end;
