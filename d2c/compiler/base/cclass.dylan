module: classes
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/cclass.dylan,v 1.28 1995/12/16 03:42:51 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.

// $All-Classes -- internal.
//
// Holds all the classes allocated.  We guarantee that superclasses preceed
// subclasses by added new classes to the end when they are created.  As
// subclasses have to be created after their superclasses, this means they
// will be added after also.
//
define constant $All-Classes = make(<stretchy-vector>);


define abstract class <cclass> 
    (<ctype>, <eql-ct-value>, <identity-preserving-mixin>)
  //
  // The name, for printing purposes.
  slot cclass-name :: <name>, required-init-keyword: name:;

  slot loaded? :: <boolean>,
    init-value: #t, init-keyword: loading:;

  // List of the direct superclasses of this class.
  slot direct-superclasses :: <list>,
    required-init-keyword: direct-superclasses:;

  // Closest primary superclass.
  slot closest-primary-superclass :: <cclass>;

  // True when the class can't be functional.  Not the same thing as
  // ~functional? because abstract classes can be neither functional? nor
  // not-functional?.
  slot not-functional? :: <boolean>,
    init-keyword: not-functional:, init-value: #f;

  // True when class is functional, sealed, abstract, and/or primary.
  slot functional? :: <boolean>, init-keyword: functional:, init-value: #f;
  slot sealed? :: <boolean>, init-keyword: sealed:, init-value: #f;
  slot abstract? :: <boolean>, init-keyword: abstract:, init-value: #f;
  slot primary? :: <boolean>, init-keyword: primary:, init-value: #f;

  // Type describing direct instances of this class.  Either a
  // <direct-instance-ctype> (if concrete) or the empty type (if abstract).
  slot direct-type :: <ctype>;

  // class precedence list of all classes inherited, including this class and
  // indirectly inherited classes.  Unbound if not yet computed.
  slot precedence-list :: <list>, init-keyword: precedence-list:;

  // List of the direct subclasses.
  slot direct-subclasses :: <list>, init-value: #();

  // List of all known subclasses (including this class and indirect
  // subclasses).  If sealed, then this is all of 'em.
  slot subclasses :: <list>, init-value: #();

  // The unique id number associated with this class (only if concrete,
  // though.)
  slot unique-id :: false-or(<fixed-integer>), init-value: #f,
    init-keyword: unique-id:;

  // The range of ids that cover all the subclasses of this class and
  // only the subclasses of this class, if such a range exists.  That
  // range will exist if this class is never mixed in with any other
  // class.  And if this class is sealed.
  slot subclass-id-range-min :: false-or(<fixed-integer>), init-value: #f,
    init-keyword: subclass-id-range-min:;
  slot subclass-id-range-max :: false-or(<fixed-integer>), init-value: #f,
    init-keyword: subclass-id-range-max:;

  // The representation of instances of this class or #f if we haven't
  // picked them yet.
  slot speed-representation :: false-or(<representation>),
    init-value: #f, init-keyword: speed-representation:;
  slot space-representation :: false-or(<representation>),
    init-value: #f, init-keyword: space-representation:;
  //
  // Vector of <slot-info>s for the slots introduced by this class.
  slot new-slot-infos :: <simple-object-vector>,
    init-keyword: slots:;
  //
  // Vector of all the slots in instances of this class, in no particular
  // order. Filled in when the slot layouts are computed.
  slot all-slot-infos :: <vector>,
    init-keyword: all-slot-infos:;
  //
  // Vector of <override-info>s for the overrides introduced by this class.
  slot override-infos :: <simple-object-vector>,
    init-keyword: overrides:;
  //
  // Layout of the instance slots.  Filled in when the slot layouts are
  // computed.
  slot instance-slots-layout :: <layout-table>,
    init-keyword: instance-slots-layout:;
  //
  // The trailing vector slot, if any.  Filled in when the slot layouts are
  // computed.
  slot vector-slot :: false-or(<vector-slot-info>),
    init-keyword: vector-slot:;
  //
  // Count of the number of each-subclass slots.
  slot each-subclass-slots-count :: <fixed-integer>,
    init-keyword: each-subclass-slots-count:;
  //
  // Used by the heap builder.
  slot class-heap-fields :: false-or(<simple-object-vector>),
    init-value: #f;
end class;

define method initialize
    (class :: <cclass>, #next next-method,
     #key loading: loading? = #t, precedence-list, slots, overrides)
    => ();
  next-method();
  
  // Record the class.
  add!($All-Classes, class);

  // Make the direct-type for this cclass.
  class.direct-type
    := if (class.abstract?)
	 empty-ctype();
       else
	 make(<direct-instance-ctype>, base-class: class);
       end;

  // Compute the cpl if it wasn't already handed to us.
  let supers = class.direct-superclasses;
  let cpl = (precedence-list
	       | (class.precedence-list := compute-cpl(class, supers)));

  // Add us to all our direct superclasses direct-subclass lists.
  for (super in supers)
    if (super.obj-resolved?)
      super.direct-subclasses := pair(class, super.direct-subclasses);
    else
      request-backpatch(super,
			method (actual)
			  actual.direct-subclasses
			    := pair(class, actual.direct-subclasses);
			end);
    end;
  end;

  // Add us to all our superclasses subclass lists.
  for (super in cpl)
    if (super.obj-resolved?)
      super.subclasses := pair(class, super.subclasses);
    else
      request-backpatch(super,
			method (actual)
			  actual.subclasses := pair(class, actual.subclasses);
			end);
    end;
  end;

  // Find the closest primary superclass.  Note: we don't have to do any
  // error checking, because that is done for us in defclass.dylan.
  // Unless we are loading this class, in which case the loader will set
  // it up for us.
  unless (loading?)
    if (class.primary?)
      class.closest-primary-superclass := class;
    else
      let closest = #f;
      for (super in class.direct-superclasses)
	let primary-super = super.closest-primary-superclass;
	if (~closest | csubtype?(primary-super, closest))
	  closest := primary-super;
	end;
      end;
      class.closest-primary-superclass := closest;
    end;

    // Fill in introduced-by for the slots and overrides.
    for (slot in slots)
      slot.slot-introduced-by := class;
    end;
    for (override in overrides)
      override.override-introduced-by := class;
    end;
  end;
end;

define method print-object (cclass :: <cclass>, stream :: <stream>) => ();
  pprint-fields(cclass, stream, name: cclass.cclass-name);
end;

define method print-message (cclass :: <cclass>, stream :: <stream>) => ();
  write(as(<string>, cclass.cclass-name.name-symbol), stream);
end;


define constant <slot-allocation>
  = one-of(#"instance", #"class", #"each-subclass", #"virtual");

define abstract class <slot-info> (<eql-ct-value>, <identity-preserving-mixin>)
  //
  // The cclass that introduces this slot.  Not required, because we have to
  // make the regular slots before we can make the cclass that defines them.
  slot slot-introduced-by :: <cclass>,
    init-keyword: introduced-by:;
  //
  // The type we've decided to use for this slot.  Either the declared type,
  // or <object> if we can't figure out what the declared type is at
  // compile-time.
  slot slot-type :: <ctype>, init-keyword: type:;
  //
  // The getter generic function definition.  Used for slot identity.  If #f,
  // that means that the slot is an auxiliary slot hung off some other slot,
  // and therefore doesn't need additional identity information.
  slot slot-getter :: false-or(<variable>),
    required-init-keyword: getter:;
  //
  // True if the slot is read-only (i.e. no setter), False otherwise.
  slot slot-read-only? :: <boolean>,
    init-value: #f, init-keyword: read-only:;
  //
  // The initial value.  A <ct-value> if we can figure one out, #t if there is
  // one but we can't tell what it is, and #f if there isn't one.
  slot slot-init-value :: type-union(<ct-value>, <boolean>),
    init-value: #f, init-keyword: init-value:;
  //
  // The init-function.  A <ct-value> if we can figure one out, #t if there is
  // one but we can't tell what it is, and #f if there isn't one.
  slot slot-init-function :: type-union(<ct-value>, <boolean>),
    init-value: #f, init-keyword: init-function:;
  //
  // The init-keyword, or #f if there isn't one.
  slot slot-init-keyword :: false-or(<symbol>),
    init-value: #f, init-keyword: init-keyword:;
  //
  // True if the init-keyword is required, False if not.
  slot slot-init-keyword-required? :: <boolean>,
    init-value: #f, init-keyword: init-keyword-required:;
  //
  // List of all the overrides for this slot.  Filled in when the overrides
  // for some class are processed.
  slot slot-overrides :: <list>, init-value: #();
end;

define method print-message
    (lit :: <slot-info>, stream :: <stream>) => ();
  write("{a <slot-descriptor>}", stream);
end;

define method make (class == <slot-info>, #rest keys, #key allocation)
    => res :: <slot-info>;
  apply(make,
	select (allocation)
	  #"instance" => <instance-slot-info>;
	  #"class" => <class-slot-info>;
	  #"each-subclass" => <each-subclass-slot-info>;
	  #"virtual" => <virtual-slot-info>;
	end,
	keys);
end;

// initialize -- gf method.
//
// This method's only purpose is to make allocation be an acceptable keyword
// for the various subclasses of <slot-info> so we don't have to remove it from
// the set of keys passed in when we make the particular kind of slot-info
// above.
// 
define method initialize
    (info :: <slot-info>, #next next-method, #key allocation) => ();
  next-method();
end;

define class <instance-slot-info> (<slot-info>)
  slot slot-representation :: false-or(<representation>),
    init-value: #f, init-keyword: slot-representation:;
  slot slot-positions :: <list>,
    init-value: #(), init-keyword: slot-positions:;
  slot slot-initialized?-slot :: false-or(<instance-slot-info>),
    init-value: #f, init-keyword: slot-initialized?-slot:;
end;

define class <vector-slot-info> (<instance-slot-info>)
  slot slot-size-slot :: <instance-slot-info>,
    init-keyword: size-slot:;
end;

define class <class-slot-info> (<slot-info>)
end;

define class <each-subclass-slot-info> (<slot-info>)
  slot slot-positions :: <list>,
    init-value: #(), init-keyword: slot-positions:;
end;

define class <virtual-slot-info> (<slot-info>)
end;



define class <override-info> (<identity-preserving-mixin>)
  //
  // The cclass that introduces this override.  Filled in when the cclass that
  // introduces this override is initialized.
  slot override-introduced-by :: <cclass>,
    init-keyword: introduced-by:;
  //
  // The getter generic function definition.  Used for slot identity.
  slot override-getter :: <variable>,
    required-init-keyword: getter:;
  //
  // The slot-info this override is overriding.  Filled in when overrides are
  // inherited.
  slot override-slot :: <slot-info>;
  //
  // The initial value.  A <ct-value> if we can figure one out, #t if there is
  // one but we can't tell what it is, and #f if there isn't one.
  slot override-init-value :: type-union(<ct-value>, <boolean>),
    init-value: #f, init-keyword: init-value:;
  //
  // The init-function.  A <ct-value> if we can figure one out, #t if there is
  // one but we can't tell what it is, and #f if there isn't one.
  slot override-init-function :: type-union(<ct-value>, <boolean>),
    init-value: #f, init-keyword: init-function:;
end;


define method ct-value-cclass (object :: <cclass>) => res :: <cclass>;
  dylan-value(#"<class>");
end;

define method ct-value-cclass (object :: <slot-info>) => res :: <cclass>;
  dylan-value(#"<slot-descriptor>");
end;



// Ctype operations.

define method csubtype-dispatch (type1 :: <cclass>, type2 :: <cclass>)
    => result :: <boolean>;
  member?(type2, type1.precedence-list);
end method;

define method csubtype-dispatch
    (type1 :: <cclass>, type2 :: <direct-instance-ctype>)
    => result :: <boolean>;
  type1 == type2.base-class & type1.sealed? & empty?(type1.subclasses);
end;

define method ctype-intersection-dispatch(type1 :: <cclass>, type2 :: <cclass>)
    => (result :: <ctype>, precise :: <boolean>);
  if (type1.sealed?)
    values(reduce(ctype-union, empty-ctype(),
		  choose(rcurry(csubtype?, type2), type1.subclasses)),
	   #t);
  elseif (type2.sealed?)
    values(reduce(ctype-union, empty-ctype(),
		  choose(rcurry(csubtype?, type1), type2.subclasses)),
	   #t);
  else
    let primary1 = type1.closest-primary-superclass;
    let primary2 = type2.closest-primary-superclass;
    if (csubtype?(primary1, primary2) | csubtype?(primary2, primary1))
      // The closest primary superclasses are not inconsistent.  Therefore,
      // someone could make a new subclass that inherits from both.
      values(type1, #f);
    else
      values(empty-ctype(), #t);
    end;
  end;
end method;

define method find-direct-classes(type :: <cclass>) => res :: false-or(<list>);
  if (type.sealed?)
    choose(complement(abstract?), type.subclasses);
  else
    #f;
  end;
end method;


// Class Precedence List computation

// This class is a temporary data structure used during CPL computation.
define class <class-precedence-description> (<object>)
  //
  // The class this cpd describes the precedence of.
  slot cpd-class :: <cclass>, required-init-keyword: class:;
  //
  // List of cpd's for the direct superclasses.
  slot cpd-supers :: <list>, init-value: #();
  //
  // List of cpd's for classes that have to follow this class.
  slot cpd-after :: <list>, init-value: #();
  //
  // Count of times this cpd appeards in some other cpd's after list.
  slot cpd-count :: <fixed-integer>, init-value: 0;
end class;

define constant compute-cpl = method (cl, superclasses)
  case
    superclasses == #() =>
      list(cl);

    superclasses.tail == #() =>
      pair(cl, superclasses.head.precedence-list);

    otherwise =>
      slow-compute-cpl(cl, superclasses);
  end;
end method;

// Find CPL when there are multiple direct superclasses
define constant slow-compute-cpl = method (cl, superclasses)
  let cpds = #();
  let class-count = 0;
  local
    // find CPD for a class, making a new one if necessary.
    method find-cpd (cl)
      block (return)
	for (x in cpds)
	  if (x.cpd-class == cl)
	    return(x);
	  end;
	end;
	compute-cpd(cl, cl.direct-superclasses);
      end;
    end method,

    method compute-cpd (cl, supers)
      let cpd = make(<class-precedence-description>, class: cl);
      cpds := pair(cpd, cpds);
      class-count := class-count + 1;
      unless (supers == #())
        let prev-super-cpd = find-cpd(supers.head);
	cpd.cpd-supers := pair(prev-super-cpd, cpd.cpd-supers);
	cpd.cpd-after := pair(prev-super-cpd, cpd.cpd-after);
	prev-super-cpd.cpd-count := prev-super-cpd.cpd-count + 1;
	for (super in supers.tail)
	  let super-cpd = find-cpd(super);
	  cpd.cpd-supers := pair(super-cpd, cpd.cpd-supers);
	  cpd.cpd-after := pair(super-cpd, cpd.cpd-after);
	  prev-super-cpd.cpd-after
	    := pair(super-cpd, prev-super-cpd.cpd-after);
	  super-cpd.cpd-count := super-cpd.cpd-count + 2;
	  prev-super-cpd := super-cpd;
	end;
      end unless;
      cpd;
    end method;
      
  let candidates = list(compute-cpd(cl, superclasses));
  let rcpl = #();

  for (index from 0 below class-count)
    if (candidates == #())
      error("Inconsistent CPL");
    end;

    local
      handle (cpd)
        candidates := remove!(candidates, cpd);
	rcpl := pair(cpd.cpd-class, rcpl);
	for (after in cpd.cpd-after)
	  if (zero?(after.cpd-count := after.cpd-count - 1))
	    candidates := pair(after, candidates);
	  end;
	end;
      end method;

    if (candidates.tail == #())
      handle(candidates.head);
    else
      // There is more than one candidate, so pick one.
      block (tie-breaker)
	for (c in rcpl)
	  let supers = c.direct-superclasses;
	  for (candidate in candidates)
	    if (member?(candidate.cpd-class, supers))
	      handle(candidate);
	      tie-breaker();
	    end if;
	  end for;
	end for;
	error("Can't happen.");
      end block;
    end if;
  end for;

  reverse!(rcpl);
end method;


// Slot inheritance.

// inherit-slots -- exported.
//
// Populate each class with complete slot information by inhereting whatever
// is necessary.
//
define method inherit-slots () => ();
  //
  // This relies on the ordering of classes in $All-Classes.  Specifically,
  // we need to process superclasses before their subclasses.
  do(inherit-slots-for, $All-Classes);
end;

define method inherit-slots-for (class :: <cclass>) => ();
  let processed = #();
  let supers = class.direct-superclasses;
  if (empty?(supers))
    class.all-slot-infos := make(<stretchy-vector>, size: 0);
  else
    let first-super = supers.first;
    processed := first-super.precedence-list;
    class.all-slot-infos :=
      map-as(<stretchy-vector>, identity, first-super.all-slot-infos);
  end;
  local
    method process (super)
      unless (member?(super, processed))
	//
	// Mark this super as processed.
	processed := pair(super, processed);
	//
	// Process the super's superclasses.
	do(process, super.direct-superclasses);
	//
	// Inherit the slots.
	for (slot in super.new-slot-infos)
	  add-slot(slot, class);
	end;
      end;
    end;
  do(process, supers);
  for (slot in class.new-slot-infos)
    reset-slot(slot);
    add-slot(slot, class);
  end;
end;

define method reset-slot (slot :: <slot-info>) => ();
  slot.slot-overrides := #();
end;

define method reset-slot (slot :: <instance-slot-info>) => ();
  slot.slot-overrides := #();
  slot.slot-representation := #f;
  slot.slot-positions := #();
  slot.slot-initialized?-slot := #f;
end;

define method add-slot (slot :: <slot-info>, class :: <cclass>) => ();
  //
  // Make sure the slot doesn't clash with some other slot with the same
  // getter.
  if (slot.slot-getter)
    for (other-slot in class.all-slot-infos)
      if (slot.slot-getter == other-slot.slot-getter)
	compiler-error("Class %= can't combine two different %s slots, "
			 "one introduced by %= and the other by %=",
		       class, slot.slot-getter.variable-name,
		       slot.slot-introduced-by, other-slot.slot-introduced-by);
      end;
    end;
  end;
  //
  // Add the slot to the all-slot-infos.
  add!(class.all-slot-infos, slot);
end;


// Override inheritance.

define method inherit-overrides ()
  for (cclass in $All-Classes)
    for (override in cclass.override-infos)
      block (next-override)
	for (slot in cclass.all-slot-infos)
	  if (override.override-getter == slot.slot-getter)
	    if (slot.slot-introduced-by == cclass)
	      compiler-error("Class %= can't both introduce and override "
			       "slot %s",
			     cclass, slot.slot-getter.variable-name);
	    end;
	    if (instance?(slot, <class-slot-info>))
	      compiler-error("Can't override class allocation slots");
	    end;
	    if (instance?(slot, <virtual-slot-info>))
	      compiler-error("Can't override virtual slots");
	    end;
	    slot.slot-overrides := pair(override, slot.slot-overrides);
	    override.override-slot := slot;
	    next-override();
	  end;
	end;
	compiler-error("Class %= can't override slot %s, because is doesn't "
			 "have that slot.",
		       cclass, override.override-getter.variable-name);
      end;
    end;
    for (slot in cclass.all-slot-infos)
      let active-overrides = #();
      for (override in slot.slot-overrides)
	if (csubtype?(cclass, override.override-introduced-by))
	  unless (any?(method (other)
			 csubtype?(other.override-introduced-by,
				   override.override-introduced-by);
		       end,
		       active-overrides))
	    active-overrides
	      := pair(override,
		      choose(method (other)
			       ~csubtype?(override.override-introduced-by,
					  other.override-introduced-by);
			     end,
			     active-overrides));
	  end;
	  unless (active-overrides.tail == #())
	    compiler-error("Class %= must override slot %s itself to resolve "
			     "the conflict in inheriting overrides from each "
			     "of %=",
			   cclass, slot.slot-getter.variable-name,
			   map(override-introduced-by, active-overrides));
	  end;
	end;
      end;
    end;
  end;
end;



// Unique ID assignment.

define constant $class-for-id = make(<object-table>);

define method set-and-record-unique-id
    (id :: false-or(<fixed-integer>), class :: <cclass>) => ();
  if (id)
    let clash = element($class-for-id, id, default: #f);
    if (clash)
      compiler-error("Can't give both %= and %= unique id %d, because then "
		       "it wouldn't be unique.",
		     clash, class, id);
    end;
    $class-for-id[id] := class;
    class.unique-id := id;
  end;
end;

define method assign-unique-ids (base :: <fixed-integer>) => ();
  local
    method grovel (class :: <cclass>, this-id :: <fixed-integer>)
	=> (next-id :: <fixed-integer>);
      let next-id = this-id;
      if (class.loaded?)
	unless (class.sealed?)
	  for (sub in class.direct-subclasses)
	    if (sub.direct-superclasses.first == class)
	      next-id := grovel(sub, next-id);
	    end;
	  end;
	end;
      else
	unless (class.abstract?)
	  if (element($class-for-id, next-id, default: #f))
	    compiler-error("Attempting to reuse unique id %d, you should pick "
			     "a different unique-id-base.",
			   next-id);
	  end;
	  $class-for-id[next-id] := class;
	  class.unique-id := next-id;
	  next-id := next-id + 1;
	end;
	for (sub in class.direct-subclasses)
	  if (sub.direct-superclasses.first == class)
	    next-id := grovel(sub, next-id);
	  end;
	end;
	if (class.sealed?)
	  block (return)
	    for (sub in class.subclasses)
	      unless (sub.abstract?
			| (sub.unique-id & this-id <= sub.unique-id))
		return();
	      end;
	    end;
	    class.subclass-id-range-min := this-id;
	    class.subclass-id-range-max := next-id - 1;
	  end;
	end;
      end;
      next-id;
    end;
  grovel(dylan-value(#"<object>"), base);
end;



// Layout tables.

define class <layout-table> (<object>)
  slot layout-length :: <fixed-integer>,
    init-value: 0, init-keyword: length:;
  slot layout-holes :: <list>,
    init-value: #(), init-keyword: holes:;
end;

define method copy-layout-table (layout :: <layout-table>)
  make(<layout-table>,
       length: layout.layout-length,
       holes: shallow-copy(layout.layout-holes));
end;

define method find-position (layout :: <layout-table>,
			     bytes :: <fixed-integer>,
			     alignment :: <fixed-integer>)
    => offset :: <fixed-integer>;
  block (return)
    for (prev = #f then remaining,
	 remaining = layout.layout-holes then remaining.tail,
	 until: remaining == #())
      unless (zero?(bytes))
	let hole = remaining.head;
	let posn = hole.head;
	let aligned = ceiling/(posn, alignment) * alignment;
	let surplus = (posn + hole.tail) - (aligned + bytes);
	if (zero?(surplus))
	  if (posn == aligned)
	    if (prev)
	      prev.tail := remaining.tail;
	    else
	      layout.layout-holes := remaining.tail;
	    end;
	  else
	    remaining.head := pair(posn, aligned - posn);
	  end;
	  return(aligned);
	elseif (positive?(surplus))
	  if (posn == aligned)
	    remaining.head := pair(aligned + bytes, surplus);
	  else
	    hole.tail := aligned - posn;
	    remaining.tail
	      := pair(pair(aligned + bytes, surplus), remaining.tail);
	  end;
	  return(aligned);
	end;
      end;
      
    finally
      let len = layout.layout-length;
      let aligned = ceiling/(len, alignment) * alignment;
      if (len < aligned)
	let new = list(pair(len, aligned - len));
	if (prev)
	  prev.tail := new;
	else
	  layout.layout-holes := new;
	end;
      end;
      layout.layout-length := aligned + bytes;
      return(aligned);
    end;
  end;
end;


// Slot representation assignment.

define method assign-slot-representations () => ();
  for (class in $All-Classes)
    for (slot in class.new-slot-infos)
      assign-slot-representation(slot);
    end;
    //
    // Now that all slots have been added, convert them into a simple
    // object vector.
    class.all-slot-infos := as(<simple-object-vector>, class.all-slot-infos);
  end;
end;

define method assign-slot-representation (slot :: <instance-slot-info>) => ();
  let rep = pick-representation(slot.slot-type, #"space");
  slot.slot-representation := rep;
  unless (slot-guaranteed-initialized?(slot, slot.slot-introduced-by)
	    | rep.representation-has-bottom-value?)
    let class = slot.slot-introduced-by;
    let init?-slot = make(<instance-slot-info>,
			  introduced-by: class,
			  type: dylan-value(#"<boolean>"),
			  getter: #f,
			  init-value: make(<literal-false>));
    slot.slot-initialized?-slot := init?-slot;
    for (subclass in class.subclasses)
      add-slot(init?-slot, subclass);
    end;
    assign-slot-representation(init?-slot);
  end;
end;



// Slot layout stuff.

define method layout-instance-slots () => ();
  do(layout-slots-for, $All-Classes);
end;

define method layout-slots-for (class :: <cclass>) => ();
  let supers = class.direct-superclasses;
  let processed :: <simple-object-vector>
    = if (empty?(supers))
	class.instance-slots-layout := make(<layout-table>);
	class.vector-slot := #f;
	class.each-subclass-slots-count := 0;
	#[];
      else
	let critical-super = supers.head;
	let critical-primary = critical-super.closest-primary-superclass;
	for (super in supers.tail)
	  let primary = super.closest-primary-superclass;
	  if (~(primary == critical-primary)
		& csubtype?(primary, critical-primary))
	    critical-super := super;
	    critical-primary := primary;
	  end;
	end;
	class.instance-slots-layout
	  := copy-layout-table(critical-super.instance-slots-layout);
	class.vector-slot := critical-super.vector-slot;
	class.each-subclass-slots-count
	  := critical-super.each-subclass-slots-count;
	for (slot in critical-super.all-slot-infos)
	  inherit-layout(slot, class, critical-super);
	end;
	critical-super.all-slot-infos;
      end;
  //
  for (slot in class.all-slot-infos)
    unless (member?(slot, processed))
      layout-slot(slot, class);
    end;
  end;
end;

define method inherit-layout
    (slot :: <slot-info>, class :: <cclass>, super :: <cclass>) => ();
  // Default method -- do nothing.
end;

define method inherit-layout
    (slot :: type-union(<instance-slot-info>, <each-subclass-slot-info>),
     class :: <cclass>, super :: <cclass>)
    => ();
  for (remaining = slot.slot-positions then remaining.tail,
       until: csubtype?(class, remaining.head.head))
  finally
    let this-offset = remaining.head.tail;
    for (remaining = remaining then remaining.tail,
	 until: csubtype?(super, remaining.head.head))
    finally
      let super-offset = remaining.head.tail;
      unless (this-offset == super-offset)
	slot.slot-positions := pair(pair(class, super-offset),
				    slot.slot-positions);
      end;
    end
  end;
end;

define method layout-slot (slot :: <slot-info>, class :: <cclass>) => ();
  // Default method -- do nothing.
end;

define method layout-slot (slot :: <instance-slot-info>, class :: <cclass>)
    => ();
  if (class.vector-slot)
    compiler-error("variable length slots must be the last slot in "
		     "the class.");
  end;
  let rep = slot.slot-representation;
  let offset = find-position(class.instance-slots-layout,
			     rep.representation-size,
			     rep.representation-alignment);
  slot.slot-positions := pair(pair(class, offset), slot.slot-positions);
end;

define method layout-slot (slot :: <vector-slot-info>, class :: <cclass>)
    => ();
  if (class.vector-slot)
    compiler-error("variable length slots must be the last slot in "
		     "the class.");
  end;
  class.vector-slot := slot;
  let rep = slot.slot-representation;
  let offset = find-position(class.instance-slots-layout, 0,
			     rep.representation-alignment);
  slot.slot-positions := pair(pair(class, offset), slot.slot-positions);
end;

define method layout-slot
    (slot :: <each-subclass-slot-info>, class :: <cclass>)
    => ();
  let posn = class.each-subclass-slots-count;
  slot.slot-positions := pair(pair(class, posn), slot.slot-positions);
  class.each-subclass-slots-count := posn + 1;
end;


// Compile time determination of slot offsets and other gunk.

define method find-slot-offset
    (slot :: <instance-slot-info>, instance-type :: <ctype>)
    => res :: false-or(<fixed-integer>);
  let instance-class
    = best-idea-of-class(instance-type) | slot.slot-introduced-by;
  if (csubtype?(instance-class.closest-primary-superclass,
		slot.slot-introduced-by))
    block (return)
      for (element in slot.slot-positions)
	if (csubtype?(instance-class, element.head))
	  return(element.tail);
	end;
      end;
      error("%= isn't in %=?", slot, instance-class);
    end;
  elseif (instance-class.sealed?)
    block (return)
      let guess = #f;
      for (element in slot.slot-positions)
	if (csubtype?(instance-class, element.head))
	  if (guess & guess ~= element.tail)
	    return(#f);
	  else
	    return(element.tail);
	  end;
	elseif (csubtype?(element.head, instance-class))
	  if (guess)
	    // At different locations in different subclasses.
	    return(#f);
	  else
	    guess := element.tail;
	  end;
	end;
      end;
      if (guess)
	guess;
      else
	error("%= isn't in %=?", slot, instance-class);
      end;
    end;
  else
    // The class isn't sealed and isn't sufficiently primary.
    #f;
  end;
end;
	  


define method slot-guaranteed-initialized?
    (slot :: <slot-info>, instance-type :: <ctype>) => res :: <boolean>;
  if (slot.slot-init-value | slot.slot-init-function
	| slot.slot-init-keyword-required?)
    #t;
  elseif (empty?(slot.slot-overrides))
    #f;
  else
    csubtype?(instance-type,
	      reduce1(ctype-union,
		      map(override-introduced-by,
			  slot.slot-overrides)));
  end;
end;
    


define method best-idea-of-class (type :: <cclass>)
    => res :: <cclass>;
  type;
end;

define method best-idea-of-class (type :: <limited-ctype>)
    => res :: <cclass>;
  base-class(type);
end;

define method best-idea-of-class (type :: <union-ctype>)
    => res :: false-or(<cclass>);
  let mems = type.members;
  if (empty?(mems))
    #f;
  else
    block (punt)
      let result = best-idea-of-class(mems.head) | punt(#f);
      for (mem in mems.tail)
	let other-base-class = best-idea-of-class(mem) | punt(#f);
	block (return)
	  for (super in result.precedence-list)
	    if (csubtype?(mem, super))
	      result := super;
	      return();
	    end;
	  end;
	end;
      end;
      result;
    end;
  end;
end;

define method best-idea-of-class (type :: <unknown-ctype>)
    => res :: <false>;
  #f;
end;



// Defined classes.

define class <defined-cclass> (<cclass>)
  //
  // The <class-definition> that installed this class.
  slot class-defn :: <class-definition>, init-keyword: defn:;
end class;


// Limited mumble classes.

define class <limited-cclass> (<cclass>)
end;


// Proxies

define class <proxy> (<ct-value>, <identity-preserving-mixin>)
  slot proxy-for :: <cclass>, required-init-keyword: for:;
end;

define constant $proxy-memo = make(<object-table>);

define method make (class == <proxy>, #next next-method, #key for: cclass)
  element($proxy-memo, cclass, default: #f)
    | (element($proxy-memo, cclass) := next-method());
end;

define method print-object (proxy :: <proxy>, stream :: <stream>) => ();
  pprint-fields(proxy, stream, for: proxy.proxy-for);
end;

define method print-message (proxy :: <proxy>, stream :: <stream>) => ();
  format(stream, "proxy for %s", proxy.proxy-for);
end;
