module: classes
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/cclass.dylan,v 1.3 1995/04/25 02:49:45 wlott Exp $
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


define abstract class <cclass> (<ctype>, <eql-ct-value>)
  //
  // The name, for printing purposes.
  slot cclass-name :: <name>, required-init-keyword: name:;

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

  // Type describing direct instances of this class.
  slot direct-type :: <direct-instance-ctype>;

  // class precedence list of all classes inherited, including this class and
  // indirectly inherited classes.  Unbound if not yet computed.
  slot precedence-list :: <list>;

  // List of all known subclasses (including this class and indirect
  // subclasses).  If sealed, then this is all of 'em.
  slot subclasses :: <list>, init-value: #();
  //
  // The representation of instances of this class or #f if we haven't
  // picked them yet.
  slot speed-representation :: union(<false>, <representation>),
    init-value: #f;
  slot space-representation :: union(<false>, <representation>),
    init-value: #f;
  //
  // Vector of <slot-info>s for the slots introduced by this class.
  slot new-slot-infos :: <simple-object-vector>,
    required-init-keyword: slots:;
  //
  // Vector of all the slots in instances of this class, in no particular
  // order. Filled in when the slot layouts are computed.
  slot all-slot-infos :: <vector>;
  //
  // Layout of the instance slots.  Filled in when the slot layouts are
  // computed.
  slot instance-slots-layout :: <layout-table>;
  //
  // Count of the number of each-subclass slots.
  slot each-subclass-slots-count :: <fixed-integer>;
end class;

define method initialize (obj :: <cclass>, #key slots)
  // Record the class.
  add!($All-Classes, obj);

  // Make the direct-type for this cclass.
  obj.direct-type := make(<direct-instance-ctype>, base-class: obj);

  // Compute the cpl.
  let supers = obj.direct-superclasses;
  let cpl = compute-cpl(obj, supers);
  obj.precedence-list := cpl;

  // Add us to all our superclasses subclass lists.
  for (super in cpl)
    super.subclasses := pair(obj, super.subclasses);
  end;

  // Find the closest primary superclass.  Note: we don't have to do any
  // error checking, because that is done for us in defclass.dylan.
  if (obj.primary?)
    obj.closest-primary-superclass := obj;
  else
    let closest = #f;
    for (super in obj.direct-superclasses)
      let primary-super = super.closest-primary-superclass;
      if (~closest | csubtype?(primary-super, closest))
	closest := primary-super;
      end;
    end;
    obj.closest-primary-superclass := closest;
  end;

  // Fill in introduced-by for the slots.
  for (slot in slots)
    slot.slot-introduced-by := obj;
  end;
end;

define method print-object (cclass :: <cclass>, stream :: <stream>) => ();
  pprint-fields(cclass, stream, name: cclass.cclass-name);
end;



define constant <slot-allocation>
  = one-of(#"instance", #"class", #"each-subclass", #"constant", #"virtual");

define abstract class <slot-info> (<object>)
  //
  // The cclass that introduces this slot.  Not required, because we have to
  // make the regular slots before we can make the cclass that defines them.
  slot slot-introduced-by :: <cclass>,
    init-keyword: introduced-by:;
  //
  // The type of this slot.  Possibly an unknown type.  Possibly not filled in
  // until later.
  slot slot-type :: <ctype>, init-keyword: type:;
  //
  // The getter generic function definition.  Used for slot identity.  If #f,
  // that means that the slot is an auxiliary slot hung off some other slot,
  // and therefore doesn't need additional identity information.
  slot slot-getter :: union(<false>, <variable>),
    required-init-keyword: getter:;
  //
  // The init-value, or #f if none.
  slot slot-init-value :: union(<false>, <ct-value>),
    init-value: #f, init-keyword: init-value:;
  //
  // The init-function, or #f if none.
  slot slot-init-function :: union(<false>, <definition>),
    init-value: #f, init-keyword: init-function:;
  //
  // The init-keyword, of #f if none.
  slot slot-init-keyword :: union(<false>, <literal-symbol>),
    init-value: #f, init-keyword: init-keyword:;
  //
  // #t if the init-keyword is required, #f if not.
  slot slot-init-keyword-required? :: <boolean>,
    init-value: #f, init-keyword: init-keyword-required:;
end;

define method make (class == <slot-info>, #rest keys, #key allocation)
    => res :: <slot-info>;
  apply(make,
	select (allocation)
	  #"instance" => <instance-slot-info>;
	  #"class" => <class-slot-info>;
	  #"each-subclass" => <each-subclass-slot-info>;
	  #"constant" => <constant-slot-info>;
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
define method initialize (info :: <slot-info>, #next next-method,
			  #key allocation)
  next-method();
end;

define class <instance-slot-info> (<slot-info>)
  slot slot-representation :: union(<representation>, <false>),
    init-value: #f;
  slot slot-positions :: <list>,
    init-value: #();
  slot slot-initialized?-slot :: union(<false>, <instance-slot-info>),
    init-value: #f;
end;

define class <class-slot-info> (<slot-info>)
end;

define class <each-subclass-slot-info> (<slot-info>)
  slot slot-positions :: <list>,
    init-value: #();
end;

define class <constant-slot-info> (<slot-info>)
end;

define class <virtual-slot-info> (<slot-info>)
end;


// Ctype operations.

define method csubtype-dispatch (type1 :: <cclass>, type2 :: <cclass>)
    => result :: <boolean>;
  member?(type2, type1.precedence-list);
end method;

define method csubtype-dispatch
    (type1 :: <cclass>, type2 :: <direct-instance-ctype>)
    => result :: <boolean>;
  type1 == type1.base-class & type1.sealed? & empty?(type1.subclasses);
end;

define method ctype-intersection-dispatch(type1 :: <cclass>, type2 :: <cclass>)
    => (result :: <ctype>, precise :: <boolean>);
  if (type1.sealed? & type2.sealed?)
    values(reduce(ctype-union, empty-ctype(),
		  intersection(type1.subclasses, type2.subclasses)),
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
    add-slot(slot, class);
  end;
end;

define method add-slot (slot :: <slot-info>, class :: <cclass>) => ();
  //
  // Make sure the slot doesn't clash with some other slot with the same
  // getter.
  if (slot.slot-getter)
    for (other-slot in class.all-slot-infos)
      if (slot.slot-getter == other-slot.slot-getter)
	compiler-error("Class %= can't combine two different %= slots, "
			 "one introduced by %= and the other by %=",
		       class, slot.slot-getter,
		       slot.slot-introduced-by, other-slot.slot-introduced-by);
      end;
    end;
  end;
  //
  // Add the slot to the all-slot-infos.
  add!(class.all-slot-infos, slot);
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
      let hole = remaining.head;
      let posn = hole.head;
      let aligned = ceiling/(posn, alignment) * alignment;
      let surplus = (aligned + bytes) - (posn + hole.tail);
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
	  remaining.tail
	    := pair(pair(aligned + bytes, surplus), remaining.tail);
	end;
	return(aligned);
      end;
    finally
      let len = layout.layout-length;
      let aligned = ceiling/(len, alignment) * alignment;
      if (len < aligned)
	if (prev)
	  prev.tail := list(len, aligned - len);
	else
	  layout.layout-holes := list(len, aligned - len);
	end;
      end;
      layout.layout-length := aligned + bytes;
      return(aligned);
    end;
  end;
end;


// Slot layout stuff.

define method layout-instance-slots () => ();
  do(layout-slots-for, $All-Classes);
end;

define method layout-slots-for (class :: <cclass>) => ();
  //
  // Start with a duplicate of the layout of the first superclass.
  let supers = class.direct-superclasses;
  if (empty?(supers))
    class.instance-slots-layout := make(<layout-table>);
    class.each-subclass-slots-count := 0;
  else
    let first-super = supers.first;
    class.instance-slots-layout
      := copy-layout-table(first-super.instance-slots-layout);
    class.each-subclass-slots-count := first-super.each-subclass-slots-count;
  end;
  //
  // We only want to layout the slots that are already added to the class.
  // If laying out any of these slots causes more slots to be added (e.g.
  // unbound? slots) whoever adds them will also lay them out.
  let slots = class.all-slot-infos;
  let orig-len = slots.size;
  for (index from 0 below orig-len)
    layout-slot(slots[index], class);
  end;
  //
  // Now that all slots have been added and layed out, convert them into
  // a simple object vector.
  class.all-slot-infos := as(<simple-object-vector>, class.all-slot-infos);
end;

define method layout-slot (slot :: <slot-info>, class :: <cclass>) => ();
  // Default method -- do nothing.
end;

define method layout-slot (slot :: <instance-slot-info>, class :: <cclass>)
    => ();
  unless (slot.slot-representation)
    let rep = pick-representation(slot.slot-type, #"space");
    slot.slot-representation := rep;
    unless (slot.slot-init-value
	      | slot.slot-init-function
	      | slot.slot-init-keyword-required?
	      | rep.representation-has-bottom-value?)
      slot.slot-initialized?-slot
	:= make(<instance-slot-info>,
		introduced-by: class,
		type: dylan-value(#"<boolean>"),
		getter: #f,
		init-value: make(<literal-false>));
    end;
  end;
  if (slot.slot-initialized?-slot)
    add-slot(slot.slot-initialized?-slot, class);
    layout-slot(slot.slot-initialized?-slot, class);
  end;
  let rep = slot.slot-representation;
  let offset = find-position(class.instance-slots-layout,
			     rep.representation-size,
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


// Defined classes.

define class <defined-cclass> (<cclass>)
end class;


// Limited mumble classes.

define class <limited-cclass> (<cclass>)
end;
