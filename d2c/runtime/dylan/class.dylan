rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/class.dylan,v 1.1 1998/05/03 19:55:37 andreas Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
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
//    University.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports, questions, comments, and suggestions should be sent by
// E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
//
//======================================================================

define class <class> (<type>)
  //
  constant slot class-name :: false-or(<byte-string>),
    init-value: #f, init-keyword: debug-name:;
  //
  constant slot unique-id :: <integer>, init-value: -1;
  //
  // The direct superclasses.
  constant slot direct-superclasses :: <simple-object-vector>,
    required-init-keyword: superclasses:;
  //
  // The Class Precedence List.  We are the first and <object> is the last.
  constant slot all-superclasses :: <simple-object-vector>,
    required-init-keyword: all-superclasses:;
  //
  // The primary superclass that is closest to us in the hierarchy (including
  // ourself).
  constant slot closest-primary-superclass :: <class>,
    required-init-keyword: closest-primary-superclass:;
  //
  // The direct subclasses.
  slot direct-subclasses :: <list>,
    init-value: #();
  //
  // Boolean properties of classes.
  constant slot class-functional? :: <boolean>,
    init-value: #f, init-keyword: functional:;
  constant slot class-primary? :: <boolean>,
    init-value: #f, init-keyword: primary:;
  constant slot class-abstract? :: <boolean>,
    init-value: #f, init-keyword: abstract:;
  constant slot class-sealed? :: <boolean>,
    init-value: #t, init-keyword: sealed:;
  //
  // The defered evaluations for this class.
  slot class-defered-evaluations :: false-or(<function>),
    init-value: #f;
  //
  // The key defaulter function, or #f if no key defaults.
  slot class-key-defaulter :: false-or(<function>),
    init-value: #f;
  //
  // The maker function, or #f if we haven't computed it yet.
  slot class-maker :: false-or(<function>),
    init-value: #f;
  //
  // Vector of the slots introduced by this class.
  constant slot class-new-slot-descriptors :: <simple-object-vector>,
    required-init-keyword: slots:;
/*
  //
  // Vector of keyword initialization arguments introduced by this class.
  constant slot class-keyword-init-args :: <simple-object-vector>,
    required-init-keyword: keyword-init-args:;
*/
  //
  // Vector of inherited slot overrides introduced by this class.
  constant slot class-slot-overrides :: <simple-object-vector>,
    required-init-keyword: slot-overrides:;
  //
  // Vector of all the slots for this class.  Filled in for real when defered-
  // evaluations are processed.
  slot class-all-slot-descriptors :: <simple-object-vector>,
    init-value: #[];
/*
  //
  // Layout of instance allocation slots.  #f until computed.
  slot class-instance-layout :: type-union(<false>, <layout>),
    init-value: #f;
  //
  // Layout of each-subclass allocation slots.  #f until computed or if there
  // are no each-subclass slots.
  slot class-each-subclass-layout :: type-union(<false>, <layout>),
    init-value: #f;
  //
  // Vector of each-subclass allocation slots.  Filled in when the layout
  // is computed.
  slot class-each-subclass-slots :: <simple-object-vector>;
*/
end;

/*
define method initialize (class :: <class>, #key)
  class.all-superclasses := compute-cpl(class, class.direct-superclasses);
  class.closest-primary-superclass := find-closest-primary-superclass(class);
end;
*/

define constant <slot-allocation>
  = one-of(#"instance", #"class", #"each-subclass", #"virtual");

define class <slot-descriptor> (<object>)
  //
  // Some human-readable name for this slot, if one is available.
  constant slot slot-name :: false-or(<byte-string>) = #f,
    init-keyword: name:;
  //
  // How this slot is to be allocated.
  constant slot slot-allocation :: <slot-allocation>,
    required-init-keyword: allocation:;
  //
  // The type of the slot, or uninitialized if deferred.
  slot slot-type :: <type>,
    init-keyword: type:;
/*
  //
  // The function to compute the type when deferred.
  slot slot-deferred-type :: type-union(<false>, <function>),
    required-init-keyword: deferred-type:;
  //
  // The getter generic function.  Also used to identify the slot.
  constant slot slot-getter :: <generic-function>,
    required-init-keyword: getter:;
  //
  // The method added to that generic function, or #f if it either hasn't
  // beed added yet or isn't going to be added ('cause of virtual allocation).
  slot slot-getter-method :: type-union(<false>, <method>),
    init-value: #f;
  //
  // the setter generic function, or #f if there isn't one.
  constant slot slot-setter :: type-union(<false>, <generic-function>),
    init-value: #f;
  //
  // The method added to the setter generic function if one had been added.
  slot slot-setter-method :: type-union(<false>, <method>),
    init-value: #f;
*/
  //
  // The function to compute the initial value, or #f if there isn't one.
  slot slot-init-function :: type-union(<false>, <function>),
    init-value: #f;
  //
  // The init-value, or $not-supplied if there isn't one.
  slot slot-init-value :: <object>,
    init-value: $not-supplied;
  //
  // The init keyword, if there is one.
  constant slot slot-init-keyword :: type-union(<false>, <symbol>),
    init-value: #f;
  //
  // #t if the init-keyword is required, #f if not.
  constant slot slot-init-keyword-required? :: <boolean>,
    init-value: #f;
  //
  // A-list mapping classes to positions for the slot.  An entry is for all
  // subclasses of the key class, therefore more specific classes must preceed
  // less specific classes.  Positions are either integers for heap allocated
  // slots or #"data-word" for slots allocated in the data-word.
  slot slot-positions :: <list>,
    init-value: #();
  //
  // Either a self-organizing linked list mapping specific classes to slot
  // offsets or the one and only offset that is ever used.  Use of this avoids
  // having to do the subtype? tests that checking slot-positions directly
  // entails.
  slot slot-positions-cache
    :: type-union(<position-cache-node>, <integer>, one-of(#f, #"data-word")),
    init-value: #f;
end;

/*
define method initialize
    (slot :: <slot-descriptor>,
     #key setter :: type-union(<false>, <generic-function>),
     type :: type-union(<false>, <type>),
     deferred-type :: type-union(<false>, <function>),
     init-value = $not-supplied,
     init-function :: type-union(<false>, <function>),
     init-keyword :: type-union(<false>, <symbol>),
     required-init-keyword :: type-union(<false>, <symbol>),
     allocation :: <slot-allocation> = #"instance")
    => res :: <slot-descriptor>;

  // Check the consistency of the various init options.
  if (required-init-keyword)
    if (init-value ~= $not-supplied)
      error("Can't mix init-value: and required-init-keyword:");
    elseif (init-function)
      error("Can't mix init-function: and required-init-keyword:");
    elseif (init-keyword)
      error("Can't mix init-keyword: and required-init-keyword:");
    end;
  elseif (init-value ~= $not-supplied)
    if (init-function)
      error("Can't mix init-value: and init-function:");
    end;
  end;

  // Check the consistency of the various type options.
  if (deferred-type & type)
    error("Can't mix type: and deferred-type:");
  end;

  
end;
*/

define class <position-cache-node> (<object>)
  //
  // The class this node is an entry for.
  constant slot cache-class :: <class>,
    required-init-keyword: class:;
  //
  // The position this slot shows up at in the above class (and subclasses).
  constant slot cache-position
      :: type-union(<integer>, singleton(#"data-word")),
    required-init-keyword: position:;
  //
  // The next node in the position cache.
  slot cache-next :: false-or(<position-cache-node>),
    required-init-keyword: next:;
end class <position-cache-node>;

define sealed domain make (singleton(<position-cache-node>));
define sealed domain initialize (<position-cache-node>);


define class <override-descriptor> (<object>)
  //
  // The init-value, or $not-supplied if there is none.
  slot override-init-value :: <object> = $not-supplied,
    init-keyword: init-value:;
  //
  // The init-function, or #f if there is none.
  slot override-init-function :: false-or(<function>) = #f,
    init-keyword: init-function:;
end class <override-descriptor>;

define sealed domain make (singleton(<override-descriptor>));
define sealed domain initialize (<override-descriptor>);


/*
define method make (class == <class>,
		    #key superclasses :: type-union(<class>, <sequence>)
		           = <object>,
		         slots :: <sequence> = #())
  let slots = map-as(<simple-object-vector>,
		     curry(apply, make, <slot-descriptor>),
		     slots);
  next-method(superclasses:
		select (superclasses by instance?)
		  <class> =>
		    vector(superclasses);
		  <sequence> =>
		    as(<simple-object-vector>, superclasses);
		end,
	      slots: slots);
end;

*/


// Class precedence list computation.

/*

define class <cpd> (<object>)
  slot cpd-class :: <class>, required-init-keyword: class:;
  slot cpd-supers :: <list>, init-value: #();
  slot cpd-after :: <list>, init-value: #();
  slot cpd-count :: <integer>, init-value: 0;
end;

define method compute-cpl (class :: <class>, supers :: <list>)
    => res :: <list>;
  if (supers == #())
    list(class);
  elseif (supers.tail == #())
    pair(class, supers.head.all-superclasses);
  else
    slow-compute-cpl(class, supers);
  end;
end;

define method slow-compute-cpl (class :: <class>, supers :: <list>)
    => res :: <list>;
  let cpds = make(<table>);
  let class-count = 0;
  local
    method compute-cpd (class :: <class>, supers :: <list>)
      let cpd = make(<cpd>, class: class);
      cpds[class] := cpd;
      class-count := class-count + 1;
      if (supers != #())
	let prev-super-cpd = find-cpd(supers.head);
	cpd.cpd-supers := pair(prev-super-cpd, cpd.cpd-supers);
	cpd.cpd-after := pair(prev-super-cpd, cpd.cpd-after);
	prev-super-cpd.cpd-count := prev-super-cpd.cpd-count + 1;
	for (super :: <class> in supers.tail)
	  let super-cpd = find-cpd(super);
	  cpd.cpd-supers := pair(super-cpd, cpd.cpd-supers);
	  cpd.cpd-after := pair(super-cpd, cpd.cpd-after);
	  prev-super-cpd.cpd-after := pair(super-cpd,prev-super-cpd.cpd-after);
	  super-cpd.cpd-count := super-cpd.cpd-count + 2;
	  prev-super-cpd := super-cpd;
	end;
      end;
      cpd;
    end,
    method find-cpd (class :: <class>)
      element(cpds, class, default: #f)
	| compute-cpd(class, class.direct-superclasses);
    end;
  let candidates = list(compute-cpd(class, supers));
  let rcpl = #();
  for (count :: <integer> from 0 below class-count)
    let candidate
      = if (candidates == #())
	  error("Inconsistent CPL");
	elseif (candidates.tail == #())
	  candidates.head;
	else
	  tie-breaker(candidates, rcpl);
	end;
    candidates := remove!(candidates, candidate);
    rcpl := pair(candidate.cpd-class, rcpl);
    for (after in candidate.cpd-after)
      if (zero?(after.cpd-count := after.cpd-count - 1))
	candidates := pair(after, candidates);
      end;
    end;
  end;
  reverse!(rcpl);
end;

define method tie-breaker (candidates :: <list>, rcpl :: <list>)
    => candidate :: <cpd>;
  block (return)
    for (class in rcpl)
      let supers = class.direct-superclasses;
      for (candidate in candidates)
	if (member?(candidate.cpd-class, supers))
	  return(candidate);
	end;
      end;
    end;
    lose("Can't happen.\n");
  end;
end;

*/


// Find-closest-primary-superclass

/*

define method find-closest-primary-superclass (class :: <class>)
    => res :: <class>;
  let closest-primary = #f;
  for (super in class.direct-superclasses)
    let other-primary = super.closest-primary-superclass;
    if (~closest-primary | subtype?(other-primary, closest-primary))
      closest-primary := other-primary;
    elseif (~subtype?(closest-primary, other-primary))
      error("Can't mix ~= and ~= because they are both primary",
	    closest-primary, other-primary);
    end;
  end;
  if (class.class-primary?)
    class;
  elseif (closest-primary)
    closest-primary;
  else
    lose("<object> isn't being inherited or isn't primary?");
  end;
end;

*/


// Slot methods.

// find-slot-offset -- internal
//
// Calls to this are introduced by the compiler when it cannot statically
// determine where a slot is located.
// 
define method find-slot-offset (class :: <class>, slot :: <slot-descriptor>)
    => offset :: type-union(<integer>, singleton(#"data-word"));
  block (return)
    let cache = slot.slot-positions-cache;
    if (instance?(cache, <integer>) | cache == #"data-word")
      return(cache);
    end if;
    for (prev :: false-or(<position-cache-node>) = #f then node,
	 node :: false-or(<position-cache-node>) = cache then node.cache-next,
	 while: node)
      if (node.cache-class == class)
	if (prev)
	  prev.cache-next := node.cache-next;
	  node.cache-next := cache;
	  slot.slot-positions-cache := node;
	end if;
	return(node.cache-position);
      end if;
    end for;
    let positions = slot.slot-positions;
    if (positions.tail == #())
      return(slot.slot-positions-cache := positions.head.tail);
    end if;
    for (entry :: <list> in positions)
      let entry-class :: <class> = entry.head;
      if (subtype?(class, entry-class))
	let position = entry.tail;
	let node = make(<position-cache-node>,
			class: class, position: position,
			next: slot.slot-positions-cache);
	slot.slot-positions-cache := node;
	return(position);
      end if;
    end for;
    lose("Can't find the position for %s in %s.",
	 slot, class.class-name);
  end block;
end method find-slot-offset;

// slot-initialized? -- exported.
//
// Return #t if the slot has been initialized, and #f if not.
// 
define method slot-initialized?
    (instance :: <object>, getter :: <generic-function>)
    => res :: <boolean>;
  error("### runtime slot-initialized? not yet implemented.");
end;


// Layout stuff.

/*

define method compute-layout (class :: <class>) => ();
  let direct-supers = class.direct-superclasses;
  //
  // First of all, clone the layout info for the first direct superclass.
  // 
  // Note: we can assume that there will be at least one superclass because
  // everything must inherit at least <object>, and <object> itself is set
  // up by the linker.
  let first-super :: <class> = first(direct-supers);
  let processed :: <list> = first-super.all-superclasses;
  let instance-layout = clone-layout(first-super.class-instance-layout);
  let each-subclass-layout
    = (first-super.class-each-subclass-layout
	 & clone-layout(first-super.class-each-subclass-layout));
  let all-slots = 
  //
  // Now, add all the slots for classes picked up from the additional
  // superclasses.
  local method process-super (super)
	  unless (member?(super, processed))
	    processed := pair(super, processed);
	    
	    do(process-super, super.direct-superclasses);
	  end;
	end;
  for (index from 1 below direct-supers.size)
    process-super(direct-supers[index]);
  end;

*/



// The default make method.

define open generic make (type :: <type>, #rest supplied-keys, #all-keys)
    => instance;

define method make (class :: <class>, #rest supplied-keys, #all-keys)
    => instance;
  if (class.class-abstract?)
    error("Can't make instances of %= because it is abstract.", class);
  end;
  maybe-do-defered-evaluations(class);
  let defaulted-keys :: <simple-object-vector>
    = if (class.class-key-defaulter)
	class.class-key-defaulter(supplied-keys);
      else
	supplied-keys;
      end;
  /*
  let valid-keys = class.class-valid-init-keywords;
  for (index from 0 below defaulted-keys.size by 2)
    let key = defaulted-keys[index];
    unless (member?(key, valid-keys))
      error("Invalid initialization keyword %= in make of %=", key, class);
    end;
  end;
  */
  let instance = apply(class.class-maker, defaulted-keys);
  apply(initialize, instance, defaulted-keys);
  instance;
end;

define inline method maybe-do-defered-evaluations (class :: <class>) => ();
  if (class.class-defered-evaluations)
    do-defered-evaluations(class);
  end;
end;

define method do-defered-evaluations (class :: <class>) => ();
  let defered-evaluations = class.class-defered-evaluations;
  class.class-defered-evaluations
    := method () => res :: <never-returns>;
	 error("Circularity detected while processing the defered evaluations "
		 "for %=",
	       class);
       end;
  defered-evaluations();
  class.class-defered-evaluations := #f;
end;

define open generic initialize (instance :: <object>, #rest keys, #all-keys);

define inline method initialize (instance :: <object>, #all-keys) => ();
end;


