module: cback
rcs-header: $Header: /scm/cvs/src/d2c/compiler/cback/heap.dylan,v 1.36 2002/09/11 11:21:40 andreas Exp $
copyright: see below

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000, 2001, 2002  Gwydion Dylan Maintainers
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

// Heap Building.
//
// The file is responsible for building the initial heap and the roots
// vectors.  There are two entry points into the heap builder: one for building
// a library specific local heap, and one for building the final global heap.
// We dump as much stuff as possible in local heaps to keep the global heap
// small.  We want to keep the global heap small because the larger the global
// heap, the longer every final program will take to compile.  By moving as
// much of the effort into local heap production that we can, we only pay
// the cost for dumping the stuff when we recompile that particular library.
//
// But some objects must be dumped in the global heap.  Such objects fall
// into two categories: symbols and objects whos initial definitions depend
// on global information about the program (e.g. open classes and generic
// functions).
//
// Symbols must be dumped in the global heap because multiple different
// libraries can all independently introduce the same symbol and we have to
// make sure we only allocate a single heap object for that symbol.  If each
// library dumped their own copy of said symbol, then we couldn't implement
// == on symbols as a simple pointer comparison.
//
// Open classes must be dumped in the global heap because all classes
// contain an enumeration of their (compile-time installed) direct subclasses.
// If the class is open, then other libraries can add additional subclasses.
// If we dumped the class in a local heap, we would have no way of including
// these new subclasses in the direct-subclasses list.
//
// Also, due to the new %subclass? code, we have to dump all classes to the
// global heap, since we have to calculate the type inclusion matrix after
// we know about all of them. This step should be deferred to program
// initialization time, because this would allow us to dump classes to
// the local heap again.
//
// A related issue is slot descriptors for open classes.  Slot descriptors
// contain a ``position table'' mapping (sub)class to slot position.  But slot
// descriptors for primary classes can be eagerly dumped because the position
// table for them won't change with the addition of new subclasses.
//
// Just like there can only be one copy each symbol, there can only be one
// copy of #t, #f, #(), and $not-supplied.  But it turns out that we don't
// actually have to do anything to guarantee that.  They are all referenced
// by the Dylan library, so they will all be dumped in the Dylan library's
// local heap.  And any other library must use the Dylan library, and will
// therefore pick up the labels used for them when the Dylan library's local
// heap was built.



// <global-heap-state> -- internal.
//
// The additional information needed while dumping the final global heap.
// 
define constant <global-heap-file-state> = <file-state>;

// <local-heap-file-state> -- internal.
//
// The additional information needed while dumping a library local heap.
// 
define constant <local-heap-file-state>  = <file-state>;

// <extra-label> -- internal.
//
// Sometimes we will need to reference some ctv defined (w/ load-external: #t)
// in some other library.  We can't just pick a name and add it to the
// <constant-info> for that ctv because when we go to dump the ctv again
// we will just end up dumping a reference to the original external definition.
// Therefore, we instead use an <extra-label> object to record that we need
// an extra label added to that ctv and then dump the <extra-label>.
//
// XXX - emk - The <extra-label> code appears to be slightly stale. Back
// when the heap dumper generated assembly code, we could assign more than
// one label to an object. But now that we dump C, we can only assign
// a single label. So if an object's 'const-info-heap-labels' ever
// contains multiple (unmatched) labels, we're in trouble.
// See 'object-label', which now tries to generate unique labels for
// everything. And think about cleaning this up.
//
define class <extra-label> (<object>)
  //
  // The ctv this extra label is for.
  slot extra-label-ctv :: <ct-value>, required-init-keyword: ctv:;
  //
  // The extra label itself.
  slot extra-label-label :: <byte-string>, required-init-keyword: label:;
end class <extra-label>;

define sealed domain make(singleton(<extra-label>));
define sealed domain initialize(<extra-label>);

add-make-dumper
  (#"extra-label", *compiler-dispatcher*, <extra-label>,
   list(extra-label-ctv, ctv:, #f,
	extra-label-label, label:, #f),
   load-side-effect:
     method (extra-label :: <extra-label>) => ();
       let ctv = extra-label.extra-label-ctv;
       let label = extra-label.extra-label-label;
       let info = get-info-for(extra-label.extra-label-ctv, #f);
       unless (member?(label, info.const-info-heap-labels, test: \=))
	 info.const-info-heap-labels
	   := add(info.const-info-heap-labels, label);
       end unless;
     end method);


// The top level heap building entry points.

// build-global-heap -- exported.
// 
// Builds the global heap image.  Called after all the libraries have been
// compiled or loaded.  Dumps all the objects that were deferred during the
// dumping of the library specific local heaps.
// 
define method build-global-heap
    (undumped-objects :: <simple-object-vector>,
     state :: <global-heap-file-state>)
    => ();
  state.dumping-global-heap? := #t;
  maybe-emit-include("runtime.h", state, left: '"', right: '"');

  for (obj in undumped-objects)
    object-name(obj, state);
  end for;

  spew-objects-in-queue(state);

  let stream = state.file-body-stream;
  format(stream, "\n\n");

  spew-reference(state.symbols, *heap-rep*, "Initial Symbols", state);
  format(stream, "heapptr_t initial_symbols = ");
  write(stream, get-string(state.file-guts-stream));
  format(stream, ";\n");
  state.dumping-global-heap? := #t;
end;

// build-local-heap -- exported.
//
// Build a library specific local heap and return the set of objects skipped
// and the additional labels this heap depends on.  Starts by building the
// roots vector, and then dumps all the objects refered to by any roots.
// Except, of course, any of the objects that have to wait for the global heap
// for some reason or other.
// 
define method build-local-heap
    (unit :: <unit-state>, state :: <local-heap-file-state>)
 => (undumped :: <simple-object-vector>,
     extra-labels :: <simple-object-vector>);
  maybe-emit-include("runtime.h", state, left: '"', right: '"');

  let stream = state.file-body-stream;
  for (root in unit.unit-init-roots, index from 0)
    let name = root.root-name;
    if (root.root-comment)
      format(stream, "\n/* %s */\n", root.root-comment.clean-for-comment);
    else
      new-line(stream);
    end if;

    spew-reference(root.root-init-value, *general-rep*,
		   stringify("roots[", index, ']'),
		   state);

    if (name)
      format(stream, "descriptor_t %s =\n", name);
      write(stream, get-string(state.file-guts-stream));
      format(stream, ";\n\n");
    else
      error("build-local-heap: root %= has no name", root);
    end if;
  end;

  for (obj in unit.unit-eagerly-reference)
    object-name(obj, state);
  end for;

  spew-objects-in-queue(state);

  values(as(<simple-object-vector>, state.undumped-objects),
	 as(<simple-object-vector>, state.extra-labels));
end method build-local-heap;

// spew-objects-in-queue -- internal.
//
// Keep spewing objects until we finally drain the spew queue.
// 
define method spew-objects-in-queue (state :: <file-state>) => ();
  let stream = state.file-body-stream;
  format(stream, "\n/* heap base */\n");
  until (state.object-queue.empty?)
    let object = pop(state.object-queue);
    let info = get-info-for(object, #f);

    format(stream, "\n/* %s */\n", object.clean-for-comment);

    let labels = info.const-info-heap-labels;
    if (labels.empty?)
      error("Trying to spew %=, but it doesn't have any labels.", object);
    end if;
    if(labels.size > 1)
      format(stream, "/* ");
      for(label in labels)
	format(stream, "%s: ");
      end for;
      format(stream, "*/\n");
    end if;
    spew-object(first(labels), object, state);
  end;
end method spew-objects-in-queue;


//------------------------------------------------------------------------
//  Spew-reference
//
// This function creates a reference to some object.  In the case of literals,
// the "reference" may be the object's value, but it will usually simply be a
// symbolic name for the object.  As a side effect, this routine will add the
// object to the "to be dumped" queue (via object-name) if it has not already
// been scheduled for dumping. 
//
// The "tag" is a string which typically describes the particular slot being
// defined.  See "../base/rep.dylan" and "../base/c-rep.dylan" for hints
// concerning the meaning of "rep".
//------------------------------------------------------------------------

define generic spew-reference
    (object :: false-or(<ct-value>), rep :: <representation>,
     tag :: <byte-string>, state :: <file-state>)
    => ();

// spew-reference{<false>,<representation>}
//
// #f takes the place of a ctv when we want to reserve space for an
// uninitialized value.
// 
define method spew-reference
    (object :: <false>, rep :: <representation>,
     tag :: <byte-string>, state :: <file-state>)
    => ();
  if(rep == *general-rep*)
    format(state.file-guts-stream, "{ 0, { 0 } } /* %s */",
           tag.clean-for-comment);
  else
    format(state.file-guts-stream, "(%s) 0 /* %s */",
           rep.representation-c-type, tag.clean-for-comment);
  end if;
end;

// spew-reference{<literal>,<immediate-representation>}
//
// Representing a literal as an immediate is easy.
//
define method spew-reference
    (object :: <literal-true>, rep :: <immediate-representation>,
     tag :: <byte-string>, state :: <file-state>)
    => ();
  format(state.file-guts-stream, "1 /* %s */", tag.clean-for-comment)
end;

define method spew-reference
    (object :: <literal-false>, rep :: <immediate-representation>,
     tag :: <byte-string>, state :: <file-state>)
    => ();
  format(state.file-guts-stream, "0 /* %s */", tag.clean-for-comment)
end;

define method spew-reference
    (object :: <literal-integer>, rep :: <immediate-representation>,
     tag :: <byte-string>, state :: <file-state>)
    => ();
  format(state.file-guts-stream, "%d /* %s */", object.literal-value,
	 tag.clean-for-comment)
end;

define method spew-reference
    (object :: <literal-single-float>, rep :: <immediate-representation>,
     tag :: <byte-string>, state :: <file-state>)
    => ();
  format(state.file-guts-stream, "%s /* %s */",
	 float-to-string(object.literal-value, 8), tag.clean-for-comment)
end;

define method spew-reference
    (object :: <literal-double-float>, rep :: <immediate-representation>,
     tag :: <byte-string>, state :: <file-state>)
    => ();
  format(state.file-guts-stream, "%s /* %s */",
	 float-to-string(object.literal-value, 16), tag.clean-for-comment)
end;

define method spew-reference
    (object :: <literal-extended-float>, rep :: <immediate-representation>,
     tag :: <byte-string>, state :: <file-state>)
    => ();
  format(state.file-guts-stream, "%s /* %s */",
	 float-to-string(object.literal-value, 35), tag.clean-for-comment)
end;

define method spew-reference
    (object :: <literal-character>, rep :: <immediate-representation>,
     tag :: <byte-string>, state :: <file-state>)
    => ();
  format(state.file-guts-stream, "%d /* %s */",
	 as(<integer>, object.literal-value), tag)
end;

// spew-reference{<ct-value>,<general-representation>}
//
// Dump the full dual-word representation of the object.
// 
define method spew-reference
    (object :: <ct-value>, rep :: <general-representation>,
     tag :: <byte-string>, state :: <file-state>)
    => ();
  let cclass = object.ct-value-cclass;
  let best-rep = pick-representation(cclass, #"speed");
  let (heapptr, dataword)
    = if (instance?(best-rep, <data-word-representation>))
	values(make(<proxy>, for: cclass), literal-value(object));
      else
	values(object, 0);
      end;
  let object-name = object-name(heapptr, state);
  spew-heap-prototype(object-name, heapptr, state);
  select (object by instance?)
    <literal-integer> =>
      format(state.file-guts-stream, "{ (heapptr_t) &%s, { %d } } /* %s */", 
	     object-name, dataword, tag.clean-for-comment);
    <literal-single-float> =>
      format(state.file-guts-stream, "{ (heapptr_t) &%s, { %d } } /* %s */",
	     object-name, single-float-bits(dataword),
	     tag.clean-for-comment);
    <literal-character> =>
      format(state.file-guts-stream, "{ (heapptr_t) &%s, { %d } } /* %s */", 
	     object-name, as(<integer>, dataword),
	     tag.clean-for-comment);
    otherwise =>
      format(state.file-guts-stream, "{ (heapptr_t) &%s, { %= } } /* %s */", 
	     object-name, dataword, tag.clean-for-comment);
  end select;
end;

// spew-reference{<proxy>,<general-representation>}
//
// Reference the heap proxy object.  This method is needed because cback will
// put proxy objects in the roots vector in order to reference them.
//
define method spew-reference
    (object :: <proxy>, rep :: <general-representation>,
     tag :: <byte-string>, state :: <file-state>)
    => ();
  let object-name = object-name(object, state);
  spew-heap-prototype(object-name, object, state);
  format(state.file-guts-stream, "{ (heapptr_t) &%s, { 0 } } /* %s */",
	 object-name, tag.clean-for-comment);
end;

// spew-reference{<ct-value>,<heap-representation>}
//
// Dump a heap pointer to the object.
// 
define method spew-reference
    (object :: <ct-value>, rep :: <heap-representation>,
     tag :: <byte-string>, state :: <file-state>) => ();
  let object-name = object-name(object, state);
  spew-heap-prototype(object-name, object, state);
  format(state.file-guts-stream, "(heapptr_t) &%s /* %s */",
	 object-name, tag.clean-for-comment);
end;

// spew-reference{<ct-entry-point>,<immediate-representation>}
//
// When reference entry points, we are really referencing the C function
// that encodes the entry.  So instead of using raw-bits (as in the
// general <immediate-representation> method above) we just emit the name
// of the C function.
// 
define method spew-reference
    (object :: <ct-entry-point>, rep :: <immediate-representation>,
     tag :: <byte-string>, state :: <file-state>)
    => ();
  let info = get-info-for(object.ct-entry-point-for, state);
  let name = entry-name(object, state);
  select (object.ct-entry-point-kind)
    #"main" => maybe-emit-prototype(name, info, state);
    #"general" => maybe-emit-prototype(name, #"general", state);
    #"generic" => maybe-emit-prototype(name, #"generic", state);
    #"callback" => maybe-emit-prototype(name, info, state);
  end select;

  format(state.file-guts-stream, "%s /* %s */", name, tag.clean-for-comment);
end;

// spew-reference{<ct-entry-point>,<general-representation>}
//
// Likewise for general rep references.
// 
define method spew-reference
    (object :: <ct-entry-point>, rep :: <general-representation>,
     tag :: <byte-string>, state :: <file-state>)
    => ();
  let proxy = make(<proxy>, for: object.ct-value-cclass);
  let object-name = object-name(proxy, state);
  spew-heap-prototype(object-name, proxy, state);

  let info = get-info-for(object.ct-entry-point-for, state);
  let name = entry-name(object, state);
  select (object.ct-entry-point-kind)
    #"main" => maybe-emit-prototype(name, info, state);
    #"general" => maybe-emit-prototype(name, #"general", state);
    #"generic" => maybe-emit-prototype(name, #"generic", state);
    #"callback" => maybe-emit-prototype(name, info, state);
  end select;

  format(state.file-guts-stream, "{ (heapptr_t) &%s, { %s } }",
	 object-name, name, tag.clean-for-comment);
end;

// object-name -- internal.
// 
// Object-name returns a name for an object -- generating it if necessary.  As
// a side effect, it also checks whether the object has been dumped and queues
// it if not.
//
define method object-name (object :: <ct-value>, state :: <file-state>)
    => name :: <string>;
  let info = get-info-for(object, #f);
  unless (info.const-info-dumped?)
    //
    // The object hasn't been dumped.  So check to see if we should dump it
    // now or not.
    if (~state.dumping-global-heap? & defer-for-global-heap?(object, state))
      //
      // Nope, we need to wait for the global heap.  So remember that this
      // object wasn't dumped.
      add-new!(state.undumped-objects, object);
    else
      //
      // Dump-o-rama.  Mark it as dumped, and queue it.
      info.const-info-dumped? := #t;
      push-last(state.object-queue, object);
    end if;
    //
    // Make sure the object has at least one label.
    if (info.const-info-heap-labels.empty?)
      //
      // Make (and record) a new label.
      let label = object-label(object)
	           | stringify(state.id-prefix, state.next-id);
      state.next-id := state.next-id + 1;
      info.const-info-heap-labels := vector(label);
      //
      // If the object is defined externally and we are building a local heap,
      // then we need to record that we want the object to have an extra label.
      if (~state.dumping-global-heap?
            & object.defined-externally?)
	add!(state.extra-labels,
	     make(<extra-label>, ctv: object, label: label));
      end if;
    end if;
  end unless;
  info.const-info-heap-labels.first;
end method object-name;


// object-label -- external (created in cback)
// 
// Since the following objects are deferred to the global heap (see
// defer-for-global-heap? below), we need to try to give them them
// unique heap labels.  Otherwise, different libraries may try to give
// their own local names to the same object.

define generic object-label
    (object :: <ct-value>)
 => (label :: false-or(<byte-string>));

// By default we can't come up with a name.
//
define method object-label
    (object :: <ct-value>)
 => (label :: false-or(<byte-string>));
  #f;
end;

define method object-label
    (object :: <literal-symbol>)
 => (label :: <byte-string>);
  concatenate("SYM_",
	      string-to-c-name(as(<string>, object.literal-value)),
	      "_HEAP");
end;

define method object-label
    (object :: <ct-open-generic>)
 => (label :: <byte-string>);
  concatenate(object.ct-function-definition.defn-name.c-name-global, "_HEAP");
end;

define method object-label
    (object :: <defined-cclass>)
 => (label :: <byte-string>);
  concatenate(object.class-defn.defn-name.c-name-global, "_HEAP");
end;

define method object-label
    (object :: <slot-info>)
 => (label :: false-or(<byte-string>));
  let cclass-defn = object.slot-introduced-by.class-defn;
  let getter = object.slot-getter;
  let getter-defn = getter & getter.variable-definition;
  getter-defn
    & concatenate(cclass-defn.defn-name.c-name-global,
		  "Z", getter-defn.defn-name.c-name-global, "_SLOT_HEAP");
end;

// entry-name -- internal.
//
// Return the name of the function that corresponds to this entry point.
// The function must have been defined someplace in the C code, because
// there isn't diddly we can do about it now.
// 
define method entry-name
    (object :: <ct-entry-point>, state :: <file-state>)
 => (name :: <string>);
  let name = object.entry-point-c-name;
  name;
end;
	

// extract-base-name -- internal.
//
// Return the base-name of name.  If name is of the form ``foo+37'' then the
// base-name is foo.  Otherwise the base-name and name are the same.
// 
define function extract-base-name (name :: <byte-string>)
    => res :: <byte-string>;
  block (return)
    for (char in name, posn from 0)
      if (char == '+')
	return(copy-sequence(name, end: posn));
      end if;
    end for;
    name;
  end block;
end function extract-base-name;


// single-float-bits -- internal
//
// Turns a single-precision floating-point number into an integer,
// since we can only initialize the first element of the descriptor_t's
// dataword union.
//
define constant $single-float-precision = 24;
define constant $single-float-exponent-bits = 8;
define constant $single-float-bias = 127;

define method single-float-bits (num :: <ratio>)
 => res :: <general-integer>;
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
	  for (exponent from 0,
	       fraction = num / 2 then fraction / 2,
	       while: fraction >= 1)
	  finally
	    values(exponent, fraction);
	  end;
	else
	  for (exponent from -1 by -1,
	       fraction = num then fraction * 2,
	       while: fraction < ratio(1,2))
	  finally
	    values(exponent, fraction);
	  end;
	end;
    let biased-exponent = exponent + $single-float-bias;
    if (biased-exponent >= ash(1, $single-float-exponent-bits))
      // Overflow.
      error("<single-float> constant too large");
    end;
    if (biased-exponent <= 0)
      if (-biased-exponent >= $single-float-precision - 1)
	// Underflow.
	error("<single-float> constant too small");
      end;
      fraction := fraction / ash(1, -biased-exponent);
      biased-exponent := 0;
    end;
    let shifted-fraction
      = round/(ash(numerator(fraction), $single-float-precision),
	       denominator(fraction));
    let bits = logior(ash(as(<extended-integer>, biased-exponent),
			  $single-float-precision - 1),
		      logand(shifted-fraction,
			     ash(as(<extended-integer>, 1),
				 $single-float-precision - 1)
			       - 1));
    if (neg?)
      logior(bits,
	     ash(as(<extended-integer>, 1),
		 $single-float-precision + $single-float-exponent-bits - 1));
    else
      bits;
    end;
  end;
end;


// defer-for-global-heap? -- internal.
//
// Decide if we should be deferring the dump of this object, and queue it
// for deferred dumping if so.
// XXX - If we defer anything, we need to generate a unique label for it, too.
// See 'object-label' above.
// 
define generic defer-for-global-heap? (object :: <ct-value>, state :: <file-state>)
    => defer? :: <boolean>;

// If we are building the global heap, we can't go defering anything any
// further.
//
/*
define method defer-for-global-heap?
    (object :: <ct-value>, state :: <global-heap-file-state>)
    => defer? :: <boolean>;
  #f;
end method defer-for-global-heap?;
*/

// By default, we defer anything defined externally.  We do this under the
// assumption that nobody would have gone out of their way to use external
// references for some object unless it was critical that there be exactly
// one copy.  For us to be able to guarantee that there be exactly one copy,
// we either need to dump it in the heap for the library defining the object
// or in the global heap.
// 
define method defer-for-global-heap?
    (object :: <ct-value>, state :: <local-heap-file-state>)
    => defer? :: <boolean>;
  object.defined-externally?;
end method defer-for-global-heap?;

// Symbols, on the other hand, must always be deferred so we can correctly
// chain them together and guarantee uniqueness.
// 
define method defer-for-global-heap?
    (object :: <literal-symbol>, state :: <local-heap-file-state>)
    => defer? :: <boolean>;
  #t;
end method defer-for-global-heap?;


// Open generic functions must be deferred, because they need to be populated
// with any methods defined elsewhere.
// 
define method defer-for-global-heap?
    (object :: <ct-open-generic>, state :: <local-heap-file-state>)
    => defer? :: <boolean>;
  #t;
end method defer-for-global-heap?;

// Open classes must be deferred because they must be populated with any
// subclasses defined elsewhere.  Likewise, classes that were not dumped
// when originally defined must be deferred because we *must* not ever dump
// more than one copy.
// 
// New: dump all the classes to global heap.
define method defer-for-global-heap?
    (object :: <cclass>, state :: <local-heap-file-state>)
    => defer? :: <boolean>;
//  ~object.sealed? | object.defined-externally?;
  #t;
end method defer-for-global-heap?;

// Likewise, slot infos for open classes must be deferred because their
// position table must be populated with entries for any subclasses that
// are defined elsewhere.  Except that slots introduced by primary open classes
// can be dumped now, because the position table can't be changed by
// subclasses.
// 
define method defer-for-global-heap?
    (object :: <slot-info>, state :: <local-heap-file-state>)
    => defer? :: <boolean>;
  let class = object.slot-introduced-by;
  ~(class.all-subclasses-known? | class.primary?) | object.defined-externally?;
end method defer-for-global-heap?;



//------------------------------------------------------------------------
// Spew-object
//
// This function writes out the code for an object's value (as opposed to a
// simple reference to that value).  This serves primarily as a front end to
// "spew-instance" (described below) -- it computes the class of the object
// and a set of field values, all of which will be passed on to spew-instance.
//------------------------------------------------------------------------

define generic spew-object
    (name :: <byte-string>, object :: <ct-value>, state :: <file-state>)
 => ();


define method spew-object
    (name :: <byte-string>,
     object :: <ct-not-supplied-marker>,
     state :: <file-state>) => ();
  spew-instance(name, specifier-type(#"<not-supplied-marker>"), state);
end;

define method spew-object
    (name :: <byte-string>,
     object :: <literal-boolean>,
     state :: <file-state>) => ();
  spew-instance(name, object.ct-value-cclass, state);
end;

define method spew-object
    (name :: <byte-string>,
     object :: <literal-extended-integer>,
     state :: <file-state>) => ();
  let digits = make(<stretchy-vector>);
  local
    method repeat (remainder :: <extended-integer>);
      let (remainder :: <extended-integer>, digit :: <general-integer>)
	= floor/(remainder, 65536);
      add!(digits,
	   make(<literal-integer>,
		value: as(<extended-integer>, digit)));
      unless (if (logbit?(15, digit))
		remainder = -1;
	      else
		remainder = 0;
	      end)
	repeat(remainder);
      end;
    end;
  repeat(object.literal-value);
  spew-instance(name, specifier-type(#"<extended-integer>"), state,
		bignum-size: as(<ct-value>, digits.size),
		bignum-digit: digits);
end;

define method spew-object
    (name :: <byte-string>,
     object :: <literal-ratio>, state :: <file-state>) => ();
  let num = as(<ratio>, object.literal-value);
  spew-instance(name, object.ct-value-cclass, state,
		numerator:
		  make(<literal-extended-integer>, value: num.numerator),
		denominator:
		  make(<literal-extended-integer>, value: num.denominator));
end;

define method spew-object
    (name :: <byte-string>,
     object :: <literal-float>, state :: <file-state>) => ();
  spew-instance(name, object.ct-value-cclass, state, value: object);
end;

define method spew-object
    (name :: <byte-string>,
     object :: <literal-symbol>, state :: <file-state>) => ();
  spew-instance(name, specifier-type(#"<symbol>"), state,
		symbol-string:
		  as(<ct-value>, as(<string>, object.literal-value)),
		symbol-next: state.symbols);
  state.symbols := object;
end;

define method spew-object
    (name :: <byte-string>,
     object :: <literal-pair>, state :: <file-state>) => ();
  spew-instance(name, specifier-type(#"<pair>"), state,
		head: object.literal-head,
		tail: object.literal-tail);
end;

define method spew-object
    (name :: <byte-string>,
     object :: <literal-empty-list>, state :: <file-state>) => ();
  spew-instance(name, specifier-type(#"<empty-list>"), state,
		head: object, tail: object);
end;

define method spew-object
    (name :: <byte-string>,
     object :: <literal-simple-object-vector>,
     state :: <file-state>) => ();
  let contents = object.literal-value;
  spew-instance(name, specifier-type(#"<simple-object-vector>"), state,
		size: as(<ct-value>, contents.size),
		%element: contents);
end;

define constant $spewed-string-buffer = as(<stretchy-vector>, "\"");
define constant $spewed-string-initial-size :: <integer>
  = $spewed-string-buffer.size;

define method spew-object
    (name :: <byte-string>,
     object :: <literal-string>, state :: <file-state>) => ();
  let str = object.literal-value;
  let class = specifier-type(#"<byte-string>");
  let fields = get-class-fields(class);
  let stream = state.file-guts-stream;
  for (field in fields)
    select (field by instance?)
      <false> => #f;
      <integer> =>
	error("hole in a literal-string!");
      <instance-slot-info> =>
	select (field.slot-getter.variable-name)
	  #"%object-class" =>
	    spew-reference(class, field.slot-representation, "%object-class",
			   state);
	    format(stream, ",\n");
	  #"size" =>
	    spew-reference(as(<ct-value>, str.size), field.slot-representation,
			   "size", state);
	    format(stream, ",\n");
	  #"%element" =>
	    // The following ugly code should be immensely faster than
	    // writing a character at a time to a stream.
	    for (i :: <integer> from 0 below str.size)
	      let char = str[i];
	      select (char)
		'\\' =>
		  add!($spewed-string-buffer, '\\');
		  add!($spewed-string-buffer, '\\');
		'"' =>
		  add!($spewed-string-buffer, '\\');
		  add!($spewed-string-buffer, '"');
		'\0' =>
		  add!($spewed-string-buffer, '\\');
		  add!($spewed-string-buffer, '0');
		'\n' =>
		  add!($spewed-string-buffer, '\\');
		  add!($spewed-string-buffer, 'n');
		'\t' =>
		  add!($spewed-string-buffer, '\\');
		  add!($spewed-string-buffer, 't');
		'\b' =>
		  add!($spewed-string-buffer, '\\');
		  add!($spewed-string-buffer, 'b');
		'\r' =>
		  add!($spewed-string-buffer, '\\');
		  add!($spewed-string-buffer, 'r');
		'\f' =>
		  add!($spewed-string-buffer, '\\');
		  add!($spewed-string-buffer, 'f');
		otherwise =>
		  // characters outside of C0 and C1 are passed through
		  // as is: the rest are escaped
		  if (char >= ' ' & char <= '~')
		    add!($spewed-string-buffer, char);
		  elseif (as(<integer>, char) >= #xa1
			    & as(<integer>, char) <= #xff)
		    add!($spewed-string-buffer, char);
		  else
		    let code = as(<integer>, char);
		    // this will need to be updated when
		    // Unicode is fully supported
		    let substr = format-to-string("\\x%x", logand(code, #xff));
		    do(curry(add!, $spewed-string-buffer), substr);
		  end if;
	      end select;
	    end for;
	    add!($spewed-string-buffer, '"');
	    if(str.size > 0)
	      write(stream, as(<byte-string>, $spewed-string-buffer));
	      format(stream, ",\n");
	    end if;
	    $spewed-string-buffer.size := $spewed-string-initial-size;
	end select;
    end select;
  end for;
  spew-layout(class, state, size: str.size);
  format(state.file-body-stream, " %s = {\n", name);
  write(state.file-body-stream, get-string(state.file-guts-stream));
  format(state.file-body-stream, "};\n");
  state.file-prototypes-exist-for[name] := #t;
end method spew-object;

define method spew-object
    (name :: <byte-string>,
     object :: <union-ctype>, state :: <file-state>) => ();
  let mems = #();
  let sings = #();
  for (member in object.members)
    if (instance?(member, <singleton-ctype>))
      sings := pair(member.singleton-value, sings);
    else
      mems := pair(member, mems);
    end;
  end;
  spew-instance(name, specifier-type(#"<union>"), state,
		union-members: make(<literal-simple-object-vector>,
				    contents: mems,
				    sharable: #t),
		union-singletons: make(<literal-simple-object-vector>,
				       contents: sings,
				       sharable: #t));
end;

define method spew-object
    (name :: <byte-string>,
     object :: <limited-integer-ctype>, state :: <file-state>) => ();
  local method make-lit (x :: false-or(<general-integer>))
	  if (x == #f)
	    as(<ct-value>, x);
	  else
	    let min-int = ash(as(<extended-integer>, -1),
			      *current-target*.platform-integer-length - 1);
	    let max-int = lognot(min-int);
	    if (x < min-int | x > max-int)
	      make(<literal-extended-integer>, value: x);
	    else
	      make(<literal-integer>, value: x);
	    end if;
	  end if;
	end method make-lit;
  spew-instance(name, specifier-type(#"<limited-integer>"), state,
		limited-integer-base-class: object.base-class,
		limited-integer-minimum: make-lit(object.low-bound),
		limited-integer-maximum: make-lit(object.high-bound));
end;

define method spew-object
    (name :: <byte-string>,
     object :: <limited-collection-ctype>, state :: <file-state>) => ();
  spew-instance(name, specifier-type(#"<limited-collection>"), state,
                limited-integer-base-class: object.base-class,
		limited-element-type: object.element-type,
                limited-size-restriction: if (object.size-or-dimension)
                        make(<literal-integer>, 
                             value: as(<extended-integer>,
                             object.size-or-dimension));
                      else
                        as(<ct-value>, #f);
                      end if,
                limited-dimensions: #f);
end method spew-object;

define method spew-object
    (name :: <byte-string>,
     object :: <singleton-ctype>, state :: <file-state>) => ();
  spew-instance(name, specifier-type(#"<singleton>"), state,
		singleton-object: object.singleton-value);
end;

define method spew-object
    (name :: <byte-string>,
     object :: <direct-instance-ctype>, state :: <file-state>) => ();
  spew-instance(name, specifier-type(#"<direct-instance>"), state,
		direct-instance-of: object.base-class);
end;

define method spew-object
    (name :: <byte-string>,
     object :: <byte-character-ctype>, state :: <file-state>) => ();
  spew-instance(name, specifier-type(#"<byte-character-type>"), state);
end;

define method spew-object
    (name :: <byte-string>,
     object :: <subclass-ctype>, state :: <file-state>) => ();
  spew-instance(name, specifier-type(#"<subclass>"), state,
		subclass-of: object.subclass-of);
end;


// spew-object{<byte-string>,<defined-cclass>,<file-state>}
// and helpers.
//
define generic spew-class
    (name :: <byte-string>,
     state :: <file-state>,
     metaclass :: false-or(<meta-cclass>),
     slots :: <simple-object-vector>)
   => ();

// Specific metaclass.
define method spew-class
    (name :: <byte-string>,
     state :: <file-state>,
     metaclass :: <meta-cclass>,
     slots :: <simple-object-vector>) => ();
  apply(spew-instance, name, metaclass, state,
	%object-class: class-ctype(),
	slots);
end;

// Standard metaclass.
define method spew-class
    (name :: <byte-string>,
     state :: <file-state>,
     metaclass == #f,
     slots :: <simple-object-vector>) => ();
  apply(spew-instance, name, class-ctype(), state, slots);
end;

define method spew-object
    (name :: <byte-string>,
     object :: <defined-cclass>, state :: <file-state>) => ();

  local method sharable-literal-vector(vec :: <sequence>)
	  => literal-vector :: <literal-simple-object-vector>;
	  make(<literal-simple-object-vector>,
		contents: vec,
		sharable: #t)
	end,
	method as-ct-value(slot :: <function>)
	  => value :: <ct-value>;
	  as(<ct-value>, object.slot)
	end;

  let defn = object.class-defn;
  
  spew-class
    (name, state,
     object.class-metaclass,
     vector
       (class-name:
	  make(<literal-string>,
	       value: as(<byte-string>,
			 object.cclass-name.name-symbol)),
	unique-id:
	  as(<ct-value>, object.unique-id | -1),
	direct-superclasses:
	  sharable-literal-vector(object.direct-superclasses),
	all-superclasses:
	  sharable-literal-vector(object.precedence-list),
	closest-primary-superclass: object.closest-primary-superclass,
	direct-subclasses:
	  make(<literal-list>, contents: object.direct-subclasses),
	class-functional?: functional?.as-ct-value,
	class-primary?: primary?.as-ct-value,
	class-abstract?: abstract?.as-ct-value,
	class-sealed?: sealed?.as-ct-value,
	class-deferred-evaluations:
	  defn.class-defn-deferred-evaluations-function
	  | as(<ct-value>, #f),
	class-key-defaulter:
	  defn.class-defn-key-defaulter-function
	  | as(<ct-value>, #f),
	class-maker: defn.class-defn-maker-function
	  | as(<ct-value>, #f),
	class-new-slot-descriptors:
	  sharable-literal-vector(object.new-slot-infos),
	class-slot-overrides:
	  sharable-literal-vector(object.override-infos),
	class-all-slot-descriptors:
	  sharable-literal-vector(object.all-slot-infos),
	class-bucket: bucket.as-ct-value,
	class-row: sharable-literal-vector
		     (map(curry(as, <ct-value>), object.row))));
end;

define method spew-object
    (name :: <byte-string>,
     object :: <meta-cclass>, state :: <file-state>) => ();
end;

define method spew-object
    (name :: <byte-string>,
     object :: <slot-info>, state :: <file-state>) => ();
  spew-instance(name, specifier-type(#"<slot-descriptor>"), state,
		slot-name:
		  as(<ct-value>,
		     object.slot-getter
		       & as(<byte-string>, variable-name(object.slot-getter))),
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
		    as(<ct-value>, as(<list>, object.slot-positions));
		  end if,
		slot-representation:
		  if (instance?(object, <instance-slot-info>))
		    as(<ct-value>,
		       object.slot-representation.representation-name);
		  end if,
		slot-initialized?-slot:
		  if (instance?(object, <instance-slot-info>))
		    as(<ct-value>, object.slot-initialized?-slot);
		  end if);
end method spew-object;

define method spew-object
    (name :: <byte-string>,
     object :: <override-info>, state :: <file-state>) => ();
  spew-instance(name, specifier-type(#"<override-descriptor>"), state,
		override-init-value:
		  if (instance?(object.override-init-value, <ct-value>))
		    object.override-init-value;
		  end,
		override-init-function:
		  if (instance?(object.override-init-function, <ct-value>))
		    object.override-init-function;
		  end);
end method spew-object;

define method spew-object
    (name :: <byte-string>,
     object :: <defined-cdclass>, state :: <file-state>) => ();
  let defn = object.class-defn;
  spew-instance(name, specifier-type(#"<designator-class>"), state,
		class-name:
		  make(<literal-string>,
		       value: as(<byte-string>,
				 object.cclass-name.name-symbol)),
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
		class-deferred-evaluations:
		  defn.class-defn-deferred-evaluations-function
		  | as(<ct-value>, #f),
		class-key-defaulter:
		  defn.class-defn-key-defaulter-function
		  | as(<ct-value>, #f),
		class-maker: defn.class-defn-maker-function
		  | as(<ct-value>, #f),
		class-new-slot-descriptors:
		  make(<literal-simple-object-vector>,
		       contents: object.new-slot-infos,
		       sharable: #t),
		class-slot-overrides:
		  make(<literal-simple-object-vector>,
		       contents: object.override-infos,
		       sharable: #t),
		class-all-slot-descriptors:
		  make(<literal-simple-object-vector>,
		       contents: object.all-slot-infos,
		       sharable: #t),
		class-bucket: as(<ct-value>, object.bucket),
		class-row:
		  make(<literal-simple-object-vector>,
		       contents: map(curry(as,<ct-value>), object.row),
		       sharable: #t),
		size-of: as(<ct-value>, object.size-of),
		alignment-of: as(<ct-value>, object.alignment-of),
		referenced-type: object.referenced-type | as(<ct-value>, #f),
		class-struct-slot-descriptors:
		  make(<literal-simple-object-vector>,
		       contents: object.struct-slot-infos,
		       sharable: #t));
end;

define method spew-object
    (name :: <byte-string>,
     object :: <struct-slot-info>, state :: <file-state>) => ();
  spew-instance(name, specifier-type(#"<struct-slot-descriptor>"), state,
		c-type: object.struct-slot-c-type,
		offset: as(<ct-value>, object.struct-slot-offset));
end;

define method spew-object
    (name :: <byte-string>,
     object :: <proxy>, state :: <file-state>) => ();
  spew-reference(object.proxy-for, *heap-rep*, "%object-class", state);
  format(state.file-body-stream, "heapptr_t %s =\n", name);
  write(state.file-body-stream, get-string(state.file-guts-stream));
  format(state.file-body-stream, ";\n");
  state.file-prototypes-exist-for[name] := #t;
end;

define method spew-object
    (name :: <byte-string>,
     object :: <ct-function>, state :: <file-state>) => ();
  if (object.has-general-entry?)
    spew-function(name, object, state,
		  general-entry: make(<ct-entry-point>,
				      for: object, kind: #"general"));
  else
    spew-function(name, object, state);
  end if;
end;

define method spew-object
    (name :: <byte-string>,
     object :: <ct-callback-function>, state :: <file-state>) => ();
  spew-function(name, object, state,
		callback-entry: make(<ct-entry-point>,
				     for: object, kind: #"callback"),
		callback-signature: make(<literal-string>,
					 value: callback-signature(object)));
end;

define method callback-signature(func :: <ct-callback-function>)
 => (signature :: <byte-string>);
  let sig = func.ct-function-signature;
  let args = sig.specializers;
  let returns = sig.returns;

  let callback-sig = make(<byte-string>, size: sig.specializers.size + 1);

  callback-sig[0] := callback-signature-key(returns);
  for(type in args, i = 1 then i + 1)
    callback-sig[i] := callback-signature-key(type);
  end;
    
  callback-sig;
end;
    
define method callback-signature-key (type :: <values-ctype>)
 => (key :: <byte-character>);
  if (type == empty-ctype())
    'v';
  elseif (type.min-values ~== 1)
    'v';
  else
    let rep = pick-representation(type, #"speed");
    select(rep)
      *general-rep* => 'o';
      *heap-rep* => 'h';
      *boolean-rep* => 'B';
      
      *long-rep* => 'l';
      *int-rep* => 'i';
      *uint-rep* => 'u';
      *short-rep* => 's';
      *ushort-rep* => 't';
      *byte-rep* => 'c';
      *ubyte-rep* => 'b';
      
      *ptr-rep* => 'p';
      
      *float-rep* => 'f';
      *double-rep* => 'd';
      *long-double-rep* => 'D';
      otherwise =>
	error("Couldn't find a callback signature key for representation %=",
	      rep);
    end;
  end;
end;

define method spew-object
    (name :: <byte-string>,
     object :: <ct-generic-function>, state :: <file-state>) => ();
  let defn = object.ct-function-definition;
  let discriminator = defn.generic-defn-discriminator;
  apply(spew-function, name, object, state,
	general-entry: object.gf-generic-entry-point,
	if (discriminator)
	  #[];
	else
	  vector(generic-function-methods:
		 make(<literal-list>,
		      contents: remove(map(ct-value,
					   generic-defn-methods(defn)),
				       #f), 
		      sharable: #f));
	end if);
end;

// method-general-entry -- internal.
//
// Utility routine to find the <ct-entry-point> to use for the given method's
// general entry.  Basically, if the method is hidden (i.e. inside a generic)
// we use the main entry for general-call if general-call is defined and we
// leave the entry uninitialized if general-call is not.  If the method
// is not hidden, then it will have a custom built general entry, so we use
// that.
// 
define method method-general-entry (meth :: <ct-method>)
    => entry :: false-or(<ct-entry-point>);
  if (meth.ct-method-hidden?)
    let tramp = dylan-defn(#"general-call");
    if (tramp)
      make(<ct-entry-point>, for: tramp.ct-value, kind: #"main");
    else
      #f;
    end;
  else
    meth.has-general-entry?
      & make(<ct-entry-point>, for: meth, kind: #"general");
  end if;
end method method-general-entry;

define method spew-object
    (name :: <byte-string>,
     object :: <ct-method>, state :: <file-state>) => ();
  spew-function(name, object, state,
		general-entry: method-general-entry(object),
		generic-entry:
		  (object.has-generic-entry?
		     & make(<ct-entry-point>, for: object, kind: #"generic")));
end;

define method spew-object
    (name :: <byte-string>,
     object :: <ct-accessor-method>, state :: <file-state>)
 => ();
  let standin = object.ct-accessor-standin;
  spew-function(name, object, state,
		general-entry: method-general-entry(object),
		generic-entry:
		  if (standin)
		    make(<ct-entry-point>, for: standin, kind: #"main");
		  elseif (object.has-generic-entry?)
		    make(<ct-entry-point>, for: object, kind: #"generic");
		  end,
		accessor-slot: object.ct-accessor-method-slot-info);
end;

// Spew-function is a slightly lower-level front-end to "spew-instance".  It
// automatically fills in the various slots which are common to all function
// objects. 
//
define method spew-function
    (name :: <byte-string>,
     func :: <ct-function>, state :: <file-state>, #rest slots) => ();
  let sig = func.ct-function-signature;
  let returns = sig.returns;
  let positionals = returns.positional-types;
  let min-values = returns.min-values;
  apply(spew-instance, name, func.ct-value-cclass, state,
	function-name:
	  make(<literal-string>, value:
	       format-to-string("%s", func.ct-function-name)),
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



// Spew-instance is the workhorse function which actually writes out the value
// for an object.  Given a class and a sequence of slot values, it spews
// C code which creates a new object instance and provides values for
// each slot.  Slot values may be specified explicitly as a keyword/value in
// "slots".  Any slot which is not explicitly specified will be filled in with
// a default value.
//
define function spew-instance
    (name :: <byte-string>,
     class :: <cclass>, state :: <file-state>, #rest slots) => ();
  let stream = state.file-guts-stream;
  let vector-size = #f;
  for (field in get-class-fields(class))
    select (field by instance?)
      <false> => #f;
      <integer> =>
	format(stream, "{ ");
	for (count from 0 below field)
	  format(stream, "0, ");
	end for;
	format(stream, "}, /* hole */\n");
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
	  unless (instance?(len-ctv, <literal-integer>))
	    error("Bogus length: %=", len-ctv);
	  end;
	  vector-size := as(<integer>, len-ctv.literal-value);
	  if (vector-size > 0)
	    format(stream, "{\n");
	    indent(stream, $indentation-step);
	    if (instance?(init-value, <sequence>))
	      unless (init-value.size == vector-size)
		error("Size mismatch.");
	      end;
	      for (element in init-value,
		   index from 0)
		spew-reference(element, field.slot-representation,
			       stringify(name, '[', index, ']'),
			       state);
		format(stream, ",\n");
	      end;
	    else
	      for (index from 0 below vector-size)
		spew-reference(init-value, field.slot-representation,
			       stringify(name, '[', index, ']'),
			     state);
		format(stream, ",\n");
	      end;
	    end;
	    indent(stream, -$indentation-step);
	    format(stream, "},\n");
	  end if;
        else
	  spew-reference(init-value, field.slot-representation, name, state);
	  format(stream, ",\n");
	end;
    end;
  end;
  spew-layout(class, state, size: vector-size);
  format(state.file-body-stream, " %s = {\n", name);
  write(state.file-body-stream, get-string(state.file-guts-stream));
  format(state.file-body-stream, "};\n");
  state.file-prototypes-exist-for[name] := #t;
end;

// layouter-cclass -- internal.
// Return the <cclass> for a ctv that is containing the layout information
// for that ctv.
//
define generic layouter-cclass (object :: <ct-value>) => res :: <cclass>;

define method layouter-cclass (object :: <ct-value>) => res :: <cclass>;
  object.ct-value-cclass;
end;

define method layouter-cclass (class :: <cclass>, #next next-method) => res
:: <cclass>;
  class.class-metaclass | next-method();
end;

// spew-heap-prototype is like maybe-emit-prototype except that it
// writes out a prototype for the actual literal instead of the root.
//
define method spew-heap-prototype
    (name :: <byte-string>, defn :: <ct-value>, state :: <file-state>)
 => did :: <boolean>;
  unless (element(state.file-prototypes-exist-for, name, default: #f))
    let stream = state.file-body-stream;
    let cclass = defn.layouter-cclass;
    format(stream, "extern ");
    spew-layout(cclass, state, size: literal-vector-size(defn));
    format(stream, " %s;\n\n", name);
    state.file-prototypes-exist-for[name] := #t;
  end unless;
end method;

define method spew-heap-prototype
    (name :: <byte-string>, defn :: <proxy>, state :: <file-state>)
 => did :: <boolean>;
  unless (element(state.file-prototypes-exist-for, name, default: #f))
    let stream = state.file-body-stream;
    format(stream, "extern heapptr_t %s;\n\n", name);
    state.file-prototypes-exist-for[name] := #t;
  end unless;
end method;

// spew-layout emits the C struct name corresponding to the layout of
// the given <cclass>.  Classes with "vector" slots get a different
// layout for each distinct vector size.
//
define method spew-layout
    (class :: <cclass>, state :: <file-state>, #key size)
 => ();
  let stream = state.file-body-stream;
  let classname = class.cclass-name.c-name-global;
  let name = if(size)
               stringify(classname, "_SIZE", size);
             else
               classname;
             end if;
  if(element(state.file-layouts-exist-for, name, default: #f))
    format(stream, "struct %s", name);
  else
    format(stream, "struct %s {\n", name);
    let holes = 0;
    let slots = 0;
    for (field in get-class-fields(class))
      select (field by instance?)
        <false> => #f;
        <integer> =>
          holes := holes + 1;
          format(stream, "    unsigned char HOLE%d[%d];\n", holes, field);
        <instance-slot-info> =>
          let getter = field.slot-getter;
          let name = if (getter)
                       string-to-c-name(as(<string>, getter.variable-name));
                     else
                       slots := slots + 1;
                       stringify("SLOT", slots);
                     end if;
          unless(instance?(field, <vector-slot-info>) & size = 0)
            format(stream, "    %s %s",
                   field.slot-representation.representation-c-type,
                   name);
            if(instance?(field, <vector-slot-info>))
              format(stream, "[%d]", size);
            end if;
            if(getter)
              format(stream, ";\t /* %s */\n",
                     getter.variable-name.clean-for-comment);
            else
              format(stream, ";\n");
            end if;
          end unless;
      end select;
    end for;
    format(stream, "}");
    state.file-layouts-exist-for[name] := #t;
  end if;
end method;

// Returns the vector size of the given compile-time literal, or #f
// if it is not a vector type.
//
define method literal-vector-size 
    (object :: <ct-value>) => (size :: false-or(<integer>));
  #f;
end method;

define method literal-vector-size
    (object :: <literal-extended-integer>) => (size :: false-or(<integer>));
  let vector-size = 0;
  local
    method repeat (remainder :: <extended-integer>);
      vector-size := vector-size + 1;
      let (remainder :: <extended-integer>, digit :: <general-integer>)
	= floor/(remainder, 65536);
      unless (if (logbit?(15, digit))
		remainder = -1;
	      else
		remainder = 0;
	      end)
	repeat(remainder);
      end;
    end;
  repeat(object.literal-value);
  vector-size;
end method;

define method literal-vector-size
    (object :: <literal-simple-object-vector>)
 => (size :: false-or(<integer>));
  object.literal-value.size;
end method;

define method literal-vector-size
    (object :: <literal-string>)
 => (size :: false-or(<integer>));
  object.literal-value.size;
end method;

  
define method get-class-fields (class :: <cclass>)
    => res :: <simple-object-vector>;
  if (class.class-heap-fields)
    class.class-heap-fields;
  else
    if (~instance?(class, <meta-cclass>) & class.abstract?) // ### put this in spew-class???? FIXME???
      error("Spewing an instance of an abstract class?");
    end;
    let layout = class.instance-slots-layout;
    let fields = make(<vector>, size: layout.layout-length + 1, fill: #f);
    for (slot in class.all-slot-infos)
      if (instance?(slot, <instance-slot-info>))
	let posn = get-direct-position(slot.slot-positions, class);
	unless (posn)
	  error("Can't find the position for %= in %s?", slot, class)
	end unless;
	fields[posn] := slot;
      end if;
    end for;
    for (hole in layout.layout-holes)
      fields[hole.head] := hole.tail;
    end;
    class.class-heap-fields := fields;
  end if;
end method get-class-fields;

// Returns the value which should be used to initialize the heap for an
// instance slot.  This value may be specified in the "slots" key/value
// sequence, or it may be computed based upon the intial-value specification
// for the slot.
//
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
    // savings are large. Can be customized by providing it as
    // the first pair in slots.
    if (slot.slot-introduced-by == object-type)
      return(if (slots.size >= 2 & slots[0] == #"%object-class")
		slots[1]
	     else
		class
	     end if);
    end if;

    // Check to see whether the caller provided an explict value for this
    // slot. 
    let getter = slot.slot-getter;
    let slot-name = getter & getter.variable-name;
    if (getter)
      for (index :: <integer> from 0 below slots.size by 2)
	if (slots[index] == slot-name)
	  let val = slots[index + 1];
	  if (val)
	    return(val);
	  end;
	end;
      end;
    end;

    // Find the default value for this slot in this class.  This involves
    // searching the list of "overrides" to determine whether any "inherited
    // slot" specification provided new default values for this slot in this
    // class.  If not, we use the default value supplied in the initial slot
    // defintion.
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


// Object-size.

// object-size -- internal.
//
// Return the size (in bytes) of the compile-time value.
// 
define generic object-size (ctv :: <ct-value>) => size :: <integer>;

// object-size{<ct-value>} -- method on internal GF.
//
// For most kinds of ctvs the size is static and hung off the class'es
// instance slot layout table.
// 
define method object-size (ctv :: <ct-value>) => size :: <integer>;
  let class = ctv.layouter-cclass;
  assert(~class.vector-slot);
  class.instance-slots-layout.layout-length;
end method object-size;

// object-size{<literal-extended-integer>} -- method on internal GF.
//
define method object-size (ctv :: <literal-extended-integer>)
    => size :: <integer>;
  object-size-from-length
    (ctv, ceiling/(ctv.literal-value.integer-length + 1, 8));
end method object-size;

// object-size{<literal-vector>} -- method on internal GF.
//
define method object-size (ctv :: <literal-vector>)
    => size :: <integer>;
  object-size-from-length(ctv, ctv.literal-value.size);
end method object-size;

// object-size-from-length -- internal.
//
// Compute the size of the ctv assuming that there are the number of elements
// given in elements in the instance's vector slot.
// 
define function object-size-from-length
    (ctv :: <ct-value>, elements :: <integer>)
    => size :: <integer>;
  let class = ctv.layouter-cclass;
  let vector-slot = class.vector-slot;
  assert(vector-slot);
  get-direct-position(vector-slot.slot-positions, class)
    + vector-slot.slot-representation.representation-size * elements;
end function object-size-from-length;

// object-size{<proxy>} -- method on internal GF.
//
// Proxies are magical and in fact, only have one slot: the class.  So we just
// return the amount of storage needed to store the class.
// 
define method object-size (ctv :: <proxy>)
    => size :: <integer>;
  *heap-rep*.representation-size;
end method object-size;

