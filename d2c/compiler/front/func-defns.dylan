module: function-definitions
rcs-header: $Header: /scm/cvs/src/d2c/compiler/front/func-defns.dylan,v 1.1 1998/05/03 19:55:28 andreas Exp $
copyright: Copyright (c) 1996  Carnegie Mellon University
	   All rights reserved.

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

define method defn-type (defn :: <function-definition>) => res :: <cclass>;
  dylan-value(#"<function>");
end;

define class <generic-definition> (<function-definition>)
  //
  // #f iff the open adjective wasn't supplied.
  slot generic-defn-sealed? :: <boolean>, init-keyword: sealed:,
    init-function:
      curry(error, "sealed: unsupplied in make of <generic-definition>");
  //
  // All the <method-definition>s defined on this generic function.
  slot generic-defn-methods :: <list>,
    init-value: #(), init-keyword: methods:;
  //
  // List of all the seals on this generic function.  Each seal is a list of
  // <seal-info>s.
  slot generic-defn-seals :: <list>,
    init-value: #(), init-keyword: seals:;
  //
  // The discriminator ct-value, if there is one.
  slot %generic-defn-discriminator
    :: type-union(<ct-function>, one-of(#f, #"not-computed-yet")),
    init-value: #"not-computed-yet", init-keyword: discriminator:;
end;

define open generic generic-defn-discriminator (gf :: <generic-definition>)
    => res :: false-or(<ct-function>);

define method defn-type (defn :: <generic-definition>) => res :: <cclass>;
  dylan-value(#"<generic-function>");
end;

// definition-kind{<generic-definition>} -- method on exported GF
//
define method definition-kind
    (defn :: <generic-definition>) => kind :: <byte-string>;
  "generic function";
end method definition-kind;

define class <implicit-generic-definition>
    (<generic-definition>, <implicit-definition>)
  //
  // Implicit generic definitions are sealed.
  inherited slot generic-defn-sealed?, init-value: #t;
end;

define open abstract class <abstract-method-definition> (<function-definition>)
  //
  // The <function-literal> to clone when inlining this method, #f if we can't
  // inline it, and #"not-computed-yet" if we haven't tried yet.
  slot %method-defn-inline-function
    :: type-union(<function-literal>, <function>, <false>) = #f,
    init-keyword: inline-function:;
end;

define method method-defn-inline-function
    (defn :: <abstract-method-definition>)
    => res :: false-or(<function-literal>);
  let res = defn.%method-defn-inline-function;
  if (instance?(res, <function>))
    defn.%method-defn-inline-function := res(defn);
  else
    res;
  end if;
end method method-defn-inline-function;


define method defn-type (defn :: <abstract-method-definition>)
    => res :: <cclass>;
  dylan-value(#"<method>");
end;

define class <method-definition> (<abstract-method-definition>)
  //
  // The generic function this method is part of, or #f if the base-name is
  // undefined or not a generic function.
  slot method-defn-of :: false-or(<generic-definition>),
    init-value: #f, init-keyword: method-of:;
  //
  // True if this method is congruent with the corresponding GF.
  slot method-defn-congruent? :: <boolean>,
    init-value: #f, init-keyword: congruent:;
end;

// definition-kind{<method-definition>} -- method on exported GF
//
define method definition-kind
    (defn :: <method-definition>) => kind :: <byte-string>;
  "method";
end method definition-kind;

define class <accessor-method-definition> (<method-definition>)
  slot accessor-method-defn-slot-info :: false-or(<slot-info>),
    required-init-keyword: slot:;
end;

define class <getter-method-definition> (<accessor-method-definition>)
end;

define class <setter-method-definition> (<accessor-method-definition>)
end;


define method make
    (class :: one-of(<method-definition>, <accessor-method-definition>,
		     <getter-method-definition>, <setter-method-definition>),
     #next next-method, #rest keys,
     #key base-name, signature, hairy: hairy?,
          movable: movable?, flushable: flushable?)
    => res :: <method-definition>;
  if (base-name)
    let var = find-variable(base-name);
    let generic-defn
      = if (var & instance?(var.variable-definition, <generic-definition>))
	  var.variable-definition;
	end;
    apply(next-method, class,
	  name: make(<method-name>,
		     generic-function: base-name,
		     specializers: signature.specializers),
	  hairy: hairy?,
	  movable: movable?
	    | (generic-defn & generic-defn.function-defn-movable?),
	  flushable: flushable?
	    | (generic-defn & generic-defn.function-defn-flushable?),
	  method-of: generic-defn,
	  keys);
  else
    next-method();
  end;
end;

// This method exists just so make will recognize base-name as a valid
// init keyword.
// 
define method initialize
    (defn :: <method-definition>, #next next-method, #key base-name) => ();
  next-method();
end;



// Seal processing.

define class <seal-info> (<object>)
  slot seal-types :: <list>, required-init-keyword: types:;
  slot seal-methods :: false-or(<list>), init-value: #f;
end class <seal-info>;


define method add-seal
    (defn :: <generic-definition>, library :: <library>, types :: <list>,
     sloc :: false-or(<source-location-mixin>))
    => ();
  block (return)
    let specs = defn.function-defn-signature.specializers;
    if (specs.size ~== types.size)
      compiler-warning-location
        (sloc, "Wrong number of types in seal, wanted %d but got %d",
	 specs.size, types.size);
      return();
    end if;
    let bogus? = #f;
    for (spec in specs, type in types, index from 0)
      if (instance?(type, <unknown-ctype>))
	compiler-warning-location
	  (sloc, "Type for arg %d in seal is unknown, hence ignoring seal.",
	   index);
	return();
      end;
      unless (instance?(spec, <unknown-ctype>)
		| csubtype?(type, spec))
	compiler-warning-location
	  (sloc, "bad type in seal: %s is not a subtype of gf type %s",
	   type, spec);
	bogus? := #t;
      end;
    end;
    if (bogus?)
      return();
    end;
    let new-seals = list(make(<seal-info>, types: types));
    for (old-seal in defn.generic-defn-seals)
      let all-subtype? = #t;
      let all-supertype? = #t;
      for (type1 in types,
	   type2 in old-seal.seal-types,
	   all-subtype? = #t then all-subtype? & csubtype?(type1, type2),
	   all-supertype? = #t then all-supertype? & csubtype?(type2, type1),
	   while: all-subtype? | all-supertype?)
      finally
	if (all-subtype?)
	  // The new seal is completely enclosed by an existing seal.  Ignore
	  // the new one.
	  return();
	elseif (all-supertype?)
	  // The new seal completely encloses the old seal.  Ignore the old
	  // one.
	  begin end;
	else
	  // We need them both.
	  new-seals := pair(old-seal, new-seals);
	end if;
      end for;
    end for;
    defn.generic-defn-seals := new-seals;
  end block;
end;

define method add-seal
    (defn :: <generic-definition>, library :: <library>, types :: <sequence>,
     sloc :: false-or(<source-location-mixin>))
    => ();
  add-seal(defn, library, as(<list>, types), sloc);
end;



// Congruence testing.

// Check congruence at one position, returning #t if definitely congruent.
// Meth and GF are for error context.
//
define method check-1-arg-congruent
    (mspec :: <values-ctype>, gspec :: <values-ctype>,
     wot :: <byte-string>,
     meth :: <method-definition>, gf :: <generic-definition>)
    => res :: <boolean>;
  let (val, val-p) = values-subtype?(mspec, gspec);
  case
    ~val-p =>
      compiler-warning-location
	(meth,
	 "Can't tell if %s %s is a subtype of %s,\n"
	 "so can't tell if method:\n"
	 "  %s\n"
	 "is congruent to GF\n"
	 "  %s",
	 wot, mspec, gspec, meth.defn-name, gf.defn-name);
      #f;

    ~val =>
      compiler-warning-location
	(meth,
	 "Method \n  %s \n"
	 "isn't congruent to GF\n   %s \n"
	 "because method %s type %s isn't a subtype of GF type %s.",
	 meth.defn-name, gf.defn-name, wot, mspec, gspec);
      #f;

    otherwise => #t;
  end case;
end method;


// Check that the methods on GF are congruent to it, and return the methods
// that are congruent.
//
define method check-congruence
    (meth :: <method-definition>, gf :: <generic-definition>)
    => res :: <boolean>;
  let gsig = gf.function-defn-signature;
  let gspecs = gsig.specializers;
  let msig = meth.function-defn-signature;
  let win = #t;

  let mspecs = msig.specializers;
  unless (size(mspecs) = size(gspecs))
    compiler-warning-location
      (meth,
       "Method %s has different number of required arguments than GF %s.",
       meth.defn-name, gf.defn-name);
    win := #f;
  end;
  for (mspec in mspecs, gspec in gspecs)
    win := check-1-arg-congruent(mspec, gspec, "argument", meth, gf) & win;
  end for;

  case
    gsig.key-infos =>
      if (~msig.key-infos)
	compiler-warning-location
	  (gf,
	   "GF %s accepts keywords but method %s doesn't.",
	   gf.defn-name, meth.defn-name);
	win := #f;
      elseif (~msig.all-keys?)
	for (gkey in gsig.key-infos)
	  let gkey-name = gkey.key-name;
	  let gspec = gkey.key-type;
	  block (found-it)
	    for (mkey in msig.key-infos)
	      if (mkey.key-name == gkey-name)
		win := check-1-arg-congruent(mkey.key-type, gspec,
					     "keyword arg", meth, gf)
		         & win;
		found-it();
	      end;
	    end for;
	    
	    compiler-warning-location
	      (gf,
	       "GF %s mandatory keyword arg %= is not accepted by method %s.",
	       gf.defn-name, gkey-name, meth.defn-name);
	    win := #f;
	  end block;
	end for;
      end if;

    msig.key-infos =>
      compiler-warning-location
	(meth,
	 "Method %s accepts keywords but GF %s doesn't.",
	 meth.defn-name, gf.defn-name);
      win := #f;

    gsig.rest-type & ~msig.rest-type =>
      compiler-warning-location
	(meth,
	 "GF %s accepts variable arguments, but method %s doesn't.",
	 gf.defn-name, meth.defn-name);
      win := #f;

    ~gsig.rest-type & msig.rest-type =>
      compiler-warning-location
	(meth,
	 "Method %s accepts variable arguments, but GF %s doesn't.",
	 meth.defn-name, gf.defn-name);
      win := #f;
  end;

  win & check-1-arg-congruent(msig.returns, gsig.returns, "result", meth, gf);
end method;


define method ct-add-method
    (gf :: <generic-definition>, meth :: <method-definition>)
    => ();
  if (gf.generic-defn-sealed?
	& gf.defn-library ~== meth.defn-library)
    compiler-warning-location
      (meth, "Library %s can't define methods on sealed generic %s.",
       meth.defn-library.library-name, gf.defn-name);
    meth.function-defn-hairy? := #t;
  else
    let old-methods = gf.generic-defn-methods;
    let meth-specs = meth.function-defn-signature.specializers;
    block (return)
      for (old-meth in old-methods)
	if (meth-specs = old-meth.function-defn-signature.specializers)
	  compiler-warning-location
	    (meth,
	     "%s is multiply defined -- ignoring extra definition.",
	     meth.defn-name);
	  return();
	end;
      end;
      gf.generic-defn-methods := pair(meth, old-methods);
    end;
    if (check-congruence(meth, gf))
      meth.method-defn-congruent? := #t;
      unless (empty?(gf.function-defn-transformers))
	install-transformers(meth, gf.function-defn-transformers);
      end unless;
    else
      meth.function-defn-hairy? := #t;
    end;
  end if;
end method ct-add-method;

define method install-transformers
    (gf :: <generic-definition>, transformers :: <list>, #next next-method)
    => ();
  next-method();
  for (meth in gf.generic-defn-methods)
    install-transformers(meth, transformers);
  end for;
end method install-transformers;

define method install-transformers
    (meth :: <method-definition>, transformers :: <list>, #next next-method)
    => ();
  let meth-specs = meth.function-defn-signature.specializers;
  let new-transformers
    = choose(method (transformer)
	       let specs = transformer.transformer-specializers;
	       specs == #f | specs = meth-specs;
	     end,
	     transformers);
  unless (empty?(new-transformers))
    next-method(meth, new-transformers);
  end unless;
end method install-transformers;


// ct-applicable-methods

define method ct-applicable-methods
    (gf :: <generic-definition>, call-types :: <list>)
    => (definitely :: false-or(<list>), maybe :: false-or(<list>));
  let seal-info = find-seal(gf, call-types);
  if (seal-info)
    let definitely-applicable = #();
    let maybe-applicable = #();
    for (meth in seal-info.seal-methods | compute-seal-methods(seal-info, gf))
      block (next)
	let definitely? = #t;
	for (call-type in call-types,
	     spec in meth.function-defn-signature.specializers)
	  if (instance?(spec, <unknown-ctype>))
	    definitely? := #f;
	  else
	    let spec-extent = spec.ctype-extent;
	    unless (csubtype?(call-type, spec-extent))
	      if (ctypes-intersect?(call-type, spec-extent))
		definitely? := #f;
	      else
		next();
	      end if;
	    end unless;
	  end if;
	end for;
	if (definitely?)
	  definitely-applicable := pair(meth, definitely-applicable);
	else
	  maybe-applicable := pair(meth, maybe-applicable);
	end if;
      end block;
    end for;
    values(definitely-applicable, maybe-applicable);
  else
    // The argument types arn't covered by a seal.
    values(#f, #f);
  end if;
end method;


define method find-seal (gf :: <generic-definition>, call-types :: <list>)
    => res :: false-or(<seal-info>);
  block (return)
    for (seal-info in gf.generic-defn-seals)
      block (next)
	for (call-type in call-types, seal-type in seal-info.seal-types)
	  unless (csubtype?(call-type, seal-type.ctype-extent))
	    next();
	  end unless;
	end for;
	return(seal-info);
      end block;
    end for;
    #f;
  end block;
end method find-seal;


define method compute-seal-methods
    (seal-info :: <seal-info>, gf :: <generic-definition>)
    => methods :: <list>;
  let types = seal-info.seal-types;
  let methods = #();
  for (meth in gf.generic-defn-methods)
    block (next)
      for (type in types,
	   spec in meth.function-defn-signature.specializers)
	unless (ctypes-intersect?(type, spec))
	  next();
	end unless;
      end for;
      methods := pair(meth, methods);
    end block;
  end for;
  seal-info.seal-methods := methods;
end method compute-seal-methods;


// method sorting and comparision utilities.

// sort-methods
//
// This routine takes a set of methods and sorts them by some subset of the
// arguments.
// 
define method sort-methods
    (methods :: <list>, arg-classes :: false-or(<simple-object-vector>))
    => (ordered :: false-or(<list>), ambiguous :: false-or(<list>));

  block (return)

    // Ordered accumulates the methods we can tell the ordering of.  Each
    // element in this list is either a method or a list of equivalent methods.
    let ordered = #();

    // Ambiguous accumulates the set of methods of which it is unclear which
    // follows next after ordered.  These methods will all be mutually
    // ambiguous or equivalent.
    let ambiguous = #();

    for (meth in methods)
      block (done-with-method)
	for (remaining = ordered then remaining.tail,
	     prev = #f then remaining,
	     until: remaining == #())
	  //
	  // Grab the method to compare this method against.  If the next
	  // element in ordered is a list of equivalent methods, grab the first
	  // one as characteristic.
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
	      if (prev)
		prev.tail := #();
	      else
		ordered := #();
	      end if;
	      if (instance?(remaining.head, <pair>))
		ambiguous := pair(meth, remaining.head);
	      else
		ambiguous := list(meth, remaining.head);
	      end;
	      done-with-method();
	    #"unknown" =>
	      compiler-warning-location
	        (meth,
		 "Can't statically determine the ordering of %s "
		 "and %s and both are applicable.",
		 meth.defn-name, other.defn-name);
	      return(#f, #f);
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
	      #"unknown" =>
		compiler-warning-location
		  (meth,
		   "Can't statically determine the ordering of "
		   "%s and %s and both are applicable.",
		   meth.defn-name, remaining.head.defn-name);
		return(#f, #f);
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

    values(ordered, ambiguous);
  end;
end method sort-methods;


define method compare-methods
    (meth1 :: <method-definition>, meth2 :: <method-definition>,
     arg-classes :: false-or(<simple-object-vector>))
    => res :: one-of(#"more-specific", #"less-specific", #"unordered",
		     #"disjoint", #"ambiguous", #"unknown");
  let specs1 = meth1.function-defn-signature.specializers;
  let specs2 = meth2.function-defn-signature.specializers;
  block (return)
    let more-specific? = #f;
    let less-specific? = #f;
    let ambiguous? = #f;
    let unknown? = #f;
    for (index from 0,
	 spec1 in specs1,
	 spec2 in specs2)
      //
      // If this is an argument that we are actually sorting by, check to
      // see what kind of ordering this arg imposes.
      let arg-class = arg-classes & arg-classes[index];
      if (arg-classes == #f | arg-class)
	if (spec1 == spec2)
	  // The two specializers are identical, so they impose no ordering.
	  #f;
	elseif (csubtype?(spec1, spec2))
	  // Spec1 is more specific.
	  more-specific? := #t;
	elseif (csubtype?(spec2, spec1))
	  // Spec2 is more specific.
	  less-specific? := #t;
	elseif (~ctypes-intersect?(spec1, spec2))
	  // They are disjoint.
	  return(#"disjoint");
	elseif (instance?(spec1, <cclass>) & instance?(spec2, <cclass>))
	  // Neither argument is a subclass of the other.  So we have to
	  // base it on the precedence list of the actual argument class.
	  if (arg-classes)
	    block (found)
	      for (super in arg-class.precedence-list)
		if (super == spec1)
		  more-specific? := #t;
		  found();
		elseif (super == spec2)
		  less-specific? := #t;
		  found();
		end;
	      finally
		error("bug in compare-methods: %s and %s are not in %s's "
			"precedence list",
		      spec1, spec2, arg-class);
	      end for;
	    end block;
	  else
	    // We don't know the actual argument class, so we can't know what
	    // kind of ordering this position would impose.
	    unknown? := #t;
	  end if;
	elseif (instance?(spec1, <subclass-ctype>)
		  & instance?(spec2, <subclass-ctype>))
	  // If both specializers are subclass-ctypes, then the ordering
	  // depends on their relative positions in the cpl of the actual
	  // argument, so knowning the argument class isn't good enough.  So
	  // we just have to give the answer up as unknown.
	  unknown? := #t;
	elseif (instance?(spec1, <unknown-ctype>)
		  | instance?(spec2, <unknown-ctype>))
	  // If either specializer is unknown, then we can't know what kind of
	  // ordering it would impose.
	  unknown? := #t;
	else
	  // Neither argument is a subtype of the other and we have a
	  // non-class specializers.  That's ambiguous, folks.
	  ambiguous? := #t;
	end if;
      end if;
    end for;
    //
    if (unknown?)
      // Some position was unknown, therefore the overall result is unknown.
      #"unknown";
    elseif (more-specific?)
      if (less-specific?)
	// Two different positions disagree, therefore the overall result
	// is ambiguous.
	#"ambiguous";
      else
	// Some position was more specific, and no position was unknown or
	// less specific, therefore the overall result is more-specific.
	#"more-specific";
      end if;
    elseif (less-specific?)
      // Some position is less specific and no position was unknown or
      // more specific, therefore the overall result is less-specific.
      #"less-specific";
    elseif (ambiguous?)
      // Some position was ambiguous, therefore the overall result is
      // ambiguous.
      #"ambiguous";
    else
      // No position could impose any kind of ordering information, therefore
      // the overall result is unordered.
      #"unordered";
    end if;
  end block;
end method compare-methods;


// Static next-method-info determination stuff.

// static-next-method-info
//
// If the next-method-info list is guaranteed to always be the same for all
// invocations of the given method, then return that list.  Otherwise, return
// #f.
//
define method static-next-method-info
    (defn :: <method-definition>) => res :: false-or(<list>);
  block (return)
    if (defn.function-defn-hairy?)
      return(#f);
    end if;

    let gf = defn.method-defn-of;
    unless (gf)
      return(#f);
    end unless;
    
    let call-types = map(ctype-extent,
			 defn.function-defn-signature.specializers);
    let seal-info = find-seal(gf, call-types);
    unless (seal-info)
      return(#f);
    end unless;

    let less-specific = #();
    for (meth in seal-info.seal-methods | compute-seal-methods(seal-info, gf))
      select (compare-methods(meth, defn, #f))
	#"disjoint", #"unordered", #"more-specific" =>
	  begin end;
	#"less-specific" =>
	  less-specific := pair(meth, less-specific);
	otherwise =>
	  return(#f);
      end select;
    end for;

    let (ordered, ambiguous) = sort-methods(less-specific, #f);
    unless (ordered)
      return(#f);
    end unless;

    local method ct-value-or-give-up
	      (defn :: <method-definition>) => res :: <ct-function>;
	    ct-value(defn) | return(#f);
	  end method ct-value-or-give-up;

    let ordered = map(ct-value-or-give-up, ordered);
    let ambiguous = map(ct-value-or-give-up, ambiguous);

    if (ambiguous.empty?)
      ordered;
    else
      concatenate(ordered, list(ambiguous));
    end if;
  end block;
end method static-next-method-info;


// Dumping stuff.

define constant $function-definition-slots
  = concatenate($definition-slots,
		list(function-defn-signature, signature:,
		       function-defn-signature-setter,
		     function-defn-hairy?, hairy:, #f,
		     function-defn-ct-value, #f, function-defn-ct-value-setter,
		     function-defn-flushable?, flushable:, #f,
		     function-defn-movable?, movable:, #f));

define constant $generic-definition-slots
  = concatenate($function-definition-slots,
		list(generic-defn-sealed?, sealed:, #f,
		     generic-defn-seals, seals:, #f,
		     generic-defn-discriminator, discriminator:,
		       %generic-defn-discriminator-setter));

// List of method definitions that we saw in a GF and that we need to make sure
// are dumped.
//
define variable *queued-methods* = #();

// Make sure that all of the method definitions get dumped too.  If some of
// them came from elsewhere it doesn't matter, we'll just have a gratuitous
// external reference.
// 
define constant dump-them-methods = method
    (x :: <generic-definition>, buf :: <dump-buffer>)
 => ();
  ignore(buf);
  for (meth in x.generic-defn-methods)
    *queued-methods* := pair(meth, *queued-methods*);
  end;
// dformat("Queuing %=\n", x);
end method;

define /* exported */ method dump-queued-methods (buf :: <dump-buffer>)
  // We clear the queue before dumping the methods in case dumping some
  // method triggers the addition of other methods to the queue.  But we
  // flame out if that happened, because I'm not sure that should be supported.
  // Basically, I noticed that the way this code was written before it would
  // quietly ignore any additional methods, so I figured it would be an
  // improvement to at least puke if that happens.  If it doens't puke, well,
  // then there isn't any need for anything fancier.  If it does puke, then
  // we can easily change it again.  -William
  let methods = *queued-methods*;
  *queued-methods* := #();
  for (meth in methods)
    dump-od(meth, buf);
  end;
  assert(*queued-methods* == #());
end;


add-make-dumper(#"generic-definition", *compiler-dispatcher*,
		<generic-definition>, $generic-definition-slots,
		load-external: #t,
		dump-side-effect: dump-them-methods);

add-make-dumper(#"implicit-generic-definition", *compiler-dispatcher*,
		<implicit-generic-definition>, $generic-definition-slots,
		load-external: #t,
		dump-side-effect: dump-them-methods);

add-make-dumper(#"seal-info", *compiler-dispatcher*, <seal-info>,
		list(seal-types, types:, #f));

define constant $abstract-method-definition-slots
  = concatenate($function-definition-slots,
		list(method-defn-inline-function, inline-function:,
		       %method-defn-inline-function-setter));

define method set-method-defn-of
    (gf :: false-or(<generic-definition>), meth :: <method-definition>) => ();
  meth.method-defn-of := gf;
  if (gf)
    ct-add-method(gf, meth);
  end;
end;
/*
define constant hackola = method (x, y)
  dformat("Dumping defn for method on %=\n",
  	  x.defn-name.method-name-generic-function.name-symbol);
end method;
*/
define constant $method-definition-slots
  = concatenate($abstract-method-definition-slots,
		list(method-defn-of, #f, set-method-defn-of,
		       method-defn-congruent?, congruent:, #f));

add-make-dumper(#"method-definition", *compiler-dispatcher*,
		<method-definition>, $method-definition-slots,
		load-external: #t);

define constant $accessor-method-definition-slots
  = concatenate($method-definition-slots,
		list(accessor-method-defn-slot-info, slot:,
		       accessor-method-defn-slot-info-setter));

add-make-dumper(#"getter-method-definition", *compiler-dispatcher*,
		<getter-method-definition>, $accessor-method-definition-slots,
		load-external: #t);

add-make-dumper(#"setter-method-definition", *compiler-dispatcher*,
		<setter-method-definition>, $accessor-method-definition-slots,
		load-external: #t);

// Seals for file func-defns.dylan

// <generic-definition> -- subclass of <function-definition>
define sealed domain make(singleton(<generic-definition>));
define sealed domain initialize(<generic-definition>);
// <implicit-generic-definition> -- subclass of <generic-definition>, <implicit-definition>
define sealed domain make(singleton(<implicit-generic-definition>));
// <method-definition> -- subclass of <abstract-method-definition>
define sealed domain make(singleton(<method-definition>));
define sealed domain initialize(<method-definition>);
// <accessor-method-definition> -- subclass of <method-definition>
define sealed domain make(singleton(<accessor-method-definition>));
// <getter-method-definition> -- subclass of <accessor-method-definition>
define sealed domain make(singleton(<getter-method-definition>));
// <setter-method-definition> -- subclass of <accessor-method-definition>
define sealed domain make(singleton(<setter-method-definition>));
// <seal-info> -- subclass of <object>
define sealed domain make(singleton(<seal-info>));
define sealed domain initialize(<seal-info>);
