Module: define-functions
Description: stuff to process method seals and build method trees
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/Attic/method-tree.dylan,v 1.9 1995/06/07 15:16:09 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

/*
Method Trees:

A next-method tree represents the method ordering of statically orderable
methods.  The static method ordering is computed based only on the compile-time
method specializers, thus it can't describe:
 A] Methods whose ordering depends on the class-precedence list of the actual
    argument.  This rule comes into play only when two class specializers
    intersect, but neither is a subtype of the other.
 B] Methods with specializers whose type is not known at compile time, due to a
    non-constant type expression.

The structure of the method also statically encodes the next-method for each
method, thus disallowing:
 C] Methods that don't always have the same next method or that might not have
    a legal next method due to ambiguous methods.

The structure of the tree is a list of lists or methods.  Method precedence
becomes greater as the tree is descended.  When we reach a leaf method, we know
that no method can have greater precedence than that method on any call to
which the method is applicable.  The format of an interior node is:
    (method, sub1, sub2, ...)

The "method" at the root of the tree is #f, which can be thought of as
representing the "no-applicable-methods" method.

The method tree maintains two invariants:
 1] The children of node are mutually disjoint --- no two of the sibling
    methods can ever be applicable to the same call. 
 2] The method at a node is always the next method for its immediate children.
    This means that the precedence of the child methods must be greater, and
    that no method can somehow interpose itself.  A corrolary is:
 3] each child specializer is a subtype of the parent specializer (possibly
    the same type.)

The idea behind the method tree is that compile-time method selection walks
the tree, descending into the unique child that "must be applicable" to the
call.

*/


/// Specializer comparison:

/*

For our purposes, there are 6 possible static relations between method
specializers:
 disjoint: no intersection
 >: more specific (a subtype)
 <: less specific (a supertype)
 unordered: identical
 ambiguous: definitely ambiguous (non-hierarchial non-class types)
 unknown: unknown types or non-hierarchical classes.

The 6 corresponding method orderings occur like this:
 disjoint: definitely disjoint (at least one specializer doesn't intersect)
 >: more specific (all = or subtype)
 <: less specific (all = or supertype)
 unordered: the same (all =, an error)
 ambiguous: anything combined with either both < and > or any
            single definitely ambiguous position (non-class non-hierarchical.)
 unknown: unknown

For sealed methods, at least one spec would have to be unknown or a
non-hierarchical class for us to end up in the "unknown" bucket.  But even
with such a spec, methods can sometimes be determined to be defintely disjoint
or definitely ambiguous by considering other specializers.

*/

define constant method-precedences
  = one-of(#"disjoint", #">", #"<", #"unordered", #"ambiguous", #"unknown");


// ### hack version...  Would really be defined in type system.
define method assume-disjoint(spec1, spec2, gf, index);
  #f;
end;


// compare-one-specializer  --  Internal
//
//    Helper function of compare-specializers.  Index is the index of the arg
// position we are comparing, for context to assume-disjoint.
//
define method compare-one-specializer
    (spec1 :: <ctype>, spec2 :: <ctype>,
     gf :: false-or(<definition>), index :: <fixed-integer>)
 => res :: method-precedences;
  case
    spec1 == spec2 => #"unordered";
    csubtype?(spec1, spec2) => #">";
    csubtype?(spec2, spec1) => #"<";
    ~ctypes-intersect?(spec1, spec2) => #"disjoint";

    instance?(spec1, <unknown-ctype>)
      | instance?(spec2, <unknown-ctype>) =>
      #"unknown";

    ~(instance?(spec1, <cclass>) & instance?(spec2, <cclass>)) =>
      #"ambiguous";

    gf & assume-disjoint(spec1, spec2, gf, index) => #"disjoint";
      
    otherwise => #"unknown";
  end;
end method;


// compare-specializers  --  Exported
//
//   Compare the static method precedence ordering of two specializer lists.
// If GF is true, then we are comparing two sealed methods of GF, and can
// assume that currently disjoint classes remain disjoint.  The signatures will
// have the same number of specializers because they are congruent.
//
define method compare-specializers
    (specs1 :: <list>, specs2 :: <list>,
     gf :: false-or(<definition>))
 => res :: method-precedences;

  assert(size(specs1) == size(specs2));
  let result = #"unordered";
  block (done)
    for (spec1 in specs1, spec2 in specs2, index from 0)
      let res = compare-one-specializer(spec1, spec2, gf, index);

      unless (res == result)
	select (res)
	  #"unordered" => begin end;
	  #"ambiguous", #"disjoint" => done(res);
	  #"unknown" => result := res;

	  #"<", #">" =>
	    select (result)
	      #"unknown" => begin end;
	      #"unordered" => result := res;

	      // we already picked off the res == result case, so they must
	      // mismatch.
	      #"<", #">" => done(#"ambiguous");
	    end;
	end;
      end unless;

      finally result;

    end for;
  end;
end method;


// The <seal-info> describes methods relevant to compile-time selection of
// calls that intersect with a particular seal.  Some methods could appear in
// more than one seal-info, either in the method-tree or in the hairy-methods.
// Methods with lower precedence than the seal can appear in the method trees
// on disjoint sealed branches.  Unknown-type methods can easily appear in all
// branches, since we have no idea of which branch (if any) then fall on.
//
define class <seal-info> (<object>)
  //
  // The generic function this seal is on.
  slot generic-function :: <generic-definition>,
    required-init-keyword: generic-function:;
  //
  // List of types describing the seal.
  slot seal-types :: <list>, required-init-keyword: seal-types:;
  //
  // List of types or #f corresponding to the required arguments.  If a type,
  // then all methods under this seal have that exact type for that argument,
  // thus that position is unselective.  If #f, then the position is selective.
  slot unselective-positions :: <list>;
  //
  // Next-method tree describing the static sorting of methods that intersect
  // with this seal.
  slot method-tree :: <list>, init-value: #();
  //
  // A list of all the known methods that may intersect with this seal but are
  // not in the method-tree.  Methods can end up here because they can't be
  // statically ordered or because a specializer is unknown.
  slot hairy-methods :: <list>, init-value: #();
end class;


// Given a method definition or interior method tree node, return the
// specializers.
//
define method get-specializers (node :: <method-definition>)
  node.function-defn-signature.specializers;
end method;
//
define method get-specializers (node :: <list>)
  node.head.function-defn-signature.specializers;
end method;


// Merge a new seal into the seal-infos in GF.  The "canonical" property that
// we want to preserve that is all seals are disjoint.
//
// We ignore (don't add) seals containing unknown types or that aren't known to
// be disjoint from other seals.  We pick off unknown types before entering
// anything to protect against the possibility that first seal we look at might
// have unknown types.
//
define method canonicalize-1-seal
    (new :: <list>, gf :: <generic-definition>) => ();
  block (done)
    if (any?(rcurry(instance?, <unknown-ctype>), new))
      done();
    end;

    let res = #();
    for (info in gf.%generic-defn-seal-info)
      let cur-seal = info.seal-types;
      select (compare-specializers(new, cur-seal, gf))
        #"unordered", #">", #"ambiguous", #"unknown" => done();
	#"<" => begin end;
	#"disjoint" => res := pair(info, res);
      end;
    end for;

    gf.%generic-defn-seal-info
      := pair(make(<seal-info>, seal-types: new, generic-function: gf), res);
  end block;
end method;


// Copy all methods that may intersect with the seal into the seal-info.
// Methods that definitely intersect are stashed in the method-tree slot (even
// though not they are not yet a method tree.)  Any unclear methods become
// hairy-methods.
//
define method methods-to-seals
    (seal-info :: <seal-info>, meths :: <list>)
 => ();

  let cur-seal = seal-info.seal-types;
  let gf = seal-info.generic-function;
  for (meth in meths)
    select (compare-specializers(get-specializers(meth), cur-seal, gf))
      #"unordered", #">", #"<" =>
        seal-info.method-tree := pair(meth, seal-info.method-tree);

      #"disjoint" => begin end;

      #"ambiguous", #"unknown" =>
        seal-info.hairy-methods := pair(meth, seal-info.hairy-methods);
    end;
  end;
end method;


// This function takes a method tree and a method and tries to return a new
// method tree.  If the method can't be added, then we call the "punt" function.
// GF is the generic function for context to compare-specializers.
//
// What we do is compare the added method to each method at this level in the
// tree.  If some child is less specific than the method, then we recurse
// on that (necessarily unique) child.  If all sibs are disjoint, we introduce
// We introduce a new leaf.  If some children are more specific, then the
// method becomes the parent of those nodes.
//
// In all cases, disjoint children remain sibs of the newly introduced (or
// augmented) subtree.
//
define method method-tree-add(mt, meth, gf, punt);
  let meth-spec = get-specializers(meth);
  let greater = #();
  let disjoint = #();
  let less = #f;
      
  for (child in mt.tail)
    select (compare-specializers(get-specializers(child), meth-spec, gf))
      #">" =>
        greater := pair(child, greater);

      #"disjoint" =>
        disjoint := pair(child, disjoint);

      #"<" =>
        assert(~less);
	less := child;

      #"unordered" =>
        compiler-warning("### Multiply defined method: %s", meth.defn-name);
	punt();

      #"ambiguous" =>
        compiler-warning("### Ambiguous methods: %s %s",
			 meth.defn-name, child.defn-name);
	punt();

      #"unknown" =>
        punt();
    end;
  end;

  let new-child
    = case
        less =>
	  assert(empty?(greater));
	  if (instance?(less, <pair>))
	    method-tree-add(less, meth, gf, punt);
	  else
	    list(less, meth);
	  end;

	empty?(greater) => meth;
        otherwise => pair(meth, greater);
      end;

  pair(mt.head, pair(new-child, disjoint));
end method;


// Convert the method list stashed in the method-tree slot into a real method
// tree.  We try adding each method, putting methods that can't be added into
// the hairy-methods.
//
define method build-method-tree(cur-seal :: <seal-info>) => ();
  let gf = cur-seal.generic-function;
  let res = list(#f);
  for (meth in cur-seal.method-tree)
    block (next)
      block (punt)
        res := method-tree-add(res, meth, gf, punt);
	next();
      end;
      cur-seal.hairy-methods := pair(meth, cur-seal.hairy-methods);
    end;
  end;
  cur-seal.method-tree := res;
end method;


// Check congruence at one position, returning #t if definitely congruent.
// Meth and GF are for error context.
//
define method check-1-arg-congruent(mspec, gspec, meth, gf)
  let (val, val-p) = values-subtype?(mspec, gspec);
  case
    ~val-p =>
      compiler-warning
	("Can't tell if %s is a subtype of %s, so can't tell if method %s is "
	   "congruent to GF %s.",
	 mspec, gspec, meth.defn-name, gf.defn-name);
      #f;

    ~val =>
      compiler-warning
	("Method %s isn't congruent to GF %s because method type %s isn't a "
	   "subtype of GF type %s.",
	 meth.defn-name, gf.defn-name, mspec, gspec);
      #f;

    otherwise => #t;
  end case;
end method;


// Check that the methods on GF are congruent to it, and return the methods
// that are congruent.
//
define method check-gf-congruence(gf :: <generic-definition>) => res :: <list>;
  let gsig = gf.function-defn-signature;
  let gspecs = gsig.specializers;
  let res = #();

  for (meth in gf.generic-defn-methods)
    let msig = meth.function-defn-signature;
    let win = #t;

    let mspecs = msig.specializers;
    unless (size(mspecs) = size(gspecs))
      compiler-warning
        ("Method %s has different number of required arguments than GF %s.",
	 meth.defn-name, gf.defn-name);
      win := #f;
    end;
    for (mspec in mspecs, gspec in gspecs)
      win := check-1-arg-congruent(mspec, gspec, meth, gf) & win;
    end for;

    case
      gsig.key-infos =>
        if (~msig.key-infos)
	  compiler-warning
	    ("GF %s accepts keywords but method %s doesn't.",
	     gf.defn-name, meth.defn-name);
	  win := #f;
	elseif (msig.all-keys? & ~gsig.all-keys?)
	  compiler-warning
	    ("Method %s accepts all keys but GF %s doesn't.",
	     meth.defn-name, gf.defn-name);
	  win := #f;
	else
	  for (gkey in gsig.key-infos)
	    let gkey-name = gkey.key-name;
	    let gspec = gkey.key-type;
	    block (found-it)
	      for (mkey in msig.key-infos)
	        if (mkey.key-name == gkey-name)
		  win := check-1-arg-congruent (mkey.key-type, gspec, meth, gf)
			   & win;
		  found-it();
		end;
	      end for;
		  
	      compiler-warning
		("GF %s mandatory keyword arg %= is not accepted by "
		   "method %s.",
		 gf.defn-name, gkey-name, meth.defn-name);
	      win := #f;
	    end block;
	  end for;
	end if;

      msig.key-infos =>
	compiler-warning
	  ("Method %s accepts keywords but GF %s doesn't.",
	   meth.defn-name, gf.defn-name);
	win := #f;

      gsig.rest-type & ~msig.rest-type =>
        compiler-warning
	  ("GF %s accepts variable arguments, but method %s doesn't.",
	   gf.defn-name, meth.defn-name);
	win := #f;

      ~gsig.rest-type & msig.rest-type =>
        compiler-warning
	  ("Method %s accepts variable arguments, but GF %s doesn't.",
	   meth.defn-name, gf.defn-name);
	win := #f;
      
      ~check-1-arg-congruent(msig.returns, gsig.returns, meth, gf) =>
        win := #f;

    end case;
    if (win)
      res := pair(meth, res);
    end;

  end for;
  res;
end method;


// Do various stuff to finalize analysis of the method set for a GF.  This is
// triggered on demand off of GENERIC-DEFN-SEAL-INFO.
//
define method grovel-gf-stuff (gf :: <generic-definition>) => ();
  let meths = check-gf-congruence(gf);
  let seals = gf.generic-defn-seals;

  gf.%generic-defn-seal-info := #();
  for (cur-seal in seals)
    canonicalize-1-seal(cur-seal, gf);
  end;

  for (cur-seal in gf.%generic-defn-seal-info)
    methods-to-seals(cur-seal, meths);
    build-method-tree(cur-seal);
  end;
end method;


// Create seal-info if not already created.
//
define method generic-defn-seal-info (gf :: <generic-definition>) 
 => res :: <list>;
  unless (slot-initialized?(gf, %generic-defn-seal-info))
    grovel-gf-stuff(gf);
  end;
  gf.%generic-defn-seal-info;
end method;



// ct-sorted-applicable-methods
//
// Return a guess at the applicable methods for call with arguments of the
// given types as list of <method-definition>s, or #f if we can't tell.  If we
// can tell that no methods are applicable, we return the empty list.
// 
// The methods returned might not be applicable, but if they arn't then
// nothing is.
// 
define method ct-sorted-applicable-methods
    (gf :: <generic-definition>, call-types :: <list>)
    => res :: union(<list>, <false>);
  block (return)
    if (gf.generic-defn-sealed?)
      let maybe-applicable = #();
      let definitely-applicable = #();
      for (meth in gf.generic-defn-methods)
	select (compare-specializers(meth.get-specializers, call-types, #f))
	  #"disjoint" => #f;
	  #"<", #"unordered" =>
	    definitely-applicable := pair(meth, definitely-applicable);
	  #">", #"ambiguous", #"unknown" =>
	    maybe-applicable := pair(meth, maybe-applicable);
	end;
      end;
      
      if (definitely-applicable == #())
	// Nothing is definitely applicable.
	if (maybe-applicable == #())
	  // And nothing is potentially applicable.
	  return(#());
	elseif (maybe-applicable.tail == #())
	  // Only one method is potentially applicable.  Select it.
	  return(maybe-applicable);
	end;
      elseif (maybe-applicable == #())
	// We know precisely which methods are applicable, so not just
	// check to see if we can sort them.
	if (definitely-applicable.tail == #())
	  // There is only one method applicable, so sorting it is easy.
	  return(definitely-applicable);
	else
	  block (punt)
	    return(sort!(definitely-applicable,
			 test: method (meth1, meth2)
				 select (compare-specializers
					   (meth1.get-specializers,
					    meth2.get-specializers,
					    #f))
				   #">" => #t;
				   #"<" => #f;
				   #"ambiguous", #"unknown" => punt();
				 end;
			       end));
	  end;
	end;
      end;
    end;

    for (child in generic-defn-seal-info(gf))
      select (compare-specializers(child.seal-types, call-types, #f))
	#"disjoint" =>
	  // We are disjoint from this seal -- ignore it.
	  #f;

	#"<", #"unordered" =>
	  //
	  // Make sure we don't intersect any of the hairy methods.
	  for (hairy in child.hairy-methods)
	    let hairy-specs = hairy.function-defn-signature.specializers;
	    unless (compare-specializers(hairy-specs, call-types, #f)
		      == #"disjoint")
	      return(#f);
	    end;
	  end;

	  // Okay, we have a candidate seal.  Now grovel it finding the most
	  // specific potentially applicable method.
	  return(find-applicable(child.method-tree, #(), call-types));

	otherwise =>
	  //
	  // We overlap this seal so we have to bail.
	  return(#f);
      end;
    end;
  end;
end;


define method find-applicable (mt :: <list>, nexts :: <list>, types :: <list>)
    => res :: false-or(<list>);
  block (punt)
    let intersects-with = #f;
    block (found)
      for (sub in mt.tail)
	select (compare-specializers(get-specializers(sub), types, #f))
	  #"disjoint" =>
	    #f;
	  #"<", #"unordered" =>
	    intersects-with := sub;
	    found();
	  otherwise =>
	    // If we intersect with two root level methods, then we have
	    // to punt.  If we intersect with a child method, we have to
	    // punt because we can't know if the results should include
	    // the child or not.
	    if (intersects-with | mt.head)
	      punt(#f);
	    else
	      intersects-with := sub;
	    end;
	end;
      end;
    end;
    let nexts = if (mt.head)
		  pair(mt.head, nexts);
		else
		  nexts;
		end;
    if (intersects-with)
      find-applicable(intersects-with, nexts, types);
    else
      nexts;
    end;
  end;
end;

define method find-applicable
    (meth :: <method-definition>, nexts :: <list>, types :: <list>)
    => res :: <list>;
  pair(meth, nexts);
end;
