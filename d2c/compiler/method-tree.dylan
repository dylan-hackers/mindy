Module: define-functions
Description: stuff to process method seals and build method trees
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/Attic/method-tree.dylan,v 1.13 1995/12/15 16:16:36 wlott Exp $
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

The structure of the tree is a list of sub-nodes.  Method precedence
becomes greater as the tree is descended.  When we reach a leaf
method, we know that no method can have greater precedence than that
method on any call to which the method is applicable.  Each interior
node holds a method and a method tree of the stuff more specific than
that method.

*/


/// Specializer comparison:

/*

For our purposes, there are 6 possible static relations between method
specializers:
 disjoint: no intersection
 >: more specific (a subtype)
 <: less specific (a supertype)
 =: identical
 ambiguous: definitely ambiguous (non-hierarchial non-class types)
 unknown: unknown types or non-hierarchical classes.

The 6 corresponding method orderings occur like this:
 disjoint: definitely disjoint (at least one specializer doesn't intersect)
 >: more specific (all = or subtype)
 <: less specific (all = or supertype)
 =: the same (all =, an error)
 ambiguous: anything combined with either both < and > or any
            single definitely ambiguous position (non-class non-hierarchical.)
 unknown: unknown

For sealed methods, at least one spec would have to be unknown or a
non-hierarchical class for us to end up in the "unknown" bucket.  But even
with such a spec, methods can sometimes be determined to be defintely disjoint
or definitely ambiguous by considering other specializers.

*/

define constant method-precedences
  = one-of(#"disjoint", #">", #"<", #"=", #"ambiguous", #"unknown");


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
    spec1 == spec2 => #"=";
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
  let result = #"=";
  block (done)
    for (spec1 in specs1, spec2 in specs2, index from 0)
      let res = compare-one-specializer(spec1, spec2, gf, index);

      unless (res == result)
	select (res)
	  #"=" => begin end;
	  #"ambiguous", #"disjoint" => done(res);
	  #"unknown" => result := res;

	  #"<", #">" =>
	    select (result)
	      #"unknown" => begin end;
	      #"=" => result := res;

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


// Merge a new seal into the seal-infos in GF.
//
// We ignore (don't add) seals containing unknown types.  Also, if some seal
// is strictly more specific than some other seal, we only enter the less
// specific of the two.
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
        #"=", #">", #"unknown" => done();
	#"<" => begin end;
	#"disjoint", #"ambiguous" => res := pair(info, res);
      end;
    end for;

    gf.%generic-defn-seal-info
      := pair(make(<seal-info>, seal-types: new, generic-function: gf), res);
  end block;
end method;


define class <method-tree-node> (<object>)
  //
  // The specializers for this node.  Copied out of the method for faster
  // access.
  slot node-specializers :: <list>, init-value: #();
  //
  // The method for this node.  This method is strictly more specific than
  // any methods in the children, and strictly less specific than any
  // method above us in the tree.
  slot node-method :: <method-definition>, required-init-keyword: method:;
  //
  // The child nodes, possibly empty.
  slot node-children :: <list>, init-value: #(), init-keyword: children:;
end;

define method initialize (node :: <method-tree-node>, #key) => ();
  node.node-specializers
    := node.node-method.function-defn-signature.specializers;
end method;


// This function takes a method tree and a method and tries to return a new
// method tree.
// 
// GF is the generic function for context to compare-specializers.
//
// What we do is compare the added method to each method at this level in the
// tree.  If some child is less specific than the method, then we recurse
// on that child.  If some children are more specific, then the
// method becomes the parent of those nodes.  If we intersect any children
// but aren't ordered with respect to them, they stay siblings.
//
define method method-tree-add
    (mt :: <list>, meth :: <method-definition>,
     gf :: false-or(<generic-definition>), punt :: <function>)
    => res :: <list>;

  let meth-spec = meth.function-defn-signature.specializers;
  let greater = #();
  let siblings = #();
  let less = #();
      
  for (child in mt)
    select (compare-specializers(child.node-specializers, meth-spec, gf))
      #">" =>
        greater := pair(child, greater);

      #"disjoint", #"ambiguous" =>
	siblings := pair(child, siblings);

      #"<" =>
	less := pair(child, less);

      #"=" =>
	error("identical methods made it into the method tree?");

      #"unknown" =>
	compiler-warning
	  ("Cannot statically determine the relationship between %s and %s.",
	   child.node-method.defn-name, meth.defn-name);
	punt();
    end;
  end;

  if (less == #())
    pair(make(<method-tree-node>, method: meth, children: greater),
	 siblings);
  else
    assert(greater == #());
    for (result = siblings
	   then pair(make(<method-tree-node>,
			  method: node.node-method,
			  children: method-tree-add(node.node-children, meth,
						    gf, punt)),
		     result),
	 node in less)
    finally
      result;
    end;
  end;
end method;


// Build the method tree for the given seal.  For any congruent method that
// intersects the seal, we either add it to the seal's method tree or add it to
// the seal's hairy-methods.
//
define method build-method-tree-for (seal-info :: <seal-info>) => ();
  let gf = seal-info.generic-function;
  let seal-spec = seal-info.seal-types;

  let method-tree = #();
  let hairy-methods = #();

  for (meth in gf.generic-defn-methods)
    if (meth.method-defn-congruent?)
      let meth-spec = meth.function-defn-signature.specializers;
      select (compare-specializers(meth-spec, seal-spec, gf))
	#"=", #">", #"<", #"ambiguous" =>
	  block (okay)
	    block (punt)
	      method-tree := method-tree-add(method-tree, meth, gf, punt);
	      okay();
	    end;
	    hairy-methods := pair(meth, hairy-methods);
	  end;

	#"disjoint" => begin end;

	#"unknown" =>
	  hairy-methods := pair(meth, hairy-methods);
      end;
    end;
  end;

  seal-info.method-tree := method-tree;
  seal-info.hairy-methods := hairy-methods;
end method;


// Do various stuff to finalize analysis of the method set for a GF.  This is
// triggered on demand off of GENERIC-DEFN-SEAL-INFO.
//
define method grovel-gf-stuff (gf :: <generic-definition>) => ();
  gf.%generic-defn-seal-info := #();
  if (gf.generic-defn-sealed?)
    // ### This isn't quite right because ``define sealed generic'' is a
    // different kind of sealing than ``seal generic (<object>,...)''.  But
    // for now it doesn't matter because we don't make any use of the stronger
    // guarantees implied by seal generic.
    canonicalize-1-seal(gf.function-defn-signature.specializers, gf);
  end;
  for (cur-seal in gf.generic-defn-seals)
    canonicalize-1-seal(cur-seal, gf);
  end;

  for (cur-seal in gf.%generic-defn-seal-info)
    build-method-tree-for(cur-seal);
  end;
end method;


// Create seal-info if not already created.
//
define method generic-defn-seal-info (gf :: <generic-definition>) 
 => res :: <list>;
  unless (gf.%generic-defn-seal-info)
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
    => (ordered :: false-or(<list>),
	ambiguous :: false-or(<list>));
  block (return)
    for (seal-info in gf.generic-defn-seal-info)
      select (compare-specializers(seal-info.seal-types, call-types, #f))
	#"<", #"=" =>
	  //
	  // Make sure we don't intersect any of the hairy methods.
	  block (next-seal)
	    for (hairy in seal-info.hairy-methods)
	      let hairy-specs = hairy.function-defn-signature.specializers;
	      unless (compare-specializers(hairy-specs, call-types, #f)
			== #"disjoint")
		next-seal();
	      end;
	    end;

	    // Okay, we have a candidate seal.  Recursivly grovel its method
	    // tree and return the results if we can come up with any.
	    let (ordered, ambiguous)
	      = find-applicable(seal-info.method-tree, #(), #(), call-types);
	    if (ordered)
	      return(ordered, ambiguous);
	    end;
	  end;

	otherwise =>
	  //
	  // We can't use this seal to figure anything out about the
	  // call.
	  begin end;
      end;
    end;
    values(#f, #f);
  end;
end;


define method find-applicable
    (mt :: <list>, ordered :: <list>, ambiguous :: <list>, types :: <list>)
    => (ordered :: false-or(<list>), ambiguous :: false-or(<list>));
  let definitely-applicable = #();
  let maybe-applicable = #();
  for (sub in mt)
    select (compare-specializers(sub.node-specializers, types, #f))
      #"disjoint" =>
	#f;

      #"<", #"=" =>
	definitely-applicable := pair(sub, definitely-applicable);
	
      otherwise =>
	maybe-applicable := pair(sub, maybe-applicable);
    end;
  end;

  if (maybe-applicable == #())
    // The things we know, we know we know.
    if (definitely-applicable == #())
      // Nothing is applicable.  Bottom out.
      values(ordered, ambiguous);
    elseif (definitely-applicable.tail == #())
      let sub = definitely-applicable.head;
      find-applicable(sub.node-children, pair(sub.node-method, ordered),
		      ambiguous, types);
    else
      block (punt)
	// A couple things are applicable.  Their relationship with each
	// other is either unknown or ambiguous.  If they all are mutually
	// ambiguous, then we can keep going.  If not, we have to punt.
	for (remaining = definitely-applicable then remaining.tail)
	  let spec = remaining.head.node-specializers;
	  for (other in remaining.tail)
	    select (compare-specializers(spec, other.node-specializers, #f))
	      #"ambiguous" => begin end;
	      #"unknown" => punt(#f, #f);
	    end;
	  end;
	end;
	// Okay, they all were mutually ambiguous.  Great.  Note: each one
	// of them will have all more specific methods under them so we can
	// recurse using any of them and get the same results.
	find-applicable(definitely-applicable.head.node-children, #(),
			map(node-method, definitely-applicable),
			types);
      end;
    end;
  elseif (ordered == #() & ambiguous == #() & maybe-applicable.tail == #()
	    & definitely-applicable == #())
    // Only one thing is maybe applicable, and we are at the top level.
    // So assume that it is applicable and recurse on it.
    let sub = maybe-applicable.head;
    find-applicable(sub.node-children, list(sub.node-method), #(), types);
  else
    // Too much stuff is applicable for us to be able to tell at compile
    // time.
    values(#f, #f);
  end;
end;

