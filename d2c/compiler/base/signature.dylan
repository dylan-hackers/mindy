Module: front
Description: Method/GF signatures and operations on them
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/signature.dylan,v 1.1 1994/12/12 13:01:37 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

// A <signature> represents what we know at compile-time about the
// argument/return-value protocol of a GF or method.
// 
define class <signature> (<object>)

  // List of <ctype>s representing the specializers for required arguments.
  slot specializers :: <list>, required-init-keyword: specializers:;

  // If no #rest args, #f, otherwise the rest arg type.
  slot rest-type :: false-or(<ctype>), required-init-keyword: rest-type:;

  // List of <key-info>s describing the specified keyword args.  #f if #key was
  // not specified.
  slot key-infos :: false-or(<list>), required-init-keyword: keys:;
  slot all-keys? :: <boolean>, required-init-keyword: all-keys:;

  // list of <ctype>s reprepsenting the required result types.
  slot returns :: <list>, required-init-keyword: returns:;

  // If no #rest in returns, #f, otherwise the rest values type.
  slot returns-rest-type :: false-or(<ctype>),
    required-init-keyword: returns-rest-type:;

end;

define method print-object (sig :: <signature>, stream :: <stream>) => ();
  pprint-fields(sig, stream,
		specializers: sig.specializers,
		sig.rest-type & (rest-type:), sig.rest-type,
		sig.key-infos & (key-infos:), sig.key-infos,
		sig.all-keys? & (all-keys:), #t,
		returns:, sig.returns,
		sig.returns-rest-type & (returns-rest-type:),
		  sig.returns-rest-type);
end;

define class <key-info> (<object>)

  // name of this keyword arg.
  slot key-name :: <symbol>, required-init-keyword: key-name:;

  // type restriction.
  slot key-type :: <ctype>, required-init-keyword: type:;

  // true if a required keyword.
  // ??? if this means anything, it means the non-strictly-Dylan
  // concept of keywords that are effictively required, e.g. due to an
  // error-default.  Or a required-init-keyword on a make method?
  slot required? :: <boolean>, init-value: #f;

  // ??? may want to wedge info about constant defaults in here.
end;

define method print-object (key :: <key-info>, stream :: <stream>) => ();
  pprint-fields(key, stream,
		name: key.key-name,
		type: key.key-type,
		required: key.required?);
end;

/* 

sorted-statically-applicable-methods methods operation
    See what compiler methods are statically applicable to operation.  This
    basically deals with official Dylan method selection, looking only at the
    specializers (but if some of them are funny types like direct-instance,
    that will be taken into consideration.)  Some specializers may be
    duplicated between builtin methods and other methods.  Builtin methods will
    be sorted before non-builtin methods having the same specializers.  We
    return #(), #F if type uncertainty prevented us from determining
    applicability or sorting.


congruent-signatures? gf-sig meth-sig
    True if a method signature is/might-be congruent to a GF signature.

signatures-equal? sig1 sig2
    True if two signatures are identical.  Probably not worth hash-consing
    signatures.  Not an = method because we want to do funny stuff
    to indicate any confusion or differnce we come across.

legal-signature? signature operation output-type
    True if a signature is/might-be legal for an operation, assuming that
    specializers are applicable.  This deals with the possibility that
    it would be an error to actually call due to illegal keywords, missing
    keywords or bad key or rest types.
*/

// True if two signatures may/definitely have the same specializers (to see
// if we have multiply defined methods.)
//
define constant same-specializers? = method
    (sig1 :: <signature>, sig2 :: <signature>) 
 => (res :: <boolean>, win :: <boolean>);

  let specs1 = sig1.specializers;
  let specs2 = sig2.specializers;
  if (size(specs1) == size(specs2))
    block (done)
      for (spec1 in specs1, spec2 in specs2)
	let (val, win) = ctype-eq?(spec1, spec2);
	unless (val) done(#f, win) end;
      end;
      values(#t, #t);
    end block;
  else
    values(#f, #t);
  end;
end method;


// specializers-applicable?
//
// Determine if the specializers in Signature are applicable to Operation.
// Three cases:
//   #t, #t: all args are subtypes their specializers.
//   #f, #t: at least one arg is definitely not a subtype.
//   #f, #f: at least one arg is an intersecting non-subtype or we can't even
//           determine if they intersect (an unknown type.)
//
define constant specializers-applicable? = method
    (signature :: <signature>, operation :: <operation>)
 => (res :: <boolean>, win :: <boolean>);

  let result = #t;
  block (done)
    for (specs = signature.specializers then specs.tail,
         ops = operation.operands then ops.dependent-next,
	 while ~empty?(specs) & ops)

      let spec = specs.head;
      let op-type = ops.source-exp.derived-type;
      let (int, int-win) = ctypes-intersect?(spec, op-type);
      if (int)
        let (sub, win) = csubtype?(op-type, spec);
	assert(win);
	result := result & sub;
      elseif (int-win)
        done(#f, #t);
      else
        result := #f;
      end if;

      finally
        values(result & empty?(specs) & ~ops, result);
    end for;
  end;
end;


// Method precedence:


// one-specializer-ordered?
//
// Helper function for specializers-ordered? that deals with a single
// specializer.  Note that since both specs are known to be applicable to arg,
// there can't be any unknown types floating around.
//
// The ordering rule applicable to all types is fairly simple: a specializer is
// more specific if it is a subtype of the other specializer.  If neither is a
// strict subtype of the other, then they are unordered.
//
// With classes, there is an additional ordering rule involving the class
// precedence-list of the actual argument being dispatched.  The rule is
// that a class precedes another if it precedes that class in the object CPL.
// Since the CPL is in order of decreasing precedence, the precedence is
// greater when the index is *less*.
//
// Since we don't have the actual argument, some uncertainty is introduced
// here.  If we can find all the possible direct classes of the arg, then we
// can compare the result that would be obtained from all the possible CPLs.
// If these results agree, then we can return a definite true or false.  If
// not, we can't tell.
//
// It is also possible that we may find there are no possible direct classes
// (if all classes are abstract.)  In this case, we also consider the methods
// to be uncertainly unordered, though in reality there is a type error or dead
// code.
//
define constant one-specializer-ordered? = method
    (spec1 :: <ctype>, spec2 :: <ctype>, arg :: <ctype>)
 => (res :: union(<boolean>, singleton(#"unordered")), win :: <boolean>);
  case
    spec1 == spec2 => values(#"unordered", #t);

    csubtype?(spec1, spec2) => values(#t, #t);
    csubtype?(spec2, spec1) => values(#f, #t);

    ~(instance?(spec1, <cclass>) & instance?(spec2, <cclass>)) =>
      values(#"unordered", #t);

    otherwise =>
      let classes = find-direct-classes(arg);
      if (classes == #f | classes == #())
        values(#"unordered", #f);
      else
        let some-greater = #f;
	let some-not-greater = #f;
	block (done)
	  for (cl in classes)
	    let cpl = cl.precedence-list;
	    if (key-of(spec1, cpl) < key-of(spec2, cpl))
	      some-greater := #t;
	    else
	      some-not-greater := #t;
	    end;

	    if (some-greater & some-not-greater)
	      done(#"unordered", #f);
	    end;

	    finally values(some-greater, #t);
	  end for;
	end block;
      end if;
  end case;
end method;
        

// specializers-ordered?
//
// Test if Sig1 and Sig2 are properly ordered by method precedence rules when
// applied to Op.  Sig1 and Sig2 are assumed to be applicable to Op.
// Four cases:
//   #f, #t: Sig1 definitely not greater than Sig2.
//   #t, #t: Sig1 definitely greater than Sig2.
//   #"unordered", #t: definitely unordered.
//   #"unordered", #f: can't determine ordering.
//
// What we do is loop over the specializers, tentatively assuming they are
// unordered.  Once we find an ordered specializer, we can't find a
// conflictingly ordered one (or the result is unordered.)
//
define constant specializers-ordered? = method
    (sig1 :: <signature>, sig2 :: <signature>, op :: <operation>)
 => (res :: union(<boolean>, singleton(#"unordered")), win :: <boolean>);

  let specs1 = sig1.specializers;
  let specs2 = sig2.specializers;
  assert(size(specs1) == size(specs2));
  let result = #"unordered";
  block (done)
    for (spec1 in specs1, spec2 in specs2,
	 args = op.operands then args.dependent-next)
      let (res, win)
	= one-specializer-ordered?(spec1, spec2, args.source-exp.derived-type);
      unless (win) done(#"unordered", #f) end;

      case
        res == result => begin end;
	res == #"unordered" => begin end;
	result == #"unordered" => result := res;
	otherwise => done(#"unordered", #t);
      end;

      finally values(result, #t);
    end;
  end;
end method;
