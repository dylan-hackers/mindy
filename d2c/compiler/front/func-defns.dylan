module: function-definitions

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
      select (compare-methods(types, old-seal.seal-types, #f))
	#"more-specific", #"unordered" =>
	  // The new seal is more specific than or the same as the old seals,
	  // so we can ignore the new seal.
	  return();
	#"less-specific" =>
	  // The new seal is less specific than the old one, so we can blow off
	  // the old one.
	  begin end;
	otherwise =>
	  // We need them both.
	  new-seals := pair(old-seal, new-seals);
      end select;
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
      select (compare-methods(meth, call-types, #f))
	#"disjoint" =>
	  begin end;
	#"less-specific", #"unordered" =>
	  definitely-applicable := pair(meth, definitely-applicable);
	otherwise =>
	  maybe-applicable := pair(meth, maybe-applicable);
      end select;
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
      select (compare-methods(call-types, seal-info.seal-types, #f))
	#"more-specific", #"unordered" => return(seal-info);
	otherwise => begin end;
      end select;
    end for;
    #f;
  end block;
end method find-seal;


define method compute-seal-methods
    (seal-info :: <seal-info>, gf :: <generic-definition>)
    => methods :: <list>;
  let types = seal-info.seal-types;
  seal-info.seal-methods
    := choose(method (meth :: <method-definition>)
		  => res :: <boolean>;
		if (meth.method-defn-congruent?)
		  compare-methods(meth, types, #f) ~== #"disjoint";
		end;
	      end method,
	      gf.generic-defn-methods)
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
	      remaining.tail := #();
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
  compare-methods(meth1.function-defn-signature.specializers, 
		  meth2.function-defn-signature.specializers,
		  arg-classes);
end;

define method compare-methods
    (meth1 :: <method-definition>, specs2 :: <list>,
     arg-classes :: false-or(<simple-object-vector>))
    => res :: one-of(#"more-specific", #"less-specific", #"unordered",
		     #"disjoint", #"ambiguous", #"unknown");
  compare-methods(meth1.function-defn-signature.specializers, specs2,
		  arg-classes);
end;

define method compare-methods
    (specs1 :: <list>, specs2 :: <list>,
     arg-classes :: false-or(<simple-object-vector>))
    => res :: one-of(#"more-specific", #"less-specific", #"unordered",
		     #"disjoint", #"ambiguous", #"unknown");
  block (return)
    let result = #"unordered";
    for (index from 0,
	 spec1 in specs1,
	 spec2 in specs2)
      let arg-class = arg-classes & arg-classes[index];
      //
      // If this is an argument that we are actually sorting by,
      if ((arg-classes == #f | arg-class) & spec1 ~== spec2)
	//
	// If the two specializers are the same, then this argument offers no
	// ordering.
	let this-one
	  = if (csubtype?(spec1, spec2))
	      #"more-specific";
	    elseif (csubtype?(spec2, spec1))
	      #"less-specific";
	    elseif (~ctypes-intersect?(spec1, spec2))
	      return(#"disjoint");
	    elseif (instance?(spec1, <cclass>) & instance?(spec2, <cclass>))
	      // Neither argument is a subclass of the other.  So we have to
	      // base it on the precedence list of the actual argument class.
	      if (arg-classes)
		let cpl = arg-class.precedence-list;
		block (found)
		  for (super in cpl)
		    if (super == spec1)
		      found(#"more-specific");
		    elseif (super == spec2)
		      found(#"less-specific");
		    end;
		  finally
		    error("%= isn't applicable", arg-class);
		  end;
		end;
	      else
		return(#"unknown");
	      end if;
	    elseif (instance?(spec1, <unknown-ctype>)
		      | instance?(spec2, <unknown-ctype>))
	      return(#"unknown");
	    else
	      // Neither argument is a subtype of the other and we have a
	      // non-class specializers.  That's ambiguous, folks.
	      return(#"ambiguous");
	    end;
	unless (result == this-one)
	  if (result == #"unordered")
	    result := this-one;
	  else
	    return(#"ambiguous");
	  end;
	end;
      end;
    end;
    result;
  end;
end;


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
