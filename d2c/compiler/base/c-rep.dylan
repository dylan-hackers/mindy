module: representation

define class <c-representation> (<representation>)
  slot more-general-representation :: union(<false>, <representation>),
    setter: #f, init-value: #f, init-keyword: more-general:;
  slot representation-depth :: <fixed-integer>;
  slot representation-alignment :: <fixed-integer>, setter: #f,
    required-init-keyword: alignment:;
  slot representation-size :: <fixed-integer>, setter: #f,
    required-init-keyword: size:;
  slot representation-has-bottom-value? :: <boolean>, setter: #f,
    init-value: #f, init-keyword: has-bottom-value:;
  slot representation-c-type :: <string>, setter: #f,
    required-init-keyword: c-type:;
end;

define method initialize (rep :: <c-representation>, #key more-general)
  rep.representation-depth
    := if (more-general)
	 more-general.representation-depth + 1;
       else
	 0;
       end;
end;

define method print-object (rep :: <c-representation>, stream :: <stream>)
    => ();
  pprint-fields(rep, stream, c-type: rep.representation-c-type);
end;

define constant $general-rep
  = make(<c-representation>, alignment: 4, size: 8, has-bottom-value: #t,
	 c-type: "descriptor_t");
define constant $heap-rep
  = make(<c-representation>, alignment: 4, size: 4, has-bottom-value: #t,
	 more-general: $general-rep, c-type: "heap_ptr_t");

define constant $extended-rep
  = make(<c-representation>, alignment: 8, size: 8, more-general: $heap-rep,
	 c-type: "long double");
define constant $double-rep
  = make(<c-representation>, alignment: 8, size: 8, more-general: $heap-rep,
	 c-type: "double");
define constant $single-rep
  = make(<c-representation>, alignment: 4, size: 4, more-general: $general-rep,
	 c-type: "float");

define constant $long-rep
  = make(<c-representation>, alignment: 4, size: 4, more-general: $general-rep,
	 c-type: "long");
define constant $int-rep
  = make(<c-representation>, alignment: 4, size: 4, more-general: $long-rep,
	 c-type: "int");
define constant $uint-rep
  = make(<c-representation>, alignment: 4, size: 4, more-general: $long-rep,
	 c-type: "unsigned int");
define constant $short-rep
  = make(<c-representation>, alignment: 2, size: 2, more-general: $int-rep,
	 c-type: "short");
define constant $ushort-rep
  = make(<c-representation>, alignment: 2, size: 2, more-general: $uint-rep,
	 c-type: "unsigned short");
define constant $byte-rep
  = make(<c-representation>, alignment: 1, size: 1, more-general: $short-rep,
	 c-type: "signed char");
define constant $ubyte-rep
  = make(<c-representation>, alignment: 1, size: 1, more-general: $ushort-rep,
	 c-type: "unsigned char");

define constant $char-space-rep
  = make(<c-representation>, alignment: 2, size: 2, more-general: $general-rep,
	 c-type: "unsigned short");
define constant $byte-char-space-rep
  = make(<c-representation>, alignment: 1, size: 1,
	 more-general: $char-space-rep, c-type: "unsigned char");
define constant $char-speed-rep
  = make(<c-representation>, alignment: 4, size: 4, more-general: $general-rep,
	 c-type: "unsigned int");

define constant $boolean-space-rep
  = make(<c-representation>, alignment: 1, size: 1, more-general: $heap-rep,
	 c-type: "char");
define constant $boolean-speed-rep
  = make(<c-representation>, alignment: 4, size: 4, more-general: $heap-rep,
	 c-type: "int");

define method pick-representation
    (type :: <ctype>, optimize-for :: one-of(#"speed", #"space"))
    => rep :: <c-representation>;
  $general-rep;
end;

define method pick-representation
    (type :: <cclass>, optimize-for :: one-of(#"speed", #"space"))
    => rep :: <c-representation>;
  if (type == dylan-value(#"<fixed-integer>"))
    $long-rep;
  elseif (type == dylan-value(#"<single-float>"))
    $single-rep;
  elseif (type == dylan-value(#"<double-float>"))
    $double-rep;
  elseif (type == dylan-value(#"<extended-float>"))
    $extended-rep;
  elseif (type == dylan-value(#"<boolean>"))
    select (optimize-for)
      #"speed" => $boolean-speed-rep;
      #"space" => $boolean-space-rep;
    end;
  elseif (type == dylan-value(#"<character>"))
    select (optimize-for)
      #"speed" => $char-speed-rep;
      #"space" => $char-space-rep;
    end;
  elseif (#f /* ### no data word */)
    $heap-rep;
  else
    $general-rep;
  end;
end;

define method pick-representation
    (type :: <limited-ctype>, optimize-for :: one-of(#"speed", #"space"))
    => rep :: <c-representation>;
  pick-representation(type.base-class, optimize-for);
end;

define method pick-representation
    (type :: <byte-character-ctype>, optimize-for == #"space")
    => rep :: <c-representation>;
  $byte-char-space-rep;
end;

define constant $byte-bits = 8;
define constant $short-bits = 16;
define constant $int-bits = 32;

define method pick-representation
    (type :: <limited-integer-ctype>, optimize-for == #"space",
     #next next-method)
    => rep :: <c-representation>;
  if (type.base-class == dylan-value(#"<fixed-integer>"))
    let bits = max(integer-length(type.low-bound),
		   integer-length(type.high-bound));
    if (negative?(type.low-bound))
      if (bits < $byte-bits)
	$byte-rep;
      elseif (bits < $short-bits)
	$short-rep;
      elseif (bits < $int-bits)
	$int-rep;
      else
	$long-rep;
      end;
    else
      if (bits <= $byte-bits)
	$ubyte-rep;
      elseif (bits <= $short-bits)
	$ushort-rep;
      elseif (bits <= $int-bits)
	$uint-rep;
      else
	$long-rep;
      end;
    end;
  else
    next-method();
  end;
end;

define method integer-length (int :: <integer>) => res :: <integer>;
  if (negative?(int))
    integer-length(lognot(int));
  else
    for (len from 0,
	 int = int then ash(int, -1),
	 until: zero?(int))
    finally
      len;
    end;
  end;
end;

define method pick-representation
    (type :: <union-ctype>, optimize-for :: one-of(#"speed", #"space"))
    => rep :: <c-representation>;
  let mem = type.members;
  if (empty?(mem))
    error("The empty type has no representation.");
  else
    reduce1(merge-representations,
	    map(rcurry(pick-representation, optimize-for)));
  end;
end;

define method merge-representations
    (rep1 :: <c-representation>, rep2 :: <c-representation>)
    => res :: <c-representation>;
  until (rep1 == rep2)
    if (rep1.representation-depth > rep2.representation-depth)
      rep1 := rep1.more-general-representation;
    else
      rep2 := rep2.more-general-representation;
    end;
  end;
  rep1;
end;
