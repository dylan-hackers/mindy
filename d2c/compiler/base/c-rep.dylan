module: c-representation
rcs-header: $Header: /scm/cvs/src/d2c/compiler/base/c-rep.dylan,v 1.11 2002/12/05 21:12:00 andreas Exp $
copyright: see below

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
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

define constant $byte-bits = 8;

define variable *pointer-alignment* = #f;
define variable *pointer-size* = #f;
define variable *short-alignment* = #f;
define variable *short-size* = #f;
define variable *int-alignment* = #f;
define variable *int-size* = #f;
define variable *long-alignment* = #f;
define variable *long-size* = #f;
define variable *long-long-alignment* = #f;
define variable *long-long-size* = #f;
define variable *single-alignment* = #f;
define variable *single-size* = #f;
define variable *double-alignment* = #f;
define variable *double-size* = #f;
define variable *long-double-alignment* = #f;
define variable *long-double-size* = #f;

define variable *data-word-size* = #f;
define variable *data-word-alignment* = #f;

define abstract class <c-representation>
    (<representation>, <identity-preserving-mixin>)
  slot representation-name :: false-or(<symbol>),
    init-value: #f, init-keyword: name:;
  slot more-general-representation :: false-or(<representation>),
    init-value: #f, init-keyword: more-general:;
  slot representation-depth :: <integer>;
  slot representation-to-more-general
      :: type-union(<byte-string>, one-of(#t, #f)),
    init-value: #t, init-keyword: to-more-general:;
  slot representation-from-more-general
      :: type-union(<byte-string>, one-of(#t, #f)),
    init-value: #t, init-keyword: from-more-general:;
  constant slot representation-alignment :: <integer>,
    required-init-keyword: alignment:;
  constant slot representation-size :: <integer>,
    required-init-keyword: size:;
  constant slot representation-c-type :: <string>,
    required-init-keyword: c-type:;
end;

define method representation-has-bottom-value? (res :: <representation>)
    => res :: <boolean>;
  #t;
end method representation-has-bottom-value?;

define method initialize
    (rep :: <c-representation>, #next next-method, #key more-general) => ();
  next-method();
  rep.representation-depth
    := if (more-general)
	 more-general.representation-depth + 1;
       else
	 0;
       end;
end method initialize;

define method print-object (rep :: <c-representation>, stream :: <stream>)
    => ();
  pprint-fields(rep, stream, c-type: rep.representation-c-type);
end method print-object;

define class <general-representation> (<c-representation>)
  inherited slot representation-to-more-general, init-value: #f;
  inherited slot representation-from-more-general, init-value: #f;
end class <general-representation>;

define class <heap-representation> (<c-representation>)
end class <heap-representation>;

define class <immediate-representation> (<c-representation>)
end class <immediate-representation>;

define method representation-has-bottom-value?
    (res :: <immediate-representation>)
    => res :: <boolean>;
  #f;
end method representation-has-bottom-value?;

define class <c-data-word-representation>
    (<immediate-representation>, <data-word-representation>)
  slot representation-class :: <cclass>, init-keyword: class:;
  slot representation-data-word-member :: <byte-string>,
    required-init-keyword: data-word-member:;
end class <c-data-word-representation>;


define variable *general-rep* :: false-or(<c-representation>) = #f;
define variable *heap-rep* :: false-or(<c-representation>) = #f;
define variable *boolean-rep* :: false-or(<c-representation>) = #f;

define variable *long-long-rep* :: false-or(<c-representation>) = #f;
define variable *long-rep* :: false-or(<c-representation>) = #f;
define variable *int-rep* :: false-or(<c-representation>) = #f;
define variable *uint-rep* :: false-or(<c-representation>) = #f;
define variable *short-rep* :: false-or(<c-representation>) = #f;
define variable *ushort-rep* :: false-or(<c-representation>) = #f;
define variable *byte-rep* :: false-or(<c-representation>) = #f;
define variable *ubyte-rep* :: false-or(<c-representation>) = #f;

define variable *ptr-rep* :: false-or(<c-representation>) = #f;

define variable *float-rep* :: false-or(<c-representation>) = #f;
define variable *double-rep* :: false-or(<c-representation>) = #f;
define variable *long-double-rep* :: false-or(<c-representation>) = #f;


define method seed-representations () => ();
  local
    method set-representations
	(class :: <cclass>, speed-rep :: <c-representation>,
	 space-rep :: <c-representation>)
	=> ();
      unless (class.abstract?)
	class.direct-speed-representation := speed-rep;
	class.direct-space-representation := space-rep;
      end unless;
      class.general-speed-representation := speed-rep;
      class.general-space-representation := space-rep;
    end;

  *pointer-size* := *current-target*.pointer-size;
  *pointer-alignment*
    := *current-target*.pointer-alignment | *current-target*.pointer-size;
  
  *short-size* := *current-target*.short-size;
  *short-alignment*
    := *current-target*.short-alignment | *current-target*.short-size;
  
  *int-size* := *current-target*.integer-size;
  *int-alignment*
    := *current-target*.integer-alignment | *current-target*.integer-size;
  
  *long-size* := *current-target*.long-size;
  *long-alignment*
    := *current-target*.long-alignment | *current-target*.long-size;
  
  *long-long-size* := *current-target*.long-long-size;
  *long-long-alignment*
    := *current-target*.long-long-alignment | *current-target*.long-long-size;

  *single-size* := *current-target*.single-size;
  *single-alignment*
    := *current-target*.single-alignment | *current-target*.single-size;

  *double-size* := *current-target*.double-size;
  *double-alignment*
    := *current-target*.double-alignment | *current-target*.double-size;

  *long-double-size* := *current-target*.long-double-size;
  *long-double-alignment*
    := *current-target*.long-double-alignment
    | *current-target*.long-double-size;

  *data-word-size* := max(*pointer-size*, *long-size*);
  *data-word-alignment* := max(*pointer-alignment*, *long-alignment*);

  unless (*general-rep*)
    *general-rep*
      := make(<general-representation>, name: #"general",
	      alignment: *pointer-alignment*,
	      size: *pointer-size* + *data-word-size*,
	      c-type: "descriptor_t");
  end;
  unless (*heap-rep*)
    *heap-rep*
      := make(<heap-representation>, name: #"heap",
	      alignment: *pointer-alignment*, size: *pointer-size*,
	      c-type: "heapptr_t", more-general: *general-rep*,
	      to-more-general: #f, from-more-general: "%s.heapptr");
  end;
  unless (*boolean-rep*)
    *boolean-rep*
      := make(<immediate-representation>, name: #"boolean",
	      more-general: *heap-rep*,
	      to-more-general: "(%s ? obj_True : obj_False)",
	      from-more-general: "(%s != obj_False)",
	      alignment: *int-alignment*, size: *int-size*,
	      c-type: "int");
    let space-rep = make(<immediate-representation>, name: #"boolean-char", 
			 more-general: *boolean-rep*,
			 alignment: 1, size: 1, c-type: "char");
    set-representations(dylan-value(#"<boolean>"), *boolean-rep*, space-rep);
    set-representations(dylan-value(#"<true>"), *boolean-rep*, space-rep);
    set-representations(dylan-value(#"<false>"), *boolean-rep*, space-rep);
  end;
  unless (*long-long-rep*)
    let double-int-cclass = dylan-value(#"<double-integer>");
    if (*long-long-size*)
      *long-long-rep*
        := make(<immediate-representation>, name: #"long-long",
                alignment: *long-long-alignment*, size: *long-long-size*,
                more-general: *heap-rep*, c-type: "long long",
                to-more-general: "make_double_integer(%s)",
                from-more-general: "double_integer_value(%s)");
    else
      *long-long-rep*
        := make(<immediate-representation>, name: #"long-long",
                alignment: *long-alignment*, size: *long-size* + *long-size*,
                more-general: *heap-rep*, c-type: "gd_long_long",
                to-more-general: "make_double_integer(%s)",
                from-more-general: "double_integer_value(%s)");
    end;
    set-representations(double-int-cclass, *long-long-rep*, *long-long-rep*);
  end;
  begin
    let fixed-int-cclass = dylan-value(#"<integer>");
    unless (*long-rep*)
      *long-rep* := make(<c-data-word-representation>, name: #"long",
			 alignment: *long-alignment*, size: *long-size*,
			 more-general: *general-rep*, c-type: "long",
			 to-more-general: #f,
			 from-more-general: "%s.dataword.l",
			 class: fixed-int-cclass, data-word-member: "l");
      set-representations(fixed-int-cclass, *long-rep*, *long-rep*);
    end;
    unless (*int-rep*)
      *int-rep* := make(<c-data-word-representation>, name: #"int",
			alignment: *int-alignment*, size: *int-size*,
			more-general: *long-rep*, c-type: "int",
			class: fixed-int-cclass, data-word-member: "l");
    end;
    unless (*uint-rep*)
      *uint-rep* := make(<c-data-word-representation>, name: #"uint",
			 alignment: *int-alignment*, size: *int-size*,
			 more-general: *long-rep*, c-type: "unsigned int",
			 class: fixed-int-cclass, data-word-member: "l");
    end;
    unless (*short-rep*)
      *short-rep* := make(<c-data-word-representation>, name: #"short",
			  alignment: *short-alignment*, size: *short-size*,
			  more-general: *int-rep*, c-type: "short",
			  class: fixed-int-cclass, data-word-member: "l");
    end;
    unless (*ushort-rep*)
      *ushort-rep* := make(<c-data-word-representation>, name: #"ushort",
			   alignment: *short-alignment*, size: *short-size*,
			   more-general: *uint-rep*, c-type: "unsigned short",
			   class: fixed-int-cclass, data-word-member: "l");
    end;
    unless (*byte-rep*)
      *byte-rep* := make(<c-data-word-representation>, name: #"byte",
			 alignment: 1, size: 1,
			 more-general: *short-rep*, c-type: "signed char",
			 class: fixed-int-cclass, data-word-member: "l");
    end;
    unless (*ubyte-rep*)
      *ubyte-rep* := make(<c-data-word-representation>, name: #"ubyte",
			  alignment: 1, size: 1,
			  more-general: *ushort-rep*, c-type: "unsigned char",
			  class: fixed-int-cclass, data-word-member: "l");
    end;
  end;
  unless (*float-rep*)
    let sf-cclass = dylan-value(#"<single-float>");
    let sf-rep
      = make(<c-data-word-representation>, name: #"float",
	     more-general: *general-rep*,
	     to-more-general: #f, from-more-general: "%s.dataword.f",
	     alignment: 4, size: 4, c-type: "float",
	     class: sf-cclass, data-word-member: "f");
    set-representations(sf-cclass, sf-rep, sf-rep);
    *float-rep* := sf-rep;
  end;
  unless (*double-rep*)
    let df-class = dylan-value(#"<double-float>");
    let df-rep
      = if (*double-size* > *data-word-size*
              | *double-alignment* > *data-word-alignment*)
	  make(<immediate-representation>, name: #"double",
	       more-general: *heap-rep*,
	       to-more-general: "make_double_float(%s)",
	       from-more-general: "double_float_value(%s)",
	       alignment: *double-alignment*, size: *double-size*,
	       c-type: "double");
	else
	  make(<c-data-word-representation>, name: #"double",
	       more-general: *general-rep*,
	       to-more-general: #f, from-more-general: "%s.dataword.d",
	       alignment: *double-alignment*, size: *double-size*,
	       c-type: "double", class: df-class, data-word-member: "d");
	end;
    set-representations(df-class, df-rep, df-rep);
    *double-rep* := df-rep;
  end;
  unless (*long-double-rep*)
    let xf-class = dylan-value(#"<extended-float>");
    let xf-rep
      = if (*long-double-size* > *data-word-size*
              | *long-double-alignment* > *data-word-alignment*)
	  make(<immediate-representation>, name: #"long-double",
	       more-general: *heap-rep*,
	       to-more-general: "make_extended_float(%s)",
	       from-more-general: "extended_float_value(%s)",
	       alignment: *long-double-alignment*, size: *long-double-size*,
	       c-type: "long double");
	else
	  make(<c-data-word-representation>, name: #"long-double",
	       more-general: *general-rep*,
	       to-more-general: #f, from-more-general: "%s.dataword.x",
	       alignment: *long-double-alignment*, size: *long-double-size*,
	       c-type: "long double", class: xf-class, data-word-member: "x");
	end;
    set-representations(xf-class, xf-rep, xf-rep);
    *long-double-rep* := xf-rep;
  end;
  unless (*ptr-rep*)
    let ptr-cclass = dylan-value(#"<raw-pointer>");
    *ptr-rep* := make(<c-data-word-representation>, name: #"ptr",
		      alignment: *pointer-alignment*, size: *pointer-size*,
		      more-general: *general-rep*, c-type: "void *",
		      to-more-general: #f,
		      from-more-general: "%s.dataword.ptr",
		      class: ptr-cclass, data-word-member: "ptr");
    set-representations(ptr-cclass, *ptr-rep*, *ptr-rep*);
  end;
end method seed-representations;

define method c-rep (c-type :: <symbol>) => rep :: false-or(<representation>);
  select (c-type)
    #"general", #"object" => *general-rep*;
    #"heap" => *heap-rep*;
    #"boolean" => *boolean-rep*;
    #"long-long" => *long-long-rep*;
    #"long" => *long-rep*;
    #"int" => *int-rep*;
    #"uint", #"unsigned-int" => *uint-rep*;
    #"short" => *short-rep*;
    #"ushort", #"unsigned-short" => *ushort-rep*;
    #"byte", #"char" => *byte-rep*;
    #"ubyte", #"unsigned-char" => *ubyte-rep*;
    #"float" => *float-rep*;
    #"double" => *double-rep*;
    #"long-double" => *long-double-rep*;
    #"ptr" => *ptr-rep*;
    #"void" => #f;
    otherwise =>
      error("unknown c-rep %=", c-type);
  end;
end method;


// Assigning class representations.

define method assign-representations (class :: <cclass>) => ();
  assert(class.direct-speed-representation == #f);
  assert(class.direct-space-representation == #f);
  //
  // If the class is functional, then the representation to use depends on
  // what is in the class.
  if (class.functional?)
    //
    // Make sure we've computed the layout for the class.
    // This is wrong if we arrive here because of a `layout-slots-for'
    // call.  Thus I changed the following.  tc
    //
    // layout-slots-for(class); // tc
    //
    layout-slots-for-if-possible(class);
    //
    // Layout-slots-for will call either use-data-word-representation or
    // use-general-representation if appropriate.  Therefore, if it finishes
    // and neither were called, then use a heap representation.
    unless (class.direct-speed-representation)
      class.direct-speed-representation := *heap-rep*;
      class.direct-space-representation := *heap-rep*;    
    end unless;
  else
    //
    // The class isn't functional, so use the heap representation.
    class.direct-speed-representation := *heap-rep*;
    class.direct-space-representation := *heap-rep*;    
  end if;
end method assign-representations;


define method use-data-word-representation
    (class :: <cclass>, data-word-type :: <ctype>)
    => ();
  unless (class.abstract? | class.direct-speed-representation)
    local
      method dup-rep (rep :: <c-data-word-representation>,
		      more-gen :: <c-representation>,
		      to-more-gen, from-more-gen)
	  => new-rep :: <c-data-word-representation>;
	make(<c-data-word-representation>,
	     more-general: more-gen,
	     to-more-general: to-more-gen,
	     from-more-general: from-more-gen,
	     alignment: rep.representation-alignment,
	     size: rep.representation-size,
	     c-type: rep.representation-c-type,
	     class: class,
	     data-word-member: rep.representation-data-word-member);
      end method dup-rep;
    let speed-rep = pick-representation(data-word-type, #"speed");
    class.direct-speed-representation
      := dup-rep(speed-rep, *general-rep*, #f,
		 concatenate("%s.dataword.",
			     speed-rep.representation-data-word-member));
    let space-rep = pick-representation(data-word-type, #"space");
    class.direct-space-representation
      := dup-rep(space-rep, class.direct-speed-representation, #t, #t);
  end unless;
end method use-data-word-representation;


define method use-general-representation
    (class :: <cclass>) => ();
  unless (class.abstract?)
    class.direct-speed-representation := *general-rep*;
    class.direct-space-representation := *general-rep*;
  end unless;
end method use-general-representation;



// Pick-representation methods.
  
define method pick-representation
    (class :: <cclass>, optimize-for :: one-of(#"speed", #"space"))
    => rep :: <c-representation>;
  select (optimize-for)
    #"speed" =>
      class.general-speed-representation
	| (class.general-speed-representation
	     := general-representation(class, #"speed"));
    #"space" =>
      class.general-space-representation
	| (class.general-space-representation
	     := general-representation(class, #"space"));
  end select;
end method pick-representation;

define method general-representation
    (class :: <cclass>, optimize-for :: one-of(#"speed", #"space"))
    => rep :: <c-representation>;
  //
  // Check to see if the class and its subclasses are sealed.
  if (class.all-subclasses-known?)
    //
    // The class is sealed.  We can explicitly look at all of possible
    // direct classes.
    let direct-classes = find-direct-classes(class);
    assert(direct-classes);
    if (direct-classes.size == 1)
      //
      // There is only one possible direct class, so we don't need to
      // represent any type information.  
      direct-representation(direct-classes.first, optimize-for);
    else
      block (return)
	for (direct-class in direct-classes)
	  let direct-rep = direct-representation(direct-class, optimize-for);
	  if (instance?(direct-rep, <data-word-representation>)
		| instance?(direct-rep, <general-representation>))
	    return(*general-rep*);
	  end if;
	end for;
	*heap-rep*;
      end block;
    end if;
  elseif (class.not-functional?)
    *heap-rep*;
  else
    *general-rep*;
  end if;
end method general-representation;

define method pick-representation
    (type :: <direct-instance-ctype>,
     optimize-for :: one-of(#"speed", #"space"))
    => rep :: <c-representation>;
  direct-representation(type.base-class, optimize-for);
end method pick-representation;

define method direct-representation
    (class :: <cclass>, optimize-for :: one-of(#"speed", #"space"))
    => rep :: <c-representation>;
  unless (class.direct-speed-representation)
    assign-representations(class);
  end unless;
  select (optimize-for)
    #"speed" =>
      class.direct-speed-representation;
    #"space" =>
      class.direct-space-representation;
  end select;
end method direct-representation;


define method pick-representation
    (type :: <limited-ctype>, optimize-for :: one-of(#"speed", #"space"))
    => rep :: <c-representation>;
  pick-representation(type.base-class, optimize-for);
end;


define variable *byte-char-rep* = #f;

define method pick-representation
    (type :: <byte-character-ctype>, optimize-for == #"space")
    => rep :: <c-representation>;
  if (*byte-char-rep*)
    *byte-char-rep*;
  else
    let char-rep = pick-representation(type.base-class, optimize-for);
    *byte-char-rep*
      := make(<c-data-word-representation>, name: #"byte-char",
	      more-general: char-rep,
	      alignment: 1, size: 1, c-type: "unsigned char",
	      class: type.base-class, data-word-member: "l");
  end;
end;


define method pick-representation
    (type :: <limited-integer-ctype>, optimize-for == #"space",
     #next next-method)
    => rep :: <c-representation>;
  if (type.base-class == dylan-value(#"<integer>"))
    let bits = max(integer-length(type.low-bound),
		   integer-length(type.high-bound));
    if (negative?(type.low-bound))
      let bytes = ceiling/(bits + 1, $byte-bits);
      if (bytes <= 1)
	*byte-rep*;
      elseif (bytes <= *short-size*)
	*short-rep*;
      elseif (bytes <= *int-size*)
	*int-rep*;
      else
	*long-rep*;
      end;
    else
      let bytes = ceiling/(bits, $byte-bits);
      if (bytes <= 1)
	*ubyte-rep*;
      elseif (bytes <= *short-size*)
	*ushort-rep*;
      elseif (bytes <= *int-size*)
	*uint-rep*;
      else
	*long-rep*;
      end if;
    end if;
  else
    next-method();
  end if;
end method pick-representation;

define method pick-representation
    (type :: <union-ctype>, optimize-for :: one-of(#"speed", #"space"))
    => rep :: <c-representation>;
  let mem = type.members;
  if (empty?(mem))
    error("The empty type has no representation.");
  else
    reduce1(merge-representations,
	    map(rcurry(pick-representation, optimize-for), mem));
  end;
end;

define method merge-representations
    (rep1 :: <c-representation>, rep2 :: <c-representation>)
    => res :: <c-representation>;
  if (rep1 == rep2)
    rep1;
  elseif (rep1.representation-depth > rep2.representation-depth)
    merge-representations(rep1.more-general-representation, rep2);
  else
    merge-representations(rep1, rep2.more-general-representation);
  end;
end;

define method pick-representation
    (type :: <unknown-ctype>, optimize-for :: one-of(#"speed", #"space"))
    => rep :: <c-representation>;
  *general-rep*;
end method pick-representation;



// Dump/Load stuff.

define method set-name-and-remember
    (name :: false-or(<symbol>), rep :: <c-representation>)
    => name :: false-or(<symbol>);
  if (name)
    rep.representation-name := name;
    select (name)
      #"general" => assert(~*general-rep*); *general-rep* := rep;
      #"heap" => assert(~*heap-rep*); *heap-rep* := rep;
      #"boolean" => assert(~*boolean-rep*); *boolean-rep* := rep;
      #"long-long" => assert(~*long-long-rep*); *long-long-rep* := rep;
      #"long" => assert(~*long-rep*); *long-rep* := rep;
      #"int" => assert(~*int-rep*); *int-rep* := rep;
      #"uint" => assert(~*uint-rep*); *uint-rep* := rep;
      #"short" => assert(~*short-rep*); *short-rep* := rep;
      #"ushort" => assert(~*ushort-rep*); *ushort-rep* := rep;
      #"byte" => assert(~*byte-rep*); *byte-rep* := rep;
      #"ubyte" => assert(~*ubyte-rep*); *ubyte-rep* := rep;
      #"float" => assert(~*float-rep*); *float-rep* := rep;
      #"double" => assert(~*double-rep*); *double-rep* := rep;
      #"long-double" => assert(~*long-double-rep*); *long-double-rep* := rep;
      #"ptr" => assert(~*ptr-rep*); *ptr-rep* := rep;
      #"byte-char" => assert(~*byte-char-rep*); *byte-char-rep* := rep;
      otherwise => #f;
    end;
  end;
  name;
end;

define constant $rep-slots
  = list(representation-name, #f, set-name-and-remember,
	 more-general-representation, more-general:,
	   more-general-representation-setter,
	 representation-to-more-general, to-more-general:, #f,
	 representation-from-more-general, from-more-general:, #f,
	 representation-alignment, alignment:, #f,
	 representation-size, size:, #f,
	 representation-c-type, c-type:, #f);

add-make-dumper(#"general-representation", *compiler-dispatcher*,
		<general-representation>, $rep-slots, load-external: #t);

add-make-dumper(#"heap-representation", *compiler-dispatcher*,
		<heap-representation>, $rep-slots, load-external: #t);

add-make-dumper(#"immediate-representation", *compiler-dispatcher*,
		<immediate-representation>, $rep-slots, load-external: #t);

add-make-dumper(#"data-word-representation", *compiler-dispatcher*,
		<c-data-word-representation>,
		concatenate($rep-slots,
			    list(representation-class, class:,
				   representation-class-setter,
				 representation-data-word-member,
				   data-word-member:, #f)),
		load-external: #t);


// Seals for file c-rep.dylan

// <general-representation> -- subclass of <c-representation>
define sealed domain make(singleton(<general-representation>));
// <heap-representation> -- subclass of <c-representation>
define sealed domain make(singleton(<heap-representation>));
// <immediate-representation> -- subclass of <c-representation>
define sealed domain make(singleton(<immediate-representation>));
// <c-data-word-representation> -- subclass of <immediate-representation>
define sealed domain make(singleton(<c-data-word-representation>));
