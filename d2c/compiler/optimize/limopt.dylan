RCS-Header: $Header: /scm/cvs/src/d2c/compiler/optimize/limopt.dylan,v 1.3 2002/07/13 00:57:21 bruce Exp $
module: cheese
Copyright: See below.
Synopsis: Optimizer support for limited collections.

//-------------------------------------------------------------------------
// Copyright (C) 2000 Eric Kidd
// (Please add your name and the year you released your changes here.)
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE AUTHOR(S) BE LIABLE FOR ANY CLAIM, DAMAGES OR
// OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
// ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
// OTHER DEALINGS IN THE SOFTWARE.
//
// Except as contained in this notice, the name of the author(s) shall not
// be used in advertising or otherwise to promote the sale, use or other
// dealings in this Software without prior written authorization from the
// author(s).
//-------------------------------------------------------------------------


//=========================================================================
//  find-limited-collection-implementation
//=========================================================================
//  Map a limited collection type to the class we'll use to implement it
//  in the runtime. The ctype module uses this information to improve the
//  results of some type operations.
//
//  Return #f if there's no implementation class (as in the case of
//  uninstantiable types), or if you're too lazy to tell the optimizer
//  about it. But don't supply the *wrong* implementation class, or
//  Bad Things will happen.
//
//  Limited types are memoized, so the result of this function is
//  effectively cached.

// "Limited simple vector of limited integer" classes. Used below.
define constant $lsvli-classes =
  #[#"<simple-uchar-vector>", #"<simple-schar-vector>",
    #"<simple-ushort-vector>", #"<simple-sshort-vector>",
    #"<limited-simple-vector>"];

define method find-limited-collection-implementation
    (type :: <limited-collection-ctype>)
 => (cclass :: false-or(<cclass>))
  select (type.base-class)
    
    // limited(<table>, of: ...)
    // limited(<object-table>, of: ...)
    specifier-type(#"<table>"), specifier-type(#"<object-table>") =>
      if (type.element-type == specifier-type(#"<object>"))
	specifier-type(#"<simple-object-table>");
      else
	specifier-type(#"<limited-object-table>");
      end if;
      
    // limited(<vector>, of: ...)
    // We can't do anything with this, because it might be a stretchy vector
    // or a simple vector. D'oh! So don't use this when declaring type
    // constraints.

    // limited(<simple-vector>, of: ...)
    specifier-type(#"<simple-vector>") =>
      let elem-type = type.element-type;
      case
	elem-type == specifier-type(#"<object>") =>
	  specifier-type(#"<simple-object-vector>");
	elem-type == specifier-type(#"<integer>") =>
	  specifier-type(#"<simple-integer-vector>");
	elem-type == specifier-type(#"<double-float>") =>
	  specifier-type(#"<simple-double-vector>");
	instance?(elem-type, <limited-integer-ctype>) =>
	  look-up-class-by-limited-integer-type(elem-type, $lsvli-classes);
	otherwise =>
	  specifier-type(#"<limited-simple-vector>")
      end case;

    otherwise =>
      #f;
  end select;
end method find-limited-collection-implementation;

// Hang our method on the ctype module's hook.
*find-limited-collection-implementation* :=
  find-limited-collection-implementation;


//=========================================================================
//  look-up-class-by-limited-integer-type
//=========================================================================
//  This is some gross support code for dealing with limited integer
//  element types gracefully.
//
//  XXX - This code is largely duplicated in
//  runtime/limited-collection.dylan.
//  XXX - These constants *really* don't belong here. We could probably
//  get them from the C-Representation module.

// From /usr/include/limits.h.
define constant $CHAR_MIN  = -128;
define constant $CHAR_MAX  = 127;
define constant $UCHAR_MAX = 255;
define constant $SHRT_MIN  = -32768;
define constant $SHRT_MAX  = 32767;
define constant $USHRT_MAX = 65535;

// A little help for selecting one of several classes based on a limited
// integer type. The 'five-classes' vector should contain an element for
// each of <%u-char>, <%s-char>, <%u-short>, <%s-short> and a default value,
// in that order.
define function look-up-class-by-limited-integer-type
    (type :: <limited-integer-ctype>, five-classes :: <simple-object-vector>)
 => (class :: <cclass>)
  let choice =
    block (return)
      let min-ext = type.low-bound;
      let max-ext = type.high-bound;
      unless (min-ext & max-ext)
	return(4);
      end unless;
      // These two conversions may cause an error.
      let min = as(<integer>, min-ext);
      let max = as(<integer>, max-ext);
      case
	(min == 0) =>
	  select (max by \==)
	    $UCHAR_MAX => 0;
	    $USHRT_MAX => 2;
	    otherwise  => 4;
	  end select;
	(min == $CHAR_MIN & max == $CHAR_MAX) => 1;
	(min == $SHRT_MIN & max == $SHRT_MAX) => 3;
	otherwise => 4;
      end case;
    exception (e :: <error>)
      4;
    end block;
  specifier-type(five-classes[choice]);
end function look-up-class-by-limited-integer-type;


//=========================================================================
//  make-limited-collection-transformer
//=========================================================================
//  We optimize make(<limited-collection>) to call make-limited-collection
//  directly. This allows the compiler to select the appropriate method
//  at compile time.
//
//  We get called (indirectly) by optimize(<component>,<known-call>).
//
//  make(type :: <limited-collection>, #rest args)
//    => apply(make-limited-collection,
//             type.base-class, type.element-type, type, args);

define method make-limited-collection-transformer
    (component :: <component>, call :: <known-call>)
 => (did-anything? :: <boolean>)
  //dformat("\nWell, we're making a valiant attempt, anyway.\n");
  block (return)
    local method give-up () return (#f) end;
    
    // Fetch our arguments.
    let (okay?, type, init-keywords) = extract-args(call, 1, #f, #t, #f);
    unless (okay?) give-up() end;
    
    // Examine our first argument to see if we can do anything with it.
    let ctype = extract-constant-type(type);
    unless (ctype & instance?(ctype, <limited-collection-ctype>))
      give-up();
    end unless;
    
    //dformat("Looks promising!\n");

    // Fire up our builder.
    let builder = make-builder(component);
    let assign = call.dependents.dependent;
    let policy = assign.policy;
    let source = assign.source-location;

    // Create our first two arguments by ripping apart the ctype.
    let base = make-ssa-var(builder, #"base-class",
			    specifier-type(#"<class>"));
    build-assignment(builder, policy, source, base,
		     make-literal-constant(builder, ctype.base-class));
    let contains = make-ssa-var(builder, #"element-type",
				specifier-type(#"<type>"));
    build-assignment(builder, policy, source, contains,
		     make-literal-constant(builder, ctype.element-type));
    
    // Now, build our a call to make-limited-collection.
    let new-call
      = make-unknown-call(builder,
			  ref-dylan-defn(builder, policy, source,
					 #"make-limited-collection"),
			  #f, pair(base, pair(contains,
					      pair(type, init-keywords))));
    insert-before(component, assign, builder-result(builder));
    replace-expression(component, call.dependents, new-call);

    #t;
  end block;
end method make-limited-collection-transformer;

define-transformer(#"make", #(#"<limited-collection>"),
		   make-limited-collection-transformer);


//=========================================================================
//  Delayed Call Optimizations
//=========================================================================
//  We don't want to optimize certain calls during a simplification pass,
//  because we'll be able to do a better job later.
//
//  When convert encounters an inline function, it passes it to the
//  optimizer for simplification, then stores it for later use. When the
//  function is inlined, we optimize it again.
//
//  This normally works. But in certain cases, it backfires--the
//  simplifcation pass chooses the wrong optimization, and we can't fix it
//  later. So we occasionally need to hide things from the optimizer.
//
//  Here's how it works:
//    1) Hide the call during the simplification pass by changing
//       it from a <unknown-call> to a <delayed-optimization-call> using
//       a transformer.
//    2) Unhide the call during a regular pass (using a method on optimize).
//    3) Transform the call in the usual fashion.
//
//  Of course, if there's no simplification pass, we'll go directly to 3.

define method optimize
    (component :: <component>, call :: <delayed-optimization-call>) => ()
  unless (*optimizer*.simplification-pass?)
    // STEP 2: Unhide the call during a real optimization pass.
    change-call-kind(component, call, <unknown-call>,
		     use-generic-entry: call.use-generic-entry?);
  end unless;
end method optimize;

define method delayed-transformer-wrapper
    (transformer :: <function>, component :: <component>,
     call :: <unknown-call>)
 => (did-anything? :: <boolean>)
  if (*optimizer*.simplification-pass?)
    // STEP 1: Hide the call so we can optimize it later, when we know more.
    change-call-kind(component, call, <delayed-optimization-call>,
		     use-generic-entry: call.use-generic-entry?);
    #t;
  else
    // STEP 3: OK, it's later. Call the real transformer.
    transformer(component, call);
  end if;
end method delayed-transformer-wrapper;

define method define-delayed-transformer
    (call :: <symbol>, transformer :: <function>) => ()
  define-transformer(call, #"gf", curry(delayed-transformer-wrapper,
					transformer));
end method define-delayed-transformer;


//=========================================================================
//  size() optimizations
//=========================================================================
//  When possible, we want to get a collection's size at compile time.
//  This allows us to optimize away unnecessary bounds checks.
//
//  We can do this if the collection's derived-type is a limited type
//  with a value for size-or-dimensions.

define method size-transformer
    (component :: <component>, call :: <unknown-call>)
 => (did-anything? :: <boolean>)
  //dformat("We might be able to do something here.\n");
  block (return)
    let (okay?, arg) = extract-args(call, 1, #f, #f, #f);
    if (okay?)
      let type = arg.derived-type;
      if (instance?(type, <limited-collection-ctype>))
	let sz = type.size-or-dimension;
	if (instance?(sz, <integer>))
	  //dformat("We should do something here.\n");
	  replace-expression(component, call.dependents,
			     make-literal-constant(make-builder(component),
						   as(<ct-value>, sz)));
	  return(#t);
	end if;
      end if;
    end if;
    #f;
  end block;
end method size-transformer;

define-delayed-transformer(#"size", size-transformer);


//=========================================================================
//  instance? optimizations
//=========================================================================
//  We want to suppress type-checks of the form:
//    let v :: <limited-object-table> = ...;
//    instance?(v, limited(<table>, of: <object>));
//  These are very rare, but typically show up in 'make' methods.
//
//  What about:
//    let <10-vec> = limited(<simple-vector>, size: 10);
//    let v :: <10-vec> = ...;
//    instance?(v, limited(<table>, of: <object>));
//
//  This is the wrong approach. I need to fix 'make' somehow. Perhaps we
//  can wrap a <truly-the> around the result of calling 'make' on limited
//  collection types? Or screw with the 'make' handler above? Hmm.

/*
define method build-instance?
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     value :: <leaf>, lim-type :: <limited-collection-ctype>,
     #next next-method)
 => (res :: <expression>)
  let only-possible-cclass = lim-type.implementation-class;
  if (value.derived-type == only-possible-cclass)
    
  else
    next-method();
  end if;
end method build-instance?;
*/
